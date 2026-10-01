#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>

struct halo_gzip_workspace {
  unsigned char *input;
  unsigned char *output;
  z_stream stream;
  int initialized;
};

static void halo_gzip_dispose(value workspace) {
  struct halo_gzip_workspace **slot = Data_custom_val(workspace);
  struct halo_gzip_workspace *w = *slot;
  if (w == NULL) return;
  if (w->initialized) {
    deflateEnd(&w->stream);
    w->initialized = 0;
  }
  free(w->input);
  free(w->output);
  free(w);
  *slot = NULL;
}

/* Own buffers across allocations and exceptions, including result allocation. */
static struct custom_operations halo_gzip_ops = {
  .identifier = "halo.gzip.workspace",
  .finalize = halo_gzip_dispose,
  .compare = custom_compare_default,
  .hash = custom_hash_default,
  .serialize = custom_serialize_default,
  .deserialize = custom_deserialize_default,
  .compare_ext = custom_compare_ext_default,
  .fixed_length = custom_fixed_length_default,
};

CAMLprim value ocaml_halo_gzip(value input, value level_value) {
  CAMLparam2(input, level_value);
  CAMLlocal2(workspace, result);
  size_t input_length = caml_string_length(input);
  int level = Int_val(level_value);
  if (level < 0 || level > 9)
    caml_invalid_argument("Compression: invalid gzip level");

  workspace = caml_alloc_custom_mem(&halo_gzip_ops,
                                   sizeof(struct halo_gzip_workspace *), input_length);
  struct halo_gzip_workspace **slot = Data_custom_val(workspace);
  *slot = NULL;
  struct halo_gzip_workspace *w = calloc(1, sizeof(*w));
  if (w == NULL) caml_raise_out_of_memory();
  *slot = w;
  int code = deflateInit2(&w->stream, level, Z_DEFLATED, 15 + 16, 8,
                          Z_DEFAULT_STRATEGY);
  if (code != Z_OK) {
    halo_gzip_dispose(workspace);
    if (code == Z_MEM_ERROR) caml_raise_out_of_memory();
    caml_failwith("Compression: gzip initialization failed");
  }
  w->initialized = 1;
  uLong bound = deflateBound(&w->stream, (uLong)input_length);
  if (bound < input_length || bound > Bsize_wsize(Max_wosize) - 1) {
    halo_gzip_dispose(workspace);
    caml_invalid_argument("Compression: gzip body is too large");
  }
  w->input = malloc(input_length == 0 ? 1 : input_length);
  w->output = malloc((size_t)bound);
  if (w->input == NULL || w->output == NULL) {
    halo_gzip_dispose(workspace);
    caml_raise_out_of_memory();
  }
  memcpy(w->input, String_val(input), input_length);

  /* Keep only native pointers while the OCaml runtime is released. */
  unsigned char *source = w->input;
  unsigned char *destination = w->output;
  z_stream *stream = &w->stream;
  size_t consumed = 0, produced = 0;
  caml_enter_blocking_section();
  do {
    size_t remaining_input = input_length - consumed;
    size_t remaining_output = (size_t)bound - produced;
    stream->next_in = source + consumed;
    stream->avail_in = remaining_input > UINT_MAX ? UINT_MAX : (uInt)remaining_input;
    stream->next_out = destination + produced;
    stream->avail_out = remaining_output > UINT_MAX ? UINT_MAX : (uInt)remaining_output;
    uInt before_input = stream->avail_in, before_output = stream->avail_out;
    int flush = remaining_input > UINT_MAX ? Z_NO_FLUSH : Z_FINISH;
    code = deflate(stream, flush);
    consumed += before_input - stream->avail_in;
    produced += before_output - stream->avail_out;
  } while (code == Z_OK);
  deflateEnd(stream);
  w->initialized = 0;
  caml_leave_blocking_section();
  if (code != Z_STREAM_END) {
    halo_gzip_dispose(workspace);
    if (code == Z_MEM_ERROR) caml_raise_out_of_memory();
    caml_failwith("Compression: gzip compression failed");
  }
  result = caml_alloc_initialized_string(produced, (const char *)w->output);
  halo_gzip_dispose(workspace);
  CAMLreturn(result);
}
