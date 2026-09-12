#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <limits.h>
#include <openssl/rand.h>
#include <uv.h>

CAMLprim value ocaml_halo_telemetry_random_bytes(value length_value) {
  CAMLparam1(length_value);
  CAMLlocal1(bytes);
  mlsize_t length;

  length = Long_val(length_value);
  if (length > INT_MAX)
    caml_invalid_argument("telemetry random byte count is too large");

  bytes = caml_alloc_string(length);
  if (length > 0 && RAND_bytes((unsigned char*) Bytes_val(bytes), (int) length) != 1)
    caml_failwith("OpenSSL could not generate telemetry identifiers");

  CAMLreturn(bytes);
}

CAMLprim value ocaml_halo_telemetry_monotonic_time_ns(value unit) {
  CAMLparam1(unit);
  CAMLreturn(caml_copy_int64((int64_t) uv_hrtime()));
}
