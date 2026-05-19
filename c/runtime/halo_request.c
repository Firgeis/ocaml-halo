#include "halo_runtime_internal.h"
#include "halo_utils.h"

static int halo_request_reserve_headers(struct halo_request* request, size_t min_cap) {
  struct halo_header* headers;
  size_t next_cap;

  if (request->header_cap >= min_cap)
    return 0;

  next_cap = request->header_cap == 0 ? 8 : request->header_cap;
  while (next_cap < min_cap) {
    if (next_cap > SIZE_MAX / 2) {
      next_cap = min_cap;
      break;
    }
    next_cap *= 2;
  }

  headers =
    halo_request_storage_alloc(&request->storage,
                               next_cap * sizeof(*headers),
                               _Alignof(struct halo_header));
  if (headers == NULL)
    return -1;

  if (request->header_count != 0)
    memcpy(headers, request->headers, request->header_count * sizeof(*headers));
  request->headers = headers;
  request->header_cap = next_cap;
  return 0;
}

static int halo_request_reserve_connection_options(struct halo_request* request,
                                                   size_t min_cap) {
  struct halo_connection_option* options;
  size_t next_cap;

  if (request->connection_option_cap >= min_cap)
    return 0;

  next_cap =
    request->connection_option_cap == 0 ? 4 : request->connection_option_cap;
  while (next_cap < min_cap) {
    if (next_cap > SIZE_MAX / 2) {
      next_cap = min_cap;
      break;
    }
    next_cap *= 2;
  }

  options =
    halo_request_storage_alloc(&request->storage,
                               next_cap * sizeof(*options),
                               _Alignof(struct halo_connection_option));
  if (options == NULL)
    return -1;

  if (request->connection_option_count != 0) {
    memcpy(options,
           request->connection_options,
           request->connection_option_count * sizeof(*options));
  }
  request->connection_options = options;
  request->connection_option_cap = next_cap;
  return 0;
}

int halo_request_store_header(struct halo_request* request,
                              const char* name,
                              size_t name_len,
                              const char* value,
                              size_t value_len,
                              struct halo_header** stored_header) {
  if (request == NULL || name == NULL)
    return -1;

  if (halo_request_reserve_headers(request, request->header_count + 1) != 0)
    return -1;

  request->headers[request->header_count].name = (char*) name;
  request->headers[request->header_count].name_len = name_len;
  request->headers[request->header_count].value =
    value == NULL ? (char*) "" : (char*) value;
  request->headers[request->header_count].value_len = value_len;
  request->headers[request->header_count].is_hop_by_hop = 0;
  if (stored_header != NULL)
    *stored_header = &request->headers[request->header_count];
  request->header_count++;
  return 0;
}

int halo_request_store_connection_option(struct halo_request* request,
                                         const char* name,
                                         size_t name_len) {
  char* stored_name;

  if (request == NULL || name == NULL || name_len == 0)
    return -1;

  if (halo_request_reserve_connection_options(request,
                                              request->connection_option_count + 1)
      != 0)
    return -1;

  stored_name = halo_request_storage_strndup(&request->storage, name, name_len);
  if (stored_name == NULL)
    return -1;

  request->connection_options[request->connection_option_count].name = stored_name;
  request->connection_options[request->connection_option_count].name_len = name_len;
  request->connection_option_count++;
  return 0;
}

void halo_request_clear(struct halo_request* request) {
  if (request == NULL)
    return;

  request->method = 0;
  request->method_seen = 0;
  request->headers_complete = 0;
  request->has_content_length = 0;
  request->http_major = 0;
  request->http_minor = 0;
  request->content_length = 0;
  request->method_len = 0;
  request->url_len = 0;
  request->body_len = 0;
  request->current_header_name_len = 0;
  request->current_header_value_len = 0;
  halo_request_storage_reset(&request->storage);
  request->method_text = NULL;
  request->method_cap = 0;
  request->url = NULL;
  request->url_cap = 0;
  request->current_header_name = NULL;
  request->current_header_name_cap = 0;
  request->current_header_value = NULL;
  request->current_header_value_cap = 0;
  request->headers = NULL;
  request->header_count = 0;
  request->header_cap = 0;
  request->connection_options = NULL;
  request->connection_option_count = 0;
  request->connection_option_cap = 0;
  request->known_headers.host_count = 0;
  request->known_headers.host_value = NULL;
  request->known_headers.host_value_len = 0;
  request->known_headers.expect_count = 0;
  request->known_headers.expectation = HALO_EXPECTATION_NONE;

  if (request->body != NULL)
    request->body[0] = '\0';
}

void halo_request_release_body(struct halo_request* request) {
  if (request == NULL)
    return;

  free(request->body);
  request->body = NULL;
  request->body_len = 0;
  request->body_cap = 0;
}

void halo_request_free(struct halo_request* request) {
  if (request == NULL)
    return;

  halo_request_clear(request);
  halo_request_release_body(request);
  halo_request_storage_destroy(&request->storage);
  memset(request, 0, sizeof(*request));
}

struct halo_borrowed_request** halo_borrowed_request_slot(value request) {
  return (struct halo_borrowed_request**) Data_custom_val(request);
}

static void halo_borrowed_request_finalize(value request) {
  struct halo_borrowed_request** slot;
  struct halo_borrowed_request* borrowed;

  slot = halo_borrowed_request_slot(request);
  borrowed = *slot;
  if (borrowed == NULL)
    return;

  free(borrowed);
  *slot = NULL;
}

static struct custom_operations halo_borrowed_request_ops = {
  "ocaml_halo.borrowed_request",
  halo_borrowed_request_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default
};

value halo_borrowed_request_alloc(struct halo_request* request) {
  CAMLparam0();
  CAMLlocal1(ocaml_request);
  struct halo_borrowed_request* borrowed;

  borrowed = malloc(sizeof(*borrowed));
  if (borrowed == NULL)
    caml_failwith("could not allocate borrowed request handle");

  borrowed->request = request;
  borrowed->valid = 1;

  ocaml_request =
    caml_alloc_custom(&halo_borrowed_request_ops,
                      sizeof(struct halo_borrowed_request*),
                      0,
                      1);
  *halo_borrowed_request_slot(ocaml_request) = borrowed;
  CAMLreturn(ocaml_request);
}

struct halo_borrowed_request* halo_borrowed_request_get(value request) {
  struct halo_borrowed_request* borrowed;

  borrowed = *halo_borrowed_request_slot(request);
  if (borrowed == NULL || !borrowed->valid || borrowed->request == NULL)
    caml_invalid_argument("borrowed request escaped its scope");

  return borrowed;
}

static value halo_copy_ocaml_string(const char* data, size_t len) {
  return caml_alloc_initialized_string(len, data == NULL ? "" : data);
}

int halo_header_name_matches(const struct halo_header* header,
                             const char* name,
                             size_t name_len) {
  return header->name_len == name_len
    && strncasecmp(header->name, name, name_len) == 0;
}

int halo_iterate_comma_separated_header_tokens(const char* value,
                                               size_t value_len,
                                               halo_header_token_callback callback,
                                               void* context,
                                               int* saw_token) {
  const char* cursor;
  const char* end;
  int saw_any;

  if (value == NULL || callback == NULL)
    return 0;

  cursor = value;
  end = value + value_len;
  saw_any = 0;

  while (cursor < end) {
    const char* token_start;
    const char* token_end;

    while (cursor < end && (halo_is_ows_char(*cursor) || *cursor == ','))
      cursor++;
    if (cursor >= end)
      break;

    token_start = cursor;
    while (cursor < end && *cursor != ',')
      cursor++;
    token_end = cursor;
    while (token_end > token_start && halo_is_ows_char(token_end[-1]))
      token_end--;
    while (token_start < token_end && halo_is_ows_char(*token_start))
      token_start++;
    if (token_start == token_end)
      continue;

    if (!callback(token_start, (size_t) (token_end - token_start), context))
      return 0;
    saw_any = 1;
  }

  if (saw_token != NULL)
    *saw_token = saw_any;
  return 1;
}

static int halo_header_is_hop_by_hop(const struct halo_header* header) {
  return header->is_hop_by_hop != 0;
}

static value halo_copy_ocaml_headers_filtered(const struct halo_request* request,
                                              int strip_hop_by_hop) {
  CAMLparam0();
  CAMLlocal5(list, pair, tuple, name_value, header_value);
  size_t i;

  list = Val_emptylist;
  for (i = request->header_count; i > 0; i--) {
    if (strip_hop_by_hop && halo_header_is_hop_by_hop(&request->headers[i - 1]))
      continue;

    name_value = halo_copy_ocaml_string(request->headers[i - 1].name,
                                        request->headers[i - 1].name_len);
    header_value = halo_copy_ocaml_string(request->headers[i - 1].value,
                                          request->headers[i - 1].value_len);

    tuple = caml_alloc_tuple(2);
    Store_field(tuple, 0, name_value);
    Store_field(tuple, 1, header_value);
    pair = caml_alloc(2, 0);
    Store_field(pair, 0, tuple);
    Store_field(pair, 1, list);
    list = pair;
  }

  CAMLreturn(list);
}

static value halo_copy_ocaml_headers(const struct halo_request* request) {
  return halo_copy_ocaml_headers_filtered(request, 1);
}

static value halo_copy_ocaml_raw_headers(const struct halo_request* request) {
  return halo_copy_ocaml_headers_filtered(request, 0);
}

static value halo_find_ocaml_header_filtered(const struct halo_request* request,
                                             const char* name,
                                             size_t name_len,
                                             int strip_hop_by_hop) {
  CAMLparam0();
  CAMLlocal2(result, header_value);
  size_t i;

  for (i = 0; i < request->header_count; i++) {
    const struct halo_header* header;

    header = &request->headers[i];
    if (strip_hop_by_hop && halo_header_is_hop_by_hop(header))
      continue;
    if (!halo_header_name_matches(header, name, name_len))
      continue;

    header_value = halo_copy_ocaml_string(header->value, header->value_len);
    result = caml_alloc(1, 0);
    Store_field(result, 0, header_value);
    CAMLreturn(result);
  }

  CAMLreturn(Val_int(0));
}

static value halo_copy_ocaml_header_values_filtered(const struct halo_request* request,
                                                    const char* name,
                                                    size_t name_len,
                                                    int strip_hop_by_hop) {
  CAMLparam0();
  CAMLlocal3(list, pair, header_value);
  size_t i;

  list = Val_emptylist;
  for (i = request->header_count; i > 0; i--) {
    const struct halo_header* header;

    header = &request->headers[i - 1];
    if (strip_hop_by_hop && halo_header_is_hop_by_hop(header))
      continue;
    if (!halo_header_name_matches(header, name, name_len))
      continue;

    header_value = halo_copy_ocaml_string(header->value, header->value_len);
    pair = caml_alloc(2, 0);
    Store_field(pair, 0, header_value);
    Store_field(pair, 1, list);
    list = pair;
  }

  CAMLreturn(list);
}

static value halo_fold_ocaml_headers_filtered(const struct halo_request* request,
                                              value init,
                                              value callback,
                                              int strip_hop_by_hop) {
  CAMLparam2(init, callback);
  CAMLlocal3(acc, header_name, header_value);
  size_t i;

  acc = init;
  for (i = 0; i < request->header_count; i++) {
    const struct halo_header* header;
    value next_acc;

    header = &request->headers[i];
    if (strip_hop_by_hop && halo_header_is_hop_by_hop(header))
      continue;

    header_name = halo_copy_ocaml_string(header->name, header->name_len);
    header_value = halo_copy_ocaml_string(header->value, header->value_len);
    next_acc = caml_callback3_exn(callback, acc, header_name, header_value);
    if (Is_exception_result(next_acc))
      caml_raise(Extract_exception(next_acc));
    acc = next_acc;
  }

  CAMLreturn(acc);
}

static int halo_request_has_raw_header_named(const struct halo_request* request,
                                             const char* name,
                                             size_t name_len) {
  size_t i;

  for (i = 0; i < request->header_count; i++) {
    if (halo_header_name_matches(&request->headers[i], name, name_len))
      return 1;
  }

  return 0;
}

CAMLprim value ocaml_halo_request_method_text(value request) {
  CAMLparam1(request);
  struct halo_borrowed_request* borrowed;

  borrowed = halo_borrowed_request_get(request);
  CAMLreturn(halo_copy_ocaml_string(borrowed->request->method_text,
                                    borrowed->request->method_len));
}

CAMLprim value ocaml_halo_request_target(value request) {
  CAMLparam1(request);
  struct halo_borrowed_request* borrowed;

  borrowed = halo_borrowed_request_get(request);
  CAMLreturn(halo_copy_ocaml_string(borrowed->request->url,
                                    borrowed->request->url_len));
}

CAMLprim value ocaml_halo_request_body(value request) {
  CAMLparam1(request);
  struct halo_borrowed_request* borrowed;

  borrowed = halo_borrowed_request_get(request);
  CAMLreturn(halo_copy_ocaml_string(borrowed->request->body,
                                    borrowed->request->body_len));
}

CAMLprim value ocaml_halo_request_header(value request, value name) {
  CAMLparam2(request, name);
  struct halo_borrowed_request* borrowed;

  borrowed = halo_borrowed_request_get(request);
  CAMLreturn(halo_find_ocaml_header_filtered(borrowed->request,
                                             String_val(name),
                                             caml_string_length(name),
                                             1));
}

CAMLprim value ocaml_halo_request_headers(value request, value name) {
  CAMLparam2(request, name);
  struct halo_borrowed_request* borrowed;

  borrowed = halo_borrowed_request_get(request);
  CAMLreturn(halo_copy_ocaml_header_values_filtered(borrowed->request,
                                                    String_val(name),
                                                    caml_string_length(name),
                                                    1));
}

CAMLprim value ocaml_halo_request_fold_headers(value request,
                                               value init,
                                               value callback) {
  CAMLparam3(request, init, callback);
  struct halo_borrowed_request* borrowed;

  borrowed = halo_borrowed_request_get(request);
  CAMLreturn(halo_fold_ocaml_headers_filtered(borrowed->request,
                                              init,
                                              callback,
                                              1));
}

CAMLprim value ocaml_halo_request_has_raw_header(value request, value name) {
  CAMLparam2(request, name);
  struct halo_borrowed_request* borrowed;

  borrowed = halo_borrowed_request_get(request);
  CAMLreturn(Val_bool(halo_request_has_raw_header_named(borrowed->request,
                                                        String_val(name),
                                                        caml_string_length(name))));
}

CAMLprim value ocaml_halo_request_copy_headers(value request) {
  CAMLparam1(request);
  struct halo_borrowed_request* borrowed;

  borrowed = halo_borrowed_request_get(request);
  CAMLreturn(halo_copy_ocaml_headers(borrowed->request));
}

CAMLprim value ocaml_halo_request_copy_raw_headers(value request) {
  CAMLparam1(request);
  struct halo_borrowed_request* borrowed;

  borrowed = halo_borrowed_request_get(request);
  CAMLreturn(halo_copy_ocaml_raw_headers(borrowed->request));
}
