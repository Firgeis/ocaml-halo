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

  if (next_cap > SIZE_MAX / sizeof(*headers))
    return -1;

  headers = realloc(request->headers, next_cap * sizeof(*headers));
  if (headers == NULL)
    return -1;

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

  if (next_cap > SIZE_MAX / sizeof(*options))
    return -1;

  options = realloc(request->connection_options, next_cap * sizeof(*options));
  if (options == NULL)
    return -1;

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
  request->headers[request->header_count].name_borrowed = 0;
  request->headers[request->header_count].value_borrowed = 0;
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
  request->request_line_bytes = 0;
  request->header_bytes = 0;
  request->method_len = 0;
  request->url_len = 0;
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
  request->header_count = 0;
  request->connection_option_count = 0;
  request->known_headers.host_count = 0;
  request->known_headers.host_value = NULL;
  request->known_headers.host_value_len = 0;
  request->known_headers.expect_count = 0;
  request->known_headers.expectation = HALO_EXPECTATION_NONE;

}

void halo_request_free(struct halo_request* request) {
  if (request == NULL)
    return;

  halo_request_clear(request);
  free(request->headers);
  free(request->connection_options);
  halo_request_storage_destroy(&request->storage);
  memset(request, 0, sizeof(*request));
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
