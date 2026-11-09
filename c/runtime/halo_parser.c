#include "halo_bridge.h"
#include "halo_runtime_internal.h"
#include "halo_utils.h"

static llhttp_settings_t halo_http_parser_settings;
static uv_once_t halo_http_parser_settings_once = UV_ONCE_INIT;

static struct halo_client* halo_parser_client(llhttp_t* parser) {
  return (struct halo_client*) parser->data;
}

static int halo_parser_fail_status(llhttp_t* parser, const char* reason, int status) {
  struct halo_client* client;

  client = halo_parser_client(parser);
  client->direct_response_status = status;
  llhttp_set_error_reason(parser, reason);
  return HPE_USER;
}

static int halo_parser_fail(llhttp_t* parser, const char* reason) {
  return halo_parser_fail_status(parser, reason, 400);
}

static int halo_parser_content_too_large(llhttp_t* parser, const char* reason) {
  return halo_parser_fail_status(parser, reason, 413);
}

static int halo_parser_uri_too_long(llhttp_t* parser, const char* reason) {
  return halo_parser_fail_status(parser, reason, 414);
}

static int halo_parser_request_header_fields_too_large(llhttp_t* parser,
                                                       const char* reason) {
  return halo_parser_fail_status(parser, reason, 431);
}

static void halo_trim_ows(const char* value,
                          size_t value_len,
                          const char** trimmed,
                          size_t* trimmed_len) {
  size_t start;
  size_t end;

  start = 0;
  end = value_len;

  while (start < end && halo_is_ows_char(value[start]))
    start++;
  while (end > start && halo_is_ows_char(value[end - 1]))
    end--;

  *trimmed = value + start;
  *trimmed_len = end - start;
}

static int halo_is_alpha(char ch) {
  return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');
}

static int halo_is_digit(char ch) {
  return ch >= '0' && ch <= '9';
}

static int halo_is_hexdig(char ch) {
  return halo_is_digit(ch)
    || (ch >= 'a' && ch <= 'f')
    || (ch >= 'A' && ch <= 'F');
}

static int halo_is_unreserved(char ch) {
  return halo_is_alpha(ch) || halo_is_digit(ch)
    || ch == '-' || ch == '.' || ch == '_' || ch == '~';
}

static int halo_is_sub_delim(char ch) {
  switch (ch) {
    case '!':
    case '$':
    case '&':
    case '\'':
    case '(':
    case ')':
    case '*':
    case '+':
    case ',':
    case ';':
    case '=':
      return 1;
    default:
      return 0;
  }
}

static int halo_is_tchar(char ch) {
  return halo_is_alpha(ch)
    || halo_is_digit(ch)
    || ch == '!' || ch == '#' || ch == '$' || ch == '%'
    || ch == '&' || ch == '\'' || ch == '*' || ch == '+'
    || ch == '-' || ch == '.' || ch == '^' || ch == '_'
    || ch == '`' || ch == '|' || ch == '~';
}

static int halo_validate_connection_token(const char* value, size_t value_len) {
  size_t i;

  if (value_len == 0)
    return 0;

  for (i = 0; i < value_len; i++) {
    if (!halo_is_tchar(value[i]))
      return 0;
  }

  return 1;
}

struct halo_connection_option_parse_state {
  struct halo_request* request;
  int invalid;
};

struct halo_expectation_parse_state {
  int saw_continue;
  int unsupported;
};

static int halo_connection_option_builder(const char* token,
                                          size_t token_len,
                                          void* context) {
  struct halo_connection_option_parse_state* state;

  state = (struct halo_connection_option_parse_state*) context;
  if (!halo_validate_connection_token(token, token_len)) {
    state->invalid = 1;
    return 0;
  }

  if (halo_request_store_connection_option(state->request, token, token_len) != 0) {
    return 0;
  }

  return 1;
}

static int halo_expectation_token_parser(const char* token, size_t token_len, void* context) {
  struct halo_expectation_parse_state* state;

  state = (struct halo_expectation_parse_state*) context;
  if (token_len != 12 || strncasecmp(token, "100-continue", 12) != 0) {
    state->unsupported = 1;
    return 0;
  }

  state->saw_continue = 1;
  return 1;
}

static int halo_validate_port(const char* value, size_t value_len) {
  size_t i;
  unsigned int port;

  if (value_len == 0 || value_len > 5)
    return 0;

  port = 0;
  for (i = 0; i < value_len; i++) {
    if (!halo_is_digit(value[i]))
      return 0;
    port = port * 10U + (unsigned int) (value[i] - '0');
    if (port > 65535U)
      return 0;
  }

  return 1;
}

static int halo_validate_ipvfuture(const char* value, size_t value_len) {
  size_t i;

  if (value_len < 4 || (value[0] != 'v' && value[0] != 'V'))
    return 0;

  i = 1;
  while (i < value_len && halo_is_hexdig(value[i]))
    i++;
  if (i == 1 || i >= value_len || value[i] != '.')
    return 0;
  i++;
  if (i >= value_len)
    return 0;

  for (; i < value_len; i++) {
    char ch;

    ch = value[i];
    if (!halo_is_unreserved(ch) && !halo_is_sub_delim(ch) && ch != ':')
      return 0;
  }

  return 1;
}

static int halo_validate_reg_name(const char* value, size_t value_len) {
  size_t i;

  if (value_len == 0)
    return 0;

  for (i = 0; i < value_len; i++) {
    char ch;

    ch = value[i];
    if (halo_is_unreserved(ch) || halo_is_sub_delim(ch))
      continue;
    if (ch == '%') {
      if (i + 2 >= value_len
          || !halo_is_hexdig(value[i + 1])
          || !halo_is_hexdig(value[i + 2]))
        return 0;
      i += 2;
      continue;
    }
    return 0;
  }

  return 1;
}

static int halo_validate_host_literal(const char* value, size_t value_len) {
  char buffer[INET6_ADDRSTRLEN];
  struct in6_addr ipv6_addr;

  if (value_len == 0 || value_len >= sizeof(buffer))
    return 0;

  memcpy(buffer, value, value_len);
  buffer[value_len] = '\0';

  return uv_inet_pton(AF_INET6, buffer, &ipv6_addr) == 0
    || halo_validate_ipvfuture(value, value_len);
}

static int halo_validate_host_value(const char* value, size_t value_len) {
  const char* host;
  size_t host_len;
  const char* port;
  size_t port_len;
  size_t i;
  char buffer[INET_ADDRSTRLEN];
  struct in_addr ipv4_addr;
  struct in6_addr ipv6_addr;

  halo_trim_ows(value, value_len, &host, &host_len);
  if (host_len == 0)
    return 0;

  for (i = 0; i < host_len; i++) {
    unsigned char ch;

    ch = (unsigned char) host[i];
    if (ch <= 0x20 || ch == 0x7f)
      return 0;
    if (host[i] == '/' || host[i] == '?' || host[i] == '#' || host[i] == '@')
      return 0;
  }

  port = NULL;
  port_len = 0;

  if (host[0] == '[') {
    const char* closing;

    closing = memchr(host, ']', host_len);
    if (closing == NULL)
      return 0;
    if ((size_t) (closing - host) <= 1)
      return 0;
    if ((size_t) (closing - host) + 1 < host_len) {
      if (closing[1] != ':')
        return 0;
      port = closing + 2;
      port_len = host_len - ((size_t) (closing - host) + 2);
      if (!halo_validate_port(port, port_len))
        return 0;
    }
    return halo_validate_host_literal(host + 1, (size_t) (closing - host - 1));
  }

  if (memchr(host, ':', host_len) != NULL) {
    const char* colon;

    colon = host + host_len;
    while (colon > host) {
      colon--;
      if (*colon == ':')
        break;
    }
    if (colon <= host && *colon != ':')
      return 0;
    if (memchr(host, ':', (size_t) (colon - host)) != NULL)
      return 0;
    port = colon + 1;
    port_len = host_len - ((size_t) (colon - host) + 1);
    if (!halo_validate_port(port, port_len))
      return 0;
    host_len = (size_t) (colon - host);
    if (host_len == 0)
      return 0;
  }

  if (host_len < sizeof(buffer)) {
    memcpy(buffer, host, host_len);
    buffer[host_len] = '\0';
    if (uv_inet_pton(AF_INET, buffer, &ipv4_addr) == 0)
      return 1;
  }

  if (uv_inet_pton(AF_INET6, host, &ipv6_addr) == 0)
    return 0;

  return halo_validate_reg_name(host, host_len);
}

static int halo_validate_http11_host(llhttp_t* parser, struct halo_request* request) {
  if (request->known_headers.host_count == 0)
    return halo_parser_fail(parser, "HTTP/1.1 requires exactly one Host header");
  if (request->known_headers.host_count != 1)
    return halo_parser_fail(parser, "HTTP/1.1 requires exactly one Host header");
  if (!halo_validate_host_value(request->known_headers.host_value,
                                request->known_headers.host_value_len))
    return halo_parser_fail(parser, "Host header is not a valid authority");

  return 0;
}

static int halo_validate_connect_authority_target(const char* value, size_t value_len) {
  const char* target;
  size_t target_len;
  const char* colon;

  halo_trim_ows(value, value_len, &target, &target_len);
  if (target_len == 0 || !halo_validate_host_value(target, target_len))
    return 0;

  if (target[0] == '[') {
    const char* closing;

    closing = memchr(target, ']', target_len);
    if (closing == NULL || closing + 1 >= target + target_len || closing[1] != ':')
      return 0;
    return halo_validate_port(closing + 2,
                              target_len - ((size_t) (closing - target) + 2));
  }

  colon = target + target_len;
  while (colon > target) {
    colon--;
    if (*colon == ':')
      break;
  }

  if (*colon != ':' || colon == target)
    return 0;

  if (memchr(target, ':', (size_t) (colon - target)) != NULL)
    return 0;

  return halo_validate_port(colon + 1,
                            target_len - ((size_t) (colon - target) + 1));
}

static int halo_validate_request_target(llhttp_t* parser,
                                        struct halo_request* request) {
  if (request->method == HTTP_OPTIONS
      && request->url_len == 1
      && request->url[0] == '*')
    return 0;

  if (request->method == HTTP_CONNECT) {
    if (!halo_validate_connect_authority_target(request->url, request->url_len))
      return halo_parser_fail(parser, "CONNECT request target must be authority-form");
    return 0;
  }

  if (request->url[0] != '/')
    return halo_parser_fail(parser, "request target must be origin-form");

  return 0;
}

static enum halo_expectation_state halo_parse_expectation_header(const char* value,
                                                                 size_t value_len) {
  struct halo_expectation_parse_state state;
  int saw_token;

  state.saw_continue = 0;
  state.unsupported = 0;
  saw_token = 0;

  if (!halo_iterate_comma_separated_header_tokens(value,
                                                  value_len,
                                                  halo_expectation_token_parser,
                                                  &state,
                                                  &saw_token)
      && state.unsupported) {
    return HALO_EXPECTATION_UNSUPPORTED;
  }

  if (state.unsupported)
    return HALO_EXPECTATION_UNSUPPORTED;
  if (!saw_token || !state.saw_continue)
    return HALO_EXPECTATION_NONE;

  return HALO_EXPECTATION_100_CONTINUE;
}

static int halo_request_expects_100_continue(llhttp_t* parser,
                                             struct halo_request* request,
                                             int http_major,
                                             int http_minor) {
  if (request->known_headers.expect_count > 1)
    return halo_parser_fail(parser, "multiple Expect headers are not supported");

  if (request->known_headers.expectation == HALO_EXPECTATION_NONE)
    return 0;

  if (request->known_headers.expectation == HALO_EXPECTATION_UNSUPPORTED)
    return halo_parser_fail_status(parser, "unsupported expectation", 417);

  if (http_major != 1 || http_minor != 1)
    return halo_parser_fail_status(parser, "100-continue requires HTTP/1.1", 417);

  return 1;
}

static int halo_request_storage_append(struct halo_request* request,
                                       char** buf,
                                       size_t* len,
                                       size_t* cap,
                                       size_t limit,
                                       const char* data,
                                       size_t data_len) {
  char* next_buf;
  size_t next_cap;

  if (data_len == 0)
    return 0;

  if (*len > limit || data_len > limit - *len)
    return -1;

  if (*len + data_len + 1 > *cap) {
    next_cap = *cap == 0 ? data_len + 1 : *cap;
    while (next_cap < *len + data_len + 1) {
      if (next_cap >= limit) {
        next_cap = limit + 1;
        break;
      }
      next_cap *= 2;
    }

    if (next_cap < *len + data_len + 1)
      return -1;

    next_buf =
      halo_request_storage_alloc(&request->storage, next_cap, _Alignof(char));
    if (next_buf == NULL)
      return -1;

    if (*len != 0 && *buf != NULL)
      memcpy(next_buf, *buf, *len);
    *buf = next_buf;
    *cap = next_cap;
  }

  memcpy(*buf + *len, data, data_len);
  *len += data_len;
  (*buf)[*len] = '\0';
  return 0;
}

/* Borrow contiguous llhttp spans while the current input buffer is alive. */
static int halo_request_header_span_append(struct halo_request* request,
                                           char** buf,
                                           size_t* len,
                                           size_t* cap,
                                           size_t limit,
                                           const char* data,
                                           size_t data_len) {
  char* copy;
  size_t next_len;

  if (data_len == 0)
    return 0;
  if (*len > limit || data_len > limit - *len)
    return -1;
  if (*buf == NULL) {
    *buf = (char*) data;
    *len = data_len;
    *cap = 0;
    return 0;
  }
  if (*cap == 0 && data == *buf + *len) {
    *len += data_len;
    return 0;
  }
  if (*cap != 0)
    return halo_request_storage_append(request, buf, len, cap, limit,
                                       data, data_len);

  next_len = *len + data_len;
  copy = halo_request_storage_alloc(&request->storage, next_len + 1,
                                    _Alignof(char));
  if (copy == NULL)
    return -1;
  memcpy(copy, *buf, *len);
  memcpy(copy + *len, data, data_len);
  copy[next_len] = '\0';
  *buf = copy;
  *len = next_len;
  *cap = next_len + 1;
  return 0;
}

static int halo_materialize_header_span(struct halo_request* request,
                                        char** data,
                                        size_t length) {
  char* copy;

  copy = halo_request_storage_strndup(&request->storage, *data, length);
  if (copy == NULL)
    return -1;
  *data = copy;
  return 0;
}

/* Save spans whenever the request can outlive the current input buffer. */
int halo_request_materialize_header_spans(struct halo_request* request) {
  size_t index;

  for (index = 0; index < request->header_count; index++) {
    struct halo_header* header;
    char* old_value;

    header = &request->headers[index];
    if (header->name_borrowed) {
      if (halo_materialize_header_span(request, &header->name,
                                       header->name_len) != 0)
        return -1;
      header->name_borrowed = 0;
    }
    if (header->value_borrowed) {
      old_value = header->value;
      if (halo_materialize_header_span(request, &header->value,
                                       header->value_len) != 0)
        return -1;
      if (request->known_headers.host_value == old_value)
        request->known_headers.host_value = header->value;
      header->value_borrowed = 0;
    }
  }
  if (request->current_header_name != NULL
      && request->current_header_name_cap == 0) {
    if (halo_materialize_header_span(request, &request->current_header_name,
                                     request->current_header_name_len) != 0)
      return -1;
    request->current_header_name_cap = request->current_header_name_len + 1;
  }
  if (request->current_header_value != NULL
      && request->current_header_value_cap == 0) {
    if (halo_materialize_header_span(request, &request->current_header_value,
                                     request->current_header_value_len) != 0)
      return -1;
    request->current_header_value_cap = request->current_header_value_len + 1;
  }
  return 0;
}

static int halo_request_charge_bytes(size_t* total,
                                     size_t limit,
                                     size_t amount) {
  if (*total > limit || amount > limit - *total)
    return -1;

  *total += amount;
  return 0;
}

static int halo_header_name_matches_connection_option(
  const struct halo_request* request,
  const struct halo_header* header) {
  size_t i;

  for (i = 0; i < request->connection_option_count; i++) {
    if (header->name_len == request->connection_options[i].name_len
        && strncasecmp(header->name,
                       request->connection_options[i].name,
                       header->name_len) == 0) {
      return 1;
    }
  }

  return 0;
}

static int halo_header_is_fixed_hop_by_hop(const struct halo_header* header) {
  return halo_header_name_matches(header, "connection", 10)
    || halo_header_name_matches(header, "keep-alive", 10)
    || halo_header_name_matches(header, "proxy-authenticate", 18)
    || halo_header_name_matches(header, "proxy-authorization", 19)
    || halo_header_name_matches(header, "te", 2)
    || halo_header_name_matches(header, "trailer", 7)
    || halo_header_name_matches(header, "transfer-encoding", 17)
    || halo_header_name_matches(header, "upgrade", 7);
}

static int halo_request_mark_hop_by_hop_headers(llhttp_t* parser,
                                                struct halo_request* request) {
  size_t i;

  request->connection_option_count = 0;

  for (i = 0; i < request->header_count; i++) {
    struct halo_header* header;
    struct halo_connection_option_parse_state state;
    int saw_token;

    header = &request->headers[i];

    header->is_hop_by_hop = (unsigned char) halo_header_is_fixed_hop_by_hop(header);
    if (!halo_header_name_matches(header, "connection", 10))
      continue;

    state.request = request;
    state.invalid = 0;
    saw_token = 0;
    if (!halo_iterate_comma_separated_header_tokens(header->value,
                                                    header->value_len,
                                                    halo_connection_option_builder,
                                                    &state,
                                                    &saw_token)) {
      if (state.invalid)
        return halo_parser_fail(parser, "Connection header has invalid options");
      return halo_parser_fail(parser, "could not process Connection header options");
    }

    if (!saw_token)
      return halo_parser_fail(parser, "Connection header has invalid options");
  }

  for (i = 0; i < request->header_count; i++) {
    struct halo_header* header;

    header = &request->headers[i];
    if (header->is_hop_by_hop != 0)
      continue;
    if (halo_header_name_matches_connection_option(request, header))
      header->is_hop_by_hop = 1;
  }

  return 0;
}

int halo_request_has_data(const struct halo_request* request) {
  return request->method_len != 0 || request->url_len != 0
    || request->header_count != 0 || request->current_header_name_len != 0
    || request->current_header_value_len != 0;
}

static int halo_current_header_name_matches(const struct halo_request* request,
                                            const char* name,
                                            size_t name_len) {
  return request->current_header_name_len == name_len
    && strncasecmp(request->current_header_name, name, name_len) == 0;
}

static int halo_request_finalize_header(struct halo_request* request) {
  struct halo_header* header;
  char* endptr;
  unsigned long long content_length;
  char* name;
  char* value;
  size_t value_len;
  enum halo_expectation_state expectation;

  if (request->current_header_name_len == 0)
    return -1;

  name = request->current_header_name;
  value = request->current_header_value == NULL ? "" : request->current_header_value;
  value_len = request->current_header_value_len;

  if (halo_current_header_name_matches(request, "content-length", 14)) {
    /* strtoull needs a terminator; ordinary header values remain borrowed. */
    if (request->current_header_value_cap == 0
        && request->current_header_value != NULL) {
      if (halo_materialize_header_span(request,
                                       &request->current_header_value,
                                       value_len) != 0)
        return -1;
      request->current_header_value_cap = value_len + 1;
      value = request->current_header_value;
    }
    content_length = strtoull(value, &endptr, 10);
    if (*value == '\0' || *endptr != '\0') {
      return -1;
    }
    request->has_content_length = 1;
    request->content_length = (uint64_t) content_length;
  }

  if (halo_request_store_header(request,
                                name,
                                request->current_header_name_len,
                                value,
                                value_len,
                                &header) != 0)
    return -1;
  header->name_borrowed = request->current_header_name_cap == 0;
  header->value_borrowed = value_len != 0
    && request->current_header_value_cap == 0;

  if (halo_current_header_name_matches(request, "host", 4)) {
    request->known_headers.host_count++;
    request->known_headers.host_value = header->value;
    request->known_headers.host_value_len = header->value_len;
  }

  if (halo_current_header_name_matches(request, "expect", 6)) {
    request->known_headers.expect_count++;
    expectation = halo_parse_expectation_header(header->value, header->value_len);
    if (expectation == HALO_EXPECTATION_UNSUPPORTED)
      request->known_headers.expectation = HALO_EXPECTATION_UNSUPPORTED;
    else if (expectation == HALO_EXPECTATION_100_CONTINUE
             && request->known_headers.expectation == HALO_EXPECTATION_NONE)
      request->known_headers.expectation = HALO_EXPECTATION_100_CONTINUE;
  }

  request->current_header_name_len = 0;
  request->current_header_name = NULL;
  request->current_header_name_cap = 0;
  request->current_header_value_len = 0;
  request->current_header_value = NULL;
  request->current_header_value_cap = 0;
  return 0;
}

static int halo_on_method(llhttp_t* parser, const char* at, size_t length) {
  struct halo_client* client;
  size_t max_request_line_size;

  client = halo_parser_client(parser);
  max_request_line_size = client->listener_ref->max_request_line_size;
  if (halo_request_charge_bytes(&client->request.request_line_bytes,
                                max_request_line_size,
                                length) != 0)
    return halo_parser_fail(parser, "request line is too large");

  if (halo_request_storage_append(&client->request,
                                  &client->request.method_text,
                                  &client->request.method_len,
                                  &client->request.method_cap,
                                  max_request_line_size,
                                  at,
                                  length) != 0)
    return halo_parser_fail(parser, "request method is too large");
  return 0;
}

static int halo_on_url(llhttp_t* parser, const char* at, size_t length) {
  struct halo_client* client;
  size_t max_request_line_size;

  client = halo_parser_client(parser);
  max_request_line_size = client->listener_ref->max_request_line_size;
  if (halo_request_charge_bytes(&client->request.request_line_bytes,
                                max_request_line_size,
                                length) != 0)
    return halo_parser_uri_too_long(parser, "request line is too large");

  if (halo_request_storage_append(&client->request,
                                  &client->request.url,
                                  &client->request.url_len,
                                  &client->request.url_cap,
                                  max_request_line_size,
                                  at,
                                  length) != 0)
    return halo_parser_uri_too_long(parser, "request target is too large");
  return 0;
}

static int halo_on_header_field(llhttp_t* parser, const char* at, size_t length) {
  struct halo_client* client;
  size_t max_header_size;

  client = halo_parser_client(parser);
  if (parser->flags & F_TRAILING)
    return halo_parser_fail(parser, "request trailers are not supported");
  if (client->request.current_header_value_len != 0)
    return halo_parser_fail(parser, "header field/value callback order is invalid");

  max_header_size = client->listener_ref->max_header_size;
  if (halo_request_charge_bytes(&client->request.header_bytes,
                                max_header_size,
                                length) != 0)
    return halo_parser_request_header_fields_too_large(
      parser, "request headers are too large");

  if (halo_request_header_span_append(&client->request,
                                      &client->request.current_header_name,
                                      &client->request.current_header_name_len,
                                      &client->request.current_header_name_cap,
                                      max_header_size,
                                      at,
                                      length) != 0)
    return halo_parser_request_header_fields_too_large(parser,
                                                       "header field is too large");
  return 0;
}

static int halo_on_header_value(llhttp_t* parser, const char* at, size_t length) {
  struct halo_client* client;
  size_t max_header_size;

  client = halo_parser_client(parser);
  if (parser->flags & F_TRAILING)
    return halo_parser_fail(parser, "request trailers are not supported");
  if (client->request.current_header_name_len == 0)
    return halo_parser_fail(parser, "header value arrived before a header field");

  max_header_size = client->listener_ref->max_header_size;
  if (halo_request_charge_bytes(&client->request.header_bytes,
                                max_header_size,
                                length) != 0)
    return halo_parser_request_header_fields_too_large(
      parser, "request headers are too large");

  if (halo_request_header_span_append(&client->request,
                                      &client->request.current_header_value,
                                      &client->request.current_header_value_len,
                                      &client->request.current_header_value_cap,
                                      max_header_size,
                                      at,
                                      length) != 0)
    return halo_parser_request_header_fields_too_large(parser,
                                                       "header value is too large");
  return 0;
}

static int halo_on_method_complete(llhttp_t* parser) {
  struct halo_client* client;

  client = halo_parser_client(parser);
  client->request.method = (llhttp_method_t) llhttp_get_method(parser);
  client->request.method_seen = 1;
  return 0;
}

static int halo_on_header_value_complete(llhttp_t* parser) {
  struct halo_client* client;

  client = halo_parser_client(parser);
  if (halo_request_finalize_header(&client->request) != 0)
    return halo_parser_fail(parser, "could not finalize request header");
  return 0;
}

static int halo_on_headers_complete(llhttp_t* parser) {
  struct halo_client* client;
  int http_major;
  int http_minor;
  int expects_continue;

  client = halo_parser_client(parser);
  if (llhttp_get_type(parser) != HTTP_REQUEST)
    return halo_parser_fail(parser, "only HTTP requests are supported");

  if (!client->request.method_seen || client->request.method_text == NULL)
    return halo_parser_fail(parser, "request method is missing");

  if (client->request.url == NULL || client->request.url_len == 0)
    return halo_parser_fail(parser, "request URL is missing");

  http_major = llhttp_get_http_major(parser);
  http_minor = llhttp_get_http_minor(parser);
  if (http_major != 1 || (http_minor != 0 && http_minor != 1))
    return halo_parser_fail_status(parser, "HTTP version is not supported", 505);

  if (http_minor == 1
      && halo_validate_http11_host(parser, &client->request) != 0)
    return HPE_USER;

  if (halo_validate_request_target(parser, &client->request) != 0)
    return HPE_USER;

  if (halo_request_mark_hop_by_hop_headers(parser, &client->request) != 0)
    return HPE_USER;

  if (client->request.has_content_length
      && client->request.content_length
           > (uint64_t) client->listener_ref->max_body_size)
    return halo_parser_content_too_large(parser, "request body is too large");

  expects_continue =
    halo_request_expects_100_continue(parser,
                                      &client->request,
                                      http_major,
                                      http_minor);
  if (expects_continue == HPE_USER)
    return HPE_USER;

  client->request.http_major = http_major;
  client->request.http_minor = http_minor;
  client->request.headers_complete = 1;
  client->should_keep_alive = llhttp_should_keep_alive(parser);

  if (expects_continue == 1
      && (client->request.content_length > 0 || (parser->flags & F_CHUNKED) != 0)) {
    client->dispatch_after_headers = 1;
    client->pending_continue = 1;
    return HPE_PAUSED;
  }

  if (client->request.content_length > 0 || (parser->flags & F_CHUNKED) != 0) {
    client->dispatch_after_headers = 1;
    return HPE_PAUSED;
  }

  return 0;
}

static int halo_on_body(llhttp_t* parser, const char* at, size_t length) {
  struct halo_client* client;
  size_t max_body_size;

  client = halo_parser_client(parser);
  max_body_size = client->listener_ref->max_body_size;
  if (client->body_bytes_read > max_body_size
      || length > max_body_size - client->body_bytes_read)
    return halo_parser_content_too_large(parser, "request body is too large");

  client->body_bytes_read += length;
  if (client->awaiting_request_body) {
    if (halo_bridge_buffer_body_chunk(client, at, length) != 0)
      return halo_parser_fail(parser, "could not deliver request body");
    client->body_chunk_delivered = 1;
  }
  return 0;
}

static int halo_on_message_complete(llhttp_t* parser) {
  struct halo_client* client;
  struct halo_runtime* runtime;

  client = halo_parser_client(parser);
  runtime = (struct halo_runtime*) client->handle.loop->data;
  client->message_complete = 1;
  client->should_keep_alive = runtime != NULL
    && !runtime->is_draining
    && !runtime->is_stopping
    && llhttp_should_keep_alive(parser);
  return HPE_PAUSED;
}

static void halo_http_settings_init(void) {
  llhttp_settings_init(&halo_http_parser_settings);
  halo_http_parser_settings.on_method = halo_on_method;
  halo_http_parser_settings.on_url = halo_on_url;
  halo_http_parser_settings.on_header_field = halo_on_header_field;
  halo_http_parser_settings.on_header_value = halo_on_header_value;
  halo_http_parser_settings.on_method_complete = halo_on_method_complete;
  halo_http_parser_settings.on_header_value_complete = halo_on_header_value_complete;
  halo_http_parser_settings.on_headers_complete = halo_on_headers_complete;
  halo_http_parser_settings.on_body = halo_on_body;
  halo_http_parser_settings.on_message_complete = halo_on_message_complete;
}

const llhttp_settings_t* halo_http_settings(void) {
  uv_once(&halo_http_parser_settings_once, halo_http_settings_init);
  return &halo_http_parser_settings;
}
