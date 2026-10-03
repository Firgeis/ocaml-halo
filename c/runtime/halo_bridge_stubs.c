#include "halo_bridge_internal.h"

#include <unistd.h>

#include <caml/socketaddr.h>
#include <caml/threads.h>

static value halo_bridge_alloc_string(const char* data, size_t length) {
  return caml_alloc_initialized_string(length, data == NULL ? "" : data);
}

static value halo_bridge_alloc_string_option(const unsigned char* data,
                                             size_t length) {
  CAMLparam0();
  CAMLlocal2(string, option);

  if (length == 0)
    CAMLreturn(Val_none);
  string = halo_bridge_alloc_string((const char*) data, length);
  option = caml_alloc(1, 0);
  Store_field(option, 0, string);
  CAMLreturn(option);
}

static value halo_bridge_alloc_sockaddr(const unsigned char* data,
                                        size_t length) {
  CAMLparam0();
  CAMLlocal1(address);
  union sock_addr_union storage;

  if (length > sizeof(storage))
    caml_invalid_argument("invalid bridge socket address");
  memset(&storage, 0, sizeof(storage));
  memcpy(&storage, data, length);
  address = caml_unix_alloc_sockaddr(&storage,
                                     (socklen_param_type) length,
                                     -1);
  CAMLreturn(address);
}

static value halo_bridge_alloc_tls_protocol_version(uint8_t version) {
  CAMLparam0();
  CAMLlocal1(result);

  switch (version) {
    case S2N_SSLv2:
      CAMLreturn(Val_int(0));
    case S2N_SSLv3:
      CAMLreturn(Val_int(1));
    case S2N_TLS10:
      CAMLreturn(Val_int(2));
    case S2N_TLS11:
      CAMLreturn(Val_int(3));
    case S2N_TLS12:
      CAMLreturn(Val_int(4));
    case S2N_TLS13:
      CAMLreturn(Val_int(5));
    default:
      result = caml_alloc(1, 0);
      Store_field(result, 0, Val_int(version));
      CAMLreturn(result);
  }
}

CAMLprim value ocaml_halo_bridge_enable(value runtime_value,
                                        value event_capacity,
                                        value command_capacity,
                                        value telemetry_enabled) {
  CAMLparam4(runtime_value, event_capacity, command_capacity,
             telemetry_enabled);
  struct halo_runtime* runtime;
  int code;

  runtime = halo_runtime_get(runtime_value);
  code = halo_bridge_enable(runtime,
                            (size_t) Int_val(event_capacity),
                            (size_t) Int_val(command_capacity),
                            Bool_val(telemetry_enabled));
  if (code != 0)
    CAMLreturn(halo_result_error_uv("halo_bridge_enable", code));
  CAMLreturn(halo_result_ok(Val_unit));
}

CAMLprim value ocaml_halo_bridge_event_fd(value runtime_value) {
  CAMLparam1(runtime_value);
  struct halo_runtime* runtime;

  runtime = halo_runtime_get(runtime_value);
  if (runtime->bridge == NULL)
    caml_invalid_argument("reactor bridge is not enabled");
  CAMLreturn(Val_int(runtime->bridge->event_pipe[0]));
}

CAMLprim value ocaml_halo_bridge_drain_wakeup(value runtime_value) {
  CAMLparam1(runtime_value);
  struct halo_runtime* runtime;
  unsigned char buffer[256];
  ssize_t result;

  runtime = halo_runtime_get(runtime_value);
  if (runtime->bridge == NULL)
    caml_invalid_argument("reactor bridge is not enabled");
  do {
    result = read(runtime->bridge->event_pipe[0], buffer, sizeof(buffer));
  } while (result > 0 || (result < 0 && errno == EINTR));
  CAMLreturn(Val_unit);
}

/* Private header encoding: bytes followed by two native size_t lengths and
 * one hop-by-hop flag. Trailing descriptors allow decoding in reverse order
 * without allocating an index. No pointer into C storage reaches OCaml. */
#define HALO_PACKED_HEADER_METADATA_SIZE (2 * sizeof(size_t) + 1)
_Static_assert(HALO_PACKED_HEADER_METADATA_SIZE
                 <= sizeof(struct halo_bridge_header),
               "packed headers must fit the validated event allocation size");

static value halo_bridge_alloc_headers(
    const struct halo_bridge_request_data* request) {
  CAMLparam0();
  CAMLlocal2(packed, snapshot);
  size_t length = 0;
  size_t index;
  unsigned char* cursor;

  for (index = 0; index < request->header_count; index++) {
    const struct halo_bridge_header* header = &request->headers[index];
    length += header->name_len + header->value_len
      + HALO_PACKED_HEADER_METADATA_SIZE;
  }
  packed = caml_alloc_string(length);
  cursor = Bytes_val(packed);
  for (index = 0; index < request->header_count; index++) {
    const struct halo_bridge_header* header = &request->headers[index];
    if (header->name_len > 0)
      memcpy(cursor, header->name, header->name_len);
    cursor += header->name_len;
    if (header->value_len > 0)
      memcpy(cursor, header->value, header->value_len);
    cursor += header->value_len;
    memcpy(cursor, &header->name_len, sizeof(size_t));
    cursor += sizeof(size_t);
    memcpy(cursor, &header->value_len, sizeof(size_t));
    cursor += sizeof(size_t);
    *cursor++ = (unsigned char) header->is_hop_by_hop;
  }
  /* Keep in sync with Request_headers.t. */
  snapshot = caml_alloc(2, 0);
  Store_field(snapshot, 0, packed);
  Store_field(snapshot, 1, Val_none);
  CAMLreturn(snapshot);
}

CAMLprim value ocaml_halo_bridge_decode_headers(value packed) {
  CAMLparam1(packed);
  CAMLlocal5(headers, raw_headers, name, header_value, pair);
  CAMLlocal2(cell, result);
  size_t offset = caml_string_length(packed);

  headers = Val_emptylist;
  raw_headers = Val_emptylist;
  while (offset > 0) {
    size_t name_len;
    size_t value_len;
    unsigned char is_hop_by_hop;

    if (offset < HALO_PACKED_HEADER_METADATA_SIZE)
      caml_invalid_argument("invalid packed bridge headers");
    offset -= HALO_PACKED_HEADER_METADATA_SIZE;
    memcpy(&name_len, String_val(packed) + offset, sizeof(size_t));
    memcpy(&value_len, String_val(packed) + offset + sizeof(size_t),
           sizeof(size_t));
    is_hop_by_hop =
      (unsigned char) String_val(packed)[offset + 2 * sizeof(size_t)];
    if (value_len > offset || name_len > offset - value_len)
      caml_invalid_argument("invalid packed bridge header lengths");
    offset -= name_len + value_len;
    /* Allocate before deriving pointers: a GC may move [packed]. */
    name = caml_alloc_string(name_len);
    memcpy(Bytes_val(name), String_val(packed) + offset, name_len);
    header_value = caml_alloc_string(value_len);
    memcpy(Bytes_val(header_value), String_val(packed) + offset + name_len,
           value_len);
    pair = caml_alloc_tuple(2);
    Store_field(pair, 0, name);
    Store_field(pair, 1, header_value);
    cell = caml_alloc(2, 0);
    Store_field(cell, 0, pair);
    Store_field(cell, 1, raw_headers);
    raw_headers = cell;
    if (!is_hop_by_hop) {
      cell = caml_alloc(2, 0);
      Store_field(cell, 0, pair);
      Store_field(cell, 1, headers);
      headers = cell;
    }
  }
  result = caml_alloc_tuple(2);
  Store_field(result, 0, headers);
  Store_field(result, 1, raw_headers);
  CAMLreturn(result);
}

static value halo_bridge_alloc_connection(
    const struct halo_bridge_transport* transport) {
  CAMLparam0();
  CAMLlocal5(option, connection, tls_option, tls, remote_address);
  CAMLlocal5(local_address, server_name, application_protocol, cipher,
             protocol_version);
  const unsigned char* cursor;

  if (transport == NULL)
    CAMLreturn(Val_none);
  cursor = transport->storage;
  remote_address = halo_bridge_alloc_sockaddr(
    cursor, transport->remote_address_len);
  cursor += transport->remote_address_len;
  local_address = halo_bridge_alloc_sockaddr(cursor, transport->local_address_len);
  cursor += transport->local_address_len;
  tls_option = Val_none;
  if (transport->tls_protocol_version != 0) {
    server_name = halo_bridge_alloc_string_option(cursor, transport->server_name_len);
    cursor += transport->server_name_len;
    application_protocol =
      halo_bridge_alloc_string_option(cursor, transport->application_protocol_len);
    cursor += transport->application_protocol_len;
    cipher = halo_bridge_alloc_string_option(cursor, transport->cipher_len);
    protocol_version =
      halo_bridge_alloc_tls_protocol_version(transport->tls_protocol_version);
    tls = caml_alloc(4, 0);
    Store_field(tls, 0, server_name);
    Store_field(tls, 1, application_protocol);
    Store_field(tls, 2, protocol_version);
    Store_field(tls, 3, cipher);
    tls_option = caml_alloc(1, 0);
    Store_field(tls_option, 0, tls);
  }
  connection = caml_alloc(3, 0);
  Store_field(connection, 0, remote_address);
  Store_field(connection, 1, local_address);
  Store_field(connection, 2, tls_option);
  option = caml_alloc(1, 0);
  Store_field(option, 0, connection);
  CAMLreturn(option);
}

static value halo_bridge_alloc_int64_option(uint64_t number) {
  CAMLparam0();
  CAMLlocal2(boxed, option);

  if (number == 0)
    CAMLreturn(Val_none);
  boxed = caml_copy_int64((int64_t) number);
  option = caml_alloc(1, 0);
  Store_field(option, 0, boxed);
  CAMLreturn(option);
}

/* [fields] belongs to the caller's CAMLlocalN roots and survives allocation. */
static value halo_bridge_alloc_record(mlsize_t size, value* fields) {
  value record = caml_alloc(size, 0);
  mlsize_t index;

  for (index = 0; index < size; index++)
    Store_field(record, index, fields[index]);
  return record;
}

static uint64_t halo_bridge_decode_uint64(const char* source) {
  uint64_t number = 0;
  size_t index;

  for (index = 0; index < 8; index++)
    number = (number << 8) | (unsigned char) source[index];
  return number;
}

CAMLprim value ocaml_halo_bridge_poll_event(value runtime_value) {
  CAMLparam1(runtime_value);
  CAMLlocal3(option, variant, record);
  CAMLlocalN(fields, 15);
  struct halo_runtime* runtime;
  struct halo_bridge_event* event;
  const struct halo_bridge_request_data* request;
  int tag;

  runtime = halo_runtime_get(runtime_value);
  if (runtime->bridge == NULL)
    caml_invalid_argument("reactor bridge is not enabled");
  event = halo_bridge_pop_event(runtime->bridge);
  if (event == NULL)
    CAMLreturn(Val_none);

  /* Constructor tags and field order match Reactor.event and its records.
   * Constant constructors have separate numbering from payload constructors. */
  tag = -1;
  switch (event->kind) {
    case HALO_BRIDGE_REQUEST:
      request = event->request;
      fields[0] = caml_copy_int64((int64_t) event->request_id);
      fields[1] = caml_copy_int64((int64_t) event->listener_id);
      fields[2] = halo_bridge_alloc_string(request->method_text, request->method_len);
      fields[3] = halo_bridge_alloc_string(request->target, request->target_len);
      fields[4] = halo_bridge_alloc_headers(request);
      fields[5] = Val_int(request->http_major);
      fields[6] = Val_int(request->http_minor);
      fields[7] = Val_bool(request->keep_alive);
      fields[8] = Val_bool(request->body_expected);
      fields[9] = Val_bool(request->is_early_data);
      fields[10] = caml_copy_int64((int64_t) event->auxiliary);
      fields[11] = halo_bridge_alloc_connection(request->transport);
      fields[12] = caml_copy_int64((int64_t) event->monotonic_time_ns);
      fields[13] = halo_bridge_alloc_int64_option(event->unix_time_ns);
      record = halo_bridge_alloc_record(14, fields);
      tag = 0;
      break;
    case HALO_BRIDGE_BODY_CHUNK:
    case HALO_BRIDGE_METRICS:
      fields[0] = caml_copy_int64((int64_t) event->request_id);
      fields[1] = halo_bridge_alloc_string(event->payload, event->payload_len);
      record = halo_bridge_alloc_record(2, fields);
      tag = event->kind == HALO_BRIDGE_BODY_CHUNK ? 1 : 6;
      break;
    case HALO_BRIDGE_BODY_END:
    case HALO_BRIDGE_WRITE_COMPLETE:
    case HALO_BRIDGE_CONNECTION_CLOSED:
      record = caml_copy_int64((int64_t) event->request_id);
      tag = event->kind == HALO_BRIDGE_BODY_END ? 2
        : event->kind == HALO_BRIDGE_WRITE_COMPLETE ? 5 : 8;
      break;
    case HALO_BRIDGE_RESPONSE_COMPLETE:
    case HALO_BRIDGE_DISCONNECTED:
      fields[0] = caml_copy_int64((int64_t) event->request_id);
      fields[1] = caml_copy_int64((int64_t) event->monotonic_time_ns);
      fields[2] = caml_copy_int64((int64_t) event->auxiliary);
      record = halo_bridge_alloc_record(3, fields);
      tag = event->kind == HALO_BRIDGE_RESPONSE_COMPLETE ? 3 : 4;
      break;
    case HALO_BRIDGE_STOPPED:
      variant = Val_int(0);
      break;
    case HALO_BRIDGE_DRAINED:
      variant = Val_int(1);
      break;
    case HALO_BRIDGE_REPLAY_CLAIM:
      fields[0] = caml_copy_int64((int64_t) event->request_id);
      fields[1] = caml_copy_int64((int64_t) event->listener_id);
      fields[2] = halo_bridge_alloc_string(event->payload, event->payload_len);
      fields[3] = caml_copy_int64((int64_t) event->auxiliary);
      record = halo_bridge_alloc_record(4, fields);
      tag = 7;
      break;
    case HALO_BRIDGE_PRE_DISPATCH_RESPONSE: {
      uint64_t status_code;
      uint64_t request_body_size;
      uint64_t response_body_size;

      if (event->payload_len != 25)
        goto unknown;
      status_code = halo_bridge_decode_uint64(event->payload);
      request_body_size = halo_bridge_decode_uint64(event->payload + 8);
      response_body_size = halo_bridge_decode_uint64(event->payload + 16);
      if (status_code < 100 || status_code > 599
          || request_body_size > INT64_MAX || response_body_size > INT64_MAX
          || (event->payload[24] != 0 && event->payload[24] != 1))
        goto unknown;
      request = event->request;
      fields[0] = caml_copy_int64((int64_t) event->request_id);
      fields[1] = halo_bridge_alloc_string(request->method_text, request->method_len);
      fields[2] = halo_bridge_alloc_string(request->target, request->target_len);
      fields[3] = halo_bridge_alloc_headers(request);
      fields[4] = Val_int(request->http_major);
      fields[5] = Val_int(request->http_minor);
      fields[6] = Val_bool(request->is_early_data);
      fields[7] = halo_bridge_alloc_connection(request->transport);
      fields[8] = caml_copy_int64((int64_t) event->monotonic_time_ns);
      fields[9] = halo_bridge_alloc_int64_option(event->unix_time_ns);
      fields[10] = caml_copy_int64((int64_t) event->auxiliary);
      fields[11] = Val_int(status_code);
      fields[12] = caml_copy_int64((int64_t) request_body_size);
      fields[13] = caml_copy_int64((int64_t) response_body_size);
      fields[14] = Val_bool(event->payload[24]);
      record = halo_bridge_alloc_record(15, fields);
      tag = 9;
      break;
    }
    default:
    unknown:
      fields[0] = Val_int(event->kind);
      fields[1] = caml_copy_int64((int64_t) event->request_id);
      fields[2] = halo_bridge_alloc_string(event->payload, event->payload_len);
      record = halo_bridge_alloc_record(3, fields);
      tag = 10;
      break;
  }
  if (tag >= 0) {
    variant = caml_alloc(1, tag);
    Store_field(variant, 0, record);
  }
  option = caml_alloc(1, 0);
  Store_field(option, 0, variant);
  free(event);
  CAMLreturn(option);
}

static value halo_bridge_enqueue_command_stub(
  value runtime_value,
  value request_id_value,
  enum halo_bridge_command_kind kind,
  const char* payload,
  size_t payload_len
) {
  struct halo_runtime* runtime;
  int result;

  runtime = halo_runtime_get(runtime_value);
  if (runtime->bridge == NULL)
    caml_invalid_argument("reactor bridge is not enabled");
  result = halo_bridge_enqueue_command(runtime->bridge,
                                       kind,
                                       (uint64_t) Int64_val(request_id_value),
                                       payload,
                                       payload_len);
  if (result == UV_ENOMEM)
    caml_raise_out_of_memory();
  return Val_int(result);
}

CAMLprim value ocaml_halo_bridge_send_response(value runtime_value,
                                               value request_id,
                                               value response) {
  CAMLparam3(runtime_value, request_id, response);
  CAMLreturn(halo_bridge_enqueue_command_stub(runtime_value,
                                              request_id,
                                              HALO_BRIDGE_SEND_RESPONSE,
                                              String_val(response),
                                              caml_string_length(response)));
}

CAMLprim value ocaml_halo_bridge_read_body(value runtime_value,
                                           value request_id) {
  CAMLparam2(runtime_value, request_id);
  CAMLreturn(halo_bridge_enqueue_command_stub(runtime_value,
                                              request_id,
                                              HALO_BRIDGE_READ_BODY,
                                              NULL,
                                              0));
}

CAMLprim value ocaml_halo_bridge_close_request(value runtime_value,
                                               value request_id) {
  CAMLparam2(runtime_value, request_id);
  CAMLreturn(halo_bridge_enqueue_command_stub(runtime_value,
                                              request_id,
                                              HALO_BRIDGE_CLOSE_REQUEST,
                                              NULL,
                                              0));
}

CAMLprim value ocaml_halo_bridge_stop(value runtime_value) {
  CAMLparam1(runtime_value);
  CAMLlocal1(zero);

  zero = caml_copy_int64(0);
  CAMLreturn(halo_bridge_enqueue_command_stub(runtime_value,
                                              zero,
                                              HALO_BRIDGE_STOP,
                                              NULL,
                                              0));
}

CAMLprim value ocaml_halo_bridge_begin_drain(value runtime_value) {
  CAMLparam1(runtime_value);
  CAMLlocal1(zero);

  zero = caml_copy_int64(0);
  CAMLreturn(halo_bridge_enqueue_command_stub(runtime_value,
                                              zero,
                                              HALO_BRIDGE_BEGIN_DRAIN,
                                              NULL,
                                              0));
}

static value halo_bridge_enqueue_payload_command(value runtime_value,
                                                 value request_id,
                                                 value payload,
                                                 enum halo_bridge_command_kind kind) {
  return halo_bridge_enqueue_command_stub(runtime_value,
                                          request_id,
                                          kind,
                                          String_val(payload),
                                          caml_string_length(payload));
}

CAMLprim value ocaml_halo_bridge_send_informational(value runtime_value,
                                                    value request_id,
                                                    value response) {
  CAMLparam3(runtime_value, request_id, response);
  CAMLreturn(halo_bridge_enqueue_payload_command(runtime_value,
                                                 request_id,
                                                 response,
                                                 HALO_BRIDGE_SEND_INFORMATIONAL));
}

CAMLprim value ocaml_halo_bridge_start_response_stream(value runtime_value,
                                                       value request_id,
                                                       value response) {
  CAMLparam3(runtime_value, request_id, response);
  CAMLreturn(halo_bridge_enqueue_payload_command(runtime_value,
                                                 request_id,
                                                 response,
                                                 HALO_BRIDGE_START_RESPONSE_STREAM));
}

CAMLprim value ocaml_halo_bridge_write_response_chunk(value runtime_value,
                                                      value request_id,
                                                      value response) {
  CAMLparam3(runtime_value, request_id, response);
  CAMLreturn(halo_bridge_enqueue_payload_command(runtime_value,
                                                 request_id,
                                                 response,
                                                 HALO_BRIDGE_WRITE_RESPONSE_CHUNK));
}

CAMLprim value ocaml_halo_bridge_finish_response_stream(value runtime_value,
                                                        value request_id) {
  CAMLparam2(runtime_value, request_id);
  CAMLreturn(halo_bridge_enqueue_command_stub(runtime_value,
                                              request_id,
                                              HALO_BRIDGE_FINISH_RESPONSE_STREAM,
                                              NULL,
                                              0));
}

CAMLprim value ocaml_halo_bridge_abort_response_stream(value runtime_value,
                                                       value request_id) {
  CAMLparam2(runtime_value, request_id);
  CAMLreturn(halo_bridge_enqueue_command_stub(runtime_value,
                                              request_id,
                                              HALO_BRIDGE_ABORT_RESPONSE_STREAM,
                                              NULL,
                                              0));
}

CAMLprim value ocaml_halo_bridge_read_metrics(value runtime_value,
                                              value query_id) {
  CAMLparam2(runtime_value, query_id);
  CAMLreturn(halo_bridge_enqueue_command_stub(runtime_value,
                                              query_id,
                                              HALO_BRIDGE_READ_METRICS,
                                              NULL,
                                              0));
}

CAMLprim value ocaml_halo_bridge_resolve_replay_claim(value runtime_value,
                                                       value claim_id,
                                                       value accept) {
  CAMLparam3(runtime_value, claim_id, accept);
  const char decision = Bool_val(accept) ? 1 : 0;

  CAMLreturn(halo_bridge_enqueue_command_stub(
    runtime_value,
    claim_id,
    HALO_BRIDGE_RESOLVE_REPLAY_CLAIM,
    &decision,
    sizeof(decision)));
}

CAMLprim value ocaml_halo_bridge_start(value runtime_value) {
  CAMLparam1(runtime_value);
  struct halo_runtime* runtime;
  int code;

  runtime = halo_runtime_get(runtime_value);
  /* Bridge callbacks remain OCaml-free while this domain is released. */
  caml_enter_blocking_section();
  code = halo_bridge_run_reactor(runtime);
  caml_leave_blocking_section();
  if (code != 0)
    CAMLreturn(halo_result_error_uv("halo_bridge_run", code));
  CAMLreturn(halo_result_ok(Val_unit));
}

CAMLprim value ocaml_halo_server_bridge_id(value server) {
  CAMLparam1(server);
  struct halo_listener_ref* ref;

  ref = *halo_listener_slot(server);
  if (ref == NULL)
    caml_failwith("halo server is closed");
  CAMLreturn(caml_copy_int64((int64_t) ref->bridge_listener_id));
}
