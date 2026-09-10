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
                                        value command_capacity) {
  CAMLparam3(runtime_value, event_capacity, command_capacity);
  struct halo_runtime* runtime;
  int code;

  runtime = halo_runtime_get(runtime_value);
  code = halo_bridge_enable(runtime,
                            (size_t) Int_val(event_capacity),
                            (size_t) Int_val(command_capacity));
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

CAMLprim value ocaml_halo_bridge_poll_event(value runtime_value) {
  CAMLparam1(runtime_value);
  CAMLlocal5(option, record, headers, cell, pair);
  CAMLlocal5(name, header_value, method_text, target, payload);
  CAMLlocal3(request_id, listener_id, auxiliary);
  CAMLlocal1(raw_headers);
  CAMLlocal5(connection_option, connection, tls_option, tls, remote_address);
  CAMLlocal5(local_address, server_name, application_protocol, cipher,
             protocol_version);
  struct halo_runtime* runtime;
  struct halo_bridge_event* event;
  size_t index;

  runtime = halo_runtime_get(runtime_value);
  if (runtime->bridge == NULL)
    caml_invalid_argument("reactor bridge is not enabled");
  event = halo_bridge_pop_event(runtime->bridge);
  if (event == NULL)
    CAMLreturn(Val_none);

  headers = Val_emptylist;
  raw_headers = Val_emptylist;
  for (index = event->header_count; index > 0; index--) {
    struct halo_bridge_header* header;

    header = &event->headers[index - 1];
    name = halo_bridge_alloc_string(header->name, header->name_len);
    header_value = halo_bridge_alloc_string(header->value, header->value_len);
    pair = caml_alloc_tuple(2);
    Store_field(pair, 0, name);
    Store_field(pair, 1, header_value);
    cell = caml_alloc(2, 0);
    Store_field(cell, 0, pair);
    Store_field(cell, 1, raw_headers);
    raw_headers = cell;
    if (!header->is_hop_by_hop) {
      cell = caml_alloc(2, 0);
      Store_field(cell, 0, pair);
      Store_field(cell, 1, headers);
      headers = cell;
    }
  }

  request_id = caml_copy_int64((int64_t) event->request_id);
  listener_id = caml_copy_int64((int64_t) event->listener_id);
  auxiliary = caml_copy_int64((int64_t) event->auxiliary);
  method_text = halo_bridge_alloc_string(event->method_text, event->method_len);
  target = halo_bridge_alloc_string(event->target, event->target_len);
  payload = halo_bridge_alloc_string(event->payload, event->payload_len);
  connection_option = Val_none;
  if (event->transport != NULL) {
    const struct halo_bridge_transport* transport;
    const unsigned char* cursor;

    transport = event->transport;
    cursor = transport->storage;
    remote_address = halo_bridge_alloc_sockaddr(
      cursor, transport->remote_address_len);
    cursor += transport->remote_address_len;
    local_address = halo_bridge_alloc_sockaddr(
      cursor, transport->local_address_len);
    cursor += transport->local_address_len;

    tls_option = Val_none;
    if (transport->tls_protocol_version != 0) {
      server_name = halo_bridge_alloc_string_option(
        cursor, transport->server_name_len);
      cursor += transport->server_name_len;
      application_protocol = halo_bridge_alloc_string_option(
        cursor, transport->application_protocol_len);
      cursor += transport->application_protocol_len;
      cipher = halo_bridge_alloc_string_option(cursor, transport->cipher_len);
      protocol_version = halo_bridge_alloc_tls_protocol_version(
        transport->tls_protocol_version);
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
    connection_option = caml_alloc(1, 0);
    Store_field(connection_option, 0, connection);
  }

  record = caml_alloc(15, 0);
  Store_field(record, 0, Val_int(event->kind));
  Store_field(record, 1, request_id);
  Store_field(record, 2, listener_id);
  Store_field(record, 3, method_text);
  Store_field(record, 4, target);
  Store_field(record, 5, headers);
  Store_field(record, 6, raw_headers);
  Store_field(record, 7, Val_int(event->http_major));
  Store_field(record, 8, Val_int(event->http_minor));
  Store_field(record, 9, Val_bool(event->keep_alive));
  Store_field(record, 10, Val_bool(event->body_expected));
  Store_field(record, 11, Val_bool(event->is_early_data));
  Store_field(record, 12, connection_option);
  Store_field(record, 13, auxiliary);
  Store_field(record, 14, payload);
  option = caml_alloc(1, 0);
  Store_field(option, 0, record);
  halo_bridge_event_destroy(event);
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
