#include "halo_bridge_internal.h"

#include <inttypes.h>
#include <unistd.h>

#define HALO_PRE_DISPATCH_RESPONSE_PAYLOAD_SIZE 25U

/* Descriptors precede byte storage so every typed region stays aligned. */
_Static_assert(sizeof(struct halo_bridge_event)
                 % _Alignof(struct halo_bridge_request_data) == 0,
               "request metadata must be aligned after the event header");
_Static_assert(sizeof(struct halo_bridge_request_data)
                 % _Alignof(struct halo_bridge_transport) == 0,
               "transport metadata must be aligned after request metadata");

struct halo_bridge_transport_source {
  struct sockaddr_storage remote_address;
  struct sockaddr_storage local_address;
  int remote_address_len;
  int local_address_len;
  const char* server_name;
  const char* application_protocol;
  const char* cipher;
  size_t server_name_len;
  size_t application_protocol_len;
  size_t cipher_len;
  uint8_t tls_protocol_version;
};

static int halo_bridge_prepare_transport(
    struct halo_bridge_transport_source* source,
    struct halo_client* client,
    size_t* allocation_size) {
  int tls_protocol_version;

  source->remote_address_len = (int) sizeof(source->remote_address);
  source->local_address_len = (int) sizeof(source->local_address);
  if (uv_tcp_getpeername(&client->handle,
                         (struct sockaddr*) &source->remote_address,
                         &source->remote_address_len) != 0
      || uv_tcp_getsockname(&client->handle,
                            (struct sockaddr*) &source->local_address,
                            &source->local_address_len) != 0
      || source->remote_address_len <= 0
      || source->remote_address_len > (int) sizeof(source->remote_address)
      || source->local_address_len <= 0
      || source->local_address_len > (int) sizeof(source->local_address))
    return -1;

  source->server_name = NULL;
  source->application_protocol = NULL;
  source->cipher = NULL;
  tls_protocol_version = 0;
  if (client->tls_conn != NULL) {
    tls_protocol_version =
      s2n_connection_get_actual_protocol_version(client->tls_conn);
    if (tls_protocol_version > UINT8_MAX)
      return -1;
    if (tls_protocol_version <= 0)
      tls_protocol_version = UINT8_MAX;
    else {
      source->server_name = s2n_get_server_name(client->tls_conn);
      source->application_protocol =
        s2n_get_application_protocol(client->tls_conn);
      source->cipher = s2n_connection_get_cipher(client->tls_conn);
    }
  }

  source->tls_protocol_version = (uint8_t) tls_protocol_version;
  source->server_name_len =
    source->server_name == NULL ? 0 : strlen(source->server_name);
  source->application_protocol_len = source->application_protocol == NULL
    ? 0 : strlen(source->application_protocol);
  source->cipher_len = source->cipher == NULL ? 0 : strlen(source->cipher);
  if (source->server_name_len > UINT16_MAX
      || source->application_protocol_len > UINT16_MAX
      || source->cipher_len > UINT16_MAX)
    return -1;

  *allocation_size = sizeof(struct halo_bridge_transport)
    + (size_t) source->remote_address_len + (size_t) source->local_address_len
    + source->server_name_len + source->application_protocol_len
    + source->cipher_len;
  return 0;
}

static void halo_bridge_store_transport(
    struct halo_bridge_transport* transport,
    const struct halo_bridge_transport_source* source) {
  unsigned char* cursor;

  transport->remote_address_len = (uint16_t) source->remote_address_len;
  transport->local_address_len = (uint16_t) source->local_address_len;
  transport->server_name_len = (uint16_t) source->server_name_len;
  transport->application_protocol_len =
    (uint16_t) source->application_protocol_len;
  transport->cipher_len = (uint16_t) source->cipher_len;
  transport->tls_protocol_version = source->tls_protocol_version;
  cursor = transport->storage;
  memcpy(cursor, &source->remote_address, (size_t) source->remote_address_len);
  cursor += source->remote_address_len;
  memcpy(cursor, &source->local_address, (size_t) source->local_address_len);
  cursor += source->local_address_len;
  if (source->server_name_len > 0) {
    memcpy(cursor, source->server_name, source->server_name_len);
    cursor += source->server_name_len;
  }
  if (source->application_protocol_len > 0) {
    memcpy(cursor, source->application_protocol, source->application_protocol_len);
    cursor += source->application_protocol_len;
  }
  if (source->cipher_len > 0)
    memcpy(cursor, source->cipher, source->cipher_len);
}

static int halo_bridge_add_size(size_t* total, size_t amount) {
  if (amount > SIZE_MAX - *total)
    return -1;
  *total += amount;
  return 0;
}

static char* halo_bridge_store_bytes(char** cursor,
                                     const char* source,
                                     size_t length) {
  char* destination;

  if (length == 0)
    return NULL;
  destination = *cursor;
  memcpy(destination, source, length);
  *cursor += length;
  return destination;
}

static struct halo_bridge_event* halo_bridge_request_event(
    struct halo_client* client,
    enum halo_bridge_event_kind kind,
    const char* method_text,
    size_t method_len,
    int include_transport,
    size_t payload_len) {
  const struct halo_request* request;
  struct halo_bridge_transport_source transport_source;
  struct halo_bridge_request_data* data;
  struct halo_bridge_event* event;
  size_t transport_size;
  size_t headers_size;
  size_t allocation_size;
  size_t index;
  char* cursor;

  request = &client->request;
  transport_size = 0;
  if (include_transport
      && halo_bridge_prepare_transport(&transport_source, client,
                                        &transport_size) != 0)
    return NULL;

  headers_size = 0;
  for (index = 0; index < request->header_count; index++) {
    if (halo_bridge_add_size(&headers_size,
                             request->headers[index].name_len) != 0
        || halo_bridge_add_size(&headers_size,
                                 request->headers[index].value_len) != 0
        || halo_bridge_add_size(&headers_size,
                                 HALO_BRIDGE_PACKED_HEADER_METADATA_SIZE) != 0)
      return NULL;
  }

  allocation_size = sizeof(*event) + sizeof(*data);
  if (halo_bridge_add_size(&allocation_size, transport_size) != 0
      || halo_bridge_add_size(&allocation_size, method_len) != 0
      || halo_bridge_add_size(&allocation_size, request->url_len) != 0
      || halo_bridge_add_size(&allocation_size, headers_size) != 0
      || halo_bridge_add_size(&allocation_size, payload_len) != 0)
    return NULL;

  event = malloc(allocation_size);
  if (event == NULL)
    return NULL;
  memset(event, 0, sizeof(*event));
  event->kind = kind;
  event->listener_id = client->listener_ref->bridge_listener_id;
  data = (struct halo_bridge_request_data*) (event + 1);
  memset(data, 0, sizeof(*data));
  event->request = data;
  cursor = (char*) (data + 1);
  if (include_transport) {
    data->transport = (struct halo_bridge_transport*) cursor;
    halo_bridge_store_transport(data->transport, &transport_source);
    cursor += transport_size;
  }
  data->method_text = halo_bridge_store_bytes(&cursor, method_text, method_len);
  data->method_len = method_len;
  data->target = halo_bridge_store_bytes(&cursor, request->url, request->url_len);
  data->target_len = request->url_len;
  data->packed_headers = headers_size == 0 ? NULL : cursor;
  data->packed_headers_len = headers_size;
  for (index = 0; index < request->header_count; index++) {
    const struct halo_header* source;

    source = &request->headers[index];
    (void) halo_bridge_store_bytes(&cursor, source->name, source->name_len);
    (void) halo_bridge_store_bytes(&cursor, source->value, source->value_len);
    memcpy(cursor, &source->name_len, sizeof(source->name_len));
    cursor += sizeof(source->name_len);
    memcpy(cursor, &source->value_len, sizeof(source->value_len));
    cursor += sizeof(source->value_len);
    *cursor++ = source->is_hop_by_hop;
  }
  if (payload_len > 0)
    event->payload = cursor;
  event->payload_len = payload_len;
  assert((size_t) (cursor - (char*) event) + payload_len == allocation_size);
  data->http_major = request->http_major;
  data->http_minor = request->http_minor;
  data->keep_alive = client->should_keep_alive;
  data->body_expected = request->content_length > 0
    || (client->parser.flags & F_CHUNKED) != 0;
  data->is_early_data = client->request_is_early_data;
  return event;
}

static void halo_bridge_notify(struct halo_bridge* bridge) {
  const unsigned char byte = 1;
  ssize_t result;

  do {
    result = write(bridge->event_pipe[1], &byte, sizeof(byte));
  } while (result < 0 && errno == EINTR);
}

int halo_bridge_push_event(struct halo_bridge* bridge,
                           struct halo_bridge_event* event) {
  int accepted;
  int notify;

  accepted = 0;
  notify = 0;
  pthread_mutex_lock(&bridge->event_mutex);
  if (bridge->event_count < bridge->event_capacity) {
    event->next = NULL;
    if (bridge->event_tail == NULL) {
      bridge->event_head = event;
      notify = 1;
    } else
      bridge->event_tail->next = event;
    bridge->event_tail = event;
    bridge->event_count += 1;
    accepted = 1;
  }
  pthread_mutex_unlock(&bridge->event_mutex);

  if (notify)
    halo_bridge_notify(bridge);
  return accepted ? 0 : -1;
}

static void halo_bridge_push_required_event(struct halo_bridge* bridge,
                                            struct halo_bridge_event* event) {
  int notify;

  pthread_mutex_lock(&bridge->event_mutex);
  event->next = NULL;
  notify = bridge->event_tail == NULL;
  if (notify)
    bridge->event_head = event;
  else
    bridge->event_tail->next = event;
  bridge->event_tail = event;
  bridge->event_count += 1;
  pthread_mutex_unlock(&bridge->event_mutex);
  if (notify)
    halo_bridge_notify(bridge);
}

struct halo_bridge_event* halo_bridge_pop_event(struct halo_bridge* bridge) {
  struct halo_bridge_event* event;

  pthread_mutex_lock(&bridge->event_mutex);
  event = bridge->event_head;
  if (event != NULL) {
    bridge->event_head = event->next;
    if (bridge->event_head == NULL)
      bridge->event_tail = NULL;
    bridge->event_count -= 1;
    if ((event->kind == HALO_BRIDGE_RESPONSE_COMPLETE
         || event->kind == HALO_BRIDGE_DISCONNECTED)
        && bridge->request_count > 0)
      bridge->request_count -= 1;
    event->next = NULL;
  }
  pthread_mutex_unlock(&bridge->event_mutex);
  return event;
}

static int halo_bridge_reserve_request(struct halo_bridge* bridge) {
  int accepted;

  pthread_mutex_lock(&bridge->event_mutex);
  accepted = bridge->request_count < bridge->request_capacity;
  if (accepted)
    bridge->request_count += 1;
  pthread_mutex_unlock(&bridge->event_mutex);
  return accepted ? 0 : -1;
}

static void halo_bridge_release_request(struct halo_bridge* bridge) {
  pthread_mutex_lock(&bridge->event_mutex);
  if (bridge->request_count > 0)
    bridge->request_count -= 1;
  pthread_mutex_unlock(&bridge->event_mutex);
}

struct halo_client* halo_bridge_find_client(struct halo_bridge* bridge,
                                            uint64_t request_id) {
  return halo_client_registry_lookup(&bridge->requests, request_id);
}

static int halo_bridge_register_client(struct halo_bridge* bridge,
                                       struct halo_client* client,
                                       uint64_t request_id) {
  return halo_client_registry_insert(&bridge->requests, request_id, client);
}

static void halo_bridge_unregister_client(struct halo_bridge* bridge,
                                          uint64_t request_id) {
  (void) halo_client_registry_remove(&bridge->requests, request_id);
}

static int halo_bridge_register_replay_claim(struct halo_bridge* bridge,
                                             struct halo_client* client,
                                             uint64_t claim_id) {
  return halo_client_registry_insert(&bridge->replay_claims, claim_id, client);
}

static struct halo_client* halo_bridge_unregister_replay_claim(
    struct halo_bridge* bridge,
    uint64_t claim_id) {
  return halo_client_registry_remove(&bridge->replay_claims, claim_id);
}

static struct halo_bridge_event* halo_bridge_simple_event(
  enum halo_bridge_event_kind kind,
  uint64_t request_id
) {
  struct halo_bridge_event* event;

  event = calloc(1, sizeof(*event));
  if (event == NULL)
    return NULL;
  event->kind = kind;
  event->request_id = request_id;
  return event;
}

static struct halo_bridge_event* halo_bridge_payload_event(
    enum halo_bridge_event_kind kind,
    uint64_t request_id,
    const char* payload,
    size_t payload_len) {
  struct halo_bridge_event* event;

  if (payload_len > SIZE_MAX - sizeof(*event))
    return NULL;
  event = malloc(sizeof(*event) + payload_len);
  if (event == NULL)
    return NULL;
  memset(event, 0, sizeof(*event));
  event->kind = kind;
  event->request_id = request_id;
  if (payload_len > 0) {
    event->payload = (char*) (event + 1);
    memcpy(event->payload, payload, payload_len);
  }
  event->payload_len = payload_len;
  return event;
}

static void halo_bridge_encode_uint64(unsigned char* destination,
                                      uint64_t value) {
  size_t index;

  for (index = 0; index < 8; index++) {
    destination[index] =
      (unsigned char) ((value >> ((7U - index) * 8U)) & 0xffU);
  }
}

int halo_bridge_dispatch_replay_claim(struct halo_client* client,
                                      const uint8_t* identity,
                                      size_t identity_len,
                                      uint64_t ttl_secs) {
  struct halo_runtime* runtime;
  struct halo_bridge* bridge;
  struct halo_bridge_event* event;
  uint64_t claim_id;

  if (client == NULL || client->listener_ref == NULL
      || client->handle.loop == NULL || identity == NULL || identity_len == 0
      || ttl_secs == 0
      || client->bridge_replay_claim_id != 0)
    return -1;
  runtime = client->handle.loop->data;
  bridge = runtime != NULL ? runtime->bridge : NULL;
  if (bridge == NULL)
    return -1;

  event = halo_bridge_payload_event(HALO_BRIDGE_REPLAY_CLAIM, 0,
                                    (const char*) identity, identity_len);
  if (event == NULL)
    return -1;
  event->listener_id = client->listener_ref->bridge_listener_id;
  event->auxiliary = ttl_secs;

  claim_id = ++bridge->next_replay_claim_id;
  if (claim_id == 0)
    claim_id = ++bridge->next_replay_claim_id;
  event->request_id = claim_id;
  if (halo_bridge_register_replay_claim(bridge, client, claim_id) != 0) {
    free(event);
    return -1;
  }
  client->bridge_replay_claim_id = claim_id;
  if (halo_bridge_push_event(bridge, event) != 0) {
    client->bridge_replay_claim_id = 0;
    (void) halo_bridge_unregister_replay_claim(bridge, claim_id);
    free(event);
    return -1;
  }
  return 0;
}

void halo_bridge_cancel_replay_claim(struct halo_client* client) {
  struct halo_runtime* runtime;
  struct halo_bridge* bridge;

  if (client == NULL || client->bridge_replay_claim_id == 0
      || client->handle.loop == NULL)
    return;
  runtime = client->handle.loop->data;
  bridge = runtime != NULL ? runtime->bridge : NULL;
  if (bridge != NULL)
    (void) halo_bridge_unregister_replay_claim(
      bridge, client->bridge_replay_claim_id);
  client->bridge_replay_claim_id = 0;
  client->tls_pending_early_data = NULL;
}

int halo_bridge_resolve_replay_claim(struct halo_bridge* bridge,
                                    uint64_t claim_id,
                                    int accept) {
  struct halo_client* client;

  if (bridge == NULL || claim_id == 0)
    return -1;
  client = halo_bridge_unregister_replay_claim(bridge, claim_id);
  if (client == NULL)
    return 0;
  if (halo_client_bridge_resolve_replay_claim(client, claim_id, accept) != 0) {
    halo_client_bridge_close(client);
    return -1;
  }
  return 0;
}

int halo_bridge_dispatch_request(struct halo_client* client) {
  struct halo_runtime* runtime;
  struct halo_bridge* bridge;
  struct halo_bridge_event* event;
  struct halo_request* request;
  uint64_t request_id;
  int first_request_on_connection;

  if (client == NULL || client->handle.loop == NULL)
    return -1;
  runtime = client->handle.loop->data;
  bridge = runtime != NULL ? runtime->bridge : NULL;
  if (bridge == NULL || client->bridge_request_id != 0)
    return -1;

  if (halo_bridge_reserve_request(bridge) != 0)
    return halo_client_bridge_reject_busy(client);

  first_request_on_connection = client->bridge_connection_id == 0;
  /* One-shot connections carry metadata directly and never enter the shared
   * OCaml cache. */
  if (first_request_on_connection && client->should_keep_alive) {
    client->bridge_connection_id = ++bridge->next_connection_id;
    if (client->bridge_connection_id == 0)
      client->bridge_connection_id = ++bridge->next_connection_id;
  }

  request = &client->request;
  event = halo_bridge_request_event(client, HALO_BRIDGE_REQUEST,
                                    request->method_text, request->method_len,
                                    first_request_on_connection, 0);
  if (event == NULL) {
    halo_bridge_release_request(bridge);
    return -1;
  }
  event->auxiliary = client->bridge_connection_id;
  if (runtime->telemetry_enabled) {
    event->monotonic_time_ns = client->request_start_monotonic_ns;
    event->unix_time_ns = client->request_start_unix_ns;
  }

  request_id = ++bridge->next_request_id;
  if (request_id == 0)
    request_id = ++bridge->next_request_id;
  event->request_id = request_id;
  if (halo_bridge_register_client(bridge, client, request_id) != 0) {
    halo_bridge_release_request(bridge);
    free(event);
    return -1;
  }
  client->bridge_request_id = request_id;
  if (halo_bridge_push_event(bridge, event) != 0) {
    client->bridge_request_id = 0;
    halo_bridge_unregister_client(bridge, request_id);
    halo_bridge_release_request(bridge);
    free(event);
    return -1;
  }
  return 0;
}

int halo_bridge_buffer_body_chunk(struct halo_client* client,
                                  const char* data,
                                  size_t length) {
  struct halo_bridge_event* event;
  size_t required;
  size_t capacity;

  if (client == NULL || client->bridge_request_id == 0)
    return -1;
  if (length == 0)
    return 0;
  event = client->bridge_body_event;
  if (length > SIZE_MAX - (event == NULL ? 0 : event->payload_len))
    return -1;
  required = length + (event == NULL ? 0 : event->payload_len);
  capacity = client->bridge_body_capacity;
  if (event == NULL || required > capacity) {
    struct halo_bridge_event* next;
    int first = event == NULL;

    if (capacity == 0)
      capacity = 1024;
    while (capacity < required) {
      if (capacity > (SIZE_MAX - sizeof(*event)) / 2) {
        capacity = required;
        break;
      }
      capacity *= 2;
    }
    if (capacity > SIZE_MAX - sizeof(*event))
      return -1;
    next = realloc(event, sizeof(*event) + capacity);
    if (next == NULL)
      return -1;
    if (first) {
      memset(next, 0, sizeof(*next));
      next->kind = HALO_BRIDGE_BODY_CHUNK;
      next->request_id = client->bridge_request_id;
    }
    event = next;
    event->payload = (char*) (event + 1);
    client->bridge_body_event = event;
    client->bridge_body_capacity = capacity;
  }
  memcpy(event->payload + event->payload_len, data, length);
  event->payload_len = required;
  return 0;
}

int halo_bridge_flush_body_chunk(struct halo_client* client) {
  struct halo_runtime* runtime;
  struct halo_bridge_event* event;

  event = client->bridge_body_event;
  if (event == NULL)
    return 0;
  client->bridge_body_event = NULL;
  client->bridge_body_capacity = 0;
  runtime = client->handle.loop->data;
  if (halo_bridge_push_event(runtime->bridge, event) != 0) {
    free(event);
    return -1;
  }
  return 0;
}

int halo_bridge_emit_body_end(struct halo_client* client) {
  struct halo_runtime* runtime;
  struct halo_bridge_event* event;

  if (client == NULL || client->bridge_request_id == 0)
    return -1;
  runtime = client->handle.loop->data;
  event = halo_bridge_simple_event(HALO_BRIDGE_BODY_END,
                                   client->bridge_request_id);
  if (event == NULL)
    return -1;
  if (halo_bridge_push_event(runtime->bridge, event) != 0) {
    free(event);
    return -1;
  }
  return 0;
}

static void halo_bridge_finish_request(struct halo_client* client,
                                       enum halo_bridge_event_kind kind) {
  struct halo_runtime* runtime;
  struct halo_bridge* bridge;
  struct halo_bridge_event* event;
  uint64_t request_id;

  if (client == NULL || client->bridge_request_id == 0)
    return;
  runtime = client->handle.loop->data;
  bridge = runtime != NULL ? runtime->bridge : NULL;
  if (bridge == NULL)
    return;

  request_id = client->bridge_request_id;
  client->bridge_request_id = 0;
  halo_bridge_unregister_client(bridge, request_id);
  event = halo_bridge_simple_event(kind, request_id);
  if (event == NULL) {
    halo_bridge_release_request(bridge);
    return;
  }
  if (runtime->telemetry_enabled)
    event->monotonic_time_ns = uv_hrtime();
  event->auxiliary = client->body_bytes_read;
  halo_bridge_push_required_event(bridge, event);
}

void halo_bridge_complete_request(struct halo_client* client) {
  halo_bridge_finish_request(client, HALO_BRIDGE_RESPONSE_COMPLETE);
}

void halo_bridge_disconnect_request(struct halo_client* client) {
  halo_bridge_finish_request(client, HALO_BRIDGE_DISCONNECTED);
}

void halo_bridge_observe_pre_dispatch_response(struct halo_client* client,
                                               int status_code,
                                               size_t response_body_size,
                                               int completed) {
  static const char unknown_method[] = "_OTHER";
  struct halo_runtime* runtime;
  struct halo_bridge* bridge;
  struct halo_bridge_event* event;
  const struct halo_request* request;
  const char* method_text;
  size_t method_len;
  uint64_t request_id;
  uint64_t end_monotonic_ns;

  if (client == NULL || client->handle.loop == NULL
      || client->listener_ref == NULL || status_code < 100
      || status_code > 599)
    return;
  runtime = client->handle.loop->data;
  if (runtime == NULL || !runtime->telemetry_enabled)
    return;
  bridge = runtime->bridge;
  if (bridge == NULL)
    return;

  end_monotonic_ns = uv_hrtime();
  request = &client->request;
  if (request->method_seen && request->method_len > 0) {
    method_text = request->method_text;
    method_len = request->method_len;
  }
  else {
    method_text = unknown_method;
    method_len = sizeof(unknown_method) - 1;
  }
  event = halo_bridge_request_event(client, HALO_BRIDGE_PRE_DISPATCH_RESPONSE,
                                    method_text, method_len, 1,
                                    HALO_PRE_DISPATCH_RESPONSE_PAYLOAD_SIZE);
  if (event == NULL)
    return;
  if (request->http_major == 0) {
    event->request->http_major = llhttp_get_http_major(&client->parser);
    event->request->http_minor = llhttp_get_http_minor(&client->parser);
  }
  event->monotonic_time_ns = end_monotonic_ns;
  if (client->request_start_monotonic_ns != 0)
    event->monotonic_time_ns = client->request_start_monotonic_ns;
  event->unix_time_ns = client->request_start_unix_ns;
  event->auxiliary = end_monotonic_ns;
  halo_bridge_encode_uint64((unsigned char*) event->payload,
                            (uint64_t) status_code);
  halo_bridge_encode_uint64((unsigned char*) event->payload + 8,
                            (uint64_t) client->body_bytes_read);
  halo_bridge_encode_uint64((unsigned char*) event->payload + 16,
                            (uint64_t) response_body_size);
  event->payload[24] = 0;
  if (completed)
    event->payload[24] = 1;

  request_id = ++bridge->next_request_id;
  if (request_id == 0)
    request_id = ++bridge->next_request_id;
  event->request_id = request_id;
  if (halo_bridge_push_event(bridge, event) != 0)
    free(event);
}

void halo_bridge_connection_closed(struct halo_client* client) {
  struct halo_runtime* runtime;
  struct halo_bridge* bridge;
  struct halo_bridge_event* event;
  uint64_t connection_id;

  if (client == NULL || client->bridge_connection_id == 0)
    return;
  connection_id = client->bridge_connection_id;
  client->bridge_connection_id = 0;
  if (client->handle.loop == NULL)
    return;
  runtime = client->handle.loop->data;
  bridge = runtime != NULL ? runtime->bridge : NULL;
  if (bridge == NULL)
    return;
  event = halo_bridge_simple_event(HALO_BRIDGE_CONNECTION_CLOSED,
                                   connection_id);
  if (event == NULL)
    return;
  halo_bridge_push_required_event(bridge, event);
}

int halo_bridge_complete_write(struct halo_client* client) {
  struct halo_runtime* runtime;
  struct halo_bridge_event* event;

  if (client == NULL || client->bridge_request_id == 0
      || client->handle.loop == NULL)
    return -1;
  runtime = client->handle.loop->data;
  if (runtime == NULL || runtime->bridge == NULL)
    return -1;
  event = halo_bridge_simple_event(HALO_BRIDGE_WRITE_COMPLETE,
                                   client->bridge_request_id);
  if (event == NULL)
    return -1;
  halo_bridge_push_required_event(runtime->bridge, event);
  return 0;
}

int halo_bridge_emit_metrics(struct halo_runtime* runtime, uint64_t query_id) {
  struct halo_bridge_event* event;
  char payload[384];
  size_t event_queue_depth;
  size_t command_queue_depth;
  int length;

  if (runtime == NULL || runtime->bridge == NULL)
    return -1;
  halo_runtime_controller_tick(runtime);
  pthread_mutex_lock(&runtime->bridge->event_mutex);
  event_queue_depth = runtime->bridge->event_count;
  pthread_mutex_unlock(&runtime->bridge->event_mutex);
  pthread_mutex_lock(&runtime->bridge->command_mutex);
  command_queue_depth = runtime->bridge->command_count;
  pthread_mutex_unlock(&runtime->bridge->command_mutex);
  length = snprintf(payload,
                    sizeof(payload),
                    "%.17g %" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64
                    " %" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64
                    " %zu %zu %d %d",
                    runtime->metrics.utilization,
                    runtime->metrics.active_connections,
                    runtime->metrics.active_requests,
                    runtime->metrics.accepted_connections,
                    runtime->metrics.rejected_connections,
                    runtime->metrics.requests_completed,
                    runtime->metrics.tls_full_handshakes,
                    runtime->metrics.tls_resumed_handshakes,
                    runtime->metrics.tls_handshake_failures,
                    event_queue_depth,
                    command_queue_depth,
                    (int) runtime->controller.state,
                    runtime->controller.backpressure_enabled);
  if (length < 0 || (size_t) length >= sizeof(payload))
    return -1;
  event = halo_bridge_payload_event(HALO_BRIDGE_METRICS, query_id,
                                    payload, (size_t) length);
  if (event == NULL)
    return -1;
  halo_bridge_push_required_event(runtime->bridge, event);
  return 0;
}

void halo_bridge_mark_stopped(struct halo_runtime* runtime) {
  struct halo_bridge_event* event;

  if (runtime == NULL || runtime->bridge == NULL)
    return;
  event = halo_bridge_simple_event(HALO_BRIDGE_STOPPED, 0);
  if (event == NULL)
    return;
  halo_bridge_push_required_event(runtime->bridge, event);
}

void halo_bridge_mark_drained(struct halo_runtime* runtime) {
  struct halo_bridge_event* event;

  if (runtime == NULL || runtime->bridge == NULL)
    return;
  event = halo_bridge_simple_event(HALO_BRIDGE_DRAINED, 0);
  if (event == NULL)
    return;
  halo_bridge_push_required_event(runtime->bridge, event);
}
