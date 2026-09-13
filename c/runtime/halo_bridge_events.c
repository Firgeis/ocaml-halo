#include "halo_bridge_internal.h"

#include <inttypes.h>
#include <unistd.h>

#define HALO_PRE_DISPATCH_RESPONSE_PAYLOAD_SIZE 25U

void halo_bridge_event_destroy(struct halo_bridge_event* event) {
  size_t index;

  if (event == NULL)
    return;
  free(event->method_text);
  free(event->target);
  for (index = 0; index < event->header_count; index++) {
    free(event->headers[index].name);
    free(event->headers[index].value);
  }
  free(event->headers);
  free(event->transport);
  free(event->payload);
  free(event);
}

static int halo_bridge_copy_transport(struct halo_bridge_event* event,
                                      struct halo_client* client) {
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
  size_t storage_len;
  struct halo_bridge_transport* transport;
  unsigned char* cursor;
  int tls_protocol_version;

  remote_address_len = (int) sizeof(remote_address);
  local_address_len = (int) sizeof(local_address);
  if (uv_tcp_getpeername(&client->handle,
                         (struct sockaddr*) &remote_address,
                         &remote_address_len) != 0
      || uv_tcp_getsockname(&client->handle,
                            (struct sockaddr*) &local_address,
                            &local_address_len) != 0
      || remote_address_len <= 0 || remote_address_len > UINT16_MAX
      || local_address_len <= 0 || local_address_len > UINT16_MAX)
    return -1;

  server_name = NULL;
  application_protocol = NULL;
  cipher = NULL;
  tls_protocol_version = 0;
  if (client->tls_conn != NULL) {
    tls_protocol_version =
      s2n_connection_get_actual_protocol_version(client->tls_conn);
    if (tls_protocol_version > UINT8_MAX)
      return -1;
    if (tls_protocol_version <= 0)
      tls_protocol_version = UINT8_MAX;
    else {
      server_name = s2n_get_server_name(client->tls_conn);
      application_protocol = s2n_get_application_protocol(client->tls_conn);
      cipher = s2n_connection_get_cipher(client->tls_conn);
    }
  }

  server_name_len = server_name == NULL ? 0 : strlen(server_name);
  application_protocol_len =
    application_protocol == NULL ? 0 : strlen(application_protocol);
  cipher_len = cipher == NULL ? 0 : strlen(cipher);
  if (server_name_len > UINT16_MAX
      || application_protocol_len > UINT16_MAX
      || cipher_len > UINT16_MAX)
    return -1;

  storage_len = (size_t) remote_address_len + (size_t) local_address_len
    + server_name_len + application_protocol_len + cipher_len;
  if (storage_len > SIZE_MAX - sizeof(*transport))
    return -1;
  transport = malloc(sizeof(*transport) + storage_len);
  if (transport == NULL)
    return -1;

  transport->remote_address_len = (uint16_t) remote_address_len;
  transport->local_address_len = (uint16_t) local_address_len;
  transport->server_name_len = (uint16_t) server_name_len;
  transport->application_protocol_len =
    (uint16_t) application_protocol_len;
  transport->cipher_len = (uint16_t) cipher_len;
  transport->tls_protocol_version = (uint8_t) tls_protocol_version;
  cursor = transport->storage;
  memcpy(cursor, &remote_address, (size_t) remote_address_len);
  cursor += remote_address_len;
  memcpy(cursor, &local_address, (size_t) local_address_len);
  cursor += local_address_len;
  if (server_name_len > 0) {
    memcpy(cursor, server_name, server_name_len);
    cursor += server_name_len;
  }
  if (application_protocol_len > 0) {
    memcpy(cursor, application_protocol, application_protocol_len);
    cursor += application_protocol_len;
  }
  if (cipher_len > 0)
    memcpy(cursor, cipher, cipher_len);

  event->transport = transport;
  return 0;
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

  accepted = 0;
  pthread_mutex_lock(&bridge->event_mutex);
  if (bridge->event_count < bridge->event_capacity) {
    event->next = NULL;
    if (bridge->event_tail == NULL)
      bridge->event_head = event;
    else
      bridge->event_tail->next = event;
    bridge->event_tail = event;
    bridge->event_count += 1;
    accepted = 1;
  }
  pthread_mutex_unlock(&bridge->event_mutex);

  if (accepted)
    halo_bridge_notify(bridge);
  return accepted ? 0 : -1;
}

static void halo_bridge_push_required_event(struct halo_bridge* bridge,
                                            struct halo_bridge_event* event) {
  pthread_mutex_lock(&bridge->event_mutex);
  event->next = NULL;
  if (bridge->event_tail == NULL)
    bridge->event_head = event;
  else
    bridge->event_tail->next = event;
  bridge->event_tail = event;
  bridge->event_count += 1;
  pthread_mutex_unlock(&bridge->event_mutex);
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
  struct halo_bridge_request_entry* entry;

  for (entry = bridge->requests; entry != NULL; entry = entry->next) {
    if (entry->request_id == request_id)
      return entry->client;
  }
  return NULL;
}

static int halo_bridge_register_client(struct halo_bridge* bridge,
                                       struct halo_client* client,
                                       uint64_t request_id) {
  struct halo_bridge_request_entry* entry;

  entry = malloc(sizeof(*entry));
  if (entry == NULL)
    return -1;
  entry->request_id = request_id;
  entry->client = client;
  entry->next = bridge->requests;
  bridge->requests = entry;
  return 0;
}

static void halo_bridge_unregister_client(struct halo_bridge* bridge,
                                          uint64_t request_id) {
  struct halo_bridge_request_entry** cursor;

  cursor = &bridge->requests;
  while (*cursor != NULL) {
    struct halo_bridge_request_entry* entry;

    entry = *cursor;
    if (entry->request_id == request_id) {
      *cursor = entry->next;
      free(entry);
      return;
    }
    cursor = &entry->next;
  }
}

static int halo_bridge_register_replay_claim(struct halo_bridge* bridge,
                                             struct halo_client* client,
                                             uint64_t claim_id) {
  struct halo_bridge_replay_claim_entry* entry;
  size_t bucket_index;

  entry = malloc(sizeof(*entry));
  if (entry == NULL)
    return -1;
  bucket_index = claim_id % HALO_BRIDGE_REPLAY_CLAIM_BUCKET_COUNT;
  entry->claim_id = claim_id;
  entry->client = client;
  entry->next = bridge->replay_claims[bucket_index];
  bridge->replay_claims[bucket_index] = entry;
  return 0;
}

static struct halo_client* halo_bridge_unregister_replay_claim(
    struct halo_bridge* bridge,
    uint64_t claim_id) {
  struct halo_bridge_replay_claim_entry** cursor;
  size_t bucket_index;

  bucket_index = claim_id % HALO_BRIDGE_REPLAY_CLAIM_BUCKET_COUNT;
  cursor = &bridge->replay_claims[bucket_index];
  while (*cursor != NULL) {
    struct halo_bridge_replay_claim_entry* entry;
    struct halo_client* client;

    entry = *cursor;
    if (entry->claim_id == claim_id) {
      client = entry->client;
      *cursor = entry->next;
      free(entry);
      return client;
    }
    cursor = &entry->next;
  }
  return NULL;
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

static int halo_bridge_copy_request_headers(struct halo_bridge_event* event,
                                            const struct halo_request* request) {
  size_t index;

  if (request->header_count == 0)
    return 0;

  event->headers = calloc(request->header_count, sizeof(*event->headers));
  if (event->headers == NULL)
    return -1;

  for (index = 0; index < request->header_count; index++) {
    const struct halo_header* source;
    struct halo_bridge_header* destination;

    source = &request->headers[index];
    destination = &event->headers[index];
    destination->name = halo_bridge_copy_bytes(source->name, source->name_len);
    destination->name_len = source->name_len;
    destination->value = halo_bridge_copy_bytes(source->value,
                                                source->value_len);
    destination->value_len = source->value_len;
    destination->is_hop_by_hop = source->is_hop_by_hop;
    event->header_count = index + 1;
    if ((source->name_len > 0 && destination->name == NULL)
        || (source->value_len > 0 && destination->value == NULL))
      return -1;
  }
  return 0;
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

  event = halo_bridge_simple_event(HALO_BRIDGE_REPLAY_CLAIM, 0);
  if (event == NULL)
    return -1;
  event->listener_id = client->listener_ref->bridge_listener_id;
  event->auxiliary = ttl_secs;
  event->payload = halo_bridge_copy_bytes((const char*) identity, identity_len);
  event->payload_len = identity_len;
  if (event->payload == NULL) {
    halo_bridge_event_destroy(event);
    return -1;
  }

  claim_id = ++bridge->next_replay_claim_id;
  if (claim_id == 0)
    claim_id = ++bridge->next_replay_claim_id;
  event->request_id = claim_id;
  if (halo_bridge_register_replay_claim(bridge, client, claim_id) != 0) {
    halo_bridge_event_destroy(event);
    return -1;
  }
  client->bridge_replay_claim_id = claim_id;
  if (halo_bridge_push_event(bridge, event) != 0) {
    client->bridge_replay_claim_id = 0;
    (void) halo_bridge_unregister_replay_claim(bridge, claim_id);
    halo_bridge_event_destroy(event);
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
  event = calloc(1, sizeof(*event));
  if (event == NULL) {
    halo_bridge_release_request(bridge);
    return -1;
  }
  event->kind = HALO_BRIDGE_REQUEST;
  event->listener_id = client->listener_ref->bridge_listener_id;
  event->auxiliary = client->bridge_connection_id;
  event->method_text = halo_bridge_copy_bytes(request->method_text,
                                              request->method_len);
  event->method_len = request->method_len;
  event->target = halo_bridge_copy_bytes(request->url, request->url_len);
  event->target_len = request->url_len;
  event->http_major = request->http_major;
  event->http_minor = request->http_minor;
  event->keep_alive = client->should_keep_alive;
  event->body_expected = request->content_length > 0
    || (client->parser.flags & F_CHUNKED) != 0;
  event->is_early_data = client->request_is_early_data;
  if (runtime->telemetry_enabled) {
    event->monotonic_time_ns = client->request_start_monotonic_ns;
    event->unix_time_ns = client->request_start_unix_ns;
  }
  if ((request->method_len > 0 && event->method_text == NULL)
      || (request->url_len > 0 && event->target == NULL)
      || (first_request_on_connection
          && halo_bridge_copy_transport(event, client) != 0)
      || halo_bridge_copy_request_headers(event, request) != 0) {
    halo_bridge_event_destroy(event);
    halo_bridge_release_request(bridge);
    return -1;
  }

  request_id = ++bridge->next_request_id;
  if (request_id == 0)
    request_id = ++bridge->next_request_id;
  event->request_id = request_id;
  if (halo_bridge_register_client(bridge, client, request_id) != 0) {
    halo_bridge_release_request(bridge);
    halo_bridge_event_destroy(event);
    return -1;
  }
  client->bridge_request_id = request_id;
  if (halo_bridge_push_event(bridge, event) != 0) {
    client->bridge_request_id = 0;
    halo_bridge_unregister_client(bridge, request_id);
    halo_bridge_release_request(bridge);
    halo_bridge_event_destroy(event);
    return -1;
  }
  return 0;
}

int halo_bridge_emit_body_chunk(struct halo_client* client,
                                const char* data,
                                size_t length) {
  struct halo_runtime* runtime;
  struct halo_bridge_event* event;

  if (client == NULL || client->bridge_request_id == 0)
    return -1;
  runtime = client->handle.loop->data;
  event = halo_bridge_simple_event(HALO_BRIDGE_BODY_CHUNK,
                                   client->bridge_request_id);
  if (event == NULL)
    return -1;
  event->payload = halo_bridge_copy_bytes(data, length);
  event->payload_len = length;
  if (length > 0 && event->payload == NULL) {
    halo_bridge_event_destroy(event);
    return -1;
  }
  if (halo_bridge_push_event(runtime->bridge, event) != 0) {
    halo_bridge_event_destroy(event);
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
    halo_bridge_event_destroy(event);
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
  event = calloc(1, sizeof(*event));
  if (event == NULL)
    return;
  event->kind = HALO_BRIDGE_PRE_DISPATCH_RESPONSE;
  event->listener_id = client->listener_ref->bridge_listener_id;
  if (request->method_seen && request->method_len > 0) {
    event->method_text = halo_bridge_copy_bytes(request->method_text,
                                                request->method_len);
    event->method_len = request->method_len;
  }
  else {
    event->method_text = halo_bridge_copy_bytes(unknown_method,
                                                sizeof(unknown_method) - 1);
    event->method_len = sizeof(unknown_method) - 1;
  }
  event->target = halo_bridge_copy_bytes(request->url, request->url_len);
  event->target_len = request->url_len;
  event->http_major = llhttp_get_http_major(&client->parser);
  event->http_minor = llhttp_get_http_minor(&client->parser);
  if (request->http_major != 0) {
    event->http_major = request->http_major;
    event->http_minor = request->http_minor;
  }
  event->is_early_data = client->request_is_early_data;
  event->monotonic_time_ns = end_monotonic_ns;
  if (client->request_start_monotonic_ns != 0)
    event->monotonic_time_ns = client->request_start_monotonic_ns;
  event->unix_time_ns = client->request_start_unix_ns;
  event->auxiliary = end_monotonic_ns;
  event->payload = malloc(HALO_PRE_DISPATCH_RESPONSE_PAYLOAD_SIZE);
  event->payload_len = HALO_PRE_DISPATCH_RESPONSE_PAYLOAD_SIZE;
  if (event->method_text == NULL
      || (request->url_len > 0 && event->target == NULL)
      || event->payload == NULL
      || halo_bridge_copy_request_headers(event, request) != 0
      || halo_bridge_copy_transport(event, client) != 0) {
    halo_bridge_event_destroy(event);
    return;
  }
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
    halo_bridge_event_destroy(event);
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
  event = halo_bridge_simple_event(HALO_BRIDGE_METRICS, query_id);
  if (event == NULL)
    return -1;
  event->payload = halo_bridge_copy_bytes(payload, (size_t) length);
  event->payload_len = (size_t) length;
  if (length > 0 && event->payload == NULL) {
    halo_bridge_event_destroy(event);
    return -1;
  }
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
