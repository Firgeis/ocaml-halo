#include "halo_bridge_internal.h"

#include <inttypes.h>
#include <unistd.h>

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
  free(event->payload);
  free(event);
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

int halo_bridge_dispatch_request(struct halo_client* client) {
  struct halo_runtime* runtime;
  struct halo_bridge* bridge;
  struct halo_bridge_event* event;
  struct halo_request* request;
  size_t index;
  uint64_t request_id;

  if (client == NULL || client->handle.loop == NULL)
    return -1;
  runtime = client->handle.loop->data;
  bridge = runtime != NULL ? runtime->bridge : NULL;
  if (bridge == NULL || client->bridge_request_id != 0)
    return -1;

  request = &client->request;
  event = calloc(1, sizeof(*event));
  if (event == NULL)
    return -1;
  event->kind = HALO_BRIDGE_REQUEST;
  event->listener_id = client->listener_ref->bridge_listener_id;
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
  if ((request->method_len > 0 && event->method_text == NULL)
      || (request->url_len > 0 && event->target == NULL)) {
    halo_bridge_event_destroy(event);
    return -1;
  }

  if (request->header_count > 0) {
    size_t output_index;

    event->headers = calloc(request->header_count, sizeof(*event->headers));
    if (event->headers == NULL) {
      halo_bridge_event_destroy(event);
      return -1;
    }
    output_index = 0;
    for (index = 0; index < request->header_count; index++) {
      const struct halo_header* source;
      struct halo_bridge_header* destination;

      source = &request->headers[index];
      destination = &event->headers[output_index++];
      destination->name = halo_bridge_copy_bytes(source->name, source->name_len);
      destination->name_len = source->name_len;
      destination->value = halo_bridge_copy_bytes(source->value,
                                                  source->value_len);
      destination->value_len = source->value_len;
      destination->is_hop_by_hop = source->is_hop_by_hop;
      event->header_count = output_index;
      if ((source->name_len > 0 && destination->name == NULL)
          || (source->value_len > 0 && destination->value == NULL)) {
        halo_bridge_event_destroy(event);
        return -1;
      }
    }
  }

  request_id = ++bridge->next_request_id;
  if (request_id == 0)
    request_id = ++bridge->next_request_id;
  event->request_id = request_id;
  if (halo_bridge_reserve_request(bridge) != 0) {
    halo_bridge_event_destroy(event);
    return halo_client_bridge_reject_busy(client);
  }
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
  halo_bridge_push_required_event(bridge, event);
}

void halo_bridge_complete_request(struct halo_client* client) {
  halo_bridge_finish_request(client, HALO_BRIDGE_RESPONSE_COMPLETE);
}

void halo_bridge_disconnect_request(struct halo_client* client) {
  halo_bridge_finish_request(client, HALO_BRIDGE_DISCONNECTED);
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
  int length;

  if (runtime == NULL || runtime->bridge == NULL)
    return -1;
  halo_runtime_controller_tick(runtime);
  length = snprintf(payload,
                    sizeof(payload),
                    "%.17g %" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64
                    " %" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64
                    " %d %d",
                    runtime->metrics.utilization,
                    runtime->metrics.active_connections,
                    runtime->metrics.active_requests,
                    runtime->metrics.accepted_connections,
                    runtime->metrics.rejected_connections,
                    runtime->metrics.requests_completed,
                    runtime->metrics.tls_full_handshakes,
                    runtime->metrics.tls_resumed_handshakes,
                    runtime->metrics.tls_handshake_failures,
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
