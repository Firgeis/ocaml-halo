#include "halo_bridge_internal.h"

void halo_bridge_command_destroy(struct halo_bridge_command* command) {
  if (command == NULL)
    return;
  free(command->payload);
  free(command);
}

static struct halo_bridge_command* halo_bridge_pop_command(
  struct halo_bridge* bridge
) {
  struct halo_bridge_command* command;

  pthread_mutex_lock(&bridge->command_mutex);
  command = bridge->command_head;
  if (command != NULL) {
    bridge->command_head = command->next;
    if (bridge->command_head == NULL)
      bridge->command_tail = NULL;
    bridge->command_count -= 1;
    command->next = NULL;
  }
  pthread_mutex_unlock(&bridge->command_mutex);
  return command;
}

void halo_bridge_command_async_cb(uv_async_t* async) {
  struct halo_bridge* bridge;
  struct halo_bridge_command* command;

  bridge = async != NULL ? async->data : NULL;
  if (bridge == NULL)
    return;

  while ((command = halo_bridge_pop_command(bridge)) != NULL) {
    struct halo_client* client;

    client = halo_bridge_find_client(bridge, command->request_id);
    switch (command->kind) {
      case HALO_BRIDGE_SEND_RESPONSE:
        if (client != NULL) {
          char* payload;

          payload = command->payload;
          command->payload = NULL;
          if (halo_client_bridge_send_response(client,
                                               payload,
                                               command->payload_len) != 0)
            halo_client_bridge_close(client);
        }
        break;
      case HALO_BRIDGE_READ_BODY:
        if (client != NULL && halo_client_bridge_request_body(client) != 0)
          halo_client_bridge_close(client);
        break;
      case HALO_BRIDGE_CLOSE_REQUEST:
        if (client != NULL)
          halo_client_bridge_close(client);
        break;
      case HALO_BRIDGE_STOP:
        halo_runtime_begin_shutdown(bridge->runtime);
        break;
      case HALO_BRIDGE_SEND_INFORMATIONAL:
        if (client != NULL
            && halo_client_bridge_send_informational(client,
                                                     command->payload,
                                                     command->payload_len) != 0)
          halo_client_bridge_close(client);
        break;
      case HALO_BRIDGE_START_RESPONSE_STREAM:
        if (client != NULL
            && halo_client_bridge_start_response_stream(client,
                                                        command->payload,
                                                        command->payload_len) != 0)
          halo_client_bridge_close(client);
        break;
      case HALO_BRIDGE_WRITE_RESPONSE_CHUNK:
        if (client != NULL
            && halo_client_bridge_write_response_chunk(client,
                                                       command->payload,
                                                       command->payload_len) != 0)
          halo_client_bridge_close(client);
        break;
      case HALO_BRIDGE_FINISH_RESPONSE_STREAM:
        if (client != NULL
            && halo_client_bridge_finish_response_stream(client) != 0)
          halo_client_bridge_close(client);
        break;
      case HALO_BRIDGE_READ_METRICS:
        (void) halo_bridge_emit_metrics(bridge->runtime, command->request_id);
        break;
      default:
        break;
    }
    halo_bridge_command_destroy(command);
  }
}

int halo_bridge_enqueue_command(struct halo_bridge* bridge,
                                enum halo_bridge_command_kind kind,
                                uint64_t request_id,
                                const char* payload,
                                size_t payload_len) {
  struct halo_bridge_command* command;
  int result;

  if (bridge == NULL)
    return HALO_BRIDGE_COMMAND_CLOSED;
  command = calloc(1, sizeof(*command));
  if (command == NULL)
    return UV_ENOMEM;
  command->kind = kind;
  command->request_id = request_id;
  command->payload = halo_bridge_copy_bytes(payload, payload_len);
  command->payload_len = payload_len;
  if (payload_len > 0 && command->payload == NULL) {
    halo_bridge_command_destroy(command);
    return UV_ENOMEM;
  }

  result = HALO_BRIDGE_COMMAND_FULL;
  pthread_mutex_lock(&bridge->command_mutex);
  if (!bridge->accepting_commands) {
    result = HALO_BRIDGE_COMMAND_CLOSED;
  }
  else if (bridge->command_count < bridge->command_capacity) {
    command->next = NULL;
    if (bridge->command_tail == NULL)
      bridge->command_head = command;
    else
      bridge->command_tail->next = command;
    bridge->command_tail = command;
    bridge->command_count += 1;
    if (kind == HALO_BRIDGE_STOP)
      bridge->accepting_commands = 0;
    uv_async_send(&bridge->command_async);
    result = HALO_BRIDGE_COMMAND_ENQUEUED;
  }
  pthread_mutex_unlock(&bridge->command_mutex);

  if (result != HALO_BRIDGE_COMMAND_ENQUEUED)
    halo_bridge_command_destroy(command);
  return result;
}
