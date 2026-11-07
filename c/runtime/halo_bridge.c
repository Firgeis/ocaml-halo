#include "halo_bridge_internal.h"

#include <fcntl.h>
#include <unistd.h>

char* halo_bridge_copy_bytes(const char* source, size_t length) {
  char* copy;

  if (length == 0)
    return NULL;
  copy = malloc(length);
  if (copy != NULL)
    memcpy(copy, source, length);
  return copy;
}

static void halo_bridge_release_partial(struct halo_bridge* bridge) {
  struct halo_runtime temporary;

  memset(&temporary, 0, sizeof(temporary));
  temporary.bridge = bridge;
  halo_bridge_destroy(&temporary);
}

int halo_bridge_enable(struct halo_runtime* runtime,
                       size_t event_capacity,
                       size_t command_capacity,
                       int telemetry_enabled) {
  struct halo_bridge* bridge;
  int flags;
  int code;

  if (runtime == NULL || runtime->bridge != NULL
      || event_capacity == 0 || event_capacity > (SIZE_MAX - 1) / 2
      || command_capacity == 0)
    return UV_EINVAL;

  bridge = calloc(1, sizeof(*bridge));
  if (bridge == NULL)
    return UV_ENOMEM;
  bridge->kind = HALO_HANDLE_BRIDGE;
  bridge->runtime = runtime;
  bridge->event_pipe[0] = -1;
  bridge->event_pipe[1] = -1;
  bridge->request_capacity = event_capacity;
  bridge->event_capacity = (event_capacity * 2) + 1;
  bridge->command_capacity = command_capacity;
  bridge->accepting_commands = 1;
  runtime->telemetry_enabled = telemetry_enabled != 0;

  if (pthread_mutex_init(&bridge->event_mutex, NULL) != 0) {
    free(bridge);
    return UV_ENOMEM;
  }
  bridge->event_mutex_initialized = 1;
  if (pthread_mutex_init(&bridge->command_mutex, NULL) != 0) {
    halo_bridge_release_partial(bridge);
    return UV_ENOMEM;
  }
  bridge->command_mutex_initialized = 1;

  if (halo_client_registry_init(&bridge->requests, bridge->request_capacity) != 0
      || halo_client_registry_init(&bridge->replay_claims,
                                   HALO_BRIDGE_REPLAY_CLAIM_BUCKET_COUNT) != 0) {
    halo_bridge_release_partial(bridge);
    return UV_ENOMEM;
  }

  if (pipe(bridge->event_pipe) != 0) {
    code = uv_translate_sys_error(errno);
    halo_bridge_release_partial(bridge);
    return code;
  }
  flags = fcntl(bridge->event_pipe[0], F_GETFL, 0);
  if (flags < 0 || fcntl(bridge->event_pipe[0], F_SETFL, flags | O_NONBLOCK) < 0) {
    code = uv_translate_sys_error(errno);
    halo_bridge_release_partial(bridge);
    return code;
  }
  flags = fcntl(bridge->event_pipe[1], F_GETFL, 0);
  if (flags < 0 || fcntl(bridge->event_pipe[1], F_SETFL, flags | O_NONBLOCK) < 0) {
    code = uv_translate_sys_error(errno);
    halo_bridge_release_partial(bridge);
    return code;
  }
  (void) fcntl(bridge->event_pipe[0], F_SETFD, FD_CLOEXEC);
  (void) fcntl(bridge->event_pipe[1], F_SETFD, FD_CLOEXEC);

  code = uv_async_init(&runtime->loop,
                       &bridge->command_async,
                       halo_bridge_command_async_cb);
  if (code != 0) {
    halo_bridge_release_partial(bridge);
    return code;
  }
  bridge->command_async.data = bridge;
  runtime->bridge = bridge;
  return 0;
}

void halo_bridge_destroy(struct halo_runtime* runtime) {
  struct halo_bridge* bridge;
  struct halo_bridge_event* event;
  struct halo_bridge_command* command;

  if (runtime == NULL || runtime->bridge == NULL)
    return;
  bridge = runtime->bridge;
  runtime->bridge = NULL;

  while ((event = bridge->event_head) != NULL) {
    bridge->event_head = event->next;
    free(event);
  }
  while ((command = bridge->command_head) != NULL) {
    bridge->command_head = command->next;
    halo_bridge_command_destroy(command);
  }
  halo_client_registry_destroy(&bridge->requests);
  halo_client_registry_destroy(&bridge->replay_claims);
  if (bridge->event_pipe[0] >= 0)
    close(bridge->event_pipe[0]);
  if (bridge->event_pipe[1] >= 0)
    close(bridge->event_pipe[1]);
  if (bridge->command_mutex_initialized)
    pthread_mutex_destroy(&bridge->command_mutex);
  if (bridge->event_mutex_initialized)
    pthread_mutex_destroy(&bridge->event_mutex);
  free(bridge);
}

void halo_bridge_begin_shutdown(struct halo_runtime* runtime) {
  struct halo_bridge* bridge;

  if (runtime == NULL || runtime->bridge == NULL)
    return;
  bridge = runtime->bridge;
  pthread_mutex_lock(&bridge->command_mutex);
  bridge->accepting_commands = 0;
  pthread_mutex_unlock(&bridge->command_mutex);
}

int halo_bridge_run_reactor(struct halo_runtime* runtime) {
  struct halo_bridge* bridge;

  if (runtime == NULL || runtime->bridge == NULL)
    return UV_EINVAL;
  bridge = runtime->bridge;
  if (bridge->reactor_running)
    return UV_EBUSY;
  bridge->reactor_running = 1;
  uv_run(&runtime->loop, UV_RUN_DEFAULT);
  halo_bridge_mark_stopped(runtime);
  bridge->reactor_running = 0;
  return 0;
}
