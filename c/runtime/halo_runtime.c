#include "halo_bridge.h"
#include "halo_runtime_internal.h"

#define HALO_CONTROLLER_INTERVAL_MS_DEFAULT 100
#define HALO_CONTROLLER_BUSY_THRESHOLD_DEFAULT 0.85
#define HALO_CONTROLLER_SATURATED_THRESHOLD_DEFAULT 0.92
#define HALO_CONTROLLER_RECOVERY_THRESHOLD_DEFAULT 0.60
#define HALO_CONTROLLER_BUSY_SAMPLES_DEFAULT 3
#define HALO_CONTROLLER_SATURATED_SAMPLES_DEFAULT 5
#define HALO_CONTROLLER_RECOVERY_SAMPLES_DEFAULT 5

struct halo_runtime_owner {
  struct halo_runtime* handle;
  int loop_initialized;
};

static struct halo_runtime* halo_runtime_from_client(struct halo_client* client) {
  if (client == NULL)
    return NULL;
  if (client->handle.loop == NULL)
    return NULL;
  return (struct halo_runtime*) client->handle.loop->data;
}

static void halo_runtime_maybe_finish_drain(struct halo_runtime* runtime) {
  if (runtime == NULL
      || !runtime->is_draining
      || runtime->is_stopping
      || runtime->drain_complete_emitted
      || runtime->metrics.active_connections != 0)
    return;

  runtime->drain_complete_emitted = 1;
  halo_bridge_mark_drained(runtime);
}

static double halo_runtime_clamp_utilization(double utilization) {
  if (utilization < 0.0)
    return 0.0;
  if (utilization > 1.0)
    return 1.0;
  return utilization;
}

void halo_runtime_controller_tick(struct halo_runtime* runtime) {
  struct halo_runtime_metrics* metrics;
  struct halo_runtime_controller* controller;
  uint64_t now_ns;
  uint64_t idle_time_ns;
  uint64_t elapsed_ns;
  uint64_t idle_delta_ns;
  double utilization;
  int busy;
  int saturated;
  int recovering;

  if (runtime == NULL || runtime->is_closed)
    return;

  metrics = &runtime->metrics;
  controller = &runtime->controller;
  now_ns = uv_hrtime();
  idle_time_ns = uv_metrics_idle_time(&runtime->loop);

  if (metrics->last_sample_time_ns == 0) {
    metrics->last_sample_time_ns = now_ns;
    metrics->last_idle_time_ns = idle_time_ns;
    return;
  }

  elapsed_ns = now_ns - metrics->last_sample_time_ns;
  idle_delta_ns = idle_time_ns - metrics->last_idle_time_ns;
  if (elapsed_ns == 0)
    return;

  utilization =
    halo_runtime_clamp_utilization(1.0 - ((double) idle_delta_ns / (double) elapsed_ns));
  metrics->utilization = utilization;
  metrics->last_sample_time_ns = now_ns;
  metrics->last_idle_time_ns = idle_time_ns;

  busy =
    utilization >= controller->busy_threshold && metrics->active_connections > 0;
  saturated =
    utilization >= controller->saturated_threshold && metrics->active_connections > 0;
  recovering =
    utilization <= controller->recovery_threshold
    && metrics->active_requests == 0;

  controller->busy_samples = busy ? controller->busy_samples + 1 : 0;
  controller->saturated_samples =
    saturated ? controller->saturated_samples + 1 : 0;
  controller->recovery_samples =
    recovering ? controller->recovery_samples + 1 : 0;

  if (controller->backpressure_enabled) {
    if (controller->recovery_samples >= controller->recovery_samples_threshold) {
      controller->backpressure_enabled = 0;
      controller->state = HALO_CONTROLLER_NORMAL;
      return;
    }

    controller->state =
      saturated ? HALO_CONTROLLER_SATURATED : HALO_CONTROLLER_RECOVERING;
    return;
  }

  if (controller->saturated_samples >= controller->saturated_samples_threshold) {
    controller->backpressure_enabled = 1;
    controller->state = HALO_CONTROLLER_SATURATED;
    return;
  }

  if (controller->busy_samples >= controller->busy_samples_threshold) {
    controller->state = HALO_CONTROLLER_BUSY;
    return;
  }

  controller->state = HALO_CONTROLLER_NORMAL;
}

static void halo_runtime_controller_cb(uv_timer_t* timer) {
  struct halo_runtime* runtime;

  runtime = timer->data;
  halo_runtime_controller_tick(runtime);
}

int halo_runtime_configure_controller(struct halo_runtime* runtime,
                                      uint64_t interval_ms,
                                      double busy_threshold,
                                      double saturated_threshold,
                                      double recovery_threshold,
                                      unsigned int busy_samples_threshold,
                                      unsigned int saturated_samples_threshold,
                                      unsigned int recovery_samples_threshold) {
  int code;

  if (runtime == NULL)
    return UV_EINVAL;
  if (interval_ms == 0)
    return UV_EINVAL;
  if (busy_threshold < 0.0 || busy_threshold > 1.0)
    return UV_EINVAL;
  if (saturated_threshold < 0.0 || saturated_threshold > 1.0)
    return UV_EINVAL;
  if (recovery_threshold < 0.0 || recovery_threshold > 1.0)
    return UV_EINVAL;
  if (busy_threshold > saturated_threshold)
    return UV_EINVAL;
  if (recovery_threshold > busy_threshold)
    return UV_EINVAL;
  if (busy_samples_threshold == 0
      || saturated_samples_threshold == 0
      || recovery_samples_threshold == 0)
    return UV_EINVAL;

  runtime->controller.interval_ms = interval_ms;
  runtime->controller.busy_threshold = busy_threshold;
  runtime->controller.saturated_threshold = saturated_threshold;
  runtime->controller.recovery_threshold = recovery_threshold;
  runtime->controller.busy_samples_threshold = busy_samples_threshold;
  runtime->controller.saturated_samples_threshold = saturated_samples_threshold;
  runtime->controller.recovery_samples_threshold = recovery_samples_threshold;
  runtime->controller.busy_samples = 0;
  runtime->controller.saturated_samples = 0;
  runtime->controller.recovery_samples = 0;
  runtime->controller.backpressure_enabled = 0;
  runtime->controller.state = HALO_CONTROLLER_NORMAL;
  runtime->metrics.last_sample_time_ns = 0;
  runtime->metrics.last_idle_time_ns = 0;
  runtime->metrics.utilization = 0.0;

  if (!runtime->controller.timer_initialized)
    return 0;

  uv_timer_stop(&runtime->controller.timer);
  code = uv_timer_start(&runtime->controller.timer,
                        halo_runtime_controller_cb,
                        runtime->controller.interval_ms,
                        runtime->controller.interval_ms);
  return code;
}

static void halo_runtime_owner_init(struct halo_runtime_owner* owner) {
  if (owner == NULL)
    return;

  memset(owner, 0, sizeof(*owner));
}

static void halo_runtime_owner_release(struct halo_runtime_owner* owner) {
  if (owner == NULL || owner->handle == NULL)
    return;

  if (owner->handle->controller.timer_initialized
      && !uv_is_closing((uv_handle_t*) &owner->handle->controller.timer)) {
    uv_timer_stop(&owner->handle->controller.timer);
    uv_close((uv_handle_t*) &owner->handle->controller.timer, NULL);
  }

  if (owner->loop_initialized) {
    uv_run(&owner->handle->loop, UV_RUN_DEFAULT);
    uv_loop_close(&owner->handle->loop);
  }

  free(owner->handle);
  halo_runtime_owner_init(owner);
}

struct halo_runtime** halo_runtime_slot(value runtime) {
  return (struct halo_runtime**) Data_custom_val(runtime);
}

struct halo_listener_ref** halo_listener_slot(value listener) {
  return (struct halo_listener_ref**) Data_custom_val(listener);
}

struct halo_runtime* halo_runtime_get(value runtime) {
  struct halo_runtime* handle;

  handle = *halo_runtime_slot(runtime);
  if (handle == NULL || handle->is_closed)
    caml_failwith("halo runtime is closed");

  return handle;
}

value halo_result_ok(value payload) {
  CAMLparam1(payload);
  CAMLlocal1(result);

  result = caml_alloc(1, 0);
  Store_field(result, 0, payload);
  CAMLreturn(result);
}

value halo_result_error_message(const char* message) {
  CAMLparam0();
  CAMLlocal2(text, result);

  text = caml_copy_string(message);
  result = caml_alloc(1, 1);
  Store_field(result, 0, text);
  CAMLreturn(result);
}

value halo_result_error_uv(const char* operation, int code) {
  CAMLparam0();
  char buffer[256];

  snprintf(buffer, sizeof(buffer), "%s: %s", operation, uv_strerror(code));
  CAMLreturn(halo_result_error_message(buffer));
}

value halo_option_none(void) {
  return Val_int(0);
}

value halo_option_some_message(const char* message) {
  CAMLparam0();
  CAMLlocal2(text, option);

  text = caml_copy_string(message);
  option = caml_alloc(1, 0);
  Store_field(option, 0, text);
  CAMLreturn(option);
}

value halo_option_some_uv(const char* operation, int code) {
  CAMLparam0();
  char buffer[256];

  snprintf(buffer, sizeof(buffer), "%s: %s", operation, uv_strerror(code));
  CAMLreturn(halo_option_some_message(buffer));
}

void halo_listener_ref_release(struct halo_listener_ref* ref) {
  if (ref == NULL)
    return;
  if (ref->ocaml_alive || ref->handle != NULL || ref->active_clients != 0)
    return;
  if (ref->tls_cert_chain != NULL) {
    s2n_cert_chain_and_key_free(ref->tls_cert_chain);
    ref->tls_cert_chain = NULL;
  }
  if (ref->tls_config != NULL) {
    s2n_config_free(ref->tls_config);
    ref->tls_config = NULL;
  }
  free(ref);
}

void halo_runtime_note_connection_opened(struct halo_client* client) {
  struct halo_runtime* runtime;

  runtime = halo_runtime_from_client(client);
  if (runtime == NULL || client == NULL || client->connection_accounted)
    return;

  runtime->metrics.accepted_connections += 1;
  runtime->metrics.active_connections += 1;
  client->connection_accounted = 1;
}

void halo_runtime_note_connection_closed(struct halo_client* client) {
  struct halo_runtime* runtime;

  runtime = halo_runtime_from_client(client);
  if (runtime == NULL
      || client == NULL
      || !client->connection_accounted
      || client->connection_closed_accounted)
    return;

  if (runtime->metrics.active_connections > 0)
    runtime->metrics.active_connections -= 1;
  client->connection_closed_accounted = 1;
  halo_runtime_maybe_finish_drain(runtime);
}

void halo_runtime_note_request_started(struct halo_client* client) {
  struct halo_runtime* runtime;

  runtime = halo_runtime_from_client(client);
  if (runtime == NULL || client == NULL || client->request_in_flight)
    return;

  runtime->metrics.requests_started += 1;
  runtime->metrics.active_requests += 1;
  client->request_in_flight = 1;
}

void halo_runtime_note_request_finished(struct halo_client* client, int completed) {
  struct halo_runtime* runtime;

  runtime = halo_runtime_from_client(client);
  if (runtime == NULL || client == NULL || !client->request_in_flight)
    return;

  if (runtime->metrics.active_requests > 0)
    runtime->metrics.active_requests -= 1;
  if (completed)
    runtime->metrics.requests_completed += 1;
  client->request_in_flight = 0;
}

void halo_runtime_note_tls_handshake(struct halo_client* client, int resumed) {
  struct halo_runtime* runtime;

  runtime = halo_runtime_from_client(client);
  if (runtime == NULL || client == NULL || client->tls_handshake_accounted)
    return;

  if (resumed)
    runtime->metrics.tls_resumed_handshakes += 1;
  else
    runtime->metrics.tls_full_handshakes += 1;
  client->tls_handshake_accounted = 1;
}

void halo_runtime_note_tls_handshake_failure(struct halo_client* client) {
  struct halo_runtime* runtime;

  runtime = halo_runtime_from_client(client);
  if (runtime == NULL
      || client == NULL
      || client->tls_handshake_accounted
      || !client->listener_ref->tls_enabled)
    return;

  runtime->metrics.tls_handshake_failures += 1;
  client->tls_handshake_accounted = 1;
}

static void halo_close_walk_cb(uv_handle_t* handle, void* arg) {
  struct halo_runtime* runtime;
  void* data;
  enum halo_handle_kind kind;
  struct halo_client* client;

  runtime = arg;

  (void)arg;
  if ((uv_handle_t*) &runtime->controller.timer == handle)
    return;

  data = handle->data;
  if (data != NULL) {
    kind = *(enum halo_handle_kind*) data;
    if (kind == HALO_HANDLE_CLIENT) {
      client = (struct halo_client*) data;
      halo_close_client(client);
      return;
    }
  }

  if (!uv_is_closing(handle))
    uv_close(handle, halo_on_handle_closed);
}

static void halo_drain_walk_cb(uv_handle_t* handle, void* arg) {
  struct halo_runtime* runtime;
  void* data;
  enum halo_handle_kind kind;
  struct halo_client* client;

  runtime = arg;
  if ((uv_handle_t*) &runtime->controller.timer == handle)
    return;

  data = handle->data;
  if (data == NULL)
    return;

  kind = *(enum halo_handle_kind*) data;
  if (kind == HALO_HANDLE_LISTENER) {
    if (!uv_is_closing(handle))
      uv_close(handle, halo_on_handle_closed);
    return;
  }

  if (kind == HALO_HANDLE_CLIENT) {
    client = (struct halo_client*) data;
    client->should_keep_alive = 0;
    if (!client->request_in_flight
        && (!client->response_started || client->shutdown_started)
        && !client->interim_response_started)
      halo_close_client(client);
  }
}

void halo_runtime_begin_drain(struct halo_runtime* handle) {
  if (handle->is_closed || handle->is_stopping || handle->is_draining)
    return;

  handle->is_draining = 1;
  uv_walk(&handle->loop, halo_drain_walk_cb, handle);
  halo_runtime_maybe_finish_drain(handle);
}

void halo_runtime_begin_shutdown(struct halo_runtime* handle) {
  if (handle->is_closed || handle->is_stopping)
    return;

  handle->is_stopping = 1;
  halo_bridge_begin_shutdown(handle);

  if (handle->controller.timer_initialized
      && !uv_is_closing((uv_handle_t*) &handle->controller.timer)) {
    uv_timer_stop(&handle->controller.timer);
    uv_close((uv_handle_t*) &handle->controller.timer, NULL);
  }

  uv_walk(&handle->loop, halo_close_walk_cb, handle);

}

static void halo_runtime_destroy(struct halo_runtime* handle) {
  int code;

  if (handle->is_closed)
    return;

  halo_runtime_begin_shutdown(handle);

  code = uv_loop_close(&handle->loop);
  while (code == UV_EBUSY) {
    uv_run(&handle->loop, UV_RUN_DEFAULT);
    code = uv_loop_close(&handle->loop);
  }

  if (code != 0)
    return;

  handle->is_closed = 1;
  halo_bridge_destroy(handle);
}

static void halo_runtime_finalize(value runtime) {
  struct halo_runtime** slot;
  struct halo_runtime* handle;

  slot = halo_runtime_slot(runtime);
  handle = *slot;
  if (handle == NULL)
    return;

  *slot = NULL;
  if (handle->is_closed)
    free(handle);
}

static struct custom_operations halo_runtime_ops = {
  "ocaml_halo.runtime",
  halo_runtime_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static void halo_listener_finalize(value listener) {
  struct halo_listener_ref** slot;
  struct halo_listener_ref* ref;

  slot = halo_listener_slot(listener);
  ref = *slot;
  if (ref == NULL)
    return;

  *slot = NULL;
  ref->ocaml_alive = 0;
  if (ref->handle != NULL && !uv_is_closing((uv_handle_t*) &ref->handle->server))
    uv_close((uv_handle_t*) &ref->handle->server, halo_on_handle_closed);
  halo_listener_ref_release(ref);
}

static struct custom_operations halo_listener_ops = {
  "ocaml_halo.server",
  halo_listener_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

value halo_listener_alloc(struct halo_listener_ref* ref) {
  value listener;

  listener = caml_alloc_custom(&halo_listener_ops, sizeof(struct halo_listener_ref*), 0, 1);
  *halo_listener_slot(listener) = ref;
  return listener;
}

CAMLprim value ocaml_halo_server_release(value listener) {
  CAMLparam1(listener);
  struct halo_listener_ref** slot;
  struct halo_listener_ref* ref;

  slot = halo_listener_slot(listener);
  ref = *slot;
  if (ref == NULL)
    CAMLreturn(Val_unit);

  *slot = NULL;
  ref->ocaml_alive = 0;
  halo_listener_ref_release(ref);
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_halo_runtime_backend_name(value unit) {
  CAMLparam1(unit);
  CAMLreturn(caml_copy_string(uv_version_string()));
}

static value halo_runtime_create_internal(void) {
  CAMLparam0();
  CAMLlocal1(runtime);
  struct halo_runtime_owner owner;
  struct halo_runtime* handle;
  int code;

  halo_runtime_owner_init(&owner);
  handle = malloc(sizeof(*handle));
  if (handle == NULL)
    CAMLreturn(halo_result_error_message("out of memory"));

  owner.handle = handle;
  handle->is_closed = 0;
  handle->is_draining = 0;
  handle->is_stopping = 0;
  handle->drain_complete_emitted = 0;
  handle->bridge = NULL;
  handle->next_listener_id = 0;
  memset(&handle->metrics, 0, sizeof(handle->metrics));
  memset(&handle->controller, 0, sizeof(handle->controller));
  handle->controller.state = HALO_CONTROLLER_NORMAL;

  code = uv_loop_init(&handle->loop);
  if (code != 0) {
    CAMLreturn(halo_result_error_uv("uv_loop_init", code));
  }
  owner.loop_initialized = 1;

  code = uv_loop_configure(&handle->loop, UV_METRICS_IDLE_TIME);
  if (code != 0 && code != UV_ENOSYS) {
    halo_runtime_owner_release(&owner);
    CAMLreturn(halo_result_error_uv("uv_loop_configure", code));
  }

  handle->loop.data = handle;

  code = uv_timer_init(&handle->loop, &handle->controller.timer);
  if (code != 0) {
    halo_runtime_owner_release(&owner);
    CAMLreturn(halo_result_error_uv("uv_timer_init", code));
  }

  handle->controller.timer.data = handle;
  handle->controller.timer_initialized = 1;

  code = halo_runtime_configure_controller(handle,
                                           HALO_CONTROLLER_INTERVAL_MS_DEFAULT,
                                           HALO_CONTROLLER_BUSY_THRESHOLD_DEFAULT,
                                           HALO_CONTROLLER_SATURATED_THRESHOLD_DEFAULT,
                                           HALO_CONTROLLER_RECOVERY_THRESHOLD_DEFAULT,
                                           HALO_CONTROLLER_BUSY_SAMPLES_DEFAULT,
                                           HALO_CONTROLLER_SATURATED_SAMPLES_DEFAULT,
                                           HALO_CONTROLLER_RECOVERY_SAMPLES_DEFAULT);
  if (code != 0) {
    halo_runtime_owner_release(&owner);
    CAMLreturn(halo_result_error_uv("uv_timer_start", code));
  }

  runtime = caml_alloc_custom(&halo_runtime_ops, sizeof(struct halo_runtime*), 0, 1);
  *halo_runtime_slot(runtime) = handle;
  owner.handle = NULL;
  CAMLreturn(halo_result_ok(runtime));
}

CAMLprim value ocaml_halo_bridge_runtime_create(value unit) {
  CAMLparam1(unit);
  CAMLreturn(halo_runtime_create_internal());
}

CAMLprim value ocaml_halo_runtime_configure_controller(value runtime,
                                                       value interval_ms,
                                                       value busy_threshold,
                                                       value saturated_threshold,
                                                       value recovery_threshold,
                                                       value busy_samples,
                                                       value saturated_samples,
                                                       value recovery_samples) {
  CAMLparam5(runtime,
             interval_ms,
             busy_threshold,
             saturated_threshold,
             recovery_threshold);
  CAMLxparam3(busy_samples, saturated_samples, recovery_samples);
  struct halo_runtime* handle;
  int code;

  handle = halo_runtime_get(runtime);
  code = halo_runtime_configure_controller(handle,
                                           (uint64_t) Int_val(interval_ms),
                                           Double_val(busy_threshold),
                                           Double_val(saturated_threshold),
                                           Double_val(recovery_threshold),
                                           (unsigned int) Int_val(busy_samples),
                                           (unsigned int) Int_val(saturated_samples),
                                           (unsigned int) Int_val(recovery_samples));
  if (code != 0)
    CAMLreturn(halo_option_some_uv("halo_runtime_configure_controller", code));

  CAMLreturn(halo_option_none());
}

CAMLprim value ocaml_halo_runtime_configure_controller_bytecode(value* argv, int argn) {
  (void) argn;
  return ocaml_halo_runtime_configure_controller(argv[0],
                                                 argv[1],
                                                 argv[2],
                                                 argv[3],
                                                 argv[4],
                                                 argv[5],
                                                 argv[6],
                                                 argv[7]);
}

CAMLprim value ocaml_halo_runtime_close(value runtime) {
  CAMLparam1(runtime);
  struct halo_runtime** slot;
  struct halo_runtime* handle;

  slot = halo_runtime_slot(runtime);
  handle = *slot;
  if (handle == NULL)
    CAMLreturn(halo_option_none());

  halo_runtime_destroy(handle);
  if (!handle->is_closed)
    CAMLreturn(halo_option_some_message("uv_loop_close: runtime still has active handles"));

  free(handle);
  *slot = NULL;
  CAMLreturn(halo_option_none());
}
