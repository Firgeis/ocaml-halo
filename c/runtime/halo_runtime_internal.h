#ifndef OCAML_HALO_RUNTIME_INTERNAL_H
#define OCAML_HALO_RUNTIME_INTERNAL_H

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <assert.h>
#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include <time.h>
#include "../lib/halo_linear_buffer.h"

#include "../../vendor/llhttp/include/llhttp.h"
#include "../../vendor/libuv/include/uv.h"
#include "../../vendor/s2n-tls/api/s2n.h"

enum halo_handle_kind {
  HALO_HANDLE_LISTENER = 1,
  HALO_HANDLE_CLIENT = 2,
  HALO_HANDLE_BRIDGE = 3
};

enum halo_controller_state {
  HALO_CONTROLLER_NORMAL = 0,
  HALO_CONTROLLER_BUSY = 1,
  HALO_CONTROLLER_SATURATED = 2,
  HALO_CONTROLLER_RECOVERING = 3
};

struct halo_runtime_metrics {
  uint64_t last_sample_time_ns;
  uint64_t last_idle_time_ns;
  double utilization;
  uint64_t accepted_connections;
  uint64_t rejected_connections;
  uint64_t active_connections;
  uint64_t requests_started;
  uint64_t requests_completed;
  uint64_t active_requests;
  uint64_t tls_full_handshakes;
  uint64_t tls_resumed_handshakes;
  uint64_t tls_handshake_failures;
};

struct halo_runtime_controller {
  uv_timer_t timer;
  int timer_initialized;
  enum halo_controller_state state;
  int backpressure_enabled;
  uint64_t interval_ms;
  double busy_threshold;
  double saturated_threshold;
  double recovery_threshold;
  unsigned int busy_samples_threshold;
  unsigned int saturated_samples_threshold;
  unsigned int recovery_samples_threshold;
  unsigned int busy_samples;
  unsigned int saturated_samples;
  unsigned int recovery_samples;
};

struct halo_bridge;
struct halo_bridge_event;

#define HALO_BODY_DELIVERY_BATCH_SIZE (64U * 1024U)

struct halo_runtime {
  uv_loop_t loop;
  int is_closed;
  int is_draining;
  int is_stopping;
  int drain_complete_emitted;
  int telemetry_enabled;
  struct halo_runtime_metrics metrics;
  struct halo_runtime_controller controller;
  struct halo_bridge* bridge;
  uint64_t next_listener_id;
};

struct halo_listener_ref;
struct halo_tls_write_chunk;

struct halo_listener {
  enum halo_handle_kind kind;
  uv_tcp_t server;
  struct halo_listener_ref* ref;
};

struct halo_header {
  char* name;
  size_t name_len;
  char* value;
  size_t value_len;
  unsigned char is_hop_by_hop;
  unsigned char name_borrowed;
  unsigned char value_borrowed;
};

struct halo_connection_option {
  char* name;
  size_t name_len;
};

struct halo_request_storage_block {
  struct halo_request_storage_block* next;
  size_t cap;
  size_t used;
  max_align_t alignment;
  unsigned char data[];
};

struct halo_request_storage {
  struct halo_request_storage_block* head;
  size_t initial_block_size;
};

enum halo_expectation_state {
  HALO_EXPECTATION_NONE = 0,
  HALO_EXPECTATION_100_CONTINUE = 1,
  HALO_EXPECTATION_UNSUPPORTED = 2
};

struct halo_known_headers {
  size_t host_count;
  const char* host_value;
  size_t host_value_len;
  size_t expect_count;
  enum halo_expectation_state expectation;
};

struct halo_request {
  llhttp_method_t method;
  int method_seen;
  int headers_complete;
  int has_content_length;
  int http_major;
  int http_minor;
  uint64_t content_length;
  size_t request_line_bytes;
  size_t header_bytes;
  char* method_text;
  size_t method_len;
  size_t method_cap;
  char* url;
  size_t url_len;
  size_t url_cap;
  struct halo_request_storage storage;
  /* A non-NULL header pointer with cap == 0 borrows llhttp's input buffer. */
  char* current_header_name;
  size_t current_header_name_len;
  size_t current_header_name_cap;
  char* current_header_value;
  size_t current_header_value_len;
  size_t current_header_value_cap;
  struct halo_header* headers;
  size_t header_count;
  size_t header_cap;
  struct halo_connection_option* connection_options;
  size_t connection_option_count;
  size_t connection_option_cap;
  struct halo_known_headers known_headers;
};

struct halo_client {
  enum halo_handle_kind kind;
  uv_tcp_t handle;
  uv_shutdown_t shutdown_req;
  uv_timer_t timer;
  int timer_initialized;
  int handle_closed;
  int timer_closed;
  int response_started;
  int interim_response_started;
  int shutdown_started;
  int message_complete;
  int should_keep_alive;
  /* Nonzero while a final response generated entirely by the reactor is in
   * flight. Parser failures initialize it directly; other pre-dispatch
   * responses set it before writing. */
  int direct_response_status;
  int pending_continue;
  int dispatch_started;
  int dispatch_pending_until_handshake;
  int dispatch_after_headers;
  int awaiting_request_body;
  int body_chunk_delivered;
  /* Reactor-owned until the parser result flushes it to the event queue. */
  struct halo_bridge_event* bridge_body_event;
  size_t bridge_body_capacity;
  int connection_accounted;
  int connection_closed_accounted;
  int request_in_flight;
  int tls_handshake_complete;
  int tls_handshake_accounted;
  int tls_close_started;
  int tls_early_data_complete;
  int request_is_early_data;
  uint64_t request_start_monotonic_ns;
  uint64_t request_start_unix_ns;
  uint64_t bridge_replay_claim_id;
  struct s2n_offered_early_data* tls_pending_early_data;
  size_t body_bytes_read;
  uint64_t bridge_request_id;
  uint64_t bridge_connection_id;
  int bridge_response_in_flight;
  char* paused_data;
  size_t paused_data_len;
  struct halo_linear_buffer tls_input;
  struct halo_tls_write_chunk* tls_capture;
  struct halo_tls_write_chunk* tls_write_spare;
  struct halo_tls_write_chunk* tls_write_head;
  struct halo_tls_write_chunk* tls_write_tail;
  int tls_write_in_flight;
  int timeout_mode;
  struct halo_listener_ref* listener_ref;
  struct halo_request request;
  llhttp_t parser;
  struct s2n_connection* tls_conn;
};

enum halo_timeout_mode {
  HALO_TIMEOUT_NONE = 0,
  HALO_TIMEOUT_REQUEST = 1,
  HALO_TIMEOUT_IDLE = 2
};

enum halo_write_kind {
  HALO_WRITE_EXPECT_CONTINUE = 1,
  HALO_WRITE_DISPATCH_INTERIM = 2,
  HALO_WRITE_RESPONSE_STREAM_START = 3,
  HALO_WRITE_RESPONSE_CHUNK = 4,
  HALO_WRITE_RESPONSE_STREAM_FINISH = 5,
  HALO_WRITE_FINAL = 6
};

struct halo_write_req {
  uv_write_t req;
  uv_buf_t buf;
  enum halo_write_kind kind;
};

struct halo_tls_write_chunk {
  uv_write_t req;
  uv_buf_t buf;
  size_t capacity;
  enum halo_write_kind kind;
  struct halo_tls_write_chunk* next;
};

struct halo_listener_ref {
  struct halo_listener* handle;
  int ocaml_alive;
  size_t active_clients;
  size_t max_request_line_size;
  size_t max_header_size;
  size_t max_body_size;
  int request_timeout_ms;
  int idle_timeout_ms;
  int tls_enabled;
  int tls_max_early_data_size;
  int tls_server_keying_material_lifetime_secs;
  uint64_t tls_replay_ttl_secs;
  struct s2n_config* tls_config;
  struct s2n_cert_chain_and_key* tls_cert_chain;
  uint64_t bridge_listener_id;
};

struct halo_runtime** halo_runtime_slot(value runtime);
struct halo_listener_ref** halo_listener_slot(value listener);

struct halo_runtime* halo_runtime_get(value runtime);

typedef int (*halo_header_token_callback)(const char* token,
                                          size_t token_len,
                                          void* context);

value halo_result_ok(value payload);
value halo_result_error_message(const char* message);
value halo_result_error_uv(const char* operation, int code);
value halo_option_none(void);
value halo_option_some_message(const char* message);
value halo_option_some_uv(const char* operation, int code);
value halo_listener_alloc(struct halo_listener_ref* ref);

void halo_listener_ref_release(struct halo_listener_ref* ref);
/* Returns -1 when unavailable, 0 when already seen, and 1 when claimed. */
int halo_tls_replay_cache_claim(const uint8_t* identity,
                                size_t identity_len,
                                uint64_t ttl_secs);
int halo_request_store_header(struct halo_request* request,
                              const char* name,
                              size_t name_len,
                              const char* value,
                              size_t value_len,
                              struct halo_header** stored_header);
int halo_request_store_connection_option(struct halo_request* request,
                                         const char* name,
                                         size_t name_len);
void halo_request_clear(struct halo_request* request);
void halo_request_free(struct halo_request* request);
int halo_header_name_matches(const struct halo_header* header,
                             const char* name,
                             size_t name_len);
int halo_iterate_comma_separated_header_tokens(const char* value,
                                               size_t value_len,
                                               halo_header_token_callback callback,
                                               void* context,
                                               int* saw_token);
int halo_request_has_data(const struct halo_request* request);
int halo_request_materialize_header_spans(struct halo_request* request);
void halo_request_storage_init(struct halo_request_storage* storage,
                               size_t initial_block_size);
void* halo_request_storage_alloc(struct halo_request_storage* storage,
                                 size_t size,
                                 size_t align);
char* halo_request_storage_strndup(struct halo_request_storage* storage,
                                   const char* src,
                                   size_t len);
void halo_request_storage_reset(struct halo_request_storage* storage);
void halo_request_storage_destroy(struct halo_request_storage* storage);

const llhttp_settings_t* halo_http_settings(void);
void halo_on_handle_closed(uv_handle_t* handle);
void halo_close_client(struct halo_client* client);
void halo_runtime_note_connection_opened(struct halo_client* client);
void halo_runtime_note_connection_closed(struct halo_client* client);
void halo_runtime_note_request_started(struct halo_client* client);
void halo_runtime_note_request_finished(struct halo_client* client, int completed);
void halo_runtime_note_tls_handshake(struct halo_client* client, int resumed);
void halo_runtime_note_tls_handshake_failure(struct halo_client* client);
void halo_runtime_controller_tick(struct halo_runtime* runtime);
void halo_runtime_begin_drain(struct halo_runtime* runtime);
void halo_runtime_begin_shutdown(struct halo_runtime* runtime);
int halo_runtime_configure_controller(struct halo_runtime* runtime,
                                      uint64_t interval_ms,
                                      double busy_threshold,
                                      double saturated_threshold,
                                      double recovery_threshold,
                                      unsigned int busy_samples_threshold,
                                      unsigned int saturated_samples_threshold,
                                      unsigned int recovery_samples_threshold);

#endif
