#ifndef HALO_BRIDGE_INTERNAL_H
#define HALO_BRIDGE_INTERNAL_H

#include "halo_bridge.h"
#include "halo_runtime_internal.h"
#include "../lib/halo_client_registry.h"

#include <pthread.h>

#define HALO_BRIDGE_REPLAY_CLAIM_BUCKET_COUNT 1024U
#define HALO_BRIDGE_PACKED_HEADER_METADATA_SIZE (2 * sizeof(size_t) + 1)

enum halo_bridge_event_kind {
  HALO_BRIDGE_REQUEST = 0,
  HALO_BRIDGE_BODY_CHUNK = 1,
  HALO_BRIDGE_BODY_END = 2,
  HALO_BRIDGE_RESPONSE_COMPLETE = 3,
  HALO_BRIDGE_DISCONNECTED = 4,
  HALO_BRIDGE_STOPPED = 5,
  HALO_BRIDGE_WRITE_COMPLETE = 6,
  HALO_BRIDGE_METRICS = 7,
  HALO_BRIDGE_REPLAY_CLAIM = 8,
  HALO_BRIDGE_DRAINED = 9,
  HALO_BRIDGE_CONNECTION_CLOSED = 10,
  HALO_BRIDGE_PRE_DISPATCH_RESPONSE = 11
};

enum halo_bridge_command_kind {
  HALO_BRIDGE_SEND_RESPONSE = 0,
  HALO_BRIDGE_READ_BODY = 1,
  HALO_BRIDGE_CLOSE_REQUEST = 2,
  HALO_BRIDGE_STOP = 3,
  HALO_BRIDGE_SEND_INFORMATIONAL = 4,
  HALO_BRIDGE_START_RESPONSE_STREAM = 5,
  HALO_BRIDGE_WRITE_RESPONSE_CHUNK = 6,
  HALO_BRIDGE_FINISH_RESPONSE_STREAM = 7,
  HALO_BRIDGE_READ_METRICS = 8,
  HALO_BRIDGE_RESOLVE_REPLAY_CLAIM = 9,
  HALO_BRIDGE_ABORT_RESPONSE_STREAM = 10,
  HALO_BRIDGE_BEGIN_DRAIN = 11
};

enum halo_bridge_command_status {
  HALO_BRIDGE_COMMAND_ENQUEUED = 0,
  HALO_BRIDGE_COMMAND_FULL = 1,
  HALO_BRIDGE_COMMAND_CLOSED = 2
};

/* Request-only transport metadata. The trailing storage contains, in order,
 * the remote sockaddr, local sockaddr, SNI, ALPN protocol, and cipher name.
 * It occupies an exact-size region of its request event, avoiding worst-case
 * address and TLS buffers in every event. A zero TLS version means plaintext. */
struct halo_bridge_transport {
  uint16_t remote_address_len;
  uint16_t local_address_len;
  uint16_t server_name_len;
  uint16_t application_protocol_len;
  uint16_t cipher_len;
  uint8_t tls_protocol_version;
  unsigned char storage[];
};

/* Present only on request and pre-dispatch response events. Transport metadata,
 * strings, and packed headers share the event's allocation. */
struct halo_bridge_request_data {
  char* method_text;
  size_t method_len;
  char* target;
  size_t target_len;
  char* packed_headers;
  size_t packed_headers_len;
  struct halo_bridge_transport* transport;
  uint8_t http_major;
  uint8_t http_minor;
  uint8_t keep_alive;
  uint8_t body_expected;
  uint8_t is_early_data;
};

/* The event owns one allocation. All optional request/payload pointers refer
 * into its trailing storage and must never be freed independently. */
struct halo_bridge_event {
  enum halo_bridge_event_kind kind;
  uint64_t request_id;
  uint64_t listener_id;
  uint64_t auxiliary;
  uint64_t monotonic_time_ns;
  uint64_t unix_time_ns;
  char* payload;
  size_t payload_len;
  struct halo_bridge_event* next;
  struct halo_bridge_request_data* request;
};

struct halo_bridge_command {
  enum halo_bridge_command_kind kind;
  uint64_t request_id;
  char* payload;
  size_t payload_len;
  struct halo_bridge_command* next;
};

/* The event side is produced by the reactor and consumed by Eio. The command
 * side has the opposite ownership direction. Each queue has its own mutex so
 * neither domain holds a lock while performing reactor or OCaml work. The
 * request and replay-claim registries and their ID counters are reactor-domain
 * state. */
struct halo_bridge {
  enum halo_handle_kind kind;
  struct halo_runtime* runtime;
  pthread_mutex_t event_mutex;
  pthread_mutex_t command_mutex;
  int event_mutex_initialized;
  int command_mutex_initialized;
  int event_pipe[2];
  uv_async_t command_async;
  int reactor_running;
  size_t request_capacity;
  /* Includes terminal events that Eio has not consumed yet. */
  size_t request_count;
  size_t event_capacity;
  size_t event_count;
  struct halo_bridge_event* event_head;
  struct halo_bridge_event* event_tail;
  size_t command_capacity;
  size_t command_count;
  int accepting_commands;
  struct halo_bridge_command* command_head;
  struct halo_bridge_command* command_tail;
  uint64_t next_request_id;
  uint64_t next_connection_id;
  struct halo_client_registry requests;
  uint64_t next_replay_claim_id;
  struct halo_client_registry replay_claims;
};

char* halo_bridge_copy_bytes(const char* source, size_t length);

int halo_bridge_push_event(struct halo_bridge* bridge,
                           struct halo_bridge_event* event);
struct halo_bridge_event* halo_bridge_pop_event(struct halo_bridge* bridge);

void halo_bridge_command_destroy(struct halo_bridge_command* command);
void halo_bridge_command_async_cb(uv_async_t* async);
int halo_bridge_enqueue_command(struct halo_bridge* bridge,
                                enum halo_bridge_command_kind kind,
                                uint64_t request_id,
                                const char* payload,
                                size_t payload_len);

struct halo_client* halo_bridge_find_client(struct halo_bridge* bridge,
                                            uint64_t request_id);
int halo_bridge_resolve_replay_claim(struct halo_bridge* bridge,
                                    uint64_t claim_id,
                                    int accept);
int halo_bridge_run_reactor(struct halo_runtime* runtime);

#endif
