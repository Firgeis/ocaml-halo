#include "../c/runtime/halo_bridge_internal.h"

#include <assert.h>
#include <stdlib.h>

/* Fail a selected allocation across both event construction and registration. */
static int allocations_until_failure = -1;
static void* test_malloc(size_t size) {
  if (allocations_until_failure == 0) {
    allocations_until_failure = -1;
    return NULL;
  }
  if (allocations_until_failure > 0)
    allocations_until_failure--;
  return malloc(size);
}
static void* test_calloc(size_t count, size_t size) {
  if (allocations_until_failure == 0) {
    allocations_until_failure = -1;
    return NULL;
  }
  if (allocations_until_failure > 0)
    allocations_until_failure--;
  return calloc(count, size);
}
static void* tracked_payload;
static int payload_frees;
static void test_free(void* ptr) {
  if (ptr != NULL && ptr == tracked_payload)
    payload_frees++;
  free(ptr);
}
#define free test_free
#define malloc test_malloc
#define calloc test_calloc
#include "../c/lib/halo_client_registry.c"
#include "../c/runtime/halo_bridge_events.c"
#include "../c/runtime/halo_bridge_commands.c"
#undef malloc
#undef calloc
#undef free

static int close_count;
int halo_client_bridge_reject_busy(struct halo_client* client) { (void) client; return -42; }
void halo_client_bridge_close(struct halo_client* client) { (void) client; close_count++; }
static int response_calls;
static int response_result;
static enum halo_bridge_command_kind expected_kind;
static int consume_response(struct halo_client* c, char* p, size_t n,
                            enum halo_bridge_command_kind kind) {
  assert(c != NULL && p == tracked_payload && n == 4 && kind == expected_kind);
  response_calls++;
  test_free(p);
  return response_result;
}
int halo_client_bridge_send_response(struct halo_client* c, char* p, size_t n) { return consume_response(c, p, n, HALO_BRIDGE_SEND_RESPONSE); }
int halo_client_bridge_send_informational(struct halo_client* c, char* p, size_t n) { return consume_response(c, p, n, HALO_BRIDGE_SEND_INFORMATIONAL); }
int halo_client_bridge_start_response_stream(struct halo_client* c, char* p, size_t n) { return consume_response(c, p, n, HALO_BRIDGE_START_RESPONSE_STREAM); }
int halo_client_bridge_write_response_chunk(struct halo_client* c, char* p, size_t n) { return consume_response(c, p, n, HALO_BRIDGE_WRITE_RESPONSE_CHUNK); }
int halo_client_bridge_finish_response_stream(struct halo_client* c) { (void)c; return 0; }
int halo_client_bridge_abort_response_stream(struct halo_client* c) { (void)c; return 0; }
int halo_client_bridge_request_body(struct halo_client* c) { (void)c; return 0; }
void halo_runtime_begin_shutdown(struct halo_runtime* r) { (void)r; }
void halo_runtime_begin_drain(struct halo_runtime* r) { (void)r; }
int uv_async_send(uv_async_t* h) { (void)h; return 0; }
char* halo_bridge_copy_bytes(const char* p, size_t n) { (void)p; (void)n; return NULL; }
int halo_client_bridge_resolve_replay_claim(struct halo_client* client, uint64_t id, int accept) {
  (void) client; (void) id; (void) accept; return 0;
}
void halo_runtime_controller_tick(struct halo_runtime* runtime) { (void) runtime; }
uint8_t llhttp_get_http_major(llhttp_t* parser) { (void) parser; return 1; }
uint8_t llhttp_get_http_minor(llhttp_t* parser) { (void) parser; return 1; }
int uv_tcp_getpeername(const uv_tcp_t* h, struct sockaddr* a, int* n) { (void) h; (void) a; (void) n; return -1; }
int uv_tcp_getsockname(const uv_tcp_t* h, struct sockaddr* a, int* n) { (void) h; (void) a; (void) n; return -1; }
uint64_t uv_hrtime(void) { return 0; }
int s2n_connection_get_actual_protocol_version(struct s2n_connection* c) { (void) c; return 0; }
const char* s2n_get_server_name(struct s2n_connection* c) { (void) c; return NULL; }
const char* s2n_get_application_protocol(struct s2n_connection* c) { (void) c; return NULL; }
const char* s2n_connection_get_cipher(struct s2n_connection* c) { (void) c; return NULL; }

static void consume(struct halo_bridge* bridge, enum halo_bridge_event_kind kind, uint64_t id) {
  struct halo_bridge_event* event = halo_bridge_pop_event(bridge);
  assert(event != NULL && event->kind == kind && event->request_id == id);
  free(event);
}

static void close_command(struct halo_bridge* bridge, uint64_t id) {
  struct halo_bridge_command* command = calloc(1, sizeof(*command));
  assert(command != NULL && bridge->command_head == NULL);
  command->kind = HALO_BRIDGE_CLOSE_REQUEST;
  command->request_id = id;
  bridge->command_head = bridge->command_tail = command;
  bridge->command_count = 1;
  bridge->command_async.data = bridge;
  halo_bridge_command_async_cb(&bridge->command_async);
}

static void response_command(struct halo_bridge* bridge,
                             enum halo_bridge_command_kind kind,
                             uint64_t id, int result, int present) {
  struct halo_bridge_command* command = calloc(1, sizeof(*command));
  int old_closes = close_count;
  assert(command != NULL && bridge->command_head == NULL);
  command->payload = malloc(4);
  assert(command->payload != NULL);
  tracked_payload = command->payload;
  payload_frees = response_calls = 0;
  expected_kind = kind;
  response_result = result;
  command->payload_len = 4;
  command->kind = kind;
  command->request_id = id;
  bridge->command_head = bridge->command_tail = command;
  bridge->command_count = 1;
  bridge->command_async.data = bridge;
  halo_bridge_command_async_cb(&bridge->command_async);
  assert(payload_frees == 1 && response_calls == present);
  assert(close_count == old_closes + (present && result != 0));
  tracked_payload = NULL;
}

int main(void) {
  struct halo_bridge bridge = {0};
  struct halo_runtime runtime = {0};
  struct halo_listener_ref listener = {0};
  struct halo_client a = {0}, b = {0}, c = {0};
  uint64_t first, second, third;

  assert(pthread_mutex_init(&bridge.event_mutex, NULL) == 0);
  assert(pthread_mutex_init(&bridge.command_mutex, NULL) == 0);
  bridge.event_pipe[1] = -1;
  bridge.accepting_commands = 1;
  bridge.command_capacity = 1;
  {
    char head[] = "head";
    char body[] = "body";
    struct halo_bridge_command* command;
    assert(halo_bridge_enqueue_response_parts(&bridge, 42, head, 4, body, 4)
           == HALO_BRIDGE_COMMAND_ENQUEUED);
    head[0] = 'X';
    body[0] = 'Y';
    assert(halo_bridge_enqueue_response_parts(&bridge, 43, head, 4, body, 4)
           == HALO_BRIDGE_COMMAND_FULL);
    command = halo_bridge_pop_command(&bridge);
    assert(command != NULL && command->request_id == 42 && command->payload_len == 8);
    assert(memcmp(command->payload, "headbody", 8) == 0);
    halo_bridge_command_destroy(command);
    assert(halo_bridge_enqueue_command(&bridge, HALO_BRIDGE_WRITE_RESPONSE_CHUNK,
                                       42, "abc", 3) == HALO_BRIDGE_COMMAND_ENQUEUED);
    command = halo_bridge_pop_command(&bridge);
    assert(command != NULL && command->payload_len == 8);
    assert(memcmp(command->payload, "3\r\nabc\r\n", 8) == 0);
    halo_bridge_command_destroy(command);
    allocations_until_failure = 1;
    assert(halo_bridge_enqueue_response_parts(&bridge, 42, head, 4, body, 4)
           == UV_ENOMEM);
    assert(bridge.command_count == 0);
    bridge.accepting_commands = 0;
    assert(halo_bridge_enqueue_response_parts(&bridge, 42, head, 4, body, 4)
           == HALO_BRIDGE_COMMAND_CLOSED);
  }
  bridge.request_capacity = 2;
  bridge.event_capacity = 8;
  assert(halo_client_registry_init(&bridge.requests, 2) == 0);
  assert(halo_client_registry_init(&bridge.replay_claims, 2) == 0);
  runtime.bridge = &bridge;
  runtime.loop.data = &runtime;
  a.handle.loop = b.handle.loop = c.handle.loop = &runtime.loop;
  a.listener_ref = b.listener_ref = c.listener_ref = &listener;
  /* Existing keep-alive connections do not need socket metadata here. */
  a.bridge_connection_id = b.bridge_connection_id = c.bridge_connection_id = 1;

  assert(halo_bridge_dispatch_request(&a) == 0);
  first = a.bridge_request_id;
  assert(halo_bridge_dispatch_request(&b) == 0);
  second = b.bridge_request_id;
  assert(first != second && halo_bridge_find_client(&bridge, first) == &a);
  assert(halo_bridge_find_client(&bridge, second) == &b);
  assert(halo_client_registry_insert(&bridge.replay_claims, first, &c) == 0);
  assert(halo_bridge_find_client(&bridge, first) == &a);
  assert(halo_bridge_dispatch_request(&c) == -42);
  consume(&bridge, HALO_BRIDGE_REQUEST, first);
  consume(&bridge, HALO_BRIDGE_REQUEST, second);
  halo_bridge_complete_request(&a);
  assert(a.bridge_request_id == 0 && halo_bridge_find_client(&bridge, first) == NULL);
  assert(bridge.request_count == 2 && halo_bridge_dispatch_request(&c) == -42);
  consume(&bridge, HALO_BRIDGE_RESPONSE_COMPLETE, first);
  assert(bridge.request_count == 1);
  assert(halo_bridge_dispatch_request(&a) == 0);
  third = a.bridge_request_id;
  assert(third != first && halo_bridge_find_client(&bridge, first) == NULL);
  assert(halo_bridge_find_client(&bridge, third) == &a);
  {
    enum halo_bridge_command_kind kinds[] = {
      HALO_BRIDGE_SEND_RESPONSE, HALO_BRIDGE_SEND_INFORMATIONAL,
      HALO_BRIDGE_START_RESPONSE_STREAM, HALO_BRIDGE_WRITE_RESPONSE_CHUNK
    };
    size_t i;
    for (i = 0; i < sizeof(kinds) / sizeof(kinds[0]); i++) {
      response_command(&bridge, kinds[i], third, 0, 1);
      response_command(&bridge, kinds[i], third, -1, 1);
      response_command(&bridge, kinds[i], first, 0, 0);
      response_command(&bridge, kinds[i], 0, 0, 0);
    }
    close_count = 0;
  }
  close_command(&bridge, first);
  assert(close_count == 0);
  close_command(&bridge, third);
  assert(close_count == 1);
  consume(&bridge, HALO_BRIDGE_REQUEST, third);
  halo_bridge_disconnect_request(&b);
  assert(halo_bridge_find_client(&bridge, second) == NULL && bridge.request_count == 2);
  close_command(&bridge, second);
  assert(close_count == 1);
  consume(&bridge, HALO_BRIDGE_DISCONNECTED, second);
  halo_bridge_complete_request(&a);
  consume(&bridge, HALO_BRIDGE_RESPONSE_COMPLETE, third);
  assert(bridge.request_count == 0);

  /* Request-event and registry-entry allocation failures release exactly once. */
  allocations_until_failure = 0;
  assert(halo_bridge_dispatch_request(&a) == -1 && bridge.request_count == 0);
  allocations_until_failure = 1;
  assert(halo_bridge_dispatch_request(&a) == -1 && bridge.request_count == 0);
  assert(a.bridge_request_id == 0 && halo_bridge_find_client(&bridge, bridge.next_request_id) == NULL);

  /* Failed publication rolls back the ID, mapping, and reservation. */
  bridge.event_capacity = 0;
  assert(halo_bridge_dispatch_request(&a) == -1 && bridge.request_count == 0);
  assert(a.bridge_request_id == 0 && halo_bridge_find_client(&bridge, bridge.next_request_id) == NULL);
  bridge.event_capacity = 1;
  assert(halo_bridge_dispatch_request(&a) == 0);
  first = a.bridge_request_id;
  /* Terminal events bypass the ordinary queue limit. */
  halo_bridge_complete_request(&a);
  assert(bridge.event_count == 2 && bridge.request_count == 1);
  consume(&bridge, HALO_BRIDGE_REQUEST, first);
  assert(bridge.request_count == 1);
  consume(&bridge, HALO_BRIDGE_RESPONSE_COMPLETE, first);
  assert(bridge.request_count == 0);

  assert(halo_bridge_dispatch_request(&a) == 0);
  first = a.bridge_request_id;
  consume(&bridge, HALO_BRIDGE_REQUEST, first);
  allocations_until_failure = 0;
  halo_bridge_disconnect_request(&a);
  assert(bridge.request_count == 0 && bridge.event_count == 0);
  assert(a.bridge_request_id == 0 && halo_bridge_find_client(&bridge, first) == NULL);

  halo_client_registry_destroy(&bridge.requests);
  halo_client_registry_destroy(&bridge.replay_claims);
  assert(pthread_mutex_destroy(&bridge.event_mutex) == 0);
  assert(pthread_mutex_destroy(&bridge.command_mutex) == 0);
  return 0;
}
