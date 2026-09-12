#ifndef HALO_BRIDGE_H
#define HALO_BRIDGE_H

#include <stddef.h>
#include <stdint.h>

struct halo_client;
struct halo_runtime;

/* Runtime lifecycle. */
int halo_bridge_enable(struct halo_runtime* runtime,
                       size_t event_capacity,
                       size_t command_capacity,
                       int telemetry_enabled);
void halo_bridge_destroy(struct halo_runtime* runtime);
void halo_bridge_begin_shutdown(struct halo_runtime* runtime);
void halo_bridge_mark_stopped(struct halo_runtime* runtime);
void halo_bridge_mark_drained(struct halo_runtime* runtime);

/* Reactor-to-Eio request events. */
int halo_bridge_dispatch_request(struct halo_client* client);
int halo_bridge_emit_body_chunk(struct halo_client* client,
                                const char* data,
                                size_t length);
int halo_bridge_emit_body_end(struct halo_client* client);
void halo_bridge_complete_request(struct halo_client* client);
void halo_bridge_disconnect_request(struct halo_client* client);
void halo_bridge_connection_closed(struct halo_client* client);
int halo_bridge_complete_write(struct halo_client* client);
int halo_bridge_emit_metrics(struct halo_runtime* runtime, uint64_t query_id);
int halo_bridge_dispatch_replay_claim(struct halo_client* client,
                                      const uint8_t* identity,
                                      size_t identity_len,
                                      uint64_t ttl_secs);
void halo_bridge_cancel_replay_claim(struct halo_client* client);

/* Reactor-owned client operations invoked by bridge commands. */
/* Consumes [response], including on failure. */
int halo_client_bridge_send_response(struct halo_client* client,
                                     char* response,
                                     size_t response_len);
int halo_client_bridge_send_informational(struct halo_client* client,
                                          const char* response,
                                          size_t response_len);
int halo_client_bridge_start_response_stream(struct halo_client* client,
                                             const char* response,
                                             size_t response_len);
int halo_client_bridge_write_response_chunk(struct halo_client* client,
                                            const char* response,
                                            size_t response_len);
int halo_client_bridge_finish_response_stream(struct halo_client* client);
int halo_client_bridge_abort_response_stream(struct halo_client* client);
int halo_client_bridge_resolve_replay_claim(struct halo_client* client,
                                            uint64_t claim_id,
                                            int accept);
int halo_client_bridge_reject_busy(struct halo_client* client);
int halo_client_bridge_request_body(struct halo_client* client);
void halo_client_bridge_close(struct halo_client* client);

#endif
