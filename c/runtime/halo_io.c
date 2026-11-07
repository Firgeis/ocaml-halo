#include "halo_bridge.h"
#include "halo_runtime_internal.h"
#include "halo_utils.h"
#include <errno.h>
#include <inttypes.h>
#include <netinet/in.h>
#include <openssl/sha.h>
#include <signal.h>
#include "s2n_connection.h"

#define HALO_TLS13_REPLAY_TTL_SECS (7ULL * 24ULL * 60ULL * 60ULL)

static const char halo_http_continue_response[] =
  "HTTP/1.1 100 Continue\r\n"
  "\r\n";

static const char halo_internal_server_error_response[] =
  "HTTP/1.1 500 Internal Server Error\r\n"
  "Content-Length: 22\r\n"
  "Connection: close\r\n"
  "\r\n"
  "internal server error\n";

static const char halo_request_timeout_response[] =
  "HTTP/1.1 408 Request Timeout\r\n"
  "Content-Length: 16\r\n"
  "Connection: close\r\n"
  "\r\n"
  "request timeout\n";

static const char halo_service_unavailable_response[] =
  "HTTP/1.1 503 Service Unavailable\r\n"
  "Content-Length: 18\r\n"
  "Connection: close\r\n"
  "\r\n"
  "server overloaded\n";

static int halo_client_start_request_timeout(struct halo_client* client);
static int halo_client_start_idle_timeout(struct halo_client* client);
static void halo_close_with_error_response(uv_stream_t* stream,
                                           struct halo_client* client,
                                           int status,
                                           const char* body);
static int halo_write_response(uv_stream_t* stream,
                               struct halo_client* client,
                               const char* response,
                               size_t response_len);
static int halo_write_buffer(uv_stream_t* stream,
                             struct halo_client* client,
                             const char* response,
                             size_t response_len,
                             enum halo_write_kind kind);
static int halo_write_owned_buffer(uv_stream_t* stream,
                                   struct halo_client* client,
                                   char* response,
                                   size_t response_len,
                                   enum halo_write_kind kind);
static int halo_handle_parser_result(uv_stream_t* stream,
                                     struct halo_client* client,
                                     llhttp_errno_t err);
static int halo_capture_paused_remainder(struct halo_client* client,
                                         const char* buf_base,
                                         size_t buf_len);
static int halo_wait_for_request_body(uv_stream_t* stream,
                                      struct halo_client* client);
static int halo_request_body_expected(const struct halo_client* client);
static int halo_request_body_complete_for_reuse(const struct halo_client* client);
static void halo_read_cb(uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf);
static const char* halo_error_response_body(int status);
static void halo_mark_direct_response(struct halo_client* client, int status);
static void halo_observe_direct_response(struct halo_client* client,
                                         int completed);
static int halo_tls_enabled(const struct halo_client* client);
static int halo_load_file(const char* path, char** contents, size_t* length);
static int halo_tls_prepare_operation(struct halo_client* client);
static int halo_tls_finish_operation(struct halo_client* client, enum halo_write_kind kind);
static void halo_tls_write_chunk_destroy(struct halo_tls_write_chunk* chunk);
static int halo_tls_flush_pending(uv_stream_t* stream, struct halo_client* client);
static int halo_tls_drive_handshake(uv_stream_t* stream, struct halo_client* client);
static int halo_tls_drain_plaintext(uv_stream_t* stream, struct halo_client* client);
static int halo_tls_drain_early_plaintext(uv_stream_t* stream, struct halo_client* client);
static void halo_tls_try_start_shutdown(struct halo_client* client);
static void halo_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf);
static void halo_after_shutdown(uv_shutdown_t* req, int status);
static int halo_start_graceful_close(uv_stream_t* stream, struct halo_client* client);
static int halo_handle_write_kind_complete(uv_stream_t* stream,
                                           struct halo_client* client,
                                           enum halo_write_kind kind);
static int halo_write_internal_server_error_response(uv_stream_t* stream,
                                                     struct halo_client* client);
static int halo_feed_plaintext(uv_stream_t* stream,
                               struct halo_client* client,
                               const char* plaintext,
                               size_t plaintext_len);
static int halo_tls_load_ticket_key_file(struct s2n_config* tls_config,
                                         const char* path,
                                         char* error_message,
                                         size_t error_message_cap);
static int halo_tls_add_ephemeral_ticket_key(struct s2n_config* tls_config);
static int halo_tls_build_server_config(value tls_cert_chain_path,
                                        value tls_private_key_path,
                                        value tls_security_policy,
                                        value tls_minimum_protocol_version,
                                        value tls_alpn_protocols,
                                        value tls_ocsp_staple_path,
                                        value tls_client_auth_mode,
                                        value tls_client_auth_ca_cert_path,
                                        value tls_client_auth_ca_cert_dir,
                                        value tls_session_tickets_enabled,
                                        value tls_session_ticket_keys_path,
                                        value tls_session_ticket_encrypt_decrypt_lifetime_secs,
                                        value tls_session_ticket_decrypt_lifetime_secs,
                                        value tls_session_ticket_initial_count,
                                        value tls_server_keying_material_lifetime_secs,
                                        value tls_max_early_data_size,
                                        struct s2n_config** out_tls_config,
                                        struct s2n_cert_chain_and_key** out_tls_cert_chain,
                                        int* out_max_early_data_size,
                                        int* out_server_keying_material_lifetime_secs,
                                        uint64_t* out_replay_ttl_secs,
                                        char* error_message,
                                        size_t error_message_cap);
static int halo_tls_early_data_cb(struct s2n_connection* conn,
                                  struct s2n_offered_early_data* early_data);

static void halo_tls_global_init(void);

static uv_once_t halo_tls_global_once = UV_ONCE_INIT;
static int halo_tls_global_init_status = 0;
static char halo_tls_global_init_error[256];

struct halo_tls_server_files {
  char* cert_chain_pem;
  size_t cert_chain_pem_len;
  char* private_key_pem;
  size_t private_key_pem_len;
  char* ocsp_data;
  size_t ocsp_data_len;
};

static void halo_tls_global_init(void) {
  signal(SIGPIPE, SIG_IGN);
  if (s2n_init() != S2N_SUCCESS) {
    halo_tls_global_init_status = -1;
    snprintf(halo_tls_global_init_error,
             sizeof(halo_tls_global_init_error),
             "%s",
             s2n_strerror(s2n_errno, "EN"));
  }
}

static void halo_tls_server_files_init(struct halo_tls_server_files* files) {
  if (files == NULL)
    return;

  memset(files, 0, sizeof(*files));
}

static void halo_tls_server_files_release(struct halo_tls_server_files* files) {
  if (files == NULL)
    return;

  free(files->cert_chain_pem);
  free(files->private_key_pem);
  free(files->ocsp_data);
  halo_tls_server_files_init(files);
}

struct halo_listener_pair {
  struct halo_listener* listener;
  struct halo_listener_ref* ref;
};

static void halo_listener_pair_init(struct halo_listener_pair* pair) {
  if (pair == NULL)
    return;

  memset(pair, 0, sizeof(*pair));
}

static void halo_listener_pair_release(struct halo_listener_pair* pair) {
  if (pair == NULL)
    return;

  if (pair->ref != NULL) {
    if (pair->ref->tls_cert_chain != NULL) {
      s2n_cert_chain_and_key_free(pair->ref->tls_cert_chain);
      pair->ref->tls_cert_chain = NULL;
    }
    if (pair->ref->tls_config != NULL) {
      s2n_config_free(pair->ref->tls_config);
      pair->ref->tls_config = NULL;
    }
    free(pair->ref);
    pair->ref = NULL;
  }

  free(pair->listener);
  pair->listener = NULL;
}

static int halo_listener_pair_create(struct halo_listener_pair* pair,
                                     struct halo_runtime* runtime,
                                     value max_request_line_size,
                                     value max_header_size,
                                     value max_body_size,
                                     value request_timeout_ms,
                                     value idle_timeout_ms,
                                     value tls_enabled) {
  if (pair == NULL)
    return -1;

  halo_listener_pair_init(pair);

  pair->listener = malloc(sizeof(*pair->listener));
  if (pair->listener == NULL)
    return -1;

  pair->ref = malloc(sizeof(*pair->ref));
  if (pair->ref == NULL) {
    halo_listener_pair_release(pair);
    return -1;
  }

  pair->listener->kind = HALO_HANDLE_LISTENER;
  pair->listener->ref = pair->ref;
  pair->ref->handle = pair->listener;
  pair->ref->ocaml_alive = 0;
  pair->ref->active_clients = 0;
  pair->ref->max_request_line_size = (size_t) Long_val(max_request_line_size);
  pair->ref->max_header_size = (size_t) Long_val(max_header_size);
  pair->ref->max_body_size = (size_t) Long_val(max_body_size);
  pair->ref->request_timeout_ms = Int_val(request_timeout_ms);
  pair->ref->idle_timeout_ms = Int_val(idle_timeout_ms);
  pair->ref->tls_enabled = Bool_val(tls_enabled);
  pair->ref->tls_max_early_data_size = -1;
  pair->ref->tls_server_keying_material_lifetime_secs = -1;
  pair->ref->tls_replay_ttl_secs = 0;
  pair->ref->tls_config = NULL;
  pair->ref->tls_cert_chain = NULL;
  pair->ref->bridge_listener_id = ++runtime->next_listener_id;
  return 0;
}

static void halo_client_clear_paused_data(struct halo_client* client) {
  if (client == NULL)
    return;

  free(client->paused_data);
  client->paused_data = NULL;
  client->paused_data_len = 0;
}

static int halo_request_body_expected(const struct halo_client* client) {
  return client->request.content_length > 0
    || (client->parser.flags & F_CHUNKED) != 0;
}

static int halo_request_body_complete_for_reuse(const struct halo_client* client) {
  if (!halo_request_body_expected(client))
    return 1;

  if (client->message_complete)
    return 1;

  if ((client->parser.flags & F_CHUNKED) != 0)
    return 0;

  return client->request.has_content_length
    && client->body_bytes_read >= (size_t) client->request.content_length;
}

static int halo_tls_enabled(const struct halo_client* client) {
  return client != NULL
    && client->listener_ref != NULL
    && client->listener_ref->tls_enabled
    && client->tls_conn != NULL;
}

static int halo_tls_parse_ticket_key_line(struct s2n_config* tls_config,
                                          const char* line,
                                          size_t line_number,
                                          char* error_message,
                                          size_t error_message_cap) {
  char* copy;
  char* name_hex;
  char* secret_hex;
  char* intro_text;
  char* endptr;
  uint8_t name[16];
  uint8_t secret[32];
  size_t name_len;
  size_t secret_len;
  unsigned long long intro_time;

  copy = strdup(line);
  if (copy == NULL) {
    snprintf(error_message, error_message_cap, "out of memory");
    return -1;
  }

  name_hex = copy;
  secret_hex = strchr(name_hex, ':');
  if (secret_hex == NULL) {
    snprintf(error_message,
             error_message_cap,
             "invalid tls session ticket key line %zu",
             line_number);
    free(copy);
    return -1;
  }
  *secret_hex = '\0';
  secret_hex += 1;

  intro_text = strchr(secret_hex, ':');
  if (intro_text == NULL || strchr(intro_text + 1, ':') != NULL) {
    snprintf(error_message,
             error_message_cap,
             "invalid tls session ticket key line %zu",
             line_number);
    free(copy);
    return -1;
  }
  *intro_text = '\0';
  intro_text += 1;

  name_hex = halo_trim_in_place(name_hex);
  secret_hex = halo_trim_in_place(secret_hex);
  intro_text = halo_trim_in_place(intro_text);

  if (halo_decode_hex(name_hex, name, sizeof(name), &name_len) != 0
      || name_len == 0
      || name_len > sizeof(name)) {
    snprintf(error_message,
             error_message_cap,
             "invalid tls session ticket key name on line %zu",
             line_number);
    free(copy);
    return -1;
  }

  if (halo_decode_hex(secret_hex, secret, sizeof(secret), &secret_len) != 0
      || secret_len != sizeof(secret)) {
    snprintf(error_message,
             error_message_cap,
             "invalid tls session ticket secret on line %zu",
             line_number);
    free(copy);
    return -1;
  }

  errno = 0;
  intro_time = strtoull(intro_text, &endptr, 10);
  endptr = halo_trim_in_place(endptr);
  if (errno != 0 || intro_text[0] == '\0' || endptr[0] != '\0') {
    snprintf(error_message,
             error_message_cap,
             "invalid tls session ticket intro time on line %zu",
             line_number);
    free(copy);
    return -1;
  }

  if (s2n_config_add_ticket_crypto_key(tls_config,
                                       name,
                                       (uint32_t) name_len,
                                       secret,
                                       (uint32_t) sizeof(secret),
                                       (uint64_t) intro_time) != S2N_SUCCESS) {
    snprintf(error_message,
             error_message_cap,
             "%s",
             s2n_strerror(s2n_errno, "EN"));
    free(copy);
    return -1;
  }

  free(copy);
  return 0;
}

static int halo_tls_load_ticket_key_file(struct s2n_config* tls_config,
                                         const char* path,
                                         char* error_message,
                                         size_t error_message_cap) {
  FILE* file;
  char* line;
  size_t line_cap;
  ssize_t line_len;
  size_t line_number;
  size_t key_count;

  file = fopen(path, "rb");
  if (file == NULL) {
    snprintf(error_message,
             error_message_cap,
             "failed to open tls session ticket key file");
    return -1;
  }

  line = NULL;
  line_cap = 0;
  line_number = 0;
  key_count = 0;
  while ((line_len = getline(&line, &line_cap, file)) >= 0) {
    char* trimmed;

    (void) line_len;
    line_number += 1;
    trimmed = halo_trim_in_place(line);
    if (trimmed[0] == '\0' || trimmed[0] == '#')
      continue;
    if (halo_tls_parse_ticket_key_line(tls_config,
                                       trimmed,
                                       line_number,
                                       error_message,
                                       error_message_cap) != 0) {
      free(line);
      fclose(file);
      return -1;
    }
    key_count += 1;
  }

  free(line);
  fclose(file);
  if (key_count == 0) {
    snprintf(error_message,
             error_message_cap,
             "tls session ticket key file did not contain any keys");
    return -1;
  }
  return 0;
}

static int halo_tls_add_ephemeral_ticket_key(struct s2n_config* tls_config) {
  uint8_t name[16];
  uint8_t secret[32];

  if (uv_random(NULL, NULL, name, sizeof(name), 0, NULL) != 0
      || uv_random(NULL, NULL, secret, sizeof(secret), 0, NULL) != 0) {
    return -1;
  }

  if (s2n_config_add_ticket_crypto_key(tls_config,
                                       name,
                                       (uint32_t) sizeof(name),
                                       secret,
                                       (uint32_t) sizeof(secret),
                                       halo_now_seconds()) != S2N_SUCCESS) {
    return -1;
  }

  return 0;
}

static int halo_tls_build_server_config(value tls_cert_chain_path,
                                        value tls_private_key_path,
                                        value tls_security_policy,
                                        value tls_minimum_protocol_version,
                                        value tls_alpn_protocols,
                                        value tls_ocsp_staple_path,
                                        value tls_client_auth_mode,
                                        value tls_client_auth_ca_cert_path,
                                        value tls_client_auth_ca_cert_dir,
                                        value tls_session_tickets_enabled,
                                        value tls_session_ticket_keys_path,
                                        value tls_session_ticket_encrypt_decrypt_lifetime_secs,
                                        value tls_session_ticket_decrypt_lifetime_secs,
                                        value tls_session_ticket_initial_count,
                                        value tls_server_keying_material_lifetime_secs,
                                        value tls_max_early_data_size,
                                        struct s2n_config** out_tls_config,
                                        struct s2n_cert_chain_and_key** out_tls_cert_chain,
                                        int* out_max_early_data_size,
                                        int* out_server_keying_material_lifetime_secs,
                                        uint64_t* out_replay_ttl_secs,
                                        char* error_message,
                                        size_t error_message_cap) {
  struct halo_tls_server_files files;
  struct s2n_config* tls_config;
  struct s2n_cert_chain_and_key* tls_cert_chain;
  const char* security_policy_name;
  const char* client_auth_ca_cert_path_value;
  const char* client_auth_ca_cert_dir_value;
  const char* alpn_data;
  size_t alpn_len;
  size_t alpn_index;
  int minimum_protocol_version;
  int client_auth_mode;
  int session_tickets_enabled;
  int session_ticket_encrypt_decrypt_lifetime_secs;
  int session_ticket_decrypt_lifetime_secs;
  int session_ticket_initial_count;
  int server_keying_material_lifetime_secs;
  int max_early_data_size;

  halo_tls_server_files_init(&files);
  tls_config = NULL;
  tls_cert_chain = NULL;

  uv_once(&halo_tls_global_once, halo_tls_global_init);
  if (halo_tls_global_init_status != 0) {
    snprintf(error_message, error_message_cap, "%s", halo_tls_global_init_error);
    return -1;
  }

  if (halo_load_file(String_val(tls_cert_chain_path),
                     &files.cert_chain_pem,
                     &files.cert_chain_pem_len) != 0) {
    snprintf(error_message, error_message_cap, "failed to open tls cert chain");
    return -1;
  }
  if (halo_load_file(String_val(tls_private_key_path),
                     &files.private_key_pem,
                     &files.private_key_pem_len) != 0) {
    snprintf(error_message, error_message_cap, "failed to open tls private key");
    goto error;
  }

  tls_config = s2n_config_new_minimal();
  tls_cert_chain = s2n_cert_chain_and_key_new();
  if (tls_config == NULL
      || tls_cert_chain == NULL
      || s2n_cert_chain_and_key_load_pem(
           tls_cert_chain,
           files.cert_chain_pem,
           files.private_key_pem) != S2N_SUCCESS
      || s2n_config_add_cert_chain_and_key_to_store(tls_config, tls_cert_chain) != S2N_SUCCESS
      /* The recv callback is nonblocking: partial batches return on EAGAIN. */
      || s2n_config_set_recv_multi_record(tls_config, true) != S2N_SUCCESS) {
    snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
    goto error;
  }

  security_policy_name = String_val(tls_security_policy);
  minimum_protocol_version = Int_val(tls_minimum_protocol_version);
  if (security_policy_name[0] != '\0') {
    if (s2n_config_set_cipher_preferences(tls_config, security_policy_name) != S2N_SUCCESS) {
      snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
      goto error;
    }
  } else if (minimum_protocol_version == 13) {
    if (s2n_config_set_cipher_preferences(tls_config, "default_tls13") != S2N_SUCCESS) {
      snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
      goto error;
    }
  } else if (minimum_protocol_version == 12) {
    if (s2n_config_set_cipher_preferences(tls_config, "default") != S2N_SUCCESS) {
      snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
      goto error;
    }
  }

  alpn_data = String_val(tls_alpn_protocols);
  alpn_len = caml_string_length(tls_alpn_protocols);
  alpn_index = 0;
  while (alpn_index < alpn_len) {
    size_t start;
    uint8_t protocol_len;

    start = alpn_index;
    while (alpn_index < alpn_len && alpn_data[alpn_index] != '\0')
      alpn_index += 1;
    protocol_len = (uint8_t) (alpn_index - start);
    if (protocol_len > 0
        && s2n_config_append_protocol_preference(tls_config,
                                                 (const uint8_t*) (alpn_data + start),
                                                 protocol_len) != S2N_SUCCESS) {
      snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
      goto error;
    }
    alpn_index += 1;
  }

  if (caml_string_length(tls_ocsp_staple_path) > 0) {
    if (halo_load_file(String_val(tls_ocsp_staple_path),
                       &files.ocsp_data,
                       &files.ocsp_data_len) != 0
        || s2n_cert_chain_and_key_set_ocsp_data(tls_cert_chain,
                                                (const uint8_t*) files.ocsp_data,
                                                (uint32_t) files.ocsp_data_len) != S2N_SUCCESS) {
      snprintf(error_message, error_message_cap, "failed to load tls ocsp staple");
      goto error;
    }
  }

  client_auth_mode = Int_val(tls_client_auth_mode);
  if (client_auth_mode != 0) {
    client_auth_ca_cert_path_value = caml_string_length(tls_client_auth_ca_cert_path) > 0
      ? String_val(tls_client_auth_ca_cert_path)
      : NULL;
    client_auth_ca_cert_dir_value = caml_string_length(tls_client_auth_ca_cert_dir) > 0
      ? String_val(tls_client_auth_ca_cert_dir)
      : NULL;

    if (((client_auth_ca_cert_path_value != NULL || client_auth_ca_cert_dir_value != NULL)
          && s2n_config_set_verification_ca_location(tls_config,
                                                     client_auth_ca_cert_path_value,
                                                     client_auth_ca_cert_dir_value) != S2N_SUCCESS)
        || ((client_auth_ca_cert_path_value == NULL
             && client_auth_ca_cert_dir_value == NULL)
            && s2n_config_load_system_certs(tls_config) != S2N_SUCCESS)
        || s2n_config_set_client_auth_type(
             tls_config,
             client_auth_mode == 2 ? S2N_CERT_AUTH_REQUIRED : S2N_CERT_AUTH_OPTIONAL)
           != S2N_SUCCESS) {
      snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
      goto error;
    }
  }

  session_tickets_enabled = Int_val(tls_session_tickets_enabled);
  session_ticket_encrypt_decrypt_lifetime_secs =
    Int_val(tls_session_ticket_encrypt_decrypt_lifetime_secs);
  session_ticket_decrypt_lifetime_secs =
    Int_val(tls_session_ticket_decrypt_lifetime_secs);
  session_ticket_initial_count = Int_val(tls_session_ticket_initial_count);
  server_keying_material_lifetime_secs =
    Int_val(tls_server_keying_material_lifetime_secs);
  if (session_tickets_enabled >= 0
      && s2n_config_set_session_tickets_onoff(
           tls_config,
           (uint8_t) (session_tickets_enabled != 0)) != S2N_SUCCESS) {
    snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
    goto error;
  }

  if (session_ticket_encrypt_decrypt_lifetime_secs >= 0
      && s2n_config_set_ticket_encrypt_decrypt_key_lifetime(
           tls_config,
           (uint64_t) session_ticket_encrypt_decrypt_lifetime_secs) != S2N_SUCCESS) {
    snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
    goto error;
  }

  if (session_ticket_decrypt_lifetime_secs >= 0
      && s2n_config_set_ticket_decrypt_key_lifetime(
           tls_config,
           (uint64_t) session_ticket_decrypt_lifetime_secs) != S2N_SUCCESS) {
    snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
    goto error;
  }

  if (session_ticket_initial_count >= 0
      && s2n_config_set_initial_ticket_count(
           tls_config,
           (uint8_t) session_ticket_initial_count) != S2N_SUCCESS) {
    snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
    goto error;
  }

  if (caml_string_length(tls_session_ticket_keys_path) > 0) {
    if (halo_tls_load_ticket_key_file(tls_config,
                                      String_val(tls_session_ticket_keys_path),
                                      error_message,
                                      error_message_cap) != 0) {
      goto error;
    }
  } else if (session_tickets_enabled > 0
             && halo_tls_add_ephemeral_ticket_key(tls_config) != 0) {
    snprintf(error_message,
             error_message_cap,
             "failed to generate tls session ticket key");
    goto error;
  }

  max_early_data_size = Int_val(tls_max_early_data_size);
  if (max_early_data_size >= 0
      && s2n_config_set_server_max_early_data_size(tls_config,
                                                   (uint32_t) max_early_data_size) != S2N_SUCCESS) {
    snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
    goto error;
  }

  if (max_early_data_size > 0
      && s2n_config_set_early_data_cb(tls_config, halo_tls_early_data_cb) != S2N_SUCCESS) {
    snprintf(error_message, error_message_cap, "%s", s2n_strerror(s2n_errno, "EN"));
    goto error;
  }

  halo_tls_server_files_release(&files);
  *out_tls_config = tls_config;
  *out_tls_cert_chain = tls_cert_chain;
  *out_max_early_data_size = max_early_data_size;
  *out_server_keying_material_lifetime_secs = server_keying_material_lifetime_secs;
  /* TLS 1.3 caps ticket lifetimes at seven days. Retaining a claim for that
   * full interval from the offer is conservative even when the ticket was
   * already partly expired, and avoids underestimating combined key phases. */
  *out_replay_ttl_secs = HALO_TLS13_REPLAY_TTL_SECS;
  return 0;

error:
  halo_tls_server_files_release(&files);
  if (tls_cert_chain != NULL)
    s2n_cert_chain_and_key_free(tls_cert_chain);
  if (tls_config != NULL)
    s2n_config_free(tls_config);
  return -1;
}

static int halo_tls_early_data_cb(struct s2n_connection* conn,
                                  struct s2n_offered_early_data* early_data) {
  struct halo_client* client;
  uint16_t identity_len;
  uint8_t* identity;
  uint8_t identity_digest[SHA256_DIGEST_LENGTH];
  int result;

  if (conn == NULL || early_data == NULL)
    return S2N_FAILURE;

  client = s2n_connection_get_ctx(conn);
  if (client == NULL || client->listener_ref == NULL)
    return S2N_FAILURE;

  identity_len = 0;
  if (s2n_connection_get_negotiated_psk_identity_length(conn, &identity_len) != S2N_SUCCESS
      || identity_len == 0) {
    return s2n_offered_early_data_reject(early_data);
  }

  identity = malloc(identity_len);
  if (identity == NULL)
    return s2n_offered_early_data_reject(early_data);

  result = s2n_connection_get_negotiated_psk_identity(conn, identity, identity_len);
  if (result != S2N_SUCCESS) {
    free(identity);
    return S2N_FAILURE;
  }

  if (SHA256(identity, identity_len, identity_digest) == NULL) {
    free(identity);
    return s2n_offered_early_data_reject(early_data);
  }
  free(identity);
  client->tls_pending_early_data = early_data;
  if (halo_bridge_dispatch_replay_claim(
        client,
        identity_digest,
        sizeof(identity_digest),
        client->listener_ref->tls_replay_ttl_secs) != 0) {
    client->tls_pending_early_data = NULL;
    return s2n_offered_early_data_reject(early_data);
  }
  return S2N_SUCCESS;
}

static int halo_load_file(const char* path, char** contents, size_t* length) {
  FILE* file;
  long file_len;
  char* buffer;

  if (path == NULL || contents == NULL || length == NULL)
    return -1;

  file = fopen(path, "rb");
  if (file == NULL)
    return -1;
  if (fseek(file, 0, SEEK_END) != 0) {
    fclose(file);
    return -1;
  }
  file_len = ftell(file);
  if (file_len < 0 || fseek(file, 0, SEEK_SET) != 0) {
    fclose(file);
    return -1;
  }

  buffer = malloc((size_t) file_len + 1);
  if (buffer == NULL) {
    fclose(file);
    return -1;
  }
  if (fread(buffer, 1, (size_t) file_len, file) != (size_t) file_len) {
    fclose(file);
    free(buffer);
    return -1;
  }
  fclose(file);
  buffer[file_len] = '\0';

  *contents = buffer;
  *length = (size_t) file_len;
  return 0;
}

/* Bound idle storage per connection, while allowing larger live TLS flights. */
#define HALO_TLS_RETAIN_CAPACITY (64 * 1024)

static int halo_tls_prepare_operation(struct halo_client* client) {
  if (client == NULL)
    return -1;
  assert(client->tls_capture == NULL || client->tls_capture->buf.len == 0);
  return 0;
}

static int halo_tls_capture_append(struct halo_client* client,
                                   const char* data,
                                   size_t len) {
  struct halo_tls_write_chunk* chunk;
  size_t required;

  if (len == 0)
    return 0;
  chunk = client->tls_capture;
  if (chunk == NULL) {
    chunk = client->tls_write_spare;
    if (chunk != NULL)
      client->tls_write_spare = NULL;
    else {
      chunk = calloc(1, sizeof(*chunk));
      if (chunk == NULL)
        return -1;
    }
    client->tls_capture = chunk;
  }
  if (len > SIZE_MAX - chunk->buf.len)
    return -1;
  required = chunk->buf.len + len;
  if (required > chunk->capacity) {
    size_t capacity = chunk->capacity == 0 ? 4096 : chunk->capacity;
    char* next;

    while (capacity < required) {
      if (capacity > SIZE_MAX / 2) {
        capacity = required;
        break;
      }
      capacity *= 2;
    }
    next = realloc(chunk->buf.base, capacity);
    if (next == NULL)
      return -1;
    chunk->buf.base = next;
    chunk->capacity = capacity;
  }
  memcpy(chunk->buf.base + chunk->buf.len, data, len);
  chunk->buf.len += len;
  return 0;
}

static void halo_tls_write_chunk_destroy(struct halo_tls_write_chunk* chunk) {
  if (chunk == NULL)
    return;
  free(chunk->buf.base);
  free(chunk);
}

static int halo_tls_finish_operation(struct halo_client* client, enum halo_write_kind kind) {
  struct halo_tls_write_chunk* chunk;

  if (client == NULL)
    return -1;

  chunk = client->tls_capture;
  if (chunk == NULL || chunk->buf.len == 0)
    return 0;

  /* Transfer ownership to the write queue; storage remains stable until the
   * libuv completion callback returns it to the bounded spare slot. */
  client->tls_capture = NULL;
  chunk->kind = kind;
  chunk->next = NULL;
  if (client->tls_write_tail == NULL) {
    client->tls_write_head = chunk;
    client->tls_write_tail = chunk;
  } else {
    client->tls_write_tail->next = chunk;
    client->tls_write_tail = chunk;
  }
  return 0;
}

static int halo_tls_send_cb(void* ctx, const uint8_t* buf, uint32_t len) {
  struct halo_client* client;

  client = ctx;
  if (client == NULL) {
    errno = EINVAL;
    return -1;
  }

  if (halo_tls_capture_append(client, (const char*) buf, (size_t) len) != 0) {
    errno = ENOMEM;
    return -1;
  }

  return (int) len;
}

static int halo_tls_recv_cb(void* ctx, uint8_t* buf, uint32_t len) {
  struct halo_client* client;
  size_t amount;

  client = ctx;
  if (client == NULL) {
    errno = EINVAL;
    return -1;
  }

  amount = halo_linear_buffer_read(&client->tls_input, buf, (size_t) len);
  if (amount == 0) {
    errno = EAGAIN;
    return -1;
  }
  return (int) amount;
}

static void halo_free_handle(uv_handle_t* handle) {
  void* data;
  enum halo_handle_kind kind;
  struct halo_listener* listener;
  struct halo_client* client;
  struct halo_runtime* runtime;

  data = handle->data;
  if (data == NULL)
    return;

  runtime = handle->loop != NULL ? (struct halo_runtime*) handle->loop->data : NULL;
  if (runtime != NULL
      && handle == (uv_handle_t*) &runtime->controller.timer)
    return;

  kind = *(enum halo_handle_kind*) data;
  switch (kind) {
    case HALO_HANDLE_LISTENER:
      listener = (struct halo_listener*) data;
      if (listener->ref != NULL) {
        listener->ref->handle = NULL;
        halo_listener_ref_release(listener->ref);
      }
      free(listener);
      break;

    case HALO_HANDLE_CLIENT:
      client = (struct halo_client*) data;
      if (handle == (uv_handle_t*) &client->handle)
        client->handle_closed = 1;
      else if (client->timer_initialized
               && handle == (uv_handle_t*) &client->timer)
        client->timer_closed = 1;
      if (client->handle_closed
          && (!client->timer_initialized || client->timer_closed)) {
        struct halo_tls_write_chunk* chunk;
        struct halo_tls_write_chunk* next;
        struct halo_listener_ref* listener_ref;

        if (client->tls_conn != NULL) {
          s2n_connection_free(client->tls_conn);
          client->tls_conn = NULL;
        }
        halo_linear_buffer_destroy(&client->tls_input);
        halo_tls_write_chunk_destroy(client->tls_capture);
        halo_tls_write_chunk_destroy(client->tls_write_spare);
        free(client->bridge_body_event);
        chunk = client->tls_write_head;
        while (chunk != NULL) {
          next = chunk->next;
          halo_tls_write_chunk_destroy(chunk);
          chunk = next;
        }
        halo_client_clear_paused_data(client);
        halo_request_free(&client->request);
        listener_ref = client->listener_ref;
        client->listener_ref = NULL;
        if (listener_ref != NULL) {
          if (listener_ref->active_clients > 0)
            listener_ref->active_clients -= 1;
          halo_listener_ref_release(listener_ref);
        }
        free(client);
      }
      break;

    case HALO_HANDLE_BRIDGE:
      break;

    default:
      assert(0 && "unknown halo handle kind");
  }
}

void halo_on_handle_closed(uv_handle_t* handle) {
  halo_free_handle(handle);
}

static void halo_client_timeout_cb(uv_timer_t* timer) {
  struct halo_client* client;

  client = (struct halo_client*) timer->data;
  if (client == NULL)
    return;

  if (client->timeout_mode == HALO_TIMEOUT_REQUEST) {
    if (client->interim_response_started) {
      if (halo_client_start_request_timeout(client) != 0)
        halo_close_client(client);
      return;
    }

    if (!client->response_started) {
      halo_mark_direct_response(client, 408);
      client->should_keep_alive = 0;
      if (halo_write_response((uv_stream_t*) &client->handle,
                              client,
                              halo_request_timeout_response,
                              sizeof(halo_request_timeout_response) - 1) != 0)
        halo_close_client(client);
    }
    else
      halo_close_client(client);
    return;
  }

  if (client->timeout_mode == HALO_TIMEOUT_IDLE)
    halo_close_client(client);
}

void halo_client_stop_timer(struct halo_client* client) {
  if (client == NULL || !client->timer_initialized)
    return;
  if (uv_is_closing((uv_handle_t*) &client->timer))
    return;

  uv_timer_stop(&client->timer);
  client->timeout_mode = HALO_TIMEOUT_NONE;
}

void halo_close_client(struct halo_client* client) {
  if (client == NULL)
    return;

  halo_bridge_cancel_replay_claim(client);

  if (client->listener_ref != NULL
      && client->listener_ref->tls_enabled
      && !client->tls_handshake_complete
      && !client->response_started
      && !client->interim_response_started) {
    halo_runtime_note_tls_handshake_failure(client);
  }

  halo_tls_try_start_shutdown(client);
  halo_runtime_note_request_finished(client, 0);
  halo_runtime_note_connection_closed(client);

  if (client->bridge_request_id != 0)
    halo_bridge_disconnect_request(client);
  else
    halo_observe_direct_response(client, 0);
  halo_bridge_connection_closed(client);

  halo_client_stop_timer(client);

  if (client->timer_initialized && !uv_is_closing((uv_handle_t*) &client->timer))
    uv_close((uv_handle_t*) &client->timer, halo_on_handle_closed);
  if (!uv_is_closing((uv_handle_t*) &client->handle))
    uv_close((uv_handle_t*) &client->handle, halo_on_handle_closed);
}

static void halo_tls_try_start_shutdown(struct halo_client* client) {
  s2n_blocked_status blocked;
  uv_stream_t* stream;

  if (!halo_tls_enabled(client) || client->tls_close_started)
    return;

  stream = (uv_stream_t*) &client->handle;
  client->tls_close_started = 1;
  if (halo_tls_prepare_operation(client) != 0)
    return;

  (void) s2n_shutdown_send(client->tls_conn, &blocked);
  if (halo_tls_finish_operation(client, 0) != 0)
    return;
  (void) halo_tls_flush_pending(stream, client);
}

static void halo_after_shutdown(uv_shutdown_t* req, int status) {
  struct halo_client* client;

  client = req != NULL ? (struct halo_client*) req->handle->data : NULL;
  if (status < 0 && client != NULL)
    halo_close_client(client);
}

static int halo_start_graceful_close(uv_stream_t* stream, struct halo_client* client) {
  if (client == NULL)
    return -1;

  halo_client_clear_paused_data(client);
  halo_request_clear(&client->request);
  client->message_complete = 0;
  client->direct_response_status = 0;
  client->pending_continue = 0;
  client->dispatch_started = 0;
  client->dispatch_pending_until_handshake = 0;
  client->dispatch_after_headers = 0;
  client->awaiting_request_body = 0;
  client->body_chunk_delivered = 0;
  client->request_is_early_data = 0;
  client->body_bytes_read = 0;
  client->request_start_monotonic_ns = 0;
  client->request_start_unix_ns = 0;

  if (!client->shutdown_started) {
    client->shutdown_started = 1;
    if (uv_shutdown(&client->shutdown_req, stream, halo_after_shutdown) != 0)
      return -1;
  }

  if (!uv_is_active((uv_handle_t*) stream)
      && uv_read_start(stream, halo_alloc_cb, (uv_read_cb) halo_read_cb) != 0)
    return -1;

  return halo_client_start_request_timeout(client);
}

static int halo_client_start_request_timeout(struct halo_client* client) {
  if (client == NULL || client->listener_ref == NULL)
    return -1;
  if (!client->timer_initialized || uv_is_closing((uv_handle_t*) &client->timer))
    return -1;

  if (client->listener_ref->request_timeout_ms == 0) {
    halo_client_stop_timer(client);
    return 0;
  }

  client->timeout_mode = HALO_TIMEOUT_REQUEST;
  uv_timer_stop(&client->timer);
  return uv_timer_start(&client->timer,
                        halo_client_timeout_cb,
                        (uint64_t) client->listener_ref->request_timeout_ms,
                        0);
}

static int halo_client_start_idle_timeout(struct halo_client* client) {
  if (client == NULL || client->listener_ref == NULL)
    return -1;
  if (!client->timer_initialized || uv_is_closing((uv_handle_t*) &client->timer))
    return -1;

  if (client->listener_ref->idle_timeout_ms == 0) {
    halo_client_stop_timer(client);
    return 0;
  }

  client->timeout_mode = HALO_TIMEOUT_IDLE;
  uv_timer_stop(&client->timer);
  return uv_timer_start(&client->timer,
                        halo_client_timeout_cb,
                        (uint64_t) client->listener_ref->idle_timeout_ms,
                        0);
}

static void halo_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf) {
  char* base;

  (void)handle;

  if (suggested_size > HALO_BODY_DELIVERY_BATCH_SIZE)
    suggested_size = HALO_BODY_DELIVERY_BATCH_SIZE;
  base = malloc(suggested_size);
  if (base == NULL) {
    buf->base = NULL;
    buf->len = 0;
    return;
  }

  *buf = uv_buf_init(base, (unsigned int) suggested_size);
}

static void halo_write_req_destroy(struct halo_write_req* write_req) {
  free(write_req->buf.base);
  free(write_req);
}

static int halo_wait_for_request_body(uv_stream_t* stream,
                                      struct halo_client* client) {
  if (halo_client_start_request_timeout(client) != 0) {
    halo_close_client(client);
    return 1;
  }

  if (client->request.has_content_length
      && client->body_bytes_read >= (size_t) client->request.content_length)
    return halo_bridge_emit_body_end(client);
  if (client->message_complete)
    return halo_bridge_emit_body_end(client);

  client->awaiting_request_body = 1;
  llhttp_resume(&client->parser);
  if (client->paused_data_len > 0) {
    llhttp_errno_t err;
    char* paused_data;
    size_t paused_data_len;

    paused_data = client->paused_data;
    paused_data_len = client->paused_data_len;
    client->paused_data = NULL;
    client->paused_data_len = 0;
    err = llhttp_execute(&client->parser, paused_data, paused_data_len);
    if (err == HPE_PAUSED
        && (client->pending_continue
            || client->dispatch_after_headers
            || client->awaiting_request_body)) {
      if (halo_capture_paused_remainder(client, paused_data, paused_data_len) != 0) {
        free(paused_data);
        halo_close_client(client);
        return 1;
      }
    }
    free(paused_data);
    halo_handle_parser_result(stream, client, err);
    return 0;
  }

  if (halo_tls_enabled(client)) {
    if (halo_tls_drain_plaintext(stream, client) != 0) {
      halo_close_client(client);
      return 1;
    }
    if (!client->awaiting_request_body)
      return 0;
  }

  if (uv_is_active((uv_handle_t*) stream))
    return 0;

  if (uv_read_start(stream, halo_alloc_cb, (uv_read_cb) halo_read_cb) != 0) {
    halo_close_client(client);
    return 1;
  }

  return 0;
}

static int halo_write_internal_server_error_response(uv_stream_t* stream,
                                                     struct halo_client* client) {
  halo_mark_direct_response(client, 500);
  client->should_keep_alive = 0;
  return halo_write_response(stream,
                             client,
                             halo_internal_server_error_response,
                             sizeof(halo_internal_server_error_response) - 1);
}

static int halo_try_dispatch_request(uv_stream_t* stream,
                                     struct halo_client* client) {
  int result;

  if (halo_tls_enabled(client)
      && client->request_is_early_data
      && !client->tls_handshake_complete) {
    if (halo_request_materialize_header_spans(&client->request) != 0)
      return -1;
    if (!uv_is_active((uv_handle_t*) stream)
        && uv_read_start(stream, halo_alloc_cb, halo_read_cb) != 0) {
      return -1;
    }
    client->dispatch_pending_until_handshake = 1;
    return 0;
  }
  halo_runtime_note_request_started(client);
  result = halo_bridge_dispatch_request(client);
  /* A rejected request may be observed after this input buffer is gone. */
  if ((result != 0 || client->direct_response_status != 0)
      && halo_request_materialize_header_spans(&client->request) != 0)
    return -1;
  return result;
}

static int halo_handle_write_kind_complete(uv_stream_t* stream,
                                           struct halo_client* client,
                                           enum halo_write_kind kind) {
  llhttp_errno_t err;
  int fast_close_direct_response;

  if (client == NULL)
    return 0;

  fast_close_direct_response =
    client->direct_response_status != 0
    && (client->parser.flags & F_CHUNKED) != 0;

  if (kind == HALO_WRITE_EXPECT_CONTINUE) {
    client->interim_response_started = 0;
    client->pending_continue = 0;

    if (client->dispatch_after_headers) {
      client->dispatch_after_headers = 0;
      client->dispatch_started = 1;
      halo_client_stop_timer(client);
      if (halo_try_dispatch_request(stream, client) != 0
          && halo_write_internal_server_error_response(stream, client) != 0)
        return -1;
      return 0;
    }

    llhttp_resume(&client->parser);

    if (client->paused_data_len > 0) {
      err = llhttp_execute(&client->parser, client->paused_data, client->paused_data_len);
      halo_client_clear_paused_data(client);
      if (err != HPE_OK && err != HPE_PAUSED)
        client->response_started = 0;
      if (halo_handle_parser_result(stream, client, err))
        return 0;
    }

    if (uv_read_start(stream, halo_alloc_cb, (uv_read_cb) halo_read_cb) != 0)
      return -1;
    return 0;
  }

  if (kind == HALO_WRITE_DISPATCH_INTERIM) {
    client->interim_response_started = 0;
    return halo_bridge_complete_write(client);
  }

  if (kind == HALO_WRITE_RESPONSE_STREAM_START
      || kind == HALO_WRITE_RESPONSE_CHUNK)
    return halo_bridge_complete_write(client);

  if (kind == HALO_WRITE_RESPONSE_STREAM_FINISH) {
    halo_runtime_note_request_finished(client, 1);

    halo_bridge_complete_request(client);

    if (!client->should_keep_alive) {
      if (fast_close_direct_response) {
        halo_client_clear_paused_data(client);
        return -1;
      }
      if (!halo_tls_enabled(client))
        return halo_start_graceful_close(stream, client);
      return -1;
    }

    halo_client_clear_paused_data(client);
    halo_request_clear(&client->request);
    client->message_complete = 0;
    client->response_started = 0;
    client->interim_response_started = 0;
    client->should_keep_alive = 0;
    client->direct_response_status = 0;
    client->pending_continue = 0;
    client->dispatch_started = 0;
    client->dispatch_pending_until_handshake = 0;
    client->dispatch_after_headers = 0;
    client->awaiting_request_body = 0;
    client->body_chunk_delivered = 0;
    client->request_is_early_data = 0;
    client->body_bytes_read = 0;
    client->request_start_monotonic_ns = 0;
    client->request_start_unix_ns = 0;
    llhttp_reset(&client->parser);
    llhttp_set_lenient_version(&client->parser, 1);
    if (uv_read_start(stream, halo_alloc_cb, (uv_read_cb) halo_read_cb) != 0)
      return -1;
    else if (halo_client_start_idle_timeout(client) != 0)
      return -1;
    return 0;
  }

  if (kind == HALO_WRITE_FINAL) {
    if (client->bridge_response_in_flight) {
      client->bridge_response_in_flight = 0;
      halo_bridge_complete_request(client);
    }
    else if (client->bridge_request_id != 0) {
      client->direct_response_status = 0;
      halo_bridge_disconnect_request(client);
    }
    else
      halo_observe_direct_response(client, 1);
  }

  halo_runtime_note_request_finished(client, 1);

  if (!client->should_keep_alive) {
    if (fast_close_direct_response) {
      halo_client_clear_paused_data(client);
      return -1;
    }
    if (!halo_tls_enabled(client))
      return halo_start_graceful_close(stream, client);
    return -1;
  }

  halo_client_clear_paused_data(client);
  halo_request_clear(&client->request);
  client->message_complete = 0;
  client->response_started = 0;
  client->interim_response_started = 0;
  client->should_keep_alive = 0;
  client->direct_response_status = 0;
  client->pending_continue = 0;
  client->dispatch_started = 0;
  client->dispatch_pending_until_handshake = 0;
  client->dispatch_after_headers = 0;
  client->awaiting_request_body = 0;
  client->body_chunk_delivered = 0;
  client->request_is_early_data = 0;
  client->body_bytes_read = 0;
  client->request_start_monotonic_ns = 0;
  client->request_start_unix_ns = 0;
  llhttp_reset(&client->parser);
  llhttp_set_lenient_version(&client->parser, 1);
  if (uv_read_start(stream, halo_alloc_cb, (uv_read_cb) halo_read_cb) != 0)
    return -1;
  else if (halo_client_start_idle_timeout(client) != 0)
    return -1;
  return 0;
}

static void halo_after_write(uv_write_t* req, int status) {
  struct halo_write_req* write_req;
  struct halo_client* client;
  uv_stream_t* stream;

  write_req = (struct halo_write_req*) req;
  client = (struct halo_client*) req->handle->data;
  stream = (uv_stream_t*) req->handle;

  if (status == 0 && halo_handle_write_kind_complete(stream, client, write_req->kind) != 0)
    halo_close_client(client);
  else if (status != 0 && client != NULL)
    halo_close_client(client);

  halo_write_req_destroy(write_req);
}

static void halo_tls_after_write(uv_write_t* req, int status) {
  struct halo_tls_write_chunk* chunk;
  struct halo_client* client;
  uv_stream_t* stream;
  enum halo_write_kind kind;

  chunk = (struct halo_tls_write_chunk*) req;
  client = (struct halo_client*) req->handle->data;
  stream = (uv_stream_t*) req->handle;
  kind = chunk->kind;

  if (client != NULL) {
    client->tls_write_in_flight = 0;
    client->tls_write_head = chunk->next;
    if (client->tls_write_head == NULL)
      client->tls_write_tail = NULL;
  }

  if (client != NULL && status == 0
      && chunk->capacity <= HALO_TLS_RETAIN_CAPACITY
      && client->tls_write_spare == NULL) {
    chunk->buf.len = 0;
    chunk->next = NULL;
    client->tls_write_spare = chunk;
  } else {
    halo_tls_write_chunk_destroy(chunk);
  }

  if (client == NULL)
    return;

  if (status != 0) {
    halo_close_client(client);
    return;
  }

  if (kind != 0 && halo_handle_write_kind_complete(stream, client, kind) != 0) {
    halo_close_client(client);
    return;
  }

  if (client->tls_write_head != NULL) {
    if (halo_tls_flush_pending(stream, client) != 0)
      halo_close_client(client);
    return;
  }

  if (!client->tls_handshake_complete) {
    if (halo_tls_drive_handshake(stream, client) != 0)
      halo_close_client(client);
    return;
  }

  if (kind == 0 && halo_tls_drain_plaintext(stream, client) != 0)
    halo_close_client(client);
}

static int halo_tls_flush_pending(uv_stream_t* stream, struct halo_client* client) {
  struct halo_tls_write_chunk* chunk;

  if (client == NULL || client->tls_write_in_flight)
    return 0;

  chunk = client->tls_write_head;
  if (chunk == NULL)
    return 0;

  if (uv_write(&chunk->req, stream, &chunk->buf, 1, halo_tls_after_write) != 0)
    return -1;

  client->tls_write_in_flight = 1;
  return 0;
}

static int halo_tls_handle_blocked_error(void) {
  int error_type;

  error_type = s2n_error_get_type(s2n_errno);
  if (error_type == S2N_ERR_T_BLOCKED)
    return 0;
  return -1;
}

static uint64_t halo_estimate_unix_time_ns(uint64_t monotonic_time_ns) {
  uv_timeval64_t wall_time;
  uint64_t seconds;
  uint64_t nanoseconds;
  uint64_t clock_read_ns;
  uint64_t unix_ns;

  if (uv_gettimeofday(&wall_time) != 0 || wall_time.tv_sec < 0)
    return 0;

  seconds = (uint64_t) wall_time.tv_sec;
  nanoseconds = (uint64_t) wall_time.tv_usec * 1000ULL;
  if (seconds > (UINT64_MAX - nanoseconds) / 1000000000ULL)
    return 0;

  unix_ns = seconds * 1000000000ULL + nanoseconds;
  clock_read_ns = uv_hrtime() - monotonic_time_ns;
  if (unix_ns < clock_read_ns)
    return 0;

  return unix_ns - clock_read_ns;
}

static size_t halo_direct_response_body_size(int status) {
  switch (status) {
    case 500:
      return sizeof("internal server error\n") - 1;
    case 503:
      return sizeof("server overloaded\n") - 1;
    default:
      return strlen(halo_error_response_body(status));
  }
}

static void halo_mark_direct_response(struct halo_client* client, int status) {
  struct halo_runtime* runtime;

  if (client == NULL)
    return;
  client->direct_response_status = status;
  runtime = NULL;
  if (client->handle.loop != NULL)
    runtime = client->handle.loop->data;
  if (client->request_start_monotonic_ns == 0
      && runtime != NULL && runtime->telemetry_enabled) {
    client->request_start_monotonic_ns = uv_hrtime();
    client->request_start_unix_ns =
      halo_estimate_unix_time_ns(client->request_start_monotonic_ns);
  }
}

static void halo_observe_direct_response(struct halo_client* client,
                                         int completed) {
  struct halo_runtime* runtime;
  int status;

  if (client == NULL || client->direct_response_status == 0)
    return;
  status = client->direct_response_status;
  client->direct_response_status = 0;
  runtime = NULL;
  if (client->handle.loop != NULL)
    runtime = client->handle.loop->data;
  if (runtime == NULL || !runtime->telemetry_enabled)
    return;
  halo_bridge_observe_pre_dispatch_response(
    client, status, halo_direct_response_body_size(status), completed);
}

static int halo_feed_plaintext(uv_stream_t* stream,
                               struct halo_client* client,
                               const char* plaintext,
                               size_t plaintext_len) {
  llhttp_errno_t err;
  struct halo_runtime* runtime;

  runtime = NULL;

  if (client->handle.loop != NULL)
    runtime = (struct halo_runtime*) client->handle.loop->data;

  if (plaintext_len > 0
      && client->request_start_monotonic_ns == 0
      && runtime != NULL
      && runtime->telemetry_enabled) {
    client->request_start_monotonic_ns = uv_hrtime();
    client->request_start_unix_ns =
      halo_estimate_unix_time_ns(client->request_start_monotonic_ns);
  }

  err = llhttp_execute(&client->parser, plaintext, plaintext_len);
  if (!client->dispatch_started
      && (err != HPE_PAUSED || client->pending_continue)
      && halo_request_materialize_header_spans(&client->request) != 0)
    return -1;
  if (err == HPE_PAUSED
      && (client->pending_continue
          || client->dispatch_after_headers
          || client->awaiting_request_body)) {
    if (halo_capture_paused_remainder(client, plaintext, plaintext_len) != 0)
      return -1;
  }

  return halo_handle_parser_result(stream, client, err) != 0 ? 1 : 0;
}

static int halo_tls_drain_early_plaintext(uv_stream_t* stream, struct halo_client* client) {
  uint8_t plaintext[HALO_BODY_DELIVERY_BATCH_SIZE];

  while (halo_tls_enabled(client) && !client->tls_early_data_complete) {
    ssize_t data_received;
    s2n_blocked_status blocked;
    s2n_early_data_status_t early_status;
    int result;

    if (client->response_started || client->interim_response_started)
      return 0;

    if (halo_tls_prepare_operation(client) != 0)
      return -1;
    data_received = 0;
    result = s2n_recv_early_data(client->tls_conn,
                                 plaintext,
                                 sizeof(plaintext),
                                 &data_received,
                                 &blocked);
    if (halo_tls_finish_operation(client, 0) != 0)
      return -1;
    if (halo_tls_flush_pending(stream, client) != 0)
      return -1;
    if (client->tls_write_in_flight)
      return 0;

    if (result == S2N_SUCCESS) {
      if (s2n_connection_get_early_data_status(client->tls_conn, &early_status) != S2N_SUCCESS)
        return -1;
      if (early_status != S2N_EARLY_DATA_STATUS_OK)
        client->tls_early_data_complete = 1;
    }

    if (data_received > 0) {
      int parse_result;

      client->request_is_early_data = 1;
      if (halo_client_start_request_timeout(client) != 0)
        return -1;
      parse_result =
        halo_feed_plaintext(stream, client, (const char*) plaintext, (size_t) data_received);
      if (parse_result < 0)
        return -1;
      if (parse_result > 0 && !client->dispatch_pending_until_handshake)
        return 0;
    }

    if (result == S2N_SUCCESS) {
      if (early_status == S2N_EARLY_DATA_STATUS_OK) {
        if (data_received == 0)
          return 0;
        continue;
      }
      return 0;
    }

    if (halo_tls_handle_blocked_error() == 0)
      return 0;
    return -1;
  }

  return 0;
}

static int halo_tls_drive_handshake(uv_stream_t* stream, struct halo_client* client) {
  s2n_blocked_status blocked;

  if (!halo_tls_enabled(client) || client->tls_handshake_complete)
    return 0;

  while (!client->tls_handshake_complete) {
    if (client->listener_ref->tls_max_early_data_size > 0
        && !client->tls_early_data_complete) {
      if (halo_tls_drain_early_plaintext(stream, client) != 0)
        return -1;
      if (!client->tls_early_data_complete
          || client->response_started
          || client->interim_response_started) {
        return 0;
      }
    }

    if (halo_tls_prepare_operation(client) != 0)
      return -1;

    if (s2n_negotiate(client->tls_conn, &blocked) == S2N_SUCCESS) {
      int resumed;

      if (halo_tls_finish_operation(client, 0) != 0)
        return -1;
      resumed = s2n_connection_is_session_resumed(client->tls_conn);
      if (resumed < 0)
        return -1;
      client->tls_handshake_complete = 1;
      halo_runtime_note_tls_handshake(client, resumed != 0);
      if (client->dispatch_pending_until_handshake) {
        client->dispatch_pending_until_handshake = 0;
        if (halo_try_dispatch_request(stream, client) != 0
            && halo_write_internal_server_error_response(stream, client) != 0)
          return -1;
      }
      if (halo_tls_flush_pending(stream, client) != 0)
        return -1;
      if (client->tls_write_in_flight)
        return 0;
      break;
    }

    if (halo_tls_finish_operation(client, 0) != 0)
      return -1;
    if (halo_tls_flush_pending(stream, client) != 0)
      return -1;
    if (client->tls_write_in_flight)
      return 0;
    if (halo_tls_handle_blocked_error() == 0)
      return 0;
    return -1;
  }

  return halo_tls_drain_plaintext(stream, client);
}

static int halo_tls_drain_plaintext(uv_stream_t* stream, struct halo_client* client) {
  char plaintext[HALO_BODY_DELIVERY_BATCH_SIZE];
  llhttp_errno_t err;

  while (halo_tls_enabled(client) && client->tls_handshake_complete) {
    ssize_t nread;
    s2n_blocked_status blocked;

    if (client->response_started || client->interim_response_started)
      return 0;

    if (halo_client_start_request_timeout(client) != 0)
      return -1;

    if (halo_tls_prepare_operation(client) != 0)
      return -1;
    nread = s2n_recv(client->tls_conn, plaintext, sizeof(plaintext), &blocked);
    if (halo_tls_finish_operation(client, 0) != 0)
      return -1;
    if (halo_tls_flush_pending(stream, client) != 0)
      return -1;
    if (client->tls_write_in_flight)
      return 0;

    if (nread < 0) {
      if (halo_tls_handle_blocked_error() == 0)
        return 0;
      return -1;
    }

    if (nread == 0) {
      if (blocked != S2N_NOT_BLOCKED)
        return 0;
      if (!client->response_started && halo_request_has_data(&client->request)) {
        err = llhttp_finish(&client->parser);
        if (halo_handle_parser_result(stream, client, err))
          return 0;
      }
      return -1;
    }

    if (halo_feed_plaintext(stream, client, plaintext, (size_t) nread) != 0)
      return 0;
  }

  return 0;
}

static int halo_write_buffer(uv_stream_t* stream,
                             struct halo_client* client,
                             const char* response,
                             size_t response_len,
                             enum halo_write_kind kind) {
  if (halo_tls_enabled(client)) {
    size_t offset;

    offset = 0;
    uv_read_stop(stream);
    if (kind == HALO_WRITE_EXPECT_CONTINUE
        || kind == HALO_WRITE_DISPATCH_INTERIM)
      client->interim_response_started = 1;
    else
      client->response_started = 1;

    while (offset < response_len) {
      ssize_t written;
      enum halo_write_kind chunk_kind;
      s2n_blocked_status blocked;

      if (halo_tls_prepare_operation(client) != 0)
        return -1;
      written = s2n_send(client->tls_conn,
                         response + offset,
                         response_len - offset,
                         &blocked);
      chunk_kind = (written > 0 && offset + (size_t) written >= response_len) ? kind : 0;
      if (halo_tls_finish_operation(client, chunk_kind) != 0)
        return -1;
      if (halo_tls_flush_pending(stream, client) != 0)
        return -1;

      if (written < 0) {
        if (halo_tls_handle_blocked_error() == 0)
          return 0;
        return -1;
      }

      offset += (size_t) written;
      if (client->tls_write_in_flight)
        return 0;
    }

    return 0;
  }

  struct halo_write_req* write_req;

  write_req = malloc(sizeof(*write_req));
  if (write_req == NULL)
    return -1;

  write_req->buf.base = malloc(response_len);
  if (write_req->buf.base == NULL) {
    halo_write_req_destroy(write_req);
    return -1;
  }

  memcpy(write_req->buf.base, response, response_len);
  write_req->buf.len = response_len;
  write_req->kind = kind;

  if (uv_write(&write_req->req,
               stream,
               &write_req->buf,
               1,
               halo_after_write) != 0) {
    halo_write_req_destroy(write_req);
    return -1;
  }

  if (kind == HALO_WRITE_EXPECT_CONTINUE
      || kind == HALO_WRITE_DISPATCH_INTERIM)
    client->interim_response_started = 1;
  else
    client->response_started = 1;
  uv_read_stop(stream);
  return 0;
}

static int halo_write_response(uv_stream_t* stream,
                               struct halo_client* client,
                               const char* response,
                               size_t response_len) {
  return halo_write_buffer(stream,
                           client,
                           response,
                           response_len,
                           HALO_WRITE_FINAL);
}

static int halo_write_owned_buffer(uv_stream_t* stream,
                                   struct halo_client* client,
                                   char* response,
                                   size_t response_len,
                                   enum halo_write_kind kind) {
  struct halo_write_req* write_req;

  write_req = malloc(sizeof(*write_req));
  if (write_req == NULL) {
    free(response);
    return -1;
  }

  write_req->buf.base = response;
  write_req->buf.len = response_len;
  write_req->kind = kind;

  if (uv_write(&write_req->req,
               stream,
               &write_req->buf,
               1,
               halo_after_write) != 0) {
    halo_write_req_destroy(write_req);
    return -1;
  }

  if (kind == HALO_WRITE_EXPECT_CONTINUE
      || kind == HALO_WRITE_DISPATCH_INTERIM)
    client->interim_response_started = 1;
  else
    client->response_started = 1;
  uv_read_stop(stream);
  return 0;
}

int halo_client_bridge_send_response(struct halo_client* client,
                                     char* response,
                                     size_t response_len) {
  int result;

  if (client == NULL || client->bridge_request_id == 0
      || client->response_started || client->interim_response_started) {
    free(response);
    return -1;
  }
  if (!halo_request_body_complete_for_reuse(client))
    client->should_keep_alive = 0;
  client->bridge_response_in_flight = 1;
  if (halo_tls_enabled(client)) {
    result = halo_write_response((uv_stream_t*) &client->handle,
                                 client,
                                 response,
                                 response_len);
    free(response);
  }
  else {
    result = halo_write_owned_buffer((uv_stream_t*) &client->handle,
                                     client,
                                     response,
                                     response_len,
                                     HALO_WRITE_FINAL);
  }
  if (result != 0) {
    client->bridge_response_in_flight = 0;
    return -1;
  }
  return 0;
}

int halo_client_bridge_send_informational(struct halo_client* client,
                                          const char* response,
                                          size_t response_len) {
  if (client == NULL || client->bridge_request_id == 0
      || client->response_started || client->interim_response_started)
    return -1;
  return halo_write_buffer((uv_stream_t*) &client->handle,
                           client,
                           response,
                           response_len,
                           HALO_WRITE_DISPATCH_INTERIM);
}

int halo_client_bridge_start_response_stream(struct halo_client* client,
                                             const char* response,
                                             size_t response_len) {
  if (client == NULL || client->bridge_request_id == 0
      || client->response_started || client->interim_response_started)
    return -1;
  /* Chunk data and its final terminator are separate writes. Nagle can hold
   * the latter until a delayed ACK, adding latency even to a flushed batch.
   * Keep low-latency writes enabled for this connection after streaming. */
  if (uv_tcp_nodelay(&client->handle, 1) != 0)
    return -1;
  if (!halo_request_body_complete_for_reuse(client))
    client->should_keep_alive = 0;
  return halo_write_buffer((uv_stream_t*) &client->handle,
                           client,
                           response,
                           response_len,
                           HALO_WRITE_RESPONSE_STREAM_START);
}

int halo_client_bridge_write_response_chunk(struct halo_client* client,
                                            const char* response,
                                            size_t response_len) {
  if (client == NULL || client->bridge_request_id == 0
      || !client->response_started || client->interim_response_started)
    return -1;
  return halo_write_buffer((uv_stream_t*) &client->handle,
                           client,
                           response,
                           response_len,
                           HALO_WRITE_RESPONSE_CHUNK);
}

int halo_client_bridge_finish_response_stream(struct halo_client* client) {
  if (client == NULL || client->bridge_request_id == 0
      || !client->response_started || client->interim_response_started)
    return -1;
  return halo_write_buffer((uv_stream_t*) &client->handle,
                           client,
                           "0\r\n\r\n",
                           5,
                           HALO_WRITE_RESPONSE_STREAM_FINISH);
}

int halo_client_bridge_abort_response_stream(struct halo_client* client) {
  if (client == NULL || client->bridge_request_id == 0
      || !client->response_started || client->interim_response_started
      || client->bridge_response_in_flight)
    return -1;
  halo_close_client(client);
  return 0;
}

int halo_client_bridge_resolve_replay_claim(struct halo_client* client,
                                            uint64_t claim_id,
                                            int accept) {
  struct s2n_offered_early_data* early_data;

  if (client == NULL || claim_id == 0
      || client->bridge_replay_claim_id != claim_id
      || client->tls_pending_early_data == NULL
      || client->tls_handshake_complete)
    return -1;

  early_data = client->tls_pending_early_data;
  client->bridge_replay_claim_id = 0;
  client->tls_pending_early_data = NULL;
  if ((accept
         ? s2n_offered_early_data_accept(early_data)
         : s2n_offered_early_data_reject(early_data)) != S2N_SUCCESS)
    return -1;
  return halo_tls_drive_handshake((uv_stream_t*) &client->handle, client);
}

int halo_client_bridge_reject_busy(struct halo_client* client) {
  if (client == NULL || client->response_started
      || client->interim_response_started)
    return -1;
  halo_mark_direct_response(client, 503);
  client->should_keep_alive = 0;
  return halo_write_response((uv_stream_t*) &client->handle,
                             client,
                             halo_service_unavailable_response,
                             sizeof(halo_service_unavailable_response) - 1);
}

int halo_client_bridge_request_body(struct halo_client* client) {
  uv_stream_t* stream;

  if (client == NULL || client->bridge_request_id == 0
      || client->response_started || client->interim_response_started)
    return -1;
  if (client->awaiting_request_body)
    return 0;
  stream = (uv_stream_t*) &client->handle;
  return halo_wait_for_request_body(stream, client);
}

void halo_client_bridge_close(struct halo_client* client) {
  halo_close_client(client);
}

static int halo_write_continue_response(uv_stream_t* stream,
                                        struct halo_client* client) {
  return halo_write_buffer(stream,
                           client,
                           halo_http_continue_response,
                           sizeof(halo_http_continue_response) - 1,
                           HALO_WRITE_EXPECT_CONTINUE);
}

static const char* halo_error_reason_phrase(int status) {
  switch (status) {
    case 400: return "Bad Request";
    case 408: return "Request Timeout";
    case 411: return "Length Required";
    case 413: return "Content Too Large";
    case 414: return "URI Too Long";
    case 417: return "Expectation Failed";
    case 431: return "Request Header Fields Too Large";
    case 505: return "HTTP Version Not Supported";
    default: return "Bad Request";
  }
}

static int halo_write_bridge_error_response(uv_stream_t* stream,
                                            struct halo_client* client,
                                            int status,
                                            const char* body) {
  const char* reason;
  char* response;
  int response_len;
  int result;

  reason = halo_error_reason_phrase(status);
  if (getenv("HALO_BRIDGE_TRACE") != NULL)
    fprintf(stderr, "halo parser: sending error status=%d\n", status);
  response_len = snprintf(NULL,
                          0,
                          "HTTP/1.1 %d %s\r\n"
                          "Content-Length: %zu\r\n"
                          "Connection: close\r\n"
                          "Content-Type: text/plain; charset=utf-8\r\n"
                          "\r\n"
                          "%s",
                          status,
                          reason,
                          strlen(body),
                          body);
  if (response_len < 0)
    return -1;
  response = malloc((size_t) response_len + 1);
  if (response == NULL)
    return -1;
  snprintf(response,
           (size_t) response_len + 1,
           "HTTP/1.1 %d %s\r\n"
           "Content-Length: %zu\r\n"
           "Connection: close\r\n"
           "Content-Type: text/plain; charset=utf-8\r\n"
           "\r\n"
           "%s",
           status,
           reason,
           strlen(body),
           body);
  halo_mark_direct_response(client, status);
  client->should_keep_alive = 0;
  result = halo_write_response(stream, client, response, (size_t) response_len);
  free(response);
  return result;
}

static void halo_close_with_bad_request(uv_stream_t* stream, struct halo_client* client) {
  if (halo_write_bridge_error_response(stream,
                                       client,
                                       400,
                                       "bad request\n") != 0)
    halo_close_client(client);
}

static void halo_close_with_error_response(uv_stream_t* stream,
                                           struct halo_client* client,
                                           int status,
                                           const char* body) {
  if (halo_write_bridge_error_response(stream, client, status, body) != 0)
    halo_close_client(client);
}

static const char* halo_error_response_body(int status) {
  switch (status) {
    case 408:
      return "request timeout\n";
    case 411:
      return "length required\n";
    case 413:
      return "content too large\n";
    case 414:
      return "uri too long\n";
    case 417:
      return "expectation failed\n";
    case 431:
      return "request header fields too large\n";
    case 505:
      return "http version not supported\n";
    default:
      return "bad request\n";
  }
}

static int halo_capture_paused_remainder(struct halo_client* client,
                                         const char* buf_base,
                                         size_t buf_len) {
  const char* error_pos;
  size_t consumed_len;
  size_t remaining_len;
  char* paused;

  halo_client_clear_paused_data(client);
  error_pos = llhttp_get_error_pos(&client->parser);
  if (error_pos == NULL)
    return 0;

  if (error_pos < buf_base)
    error_pos = buf_base;
  if (error_pos > buf_base + buf_len)
    error_pos = buf_base + buf_len;

  consumed_len = (size_t) (error_pos - buf_base);
  if (consumed_len >= buf_len)
    return 0;

  remaining_len = buf_len - consumed_len;
  paused = malloc(remaining_len);
  if (paused == NULL)
    return -1;

  memcpy(paused, error_pos, remaining_len);
  client->paused_data = paused;
  client->paused_data_len = remaining_len;
  return 0;
}

static int halo_handle_parser_result(uv_stream_t* stream,
                                     struct halo_client* client,
                                     llhttp_errno_t err) {
  /* Publish one chunk per input buffer, before body-end or error handling.
   * Never wait for a full batch: a streaming peer may need this data now. */
  if (halo_bridge_flush_body_chunk(client) != 0) {
    halo_close_client(client);
    return 1;
  }
  if (err == HPE_OK
      && client->awaiting_request_body
      && client->body_chunk_delivered) {
    client->awaiting_request_body = 0;
    client->body_chunk_delivered = 0;
    uv_read_stop(stream);
    return 1;
  }

  if (err == HPE_OK)
    return 0;

  if (err != HPE_PAUSED && getenv("HALO_BRIDGE_TRACE") != NULL)
    fprintf(stderr,
            "halo parser: error=%s reason=%s body=%zu expected=%" PRIu64 "\n",
            llhttp_errno_name(err),
            llhttp_get_error_reason(&client->parser),
            client->body_bytes_read,
            client->request.content_length);

  if (err == HPE_PAUSED && client->pending_continue) {
    if (halo_client_start_request_timeout(client) != 0) {
      halo_close_client(client);
      return 1;
    }
    if (halo_write_continue_response(stream, client) != 0)
      halo_close_client(client);
    return 1;
  }

  if (err == HPE_PAUSED && client->dispatch_after_headers) {
    client->dispatch_after_headers = 0;
    client->dispatch_started = 1;
    uv_read_stop(stream);
    halo_client_stop_timer(client);
    if (halo_try_dispatch_request(stream, client) != 0
        && halo_write_internal_server_error_response(stream, client) != 0)
      halo_close_client(client);
    return 1;
  }

  if (err == HPE_PAUSED && client->awaiting_request_body) {
    client->awaiting_request_body = 0;
    client->body_chunk_delivered = 0;
    if (client->message_complete && halo_bridge_emit_body_end(client) != 0)
      halo_close_client(client);
    return 1;
  }

  if (err == HPE_PAUSED && client->message_complete) {
    if (client->dispatch_started) {
      return 1;
    }

    uv_read_stop(stream);
    halo_client_stop_timer(client);
    if (halo_try_dispatch_request(stream, client) != 0
        && halo_write_internal_server_error_response(stream, client) != 0)
      halo_close_client(client);
    return 1;
  }

  if (client->direct_response_status != 0) {
    halo_client_stop_timer(client);
    halo_close_with_error_response(stream,
                                   client,
                                   client->direct_response_status,
                                   halo_error_response_body(client->direct_response_status));
    return 1;
  }

  halo_client_stop_timer(client);
  if (getenv("HALO_BRIDGE_TRACE") != NULL)
    fprintf(stderr,
            "halo parser: unhandled state error=%s pending=%d dispatch=%d "
            "awaiting=%d delivered=%d complete=%d\n",
            llhttp_errno_name(err),
            client->pending_continue,
            client->dispatch_after_headers,
            client->awaiting_request_body,
            client->body_chunk_delivered,
            client->message_complete);
  halo_close_with_bad_request(stream, client);
  return 1;
}

static void halo_read_cb(uv_stream_t* stream, ssize_t nread, const uv_buf_t* buf) {
  struct halo_client* client;
  llhttp_errno_t err;

  client = (struct halo_client*) stream->data;

  if (halo_tls_enabled(client)) {
    if (nread < 0) {
      free(buf->base);
      if (nread == UV_EOF)
        halo_close_client(client);
      else
        halo_close_client(client);
      return;
    }

    if (nread == 0) {
      free(buf->base);
      return;
    }

    if (halo_linear_buffer_append(&client->tls_input, buf->base, (size_t) nread) != 0) {
      free(buf->base);
      halo_close_client(client);
      return;
    }
    free(buf->base);

    if (!client->tls_handshake_complete) {
      if (halo_tls_drive_handshake(stream, client) != 0)
        halo_close_client(client);
      return;
    }

    if (halo_tls_drain_plaintext(stream, client) != 0)
      halo_close_client(client);
    return;
  }

  if (nread < 0) {
    free(buf->base);
    if (nread == UV_EOF && !client->response_started) {
      if (halo_request_has_data(&client->request)) {
        err = llhttp_finish(&client->parser);
        if (halo_handle_parser_result(stream, client, err))
          return;
      } else {
        halo_close_client(client);
        return;
      }
    }
    halo_close_client(client);
    return;
  }

  if (nread == 0) {
    free(buf->base);
    return;
  }

  if (client->response_started || client->interim_response_started) {
    free(buf->base);
    return;
  }

  if (halo_client_start_request_timeout(client) != 0) {
    free(buf->base);
    halo_close_client(client);
    return;
  }

  if (halo_feed_plaintext(stream, client, buf->base, (size_t) nread) < 0) {
    free(buf->base);
    halo_close_client(client);
    return;
  }
  free(buf->base);
}

static void halo_connection_cb(uv_stream_t* server, int status) {
  struct halo_client* client;
  struct halo_listener* listener;
  struct halo_runtime* runtime;

  if (status < 0)
    return;

  listener = (struct halo_listener*) server->data;
  runtime = (struct halo_runtime*) server->loop->data;
  if (runtime == NULL || runtime->is_draining || runtime->is_stopping)
    return;
  client = malloc(sizeof(*client));
  if (client == NULL)
    return;

  memset(client, 0, sizeof(*client));
  halo_linear_buffer_init(&client->tls_input, HALO_TLS_RETAIN_CAPACITY);
  client->kind = HALO_HANDLE_CLIENT;
  client->listener_ref = listener->ref;
  halo_request_storage_init(&client->request.storage, 1024);

  if (uv_tcp_init(server->loop, &client->handle) != 0) {
    free(client);
    return;
  }
  client->handle.data = client;
  client->listener_ref->active_clients += 1;

  if (uv_timer_init(server->loop, &client->timer) != 0) {
    uv_close((uv_handle_t*) &client->handle, halo_on_handle_closed);
    return;
  }

  llhttp_init(&client->parser, HTTP_REQUEST, halo_http_settings());
  llhttp_set_lenient_version(&client->parser, 1);
  client->parser.data = client;
  client->timer.data = client;
  client->timer_initialized = 1;
  client->dispatch_started = 0;
  client->dispatch_after_headers = 0;
  client->awaiting_request_body = 0;
  client->body_chunk_delivered = 0;
  client->bridge_request_id = 0;
  client->bridge_response_in_flight = 0;
  client->tls_conn = NULL;

  if (uv_accept(server, (uv_stream_t*) &client->handle) != 0) {
    halo_close_client(client);
    return;
  }

  if (client->listener_ref->tls_enabled) {
    client->tls_conn = s2n_connection_new(S2N_SERVER);
    if (client->tls_conn == NULL) {
      halo_close_client(client);
      return;
    }
    if (s2n_connection_set_config(client->tls_conn, client->listener_ref->tls_config) != S2N_SUCCESS
        || s2n_connection_set_recv_cb(client->tls_conn, halo_tls_recv_cb) != S2N_SUCCESS
        || s2n_connection_set_send_cb(client->tls_conn, halo_tls_send_cb) != S2N_SUCCESS
        || s2n_connection_set_recv_ctx(client->tls_conn, client) != S2N_SUCCESS
        || s2n_connection_set_send_ctx(client->tls_conn, client) != S2N_SUCCESS
        || s2n_connection_set_ctx(client->tls_conn, client) != S2N_SUCCESS) {
      halo_close_client(client);
      return;
    }
    if (client->listener_ref->tls_max_early_data_size > 0)
      client->tls_conn->early_data_expected = 1;
    if (client->listener_ref->tls_server_keying_material_lifetime_secs >= 0
        && s2n_connection_set_server_keying_material_lifetime(
             client->tls_conn,
             (uint32_t) client->listener_ref->tls_server_keying_material_lifetime_secs)
             != S2N_SUCCESS) {
      halo_close_client(client);
      return;
    }
  }

  halo_runtime_note_connection_opened(client);

  if (runtime != NULL && runtime->controller.backpressure_enabled) {
    runtime->metrics.rejected_connections += 1;
    if (halo_client_bridge_reject_busy(client) != 0)
      halo_close_client(client);
    return;
  }

  if (uv_read_start((uv_stream_t*) &client->handle, halo_alloc_cb, halo_read_cb) != 0)
    halo_close_client(client);
  else if (halo_client_start_request_timeout(client) != 0)
    halo_close_client(client);
}

/* Keep configuration validation and binding on the same literal parser. */
static int halo_parse_bind_address(value host,
                                   intnat port,
                                   struct sockaddr_storage* addr) {
  const char* text = String_val(host);
  size_t length = caml_string_length(host);

  if (port < 0 || port > 65535 || length == 0
      || memchr(text, '\0', length) != NULL
      || memchr(text, '%', length) != NULL)
    return UV_EINVAL;

  memset(addr, 0, sizeof(*addr));
  if (uv_ip4_addr(text, (int) port, (struct sockaddr_in*) addr) == 0)
    return 0;
  return uv_ip6_addr(text, (int) port, (struct sockaddr_in6*) addr);
}

CAMLprim value ocaml_halo_host_address_family(value host) {
  struct sockaddr_storage addr;

  if (halo_parse_bind_address(host, 0, &addr) != 0)
    return Val_int(0);
  return Val_int(addr.ss_family == AF_INET6 ? 6 : 4);
}

/* Some platforms ignore libuv's IPv6 bind flags. Require the chosen mode. */
static int halo_check_ipv6_mode(uv_tcp_t* server, int ipv6_only) {
#ifdef IPV6_V6ONLY
  uv_os_fd_t fd;
  int actual;
  socklen_t length = sizeof(actual);
  int code = uv_fileno((uv_handle_t*) server, &fd);

  if (code != 0)
    return code;
  if (getsockopt(fd, IPPROTO_IPV6, IPV6_V6ONLY, &actual, &length) != 0)
    return uv_translate_sys_error(errno);
  return (actual != 0) == ipv6_only ? 0 : UV_ENOTSUP;
#else
  return UV_ENOTSUP;
#endif
}

CAMLprim value ocaml_halo_create_server(value runtime,
                                        value host,
                                        value port,
                                        value backlog,
                                        value max_request_line_size,
                                        value max_header_size,
                                        value max_body_size,
                                        value request_timeout_ms,
                                        value idle_timeout_ms,
                                        value tls_enabled,
                                        value tls_cert_chain_path,
                                        value tls_private_key_path,
                                        value tls_security_policy,
                                        value tls_minimum_protocol_version,
                                        value tls_alpn_protocols,
                                        value tls_ocsp_staple_path,
                                        value tls_client_auth_mode,
                                        value tls_client_auth_ca_cert_path,
                                        value tls_client_auth_ca_cert_dir,
                                        value tls_session_tickets_enabled,
                                        value tls_session_ticket_keys_path,
                                        value tls_session_ticket_encrypt_decrypt_lifetime_secs,
                                        value tls_session_ticket_decrypt_lifetime_secs,
                                        value tls_session_ticket_initial_count,
                                        value tls_server_keying_material_lifetime_secs,
                                        value tls_max_early_data_size,
                                        value ipv6_mode,
                                        value reuse_port) {
  CAMLparam5(runtime, host, port, backlog, max_request_line_size);
  CAMLxparam5(max_header_size,
              max_body_size,
              request_timeout_ms,
              idle_timeout_ms,
              tls_enabled);
  CAMLxparam5(tls_cert_chain_path,
              tls_private_key_path,
              tls_security_policy,
              tls_minimum_protocol_version,
              tls_alpn_protocols);
  CAMLxparam5(tls_ocsp_staple_path,
              tls_client_auth_mode,
              tls_client_auth_ca_cert_path,
              tls_client_auth_ca_cert_dir,
              tls_session_tickets_enabled);
  CAMLxparam5(tls_session_ticket_keys_path,
              tls_session_ticket_encrypt_decrypt_lifetime_secs,
              tls_session_ticket_decrypt_lifetime_secs,
              tls_session_ticket_initial_count,
              tls_server_keying_material_lifetime_secs);
  CAMLxparam3(tls_max_early_data_size, ipv6_mode, reuse_port);
  CAMLlocal1(listener_value);
  struct halo_listener_pair pair;
  struct halo_runtime* rt;
  struct halo_listener* listener;
  struct halo_listener_ref* ref;
  struct sockaddr_storage addr;
  unsigned int bind_flags;
  int code;
  struct s2n_config* tls_config;
  struct s2n_cert_chain_and_key* tls_cert_chain;
  int max_early_data_size;
  int server_keying_material_lifetime_secs;
  uint64_t replay_ttl_secs;
  char tls_error_message[256];

  rt = halo_runtime_get(runtime);
  code = halo_parse_bind_address(host, Long_val(port), &addr);
  if (code != 0)
    CAMLreturn(halo_result_error_uv("parse bind address", code));
  if (Int_val(ipv6_mode) < 0 || Int_val(ipv6_mode) > 2
      || (addr.ss_family != AF_INET6 && Int_val(ipv6_mode) != 0))
    CAMLreturn(halo_result_error_uv("ipv6_mode", UV_EINVAL));
  bind_flags = Bool_val(reuse_port) ? UV_TCP_REUSEPORT : 0;
  if (addr.ss_family == AF_INET6 && Int_val(ipv6_mode) != 2)
    bind_flags |= UV_TCP_IPV6ONLY;

  if (halo_listener_pair_create(&pair,
                                rt,
                                max_request_line_size,
                                max_header_size,
                                max_body_size,
                                request_timeout_ms,
                                idle_timeout_ms,
                                tls_enabled) != 0) {
    CAMLreturn(halo_result_error_message("out of memory"));
  }

  listener = pair.listener;
  ref = pair.ref;

  if (ref->tls_enabled) {
    tls_config = NULL;
    tls_cert_chain = NULL;
    max_early_data_size = -1;
    server_keying_material_lifetime_secs = -1;
    replay_ttl_secs = 0;
    if (halo_tls_build_server_config(tls_cert_chain_path,
                                     tls_private_key_path,
                                     tls_security_policy,
                                     tls_minimum_protocol_version,
                                     tls_alpn_protocols,
                                     tls_ocsp_staple_path,
                                     tls_client_auth_mode,
                                     tls_client_auth_ca_cert_path,
                                     tls_client_auth_ca_cert_dir,
                                     tls_session_tickets_enabled,
                                     tls_session_ticket_keys_path,
                                     tls_session_ticket_encrypt_decrypt_lifetime_secs,
                                     tls_session_ticket_decrypt_lifetime_secs,
                                     tls_session_ticket_initial_count,
                                     tls_server_keying_material_lifetime_secs,
                                     tls_max_early_data_size,
                                     &tls_config,
                                     &tls_cert_chain,
                                     &max_early_data_size,
                                     &server_keying_material_lifetime_secs,
                                     &replay_ttl_secs,
                                     tls_error_message,
                                     sizeof(tls_error_message)) != 0) {
      halo_listener_pair_release(&pair);
      CAMLreturn(halo_result_error_message(tls_error_message));
    }

    ref->tls_max_early_data_size = max_early_data_size;
    ref->tls_server_keying_material_lifetime_secs =
      server_keying_material_lifetime_secs;
    ref->tls_replay_ttl_secs = replay_ttl_secs;
    ref->tls_config = tls_config;
    ref->tls_cert_chain = tls_cert_chain;
  }

  code = uv_tcp_init(&rt->loop, &listener->server);
  if (code != 0) {
    halo_listener_pair_release(&pair);
    CAMLreturn(halo_result_error_uv("uv_tcp_init", code));
  }

  listener->server.data = listener;

  code = uv_tcp_bind(&listener->server, (const struct sockaddr*) &addr, bind_flags);
  if (code != 0) {
    uv_close((uv_handle_t*) &listener->server, halo_on_handle_closed);
    CAMLreturn(halo_result_error_uv("uv_tcp_bind", code));
  }

  if (addr.ss_family == AF_INET6) {
    code = halo_check_ipv6_mode(&listener->server,
                                (bind_flags & UV_TCP_IPV6ONLY) != 0);
    if (code != 0) {
      uv_close((uv_handle_t*) &listener->server, halo_on_handle_closed);
      CAMLreturn(halo_result_error_uv("IPV6_V6ONLY", code));
    }
  }

  code = uv_listen((uv_stream_t*) &listener->server, Int_val(backlog), halo_connection_cb);
  if (code != 0) {
    uv_close((uv_handle_t*) &listener->server, halo_on_handle_closed);
    CAMLreturn(halo_result_error_uv("uv_listen", code));
  }

  ref->ocaml_alive = 1;
  listener_value = halo_listener_alloc(ref);
  CAMLreturn(halo_result_ok(listener_value));
}

CAMLprim value ocaml_halo_create_server_bytecode(value* argv, int argn) {
  (void) argn;
  return ocaml_halo_create_server(argv[0],
                                  argv[1],
                                  argv[2],
                                  argv[3],
                                  argv[4],
                                  argv[5],
                                  argv[6],
                                  argv[7],
                                  argv[8],
                                  argv[9],
                                  argv[10],
                                  argv[11],
                                  argv[12],
                                  argv[13],
                                  argv[14],
                                  argv[15],
                                  argv[16],
                                  argv[17],
                                  argv[18],
                                  argv[19],
                                  argv[20],
                                  argv[21],
                                  argv[22],
                                  argv[23],
                                  argv[24],
                                  argv[25],
                                  argv[26],
                                  argv[27]);
}
