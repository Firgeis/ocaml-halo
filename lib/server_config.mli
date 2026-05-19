type controller_config = {
  sample_interval_ms : int;
  busy_threshold : float;
  saturated_threshold : float;
  recovery_threshold : float;
  busy_samples : int;
  saturated_samples : int;
  recovery_samples : int;
}

type tls_minimum_protocol_version =
  | TLS12
  | TLS13

type tls_client_auth_mode =
  | Client_auth_none
  | Client_auth_optional
  | Client_auth_required

type tls_client_auth_config = {
  mode : tls_client_auth_mode;
  ca_cert_path : string option;
  ca_cert_dir : string option;
}

type tls_config = {
  cert_chain_path : string;
  private_key_path : string;
  security_policy : string option;
  minimum_protocol_version : tls_minimum_protocol_version option;
  alpn_protocols : string list;
  ocsp_staple_path : string option;
  client_auth : tls_client_auth_config;
  session_tickets_enabled : bool option;
  session_ticket_keys_path : string option;
  session_ticket_encrypt_decrypt_lifetime_secs : int option;
  session_ticket_decrypt_lifetime_secs : int option;
  session_ticket_initial_count : int option;
  server_keying_material_lifetime_secs : int option;
  max_early_data_size : int option;
}

type t = {
  host : string;
  port : int;
  backlog : int;
  max_request_size : int;
  request_timeout_ms : int;
  idle_timeout_ms : int;
  tls : tls_config option;
  controller : controller_config;
  router : Router.t;
}

val default_host : string
val default_backlog : int
val default_max_request_size : int
val default_request_timeout_ms : int
val default_idle_timeout_ms : int
val default_controller : controller_config

val tls_client_auth :
  ?ca_cert_path:string ->
  ?ca_cert_dir:string ->
  tls_client_auth_mode ->
  tls_client_auth_config

val tls :
  cert_chain_path:string ->
  private_key_path:string ->
  ?security_policy:string ->
  ?minimum_protocol_version:tls_minimum_protocol_version ->
  ?alpn_protocols:string list ->
  ?ocsp_staple_path:string ->
  ?client_auth:tls_client_auth_config ->
  ?session_tickets_enabled:bool ->
  ?session_ticket_keys_path:string ->
  ?session_ticket_encrypt_decrypt_lifetime_secs:int ->
  ?session_ticket_decrypt_lifetime_secs:int ->
  ?session_ticket_initial_count:int ->
  ?server_keying_material_lifetime_secs:int ->
  ?max_early_data_size:int ->
  unit ->
  tls_config

val create :
  ?host:string ->
  ?backlog:int ->
  ?max_request_size:int ->
  ?request_timeout_ms:int ->
  ?idle_timeout_ms:int ->
  ?tls:tls_config ->
  ?controller:controller_config ->
  port:int ->
  Router.t ->
  t

val validate : t -> string option
