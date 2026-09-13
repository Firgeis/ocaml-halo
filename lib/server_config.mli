type controller_config = {
  sample_interval_ms : int;
  busy_threshold : float;
  saturated_threshold : float;
  recovery_threshold : float;
  busy_samples : int;
  saturated_samples : int;
  recovery_samples : int;
}

type tls_minimum_protocol_version = TLS12 | TLS13

type tls_client_auth_mode =
  | Client_auth_none
  | Client_auth_optional
  | Client_auth_required

type tls_client_auth_config = {
  mode : tls_client_auth_mode;
  ca_cert_path : string option;
  ca_cert_dir : string option;
}

type early_data_policy =
  method_text:string -> target:string -> headers:(string * string) list -> bool
(** Decide whether a TLS 1.3 early-data request may be dispatched. The request
    body has not been consumed when this function runs. *)

module Early_data_replay_store : sig
  type claim = Fresh | Already_seen | Unavailable
  type t

  val create :
    ?timeout_seconds:float -> (identity:string -> ttl_secs:int64 -> claim) -> t
  (** Build a shared replay-store adapter. [identity] is an opaque 32-byte
      SHA-256 digest and must be claimed atomically for at least [ttl_secs]. The
      callback runs in an Eio fiber and must use cooperative Eio I/O.
      Exceptions, timeouts, and [Unavailable] reject early data without aborting
      the TLS handshake. *)

  val create_blocking :
    ?timeout_seconds:float ->
    ?max_in_flight:int ->
    (identity:string -> ttl_secs:int64 -> claim) ->
    t
  (** Build an adapter for a blocking replay-store client. Each callback runs in
      a system thread so it cannot block Halo's Eio domain. At most
      [max_in_flight] callbacks may run at once (64 by default); excess claims
      return [Unavailable]. A callback may continue after Halo's timeout, so the
      client must also configure a finite I/O timeout. *)

  val claim : t -> identity:string -> ttl_secs:int64 -> claim
  val timeout_seconds : t -> float

  val process_local : t
  (** The bounded, process-wide reference implementation. It is shared by all
      Halo listeners and runtimes in this process, but not by other processes or
      instances. *)
end

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
  max_request_line_size : int;
  max_header_size : int;
  max_body_size : int;
  request_timeout_ms : int;
  idle_timeout_ms : int;
  early_data_policy : early_data_policy;
  early_data_replay_store : Early_data_replay_store.t option;
  tls : tls_config option;
  controller : controller_config;
  router : Router.t;
}

val default_host : string
val default_backlog : int

val default_max_request_line_size : int
(** Maximum combined method and request-target bytes. *)

val default_max_header_size : int
(** Maximum combined header-name and header-value bytes. *)

val default_max_body_size : int
(** Maximum decoded request-body bytes. *)

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
  ?max_request_line_size:int ->
  ?max_header_size:int ->
  ?max_body_size:int ->
  ?request_timeout_ms:int ->
  ?idle_timeout_ms:int ->
  ?early_data_policy:early_data_policy ->
  ?early_data_replay_store:Early_data_replay_store.t ->
  ?tls:tls_config ->
  ?controller:controller_config ->
  port:int ->
  Router.t ->
  t
(** Build a listener configuration. Request-line, header, and body limits are
    independent and are enforced cumulatively within their respective request
    sections. TLS 1.3 early-data requests receive 425 unless [early_data_policy]
    explicitly accepts them. [early_data_replay_store] replaces the process-wide
    replay cache for this listener. *)

val validate : t -> string option
