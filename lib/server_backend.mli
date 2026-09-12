type t
type server

type controller_state =
  | Normal
  | Busy
  | Saturated
  | Recovering

type runtime_metrics = {
  utilization : float;
  active_connections : int64;
  active_requests : int64;
  accepted_connections : int64;
  rejected_connections : int64;
  completed_requests : int64;
  tls_full_handshakes : int64;
  tls_resumed_handshakes : int64;
  tls_handshake_failures : int64;
  event_queue_depth : int64;
  command_queue_depth : int64;
  controller_state : controller_state;
  backpressure_enabled : bool;
}

type controller_config = Server_config.controller_config
type tls_minimum_protocol_version = Server_config.tls_minimum_protocol_version =
  | TLS12
  | TLS13
type tls_client_auth_mode = Server_config.tls_client_auth_mode =
  | Client_auth_none
  | Client_auth_optional
  | Client_auth_required
type tls_client_auth_config = Server_config.tls_client_auth_config
type tls_config = Server_config.tls_config
type runtime_error = string
type config = Server_config.t

module Config = Server_config

val runtime_backend_name : unit -> string
val close : t -> runtime_error option
val release_server : server -> unit
val listen : t -> config -> (server, string) result
