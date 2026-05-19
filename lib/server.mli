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

type request_body_event =
  | Body_chunk of string
  | Body_end

type runtime_command =
  | Final of string
  | Interim of string * int
  | Await_request_body of int
  | Await_timeout of int * int
  | Start_response_stream of string * int
  | Write_response_chunk of string * int
  | Finish_response_stream

type runtime_error = string

type config = Server_config.t

module Config = Server_config

val runtime_backend_name : unit -> string
val close_server : server -> runtime_error option
val run : t -> unit
val run_once : t -> bool
val stop : t -> unit
val close : t -> runtime_error option
val runtime_metrics : t -> runtime_metrics
val listen : t -> config -> (server, string) result
val with_runtime : (t -> ('a, Error.t) result) -> ('a, Error.t) result
