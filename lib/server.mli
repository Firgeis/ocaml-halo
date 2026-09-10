module Shutdown : sig
  type t

  val create : unit -> t
  val request : t -> unit
end
(** A thread-safe shutdown signal. *)

type controller_state = Server_backend.controller_state =
  | Normal
  | Busy
  | Saturated
  | Recovering

type runtime_metrics = Server_backend.runtime_metrics = {
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
type config = Server_config.t

module Config = Server_config

val runtime_backend_name : unit -> string
val default_shutdown_timeout_ms : int

exception Client_disconnected
exception Runtime_error of string

type running
(** A live server handle. It is valid until [run] returns. *)

val metrics : running -> runtime_metrics
(** Request a metrics snapshot from the reactor domain. *)

val shutdown : running -> unit
(** Request graceful shutdown. *)

val run :
  ?shutdown:Shutdown.t ->
  ?handle_sigint:bool ->
  ?shutdown_timeout_ms:int ->
  ?max_in_flight_requests:int ->
  ?command_capacity:int ->
  ?on_ready:(running -> unit) ->
  ?on_error:(int64 -> exn -> Printexc.raw_backtrace -> unit) ->
  Config.t list ->
  unit
(** Run all configured listeners. Each request handler executes in its own Eio
    fiber. [on_ready] runs in a child fiber after listeners are bound.

    On shutdown, Halo closes its listeners and idle connections, lets active
    requests and responses drain, and then force-closes anything remaining
    after [shutdown_timeout_ms]. The default is
    [default_shutdown_timeout_ms]. Zero requests an immediate force-stop.

    [on_error request_id exn backtrace] reports request-processing and response
    stream failures, and response lifecycle callback failures. If no response
    has started, Halo sends a 500 response after the callback returns.
    [backtrace] is empty when OCaml backtrace recording is disabled. The default
    callback prints the exception and backtrace to standard error. *)
