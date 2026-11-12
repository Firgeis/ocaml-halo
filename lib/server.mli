(** A thread-safe shutdown signal. *)
module Shutdown : sig
  type t

  val create : unit -> t
  val request : t -> unit
end

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
  event_queue_depth : int64;
  command_queue_depth : int64;
  controller_state : controller_state;
  backpressure_enabled : bool;
}

type controller_config = Server_config.controller_config
type ipv6_mode = Server_config.ipv6_mode = IPv6_only | Dual_stack

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
(** Request a metrics snapshot from the reactor. Must be called from an Eio
    fiber in the domain running [Server.run]; raises [Invalid_argument] when
    called from another domain. *)

val collect_metrics : running -> Metrics.snapshot
(** Request a fresh reactor snapshot and combine it with the cumulative HTTP
    request measurements recorded by the configured [Metrics.t]. Has the same
    domain requirement as [metrics]. *)

val shutdown : running -> unit
(** Request graceful shutdown. Safe to call from another domain. *)

val run :
  ?shutdown:Shutdown.t ->
  ?handle_sigint:bool ->
  ?shutdown_timeout_ms:int ->
  ?max_in_flight_requests:int ->
  ?command_capacity:int ->
  ?on_ready:(running -> unit) ->
  ?telemetry:Telemetry.t ->
  ?metrics:Metrics.t ->
  ?on_error:(int64 -> exn -> Printexc.raw_backtrace -> unit) ->
  Config.t list ->
  unit
(** Run all configured listeners. Each request handler executes in its own Eio
    fiber. [on_ready] runs in a child fiber after listeners are bound.

    [max_in_flight_requests] defaults to 4096. The request lookup table
    allocates one pointer-sized bucket per configured slot at startup (32 KiB at
    the default on a 64-bit system), plus an entry for each active request. A
    finished request continues to occupy admission capacity until its completion
    or disconnection event is consumed.

    On shutdown, Halo closes its listeners and idle connections, lets active
    requests and responses drain, and then force-closes anything remaining after
    [shutdown_timeout_ms]. The default is [default_shutdown_timeout_ms]. Zero
    requests an immediate force-stop.

    An application-owned [Compute.t] must remain open while requests drain.
    Close it after [run] returns (or after every server sharing it has
    returned), using a cleanup handler to cover exceptional exits as well.

    [telemetry] receives each request's full lifecycle, from the first plaintext
    byte observed by the C reactor through the terminal response write or
    disconnect. Parser errors and other final responses generated before OCaml
    dispatch are included. It defaults to [Telemetry.noop].

    [metrics] records OpenTelemetry HTTP request metrics and Halo runtime
    metrics. It defaults to [Metrics.noop].

    [on_error request_id exn backtrace] reports request-processing and response
    stream failures, and response lifecycle callback failures. If no response
    has started, Halo sends a 500 response after the callback returns.
    [backtrace] is empty when OCaml backtrace recording is disabled. The default
    callback prints the exception and backtrace to standard error. *)
