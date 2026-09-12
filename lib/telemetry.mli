(** Implementation-neutral request telemetry.

    This module is the boundary between Halo and tracing/metrics exporters. It
    deliberately does not depend on a particular OpenTelemetry SDK. An observer
    can extract propagated context from [headers], create an HTTP server span,
    activate that span in [with_span], and derive request metrics from the
    terminal result. *)

type server_request = {
  request_id : int64;
  method_ : string;
  target : string;
  headers : (string * string) list;
  http_version : int * int;
  connection_info : Router.connection_info;
  is_early_data : bool;
  start_time_unix_ns : int64 option;
  start_monotonic_ns : int64;
}
(** Metadata available when Halo dispatches a request.

    [start_time_unix_ns] and [start_monotonic_ns] are captured by the C reactor
    when the first plaintext request byte is received. The Unix timestamp is
    absent only if the platform clock could not be read. The monotonic value is
    suitable for computing duration; it has no defined epoch. *)

type server_request_result = {
  route : string option;
  status_code : int option;
  outcome : Router.response_outcome option;
  end_monotonic_ns : int64 option;
}
(** Information known when request processing leaves its telemetry scope.

    Under normal operation [outcome] and [end_monotonic_ns] are present and
    describe the terminal transport event, including the final response write.
    They may be absent if the request fiber is terminated by an unexpected
    runtime failure. [route], when present, is the low-cardinality template
    explicitly supplied to [Router.route]. *)

module type Observer = sig
  type span

  val start_server_request : server_request -> span

  val with_span : span -> (unit -> 'a) -> 'a
  (** Run request processing with [span] as the current tracing context. The
      callback must be invoked exactly once. *)

  val finish_server_request : span -> server_request_result -> unit
end

type t

val of_observer : (module Observer) -> t
(** Package a server-request telemetry observer for [Server.run]. *)

val noop : t
(** An observer that records nothing. *)

module Lifecycle : sig
  type span

  val enabled : t -> bool
  val start_server_request : t -> server_request -> span
  val with_span : span -> (unit -> 'a) -> 'a
  val finish_server_request : span -> server_request_result -> unit
end
(** Operations used by Halo's request runtime to drive the configured
    observer. Application code should use {!of_observer} instead. *)
