(** OpenTelemetry-shaped HTTP and Halo runtime metrics.

    A collector is updated by [Server.run ~metrics]. Use
    [Server.collect_metrics] to combine request measurements with a fresh
    reactor snapshot before encoding it for an exporter. *)

type attribute_value = String of string | Int of int64 | Bool of bool
type attribute = string * attribute_value
type number = Int of int64 | Float of float

type histogram = {
  count : int64;
  sum : float;
  explicit_bounds : float list;
  bucket_counts : int64 list;
  min : float option;
  max : float option;
}
(** A cumulative explicit-bucket histogram. [bucket_counts] contains one
    cumulative count for every explicit bound followed by the [+Inf] count. *)

type aggregation =
  | Gauge of number
  | Sum of { value : number; monotonic : bool }
  | Histogram of histogram

type point = {
  name : string;
  description : string;
  unit_ : string;
  attributes : attribute list;
  aggregation : aggregation;
}

type snapshot = {
  start_time_unix_ns : int64;
  time_unix_ns : int64;
  points : point list;
}

type t

val create :
  ?duration_buckets:float list -> ?size_buckets:float list -> unit -> t
(** Create a cumulative collector. Bucket boundaries must be finite, strictly
    increasing, and non-negative. *)

val noop : t
(** A collector that discards every observation. *)

val snapshot : t -> snapshot
(** Take an immutable snapshot. Runtime points are present after the first
    [Server.metrics] or [Server.collect_metrics] call. *)

val combine : snapshot list -> snapshot
(** Combine immutable snapshots, such as server metrics and one or more compute
    pools. Raises [Invalid_argument] if two points have the same name and
    attributes; use distinct pool names. The combined snapshot uses the earliest
    start time and latest observation time among nonempty inputs. Export
    snapshots separately if exact per-source start times are required. *)

module Prometheus : sig
  val content_type : string

  val encode : snapshot -> string
  (** Encode Prometheus text exposition format 0.0.4. OpenTelemetry dots are
      translated to underscores, units become conventional suffixes, and
      monotonic sums receive the [_total] suffix. *)
end

module Otlp_http : sig
  module Retry : sig
    type t

    val create :
      ?max_attempts:int ->
      ?initial_delay:float ->
      ?max_delay:float ->
      ?multiplier:float ->
      ?jitter:float ->
      unit ->
      t
    (** Configure bounded exponential backoff. [max_attempts] includes the
        initial request. *)

    val default : t
    val disabled : t
  end

  type transport =
    endpoint:Uri.t ->
    headers:(string * string) list ->
    body:string ->
    (unit, string) result

  type exporter

  val default_endpoint : Uri.t

  val exporter_with_transport :
    transport:transport ->
    service_name:string ->
    ?endpoint:Uri.t ->
    ?headers:(string * string) list ->
    ?resource_attributes:attribute list ->
    ?instrumentation_scope:string ->
    ?instrumentation_scope_version:string ->
    ?retry:Retry.t ->
    unit ->
    exporter
  (** Build an OTLP protobuf exporter around an application-supplied HTTP
      transport. Transport errors are retried according to [retry]. *)

  val exporter :
    net:_ Eio.Net.t ->
    service_name:string ->
    ?endpoint:Uri.t ->
    ?headers:(string * string) list ->
    ?resource_attributes:attribute list ->
    ?instrumentation_scope:string ->
    ?instrumentation_scope_version:string ->
    ?retry:Retry.t ->
    unit ->
    exporter
  (** An Eio OTLP/HTTP protobuf exporter. The built-in transport accepts
      unencrypted [http] endpoints; use {!exporter_with_transport} for an
      existing TLS-capable client. *)

  val export : exporter -> snapshot -> (unit, string) result
  (** Export one cumulative OTLP protobuf snapshot. *)

  val shutdown : exporter -> unit
  (** Reject subsequent exports. *)
end

(** Internal operations used by Halo's request and reactor runtime. They are
    exposed so alternate Halo runtimes can feed the same collector. *)
module Private : sig
  type request

  val enabled : t -> bool

  val start_request :
    t -> method_:string -> scheme:string -> start_monotonic_ns:int64 -> request

  val finish_request :
    request ->
    route:string option ->
    status_code:int option ->
    outcome:string ->
    end_monotonic_ns:int64 option ->
    request_body_size:int64 ->
    response_body_size:int64 ->
    unit

  val record_runtime :
    t ->
    utilization:float ->
    active_connections:int64 ->
    active_requests:int64 ->
    accepted_connections:int64 ->
    rejected_connections:int64 ->
    completed_requests:int64 ->
    tls_full_handshakes:int64 ->
    tls_resumed_handshakes:int64 ->
    tls_handshake_failures:int64 ->
    event_queue_depth:int64 ->
    command_queue_depth:int64 ->
    controller_state:string ->
    backpressure_enabled:bool ->
    unit
end
