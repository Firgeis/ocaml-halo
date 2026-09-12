(** Implementation-neutral request telemetry and a small local tracing SDK.

    This module is the boundary between Halo and tracing/metrics exporters. It
    deliberately does not depend on a particular OpenTelemetry SDK. An observer
    receives validated propagated context, creates an HTTP server span,
    activates that span in [with_span], and derives request metrics from the
    terminal result. The modules below the observer boundary provide local span
    creation, fiber-local propagation, processing, and OTLP/HTTP export without
    depending on a third-party OpenTelemetry package. *)

(** A non-zero 16-byte trace identifier. Its representation is binary; hex is
    produced only at propagation and export boundaries. *)
module Trace_id : sig
  type t

  val of_hex : string -> t option
  val of_bytes : string -> t option
  val to_hex : t -> string
  val to_bytes : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

(** A non-zero 8-byte span identifier. *)
module Span_id : sig
  type t

  val of_hex : string -> t option
  val of_bytes : string -> t option
  val to_hex : t -> string
  val to_bytes : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
end

module Id_generator : sig
  type t

  val create :
    ?random:bool ->
    trace_id:(unit -> Trace_id.t) ->
    span_id:(unit -> Span_id.t) ->
    unit ->
    t
  (** Build a generator. This is useful for deterministic application tests. Set
      [random] only when trace IDs are uniformly random so W3C's random trace
      flag can be propagated correctly. *)

  val secure : t
  (** The default generator, backed by OpenSSL's cryptographic RNG. *)

  val trace_id : t -> Trace_id.t
  val span_id : t -> Span_id.t
  val trace_and_span_id : t -> Trace_id.t * Span_id.t
end

(** Validated, ordered W3C tracestate. *)
module Trace_state : sig
  type t

  val empty : t
  val of_list : (string * string) list -> t option
  val to_list : t -> (string * string) list
  val find : string -> t -> string option

  val add : string -> string -> t -> t option
  (** Add or update a vendor entry at the front of the state. *)

  val remove : string -> t -> t
end

module Trace_context : sig
  type t
  (** A valid W3C span context. Trace and span identifiers use lowercase
      hexadecimal wire format. *)

  val make :
    trace_id:string ->
    span_id:string ->
    trace_flags:int ->
    tracestate:(string * string) list ->
    t option
  (** Build a context after validating all W3C field constraints. *)

  val of_headers : (string * string) list -> t option
  (** Extract a remote context from HTTP fields. Header names are matched
      case-insensitively. An invalid or duplicated [traceparent] returns [None].
      An invalid [tracestate] is discarded without discarding an otherwise valid
      parent. *)

  val trace_id : t -> string
  val span_id : t -> string
  val trace_id_value : t -> Trace_id.t
  val span_id_value : t -> Span_id.t
  val trace_flags : t -> int
  val sampled : t -> bool
  val is_remote : t -> bool
  val tracestate : t -> (string * string) list
  val trace_state : t -> Trace_state.t

  val traceparent : t -> string
  (** Serialize the context using W3C traceparent version [00]. *)

  val tracestate_header : t -> string option

  val to_headers : t -> (string * string) list
  (** Serialize the context as lowercase [traceparent] and, when non-empty,
      [tracestate] HTTP fields for downstream propagation. *)
end

type server_request = {
  request_id : int64;
  method_ : string;
  target : string;
  headers : (string * string) list;
  remote_parent : Trace_context.t option;
  http_version : int * int;
  connection_info : Router.connection_info;
  is_early_data : bool;
  start_time_unix_ns : int64 option;
  start_monotonic_ns : int64;
}
(** Metadata available when Halo dispatches a request.

    [remote_parent] is the valid W3C context extracted from [traceparent] and
    [tracestate], if present. Invalid propagation fields are ignored and do not
    reject the HTTP request.

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

(** Operations used by Halo's request runtime to drive the configured observer.
    Application code should use {!of_observer} instead. *)
module Lifecycle : sig
  type span

  val enabled : t -> bool
  val start_server_request : t -> server_request -> span
  val with_span : span -> (unit -> 'a) -> 'a
  val finish_server_request : span -> server_request_result -> unit
end

module Attribute : sig
  type value =
    | String of string
    | Bool of bool
    | Int of int64
    | Float of float
    | Bytes of string
    | Array of value list

  type t = string * value
end

module Span_kind : sig
  type t = Internal | Server | Client | Producer | Consumer
end

module Sampler : sig
  type decision = Drop | Record_only | Record_and_sample

  type input = {
    parent : Trace_context.t option;
    trace_id : Trace_id.t;
    name : string;
    kind : Span_kind.t;
    attributes : Attribute.t list;
  }

  type t

  val create : (input -> decision) -> t
  val always_on : t
  val always_off : t

  val parent_based : ?root:t -> unit -> t
  (** Follow a valid parent's sampled bit and use [root] for root spans. The
      default root sampler is {!always_on}. *)
end

module Span : sig
  type status = Unset | Ok | Error of string option

  type event = {
    name : string;
    timestamp_unix_ns : int64;
    attributes : Attribute.t list;
  }

  type data = {
    name : string;
    context : Trace_context.t;
    parent_span_id : Span_id.t option;
    kind : Span_kind.t;
    start_time_unix_ns : int64;
    end_time_unix_ns : int64;
    attributes : Attribute.t list;
    events : event list;
    status : status;
    instrumentation_scope : string;
    instrumentation_scope_version : string option;
  }

  type t

  val context : t -> Trace_context.t
  val is_recording : t -> bool
  val set_name : t -> string -> unit
  val set_attribute : t -> string -> Attribute.value -> unit
  val add_attributes : t -> Attribute.t list -> unit

  val add_event :
    ?timestamp_unix_ns:int64 ->
    ?attributes:Attribute.t list ->
    t ->
    string ->
    unit

  val set_status : t -> status -> unit

  val finish : ?end_time_unix_ns:int64 -> t -> unit
  (** Finish once and submit the immutable span to its processor. Repeated calls
      have no effect. *)
end

module Context : sig
  val current_span : unit -> Span.t option
  val current : unit -> Trace_context.t option

  val with_span : Span.t -> (unit -> 'a) -> 'a
  (** Bind the span to the current fiber. Child Eio fibers inherit it. *)
end

module Exporter : sig
  type t

  val create :
    ?force_flush:(unit -> (unit, string) result) ->
    ?shutdown:(unit -> (unit, string) result) ->
    export:(Span.data list -> (unit, string) result) ->
    unit ->
    t

  val export : t -> Span.data list -> (unit, string) result
  val force_flush : t -> (unit, string) result
  val shutdown : t -> (unit, string) result
end

module In_memory_exporter : sig
  type t

  val create : unit -> t
  val exporter : t -> Exporter.t
  val finished_spans : t -> Span.data list
  val clear : t -> unit
end

module Processor : sig
  type t

  val simple : Exporter.t -> t
  (** Export synchronously when each recording span finishes. *)

  val force_flush : t -> (unit, string) result
  val shutdown : t -> (unit, string) result
  val dropped_spans : t -> int
  val last_error : t -> string option
end

module Batch_processor : sig
  val create :
    sw:Eio.Switch.t ->
    ?max_queue_size:int ->
    ?scheduled_delay:float ->
    ?export_timeout:float ->
    ?max_export_batch_size:int ->
    Exporter.t ->
    Processor.t
  (** Create a bounded, asynchronous processor. Finishing a span never waits for
      exporter I/O. Full queues drop new spans and increment
      {!Processor.dropped_spans}. The worker belongs to [sw]. *)
end

module Tracer : sig
  type t

  val create :
    ?id_generator:Id_generator.t ->
    ?sampler:Sampler.t ->
    ?instrumentation_scope:string ->
    ?instrumentation_scope_version:string ->
    processor:Processor.t ->
    unit ->
    t

  val start_span :
    ?kind:Span_kind.t ->
    ?start_time_unix_ns:int64 ->
    ?start_monotonic_ns:int64 ->
    ?attributes:Attribute.t list ->
    t ->
    string ->
    Span.t
  (** Start a span using {!Context.current} as its parent. *)

  val start_span_with_parent :
    parent:Trace_context.t option ->
    ?kind:Span_kind.t ->
    ?start_time_unix_ns:int64 ->
    ?start_monotonic_ns:int64 ->
    ?attributes:Attribute.t list ->
    t ->
    string ->
    Span.t
end

val of_tracer : Tracer.t -> t
(** Create Halo's HTTP server observer using the local tracer. *)

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
        initial request. [jitter] is the randomization fraction applied to each
        computed delay and must be between [0.] and [1.]. *)

    val default : t
    val disabled : t
  end

  type transport =
    endpoint:Uri.t ->
    headers:(string * string) list ->
    body:string ->
    (unit, string) result

  val default_endpoint : Uri.t

  val exporter_with_transport :
    transport:transport ->
    service_name:string ->
    ?endpoint:Uri.t ->
    ?headers:(string * string) list ->
    ?resource_attributes:Attribute.t list ->
    ?retry:Retry.t ->
    unit ->
    Exporter.t
  (** Build an OTLP protobuf exporter over an application-supplied HTTP
      transport. This allows an existing TLS-capable HTTP client to be used
      without coupling Halo to it. The transport must POST [body] as
      [application/x-protobuf] to [endpoint], adding the supplied [headers].
      Transport [Error] results are retried according to [retry]. *)

  val exporter :
    net:_ Eio.Net.t ->
    service_name:string ->
    ?endpoint:Uri.t ->
    ?headers:(string * string) list ->
    ?resource_attributes:Attribute.t list ->
    ?retry:Retry.t ->
    unit ->
    Exporter.t
  (** An OTLP/HTTP protobuf trace exporter using Eio directly. This built-in
      transport supports unencrypted [http] collector endpoints; use
      {!exporter_with_transport} with a TLS-capable client for [https].
      Connection failures and HTTP 429, 502, 503, and 504 responses are retried;
      other HTTP failures are returned immediately. *)
end
