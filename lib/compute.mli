type t
(** Application-owned CPU worker pool.

    Create one pool under an application switch and pass it to handlers in their
    closures. It is independent of server listeners and reactors. *)

exception Queue_full
exception Queue_timeout
exception Operation_timeout
exception Pool_closed
exception Pool_failed of exn

type error =
  [ `Queue_full
  | `Queue_timeout
  | `Operation_timeout
  | `Pool_closed
  | `Pool_failed of exn
  | `Job_failed of exn * Printexc.raw_backtrace ]
(** Submission failures and exceptions raised by application jobs. *)

module Cancellation : sig
  type t
  (** A job's thread-safe cooperative cancellation signal. *)

  exception Cancelled

  val is_cancelled : t -> bool
  (** Whether the caller was cancelled, a deadline expired, or the pool is
      shutting down. *)

  val check : t -> unit
  (** Raise [Cancelled] if cancellation has been signalled. Long-running CPU
      jobs should call this regularly, including inside expensive loops. *)
end

val create :
  sw:Eio.Switch.t ->
  domain_count:int ->
  max_queued:int ->
  ?name:string ->
  ?queue_timeout:Eio.Time.Timeout.t ->
  ?operation_timeout:Eio.Time.Timeout.t ->
  _ Eio.Domain_manager.t ->
  t
(** [create ~sw ~domain_count ~max_queued domain_mgr] starts [domain_count]
    workers and waits for all of them to accept work. [domain_count] must be
    positive and [max_queued] non-negative. If startup fails, raises
    [Pool_failed] with the worker infrastructure exception. The switch owns the
    pool and must outlive all users. [name] is a stable, nonempty pool
    identifier for metrics; it defaults to ["compute"].

    Timeouts default to unlimited. The queue timeout covers the wait until a
    worker starts the job; the operation timeout covers that wait and execution.
    Use a monotonic clock to construct timeout values. *)

val name : t -> string
(** The configured pool identifier. *)

val metrics : t -> Metrics.snapshot
(** Take a thread-safe, immutable snapshot for {!Metrics.Prometheus} or
    {!Metrics.Otlp_http}. Every point has the [halo.compute.pool.name]
    attribute. It contains configured domains and queue capacity, queued and
    running jobs, jobs still running after their callers stopped waiting,
    cumulative job outcome counters, and queue-wait and execution-duration
    histograms.

    Queue wait is recorded for every admitted job when it starts or leaves the
    queue. Execution duration is recorded when a worker exits a job. [completed]
    and [failed] count worker outcomes; [cancelled] and [expired] count callers
    that stopped waiting, so a non-cooperative job may contribute to both
    categories. [rejected] counts submissions refused because the queue is full
    or the pool has closed or failed. Startup probes are excluded. This snapshot
    owns its data and can be exported independently of any request or server
    collector. *)

val close : t -> unit
(** [close t] stops admission, wakes queued and running callers with
    [`Pool_closed], and signals cancellation to all admitted jobs. It is
    thread-safe, idempotent, and does not wait for running jobs or worker
    domains to exit. Call it after all servers sharing the pool have drained.
    The owning switch waits for worker domains when it finishes. OCaml domains
    cannot be forcibly killed safely; a non-cooperative job can therefore delay
    switch shutdown indefinitely. Use separate processes for work requiring a
    hard termination deadline. *)

val run :
  ?trace_context:Telemetry.Trace_context.t ->
  t ->
  (unit -> 'a) ->
  ('a, error) result
(** [run t job] executes [job] on a compute domain and suspends the calling
    fiber while waiting, allowing other fibers on the handler domain to run. It
    returns [`Job_failed (exn, backtrace)] for an application exception, with
    the backtrace captured on the worker. Pool overload, queue and operation
    deadlines, pool closure, and worker infrastructure failure have distinct
    error variants. A worker infrastructure failure closes admission and wakes
    all callers; ordinary job exceptions leave the pool usable. Caller
    cancellation propagates normally.

    At most [domain_count + max_queued] jobs are admitted, including jobs
    running or waiting to start. Cancellation and deadlines stop waiting but
    cannot preempt CPU code already running; it retains its slot until it
    finishes. A peer reset can cancel a handler when the reactor detects it, but
    a clean TCP half-close or silent network failure does not necessarily
    indicate that the peer has stopped waiting. Use a deadline when the wait
    must be bounded. A job should receive only domain-safe application data.
    Read request bodies, write responses, and perform request lifecycle
    operations on the handler domain; do not pass request objects or
    unsynchronized mutable state to a worker. Bounded input bytes can be
    collected on the handler and then decoded on a worker. *)

val run_cancellable :
  ?trace_context:Telemetry.Trace_context.t ->
  t ->
  (Cancellation.t -> 'a) ->
  ('a, error) result
(** Like [run], but passes a cooperative cancellation signal to the job. Halo
    checks the signal before calling the job. If the caller is cancelled or a
    deadline expires, or the pool closes after execution starts, Halo signals it
    and the caller may stop waiting. The job should call {!Cancellation.check}
    periodically to stop promptly. The worker retains its slot until the job
    actually exits, even if it ignores cancellation. The signal is intended for
    use during the job and may be checked from any domain.

    All submission functions capture {!Telemetry.Context.current} on the caller
    unless [trace_context] is supplied. The immutable context is bound on the
    worker, where [Telemetry.Context.current] returns it and
    [Telemetry.Context.current_span] is [None]; request-owned mutable spans are
    not transferred. Child spans may use that context as their parent. *)

val submit : ?trace_context:Telemetry.Trace_context.t -> t -> (unit -> 'a) -> 'a
(** [submit t job] runs [job] on a compute domain, with executor weight 1.0, and
    returns its result or propagates its exception. At most
    [domain_count + max_queued] jobs are admitted, including jobs running or
    waiting to start; excess submissions raise [Queue_full] immediately.

    Raises [Queue_timeout] or [Operation_timeout] when the corresponding limit
    expires, [Pool_closed] when the pool closes, or [Pool_failed] after a worker
    infrastructure failure. Job exceptions are re-raised with their worker
    backtraces. Cancellation and timeouts stop waiting, but cannot preempt CPU
    code already running. Such a job retains its admission slot until it
    finishes. Jobs must use only values safe to access from another domain and
    should not access request-scoped Eio resources. *)
