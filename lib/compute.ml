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

module Cancellation = struct
  type t = bool Atomic.t

  exception Cancelled

  let is_cancelled = Atomic.get
  let check t = if is_cancelled t then raise Cancelled
end

type job_state = Queued | Running | Finished

type job_control = {
  id : int;
  mutable state : job_state;
  cancellation : Cancellation.t;
  queued_at : int64;
  mutable started_at : int64;
  mutable caller_stopped : bool;
}

type shutdown_reason = Closed | Failed of exn

type histogram_state = {
  counts : int64 array;
  mutable count : int64;
  mutable sum : float;
  mutable min : float option;
  mutable max : float option;
}

type observations = {
  start_time_unix_ns : int64;
  domain_count : int;
  max_queued : int;
  mutable completed : int64;
  mutable failed : int64;
  mutable rejected : int64;
  mutable cancelled : int64;
  mutable expired : int64;
  queue_wait : histogram_state;
  execution_duration : histogram_state;
}

type lifecycle = {
  mutex : Mutex.t;
  mutable shutdown_reason : shutdown_reason option;
  jobs : (int, job_control) Hashtbl.t;
  mutable next_id : int;
  shutdown : shutdown_reason Eio.Promise.t;
  notify_shutdown : shutdown_reason Eio.Promise.u;
  stop_supervisor : unit Eio.Promise.t;
  notify_supervisor : unit Eio.Promise.u;
  observations : observations;
}

type t = {
  pool : Eio.Executor_pool.t;
  lifecycle : lifecycle;
  name : string;
  capacity : int;
  queue_timeout : Eio.Time.Timeout.t;
  operation_timeout : Eio.Time.Timeout.t;
}

let shutdown_exception = function
  | Closed -> Pool_closed
  | Failed ex -> Pool_failed ex

let raise_shutdown reason = raise (shutdown_exception reason)

external monotonic_time_ns : unit -> int64
  = "ocaml_halo_telemetry_monotonic_time_ns"

let duration_bounds =
  [|
    0.0001;
    0.0005;
    0.001;
    0.005;
    0.01;
    0.025;
    0.05;
    0.1;
    0.25;
    0.5;
    1.;
    2.5;
    5.;
    10.;
  |]

let create_histogram () =
  {
    counts = Array.make (Array.length duration_bounds + 1) 0L;
    count = 0L;
    sum = 0.;
    min = None;
    max = None;
  }

let record_duration histogram start_time end_time =
  let seconds =
    Int64.to_float (Int64.max 0L (Int64.sub end_time start_time)) /. 1e9
  in
  let rec bucket index =
    if
      index = Array.length duration_bounds || seconds <= duration_bounds.(index)
    then index
    else bucket (index + 1)
  in
  let index = bucket 0 in
  histogram.counts.(index) <- Int64.succ histogram.counts.(index);
  histogram.count <- Int64.succ histogram.count;
  histogram.sum <- histogram.sum +. seconds;
  histogram.min <-
    Some (Option.fold ~none:seconds ~some:(Float.min seconds) histogram.min);
  histogram.max <-
    Some (Option.fold ~none:seconds ~some:(Float.max seconds) histogram.max)

let finish_queued_locked lifecycle control =
  control.state <- Finished;
  record_duration lifecycle.observations.queue_wait control.queued_at
    (monotonic_time_ns ());
  Hashtbl.remove lifecycle.jobs control.id

(* A queued slot belongs to its submitter. Once claimed, only the worker
   releases it, even when the submitter or the pool is cancelled. *)
let cancel_job_locked lifecycle control =
  if control.state <> Finished then (
    Atomic.set control.cancellation true;
    if control.state = Queued then finish_queued_locked lifecycle control)

let cancel_job lifecycle control =
  Mutex.protect lifecycle.mutex (fun () -> cancel_job_locked lifecycle control)

let shutdown lifecycle reason =
  Mutex.protect lifecycle.mutex (fun () ->
      match lifecycle.shutdown_reason with
      | Some _ -> ()
      | None ->
          lifecycle.shutdown_reason <- Some reason;
          let jobs =
            Hashtbl.fold
              (fun _ control jobs -> control :: jobs)
              lifecycle.jobs []
          in
          List.iter (cancel_job_locked lifecycle) jobs;
          Eio.Promise.resolve lifecycle.notify_shutdown reason;
          Eio.Promise.resolve lifecycle.notify_supervisor ())

(* Register directly with the cancellation context so a queued job is
   invalidated before the cancelled submitter is scheduled again. *)
let watch_cancellation on_cancel =
  Effect.perform
    (Eio.Private.Effects.Suspend
       (fun fiber enqueue ->
         Eio.Private.Fiber_context.set_cancel_fn fiber (fun ex ->
             on_cancel ();
             enqueue (Error ex))))

let fork_cancellation_watcher ~sw on_cancel =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      try watch_cancellation on_cancel
      with Eio.Cancel.Cancelled _ -> `Stop_daemon)

let supervisor_reason application_sw pool_sw =
  match Eio.Switch.get_error application_sw with
  | Some _ -> Closed
  | None -> (
      match Eio.Switch.get_error pool_sw with
      | Some (Eio.Cancel.Cancelled ex) -> Failed ex
      | Some ex -> Failed ex
      | None -> Closed)

let monitor_domain_manager lifecycle (Eio.Resource.T (manager, operations)) =
  let module Delegate =
    (val Eio.Resource.get operations Eio.Domain_manager.Pi.Mgr)
  in
  let module Monitor = struct
    type t = unit

    let run () callback =
      match Delegate.run manager callback with
      | value ->
          if not (Eio.Fiber.is_cancelled ()) then
            shutdown lifecycle
              (Failed (Failure "compute worker exited unexpectedly"));
          value
      | exception ex ->
          let backtrace = Printexc.get_raw_backtrace () in
          if not (Eio.Fiber.is_cancelled ()) then shutdown lifecycle (Failed ex);
          Printexc.raise_with_backtrace ex backtrace

    let run_raw () callback = Delegate.run_raw manager callback
  end in
  Eio.Resource.T ((), Eio.Domain_manager.Pi.mgr (module Monitor))

let wait_for_workers ~sw ~domain_count pool =
  let started = Eio.Stream.create domain_count in
  let finished = Eio.Stream.create domain_count in
  let release, notify_release = Eio.Promise.create () in
  for _ = 1 to domain_count do
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Executor_pool.submit_exn pool ~weight:1.0 (fun () ->
            Eio.Stream.add started ();
            Eio.Promise.await release);
        Eio.Stream.add finished ();
        `Stop_daemon)
  done;
  for _ = 1 to domain_count do
    Eio.Stream.take started
  done;
  Eio.Promise.resolve notify_release ();
  for _ = 1 to domain_count do
    Eio.Stream.take finished
  done

let supervise lifecycle ~application_sw ~domain_count domain_mgr notify_ready =
  try
    Eio.Switch.run ~name:"halo-compute-pool" (fun sw ->
        fork_cancellation_watcher ~sw (fun () ->
            let reason = supervisor_reason application_sw sw in
            shutdown lifecycle reason;
            ignore (Eio.Promise.try_resolve notify_ready (Error reason)));
        let domain_mgr = monitor_domain_manager lifecycle domain_mgr in
        let pool = Eio.Executor_pool.create ~sw ~domain_count domain_mgr in
        match
          Eio.Fiber.first
            (fun () ->
              wait_for_workers ~sw ~domain_count pool;
              `Ready)
            (fun () -> `Stopped (Eio.Promise.await lifecycle.shutdown))
        with
        | `Stopped reason ->
            ignore (Eio.Promise.try_resolve notify_ready (Error reason))
        | `Ready ->
            ignore (Eio.Promise.try_resolve notify_ready (Ok pool));
            Eio.Promise.await lifecycle.stop_supervisor)
  with ex ->
    let reason =
      match ex with Eio.Cancel.Cancelled _ -> Closed | _ -> Failed ex
    in
    shutdown lifecycle reason;
    ignore (Eio.Promise.try_resolve notify_ready (Error reason))

let create ~sw ~domain_count ~max_queued ?(name = "compute")
    ?(queue_timeout = Eio.Time.Timeout.none)
    ?(operation_timeout = Eio.Time.Timeout.none) domain_mgr =
  if domain_count <= 0 then
    invalid_arg "Compute.create: domain_count must be positive";
  if max_queued < 0 || max_queued > max_int - domain_count then
    invalid_arg "Compute.create: invalid max_queued";
  if name = "" then invalid_arg "Compute.create: name must not be empty";
  let shutdown_promise, notify_shutdown = Eio.Promise.create () in
  let stop_supervisor, notify_supervisor = Eio.Promise.create () in
  let lifecycle =
    {
      mutex = Mutex.create ();
      shutdown_reason = None;
      jobs = Hashtbl.create 16;
      next_id = 0;
      shutdown = shutdown_promise;
      notify_shutdown;
      stop_supervisor;
      notify_supervisor;
      observations =
        {
          start_time_unix_ns = Int64.of_float (Unix.gettimeofday () *. 1e9);
          domain_count;
          max_queued;
          completed = 0L;
          failed = 0L;
          rejected = 0L;
          cancelled = 0L;
          expired = 0L;
          queue_wait = create_histogram ();
          execution_duration = create_histogram ();
        };
    }
  in
  let ready, notify_ready = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      supervise lifecycle ~application_sw:sw ~domain_count domain_mgr
        notify_ready;
      `Stop_daemon);
  let pool =
    try
      match
        Eio.Fiber.first
          (fun () -> Eio.Promise.await ready)
          (fun () -> Error (Eio.Promise.await lifecycle.shutdown))
      with
      | Ok pool -> pool
      | Error reason -> raise_shutdown reason
    with ex ->
      let backtrace = Printexc.get_raw_backtrace () in
      shutdown lifecycle Closed;
      Printexc.raise_with_backtrace ex backtrace
  in
  {
    pool;
    lifecycle;
    name;
    capacity = domain_count + max_queued;
    queue_timeout;
    operation_timeout;
  }

let name t = t.name
let close t = shutdown t.lifecycle Closed

let snapshot_histogram histogram =
  let total = ref 0L in
  let bucket_counts =
    Array.to_list histogram.counts
    |> List.map (fun count ->
        total := Int64.add !total count;
        !total)
  in
  Metrics.
    {
      count = histogram.count;
      sum = histogram.sum;
      explicit_bounds = Array.to_list duration_bounds;
      bucket_counts;
      min = histogram.min;
      max = histogram.max;
    }

let count_jobs jobs =
  Hashtbl.fold
    (fun _ control (queued, running, detached) ->
      match control.state with
      | Queued -> (queued + 1, running, detached)
      | Running ->
          ( queued,
            running + 1,
            detached + if control.caller_stopped then 1 else 0 )
      | Finished -> (queued, running, detached))
    jobs (0, 0, 0)

let metric_points name observations (queued, running, detached) =
  let attributes = [ ("halo.compute.pool.name", Metrics.String name) ] in
  let point name description unit_ aggregation =
    Metrics.{ name; description; unit_; attributes; aggregation }
  in
  let gauge name description unit_ value =
    point name description unit_
      (Metrics.Gauge (Metrics.Int (Int64.of_int value)))
  in
  let counter outcome value =
    Metrics.
      {
        name = "halo.compute.jobs";
        description = "Compute job outcomes.";
        unit_ = "{job}";
        attributes = ("outcome", String outcome) :: attributes;
        aggregation = Sum { value = Int value; monotonic = true };
      }
  in
  let histogram name description value =
    point name description "s" (Metrics.Histogram (snapshot_histogram value))
  in
  [
    gauge "halo.compute.domains.configured" "Configured compute domains."
      "{domain}" observations.domain_count;
    gauge "halo.compute.queue.capacity" "Maximum queued compute jobs." "{job}"
      observations.max_queued;
    gauge "halo.compute.jobs.queued" "Admitted jobs waiting for a worker."
      "{job}" queued;
    gauge "halo.compute.jobs.running" "Jobs currently executing on workers."
      "{job}" running;
    gauge "halo.compute.jobs.detached"
      "Jobs still executing after their callers stopped waiting." "{job}"
      detached;
    counter "completed" observations.completed;
    counter "failed" observations.failed;
    counter "rejected" observations.rejected;
    counter "cancelled" observations.cancelled;
    counter "expired" observations.expired;
    histogram "halo.compute.queue.wait"
      "Time admitted jobs spent waiting in the queue." observations.queue_wait;
    histogram "halo.compute.job.duration"
      "Time jobs spent executing on a worker." observations.execution_duration;
  ]

let metrics t =
  Mutex.protect t.lifecycle.mutex (fun () ->
      let observations = t.lifecycle.observations in
      let counts = count_jobs t.lifecycle.jobs in
      Metrics.
        {
          start_time_unix_ns = observations.start_time_unix_ns;
          time_unix_ns = Int64.of_float (Unix.gettimeofday () *. 1e9);
          points = metric_points t.name observations counts;
        })

let reject lifecycle ex =
  lifecycle.observations.rejected <- Int64.succ lifecycle.observations.rejected;
  raise ex

let check_admission_locked lifecycle capacity =
  (match lifecycle.shutdown_reason with
  | Some reason -> reject lifecycle (shutdown_exception reason)
  | None -> ());
  if Hashtbl.length lifecycle.jobs >= capacity then reject lifecycle Queue_full

let next_job_id_locked lifecycle =
  let next id = if id = max_int then 0 else id + 1 in
  let rec find_available id =
    if Hashtbl.mem lifecycle.jobs id then find_available (next id) else id
  in
  let id = find_available lifecycle.next_id in
  lifecycle.next_id <- next id;
  id

let admit t =
  Mutex.protect t.lifecycle.mutex (fun () ->
      check_admission_locked t.lifecycle t.capacity;
      let id = next_job_id_locked t.lifecycle in
      let control =
        {
          id;
          state = Queued;
          cancellation = Atomic.make false;
          queued_at = monotonic_time_ns ();
          started_at = 0L;
          caller_stopped = false;
        }
      in
      Hashtbl.add t.lifecycle.jobs id control;
      control)

let start_job lifecycle control =
  Mutex.protect lifecycle.mutex (fun () ->
      if control.state = Queued then (
        let now = monotonic_time_ns () in
        control.state <- Running;
        control.started_at <- now;
        record_duration lifecycle.observations.queue_wait control.queued_at now;
        true)
      else false)

let finish_running_job lifecycle control outcome =
  Mutex.protect lifecycle.mutex (fun () ->
      record_duration lifecycle.observations.execution_duration
        control.started_at (monotonic_time_ns ());
      (match outcome with
      | `Completed ->
          lifecycle.observations.completed <-
            Int64.succ lifecycle.observations.completed
      | `Failed ->
          lifecycle.observations.failed <-
            Int64.succ lifecycle.observations.failed
      | `Interrupted -> ());
      control.state <- Finished;
      Hashtbl.remove lifecycle.jobs control.id)

let worker_outcome cancellation = function
  | Ok _ -> `Completed
  | Error (`Job_failed (Cancellation.Cancelled, _))
    when Cancellation.is_cancelled cancellation ->
      `Interrupted
  | Error _ -> `Failed

let run_job t control notify_started job () =
  if start_job t.lifecycle control then
    let outcome = ref `Interrupted in
    Fun.protect
      ~finally:(fun () -> finish_running_job t.lifecycle control !outcome)
      (fun () ->
        Cancellation.check control.cancellation;
        ignore (Eio.Promise.try_resolve notify_started (Ok ()));
        let result = job control.cancellation in
        outcome := worker_outcome control.cancellation result;
        result)
  else raise Queue_timeout

let executor_exception lifecycle ex =
  match ex with
  | Eio.Cancel.Cancelled _ ->
      Eio.Fiber.check ();
      shutdown_exception (Eio.Promise.await lifecycle.shutdown)
  | Invalid_argument _ ->
      shutdown lifecycle (Failed ex);
      shutdown_exception (Eio.Promise.await lifecycle.shutdown)
  | _ -> ex

let submit_to_executor t control notify_started job () =
  try
    Eio.Executor_pool.submit_exn t.pool ~weight:1.0
      (run_job t control notify_started job)
  with ex ->
    let backtrace = Printexc.get_raw_backtrace () in
    (* The caller is already being woken by cancellation or shutdown. *)
    (match ex with
    | (Cancellation.Cancelled | Queue_timeout)
      when Cancellation.is_cancelled control.cancellation ->
        Printexc.raise_with_backtrace ex backtrace
    | _ -> ());
    let ex = executor_exception t.lifecycle ex in
    ignore (Eio.Promise.try_resolve notify_started (Error ex));
    Printexc.raise_with_backtrace ex backtrace

let await_start t control started =
  match
    Eio.Time.Timeout.run t.queue_timeout (fun () ->
        Eio.Switch.run (fun queue_sw ->
            let waiting = Atomic.make true in
            fork_cancellation_watcher ~sw:queue_sw (fun () ->
                if Atomic.compare_and_set waiting true false then
                  cancel_job t.lifecycle control);
            let result = Eio.Promise.await_exn started in
            Atomic.set waiting false;
            Ok result))
  with
  | Ok () -> ()
  | Error `Timeout ->
      cancel_job t.lifecycle control;
      raise Queue_timeout

let submit_on_switch t control job sw =
  fork_cancellation_watcher ~sw (fun () -> cancel_job t.lifecycle control);
  let started, notify_started = Eio.Promise.create () in
  let result =
    Eio.Fiber.fork_promise ~sw (submit_to_executor t control notify_started job)
  in
  await_start t control started;
  Eio.Promise.await_exn result

let resolve_shutdown_on_completion lifecycle control outcome =
  match outcome with
  | `Completed _ when Cancellation.is_cancelled control.cancellation -> (
      let reason =
        Mutex.protect lifecycle.mutex (fun () -> lifecycle.shutdown_reason)
      in
      match reason with Some reason -> `Stopped reason | None -> outcome)
  | _ -> outcome

let await_result_or_shutdown t control job =
  let outcome =
    Eio.Fiber.first
      (fun () ->
        try `Completed (Eio.Switch.run (submit_on_switch t control job)) with
        | Pool_closed -> `Stopped Closed
        | Pool_failed ex -> `Stopped (Failed ex))
      (fun () -> `Stopped (Eio.Promise.await t.lifecycle.shutdown))
  in
  Eio.Fiber.check ();
  resolve_shutdown_on_completion t.lifecycle control outcome

let await_operation t control job =
  match
    Eio.Time.Timeout.run t.operation_timeout (fun () ->
        Ok (await_result_or_shutdown t control job))
  with
  | Ok (`Completed value) -> value
  | Ok (`Stopped reason) -> raise_shutdown reason
  | Error `Timeout -> raise Operation_timeout

let record_stopped_caller lifecycle control ex =
  Mutex.protect lifecycle.mutex (fun () ->
      control.caller_stopped <- true;
      match ex with
      | Queue_timeout | Operation_timeout ->
          lifecycle.observations.expired <-
            Int64.succ lifecycle.observations.expired
      | Eio.Cancel.Cancelled _ | Pool_closed | Pool_failed _ ->
          lifecycle.observations.cancelled <-
            Int64.succ lifecycle.observations.cancelled
      | _ -> ())

let submit_raw t job =
  let control = admit t in
  try await_operation t control job
  with ex ->
    let backtrace = Printexc.get_raw_backtrace () in
    cancel_job t.lifecycle control;
    record_stopped_caller t.lifecycle control ex;
    Printexc.raise_with_backtrace ex backtrace

let run_with_trace_context trace_context job cancellation =
  let backtrace_recording_enabled = Printexc.backtrace_status () in
  if not backtrace_recording_enabled then Printexc.record_backtrace true;
  Fun.protect
    ~finally:(fun () ->
      if not backtrace_recording_enabled then Printexc.record_backtrace false)
    (fun () ->
      Telemetry.Context.with_trace_context trace_context (fun () ->
          match job cancellation with
          | value -> Ok value
          | exception ex ->
              let backtrace = Printexc.get_raw_backtrace () in
              Eio.Fiber.check ();
              Error (`Job_failed (ex, backtrace))))

let run_cancellable ?trace_context t job =
  let trace_context =
    match trace_context with
    | Some context -> Some context
    | None -> Telemetry.Context.current ()
  in
  try submit_raw t (run_with_trace_context trace_context job) with
  | Queue_full -> Error `Queue_full
  | Queue_timeout -> Error `Queue_timeout
  | Operation_timeout -> Error `Operation_timeout
  | Pool_closed -> Error `Pool_closed
  | Pool_failed ex -> Error (`Pool_failed ex)

let run ?trace_context t job =
  run_cancellable ?trace_context t (fun _cancellation -> job ())

let submit ?trace_context t job =
  match run ?trace_context t job with
  | Ok value -> value
  | Error `Queue_full -> raise Queue_full
  | Error `Queue_timeout -> raise Queue_timeout
  | Error `Operation_timeout -> raise Operation_timeout
  | Error `Pool_closed -> raise Pool_closed
  | Error (`Pool_failed ex) -> raise (Pool_failed ex)
  | Error (`Job_failed (ex, backtrace)) ->
      Printexc.raise_with_backtrace ex backtrace
