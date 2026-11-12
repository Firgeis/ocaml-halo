let timeout env seconds =
  Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) seconds

let contains needle text =
  let length = String.length needle in
  let rec search index =
    index + length <= String.length text
    && (String.sub text index length = needle || search (index + 1))
  in
  search 0

let metric name ?outcome snapshot =
  match
    List.find_opt
      (fun (point : Halo.Metrics.point) ->
        point.name = name
        && Option.fold ~none:true
             ~some:(fun outcome ->
               List.mem
                 ("outcome", Halo.Metrics.String outcome)
                 point.attributes)
             outcome)
      snapshot.Halo.Metrics.points
  with
  | Some point -> point
  | None -> Alcotest.fail ("missing metric " ^ name)

let metric_value name ?outcome snapshot =
  match (metric name ?outcome snapshot).Halo.Metrics.aggregation with
  | Halo.Metrics.Gauge (Halo.Metrics.Int value)
  | Halo.Metrics.Sum { value = Halo.Metrics.Int value; _ } ->
      value
  | _ -> Alcotest.fail ("unexpected aggregation for " ^ name)

let histogram name snapshot =
  match (metric name snapshot).Halo.Metrics.aggregation with
  | Halo.Metrics.Histogram histogram -> histogram
  | _ -> Alcotest.fail ("expected histogram for " ^ name)

let test_metrics () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:1 ~name:"image-cpu"
      ~queue_timeout:(timeout env 0.03) ~operation_timeout:(timeout env 0.09)
      (Eio.Stdenv.domain_mgr env)
  in
  let started, notify_started = Eio.Promise.create () in
  let finished, notify_finished = Eio.Promise.create () in
  let unblock = Eio.Semaphore.make 0 in
  let running =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.run compute (fun () ->
            Eio.Promise.resolve notify_started ();
            Eio.Semaphore.acquire unblock;
            Eio.Promise.resolve notify_finished ()))
  in
  Eio.Promise.await started;
  let queued =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.run compute (fun () -> ()))
  in
  Eio.Fiber.yield ();
  Alcotest.(check bool)
    "full queue rejected" true
    (Halo.Compute.run compute (fun () -> ()) = Error `Queue_full);
  let snapshot = Halo.Compute.metrics compute in
  let check name expected =
    Alcotest.(check int64) name expected (metric_value name snapshot)
  in
  check "halo.compute.domains.configured" 1L;
  check "halo.compute.queue.capacity" 1L;
  check "halo.compute.jobs.running" 1L;
  check "halo.compute.jobs.queued" 1L;
  check "halo.compute.jobs.detached" 0L;
  Alcotest.(check bool)
    "pool name label" true
    (List.mem
       ("halo.compute.pool.name", Halo.Metrics.String "image-cpu")
       (metric "halo.compute.jobs.running" snapshot).attributes);
  Alcotest.(check bool)
    "queue expires" true
    (Eio.Promise.await_exn queued = Error `Queue_timeout);
  Alcotest.(check bool)
    "operation expires" true
    (Eio.Promise.await_exn running = Error `Operation_timeout);
  let detached = Halo.Compute.metrics compute in
  Alcotest.(check int64)
    "expired counter" 2L
    (metric_value "halo.compute.jobs" ~outcome:"expired" detached);
  Alcotest.(check int64)
    "rejected counter" 1L
    (metric_value "halo.compute.jobs" ~outcome:"rejected" detached);
  Alcotest.(check int64)
    "detached running job" 1L
    (metric_value "halo.compute.jobs.detached" detached);
  Alcotest.(check int64)
    "two queue observations" 2L
    (histogram "halo.compute.queue.wait" detached).count;
  Alcotest.(check int64)
    "running execution not finished" 0L
    (histogram "halo.compute.job.duration" detached).count;
  Eio.Semaphore.release unblock;
  Eio.Promise.await finished;
  let rec await_completion () =
    let snapshot = Halo.Compute.metrics compute in
    if metric_value "halo.compute.jobs.running" snapshot <> 0L then (
      Eio.Fiber.yield ();
      await_completion ())
    else snapshot
  in
  let completed = await_completion () in
  Alcotest.(check int64)
    "detached cleared" 0L
    (metric_value "halo.compute.jobs.detached" completed);
  Alcotest.(check int64)
    "eventual completion" 1L
    (metric_value "halo.compute.jobs" ~outcome:"completed" completed);
  let execution = histogram "halo.compute.job.duration" completed in
  Alcotest.(check int64) "one execution" 1L execution.count;
  Alcotest.(check bool) "positive execution time" true (execution.sum > 0.0);
  Alcotest.(check int64)
    "histogram +Inf" execution.count
    (List.hd (List.rev execution.bucket_counts));
  (match Halo.Compute.run compute (fun () -> raise Exit) with
  | Error (`Job_failed (Exit, _)) -> ()
  | _ -> Alcotest.fail "expected job failure");
  let failed = Halo.Compute.metrics compute in
  Alcotest.(check int64)
    "failed counter" 1L
    (metric_value "halo.compute.jobs" ~outcome:"failed" failed);
  Alcotest.(check int64)
    "older snapshot remains unchanged" 0L
    (metric_value "halo.compute.jobs" ~outcome:"expired" snapshot);
  let server_metrics = Halo.Metrics.create () in
  let request =
    Halo.Metrics.Private.start_request server_metrics ~method_:"GET"
      ~scheme:"http" ~start_monotonic_ns:1L
  in
  Halo.Metrics.Private.finish_request request ~route:None
    ~status_code:(Some 200) ~outcome:"completed" ~end_monotonic_ns:(Some 10L)
    ~request_body_size:0L ~response_body_size:0L;
  let combined =
    Halo.Metrics.combine [ Halo.Metrics.snapshot server_metrics; failed ]
  in
  let prometheus = Halo.Metrics.Prometheus.encode combined in
  Alcotest.(check bool)
    "Prometheus includes labeled pool" true
    (contains "halo_compute_pool_name=\"image-cpu\"" prometheus);
  Alcotest.(check bool)
    "combined response includes HTTP and compute" true
    (contains "http_server_request_duration_seconds" prometheus
    && contains "halo_compute_job_duration_seconds" prometheus);
  let exported = ref "" in
  let exporter =
    Halo.Metrics.Otlp_http.exporter_with_transport
      ~transport:(fun ~endpoint:_ ~headers:_ ~body ->
        exported := body;
        Ok ())
      ~service_name:"compute-test" ()
  in
  Alcotest.(check (result unit string))
    "OTLP exports combined metrics" (Ok ())
    (Halo.Metrics.Otlp_http.export exporter combined);
  Alcotest.(check bool)
    "OTLP includes both metric families" true
    (contains "halo.compute.job.duration" !exported
    && contains "http.server.request.duration" !exported)

let test_concurrent_snapshots () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:2 ~max_queued:2
      (Eio.Stdenv.domain_mgr env)
  in
  let ready, notify_ready = Eio.Promise.create () in
  let sampler =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
            Eio.Promise.resolve notify_ready ();
            for _iteration = 1 to 4000 do
              let snapshot = Halo.Compute.metrics compute in
              let running = metric_value "halo.compute.jobs.running" snapshot in
              let queued = metric_value "halo.compute.jobs.queued" snapshot in
              let detached =
                metric_value "halo.compute.jobs.detached" snapshot
              in
              if
                running < 0L || queued < 0L || running > 2L
                || Int64.add running queued > 4L
                || detached > running
              then failwith "inconsistent concurrent compute gauges";
              let wait = histogram "halo.compute.queue.wait" snapshot in
              if List.hd (List.rev wait.bucket_counts) <> wait.count then
                failwith "inconsistent concurrent histogram"
            done))
  in
  Eio.Promise.await ready;
  for _iteration = 1 to 100 do
    match Halo.Compute.run compute (fun () -> 1) with
    | Ok 1 -> ()
    | _ -> Alcotest.fail "compute submission failed during sampling"
  done;
  Eio.Promise.await_exn sampler;
  Alcotest.(check int64)
    "all worker completions recorded" 100L
    (metric_value "halo.compute.jobs" ~outcome:"completed"
       (Halo.Compute.metrics compute))

let test_trace_context () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:1
      (Eio.Stdenv.domain_mgr env)
  in
  let memory = Halo.Telemetry.In_memory_exporter.create () in
  let processor =
    Halo.Telemetry.In_memory_exporter.exporter memory
    |> Halo.Telemetry.Processor.simple
  in
  let tracer = Halo.Telemetry.Tracer.create ~processor () in
  let parent = Halo.Telemetry.Tracer.start_span tracer "request" in
  let context = Halo.Telemetry.Span.context parent in
  let result =
    Halo.Telemetry.Context.with_span parent (fun () ->
        Halo.Compute.run compute (fun () ->
            let captured = Halo.Telemetry.Context.current () in
            let mutable_span = Halo.Telemetry.Context.current_span () in
            let child = Halo.Telemetry.Tracer.start_span tracer "worker" in
            Halo.Telemetry.Span.finish child;
            (captured, Option.is_none mutable_span)))
  in
  (match result with
  | Ok (Some captured, true) ->
      Alcotest.(check string)
        "context crosses domain"
        (Halo.Telemetry.Trace_context.traceparent context)
        (Halo.Telemetry.Trace_context.traceparent captured)
  | _ -> Alcotest.fail "worker did not receive isolated trace context");
  (match Halo.Telemetry.In_memory_exporter.finished_spans memory with
  | [ child ] ->
      Alcotest.(check bool)
        "child uses submitting span as parent" true
        (child.Halo.Telemetry.Span.parent_span_id
        = Some (Halo.Telemetry.Trace_context.span_id_value context))
  | _ -> Alcotest.fail "expected one worker span");
  let explicit =
    Halo.Telemetry.Trace_context.make
      ~trace_id:"4bf92f3577b34da6a3ce929d0e0e4736" ~span_id:"00f067aa0ba902b7"
      ~trace_flags:1 ~tracestate:[]
    |> Option.get
  in
  let override =
    Halo.Telemetry.Context.with_span parent (fun () ->
        Halo.Compute.run ~trace_context:explicit compute (fun () ->
            Halo.Telemetry.Context.current ()))
  in
  (match override with
  | Ok (Some actual) ->
      Alcotest.(check string)
        "explicit context overrides current"
        (Halo.Telemetry.Trace_context.traceparent explicit)
        (Halo.Telemetry.Trace_context.traceparent actual)
  | _ -> Alcotest.fail "explicit context missing");
  Alcotest.(check bool)
    "context cleared for next job" true
    (Halo.Compute.run compute (fun () -> Halo.Telemetry.Context.current ())
    = Ok None);
  Halo.Telemetry.Span.finish parent

let test_basic () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:2 ~max_queued:2 ~name:"cpu"
      (Eio.Stdenv.domain_mgr env)
  in
  Alcotest.(check string) "pool name" "cpu" (Halo.Compute.name compute);
  let caller = Domain.self () in
  let worker = Halo.Compute.submit compute Domain.self in
  Alcotest.(check bool) "runs on another domain" true (caller <> worker);
  Alcotest.(check int)
    "returns result" 42
    (Halo.Compute.submit compute (fun () -> 42));
  Alcotest.(check (result int reject))
    "run returns result" (Ok 42)
    (Halo.Compute.run compute (fun () -> 42));
  Alcotest.check_raises "propagates job exception" Exit (fun () ->
      ignore (Halo.Compute.submit compute (fun () -> raise Exit)));
  Alcotest.(check (result int reject))
    "pool survives job exception" (Ok 43)
    (Halo.Compute.run compute (fun () -> 43))

let[@inline never] fail_on_worker () = raise Halo.Compute.Queue_full

let test_job_failure () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:0
      (Eio.Stdenv.domain_mgr env)
  in
  match Halo.Compute.run compute fail_on_worker with
  | Error (`Job_failed (Halo.Compute.Queue_full, backtrace)) ->
      Alcotest.(check bool)
        "worker backtrace captured" true
        (Printexc.raw_backtrace_length backtrace > 0);
      let location =
        Printexc.Slot.location
          (Printexc.convert_raw_backtrace_slot
             (Printexc.get_raw_backtrace_slot backtrace 0))
      in
      Alcotest.(check bool)
        "backtrace points to job" true
        (match location with
        | Some location ->
            Filename.basename location.filename = "compute_unit.ml"
        | None -> false)
  | _ -> Alcotest.fail "expected application failure with worker backtrace"

let test_handler_fiber_suspends () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:0
      (Eio.Stdenv.domain_mgr env)
  in
  let started, notify = Eio.Promise.create () in
  let stop = Atomic.make false in
  Fun.protect
    ~finally:(fun () -> Atomic.set stop true)
    (fun () ->
      let work =
        Eio.Fiber.fork_promise ~sw (fun () ->
            Halo.Compute.run compute (fun () ->
                Eio.Promise.resolve notify ();
                while not (Atomic.get stop) do
                  Domain.cpu_relax ()
                done;
                42))
      in
      Eio.Promise.await started;
      (* This fiber can make progress while the worker consumes CPU. *)
      Atomic.set stop true;
      Alcotest.(check (result int reject))
        "worker result" (Ok 42)
        (Eio.Promise.await_exn work))

let test_queue_limits () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:1
      ~queue_timeout:(timeout env 0.05)
      (Eio.Stdenv.domain_mgr env)
  in
  let started, notify = Eio.Promise.create () in
  let unblock = Eio.Semaphore.make 0 in
  let first =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.submit compute (fun () ->
            Eio.Promise.resolve notify ();
            Eio.Semaphore.acquire unblock))
  in
  Eio.Promise.await started;
  let ran = Atomic.make false in
  let queued =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.run compute (fun () -> Atomic.set ran true))
  in
  Eio.Fiber.yield ();
  Alcotest.(check bool)
    "run reports overload" true
    (Halo.Compute.run compute (fun () -> ()) = Error `Queue_full);
  Alcotest.check_raises "full queue rejects" Halo.Compute.Queue_full (fun () ->
      Halo.Compute.submit compute (fun () -> ()));
  Alcotest.(check bool)
    "run reports queue deadline" true
    (Eio.Promise.await_exn queued = Error `Queue_timeout);
  let replacement =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.run compute (fun () -> 17))
  in
  Eio.Semaphore.release unblock;
  Eio.Promise.await_exn first;
  Alcotest.(check (result int reject))
    "expired job releases queue capacity" (Ok 17)
    (Eio.Promise.await_exn replacement);
  Alcotest.(check bool) "expired job did not run" false (Atomic.get ran);
  Alcotest.(check int)
    "capacity restored" 7
    (Halo.Compute.submit compute (fun () -> 7))

let test_running_timeout () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:0
      ~operation_timeout:(timeout env 0.05)
      (Eio.Stdenv.domain_mgr env)
  in
  let started, notify = Eio.Promise.create () in
  let finished, notify_finished = Eio.Promise.create () in
  let unblock = Eio.Semaphore.make 0 in
  let job =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.run_cancellable compute (fun cancellation ->
            Eio.Promise.resolve notify cancellation;
            Eio.Semaphore.acquire unblock;
            Eio.Promise.resolve notify_finished ()))
  in
  let cancellation = Eio.Promise.await started in
  Alcotest.(check bool)
    "run reports operation deadline" true
    (Eio.Promise.await_exn job = Error `Operation_timeout);
  Alcotest.(check bool)
    "running job is signalled" true
    (Halo.Compute.Cancellation.is_cancelled cancellation);
  Alcotest.check_raises "running job retains slot" Halo.Compute.Queue_full
    (fun () -> Halo.Compute.submit compute (fun () -> ()));
  Eio.Semaphore.release unblock;
  Eio.Promise.await finished;
  (* The notification is sent just before the worker releases its slot. *)
  let rec wait_for_slot () =
    try Halo.Compute.submit compute (fun () -> ())
    with Halo.Compute.Queue_full ->
      Eio.Fiber.yield ();
      wait_for_slot ()
  in
  wait_for_slot ()

let test_cooperative_cancellation ~operation_deadline () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let operation_timeout =
    if operation_deadline then timeout env 0.05 else Eio.Time.Timeout.none
  in
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:0 ~operation_timeout
      (Eio.Stdenv.domain_mgr env)
  in
  let started, notify_started = Eio.Promise.create () in
  let stopped, notify_stopped = Eio.Promise.create () in
  let observed = Atomic.make false in
  let emergency_stop = Atomic.make false in
  Fun.protect
    ~finally:(fun () -> Atomic.set emergency_stop true)
    (fun () ->
      let work =
        Eio.Fiber.fork_promise ~sw (fun () ->
            let submit () =
              Halo.Compute.run_cancellable compute (fun cancellation ->
                  Fun.protect
                    ~finally:(fun () -> Eio.Promise.resolve notify_stopped ())
                    (fun () ->
                      Eio.Promise.resolve notify_started ();
                      try
                        while not (Atomic.get emergency_stop) do
                          Halo.Compute.Cancellation.check cancellation;
                          Domain.cpu_relax ()
                        done
                      with Halo.Compute.Cancellation.Cancelled ->
                        Atomic.set observed true))
            in
            if operation_deadline then submit ()
            else
              match
                Eio.Time.Timeout.run (timeout env 0.05) (fun () ->
                    Ok (submit ()))
              with
              | Error `Timeout -> Error `Operation_timeout
              | Ok result -> result)
      in
      Eio.Promise.await started;
      Alcotest.(check bool)
        "caller stopped waiting" true
        (Eio.Promise.await_exn work = Error `Operation_timeout);
      Eio.Time.Timeout.run_exn (timeout env 2.) (fun () ->
          Eio.Promise.await stopped);
      Alcotest.(check bool)
        "worker observed cancellation" true (Atomic.get observed);
      let rec wait_for_capacity () =
        match Halo.Compute.run compute (fun () -> 42) with
        | Error `Queue_full ->
            Eio.Fiber.yield ();
            wait_for_capacity ()
        | Ok 42 -> ()
        | _ -> Alcotest.fail "capacity was not restored"
      in
      Eio.Time.Timeout.run_exn (timeout env 2.) wait_for_capacity)

let test_cancel_queued_before_worker_starts () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:1
      (Eio.Stdenv.domain_mgr env)
  in
  for _iteration = 1 to 30 do
    let started, notify_started = Eio.Promise.create () in
    let unblock = Eio.Semaphore.make 0 in
    let running =
      Eio.Fiber.fork_promise ~sw (fun () ->
          Halo.Compute.run compute (fun () ->
              Eio.Promise.resolve notify_started ();
              Eio.Semaphore.acquire unblock))
    in
    Eio.Promise.await started;
    let context, notify_context = Eio.Promise.create () in
    let ran = Atomic.make false in
    let queued =
      Eio.Fiber.fork_promise ~sw (fun () ->
          Eio.Cancel.sub (fun cancellation_context ->
              Eio.Promise.resolve notify_context cancellation_context;
              Halo.Compute.run_cancellable compute (fun _cancellation ->
                  Atomic.set ran true)))
    in
    let cancellation_context = Eio.Promise.await context in
    Alcotest.(check bool)
      "queue is full" true
      (Halo.Compute.run compute (fun () -> ()) = Error `Queue_full);
    Eio.Cancel.cancel cancellation_context Exit;
    Eio.Semaphore.release unblock;
    (match Eio.Promise.await queued with
    | Error (Eio.Cancel.Cancelled _) -> ()
    | _ -> Alcotest.fail "queued caller did not cancel");
    ignore (Eio.Promise.await_exn running);
    Alcotest.(check (result int reject))
      "capacity restored" (Ok 1)
      (Halo.Compute.run compute (fun () -> 1));
    Alcotest.(check bool) "cancelled job never ran" false (Atomic.get ran)
  done

let test_slot_accounting () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:2 ~max_queued:1
      (Eio.Stdenv.domain_mgr env)
  in
  let started = Eio.Stream.create 2 in
  let unblock = Eio.Semaphore.make 0 in
  let worker () =
    Halo.Compute.run compute (fun () ->
        Eio.Stream.add started ();
        Eio.Semaphore.acquire unblock)
  in
  let first = Eio.Fiber.fork_promise ~sw worker in
  let second = Eio.Fiber.fork_promise ~sw worker in
  Eio.Stream.take started;
  Eio.Stream.take started;
  let context, notify_context = Eio.Promise.create () in
  let cancelled_ran = Atomic.make false in
  let cancelled =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Eio.Cancel.sub (fun cancellation_context ->
            Eio.Promise.resolve notify_context cancellation_context;
            Halo.Compute.run compute (fun () -> Atomic.set cancelled_ran true)))
  in
  let cancellation_context = Eio.Promise.await context in
  let snapshot = Halo.Compute.metrics compute in
  Alcotest.(check int64)
    "two running jobs" 2L
    (metric_value "halo.compute.jobs.running" snapshot);
  Alcotest.(check int64)
    "one queued job" 1L
    (metric_value "halo.compute.jobs.queued" snapshot);
  Alcotest.(check bool)
    "queue rejects excess job" true
    (Halo.Compute.run compute (fun () -> ()) = Error `Queue_full);
  Eio.Cancel.cancel cancellation_context Exit;
  (match Eio.Promise.await cancelled with
  | Error (Eio.Cancel.Cancelled _) -> ()
  | _ -> Alcotest.fail "queued caller did not cancel");
  let replacement_started, notify_replacement = Eio.Promise.create () in
  let replacement_unblock = Eio.Semaphore.make 0 in
  let replacement =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.run compute (fun () ->
            Eio.Promise.resolve notify_replacement ();
            Eio.Semaphore.acquire replacement_unblock))
  in
  Alcotest.(check bool)
    "replacement fills the released queue slot" true
    (Halo.Compute.run compute (fun () -> ()) = Error `Queue_full);
  Eio.Semaphore.release unblock;
  Eio.Promise.await replacement_started;
  let snapshot = Halo.Compute.metrics compute in
  Alcotest.(check int64)
    "replacement holds the worker slot" 2L
    (metric_value "halo.compute.jobs.running" snapshot);
  Alcotest.(check int64)
    "replacement is no longer queued" 0L
    (metric_value "halo.compute.jobs.queued" snapshot);
  Eio.Semaphore.release unblock;
  Eio.Semaphore.release replacement_unblock;
  List.iter
    (fun promise ->
      Alcotest.(check (result unit reject))
        "worker completes" (Ok ())
        (Eio.Promise.await_exn promise))
    [ first; second; replacement ];
  let snapshot = Halo.Compute.metrics compute in
  Alcotest.(check bool)
    "cancelled job never ran" false (Atomic.get cancelled_ran);
  Alcotest.(check int64)
    "no running jobs remain" 0L
    (metric_value "halo.compute.jobs.running" snapshot);
  Alcotest.(check int64)
    "no queued jobs remain" 0L
    (metric_value "halo.compute.jobs.queued" snapshot);
  Alcotest.(check int64)
    "completed jobs counted once" 3L
    (metric_value "halo.compute.jobs" ~outcome:"completed" snapshot);
  Alcotest.(check int64)
    "cancelled job counted once" 1L
    (metric_value "halo.compute.jobs" ~outcome:"cancelled" snapshot);
  Alcotest.(check int64)
    "rejections counted" 2L
    (metric_value "halo.compute.jobs" ~outcome:"rejected" snapshot)

let test_cancellation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:1
      (Eio.Stdenv.domain_mgr env)
  in
  let started, notify = Eio.Promise.create () in
  let unblock = Eio.Semaphore.make 0 in
  let first =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.submit compute (fun () ->
            Eio.Promise.resolve notify ();
            Eio.Semaphore.acquire unblock))
  in
  Eio.Promise.await started;
  let ran = Atomic.make false in
  let result =
    Eio.Time.Timeout.run (timeout env 0.05) (fun () ->
        Ok (Halo.Compute.run compute (fun () -> Atomic.set ran true)))
  in
  Alcotest.(check bool) "caller cancelled" true (result = Error `Timeout);
  Alcotest.(check int64)
    "cancellation counted" 1L
    (metric_value "halo.compute.jobs" ~outcome:"cancelled"
       (Halo.Compute.metrics compute));
  Eio.Semaphore.release unblock;
  Eio.Promise.await_exn first;
  Alcotest.(check bool) "cancelled job did not run" false (Atomic.get ran);
  Alcotest.(check int)
    "capacity restored" 9
    (Halo.Compute.submit compute (fun () -> 9))

let test_two_workers () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:2 ~max_queued:0
      (Eio.Stdenv.domain_mgr env)
  in
  let starts = Eio.Stream.create 2 in
  let unblock = Eio.Semaphore.make 0 in
  let run () =
    Halo.Compute.submit compute (fun () ->
        Eio.Stream.add starts (Domain.self ());
        Eio.Semaphore.acquire unblock)
  in
  let first = Eio.Fiber.fork_promise ~sw run in
  let second = Eio.Fiber.fork_promise ~sw run in
  let worker1 = Eio.Stream.take starts in
  let worker2 = Eio.Stream.take starts in
  Alcotest.(check bool) "jobs run on distinct workers" true (worker1 <> worker2);
  Alcotest.check_raises "both worker slots occupied" Halo.Compute.Queue_full
    (fun () -> Halo.Compute.submit compute (fun () -> ()));
  Eio.Semaphore.release unblock;
  Eio.Semaphore.release unblock;
  Eio.Promise.await_exn first;
  Eio.Promise.await_exn second

let test_closed_pool () =
  Eio_main.run @@ fun env ->
  let compute =
    Eio.Switch.run (fun sw ->
        Halo.Compute.create ~sw ~domain_count:1 ~max_queued:0
          (Eio.Stdenv.domain_mgr env))
  in
  Alcotest.(check bool)
    "run reports closed pool" true
    (Halo.Compute.run compute (fun () -> ()) = Error `Pool_closed);
  Alcotest.check_raises "submit reports closed pool" Halo.Compute.Pool_closed
    (fun () -> Halo.Compute.submit compute (fun () -> ()))

let test_close_with_queued_job () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun outer_sw ->
  let first, queued =
    Eio.Switch.run (fun pool_sw ->
        let compute =
          Halo.Compute.create ~sw:pool_sw ~domain_count:1 ~max_queued:1
            (Eio.Stdenv.domain_mgr env)
        in
        let started, notify = Eio.Promise.create () in
        let blocked = Eio.Semaphore.make 0 in
        let first =
          Eio.Fiber.fork_promise ~sw:outer_sw (fun () ->
              Halo.Compute.run compute (fun () ->
                  Eio.Promise.resolve notify ();
                  Eio.Semaphore.acquire blocked))
        in
        Eio.Promise.await started;
        let queued =
          Eio.Fiber.fork_promise ~sw:outer_sw (fun () ->
              Halo.Compute.run compute (fun () -> ()))
        in
        Eio.Fiber.yield ();
        (first, queued))
  in
  Alcotest.(check bool)
    "queued job reports closed pool" true
    (Eio.Promise.await_exn queued = Error `Pool_closed);
  Alcotest.(check bool)
    "cancelled worker reports closed pool" true
    (Eio.Promise.await_exn first = Error `Pool_closed)

let test_explicit_close () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count:1 ~max_queued:1
      (Eio.Stdenv.domain_mgr env)
  in
  let started, notify_started = Eio.Promise.create () in
  let finish = Atomic.make false in
  let queued_ran = Atomic.make false in
  Fun.protect
    ~finally:(fun () -> Atomic.set finish true)
    (fun () ->
      let running =
        Eio.Fiber.fork_promise ~sw (fun () ->
            Halo.Compute.run_cancellable compute (fun cancellation ->
                Eio.Promise.resolve notify_started cancellation;
                while not (Atomic.get finish) do
                  Domain.cpu_relax ()
                done))
      in
      let cancellation = Eio.Promise.await started in
      let queued =
        Eio.Fiber.fork_promise ~sw (fun () ->
            Halo.Compute.run compute (fun () -> Atomic.set queued_ran true))
      in
      Eio.Fiber.yield ();
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
          Halo.Compute.close compute);
      Halo.Compute.close compute;
      Eio.Time.Timeout.run_exn (timeout env 2.) (fun () ->
          Alcotest.(check bool)
            "running caller released" true
            (Eio.Promise.await_exn running = Error `Pool_closed);
          Alcotest.(check bool)
            "queued caller released" true
            (Eio.Promise.await_exn queued = Error `Pool_closed));
      Alcotest.(check bool)
        "worker signalled" true
        (Halo.Compute.Cancellation.is_cancelled cancellation);
      Alcotest.(check bool)
        "queued job did not run" false (Atomic.get queued_ran);
      Alcotest.(check bool)
        "closed admission" true
        (Halo.Compute.run compute (fun () -> ()) = Error `Pool_closed);
      let snapshot = Halo.Compute.metrics compute in
      Alcotest.(check int64)
        "shutdown cancellations counted" 2L
        (metric_value "halo.compute.jobs" ~outcome:"cancelled" snapshot);
      Alcotest.(check int64)
        "closed submission rejected" 1L
        (metric_value "halo.compute.jobs" ~outcome:"rejected" snapshot);
      Alcotest.(check int64)
        "shutdown worker remains detached" 1L
        (metric_value "halo.compute.jobs.detached" snapshot))

let test_partial_startup_failure () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let attempts = ref 0 in
  let module Failing_manager = struct
    type t = unit

    let run () callback =
      incr attempts;
      if !attempts = 2 then failwith "startup worker failure";
      let cancelled, _ = Eio.Promise.create () in
      callback ~cancelled

    let run_raw () _ = failwith "unexpected run_raw"
  end in
  let manager =
    Eio.Resource.T ((), Eio.Domain_manager.Pi.mgr (module Failing_manager))
  in
  Eio.Time.Timeout.run_exn (timeout env 2.) (fun () ->
      match Halo.Compute.create ~sw ~domain_count:2 ~max_queued:0 manager with
      | _ -> Alcotest.fail "partial startup unexpectedly succeeded"
      | exception Halo.Compute.Pool_failed (Failure message) ->
          Alcotest.(check string)
            "startup cause" "startup worker failure" message
      | exception ex -> raise ex)

let test_cooperative_close () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  for _iteration = 1 to 20 do
    let compute =
      Halo.Compute.create ~sw ~domain_count:1 ~max_queued:0
        (Eio.Stdenv.domain_mgr env)
    in
    let started, notify_started = Eio.Promise.create () in
    let emergency_stop = Atomic.make false in
    Fun.protect
      ~finally:(fun () ->
        Atomic.set emergency_stop true;
        Halo.Compute.close compute)
      (fun () ->
        let running =
          Eio.Fiber.fork_promise ~sw (fun () ->
              Halo.Compute.run_cancellable compute (fun cancellation ->
                  Eio.Promise.resolve notify_started ();
                  while not (Atomic.get emergency_stop) do
                    Halo.Compute.Cancellation.check cancellation;
                    Domain.cpu_relax ()
                  done))
        in
        Eio.Promise.await started;
        Halo.Compute.close compute;
        Eio.Time.Timeout.run_exn (timeout env 2.) (fun () ->
            Alcotest.(check bool)
              "cooperative worker reports closure" true
              (Eio.Promise.await_exn running = Error `Pool_closed)))
  done

let test_worker_failure () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let crash, trigger_crash = Eio.Promise.create () in
  let module Failing_manager = struct
    type t = unit

    let run () callback =
      let cancelled, _ = Eio.Promise.create () in
      Eio.Fiber.first
        (fun () -> callback ~cancelled)
        (fun () ->
          Eio.Promise.await crash;
          failwith "worker infrastructure failure")

    let run_raw () _ = failwith "unexpected run_raw"
  end in
  let manager =
    Eio.Resource.T ((), Eio.Domain_manager.Pi.mgr (module Failing_manager))
  in
  let compute = Halo.Compute.create ~sw ~domain_count:1 ~max_queued:1 manager in
  let started, notify_started = Eio.Promise.create () in
  let blocked = Eio.Semaphore.make 0 in
  let running =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.run compute (fun () ->
            Eio.Promise.resolve notify_started ();
            Eio.Semaphore.acquire blocked))
  in
  Eio.Promise.await started;
  let queued =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Compute.run compute (fun () -> ()))
  in
  Eio.Fiber.yield ();
  Eio.Promise.resolve trigger_crash ();
  let is_worker_failure = function
    | Error (`Pool_failed (Failure message)) ->
        message = "worker infrastructure failure"
    | _ -> false
  in
  Eio.Time.Timeout.run_exn (timeout env 2.) (fun () ->
      Alcotest.(check bool)
        "running caller sees worker failure" true
        (is_worker_failure (Eio.Promise.await_exn running));
      Alcotest.(check bool)
        "queued caller sees worker failure" true
        (is_worker_failure (Eio.Promise.await_exn queued));
      Alcotest.(check bool)
        "new submissions see worker failure" true
        (is_worker_failure (Halo.Compute.run compute (fun () -> ()))))

let test_validation () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let create domain_count max_queued =
    Halo.Compute.create ~sw ~domain_count ~max_queued
      (Eio.Stdenv.domain_mgr env)
  in
  let invalid create_pool =
    match create_pool () with
    | _ -> Alcotest.fail "expected Invalid_argument"
    | exception Invalid_argument _ -> ()
  in
  invalid (fun () -> create 0 1);
  invalid (fun () -> create 1 (-1));
  invalid (fun () -> create 1 max_int)

let () =
  Alcotest.run "compute"
    [
      ( "pool",
        [
          Alcotest.test_case "compute metrics" `Quick test_metrics;
          Alcotest.test_case "concurrent snapshots" `Quick
            test_concurrent_snapshots;
          Alcotest.test_case "trace context" `Quick test_trace_context;
          Alcotest.test_case "execution" `Quick test_basic;
          Alcotest.test_case "job failure backtrace" `Quick test_job_failure;
          Alcotest.test_case "handler fiber suspends" `Quick
            test_handler_fiber_suspends;
          Alcotest.test_case "queue limits" `Quick test_queue_limits;
          Alcotest.test_case "running timeout" `Quick test_running_timeout;
          Alcotest.test_case "cooperative timeout" `Quick
            (test_cooperative_cancellation ~operation_deadline:true);
          Alcotest.test_case "cooperative caller cancellation" `Quick
            (test_cooperative_cancellation ~operation_deadline:false);
          Alcotest.test_case "queued cancellation race" `Quick
            test_cancel_queued_before_worker_starts;
          Alcotest.test_case "deterministic slot accounting" `Quick
            test_slot_accounting;
          Alcotest.test_case "cancellation" `Quick test_cancellation;
          Alcotest.test_case "two workers" `Quick test_two_workers;
          Alcotest.test_case "closed pool" `Quick test_closed_pool;
          Alcotest.test_case "close with queued job" `Quick
            test_close_with_queued_job;
          Alcotest.test_case "explicit close" `Quick test_explicit_close;
          Alcotest.test_case "partial startup failure" `Quick
            test_partial_startup_failure;
          Alcotest.test_case "cooperative close" `Quick test_cooperative_close;
          Alcotest.test_case "worker failure" `Quick test_worker_failure;
          Alcotest.test_case "configuration" `Quick test_validation;
        ] );
    ]
