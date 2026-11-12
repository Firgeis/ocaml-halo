let port = 18893

let contains needle text =
  let length = String.length needle in
  let rec search offset =
    offset + length <= String.length text
    && (String.sub text offset length = needle || search (offset + 1))
  in
  search 0

let within env operation =
  Eio.Time.Timeout.run_exn
    (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 5.)
    operation

let wait_for_helper env process seconds =
  Eio.Time.Timeout.run
    (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) seconds)
    (fun () ->
      let rec wait () =
        match Unix.waitpid [ Unix.WNOHANG ] process with
        | 0, _ ->
            Eio_unix.sleep 0.01;
            wait ()
        | _, status -> status
      in
      Ok (wait ()))

let stop_helper env process =
  Eio.Cancel.protect (fun () ->
      match wait_for_helper env process 0.5 with
      | Ok status -> status
      | Error `Timeout -> (
          (try Unix.kill process Sys.sigkill
           with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
          match wait_for_helper env process 1.0 with
          | Ok status -> status
          | Error `Timeout -> Alcotest.fail "helper could not be reaped"))

let with_helper ?(on_exit = fun _ -> ()) env client arguments operation =
  let close_once descriptor =
    let open_ = ref true in
    fun () ->
      if !open_ then (
        open_ := false;
        Unix.close descriptor)
  in
  let client_input, parent_output = Unix.pipe ~cloexec:true () in
  let close_client_input = close_once client_input in
  let close_parent_output = close_once parent_output in
  Fun.protect
    ~finally:(fun () ->
      close_client_input ();
      close_parent_output ())
    (fun () ->
      let parent_input, client_output = Unix.pipe ~cloexec:true () in
      let close_parent_input = close_once parent_input in
      let close_client_output = close_once client_output in
      Fun.protect
        ~finally:(fun () ->
          close_parent_input ();
          close_client_output ())
        (fun () ->
          let process =
            Unix.create_process client arguments client_input client_output
              Unix.stderr
          in
          close_client_input ();
          close_client_output ();
          Fun.protect
            ~finally:(fun () ->
              close_parent_output ();
              close_parent_input ();
              on_exit (stop_helper env process))
            (fun () -> operation parent_input parent_output)))

let tls_reset_client () =
  let client = Sys.getenv "HALO_TLS_RESET_CLIENT" in
  if Filename.is_relative client then "./" ^ client else client

let await_helper_ready env parent_input =
  within env (fun () ->
      Eio_unix.await_readable parent_input;
      let signal = Bytes.create 1 in
      Unix.read parent_input signal 0 1 = 1 && Bytes.get signal 0 = 'R')

let connect () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt_float socket Unix.SO_RCVTIMEO 5.;
  Unix.setsockopt_float socket Unix.SO_SNDTIMEO 5.;
  try
    Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
    socket
  with ex ->
    Unix.close socket;
    raise ex

let write_all socket text =
  let rec write offset =
    if offset < String.length text then
      let written =
        Unix.single_write_substring socket text offset
          (String.length text - offset)
      in
      write (offset + written)
  in
  write 0

let read_all socket =
  let buffer = Buffer.create 256 in
  let bytes = Bytes.create 4096 in
  let rec loop () =
    match Unix.read socket bytes 0 (Bytes.length bytes) with
    | 0 -> Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer bytes 0 count;
        loop ()
  in
  loop ()

let request path =
  let socket = connect () in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      write_all socket
        ("GET " ^ path
       ^ " HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n");
      read_all socket)

let request_async ~sw env path =
  Eio.Fiber.fork_promise ~sw (fun () ->
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
          request path))

let metric name snapshot =
  match
    List.find_opt
      (fun (point : Halo.Metrics.point) -> point.name = name)
      snapshot.Halo.Metrics.points
  with
  | Some { aggregation = Halo.Metrics.Gauge (Halo.Metrics.Int value); _ } ->
      value
  | _ -> Alcotest.fail ("missing gauge " ^ name)

let await_gauge env compute name expected =
  within env (fun () ->
      while metric name (Halo.Compute.metrics compute) <> expected do
        Eio_unix.sleep 0.001
      done)

let with_server ?operation_timeout ?tls ~domain_count ~max_queued make_router
    run =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let compute =
    Halo.Compute.create ~sw ~domain_count ~max_queued ?operation_timeout
      (Eio.Stdenv.domain_mgr env)
  in
  let router = make_router env compute in
  let config = Halo.Server.Config.create ~port ?tls router in
  let shutdown = Halo.Server.Shutdown.create () in
  let ready, notify_ready = Eio.Promise.create () in
  let server =
    Eio.Fiber.fork_promise ~sw (fun () ->
        try
          Fun.protect
            ~finally:(fun () -> Halo.Compute.close compute)
            (fun () ->
              Halo.Server.run ~shutdown ~handle_sigint:false
                ~shutdown_timeout_ms:500
                ~on_ready:(fun _ -> Eio.Promise.resolve notify_ready (Ok ()))
                [ config ])
        with ex ->
          ignore (Eio.Promise.try_resolve notify_ready (Error ex));
          raise ex)
  in
  within env (fun () -> Eio.Promise.await_exn ready);
  Fun.protect
    ~finally:(fun () ->
      Halo.Server.Shutdown.request shutdown;
      Eio.Cancel.protect (fun () ->
          within env (fun () -> Eio.Promise.await_exn server)))
    (fun () -> run env sw compute)

let test_concurrency_responsiveness_and_cancellation () =
  let handler_domain = Atomic.make None in
  let release = Atomic.make false in
  let worker_domains = Eio.Stream.create 2 in
  let queued_started, notify_queued_started = Eio.Promise.create () in
  let queued_ran = Atomic.make false in
  let make_router _env compute =
    let open Halo.Router in
    let open Matcher in
    route
      (let+ () = request_method_is GET *> path_segment "busy" *> path_end in
       fun _request ->
         Atomic.set handler_domain (Some (Domain.self ()));
         match
           Halo.Compute.run compute (fun () ->
               Eio.Stream.add worker_domains (Domain.self ());
               while not (Atomic.get release) do
                 Domain.cpu_relax ()
               done;
               "busy complete")
         with
         | Ok result -> text Status.ok result
         | Error _ -> text Status.internal_server_error "compute failed")
    <|> route
          (let+ () =
             request_method_is GET *> path_segment "queued" *> path_end
           in
           fun _request ->
             Eio.Cancel.sub (fun context ->
                 Eio.Promise.resolve notify_queued_started context;
                 match
                   Halo.Compute.run compute (fun () ->
                       Atomic.set queued_ran true)
                 with
                 | Ok () -> text Status.ok "unexpected"
                 | Error _ -> text Status.service_unavailable "not run"))
    <|> route
          (let+ () = request_method_is GET *> path_segment "fast" *> path_end in
           fun _request -> text Status.ok "fast response")
  in
  with_server ~domain_count:2 ~max_queued:1 make_router (fun env sw compute ->
      Fun.protect
        ~finally:(fun () -> Atomic.set release true)
        (fun () ->
          let first = request_async ~sw env "/busy" in
          let second = request_async ~sw env "/busy" in
          let first_domain, second_domain =
            within env (fun () ->
                (Eio.Stream.take worker_domains, Eio.Stream.take worker_domains))
          in
          let handler = Option.get (Atomic.get handler_domain) in
          Alcotest.(check bool)
            "workers differ from handler domain" true
            (first_domain <> handler && second_domain <> handler);
          Alcotest.(check bool)
            "both compute domains run concurrently" true
            (first_domain <> second_domain);
          let fast = request_async ~sw env "/fast" in
          let response = within env (fun () -> Eio.Promise.await_exn fast) in
          Alcotest.(check bool)
            "fast endpoint responds while workers are occupied" true
            (contains "fast response" response && not (Atomic.get release));
          let queued = request_async ~sw env "/queued" in
          let queued_context =
            within env (fun () -> Eio.Promise.await queued_started)
          in
          await_gauge env compute "halo.compute.jobs.queued" 1L;
          Eio.Cancel.cancel queued_context Exit;
          await_gauge env compute "halo.compute.jobs.queued" 0L;
          ignore (within env (fun () -> Eio.Promise.await queued));
          Atomic.set release true;
          let first_response =
            within env (fun () -> Eio.Promise.await_exn first)
          in
          let second_response =
            within env (fun () -> Eio.Promise.await_exn second)
          in
          Alcotest.(check bool)
            "busy requests complete" true
            (contains "busy complete" first_response
            && contains "busy complete" second_response);
          Alcotest.(check (result unit reject))
            "pool remains usable" (Ok ())
            (Halo.Compute.run compute (fun () -> ()));
          Alcotest.(check bool)
            "cancelled queued request never runs" false (Atomic.get queued_ran)))

let test_timed_out_worker_keeps_capacity () =
  let started, notify_started = Eio.Promise.create () in
  let release = Atomic.make false in
  let make_router env compute =
    let open Halo.Router in
    let open Matcher in
    route
      (let+ () = request_method_is GET *> path_segment "timeout" *> path_end in
       fun _request ->
         let result =
           Eio.Time.Timeout.run
             (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) 0.05)
             (fun () ->
               Ok
                 (Halo.Compute.run compute (fun () ->
                      Eio.Promise.resolve notify_started ();
                      while not (Atomic.get release) do
                        Domain.cpu_relax ()
                      done)))
         in
         match result with
         | Error `Timeout -> text Status.ok "timed out"
         | Ok _ -> text Status.internal_server_error "unexpected result")
    <|> route
          (let+ () = request_method_is GET *> path_segment "try" *> path_end in
           fun _request ->
             match Halo.Compute.run compute (fun () -> ()) with
             | Error `Queue_full -> text Status.ok "full"
             | Ok () -> text Status.ok "available"
             | Error _ -> text Status.internal_server_error "compute failed")
  in
  with_server ~domain_count:1 ~max_queued:0 make_router (fun env sw compute ->
      Fun.protect
        ~finally:(fun () -> Atomic.set release true)
        (fun () ->
          let timed = request_async ~sw env "/timeout" in
          within env (fun () -> Eio.Promise.await started);
          let response = within env (fun () -> Eio.Promise.await_exn timed) in
          Alcotest.(check bool)
            "caller reports timeout" true
            (contains "timed out" response);
          let attempt = request_async ~sw env "/try" in
          let response = within env (fun () -> Eio.Promise.await_exn attempt) in
          Alcotest.(check bool)
            "worker still occupies capacity" true (contains "full" response);
          Alcotest.(check int64)
            "one detached worker" 1L
            (metric "halo.compute.jobs.detached" (Halo.Compute.metrics compute));
          Atomic.set release true;
          await_gauge env compute "halo.compute.jobs.running" 0L;
          let attempt = request_async ~sw env "/try" in
          let response = within env (fun () -> Eio.Promise.await_exn attempt) in
          Alcotest.(check bool)
            "capacity returns after worker exits" true
            (contains "available" response)))

let test_streaming_transport_stays_on_handler_domain () =
  let handler_domain = Atomic.make None in
  let worker_domain = Atomic.make None in
  let body_on_handler = Atomic.make false in
  let producer_on_handler = Atomic.make false in
  let after_compute_on_handler = Atomic.make false in
  let termination_on_handler = Atomic.make false in
  let terminated, notify_terminated = Eio.Promise.create () in
  let make_router _env compute =
    let open Halo.Router in
    let open Matcher in
    route
      (let+ () = request_method_is POST *> path_end *> require_body in
       fun request ->
         let domain = Domain.self () in
         Atomic.set handler_domain (Some domain);
         let body = Halo.Body.read_all (Request.body request) in
         Atomic.set body_on_handler (Domain.self () = domain && body = "hello");
         Request.on_response_terminated request (fun _ ->
             Atomic.set termination_on_handler (Domain.self () = domain);
             Eio.Promise.resolve notify_terminated ());
         Halo.Stream.respond_buffered ~status:Status.ok (fun ~yield ~flush ->
             Atomic.set producer_on_handler (Domain.self () = domain);
             yield "prefix:";
             flush ();
             let result =
               Halo.Compute.run compute (fun () ->
                   Atomic.set worker_domain (Some (Domain.self ()));
                   String.uppercase_ascii body)
             in
             Atomic.set after_compute_on_handler (Domain.self () = domain);
             match result with
             | Ok value ->
                 yield value;
                 flush ()
             | Error _ -> failwith "compute failed in stream"))
  in
  with_server ~domain_count:1 ~max_queued:1 make_router (fun env _sw _compute ->
      let response =
        Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
            let socket = connect () in
            Fun.protect
              ~finally:(fun () -> Unix.close socket)
              (fun () ->
                write_all socket
                  "POST / HTTP/1.1\r\n\
                   Host: localhost\r\n\
                   Content-Length: 5\r\n\
                   Connection: close\r\n\
                   \r\n\
                   hello";
                read_all socket))
      in
      within env (fun () -> Eio.Promise.await terminated);
      Alcotest.(check bool)
        "stream sent worker result" true
        (contains "prefix:" response && contains "HELLO" response);
      Alcotest.(check bool)
        "request body read on handler domain" true
        (Atomic.get body_on_handler);
      Alcotest.(check bool)
        "stream starts on handler domain" true
        (Atomic.get producer_on_handler);
      Alcotest.(check bool)
        "stream resumes on handler domain" true
        (Atomic.get after_compute_on_handler);
      Alcotest.(check bool)
        "response lifecycle runs on handler domain" true
        (Atomic.get termination_on_handler);
      Alcotest.(check bool)
        "worker uses another domain" true
        (match (Atomic.get handler_domain, Atomic.get worker_domain) with
        | Some handler, Some worker -> handler <> worker
        | _ -> false))

let reset socket =
  Unix.setsockopt_optint socket Unix.SO_LINGER (Some 0);
  Unix.close socket

let test_reset_cancels_queued_and_running_work () =
  let started, notify_started = Eio.Promise.create () in
  let release = Atomic.make false in
  let cancelled = Atomic.make false in
  let queued_ran = Atomic.make false in
  let make_router _env compute =
    let open Halo.Router in
    let open Matcher in
    route
      (let+ () = request_method_is GET *> path_segment "running" *> path_end in
       fun _request ->
         ignore
           (Halo.Compute.run_cancellable compute (fun cancellation ->
                Eio.Promise.resolve notify_started ();
                while not (Atomic.get release) do
                  if Halo.Compute.Cancellation.is_cancelled cancellation then
                    Atomic.set cancelled true;
                  Domain.cpu_relax ()
                done));
         text Status.ok "done")
    <|> route
          (let+ () =
             request_method_is GET *> path_segment "queued" *> path_end
           in
           fun _request ->
             ignore
               (Halo.Compute.run compute (fun () -> Atomic.set queued_ran true));
             text Status.ok "done")
  in
  with_server ~domain_count:1 ~max_queued:1 make_router (fun env _sw compute ->
      Fun.protect
        ~finally:(fun () -> Atomic.set release true)
        (fun () ->
          let running = connect () in
          write_all running "GET /running HTTP/1.1\r\nHost: localhost\r\n\r\n";
          within env (fun () -> Eio.Promise.await started);
          let queued = connect () in
          write_all queued "GET /queued HTTP/1.1\r\nHost: localhost\r\n\r\n";
          await_gauge env compute "halo.compute.jobs.queued" 1L;
          (* Leave a pipelined request unread while the first handler waits. *)
          write_all queued "GET /queued HTTP/1.1\r\nHost: localhost\r\n\r\n";
          reset queued;
          await_gauge env compute "halo.compute.jobs.queued" 0L;
          reset running;
          within env (fun () ->
              while not (Atomic.get cancelled) do
                Eio_unix.sleep 0.001
              done);
          Alcotest.(check int64)
            "running job retains capacity" 1L
            (metric "halo.compute.jobs.running" (Halo.Compute.metrics compute));
          Atomic.set release true;
          await_gauge env compute "halo.compute.jobs.running" 0L;
          Alcotest.(check bool)
            "reset queued job never executes" false (Atomic.get queued_ran)))

let test_reset_during_unread_upload () =
  let started, notify_started = Eio.Promise.create () in
  let release = Atomic.make false in
  let cancelled = Atomic.make false in
  let make_router _env compute =
    let open Halo.Router in
    let open Matcher in
    route
      (let+ () = request_method_is POST *> path_end *> require_body in
       fun _request ->
         ignore
           (Halo.Compute.run_cancellable compute (fun cancellation ->
                Eio.Promise.resolve notify_started ();
                while not (Atomic.get release) do
                  if Halo.Compute.Cancellation.is_cancelled cancellation then
                    Atomic.set cancelled true;
                  Domain.cpu_relax ()
                done));
         text Status.ok "done")
  in
  with_server ~domain_count:1 ~max_queued:1 make_router (fun env _sw _compute ->
      Fun.protect
        ~finally:(fun () -> Atomic.set release true)
        (fun () ->
          let socket = connect () in
          write_all socket
            "POST / HTTP/1.1\r\n\
             Host: localhost\r\n\
             Content-Length: 100000\r\n\
             \r\n";
          within env (fun () -> Eio.Promise.await started);
          write_all socket (String.make 4096 'x');
          reset socket;
          within env (fun () ->
              while not (Atomic.get cancelled) do
                Eio_unix.sleep 0.001
              done)))

let test_tls_reset_cancels_queued_work () =
  let release = Atomic.make false in
  let worker_started, notify_worker_started = Eio.Promise.create () in
  let queued_ran = Atomic.make false in
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:(Sys.getenv "HALO_TLS_CERT_FIXTURE")
      ~private_key_path:(Sys.getenv "HALO_TLS_KEY_FIXTURE")
      ()
  in
  let make_router _env compute =
    let open Halo.Router in
    let open Matcher in
    route
      (let+ () = request_method_is GET *> path_end in
       fun _request ->
         ignore
           (Halo.Compute.run compute (fun () -> Atomic.set queued_ran true));
         text Status.ok "done")
  in
  with_server ~tls ~domain_count:1 ~max_queued:1 make_router
    (fun env sw compute ->
      Fun.protect
        ~finally:(fun () -> Atomic.set release true)
        (fun () ->
          let worker =
            Eio.Fiber.fork_promise ~sw (fun () ->
                Halo.Compute.run compute (fun () ->
                    Eio.Promise.resolve notify_worker_started ();
                    while not (Atomic.get release) do
                      Domain.cpu_relax ()
                    done))
          in
          within env (fun () -> Eio.Promise.await worker_started);
          let client = tls_reset_client () in
          with_helper env client
            [| client; string_of_int port |]
            (fun parent_input parent_output ->
              Alcotest.(check bool)
                "TLS request sent" true
                (await_helper_ready env parent_input);
              await_gauge env compute "halo.compute.jobs.queued" 1L;
              write_all parent_output "x";
              await_gauge env compute "halo.compute.jobs.queued" 0L);
          Atomic.set release true;
          ignore (within env (fun () -> Eio.Promise.await_exn worker));
          Alcotest.(check bool)
            "reset TLS job never executes" false (Atomic.get queued_ran)))

let test_helper_cleanup () =
  Eio_main.run @@ fun env ->
  let client = tls_reset_client () in
  let status = ref None in
  let on_exit result = status := Some result in
  (try
     with_helper ~on_exit env client [| client; "--wait-for-eof" |]
       (fun parent_input _parent_output ->
         Alcotest.(check bool)
           "helper is waiting for EOF" true
           (await_helper_ready env parent_input);
         raise Exit)
   with Exit -> ());
  Alcotest.(check bool)
    "helper sees EOF on failure" true
    (!status = Some (Unix.WEXITED 0));
  status := None;
  with_helper ~on_exit env client [| client; "--stubborn" |]
    (fun parent_input _parent_output ->
      Alcotest.(check bool)
        "stubborn helper started" true
        (await_helper_ready env parent_input));
  Alcotest.(check bool)
    "stubborn helper is killed and reaped" true
    (!status = Some (Unix.WSIGNALED Sys.sigkill))

let test_reset_during_streamed_compute () =
  let started, notify_started = Eio.Promise.create () in
  let release = Atomic.make false in
  let cancelled = Atomic.make false in
  let make_router _env compute =
    let open Halo.Router in
    let open Matcher in
    route
      (let+ () = request_method_is GET *> path_end in
       fun _request ->
         Halo.Stream.respond_buffered ~status:Status.ok (fun ~yield ~flush ->
             yield "started";
             flush ();
             ignore
               (Halo.Compute.run_cancellable compute (fun cancellation ->
                    Eio.Promise.resolve notify_started ();
                    while not (Atomic.get release) do
                      if Halo.Compute.Cancellation.is_cancelled cancellation
                      then Atomic.set cancelled true;
                      Domain.cpu_relax ()
                    done))))
  in
  with_server ~domain_count:1 ~max_queued:1 make_router (fun env _sw _compute ->
      Fun.protect
        ~finally:(fun () -> Atomic.set release true)
        (fun () ->
          let socket = connect () in
          write_all socket "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n";
          within env (fun () -> Eio.Promise.await started);
          reset socket;
          within env (fun () ->
              while not (Atomic.get cancelled) do
                Eio_unix.sleep 0.001
              done)))

let test_half_close_receives_response () =
  let started, notify_started = Eio.Promise.create () in
  let release = Atomic.make false in
  let make_router _env compute =
    let open Halo.Router in
    let open Matcher in
    route
      (let+ () = request_method_is GET *> path_end in
       fun _request ->
         match
           Halo.Compute.run compute (fun () ->
               Eio.Promise.resolve notify_started ();
               while not (Atomic.get release) do
                 Domain.cpu_relax ()
               done;
               "half-close response")
         with
         | Ok value -> text Status.ok value
         | Error _ -> text Status.internal_server_error "compute failed")
  in
  with_server ~domain_count:1 ~max_queued:1 make_router (fun env sw _compute ->
      Fun.protect
        ~finally:(fun () -> Atomic.set release true)
        (fun () ->
          let socket = connect () in
          Fun.protect
            ~finally:(fun () -> Unix.close socket)
            (fun () ->
              write_all socket
                "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n";
              within env (fun () -> Eio.Promise.await started);
              Unix.shutdown socket Unix.SHUTDOWN_SEND;
              Eio_unix.sleep 0.1;
              Atomic.set release true;
              let response =
                Eio.Fiber.fork_promise ~sw (fun () ->
                    Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env)
                      (fun () -> read_all socket))
              in
              let response =
                within env (fun () -> Eio.Promise.await_exn response)
              in
              Alcotest.(check bool)
                "half-closed client receives response" true
                (contains "half-close response" response))))

let test_pipelined_request_preserved () =
  let started, notify_started = Eio.Promise.create () in
  let release = Atomic.make false in
  let make_router _env compute =
    let open Halo.Router in
    let open Matcher in
    route
      (let+ () = request_method_is GET *> path_segment "first" *> path_end in
       fun _request ->
         ignore
           (Halo.Compute.run compute (fun () ->
                Eio.Promise.resolve notify_started ();
                while not (Atomic.get release) do
                  Domain.cpu_relax ()
                done));
         text Status.ok "first response")
    <|> route
          (let+ () =
             request_method_is GET *> path_segment "second" *> path_end
           in
           fun _request -> text Status.ok "second response")
  in
  with_server ~domain_count:1 ~max_queued:1 make_router (fun env sw _compute ->
      Fun.protect
        ~finally:(fun () -> Atomic.set release true)
        (fun () ->
          let socket = connect () in
          Fun.protect
            ~finally:(fun () -> Unix.close socket)
            (fun () ->
              write_all socket "GET /first HTTP/1.1\r\nHost: localhost\r\n\r\n";
              within env (fun () -> Eio.Promise.await started);
              write_all socket
                "GET /second HTTP/1.1\r\n\
                 Host: localhost\r\n\
                 Connection: close\r\n\
                 \r\n";
              Eio_unix.sleep 0.15;
              Atomic.set release true;
              let response =
                Eio.Fiber.fork_promise ~sw (fun () ->
                    Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env)
                      (fun () -> read_all socket))
              in
              let response =
                within env (fun () -> Eio.Promise.await_exn response)
              in
              Alcotest.(check bool)
                "both pipelined responses arrive" true
                (contains "first response" response
                && contains "second response" response))))

let () =
  Alcotest.run "compute HTTP integration"
    [
      ( "integration",
        [
          Alcotest.test_case "parallel work and responsive handler" `Quick
            test_concurrency_responsiveness_and_cancellation;
          Alcotest.test_case "timeout retains capacity" `Quick
            test_timed_out_worker_keeps_capacity;
          Alcotest.test_case "streaming domain ownership" `Quick
            test_streaming_transport_stays_on_handler_domain;
          Alcotest.test_case "reset cancels queued and running work" `Quick
            test_reset_cancels_queued_and_running_work;
          Alcotest.test_case "half-close receives response" `Quick
            test_half_close_receives_response;
          Alcotest.test_case "reset during unread upload" `Quick
            test_reset_during_unread_upload;
          Alcotest.test_case "TLS reset cancels queued work" `Quick
            test_tls_reset_cancels_queued_work;
          Alcotest.test_case "pipelined request preserved" `Quick
            test_pipelined_request_preserved;
          Alcotest.test_case "reset during streamed compute" `Quick
            test_reset_during_streamed_compute;
          Alcotest.test_case "helper cleanup" `Quick test_helper_cleanup;
        ] );
    ]
