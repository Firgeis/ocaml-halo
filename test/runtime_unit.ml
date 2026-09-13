module Status = Halo.Router.Status

let assert_int64_equal label expected actual =
  Alcotest.(check int64) label expected actual

let fail fmt = Printf.ksprintf (fun message -> Alcotest.fail message) fmt

let controller_state = function
  | Halo.Server.Normal -> "normal"
  | Halo.Server.Busy -> "busy"
  | Halo.Server.Saturated -> "saturated"
  | Halo.Server.Recovering -> "recovering"

let contains needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search index =
    if index + needle_length > haystack_length then
      false
    else if String.sub haystack index needle_length = needle then
      true
    else
      search (index + 1)
  in
  search 0

let tls_cert_fixture_path () =
  match Sys.getenv_opt "HALO_TLS_CERT_FIXTURE" with
  | Some path -> path
  | None -> fail "expected HALO_TLS_CERT_FIXTURE in the test environment"

let tls_key_fixture_path () =
  match Sys.getenv_opt "HALO_TLS_KEY_FIXTURE" with
  | Some path -> path
  | None -> fail "expected HALO_TLS_KEY_FIXTURE in the test environment"

let tls_ticket_keys_fixture_path () =
  match Sys.getenv_opt "HALO_TLS_TICKET_KEYS_FIXTURE" with
  | Some path -> path
  | None -> fail "expected HALO_TLS_TICKET_KEYS_FIXTURE in the test environment"

let s2nc_bin_path () =
  match Sys.getenv_opt "HALO_S2NC_BIN" with
  | Some path -> path
  | None -> fail "expected HALO_S2NC_BIN in the test environment"

let write_all channel text =
  output_string channel text;
  flush channel

let with_temp_file_contents prefix suffix contents f =
  let path = Filename.temp_file prefix suffix in
  Out_channel.with_open_bin path (fun channel -> output_string channel contents);
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let with_temp_path prefix suffix f =
  let path = Filename.temp_file prefix suffix in
  Sys.remove path;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> f path)

type process_result = {
  stdout : string;
  stderr : string;
  status : Unix.process_status;
}

let run_process ?input ?(close_stdin = true) command =
  let input_channel, output_channel, error_channel =
    Unix.open_process_full command (Unix.environment ())
  in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr input_channel;
      close_out_noerr output_channel;
      close_in_noerr error_channel)
    (fun () ->
      Option.iter (write_all output_channel) input;
      if close_stdin then
        close_out output_channel;
      let stdout = In_channel.input_all input_channel in
      let stderr = In_channel.input_all error_channel in
      let status =
        Unix.close_process_full (input_channel, output_channel, error_channel)
      in
      { stdout; stderr; status })

let run_s2nc ?ticket_out ?ticket_in ?early_data_file ?send_file ?input ?alpn
    ?(echo = false) ?(close_stdin = false) ?(port = 8443) () =
  let args =
    [
      Filename.quote (s2nc_bin_path ());
      "-c default_tls13";
      "-f";
      Filename.quote (tls_cert_fixture_path ());
      "--name localhost";
    ]
    @
    (match ticket_out with
    | None -> []
    | Some path -> ["--ticket-out"; Filename.quote path])
    @
    (match ticket_in with
    | None -> []
    | Some path -> ["--ticket-in"; Filename.quote path])
    @
    (match early_data_file with
    | None -> []
    | Some path -> ["--early-data"; Filename.quote path])
    @
    (match send_file with
    | None -> []
    | Some path -> ["--send-file"; Filename.quote path])
    @
    (match alpn with
    | None -> []
    | Some protocols -> ["--alpn"; Filename.quote protocols])
    @ (if echo then ["--echo"] else [])
    @ ["127.0.0.1"; string_of_int port]
  in
  run_process ?input ~close_stdin (String.concat " " args)

let assert_nonempty_file path =
  match Unix.stat path with
  | { Unix.st_size; _ } when st_size > 0 -> ()
  | _ -> fail "expected non-empty file: %s" path
  | exception Unix.Unix_error (_, _, _) ->
      fail "expected file to exist: %s" path

let assert_process_success result =
  match result.status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED code ->
      fail "process exited with code %d\nstdout:\n%s\nstderr:\n%s" code
        result.stdout result.stderr
  | Unix.WSIGNALED signal ->
      fail "process terminated by signal %d\nstdout:\n%s\nstderr:\n%s" signal
        result.stdout result.stderr
  | Unix.WSTOPPED signal ->
      fail "process stopped by signal %d\nstdout:\n%s\nstderr:\n%s" signal
        result.stdout result.stderr

let with_live_config_groups ?max_in_flight_requests ?on_error ?telemetry
    ?metrics config_groups f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let servers =
    List.map
      (fun configs ->
        let shutdown = Halo.Server.Shutdown.create () in
        let started, notify_started = Eio.Promise.create () in
        let server =
          Eio.Fiber.fork_promise ~sw (fun () ->
              Halo.Server.run ~shutdown ~handle_sigint:false
                ?max_in_flight_requests ?on_error ?telemetry ?metrics
                ~on_ready:(fun running ->
                  Eio.Promise.resolve notify_started running)
                configs)
        in
        (started, server))
      config_groups
  in
  let runnings = List.map (fun (started, _) -> Eio.Promise.await started) servers in
  let outcome =
    try
      Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) f;
      Ok (List.map Halo.Server.metrics runnings)
    with exn -> Error exn
  in
  List.iter Halo.Server.shutdown runnings;
  List.iter (fun (_, server) -> Eio.Promise.await_exn server) servers;
  match outcome with Ok metrics -> metrics | Error exn -> raise exn

let with_live_configs ?max_in_flight_requests ?on_error ?telemetry ?metrics
    configs f =
  match
    with_live_config_groups ?max_in_flight_requests ?on_error ?telemetry
      ?metrics [ configs ] f
  with
  | [ metrics ] -> metrics
  | _ -> assert false

let with_live_server ?tls ?early_data_policy ?early_data_replay_store ?router
    ?max_in_flight_requests ?on_error ?telemetry ?metrics f =
  let router =
    Option.value router
      ~default:
        Halo.Router.(
          route
            Matcher.(
              let+ () = request_method_is GET *> path_end in
              fun _request -> text Status.ok "hello world\n"))
  in
  let config =
    Halo.Server.Config.create ~port:8443 ?tls ?early_data_policy
      ?early_data_replay_store router
  in
  with_live_configs ?max_in_flight_requests ?on_error ?telemetry ?metrics
    [ config ] f

let with_live_tls_server tls = with_live_server ~tls

let connect_plaintext () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 8443));
  socket

let write_socket_all socket text =
  let payload = Bytes.of_string text in
  let rec write offset =
    if offset < Bytes.length payload then
      let count =
        Unix.write socket payload offset (Bytes.length payload - offset)
      in
      write (offset + count)
  in
  write 0

let read_socket_until socket until =
  let response = Buffer.create 256 in
  let chunk = Bytes.create 256 in
  let rec read () =
    let contents = Buffer.contents response in
    if contains until contents then
      contents
    else
      match Unix.read socket chunk 0 (Bytes.length chunk) with
      | 0 -> contents
      | count ->
          Buffer.add_subbytes response chunk 0 count;
          read ()
  in
  read ()

let plaintext_request ~until request =
  let socket = connect_plaintext () in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      write_socket_all socket request;
      read_socket_until socket until)

type observed_response_outcome =
  | Observed_completed
  | Observed_aborted of string
  | Observed_client_disconnected

let observe_response_outcome callback_count observed next request =
  Halo.Router.Request.on_response_terminated request (fun outcome ->
      ignore (Atomic.fetch_and_add callback_count 1);
      Atomic.set observed
        (Some
           (match outcome with
           | Halo.Router.Completed -> Observed_completed
           | Halo.Router.Aborted (exn, _backtrace) ->
               Observed_aborted (Printexc.to_string exn)
           | Halo.Router.Client_disconnected -> Observed_client_disconnected)));
  next request

let wait_for_response_outcome observed =
  let deadline = Unix.gettimeofday () +. 1.0 in
  let rec wait () =
    match Atomic.get observed with
    | Some outcome -> outcome
    | None when Unix.gettimeofday () >= deadline ->
        fail "timed out waiting for response termination callback"
    | None ->
        Unix.sleepf 0.01;
        wait ()
  in
  wait ()

let run_response_outcome_test ?on_error router client =
  let callback_count = Atomic.make 0 in
  let observed = Atomic.make None in
  let router =
    Halo.Router.use (observe_response_outcome callback_count observed) router
  in
  ignore
    (with_live_server ~router ?on_error (fun () ->
         client ();
         ignore (wait_for_response_outcome observed)));
  Alcotest.(check int) "callback runs exactly once" 1
    (Atomic.get callback_count);
  match Atomic.get observed with
  | Some outcome -> outcome
  | None -> fail "expected response termination outcome"

let test_fixed_response_reports_completed () =
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET *> path_end in
          fun _request -> text Status.ok "complete\n"))
  in
  let outcome =
    run_response_outcome_test router (fun () ->
        ignore
          (plaintext_request ~until:"complete\n"
             "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
  in
  match outcome with
  | Observed_completed -> ()
  | _ -> fail "expected completed response outcome"

let test_stream_response_reports_completed () =
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET *> path_end in
          fun _request ->
            Halo.Stream.respond ~status:Status.ok (fun ~yield ->
                yield "complete\n")))
  in
  let outcome =
    run_response_outcome_test router (fun () ->
        ignore
          (plaintext_request ~until:"0\r\n\r\n"
             "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))
  in
  match outcome with
  | Observed_completed -> ()
  | _ -> fail "expected completed stream outcome"

let test_failed_stream_reports_aborted () =
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET *> path_end in
          fun _request ->
            Halo.Stream.respond ~status:Status.ok (fun ~yield ->
                yield "partial\n";
                failwith "stream outcome marker")))
  in
  let outcome =
    run_response_outcome_test ~on_error:(fun _ _ _ -> ()) router (fun () ->
        let response =
          plaintext_request ~until:"unreachable response marker"
            "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n"
        in
        Alcotest.(check bool) "partial stream was sent" true
          (contains "partial\n" response);
        Alcotest.(check bool) "stream terminator was omitted" false
          (contains "\r\n0\r\n\r\n" response))
  in
  match outcome with
  | Observed_aborted message ->
      Alcotest.(check string) "producer exception is preserved"
        "Failure(\"stream outcome marker\")" message
  | _ -> fail "expected aborted response outcome"

let test_peer_disconnect_reports_client_disconnected () =
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is POST *> path_end in
          fun request ->
            Halo.Stream.respond ~status:Status.ok (fun ~yield ->
                yield "partial\n";
                ignore (Halo.Body.read (Request.body request)))))
  in
  let outcome =
    run_response_outcome_test router (fun () ->
        ignore
          (plaintext_request ~until:"partial\n"
             "POST / HTTP/1.1\r\nHost: localhost\r\nContent-Length: 100\r\n\r\n"))
  in
  match outcome with
  | Observed_client_disconnected -> ()
  | _ -> fail "expected client-disconnected response outcome"

let wait_for_telemetry observed =
  let deadline = Unix.gettimeofday () +. 1.0 in
  let rec wait () =
    match Atomic.get observed with
    | Some value -> value
    | None when Unix.gettimeofday () >= deadline ->
        fail "timed out waiting for request telemetry"
    | None ->
        Unix.sleepf 0.01;
        wait ()
  in
  wait ()

let find_metric_by_status snapshot name status =
  let status_attribute : Halo.Metrics.attribute =
    ( "http.response.status_code",
      Halo.Metrics.Int (Int64.of_int status) )
  in
  match
    List.find_opt
      (fun point ->
        String.equal point.Halo.Metrics.name name
        && List.mem status_attribute point.attributes)
      snapshot.Halo.Metrics.points
  with
  | Some point -> point
  | None -> fail "missing metric %s for status %d" name status

let check_histogram_count_and_sum label expected_count expected_sum point =
  match point.Halo.Metrics.aggregation with
  | Halo.Metrics.Histogram histogram ->
      Alcotest.(check int64) (label ^ " count") expected_count histogram.count;
      Alcotest.(check (float 0.0)) (label ^ " sum") expected_sum histogram.sum
  | _ -> fail "%s is not a histogram" label

let test_request_telemetry_covers_transport_lifecycle () =
  let scope_active = Atomic.make false in
  let handler_saw_scope = Atomic.make false in
  let finish_count = Atomic.make 0 in
  let observed = Atomic.make None in
  let module Observer = struct
    type span = Halo.Telemetry.server_request

    let start_server_request request = request

    let with_span _span run =
      Atomic.set scope_active true;
      Fun.protect ~finally:(fun () -> Atomic.set scope_active false) run

    let finish_server_request request result =
      ignore (Atomic.fetch_and_add finish_count 1);
      Atomic.set observed (Some (request, result))
  end in
  let telemetry =
    Halo.Telemetry.of_observer (module Observer)
  in
  let router =
    Halo.Router.(
      route ~template:"/users/{id}"
        Matcher.(
          let+ _id =
            request_method_is GET *> path_segment "users" *> int_parameter
            <* path_end
          in
          fun _request ->
            Atomic.set handler_saw_scope (Atomic.get scope_active);
            text Status.created "created\n"))
  in
  ignore
    (with_live_server ~router ~telemetry (fun () ->
         let response =
           plaintext_request ~until:"created\n"
             ("GET /users/42?full=1 HTTP/1.1\r\n"
             ^ "Host: localhost\r\n"
             ^ "traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-"
             ^ "00f067aa0ba902b7-01\r\n"
             ^ "tracestate: rojo=00f067aa0ba902b7,congo=t61rcWkgMzE\r\n"
             ^ "Connection: close\r\n\r\n")
         in
         Alcotest.(check bool) "response status" true
           (contains "HTTP/1.1 201 Created" response);
         ignore (wait_for_telemetry observed)));
  let request, result = wait_for_telemetry observed in
  Alcotest.(check int) "finished exactly once" 1 (Atomic.get finish_count);
  Alcotest.(check bool) "handler runs inside telemetry scope" true
    (Atomic.get handler_saw_scope);
  Alcotest.(check string) "method" "GET" request.method_;
  Alcotest.(check string) "raw target" "/users/42?full=1" request.target;
  Alcotest.(check (pair int int)) "HTTP version" (1, 1)
    request.http_version;
  Alcotest.(check bool) "trace context header is available" true
    (List.mem
       ( "traceparent",
         "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01" )
       request.headers);
  let remote_parent =
    match request.remote_parent with
    | Some remote_parent -> remote_parent
    | None -> fail "expected parsed remote trace context"
  in
  Alcotest.(check string) "remote trace ID"
    "4bf92f3577b34da6a3ce929d0e0e4736"
    (Halo.Telemetry.Trace_context.trace_id remote_parent);
  Alcotest.(check string) "remote span ID" "00f067aa0ba902b7"
    (Halo.Telemetry.Trace_context.span_id remote_parent);
  Alcotest.(check bool) "remote parent is sampled" true
    (Halo.Telemetry.Trace_context.sampled remote_parent);
  Alcotest.(check (list (pair string string))) "remote tracestate"
    [ ("rojo", "00f067aa0ba902b7"); ("congo", "t61rcWkgMzE") ]
    (Halo.Telemetry.Trace_context.tracestate remote_parent);
  Alcotest.(check bool) "first-byte monotonic timestamp" true
    (Int64.compare request.start_monotonic_ns 0L > 0);
  Alcotest.(check bool) "first-byte Unix timestamp" true
    (Option.fold ~none:false
       ~some:(fun timestamp -> Int64.compare timestamp 0L > 0)
       request.start_time_unix_ns);
  Alcotest.(check (option string)) "route template" (Some "/users/{id}")
    result.route;
  Alcotest.(check (option int)) "response status" (Some 201)
    result.status_code;
  (match result.outcome with
  | Some Halo.Router.Completed -> ()
  | _ -> fail "expected completed telemetry outcome");
  Alcotest.(check bool) "terminal timestamp follows first byte" true
    (Option.fold ~none:false
       ~some:(fun timestamp ->
         Int64.compare timestamp request.start_monotonic_ns >= 0)
       result.end_monotonic_ns);
  assert_int64_equal "request body size" 0L result.request_body_size;
  assert_int64_equal "response body size" 8L result.response_body_size

let test_pre_dispatch_parser_failure_is_observed () =
  let observed = Atomic.make None in
  let module Observer = struct
    type span = Halo.Telemetry.server_request

    let start_server_request request = request
    let with_span _span run = run ()

    let finish_server_request request result =
      if result.Halo.Telemetry.status_code = Some 400 then
        Atomic.set observed (Some (request, result))
  end in
  let telemetry = Halo.Telemetry.of_observer (module Observer) in
  let metrics = Halo.Metrics.create ~duration_buckets:[ 1.0 ] () in
  ignore
    (with_live_server ~telemetry ~metrics (fun () ->
         let response =
           plaintext_request ~until:"bad request\n"
             ("GET /invalid HTTP/1.1\r\n"
             ^ "Host: localhost\r\n"
             ^ "traceparent: 00-4bf92f3577b34da6a3ce929d0e0e4736-"
             ^ "00f067aa0ba902b7-01\r\n"
             ^ "Host: duplicate\r\nConnection: close\r\n\r\n")
         in
         Alcotest.(check bool) "parser response status" true
           (contains "HTTP/1.1 400 Bad Request" response);
         ignore (wait_for_telemetry observed)));
  let request, result = wait_for_telemetry observed in
  Alcotest.(check string) "parser method" "GET" request.method_;
  Alcotest.(check string) "parser target" "/invalid" request.target;
  Alcotest.(check (pair int int)) "parser HTTP version" (1, 1)
    request.http_version;
  Alcotest.(check bool) "parser trace context" true
    (Option.is_some request.remote_parent);
  Alcotest.(check (option string)) "parser route" None result.route;
  Alcotest.(check (option int)) "parser telemetry status" (Some 400)
    result.status_code;
  (match result.outcome with
  | Some Halo.Router.Completed -> ()
  | _ -> fail "expected completed parser-error telemetry outcome");
  Alcotest.(check bool) "parser terminal timestamp" true
    (Option.fold ~none:false
       ~some:(fun timestamp -> timestamp >= request.start_monotonic_ns)
       result.end_monotonic_ns);
  assert_int64_equal "parser request body size" 0L result.request_body_size;
  assert_int64_equal "parser response body size" 12L result.response_body_size;
  let snapshot = Halo.Metrics.snapshot metrics in
  find_metric_by_status snapshot "http.server.request.duration" 400
  |> check_histogram_count_and_sum "parser duration" 1L
       (match result.end_monotonic_ns with
       | Some end_ns ->
           Int64.to_float (Int64.sub end_ns request.start_monotonic_ns) /. 1e9
       | None -> assert false);
  find_metric_by_status snapshot "http.server.response.body.size" 400
  |> check_histogram_count_and_sum "parser response bytes" 1L 12.0;
  let requests = find_metric_by_status snapshot "halo.http.server.requests" 400 in
  Alcotest.(check bool) "parser 4xx status class" true
    (List.mem
       ("http.response.status_class", Halo.Metrics.String "4xx")
       requests.attributes)

let test_pre_dispatch_admission_rejection_is_observed () =
  let observed = Atomic.make None in
  let fast_handler_calls = Atomic.make 0 in
  let module Observer = struct
    type span = Halo.Telemetry.server_request

    let start_server_request request = request
    let with_span _span run = run ()

    let finish_server_request request result =
      if result.Halo.Telemetry.status_code = Some 503 then
        Atomic.set observed (Some (request, result))
  end in
  let telemetry = Halo.Telemetry.of_observer (module Observer) in
  let metrics = Halo.Metrics.create () in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET *> path_segment "slow" *> path_end in
          fun _request ->
            Eio_unix.sleep 0.25;
            text Status.ok "slow\n")
      <|> route
            Matcher.(
              let+ () = request_method_is GET *> path_segment "fast" *> path_end in
              fun _request ->
                ignore (Atomic.fetch_and_add fast_handler_calls 1);
                text Status.ok "fast\n"))
  in
  ignore
    (with_live_server ~max_in_flight_requests:1 ~router ~telemetry ~metrics
       (fun () ->
         let slow = connect_plaintext () in
         Fun.protect
           ~finally:(fun () -> Unix.close slow)
           (fun () ->
             write_socket_all slow
               "GET /slow HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n";
             Unix.sleepf 0.03;
             let rejected =
               plaintext_request ~until:"server overloaded\n"
                 "GET /fast HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
             in
             Alcotest.(check bool) "admission response status" true
               (contains "HTTP/1.1 503 Service Unavailable" rejected);
             ignore (wait_for_telemetry observed);
             ignore (read_socket_until slow "slow\n"))));
  Alcotest.(check int) "rejected handler did not run" 0
    (Atomic.get fast_handler_calls);
  let request, result = wait_for_telemetry observed in
  Alcotest.(check string) "admission method" "GET" request.method_;
  Alcotest.(check string) "admission target" "/fast" request.target;
  Alcotest.(check (option string)) "admission route" None result.route;
  Alcotest.(check (option int)) "admission telemetry status" (Some 503)
    result.status_code;
  (match result.outcome with
  | Some Halo.Router.Completed -> ()
  | _ -> fail "expected completed admission telemetry outcome");
  assert_int64_equal "admission response body size" 18L
    result.response_body_size;
  let snapshot = Halo.Metrics.snapshot metrics in
  let requests = find_metric_by_status snapshot "halo.http.server.requests" 503 in
  Alcotest.(check bool) "admission 5xx status class" true
    (List.mem
       ("http.response.status_class", Halo.Metrics.String "5xx")
       requests.attributes);
  find_metric_by_status snapshot "http.server.response.body.size" 503
  |> check_histogram_count_and_sum "admission response bytes" 1L 18.0

let test_invalid_trace_context_does_not_fail_request () =
  let saw_missing_parent = Atomic.make false in
  let module Observer = struct
    type span = unit

    let start_server_request (request : Halo.Telemetry.server_request) =
      Atomic.set saw_missing_parent (Option.is_none request.remote_parent)

    let with_span () run = run ()
    let finish_server_request () _result = ()
  end in
  let telemetry = Halo.Telemetry.of_observer (module Observer) in
  ignore
    (with_live_server ~telemetry (fun () ->
         let response =
           plaintext_request ~until:"hello world\n"
             ("GET / HTTP/1.1\r\n"
             ^ "Host: localhost\r\n"
             ^ "traceparent: 00-00000000000000000000000000000000-"
             ^ "00f067aa0ba902b7-01\r\n"
             ^ "Connection: close\r\n\r\n")
         in
         Alcotest.(check bool) "response status" true
           (contains "HTTP/1.1 200 OK" response)));
  Alcotest.(check bool) "invalid parent is ignored" true
    (Atomic.get saw_missing_parent)

let test_telemetry_observer_failures_do_not_fail_requests () =
  let handler_calls = Atomic.make 0 in
  let module Failing_observer = struct
    type span = unit

    let start_server_request _request = ()

    let with_span () run =
      ignore (run ());
      failwith "telemetry scope failure"

    let finish_server_request () _result =
      failwith "telemetry finish failure"
  end in
  let telemetry =
    Halo.Telemetry.of_observer (module Failing_observer)
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET *> path_end in
          fun _request ->
            ignore (Atomic.fetch_and_add handler_calls 1);
            text Status.ok "healthy\n"))
  in
  ignore
    (with_live_server ~router ~telemetry (fun () ->
         let response =
           plaintext_request ~until:"healthy\n"
             "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
         in
         Alcotest.(check bool) "request succeeds" true
           (contains "HTTP/1.1 200 OK" response)));
  Alcotest.(check int) "handler runs exactly once" 1
    (Atomic.get handler_calls)

let test_handler_error_reports_raw_backtrace () =
  let previous_backtrace_status = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  Fun.protect
    ~finally:(fun () -> Printexc.record_backtrace previous_backtrace_status)
    (fun () ->
      let reported = Atomic.make None in
      let on_error request_id exn backtrace =
        Atomic.set reported
          (Some
             ( request_id,
               Printexc.to_string exn,
               Printexc.raw_backtrace_length backtrace ))
      in
      let router =
        Halo.Router.(
          route
            Matcher.(
              let+ () = request_method_is GET *> path_end in
              fun _request -> failwith "handler backtrace marker"))
      in
      ignore
        (with_live_server ~router ~on_error (fun () ->
             let response =
               plaintext_request ~until:"internal server error\n"
                 "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
             in
             Alcotest.(check bool) "handler failure returns 500" true
               (contains "HTTP/1.1 500 Internal Server Error" response)));
      match Atomic.get reported with
      | None -> fail "expected on_error to be called"
      | Some (request_id, exn, backtrace_length) ->
          Alcotest.(check bool) "request id is populated" true
            (Int64.compare request_id 0L > 0);
          Alcotest.(check string) "exception is preserved"
            "Failure(\"handler backtrace marker\")" exn;
          Alcotest.(check bool) "raw backtrace is populated" true
            (backtrace_length > 0))

let tls_resumption_config ?session_ticket_keys_path () =
  Halo.Server.Config.tls
    ~cert_chain_path:(tls_cert_fixture_path ())
    ~private_key_path:(tls_key_fixture_path ())
    ~minimum_protocol_version:Halo.Server.TLS13
    ~session_tickets_enabled:true
    ?session_ticket_encrypt_decrypt_lifetime_secs:
      (match session_ticket_keys_path with
      | Some _ -> Some (10 * 365 * 24 * 60 * 60)
      | None -> None)
    ?session_ticket_decrypt_lifetime_secs:
      (match session_ticket_keys_path with
      | Some _ -> Some (10 * 365 * 24 * 60 * 60)
      | None -> None)
    ?session_ticket_keys_path ()

let tls_early_data_config () =
  Halo.Server.Config.tls
    ~cert_chain_path:(tls_cert_fixture_path ())
    ~private_key_path:(tls_key_fixture_path ())
    ~minimum_protocol_version:Halo.Server.TLS13
    ~session_tickets_enabled:true
    ~session_ticket_keys_path:(tls_ticket_keys_fixture_path ())
    ~session_ticket_encrypt_decrypt_lifetime_secs:(10 * 365 * 24 * 60 * 60)
    ~session_ticket_decrypt_lifetime_secs:(10 * 365 * 24 * 60 * 60)
    ~max_early_data_size:4096 ()

let string_of_sockaddr = function
  | Unix.ADDR_INET (address, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr address) port
  | Unix.ADDR_UNIX path -> "unix:" ^ path

let transport_router =
  Halo.Router.(
    route
      Matcher.(
        let+ () = request_method_is GET *> path_end in
        fun request ->
          let connection = Request.connection_info request in
          let http_major, http_minor = Request.http_version request in
          let tls =
            match Request.tls request with
            | None -> "plaintext"
            | Some tls ->
                Printf.sprintf "tls=%s;sni=%s;alpn=%s;cipher=%b"
                  (string_of_tls_protocol_version tls.protocol_version)
                  (Option.value tls.server_name ~default:"none")
                  (Option.value tls.application_protocol ~default:"none")
                  (Option.fold ~none:false
                     ~some:(fun cipher -> String.length cipher > 0)
                     tls.cipher)
          in
          text Status.ok
            (Printf.sprintf
               "http=%d.%d\nremote=%s\nlocal=%s\ntls-status=%b\n%s\naccessors=%b\n"
               http_major http_minor
               (string_of_sockaddr connection.remote_address)
               (string_of_sockaddr connection.local_address)
               (Request.is_tls request) tls
               (Request.remote_address request = connection.remote_address
               && Request.local_address request = connection.local_address))))

let test_plaintext_request_exposes_transport_metadata () =
  ignore
    (with_live_server ~router:transport_router (fun () ->
         let response =
           plaintext_request ~until:"accessors=true\n"
             "GET / HTTP/1.0\r\nHost: localhost\r\nConnection: close\r\n\r\n"
         in
         Alcotest.(check bool) "HTTP version" true
           (contains "http=1.0\n" response);
         Alcotest.(check bool) "remote address" true
           (contains "remote=127.0.0.1:" response);
         Alcotest.(check bool) "local address" true
           (contains "local=127.0.0.1:8443\n" response);
         Alcotest.(check bool) "plaintext TLS status" true
           (contains "tls-status=false\nplaintext\n" response);
         Alcotest.(check bool) "address accessors" true
           (contains "accessors=true\n" response)))

let test_tls_request_exposes_negotiated_transport_metadata () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:(tls_cert_fixture_path ())
      ~private_key_path:(tls_key_fixture_path ())
      ~minimum_protocol_version:Halo.Server.TLS13
      ~alpn_protocols:[ "http/1.1" ] ()
  in
  ignore
    (with_live_server ~tls ~router:transport_router (fun () ->
         let result =
           run_s2nc ~alpn:"http/1.1" ~echo:true
             ~input:
               "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
             ()
         in
         assert_process_success result;
         if not (contains "tls-status=true\n" result.stdout) then
           fail "expected TLS transport response\nstdout:\n%s\nstderr:\n%s"
             result.stdout result.stderr;
         Alcotest.(check bool) "TLS request HTTP version" true
           (contains "http=1.1\n" result.stdout);
         Alcotest.(check bool) "negotiated TLS metadata" true
           (contains
              "tls=TLSv1.3;sni=localhost;alpn=http/1.1;cipher=true\n"
              result.stdout);
         Alcotest.(check bool) "TLS local address" true
           (contains "local=127.0.0.1:8443\n" result.stdout)))

let test_keep_alive_requests_share_connection_metadata () =
  let previous = ref None in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET *> path_end in
          fun request ->
            let connection_info = Request.connection_info request in
            let result =
              match !previous with
              | None -> "first\n"
              | Some prior when prior == connection_info -> "shared\n"
              | Some _ -> "new\n"
            in
            previous := Some connection_info;
            text Status.ok result))
  in
  ignore
    (with_live_server ~router (fun () ->
         let socket = connect_plaintext () in
         Fun.protect
           ~finally:(fun () -> Unix.close socket)
           (fun () ->
             write_socket_all socket
               "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: keep-alive\r\n\r\n";
             let first = read_socket_until socket "first\n" in
             Alcotest.(check bool) "first request receives metadata" true
               (contains "first\n" first);
             write_socket_all socket
               "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n";
             let second = read_socket_until socket "shared\n" in
             Alcotest.(check bool) "keep-alive metadata is shared" true
               (contains "shared\n" second));
         let fresh =
           plaintext_request ~until:"new\n"
             "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
         in
         Alcotest.(check bool) "new connection receives new metadata" true
           (contains "new\n" fresh)))

let test_runtime_metrics_defaults () =
  let metrics = with_live_server (fun () -> ()) in
      Alcotest.(check bool)
        "utilization is normalized"
        true
        (metrics.utilization >= 0.0 && metrics.utilization <= 1.0);
      assert_int64_equal "active connections" 0L metrics.active_connections;
      assert_int64_equal "active requests" 0L metrics.active_requests;
      assert_int64_equal "accepted connections" 0L metrics.accepted_connections;
      assert_int64_equal "rejected connections" 0L metrics.rejected_connections;
      assert_int64_equal "completed requests" 0L metrics.completed_requests;
      assert_int64_equal "tls full handshakes" 0L metrics.tls_full_handshakes;
      assert_int64_equal "tls resumed handshakes" 0L
        metrics.tls_resumed_handshakes;
      assert_int64_equal "tls handshake failures" 0L
        metrics.tls_handshake_failures;
      assert_int64_equal "event queue depth" 0L metrics.event_queue_depth;
      assert_int64_equal "command queue depth" 0L metrics.command_queue_depth;
      Alcotest.(check string)
        "controller state"
        "normal"
        (controller_state metrics.controller_state);
      Alcotest.(check bool)
        "backpressure disabled"
        false
        metrics.backpressure_enabled

let test_request_metrics_cover_live_server () =
  let collector =
    Halo.Metrics.create ~duration_buckets:[ 1.0 ]
      ~size_buckets:[ 4.0; 8.0 ] ()
  in
  let router =
    Halo.Router.(
      route ~template:"/"
        Matcher.(
          let+ () = request_method_is POST *> path_end in
          fun request ->
            ignore (Halo.Body.read_all (Request.body request));
            text Status.created "created\n"))
  in
  ignore
    (with_live_server ~router ~metrics:collector (fun () ->
         let response =
           plaintext_request ~until:"created\n"
             ("POST / HTTP/1.1\r\nHost: localhost\r\n"
             ^ "Content-Length: 5\r\nConnection: close\r\n\r\nhello")
         in
         Alcotest.(check bool) "response status" true
           (contains "HTTP/1.1 201 Created" response)));
  let snapshot = Halo.Metrics.snapshot collector in
  let route point =
    List.mem
      ("http.route", Halo.Metrics.String "/")
      point.Halo.Metrics.attributes
  in
  let find name =
    match
      List.find_opt
        (fun point -> String.equal point.Halo.Metrics.name name && route point)
        snapshot.points
    with
    | Some point -> point
    | None -> fail "missing live metric %s" name
  in
  let histogram_sum name =
    match (find name).aggregation with
    | Halo.Metrics.Histogram histogram -> histogram.sum
    | _ -> fail "live metric %s was not a histogram" name
  in
  Alcotest.(check (float 0.0)) "request bytes" 5.0
    (histogram_sum "http.server.request.body.size");
  Alcotest.(check (float 0.0)) "response bytes" 8.0
    (histogram_sum "http.server.response.body.size");
  let requests = find "halo.http.server.requests" in
  Alcotest.(check bool) "2xx status class" true
    (List.mem
       ("http.response.status_class", Halo.Metrics.String "2xx")
       requests.attributes);
  Alcotest.(check bool) "runtime points collected" true
    (List.exists
       (fun point ->
         String.equal point.Halo.Metrics.name "halo.runtime.queue.depth")
       snapshot.points)

let test_tls_metrics_count_full_and_resumed_handshakes_with_explicit_keys () =
  with_temp_path "halo-session" ".bin" (fun session_path ->
      with_temp_file_contents "halo-request" ".txt"
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        (fun request_path ->
          let ticket_request =
            "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
          in
          let tls =
            tls_resumption_config
              ~session_ticket_keys_path:(tls_ticket_keys_fixture_path ()) ()
          in
          let metrics =
            with_live_tls_server tls (fun () ->
                let first =
                  run_s2nc ~ticket_out:session_path ~input:ticket_request
                    ~close_stdin:false ()
                in
                assert_process_success first;
                assert_nonempty_file session_path;
                Alcotest.(check bool) "first response body" true
                  (contains "hello world\n" first.stdout);
                let second =
                  run_s2nc ~ticket_in:session_path ~send_file:request_path
                    ~close_stdin:true ()
                in
                assert_process_success second)
          in
          assert_int64_equal "full handshakes" 1L metrics.tls_full_handshakes;
          assert_int64_equal "resumed handshakes" 1L metrics.tls_resumed_handshakes;
          assert_int64_equal "handshake failures" 0L metrics.tls_handshake_failures))

let test_tls_metrics_count_ephemeral_resumption_and_restart_invalidation () =
  with_temp_path "halo-session" ".bin" (fun session_path ->
      with_temp_file_contents "halo-request" ".txt"
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        (fun request_path ->
          let ticket_request =
            "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
          in
          let metrics_first_run =
            with_live_tls_server (tls_resumption_config ()) (fun () ->
                let first =
                  run_s2nc ~ticket_out:session_path ~input:ticket_request
                    ~close_stdin:false ()
                in
                assert_process_success first;
                assert_nonempty_file session_path)
          in
          assert_int64_equal "first run full handshakes" 1L
            metrics_first_run.tls_full_handshakes;
          assert_int64_equal "first run resumed handshakes" 0L
            metrics_first_run.tls_resumed_handshakes;
          let metrics_second_run =
            with_live_tls_server (tls_resumption_config ()) (fun () ->
                let fresh =
                  run_s2nc ~ticket_in:session_path ~send_file:request_path
                    ~close_stdin:true ()
                in
                assert_process_success fresh)
          in
          assert_int64_equal "restart invalidates saved session full handshakes" 1L
            metrics_second_run.tls_full_handshakes;
          assert_int64_equal "restart invalidates saved session resumed handshakes" 0L
            metrics_second_run.tls_resumed_handshakes))

let test_tls_early_data_defaults_to_425_and_rejects_replay () =
  with_temp_path "halo-session" ".bin" (fun session_path ->
      with_temp_file_contents "halo-early" ".txt"
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        (fun early_data_path ->
          let ticket_request =
            "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
          in
          let tls = tls_early_data_config () in
          let metrics =
            with_live_tls_server tls (fun () ->
                let prime =
                  run_s2nc ~ticket_out:session_path ~input:ticket_request
                    ~close_stdin:false ()
                in
                assert_process_success prime;
                assert_nonempty_file session_path;
                let first_early =
                  run_s2nc ~ticket_in:session_path
                    ~early_data_file:early_data_path ~echo:true ~close_stdin:false
                    ()
                in
                assert_process_success first_early;
                Alcotest.(check bool) "early data accepted" true
                  (contains "Early Data status: ACCEPTED" first_early.stdout);
                if not (contains "HTTP/1.1 425 Too Early" first_early.stdout) then
                  fail "expected 425 response\nstdout:\n%s\nstderr:\n%s"
                    first_early.stdout first_early.stderr;
                let replay =
                  run_s2nc ~ticket_in:session_path
                    ~early_data_file:early_data_path ~close_stdin:true ()
                in
                assert_process_success replay;
                Alcotest.(check bool) "early data replay rejected" true
                  (contains "Early Data status: REJECTED" replay.stdout))
          in
          assert_int64_equal "0-rtt full handshakes" 1L metrics.tls_full_handshakes;
          Alcotest.(check bool) "0-rtt resumed handshakes present" true
            (metrics.tls_resumed_handshakes >= 1L);
          assert_int64_equal "0-rtt handshake failures" 0L
            metrics.tls_handshake_failures))

let test_tls_early_data_policy_opts_in_and_marks_request () =
  with_temp_path "halo-session" ".bin" (fun session_path ->
      with_temp_file_contents "halo-early" ".txt"
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        (fun early_data_path ->
          let router =
            Halo.Router.(
              route
                Matcher.(
                  let+ () = request_method_is GET *> path_end in
                  fun request ->
                    text Status.ok
                      (if Request.is_early_data request then "early\n"
                       else "regular\n")))
          in
          let early_data_policy ~method_text ~target ~headers:_ =
            String.equal method_text "GET" && String.equal target "/"
          in
          let tls = tls_early_data_config () in
          ignore
            (with_live_server ~tls ~early_data_policy ~router (fun () ->
                 let prime =
                   run_s2nc ~ticket_out:session_path
                     ~input:"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                     ~close_stdin:false ()
                 in
                 assert_process_success prime;
                 assert_nonempty_file session_path;
                 Alcotest.(check bool) "ordinary request marker" true
                   (contains "regular\n" prime.stdout);
                 let early =
                   run_s2nc ~ticket_in:session_path
                     ~early_data_file:early_data_path ~echo:true ~close_stdin:false
                     ()
                 in
                 assert_process_success early;
                 Alcotest.(check bool) "early data accepted" true
                   (contains "Early Data status: ACCEPTED" early.stdout);
                 if not (contains "early\n" early.stdout) then
                   fail "expected early response marker\nstdout:\n%s\nstderr:\n%s"
                     early.stdout early.stderr))))

let test_tls_early_data_replay_is_atomic_across_runtimes () =
  with_temp_path "halo-session" ".bin" (fun session_path ->
      with_temp_file_contents "halo-early" ".txt"
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        (fun early_data_path ->
          let router =
            Halo.Router.(
              route
                Matcher.(
                  let+ () = request_method_is GET *> path_end in
                  fun _request -> text Status.ok "hello world\n"))
          in
          let tls = tls_early_data_config () in
          let config port = Halo.Server.Config.create ~port ~tls router in
          ignore
            (with_live_config_groups [ [ config 8443 ]; [ config 8444 ] ]
               (fun () ->
                 let prime =
                   run_s2nc ~port:8443 ~ticket_out:session_path
                     ~input:"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                     ~close_stdin:false ()
                 in
                 assert_process_success prime;
                 assert_nonempty_file session_path;
                 let second =
                   Domain.spawn (fun () ->
                       run_s2nc ~port:8444 ~ticket_in:session_path
                         ~early_data_file:early_data_path ~close_stdin:true ())
                 in
                 let first =
                   run_s2nc ~port:8443 ~ticket_in:session_path
                     ~early_data_file:early_data_path ~close_stdin:true ()
                 in
                 let second = Domain.join second in
                 assert_process_success first;
                 assert_process_success second;
                 let results = [ first; second ] in
                 let count_status status =
                   List.fold_left
                     (fun count result ->
                       if contains status result.stdout then count + 1 else count)
                     0 results
                 in
                 Alcotest.(check int) "one runtime accepts early data" 1
                   (count_status "Early Data status: ACCEPTED");
                 Alcotest.(check int) "one runtime rejects the replay" 1
                   (count_status "Early Data status: REJECTED")))))

let test_tls_custom_replay_store_contract () =
  with_temp_path "halo-session" ".bin" (fun session_path ->
      with_temp_file_contents "halo-early" ".txt"
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        (fun early_data_path ->
          let module Store = Halo.Server.Config.Early_data_replay_store in
          let calls = Atomic.make 0 in
          let first_identity = Atomic.make None in
          let identities_match = Atomic.make true in
          let observed_ttl = Atomic.make 0L in
          let claim ~identity ~ttl_secs =
            (match Atomic.get first_identity with
            | None -> Atomic.set first_identity (Some identity)
            | Some first ->
                if not (String.equal first identity) then
                  Atomic.set identities_match false);
            Atomic.set observed_ttl ttl_secs;
            match Atomic.fetch_and_add calls 1 with
            | 0 -> Store.Unavailable
            | 1 ->
                Eio_unix.sleep 0.05;
                Store.Fresh
            | 2 -> failwith "replay store failure"
            | 3 -> Store.Fresh
            | _ -> Store.Already_seen
          in
          let replay_store = Store.create ~timeout_seconds:0.01 claim in
          let tls = tls_early_data_config () in
          ignore
            (with_live_server ~tls ~early_data_replay_store:replay_store
               (fun () ->
                 let prime =
                   run_s2nc ~ticket_out:session_path
                     ~input:"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                     ~close_stdin:false ()
                 in
                 assert_process_success prime;
                 assert_nonempty_file session_path;
                 let offer ?(echo = false) ~close_stdin () =
                   let result =
                     run_s2nc ~ticket_in:session_path
                       ~early_data_file:early_data_path ~echo ~close_stdin ()
                   in
                   assert_process_success result;
                   result
                 in
                 let unavailable = offer ~close_stdin:true () in
                 Alcotest.(check bool) "unavailable rejects early data" true
                   (contains "Early Data status: REJECTED" unavailable.stdout);
                 let timed_out = offer ~close_stdin:true () in
                 Alcotest.(check bool) "timeout rejects early data" true
                   (contains "Early Data status: REJECTED" timed_out.stdout);
                 let failed = offer ~close_stdin:true () in
                 Alcotest.(check bool) "exception rejects early data" true
                   (contains "Early Data status: REJECTED" failed.stdout);
                 let fresh = offer ~echo:true ~close_stdin:false () in
                 Alcotest.(check bool) "fresh claim accepts early data" true
                   (contains "Early Data status: ACCEPTED" fresh.stdout);
                 let duplicate = offer ~close_stdin:true () in
                 Alcotest.(check bool) "duplicate rejects early data" true
                   (contains "Early Data status: REJECTED" duplicate.stdout)));
          Alcotest.(check int) "claim callback count" 5 (Atomic.get calls);
          Alcotest.(check bool) "all offers use the same digest" true
            (Atomic.get identities_match);
          (match Atomic.get first_identity with
          | None -> fail "expected the replay store to receive an identity"
          | Some identity ->
              Alcotest.(check int) "SHA-256 identity length" 32
                (String.length identity));
          assert_int64_equal "maximum TLS 1.3 ticket TTL" 604800L
            (Atomic.get observed_ttl)))

let test_tls_blocking_replay_store_does_not_block_timeout () =
  with_temp_path "halo-session" ".bin" (fun session_path ->
      with_temp_file_contents "halo-early" ".txt"
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        (fun early_data_path ->
          let module Store = Halo.Server.Config.Early_data_replay_store in
          let callback_started = Atomic.make false in
          let callback_finished = Atomic.make false in
          let replay_store =
            Store.create_blocking ~timeout_seconds:0.01
              (fun ~identity:_ ~ttl_secs:_ ->
                Atomic.set callback_started true;
                Thread.delay 0.5;
                Atomic.set callback_finished true;
                Store.Fresh)
          in
          let tls = tls_early_data_config () in
          ignore
            (with_live_server ~tls ~early_data_replay_store:replay_store
               (fun () ->
                 let prime =
                   run_s2nc ~ticket_out:session_path
                     ~input:"GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                     ~close_stdin:false ()
                 in
                 assert_process_success prime;
                 assert_nonempty_file session_path;
                 let early =
                   run_s2nc ~ticket_in:session_path
                     ~early_data_file:early_data_path ~close_stdin:true ()
                 in
                 assert_process_success early;
                 Alcotest.(check bool) "blocking callback started" true
                   (Atomic.get callback_started);
                 Alcotest.(check bool)
                   "timeout completes before blocking callback" false
                   (Atomic.get callback_finished);
                 Alcotest.(check bool) "timeout rejects early data" true
                   (contains "Early Data status: REJECTED" early.stdout)))))

let test_blocking_replay_store_deadline_and_capacity () =
  let module Store = Halo.Server.Config.Early_data_replay_store in
  let callback_finished = Atomic.make false in
  let store =
    Store.create_blocking ~max_in_flight:1 (fun ~identity:_ ~ttl_secs:_ ->
        Thread.delay 0.5;
        Atomic.set callback_finished true;
        Store.Fresh)
  in
  Eio_main.run @@ fun _env ->
  let result =
    Eio.Fiber.first
      (fun () ->
        Eio_unix.sleep 0.01;
        Store.Unavailable)
      (fun () -> Store.claim store ~identity:"identity" ~ttl_secs:1L)
  in
  Alcotest.(check bool) "timeout wins" true (result = Store.Unavailable);
  Alcotest.(check bool) "callback remains isolated" false
    (Atomic.get callback_finished);
  Alcotest.(check bool) "occupied capacity rejects another claim" true
    (Store.claim store ~identity:"other" ~ttl_secs:1L = Store.Unavailable)

let () =
  Alcotest.run "runtime"
    [
      ( "transport metadata",
        [
          Alcotest.test_case "plaintext request" `Quick
            test_plaintext_request_exposes_transport_metadata;
          Alcotest.test_case "TLS request" `Slow
            test_tls_request_exposes_negotiated_transport_metadata;
          Alcotest.test_case "keep-alive requests share metadata" `Quick
            test_keep_alive_requests_share_connection_metadata;
        ] );
      ( "errors",
        [
          Alcotest.test_case "handler reports raw backtrace" `Quick
            test_handler_error_reports_raw_backtrace;
        ] );
      ( "response lifecycle",
        [
          Alcotest.test_case "fixed response completes" `Quick
            test_fixed_response_reports_completed;
          Alcotest.test_case "stream response completes" `Quick
            test_stream_response_reports_completed;
          Alcotest.test_case "failed stream aborts" `Quick
            test_failed_stream_reports_aborted;
          Alcotest.test_case "peer disconnects" `Quick
            test_peer_disconnect_reports_client_disconnected;
        ] );
      ( "telemetry",
        [
          Alcotest.test_case "request transport lifecycle" `Quick
            test_request_telemetry_covers_transport_lifecycle;
          Alcotest.test_case "pre-dispatch parser failure" `Quick
            test_pre_dispatch_parser_failure_is_observed;
          Alcotest.test_case "pre-dispatch admission rejection" `Quick
            test_pre_dispatch_admission_rejection_is_observed;
          Alcotest.test_case "invalid trace context is ignored" `Quick
            test_invalid_trace_context_does_not_fail_request;
          Alcotest.test_case "observer failures are isolated" `Quick
            test_telemetry_observer_failures_do_not_fail_requests;
        ] );
      ( "metrics",
        [
          Alcotest.test_case "default snapshot" `Quick
            test_runtime_metrics_defaults;
          Alcotest.test_case "request metrics cover live server" `Quick
            test_request_metrics_cover_live_server;
          Alcotest.test_case "explicit keys count full and resumed handshakes"
            `Slow
            test_tls_metrics_count_full_and_resumed_handshakes_with_explicit_keys;
          Alcotest.test_case
            "ephemeral keys resume within a process and invalidate after restart"
            `Slow test_tls_metrics_count_ephemeral_resumption_and_restart_invalidation;
          Alcotest.test_case "early data defaults to 425 and rejects replay"
            `Slow test_tls_early_data_defaults_to_425_and_rejects_replay;
          Alcotest.test_case "early data policy opts in and marks request" `Slow
            test_tls_early_data_policy_opts_in_and_marks_request;
          Alcotest.test_case "early data replay is atomic across runtimes" `Slow
            test_tls_early_data_replay_is_atomic_across_runtimes;
          Alcotest.test_case "custom replay store contract" `Slow
            test_tls_custom_replay_store_contract;
          Alcotest.test_case "blocking replay store preserves timeout" `Slow
            test_tls_blocking_replay_store_does_not_block_timeout;
          Alcotest.test_case "blocking replay store deadline and capacity"
            `Quick test_blocking_replay_store_deadline_and_capacity;
        ] );
    ]
