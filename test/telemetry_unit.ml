module Telemetry = Halo.Telemetry

let fail fmt = Printf.ksprintf (fun message -> Alcotest.fail message) fmt

let sample_span () =
  let trace_id =
    Telemetry.Trace_id.of_hex "4bf92f3577b34da6a3ce929d0e0e4736" |> Option.get
  in
  let span_id = Telemetry.Span_id.of_hex "00f067aa0ba902b7" |> Option.get in
  let id_generator =
    Telemetry.Id_generator.create
      ~trace_id:(fun () -> trace_id)
      ~span_id:(fun () -> span_id)
      ()
  in
  let memory = Telemetry.In_memory_exporter.create () in
  let processor =
    Telemetry.In_memory_exporter.exporter memory |> Telemetry.Processor.simple
  in
  let tracer = Telemetry.Tracer.create ~id_generator ~processor () in
  let span =
    Telemetry.Tracer.start_span ~start_time_unix_ns:1L ~start_monotonic_ns:1L
      tracer "retry-test"
  in
  Telemetry.Span.finish ~end_time_unix_ns:2L span;
  match Telemetry.In_memory_exporter.finished_spans memory with
  | [ span ] -> span
  | spans -> fail "expected one sample span, got %d" (List.length spans)

let quick_retry ?(max_attempts = 3) () =
  Telemetry.Otlp_http.Retry.create ~max_attempts ~initial_delay:0.001
    ~max_delay:0.001 ~multiplier:1.0 ~jitter:0.0 ()

let test_custom_transport_retries_until_success () =
  Eio_main.run @@ fun _env ->
  let calls = ref 0 in
  let bodies = ref [] in
  let transport ~endpoint:_ ~headers:_ ~body =
    incr calls;
    bodies := body :: !bodies;
    if !calls < 3 then Error (Printf.sprintf "failure %d" !calls) else Ok ()
  in
  let exporter =
    Telemetry.Otlp_http.exporter_with_transport ~transport
      ~service_name:"retry-test" ~retry:(quick_retry ()) ()
  in
  Alcotest.(check (result unit string))
    "eventual result" (Ok ())
    (Telemetry.Exporter.export exporter [ sample_span () ]);
  Alcotest.(check int) "attempt count" 3 !calls;
  match !bodies with
  | first :: rest ->
      Alcotest.(check bool)
        "encoded body is reused" true
        (List.for_all (String.equal first) rest)
  | [] -> fail "transport was not called"

let test_custom_transport_stops_after_max_attempts () =
  Eio_main.run @@ fun _env ->
  let calls = ref 0 in
  let transport ~endpoint:_ ~headers:_ ~body:_ =
    incr calls;
    Error (Printf.sprintf "failure %d" !calls)
  in
  let exporter =
    Telemetry.Otlp_http.exporter_with_transport ~transport
      ~service_name:"retry-test" ~retry:(quick_retry ()) ()
  in
  Alcotest.(check (result unit string))
    "last error" (Error "failure 3")
    (Telemetry.Exporter.export exporter [ sample_span () ]);
  Alcotest.(check int) "attempt count" 3 !calls

let test_retry_can_be_disabled () =
  Eio_main.run @@ fun _env ->
  let calls = ref 0 in
  let transport ~endpoint:_ ~headers:_ ~body:_ =
    incr calls;
    Error "unavailable"
  in
  let exporter =
    Telemetry.Otlp_http.exporter_with_transport ~transport
      ~service_name:"retry-test" ~retry:Telemetry.Otlp_http.Retry.disabled ()
  in
  Alcotest.(check (result unit string))
    "first error" (Error "unavailable")
    (Telemetry.Exporter.export exporter [ sample_span () ]);
  Alcotest.(check int) "attempt count" 1 !calls

let test_backoff_is_cancellable () =
  Eio_main.run @@ fun _env ->
  let calls = ref 0 in
  let transport ~endpoint:_ ~headers:_ ~body:_ =
    incr calls;
    Error "unavailable"
  in
  let retry =
    Telemetry.Otlp_http.Retry.create ~max_attempts:3 ~initial_delay:1.0
      ~max_delay:1.0 ~multiplier:1.0 ~jitter:0.0 ()
  in
  let exporter =
    Telemetry.Otlp_http.exporter_with_transport ~transport
      ~service_name:"retry-test" ~retry ()
  in
  let outcome =
    Eio.Fiber.first
      (fun () ->
        ignore (Telemetry.Exporter.export exporter [ sample_span () ]);
        `Export_finished)
      (fun () ->
        Eio_unix.sleep 0.001;
        `Cancelled)
  in
  Alcotest.(check bool) "backoff was cancelled" true (outcome = `Cancelled);
  Alcotest.(check int) "no second attempt" 1 !calls

let test_retry_validation () =
  let module Retry = Telemetry.Otlp_http.Retry in
  Alcotest.check_raises "positive attempts"
    (Invalid_argument "max_attempts must be positive") (fun () ->
      ignore (Retry.create ~max_attempts:0 ()));
  Alcotest.check_raises "positive initial delay"
    (Invalid_argument "initial_delay must be positive and finite") (fun () ->
      ignore (Retry.create ~initial_delay:0.0 ()));
  Alcotest.check_raises "maximum delay"
    (Invalid_argument "max_delay must be finite and at least initial_delay")
    (fun () -> ignore (Retry.create ~initial_delay:2.0 ~max_delay:1.0 ()));
  Alcotest.check_raises "multiplier"
    (Invalid_argument "multiplier must be finite and at least one") (fun () ->
      ignore (Retry.create ~multiplier:0.5 ()));
  Alcotest.check_raises "jitter"
    (Invalid_argument "jitter must be between zero and one") (fun () ->
      ignore (Retry.create ~jitter:1.1 ()))

let header_value name line =
  match String.index_opt line ':' with
  | None -> None
  | Some separator ->
      let actual = String.sub line 0 separator |> String.trim in
      if String.equal (String.lowercase_ascii actual) name then
        Some
          (String.sub line (separator + 1) (String.length line - separator - 1)
          |> String.trim)
      else None

let read_request flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:1_048_576 in
  ignore (Eio.Buf_read.line reader);
  let rec headers content_length =
    match Eio.Buf_read.line reader with
    | "" -> content_length
    | line ->
        let content_length =
          match header_value "content-length" line with
          | Some value -> int_of_string value
          | None -> content_length
        in
        headers content_length
  in
  let content_length = headers 0 in
  ignore (Eio.Buf_read.take content_length reader)

let response status ?retry_after () =
  let reason = if status = 200 then "OK" else "test failure" in
  let retry_after =
    match retry_after with
    | None -> ""
    | Some seconds -> Printf.sprintf "Retry-After: %d\r\n" seconds
  in
  Printf.sprintf
    "HTTP/1.1 %d %s\r\n%sContent-Length: 0\r\nConnection: close\r\n\r\n" status
    reason retry_after

type test_response = Close | Respond of string

let with_http_responses env responses run =
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let socket =
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:4 net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  let calls = ref 0 in
  let rec serve = function
    | [] -> ()
    | response :: remaining ->
        let flow, _address = Eio.Net.accept ~sw socket in
        Fun.protect
          ~finally:(fun () -> Eio.Flow.close flow)
          (fun () ->
            read_request flow;
            incr calls;
            match response with
            | Close -> ()
            | Respond response -> Eio.Flow.copy_string response flow);
        serve remaining
  in
  Eio.Fiber.fork ~sw (fun () -> serve responses);
  let endpoint =
    Uri.of_string (Printf.sprintf "http://127.0.0.1:%d/v1/traces" port)
  in
  run ~net ~endpoint ~calls

let export_to_http ~net ~endpoint retry =
  let exporter =
    Telemetry.Otlp_http.exporter ~net ~endpoint ~service_name:"retry-test"
      ~retry ()
  in
  Telemetry.Exporter.export exporter [ sample_span () ]

let test_builtin_transport_retries_transient_status () =
  Eio_main.run @@ fun env ->
  with_http_responses env
    [ Respond (response 503 ~retry_after:0 ()); Respond (response 200 ()) ]
    (fun ~net ~endpoint ~calls ->
      Alcotest.(check (result unit string))
        "eventual result" (Ok ())
        (export_to_http ~net ~endpoint (quick_retry ()));
      Alcotest.(check int) "request count" 2 !calls)

let test_builtin_transport_retries_closed_connection () =
  Eio_main.run @@ fun env ->
  with_http_responses env
    [ Close; Respond (response 200 ()) ]
    (fun ~net ~endpoint ~calls ->
      Alcotest.(check (result unit string))
        "eventual result" (Ok ())
        (export_to_http ~net ~endpoint (quick_retry ()));
      Alcotest.(check int) "request count" 2 !calls)

let test_builtin_transport_does_not_retry_permanent_status () =
  Eio_main.run @@ fun env ->
  with_http_responses env
    [ Respond (response 400 ()) ]
    (fun ~net ~endpoint ~calls ->
      Alcotest.(check (result unit string))
        "permanent result" (Error "HTTP endpoint returned status 400")
        (export_to_http ~net ~endpoint (quick_retry ()));
      Alcotest.(check int) "request count" 1 !calls)

let () =
  Alcotest.run "telemetry"
    [
      ( "OTLP retry",
        [
          Alcotest.test_case "custom transport eventually succeeds" `Quick
            test_custom_transport_retries_until_success;
          Alcotest.test_case "custom transport exhausts attempts" `Quick
            test_custom_transport_stops_after_max_attempts;
          Alcotest.test_case "retry can be disabled" `Quick
            test_retry_can_be_disabled;
          Alcotest.test_case "backoff is cancellable" `Quick
            test_backoff_is_cancellable;
          Alcotest.test_case "configuration validation" `Quick
            test_retry_validation;
          Alcotest.test_case "transient HTTP status is retried" `Quick
            test_builtin_transport_retries_transient_status;
          Alcotest.test_case "closed connection is retried" `Quick
            test_builtin_transport_retries_closed_connection;
          Alcotest.test_case "permanent HTTP status is not retried" `Quick
            test_builtin_transport_does_not_retry_permanent_status;
        ] );
    ]
