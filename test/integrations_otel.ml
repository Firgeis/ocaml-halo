module Trace_context = Halo.Telemetry.Trace_context

let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
let parent_span_id = "00f067aa0ba902b7"
let trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"

let traceparent ?(version = "00") ?(trace_id = trace_id)
    ?(span_id = parent_span_id) ?(flags = "01") () =
  Printf.sprintf "%s-%s-%s-%s" version trace_id span_id flags

let fail fmt = Printf.ksprintf (fun message -> Alcotest.fail message) fmt

let require_context = function
  | Some context -> context
  | None -> Alcotest.fail "expected a valid trace context"

let test_extract_and_serialize () =
  let context =
    Trace_context.of_headers
      [
        ("TraceParent", "\t" ^ traceparent ~flags:"03" () ^ " ");
        ("tracestate", "rojo=00f067aa0ba902b7");
        ("TraceState", "congo=t61rcWkgMzE");
      ]
    |> require_context
  in
  Alcotest.(check string) "trace ID" trace_id
    (Trace_context.trace_id context);
  Alcotest.(check string) "span ID" parent_span_id
    (Trace_context.span_id context);
  Alcotest.(check int) "trace flags" 3 (Trace_context.trace_flags context);
  Alcotest.(check bool) "sampled" true (Trace_context.sampled context);
  Alcotest.(check (list (pair string string))) "tracestate"
    [ ("rojo", "00f067aa0ba902b7"); ("congo", "t61rcWkgMzE") ]
    (Trace_context.tracestate context);
  Alcotest.(check string) "canonical traceparent"
    (traceparent ~flags:"03" ())
    (Trace_context.traceparent context);
  Alcotest.(check (option string)) "canonical tracestate"
    (Some trace_state)
    (Trace_context.tracestate_header context);
  Alcotest.(check (list (pair string string))) "propagation headers"
    [
      ("traceparent", traceparent ~flags:"03" ());
      ("tracestate", trace_state);
    ]
    (Trace_context.to_headers context)

let test_make_context () =
  let context =
    Trace_context.make ~trace_id ~span_id:parent_span_id ~trace_flags:0
      ~tracestate:[ ("vendor", "value") ]
    |> require_context
  in
  Alcotest.(check bool) "not sampled" false (Trace_context.sampled context);
  Alcotest.(check string) "serialized"
    (traceparent ~flags:"00" ())
    (Trace_context.traceparent context);
  Alcotest.(check bool) "uppercase trace ID is invalid" true
    (Trace_context.make ~trace_id:(String.uppercase_ascii trace_id)
       ~span_id:parent_span_id ~trace_flags:0 ~tracestate:[]
    = None);
  Alcotest.(check bool) "trace flags must fit one byte" true
    (Trace_context.make ~trace_id ~span_id:parent_span_id ~trace_flags:256
       ~tracestate:[]
    = None)

let test_invalid_traceparent_is_ignored () =
  let invalid =
    [
      "";
      traceparent ~trace_id:"00000000000000000000000000000000" ();
      traceparent ~span_id:"0000000000000000" ();
      traceparent ~version:"ff" ();
      traceparent () ^ "-extension";
      traceparent ~trace_id:(String.uppercase_ascii trace_id) ();
      traceparent ~flags:"0g" ();
    ]
  in
  List.iter
    (fun value ->
      Alcotest.(check bool) (Printf.sprintf "invalid %S" value) true
        (Trace_context.of_headers [ ("traceparent", value) ] = None))
    invalid;
  Alcotest.(check bool) "duplicate traceparent" true
    (Trace_context.of_headers
       [
         ("traceparent", traceparent ());
         ("TraceParent", traceparent ());
       ]
    = None)

let test_future_traceparent_version () =
  let value = traceparent ~version:"01" () ^ "-future-data" in
  let context =
    Trace_context.of_headers [ ("traceparent", value) ] |> require_context
  in
  Alcotest.(check string) "future version fields are retained"
    (traceparent ())
    (Trace_context.traceparent context)

let test_invalid_tracestate_is_discarded () =
  let check label tracestate =
    let context =
      Trace_context.of_headers
        [ ("traceparent", traceparent ()); ("tracestate", tracestate) ]
      |> require_context
    in
    Alcotest.(check (list (pair string string))) label []
      (Trace_context.tracestate context)
  in
  check "duplicate keys" "vendor=one,vendor=two";
  check "uppercase key" "Vendor=value";
  check "empty member" "vendor=value,";
  check "forbidden value character" "vendor=one=two";
  let members =
    List.init 33 (fun index -> Printf.sprintf "vendor%d=value" index)
    |> String.concat ","
  in
  check "too many members" members

type managed_process = {
  pid : int;
  mutable status : Unix.process_status option;
}

let read_file path = In_channel.with_open_bin path In_channel.input_all

let poll_process process =
  match process.status with
  | Some _ as status -> status
  | None -> (
      match Unix.waitpid [ Unix.WNOHANG ] process.pid with
      | 0, _ -> None
      | _, status ->
          process.status <- Some status;
          Some status)

let wait_for_process process ~timeout =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    match poll_process process with
    | Some _ as status -> status
    | None when Unix.gettimeofday () >= deadline -> None
    | None ->
        Unix.sleepf 0.05;
        loop ()
  in
  loop ()

let signal_process process signal =
  if process.status = None then
    try Unix.kill process.pid signal with
    | Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let stop_process process =
  match poll_process process with
  | Some status -> status
  | None ->
      signal_process process Sys.sigterm;
      (match wait_for_process process ~timeout:5.0 with
      | Some status -> status
      | None ->
          signal_process process Sys.sigkill;
          match wait_for_process process ~timeout:2.0 with
          | Some status -> status
          | None -> fail "collector process %d could not be reaped" process.pid)

let string_of_process_status = function
  | Unix.WEXITED code -> Printf.sprintf "exited with code %d" code
  | Unix.WSIGNALED signal ->
      Printf.sprintf "terminated by signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

let collector_path () =
  match Sys.getenv_opt "HALO_OTELCOL_BIN" with
  | Some path when path <> "" -> path
  | _ -> fail "expected HALO_OTELCOL_BIN in the test environment"

let unused_tcp_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      Unix.setsockopt socket Unix.SO_REUSEADDR true;
      Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
      match Unix.getsockname socket with
      | Unix.ADDR_INET (_, port) -> port
      | Unix.ADDR_UNIX _ -> assert false)

let can_connect port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      try
        Unix.connect socket
          (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
        true
      with
      | Unix.Unix_error
          ((Unix.ECONNREFUSED | Unix.ECONNRESET | Unix.ETIMEDOUT), _, _) ->
          false)

let wait_for_collector process ~port ~log_path =
  let deadline = Unix.gettimeofday () +. 10.0 in
  let rec loop () =
    if can_connect port then ()
    else
      match poll_process process with
      | Some status ->
          fail "collector %s during startup\n%s"
            (string_of_process_status status)
            (read_file log_path)
      | None when Unix.gettimeofday () >= deadline ->
          fail "timed out waiting for collector on port %d\n%s" port
            (read_file log_path)
      | None ->
          Unix.sleepf 0.05;
          loop ()
  in
  loop ()

let start_collector ~config_path ~log_path =
  let executable = collector_path () in
  let input = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
  let log =
    Unix.openfile log_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o600
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close input;
      Unix.close log)
    (fun () ->
      let argv = [| executable; "--config"; config_path |] in
      { pid = Unix.create_process executable argv input log log; status = None })

let remove_if_present path =
  try Sys.remove path with Sys_error _ -> ()

let with_collector ~port run =
  let config_path = Filename.temp_file "halo-otelcol" ".yaml" in
  let output_path = Filename.temp_file "halo-otel" ".json" in
  let log_path = Filename.temp_file "halo-otelcol" ".log" in
  remove_if_present output_path;
  let config =
    Printf.sprintf
      "receivers:\n  otlp:\n    protocols:\n      http:\n        endpoint: 127.0.0.1:%d\nexporters:\n  file:\n    path: %S\n    format: json\n    flush_interval: 10ms\nservice:\n  telemetry:\n    logs:\n      level: warn\n  pipelines:\n    traces:\n      receivers: [otlp]\n      exporters: [file]\n"
      port output_path
  in
  Out_channel.with_open_bin config_path (fun channel ->
      output_string channel config);
  Fun.protect
    ~finally:(fun () ->
      remove_if_present config_path;
      remove_if_present output_path;
      remove_if_present log_path)
    (fun () ->
      let process = start_collector ~config_path ~log_path in
      let outcome =
        try
          wait_for_collector process ~port ~log_path;
          Ok (run output_path)
        with exn -> Error (exn, Printexc.get_raw_backtrace ())
      in
      let status = stop_process process in
      match outcome with
      | Error (exn, backtrace) ->
          Printf.eprintf "otelcol log:\n%s\n%!" (read_file log_path);
          Printexc.raise_with_backtrace exn backtrace
      | Ok value ->
          (match status with
          | Unix.WEXITED 0 -> ()
          | Unix.WSIGNALED signal when signal = Sys.sigterm -> ()
          | status ->
              fail "collector %s during shutdown\n%s"
                (string_of_process_status status)
                (read_file log_path));
          value)

let write_all socket value =
  let rec loop offset =
    if offset < String.length value then
      let written =
        Unix.write_substring socket value offset (String.length value - offset)
      in
      loop (offset + written)
  in
  loop 0

let read_all socket =
  let buffer = Buffer.create 1024 in
  let bytes = Bytes.create 1024 in
  let deadline = Unix.gettimeofday () +. 5.0 in
  let rec loop () =
    if Unix.gettimeofday () >= deadline then
      fail "timed out reading the Halo response"
    else
      match Unix.select [ socket ] [] [] 0.05 with
      | [], _, _ -> loop ()
      | _ ->
          let length = Unix.read socket bytes 0 (Bytes.length bytes) in
          if length = 0 then Buffer.contents buffer
          else (
            Buffer.add_subbytes buffer bytes 0 length;
            loop ())
  in
  loop ()

let request_halo port =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      Unix.connect socket
        (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
      let request =
        Printf.sprintf
          "GET /otel/42 HTTP/1.1\r\nHost: localhost\r\ntraceparent: %s\r\ntracestate: %s\r\nConnection: close\r\n\r\n"
          (traceparent ()) trace_state
      in
      write_all socket request;
      Unix.shutdown socket Unix.SHUTDOWN_SEND;
      read_all socket)

let result_error label = function
  | Ok () -> None
  | Error message -> Some (Printf.sprintf "%s: %s" label message)

let run_halo_export ~collector_port ~server_port =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let endpoint =
    Uri.of_string
      (Printf.sprintf "http://127.0.0.1:%d/v1/traces" collector_port)
  in
  let exporter =
    Halo.Telemetry.Otlp_http.exporter ~net:(Eio.Stdenv.net env)
      ~service_name:"halo-otel-integration" ~endpoint ()
  in
  let processor =
    Halo.Telemetry.Batch_processor.create ~sw ~scheduled_delay:60.0
      ~export_timeout:5.0 exporter
  in
  let tracer = Halo.Telemetry.Tracer.create ~processor () in
  let telemetry = Halo.Telemetry.of_tracer tracer in
  let router =
    Halo.Router.(
      route ~template:"/otel/{id}"
        Matcher.(
          let+ _id =
            request_method_is GET *> path_segment "otel" *> string_parameter
            <* path_end
          in
          fun _request -> text Status.ok "exported\n"))
  in
  let config =
    Halo.Server.Config.create ~host:"127.0.0.1" ~port:server_port router
  in
  let started, notify_started = Eio.Promise.create () in
  let server =
    Eio.Fiber.fork_promise ~sw (fun () ->
        Halo.Server.run ~handle_sigint:false ~telemetry
          ~on_ready:(fun running -> Eio.Promise.resolve notify_started running)
          [ config ])
  in
  let running = Eio.Promise.await started in
  let request_outcome =
    try
      let response =
        Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
            request_halo server_port)
      in
      Ok response
    with exn -> Error (exn, Printexc.get_raw_backtrace ())
  in
  Halo.Server.shutdown running;
  let server_outcome =
    try
      Eio.Promise.await_exn server;
      Ok ()
    with exn -> Error (exn, Printexc.get_raw_backtrace ())
  in
  let flush_error =
    Halo.Telemetry.Processor.force_flush processor
    |> result_error "force flush"
  in
  let shutdown_error =
    Halo.Telemetry.Processor.shutdown processor
    |> result_error "processor shutdown"
  in
  (match request_outcome with
  | Ok response ->
      Alcotest.(check bool) "Halo response status" true
        (String.starts_with ~prefix:"HTTP/1.1 200 OK\r\n" response)
  | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace);
  (match server_outcome with
  | Ok () -> ()
  | Error (exn, backtrace) -> Printexc.raise_with_backtrace exn backtrace);
  Option.iter Alcotest.fail flush_error;
  Option.iter Alcotest.fail shutdown_error

let json_field name = function
  | `Assoc fields -> (
      match List.assoc_opt name fields with
      | Some value -> value
      | None -> fail "OTLP JSON object has no %S field" name)
  | json -> fail "expected an OTLP JSON object, got %s" (Yojson.Safe.to_string json)

let json_list_field name json =
  match json_field name json with
  | `List values -> values
  | value -> fail "OTLP JSON field %S is not a list: %s" name
               (Yojson.Safe.to_string value)

let json_string_field name json =
  match json_field name json with
  | `String value -> value
  | value -> fail "OTLP JSON field %S is not a string: %s" name
               (Yojson.Safe.to_string value)

let json_int64_field name json =
  match json_field name json with
  | `String value | `Intlit value -> (
      match Int64.of_string_opt value with
      | Some value -> value
      | None -> fail "OTLP JSON field %S is not an int64: %s" name value)
  | `Int value -> Int64.of_int value
  | value -> fail "OTLP JSON field %S is not an int64: %s" name
               (Yojson.Safe.to_string value)

let attribute_value key owner =
  let attributes = json_list_field "attributes" owner in
  let rec find = function
    | [] -> fail "OTLP JSON attributes have no %S entry" key
    | attribute :: rest ->
        if json_string_field "key" attribute = key then
          json_field "value" attribute
        else
          find rest
  in
  find attributes

let string_attribute key owner =
  match json_field "stringValue" (attribute_value key owner) with
  | `String value -> value
  | value -> fail "OTLP attribute %S is not a string: %s" key
               (Yojson.Safe.to_string value)

let int64_attribute key owner =
  json_int64_field "intValue" (attribute_value key owner)

let find_exported_span documents expected_name =
  let rec find_resource_spans = function
    | [] -> None
    | resource_spans :: remaining_resources ->
        let rec find_scope_spans = function
          | [] -> find_resource_spans remaining_resources
          | scope_spans :: remaining_scopes -> (
              match
                List.find_opt
                  (fun span ->
                    json_string_field "name" span = expected_name)
                  (json_list_field "spans" scope_spans)
              with
              | Some span -> Some (resource_spans, scope_spans, span)
              | None -> find_scope_spans remaining_scopes)
        in
        find_scope_spans (json_list_field "scopeSpans" resource_spans)
  in
  let resource_spans =
    List.concat_map (json_list_field "resourceSpans") documents
  in
  match find_resource_spans resource_spans with
  | Some result -> result
  | None -> fail "collector did not export span %S" expected_name

let parse_json_lines contents =
  contents
  |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
         let line = String.trim line in
         if line = "" then None else Some (Yojson.Safe.from_string line))

let wait_for_export path =
  let deadline = Unix.gettimeofday () +. 5.0 in
  let rec loop () =
    let parsed =
      try
        if Sys.file_exists path then
          match parse_json_lines (read_file path) with
          | [] -> Error "the export file is empty"
          | documents -> Ok documents
        else
          Error "the export file does not exist"
      with
      | Yojson.Json_error message -> Error message
      | Sys_error message -> Error message
    in
    match parsed with
    | Ok documents -> documents
    | Error _ when Unix.gettimeofday () < deadline ->
        Unix.sleepf 0.05;
        loop ()
    | Error message -> fail "collector output was not ready: %s" message
  in
  loop ()

let test_export_to_reference_collector () =
  let collector_port = unused_tcp_port () in
  with_collector ~port:collector_port (fun output_path ->
      let server_port = unused_tcp_port () in
      run_halo_export ~collector_port ~server_port;
      let documents = wait_for_export output_path in
      let resource_spans, scope_spans, span =
        find_exported_span documents "GET /otel/{id}"
      in
      let resource = json_field "resource" resource_spans in
      Alcotest.(check string) "service name" "halo-otel-integration"
        (string_attribute "service.name" resource);
      let scope = json_field "scope" scope_spans in
      Alcotest.(check string) "instrumentation scope" "ocaml-halo"
        (json_string_field "name" scope);
      Alcotest.(check string) "propagated trace ID" trace_id
        (String.lowercase_ascii (json_string_field "traceId" span));
      Alcotest.(check string) "parent span ID" parent_span_id
        (String.lowercase_ascii (json_string_field "parentSpanId" span));
      let span_id = json_string_field "spanId" span in
      Alcotest.(check int) "span ID length" 16 (String.length span_id);
      Alcotest.(check bool) "span ID is nonzero" false
        (String.equal span_id "0000000000000000");
      Alcotest.(check string) "propagated tracestate" trace_state
        (json_string_field "traceState" span);
      Alcotest.(check string) "HTTP method" "GET"
        (string_attribute "http.request.method" span);
      Alcotest.(check string) "HTTP route" "/otel/{id}"
        (string_attribute "http.route" span);
      Alcotest.(check int64) "HTTP status" 200L
        (int64_attribute "http.response.status_code" span);
      let start_time = json_int64_field "startTimeUnixNano" span in
      let end_time = json_int64_field "endTimeUnixNano" span in
      Alcotest.(check bool) "C request start time is populated" true
        (Int64.compare start_time 0L > 0);
      Alcotest.(check bool) "span ends after it starts" true
        (Int64.compare end_time start_time >= 0))

let () =
  Alcotest.run "OpenTelemetry integrations"
    [
      ( "W3C trace context",
        [
          Alcotest.test_case "extract and serialize" `Quick
            test_extract_and_serialize;
          Alcotest.test_case "make context" `Quick test_make_context;
          Alcotest.test_case "invalid traceparent is ignored" `Quick
            test_invalid_traceparent_is_ignored;
          Alcotest.test_case "future traceparent version" `Quick
            test_future_traceparent_version;
          Alcotest.test_case "invalid tracestate is discarded" `Quick
            test_invalid_tracestate_is_discarded;
        ] );
      ( "OTLP/HTTP",
        [
          Alcotest.test_case "export through the reference Collector" `Slow
            test_export_to_reference_collector;
        ] );
    ]
