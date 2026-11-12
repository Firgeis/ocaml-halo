module Metrics = Halo.Metrics

let fail fmt = Printf.ksprintf (fun message -> Alcotest.fail message) fmt

let contains needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search offset =
    if offset + needle_length > haystack_length then false
    else if String.sub haystack offset needle_length = needle then true
    else search (offset + 1)
  in
  search 0

let find_point name predicate snapshot =
  match
    List.find_opt
      (fun (point : Metrics.point) ->
        String.equal point.name name && predicate point.attributes)
      snapshot.Metrics.points
  with
  | Some point -> point
  | None -> fail "metric point %s was not present" name

let has_string_attribute name expected attributes =
  List.exists
    (function
      | actual, Metrics.String value ->
          String.equal actual name && String.equal value expected
      | _ -> false)
    attributes

let sample_snapshot () =
  let metrics =
    Metrics.create ~duration_buckets:[ 0.1; 1.0 ] ~size_buckets:[ 10.0; 100.0 ]
      ()
  in
  let request =
    Metrics.Private.start_request metrics ~method_:"POST" ~scheme:"https"
      ~start_monotonic_ns:100L
  in
  Metrics.Private.finish_request request ~route:(Some "/items/{id}")
    ~status_code:(Some 204) ~outcome:"completed"
    ~end_monotonic_ns:(Some 600_000_100L) ~request_body_size:12L
    ~response_body_size:120L;
  Metrics.Private.record_runtime metrics ~utilization:0.25
    ~active_connections:2L ~active_requests:1L ~accepted_connections:5L
    ~rejected_connections:1L ~completed_requests:4L ~tls_full_handshakes:2L
    ~tls_resumed_handshakes:1L ~tls_handshake_failures:1L ~event_queue_depth:3L
    ~command_queue_depth:4L ~controller_state:"busy" ~backpressure_enabled:true;
  Metrics.snapshot metrics

let check_histogram label expected_counts expected_sum point =
  match point.Metrics.aggregation with
  | Metrics.Histogram histogram ->
      Alcotest.(check (list int64))
        label expected_counts histogram.bucket_counts;
      Alcotest.(check (float 0.000_001))
        (label ^ " sum") expected_sum histogram.sum
  | _ -> fail "%s was not a histogram" point.name

let test_request_and_runtime_snapshot () =
  let snapshot = sample_snapshot () in
  let duration =
    find_point "http.server.request.duration"
      (has_string_attribute "http.route" "/items/{id}")
      snapshot
  in
  check_histogram "duration buckets" [ 0L; 1L; 1L ] 0.6 duration;
  let request_size =
    find_point "http.server.request.body.size" (fun _ -> true) snapshot
  in
  check_histogram "request size buckets" [ 0L; 1L; 1L ] 12.0 request_size;
  let response_size =
    find_point "http.server.response.body.size" (fun _ -> true) snapshot
  in
  check_histogram "response size buckets" [ 0L; 0L; 1L ] 120.0 response_size;
  let requests =
    find_point "halo.http.server.requests"
      (has_string_attribute "http.response.status_class" "2xx")
      snapshot
  in
  (match requests.aggregation with
  | Metrics.Sum { value = Metrics.Int 1L; monotonic = true } -> ()
  | _ -> fail "request counter had the wrong aggregation");
  let event_queue =
    find_point "halo.runtime.queue.depth"
      (has_string_attribute "queue" "event")
      snapshot
  in
  match event_queue.aggregation with
  | Metrics.Gauge (Metrics.Int 3L) -> ()
  | _ -> fail "event queue depth had the wrong value"

let test_prometheus_encoding () =
  let encoded = Metrics.Prometheus.encode (sample_snapshot ()) in
  Alcotest.(check bool)
    "duration family" true
    (contains "# TYPE http_server_request_duration_seconds histogram" encoded);
  Alcotest.(check bool)
    "finite bucket" true
    (contains "http_server_request_duration_seconds_bucket" encoded
    && contains "le=\"1\"" encoded);
  Alcotest.(check bool) "infinite bucket" true (contains "le=\"+Inf\"" encoded);
  Alcotest.(check bool)
    "status class counter" true
    (contains "halo_http_server_requests_total" encoded
    && contains "http_response_status_class=\"2xx\"" encoded);
  Alcotest.(check bool)
    "runtime queue labels" true
    (contains "halo_runtime_queue_depth" encoded
    && contains "queue=\"command\"" encoded)

let test_otlp_metrics_export () =
  let calls = ref 0 in
  let endpoint_seen = ref None in
  let body_seen = ref "" in
  let transport ~endpoint ~headers:_ ~body =
    incr calls;
    endpoint_seen := Some endpoint;
    body_seen := body;
    Ok ()
  in
  let exporter =
    Metrics.Otlp_http.exporter_with_transport ~transport
      ~service_name:"metrics-test" ()
  in
  Alcotest.(check (result unit string))
    "export succeeds" (Ok ())
    (Metrics.Otlp_http.export exporter (sample_snapshot ()));
  Alcotest.(check int) "single call" 1 !calls;
  Alcotest.(check string)
    "metrics endpoint" "/v1/metrics"
    (Option.get !endpoint_seen |> Uri.path);
  Alcotest.(check bool)
    "service resource encoded" true
    (contains "metrics-test" !body_seen);
  Alcotest.(check bool)
    "metric name encoded" true
    (contains "http.server.request.duration" !body_seen);
  Metrics.Otlp_http.shutdown exporter;
  Alcotest.(check (result unit string))
    "shutdown rejects export" (Error "OTLP metrics exporter is shut down")
    (Metrics.Otlp_http.export exporter (sample_snapshot ()))

let test_bucket_validation () =
  Alcotest.check_raises "increasing duration buckets"
    (Invalid_argument
       "duration_buckets must contain finite, non-negative, increasing values")
    (fun () -> ignore (Metrics.create ~duration_buckets:[ 1.0; 1.0 ] ()))

let test_combine () =
  let snapshot = sample_snapshot () in
  let point = find_point "halo.runtime.utilization" (fun _ -> true) snapshot in
  let one = { snapshot with points = [ point ] } in
  Alcotest.check_raises "duplicate series rejected"
    (Invalid_argument
       "Metrics.combine: duplicate point halo.runtime.utilization") (fun () ->
      ignore (Metrics.combine [ one; one ]));
  let combined = Metrics.combine [ Metrics.snapshot Metrics.noop; one ] in
  Alcotest.(check int64)
    "empty collector does not reset start time" one.start_time_unix_ns
    combined.start_time_unix_ns;
  Alcotest.(check int) "one metric remains" 1 (List.length combined.points)

let () =
  Alcotest.run "metrics"
    [
      ( "collector",
        [
          Alcotest.test_case "request and runtime snapshot" `Quick
            test_request_and_runtime_snapshot;
          Alcotest.test_case "bucket validation" `Quick test_bucket_validation;
          Alcotest.test_case "combine snapshots" `Quick test_combine;
        ] );
      ( "adapters",
        [
          Alcotest.test_case "Prometheus" `Quick test_prometheus_encoding;
          Alcotest.test_case "OTLP metrics" `Quick test_otlp_metrics_export;
        ] );
    ]
