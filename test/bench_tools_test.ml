open Bench_test_support
let http = Sys.getenv "HTTP_BENCH" and https = Sys.getenv "HTTPS_BENCH"
let sample = Sys.getenv "PROCESS_SAMPLE"
let fixture = let p = Sys.getenv "BENCH_FIXTURE" in if Filename.is_implicit p then "./" ^ p else p
let cert = Sys.getenv "TLS_CERT" and key = Sys.getenv "TLS_KEY"
let with_server mode log f =
  let input, output = Unix.pipe () in
  let null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0 in
  let pid = Unix.create_process fixture [|fixture; mode; cert; key; log|] null output null in
  Unix.close output; Unix.close null;
  Fun.protect ~finally:(fun () ->
    (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()));
    (try ignore (Unix.waitpid [] pid) with Unix.Unix_error _ -> ()); Unix.close input)
    (fun () ->
      let ready, _, _ = Unix.select [input] [] [] 5. in
      if ready = [] then Alcotest.fail "server startup deadline exceeded";
      let buf = Bytes.create 32 in let n = Unix.read input buf 0 32 in
      if n = 0 then Alcotest.fail "server did not start";
      let port = int_of_string (String.trim (Bytes.sub_string buf 0 n)) in f port)
let bench ?(code=0) exe port args =
  check_run ~code exe (["--port";string_of_int port;"--requests";"6";"--concurrency";"2";"--warmup-requests";"0"] @ args) |> json
let errors payload = Yojson.Safe.Util.to_list (member "errors" payload) <> []
let test_http () = with_temp (fun d ->
  let log = Filename.concat d "requests" and body = Filename.concat d "body" in
  write body "hello\000world";
  with_server "plain" log (fun port ->
    let result = bench http port ["--method";"POST";"--body-file";body;"--chunked";"--chunk-size";"3";"--keep-alive";"--header";"X-Test: hello";"--expect-status";"200";"--expect-status";"200"] in
    Alcotest.(check int) "completed" 6 (int "completed_requests" result);
    check_json "statuses" (json {|{"200":6}|}) (member "status_counts" result);
    check_json "expected statuses" (json "[200]") (member "expected_statuses" result);
    Alcotest.(check int) "body length" 11 (int "payload_bytes" result);
    let records = String.split_on_char '\n' (read log) |> List.filter ((<>) "") in
    Alcotest.(check int) "request count" 6 (List.length records);
    let connections = List.map (fun record -> match String.split_on_char '|' record with
      | [connection; header; body] ->
          Alcotest.(check string) "custom header" " hello" header;
          Alcotest.(check string) "binary body" "68656c6c6f00776f726c64" body;
          connection
      | _ -> Alcotest.fail "malformed fixture record") records in
    Alcotest.(check int) "keep-alive connections" 2 (List.length (List.sort_uniq String.compare connections));
    Alcotest.(check bool) "positive throughput" true (Yojson.Safe.Util.to_number (member "throughput_rps" result) > 0.);
    let latency = member "latency_ms" result in
    Alcotest.(check bool) "latency order" true (Yojson.Safe.Util.to_number (member "median" latency) <= Yojson.Safe.Util.to_number (member "p99" latency));
    let failure = bench ~code:1 http port ["--expect-status";"201"] in
    check_json "unexpected statuses" (json {|{"200":6}|}) (member "unexpected_status_counts" failure)))
let make_untrusted d =
  let ca = Filename.concat d "other.pem" and ca_key = Filename.concat d "other-key.pem" in
  ignore (check_run "openssl" ["req";"-x509";"-newkey";"rsa:2048";"-nodes";"-subj";"/CN=untrusted";"-days";"1";"-keyout";ca_key;"-out";ca]); ca
let test_tls () = with_temp (fun d ->
  let invalid = Filename.concat d "invalid" in write invalid "not a certificate";
  let other = make_untrusted d in
  List.iter (fun version -> with_server version "-" (fun port ->
    let args = ["--ca-file";cert;"--tls-version";version] in
    let full = bench https port (args @ ["--reuse-mode";"full"]) in
    let reuse = bench https port (args @ ["--reuse-mode";"context-reuse"]) in
    Alcotest.(check int) "fresh completed" 6 (int "completed_requests" full);
    Alcotest.(check int) "fresh reused" 0 (int "session_reused_count" full);
    Alcotest.(check int) "reuse completed" 6 (int "completed_requests" reuse);
    Alcotest.(check int) "reused sessions" 4 (int "session_reused_count" reuse);
    let bad = bench ~code:1 https port ["--ca-file";invalid;"--tls-version";version] in
    Alcotest.(check bool) "malformed CA rejected" true (errors bad);
    let bad = bench ~code:1 https port ["--ca-file";other;"--tls-version";version] in
    Alcotest.(check int) "untrusted completed" 0 (int "completed_requests" bad);
    Alcotest.(check bool) "untrusted CA rejected" true (errors bad))) ["TLS12";"TLS13"])
let test_ready () = with_temp (fun d ->
  let other = make_untrusted d in
  let check_failure port ca =
    let start = Unix.gettimeofday () in
    let status, _, err = run ~timeout:7. https ["--wait-ready";"--port";string_of_int port;"--ca-file";ca] in
    Alcotest.(check bool) "readiness fails" true (status <> Unix.WEXITED 0);
    Alcotest.(check bool) "deadline diagnostic" true (try ignore (Str.search_forward (Str.regexp_string "server did not start listening in time") err 0); true with Not_found -> false);
    Alcotest.(check bool) "readiness deadline" true (Unix.gettimeofday () -. start < 6.5) in
  with_server "TLS13" "-" (fun port ->
    ignore (check_run ~timeout:7. https ["--wait-ready";"--port";string_of_int port;"--ca-file";cert]);
    check_failure port other);
  with_server "stall" "-" (fun port -> check_failure port cert))
let test_invalid () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let result = Fun.protect ~finally:(fun () -> Unix.close socket) (fun () ->
    Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
    let port = match Unix.getsockname socket with Unix.ADDR_INET (_, p) -> p | _ -> assert false in
    check_run ~timeout:5. ~code:1 http ["--port";string_of_int port;"--requests";"3";"--warmup-requests";"2";"--concurrency";"1"] |> json) in
  let warmup = member "warmup" result in
  Alcotest.(check int) "warmup completed" 0 (int "completed_requests" warmup);
  Alcotest.(check bool) "warmup errors" true (errors warmup);
  let status, _, err = run ~timeout:5. http ["--chunked";"--header";"Content-Length: 1"] in
  Alcotest.(check bool) "invalid arguments" true (status <> Unix.WEXITED 0);
  Alcotest.(check bool) "argument diagnostic" true (try ignore (Str.search_forward (Str.regexp_string "chunked requests cannot include Content-Length") err 0); true with Not_found -> false)
let test_sampler () =
  if String.trim (check_run "uname" ["-s"]) <> "Linux" then Alcotest.skip ();
  with_temp (fun d ->
  let output = Filename.concat d "samples" in
  let null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0 in
  let process = Unix.create_process "sleep" [|"sleep";"0.4"|] null null null in
  let sampler = Unix.create_process sample [|sample;"--pid";string_of_int process;"--interval-seconds";".02";"--output";output|] null null null in
  Unix.close null;
  Fun.protect ~finally:(fun () -> List.iter (fun pid -> try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()) [process;sampler]; List.iter (fun pid -> try ignore (Unix.waitpid [Unix.WNOHANG] pid) with Unix.Unix_error _ -> ()) [process;sampler]) (fun () ->
    ignore (wait ~timeout:5. process);
    Alcotest.(check bool) "sampler exit" true (wait ~timeout:5. sampler = Unix.WEXITED 0);
    let result = Yojson.Safe.from_file output in
    let count = int "sample_count" result in
    Alcotest.(check bool) "samples exist" true (count > 0);
    Alcotest.(check int) "samples count" count (List.length (Yojson.Safe.Util.to_list (member "samples" result)));
    Alcotest.(check bool) "rss" true (Yojson.Safe.Util.to_number (member "rss_kb_max" result) > 0.);
    Alcotest.(check bool) "cpu order" true (Yojson.Safe.Util.to_number (member "cpu_percent_max" result) >= Yojson.Safe.Util.to_number (member "cpu_percent_mean" result))))
let () = Alcotest.run "bench tools" ["integration", List.map (fun (n,f) -> Alcotest.test_case n `Quick f) ["HTTP",test_http;"TLS sessions",test_tls;"TLS readiness",test_ready;"arguments and warmup",test_invalid;"sampler",test_sampler]]
