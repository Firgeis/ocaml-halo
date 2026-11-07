open Bench_test_support
let report = Sys.getenv "BENCH_REPORT"
let path d n = Filename.concat d n
let test_args () =
  List.iter (fun args -> let status, out, err = run report args in
    Alcotest.(check bool) "usage exit" true (status = Unix.WEXITED 2);
    Alcotest.(check string) "stdout" "" out;
    Alcotest.(check string) "usage" "usage: bench_report (baseline|body-report|body-files) arguments...\n" err)
    [[]; ["baseline"]; ["unknown"]]
let test_body () = with_temp (fun d ->
  List.iter (fun (a,b) ->
    ignore (check_run report ["body-files"; path d "a"; string_of_int a; path d "b"; string_of_int b]);
    Alcotest.(check string) "a bytes" (String.make a 'a') (read (path d "a"));
    Alcotest.(check string) "b bytes" (String.make b 'b') (read (path d "b")))
    [0,1; 65535,65536; 65537,131073])
let test_baseline () = with_temp (fun d ->
  let result = path d "result" and time = path d "time" and pidstat = path d "pidstat" and sample = path d "sample" in
  write time "ignored\nelapsed=first\nelapsed=second=extra\n";
  write pidstat "Linux host\n12:00 UID PID %usr other other other other\n12:00 1 2 3 4 100 6 20.0\n12:01 1 2 3 4 300 6 40.0\nAverage: 1 2 3 4 999 6 99.0\n";
  write sample {|{"sample_interval_seconds":0.1,"sample_count":2,"cpu_percent_mean":1.234,"cpu_percent_max":5.678,"rss_kb_mean":101.25,"rss_kb_max":150}|};
  let scenarios = List.init 6 (fun i -> `Assoc ["scenario", `String (Printf.sprintf "scenario_%d" i); "text", `String "café 😀"]) in
  let args = ["baseline";result;time;pidstat;sample] @ List.map Yojson.Safe.to_string scenarios in
  let out = check_run report args in
  let payload = json out in
  check_json "scenarios" (`List scenarios) (member "scenarios" payload);
  check_json "metrics" (json {|{"elapsed":"second=extra","pidstat_cpu_percent_mean":"30.00","pidstat_cpu_percent_max":"40.00","pidstat_sample_count":"2","pidstat_rss_kb_mean":"200.00","pidstat_rss_kb_max":"300","process_sample_interval_seconds":"0.1","process_sample_count":"2","process_cpu_percent_mean":"1.23","process_cpu_percent_max":"5.68","process_rss_kb_mean":"101.25","process_rss_kb_max":"150"}|}) (member "server_metrics" payload);
  Alcotest.(check string) "file matches stdout" out (read result);
  Alcotest.(check bool) "escaped Unicode" true (let needle = "caf\\u00e9 \\ud83d\\ude00" in try ignore (Str.search_forward (Str.regexp_string needle) out 0); true with Not_found -> false);
  let scenario i = Printf.sprintf "    {\n      \"scenario\": \"scenario_%d\",\n      \"text\": \"caf\\u00e9 \\ud83d\\ude00\"\n    }" i in
  let expected = "{\n  \"scenarios\": [\n" ^ String.concat ",\n" (List.init 6 scenario) ^ "\n  ],\n  \"server_metrics\": {\n" ^
    "    \"elapsed\": \"second=extra\",\n    \"pidstat_cpu_percent_max\": \"40.00\",\n    \"pidstat_cpu_percent_mean\": \"30.00\",\n    \"pidstat_rss_kb_max\": \"300\",\n    \"pidstat_rss_kb_mean\": \"200.00\",\n    \"pidstat_sample_count\": \"2\",\n    \"process_cpu_percent_max\": \"5.68\",\n    \"process_cpu_percent_mean\": \"1.23\",\n    \"process_rss_kb_max\": \"150\",\n    \"process_rss_kb_mean\": \"101.25\",\n    \"process_sample_count\": \"2\",\n    \"process_sample_interval_seconds\": \"0.1\"\n  }\n}\n" in
  Alcotest.(check string) "exact JSON formatting" expected out;
  write time "no metrics\n"; Sys.remove pidstat; Sys.remove sample;
  let payload = json (check_run report args) in
  check_json "missing logs" (json {|{"timing_tool":"unavailable"}|}) (member "server_metrics" payload))
let test_body_report () = with_temp (fun d ->
  let result = path d "result" and sample = path d "sample" in write sample {|{"sample_count":0}|};
  let args = [result;sample;"100";"90";"110";"16";"0";"0";"0";"0";{|{"scenario":"a"}|};{|{"scenario":"b"}|};{|{"scenario":"c"}|};{|{"scenario":"d"}|};"1"] in
  let out = check_run report ("body-report" :: args) in
  let payload = json out in
  check_json "config" (json {|{"max_body_size":100,"accepted_body_size":90,"oversized_body_size":110,"chunk_size":16,"keep_alive":true}|}) (member "config" payload);
  check_json "scenarios" (json {|[{"scenario":"a","exit_code":0},{"scenario":"b","exit_code":0},{"scenario":"c","exit_code":0},{"scenario":"d","exit_code":0}]|}) (member "scenarios" payload);
  Alcotest.(check string) "report matches stdout" out (read result);
  let args = List.mapi (fun i x -> if i = 7 then "42" else if i = 11 then "failure output\nwith details" else x) args in
  let out = check_run ~code:1 report ("body-report" :: args) in
  Alcotest.(check string) "failed report matches stdout" out (read result);
  let scenarios = Yojson.Safe.Util.to_list (member "scenarios" (json out)) in
  check_json "captured error" (json {|{"scenario":"accepted_large_body_chunked","errors":["failure output\nwith details"],"exit_code":42,"failed":true}|}) (List.nth scenarios 1))
let () = Alcotest.run "bench report" ["report", List.map (fun (n,f) -> Alcotest.test_case n `Quick f) ["arguments",test_args;"body files",test_body;"baseline",test_baseline;"body report",test_body_report]]
