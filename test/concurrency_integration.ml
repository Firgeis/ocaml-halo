let host = Unix.inet_addr_loopback
let port = 8081
let fail fmt = Printf.ksprintf (fun message -> Alcotest.fail message) fmt

let rec write_all fd text offset length =
  if length > 0 then
    let written = Unix.single_write_substring fd text offset length in
    write_all fd text (offset + written) (length - written)

let contains needle haystack =
  let needle_length = String.length needle in
  let rec search index =
    if index + needle_length > String.length haystack then false
    else if String.sub haystack index needle_length = needle then true
    else search (index + 1)
  in
  search 0

let read_response fd =
  let buffer = Buffer.create 256 in
  let chunk = Bytes.create 4096 in
  let rec loop () =
    let current = Buffer.contents buffer in
    match String.index_opt current '\n' with
    | Some _ when contains "\r\n\r\n" current -> (
        let marker = "\r\n\r\n" in
        let rec marker_at index =
          if index + String.length marker > String.length current then None
          else if String.sub current index (String.length marker) = marker then
            Some index
          else marker_at (index + 1)
        in
        match marker_at 0 with
        | None -> read_more ()
        | Some header_end ->
            let headers = String.sub current 0 header_end in
            let prefix = "Content-Length: " in
            let length =
              headers |> String.split_on_char '\n'
              |> List.find_map (fun line ->
                  let line = String.trim line in
                  if String.starts_with ~prefix line then
                    int_of_string_opt
                      (String.sub line (String.length prefix)
                         (String.length line - String.length prefix))
                  else None)
              |> Option.value ~default:0
            in
            let total = header_end + String.length marker + length in
            if String.length current >= total then String.sub current 0 total
            else read_more ())
    | _ -> read_more ()
  and read_more () =
    match Unix.read fd chunk 0 (Bytes.length chunk) with
    | 0 -> Buffer.contents buffer
    | count ->
        Buffer.add_subbytes buffer chunk 0 count;
        loop ()
  in
  loop ()

let connect_to port =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect fd (Unix.ADDR_INET (host, port));
  fd

let connect () = connect_to port

let request_at port text =
  let fd = connect_to port in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      write_all fd text 0 (String.length text);
      read_response fd)

let request text = request_at port text

let server_path () =
  match Sys.getenv_opt "HALO_CONCURRENCY_SERVER_BIN" with
  | Some path ->
      if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
      else path
  | None -> fail "HALO_CONCURRENCY_SERVER_BIN is not set"

let wait_until_listening port =
  let deadline = Unix.gettimeofday () +. 5.0 in
  let rec loop () =
    try Unix.close (connect_to port)
    with Unix.Unix_error ((Unix.ECONNREFUSED | Unix.EINTR), _, _) ->
      if Unix.gettimeofday () >= deadline then
        fail "server did not start";
      Unix.sleepf 0.02;
      loop ()
  in
  loop ()

let wait_for_exit pid =
  let deadline = Unix.gettimeofday () +. 3.0 in
  let rec loop () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () >= deadline then
          fail "server did not stop";
        Unix.sleepf 0.02;
        loop ()
    | _, status -> status
  in
  loop ()

let with_server_process ?(environment = Unix.environment ()) path ports fn =
  let log_path = Filename.temp_file "halo-eio" ".log" in
  let log = Unix.openfile log_path [ Unix.O_RDWR; Unix.O_TRUNC ] 0o600 in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  Fun.protect
    ~finally:(fun () ->
      Unix.close log;
      Unix.close devnull;
      Sys.remove log_path)
    (fun () ->
      let child_log =
        if Sys.getenv_opt "HALO_BRIDGE_TRACE" = Some "1" then Unix.stderr
        else log
      in
      let pid =
        Unix.create_process_env path [| path |] environment devnull child_log
          child_log
      in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.kill pid Sys.sigint
           with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
          match wait_for_exit pid with
          | Unix.WEXITED 0 -> ()
          | status ->
              let output =
                In_channel.with_open_bin log_path In_channel.input_all
              in
              fail "server exit %s\n%s"
                (match status with
                | Unix.WEXITED code -> Printf.sprintf "code %d" code
                | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
                | Unix.WSTOPPED signal -> Printf.sprintf "stopped %d" signal)
                output)
        (fun () ->
          List.iter wait_until_listening ports;
          fn ()))

let with_server ?max_in_flight_requests fn =
  let environment =
    match max_in_flight_requests with
    | None -> Unix.environment ()
    | Some capacity ->
        Array.append (Unix.environment ())
          [| Printf.sprintf "HALO_MAX_IN_FLIGHT_REQUESTS=%d" capacity |]
  in
  with_server_process ~environment (server_path ()) [ port ] fn

let check_response ?(version = "HTTP/1.1") expected response =
  if not (String.starts_with ~prefix:(version ^ " 200") response) then
    fail "expected 200 response, received %S" response;
  if not (String.ends_with ~suffix:expected response) then
    fail "expected response body %S, received %S" expected response

let test_independent_runtimes () =
  let path =
    match Sys.getenv_opt "HALO_MULTI_SERVER_BIN" with
    | Some path when Filename.is_relative path ->
        Filename.concat (Sys.getcwd ()) path
    | Some path -> path
    | None -> fail "HALO_MULTI_SERVER_BIN is not set"
  in
  with_server_process path [ 8081; 8082 ] (fun () ->
      check_response "first\n"
        (request_at 8081
           "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n");
      check_response "second\n"
        (request_at 8082
           "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"))

let test_fixed_response () =
  check_response "hello\n"
    (request "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")

let test_request_body () =
  check_response "bridge body"
    (request
       "POST /echo HTTP/1.1\r\n\
        Host: localhost\r\n\
        Content-Length: 11\r\n\
        Connection: close\r\n\
        \r\n\
        bridge body")

let test_streamed_request_body () =
  check_response "16384\n"
    (request
       ("POST /stream-length HTTP/1.1\r\n\
         Host: localhost\r\n\
         Content-Length: 16384\r\n\
         Connection: close\r\n\
         \r\n" ^ String.make 16384 'x'))

let test_large_incremental_request_body () =
  let body = String.make (1024 * 1024) 'x' in
  let request_text =
    Printf.sprintf
      "POST /stream-length HTTP/1.1\r\nHost: localhost\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
      (String.length body) body
  in
  check_response "1048576\n" (request request_text)

let test_request_child_fiber () =
  check_response "child\n"
    (request
       "GET /child HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")

let test_keep_alive_reuses_connection () =
  let fd = connect () in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let first = "GET /fast HTTP/1.1\r\nHost: localhost\r\n\r\n" in
      write_all fd first 0 (String.length first);
      check_response "fast\n" (read_response fd);
      let second =
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      write_all fd second 0 (String.length second);
      check_response "hello\n" (read_response fd))

let test_long_keep_alive_connection () =
  let fd = connect () in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      for index = 1 to 100 do
        let request =
          if index = 100 then
            "GET /fast HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          else "GET /fast HTTP/1.1\r\nHost: localhost\r\n\r\n"
        in
        write_all fd request 0 (String.length request);
        check_response "fast\n" (read_response fd)
      done)

let test_http_1_0_keep_alive_connection () =
  let fd = connect () in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let first = "GET /fast HTTP/1.0\r\nConnection: keep-alive\r\n\r\n" in
      write_all fd first 0 (String.length first);
      let response = read_response fd in
      check_response ~version:"HTTP/1.0" "fast\n" response;
      Alcotest.(check bool)
        "keep-alive response header" true
        (contains "Connection: keep-alive\r\n" response);
      let second = "GET /fast HTTP/1.0\r\nConnection: close\r\n\r\n" in
      write_all fd second 0 (String.length second);
      check_response ~version:"HTTP/1.0" "fast\n" (read_response fd))

let test_disconnect_cancels_body_reader () =
  let fd = connect () in
  let partial =
    "POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 100\r\n\r\n"
  in
  write_all fd partial 0 (String.length partial);
  Unix.sleepf 0.03;
  Unix.close fd;
  Unix.sleepf 0.03;
  check_response "fast\n"
    (request
       "GET /fast HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n")

let test_slow_handler_does_not_block_reactor () =
  let slow = connect () in
  Fun.protect
    ~finally:(fun () -> Unix.close slow)
    (fun () ->
      let slow_request =
        "GET /slow HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      write_all slow slow_request 0 (String.length slow_request);
      Unix.sleepf 0.03;
      let started = Unix.gettimeofday () in
      let fast =
        request
          "GET /fast HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      let elapsed = Unix.gettimeofday () -. started in
      check_response "fast\n" fast;
      Alcotest.(check bool)
        "fast request completed while slow fiber slept" true (elapsed < 0.20);
      check_response "slow\n" (read_response slow))

let test_in_flight_admission_bound () =
  let slow = connect () in
  Fun.protect
    ~finally:(fun () -> Unix.close slow)
    (fun () ->
      let slow_request =
        "GET /slow HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      write_all slow slow_request 0 (String.length slow_request);
      Unix.sleepf 0.03;
      let rejected =
        request
          "GET /fast HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      Alcotest.(check bool)
        "excess request rejected" true
        (String.starts_with ~prefix:"HTTP/1.1 503" rejected);
      check_response "slow\n" (read_response slow))

let () =
  Alcotest.run "reactor-runtime"
    [
      ( "integration",
        [
          Alcotest.test_case "fixed response" `Quick (fun () ->
              with_server test_fixed_response);
          Alcotest.test_case "request body" `Quick (fun () ->
              with_server test_request_body);
          Alcotest.test_case "streamed request body" `Quick (fun () ->
              with_server test_streamed_request_body);
          Alcotest.test_case "large incremental request body" `Quick (fun () ->
              with_server test_large_incremental_request_body);
          Alcotest.test_case "request child fiber" `Quick (fun () ->
              with_server test_request_child_fiber);
          Alcotest.test_case "keep-alive request IDs" `Quick (fun () ->
              with_server test_keep_alive_reuses_connection);
          Alcotest.test_case "long keep-alive connection" `Quick (fun () ->
              with_server test_long_keep_alive_connection);
          Alcotest.test_case "HTTP/1.0 keep-alive connection" `Quick (fun () ->
              with_server test_http_1_0_keep_alive_connection);
          Alcotest.test_case "disconnect cancels body reader" `Quick (fun () ->
              with_server test_disconnect_cancels_body_reader);
          Alcotest.test_case "sleeping handler does not block reactor" `Quick
            (fun () -> with_server test_slow_handler_does_not_block_reactor);
          Alcotest.test_case "in-flight admission is bounded" `Quick (fun () ->
              with_server ~max_in_flight_requests:1
                test_in_flight_admission_bound);
          Alcotest.test_case "independent runtimes" `Quick
            test_independent_runtimes;
        ] );
    ]
