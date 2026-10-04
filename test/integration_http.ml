let host = Unix.inet_addr_loopback
let port = 8080
let fail fmt = Printf.ksprintf (fun message -> Alcotest.fail message) fmt

exception Test_timeout of string

let float_env_or_default name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some value -> Option.value (float_of_string_opt value) ~default

let test_timeout_seconds = float_env_or_default "HALO_TEST_TIMEOUT_SECONDS" 10.0

let server_shutdown_timeout_seconds =
  float_env_or_default "HALO_SERVER_SHUTDOWN_TIMEOUT_SECONDS" 5.0

let with_test_timeout name f =
  let previous_signal =
    Sys.signal Sys.sigalrm
      (Sys.Signal_handle (fun _ -> raise (Test_timeout name)))
  in
  let previous_timer =
    Unix.setitimer Unix.ITIMER_REAL
      { Unix.it_interval = 0.0; it_value = test_timeout_seconds }
  in
  Fun.protect
    ~finally:(fun () ->
      ignore (Unix.setitimer Unix.ITIMER_REAL previous_timer);
      ignore (Sys.signal Sys.sigalrm previous_signal))
    (fun () ->
      try f ()
      with Test_timeout test_name ->
        fail "test timed out after %.1fs: %s" test_timeout_seconds test_name)

let timed_test_case name speed f =
  Alcotest.test_case name speed (fun () -> with_test_timeout name f)

let rec write_all fd buffer offset length =
  if length > 0 then
    let written = Unix.single_write_substring fd buffer offset length in
    write_all fd buffer (offset + written) (length - written)

let write_repeated_char fd total_length ch =
  let chunk = String.make 16384 ch in
  let chunk_length = String.length chunk in
  let rec loop remaining =
    if remaining > 0 then (
      let length = min remaining chunk_length in
      write_all fd chunk 0 length;
      loop (remaining - length))
  in
  loop total_length

let contains needle haystack =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search index =
    if index + needle_length > haystack_length then false
    else if String.sub haystack index needle_length = needle then true
    else search (index + 1)
  in
  search 0

let find_header_body_separator response =
  let marker = "\r\n\r\n" in
  let marker_length = String.length marker in
  let response_length = String.length response in
  let rec search index =
    if index + marker_length > response_length then None
    else if String.sub response index marker_length = marker then Some index
    else search (index + 1)
  in
  search 0

let split_headers_body response =
  let marker_length = String.length "\r\n\r\n" in
  match find_header_body_separator response with
  | None -> None
  | Some index ->
      Some
        ( String.sub response 0 index,
          String.sub response (index + marker_length)
            (String.length response - index - marker_length) )

let status_code response =
  match String.split_on_char ' ' response with
  | _version :: status :: _rest -> int_of_string_opt status
  | _ -> None

let content_length headers =
  let lines = String.split_on_char '\n' headers in
  let prefix = "content-length:" in
  let prefix_length = String.length prefix in
  let parse_content_length line =
    let trimmed = String.trim line in
    let lower = String.lowercase_ascii trimmed in
    if String.length lower < prefix_length then None
    else if String.sub lower 0 prefix_length <> prefix then None
    else
      let value =
        String.sub trimmed prefix_length (String.length trimmed - prefix_length)
        |> String.trim
      in
      int_of_string_opt value
  in
  List.find_map parse_content_length lines

let read_into_buffer fd buffer =
  let chunk = Bytes.create 4096 in
  match Unix.read fd chunk 0 (Bytes.length chunk) with
  | 0 -> false
  | count ->
      Buffer.add_subbytes buffer chunk 0 count;
      true

let read_until_contains ?(timeout = 1.0) fd buffer needle =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if contains needle (Buffer.contents buffer) then Buffer.contents buffer
    else if Unix.gettimeofday () >= deadline then
      fail "timed out waiting for streamed response data: %s" needle
    else
      match Unix.select [ fd ] [] [] 0.05 with
      | [], _, _ -> loop ()
      | _ ->
          if read_into_buffer fd buffer then loop ()
          else
            fail "connection closed before streamed response contained: %s"
              needle
  in
  loop ()

let read_until_eof ?(timeout = 1.0) fd buffer =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if Unix.gettimeofday () >= deadline then
      fail "timed out waiting for the server to close the connection"
    else
      match Unix.select [ fd ] [] [] 0.05 with
      | [], _, _ -> loop ()
      | _ ->
          if read_into_buffer fd buffer then loop () else Buffer.contents buffer
  in
  loop ()

let rec read_response ?(body_expected = true) fd =
  let leftover = ref "" in
  let response = read_response_with_leftovers ~body_expected fd leftover in
  response

and read_response_with_leftovers ?(body_expected = true) fd leftover =
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer !leftover;
  leftover := "";
  let rec read_headers () =
    match find_header_body_separator (Buffer.contents buffer) with
    | Some index -> index
    | None ->
        if read_into_buffer fd buffer then read_headers ()
        else fail "connection closed before response headers were complete"
  in
  let header_end = read_headers () in
  let marker_length = String.length "\r\n\r\n" in
  let response = Buffer.contents buffer in
  let headers = String.sub response 0 header_end in
  let body_start = header_end + marker_length in
  let initial_body_length = String.length response - body_start in
  let body_length =
    match content_length headers with
    | Some _length when not body_expected -> 0
    | Some length -> length
    | None -> (
        match status_code response with
        | Some status
          when (status >= 100 && status < 200) || status = 204 || status = 304
          ->
            0
        | _ -> fail "response did not include Content-Length")
  in
  let rec read_body () =
    let current = Buffer.contents buffer in
    let current_body_length = String.length current - body_start in
    if current_body_length >= body_length then (
      let response_length = body_start + body_length in
      let remaining_length = String.length current - response_length in
      let response = String.sub current 0 response_length in
      if remaining_length > 0 then
        leftover := String.sub current response_length remaining_length;
      response)
    else if read_into_buffer fd buffer then read_body ()
    else fail "connection closed before response body was complete"
  in
  if initial_body_length >= body_length then (
    let response_length = body_start + body_length in
    let remaining_length = String.length response - response_length in
    if remaining_length > 0 then
      leftover := String.sub response response_length remaining_length;
    String.sub response 0 response_length)
  else read_body ()

let connect_once () =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.connect fd (Unix.ADDR_INET (host, port));
      true)

let wait_until_listening ~timeout =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    try
      ignore (connect_once ());
      ()
    with
    | Unix.Unix_error ((Unix.ECONNREFUSED | Unix.ENOENT), _, _)
    | Unix.Unix_error (Unix.EINTR, _, _)
    ->
      if Unix.gettimeofday () >= deadline then
        fail "server did not start listening on 127.0.0.1:%d" port;
      Unix.sleepf 0.05;
      loop ()
  in
  loop ()

let wait_until_not_listening ~timeout =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    try
      ignore (connect_once ());
      if Unix.gettimeofday () >= deadline then
        fail "server kept accepting connections on 127.0.0.1:%d" port;
      Unix.sleepf 0.01;
      loop ()
    with
    | Unix.Unix_error ((Unix.ECONNREFUSED | Unix.ECONNRESET | Unix.ENOENT), _, _)
      ->
        ()
    | Unix.Unix_error (Unix.EINTR, _, _) -> loop ()
  in
  loop ()

let request_response ?(body_expected = true) request =
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      Unix.connect fd (Unix.ADDR_INET (host, port));
      write_all fd request 0 (String.length request);
      read_response ~body_expected fd)

let wait_for_exit_status pid ~timeout =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    match Unix.waitpid [ Unix.WNOHANG ] pid with
    | 0, _ ->
        if Unix.gettimeofday () >= deadline then None
        else (
          Unix.sleepf 0.05;
          loop ())
    | _, status -> Some status
  in
  loop ()

let wait_for_exit pid ~timeout =
  match wait_for_exit_status pid ~timeout with
  | Some status -> status
  | None -> fail "server process %d did not exit after SIGINT" pid

let kill_process pid signal =
  try Unix.kill pid signal with Unix.Unix_error (Unix.ESRCH, _, _) -> ()

let rec reap_process pid =
  try ignore (Unix.waitpid [] pid) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> reap_process pid
  | Unix.Unix_error (Unix.ECHILD, _, _) -> ()

let stop_server pid log_path =
  kill_process pid Sys.sigint;
  match wait_for_exit_status pid ~timeout:server_shutdown_timeout_seconds with
  | Some status -> status
  | None ->
      kill_process pid Sys.sigkill;
      reap_process pid;
      let log = In_channel.with_open_bin log_path In_channel.input_all in
      fail
        "server process %d did not exit %.1fs after SIGINT and was killed\n%s"
        pid server_shutdown_timeout_seconds log

let server_path () =
  match Sys.getenv_opt "HALO_SERVER_BIN" with
  | Some path ->
      if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
      else path
  | None -> fail "expected HALO_SERVER_BIN in the test environment"

let tls_cert_fixture_path () =
  match Sys.getenv_opt "HALO_TLS_CERT_FIXTURE" with
  | Some path -> path
  | None -> fail "expected HALO_TLS_CERT_FIXTURE in the test environment"

let tls_key_fixture_path () =
  match Sys.getenv_opt "HALO_TLS_KEY_FIXTURE" with
  | Some path -> path
  | None -> fail "expected HALO_TLS_KEY_FIXTURE in the test environment"

let tls_ticket_keys_malformed_fixture_path () =
  match Sys.getenv_opt "HALO_TLS_TICKET_KEYS_MALFORMED_FIXTURE" with
  | Some path -> path
  | None ->
      fail
        "expected HALO_TLS_TICKET_KEYS_MALFORMED_FIXTURE in the test \
         environment"

let build_env overrides =
  let override_keys = List.map fst overrides in
  let is_override entry =
    match String.index_opt entry '=' with
    | None -> false
    | Some index ->
        let key = String.sub entry 0 index in
        List.mem key override_keys
  in
  Array.of_list
    (List.map (fun (key, value) -> key ^ "=" ^ value) overrides
    @ List.filter
        (fun entry -> not (is_override entry))
        (Array.to_list (Unix.environment ())))

let assert_socket_closed fd =
  match Unix.select [ fd ] [] [] 0.5 with
  | [], _, _ -> fail "expected server to close the connection"
  | _ -> (
      let probe = Bytes.create 1 in
      match Unix.read fd probe 0 1 with
      | 0 -> ()
      | _ -> fail "expected EOF after response")

let assert_not_plaintext_http_or_closed fd =
  match Unix.select [ fd ] [] [] 0.5 with
  | [], _, _ -> fail "expected tls listener to respond or close"
  | _ -> (
      let probe = Bytes.create 256 in
      try
        match Unix.read fd probe 0 (Bytes.length probe) with
        | 0 -> ()
        | count ->
            let payload = Bytes.sub_string probe 0 count in
            Alcotest.(check bool)
              "tls listener should not emit plaintext HTTP" false
              (String.starts_with ~prefix:"HTTP/" payload)
      with Unix.Unix_error (Unix.ECONNRESET, _, _) -> ())

let assert_response ?connection ?content_length ~status ~body response =
  let headers, actual_body =
    match split_headers_body response with
    | Some parts -> parts
    | None -> fail "response did not contain a header/body separator"
  in
  (if not (String.starts_with ~prefix:status response) then
     let actual =
       match String.index_opt response '\n' with
       | Some index -> String.sub response 0 index |> String.trim
       | None -> response
     in
     fail "expected status line %S, received %S" status actual);
  (match content_length with
  | None -> ()
  | Some expected ->
      Alcotest.(check bool)
        "content length" true
        (contains (Printf.sprintf "Content-Length: %d" expected) response));
  (match connection with
  | None -> ()
  | Some expected ->
      Alcotest.(check bool)
        "connection header" true
        (contains (Printf.sprintf "Connection: %s" expected) headers));
  Alcotest.(check string) "body" body actual_body

let with_server ?(env = []) f =
  let log_path = Filename.temp_file "ocaml-halo-server" ".log" in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let log_fd =
    Unix.openfile log_path [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_RDWR ] 0o600
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close devnull;
      Unix.close log_fd;
      Sys.remove log_path)
    (fun () ->
      let pid =
        let path = server_path () in
        Unix.create_process_env path [| path |] (build_env env) devnull log_fd
          log_fd
      in
      Fun.protect
        ~finally:(fun () ->
          match stop_server pid log_path with
          | Unix.WEXITED 0 -> ()
          | Unix.WSIGNALED signal when signal = Sys.sigint -> ()
          | Unix.WEXITED code ->
              let log =
                In_channel.with_open_bin log_path In_channel.input_all
              in
              fail "server exited with code %d\n%s" code log
          | Unix.WSIGNALED signal ->
              let log =
                In_channel.with_open_bin log_path In_channel.input_all
              in
              fail "server terminated by signal %d\n%s" signal log
          | Unix.WSTOPPED signal ->
              let log =
                In_channel.with_open_bin log_path In_channel.input_all
              in
              fail "server stopped by signal %d\n%s" signal log)
        (fun () ->
          wait_until_listening ~timeout:2.0;
          f ()))

let with_server_start_failure ?(env = []) f =
  let log_path = Filename.temp_file "ocaml-halo-server-startup" ".log" in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let log_fd =
    Unix.openfile log_path [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_RDWR ] 0o600
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close devnull;
      Unix.close log_fd;
      Sys.remove log_path)
    (fun () ->
      let path = server_path () in
      let pid =
        Unix.create_process_env path [| path |] (build_env env) devnull log_fd
          log_fd
      in
      let status = wait_for_exit pid ~timeout:2.0 in
      let log = In_channel.with_open_bin log_path In_channel.input_all in
      f status log)

let tls_server_env () =
  [
    ("HALO_TLS_CERT_CHAIN", tls_cert_fixture_path ());
    ("HALO_TLS_PRIVATE_KEY", tls_key_fixture_path ());
  ]

let tls_request_response request =
  let command =
    Printf.sprintf
      "openssl s_client -connect 127.0.0.1:%d -servername localhost -quiet \
       -CAfile %s"
      port
      (Filename.quote (tls_cert_fixture_path ()))
  in
  let input_channel, output_channel, error_channel =
    Unix.open_process_full command (Unix.environment ())
  in
  Fun.protect
    ~finally:(fun () ->
      close_in_noerr input_channel;
      close_out_noerr output_channel;
      close_in_noerr error_channel)
    (fun () ->
      output_string output_channel request;
      flush output_channel;
      close_out output_channel;
      let response = In_channel.input_all input_channel in
      let errors = In_channel.input_all error_channel in
      match
        Unix.close_process_full (input_channel, output_channel, error_channel)
      with
      | Unix.WEXITED 0 -> response
      | Unix.WEXITED code ->
          fail "openssl exited with code %d\nstdout:\n%s\nstderr:\n%s" code
            response errors
      | Unix.WSIGNALED signal ->
          fail "openssl terminated by signal %d\nstdout:\n%s\nstderr:\n%s"
            signal response errors
      | Unix.WSTOPPED signal ->
          fail "openssl stopped by signal %d\nstdout:\n%s\nstderr:\n%s" signal
            response errors)

let short_timeout_env =
  [ ("HALO_REQUEST_TIMEOUT_MS", "100"); ("HALO_IDLE_TIMEOUT_MS", "100") ]

let with_server_capture_log ?(env = []) path f =
  let log_path = Filename.temp_file "ocaml-halo-server" ".log" in
  let devnull = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
  let log_fd =
    Unix.openfile log_path [ Unix.O_CREAT; Unix.O_TRUNC; Unix.O_RDWR ] 0o600
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.close devnull;
      Unix.close log_fd;
      Sys.remove log_path)
    (fun () ->
      let pid =
        Unix.create_process_env path [| path |] (build_env env) devnull log_fd
          log_fd
      in
      Fun.protect
        ~finally:(fun () ->
          (try Unix.kill pid Sys.sigint
           with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
          try ignore (wait_for_exit pid ~timeout:2.0)
          with Unix.Unix_error (Unix.ECHILD, _, _) -> ())
        (fun () ->
          wait_until_listening ~timeout:2.0;
          f pid log_path))

let test_fixed_response () =
  with_server (fun () ->
      let response =
        request_response
          "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 200 OK" ~body:"hello world\n" response)

let test_query_parameters_are_percent_decoded () =
  with_server (fun () ->
      let response =
        request_response
          "GET /query?q=a%26b%3Dc%25%2F&plus=a+b HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:11
        ~status:"HTTP/1.1 200 OK" ~body:"a&b=c%/|a+b" response)

let test_invalid_uri_encoding_returns_bad_request () =
  with_server (fun () ->
      let response =
        request_response
          "GET /?q=%GG HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_forbidden_reason_phrase () =
  with_server (fun () ->
      let response =
        request_response
          "GET /forbidden HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:10
        ~status:"HTTP/1.1 403 Forbidden" ~body:"forbidden\n" response)

let test_tls_https_response () =
  with_server ~env:(tls_server_env ()) (fun () ->
      let response =
        tls_request_response
          "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 200 OK" ~body:"hello world\n" response)

let test_tls_large_request_body () =
  with_server ~env:(tls_server_env ()) (fun () ->
      let body = String.make (128 * 1024) 't' in
      let request =
        Printf.sprintf
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Content-Length: %d\r\n\
           Connection: close\r\n\
           \r\n\
           %s"
          (String.length body) body
      in
      let response = tls_request_response request in
      let response_body = "submitted:" ^ body in
      assert_response ~connection:"close"
        ~content_length:(String.length response_body)
        ~status:"HTTP/1.1 200 OK" ~body:response_body response)

let test_tls_rejects_plaintext_http () =
  with_server
    ~env:(tls_server_env () @ short_timeout_env)
    (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          let request =
            "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          in
          Unix.connect fd (Unix.ADDR_INET (host, port));
          write_all fd request 0 (String.length request);
          Unix.shutdown fd Unix.SHUTDOWN_SEND;
          assert_not_plaintext_http_or_closed fd))

let test_tls_rejects_malformed_ticket_key_file_on_startup () =
  with_server_start_failure
    ~env:
      (tls_server_env ()
      @ [
          ("HALO_TLS_SESSION_TICKETS", "true");
          ( "HALO_TLS_SESSION_TICKET_KEYS_FILE",
            tls_ticket_keys_malformed_fixture_path () );
        ])
    (fun status log ->
      (match status with
      | Unix.WEXITED code ->
          Alcotest.(check bool) "startup should fail" true (code <> 0)
      | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
          Alcotest.fail "server did not exit cleanly after startup failure");
      Alcotest.(check bool)
        "startup log mentions ticket key parse failure" true
        (contains "invalid tls session ticket key name" log))

let test_rejects_absolute_target () =
  with_server (fun () ->
      let response =
        request_response
          "GET http://example.com/ HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_accepts_body_request () =
  with_server (fun () ->
      let response =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Content-Length: 5\r\n\
           Connection: close\r\n\
           \r\n\
           hello"
      in
      assert_response ~connection:"close" ~content_length:15
        ~status:"HTTP/1.1 200 OK" ~body:"submitted:hello" response)

let test_head_suppresses_response_body () =
  with_server (fun () ->
      let response =
        request_response ~body_expected:false
          "HEAD / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 200 OK" ~body:"" response;
      Alcotest.(check bool)
        "head body omitted" false
        (contains "\r\n\r\nhello world\n" response))

let test_head_uses_get_route_and_suppresses_body () =
  with_server (fun () ->
      let response =
        request_response ~body_expected:false
          "HEAD /html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:21
        ~status:"HTTP/1.1 200 OK" ~body:"" response;
      let method_mismatch =
        request_response
          "POST /html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:19
        ~status:"HTTP/1.1 405 Method Not Allowed" ~body:"method not allowed\n"
        method_mismatch;
      Alcotest.(check bool)
        "GET route also allows HEAD" true
        (contains "Allow: GET, HEAD\r\n" method_mismatch))

let test_head_stream_route_omits_body () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "HEAD /stream/hold HTTP/1.1\r\n\
             Host: localhost\r\n\
             Connection: close\r\n\
             \r\n"
          in
          write_all fd request 0 (String.length request);
          let response = read_until_eof ~timeout:2.0 fd (Buffer.create 256) in
          assert_response ~connection:"close" ~status:"HTTP/1.1 200 OK" ~body:""
            response;
          Alcotest.(check bool)
            "no chunked framing" false
            (contains "Transfer-Encoding: chunked" response);
          Alcotest.(check bool)
            "unknown stream length omitted" false
            (contains "Content-Length:" response)))

let test_html_response () =
  with_server (fun () ->
      let response =
        request_response
          "GET /html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:21
        ~status:"HTTP/1.1 200 OK" ~body:"<h1>hello world</h1>\n" response;
      Alcotest.(check bool)
        "html content-type" true
        (contains "Content-Type: text/html; charset=utf-8" response))

let test_json_response () =
  with_server (fun () ->
      let response =
        request_response
          "GET /json HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:25
        ~status:"HTTP/1.1 200 OK" ~body:"{\"message\":\"hello world\"}" response;
      Alcotest.(check bool)
        "json content-type" true
        (contains "Content-Type: application/json; charset=utf-8" response))

let test_rejects_invalid_content_length () =
  with_server (fun () ->
      let response =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Content-Length: nope\r\n\
           Connection: close\r\n\
           \r\n\
           hello"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response;
      Alcotest.(check bool)
        "bad request uses text helper content-type" true
        (contains "Content-Type: text/plain; charset=utf-8" response))

let test_http11_requires_host_header () =
  with_server (fun () ->
      let response =
        request_response "GET / HTTP/1.1\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_http11_rejects_duplicate_host_headers () =
  with_server (fun () ->
      let response =
        request_response
          "GET / HTTP/1.1\r\n\
           Host: localhost\r\n\
           Host: example.com\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_http11_rejects_empty_host_header () =
  with_server (fun () ->
      let response =
        request_response "GET / HTTP/1.1\r\nHost:\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_http11_rejects_malformed_host_header () =
  with_server (fun () ->
      let response =
        request_response
          "GET / HTTP/1.1\r\n\
           Host: http://localhost\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_http11_accepts_bracketed_ipv6_host () =
  with_server (fun () ->
      let response =
        request_response
          "GET / HTTP/1.1\r\nHost: [::1]:8080\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 200 OK" ~body:"hello world\n" response)

let test_http10_allows_missing_host_header () =
  with_server (fun () ->
      let response =
        request_response "GET / HTTP/1.0\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.0 200 OK" ~body:"hello world\n" response)

let test_rejects_unsupported_http_version () =
  with_server (fun () ->
      let response =
        request_response
          "GET / HTTP/1.2\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:27
        ~status:"HTTP/1.1 505 HTTP Version Not Supported"
        ~body:"http version not supported\n" response)

let test_rejects_too_large_request_target () =
  with_server (fun () ->
      let target = "/" ^ String.make 9000 'a' in
      let request =
        Printf.sprintf
          "GET %s HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          target
      in
      let response = request_response request in
      assert_response ~connection:"close" ~content_length:13
        ~status:"HTTP/1.1 414 URI Too Long" ~body:"uri too long\n" response)

let test_rejects_too_large_request_body () =
  with_server
    ~env:[ ("HALO_MAX_BODY_SIZE", "8192") ]
    (fun () ->
      let body = String.make 9000 'a' in
      let request =
        Printf.sprintf
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Content-Length: %d\r\n\
           Connection: close\r\n\
           \r\n\
           %s"
          (String.length body) body
      in
      let response = request_response request in
      assert_response ~connection:"close" ~content_length:18
        ~status:"HTTP/1.1 413 Content Too Large" ~body:"content too large\n"
        response)

let test_rejects_streamed_too_large_request_body_without_reset () =
  with_server
    ~env:[ ("HALO_MAX_BODY_SIZE", "8192") ]
    (fun () ->
      let body_length = 1024 * 1024 in
      let request_headers =
        Printf.sprintf
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Content-Length: %d\r\n\
           Connection: close\r\n\
           \r\n"
          body_length
      in
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          write_all fd request_headers 0 (String.length request_headers);
          write_repeated_char fd body_length 'a';
          let response = read_response fd in
          assert_response ~connection:"close" ~content_length:18
            ~status:"HTTP/1.1 413 Content Too Large" ~body:"content too large\n"
            response;
          assert_socket_closed fd))

let test_rejects_too_large_request_header_fields () =
  with_server
    ~env:[ ("HALO_MAX_HEADER_SIZE", "8192") ]
    (fun () ->
      let oversized_value = String.make 9000 'a' in
      let request =
        Printf.sprintf
          "GET / HTTP/1.1\r\n\
           Host: localhost\r\n\
           X-Big: %s\r\n\
           Connection: close\r\n\
           \r\n"
          oversized_value
      in
      let response = request_response request in
      assert_response ~connection:"close" ~content_length:32
        ~status:"HTTP/1.1 431 Request Header Fields Too Large"
        ~body:"request header fields too large\n" response)

let test_request_line_limit_is_aggregate () =
  with_server
    ~env:[ ("HALO_MAX_REQUEST_LINE_SIZE", "4") ]
    (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request = "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
          write_all fd request 0 (String.length request);
          let accepted = read_response fd in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" accepted;
          write_all fd request 0 (String.length request);
          let accepted_again = read_response fd in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" accepted_again;
          let oversized = "GET /x HTTP/1.1\r\nHost: a\r\n\r\n" in
          write_all fd oversized 0 (String.length oversized);
          let rejected = read_response fd in
          assert_response ~connection:"close" ~content_length:13
            ~status:"HTTP/1.1 414 URI Too Long" ~body:"uri too long\n" rejected))

let test_header_limit_is_aggregate () =
  with_server
    ~env:[ ("HALO_MAX_HEADER_SIZE", "5") ]
    (fun () ->
      let accepted = request_response "GET / HTTP/1.1\r\nHost: a\r\n\r\n" in
      assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
        ~body:"hello world\n" accepted;
      let rejected =
        request_response "GET / HTTP/1.1\r\nHost: a\r\nX:\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:32
        ~status:"HTTP/1.1 431 Request Header Fields Too Large"
        ~body:"request header fields too large\n" rejected)

let test_body_limit_is_independent () =
  with_server
    ~env:[ ("HALO_MAX_BODY_SIZE", "1") ]
    (fun () ->
      let accepted =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: a\r\n\
           Content-Length: 1\r\n\
           Connection: close\r\n\
           \r\n\
           a"
      in
      assert_response ~connection:"close" ~content_length:11
        ~status:"HTTP/1.1 200 OK" ~body:"submitted:a" accepted;
      let rejected =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: a\r\n\
           Content-Length: 2\r\n\
           Connection: close\r\n\
           \r\n\
           aa"
      in
      assert_response ~connection:"close" ~content_length:18
        ~status:"HTTP/1.1 413 Content Too Large" ~body:"content too large\n"
        rejected)

let test_post_without_defined_length_returns_411 () =
  with_server (fun () ->
      let response =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:16
        ~status:"HTTP/1.1 411 Length Required" ~body:"length required\n"
        response)

let test_accepts_chunked_request_body () =
  with_server (fun () ->
      let response =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Transfer-Encoding: chunked\r\n\
           Connection: close\r\n\
           \r\n\
           5\r\n\
           hello\r\n\
           0\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:15
        ~status:"HTTP/1.1 200 OK" ~body:"submitted:hello" response)

let test_rejects_transfer_encoding_with_content_length () =
  with_server (fun () ->
      let response =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Transfer-Encoding: chunked\r\n\
           Content-Length: 5\r\n\
           Connection: close\r\n\
           \r\n\
           5\r\n\
           hello\r\n\
           0\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_rejects_duplicate_content_length_headers () =
  with_server (fun () ->
      let response =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Content-Length: 5\r\n\
           Content-Length: 5\r\n\
           Connection: close\r\n\
           \r\n\
           hello"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_rejects_chunked_request_trailers () =
  with_server (fun () ->
      let response =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Transfer-Encoding: chunked\r\n\
           Trailer: X-Trailer\r\n\
           Connection: close\r\n\
           \r\n\
           5\r\n\
           hello\r\n\
           0\r\n\
           X-Trailer: yes\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_custom_response_headers () =
  with_server (fun () ->
      let response =
        request_response
          "GET /headers HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:8
        ~status:"HTTP/1.1 200 OK" ~body:"headers\n" response;
      Alcotest.(check bool)
        "custom content-type" true
        (contains "Content-Type: text/custom" response);
      Alcotest.(check bool) "custom x-test" true (contains "X-Test: 1" response))

let test_request_headers_strip_hop_by_hop_fields () =
  with_server (fun () ->
      let response =
        request_response
          "GET /inspect/headers HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close, Foo\r\n\
           X-Test: 1\r\n\
           Keep-Alive: timeout=5\r\n\
           Foo: bar\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:11
        ~status:"HTTP/1.1 200 OK" ~body:"Host,X-Test" response)

let test_request_headers_combine_connection_options_across_lines () =
  with_server (fun () ->
      let response =
        request_response
          "GET /inspect/headers HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close , Foo\r\n\
           Connection:\tBar, foo , \tBaz\r\n\
           X-Test: 1\r\n\
           fOo: hidden\r\n\
           BAR: hidden\r\n\
           Baz: hidden\r\n\
           Keep-Alive: timeout=5\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:11
        ~status:"HTTP/1.1 200 OK" ~body:"Host,X-Test" response)

let test_request_headers_preserve_non_hop_by_hop_fields () =
  with_server (fun () ->
      let response =
        request_response
          "GET /inspect/headers HTTP/1.1\r\n\
           Host: localhost\r\n\
           Alpha: 1\r\n\
           Bravo: 2\r\n\
           Charlie: 3\r\n\
           Delta: 4\r\n\
           Echo: 5\r\n\
           Connection: close, Charlie\r\n\
           Charlie: hidden\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:27
        ~status:"HTTP/1.1 200 OK" ~body:"Host,Alpha,Bravo,Delta,Echo" response)

let test_rejects_invalid_connection_options () =
  with_server (fun () ->
      let response =
        request_response
          "GET / HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close, bad token\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_204_suppresses_response_body_and_content_length () =
  with_server (fun () ->
      let response =
        request_response
          "GET /empty HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~status:"HTTP/1.1 204 No Content"
        ~body:"" response;
      Alcotest.(check bool)
        "204 omits content length" false
        (contains "Content-Length:" response))

let test_304_suppresses_response_body_and_content_length () =
  with_server (fun () ->
      let response =
        request_response
          "GET /cached HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~status:"HTTP/1.1 304 Not Modified"
        ~body:"" response;
      Alcotest.(check bool)
        "304 omits content length" false
        (contains "Content-Length:" response))

let test_http11_keep_alive_reuses_connection () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" in
          write_all fd request 0 (String.length request);
          let response1 = read_response fd in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" response1;
          Alcotest.(check bool)
            "no connection close header on keep-alive" false
            (contains "Connection: close" response1);
          write_all fd request 0 (String.length request);
          let response2 = read_response fd in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" response2;
          Alcotest.(check bool)
            "second response omits connection close" false
            (contains "Connection: close" response2)))

let make_header_heavy_request path x_empty_value =
  let buffer = Buffer.create 4096 in
  Buffer.add_string buffer
    (Printf.sprintf "GET %s HTTP/1.1\r\nHost: localhost\r\n" path);
  Buffer.add_string buffer
    (match x_empty_value with
    | None -> ""
    | Some value -> Printf.sprintf "X-Empty:%s\r\n" value);
  for i = 0 to 63 do
    Buffer.add_string buffer
      (Printf.sprintf "X-Trace-%d: %s\r\n" i (String.make 64 'a'))
  done;
  Buffer.add_string buffer "\r\n";
  Buffer.contents buffer

let test_header_storage_keep_alive_reuses_capacity_and_resets_values () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let first_request =
            make_header_heavy_request "/inspect/header/x-empty" (Some "")
          in
          write_all fd first_request 0 (String.length first_request);
          let response1 = read_response fd in
          assert_response ~content_length:6 ~status:"HTTP/1.1 200 OK"
            ~body:"value:" response1;
          Alcotest.(check bool)
            "first response stays keep-alive" false
            (contains "Connection: close" response1);
          let second_request =
            make_header_heavy_request "/inspect/header/x-empty" None
          in
          write_all fd second_request 0 (String.length second_request);
          let response2 = read_response fd in
          assert_response ~content_length:7 ~status:"HTTP/1.1 200 OK"
            ~body:"missing" response2;
          Alcotest.(check bool)
            "second response stays keep-alive" false
            (contains "Connection: close" response2);
          let final_request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" in
          write_all fd final_request 0 (String.length final_request);
          let response3 = read_response fd in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" response3))

let test_http11_keep_alive_after_bodyless_response () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let first_request =
            "GET /empty HTTP/1.1\r\nHost: localhost\r\n\r\n"
          in
          write_all fd first_request 0 (String.length first_request);
          let response1 = read_response fd in
          assert_response ~status:"HTTP/1.1 204 No Content" ~body:"" response1;
          let second_request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" in
          write_all fd second_request 0 (String.length second_request);
          let response2 = read_response fd in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" response2))

let test_http11_keep_alive_reuses_connection_after_fixed_length_body () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let post_request =
            "POST /submit HTTP/1.1\r\n\
             Host: localhost\r\n\
             Content-Length: 5\r\n\
             \r\n\
             hello"
          in
          write_all fd post_request 0 (String.length post_request);
          let response1 = read_response fd in
          assert_response ~content_length:15 ~status:"HTTP/1.1 200 OK"
            ~body:"submitted:hello" response1;
          Alcotest.(check bool)
            "POST response omits close header" false
            (contains "Connection: close" response1);
          let get_request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" in
          write_all fd get_request 0 (String.length get_request);
          let response2 = read_response fd in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" response2))

let test_request_timeout_returns_408_for_partial_headers () =
  with_server ~env:short_timeout_env (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request = "GET / HTTP/1.1\r\nHost: loca" in
          write_all fd request 0 (String.length request);
          let response = read_response fd in
          assert_response ~connection:"close" ~content_length:16
            ~status:"HTTP/1.1 408 Request Timeout" ~body:"request timeout\n"
            response;
          assert_socket_closed fd))

let test_request_timeout_returns_408_for_partial_body () =
  with_server ~env:short_timeout_env (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "POST /submit HTTP/1.1\r\n\
             Host: localhost\r\n\
             Content-Length: 5\r\n\
             Connection: close\r\n\
             \r\n\
             he"
          in
          write_all fd request 0 (String.length request);
          let response = read_response fd in
          assert_response ~connection:"close" ~content_length:16
            ~status:"HTTP/1.1 408 Request Timeout" ~body:"request timeout\n"
            response;
          assert_socket_closed fd))

let test_keep_alive_idle_timeout_closes_connection () =
  with_server ~env:short_timeout_env (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" in
          write_all fd request 0 (String.length request);
          let response = read_response fd in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" response;
          Alcotest.(check bool)
            "keep-alive response omits close header" false
            (contains "Connection: close" response);
          assert_socket_closed fd))

let test_explicit_connection_close_closes_socket () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          in
          write_all fd request 0 (String.length request);
          let response = read_response fd in
          assert_response ~connection:"close" ~content_length:12
            ~status:"HTTP/1.1 200 OK" ~body:"hello world\n" response;
          assert_socket_closed fd))

let test_http10_keep_alive_header () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "GET / HTTP/1.0\r\n\
             Host: localhost\r\n\
             Connection: keep-alive\r\n\
             \r\n"
          in
          write_all fd request 0 (String.length request);
          let response1 = read_response fd in
          assert_response ~connection:"keep-alive" ~content_length:12
            ~status:"HTTP/1.0 200 OK" ~body:"hello world\n" response1;
          write_all fd request 0 (String.length request);
          let response2 = read_response fd in
          assert_response ~connection:"keep-alive" ~content_length:12
            ~status:"HTTP/1.0 200 OK" ~body:"hello world\n" response2))

let test_http10_streaming_route_returns_500 () =
  with_server (fun () ->
      let response =
        request_response
          "GET /stream HTTP/1.0\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:22
        ~status:"HTTP/1.0 500 Internal Server Error"
        ~body:"internal server error\n" response;
      Alcotest.(check bool)
        "streaming route omits transfer-encoding" false
        (contains "Transfer-Encoding: chunked" response))

let test_http11_streaming_route_streams_chunks_incrementally () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let buffer = Buffer.create 4096 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "GET /stream HTTP/1.1\r\n\
             Host: localhost\r\n\
             Connection: close\r\n\
             \r\n"
          in
          write_all fd request 0 (String.length request);
          let first = read_until_contains fd buffer "\r\n\r\n6\r\nhello \r\n" in
          Alcotest.(check bool)
            "status line" true
            (String.starts_with ~prefix:"HTTP/1.1 200 OK" first);
          Alcotest.(check bool)
            "chunked transfer" true
            (contains "Transfer-Encoding: chunked" first);
          Alcotest.(check bool)
            "no content length" false
            (contains "Content-Length:" first);
          Alcotest.(check bool)
            "second chunk not yet observed" false
            (contains "6\r\nworld\n\r\n" first);
          Alcotest.(check bool)
            "stream not finished yet" false
            (contains "\r\n0\r\n\r\n" first);
          let complete = read_until_contains fd buffer "6\r\nworld\n\r\n" in
          Alcotest.(check bool)
            "second chunk observed" true
            (contains "6\r\nworld\n\r\n" complete);
          let finished = read_until_contains fd buffer "\r\n0\r\n\r\n" in
          Alcotest.(check bool)
            "terminating chunk observed" true
            (contains "\r\n0\r\n\r\n" finished)))

let test_streaming_exception_after_start_aborts_stream () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let buffer = Buffer.create 4096 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "GET /stream/explode HTTP/1.1\r\n\
             Host: localhost\r\n\
             Connection: keep-alive\r\n\
             \r\n"
          in
          write_all fd request 0 (String.length request);
          let response = read_until_eof fd buffer in
          Alcotest.(check bool)
            "status line" true
            (String.starts_with ~prefix:"HTTP/1.1 200 OK" response);
          Alcotest.(check bool)
            "first chunk observed" true
            (contains "6\r\nhello \r\n" response);
          Alcotest.(check bool)
            "terminating chunk absent" false
            (contains "\r\n0\r\n\r\n" response);
          Alcotest.(check bool)
            "no 500 body emitted" false
            (contains "internal server error\n" response)))

let test_sse_route_streams_expected_headers_and_frames () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let buffer = Buffer.create 4096 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "GET /sse HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          in
          write_all fd request 0 (String.length request);
          let first =
            read_until_contains fd buffer
              "event: message\nid: 1\ndata: hello\ndata: world\n\n"
          in
          Alcotest.(check bool)
            "status line" true
            (String.starts_with ~prefix:"HTTP/1.1 200 OK" first);
          Alcotest.(check bool)
            "content type" true
            (contains "Content-Type: text/event-stream" first);
          Alcotest.(check bool)
            "cache control" true
            (contains "Cache-Control: no-cache" first);
          Alcotest.(check bool)
            "buffering header" true
            (contains "X-Accel-Buffering: no" first);
          Alcotest.(check bool)
            "chunked transfer" true
            (contains "Transfer-Encoding: chunked" first);
          Alcotest.(check bool)
            "no content length" false
            (contains "Content-Length:" first);
          Alcotest.(check bool)
            "later frame not yet observed" false
            (contains ": keep-alive\n\n" first);
          let with_comment = read_until_contains fd buffer ": keep-alive\n\n" in
          let complete =
            read_until_contains fd buffer "retry: 1500\ndata: done\n\n"
          in
          let finished = read_until_contains fd buffer "\r\n0\r\n\r\n" in
          Alcotest.(check bool)
            "event id encoded" true
            (contains "id: 1\n" complete);
          Alcotest.(check bool)
            "retry encoded" true
            (contains "retry: 1500\n" complete);
          Alcotest.(check bool)
            "comment encoded" true
            (contains ": keep-alive\n\n" with_comment);
          Alcotest.(check bool)
            "terminating chunk observed" true
            (contains "\r\n0\r\n\r\n" finished)))

let wait_for_file_contents path ~timeout =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    if Sys.file_exists path then
      In_channel.with_open_bin path In_channel.input_all
    else if Unix.gettimeofday () >= deadline then
      fail "timed out waiting for marker file: %s" path
    else (
      Unix.sleepf 0.05;
      loop ())
  in
  loop ()

let test_stream_disconnect_runs_cleanup () =
  let marker_path = Filename.temp_file "ocaml-halo-stream-disconnect" ".txt" in
  Sys.remove marker_path;
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists marker_path then Sys.remove marker_path)
    (fun () ->
      with_server
        ~env:[ ("HALO_STREAM_DISCONNECT_MARKER", marker_path) ]
        (fun () ->
          let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
          let buffer = Buffer.create 4096 in
          let fd_closed = ref false in
          Fun.protect
            ~finally:(fun () -> if not !fd_closed then Unix.close fd)
            (fun () ->
              Unix.connect fd (Unix.ADDR_INET (host, port));
              let request =
                "POST /stream/disconnect HTTP/1.1\r\n\
                 Host: localhost\r\n\
                 Transfer-Encoding: chunked\r\n\
                 \r\n"
              in
              write_all fd request 0 (String.length request);
              ignore (read_until_contains fd buffer "\r\n\r\n6\r\nhello \r\n");
              Unix.close fd;
              fd_closed := true;
              let marker = wait_for_file_contents marker_path ~timeout:1.0 in
              Alcotest.(check string)
                "disconnect cleanup marker" "client disconnected\n" marker)))

let test_expect_100_continue_before_request_body () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let headers =
            "POST /submit HTTP/1.1\r\n\
             Host: localhost\r\n\
             Content-Length: 5\r\n\
             Expect: 100-continue\r\n\
             Connection: close\r\n\
             \r\n"
          in
          write_all fd headers 0 (String.length headers);
          let interim = read_response fd in
          assert_response ~status:"HTTP/1.1 100 Continue" ~body:"" interim;
          Alcotest.(check bool)
            "100 omits content length" false
            (contains "Content-Length:" interim);
          write_all fd "hello" 0 5;
          let response = read_response fd in
          assert_response ~connection:"close" ~content_length:15
            ~status:"HTTP/1.1 200 OK" ~body:"submitted:hello" response))

let test_expect_100_continue_replays_coalesced_body_bytes () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "POST /submit HTTP/1.1\r\n\
             Host: localhost\r\n\
             Content-Length: 5\r\n\
             Expect: 100-continue\r\n\
             Connection: close\r\n\
             \r\n\
             hello"
          in
          write_all fd request 0 (String.length request);
          let interim = read_response fd in
          assert_response ~status:"HTTP/1.1 100 Continue" ~body:"" interim;
          let response = read_response fd in
          assert_response ~connection:"close" ~content_length:15
            ~status:"HTTP/1.1 200 OK" ~body:"submitted:hello" response))

let test_route_can_emit_informational_responses_before_final_response () =
  with_server (fun () ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let leftover = ref "" in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request = "GET /early HTTP/1.1\r\nHost: localhost\r\n\r\n" in
          write_all fd request 0 (String.length request);
          let response1 = read_response_with_leftovers fd leftover in
          assert_response ~status:"HTTP/1.1 102 Processing" ~body:"" response1;
          Alcotest.(check bool)
            "102 omits content length" false
            (contains "Content-Length:" response1);
          Alcotest.(check bool)
            "102 custom header" true
            (contains "X-Phase: processing" response1);
          let response2 = read_response_with_leftovers fd leftover in
          assert_response ~status:"HTTP/1.1 103 Early Hints" ~body:"" response2;
          Alcotest.(check bool)
            "103 omits content length" false
            (contains "Content-Length:" response2);
          Alcotest.(check bool)
            "103 link header" true
            (contains "Link: </style.css>; rel=preload; as=style" response2);
          let response3 = read_response_with_leftovers fd leftover in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" response3;
          let second_request = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n" in
          write_all fd second_request 0 (String.length second_request);
          let response4 = read_response_with_leftovers fd leftover in
          assert_response ~content_length:12 ~status:"HTTP/1.1 200 OK"
            ~body:"hello world\n" response4))

let check_shutdown_exit_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WSIGNALED signal when signal = Sys.sigint -> ()
  | Unix.WEXITED code -> fail "server exited with code %d during shutdown" code
  | Unix.WSIGNALED signal ->
      fail "server terminated by signal %d during shutdown" signal
  | Unix.WSTOPPED signal ->
      fail "server stopped by signal %d during shutdown" signal

let test_shutdown_closes_completed_half_closed_connection_promptly () =
  let path = server_path () in
  with_server_capture_log
    ~env:[ ("HALO_SHUTDOWN_TIMEOUT_MS", "2000") ]
    path
    (fun pid _log_path ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          in
          write_all fd request 0 (String.length request);
          let response = read_response fd in
          assert_response ~connection:"close" ~content_length:12
            ~status:"HTTP/1.1 200 OK" ~body:"hello world\n" response;
          assert_socket_closed fd;
          Unix.kill pid Sys.sigint;
          wait_until_not_listening ~timeout:0.25;
          wait_for_exit pid ~timeout:0.5 |> check_shutdown_exit_status))

let test_shutdown_closes_connection_after_upload_completes () =
  let path = server_path () in
  with_server_capture_log path (fun pid _log_path ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "POST /submit HTTP/1.1\r\n\
             Host: localhost\r\n\
             Content-Length: 5\r\n\
             Expect: 100-continue\r\n\
             Connection: keep-alive\r\n\
             \r\n"
          in
          write_all fd request 0 (String.length request);
          let interim = read_response fd in
          assert_response ~status:"HTTP/1.1 100 Continue" ~body:"" interim;
          write_all fd "he" 0 2;
          Unix.kill pid Sys.sigint;
          wait_until_not_listening ~timeout:0.25;
          write_all fd "llo" 0 3;
          let response = read_response fd in
          assert_response ~connection:"close" ~content_length:15
            ~status:"HTTP/1.1 200 OK" ~body:"submitted:hello" response;
          let next_request =
            "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          in
          (try write_all fd next_request 0 (String.length next_request)
           with Unix.Unix_error ((Unix.EPIPE | Unix.ECONNRESET), _, _) -> ());
          (try Unix.shutdown fd Unix.SHUTDOWN_SEND
           with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ());
          let trailing = read_until_eof ~timeout:1.0 fd (Buffer.create 16) in
          Alcotest.(check bool)
            "connection did not serve another request" false
            (contains "hello world\n" trailing);
          wait_for_exit pid ~timeout:2.0 |> check_shutdown_exit_status))

let test_shutdown_drains_active_stream () =
  let path = server_path () in
  with_server_capture_log path (fun pid _log_path ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let idle_fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let buffer = Buffer.create 4096 in
      Fun.protect
        ~finally:(fun () ->
          Unix.close idle_fd;
          Unix.close fd)
        (fun () ->
          Unix.connect idle_fd (Unix.ADDR_INET (host, port));
          let idle_request =
            "GET / HTTP/1.1\r\n\
             Host: localhost\r\n\
             Connection: keep-alive\r\n\
             \r\n"
          in
          write_all idle_fd idle_request 0 (String.length idle_request);
          ignore (read_response idle_fd);
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "GET /stream HTTP/1.1\r\n\
             Host: localhost\r\n\
             Connection: keep-alive\r\n\
             \r\n"
          in
          write_all fd request 0 (String.length request);
          ignore (read_until_contains fd buffer "\r\n\r\n6\r\nhello \r\n");
          let pipelined_request =
            "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
          in
          write_all fd pipelined_request 0 (String.length pipelined_request);
          Unix.shutdown fd Unix.SHUTDOWN_SEND;
          Unix.kill pid Sys.sigint;
          wait_until_not_listening ~timeout:0.25;
          ignore (read_until_eof ~timeout:1.0 idle_fd (Buffer.create 16));
          let complete = read_until_contains fd buffer "6\r\nworld\n\r\n" in
          Alcotest.(check bool)
            "active stream completed" true
            (contains "6\r\nworld\n\r\n" complete);
          let finished = read_until_contains fd buffer "\r\n0\r\n\r\n" in
          Alcotest.(check bool)
            "stream terminator completed" true
            (contains "\r\n0\r\n\r\n" finished);
          ignore (read_until_eof ~timeout:2.0 fd buffer);
          wait_for_exit pid ~timeout:2.0 |> check_shutdown_exit_status))

let test_shutdown_deadline_force_closes_active_stream () =
  let path = server_path () in
  with_server_capture_log
    ~env:[ ("HALO_SHUTDOWN_TIMEOUT_MS", "500") ]
    path
    (fun pid _log_path ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      let buffer = Buffer.create 4096 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          Unix.connect fd (Unix.ADDR_INET (host, port));
          let request =
            "GET /stream/hold HTTP/1.1\r\n\
             Host: localhost\r\n\
             Connection: keep-alive\r\n\
             \r\n"
          in
          write_all fd request 0 (String.length request);
          ignore (read_until_contains fd buffer "\r\n\r\n6\r\nhello \r\n");
          Unix.kill pid Sys.sigint;
          wait_until_not_listening ~timeout:0.25;
          Alcotest.(check bool)
            "server remains alive until the deadline" true
            (wait_for_exit_status pid ~timeout:0.0 = None);
          let response = read_until_eof ~timeout:2.0 fd buffer in
          Alcotest.(check bool)
            "forced stream has no terminator" false
            (contains "\r\n0\r\n\r\n" response);
          wait_for_exit pid ~timeout:2.0 |> check_shutdown_exit_status))

let test_shutdown_during_suspended_informational_response_exits_promptly () =
  let path = server_path () in
  with_server_capture_log path (fun pid _log_path ->
      let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Fun.protect
        ~finally:(fun () -> Unix.close fd)
        (fun () ->
          let request = "GET /early HTTP/1.1\r\nHost: localhost\r\n\r\n" in
          Unix.connect fd (Unix.ADDR_INET (host, port));
          write_all fd request 0 (String.length request);
          let interim = read_response fd in
          assert_response ~status:"HTTP/1.1 102 Processing" ~body:"" interim;
          Unix.shutdown fd Unix.SHUTDOWN_SEND;
          Unix.kill pid Sys.sigint;
          match wait_for_exit pid ~timeout:2.0 with
          | Unix.WEXITED 0 -> ()
          | Unix.WSIGNALED signal when signal = Sys.sigint -> ()
          | Unix.WEXITED code ->
              fail
                "server exited with code %d during suspended dispatch shutdown"
                code
          | Unix.WSIGNALED signal ->
              fail
                "server terminated by signal %d during suspended dispatch \
                 shutdown"
                signal
          | Unix.WSTOPPED signal ->
              fail
                "server stopped by signal %d during suspended dispatch shutdown"
                signal))

let test_rejects_unsupported_expectation () =
  with_server (fun () ->
      let response =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Content-Length: 5\r\n\
           Expect: kittens\r\n\
           Connection: close\r\n\
           \r\n\
           hello"
      in
      assert_response ~connection:"close" ~content_length:19
        ~status:"HTTP/1.1 417 Expectation Failed" ~body:"expectation failed\n"
        response)

let test_rejects_duplicate_expect_headers () =
  with_server (fun () ->
      let response =
        request_response
          "POST /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Content-Length: 5\r\n\
           Expect: 100-continue\r\n\
           Expect: 100-continue\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 400 Bad Request" ~body:"bad request\n" response)

let test_patch_request_is_supported () =
  with_server (fun () ->
      let response =
        request_response
          "PATCH /submit HTTP/1.1\r\n\
           Host: localhost\r\n\
           Content-Length: 5\r\n\
           Connection: close\r\n\
           \r\n\
           hello"
      in
      assert_response ~connection:"close" ~content_length:13
        ~status:"HTTP/1.1 200 OK" ~body:"patched:hello" response)

let test_options_asterisk_returns_server_capabilities () =
  with_server (fun () ->
      let response =
        request_response
          "OPTIONS * HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:0
        ~status:"HTTP/1.1 200 OK" ~body:"" response;
      Alcotest.(check bool)
        "allow header" true
        (contains "Allow: GET, HEAD, POST, PATCH, PUT, DELETE, OPTIONS, TRACE"
           response))

let test_trace_echoes_request_message () =
  with_server (fun () ->
      let request =
        "TRACE /trace HTTP/1.1\r\n\
         Host: localhost\r\n\
         Content-Length: 5\r\n\
         Connection: close\r\n\
         \r\n\
         hello"
      in
      let response = request_response request in
      assert_response ~connection:"close"
        ~content_length:(String.length request) ~status:"HTTP/1.1 200 OK"
        ~body:request response;
      Alcotest.(check bool)
        "trace content type" true
        (contains "Content-Type: message/http" response))

let test_connect_authority_form_returns_not_implemented () =
  with_server (fun () ->
      let response =
        request_response
          "CONNECT example.com:443 HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:24
        ~status:"HTTP/1.1 501 Not Implemented" ~body:"connect not implemented\n"
        response)

let test_route_exception_returns_500_and_server_keeps_running () =
  with_server (fun () ->
      let response =
        request_response
          "GET /explode HTTP/1.1\r\n\
           Host: localhost\r\n\
           Connection: close\r\n\
           \r\n"
      in
      assert_response ~connection:"close" ~content_length:22
        ~status:"HTTP/1.1 500 Internal Server Error"
        ~body:"internal server error\n" response;
      let next_response =
        request_response
          "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:12
        ~status:"HTTP/1.1 200 OK" ~body:"hello world\n" next_response)

let test_route_method_mismatch_returns_405 () =
  with_server (fun () ->
      let response =
        request_response
          "POST / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:19
        ~status:"HTTP/1.1 405 Method Not Allowed" ~body:"method not allowed\n"
        response;
      Alcotest.(check bool)
        "allow header" true
        (contains "Allow: GET, HEAD" response))

let test_route_method_mismatch_returns_single_allow_method () =
  with_server (fun () ->
      let response =
        request_response
          "GET /submit HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
      in
      assert_response ~connection:"close" ~content_length:19
        ~status:"HTTP/1.1 405 Method Not Allowed" ~body:"method not allowed\n"
        response;
      Alcotest.(check bool)
        "allow header" true
        (contains "Allow: POST, PATCH" response))

let () =
  Alcotest.run "ocaml-halo"
    [
      ( "integration",
        [
          timed_test_case "fixed HTTP response" `Quick test_fixed_response;
          timed_test_case "query parameters are percent-decoded" `Quick
            test_query_parameters_are_percent_decoded;
          timed_test_case "invalid URI encoding returns bad request" `Quick
            test_invalid_uri_encoding_returns_bad_request;
          timed_test_case "403 uses the Forbidden reason phrase" `Quick
            test_forbidden_reason_phrase;
          timed_test_case "TLS HTTPS response" `Quick test_tls_https_response;
          timed_test_case "TLS streams a large request body" `Quick
            test_tls_large_request_body;
          timed_test_case "TLS rejects plaintext HTTP" `Quick
            test_tls_rejects_plaintext_http;
          timed_test_case "TLS rejects malformed ticket key file on startup"
            `Quick test_tls_rejects_malformed_ticket_key_file_on_startup;
          timed_test_case "rejects absolute-form target" `Quick
            test_rejects_absolute_target;
          timed_test_case "accepts request body" `Quick
            test_accepts_body_request;
          timed_test_case "HEAD suppresses response body" `Quick
            test_head_suppresses_response_body;
          timed_test_case "HEAD uses GET route and suppresses body" `Quick
            test_head_uses_get_route_and_suppresses_body;
          timed_test_case "HEAD stream route omits body" `Quick
            test_head_stream_route_omits_body;
          timed_test_case "HTML response helper" `Quick test_html_response;
          timed_test_case "JSON response helper" `Quick test_json_response;
          timed_test_case "rejects invalid content-length" `Quick
            test_rejects_invalid_content_length;
          timed_test_case "HTTP/1.1 requires host header" `Quick
            test_http11_requires_host_header;
          timed_test_case "HTTP/1.1 rejects duplicate host headers" `Quick
            test_http11_rejects_duplicate_host_headers;
          timed_test_case "HTTP/1.1 rejects empty host header" `Quick
            test_http11_rejects_empty_host_header;
          timed_test_case "HTTP/1.1 rejects malformed host header" `Quick
            test_http11_rejects_malformed_host_header;
          timed_test_case "HTTP/1.1 accepts bracketed IPv6 host" `Quick
            test_http11_accepts_bracketed_ipv6_host;
          timed_test_case "HTTP/1.0 allows missing host header" `Quick
            test_http10_allows_missing_host_header;
          timed_test_case "rejects unsupported HTTP version" `Quick
            test_rejects_unsupported_http_version;
          timed_test_case "rejects too large request target" `Quick
            test_rejects_too_large_request_target;
          timed_test_case "rejects too large request body" `Quick
            test_rejects_too_large_request_body;
          timed_test_case
            "rejects streamed too large request body without reset" `Quick
            test_rejects_streamed_too_large_request_body_without_reset;
          timed_test_case "rejects too large request header fields" `Quick
            test_rejects_too_large_request_header_fields;
          timed_test_case "request line limit is aggregate" `Quick
            test_request_line_limit_is_aggregate;
          timed_test_case "header limit is aggregate" `Quick
            test_header_limit_is_aggregate;
          timed_test_case "body limit is independent" `Quick
            test_body_limit_is_independent;
          timed_test_case "POST without defined length returns 411" `Quick
            test_post_without_defined_length_returns_411;
          timed_test_case "accepts chunked request body" `Quick
            test_accepts_chunked_request_body;
          timed_test_case "rejects transfer-encoding with content-length" `Quick
            test_rejects_transfer_encoding_with_content_length;
          timed_test_case "rejects duplicate content-length headers" `Quick
            test_rejects_duplicate_content_length_headers;
          timed_test_case "rejects chunked request trailers" `Quick
            test_rejects_chunked_request_trailers;
          timed_test_case "custom response headers" `Quick
            test_custom_response_headers;
          timed_test_case "request headers strip hop-by-hop fields" `Quick
            test_request_headers_strip_hop_by_hop_fields;
          timed_test_case
            "request headers combine connection options across lines" `Quick
            test_request_headers_combine_connection_options_across_lines;
          timed_test_case "request headers preserve non hop-by-hop fields"
            `Quick test_request_headers_preserve_non_hop_by_hop_fields;
          timed_test_case "rejects invalid connection options" `Quick
            test_rejects_invalid_connection_options;
          timed_test_case "204 suppresses response body and content-length"
            `Quick test_204_suppresses_response_body_and_content_length;
          timed_test_case "304 suppresses response body and content-length"
            `Quick test_304_suppresses_response_body_and_content_length;
          timed_test_case "HTTP/1.1 keep-alive reuses connection" `Quick
            test_http11_keep_alive_reuses_connection;
          timed_test_case
            "header storage reuses keep-alive capacity and resets values" `Quick
            test_header_storage_keep_alive_reuses_capacity_and_resets_values;
          timed_test_case
            "HTTP/1.1 keep-alive reuses connection after fixed-length body"
            `Quick
            test_http11_keep_alive_reuses_connection_after_fixed_length_body;
          timed_test_case "HTTP/1.1 keep-alive after bodyless response" `Quick
            test_http11_keep_alive_after_bodyless_response;
          timed_test_case "request timeout returns 408 for partial headers"
            `Quick test_request_timeout_returns_408_for_partial_headers;
          timed_test_case "request timeout returns 408 for partial body" `Quick
            test_request_timeout_returns_408_for_partial_body;
          timed_test_case "keep-alive idle timeout closes connection" `Quick
            test_keep_alive_idle_timeout_closes_connection;
          timed_test_case "explicit connection close closes socket" `Quick
            test_explicit_connection_close_closes_socket;
          timed_test_case "HTTP/1.0 keep-alive header" `Quick
            test_http10_keep_alive_header;
          timed_test_case
            "HTTP/1.1 streaming route streams chunks incrementally" `Quick
            test_http11_streaming_route_streams_chunks_incrementally;
          timed_test_case "streaming exception after start aborts stream" `Quick
            test_streaming_exception_after_start_aborts_stream;
          timed_test_case "SSE route streams expected headers and frames" `Quick
            test_sse_route_streams_expected_headers_and_frames;
          timed_test_case "stream disconnect runs cleanup" `Quick
            test_stream_disconnect_runs_cleanup;
          timed_test_case "HTTP/1.0 streaming route returns 500" `Quick
            test_http10_streaming_route_returns_500;
          timed_test_case "Expect 100-continue before request body" `Quick
            test_expect_100_continue_before_request_body;
          timed_test_case "Expect 100-continue replays coalesced body bytes"
            `Quick test_expect_100_continue_replays_coalesced_body_bytes;
          Alcotest.test_case
            "route can emit informational responses before final response"
            `Quick
            test_route_can_emit_informational_responses_before_final_response;
          timed_test_case
            "shutdown promptly closes a completed half-closed connection" `Quick
            test_shutdown_closes_completed_half_closed_connection_promptly;
          timed_test_case "shutdown closes connection after upload completes"
            `Quick test_shutdown_closes_connection_after_upload_completes;
          timed_test_case "shutdown drains an active stream" `Quick
            test_shutdown_drains_active_stream;
          timed_test_case "shutdown deadline force-closes an active stream"
            `Quick test_shutdown_deadline_force_closes_active_stream;
          Alcotest.test_case
            "shutdown during suspended informational response exits promptly"
            `Quick
            test_shutdown_during_suspended_informational_response_exits_promptly;
          timed_test_case "rejects unsupported expectation" `Quick
            test_rejects_unsupported_expectation;
          timed_test_case "rejects duplicate expect headers" `Quick
            test_rejects_duplicate_expect_headers;
          timed_test_case "PATCH request is supported" `Quick
            test_patch_request_is_supported;
          timed_test_case "OPTIONS * returns server capabilities" `Quick
            test_options_asterisk_returns_server_capabilities;
          timed_test_case "TRACE echoes request message" `Quick
            test_trace_echoes_request_message;
          timed_test_case "CONNECT authority-form returns not implemented"
            `Quick test_connect_authority_form_returns_not_implemented;
          Alcotest.test_case
            "route exception returns 500 and server keeps running" `Quick
            test_route_exception_returns_500_and_server_keeps_running;
          timed_test_case "route method mismatch returns 405" `Quick
            test_route_method_mismatch_returns_405;
          timed_test_case "route method mismatch returns single Allow method"
            `Quick test_route_method_mismatch_returns_single_allow_method;
        ] );
    ]
