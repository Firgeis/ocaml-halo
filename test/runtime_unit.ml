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

let run_s2nc ?ticket_out ?ticket_in ?early_data_file ?send_file ?input
    ?(close_stdin = false) () =
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
    @ ["127.0.0.1"; "8443"]
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

let wait_for_child_with_runtime runtime pid =
  let rec loop () =
    ignore (Halo.Server.run_once runtime);
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ ->
        Unix.sleepf 0.01;
        loop ()
    | _, status -> status
  in
  loop ()

let with_live_tls_server tls f =
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_end in
          fun _request -> text 200 "hello world\n"))
  in
  match Halo.Server.with_runtime (fun runtime ->
      let config = Halo.Server.Config.create ~port:8443 ~tls router in
      match Halo.Server.listen runtime config with
      | Error message -> Error (Halo.Error.Runtime_error message)
      | Ok _server ->
          let pid = Unix.fork () in
          if pid = 0 then (
            try
              Unix.sleepf 0.05;
              f ();
              exit 0
            with
            | exn ->
                prerr_endline (Printexc.to_string exn);
                exit 1);
          let status = wait_for_child_with_runtime runtime pid in
          match status with
          | Unix.WEXITED 0 -> Ok (Halo.Server.runtime_metrics runtime)
          | Unix.WEXITED code ->
              Error
                (Halo.Error.Runtime_error
                   (Printf.sprintf "child exited with code %d" code))
          | Unix.WSIGNALED signal ->
              Error
                (Halo.Error.Runtime_error
                   (Printf.sprintf "child terminated by signal %d" signal))
          | Unix.WSTOPPED signal ->
              Error
                (Halo.Error.Runtime_error
                   (Printf.sprintf "child stopped by signal %d" signal)))
  with
  | Error error -> Alcotest.fail (Halo.Error.to_string error)
  | Ok metrics -> metrics

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

let test_runtime_metrics_defaults () =
  match Halo.Server.with_runtime (fun runtime ->
      ignore (Halo.Server.run_once runtime);
      Ok (Halo.Server.runtime_metrics runtime))
  with
  | Error error -> Alcotest.fail (Halo.Error.to_string error)
  | Ok metrics ->
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
      Alcotest.(check string)
        "controller state"
        "normal"
        (controller_state metrics.controller_state);
      Alcotest.(check bool)
        "backpressure disabled"
        false
        metrics.backpressure_enabled

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

let test_tls_early_data_accepts_first_use_and_rejects_replay () =
  with_temp_path "halo-session" ".bin" (fun session_path ->
      with_temp_file_contents "halo-early" ".txt"
        "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        (fun early_data_path ->
          let ticket_request =
            "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
          in
          let tls =
            Halo.Server.Config.tls
              ~cert_chain_path:(tls_cert_fixture_path ())
              ~private_key_path:(tls_key_fixture_path ())
              ~minimum_protocol_version:Halo.Server.TLS13
              ~session_tickets_enabled:true
              ~session_ticket_keys_path:(tls_ticket_keys_fixture_path ())
              ~session_ticket_encrypt_decrypt_lifetime_secs:
                (10 * 365 * 24 * 60 * 60)
              ~session_ticket_decrypt_lifetime_secs:
                (10 * 365 * 24 * 60 * 60)
              ~max_early_data_size:4096 ()
          in
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
                    ~early_data_file:early_data_path ~close_stdin:true ()
                in
                assert_process_success first_early;
                Alcotest.(check bool) "early data accepted" true
                  (contains "Early Data status: ACCEPTED" first_early.stdout);
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

let () =
  Alcotest.run "runtime"
    [
      ( "metrics",
        [
          Alcotest.test_case "default snapshot" `Quick
            test_runtime_metrics_defaults;
          Alcotest.test_case "explicit keys count full and resumed handshakes"
            `Slow
            test_tls_metrics_count_full_and_resumed_handshakes_with_explicit_keys;
          Alcotest.test_case
            "ephemeral keys resume within a process and invalidate after restart"
            `Slow test_tls_metrics_count_ephemeral_resumption_and_restart_invalidation;
          Alcotest.test_case "early data accepts first use and rejects replay"
            `Slow test_tls_early_data_accepts_first_use_and_rejects_replay;
        ] );
    ]
