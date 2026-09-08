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

let run_s2nc ?ticket_out ?ticket_in ?early_data_file ?send_file ?input
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

let with_live_config_groups ?on_error config_groups f =
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
                ?on_error
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

let with_live_configs ?on_error configs f =
  match with_live_config_groups ?on_error [ configs ] f with
  | [ metrics ] -> metrics
  | _ -> assert false

let with_live_server ?tls ?early_data_policy ?early_data_replay_store ?router
    ?on_error f =
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
  with_live_configs ?on_error [ config ] f

let with_live_tls_server tls = with_live_server ~tls

let plaintext_request ~until request =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close socket)
    (fun () ->
      Unix.connect socket
        (Unix.ADDR_INET (Unix.inet_addr_loopback, 8443));
      let payload = Bytes.of_string request in
      let rec write offset =
        if offset < Bytes.length payload then
          let count =
            Unix.write socket payload offset (Bytes.length payload - offset)
          in
          write (offset + count)
      in
      write 0;
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
      read ())

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
