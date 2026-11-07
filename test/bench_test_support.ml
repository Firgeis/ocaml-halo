let read path =
  let ch = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in_noerr ch) (fun () -> really_input_string ch (in_channel_length ch))

let write path data =
  let ch = open_out_bin path in
  Fun.protect ~finally:(fun () -> close_out_noerr ch) (fun () -> output_string ch data)

let with_temp f =
  let path = Filename.temp_file "halo-bench-test-" "" in
  Sys.remove path; Unix.mkdir path 0o700;
  Fun.protect ~finally:(fun () ->
    Array.iter (fun name -> try Sys.remove (Filename.concat path name) with Sys_error _ -> ()) (Sys.readdir path);
    Unix.rmdir path) (fun () -> f path)

let sleep s = ignore (Unix.select [] [] [] s)
let wait ?(timeout=15.) pid =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    match Unix.waitpid [Unix.WNOHANG] pid with
    | 0, _ when Unix.gettimeofday () < deadline -> sleep 0.01; loop ()
    | 0, _ -> Unix.kill pid Sys.sigkill; ignore (Unix.waitpid [] pid); Alcotest.fail "subprocess deadline exceeded"
    | _, status -> status
  in loop ()

let run ?(timeout=15.) exe args =
  with_temp (fun dir ->
    let out = Filename.concat dir "stdout" and err = Filename.concat dir "stderr" in
    let fd path = Unix.openfile path [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
    let stdout = fd out and stderr = fd err and stdin = Unix.openfile "/dev/null" [Unix.O_RDONLY] 0 in
    let pid = Unix.create_process exe (Array.of_list (exe :: args)) stdin stdout stderr in
    Unix.close stdin; Unix.close stdout; Unix.close stderr;
    let status = wait ~timeout pid in
    status, read out, read err)

let check_run ?(timeout=15.) ?(code=0) exe args =
  let status, out, err = run ~timeout exe args in
  Alcotest.(check bool) ("exit status; stdout: " ^ out ^ " stderr: " ^ err) true (status = Unix.WEXITED code);
  out

let json s = Yojson.Safe.from_string s
let member k j = Yojson.Safe.Util.member k j
let int k j = Yojson.Safe.Util.to_int (member k j)
let check_json name expected actual =
  Alcotest.(check string) name (Yojson.Safe.to_string (Yojson.Safe.sort expected)) (Yojson.Safe.to_string (Yojson.Safe.sort actual))
