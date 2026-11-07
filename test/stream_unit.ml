let run response yield =
  match Halo.Router.Response.body response with
  | Halo.Router.Stream_body producer -> producer ~yield
  | Halo.Router.Fixed_body _ -> Alcotest.fail "expected streamed response"

let buffered ?(limit = 64 * 1024) producer =
  Halo.Stream.respond_buffered ~max_buffered_bytes:limit
    ~status:Halo.Router.Status.ok producer

let collect response =
  let chunks = ref [] in
  run response (fun chunk -> chunks := chunk :: !chunks);
  List.rev !chunks

let test_coalescing () =
  let chunk = String.make 1024 'x' in
  let chunks =
    collect
      (buffered (fun ~yield ~flush:_ ->
           for _ = 1 to 64 do
             yield chunk
           done))
  in
  Alcotest.(check (list int))
    "64 yields become one write" [ 65536 ]
    (List.map String.length chunks);
  Alcotest.(check string)
    "payload preserved" (String.make 65536 'x') (String.concat "" chunks)

let test_splitting_and_flush () =
  let chunks =
    collect
      (buffered ~limit:4 (fun ~yield ~flush ->
           flush ();
           yield "a";
           yield "b";
           flush ();
           flush ();
           yield "cdefghijk"))
  in
  Alcotest.(check (list string))
    "explicit flush, bounded writes, final flush"
    [ "ab"; "cdef"; "ghij"; "k" ]
    chunks;
  Alcotest.(check (list string))
    "empty producer writes nothing" []
    (collect (buffered (fun ~yield:_ ~flush:_ -> ())))

let test_errors () =
  List.iter
    (fun limit ->
      Alcotest.check_raises "invalid limit"
        (Invalid_argument "max_buffered_bytes must be positive") (fun () ->
          ignore (buffered ~limit (fun ~yield:_ ~flush:_ -> ()))))
    [ 0; -1 ];
  Alcotest.check_raises "empty yield"
    (Invalid_argument "streamed response chunks must not be empty") (fun () ->
      ignore (collect (buffered (fun ~yield ~flush:_ -> yield ""))));
  let chunks = ref [] in
  Alcotest.check_raises "producer failure" Exit (fun () ->
      run
        (buffered ~limit:4 (fun ~yield ~flush:_ ->
             yield "abcdefghij";
             raise Exit))
        (fun chunk -> chunks := chunk :: !chunks));
  Alcotest.(check (list string))
    "unflushed remainder discarded" [ "efgh"; "abcd" ] !chunks;
  let writes = ref 0 in
  Alcotest.check_raises "caught transport failure still aborts" Exit (fun () ->
      run
        (buffered (fun ~yield ~flush ->
             yield "first";
             (try flush () with Exit -> ());
             yield "second"))
        (fun _ ->
          incr writes;
          raise Exit));
  Alcotest.(check int) "failed write is not retried" 1 !writes

let test_backpressure () =
  Eio.Switch.run @@ fun sw ->
  let started, notify_started = Eio.Promise.create () in
  let release, notify_release = Eio.Promise.create () in
  let progress = ref 0 in
  let response =
    buffered ~limit:4 (fun ~yield ~flush:_ ->
        for _ = 1 to 8 do
          yield "x";
          incr progress
        done)
  in
  progress := 0;
  let writes = ref 0 in
  let producer =
    Eio.Fiber.fork_promise ~sw (fun () ->
        run response (fun chunk ->
            incr writes;
            Alcotest.(check int) "write bounded" 4 (String.length chunk);
            if !writes = 1 then (
              Eio.Promise.resolve notify_started ();
              Eio.Promise.await release)))
  in
  Eio.Promise.await started;
  Alcotest.(check int) "producer stops at full buffer" 3 !progress;
  Eio.Promise.resolve notify_release ();
  Eio.Promise.await_exn producer;
  Alcotest.(check int) "producer resumes after completion" 8 !progress

let test_concurrent_yields () =
  let writes = ref [] in
  let in_flight = ref 0 in
  run
    (buffered ~limit:4 (fun ~yield ~flush:_ ->
         Eio.Switch.run @@ fun sw ->
         Eio.Fiber.fork ~sw (fun () -> yield "aaaa");
         yield "bbbb"))
    (fun chunk ->
      incr in_flight;
      Alcotest.(check int) "only one transport write outstanding" 1 !in_flight;
      Eio.Fiber.yield ();
      writes := chunk :: !writes;
      decr in_flight);
  Alcotest.(check (list string))
    "concurrent yields preserve complete batches" [ "aaaa"; "bbbb" ]
    (List.sort String.compare !writes)

let test_sse_flush () =
  let chunks =
    collect
      (Halo.Sse.respond_buffered (fun ~send ~flush ->
           send (Halo.Sse.event [ "hello" ]);
           flush ();
           send (Halo.Sse.comment "alive")))
  in
  Alcotest.(check (list string))
    "SSE frames and explicit flush"
    [ "data: hello\n\n"; ": alive\n\n" ]
    chunks

let () =
  Eio_main.run @@ fun _ ->
  Alcotest.run "stream"
    [
      ( "buffered",
        [
          Alcotest.test_case "coalescing" `Quick test_coalescing;
          Alcotest.test_case "splitting and flush" `Quick
            test_splitting_and_flush;
          Alcotest.test_case "errors" `Quick test_errors;
          Alcotest.test_case "backpressure" `Quick test_backpressure;
          Alcotest.test_case "concurrent yields" `Quick test_concurrent_yields;
          Alcotest.test_case "SSE flush" `Quick test_sse_flush;
        ] );
    ]
