let body_of_chunks chunks =
  let remaining = ref chunks in
  Halo.Body.Private.create ~read:(fun () ->
      match !remaining with
      | [] -> None
      | chunk :: rest ->
          remaining := rest;
          Some chunk)

let test_read_all_drains_only_unread_chunks () =
  let body = body_of_chunks [ "alpha"; "beta"; "gamma" ] in
  Alcotest.(check (option string))
    "first chunk" (Some "alpha") (Halo.Body.read body);
  Alcotest.(check string) "unread body" "betagamma" (Halo.Body.read_all body);
  Alcotest.(check string) "no body remains" "" (Halo.Body.read_all body);
  Alcotest.(check (option string)) "ended body" None (Halo.Body.read body)

let test_empty_body () =
  let body = body_of_chunks [] in
  Alcotest.(check string) "complete body" "" (Halo.Body.read_all body);
  Alcotest.(check bool) "body is complete" true (Halo.Body.is_complete body)

let test_read_all_with_prefix () =
  let read prefix chunks =
    Halo.Body.read_all_with_prefix ~prefix (body_of_chunks chunks)
  in
  List.iter
    (fun (prefix, chunks, expected) ->
      Alcotest.(check string)
        "prefix and unread chunks" expected (read prefix chunks))
    [
      ("prefix", [], "prefix");
      ("prefix", [ "body" ], "prefixbody");
      ("prefix", [ "a"; ""; "bc" ], "prefixabc");
      ("", [ "a"; "bc" ], "abc");
      ("", [], "");
    ];
  let body = body_of_chunks [ "read"; "unread"; " tail" ] in
  ignore (Halo.Body.read body);
  Alcotest.(check string)
    "only unread data" "prefix:unread tail"
    (Halo.Body.read_all_with_prefix ~prefix:"prefix:" body);
  Alcotest.(check bool) "body is complete" true (Halo.Body.is_complete body);
  Alcotest.(check string)
    "prefix when drained" "again:"
    (Halo.Body.read_all_with_prefix ~prefix:"again:" body);
  let chunk = String.init 100 (fun i -> Char.chr (i mod 256)) in
  let body = body_of_chunks [ chunk ] in
  Alcotest.(check bool)
    "empty prefix reuses a single chunk" true
    (Halo.Body.read_all_with_prefix ~prefix:"" body == chunk);
  let calls = ref 0 in
  let body =
    Halo.Body.Private.create ~read:(fun () ->
        incr calls;
        match !calls with
        | 1 -> Some "consumed"
        | 2 -> raise Exit
        | 3 -> Some "remaining"
        | _ -> None)
  in
  Alcotest.check_raises "read failure propagates" Exit (fun () ->
      ignore (Halo.Body.read_all_with_prefix ~prefix:"prefix" body));
  Alcotest.(check (option string))
    "lock released after failure" (Some "remaining") (Halo.Body.read body)

let () =
  Alcotest.run "body"
    [
      ( "body",
        [
          Alcotest.test_case "read_all drains only unread chunks" `Quick
            test_read_all_drains_only_unread_chunks;
          Alcotest.test_case "empty body" `Quick test_empty_body;
          Alcotest.test_case "read_all with prefix" `Quick
            test_read_all_with_prefix;
        ] );
    ]
