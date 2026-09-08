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
  Alcotest.(check (option string)) "first chunk" (Some "alpha")
    (Halo.Body.read body);
  Alcotest.(check string) "unread body" "betagamma"
    (Halo.Body.read_all body);
  Alcotest.(check string) "no body remains" "" (Halo.Body.read_all body);
  Alcotest.(check (option string)) "ended body" None (Halo.Body.read body)

let test_empty_body () =
  let body = body_of_chunks [] in
  Alcotest.(check string) "complete body" "" (Halo.Body.read_all body);
  Alcotest.(check bool) "body is complete" true (Halo.Body.is_complete body)

let () =
  Alcotest.run "body"
    [
      ( "body",
        [
          Alcotest.test_case "read_all drains only unread chunks" `Quick
            test_read_all_drains_only_unread_chunks;
          Alcotest.test_case "empty body" `Quick test_empty_body;
        ] );
    ]
