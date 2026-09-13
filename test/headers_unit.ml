let request ?(headers = []) ?(body = "") request_method target =
  Halo.Router.
    {
      request_method;
      raw_target = target;
      target = parse_request_target request_method target;
      headers;
      body;
    }

let test_header_lookup_case_insensitive () =
  let request =
    request
      ~headers:[ ("Host", "example.com"); ("content-type", "text/plain") ]
      Halo.Router.GET "/"
  in
  Alcotest.(check (option string))
    "header lookup" (Some "example.com")
    (Halo.Headers.header request "host")

let test_multiple_headers () =
  let request =
    request
      ~headers:
        [
          ("Set-Cookie", "a=1"); ("set-cookie", "b=2"); ("Host", "example.com");
        ]
      Halo.Router.GET "/"
  in
  Alcotest.(check (list string))
    "multiple headers" [ "a=1"; "b=2" ]
    (Halo.Headers.headers request "set-cookie")

let test_content_type_parser () =
  let request =
    request
      ~headers:
        [ ("Content-Type", "application/json; charset=utf-8; version=1") ]
      Halo.Router.POST "/submit"
  in
  match Halo.Headers.content_type request with
  | None -> Alcotest.fail "expected content-type to parse"
  | Some media_type ->
      Alcotest.(check string) "type" "application" media_type.type_;
      Alcotest.(check string) "subtype" "json" media_type.subtype;
      Alcotest.(check (list (pair string string)))
        "parameters"
        [ ("charset", "utf-8"); ("version", "1") ]
        media_type.parameters

let test_invalid_content_type () =
  let request =
    request
      ~headers:[ ("Content-Type", "not-a-media-type") ]
      Halo.Router.POST "/submit"
  in
  Alcotest.(check bool)
    "invalid content type" true
    (Option.is_none (Halo.Headers.content_type request))

let () =
  Alcotest.run "headers"
    [
      ( "headers",
        [
          Alcotest.test_case "header lookup is case-insensitive" `Quick
            test_header_lookup_case_insensitive;
          Alcotest.test_case "multiple headers" `Quick test_multiple_headers;
          Alcotest.test_case "content-type parser" `Quick
            test_content_type_parser;
          Alcotest.test_case "invalid content-type" `Quick
            test_invalid_content_type;
        ] );
    ]
