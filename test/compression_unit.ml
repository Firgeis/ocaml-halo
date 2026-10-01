module C = Halo.Compression
module R = Halo.Router
module H = R.Headers
module S = R.Status

let gunzip compressed =
  let temporary = Filename.temp_file "halo-gunzip-" ".gz" in
  Fun.protect
    ~finally:(fun () -> Sys.remove temporary)
    (fun () ->
      Out_channel.with_open_bin temporary (fun channel ->
          output_string channel compressed);
      let executable =
        Option.value (Sys.getenv_opt "HALO_GZIP_BIN") ~default:"gzip"
      in
      let channel =
        Unix.open_process_args_in executable
          [| executable; "-dc"; "--"; temporary |]
      in
      Fun.protect
        ~finally:(fun () ->
          match Unix.close_process_in channel with
          | Unix.WEXITED 0 -> ()
          | Unix.WEXITED code -> Alcotest.failf "gzip exited with code %d" code
          | Unix.WSIGNALED signal | Unix.WSTOPPED signal ->
              Alcotest.failf "gzip interrupted by signal %d" signal)
        (fun () -> In_channel.input_all channel))

let headers response = H.to_list (R.Response.headers response)

let header name response =
  headers response
  |> List.filter_map (fun (key, value) ->
      if String.lowercase_ascii key = String.lowercase_ascii name then
        Some value
      else None)

let body response =
  match R.Response.body response with
  | R.Fixed_body value -> value
  | R.Stream_body _ -> Alcotest.fail "expected buffered response"

let request ?(meth = R.GET) fields =
  R.
    {
      request_method = meth;
      raw_target = "/";
      target = parse_request_target meth "/";
      headers = fields;
      body = "";
    }

let dispatch ?(meth = R.GET) fields handler =
  R.dispatch (R.route (R.Matcher.pure handler)) (request ~meth fields)

let apply ?meth ?(fields = []) ?(min_body_size = 1024) ?(level = 6)
    ?compressible response =
  dispatch ?meth fields
    (C.middleware ~min_body_size ~level ?compressible () (fun _ -> response))

let encoding =
  Alcotest.testable
    (fun formatter -> function
      | C.Identity -> Format.pp_print_string formatter "identity"
      | C.Gzip -> Format.pp_print_string formatter "gzip"
      | C.Brotli -> Format.pp_print_string formatter "br")
    ( = )

let test_negotiation () =
  let cases =
    [
      ("absent", [], Some C.Identity);
      ("empty", [ "" ], Some C.Identity);
      ("whitespace", [ " \t" ], Some C.Identity);
      ("gzip", [ "gzip" ], Some C.Gzip);
      ("br", [ "br" ], Some C.Brotli);
      ("server tie-break", [ "gzip, br" ], Some C.Brotli);
      ("gzip higher quality", [ "br;q=0.5, gzip;q=0.8" ], Some C.Gzip);
      ("low quality is accepted", [ "gzip;q=0.001" ], Some C.Gzip);
      ( "explicit identity preference",
        [ "gzip;q=0.5, identity;q=0.9" ],
        Some C.Identity );
      ("identity tie", [ "gzip;q=0.5, identity;q=0.5" ], Some C.Gzip);
      ("forbidden gzip", [ "gzip;q=0" ], Some C.Identity);
      ("wildcard", [ "*" ], Some C.Brotli);
      ("wildcard exclusions", [ "*;q=1, br;q=0, gzip;q=0" ], Some C.Identity);
      ( "wildcard quality overridden",
        [ "*;q=1, br;q=0.1, gzip;q=0.8" ],
        Some C.Gzip );
      ("all excluded", [ "*;q=0" ], None);
      ("identity overrides wildcard", [ "*;q=0, identity;q=1" ], Some C.Identity);
      ("gzip overrides wildcard", [ "*;q=0, gzip;q=1" ], Some C.Gzip);
      ("unsupported encoding", [ "deflate" ], Some C.Identity);
      ("unsupported only", [ "deflate, identity;q=0" ], None);
      ("multiple fields", [ "gzip;q=0.8"; "br;q=0.1" ], Some C.Gzip);
      ("mixed case and OWS", [ " GZip ; Q = 0.8, BR;q=0.2 " ], Some C.Gzip);
      ("duplicate exclusion", [ "gzip;q=1, GZIP;q=0, br;q=0" ], Some C.Identity);
      ("duplicate fields", [ "gzip;q=0"; "gzip" ], Some C.Identity);
      ("empty fractional zero", [ "gzip;q=0." ], Some C.Identity);
      ("empty fractional one", [ "gzip;q=1." ], Some C.Gzip);
    ]
  in
  List.iter
    (fun (label, values, expected) ->
      Alcotest.(check (option encoding))
        label expected
        (C.negotiate ~available:[ C.Identity; C.Gzip; C.Brotli ] values))
    cases;
  List.iter
    (fun value ->
      Alcotest.(check (option encoding))
        ("malformed " ^ value) (Some C.Identity)
        (C.negotiate ~available:[ C.Identity; C.Gzip ]
           [ "*;q=1, gzip;q=" ^ value ]))
    [ "nan"; "inf"; "-1"; "2"; "0.1234"; "0.5e0"; ".5"; "1.001"; ""; "0.5;q=1" ];
  Alcotest.(check (option encoding))
    "empty does not accept compressed-only" None
    (C.negotiate ~available:[ C.Gzip ] [ "" ]);
  Alcotest.(check (option encoding))
    "absent permits compressed-only" (Some C.Brotli)
    (C.negotiate ~available:[ C.Gzip; C.Brotli ] []);
  Alcotest.(check (option encoding))
    "br is unavailable dynamically" (Some C.Gzip)
    (C.negotiate ~available:[ C.Identity; C.Gzip ] [ "br, gzip" ])

let test_content_types () =
  List.iter
    (fun value ->
      Alcotest.(check bool) value true (C.is_compressible_content_type value))
    [
      "text/html";
      "TEXT/CSS; charset=utf-8";
      "text/plain";
      "text/javascript";
      "application/json";
      "application/problem+json";
      "application/xml";
      "application/atom+xml";
      "application/javascript";
      "image/svg+xml";
    ];
  List.iter
    (fun value ->
      Alcotest.(check bool) value false (C.is_compressible_content_type value))
    [
      "text/event-stream; charset=utf-8";
      "image/png";
      "image/jpeg";
      "application/pdf";
      "application/gzip";
      "application/zip";
      "audio/mp3";
      "video/mp4";
      "font/woff";
      "font/woff2";
      "application/octet-stream";
    ]

let payload =
  String.concat ""
    (List.init 300 (fun i -> Printf.sprintf "record %d: some repeated text\n" i))

let test_gzip_roundtrip () =
  let binary =
    String.init 16384 (fun index -> Char.chr (((index * 73) + 19) mod 256))
  in
  List.iter
    (fun original ->
      List.iter
        (fun level ->
          let compressed = C.gzip ~level original in
          Alcotest.(check string)
            "gzip magic" "\x1f\x8b"
            (String.sub compressed 0 2);
          Alcotest.(check string) "round-trip" original (gunzip compressed))
        [ 0; 1; 6; 9 ])
    [ ""; "x"; payload; binary ];
  Alcotest.(check string)
    "deterministic encoding" (C.gzip payload) (C.gzip payload)

let test_compressed_metadata () =
  let response =
    R.json S.ok payload
    |> R.with_header "vArY" "Origin, accept-encoding"
    |> R.with_header "Content-Length" "999999"
    |> R.with_header "ETag" "\"original\""
    |> R.with_header "Content-MD5" "old"
    |> R.with_header "Content-Digest" "old"
    |> R.with_header "Repr-Digest" "old"
    |> R.with_header "Digest" "old"
    |> R.with_header "Accept-Ranges" "bytes"
  in
  let result = apply ~fields:[ ("Accept-Encoding", "br, gzip") ] response in
  Alcotest.(check (list string))
    "coding" [ "gzip" ]
    (header "content-encoding" result);
  Alcotest.(check (list string))
    "merged Vary"
    [ "Origin, accept-encoding" ]
    (header "vary" result);
  Alcotest.(check (list string))
    "encoded length"
    [ string_of_int (String.length (body result)) ]
    (header "content-length" result);
  Alcotest.(check (list string))
    "weak validator" [ "W/\"original\"" ] (header "etag" result);
  List.iter
    (fun name -> Alcotest.(check (list string)) name [] (header name result))
    [
      "content-md5"; "content-digest"; "repr-digest"; "digest"; "accept-ranges";
    ];
  Alcotest.(check string) "decoded payload" payload (gunzip (body result));
  let weak =
    apply
      ~fields:[ ("Accept-Encoding", "gzip") ]
      (response |> R.with_header "ETag" "W/\"original\"")
  in
  Alcotest.(check (list string))
    "existing weak validator" [ "W/\"original\"" ] (header "etag" weak)

let test_gzip_across_domains () =
  let workers =
    List.init 2 (fun worker ->
        Domain.spawn (fun () ->
            for iteration = 0 to 49 do
              let input =
                String.init 65536 (fun i ->
                    Char.chr (((i * 71) + worker + iteration) mod 256))
              in
              let encoded = C.gzip input in
              Alcotest.(check string)
                "cross-domain round-trip" input (gunzip encoded)
            done))
  in
  for _ = 0 to 19 do
    Gc.full_major ();
    Gc.compact ()
  done;
  List.iter Domain.join workers

let test_identity_and_vary () =
  List.iter
    (fun fields ->
      let result = apply ~fields (R.text S.ok payload) in
      Alcotest.(check string) "identity" payload (body result);
      Alcotest.(check (list string))
        "identity Vary" [ "Accept-Encoding" ] (header "vary" result);
      Alcotest.(check (list string))
        "no content encoding" []
        (header "content-encoding" result))
    [
      [];
      [ ("Accept-Encoding", "") ];
      [ ("Accept-Encoding", "gzip;q=0") ];
      [ ("Accept-Encoding", "br") ];
      [ ("Accept-Encoding", "gzip;q=0.2, identity;q=0.8") ];
    ];
  let fields = [ ("Accept-Encoding", "gzip") ] in
  let repeated =
    R.make ~status:S.ok
      ~headers:
        (H.of_list_exn
           [
             ("Content-Type", "text/plain");
             ("Vary", "Origin");
             ("vary", "origin, Cookie");
           ])
      payload
  in
  let result = apply ~fields repeated in
  Alcotest.(check (list string))
    "merge repeated Vary"
    [ "Origin, Cookie, Accept-Encoding" ]
    (header "vary" result);
  let result =
    apply ~fields (R.text S.ok payload |> R.with_header "Vary" "*")
  in
  Alcotest.(check (list string)) "Vary star" [ "*" ] (header "vary" result);
  let small = apply ~fields (R.text S.ok "small") in
  Alcotest.(check string) "minimum size" "small" (body small);
  let larger = apply ~fields ~level:0 (R.text S.ok payload) in
  Alcotest.(check string)
    "larger compressed body discarded" payload (body larger);
  Alcotest.(check (list string))
    "discarded encoding" []
    (header "content-encoding" larger)

let test_exclusions () =
  let fields = [ ("Accept-Encoding", "gzip") ] in
  let unchanged response =
    let result = apply ~fields response in
    Alcotest.(check string) "unchanged body" payload (body result);
    Alcotest.(check (list string))
      "no new encoding" []
      (header "content-encoding" result)
  in
  List.iter unchanged
    [
      R.text S.partial_content payload;
      R.text S.ok payload |> R.with_header "Content-Range" "bytes 0-9/20";
      R.text S.ok payload
      |> R.with_header "Cache-Control" "public, No-Transform";
      R.make ~status:S.ok payload;
      R.text S.ok payload |> R.with_content_type "image/png";
      R.text S.ok payload |> R.with_content_type "text/event-stream";
    ];
  let result =
    apply
      ~fields:[ ("Accept-Encoding", "gzip"); ("Cache-Control", "no-transform") ]
      (R.text S.ok payload)
  in
  Alcotest.(check string) "request no-transform" payload (body result);
  let result =
    apply ~fields
      (R.text S.ok payload
      |> R.with_header "Cache-Control" "private=\"x,no-transform,y\"")
  in
  Alcotest.(check (list string))
    "quoted value is not directive" [ "gzip" ]
    (header "content-encoding" result);
  let existing = R.text S.ok payload |> R.with_header "Content-Encoding" "br" in
  Alcotest.(check bool)
    "existing coding untouched" true
    (apply ~fields existing == existing);
  List.iter
    (fun status ->
      let response = R.text status payload in
      Alcotest.(check bool)
        "bodyless unchanged" true
        (apply ~fields response == response))
    [ S.no_content; S.reset_content; S.not_modified ];
  let ran = ref false in
  let stream =
    R.stream ~status:S.ok
      ~headers:(H.of_list_exn [ ("Content-Type", "text/plain") ])
      (fun ~yield ->
        ran := true;
        yield payload)
  in
  let result = apply ~fields stream in
  Alcotest.(check bool) "producer not buffered" false !ran;
  Alcotest.(check bool)
    "stream remains streaming" true
    (match R.Response.body result with R.Stream_body _ -> true | _ -> false)

let test_identity_excluded () =
  let fields = [ ("Accept-Encoding", "gzip, identity;q=0") ] in
  let result = apply ~fields (R.text S.ok "x") in
  Alcotest.(check string)
    "small gzip when identity excluded" "x"
    (gunzip (body result));
  let result = apply ~fields ~level:0 (R.text S.ok payload) in
  Alcotest.(check string)
    "larger gzip when identity excluded" payload
    (gunzip (body result));
  List.iter
    (fun response ->
      let result = apply ~fields response in
      Alcotest.(check int)
        "no acceptable representation" 406
        (S.to_int (R.Response.status result));
      Alcotest.(check string) "406 has no unacceptable body" "" (body result))
    [
      R.text S.ok payload |> R.with_content_type "image/png";
      R.text S.ok payload |> R.with_header "Cache-Control" "no-transform";
      R.stream ~status:S.ok (fun ~yield:_ ->
          Alcotest.fail "producer must not run");
    ];
  let result =
    apply
      ~fields:[ ("Accept-Encoding", "br, identity;q=0") ]
      (R.text S.ok payload)
  in
  Alcotest.(check int)
    "runtime br unavailable" 406
    (S.to_int (R.Response.status result))

let test_head_and_custom_policy () =
  let fields = [ ("Accept-Encoding", "gzip") ] in
  let response = R.text S.ok payload in
  let get = apply ~fields response
  and head = apply ~meth:R.HEAD ~fields response in
  Alcotest.(check (list (pair string string)))
    "GET/HEAD metadata" (headers get) (headers head);
  Alcotest.(check string) "same selected representation" (body get) (body head);
  let seen = ref "" in
  let result =
    apply ~fields
      ~compressible:(fun content_type ->
        seen := content_type;
        true)
      (R.text S.ok payload
      |> R.with_content_type "APPLICATION/octet-stream; foo=bar")
  in
  Alcotest.(check string)
    "normalized predicate argument" "application/octet-stream" !seen;
  Alcotest.(check (list string))
    "custom policy" [ "gzip" ]
    (header "content-encoding" result);
  let result =
    apply ~fields
      ~compressible:(fun _ -> true)
      (R.text S.ok payload |> R.with_content_type "text/event-stream")
  in
  Alcotest.(check (list string))
    "SSE always excluded" []
    (header "content-encoding" result)

let test_invalid_options () =
  List.iter
    (fun level ->
      Alcotest.check_raises "invalid level"
        (Invalid_argument "Compression: gzip level must be between 0 and 9")
        (fun () ->
          let (_ : R.middleware) = C.middleware ~level () in
          ()))
    [ -1; 10 ];
  Alcotest.check_raises "negative minimum size"
    (Invalid_argument "Compression: min_body_size must be nonnegative")
    (fun () ->
      let (_ : R.middleware) = C.middleware ~min_body_size:(-1) () in
      ())

let asset_payload = "hello world"

let with_assets f =
  let temporary = Filename.temp_file "halo-compression-" ".css" in
  let paths = [ temporary; temporary ^ ".gz"; temporary ^ ".br" ] in
  Fun.protect
    ~finally:(fun () ->
      List.iter (fun path -> if Sys.file_exists path then Sys.remove path) paths)
    (fun () ->
      Eio_main.run (fun env ->
          let path = Eio.Path.(Eio.Stdenv.fs env / temporary) in
          let root, name = path in
          Eio.Path.save ~create:(`Or_truncate 0o600) path asset_payload;
          Eio.Path.save ~create:(`Or_truncate 0o600)
            (root, name ^ ".gz")
            (C.gzip asset_payload);
          (* A prebuilt Brotli fixture for "hello world". *)
          let brotli = "\x0b\x05\x80hello world\x03" in
          Eio.Path.save ~create:(`Or_truncate 0o600) (root, name ^ ".br") brotli;
          f temporary path brotli))

let test_precompressed_assets () =
  with_assets (fun _ path brotli ->
      let handler = C.file ~content_type:"text/css; charset=utf-8" path in
      let serve ?meth fields = dispatch ?meth fields handler in
      let identity = serve [] in
      let gz = serve [ ("Accept-Encoding", "gzip") ] in
      let br = serve [ ("Accept-Encoding", "gzip, br") ] in
      Alcotest.(check string) "identity asset" asset_payload (body identity);
      Alcotest.(check string) "gzip asset" asset_payload (gunzip (body gz));
      Alcotest.(check string) "Brotli bytes passed through" brotli (body br);
      List.iter
        (fun result ->
          Alcotest.(check (list string))
            "original MIME"
            [ "text/css; charset=utf-8" ]
            (header "content-type" result);
          Alcotest.(check (list string))
            "encoded length"
            [ string_of_int (String.length (body result)) ]
            (header "content-length" result);
          Alcotest.(check (list string))
            "asset Vary" [ "Accept-Encoding" ] (header "vary" result))
        [ identity; gz; br ];
      Alcotest.(check (list string))
        "Brotli coding" [ "br" ]
        (header "content-encoding" br);
      Alcotest.(check bool)
        "variant-specific validators" true
        (header "etag" br <> header "etag" gz
        && header "etag" gz <> header "etag" identity);
      let preferred = serve [ ("Accept-Encoding", "gzip;q=0.8, br;q=0.1") ] in
      Alcotest.(check (list string))
        "asset quality" [ "gzip" ]
        (header "content-encoding" preferred);
      let head = serve ~meth:R.HEAD [ ("Accept-Encoding", "br, gzip") ] in
      Alcotest.(check (list (pair string string)))
        "HEAD metadata" (headers br) (headers head);
      let rejected = serve [ ("Accept-Encoding", "*;q=0") ] in
      Alcotest.(check int)
        "asset 406" 406
        (S.to_int (R.Response.status rejected));
      let post = serve ~meth:R.POST [] in
      Alcotest.(check int)
        "asset methods" 405
        (S.to_int (R.Response.status post));
      Alcotest.(check (list string))
        "Allow" [ "GET, HEAD" ] (header "allow" post);
      let range =
        serve [ ("Range", "bytes=0-10"); ("Accept-Encoding", "br") ]
      in
      Alcotest.(check int)
        "ranges return full response" 200
        (S.to_int (R.Response.status range));
      Alcotest.(check string) "full encoded bytes" brotli (body range))

let test_asset_conditionals () =
  with_assets (fun _ path _ ->
      let handler = C.file ~content_type:"text/css" path in
      let original = dispatch [ ("Accept-Encoding", "gzip") ] handler in
      let etag = List.hd (header "etag" original) in
      List.iter
        (fun token ->
          let result =
            dispatch
              [ ("Accept-Encoding", "gzip"); ("If-None-Match", token) ]
              handler
          in
          Alcotest.(check int)
            "matching variant" 304
            (S.to_int (R.Response.status result));
          Alcotest.(check string) "304 body" "" (body result);
          Alcotest.(check (list string))
            "304 encoding" [ "gzip" ]
            (header "content-encoding" result);
          Alcotest.(check (list string))
            "304 Vary" [ "Accept-Encoding" ] (header "vary" result))
        [ etag; "W/" ^ etag; "\"other\", " ^ etag; "*" ];
      let result =
        dispatch [ ("Accept-Encoding", "br"); ("If-None-Match", etag) ] handler
      in
      Alcotest.(check int)
        "other variant does not match" 200
        (S.to_int (R.Response.status result));
      let result =
        dispatch
          [ ("Accept-Encoding", "*;q=0"); ("If-None-Match", "*") ]
          handler
      in
      Alcotest.(check int)
        "negotiate before conditionals" 406
        (S.to_int (R.Response.status result)))

let test_asset_fallbacks () =
  with_assets (fun temporary path _ ->
      let handler = C.file ~content_type:"text/css" path in
      let now = Unix.gettimeofday () in
      Unix.utimes (temporary ^ ".br") (now -. 60.) (now -. 60.);
      let result = dispatch [ ("Accept-Encoding", "br, gzip") ] handler in
      Alcotest.(check (list string))
        "stale Brotli ignored" [ "gzip" ]
        (header "content-encoding" result);
      Sys.remove (temporary ^ ".gz");
      let result = dispatch [ ("Accept-Encoding", "br, gzip") ] handler in
      Alcotest.(check string)
        "missing sidecars fall back" asset_payload (body result);
      Sys.remove temporary;
      let result = dispatch [ ("Accept-Encoding", "br") ] handler in
      Alcotest.(check int)
        "original must exist" 404
        (S.to_int (R.Response.status result)))

let test_asset_no_transform () =
  with_assets (fun _ path _ ->
      let custom =
        H.of_list_exn
          [
            ("Cache-Control", "public, no-transform");
            ("Vary", "Origin");
            ("X-Asset", "yes");
            ("Content-Encoding", "wrong");
            ("ETag", "\"wrong\"");
            ("Digest", "wrong");
          ]
      in
      let handler = C.file ~headers:custom ~content_type:"text/css" path in
      let result = dispatch [ ("Accept-Encoding", "br, gzip") ] handler in
      Alcotest.(check string)
        "no-transform identity" asset_payload (body result);
      Alcotest.(check (list string))
        "no double coding" []
        (header "content-encoding" result);
      Alcotest.(check (list string))
        "stale digest removed" [] (header "digest" result);
      Alcotest.(check (list string))
        "custom metadata preserved" [ "yes" ] (header "x-asset" result);
      Alcotest.(check (list string))
        "asset Vary merged"
        [ "Origin, Accept-Encoding" ]
        (header "vary" result);
      let result =
        dispatch [ ("Accept-Encoding", "br, identity;q=0") ] handler
      in
      Alcotest.(check int)
        "no-transform rejection" 406
        (S.to_int (R.Response.status result));
      let handler = C.file ~content_type:"font/woff2" path in
      let result = dispatch [ ("Accept-Encoding", "br, gzip") ] handler in
      Alcotest.(check string)
        "compressed MIME skips sidecars" asset_payload (body result))

let () =
  Alcotest.run "compression"
    [
      ( "negotiation",
        [ Alcotest.test_case "Accept-Encoding" `Quick test_negotiation ] );
      ( "gzip",
        [
          Alcotest.test_case "round-trip and framing" `Quick test_gzip_roundtrip;
          Alcotest.test_case "GC across domains" `Quick test_gzip_across_domains;
          Alcotest.test_case "options" `Quick test_invalid_options;
        ] );
      ( "middleware",
        [
          Alcotest.test_case "content types" `Quick test_content_types;
          Alcotest.test_case "metadata" `Quick test_compressed_metadata;
          Alcotest.test_case "identity and Vary" `Quick test_identity_and_vary;
          Alcotest.test_case "exclusions" `Quick test_exclusions;
          Alcotest.test_case "identity excluded" `Quick test_identity_excluded;
          Alcotest.test_case "HEAD and custom policy" `Quick
            test_head_and_custom_policy;
        ] );
      ( "assets",
        [
          Alcotest.test_case "representations" `Quick test_precompressed_assets;
          Alcotest.test_case "conditionals" `Quick test_asset_conditionals;
          Alcotest.test_case "fallbacks" `Quick test_asset_fallbacks;
          Alcotest.test_case "no-transform" `Quick test_asset_no_transform;
        ] );
    ]
