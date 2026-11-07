module F = Halo.Forwarded
module R = Halo.Router

let policy ?(mode = F.Forwarded_only) ?(legacy = F.Aligned_lists) networks =
  match
    F.create ~trusted_proxies:networks ~header_mode:mode
      ~legacy_metadata_mode:legacy ()
  with
  | Ok p -> p
  | Error e -> failwith e

let ip = Unix.inet_addr_of_string

let resolve p headers =
  F.Private.resolve p ~peer:(Ipaddr.of_string_exn "10.0.0.1") headers

let check label expected actual =
  Alcotest.(check bool) label true (expected = actual)

let address result = Option.map Ipaddr.to_string (F.Private.address result)
let h value = [ ("Forwarded", value) ]
let x value = [ ("X-Forwarded-For", value) ]

let test_resolution () =
  let p = policy [ "10.0.0.0/8" ] in
  check "peer trusted" true
    (F.Private.trusted p (Ipaddr.of_string_exn "10.0.0.1"));
  check "boundary excluded" false
    (F.Private.trusted p (Ipaddr.of_string_exn "11.0.0.1"));
  check "bad policy" true
    (Result.is_error
       (F.create ~trusted_proxies:[ "host.example" ]
          ~header_mode:F.Forwarded_only ()));
  check "empty trusts none" false
    (F.Private.trusted (policy []) (Ipaddr.of_string_exn "10.0.0.1"));
  check "absent" F.Absent (F.Private.info (resolve p [])).status;
  let r =
    resolve p
      (h "for=evil, for=192.0.2.1;host=example.com;proto=https, for=10.0.0.2")
  in
  check "boundary" (Some "192.0.2.1") (address r);
  check "metadata follows client entry" (Some "example.com")
    (Option.map fst (F.Private.host r));
  check "scheme" (Some "https") (F.Private.scheme r);
  check "applied" F.Applied (F.Private.info r).status;
  check "source" F.Forwarded (F.Private.info r).client_address_source;
  let r = resolve p (h "for=10.0.0.3, for=10.0.0.2") in
  check "all trusted" (Some "10.0.0.3") (address r);
  let r = resolve p (h "for=192.0.2.1, for=unknown, for=10.0.0.2") in
  check "incomplete" F.Ignored (F.Private.info r).status;
  check "incomplete issue" [ F.Incomplete_chain ] (F.Private.info r).issues;
  let r = resolve p (h "for=192.0.2.1, for=\"_private:_port\"") in
  check "obfuscated" [ F.Incomplete_chain ] (F.Private.info r).issues;
  let r = resolve p (h "for=192.0.2.1;for=192.0.2.2") in
  check "duplicates" [ F.Malformed_input ] (F.Private.info r).issues;
  let r = resolve p (h "for=192.0.2.1;") in
  check "trailing empty parameter" F.Applied (F.Private.info r).status;
  check "trailing empty parameter address" (Some "192.0.2.1") (address r);
  let r = resolve p (h ";for=192.0.2.1;;proto=https;") in
  check "empty parameter slots" F.Applied (F.Private.info r).status;
  check "metadata after empty parameter" (Some "https") (F.Private.scheme r);
  let r = resolve p (h "for=192.0.2.1;;FOR=192.0.2.2") in
  check "duplicate across empty parameter" [ F.Malformed_input ]
    (F.Private.info r).issues;
  let r = resolve p (h ";;") in
  check "empty parameters without address" [ F.Incomplete_chain ]
    (F.Private.info r).issues;
  let r = resolve p (h "for =192.0.2.1") in
  check "invalid whitespace in pair" [ F.Malformed_input ]
    (F.Private.info r).issues;
  let r = resolve p (h "for=192.0.2.1, for=\"broken") in
  check "malformed suffix" F.Ignored (F.Private.info r).status;
  let r = resolve p (h "for=\"unterminated, for=192.0.2.1") in
  check "structural error before boundary" F.Ignored (F.Private.info r).status;
  let r = resolve p (h "for=192.0.2.1;host=\"[v1.example]\";proto=https") in
  check "unsupported host" [ F.Unsupported_value ] (F.Private.info r).issues;
  check "valid scheme survives unsupported host" (Some "https")
    (F.Private.scheme r);
  let r = resolve p (h "for=192.0.2.1;host=\"[v.bad\";proto=https") in
  check "malformed host" [ F.Malformed_input ] (F.Private.info r).issues;
  let r = resolve p (h "for=192.0.2.1;proto=https\r\n") in
  check "reject CRLF" F.Ignored (F.Private.info r).status;
  let r =
    resolve p
      [
        ( "Forwarded",
          ", \
           for=\"[2001:db8::1]:_port\";host=\"example.com:443\";proto=\"HTTPS\",, "
        );
      ]
  in
  check "quoted ipv6" (Some "2001:db8::1") (address r);
  check "quoted metadata" (Some "https") (F.Private.scheme r);
  check "parsed host and port"
    (Some ("example.com", Some 443))
    (F.Private.host r);
  let r = resolve p (h "for=\"[::00000]\"") in
  check "invalid IPv6 Forwarded status" F.Ignored (F.Private.info r).status;
  check "invalid IPv6 Forwarded issue" [ F.Malformed_input ]
    (F.Private.info r).issues;
  let r = resolve p (h "for=\"[2001:00000::1]\"") in
  check "long zero-padded component" [ F.Malformed_input ]
    (F.Private.info r).issues;
  let r = resolve p (h "for=\"[::ffff:192.0.2.1]\"") in
  check "embedded IPv4 remains valid" F.Applied (F.Private.info r).status;
  let r = resolve p (h "for=\"[::00000]\", for=192.0.2.1") in
  check "invalid address beyond trust boundary" (Some "192.0.2.1") (address r);
  check "invalid IPv6 policy" true
    (Result.is_error
       (F.create ~trusted_proxies:[ "::00000/64" ] ~header_mode:F.Forwarded_only
          ()));
  check "five-digit numeric port" F.Applied
    (F.Private.info (resolve p (h "for=\"192.0.2.1:12345\""))).status;
  check "oversized IPv4 numeric port" [ F.Malformed_input ]
    (F.Private.info (resolve p (h "for=\"192.0.2.1:123456789\""))).issues;
  check "oversized IPv6 numeric port" [ F.Malformed_input ]
    (F.Private.info (resolve p (h "for=\"[2001:db8::1]:123456\""))).issues;
  check "oversized unknown numeric port" [ F.Malformed_input ]
    (F.Private.info (resolve p (h "for=\"unknown:123456\""))).issues;
  let r =
    resolve p
      [
        ("Forwarded", "for=192.0.2.3;ext=\"x,\\\"y\"");
        ("forwarded", "for=10.0.0.2");
      ]
  in
  check "repeated and quoted" (Some "192.0.2.3") (address r);
  let r = resolve p (h "for=192.0.2.3;proto=ftp;host=example.com") in
  check "partial" F.Partial (F.Private.info r).status;
  check "unsupported scheme" [ F.Unsupported_value ] (F.Private.info r).issues;
  check "host retained" (Some "example.com") (Option.map fst (F.Private.host r));
  check "scheme fallback provenance" F.Direct (F.Private.info r).scheme_source;
  let p6 = policy [ "::ffff:10.0.0.0/104"; "2001:db8::/32" ] in
  check "families distinct" false
    (F.Private.trusted p6 (Ipaddr.of_string_exn "10.0.0.1"));
  check "IPv6 prefix" true
    (F.Private.trusted p6 (Ipaddr.of_string_exn "2001:db8::1"));
  let too_many = String.concat "," (List.init 65 (fun _ -> "")) in
  check "position limit" [ F.Resource_limit ]
    (F.Private.info (resolve p (h too_many))).issues;
  check "byte limit" [ F.Resource_limit ]
    (F.Private.info (resolve p (h (String.make 16385 'a')))).issues

let test_legacy () =
  let p = policy ~mode:F.X_forwarded_only [ "10.0.0.0/8" ] in
  let r = resolve p (x "::00000") in
  check "invalid IPv6 XFF status" F.Ignored (F.Private.info r).status;
  check "invalid IPv6 XFF issue" [ F.Malformed_input ] (F.Private.info r).issues;
  let r = resolve p (x "[::00000]:443") in
  check "invalid bracketed IPv6 XFF" [ F.Malformed_input ]
    (F.Private.info r).issues;
  let headers =
    [
      ("Forwarded", "for=203.0.113.9");
      ("X-Forwarded-For", "evil, 192.0.2.1, 10.0.0.2");
      ("X-Forwarded-Host", "evil, example.com, internal");
      ("X-Forwarded-Proto", "http, https, http");
    ]
  in
  let r = resolve p headers in
  check "XFF selected" (Some "192.0.2.1") (address r);
  check "aligned host" (Some "example.com") (Option.map fst (F.Private.host r));
  check "aligned proto" (Some "https") (F.Private.scheme r);
  let r =
    resolve p
      [
        ("X-Forwarded-For", "\"");
        ("x-forwarded-for", "192.0.2.1,10.0.0.2");
        ("X-Forwarded-Host", "attacker,public.example,proxy");
      ]
  in
  check "unbalanced quote before boundary" (Some "192.0.2.1") (address r);
  check "metadata aligned with untrusted prefix" (Some "public.example")
    (Option.map fst (F.Private.host r));
  check "untrusted prefix has no issue" F.Applied (F.Private.info r).status;
  check "same-field unbalanced prefix" (Some "192.0.2.1")
    (address (resolve p (x "\", 192.0.2.1")));
  let r = resolve p (x "192.0.2.1,\"") in
  check "malformed traversed suffix is ignored" F.Ignored
    (F.Private.info r).status;
  check "malformed traversed suffix issue" [ F.Malformed_input ]
    (F.Private.info r).issues;
  check "traversed CR is malformed" [ F.Malformed_input ]
    (F.Private.info (resolve p (x "192.0.2.1\r"))).issues;
  let r =
    resolve p
      (x "192.0.2.1,10.0.0.2"
      @ [
          ("X-Forwarded-Host", "example.com");
          ("X-Forwarded-Proto", "http,https");
        ])
  in
  check "misaligned partial" F.Partial (F.Private.info r).status;
  check "other metadata preserved" (Some "http") (F.Private.scheme r);
  let r =
    resolve p
      (x "192.0.2.1"
      @ [ ("X-Forwarded-Host", "\"broken"); ("X-Forwarded-Proto", "https") ])
  in
  check "malformed metadata partial" F.Partial (F.Private.info r).status;
  check "metadata fallback preserves scheme" (Some "https") (F.Private.scheme r);
  check "metadata byte limit" [ F.Resource_limit ]
    (F.Private.info
       (resolve p
          (x "192.0.2.1" @ [ ("X-Forwarded-Host", String.make 16385 'x') ])))
      .issues;
  let p =
    policy ~mode:F.X_forwarded_only ~legacy:F.Nearest_proxy_singletons
      [ "10.0.0.0/8" ]
  in
  let r =
    resolve p
      (x "192.0.2.1, 10.0.0.2"
      @ [
          ("X-Forwarded-Host", "example.com");
          ("X-Forwarded-Proto", "http,https");
        ])
  in
  check "singleton" (Some "example.com") (Option.map fst (F.Private.host r));
  check "multi rejected" F.Partial (F.Private.info r).status;
  let p = policy ~mode:F.Prefer_forwarded [ "10.0.0.0/8" ] in
  let r = resolve p ([ ("Forwarded", "") ] @ x "192.0.2.1") in
  check "no fallback on empty" F.Ignored (F.Private.info r).status;
  let p = policy ~mode:F.X_forwarded_only [ "10.0.0.0/8" ] in
  check "bare IPv6" (Some "2001:db8::1") (address (resolve p (x "2001:db8::1")))

let test_router () =
  let p = policy [ "10.0.0.0/8" ] in
  let seen = ref false in
  let router =
    R.(
      route
        Matcher.(
          let+ () = effective_host "public.example" *> request_method_is GET in
          fun request ->
            let host =
              Option.map
                (fun (h : Request.parsed_host) -> h.hostname)
                (Request.effective_host request)
            in
            check "effective host" (Some "public.example") host;
            check "forwarded https" "https" (Request.effective_scheme request);
            check "transport plaintext" false (Request.is_tls request);
            check "source host" F.Forwarded
              (Request.forwarded_info request).host_source;
            text Status.ok "matched")
      |> use_pre (fun request ->
          seen := Request.effective_scheme request = "https";
          Continue)
      |> with_forwarded_headers p)
  in
  Eio_main.run (fun _ ->
      Eio.Switch.run (fun sw ->
          let headers =
            [
              ("Host", "internal");
              ("Forwarded", "for=192.0.2.1;host=public.example;proto=https");
            ]
          in
          let dispatch meth =
            R.Runtime.dispatch_deferred router ~request_method:meth
              ~raw_target:"/"
              ~headers:(lazy headers)
              ~raw_headers:(lazy headers)
              ~body:(Halo.Body.Private.of_string "")
              ~http_major:1 ~http_minor:1
              ~connection_info:
                {
                  remote_address = Unix.ADDR_INET (ip "10.0.0.1", 123);
                  local_address = Unix.ADDR_UNIX "local";
                  tls = None;
                }
              ~is_early_data:false ~request_id:1L ~request_switch:sw
              ~send_informational:ignore ~on_disconnect:ignore
              ~on_response_terminated:ignore
          in
          check "GET" 200 (R.Status.to_int (R.Response.status (dispatch GET)));
          check "pre-middleware" true !seen;
          check "HEAD fallback" 200
            (R.Status.to_int (R.Response.status (dispatch HEAD)));
          check "method discovery" 405
            (R.Status.to_int (R.Response.status (dispatch POST)))))

let test_isolation () =
  let trusted = policy [ "10.0.0.0/8" ] and untrusted = policy [] in
  let original =
    [
      ("Host", "original");
      ("Forwarded", "for=192.0.2.1;host=public.example;proto=https");
    ]
  in
  let runtime router headers raw peer =
    Eio_main.run (fun _ ->
        Eio.Switch.run (fun sw ->
            R.Runtime.dispatch_deferred router ~request_method:GET
              ~raw_target:"/" ~headers ~raw_headers:raw
              ~body:(Halo.Body.Private.of_string "")
              ~http_major:1 ~http_minor:1
              ~connection_info:
                {
                  remote_address = peer;
                  local_address = Unix.ADDR_UNIX "local";
                  tls = None;
                }
              ~is_early_data:false ~request_id:1L ~request_switch:sw
              ~send_informational:ignore ~on_disconnect:ignore
              ~on_response_terminated:ignore))
  in
  let peer = Unix.ADDR_INET (ip "10.0.0.1", 1) in
  let status response = R.Status.to_int (R.Response.status response) in
  let rewritten =
    [
      ("Host", "rewritten");
      ("Forwarded", "for=203.0.113.1;host=evil;proto=http");
    ]
  in
  let nested =
    R.(
      route
        Matcher.(
          let+ () = effective_host "public.example" in
          fun request ->
            check "nested ignores rewritten header" (Some "192.0.2.1")
              (Option.map Unix.string_of_inet_addr
                 (Request.effective_client_address request));
            check "ordinary host sees rewrite" (Some "rewritten")
              (Option.map
                 (fun (h : Request.parsed_host) -> h.hostname)
                 (Request.host request));
            text Status.ok "ok")
      |> with_forwarded_headers trusted)
  in
  let outer =
    R.(
      nested
      |> use_pre (fun request ->
          check "inner pre sees inner policy" F.Applied
            (Request.forwarded_info request).status;
          Rewrite
            {
              request_method = None;
              raw_target = None;
              headers = Some rewritten;
              body = None;
            })
      |> with_forwarded_headers untrusted
      |> use_pre (fun request ->
          check "outer pre sees outer policy" F.Untrusted_peer
            (Request.forwarded_info request).status;
          Continue))
  in
  check "nested recomputes original" 200
    (status (runtime outer (lazy original) (lazy original) peer));
  let left =
    R.(
      route
        Matcher.(
          let+ () = effective_host "never" in
          fun _ -> text Status.ok "wrong")
      |> with_forwarded_headers trusted)
  in
  let right =
    R.(
      route
        Matcher.(
          let+ () = effective_host "original" in
          fun request ->
            check "failed sibling isolated" F.Disabled
              (Request.forwarded_info request).status;
            text Status.ok "ok"))
  in
  check "sibling" 200
    (status (runtime R.(left <|> right) (lazy original) (lazy original) peer));
  let absent_headers = lazy (failwith "untrusted policy decoded headers") in
  let router =
    R.(
      route
        (Matcher.pure (fun req ->
             check "untrusted status" F.Untrusted_peer
               (Request.forwarded_info req).status;
             text Status.ok "ok"))
      |> with_forwarded_headers untrusted)
  in
  check "untrusted deferred" 200
    (status (runtime router absent_headers absent_headers peer));
  check "unix deferred" 200
    (status
       (runtime
          (R.with_forwarded_headers trusted router)
          absent_headers absent_headers (Unix.ADDR_UNIX "peer")))

let () =
  Alcotest.run "forwarded"
    [
      ( "resolution",
        [
          Alcotest.test_case "boundaries and syntax" `Quick test_resolution;
          Alcotest.test_case "legacy" `Quick test_legacy;
          Alcotest.test_case "router" `Quick test_router;
          Alcotest.test_case "isolation" `Quick test_isolation;
        ] );
    ]
