let request ?(headers = []) ?(body = "") request_method target =
  Halo.Router.
    {
      request_method;
      raw_target = target;
      target = parse_request_target request_method target;
      headers;
      body;
    }
let text = Halo.Router.text

let check_response label (expected : Halo.Router.response) actual =
  let response : Halo.Router.response = actual in
  Alcotest.(check int) (label ^ " status") expected.status response.status;
  Alcotest.(check (list (pair string string))) (label ^ " headers")
    expected.headers response.headers;
  Alcotest.(check string) (label ^ " body") expected.body response.body

let health_router =
    Halo.Router.route
    Halo.Router.Matcher.(
      let+ () = request_method_is GET
      and+ () = path_segment "health"
      and+ () = path_end in
      fun _request -> text 200 "ok\n")

let user_router =
    Halo.Router.route
    Halo.Router.Matcher.(
      let+ () = request_method_is GET
      and+ () = path_segment "users"
      and+ id = int_parameter
      and+ () = path_end in
      fun _request -> text 200 (string_of_int id))

let echo_router =
    Halo.Router.route
    Halo.Router.Matcher.(
      let+ () = request_method_is POST
      and+ () = path_segment "echo"
      and+ name = string_parameter
      and+ () = path_end in
      fun _request -> text 200 ("echo:" ^ name))

let routes =
  health_router
  |> Halo.Router.or_else user_router
  |> Halo.Router.or_else echo_router

let test_exact_route () =
  check_response "exact route" (text 200 "ok\n")
    (Halo.Router.dispatch routes (request GET "/health"))

let test_method_mismatch () =
  check_response "method mismatch"
    Halo.Router.
      {
        status = 405;
        headers =
          [
            ("Allow", "GET");
            ("Content-Type", "text/plain; charset=utf-8");
          ];
        body = "method not allowed\n";
      }
    (Halo.Router.dispatch routes (request POST "/health"))

let test_int_capture () =
  check_response "int capture" (text 200 "42")
    (Halo.Router.dispatch routes (request GET "/users/42"))

let test_query_ignored () =
  check_response "query ignored" (text 200 "ok\n")
    (Halo.Router.dispatch routes (request GET "/health?verbose=1"))

let test_queryparam_optional () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "search"
        and+ () = path_end
        and+ query = queryparam "q" in
        fun _request ->
          text 200
            (match query with
            | Some value -> "q=" ^ value
            | None -> "q=<missing>"))
  in
  check_response "queryparam optional present" (text 200 "q=halo")
    (Halo.Router.dispatch router (request GET "/search?q=halo"));
  check_response "queryparam optional missing" (text 200 "q=<missing>")
    (Halo.Router.dispatch router (request GET "/search"))

let test_queryparam_required () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "search"
        and+ () = path_end
        and+ query = queryparam_required "q" in
        fun _request -> text 200 ("q=" ^ query))
  in
  check_response "queryparam required present" (text 200 "q=halo")
    (Halo.Router.dispatch router (request GET "/search?q=halo"));
  check_response "queryparam required missing" (text 404 "not found\n")
    (Halo.Router.dispatch router (request GET "/search"))

let test_int_queryparam () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "search"
        and+ () = path_end
        and+ page = int_queryparam "page" in
        fun _request ->
          text 200
            (match page with
            | Some value -> "page=" ^ string_of_int value
            | None -> "page=<missing>"))
  in
  check_response "int queryparam present" (text 200 "page=2")
    (Halo.Router.dispatch router (request GET "/search?page=2"));
  check_response "int queryparam missing" (text 200 "page=<missing>")
    (Halo.Router.dispatch router (request GET "/search"));
  check_response "int queryparam invalid" (text 200 "page=<missing>")
    (Halo.Router.dispatch router (request GET "/search?page=nope"))

let test_int_queryparam_required () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "search"
        and+ () = path_end
        and+ page = int_queryparam_required "page" in
        fun _request -> text 200 ("page=" ^ string_of_int page))
  in
  check_response "int queryparam required present" (text 200 "page=2")
    (Halo.Router.dispatch router (request GET "/search?page=2"));
  check_response "int queryparam required missing" (text 404 "not found\n")
    (Halo.Router.dispatch router (request GET "/search"));
  check_response "int queryparam required invalid" (text 404 "not found\n")
    (Halo.Router.dispatch router (request GET "/search?page=nope"))

let test_queryparams_repeated () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "search"
        and+ () = path_end
        and+ tags = queryparams "tag" in
        fun _request -> text 200 (String.concat "," tags))
  in
  check_response "queryparams repeated" (text 200 "ocaml,http")
    (Halo.Router.dispatch router (request GET "/search?tag=ocaml&tag=http"));
  check_response "queryparams empty" (text 200 "")
    (Halo.Router.dispatch router (request GET "/search"))

let test_not_found () =
  check_response "not found" (text 404 "not found\n")
    (Halo.Router.dispatch routes (request GET "/missing"))

let test_static_before_dynamic () =
  let static_user_me =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "users"
        and+ () = path_segment "me"
        and+ () = path_end in
        fun _request -> text 200 "self")
  in
  let dynamic_user =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "users"
        and+ name = string_parameter
        and+ () = path_end in
        fun _request -> text 200 name)
  in
  let router = Halo.Router.(static_user_me <|> dynamic_user) in
  check_response "static precedence" (text 200 "self")
    (Halo.Router.dispatch router (request GET "/users/me"))

let test_post_route () =
  check_response "post route" (text 200 "echo:halo")
    (Halo.Router.dispatch routes (request POST "/echo/halo"))

let test_scoped_routes () =
  let users_routes =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ user_id = int_parameter
          and+ () = path_end in
          fun _request -> text 200 ("user:" ^ string_of_int user_id))
      <|>
      route
        Matcher.(
          let+ () = request_method_is POST
          and+ () = path_end in
          fun _request -> text 201 "create-user"))
  in
  let routes =
    let users_prefix =
      Halo.Router.Matcher.(
        let+ () = path_segment "api"
        and+ () = path_segment "users" in
        ())
    in
    Halo.Router.scope users_prefix users_routes
  in
  check_response "scoped get" (text 200 "user:7")
    (Halo.Router.dispatch routes (request GET "/api/users/7"));
  check_response "scoped post" (text 201 "create-user")
    (Halo.Router.dispatch routes (request POST "/api/users"))

let test_scope_path_segment () =
  let users_routes =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ user_name = string_parameter
        and+ () = path_end in
        fun _request -> text 200 ("name:" ^ user_name))
  in
  let routes =
    Halo.Router.scope_path_segment "users" users_routes
  in
  check_response "scope path segment" (text 200 "name:alice")
    (Halo.Router.dispatch routes (request GET "/users/alice"))

let test_request_headers_and_body () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is POST
        and+ () = path_segment "inspect"
        and+ () = path_end in
        fun (request : Halo.Router.Request.t) ->
          let host =
            match Halo.Router.Request.header request "host" with
            | Some value -> value
            | None -> "missing"
          in
          text 200 (host ^ ":" ^ Halo.Router.Request.body request))
  in
  check_response "request headers and body" (text 200 "localhost:payload")
    (Halo.Router.dispatch router
       (request ~headers:[("host", "localhost"); ("x-test", "1")] ~body:"payload"
          POST "/inspect"))

let test_request_multiple_headers () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "cookies"
        and+ () = path_end in
        fun (request : Halo.Router.Request.t) ->
          let cookies =
            Halo.Router.Request.headers request "set-cookie"
            |> String.concat ","
          in
          text 200 cookies)
  in
  check_response "request multiple headers" (text 200 "a=1,b=2")
    (Halo.Router.dispatch router
       (request
          ~headers:
            [("Set-Cookie", "a=1"); ("set-cookie", "b=2"); ("host", "localhost")]
          GET "/cookies"))

let test_response_headers () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET
        and+ () = path_segment "headers"
        and+ () = path_end in
        fun _request ->
          Halo.Router.text 200 "ok\n"
          |> Halo.Router.with_header "Content-Type" "text/plain"
          |> Halo.Router.with_header "X-Test" "1")
  in
  check_response "response headers"
    Halo.Router.{ status = 200; headers = [("X-Test", "1"); ("Content-Type", "text/plain")]; body = "ok\n" }
    (Halo.Router.dispatch router (request GET "/headers"))

let test_text_sets_default_content_type () =
  check_response "text content type"
    Halo.Router.
      {
        status = 200;
        headers = [("Content-Type", "text/plain; charset=utf-8")];
        body = "hello\n";
      }
    (Halo.Router.text 200 "hello\n")

let test_html_sets_default_content_type () =
  check_response "html content type"
    Halo.Router.
      {
        status = 200;
        headers = [("Content-Type", "text/html; charset=utf-8")];
        body = "<h1>hello</h1>\n";
      }
    (Halo.Router.html 200 "<h1>hello</h1>\n")

let test_json_sets_default_content_type () =
  check_response "json content type"
    Halo.Router.
      {
        status = 200;
        headers = [("Content-Type", "application/json; charset=utf-8")];
        body = "{\"ok\":true}\n";
      }
    (Halo.Router.json 200 "{\"ok\":true}\n")

let test_with_content_type_overrides_existing_header () =
  check_response "with_content_type override"
    Halo.Router.
      {
        status = 200;
        headers = [("Content-Type", "text/custom")];
        body = "hello\n";
      }
    (Halo.Router.text 200 "hello\n"
    |> Halo.Router.with_content_type "text/custom")

let test_with_header_overrides_case_insensitively () =
  check_response "with_header override"
    Halo.Router.
      {
        status = 200;
        headers = [("content-type", "text/custom")];
        body = "hello\n";
      }
    (Halo.Router.text 200 "hello\n"
    |> Halo.Router.with_header "content-type" "text/custom")

let test_middleware_can_modify_response () =
  let add_trace_header next request =
    next request |> Halo.Router.with_header "X-Trace" "middleware"
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "hello"
          and+ () = path_end in
          fun _request -> text 200 "ok\n")
      |> use add_trace_header)
  in
  check_response "middleware modifies response"
    Halo.Router.
      {
        status = 200;
        headers =
          [
            ("X-Trace", "middleware");
            ("Content-Type", "text/plain; charset=utf-8");
          ];
        body = "ok\n";
      }
    (Halo.Router.dispatch router (request GET "/hello"))

let test_middleware_can_short_circuit () =
  let require_auth next request =
    match Halo.Router.Request.header request "authorization" with
    | Some _token -> next request
    | None -> Halo.Router.text 401 "unauthorized\n"
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "private"
          and+ () = path_end in
          fun _request -> text 200 "secret\n")
      |> use require_auth)
  in
  check_response "middleware short-circuits"
    (Halo.Router.text 401 "unauthorized\n")
    (Halo.Router.dispatch router (request GET "/private"));
  check_response "middleware allows request"
    (Halo.Router.text 200 "secret\n")
    (Halo.Router.dispatch router
       (request ~headers:[("Authorization", "Bearer token")] GET "/private"))

let test_middleware_wraps_not_found_and_method_not_allowed () =
  let add_trace_header next request =
    next request |> Halo.Router.with_header "X-Trace" "router"
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "health"
          and+ () = path_end in
          fun _request -> text 200 "ok\n")
      |> use add_trace_header)
  in
  check_response "middleware wraps 404"
    Halo.Router.
      {
        status = 404;
        headers =
          [
            ("X-Trace", "router");
            ("Content-Type", "text/plain; charset=utf-8");
          ];
        body = "not found\n";
      }
    (Halo.Router.dispatch router (request GET "/missing"));
  check_response "middleware wraps 405"
    Halo.Router.
      {
        status = 405;
        headers =
          [
            ("X-Trace", "router");
            ("Allow", "GET");
            ("Content-Type", "text/plain; charset=utf-8");
          ];
        body = "method not allowed\n";
      }
    (Halo.Router.dispatch router (request POST "/health"))

let test_pre_middleware_can_rewrite_before_route_matching () =
  let rewrite_legacy request =
    match Halo.Router.Request.raw_target request with
    | "/legacy" ->
        Halo.Router.Rewrite
          {
            request_method = None;
            raw_target = Some "/health";
            headers = None;
            body = None;
          }
    | _ -> Halo.Router.Continue
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "health"
          and+ () = path_end in
          fun request -> text 200 (Halo.Router.Request.raw_target request))
      |> use_pre rewrite_legacy)
  in
  check_response "pre middleware rewrites path"
    (Halo.Router.text 200 "/health")
    (Halo.Router.dispatch router (request GET "/legacy"))

let test_pre_middleware_can_respond_early () =
  let block_forbidden request =
    match Halo.Router.Request.raw_target request with
    | "/forbidden" -> Halo.Router.Respond (Halo.Router.text 403 "forbidden\n")
    | _ -> Halo.Router.Continue
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "forbidden"
          and+ () = path_end in
          fun _request -> text 200 "should not run\n")
      |> use_pre block_forbidden)
  in
  check_response "pre middleware responds early"
    (Halo.Router.text 403 "forbidden\n")
    (Halo.Router.dispatch router (request GET "/forbidden"))

let test_pre_middleware_can_rewrite_method () =
  let method_override request =
    match Halo.Router.Request.header request "x-http-method-override" with
    | Some "GET" ->
        Halo.Router.Rewrite
          {
            request_method = Some Halo.Router.GET;
            raw_target = None;
            headers = None;
            body = None;
          }
    | _ -> Halo.Router.Continue
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "override"
          and+ () = path_end in
          fun _request -> text 200 "rewritten\n")
      |> use_pre method_override)
  in
  check_response "pre middleware rewrites method"
    (Halo.Router.text 200 "rewritten\n")
    (Halo.Router.dispatch router
       (request ~headers:[("X-HTTP-Method-Override", "GET")] POST "/override"))

let () =
  Alcotest.run "router"
    [
      ( "router",
        [
          Alcotest.test_case "exact route" `Quick test_exact_route;
          Alcotest.test_case "method mismatch" `Quick test_method_mismatch;
          Alcotest.test_case "int capture" `Quick test_int_capture;
          Alcotest.test_case "query ignored" `Quick test_query_ignored;
          Alcotest.test_case "queryparam optional" `Quick
            test_queryparam_optional;
          Alcotest.test_case "queryparam required" `Quick
            test_queryparam_required;
          Alcotest.test_case "int queryparam" `Quick test_int_queryparam;
          Alcotest.test_case "int queryparam required" `Quick
            test_int_queryparam_required;
          Alcotest.test_case "queryparams repeated" `Quick
            test_queryparams_repeated;
          Alcotest.test_case "not found" `Quick test_not_found;
          Alcotest.test_case "static before dynamic" `Quick
            test_static_before_dynamic;
          Alcotest.test_case "post route" `Quick test_post_route;
          Alcotest.test_case "scoped routes" `Quick test_scoped_routes;
          Alcotest.test_case "scope path segment" `Quick
            test_scope_path_segment;
          Alcotest.test_case "request headers and body" `Quick
            test_request_headers_and_body;
          Alcotest.test_case "request multiple headers" `Quick
            test_request_multiple_headers;
          Alcotest.test_case "response headers" `Quick test_response_headers;
          Alcotest.test_case "text sets default content-type" `Quick
            test_text_sets_default_content_type;
          Alcotest.test_case "html sets default content-type" `Quick
            test_html_sets_default_content_type;
          Alcotest.test_case "json sets default content-type" `Quick
            test_json_sets_default_content_type;
          Alcotest.test_case "with_content_type overrides existing header" `Quick
            test_with_content_type_overrides_existing_header;
          Alcotest.test_case "with_header overrides case-insensitively" `Quick
            test_with_header_overrides_case_insensitively;
          Alcotest.test_case "middleware can modify response" `Quick
            test_middleware_can_modify_response;
          Alcotest.test_case "middleware can short-circuit" `Quick
            test_middleware_can_short_circuit;
          Alcotest.test_case
            "middleware wraps not found and method not allowed"
            `Quick
            test_middleware_wraps_not_found_and_method_not_allowed;
          Alcotest.test_case
            "pre middleware can rewrite before route matching"
            `Quick
            test_pre_middleware_can_rewrite_before_route_matching;
          Alcotest.test_case "pre middleware can respond early" `Quick
            test_pre_middleware_can_respond_early;
          Alcotest.test_case "pre middleware can rewrite method" `Quick
            test_pre_middleware_can_rewrite_method;
        ] );
    ]
