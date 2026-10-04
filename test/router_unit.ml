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

module Status = Halo.Router.Status
module Response_headers = Halo.Router.Headers

let fixed_response ~status ~headers body =
  Halo.Router.make ~status ~headers:(Response_headers.of_list_exn headers) body

let check_response label (expected : Halo.Router.response) actual =
  let response : Halo.Router.response = actual in
  Alcotest.(check int)
    (label ^ " status")
    (Status.to_int (Halo.Router.Response.status expected))
    (Status.to_int (Halo.Router.Response.status response));
  Alcotest.(check (list (pair string string)))
    (label ^ " headers")
    (Response_headers.to_list (Halo.Router.Response.headers expected))
    (Response_headers.to_list (Halo.Router.Response.headers response));
  let fixed_body = function
    | Halo.Router.Fixed_body body -> body
    | Halo.Router.Stream_body _ -> Alcotest.fail "expected a fixed response"
  in
  Alcotest.(check string)
    (label ^ " body")
    (fixed_body (Halo.Router.Response.body expected))
    (fixed_body (Halo.Router.Response.body response))

let health_router =
  Halo.Router.route
    Halo.Router.Matcher.(
      let+ () = request_method_is GET *> path_segment "health" *> path_end in
      fun _request -> text Status.ok "ok\n")

let user_router =
  Halo.Router.route
    Halo.Router.Matcher.(
      let+ id =
        request_method_is GET *> path_segment "users" *> int_parameter
        <* path_end
      in
      fun _request -> text Status.ok (string_of_int id))

let echo_router =
  Halo.Router.route
    Halo.Router.Matcher.(
      let+ name =
        request_method_is POST *> path_segment "echo" *> string_parameter
        <* path_end
      in
      fun _request -> text Status.ok ("echo:" ^ name))

let routes =
  health_router
  |> Halo.Router.or_else user_router
  |> Halo.Router.or_else echo_router

let test_exact_route () =
  check_response "exact route" (text Status.ok "ok\n")
    (Halo.Router.dispatch routes (request GET "/health"))

let test_empty_path_segments_are_significant () =
  let plain_route =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () =
          request_method_is GET *> path_segment "a" *> path_segment "b"
          <* path_end
        in
        fun _request -> text Status.ok "plain")
  in
  let empty_segment_route =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () =
          request_method_is GET *> path_segment "a" *> path_segment ""
          *> path_segment "b"
          <* path_end
        in
        fun _request -> text Status.ok "empty segment")
  in
  let trailing_segment_route =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () =
          request_method_is GET *> path_segment "a" *> path_segment "b"
          *> path_segment ""
          <* path_end
        in
        fun _request -> text Status.ok "trailing segment")
  in
  let router =
    Halo.Router.(plain_route <|> empty_segment_route <|> trailing_segment_route)
  in
  check_response "ordinary path" (text Status.ok "plain")
    (Halo.Router.dispatch router (request GET "/a/b"));
  check_response "doubled slash"
    (text Status.ok "empty segment")
    (Halo.Router.dispatch router (request GET "/a//b"));
  check_response "trailing slash"
    (text Status.ok "trailing segment")
    (Halo.Router.dispatch router (request GET "/a/b/"));
  check_response "root remains segmentless"
    (text Status.not_found "not found\n")
    (Halo.Router.dispatch router (request GET "/"))

let test_method_mismatch () =
  check_response "method mismatch"
    (fixed_response ~status:Status.method_not_allowed
       ~headers:
         [
           ("Allow", "GET, HEAD"); ("Content-Type", "text/plain; charset=utf-8");
         ]
       "method not allowed\n")
    (Halo.Router.dispatch routes (request POST "/health"))

let test_head_uses_get_route_with_original_method () =
  let open Halo.Router in
  let pre_calls = ref 0 in
  let observe_pre request =
    incr pre_calls;
    Alcotest.(check string)
      "pre middleware sees HEAD" "HEAD"
      (Request.request_method_text request);
    Continue
  in
  let add_seen_method next request =
    next request
    |> with_header "X-Seen-Method" (Request.request_method_text request)
  in
  let router =
    route
      Matcher.(
        let+ () = request_method_is GET *> path_segment "head" *> path_end in
        fun request ->
          let method_ =
            match Request.request_method request with
            | Some method_ -> string_of_http_method method_
            | None -> "unknown"
          in
          text Status.ok (Request.request_method_text request ^ "/" ^ method_))
    |> use add_seen_method |> use_pre observe_pre
  in
  check_response "HEAD falls back to GET with original method"
    (fixed_response ~status:Status.ok
       ~headers:
         [
           ("X-Seen-Method", "HEAD");
           ("Content-Type", "text/plain; charset=utf-8");
         ]
       "HEAD/HEAD")
    (dispatch router (request HEAD "/head"));
  Alcotest.(check int) "pre middleware runs once" 1 !pre_calls

let test_explicit_head_route_takes_precedence () =
  let open Halo.Router in
  let get_route =
    route
      Matcher.(
        let+ () = request_method_is GET *> path_segment "head" *> path_end in
        fun _request -> text Status.ok "GET route")
  in
  let head_route =
    route
      Matcher.(
        let+ () = request_method_is HEAD *> path_segment "head" *> path_end in
        fun _request -> text Status.ok "HEAD route")
  in
  let router = get_route <|> head_route in
  check_response "explicit HEAD wins over GET fallback"
    (text Status.ok "HEAD route")
    (dispatch router (request HEAD "/head"));
  check_response "GET still uses GET route"
    (text Status.ok "GET route")
    (dispatch router (request GET "/head"))

let test_head_only_allow_header () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is HEAD *> path_segment "head" *> path_end in
        fun _request -> text Status.ok "HEAD route")
  in
  check_response "HEAD-only route Allow header"
    (fixed_response ~status:Status.method_not_allowed
       ~headers:
         [ ("Allow", "HEAD"); ("Content-Type", "text/plain; charset=utf-8") ]
       "method not allowed\n")
    (Halo.Router.dispatch router (request POST "/head"))

let test_int_capture () =
  check_response "int capture" (text Status.ok "42")
    (Halo.Router.dispatch routes (request GET "/users/42"))

let test_applicative_sequencing_operators () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ user_id =
          pure "discarded" *> request_method_is GET *> path_segment "users"
          *> int_parameter
          <* path_segment "profile" *> pure "discarded"
          <* path_end
        in
        fun _request -> text Status.ok (string_of_int user_id))
  in
  check_response "sequencing operators preserve selected value"
    (text Status.ok "42")
    (Halo.Router.dispatch router (request GET "/users/42/profile"));
  check_response "sequencing operators thread matcher state"
    (text Status.not_found "not found\n")
    (Halo.Router.dispatch router (request GET "/users/42"))

let test_query_ignored () =
  check_response "query ignored" (text Status.ok "ok\n")
    (Halo.Router.dispatch routes (request GET "/health?verbose=1"))

let test_queryparam_optional () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ query =
          request_method_is GET *> path_segment "search" *> path_end
          *> queryparam "q"
        in
        fun _request ->
          text Status.ok
            (match query with
            | Some value -> "q=" ^ value
            | None -> "q=<missing>"))
  in
  check_response "queryparam optional present" (text Status.ok "q=halo")
    (Halo.Router.dispatch router (request GET "/search?q=halo"));
  check_response "queryparam optional missing"
    (text Status.ok "q=<missing>")
    (Halo.Router.dispatch router (request GET "/search"))

let test_queryparam_required () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ query =
          request_method_is GET *> path_segment "search" *> path_end
          *> queryparam_required "q"
        in
        fun _request -> text Status.ok ("q=" ^ query))
  in
  check_response "queryparam required present" (text Status.ok "q=halo")
    (Halo.Router.dispatch router (request GET "/search?q=halo"));
  check_response "queryparam required missing"
    (text Status.not_found "not found\n")
    (Halo.Router.dispatch router (request GET "/search"))

let test_int_queryparam () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ page =
          request_method_is GET *> path_segment "search" *> path_end
          *> int_queryparam "page"
        in
        fun _request ->
          text Status.ok
            (match page with
            | Some value -> "page=" ^ string_of_int value
            | None -> "page=<missing>"))
  in
  check_response "int queryparam present" (text Status.ok "page=2")
    (Halo.Router.dispatch router (request GET "/search?page=2"));
  check_response "int queryparam missing"
    (text Status.ok "page=<missing>")
    (Halo.Router.dispatch router (request GET "/search"));
  check_response "int queryparam invalid"
    (text Status.ok "page=<missing>")
    (Halo.Router.dispatch router (request GET "/search?page=nope"))

let test_int_queryparam_required () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ page =
          request_method_is GET *> path_segment "search" *> path_end
          *> int_queryparam_required "page"
        in
        fun _request -> text Status.ok ("page=" ^ string_of_int page))
  in
  check_response "int queryparam required present" (text Status.ok "page=2")
    (Halo.Router.dispatch router (request GET "/search?page=2"));
  check_response "int queryparam required missing"
    (text Status.not_found "not found\n")
    (Halo.Router.dispatch router (request GET "/search"));
  check_response "int queryparam required invalid"
    (text Status.not_found "not found\n")
    (Halo.Router.dispatch router (request GET "/search?page=nope"))

let test_queryparams_repeated () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ tags =
          request_method_is GET *> path_segment "search" *> path_end
          *> queryparams "tag"
        in
        fun _request -> text Status.ok (String.concat "," tags))
  in
  check_response "queryparams repeated"
    (text Status.ok "ocaml,http")
    (Halo.Router.dispatch router (request GET "/search?tag=ocaml&tag=http"));
  check_response "queryparams empty" (text Status.ok "")
    (Halo.Router.dispatch router (request GET "/search"))

let test_queryparams_are_percent_decoded () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ query =
          request_method_is GET *> path_segment "search" *> path_end
          *> queryparam_required "q"
        and+ name = queryparam_required "first name"
        and+ plus = queryparam_required "plus"
        and+ tags = queryparams "tag" in
        fun _request ->
          text Status.ok
            (String.concat "|" [ query; name; plus; String.concat "," tags ]))
  in
  check_response "percent-decoded query names and values"
    (text Status.ok "a&b=c%/|Ada Lovelace|a+b|one,two")
    (Halo.Router.dispatch router
       (request GET
          "/search?q=a%26b%3Dc%25%2F&first%20name=Ada%20Lovelace&plus=a+b&t%61g=one&tag=two"))

let test_invalid_uri_encoding_is_bad_request () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET *> path_segment "search" *> path_end in
        fun _request -> text Status.ok "should not run")
  in
  let expected =
    fixed_response ~status:Status.bad_request
      ~headers:[ ("Content-Type", "text/plain; charset=utf-8") ]
      "bad request\n"
  in
  List.iter
    (fun target ->
      check_response
        ("invalid URI target " ^ target)
        expected
        (Halo.Router.dispatch router (request GET target)))
    [
      "/search?q=%";
      "/search?q=%2";
      "/search?q=%GG";
      "/search?q=[value]";
      "/search#fragment";
      "/search?q=value#fragment";
    ]

let test_not_found () =
  check_response "not found"
    (text Status.not_found "not found\n")
    (Halo.Router.dispatch routes (request GET "/missing"))

let test_static_before_dynamic () =
  let static_user_me =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () =
          request_method_is GET *> path_segment "users" *> path_segment "me"
          *> path_end
        in
        fun _request -> text Status.ok "self")
  in
  let dynamic_user =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ name =
          request_method_is GET *> path_segment "users" *> string_parameter
          <* path_end
        in
        fun _request -> text Status.ok name)
  in
  let router = Halo.Router.(static_user_me <|> dynamic_user) in
  check_response "static precedence" (text Status.ok "self")
    (Halo.Router.dispatch router (request GET "/users/me"))

let test_post_route () =
  check_response "post route"
    (text Status.ok "echo:halo")
    (Halo.Router.dispatch routes (request POST "/echo/halo"))

let test_scoped_routes () =
  let users_routes =
    Halo.Router.(
      route
        Matcher.(
          let+ user_id = request_method_is GET *> int_parameter <* path_end in
          fun _request -> text Status.ok ("user:" ^ string_of_int user_id))
      <|> route
            Matcher.(
              let+ () = request_method_is POST *> path_end in
              fun _request -> text Status.created "create-user"))
  in
  let routes =
    let users_prefix =
      Halo.Router.Matcher.(path_segment "api" *> path_segment "users")
    in
    Halo.Router.scope users_prefix users_routes
  in
  check_response "scoped get" (text Status.ok "user:7")
    (Halo.Router.dispatch routes (request GET "/api/users/7"));
  check_response "scoped post"
    (text Status.created "create-user")
    (Halo.Router.dispatch routes (request POST "/api/users"))

let test_scope_path_segment () =
  let users_routes =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ user_name =
          request_method_is GET *> string_parameter <* path_end
        in
        fun _request -> text Status.ok ("name:" ^ user_name))
  in
  let routes = Halo.Router.scope_path_segment "users" users_routes in
  check_response "scope path segment"
    (text Status.ok "name:alice")
    (Halo.Router.dispatch routes (request GET "/users/alice"))

let test_request_headers_and_body () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () =
          request_method_is POST *> path_segment "inspect" *> path_end
        in
        fun (request : Halo.Router.Request.t) ->
          let host =
            match Halo.Router.Request.header request "host" with
            | Some value -> value
            | None -> "missing"
          in
          text Status.ok
            (host ^ ":" ^ Halo.Body.read_all (Halo.Router.Request.body request)))
  in
  check_response "request headers and body"
    (text Status.ok "localhost:payload")
    (Halo.Router.dispatch router
       (request
          ~headers:[ ("host", "localhost"); ("x-test", "1") ]
          ~body:"payload" POST "/inspect"))

let test_request_multiple_headers () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET *> path_segment "cookies" *> path_end in
        fun (request : Halo.Router.Request.t) ->
          let cookies =
            Halo.Router.Request.headers request "set-cookie"
            |> String.concat ","
          in
          text Status.ok cookies)
  in
  check_response "request multiple headers" (text Status.ok "a=1,b=2")
    (Halo.Router.dispatch router
       (request
          ~headers:
            [
              ("Set-Cookie", "a=1"); ("set-cookie", "b=2"); ("host", "localhost");
            ]
          GET "/cookies"))

let test_response_headers () =
  let router =
    Halo.Router.route
      Halo.Router.Matcher.(
        let+ () = request_method_is GET *> path_segment "headers" *> path_end in
        fun _request ->
          Halo.Router.text Status.ok "ok\n"
          |> Halo.Router.with_header "Content-Type" "text/plain"
          |> Halo.Router.with_header "X-Test" "1")
  in
  check_response "response headers"
    (fixed_response ~status:Status.ok
       ~headers:[ ("X-Test", "1"); ("Content-Type", "text/plain") ]
       "ok\n")
    (Halo.Router.dispatch router (request GET "/headers"))

let test_text_sets_default_content_type () =
  check_response "text content type"
    (fixed_response ~status:Status.ok
       ~headers:[ ("Content-Type", "text/plain; charset=utf-8") ]
       "hello\n")
    (Halo.Router.text Status.ok "hello\n")

let test_html_sets_default_content_type () =
  check_response "html content type"
    (fixed_response ~status:Status.ok
       ~headers:[ ("Content-Type", "text/html; charset=utf-8") ]
       "<h1>hello</h1>\n")
    (Halo.Router.html Status.ok "<h1>hello</h1>\n")

let test_json_sets_default_content_type () =
  check_response "json content type"
    (fixed_response ~status:Status.ok
       ~headers:[ ("Content-Type", "application/json; charset=utf-8") ]
       "{\"ok\":true}\n")
    (Halo.Router.json Status.ok "{\"ok\":true}\n")

let test_with_content_type_overrides_existing_header () =
  check_response "with_content_type override"
    (fixed_response ~status:Status.ok
       ~headers:[ ("Content-Type", "text/custom") ]
       "hello\n")
    (Halo.Router.text Status.ok "hello\n"
    |> Halo.Router.with_content_type "text/custom")

let test_with_header_overrides_case_insensitively () =
  check_response "with_header override"
    (fixed_response ~status:Status.ok
       ~headers:[ ("content-type", "text/custom") ]
       "hello\n")
    (Halo.Router.text Status.ok "hello\n"
    |> Halo.Router.with_header "content-type" "text/custom")

let test_middleware_can_modify_response () =
  let add_trace_header next request =
    next request |> Halo.Router.with_header "X-Trace" "middleware"
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET *> path_segment "hello" *> path_end in
          fun _request -> text Status.ok "ok\n")
      |> use add_trace_header)
  in
  check_response "middleware modifies response"
    (fixed_response ~status:Status.ok
       ~headers:
         [
           ("X-Trace", "middleware");
           ("Content-Type", "text/plain; charset=utf-8");
         ]
       "ok\n")
    (Halo.Router.dispatch router (request GET "/hello"))

let test_middleware_can_short_circuit () =
  let require_auth next request =
    match Halo.Router.Request.header request "authorization" with
    | Some _token -> next request
    | None -> Halo.Router.text Status.unauthorized "unauthorized\n"
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () =
            request_method_is GET *> path_segment "private" *> path_end
          in
          fun _request -> text Status.ok "secret\n")
      |> use require_auth)
  in
  check_response "middleware short-circuits"
    (Halo.Router.text Status.unauthorized "unauthorized\n")
    (Halo.Router.dispatch router (request GET "/private"));
  check_response "middleware allows request"
    (Halo.Router.text Status.ok "secret\n")
    (Halo.Router.dispatch router
       (request ~headers:[ ("Authorization", "Bearer token") ] GET "/private"))

let test_middleware_wraps_not_found_and_method_not_allowed () =
  let add_trace_header next request =
    next request |> Halo.Router.with_header "X-Trace" "router"
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () =
            request_method_is GET *> path_segment "health" *> path_end
          in
          fun _request -> text Status.ok "ok\n")
      |> use add_trace_header)
  in
  check_response "middleware wraps 404"
    (fixed_response ~status:Status.not_found
       ~headers:
         [
           ("X-Trace", "router"); ("Content-Type", "text/plain; charset=utf-8");
         ]
       "not found\n")
    (Halo.Router.dispatch router (request GET "/missing"));
  check_response "middleware wraps 405"
    (fixed_response ~status:Status.method_not_allowed
       ~headers:
         [
           ("X-Trace", "router");
           ("Allow", "GET, HEAD");
           ("Content-Type", "text/plain; charset=utf-8");
         ]
       "method not allowed\n")
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
          let+ () =
            request_method_is GET *> path_segment "health" *> path_end
          in
          fun request -> text Status.ok (Halo.Router.Request.raw_target request))
      |> use_pre rewrite_legacy)
  in
  check_response "pre middleware rewrites path"
    (Halo.Router.text Status.ok "/health")
    (Halo.Router.dispatch router (request GET "/legacy"))

let test_pre_middleware_can_respond_early () =
  let block_forbidden request =
    match Halo.Router.Request.raw_target request with
    | "/forbidden" ->
        Halo.Router.Respond (Halo.Router.text Status.forbidden "forbidden\n")
    | _ -> Halo.Router.Continue
  in
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () =
            request_method_is GET *> path_segment "forbidden" *> path_end
          in
          fun _request -> text Status.ok "should not run\n")
      |> use_pre block_forbidden)
  in
  check_response "pre middleware responds early"
    (Halo.Router.text Status.forbidden "forbidden\n")
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
          let+ () =
            request_method_is GET *> path_segment "override" *> path_end
          in
          fun _request -> text Status.ok "rewritten\n")
      |> use_pre method_override)
  in
  check_response "pre middleware rewrites method"
    (Halo.Router.text Status.ok "rewritten\n")
    (Halo.Router.dispatch router
       (request ~headers:[ ("X-HTTP-Method-Override", "GET") ] POST "/override"))

let test_status_validation () =
  let expect_invalid code =
    match Status.of_int code with
    | Error (Status.Invalid_status_code actual) ->
        Alcotest.(check int) "invalid status code" code actual
    | Ok _ -> Alcotest.failf "expected status %d to be rejected" code
  in
  expect_invalid 99;
  expect_invalid 600;
  Alcotest.(check int)
    "lowest valid status" 100
    (Status.to_int (Status.of_int_exn 100));
  Alcotest.(check int)
    "highest valid status" 599
    (Status.to_int (Status.of_int_exn 599));
  Alcotest.(check string)
    "403 reason phrase" "Forbidden"
    (Status.reason_phrase Status.forbidden);
  Alcotest.(check string)
    "unknown reason phrase" ""
    (Status.reason_phrase (Status.of_int_exn 599))

let test_response_status_classes_are_enforced () =
  Alcotest.check_raises "final response rejects 1xx"
    (Invalid_argument "final response status must be between 200 and 599")
    (fun () -> ignore (Halo.Router.text Status.early_hints "invalid"));
  Alcotest.check_raises "response status replacement rejects 1xx"
    (Invalid_argument "final response status must be between 200 and 599")
    (fun () ->
      ignore
        (Halo.Router.text Status.ok "ok"
        |> Halo.Router.with_status Status.early_hints));
  Alcotest.check_raises "informational response rejects final status"
    (Invalid_argument "informational response status must be 1xx") (fun () ->
      ignore (Halo.Router.informational Status.ok Response_headers.empty))

let test_response_header_validation () =
  let expect_invalid_name name =
    match Response_headers.of_list [ (name, "value") ] with
    | Error (Response_headers.Invalid_name actual) ->
        Alcotest.(check string) "invalid header name" name actual
    | Error (Response_headers.Invalid_value _) ->
        Alcotest.failf "header name %S was reported as an invalid value" name
    | Ok _ -> Alcotest.failf "expected header name %S to be rejected" name
  in
  let expect_invalid_value value =
    match Response_headers.of_list [ ("X-Test", value) ] with
    | Error (Response_headers.Invalid_value { name }) ->
        Alcotest.(check string) "invalid header value name" "X-Test" name
    | Error (Response_headers.Invalid_name _) ->
        Alcotest.fail "valid header name was rejected"
    | Ok _ -> Alcotest.failf "expected header value %S to be rejected" value
  in
  List.iter expect_invalid_name
    [ ""; "Bad Header"; "Bad:Header"; "X\r\nInjected" ];
  List.iter expect_invalid_value
    [
      "safe\r\nSet-Cookie: injected=1";
      "line\nfeed";
      "nul\000byte";
      "ctl\001";
      "del\127";
    ];
  let valid =
    Response_headers.of_list_exn
      [ ("X-Token", "value\twith-tab"); ("X-Bytes", "\255") ]
  in
  Alcotest.(check (list (pair string string)))
    "valid response headers"
    [ ("X-Token", "value\twith-tab"); ("X-Bytes", "\255") ]
    (Response_headers.to_list valid);
  Alcotest.check_raises "with_header rejects response splitting"
    (Invalid_argument "invalid value for HTTP header \"X-Test\"") (fun () ->
      ignore
        (Halo.Router.text Status.ok "ok"
        |> Halo.Router.with_header "X-Test" "safe\r\nInjected: yes"))

let test_detached_request_has_no_transport_flags () =
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET *> path_end in
          fun request ->
            text Status.ok
              (Printf.sprintf "%b:%b:%b"
                 (Request.is_early_data request)
                 (Request.is_tls request)
                 (Option.is_none (Request.tls request)))))
  in
  check_response "detached request transport flags"
    (text Status.ok "false:false:true")
    (Halo.Router.dispatch router (request GET "/"))

let () =
  Alcotest.run "router"
    [
      ( "router",
        [
          Alcotest.test_case "exact route" `Quick test_exact_route;
          Alcotest.test_case "empty path segments are significant" `Quick
            test_empty_path_segments_are_significant;
          Alcotest.test_case "method mismatch" `Quick test_method_mismatch;
          Alcotest.test_case "HEAD uses GET route with original method" `Quick
            test_head_uses_get_route_with_original_method;
          Alcotest.test_case "explicit HEAD route takes precedence" `Quick
            test_explicit_head_route_takes_precedence;
          Alcotest.test_case "HEAD-only Allow header" `Quick
            test_head_only_allow_header;
          Alcotest.test_case "int capture" `Quick test_int_capture;
          Alcotest.test_case "applicative sequencing operators" `Quick
            test_applicative_sequencing_operators;
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
          Alcotest.test_case "queryparams are percent-decoded" `Quick
            test_queryparams_are_percent_decoded;
          Alcotest.test_case "invalid URI encoding is a bad request" `Quick
            test_invalid_uri_encoding_is_bad_request;
          Alcotest.test_case "not found" `Quick test_not_found;
          Alcotest.test_case "static before dynamic" `Quick
            test_static_before_dynamic;
          Alcotest.test_case "post route" `Quick test_post_route;
          Alcotest.test_case "scoped routes" `Quick test_scoped_routes;
          Alcotest.test_case "scope path segment" `Quick test_scope_path_segment;
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
          Alcotest.test_case "with_content_type overrides existing header"
            `Quick test_with_content_type_overrides_existing_header;
          Alcotest.test_case "with_header overrides case-insensitively" `Quick
            test_with_header_overrides_case_insensitively;
          Alcotest.test_case "middleware can modify response" `Quick
            test_middleware_can_modify_response;
          Alcotest.test_case "middleware can short-circuit" `Quick
            test_middleware_can_short_circuit;
          Alcotest.test_case "middleware wraps not found and method not allowed"
            `Quick test_middleware_wraps_not_found_and_method_not_allowed;
          Alcotest.test_case "pre middleware can rewrite before route matching"
            `Quick test_pre_middleware_can_rewrite_before_route_matching;
          Alcotest.test_case "pre middleware can respond early" `Quick
            test_pre_middleware_can_respond_early;
          Alcotest.test_case "pre middleware can rewrite method" `Quick
            test_pre_middleware_can_rewrite_method;
          Alcotest.test_case "status validation" `Quick test_status_validation;
          Alcotest.test_case "response status classes are enforced" `Quick
            test_response_status_classes_are_enforced;
          Alcotest.test_case "response header validation" `Quick
            test_response_header_validation;
          Alcotest.test_case "detached request has no transport flags" `Quick
            test_detached_request_has_no_transport_flags;
        ] );
    ]
