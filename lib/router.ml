type borrowed_request

external borrowed_request_method_text : borrowed_request -> string
  = "ocaml_halo_request_method_text"

external borrowed_request_target : borrowed_request -> string
  = "ocaml_halo_request_target"

external borrowed_request_body : borrowed_request -> string
  = "ocaml_halo_request_body"

external borrowed_request_header : borrowed_request -> string -> string option
  = "ocaml_halo_request_header"

external borrowed_request_headers : borrowed_request -> string -> string list
  = "ocaml_halo_request_headers"

external borrowed_request_fold_headers :
  borrowed_request -> 'a -> ('a -> string -> string -> 'a) -> 'a
  = "ocaml_halo_request_fold_headers"

external borrowed_request_has_raw_header : borrowed_request -> string -> bool
  = "ocaml_halo_request_has_raw_header"

external borrowed_request_copy_headers : borrowed_request -> (string * string) list
  = "ocaml_halo_request_copy_headers"

external borrowed_request_copy_raw_headers :
  borrowed_request -> (string * string) list
  = "ocaml_halo_request_copy_raw_headers"

type _ Effect.t += Drain_request_body : string Effect.t
type _ Effect.t += Drain_request_body_with_prefix : string -> string Effect.t

module Http_method = struct
  type t =
    | GET
    | HEAD
    | POST
    | PATCH
    | PUT
    | DELETE
    | CONNECT
    | OPTIONS
    | TRACE

  let of_string = function
    | "GET" -> Some GET
    | "HEAD" -> Some HEAD
    | "POST" -> Some POST
    | "PATCH" -> Some PATCH
    | "PUT" -> Some PUT
    | "DELETE" -> Some DELETE
    | "CONNECT" -> Some CONNECT
    | "OPTIONS" -> Some OPTIONS
    | "TRACE" -> Some TRACE
    | _ -> None

  let to_string = function
    | GET -> "GET"
    | HEAD -> "HEAD"
    | POST -> "POST"
    | PATCH -> "PATCH"
    | PUT -> "PUT"
    | DELETE -> "DELETE"
    | CONNECT -> "CONNECT"
    | OPTIONS -> "OPTIONS"
    | TRACE -> "TRACE"
end

type http_method =
  Http_method.t =
  | GET
  | HEAD
  | POST
  | PATCH
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE

type request_target =
  | Origin of {
      path : string;
      query : string option;
    }
  | Absolute of {
      uri : string;
    }
  | Authority of {
      authority : string;
    }
  | Asterisk

type request = {
  request_method : http_method;
  raw_target : string;
  target : request_target;
  headers : (string * string) list;
  body : string;
}

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
}

type response_head = {
  status : int;
  headers : (string * string) list;
}

type informational_response = {
  status : int;
  headers : (string * string) list;
}

type request_patch = {
  request_method : http_method option;
  raw_target : string option;
  headers : (string * string) list option;
  body : string option;
}

type request_view =
  | Owned_request of request
  | Borrowed_request of borrowed_request

type handler = request_view -> response
type middleware = handler -> handler

type pre_result =
  | Continue
  | Respond of response
  | Rewrite of request_patch

type pre_middleware = request_view -> pre_result

type path_state = {
  segments : string array;
  index : int;
}

type queryparam = {
  name : string;
  value : string;
}

type input = {
  request_view : request_view;
  request_method : http_method option;
  path : path_state;
  queryparams : queryparam list;
}

type route_match =
  | Route_matched of handler * middleware list
  | Route_rejected of response * middleware list
  | Route_missing

type route_eval = {
  route_match : route_match;
  effective_input : input;
}

type route_result = input -> route_eval

type t = {
  run : route_result;
  middleware : middleware list;
  pre_middleware : pre_middleware list;
}

let normalize_header_name name = String.lowercase_ascii name

let fold_header_list headers ~init ~f =
  List.fold_left (fun acc (name, value) -> f acc name value) init headers

let header_from_list headers wanted =
  let wanted = normalize_header_name wanted in
  List.find_map
    (fun (name, value) ->
      if String.equal (normalize_header_name name) wanted then
        Some value
      else
        None)
    headers

let headers_from_list headers wanted =
  let wanted = normalize_header_name wanted in
  List.filter_map
    (fun (name, value) ->
      if String.equal (normalize_header_name name) wanted then
        Some value
      else
        None)
    headers

let queryparam_from_list queryparams wanted =
  List.find_map
    (fun queryparam ->
      if String.equal queryparam.name wanted then
        Some queryparam.value
      else
        None)
    queryparams

let queryparams_from_list queryparams wanted =
  List.filter_map
    (fun queryparam ->
      if String.equal queryparam.name wanted then
        Some queryparam.value
      else
        None)
    queryparams

let http_method_of_string = Http_method.of_string

let string_of_http_method = Http_method.to_string

let parse_request_target request_method raw_target =
  match raw_target with
  | "*" -> Asterisk
  | _ when request_method = CONNECT -> Authority { authority = raw_target }
  | _ when String.length raw_target > 0 && raw_target.[0] = '/' ->
      let path, query =
        match String.index_opt raw_target '?' with
        | Some index ->
            ( String.sub raw_target 0 index,
              Some
                (String.sub raw_target (index + 1)
                   (String.length raw_target - index - 1)) )
        | None -> (raw_target, None)
      in
      Origin { path; query }
  | _ -> Absolute { uri = raw_target }

module Req = struct
  type _ t =
    | Request_method_text : string t
    | Request_method : http_method option t
    | Raw_target : string t
    | Target : request_target t
    | Body : string t
    | Copy_headers : (string * string) list t
    | Raw_copy_headers : (string * string) list t
    | Materialize : request t

  let request_method_text = Request_method_text
  let request_method = Request_method
  let target = Target
  let materialize = Materialize

  let rec eval : type a. request_view -> a t -> a =
   fun request_view program ->
    match program with
    | Request_method_text -> (
        match request_view with
        | Owned_request request -> string_of_http_method request.request_method
        | Borrowed_request request -> borrowed_request_method_text request)
    | Request_method ->
        http_method_of_string (eval request_view Request_method_text)
    | Raw_target -> (
        match request_view with
        | Owned_request request -> request.raw_target
        | Borrowed_request request -> borrowed_request_target request)
    | Target ->
        parse_request_target
          (match eval request_view Request_method with
          | Some request_method -> request_method
          | None ->
              invalid_arg "cannot parse request target with unknown HTTP method")
          (eval request_view Raw_target)
    | Body -> (
        match request_view with
        | Owned_request request -> request.body
        | Borrowed_request _request -> Effect.perform Drain_request_body)
    | Copy_headers -> (
        match request_view with
        | Owned_request request -> request.headers
        | Borrowed_request request -> borrowed_request_copy_headers request)
    | Raw_copy_headers -> (
        match request_view with
        | Owned_request request -> request.headers
        | Borrowed_request request -> borrowed_request_copy_raw_headers request)
    | Materialize ->
        let request_method_text = eval request_view Request_method_text in
        (match http_method_of_string request_method_text with
        | Some request_method ->
            {
              request_method;
              raw_target = eval request_view Raw_target;
              target = eval request_view Target;
              headers = eval request_view Copy_headers;
              body = eval request_view Body;
            }
        | None ->
            invalid_arg "cannot materialize request with unknown HTTP method")
end

let split_path path =
  let pieces =
    String.split_on_char '/' path |> List.filter (fun piece -> piece <> "")
  in
  Array.of_list pieces

let parse_queryparam_piece piece =
  match String.index_opt piece '=' with
  | Some index ->
      Some
        {
          name = String.sub piece 0 index;
          value =
            String.sub piece (index + 1) (String.length piece - index - 1);
        }
  | None -> Some { name = piece; value = "" }

let parse_queryparams = function
  | Origin { query = Some query; _ } ->
      query
      |> String.split_on_char '&'
      |> List.filter (fun piece -> piece <> "")
      |> List.filter_map parse_queryparam_piece
  | Origin { query = None; _ } | Absolute _ | Authority _ | Asterisk -> []

let split_target = function
  | Origin { path; _ } -> split_path path
  | Absolute _ | Authority _ | Asterisk -> [||]

let materialize_request request_view =
  Req.eval request_view Req.materialize

let input_of_request_view request_view =
  {
    request_view;
    request_method = Req.eval request_view Req.request_method;
    path = { segments = split_target (Req.eval request_view Req.target); index = 0 };
    queryparams = parse_queryparams (Req.eval request_view Req.target);
  }

let apply_request_patch request_view (patch : request_patch) =
  let request = materialize_request request_view in
  let request_method =
    match patch.request_method with
    | Some request_method -> request_method
    | None -> request.request_method
  in
  let raw_target =
    match patch.raw_target with
    | Some raw_target -> raw_target
    | None -> request.raw_target
  in
  let headers =
    match patch.headers with
    | Some headers -> headers
    | None -> request.headers
  in
  let body =
    match patch.body with
    | Some body -> body
    | None -> request.body
  in
  Owned_request
    {
      request_method;
      raw_target;
      target = parse_request_target request_method raw_target;
      headers;
      body;
    }

module Request = struct
  type t = request_view

  let request_method_text request =
    Req.eval request Req.request_method_text

  let request_method request =
    Req.eval request Req.request_method

  let raw_target request =
    Req.eval request Req.Raw_target

  let target request =
    Req.eval request Req.Target

  let body request =
    Req.eval request Req.Body

  let header request name =
    match request with
    | Owned_request request -> header_from_list request.headers name
    | Borrowed_request request -> borrowed_request_header request name

  let headers request name =
    match request with
    | Owned_request request -> headers_from_list request.headers name
    | Borrowed_request request -> borrowed_request_headers request name

  let fold_headers request ~init ~f =
    match request with
    | Owned_request request -> fold_header_list request.headers ~init ~f
    | Borrowed_request request -> borrowed_request_fold_headers request init f

  let copy_headers request =
    Req.eval request Req.Copy_headers

  let materialize request =
    materialize_request request
end

module Matcher = struct
  type 'a match_result =
    | Match of 'a * input
    | Reject of response
    | No_match

  type 'a t = input -> 'a match_result

  let pure value input = Match (value, input)

  let map f matcher input =
    match matcher input with
    | Match (value, next) -> Match (f value, next)
    | Reject response -> Reject response
    | No_match -> No_match

  let both left right input =
    match left input with
    | Match (left_value, next) ->
        (match right next with
        | Match (right_value, final) -> Match ((left_value, right_value), final)
        | Reject response -> Reject response
        | No_match -> No_match)
    | Reject response -> Reject response
    | No_match -> No_match

  let apply mf mx =
    map (fun (f, x) -> f x) (both mf mx)

  let ( let+ ) matcher f = map f matcher
  let ( and+ ) left right = both left right
  let ( <*> ) = apply

  let request_method_is expected input =
    if input.request_method = Some expected then
      Match ((), input)
    else
      No_match

  let next_segment input =
    if input.path.index >= Array.length input.path.segments then
      None
    else
      Some input.path.segments.(input.path.index)

  let advance input =
    {
      input with
      path = { input.path with index = input.path.index + 1 };
    }

  let path_segment expected input =
    match next_segment input with
    | Some segment when String.equal segment expected -> Match ((), advance input)
    | _ -> No_match

  let string_parameter input =
    match next_segment input with
    | Some segment -> Match (segment, advance input)
    | None -> No_match

  let int_parameter input =
    match string_parameter input with
    | Match (segment, next) -> (
        match int_of_string_opt segment with
        | Some value -> Match (value, next)
        | None -> No_match)
    | Reject response -> Reject response
    | No_match -> No_match

  let queryparam name input =
    Match (queryparam_from_list input.queryparams name, input)

  let queryparams name input =
    Match (queryparams_from_list input.queryparams name, input)

  let queryparam_required name input =
    match queryparam_from_list input.queryparams name with
    | Some value -> Match (value, input)
    | None -> No_match

  let int_queryparam name input =
    match queryparam_from_list input.queryparams name with
    | Some value -> Match (int_of_string_opt value, input)
    | None -> Match (None, input)

  let int_queryparam_required name input =
    match queryparam_from_list input.queryparams name with
    | Some value -> (
        match int_of_string_opt value with
        | Some parsed -> Match (parsed, input)
        | None -> No_match)
    | None -> No_match

  let path_end input =
    if input.path.index = Array.length input.path.segments then
      Match ((), input)
    else
      No_match

  let require_body input =
    let has_defined_length =
      match input.request_view with
      | Owned_request request ->
          List.exists
            (fun (name, _value) ->
              let name = String.lowercase_ascii name in
              String.equal name "content-length"
              || String.equal name "transfer-encoding")
            request.headers
      | Borrowed_request request ->
          borrowed_request_has_raw_header request "content-length"
          || borrowed_request_has_raw_header request "transfer-encoding"
    in
    if has_defined_length then
      Match ((), input)
    else
      Reject
        {
          status = 411;
          headers = [("Content-Type", "text/plain; charset=utf-8")];
          body = "length required\n";
        }
end

let to_route_result matcher input =
  let route_match =
    match matcher input with
    | Matcher.Match (handler, final)
      when final.path.index = Array.length final.path.segments ->
        Route_matched (handler, [])
    | Matcher.Reject response ->
        Route_rejected (response, [])
    | Matcher.Match _ | Matcher.No_match ->
        Route_missing
  in
  { route_match; effective_input = input }

let route matcher =
  { run = to_route_result matcher; middleware = []; pre_middleware = [] }

let attach_middleware middleware = function
  | Route_matched (handler, existing) ->
      Route_matched (handler, middleware @ existing)
  | Route_rejected (response, existing) ->
      Route_rejected (response, middleware @ existing)
  | Route_missing -> Route_missing

let attach_middleware_eval middleware evaluation =
  { evaluation with route_match = attach_middleware middleware evaluation.route_match }

type pre_run_result =
  | Pre_continue
  | Pre_respond of response

let rec run_pre_middleware middleware request_view rewritten =
  match middleware with
  | [] -> Pre_continue, request_view, rewritten
  | middleware :: rest -> (
      match middleware request_view with
      | Continue -> run_pre_middleware rest request_view rewritten
      | Respond response -> Pre_respond response, request_view, rewritten
      | Rewrite patch ->
          let rewritten_request = apply_request_patch request_view patch in
          run_pre_middleware rest rewritten_request true)

let run_router router input =
  let pre_result, request_view, rewritten =
    run_pre_middleware (List.rev router.pre_middleware) input.request_view false
  in
  let effective_input =
    if rewritten then
      input_of_request_view request_view
    else
      input
  in
  match pre_result with
  | Pre_respond response ->
      {
        route_match = Route_rejected (response, router.middleware);
        effective_input;
      }
  | Pre_continue ->
      router.run effective_input |> attach_middleware_eval router.middleware

let scope prefix_matcher router =
  let run input =
    match prefix_matcher input with
    | Matcher.Match ((), next_input) -> run_router router next_input
    | Matcher.Reject response ->
        { route_match = Route_rejected (response, []); effective_input = input }
    | Matcher.No_match ->
        { route_match = Route_missing; effective_input = input }
  in
  { run; middleware = []; pre_middleware = [] }

let scope_path_segment segment router =
  scope Matcher.(path_segment segment) router

let use_pre middleware router =
  { router with pre_middleware = middleware :: router.pre_middleware }

let use middleware router =
  { router with middleware = middleware :: router.middleware }

let empty =
  {
    run = (fun input -> { route_match = Route_missing; effective_input = input });
    middleware = [];
    pre_middleware = [];
  }

let run_or_else left right input =
  match run_router left input with
  | ({ route_match = Route_matched _ | Route_rejected _; _ } as evaluation) ->
      evaluation
  | { route_match = Route_missing; _ } -> run_router right input

let or_else left right =
  { run = run_or_else left right; middleware = []; pre_middleware = [] }

let ( <|> ) = or_else

let all_http_methods = [GET; HEAD; POST; PATCH; PUT; DELETE; CONNECT; OPTIONS; TRACE]

let response_head status headers : response_head = { status; headers }

let not_found_response () =
  {
    status = 404;
    headers = [("Content-Type", "text/plain; charset=utf-8")];
    body = "not found\n";
  }

let method_not_allowed_response allowed_methods =
  {
    status = 405;
    headers =
      [
        ("Allow", String.concat ", " (List.map string_of_http_method allowed_methods));
        ("Content-Type", "text/plain; charset=utf-8");
      ];
    body = "method not allowed\n";
  }

let allowed_methods_for_path router input =
  let matches_method method_ =
    match (router.run { input with request_method = Some method_ }).route_match with
    | Route_matched _ | Route_rejected _ -> true
    | Route_missing -> false
  in
  all_http_methods
  |> List.filter matches_method

let apply_middleware middleware handler =
  List.fold_right (fun wrap next -> wrap next) middleware handler

let dispatch_from_input router input =
  let evaluation = run_router router input in
  let effective_input = evaluation.effective_input in
  let fallback_handler _request =
    match effective_input.request_method with
    | Some _request_method -> (
        match allowed_methods_for_path router effective_input with
        | [] -> not_found_response ()
        | allowed_methods -> method_not_allowed_response allowed_methods)
    | None -> not_found_response ()
  in
  match evaluation.route_match with
  | Route_matched (handler, middleware) ->
      apply_middleware middleware handler effective_input.request_view
  | Route_rejected (response, middleware) ->
      apply_middleware middleware (fun _request -> response)
        effective_input.request_view
  | Route_missing ->
      apply_middleware router.middleware fallback_handler
        effective_input.request_view

let dispatch router request =
  dispatch_from_input router
    {
      request_view = Owned_request request;
      request_method = Some request.request_method;
      path = { segments = split_target request.target; index = 0 };
      queryparams = parse_queryparams request.target;
    }

let with_header name value (response : response) =
  let normalized_name = normalize_header_name name in
  let headers =
    (name, value)
    :: List.filter
         (fun (header_name, _value) ->
           not
             (String.equal (normalize_header_name header_name) normalized_name))
         response.headers
  in
  { response with headers }

let informational status headers =
  if status < 100 || status >= 200 then
    invalid_arg "informational response status must be 1xx"
  else
    { status; headers }

let with_content_type content_type (response : response) =
  with_header "Content-Type" content_type response

let response_with_content_type status content_type body =
  {
    status;
    headers = [("Content-Type", content_type)];
    body;
  }

let text status body =
  response_with_content_type status "text/plain; charset=utf-8" body

let html status body =
  response_with_content_type status "text/html; charset=utf-8" body

let json status body =
  response_with_content_type status "application/json; charset=utf-8" body

module Borrowed = struct
  type nonrec borrowed_request = borrowed_request

  let request_method_text request =
    Req.eval (Borrowed_request request) Req.request_method_text

  let request_method request =
    Req.eval (Borrowed_request request) Req.request_method

  let target request =
    Req.eval (Borrowed_request request) Req.Raw_target

  let body request =
    borrowed_request_body request

  let full_body request =
    Effect.perform (Drain_request_body_with_prefix (borrowed_request_body request))

  let header request name =
    borrowed_request_header request name

  let headers request name =
    borrowed_request_headers request name

  let fold_headers request ~init ~f =
    borrowed_request_fold_headers request init f

  let copy_headers request =
    Req.eval (Borrowed_request request) Req.Copy_headers

  let copy_raw_headers request =
    Req.eval (Borrowed_request request) Req.Raw_copy_headers

  let materialize request =
    Req.eval (Borrowed_request request) Req.Materialize

  let dispatch_borrowed router request =
    dispatch_from_input router
      {
        request_view = Borrowed_request request;
        request_method = request_method request;
        path = { segments = split_target (Req.eval (Borrowed_request request) Req.target); index = 0 };
        queryparams = parse_queryparams (Req.eval (Borrowed_request request) Req.target);
      }
end
