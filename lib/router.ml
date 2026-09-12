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

module Status = struct
  type t = int
  type error = Invalid_status_code of int

  let error_message (Invalid_status_code code) =
    Printf.sprintf "invalid HTTP status code %d (expected 100..599)" code

  let of_int code =
    if code >= 100 && code <= 599 then Ok code
    else Error (Invalid_status_code code)

  let of_int_exn code =
    match of_int code with
    | Ok status -> status
    | Error error -> invalid_arg (error_message error)

  let to_int status = status
  let is_informational status = status < 200

  let reason_phrase = function
    | 100 -> "Continue"
    | 101 -> "Switching Protocols"
    | 102 -> "Processing"
    | 103 -> "Early Hints"
    | 200 -> "OK"
    | 201 -> "Created"
    | 202 -> "Accepted"
    | 203 -> "Non-Authoritative Information"
    | 204 -> "No Content"
    | 205 -> "Reset Content"
    | 206 -> "Partial Content"
    | 207 -> "Multi-Status"
    | 208 -> "Already Reported"
    | 226 -> "IM Used"
    | 300 -> "Multiple Choices"
    | 301 -> "Moved Permanently"
    | 302 -> "Found"
    | 303 -> "See Other"
    | 304 -> "Not Modified"
    | 305 -> "Use Proxy"
    | 307 -> "Temporary Redirect"
    | 308 -> "Permanent Redirect"
    | 400 -> "Bad Request"
    | 401 -> "Unauthorized"
    | 402 -> "Payment Required"
    | 403 -> "Forbidden"
    | 404 -> "Not Found"
    | 405 -> "Method Not Allowed"
    | 406 -> "Not Acceptable"
    | 407 -> "Proxy Authentication Required"
    | 408 -> "Request Timeout"
    | 409 -> "Conflict"
    | 410 -> "Gone"
    | 411 -> "Length Required"
    | 412 -> "Precondition Failed"
    | 413 -> "Content Too Large"
    | 414 -> "URI Too Long"
    | 415 -> "Unsupported Media Type"
    | 416 -> "Range Not Satisfiable"
    | 417 -> "Expectation Failed"
    | 418 -> "I'm a Teapot"
    | 421 -> "Misdirected Request"
    | 422 -> "Unprocessable Content"
    | 423 -> "Locked"
    | 424 -> "Failed Dependency"
    | 425 -> "Too Early"
    | 426 -> "Upgrade Required"
    | 428 -> "Precondition Required"
    | 429 -> "Too Many Requests"
    | 431 -> "Request Header Fields Too Large"
    | 451 -> "Unavailable For Legal Reasons"
    | 500 -> "Internal Server Error"
    | 501 -> "Not Implemented"
    | 502 -> "Bad Gateway"
    | 503 -> "Service Unavailable"
    | 504 -> "Gateway Timeout"
    | 505 -> "HTTP Version Not Supported"
    | 506 -> "Variant Also Negotiates"
    | 507 -> "Insufficient Storage"
    | 508 -> "Loop Detected"
    | 510 -> "Not Extended"
    | 511 -> "Network Authentication Required"
    | _ -> ""

  let continue = 100
  let switching_protocols = 101
  let processing = 102
  let early_hints = 103
  let ok = 200
  let created = 201
  let accepted = 202
  let non_authoritative_information = 203
  let no_content = 204
  let reset_content = 205
  let partial_content = 206
  let multi_status = 207
  let already_reported = 208
  let im_used = 226
  let multiple_choices = 300
  let moved_permanently = 301
  let found = 302
  let see_other = 303
  let not_modified = 304
  let use_proxy = 305
  let temporary_redirect = 307
  let permanent_redirect = 308
  let bad_request = 400
  let unauthorized = 401
  let payment_required = 402
  let forbidden = 403
  let not_found = 404
  let method_not_allowed = 405
  let not_acceptable = 406
  let proxy_authentication_required = 407
  let request_timeout = 408
  let conflict = 409
  let gone = 410
  let length_required = 411
  let precondition_failed = 412
  let content_too_large = 413
  let uri_too_long = 414
  let unsupported_media_type = 415
  let range_not_satisfiable = 416
  let expectation_failed = 417
  let im_a_teapot = 418
  let misdirected_request = 421
  let unprocessable_content = 422
  let locked = 423
  let failed_dependency = 424
  let too_early = 425
  let upgrade_required = 426
  let precondition_required = 428
  let too_many_requests = 429
  let request_header_fields_too_large = 431
  let unavailable_for_legal_reasons = 451
  let internal_server_error = 500
  let not_implemented = 501
  let bad_gateway = 502
  let service_unavailable = 503
  let gateway_timeout = 504
  let http_version_not_supported = 505
  let variant_also_negotiates = 506
  let insufficient_storage = 507
  let loop_detected = 508
  let not_extended = 510
  let network_authentication_required = 511
end

module Headers = struct
  type t = (string * string) list

  type error =
    | Invalid_name of string
    | Invalid_value of { name : string }

  let is_tchar = function
    | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '.' | '^' | '_'
    | '`' | '|' | '~' ->
        true
    | '0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' -> true
    | _ -> false

  let is_valid_name name =
    String.length name > 0 && String.for_all is_tchar name

  let is_valid_value value =
    String.for_all
      (fun character ->
        let code = Char.code character in
        code = 0x09 || (code >= 0x20 && code <> 0x7f))
      value

  let validate name value =
    if not (is_valid_name name) then Error (Invalid_name name)
    else if not (is_valid_value value) then Error (Invalid_value { name })
    else Ok ()

  let error_message = function
    | Invalid_name name -> Printf.sprintf "invalid HTTP header name %S" name
    | Invalid_value { name } ->
        Printf.sprintf "invalid value for HTTP header %S" name

  let empty = []

  let sse_defaults =
    [
      ("Content-Type", "text/event-stream");
      ("Cache-Control", "no-cache");
      ("X-Accel-Buffering", "no");
    ]

  let of_list fields =
    let rec validate_all = function
      | [] -> Ok fields
      | (name, value) :: rest -> (
          match validate name value with
          | Ok () -> validate_all rest
          | Error _ as error -> error)
    in
    validate_all fields

  let of_list_exn fields =
    match of_list fields with
    | Ok headers -> headers
    | Error error -> invalid_arg (error_message error)

  let to_list headers = headers

  let add name value headers =
    match validate name value with
    | Ok () -> Ok ((name, value) :: headers)
    | Error _ as error -> error

  let add_exn name value headers =
    match add name value headers with
    | Ok headers -> headers
    | Error error -> invalid_arg (error_message error)

  let set name value headers =
    match validate name value with
    | Error _ as error -> error
    | Ok () ->
        let normalized_name = String.lowercase_ascii name in
        Ok
          ((name, value)
          :: List.filter
               (fun (existing_name, _value) ->
                 not
                   (String.equal
                      (String.lowercase_ascii existing_name)
                      normalized_name))
               headers)

  let set_exn name value headers =
    match set name value headers with
    | Ok headers -> headers
    | Error error -> invalid_arg (error_message error)

  let append first second = first @ second
  let iter f headers = List.iter (fun (name, value) -> f name value) headers
end

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

type tls_protocol_version =
  | SSL_2
  | SSL_3
  | TLS_1_0
  | TLS_1_1
  | TLS_1_2
  | TLS_1_3
  | Unknown_tls_protocol_version of int

type tls_info = {
  server_name : string option;
  application_protocol : string option;
  protocol_version : tls_protocol_version;
  cipher : string option;
}

type connection_info = {
  remote_address : Unix.sockaddr;
  local_address : Unix.sockaddr;
  tls : tls_info option;
}

type response_body =
  | Fixed_body of string
  | Stream_body of (yield:(string -> unit) -> unit)

type response = {
  status : Status.t;
  headers : Headers.t;
  body : response_body;
}

type informational_response = {
  status : Status.t;
  headers : Headers.t;
}

type response_outcome =
  | Completed
  | Aborted of exn * Printexc.raw_backtrace
  | Client_disconnected

module Response = struct
  type t = response

  let status (response : response) = response.status
  let headers (response : response) = response.headers
  let body (response : response) = response.body
end

module Informational_response = struct
  type t = informational_response

  let status (response : informational_response) = response.status
  let headers (response : informational_response) = response.headers
end

type request_patch = {
  request_method : http_method option;
  raw_target : string option;
  headers : (string * string) list option;
  body : string option;
}

type request_context = {
  body : Body.t;
  http_major : int;
  http_minor : int;
  connection_info : connection_info option;
  is_early_data : bool;
  request_id : int64 option;
  request_switch : Eio.Switch.t option;
  send_informational : informational_response -> unit;
  on_disconnect : (unit -> unit) -> unit;
  on_response_terminated : (response_outcome -> unit) -> unit;
}

type request_view = {
  request_method_text : string;
  request_method : http_method option;
  raw_target : string;
  target : request_target;
  headers : (string * string) list;
  raw_headers : (string * string) list;
  context : request_context;
}

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
  valid_target : bool;
}

type route_match =
  | Route_matched of handler * middleware list * string option
  | Route_rejected of response * middleware list * string option
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

let string_of_tls_protocol_version = function
  | SSL_2 -> "SSLv2"
  | SSL_3 -> "SSLv3"
  | TLS_1_0 -> "TLSv1.0"
  | TLS_1_1 -> "TLSv1.1"
  | TLS_1_2 -> "TLSv1.2"
  | TLS_1_3 -> "TLSv1.3"
  | Unknown_tls_protocol_version version ->
      Printf.sprintf "unknown(%d)" version

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
    | Materialize : request t

  let request_method_text = Request_method_text
  let request_method = Request_method
  let target = Target
  let materialize = Materialize

  let rec eval : type a. request_view -> a t -> a =
   fun request_view program ->
    match program with
    | Request_method_text -> request_view.request_method_text
    | Request_method -> request_view.request_method
    | Raw_target -> request_view.raw_target
    | Target -> request_view.target
    | Body -> Body.read_all request_view.context.body
    | Copy_headers -> request_view.headers
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
  if String.equal path "" || String.equal path "/" then
    [||]
  else
    let offset = if path.[0] = '/' then 1 else 0 in
    String.sub path offset (String.length path - offset)
    |> String.split_on_char '/'
    |> Array.of_list

let is_alpha = function
  | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_hexdig character =
  is_digit character
  ||
  match character with
  | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

let is_unreserved = function
  | '-' | '.' | '_' | '~' -> true
  | character -> is_alpha character || is_digit character

let is_sub_delim = function
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
  | _ -> false

let is_pchar character =
  is_unreserved character
  || is_sub_delim character
  || character = ':'
  || character = '@'

let validate_encoded_component ~is_literal_allowed value =
  let length = String.length value in
  let rec scan_from index =
    if index = length then
      true
    else
      match value.[index] with
      | '%' ->
          index + 2 < length
          && is_hexdig value.[index + 1]
          && is_hexdig value.[index + 2]
          && scan_from (index + 3)
      | character -> is_literal_allowed character && scan_from (index + 1)
  in
  scan_from 0

let validate_path path =
  String.length path > 0
  && path.[0] = '/'
  && validate_encoded_component path ~is_literal_allowed:(fun character ->
         character = '/' || is_pchar character)

let validate_query query =
  validate_encoded_component query ~is_literal_allowed:(fun character ->
      character = '/' || character = '?' || is_pchar character)

let parse_queryparam_piece piece =
  match String.index_opt piece '=' with
  | Some index ->
      {
        name = Uri.pct_decode (String.sub piece 0 index);
        value =
          Uri.pct_decode
            (String.sub piece (index + 1) (String.length piece - index - 1));
      }
  | None -> { name = Uri.pct_decode piece; value = "" }

let parse_queryparams = function
  | Origin { query = Some query; _ } ->
      if validate_query query then
        query
        |> String.split_on_char '&'
        |> List.filter (fun piece -> piece <> "")
        |> List.map parse_queryparam_piece
      else
        []
  | Origin { query = None; _ } | Absolute _ | Authority _ | Asterisk -> []

let validate_request_target = function
  | Origin { path; query } ->
      validate_path path && Option.fold ~none:true ~some:validate_query query
  | Absolute _ | Authority _ | Asterisk -> true

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
    valid_target = validate_request_target (Req.eval request_view Req.target);
  }

let apply_request_patch (request_view : request_view) (patch : request_patch) =
  let request_method =
    match patch.request_method with
    | Some request_method -> request_method
    | None -> (
        match request_view.request_method with
        | Some request_method -> request_method
        | None -> invalid_arg "cannot rewrite a request with an unknown method")
  in
  let raw_target =
    match patch.raw_target with
    | Some raw_target -> raw_target
    | None -> request_view.raw_target
  in
  let headers =
    match patch.headers with
    | Some headers -> headers
    | None -> request_view.headers
  in
  let body =
    match patch.body with
    | Some body -> Body.Private.of_string body
    | None -> request_view.context.body
  in
  {
    request_method_text = string_of_http_method request_method;
    request_method = Some request_method;
    raw_target;
    target = parse_request_target request_method raw_target;
    headers;
    raw_headers = headers;
    context = { request_view.context with body };
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

  let body request = request.context.body

  let http_version request =
    match request.context.connection_info with
    | Some _ -> (request.context.http_major, request.context.http_minor)
    | None -> invalid_arg "HTTP version is unavailable outside a server handler"

  let connection_info request =
    match request.context.connection_info with
    | Some connection_info -> connection_info
    | None ->
        invalid_arg "connection info is unavailable outside a server handler"

  let remote_address request = (connection_info request).remote_address
  let local_address request = (connection_info request).local_address
  let tls request =
    match request.context.connection_info with
    | Some connection_info -> connection_info.tls
    | None -> None

  let is_tls request = Option.is_some (tls request)

  let is_early_data request = request.context.is_early_data

  let switch request =
    match request.context.request_switch with
    | Some switch -> switch
    | None -> invalid_arg "request switch is unavailable outside a server handler"

  let id request =
    match request.context.request_id with
    | Some request_id -> request_id
    | None -> invalid_arg "request ID is unavailable outside a server handler"

  let send_informational request response =
    request.context.send_informational response

  let on_disconnect request cleanup = request.context.on_disconnect cleanup

  let on_response_terminated request callback =
    request.context.on_response_terminated callback

  let header request name = header_from_list request.headers name

  let headers request name = headers_from_list request.headers name

  let fold_headers request ~init ~f =
    fold_header_list request.headers ~init ~f

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

  let ( *> ) left right input =
    match left input with
    | Match (_, next) -> right next
    | Reject response -> Reject response
    | No_match -> No_match

  let ( <* ) left right input =
    match left input with
    | Match (value, next) -> (
        match right next with
        | Match (_, final) -> Match (value, final)
        | Reject response -> Reject response
        | No_match -> No_match)
    | Reject response -> Reject response
    | No_match -> No_match

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
      List.exists
        (fun (name, _value) ->
          let name = String.lowercase_ascii name in
          String.equal name "content-length"
          || String.equal name "transfer-encoding")
        input.request_view.raw_headers
    in
    if has_defined_length then
      Match ((), input)
    else
      Reject
        {
          status = Status.length_required;
          headers =
            Headers.of_list_exn
              [("Content-Type", "text/plain; charset=utf-8")];
          body = Fixed_body "length required\n";
        }
end

let to_route_result template matcher input =
  let route_match =
    match matcher input with
    | Matcher.Match (handler, final)
      when final.path.index = Array.length final.path.segments ->
        Route_matched (handler, [], template)
    | Matcher.Reject response ->
        Route_rejected (response, [], template)
    | Matcher.Match _ | Matcher.No_match ->
        Route_missing
  in
  { route_match; effective_input = input }

let route ?template matcher =
  { run = to_route_result template matcher; middleware = []; pre_middleware = [] }

let attach_middleware middleware = function
  | Route_matched (handler, existing, template) ->
      Route_matched (handler, middleware @ existing, template)
  | Route_rejected (response, existing, template) ->
      Route_rejected (response, middleware @ existing, template)
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
  match router.pre_middleware, router.middleware with
  | [], [] -> router.run input
  | _ ->
      let pre_result, request_view, rewritten =
        run_pre_middleware (List.rev router.pre_middleware) input.request_view
          false
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
            route_match = Route_rejected (response, router.middleware, None);
            effective_input;
          }
      | Pre_continue ->
          router.run effective_input |> attach_middleware_eval router.middleware

let scope prefix_matcher router =
  let run input =
    match prefix_matcher input with
    | Matcher.Match ((), next_input) -> run_router router next_input
    | Matcher.Reject response ->
        {
          route_match = Route_rejected (response, [], None);
          effective_input = input;
        }
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

let not_found_response () =
  {
    status = Status.not_found;
    headers =
      Headers.of_list_exn [("Content-Type", "text/plain; charset=utf-8")];
    body = Fixed_body "not found\n";
  }

let bad_request_response () =
  {
    status = Status.bad_request;
    headers =
      Headers.of_list_exn [("Content-Type", "text/plain; charset=utf-8")];
    body = Fixed_body "bad request\n";
  }

let method_not_allowed_response allowed_methods =
  {
    status = Status.method_not_allowed;
    headers =
      Headers.of_list_exn
        [
          ( "Allow",
            String.concat ", " (List.map string_of_http_method allowed_methods) );
          ("Content-Type", "text/plain; charset=utf-8");
        ];
    body = Fixed_body "method not allowed\n";
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

let dispatch_from_input ?(on_route = fun _template -> ()) router input =
  if not input.valid_target then
    bad_request_response ()
  else
    let evaluation = run_router router input in
    let effective_input = evaluation.effective_input in
    if not effective_input.valid_target then
      bad_request_response ()
    else
      let fallback_handler _request =
        match effective_input.request_method with
        | Some _request_method -> (
            match allowed_methods_for_path router effective_input with
            | [] -> not_found_response ()
            | allowed_methods -> method_not_allowed_response allowed_methods)
        | None -> not_found_response ()
      in
      match evaluation.route_match with
      | Route_matched (handler, middleware, template) ->
          Option.iter on_route template;
          apply_middleware middleware handler effective_input.request_view
      | Route_rejected (response, middleware, template) ->
          Option.iter on_route template;
          apply_middleware middleware (fun _request -> response)
            effective_input.request_view
      | Route_missing ->
          apply_middleware router.middleware fallback_handler
            effective_input.request_view

let dispatch router (request : request) =
  let context =
    {
      body = Body.Private.of_string request.body;
      http_major = 0;
      http_minor = 0;
      connection_info = None;
      is_early_data = false;
      request_id = None;
      request_switch = None;
      send_informational =
        (fun _ ->
          invalid_arg "informational responses require a server handler");
      on_disconnect =
        (fun _ -> invalid_arg "disconnect hooks require a server handler");
      on_response_terminated =
        (fun _ ->
          invalid_arg "response lifecycle hooks require a server handler");
    }
  in
  dispatch_from_input router
    {
      request_view =
        {
          request_method_text = string_of_http_method request.request_method;
          request_method = Some request.request_method;
          raw_target = request.raw_target;
          target = request.target;
          headers = request.headers;
          raw_headers = request.headers;
          context;
        };
      request_method = Some request.request_method;
      path = { segments = split_target request.target; index = 0 };
      queryparams = parse_queryparams request.target;
      valid_target = validate_request_target request.target;
    }

module Runtime = struct
  let dispatch ?on_route router (request : request) ~raw_headers ~body ~http_major
      ~http_minor ~connection_info ~is_early_data ~request_id ~request_switch
      ~send_informational ~on_disconnect ~on_response_terminated =
    let request_view =
      {
        request_method_text = string_of_http_method request.request_method;
        request_method = Some request.request_method;
        raw_target = request.raw_target;
        target = request.target;
        headers = request.headers;
        raw_headers;
        context =
          {
            body;
            http_major;
            http_minor;
            connection_info = Some connection_info;
            is_early_data;
            request_id = Some request_id;
            request_switch = Some request_switch;
            send_informational;
            on_disconnect;
            on_response_terminated;
          };
      }
    in
    dispatch_from_input ?on_route router
      {
        request_view;
        request_method = Some request.request_method;
        path = { segments = split_target request.target; index = 0 };
        queryparams = parse_queryparams request.target;
        valid_target = validate_request_target request.target;
      }
end

let ensure_final_status status =
  if Status.is_informational status then
    invalid_arg "final response status must be between 200 and 599"

let make ?(headers = Headers.empty) ~status body =
  ensure_final_status status;
  { status; headers; body = Fixed_body body }

let with_status status (response : response) =
  ensure_final_status status;
  { response with status }

let with_headers headers (response : response) = { response with headers }

let with_header name value (response : response) =
  { response with headers = Headers.set_exn name value response.headers }

let informational status headers =
  if not (Status.is_informational status) then
    invalid_arg "informational response status must be 1xx"
  else
    { status; headers }

let with_content_type content_type (response : response) =
  with_header "Content-Type" content_type response

let response_with_content_type status content_type body =
  make ~status
    ~headers:(Headers.of_list_exn [("Content-Type", content_type)])
    body

let text status body =
  response_with_content_type status "text/plain; charset=utf-8" body

let html status body =
  response_with_content_type status "text/html; charset=utf-8" body

let json status body =
  response_with_content_type status "application/json; charset=utf-8" body

let stream ?(headers = Headers.empty) ~status producer =
  ensure_final_status status;
  { status; headers; body = Stream_body producer }
