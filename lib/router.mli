module Http_method : sig
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

  val of_string : string -> t option
  val to_string : t -> string
end

type http_method = Http_method.t =
  | GET
  | HEAD
  | POST
  | PATCH
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE

(** Valid HTTP status codes. *)
module Status : sig
  type t
  type error = Invalid_status_code of int

  val of_int : int -> (t, error) result
  (** Preferred constructor for dynamically selected status codes. *)

  val of_int_exn : int -> t
  (** [of_int_exn code] raises [Invalid_argument] unless [code] is between 100
      and 599. *)

  val to_int : t -> int
  val is_informational : t -> bool

  val reason_phrase : t -> string
  (** [reason_phrase status] is empty for a valid unregistered status. *)

  val error_message : error -> string
  val continue : t
  val switching_protocols : t
  val processing : t
  val early_hints : t
  val ok : t
  val created : t
  val accepted : t
  val non_authoritative_information : t
  val no_content : t
  val reset_content : t
  val partial_content : t
  val multi_status : t
  val already_reported : t
  val im_used : t
  val multiple_choices : t
  val moved_permanently : t
  val found : t
  val see_other : t
  val not_modified : t
  val use_proxy : t
  val temporary_redirect : t
  val permanent_redirect : t
  val bad_request : t
  val unauthorized : t
  val payment_required : t
  val forbidden : t
  val not_found : t
  val method_not_allowed : t
  val not_acceptable : t
  val proxy_authentication_required : t
  val request_timeout : t
  val conflict : t
  val gone : t
  val length_required : t
  val precondition_failed : t
  val content_too_large : t
  val uri_too_long : t
  val unsupported_media_type : t
  val range_not_satisfiable : t
  val expectation_failed : t
  val im_a_teapot : t
  val misdirected_request : t
  val unprocessable_content : t
  val locked : t
  val failed_dependency : t
  val too_early : t
  val upgrade_required : t
  val precondition_required : t
  val too_many_requests : t
  val request_header_fields_too_large : t
  val unavailable_for_legal_reasons : t
  val internal_server_error : t
  val not_implemented : t
  val bad_gateway : t
  val service_unavailable : t
  val gateway_timeout : t
  val http_version_not_supported : t
  val variant_also_negotiates : t
  val insufficient_storage : t
  val loop_detected : t
  val not_extended : t
  val network_authentication_required : t
end

(** Valid response-header collections. Field names follow the HTTP token
    grammar. Field values reject CR, LF, NUL, DEL, and other prohibited control
    characters. *)
module Headers : sig
  type t
  type error = Invalid_name of string | Invalid_value of { name : string }

  val empty : t

  val sse_defaults : t
  (** Default response headers for a server-sent event stream. *)

  val of_list : (string * string) list -> (t, error) result
  (** Preferred constructor when fields are not already validated. *)

  val of_list_exn : (string * string) list -> t
  (** [of_list_exn fields] raises [Invalid_argument] on the first invalid field.
  *)

  val to_list : t -> (string * string) list
  val add : string -> string -> t -> (t, error) result
  val add_exn : string -> string -> t -> t
  val set : string -> string -> t -> (t, error) result
  val set_exn : string -> string -> t -> t
  val append : t -> t -> t
  val iter : (string -> string -> unit) -> t -> unit
  val is_valid_name : string -> bool
  val is_valid_value : string -> bool
  val error_message : error -> string
end

type request_target =
  | Origin of { path : string; query : string option }
  | Absolute of { uri : string }
  | Authority of { authority : string }
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
(** Negotiated TLS metadata. [server_name] is the SNI name sent by the client,
    and [application_protocol] is the selected ALPN protocol. *)

type connection_info = {
  remote_address : Unix.sockaddr;
  local_address : Unix.sockaddr;
  tls : tls_info option;
}
(** Connection metadata captured by the server when the request is dispatched.
    [tls] is [None] for plaintext connections. TLS strings are copied only when
    the corresponding value was negotiated or supplied by the client. *)

type response_body =
  | Fixed_body of string
  | Stream_body of (yield:(string -> unit) -> unit)

type response
(** An HTTP response whose status and headers have been validated. *)

type informational_response
(** A validated 1xx response. *)

type response_outcome =
  | Completed
  | Aborted of exn * Printexc.raw_backtrace
  | Client_disconnected
      (** The terminal transport outcome of a response. [Completed] means the
          final response write completed. [Aborted] means Halo aborted a
          committed stream after a producer failure. [Client_disconnected] means
          the peer or transport disappeared before the response completed. *)

module Response : sig
  type t = response

  val status : t -> Status.t
  val headers : t -> Headers.t
  val body : t -> response_body
end

module Informational_response : sig
  type t = informational_response

  val status : t -> Status.t
  val headers : t -> Headers.t
end

type request_patch = {
  request_method : http_method option;
  raw_target : string option;
  headers : (string * string) list option;
  body : string option;
}

module Request : sig
  type t

  val request_method_text : t -> string
  val request_method : t -> http_method option
  val raw_target : t -> string
  val target : t -> request_target
  val body : t -> Body.t

  val http_version : t -> int * int
  (** The HTTP protocol major and minor version received on the wire. Raises
      [Invalid_argument] for a synthetic request passed to [dispatch]. *)

  val connection_info : t -> connection_info
  (** Network and optional TLS metadata for the connection. Raises
      [Invalid_argument] for a synthetic request passed to [dispatch]. *)

  val remote_address : t -> Unix.sockaddr
  val local_address : t -> Unix.sockaddr
  val is_tls : t -> bool

  val tls : t -> tls_info option
  (** [tls request] is [None] for plaintext and synthetic requests. *)

  val is_early_data : t -> bool
  (** Whether any bytes of this request arrived as TLS 1.3 early data. *)

  val switch : t -> Eio.Switch.t
  val id : t -> int64
  val send_informational : t -> informational_response -> unit
  val on_disconnect : t -> (unit -> unit) -> unit

  val on_response_terminated : t -> (response_outcome -> unit) -> unit
  (** Register a callback that runs exactly once when the response reaches a
      terminal transport outcome. This includes buffered responses and streams,
      and the callback may run after the handler has returned. *)

  val header : t -> string -> string option
  val headers : t -> string -> string list
  val fold_headers : t -> init:'a -> f:('a -> string -> string -> 'a) -> 'a

  val copy_headers : t -> (string * string) list
  (** Server header lists are decoded on first access and cached. Returned
      strings remain valid after the connection closes or serves another
      request. *)

  val materialize : t -> request
  (** Copy request metadata and consume the unread request body. Body bytes
      already returned by [Body.read] are not included. *)
end

type handler = Request.t -> response
type middleware = handler -> handler
type pre_result = Continue | Respond of response | Rewrite of request_patch
type pre_middleware = Request.t -> pre_result
type t

val string_of_http_method : http_method -> string
val string_of_tls_protocol_version : tls_protocol_version -> string
val parse_request_target : http_method -> string -> request_target

module Matcher : sig
  type 'a t

  val pure : 'a -> 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  val ( *> ) : 'a t -> 'b t -> 'b t
  (** [left *> right] runs [left], discards its value, then runs [right]. *)

  val ( <* ) : 'a t -> 'b t -> 'a t
  (** [left <* right] runs [left], then [right], and discards the value from
      [right]. *)

  val request_method_is : http_method -> unit t

  val path_segment : string -> unit t
  (** Match one path segment. The initial slash is not a segment, while empty
      segments created by repeated or trailing slashes are significant. The root
      path [/] has no segments. *)

  val string_parameter : string t
  val int_parameter : int t

  val queryparam : string -> string option t
  (** Query parameter names and values are percent-decoded as URI octets per RFC
      3986. A literal [+] remains [+]; form-urlencoded decoding is a separate
      convention. Invalid URI encoding produces [400 Bad Request]. *)

  val queryparams : string -> string list t
  val queryparam_required : string -> string t
  val int_queryparam : string -> int option t
  val int_queryparam_required : string -> int t
  val path_end : unit t
  val require_body : unit t
end

val route : ?template:string -> handler Matcher.t -> t
(** [route ~template matcher] associates the matched route with a stable,
    low-cardinality template such as [/users/{id}]. Telemetry backends receive
    this value as [http.route]. Do not pass the request's concrete path. *)

val scope : unit Matcher.t -> t -> t
val scope_path_segment : string -> t -> t
val use_pre : pre_middleware -> t -> t
val use : middleware -> t -> t
val empty : t
val or_else : t -> t -> t
val ( <|> ) : t -> t -> t
val dispatch : t -> request -> response

module Runtime : sig
  val dispatch :
    ?on_route:(string -> unit) ->
    t ->
    request ->
    raw_headers:(string * string) list ->
    body:Body.t ->
    http_major:int ->
    http_minor:int ->
    connection_info:connection_info ->
    is_early_data:bool ->
    request_id:int64 ->
    request_switch:Eio.Switch.t ->
    send_informational:(informational_response -> unit) ->
    on_disconnect:((unit -> unit) -> unit) ->
    on_response_terminated:((response_outcome -> unit) -> unit) ->
    response

  val dispatch_deferred :
    ?on_route:(string -> unit) ->
    t ->
    request_method:http_method ->
    raw_target:string ->
    headers:(string * string) list Lazy.t ->
    raw_headers:(string * string) list Lazy.t ->
    body:Body.t ->
    http_major:int ->
    http_minor:int ->
    connection_info:connection_info ->
    is_early_data:bool ->
    request_id:int64 ->
    request_switch:Eio.Switch.t ->
    send_informational:(informational_response -> unit) ->
    on_disconnect:((unit -> unit) -> unit) ->
    on_response_terminated:((response_outcome -> unit) -> unit) ->
    response
  (** Internal server entry point that preserves deferred header decoding. *)
end

val make : ?headers:Headers.t -> status:Status.t -> string -> response
(** Final-response constructors and modifiers raise [Invalid_argument] when
    given an informational status. *)

val with_status : Status.t -> response -> response
val with_headers : Headers.t -> response -> response

val with_header : string -> string -> response -> response
(** [with_header name value response] replaces fields named [name]
    case-insensitively. It raises [Invalid_argument] for an invalid field. *)

val informational : Status.t -> Headers.t -> informational_response
(** [informational status headers] raises [Invalid_argument] unless [status] is
    in the 1xx class. *)

val with_content_type : string -> response -> response
val text : Status.t -> string -> response
val html : Status.t -> string -> response
val json : Status.t -> string -> response

val stream :
  ?headers:Headers.t ->
  status:Status.t ->
  (yield:(string -> unit) -> unit) ->
  response
