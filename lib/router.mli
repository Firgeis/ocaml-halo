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
  body : response_body;
}

and response_body =
  | Fixed_body of string
  | Stream_body of (yield:(string -> unit) -> unit)

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

module Request : sig
  type t

  val request_method_text : t -> string
  val request_method : t -> http_method option
  val raw_target : t -> string
  val target : t -> request_target
  val body : t -> Body.t
  val switch : t -> Eio.Switch.t
  val id : t -> int64
  val send_informational : t -> informational_response -> unit
  val on_disconnect : t -> (unit -> unit) -> unit
  val header : t -> string -> string option
  val headers : t -> string -> string list
  val fold_headers : t -> init:'a -> f:('a -> string -> string -> 'a) -> 'a
  val copy_headers : t -> (string * string) list
  val materialize : t -> request
end

type handler = Request.t -> response
type middleware = handler -> handler
type pre_result =
  | Continue
  | Respond of response
  | Rewrite of request_patch

type pre_middleware = Request.t -> pre_result
type t

val string_of_http_method : http_method -> string
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
  val request_method_is : http_method -> unit t
  val path_segment : string -> unit t
  val string_parameter : string t
  val int_parameter : int t
  val queryparam : string -> string option t
  val queryparams : string -> string list t
  val queryparam_required : string -> string t
  val int_queryparam : string -> int option t
  val int_queryparam_required : string -> int t
  val path_end : unit t
  val require_body : unit t
end

val route : handler Matcher.t -> t
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
    t ->
    request ->
    raw_headers:(string * string) list ->
    body:Body.t ->
    request_id:int64 ->
    request_switch:Eio.Switch.t ->
    send_informational:(informational_response -> unit) ->
    on_disconnect:((unit -> unit) -> unit) ->
    response
end

val with_header : string -> string -> response -> response
val informational : int -> (string * string) list -> informational_response
val with_content_type : string -> response -> response
val text : int -> string -> response
val html : int -> string -> response
val json : int -> string -> response
val stream :
  ?headers:(string * string) list ->
  status:int ->
  (yield:(string -> unit) -> unit) ->
  response
