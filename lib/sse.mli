type event = {
  event : string option;
  data : string list;
  id : string option;
  retry_ms : int option;
}

type message = Event of event | Comment of string

val event :
  ?event:string -> ?id:string -> ?retry_ms:int -> string list -> message

val comment : string -> message

val respond :
  ?headers:Router.Headers.t ->
  (send:(message -> unit) -> unit) ->
  Router.response

val respond_buffered :
  ?headers:Router.Headers.t ->
  ?max_buffered_bytes:int ->
  (send:(message -> unit) -> flush:(unit -> unit) -> unit) ->
  Router.response
(** Buffered SSE using [Stream.respond_buffered]. Call [flush] after an event or
    group of events that must be delivered promptly, before waiting for more.
    [respond] continues to write each message immediately. *)
