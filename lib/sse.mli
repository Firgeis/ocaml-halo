type event = {
  event : string option;
  data : string list;
  id : string option;
  retry_ms : int option;
}

type message =
  | Event of event
  | Comment of string

val event :
  ?event:string -> ?id:string -> ?retry_ms:int -> string list -> message
val comment : string -> message
val respond :
  ?headers:(string * string) list ->
  (send:(message -> unit) -> unit) ->
  Router.response
