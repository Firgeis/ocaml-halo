type producer = yield:(string -> unit) -> unit

val respond :
  ?headers:(string * string) list -> status:int -> producer -> Router.response
