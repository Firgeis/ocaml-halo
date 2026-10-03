type t = { hostname : string; port : int option }

val parse : string -> t option
val matches : actual:t -> expected:t -> bool
