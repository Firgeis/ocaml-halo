type t = { hostname : string; port : int option }

val parse : string -> t option

type parse_error = Malformed | Unsupported

val parse_forwarded : string -> (t, parse_error) result
val matches : actual:t -> expected:t -> bool
