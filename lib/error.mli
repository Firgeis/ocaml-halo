type t =
  | Runtime_error of string

val to_string : t -> string
