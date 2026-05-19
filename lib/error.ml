type t =
  (* Keep this minimal until the OCaml side owns more protocol/runtime state.
     As library-domain failures emerge, extend this type instead of piling more
     meaning into opaque runtime strings. *)
  | Runtime_error of string

let to_string = function
  | Runtime_error message -> message
