type t

val headers : t -> (string * string) list
(** Decode and cache the filtered and wire header lists on first access. The
    snapshot owns its bytes independently of the reactor and connection. *)

val raw_headers : t -> (string * string) list
