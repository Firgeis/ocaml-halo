type t

val read : t -> string option
(** Read the next request-body chunk, or [None] at end of body. Calls are
    serialized for each body. *)

val read_all : t -> string
(** Read to the end and return the complete body, including chunks previously
    returned by [read]. *)

val is_complete : t -> bool

module Private : sig
  val create : read:(unit -> string option) -> t
  val of_string : string -> t
end
