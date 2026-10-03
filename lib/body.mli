type t

val read : t -> string option
(** Consume and return the next request-body chunk, or [None] at end of body.
    Chunk boundaries reflect delivery batches, not HTTP chunks or TLS records.
    Consumed chunks are not retained. Calls are serialized for each body. *)

val read_all : t -> string
(** Consume and return all unread body data. Bytes previously returned by [read]
    are not included. Returns the empty string when no data remains. *)

val is_complete : t -> bool

module Private : sig
  val create : read:(unit -> string option) -> t
  val of_string : string -> t
end
