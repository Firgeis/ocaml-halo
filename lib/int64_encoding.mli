(** Dependency-free conversions between 64-bit integers and wire strings. *)

val of_lower_hex : string -> offset:int -> int64 option
(** Parse exactly 16 lowercase hexadecimal digits at [offset]. Invalid ranges or
    digits return [None]. *)

val of_big_endian : string -> offset:int -> int64 option
(** Decode exactly eight bytes at [offset] in network byte order. Invalid ranges
    return [None]. *)

val to_big_endian : int64 -> string
(** Encode an integer as eight bytes in network byte order. *)
