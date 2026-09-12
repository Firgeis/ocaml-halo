(** Dependency-free parsing and validation helpers for ASCII protocol text. *)

val is_digit : char -> bool
val is_lowercase_letter : char -> bool
val is_lower_hex_digit : char -> bool
val is_printable : char -> bool

val is_lower_hex : string -> offset:int -> length:int -> bool
val is_nonzero_lower_hex : string -> offset:int -> length:int -> bool
(** Test that a bounded substring contains only lowercase hexadecimal digits
    and is not all zero. Invalid ranges return [false]. *)

val lower_hex_byte : string -> offset:int -> int option
(** Parse exactly two lowercase hexadecimal digits at [offset]. *)

val trim_optional_whitespace : string -> string
(** Remove spaces and horizontal tabs from both ends. *)
