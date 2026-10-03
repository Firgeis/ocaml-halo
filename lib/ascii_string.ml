let is_digit = function '0' .. '9' -> true | _ -> false

let is_hex_digit = function
  | '0' .. '9' | 'A' .. 'F' | 'a' .. 'f' -> true
  | _ -> false

let is_lowercase_letter = function 'a' .. 'z' -> true | _ -> false
let is_lower_hex_digit = function '0' .. '9' | 'a' .. 'f' -> true | _ -> false

let is_uri_unreserved = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
  | _ -> false

let is_uri_sub_delim = function
  | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' -> true
  | _ -> false

let is_printable character =
  let code = Char.code character in
  code >= 0x20 && code <= 0x7e

let has_valid_range value ~offset ~length =
  offset >= 0 && length >= 0
  && offset <= String.length value
  && length <= String.length value - offset

let substring_for_all predicate value ~offset ~length =
  if not (has_valid_range value ~offset ~length) then false
  else
    let limit = offset + length in
    let rec loop index =
      index = limit || (predicate value.[index] && loop (index + 1))
    in
    loop offset

let substring_exists predicate value ~offset ~length =
  if not (has_valid_range value ~offset ~length) then false
  else
    let limit = offset + length in
    let rec loop index =
      index < limit && (predicate value.[index] || loop (index + 1))
    in
    loop offset

let is_lower_hex value ~offset ~length =
  substring_for_all is_lower_hex_digit value ~offset ~length

let is_nonzero_lower_hex value ~offset ~length =
  is_lower_hex value ~offset ~length
  && substring_exists (fun character -> character <> '0') value ~offset ~length

let lower_hex_byte value ~offset =
  if not (is_lower_hex value ~offset ~length:2) then None
  else
    let digit character =
      if is_digit character then Char.code character - Char.code '0'
      else Char.code character - Char.code 'a' + 10
    in
    Some ((digit value.[offset] lsl 4) lor digit value.[offset + 1])

let trim_optional_whitespace value =
  let length = String.length value in
  let is_optional_whitespace = function ' ' | '\t' -> true | _ -> false in
  let rec find_start index =
    if index < length && is_optional_whitespace value.[index] then
      find_start (index + 1)
    else index
  in
  let start = find_start 0 in
  let rec find_end index =
    if index > start && is_optional_whitespace value.[index - 1] then
      find_end (index - 1)
    else index
  in
  let end_ = find_end length in
  String.sub value start (end_ - start)
