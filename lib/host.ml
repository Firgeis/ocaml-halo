type t = { hostname : string; port : int option }

let parse_port value =
  let length = String.length value in
  if
    length = 0 || length > 5 || not (String.for_all Ascii_string.is_digit value)
  then None
  else
    match int_of_string_opt value with
    | Some port when port <= 65535 -> Some port
    | _ -> None

let is_reg_name_char character =
  Ascii_string.is_uri_unreserved character
  || Ascii_string.is_uri_sub_delim character

let valid_reg_name value =
  let length = String.length value in
  let rec loop index =
    if index = length then true
    else if is_reg_name_char value.[index] then loop (index + 1)
    else if
      value.[index] = '%'
      && index + 2 < length
      && Ascii_string.is_hex_digit value.[index + 1]
      && Ascii_string.is_hex_digit value.[index + 2]
    then loop (index + 3)
    else false
  in
  length > 0 && loop 0

let valid_ipv6 value =
  try
    let address = Unix.inet_addr_of_string value in
    String.contains (Unix.string_of_inet_addr address) ':'
  with Failure _ -> false

let parse value =
  let value = String.trim value in
  let length = String.length value in
  let make hostname port =
    Some { hostname = String.lowercase_ascii hostname; port }
  in
  let parse_suffix hostname suffix valid =
    if not (valid hostname) then None
    else if String.equal suffix "" then make hostname None
    else if suffix.[0] <> ':' then None
    else
      match parse_port (String.sub suffix 1 (String.length suffix - 1)) with
      | Some port -> make hostname (Some port)
      | None -> None
  in
  if length = 0 then None
  else if value.[0] = '[' then
    match String.index_opt value ']' with
    | None -> None
    | Some closing ->
        let hostname = String.sub value 1 (closing - 1) in
        let suffix = String.sub value (closing + 1) (length - closing - 1) in
        parse_suffix hostname suffix valid_ipv6
  else
    match String.index_opt value ':' with
    | None -> parse_suffix value "" valid_reg_name
    | Some colon ->
        let hostname = String.sub value 0 colon in
        let suffix = String.sub value colon (length - colon) in
        parse_suffix hostname suffix valid_reg_name

let unsupported_host h =
  match Ipaddr.of_string h with
  | Ok (Ipaddr.V6 _) -> true
  | _ ->
      let len = String.length h in
      if len > 0 && h.[len - 1] = ':' then
        Option.is_some (parse (String.sub h 0 (len - 1)))
      else if len > 3 && h.[0] = '[' && (h.[1] = 'v' || h.[1] = 'V') then
        match String.index_opt h ']' with
        | None -> false
        | Some close -> (
            let suffix = String.sub h (close + 1) (len - close - 1) in
            let suffix_valid =
              suffix = ""
              || suffix.[0] = ':'
                 && String.for_all Ascii_string.is_digit
                      (String.sub suffix 1 (String.length suffix - 1))
            in
            match String.index_from_opt h 2 '.' with
            | Some dot when dot > 2 && dot + 1 < close ->
                suffix_valid
                && String.for_all Ascii_string.is_hex_digit
                     (String.sub h 2 (dot - 2))
                && String.for_all
                     (fun c ->
                       Ascii_string.is_uri_unreserved c
                       || Ascii_string.is_uri_sub_delim c
                       || c = ':')
                     (String.sub h (dot + 1) (close - dot - 1))
            | _ -> false)
      else false

type parse_error = Malformed | Unsupported

let parse_forwarded value =
  match parse value with
  | Some host -> Ok host
  | None -> Error (if unsupported_host value then Unsupported else Malformed)

let matches ~actual ~expected =
  String.equal actual.hostname expected.hostname
  && Option.fold ~none:true
       ~some:(fun port -> actual.port = Some port)
       expected.port
