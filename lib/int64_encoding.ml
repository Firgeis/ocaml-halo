let lowercase_hex_value character =
  if Ascii_string.is_digit character then Char.code character - Char.code '0'
  else Char.code character - Char.code 'a' + 10

let of_lower_hex value ~offset =
  if not (Ascii_string.is_lower_hex value ~offset ~length:16) then None
  else
    let result = ref 0L in
    for index = offset to offset + 15 do
      result :=
        Int64.logor
          (Int64.shift_left !result 4)
          (Int64.of_int (lowercase_hex_value value.[index]))
    done;
    Some !result

let of_big_endian value ~offset =
  if offset < 0 || offset > String.length value - 8 then None
  else
    let result = ref 0L in
    for index = offset to offset + 7 do
      result :=
        Int64.logor
          (Int64.shift_left !result 8)
          (Int64.of_int (Char.code value.[index]))
    done;
    Some !result

let to_big_endian value =
  let bytes = Bytes.create 8 in
  for index = 0 to 7 do
    let shift = (7 - index) * 8 in
    Bytes.set bytes index
      (Char.chr
         (Int64.to_int
            (Int64.logand (Int64.shift_right_logical value shift) 0xffL)))
  done;
  Bytes.unsafe_to_string bytes
