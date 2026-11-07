(* Strict mode validates HTTP field-value controls and balanced quoted strings.
   Permissive mode preserves the existing Cache-Control recipient behavior. *)
let split_quoted ~strict ~separator value =
  let n = String.length value in
  let rec loop start i quoted escaped acc =
    if i = n then
      if strict && (quoted || escaped) then Error ()
      else Ok (List.rev (String.sub value start (i - start) :: acc))
    else
      let c = value.[i] in
      let code = Char.code c in
      if strict && (code = 127 || (code < 32 && c <> '\t')) then Error ()
      else if escaped then loop start (i + 1) quoted false acc
      else if quoted && c = '\\' then loop start (i + 1) true true acc
      else if c = '"' then loop start (i + 1) (not quoted) false acc
      else if (not quoted) && c = separator then
        loop (i + 1) (i + 1) false false
          (String.sub value start (i - start) :: acc)
      else loop start (i + 1) quoted false acc
  in
  loop 0 0 false false []
