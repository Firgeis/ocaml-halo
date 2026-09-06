type producer = yield:(string -> unit) -> unit

let validate_chunk chunk =
  if String.length chunk = 0 then
    invalid_arg "streamed response chunks must not be empty"

let respond ?(headers = Router.Headers.empty) ~status producer =
  Router.stream ~status ~headers (fun ~yield ->
      producer ~yield:(fun chunk ->
          validate_chunk chunk;
          yield chunk))
