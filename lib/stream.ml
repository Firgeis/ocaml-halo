type producer = yield:(string -> unit) -> unit

let validate_chunk chunk =
  if String.length chunk = 0 then
    invalid_arg "streamed response chunks must not be empty"

let respond ?(headers = []) ~status producer =
  Server_effects.start_response_stream (Router.response_head status headers);
  producer ~yield:(fun chunk ->
      validate_chunk chunk;
      Server_effects.check_client_connected ();
      Server_effects.write_response_chunk chunk);
  Server_effects.finish_response_stream ()
