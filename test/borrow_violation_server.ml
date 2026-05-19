let leaked_request : Halo.Router.Borrowed.borrowed_request option ref = ref None
let seen_first_request = ref false

let ok_response =
  "HTTP/1.1 200 OK\r\n\
   Content-Length: 3\r\n\
   Connection: close\r\n\
   \r\n\
   ok\n"

let dispatch_request _router borrowed_request _http_major _http_minor
    _keep_alive =
  if not !seen_first_request then (
    seen_first_request := true;
    leaked_request := Some borrowed_request;
    ok_response)
  else
    match !leaked_request with
    | None -> ok_response
    | Some leaked ->
        ignore (Halo.Router.Borrowed.request_method_text leaked);
        ok_response

let run_server runtime =
  Callback.register "ocaml_halo_dispatch_request" dispatch_request;
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_end in
          fun _request -> text 200 "ok\n"))
  in
  let config = Halo.Server.Config.create ~port:8080 router in
  let server =
    Halo.Server.listen runtime config 
    |> Halo.Server_effects.or_fail_runtime
  in
  Halo.Server_effects.finalize_runtime (fun () ->
      Halo.Server_effects.or_fail_runtime_status (Halo.Server.close_server server));
  Halo.Server.run runtime;
  Ok ()

let run () = Halo.Server.with_runtime run_server

let () =
  Sys.catch_break true;
  match run () with
  | Error error ->
      prerr_endline (Halo.Error.to_string error);
      exit 1
  | Ok () -> ()
