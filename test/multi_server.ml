let router body =
  let open Halo.Router in
  let open Matcher in
  route
    (let+ () = request_method_is GET and+ () = path_end in
     fun _request -> text 200 body)

let run_runtime ~port ~body shutdown =
  Eio_main.run @@ fun _env ->
  let config =
    Halo.Server.Config.create ~host:"127.0.0.1" ~port (router body)
  in
  Halo.Server.run ~shutdown ~handle_sigint:false [ config ]

let () =
  let first = Halo.Server.Shutdown.create () in
  let second = Halo.Server.Shutdown.create () in
  let previous_sigint =
    Sys.signal Sys.sigint
      (Sys.Signal_handle
         (fun _signal ->
           Halo.Server.Shutdown.request first;
           Halo.Server.Shutdown.request second))
  in
  Fun.protect
    ~finally:(fun () -> Sys.set_signal Sys.sigint previous_sigint)
    (fun () ->
      let second_domain =
        Domain.spawn (fun () -> run_runtime ~port:8082 ~body:"second\n" second)
      in
      run_runtime ~port:8081 ~body:"first\n" first;
      Domain.join second_domain)
