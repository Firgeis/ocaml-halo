module Status = Halo.Router.Status

let router =
  let open Halo.Router in
  let open Matcher in
  route
    (let+ () = request_method_is GET *> path_end in
     fun _request -> text Status.ok "hello world\n")

let () =
  Eio_main.run @@ fun _env ->
  let config = Halo.Server.Config.create ~host:"127.0.0.1" ~port:8091 router in
  Halo.Server.run [ config ]
