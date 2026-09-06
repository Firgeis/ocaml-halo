let router =
  let open Halo.Router in
  let open Matcher in
  route
    (let+ () = request_method_is GET and+ () = path_end in
     fun _request -> text 200 "hello world\n")

let () =
  Eio_main.run @@ fun _env ->
  let config = Halo.Server.Config.create ~host:"127.0.0.1" ~port:8091 router in
  Halo.Server.run [ config ]
