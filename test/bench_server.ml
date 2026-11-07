module Status = Halo.Router.Status

let router =
  let open Halo.Router in
  let open Matcher in
  route
    (let+ () = request_method_is GET *> path_end in
     fun _request -> text Status.ok "hello world\n")
  <|> route
        (let+ mode =
           request_method_is GET *> path_segment "stream" *> string_parameter
           <* path_end
         in
         fun _request ->
           let chunk = String.make 1024 'x' in
           let produce yield =
             for _ = 1 to 64 do
               yield chunk
             done
           in
           if mode = "buffered" then
             Halo.Stream.respond_buffered ~status:Status.ok
               (fun ~yield ~flush:_ -> produce yield)
           else
             Halo.Stream.respond ~status:Status.ok (fun ~yield -> produce yield))

let () =
  Eio_main.run @@ fun _env ->
  let config = Halo.Server.Config.create ~host:"127.0.0.1" ~port:8091 router in
  Halo.Server.run [ config ]
