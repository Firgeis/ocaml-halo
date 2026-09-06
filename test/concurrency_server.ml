module Status = Halo.Router.Status

let router clock =
  let open Halo.Router in
  let open Matcher in
  route
    (let+ () = request_method_is GET and+ () = path_end in
     fun _request -> text Status.ok "hello\n")
  <|> route
        (let+ () = request_method_is GET
         and+ () = path_segment "fast"
         and+ () = path_end in
         fun _request -> text Status.ok "fast\n")
  <|> route
        (let+ () = request_method_is GET
         and+ () = path_segment "slow"
         and+ () = path_end in
         fun _request ->
           Eio.Time.sleep clock 0.30;
           text Status.ok "slow\n")
  <|> route
        (let+ () = request_method_is GET
         and+ () = path_segment "child"
         and+ () = path_end in
         fun _request ->
           let value, set_value = Eio.Promise.create () in
           Eio.Fiber.fork ~sw:(Request.switch _request) (fun () ->
               Eio.Time.sleep clock 0.02;
               Eio.Promise.resolve set_value "child\n");
           text Status.ok (Eio.Promise.await value))
  <|> route
        (let+ () = request_method_is POST
         and+ () = path_segment "echo"
         and+ () = path_end
         and+ () = require_body in
         fun request -> text Status.ok (Halo.Body.read_all (Request.body request)))
  <|> route
        (let+ () = request_method_is POST
         and+ () = path_segment "stream-length"
         and+ () = path_end
         and+ () = require_body in
         fun _request ->
           let rec consume total =
             match Halo.Body.read (Request.body _request) with
             | None -> total
             | Some chunk -> consume (total + String.length chunk)
           in
           text Status.ok (Printf.sprintf "%d\n" (consume 0)))

let () =
  Eio_main.run @@ fun env ->
  let router = router (Eio.Stdenv.clock env) in
  let max_in_flight_requests =
    Option.bind
      (Sys.getenv_opt "HALO_MAX_IN_FLIGHT_REQUESTS")
      int_of_string_opt
    |> Option.value ~default:4096
  in
  let config =
    Halo.Server.Config.create ~host:"127.0.0.1" ~port:8081
      ~max_request_size:(2 * 1024 * 1024) router
  in
  Halo.Server.run ~max_in_flight_requests [ config ]
