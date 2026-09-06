type controller_state = Server_backend.controller_state =
  | Normal
  | Busy
  | Saturated
  | Recovering

type runtime_metrics = Server_backend.runtime_metrics = {
  utilization : float;
  active_connections : int64;
  active_requests : int64;
  accepted_connections : int64;
  rejected_connections : int64;
  completed_requests : int64;
  tls_full_handshakes : int64;
  tls_resumed_handshakes : int64;
  tls_handshake_failures : int64;
  controller_state : controller_state;
  backpressure_enabled : bool;
}

type controller_config = Server_config.controller_config
type tls_minimum_protocol_version = Server_config.tls_minimum_protocol_version =
  | TLS12
  | TLS13
type tls_client_auth_mode = Server_config.tls_client_auth_mode =
  | Client_auth_none
  | Client_auth_optional
  | Client_auth_required
type tls_client_auth_config = Server_config.tls_client_auth_config
type tls_config = Server_config.tls_config
type config = Server_config.t

module Config = Server_config

let runtime_backend_name = Server_backend.runtime_backend_name

exception Client_disconnected = Reactor.Client_disconnected
exception Runtime_error = Reactor.Runtime_error

module Shutdown = struct
  type t = { requested : bool Atomic.t; changed : Eio.Condition.t }

  let create () =
    { requested = Atomic.make false; changed = Eio.Condition.create () }

  let request t =
    Atomic.set t.requested true;
    Eio.Condition.broadcast t.changed

  let await t =
    Eio.Condition.loop_no_mutex t.changed (fun () ->
        if Atomic.get t.requested then Some () else None)
end

type running = {
  dispatcher : Request_runtime.t;
  shutdown_control : Shutdown.t;
}

let metrics running = Request_runtime.metrics running.dispatcher
let shutdown running = Shutdown.request running.shutdown_control

let close_runtime_or_raise runtime =
  match Reactor.close_runtime runtime with
  | None -> ()
  | Some message -> raise (Runtime_error message)

let result_or_raise = function
  | Ok value -> value
  | Error message -> raise (Runtime_error message)

let listen runtime configs =
  let listeners = Hashtbl.create (List.length configs) in
  List.iter
    (fun (config : Config.t) ->
      match Server_backend.listen runtime config with
      | Error message -> raise (Runtime_error message)
      | Ok raw ->
          let listener_id = Reactor.listener_id raw in
          Server_backend.release_server raw;
          Hashtbl.add listeners listener_id
            (Request_runtime.listener config.router))
    configs;
  listeners

let install_sigint enabled shutdown =
  if enabled then
    Some
      (Sys.signal Sys.sigint
         (Sys.Signal_handle (fun _ -> Shutdown.request shutdown)))
  else None

let stop runtime = try Reactor.stop runtime with Client_disconnected -> ()

let run_bridge runtime listeners shutdown on_error on_ready =
  let reactor =
    Domain.spawn (fun () -> result_or_raise (Reactor.run runtime))
  in
  let reactor_joined = ref false in
  let join_reactor () =
    if not !reactor_joined then (
      Domain.join reactor;
      reactor_joined := true;
      Reactor.trace "reactor domain stopped")
  in
  Eio.Switch.run ~name:"halo-reactor-bridge" @@ fun bridge_switch ->
  let dispatcher =
    Request_runtime.create ~runtime ~listeners ~bridge_switch ~on_error
  in
  let running = { dispatcher; shutdown_control = shutdown } in
  Eio.Fiber.fork ~sw:bridge_switch (fun () ->
      Shutdown.await shutdown;
      stop runtime);
  Fun.protect
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () -> stop runtime);
      join_reactor ())
    (fun () ->
      Option.iter
        (fun notify ->
          Eio.Fiber.fork ~sw:bridge_switch (fun () -> notify running))
        on_ready;
      Request_runtime.run dispatcher;
      join_reactor ();
      Request_runtime.await_idle dispatcher)

let run ?shutdown ?(handle_sigint = true) ?(max_in_flight_requests = 4096)
    ?(command_capacity = 4096)
    ?on_ready
    ?(on_error = fun _request_id exn -> prerr_endline (Printexc.to_string exn))
    configs =
  if max_in_flight_requests <= 0 then
    invalid_arg "max_in_flight_requests must be positive";
  if command_capacity <= 0 then invalid_arg "command_capacity must be positive";
  let runtime = result_or_raise (Reactor.create_runtime ()) in
  Fun.protect
    ~finally:(fun () -> close_runtime_or_raise runtime)
    (fun () ->
      result_or_raise
        (Reactor.enable runtime ~max_in_flight_requests ~command_capacity);
      let listeners = listen runtime configs in
      let shutdown = Option.value shutdown ~default:(Shutdown.create ()) in
      let previous_sigint = install_sigint handle_sigint shutdown in
      Fun.protect
        ~finally:(fun () ->
          Option.iter (Sys.set_signal Sys.sigint) previous_sigint)
        (fun () -> run_bridge runtime listeners shutdown on_error on_ready))
