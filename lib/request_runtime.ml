module Bridge_backend = Reactor
open Bridge_backend

module Http = struct
  let response_version http_major http_minor =
    if http_major = 1 then Printf.sprintf "HTTP/1.%d" http_minor
    else "HTTP/1.1"

  let status_forbids_body status =
    let code = Router.Status.to_int status in
    Router.Status.is_informational status || code = 204 || code = 304

  let is_framework_header name =
    match String.lowercase_ascii name with
    | "content-length" | "connection" | "transfer-encoding" -> true
    | _ -> false

  let add_header buffer (name, value) =
    if
      (not (Router.Headers.is_valid_name name))
      || not (Router.Headers.is_valid_value value)
    then invalid_arg "invalid response header reached HTTP serializer"
    else if not (is_framework_header name) then
      Buffer.add_string buffer (Printf.sprintf "%s: %s\r\n" name value)

  let add_connection_header buffer ~http_major ~http_minor ~keep_alive =
    if keep_alive && http_major = 1 && http_minor = 0 then
      Buffer.add_string buffer "Connection: keep-alive\r\n"
    else if not keep_alive then Buffer.add_string buffer "Connection: close\r\n"

  let serialize_head ~capacity ~status ~headers ~http_major ~http_minor
      ~keep_alive =
    let buffer = Buffer.create capacity in
    let status_code = Router.Status.to_int status in
    if status_code < 100 || status_code > 599 then
      invalid_arg "invalid status reached HTTP serializer";
    Buffer.add_string buffer
      (Printf.sprintf "%s %d %s\r\n"
         (response_version http_major http_minor)
         status_code (Router.Status.reason_phrase status));
    Router.Headers.iter (fun name value -> add_header buffer (name, value)) headers;
    add_connection_header buffer ~http_major ~http_minor ~keep_alive;
    buffer

  let serialize_response ~request_method ~http_major ~http_minor ~keep_alive
      ~status ~headers ~body =
    let buffer =
      serialize_head ~capacity:(String.length body + 256) ~status ~headers
        ~http_major ~http_minor ~keep_alive
    in
    if not (status_forbids_body status) then
      Buffer.add_string buffer
        (Printf.sprintf "Content-Length: %d\r\n" (String.length body));
    Buffer.add_string buffer "\r\n";
    if
      (not (status_forbids_body status))
      && not (String.equal request_method "HEAD")
    then Buffer.add_string buffer body;
    Buffer.contents buffer

  let serialize_fixed ~request_method ~http_major ~http_minor ~keep_alive
      (response : Router.response) =
    match Router.Response.body response with
    | Router.Fixed_body body ->
        serialize_response ~request_method ~http_major ~http_minor ~keep_alive
          ~status:(Router.Response.status response)
          ~headers:(Router.Response.headers response) ~body
    | Router.Stream_body _ -> invalid_arg "expected a fixed response"

  let serialize_informational ~http_major ~http_minor
      (response : Router.informational_response) =
    serialize_response ~request_method:"GET" ~http_major ~http_minor
      ~keep_alive:true
      ~status:(Router.Informational_response.status response)
      ~headers:(Router.Informational_response.headers response) ~body:""

  let serialize_stream_head ~request_method ~http_major ~http_minor ~keep_alive
      (response : Router.response) =
    if http_major <> 1 || http_minor <> 1 then
      invalid_arg "streamed responses require HTTP/1.1";
    let status = Router.Response.status response in
    if status_forbids_body status || String.equal request_method "HEAD" then
      invalid_arg "streamed responses require a response body";
    let buffer =
      serialize_head ~capacity:256 ~status
        ~headers:(Router.Response.headers response) ~http_major ~http_minor
        ~keep_alive
    in
    Buffer.add_string buffer "Transfer-Encoding: chunked\r\n\r\n";
    Buffer.contents buffer

  let serialize_chunk chunk =
    if String.length chunk = 0 then
      invalid_arg "streamed response chunks must not be empty";
    Printf.sprintf "%x\r\n%s\r\n" (String.length chunk) chunk

  let make_request (event : Reactor.request_event) request_method =
    {
      Router.request_method;
      raw_target = event.target;
      target = Router.parse_request_target request_method event.target;
      headers = event.headers;
      body = "";
    }

  let internal_server_error =
    Router.text Router.Status.internal_server_error "internal server error\n"

  let not_implemented =
    Router.text Router.Status.not_implemented "not implemented\n"

  let connect_not_implemented =
    Router.text Router.Status.not_implemented "connect not implemented\n"

  let options_star =
    Router.text Router.Status.ok ""
    |> Router.with_header "Allow"
         "GET, HEAD, POST, PATCH, PUT, DELETE, OPTIONS, TRACE"

  let trace_response (event : Reactor.request_event) body =
    let request_line =
      Printf.sprintf "%s %s HTTP/%d.%d\r\n" event.method_text event.target
        event.http_major event.http_minor
    in
    let headers =
      event.raw_headers
      |> List.map (fun (name, value) -> Printf.sprintf "%s: %s\r\n" name value)
      |> String.concat ""
    in
    Router.text Router.Status.ok (request_line ^ headers ^ "\r\n" ^ body)
    |> Router.with_content_type "message/http"
end

type body_event = Body_chunk of string | Body_end | Body_disconnected

type request_state = {
  request_id : int64;
  body_events : body_event Eio.Stream.t;
  write_completions : unit Eio.Stream.t;
  completion : (unit, exn) result Eio.Promise.t;
  complete : (unit, exn) result Eio.Promise.u;
  mutable request_cancel : Eio.Cancel.t option;
  mutable disconnected : bool;
  mutable disconnect_cleanups : (unit -> unit) list;
}

type listener = { router : Router.t }

type t = {
  runtime : Server_backend.t;
  requests : (int64, request_state) Hashtbl.t;
  metrics_waiters :
    (int64, (Server_backend.runtime_metrics, exn) result Eio.Promise.u) Hashtbl.t;
  mutable next_metrics_query : int64;
  listeners : (int64, listener) Hashtbl.t;
  bridge_switch : Eio.Switch.t;
  on_error : int64 -> exn -> unit;
}

let listener router = { router }

let create ~runtime ~listeners ~bridge_switch ~on_error =
  {
    runtime;
    requests = Hashtbl.create 128;
    metrics_waiters = Hashtbl.create 4;
    next_metrics_query = 1L;
    listeners;
    bridge_switch;
    on_error;
  }

let is_disconnect_exception = function
  | Client_disconnected -> true
  | Eio.Cancel.Cancelled Client_disconnected -> true
  | _ -> false

let run_disconnect_cleanups state =
  let cleanups = List.rev state.disconnect_cleanups in
  state.disconnect_cleanups <- [];
  List.iter
    (fun cleanup ->
      try cleanup () with exn -> prerr_endline (Printexc.to_string exn))
    cleanups

let on_disconnect state cleanup =
  if state.disconnected then cleanup ()
  else state.disconnect_cleanups <- cleanup :: state.disconnect_cleanups

let make_body runtime state ~body_expected =
  if not body_expected then Body.Private.of_string ""
  else
    Body.Private.create ~read:(fun () ->
        let event =
          match Eio.Stream.take_nonblocking state.body_events with
          | Some event -> event
          | None ->
              Reactor.request_body runtime state.request_id;
              Eio.Stream.take state.body_events
        in
        match event with
        | Body_chunk chunk -> Some chunk
        | Body_end -> None
        | Body_disconnected -> raise Client_disconnected)

let resolve_completion state result =
  ignore (Eio.Promise.try_resolve state.complete result)

let handle_terminal_event requests request_id handle =
  Reactor.trace "terminal event request=%Ld" request_id;
  Option.iter handle (Hashtbl.find_opt requests request_id)

let await_write state =
  if state.disconnected then raise Client_disconnected;
  Eio.Stream.take state.write_completions

let handle_request listener (event : Reactor.request_event) state body
    ~send_informational ~send_response ~send_fixed ~completed ~sent ~on_error () =
  try
    if state.disconnected then raise Client_disconnected;
    let request_method = Router.Http_method.of_string event.method_text in
    let response =
      match request_method with
      | None -> Http.not_implemented
      | Some Router.CONNECT -> Http.connect_not_implemented
      | Some Router.OPTIONS when String.equal event.target "*" -> Http.options_star
      | Some Router.TRACE -> Http.trace_response event (Body.read_all body)
      | Some request_method ->
          let request = Http.make_request event request_method in
          Eio.Cancel.sub @@ fun request_cancel ->
          state.request_cancel <- Some request_cancel;
          Eio.Switch.run ~name:"halo-request" @@ fun request_switch ->
          Router.Runtime.dispatch listener.router request
            ~raw_headers:event.raw_headers ~body ~request_id:event.request_id
            ~request_switch ~send_informational
            ~on_disconnect:(on_disconnect state)
    in
    state.request_cancel <- None;
    send_response response
  with
  | exn when is_disconnect_exception exn -> completed := true
  | exn ->
      on_error event.request_id exn;
      if not !sent then
        (try send_fixed Http.internal_server_error
         with disconnect when is_disconnect_exception disconnect ->
           completed := true)

let run_handler runtime requests listener (event : Reactor.request_event) state
    on_error =
  Reactor.trace "handler start request=%Ld" event.request_id;
  let completed = ref false in
  let sent = ref false in
  let body = make_body runtime state ~body_expected:event.body_expected in
  let keep_alive () = event.keep_alive && Body.is_complete body in
  let await_response_completion () =
    match Eio.Promise.await state.completion with
    | Ok () -> completed := true
    | Error exn -> raise exn
  in
  let send_fixed response =
    let wire =
      Http.serialize_fixed ~request_method:event.method_text
        ~http_major:event.http_major ~http_minor:event.http_minor
        ~keep_alive:(keep_alive ()) response
    in
    Reactor.trace "fixed response request=%Ld status=%d bytes=%d"
      event.request_id
      (Router.Status.to_int (Router.Response.status response))
      (String.length wire);
    Reactor.send_response runtime event.request_id wire;
    sent := true;
    await_response_completion ()
  in
  let send_informational response =
    let wire =
      Http.serialize_informational ~http_major:event.http_major
        ~http_minor:event.http_minor response
    in
    Reactor.send_informational runtime event.request_id wire;
    await_write state
  in
  let send_stream response producer =
    let head =
      Http.serialize_stream_head ~request_method:event.method_text
        ~http_major:event.http_major ~http_minor:event.http_minor
        ~keep_alive:(keep_alive ()) response
    in
    Reactor.start_response_stream runtime event.request_id head;
    sent := true;
    await_write state;
    (try
       producer ~yield:(fun chunk ->
           Reactor.write_response_chunk runtime event.request_id
             (Http.serialize_chunk chunk);
           await_write state)
     with
    | exn when is_disconnect_exception exn -> raise exn
    | exn -> on_error event.request_id exn);
    Reactor.finish_response_stream runtime event.request_id;
    await_response_completion ()
  in
  let send_response (response : Router.response) =
    match Router.Response.body response with
    | Router.Fixed_body _ -> send_fixed response
    | Router.Stream_body producer -> send_stream response producer
  in
  Fun.protect
    ~finally:(fun () ->
      state.request_cancel <- None;
      Hashtbl.remove requests event.request_id;
      Reactor.trace "handler finish request=%Ld completed=%b" event.request_id
        !completed;
      if not !completed then
        Eio.Cancel.protect (fun () ->
            try Reactor.close_request runtime event.request_id
            with Client_disconnected -> ()))
    (handle_request listener event state body ~send_informational ~send_response
       ~send_fixed ~completed ~sent ~on_error)

let start_request t (event : Reactor.request_event) =
  Reactor.trace "request event request=%Ld listener=%Ld" event.request_id
    event.listener_id;
  match Hashtbl.find_opt t.listeners event.listener_id with
  | None -> Reactor.close_request t.runtime event.request_id
  | Some listener ->
      let completion, complete = Eio.Promise.create () in
      let state =
        {
          request_id = event.request_id;
          body_events = Eio.Stream.create max_int;
          write_completions = Eio.Stream.create max_int;
          completion;
          complete;
          request_cancel = None;
          disconnected = false;
          disconnect_cleanups = [];
        }
      in
      Hashtbl.replace t.requests event.request_id state;
      Eio.Fiber.fork ~sw:t.bridge_switch (fun () ->
          run_handler t.runtime t.requests listener event state t.on_error)

let dispatch_event t = function
  | Reactor.Request event -> start_request t event
  | Reactor.Body_chunk { request_id; chunk } ->
      Option.iter
        (fun state -> Eio.Stream.add state.body_events (Body_chunk chunk))
        (Hashtbl.find_opt t.requests request_id)
  | Reactor.Body_end request_id ->
      Option.iter
        (fun state -> Eio.Stream.add state.body_events Body_end)
        (Hashtbl.find_opt t.requests request_id)
  | Reactor.Write_complete request_id ->
      Option.iter
        (fun state -> Eio.Stream.add state.write_completions ())
        (Hashtbl.find_opt t.requests request_id)
  | Reactor.Response_complete request_id ->
      handle_terminal_event t.requests request_id (fun state ->
          resolve_completion state (Ok ()))
  | Reactor.Disconnected request_id ->
      handle_terminal_event t.requests request_id (fun state ->
          state.disconnected <- true;
          run_disconnect_cleanups state;
          Eio.Stream.add state.body_events Body_disconnected;
          resolve_completion state (Error Client_disconnected);
          Option.iter
            (fun cancel -> Eio.Cancel.cancel cancel Client_disconnected)
            state.request_cancel)
  | Reactor.Metrics { query_id; payload } ->
      Option.iter
        (fun resolve ->
          Hashtbl.remove t.metrics_waiters query_id;
          let result =
            try Ok (Reactor.metrics_of_payload payload) with exn -> Error exn
          in
          ignore (Eio.Promise.try_resolve resolve result))
        (Hashtbl.find_opt t.metrics_waiters query_id)
  | Reactor.Stopped ->
      Hashtbl.iter
        (fun _ resolve ->
          ignore
            (Eio.Promise.try_resolve resolve
               (Error (Reactor.Runtime_error "reactor stopped"))))
        t.metrics_waiters;
      Hashtbl.clear t.metrics_waiters
  | Reactor.Unknown { kind; request_id; payload = _ } ->
      t.on_error request_id
        (Failure (Printf.sprintf "unknown bridge event %d" kind))

let rec drain_events t stopped =
  match Reactor.poll_event t.runtime with
  | None -> stopped
  | Some event ->
      let stopped =
        match event with
        | Reactor.Stopped ->
            dispatch_event t event;
            true
        | _ ->
            dispatch_event t event;
            stopped
      in
      drain_events t stopped

let run t =
  let event_fd = Reactor.event_fd t.runtime in
  let stopped = ref false in
  while not !stopped do
    Eio_unix.await_readable event_fd;
    Reactor.drain_wakeup t.runtime;
    stopped := drain_events t !stopped;
    Eio.Fiber.yield ()
  done;
  Reactor.trace "event loop stopped"

let await_idle t =
  while Hashtbl.length t.requests > 0 do
    Eio.Fiber.yield ()
  done

let metrics t =
  let query_id = t.next_metrics_query in
  t.next_metrics_query <- Int64.succ query_id;
  let promise, resolve = Eio.Promise.create () in
  Hashtbl.add t.metrics_waiters query_id resolve;
  Fun.protect
    ~finally:(fun () -> Hashtbl.remove t.metrics_waiters query_id)
    (fun () ->
      Reactor.request_metrics t.runtime query_id;
      match Eio.Promise.await promise with
      | Ok metrics -> metrics
      | Error exn -> raise exn)
