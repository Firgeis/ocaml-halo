module Bridge_backend = Reactor
open Bridge_backend

module Http = struct
  let response_version http_major http_minor =
    if http_major = 1 then Printf.sprintf "HTTP/1.%d" http_minor else "HTTP/1.1"

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
         status_code
         (Router.Status.reason_phrase status));
    Router.Headers.iter
      (fun name value -> add_header buffer (name, value))
      headers;
    add_connection_header buffer ~http_major ~http_minor ~keep_alive;
    buffer

  let serialize_response ~request_method ~http_major ~http_minor ~keep_alive
      ~status ~headers ~body =
    let buffer =
      serialize_head
        ~capacity:(String.length body + 256)
        ~status ~headers ~http_major ~http_minor ~keep_alive
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
          ~headers:(Router.Response.headers response)
          ~body
    | Router.Stream_body _ -> invalid_arg "expected a fixed response"

  let serialize_informational ~http_major ~http_minor
      (response : Router.informational_response) =
    serialize_response ~request_method:"GET" ~http_major ~http_minor
      ~keep_alive:true
      ~status:(Router.Informational_response.status response)
      ~headers:(Router.Informational_response.headers response)
      ~body:""

  let stream_header_buffer ~http_major ~http_minor ~keep_alive
      (response : Router.response) =
    if http_major <> 1 || http_minor <> 1 then
      invalid_arg "streamed responses require HTTP/1.1";
    let status = Router.Response.status response in
    if status_forbids_body status then
      invalid_arg "streamed responses require a response body";
    serialize_head ~capacity:256 ~status
      ~headers:(Router.Response.headers response)
      ~http_major ~http_minor ~keep_alive

  let serialize_stream_head ~http_major ~http_minor ~keep_alive response =
    let buffer =
      stream_header_buffer ~http_major ~http_minor ~keep_alive response
    in
    Buffer.add_string buffer "Transfer-Encoding: chunked\r\n\r\n";
    Buffer.contents buffer

  let serialize_stream_head_only ~http_major ~http_minor ~keep_alive
      (response : Router.response) =
    let buffer =
      stream_header_buffer ~http_major ~http_minor ~keep_alive response
    in
    Buffer.add_string buffer "\r\n";
    Buffer.contents buffer

  let serialize_chunk chunk =
    if String.length chunk = 0 then
      invalid_arg "streamed response chunks must not be empty";
    Printf.sprintf "%x\r\n%s\r\n" (String.length chunk) chunk

  let internal_server_error =
    Router.text Router.Status.internal_server_error "internal server error\n"

  let too_early = Router.text Router.Status.too_early "too early\n"

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
      Request_headers.raw_headers event.headers
      |> List.filter (fun (name, _) ->
          let name = String.lowercase_ascii name in
          not
            (String.equal name "forwarded"
            || String.starts_with ~prefix:"x-forwarded-" name))
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
  mutable response_abort : (exn * Printexc.raw_backtrace) option;
  mutable response_outcome : Router.response_outcome option;
  mutable response_termination_callbacks :
    (Router.response_outcome -> unit) list;
  mutable telemetry_route : string option;
  mutable telemetry_status_code : int option;
  mutable telemetry_end_monotonic_ns : int64 option;
  mutable request_body_size : int64;
  mutable response_body_size : int64;
  observability_enabled : bool;
}

type listener = {
  router : Router.t;
  early_data_policy : Server_config.early_data_policy;
  early_data_replay_store : Server_config.Early_data_replay_store.t;
}

type t = {
  runtime : Server_backend.t;
  owner_domain : Domain.id;
  requests : (int64, request_state) Hashtbl.t;
  connection_infos : (int64, Router.connection_info) Hashtbl.t;
  metrics_waiters :
    ( int64,
      (Server_backend.runtime_metrics, exn) result Eio.Promise.u )
    Hashtbl.t;
  mutable next_metrics_query : int64;
  mutable draining : bool;
  listeners : (int64, listener) Hashtbl.t;
  bridge_switch : Eio.Switch.t;
  on_error : int64 -> exn -> Printexc.raw_backtrace -> unit;
  telemetry : Telemetry.t;
  metrics : Metrics.t;
}

let listener ~early_data_policy ~early_data_replay_store router =
  { router; early_data_policy; early_data_replay_store }

let create ~runtime ~listeners ~bridge_switch ~on_error ~telemetry ~metrics =
  {
    runtime;
    owner_domain = Domain.self ();
    requests = Hashtbl.create 128;
    connection_infos = Hashtbl.create 128;
    metrics_waiters = Hashtbl.create 4;
    next_metrics_query = 1L;
    draining = false;
    listeners;
    bridge_switch;
    on_error;
    telemetry;
    metrics;
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

let on_response_terminated state callback =
  match state.response_outcome with
  | Some outcome -> callback outcome
  | None ->
      state.response_termination_callbacks <-
        callback :: state.response_termination_callbacks

let report_callback_error on_error request_id exn backtrace =
  try on_error request_id exn backtrace
  with reporting_exn ->
    prerr_endline
      (Printf.sprintf "response lifecycle error reporter failed: %s"
         (Printexc.to_string reporting_exn))

let terminate_response state on_error outcome =
  match state.response_outcome with
  | Some _ -> ()
  | None ->
      state.response_outcome <- Some outcome;
      let callbacks = List.rev state.response_termination_callbacks in
      state.response_termination_callbacks <- [];
      List.iter
        (fun callback ->
          try callback outcome
          with exn ->
            let backtrace = Printexc.get_raw_backtrace () in
            report_callback_error on_error state.request_id exn backtrace)
        callbacks

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
  if Reactor.trace_enabled then
    Reactor.trace "terminal event request=%Ld" request_id;
  Option.iter handle (Hashtbl.find_opt requests request_id)

let await_write state =
  if state.disconnected then raise Client_disconnected;
  Eio.Stream.take state.write_completions

let with_early_data_policy listener (event : Reactor.request_event) next =
  if
    event.is_early_data
    && not
         (listener.early_data_policy ~method_text:event.method_text
            ~target:event.target
            ~headers:(Request_headers.headers event.headers))
  then Http.too_early
  else next ()

let dispatch_request listener (event : Reactor.request_event) state body
    ~connection_info ~send_informational =
  match Router.Http_method.of_string event.method_text with
  | None -> Http.not_implemented
  | Some Router.CONNECT -> Http.connect_not_implemented
  | Some Router.OPTIONS when String.equal event.target "*" -> Http.options_star
  | Some Router.TRACE -> Http.trace_response event (Body.read_all body)
  | Some request_method ->
      let on_route =
        if state.observability_enabled then
          Some (fun template -> state.telemetry_route <- Some template)
        else None
      in
      Eio.Switch.run ~name:"halo-request" @@ fun request_switch ->
      Router.Runtime.dispatch_deferred ?on_route listener.router ~request_method
        ~raw_target:event.target
        ~headers:(lazy (Request_headers.headers event.headers))
        ~raw_headers:(lazy (Request_headers.raw_headers event.headers))
        ~body ~http_major:event.http_major ~http_minor:event.http_minor
        ~connection_info ~is_early_data:event.is_early_data
        ~request_id:event.request_id ~request_switch ~send_informational
        ~on_disconnect:(on_disconnect state)
        ~on_response_terminated:(on_response_terminated state)

let handle_request listener (event : Reactor.request_event) state body
    ~connection_info ~send_informational ~send_response ~send_fixed ~completed
    ~sent ~on_error () =
  try
    if state.disconnected then raise Client_disconnected;
    let response =
      with_early_data_policy listener event @@ fun () ->
      dispatch_request listener event state body ~connection_info
        ~send_informational
    in
    send_response response
  with
  | exn when is_disconnect_exception exn -> completed := true
  | exn -> (
      let backtrace = Printexc.get_raw_backtrace () in
      on_error event.request_id exn backtrace;
      if not !sent then
        try send_fixed Http.internal_server_error
        with disconnect when is_disconnect_exception disconnect ->
          completed := true)

let run_handler runtime requests listener (event : Reactor.request_event) state
    ~connection_info ~is_draining on_error =
  if Reactor.trace_enabled then
    Reactor.trace "handler start request=%Ld" event.request_id;
  let completed = ref false in
  let sent = ref false in
  let body = make_body runtime state ~body_expected:event.body_expected in
  let keep_alive () =
    event.keep_alive && (not (is_draining ())) && Body.is_complete body
  in
  let await_response_completion () =
    match Eio.Promise.await state.completion with
    | Ok () -> completed := true
    | Error exn -> raise exn
  in
  let await_response_abort () =
    match Eio.Promise.await state.completion with
    | Ok () -> completed := true
    | Error exn when is_disconnect_exception exn -> completed := true
    | Error exn -> raise exn
  in
  let send_fixed response =
    if state.observability_enabled then
      state.telemetry_status_code <-
        Some (Router.Status.to_int (Router.Response.status response));
    let wire =
      Http.serialize_fixed ~request_method:event.method_text
        ~http_major:event.http_major ~http_minor:event.http_minor
        ~keep_alive:(keep_alive ()) response
    in
    if state.observability_enabled then
      state.response_body_size <-
        (match Router.Response.body response with
        | Router.Fixed_body body
          when (not
                  (Http.status_forbids_body (Router.Response.status response)))
               && not (String.equal event.method_text "HEAD") ->
            Int64.of_int (String.length body)
        | _ -> 0L);
    if Reactor.trace_enabled then
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
    if state.observability_enabled then
      state.telemetry_status_code <-
        Some (Router.Status.to_int (Router.Response.status response));
    if String.equal event.method_text "HEAD" then (
      let wire =
        Http.serialize_stream_head_only ~http_major:event.http_major
          ~http_minor:event.http_minor ~keep_alive:(keep_alive ()) response
      in
      Reactor.send_response runtime event.request_id wire;
      sent := true;
      await_response_completion ())
    else
      let head =
        Http.serialize_stream_head ~http_major:event.http_major
          ~http_minor:event.http_minor ~keep_alive:(keep_alive ()) response
      in
      Reactor.start_response_stream runtime event.request_id head;
      sent := true;
      await_write state;
      match
        try
          producer ~yield:(fun chunk ->
              Reactor.write_response_chunk runtime event.request_id
                (Http.serialize_chunk chunk);
              await_write state;
              if state.observability_enabled then
                state.response_body_size <-
                  Int64.add state.response_body_size
                    (Int64.of_int (String.length chunk)));
          None
        with
        | exn when is_disconnect_exception exn -> raise exn
        | exn -> Some (exn, Printexc.get_raw_backtrace ())
      with
      | None ->
          Reactor.finish_response_stream runtime event.request_id;
          await_response_completion ()
      | Some (exn, backtrace) ->
          state.response_abort <- Some (exn, backtrace);
          on_error event.request_id exn backtrace;
          Reactor.abort_response_stream runtime event.request_id;
          await_response_abort ()
  in
  let send_response (response : Router.response) =
    match Router.Response.body response with
    | Router.Fixed_body _ -> send_fixed response
    | Router.Stream_body producer -> send_stream response producer
  in
  Eio.Cancel.sub @@ fun request_cancel ->
  state.request_cancel <- Some request_cancel;
  Fun.protect
    ~finally:(fun () ->
      state.request_cancel <- None;
      Hashtbl.remove requests event.request_id;
      if Reactor.trace_enabled then
        Reactor.trace "handler finish request=%Ld completed=%b" event.request_id
          !completed;
      if not !completed then
        Eio.Cancel.protect (fun () ->
            try Reactor.close_request runtime event.request_id
            with Client_disconnected -> ()))
    (handle_request listener event state body ~connection_info
       ~send_informational ~send_response ~send_fixed ~completed ~sent ~on_error)

let start_request t (event : Reactor.request_event) =
  if Reactor.trace_enabled then
    Reactor.trace "request event request=%Ld listener=%Ld" event.request_id
      event.listener_id;
  (* Bridge events are FIFO: the first request seeds the cache, later requests
     reuse it, and the physical-close event eventually evicts it. *)
  let connection_info =
    match event.connection_info with
    | Some connection_info ->
        if event.connection_id <> 0L then
          Hashtbl.replace t.connection_infos event.connection_id connection_info;
        Some connection_info
    | None -> Hashtbl.find_opt t.connection_infos event.connection_id
  in
  match (connection_info, Hashtbl.find_opt t.listeners event.listener_id) with
  | None, _ ->
      let exn =
        Reactor.Runtime_error "request connection metadata is missing"
      in
      t.on_error event.request_id exn (Printexc.get_callstack 32);
      Reactor.close_request t.runtime event.request_id
  | Some _, None -> Reactor.close_request t.runtime event.request_id
  | Some connection_info, Some listener ->
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
          response_abort = None;
          response_outcome = None;
          response_termination_callbacks = [];
          telemetry_route = None;
          telemetry_status_code = None;
          telemetry_end_monotonic_ns = None;
          request_body_size = 0L;
          response_body_size = 0L;
          observability_enabled =
            Telemetry.Lifecycle.enabled t.telemetry
            || Metrics.Private.enabled t.metrics;
        }
      in
      Hashtbl.replace t.requests event.request_id state;
      Eio.Fiber.fork ~sw:t.bridge_switch (fun () ->
          let run_handler () =
            run_handler t.runtime t.requests listener event state
              ~connection_info
              ~is_draining:(fun () -> t.draining)
              t.on_error
          in
          let metrics_request =
            Metrics.Private.start_request t.metrics ~method_:event.method_text
              ~scheme:
                (match connection_info.tls with
                | None -> "http"
                | Some _ -> "https")
              ~start_monotonic_ns:event.start_monotonic_ns
          in
          let finish_metrics () =
            let outcome =
              match state.response_outcome with
              | Some Router.Completed -> "completed"
              | Some (Router.Aborted _) -> "aborted"
              | Some Router.Client_disconnected -> "client_disconnected"
              | None -> "incomplete"
            in
            Metrics.Private.finish_request metrics_request
              ~route:state.telemetry_route
              ~status_code:state.telemetry_status_code ~outcome
              ~end_monotonic_ns:state.telemetry_end_monotonic_ns
              ~request_body_size:state.request_body_size
              ~response_body_size:state.response_body_size
          in
          if not state.observability_enabled then run_handler ()
          else
            let telemetry_span =
              Telemetry.Lifecycle.start_server_request t.telemetry
                {
                  Telemetry.request_id = event.request_id;
                  method_ = event.method_text;
                  target = event.target;
                  headers = Request_headers.headers event.headers;
                  remote_parent =
                    Telemetry.Trace_context.of_headers
                      (Request_headers.headers event.headers);
                  http_version = (event.http_major, event.http_minor);
                  connection_info;
                  is_early_data = event.is_early_data;
                  start_time_unix_ns = event.start_time_unix_ns;
                  start_monotonic_ns = event.start_monotonic_ns;
                }
            in
            Fun.protect
              ~finally:(fun () ->
                Fun.protect ~finally:finish_metrics (fun () ->
                    Telemetry.Lifecycle.finish_server_request telemetry_span
                      {
                        Telemetry.route = state.telemetry_route;
                        status_code = state.telemetry_status_code;
                        outcome = state.response_outcome;
                        end_monotonic_ns = state.telemetry_end_monotonic_ns;
                        request_body_size = state.request_body_size;
                        response_body_size = state.response_body_size;
                      }))
              (fun () ->
                Telemetry.Lifecycle.with_span telemetry_span run_handler))

let replay_claim store (event : Reactor.replay_claim_event) =
  let module Store = Server_config.Early_data_replay_store in
  try
    Eio.Fiber.first
      (fun () ->
        Eio_unix.sleep (Store.timeout_seconds store);
        Store.Unavailable)
      (fun () ->
        Store.claim store ~identity:event.identity ~ttl_secs:event.ttl_secs)
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn ->
      if Reactor.trace_enabled then
        Reactor.trace "replay store failed claim=%Ld: %s" event.claim_id
          (Printexc.to_string exn);
      Store.Unavailable

let start_replay_claim t (event : Reactor.replay_claim_event) =
  let accept =
    match Hashtbl.find_opt t.listeners event.listener_id with
    | Some { early_data_replay_store = store; _ } ->
        replay_claim store event = Server_config.Early_data_replay_store.Fresh
    | None -> false
  in
  try Reactor.resolve_replay_claim t.runtime event.claim_id ~accept
  with Client_disconnected -> ()

let observe_pre_dispatch_response t
    (event : Reactor.pre_dispatch_response_event) =
  match event.connection_info with
  | None ->
      if Reactor.trace_enabled then
        Reactor.trace "pre-dispatch response metadata missing request=%Ld"
          event.request_id
  | Some connection_info ->
      Eio.Fiber.fork ~sw:t.bridge_switch (fun () ->
          let scheme =
            match connection_info.Router.tls with
            | None -> "http"
            | Some _ -> "https"
          in
          let metrics_request =
            Metrics.Private.start_request t.metrics ~method_:event.method_text
              ~scheme ~start_monotonic_ns:event.start_monotonic_ns
          in
          let outcome =
            if event.completed then Router.Completed
            else Router.Client_disconnected
          in
          let finish_metrics () =
            Metrics.Private.finish_request metrics_request ~route:None
              ~status_code:(Some event.status_code)
              ~outcome:
                (if event.completed then "completed" else "client_disconnected")
              ~end_monotonic_ns:(Some event.end_monotonic_ns)
              ~request_body_size:event.request_body_size
              ~response_body_size:event.response_body_size
          in
          let telemetry_span =
            Telemetry.Lifecycle.start_server_request t.telemetry
              {
                Telemetry.request_id = event.request_id;
                method_ = event.method_text;
                target = event.target;
                headers = Request_headers.headers event.headers;
                remote_parent =
                  Telemetry.Trace_context.of_headers
                    (Request_headers.headers event.headers);
                http_version = (event.http_major, event.http_minor);
                connection_info;
                is_early_data = event.is_early_data;
                start_time_unix_ns = event.start_time_unix_ns;
                start_monotonic_ns = event.start_monotonic_ns;
              }
          in
          Fun.protect ~finally:finish_metrics (fun () ->
              Telemetry.Lifecycle.finish_server_request telemetry_span
                {
                  Telemetry.route = None;
                  status_code = Some event.status_code;
                  outcome = Some outcome;
                  end_monotonic_ns = Some event.end_monotonic_ns;
                  request_body_size = event.request_body_size;
                  response_body_size = event.response_body_size;
                }))

let dispatch_event t = function
  | Reactor.Request event -> start_request t event
  | Reactor.Body_chunk { request_id; chunk } ->
      Option.iter
        (fun state ->
          if state.observability_enabled then
            state.request_body_size <-
              Int64.add state.request_body_size
                (Int64.of_int (String.length chunk));
          Eio.Stream.add state.body_events (Body_chunk chunk))
        (Hashtbl.find_opt t.requests request_id)
  | Reactor.Body_end request_id ->
      Option.iter
        (fun state -> Eio.Stream.add state.body_events Body_end)
        (Hashtbl.find_opt t.requests request_id)
  | Reactor.Write_complete request_id ->
      Option.iter
        (fun state -> Eio.Stream.add state.write_completions ())
        (Hashtbl.find_opt t.requests request_id)
  | Reactor.Response_complete
      { request_id; end_monotonic_ns; request_body_size } ->
      handle_terminal_event t.requests request_id (fun state ->
          if state.observability_enabled then (
            state.telemetry_end_monotonic_ns <- Some end_monotonic_ns;
            state.request_body_size <- request_body_size);
          resolve_completion state (Ok ());
          terminate_response state t.on_error Router.Completed)
  | Reactor.Disconnected { request_id; end_monotonic_ns; request_body_size } ->
      handle_terminal_event t.requests request_id (fun state ->
          if state.observability_enabled then (
            state.telemetry_end_monotonic_ns <- Some end_monotonic_ns;
            state.request_body_size <- request_body_size);
          let response_outcome =
            match state.response_abort with
            | Some (exn, backtrace) -> Router.Aborted (exn, backtrace)
            | None -> Router.Client_disconnected
          in
          state.disconnected <- true;
          run_disconnect_cleanups state;
          Eio.Stream.add state.body_events Body_disconnected;
          resolve_completion state (Error Client_disconnected);
          Option.iter
            (fun cancel -> Eio.Cancel.cancel cancel Client_disconnected)
            state.request_cancel;
          terminate_response state t.on_error response_outcome)
  | Reactor.Metrics { query_id; payload } ->
      if Reactor.trace_enabled then
        Reactor.trace "metrics event query=%Ld payload=%s" query_id payload;
      Option.iter
        (fun resolve ->
          Hashtbl.remove t.metrics_waiters query_id;
          let result =
            try Ok (Reactor.metrics_of_payload payload) with exn -> Error exn
          in
          (match result with
          | Ok metrics ->
              Metrics.Private.record_runtime t.metrics
                ~utilization:metrics.utilization
                ~active_connections:metrics.active_connections
                ~active_requests:metrics.active_requests
                ~accepted_connections:metrics.accepted_connections
                ~rejected_connections:metrics.rejected_connections
                ~completed_requests:metrics.completed_requests
                ~tls_full_handshakes:metrics.tls_full_handshakes
                ~tls_resumed_handshakes:metrics.tls_resumed_handshakes
                ~tls_handshake_failures:metrics.tls_handshake_failures
                ~event_queue_depth:metrics.event_queue_depth
                ~command_queue_depth:metrics.command_queue_depth
                ~controller_state:
                  (match metrics.controller_state with
                  | Server_backend.Normal -> "normal"
                  | Busy -> "busy"
                  | Saturated -> "saturated"
                  | Recovering -> "recovering")
                ~backpressure_enabled:metrics.backpressure_enabled
          | Error _ -> ());
          ignore (Eio.Promise.try_resolve resolve result))
        (Hashtbl.find_opt t.metrics_waiters query_id)
  | Reactor.Replay_claim event ->
      Eio.Fiber.fork ~sw:t.bridge_switch (fun () -> start_replay_claim t event)
  | Reactor.Connection_closed connection_id ->
      Hashtbl.remove t.connection_infos connection_id
  | Reactor.Pre_dispatch_response event -> observe_pre_dispatch_response t event
  | Reactor.Drained -> (
      try Reactor.stop t.runtime with Client_disconnected -> ())
  | Reactor.Stopped ->
      Hashtbl.iter
        (fun _ resolve ->
          ignore
            (Eio.Promise.try_resolve resolve
               (Error (Reactor.Runtime_error "reactor stopped"))))
        t.metrics_waiters;
      Hashtbl.clear t.metrics_waiters;
      Hashtbl.clear t.connection_infos
  | Reactor.Unknown { kind; request_id; payload = _ } ->
      let exn = Failure (Printf.sprintf "unknown bridge event %d" kind) in
      t.on_error request_id exn (Printexc.get_callstack 32)

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
  if Reactor.trace_enabled then Reactor.trace "event loop stopped"

let await_idle t =
  while Hashtbl.length t.requests > 0 do
    Eio.Fiber.yield ()
  done

let begin_drain t =
  if not t.draining then (
    t.draining <- true;
    Reactor.begin_drain t.runtime)

let metrics t =
  if Domain.self () <> t.owner_domain then
    invalid_arg "Server.metrics must be called from the server's domain";
  let query_id = t.next_metrics_query in
  t.next_metrics_query <- Int64.succ query_id;
  let promise, resolve = Eio.Promise.create () in
  Hashtbl.add t.metrics_waiters query_id resolve;
  Fun.protect
    ~finally:(fun () -> Hashtbl.remove t.metrics_waiters query_id)
    (fun () ->
      if Reactor.trace_enabled then
        Reactor.trace "request metrics query=%Ld" query_id;
      Reactor.request_metrics t.runtime query_id;
      match Eio.Promise.await promise with
      | Ok metrics -> metrics
      | Error exn -> raise exn)
