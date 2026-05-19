type t
type server
type controller_state =
  | Normal
  | Busy
  | Saturated
  | Recovering

type runtime_metrics = {
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

type request_body_event =
  | Body_chunk of string
  | Body_end

type runtime_command =
  | Final of string
  | Interim of string * int
  | Await_request_body of int
  | Await_timeout of int * int
  | Start_response_stream of string * int
  | Write_response_chunk of string * int
  | Finish_response_stream

type runtime_error = string

type config = Server_config.t

type create_server_tls_config = {
  enabled : bool;
  cert_chain_path : string;
  private_key_path : string;
  security_policy : string;
  minimum_protocol_version : int;
  alpn_protocols : string;
  ocsp_staple_path : string;
  client_auth_mode : int;
  client_auth_ca_cert_path : string;
  client_auth_ca_cert_dir : string;
  session_tickets_enabled : int;
  session_ticket_keys_path : string;
  session_ticket_encrypt_decrypt_lifetime_secs : int;
  session_ticket_decrypt_lifetime_secs : int;
  session_ticket_initial_count : int;
  server_keying_material_lifetime_secs : int;
  max_early_data_size : int;
}

let ( let*? ) error next =
  match error with
  | Some message -> Error message
  | None -> next ()

let response_reason_phrase status =
  match status with
  | 100 -> "Continue"
  | 101 -> "Switching Protocols"
  | 102 -> "Processing"
  | 103 -> "Early Hints"
  | 200 -> "OK"
  | 201 -> "Created"
  | 204 -> "No Content"
  | 304 -> "Not Modified"
  | 400 -> "Bad Request"
  | 404 -> "Not Found"
  | 405 -> "Method Not Allowed"
  | 408 -> "Request Timeout"
  | 411 -> "Length Required"
  | 413 -> "Content Too Large"
  | 414 -> "URI Too Long"
  | 417 -> "Expectation Failed"
  | 431 -> "Request Header Fields Too Large"
  | 500 -> "Internal Server Error"
  | 503 -> "Service Unavailable"
  | 505 -> "HTTP Version Not Supported"
  | 501 -> "Not Implemented"
  | _ -> "OK"

let response_version http_major http_minor =
  if http_major = 1 then
    Printf.sprintf "HTTP/1.%d" http_minor
  else
    "HTTP/1.1"

let connection_header_value ~http_major ~http_minor ~keep_alive =
  if keep_alive then
    if http_major = 1 && http_minor = 0 then
      Some "keep-alive"
    else
      None
  else
    Some "close"

let is_framework_header name =
  let normalized_name = String.lowercase_ascii name in
  String.equal normalized_name "content-length"
  || String.equal normalized_name "connection"
  || String.equal normalized_name "transfer-encoding"

let add_response_header_line acc (name, value) =
  if is_framework_header name then
    acc
  else
    Printf.sprintf "%s: %s\r\n" name value :: acc

let status_forbids_body status =
  (status >= 100 && status < 200) || status = 204 || status = 304

let server_allow_header_value = "GET, HEAD, POST, PATCH, PUT, DELETE, OPTIONS, TRACE"

let response_body_mode ~request_method (response : Router.response) =
  if status_forbids_body response.status then
    `Bodyless_status
  else if String.equal request_method "HEAD" then
    `Head
  else
    `Normal

let serialize_response_head_message ~status ~headers ~http_major ~http_minor
    ~connection ~extra_headers =
  let reason = response_reason_phrase status in
  let header_lines = List.fold_left add_response_header_line [] headers in
  let header_lines =
    match connection with
    | None -> header_lines
    | Some connection ->
        Printf.sprintf "Connection: %s\r\n" connection :: header_lines
  in
  let header_lines = extra_headers @ List.rev header_lines |> String.concat "" in
  Printf.sprintf "%s %d %s\r\n%s\r\n"
    (response_version http_major http_minor)
    status reason header_lines

let serialize_response_message ~status ~headers ~body_mode ~body ~http_major
    ~http_minor ~connection =
  let content_length_line =
    match body_mode with
    | `Bodyless_status -> None
    | `Head | `Normal -> Some (Printf.sprintf "Content-Length: %d\r\n" (String.length body))
  in
  let extra_headers =
    match content_length_line with
    | None -> []
    | Some line -> [line]
  in
  let response_body = match body_mode with `Bodyless_status | `Head -> "" | `Normal -> body in
  serialize_response_head_message ~status ~headers ~http_major ~http_minor
    ~connection ~extra_headers
  ^ response_body

let serialize_informational_response ~http_major ~http_minor
    (response : Router.informational_response) =
  serialize_response_message ~status:response.status ~headers:response.headers
    ~body_mode:`Bodyless_status ~body:"" ~http_major ~http_minor
    ~connection:None

let serialize_internal_server_error_response ~http_major ~http_minor =
  serialize_response_message ~status:500 ~headers:[] ~body_mode:`Normal
    ~body:"internal server error\n" ~http_major ~http_minor
    ~connection:(Some "close")

let serialize_response ~request_method ~http_major ~http_minor ~keep_alive
    (response : Router.response) =
  let body_mode = response_body_mode ~request_method response in
  serialize_response_message ~status:response.status ~headers:response.headers
    ~body_mode ~body:response.body ~http_major ~http_minor
    ~connection:(connection_header_value ~http_major ~http_minor ~keep_alive)

let serialize_streaming_response_head ~request_method ~http_major ~http_minor
    ~keep_alive (response : Router.response_head) =
  let body_mode =
    response_body_mode ~request_method
      { Router.status = response.status; headers = response.headers; body = "" }
  in
  match body_mode with
  | `Normal ->
      serialize_response_head_message ~status:response.status
        ~headers:response.headers ~http_major ~http_minor
        ~connection:(connection_header_value ~http_major ~http_minor ~keep_alive)
        ~extra_headers:["Transfer-Encoding: chunked\r\n"]
  | `Bodyless_status | `Head ->
      invalid_arg "streamed responses require a response body"

let serialize_response_chunk chunk =
  Printf.sprintf "%x\r\n%s\r\n" (String.length chunk) chunk

let trace_body ~request_method_text ~raw_target ~headers ~body http_major
    http_minor =
  let request_line =
    Printf.sprintf "%s %s HTTP/%d.%d\r\n"
      request_method_text
      raw_target
      http_major
      http_minor
  in
  let header_lines =
    headers
    |> List.map (fun (name, value) -> Printf.sprintf "%s: %s\r\n" name value)
    |> String.concat ""
  in
  request_line ^ header_lines ^ "\r\n" ^ body

let options_star_response =
  {
    Router.status = 200;
    headers = [("Allow", server_allow_header_value)];
    body = "";
  }

let trace_response ~request_method_text ~raw_target ~headers ~body http_major
    http_minor =
  {
    Router.status = 200;
    headers = [("Content-Type", "message/http")];
    body = trace_body ~request_method_text ~raw_target ~headers ~body
      http_major http_minor;
  }

let next_dispatch_token = ref 0
type suspended_dispatch =
  | Resume_after_write of {
      resume : unit -> runtime_command;
      cancel : exn -> unit;
    }
  | Resume_with_body of {
      resume : request_body_event -> runtime_command;
      cancel : exn -> unit;
    }
[@@warning "-37"]

type response_stream_state =
  | Response_not_streaming
  | Response_streaming
  | Response_stream_finished

module Config = Server_config

external runtime_backend_name : unit -> string
  = "ocaml_halo_runtime_backend_name"
external create : unit -> (t, string) result = "ocaml_halo_runtime_create"
external create_server_raw :
  t ->
  host:string ->
  port:int ->
  backlog:int ->
  max_request_size:int ->
  request_timeout_ms:int ->
  idle_timeout_ms:int ->
  tls_enabled:bool ->
  tls_cert_chain_path:string ->
  tls_private_key_path:string ->
  tls_security_policy:string ->
  tls_minimum_protocol_version:int ->
  tls_alpn_protocols:string ->
  tls_ocsp_staple_path:string ->
  tls_client_auth_mode:int ->
  tls_client_auth_ca_cert_path:string ->
  tls_client_auth_ca_cert_dir:string ->
  tls_session_tickets_enabled:int ->
  tls_session_ticket_keys_path:string ->
  tls_session_ticket_encrypt_decrypt_lifetime_secs:int ->
  tls_session_ticket_decrypt_lifetime_secs:int ->
  tls_session_ticket_initial_count:int ->
  tls_server_keying_material_lifetime_secs:int ->
  tls_max_early_data_size:int ->
  Router.t ->
  (server, string) result
  = "ocaml_halo_create_server_bytecode" "ocaml_halo_create_server"
external close_server : server -> runtime_error option = "ocaml_halo_server_close"
external configure_controller :
  t ->
  sample_interval_ms:int ->
  busy_threshold:float ->
  saturated_threshold:float ->
  recovery_threshold:float ->
  busy_samples:int ->
  saturated_samples:int ->
  recovery_samples:int ->
  runtime_error option
  = "ocaml_halo_runtime_configure_controller_bytecode"
    "ocaml_halo_runtime_configure_controller"
external runtime_metrics_raw :
  t ->
  float
  * int64
  * int64
  * int64
  * int64
  * int64
  * int64
  * int64
  * int64
  * int
  * bool
  = "ocaml_halo_runtime_metrics"
external run : t -> unit = "ocaml_halo_runtime_run"
external run_once : t -> bool = "ocaml_halo_runtime_run_once"
external stop : t -> unit = "ocaml_halo_runtime_stop"
external close : t -> runtime_error option = "ocaml_halo_runtime_close"

let controller_state_of_int = function
  | 0 -> Normal
  | 1 -> Busy
  | 2 -> Saturated
  | 3 -> Recovering
  | state ->
      invalid_arg (Printf.sprintf "unknown runtime controller state: %d" state)

let option_value value ~default =
  match value with
  | None -> default
  | Some inner -> inner

let tls_minimum_protocol_version_to_int = function
  | TLS12 -> 12
  | TLS13 -> 13

let tls_client_auth_mode_to_int = function
  | Client_auth_none -> 0
  | Client_auth_optional -> 1
  | Client_auth_required -> 2

let runtime_metrics runtime =
  let ( utilization,
        active_connections,
        active_requests,
        accepted_connections,
        rejected_connections,
        completed_requests,
        tls_full_handshakes,
        tls_resumed_handshakes,
        tls_handshake_failures,
        controller_state,
        backpressure_enabled ) =
    runtime_metrics_raw runtime
  in
  {
    utilization;
    active_connections;
    active_requests;
    accepted_connections;
    rejected_connections;
    completed_requests;
    tls_full_handshakes;
    tls_resumed_handshakes;
    tls_handshake_failures;
    controller_state = controller_state_of_int controller_state;
    backpressure_enabled;
  }

let create_server_tls_args (tls : tls_config option) =
  match tls with
  | None ->
      {
        enabled = false;
        cert_chain_path = "";
        private_key_path = "";
        security_policy = "";
        minimum_protocol_version = 0;
        alpn_protocols = "";
        ocsp_staple_path = "";
        client_auth_mode = 0;
        client_auth_ca_cert_path = "";
        client_auth_ca_cert_dir = "";
        session_tickets_enabled = -1;
        session_ticket_keys_path = "";
        session_ticket_encrypt_decrypt_lifetime_secs = -1;
        session_ticket_decrypt_lifetime_secs = -1;
        session_ticket_initial_count = -1;
        server_keying_material_lifetime_secs = -1;
        max_early_data_size = -1;
      }
  | Some tls ->
      {
        enabled = true;
        cert_chain_path = tls.cert_chain_path;
        private_key_path = tls.private_key_path;
        security_policy = option_value tls.security_policy ~default:"";
        minimum_protocol_version =
          (match tls.minimum_protocol_version with
          | None -> 0
          | Some version -> tls_minimum_protocol_version_to_int version);
        alpn_protocols = String.concat "\000" tls.alpn_protocols;
        ocsp_staple_path = option_value tls.ocsp_staple_path ~default:"";
        client_auth_mode = tls_client_auth_mode_to_int tls.client_auth.mode;
        client_auth_ca_cert_path =
          option_value tls.client_auth.ca_cert_path ~default:"";
        client_auth_ca_cert_dir =
          option_value tls.client_auth.ca_cert_dir ~default:"";
        session_tickets_enabled =
          (match tls.session_tickets_enabled with
          | None -> -1
          | Some false -> 0
          | Some true -> 1);
        session_ticket_keys_path =
          option_value tls.session_ticket_keys_path ~default:"";
        session_ticket_encrypt_decrypt_lifetime_secs =
          option_value tls.session_ticket_encrypt_decrypt_lifetime_secs
            ~default:(-1);
        session_ticket_decrypt_lifetime_secs =
          option_value tls.session_ticket_decrypt_lifetime_secs ~default:(-1);
        session_ticket_initial_count =
          option_value tls.session_ticket_initial_count ~default:(-1);
        server_keying_material_lifetime_secs =
          option_value tls.server_keying_material_lifetime_secs ~default:(-1);
        max_early_data_size = option_value tls.max_early_data_size ~default:(-1);
      }

let listen runtime (config : config) =
  let*? () = Config.validate config in
  let*? () =
    configure_controller runtime ~sample_interval_ms:config.controller.sample_interval_ms
      ~busy_threshold:config.controller.busy_threshold
      ~saturated_threshold:config.controller.saturated_threshold
      ~recovery_threshold:config.controller.recovery_threshold
      ~busy_samples:config.controller.busy_samples
      ~saturated_samples:config.controller.saturated_samples
      ~recovery_samples:config.controller.recovery_samples
  in
  let tls = create_server_tls_args config.tls in
  create_server_raw runtime ~host:config.host ~port:config.port
    ~backlog:config.backlog ~max_request_size:config.max_request_size
    ~request_timeout_ms:config.request_timeout_ms
    ~idle_timeout_ms:config.idle_timeout_ms ~tls_enabled:tls.enabled
    ~tls_cert_chain_path:tls.cert_chain_path
    ~tls_private_key_path:tls.private_key_path
    ~tls_security_policy:tls.security_policy
    ~tls_minimum_protocol_version:tls.minimum_protocol_version
    ~tls_alpn_protocols:tls.alpn_protocols
    ~tls_ocsp_staple_path:tls.ocsp_staple_path
    ~tls_client_auth_mode:tls.client_auth_mode
    ~tls_client_auth_ca_cert_path:tls.client_auth_ca_cert_path
    ~tls_client_auth_ca_cert_dir:tls.client_auth_ca_cert_dir
    ~tls_session_tickets_enabled:tls.session_tickets_enabled
    ~tls_session_ticket_keys_path:tls.session_ticket_keys_path
    ~tls_session_ticket_encrypt_decrypt_lifetime_secs:
      tls.session_ticket_encrypt_decrypt_lifetime_secs
    ~tls_session_ticket_decrypt_lifetime_secs:
      tls.session_ticket_decrypt_lifetime_secs
    ~tls_session_ticket_initial_count:tls.session_ticket_initial_count
    ~tls_server_keying_material_lifetime_secs:
      tls.server_keying_material_lifetime_secs
    ~tls_max_early_data_size:tls.max_early_data_size config.router

let suspended_dispatches : (int, suspended_dispatch) Hashtbl.t = Hashtbl.create 16

let is_disconnect_exception = function
  | Server_effects.Request_cancelled | Server_effects.Client_disconnected -> true
  | _ -> false

let run_disconnect_cleanups disconnect_cleanups =
  let pending = List.rev !disconnect_cleanups in
  disconnect_cleanups := [];
  List.iter
    (fun cleanup ->
      try cleanup () with
      | exn -> prerr_endline (Printexc.to_string exn))
    pending

let fresh_dispatch_token () =
  incr next_dispatch_token;
  !next_dispatch_token

let remove_suspended_dispatch token =
  Hashtbl.remove suspended_dispatches token

let finalize_response ~request_method_text ~http_major ~http_minor ~keep_alive
    response =
  Final
    (serialize_response ~request_method:request_method_text ~http_major
       ~http_minor ~keep_alive response)

let register_after_write ~resume ~cancel =
  let token = fresh_dispatch_token () in
  Hashtbl.replace suspended_dispatches token
    (Resume_after_write { resume; cancel });
  token

let register_body_waiter ~resume ~cancel =
  let token = fresh_dispatch_token () in
  Hashtbl.replace suspended_dispatches token
    (Resume_with_body { resume; cancel });
  token

let cancel_continuation ~client_connected ~disconnect_cleanups cancel exn =
  if is_disconnect_exception exn then (
    client_connected := false;
    run_disconnect_cleanups disconnect_cleanups);
  cancel exn

let handle_send_informational ~client_connected ~disconnect_cleanups ~http_major
    ~http_minor informational k =
  let resume () = Effect.Deep.continue k () in
  let cancel exn =
    try
      ignore (Effect.Deep.discontinue k exn)
    with
    | _ -> ()
  in
  let token =
    register_after_write ~resume
      ~cancel:(cancel_continuation ~client_connected ~disconnect_cleanups cancel)
  in
  Interim
    ( serialize_informational_response ~http_major ~http_minor informational,
      token )

let handle_await_request_body ~client_connected ~disconnect_cleanups k =
  let resume event =
    match event with
    | Body_chunk chunk -> Effect.Deep.continue k (Some chunk)
    | Body_end -> Effect.Deep.continue k None
  in
  let cancel exn =
    try
      ignore (Effect.Deep.discontinue k exn)
    with
    | _ -> ()
  in
  let token =
    register_body_waiter ~resume
      ~cancel:(cancel_continuation ~client_connected ~disconnect_cleanups cancel)
  in
  Await_request_body token

let handle_sleep_ms ~client_connected ~disconnect_cleanups delay_ms k =
  if delay_ms < 0 then
    invalid_arg "sleep_ms delay must be non-negative";
  let resume () = Effect.Deep.continue k () in
  let cancel exn =
    try
      ignore (Effect.Deep.discontinue k exn)
    with
    | _ -> ()
  in
  let token =
    register_after_write ~resume
      ~cancel:(cancel_continuation ~client_connected ~disconnect_cleanups cancel)
  in
  Await_timeout (delay_ms, token)

let handle_drain_request_body ~client_connected ~disconnect_cleanups k =
  let buffer = Buffer.create 256 in
  let rec wait_for_body () =
    let resume event =
      match event with
      | Body_chunk chunk ->
          Buffer.add_string buffer chunk;
          wait_for_body ()
      | Body_end -> Effect.Deep.continue k (Buffer.contents buffer)
    in
    let cancel exn =
      try
        ignore (Effect.Deep.discontinue k exn)
      with
      | _ -> ()
    in
    let token =
      register_body_waiter ~resume
        ~cancel:
          (cancel_continuation ~client_connected ~disconnect_cleanups cancel)
    in
    Await_request_body token
  in
  wait_for_body ()

let handle_drain_request_body_with_prefix ~client_connected
    ~disconnect_cleanups prefix k =
  let buffer = Buffer.create (String.length prefix + 256) in
  Buffer.add_string buffer prefix;
  let skip_initial_chunk = ref (prefix <> "") in
  let rec wait_for_body () =
    let resume event =
      match event with
      | Body_chunk chunk ->
          if !skip_initial_chunk then
            skip_initial_chunk := false
          else Buffer.add_string buffer chunk;
          wait_for_body ()
      | Body_end -> Effect.Deep.continue k (Buffer.contents buffer)
    in
    let cancel exn =
      try
        ignore (Effect.Deep.discontinue k exn)
      with
      | _ -> ()
    in
    let token =
      register_body_waiter ~resume
        ~cancel:
          (cancel_continuation ~client_connected ~disconnect_cleanups cancel)
    in
    Await_request_body token
  in
  wait_for_body ()

let handle_start_response_stream ~stream_state ~client_connected
    ~disconnect_cleanups ~request_method_text ~http_major ~http_minor
    ~keep_alive (response_head : Router.response_head)
    (k : (unit, runtime_command) Effect.Deep.continuation) =
  if http_major = 1 && http_minor = 0 then
    Effect.Deep.discontinue k
      (Invalid_argument "streamed responses require HTTP/1.1")
  else (
    let body_mode =
      response_body_mode ~request_method:request_method_text
        { Router.status = response_head.status; headers = response_head.headers; body = "" }
    in
    match body_mode with
    | `Bodyless_status | `Head ->
        Effect.Deep.discontinue k
          (Invalid_argument "streamed responses require a response body")
    | `Normal ->
        let response_head_message =
          serialize_streaming_response_head ~request_method:request_method_text
            ~http_major ~http_minor ~keep_alive response_head
        in
        let resume () =
          stream_state := Response_streaming;
          Effect.Deep.continue k ()
        in
        let cancel exn =
          try
            ignore (Effect.Deep.discontinue k exn)
          with
          | _ -> ()
        in
        let token =
          register_after_write ~resume
            ~cancel:
              (cancel_continuation ~client_connected ~disconnect_cleanups cancel)
        in
        Start_response_stream
          (response_head_message, token))

let handle_write_response_chunk ~stream_state ~client_connected
    ~disconnect_cleanups chunk k =
  if !stream_state <> Response_streaming then
    invalid_arg "response stream has not been started";
  let resume () = Effect.Deep.continue k () in
  let cancel exn =
    try
      ignore (Effect.Deep.discontinue k exn)
    with
    | _ -> ()
  in
  let token =
    register_after_write ~resume
      ~cancel:(cancel_continuation ~client_connected ~disconnect_cleanups cancel)
  in
  Write_response_chunk (serialize_response_chunk chunk, token)

let handle_finish_response_stream ~stream_state _k =
  if !stream_state <> Response_streaming then
    invalid_arg "response stream has not been started";
  stream_state := Response_stream_finished;
  Finish_response_stream

let handle_on_client_disconnect disconnect_cleanups cleanup k =
  disconnect_cleanups := cleanup :: !disconnect_cleanups;
  Effect.Deep.continue k ()

let handle_check_client_connected client_connected k =
  if !client_connected then
    Effect.Deep.continue k ()
  else
    Effect.Deep.discontinue k Server_effects.Client_disconnected

let run_staged_dispatch ~request_method_text ~http_major ~http_minor
    ~keep_alive thunk =
  let stream_state = ref Response_not_streaming in
  let client_connected = ref true in
  let disconnect_cleanups = ref [] in
  Effect.Deep.match_with thunk ()
    {
      retc =
        (fun response ->
          match !stream_state with
          | Response_not_streaming ->
              finalize_response ~request_method_text ~http_major ~http_minor
                ~keep_alive response
          | Response_streaming ->
              stream_state := Response_stream_finished;
              Finish_response_stream
          | Response_stream_finished ->
              Finish_response_stream);
      exnc =
        (fun _exn ->
          match !stream_state with
          | Response_not_streaming ->
              Final
                (serialize_internal_server_error_response ~http_major
                   ~http_minor)
          | Response_streaming | Response_stream_finished ->
              Finish_response_stream);
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Router.Drain_request_body ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_drain_request_body ~client_connected
                    ~disconnect_cleanups k)
          | Router.Drain_request_body_with_prefix prefix ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_drain_request_body_with_prefix ~client_connected
                    ~disconnect_cleanups prefix k)
          | Server_effects.Await_request_body ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_await_request_body ~client_connected
                    ~disconnect_cleanups k)
          | Server_effects.Sleep_ms delay_ms ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_sleep_ms ~client_connected ~disconnect_cleanups delay_ms
                    k)
          | Server_effects.Start_response_stream response_head ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_start_response_stream ~stream_state ~client_connected
                    ~disconnect_cleanups ~request_method_text ~http_major
                    ~http_minor ~keep_alive
                    response_head k)
          | Server_effects.Write_response_chunk chunk ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_write_response_chunk ~stream_state ~client_connected
                    ~disconnect_cleanups chunk k)
          | Server_effects.Finish_response_stream ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_finish_response_stream ~stream_state k)
          | Server_effects.Send_informational informational ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_send_informational ~client_connected
                    ~disconnect_cleanups ~http_major ~http_minor
                    informational k)
          | Server_effects.On_client_disconnect cleanup ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_on_client_disconnect disconnect_cleanups cleanup k)
          | Server_effects.Check_client_connected ->
              Some
                (fun (k : (a, runtime_command) Effect.Deep.continuation) ->
                  handle_check_client_connected client_connected k)
          | _ -> None);
    }

let dispatch_request router borrowed_request http_major http_minor keep_alive =
  let request_method_text =
    Router.Borrowed.request_method_text borrowed_request
  in
  match Router.Borrowed.request_method borrowed_request with
  | None ->
      Final
        (serialize_response_message ~status:501 ~headers:[] ~body_mode:`Normal
           ~body:"not implemented\n" ~http_major ~http_minor
           ~connection:
             (connection_header_value ~http_major ~http_minor ~keep_alive:false))
  | Some request_method ->
      let request_target = Router.Borrowed.target borrowed_request in
      let response_thunk () =
        match request_method with
        | Router.CONNECT ->
            Router.text 501 "connect not implemented\n"
        | Router.OPTIONS when String.equal request_target "*" ->
            options_star_response
        | Router.TRACE ->
            let raw_headers =
              Router.Borrowed.copy_raw_headers borrowed_request
            in
            let raw_body = Router.Borrowed.full_body borrowed_request in
            trace_response ~request_method_text ~raw_target:request_target
              ~headers:raw_headers ~body:raw_body http_major http_minor
        | _request_method ->
            Router.Borrowed.dispatch_borrowed router borrowed_request
      in
      run_staged_dispatch ~request_method_text ~http_major ~http_minor
        ~keep_alive response_thunk

let resume_dispatch token =
  match Hashtbl.find suspended_dispatches token with
  | Resume_after_write dispatch ->
      remove_suspended_dispatch token;
      dispatch.resume ()
  | Resume_with_body _ ->
      invalid_arg "dispatch token is waiting for a request body event"

let resume_dispatch_with_body_event token event =
  match Hashtbl.find suspended_dispatches token with
  | Resume_after_write _ ->
      invalid_arg "dispatch token is not waiting for a request body event"
  | Resume_with_body dispatch ->
      remove_suspended_dispatch token;
      dispatch.resume event

let resume_dispatch_with_body_chunk token chunk =
  resume_dispatch_with_body_event token (Body_chunk chunk)

let resume_dispatch_with_body_end token =
  resume_dispatch_with_body_event token Body_end

let cancel_dispatch token =
  match Hashtbl.find_opt suspended_dispatches token with
  | None -> ()
  | Some (Resume_after_write dispatch) ->
      remove_suspended_dispatch token;
      dispatch.cancel Server_effects.Client_disconnected
  | Some (Resume_with_body dispatch) ->
      remove_suspended_dispatch token;
      dispatch.cancel Server_effects.Client_disconnected

let cancel_all_dispatches exn =
  let pending =
    Hashtbl.fold
      (fun _token dispatch acc -> dispatch :: acc)
      suspended_dispatches []
  in
  Hashtbl.clear suspended_dispatches;
  List.iter
    (function
      | Resume_after_write dispatch -> dispatch.cancel exn
      | Resume_with_body dispatch -> dispatch.cancel exn)
    pending

let serialize_bad_request_response () =
  Router.text 400 "bad request\n"
  |> serialize_response ~request_method:"GET" ~http_major:1 ~http_minor:1
       ~keep_alive:false

let serialize_error_response status body =
  Router.text status body
  |> serialize_response ~request_method:"GET" ~http_major:1 ~http_minor:1
       ~keep_alive:false

let () = Callback.register "ocaml_halo_dispatch_request" dispatch_request
let () = Callback.register "ocaml_halo_resume_dispatch" resume_dispatch
let () =
  Callback.register "ocaml_halo_resume_dispatch_with_body_chunk"
    resume_dispatch_with_body_chunk
let () =
  Callback.register "ocaml_halo_resume_dispatch_with_body_end"
    resume_dispatch_with_body_end
let () = Callback.register "ocaml_halo_cancel_dispatch" cancel_dispatch
let () =
  Callback.register "ocaml_halo_serialize_bad_request_response"
    serialize_bad_request_response
let () =
  Callback.register "ocaml_halo_serialize_error_response"
    serialize_error_response

let with_runtime f =
  Server_effects.run @@ fun () ->
  let runtime =
    match create () with
    | Ok runtime -> runtime
    | Error message -> Server_effects.fail (Error.Runtime_error message)
  in
  let close_runtime () =
    cancel_all_dispatches Server_effects.Request_cancelled;
    match close runtime with
    | None -> ()
    | Some message -> prerr_endline message
  in
  Server_effects.finalize_runtime close_runtime;
  f runtime
