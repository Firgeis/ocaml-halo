module Status = Halo.Router.Status
module Response_headers = Halo.Router.Headers

let response_header_error error =
  Halo.Router.text Status.internal_server_error
    (Response_headers.error_message error ^ "\n")

let respond_with_headers headers respond =
  Result.fold ~ok:respond ~error:response_header_error headers

let plain_text_headers =
  Response_headers.of_list
    [("Content-Type", "text/plain; charset=utf-8")]

let informational_headers =
  Result.bind
    (Response_headers.of_list [("X-Phase", "processing")])
    (fun processing ->
      Response_headers.of_list
        [("Link", "</style.css>; rel=preload; as=style")]
      |> Result.map (fun early_hints -> processing, early_hints))

let int_env_or_default name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some value -> Option.value (int_of_string_opt value) ~default

let bool_env name =
  match Sys.getenv_opt name with
  | None -> None
  | Some value -> (
      match String.lowercase_ascii (String.trim value) with
      | "1" | "true" | "yes" | "on" -> Some true
      | "0" | "false" | "no" | "off" -> Some false
      | _ -> invalid_arg (Printf.sprintf "invalid boolean for %s" name))

let non_empty_env name =
  match Sys.getenv_opt name with
  | Some value when String.length value > 0 -> Some value
  | Some _ -> invalid_arg (Printf.sprintf "%s must not be empty" name)
  | None -> None

let split_comma_separated value =
  value
  |> String.split_on_char ','
  |> List.map String.trim
  |> List.filter (fun item -> String.length item > 0)

let int_env name =
  match Sys.getenv_opt name with
  | None -> None
  | Some value -> (
      match int_of_string_opt value with
      | Some parsed -> Some parsed
      | None -> invalid_arg (Printf.sprintf "invalid integer for %s" name))

let tls_minimum_protocol_version_from_env () =
  match Sys.getenv_opt "HALO_TLS_MIN_PROTOCOL_VERSION" with
  | None -> None
  | Some "TLS12" | Some "tls12" -> Some Halo.Server.TLS12
  | Some "TLS13" | Some "tls13" -> Some Halo.Server.TLS13
  | Some _ ->
      invalid_arg
        "HALO_TLS_MIN_PROTOCOL_VERSION must be one of: TLS12, TLS13"

let tls_client_auth_from_env () =
  let mode =
    match Sys.getenv_opt "HALO_TLS_CLIENT_AUTH_MODE" with
    | None -> Halo.Server.Client_auth_none
    | Some "none" -> Halo.Server.Client_auth_none
    | Some "optional" -> Halo.Server.Client_auth_optional
    | Some "required" -> Halo.Server.Client_auth_required
    | Some _ ->
        invalid_arg
          "HALO_TLS_CLIENT_AUTH_MODE must be one of: none, optional, required"
  in
  Halo.Server.Config.tls_client_auth
    ?ca_cert_path:(non_empty_env "HALO_TLS_CLIENT_AUTH_CA_CERT")
    ?ca_cert_dir:(non_empty_env "HALO_TLS_CLIENT_AUTH_CA_DIR")
    mode

let tls_config_from_env () =
  match
    Sys.getenv_opt "HALO_TLS_CERT_CHAIN",
    Sys.getenv_opt "HALO_TLS_PRIVATE_KEY"
  with
  | None, None -> None
  | Some cert_chain_path, Some private_key_path ->
      let alpn_protocols =
        match Sys.getenv_opt "HALO_TLS_ALPN_PROTOCOLS" with
        | None -> []
        | Some value -> split_comma_separated value
      in
      Some
        (Halo.Server.Config.tls ~cert_chain_path ~private_key_path
           ?security_policy:(non_empty_env "HALO_TLS_SECURITY_POLICY")
           ?minimum_protocol_version:(tls_minimum_protocol_version_from_env ())
           ~alpn_protocols
           ?ocsp_staple_path:(non_empty_env "HALO_TLS_OCSP_STAPLE")
           ~client_auth:(tls_client_auth_from_env ())
           ?session_tickets_enabled:(bool_env "HALO_TLS_SESSION_TICKETS")
           ?session_ticket_keys_path:
             (non_empty_env "HALO_TLS_SESSION_TICKET_KEYS_FILE")
           ?session_ticket_encrypt_decrypt_lifetime_secs:
             (int_env "HALO_TLS_SESSION_TICKET_ENCRYPT_DECRYPT_LIFETIME_SECS")
           ?session_ticket_decrypt_lifetime_secs:
             (int_env "HALO_TLS_SESSION_TICKET_DECRYPT_LIFETIME_SECS")
           ?session_ticket_initial_count:(int_env "HALO_TLS_SESSION_TICKET_COUNT")
           ?server_keying_material_lifetime_secs:
             (int_env "HALO_TLS_KEYING_MATERIAL_LIFETIME_SECS")
           ?max_early_data_size:(int_env "HALO_TLS_MAX_EARLY_DATA_SIZE")
           ())
  | Some _, None ->
      invalid_arg "HALO_TLS_PRIVATE_KEY is required when HALO_TLS_CERT_CHAIN is set"
  | None, Some _ ->
      invalid_arg "HALO_TLS_CERT_CHAIN is required when HALO_TLS_PRIVATE_KEY is set"

let visible_header_names request =
  Halo.Router.Request.fold_headers request ~init:[] ~f:(fun names name _value ->
      name :: names)
  |> List.rev
  |> String.concat ","

let add_example_header next request =
  next request |> Halo.Router.with_header "X-Example-Middleware" "post-route"

let rewrite_legacy request =
match Halo.Router.Request.raw_target request with
| "/legacy" ->
    Halo.Router.Rewrite
      {
        request_method = None;
        raw_target = Some "/";
        headers = None;
        body = None;
      }
| "/forbidden" -> Halo.Router.Respond (Halo.Router.text Status.forbidden "forbidden\n")
| _ -> Halo.Router.Continue

let write_disconnect_marker () =
  match non_empty_env "HALO_STREAM_DISCONNECT_MARKER" with
  | None -> ()
  | Some path ->
      Out_channel.with_open_bin path (fun channel ->
          output_string channel "client disconnected\n")

let run_server clock =
  let router =
    Halo.Router.(
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "early"
          and+ () = path_end in
          fun request ->
            respond_with_headers informational_headers
              (fun (processing_headers, early_hints_headers) ->
                Request.send_informational request
                  (informational Status.processing processing_headers);
                Request.send_informational request
                  (informational Status.early_hints early_hints_headers);
                text Status.ok "hello world\n"))
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "explode"
          and+ () = path_end in
          fun _request -> failwith "boom")
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "empty"
          and+ () = path_end in
          fun _request -> text Status.no_content "should not be sent")
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "cached"
          and+ () = path_end in
          fun _request -> text Status.not_modified "should not be sent")
      <|>
      route
        Matcher.(
          let+ () = request_method_is HEAD
          and+ () = path_end in
          fun _request -> text Status.ok "hello world\n")
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "stream"
          and+ () = path_end in
          fun _request ->
            respond_with_headers plain_text_headers (fun headers ->
                Halo.Stream.respond ~status:Status.ok ~headers (fun ~yield ->
                    yield "hello ";
                    Eio.Time.sleep clock 0.15;
                    yield "world\n")))
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "stream"
          and+ () = path_segment "explode"
          and+ () = path_end in
          fun _request ->
            respond_with_headers plain_text_headers (fun headers ->
                Halo.Stream.respond ~status:Status.ok ~headers (fun ~yield ->
                    yield "hello ";
                    raise Exit)))
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "sse"
          and+ () = path_end in
          fun _request ->
            Halo.Sse.respond (fun ~send ->
                send
                  (Halo.Sse.event ~event:"message" ~id:"1"
                     ["hello"; "world"]);
                Eio.Time.sleep clock 0.15;
                send (Halo.Sse.comment "keep-alive");
                send (Halo.Sse.event ~retry_ms:1500 ["done"])))
      <|>
      route
        Matcher.(
          let+ () = request_method_is POST
          and+ () = path_segment "stream"
          and+ () = path_segment "disconnect"
          and+ () = path_end
          and+ () = require_body in
          fun request ->
            Request.on_disconnect request write_disconnect_marker;
            respond_with_headers plain_text_headers (fun headers ->
                Halo.Stream.respond ~status:Status.ok ~headers (fun ~yield ->
                    yield "hello ";
                    ignore (Halo.Body.read (Request.body request)))))
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "headers"
          and+ () = path_end in
          fun _request ->
            text Status.ok "headers\n"
            |> with_content_type "text/custom"
            |> with_header "X-Test" "1")
      |> use add_example_header
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "inspect"
          and+ () = path_segment "parse-only"
          and+ () = path_end in
          fun _request -> text Status.ok "ok\n")
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "inspect"
          and+ () = path_segment "headers"
          and+ () = path_end in
          fun request -> text Status.ok (visible_header_names request))
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "inspect"
          and+ () = path_segment "header"
          and+ name = string_parameter
          and+ () = path_end in
          fun request ->
            let body =
              match Halo.Router.Request.header request name with
              | None -> "missing"
              | Some value -> "value:" ^ value
            in
            text Status.ok body)
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "html"
          and+ () = path_end in
          fun _request -> html Status.ok "<h1>hello world</h1>\n")
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_segment "json"
          and+ () = path_end in
          fun _request -> json Status.ok "{\"message\":\"hello world\"}")
      <|>
      route
        Matcher.(
          let+ () = request_method_is GET
          and+ () = path_end in
          fun _request -> text Status.ok "hello world\n")
      |> use_pre rewrite_legacy
      <|>
      route
        Matcher.(
          let+ () = request_method_is POST
          and+ () = path_segment "submit"
          and+ () = path_end
          and+ () = require_body in
          fun (request : Halo.Router.Request.t) ->
            text Status.ok
              ("submitted:"
              ^ Halo.Body.read_all (Halo.Router.Request.body request)))
      <|>
      route
        Matcher.(
          let+ () = request_method_is PATCH
          and+ () = path_segment "submit"
          and+ () = path_end
          and+ () = require_body in
          fun (request : Halo.Router.Request.t) ->
            text Status.ok
              ("patched:"
              ^ Halo.Body.read_all (Halo.Router.Request.body request))))
  in
  let request_timeout_ms =
    int_env_or_default "HALO_REQUEST_TIMEOUT_MS" 30000
  in
  let idle_timeout_ms =
    int_env_or_default "HALO_IDLE_TIMEOUT_MS" 5000
  in
  let max_request_line_size =
    int_env_or_default "HALO_MAX_REQUEST_LINE_SIZE"
      Halo.Server.Config.default_max_request_line_size
  in
  let max_header_size =
    int_env_or_default "HALO_MAX_HEADER_SIZE"
      Halo.Server.Config.default_max_header_size
  in
  let max_body_size =
    int_env_or_default "HALO_MAX_BODY_SIZE"
      Halo.Server.Config.default_max_body_size
  in
  let config =
    Halo.Server.Config.create ~port:8080
      ~max_request_line_size ~max_header_size ~max_body_size ~request_timeout_ms
      ~idle_timeout_ms
      ?tls:(tls_config_from_env ())
      router
  in
  Printf.printf "server on %s:%d using %s\n%!" config.host config.port
    (Halo.Server.runtime_backend_name ());
  Halo.Server.run [config]

let () =
  try Eio_main.run @@ fun env -> run_server (Eio.Stdenv.clock env)
  with Halo.Server.Runtime_error message ->
    prerr_endline message;
    exit 1
