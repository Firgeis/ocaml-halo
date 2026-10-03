(* Reactor-domain backend. Public lifecycle is defined in [Server]. *)
type t
type server
type controller_state = Normal | Busy | Saturated | Recovering

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
  event_queue_depth : int64;
  command_queue_depth : int64;
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

module Config = Server_config

external runtime_backend_name : unit -> string
  = "ocaml_halo_runtime_backend_name"

external create_server_raw :
  t ->
  host:string ->
  port:int ->
  backlog:int ->
  max_request_line_size:int ->
  max_header_size:int ->
  max_body_size:int ->
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
  ipv6_mode:int ->
  reuse_port:bool ->
  (server, string) result
  = "ocaml_halo_create_server_bytecode" "ocaml_halo_create_server"

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

external close : t -> runtime_error option = "ocaml_halo_runtime_close"
external release_server : server -> unit = "ocaml_halo_server_release"

let option_value value ~default =
  match value with None -> default | Some inner -> inner

let tls_minimum_protocol_version_to_int = function TLS12 -> 12 | TLS13 -> 13

let tls_client_auth_mode_to_int = function
  | Client_auth_none -> 0
  | Client_auth_optional -> 1
  | Client_auth_required -> 2

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

let ( let*? ) error next =
  match error with Some message -> Error message | None -> next ()

let listen_config runtime (config : config) =
  let*? () = Config.validate config in
  let*? () =
    configure_controller runtime
      ~sample_interval_ms:config.controller.sample_interval_ms
      ~busy_threshold:config.controller.busy_threshold
      ~saturated_threshold:config.controller.saturated_threshold
      ~recovery_threshold:config.controller.recovery_threshold
      ~busy_samples:config.controller.busy_samples
      ~saturated_samples:config.controller.saturated_samples
      ~recovery_samples:config.controller.recovery_samples
  in
  let tls = create_server_tls_args config.tls in
  create_server_raw runtime ~host:config.host ~port:config.port
    ~backlog:config.backlog ~max_request_line_size:config.max_request_line_size
    ~max_header_size:config.max_header_size ~max_body_size:config.max_body_size
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
    ~tls_max_early_data_size:tls.max_early_data_size
    ~ipv6_mode:
      (match config.ipv6_mode with
      | None -> 0
      | Some Server_config.IPv6_only -> 1
      | Some Server_config.Dual_stack -> 2)
    ~reuse_port:config.reuse_port

let listen runtime (config : config) =
  match listen_config runtime config with
  | Ok _ as result -> result
  | Error message ->
      let host =
        if String.contains config.host ':' then "[" ^ config.host ^ "]"
        else config.host
      in
      let ipv6_mode =
        match config.ipv6_mode with
        | None -> "default"
        | Some Server_config.IPv6_only -> "IPv6_only"
        | Some Server_config.Dual_stack -> "Dual_stack"
      in
      Error
        (Printf.sprintf "listener %s:%d (ipv6_mode=%s, reuse_port=%b): %s" host
           config.port ipv6_mode config.reuse_port message)
