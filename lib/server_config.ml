type controller_config = {
  sample_interval_ms : int;
  busy_threshold : float;
  saturated_threshold : float;
  recovery_threshold : float;
  busy_samples : int;
  saturated_samples : int;
  recovery_samples : int;
}

type tls_minimum_protocol_version =
  | TLS12
  | TLS13

type tls_client_auth_mode =
  | Client_auth_none
  | Client_auth_optional
  | Client_auth_required

type tls_client_auth_config = {
  mode : tls_client_auth_mode;
  ca_cert_path : string option;
  ca_cert_dir : string option;
}

type tls_config = {
  cert_chain_path : string;
  private_key_path : string;
  security_policy : string option;
  minimum_protocol_version : tls_minimum_protocol_version option;
  alpn_protocols : string list;
  ocsp_staple_path : string option;
  client_auth : tls_client_auth_config;
  session_tickets_enabled : bool option;
  session_ticket_keys_path : string option;
  session_ticket_encrypt_decrypt_lifetime_secs : int option;
  session_ticket_decrypt_lifetime_secs : int option;
  session_ticket_initial_count : int option;
  server_keying_material_lifetime_secs : int option;
  max_early_data_size : int option;
}

type t = {
  host : string;
  port : int;
  backlog : int;
  max_request_size : int;
  request_timeout_ms : int;
  idle_timeout_ms : int;
  tls : tls_config option;
  controller : controller_config;
  router : Router.t;
}

let option_has value predicate =
  match value with
  | None -> false
  | Some inner -> predicate inner

let option_is_some = function
  | None -> false
  | Some _ -> true

let ( let*? ) validation next =
  match validation with
  | None -> next ()
  | Some _ as error -> error

let default_host = "127.0.0.1"
let default_backlog = 128
let default_max_request_size = 8192
let default_request_timeout_ms = 30000
let default_idle_timeout_ms = 5000

let default_controller =
  {
    sample_interval_ms = 100;
    busy_threshold = 0.85;
    saturated_threshold = 0.92;
    recovery_threshold = 0.60;
    busy_samples = 3;
    saturated_samples = 5;
    recovery_samples = 5;
  }

let tls_client_auth ?ca_cert_path ?ca_cert_dir mode =
  { mode; ca_cert_path; ca_cert_dir }

let backlog_error (config : t) =
  if config.backlog <= 0 then
    Some "server backlog must be positive"
  else
    None

let max_request_size_error (config : t) =
  if config.max_request_size <= 0 then
    Some "server max_request_size must be positive"
  else
    None

let request_timeout_error (config : t) =
  if config.request_timeout_ms < 0 then
    Some "server request_timeout_ms must be non-negative"
  else
    None

let idle_timeout_error (config : t) =
  if config.idle_timeout_ms < 0 then
    Some "server idle_timeout_ms must be non-negative"
  else
    None

let tls_cert_chain_path_error (tls : tls_config) =
  if String.length tls.cert_chain_path = 0 then
    Some "server tls cert_chain_path must not be empty"
  else
    None

let tls_private_key_path_error (tls : tls_config) =
  if String.length tls.private_key_path = 0 then
    Some "server tls private_key_path must not be empty"
  else
    None

let tls_security_policy_conflict_error (tls : tls_config) =
  if option_is_some tls.security_policy
     && option_is_some tls.minimum_protocol_version
  then
    Some
      "server tls security_policy cannot be combined with minimum_protocol_version"
  else
    None

let tls_security_policy_error (tls : tls_config) =
  if option_has tls.security_policy (fun value -> String.length value = 0) then
    Some "server tls security_policy must not be empty"
  else
    None

let tls_alpn_empty_protocol_error (tls : tls_config) =
  if List.exists (fun protocol -> String.length protocol = 0) tls.alpn_protocols then
    Some "server tls alpn_protocols must not contain empty values"
  else
    None

let tls_alpn_protocol_format_error (tls : tls_config) =
  if
    List.exists
      (fun protocol ->
        String.length protocol > 255 || String.contains protocol '\000')
      tls.alpn_protocols
  then
    Some
      "server tls alpn_protocols entries must be at most 255 bytes and cannot contain NUL"
  else
    None

let tls_ocsp_staple_path_error (tls : tls_config) =
  if option_has tls.ocsp_staple_path (fun value -> String.length value = 0) then
    Some "server tls ocsp_staple_path must not be empty"
  else
    None

let tls_max_early_data_non_negative_error (tls : tls_config) =
  if option_has tls.max_early_data_size (fun value -> value < 0) then
    Some "server tls max_early_data_size must be non-negative"
  else
    None

let tls_max_early_data_range_error (tls : tls_config) =
  if option_has tls.max_early_data_size (fun value -> value > 0xFFFF_FFFF) then
    Some "server tls max_early_data_size must fit in 32 bits"
  else
    None

let tls_session_ticket_keys_path_error (tls : tls_config) =
  if option_has tls.session_ticket_keys_path (fun value -> String.length value = 0) then
    Some "server tls session_ticket_keys_path must not be empty"
  else
    None

let tls_session_ticket_encrypt_decrypt_lifetime_error (tls : tls_config) =
  if
    option_has tls.session_ticket_encrypt_decrypt_lifetime_secs (fun value ->
        value < 0)
  then
    Some
      "server tls session_ticket_encrypt_decrypt_lifetime_secs must be non-negative"
  else
    None

let tls_session_ticket_decrypt_lifetime_error (tls : tls_config) =
  if option_has tls.session_ticket_decrypt_lifetime_secs (fun value -> value < 0) then
    Some "server tls session_ticket_decrypt_lifetime_secs must be non-negative"
  else
    None

let tls_session_ticket_initial_count_error (tls : tls_config) =
  if
    option_has tls.session_ticket_initial_count (fun value ->
        value < 0 || value > 0xFF)
  then
    Some "server tls session_ticket_initial_count must fit in uint8"
  else
    None

let tls_server_keying_material_lifetime_error (tls : tls_config) =
  if
    option_has tls.server_keying_material_lifetime_secs (fun value ->
        value < 0 || value > 0xFFFF_FFFF)
  then
    Some "server tls server_keying_material_lifetime_secs must fit in uint32"
  else
    None

let tls_resumption_requires_session_tickets_error (tls : tls_config) =
  match tls.session_tickets_enabled with
  | Some false ->
      if
        option_is_some tls.session_ticket_keys_path
        || option_is_some tls.session_ticket_encrypt_decrypt_lifetime_secs
        || option_is_some tls.session_ticket_decrypt_lifetime_secs
        || option_is_some tls.session_ticket_initial_count
        || option_is_some tls.server_keying_material_lifetime_secs
        || option_is_some tls.max_early_data_size
      then
        Some
          "server tls resumption settings require session_tickets_enabled to be true"
      else
        None
  | _ -> None

let tls_early_data_requires_session_tickets_error (tls : tls_config) =
  match tls.max_early_data_size with
  | Some value when value > 0 -> (
      match tls.session_tickets_enabled with
      | Some true -> None
      | _ ->
          Some
            "server tls max_early_data_size requires session_tickets_enabled to be true")
  | _ -> None

let tls_client_auth_ca_cert_path_error (tls : tls_config) =
  if option_has tls.client_auth.ca_cert_path (fun value -> String.length value = 0) then
    Some "server tls client_auth ca_cert_path must not be empty"
  else
    None

let tls_client_auth_ca_cert_dir_error (tls : tls_config) =
  if option_has tls.client_auth.ca_cert_dir (fun value -> String.length value = 0) then
    Some "server tls client_auth ca_cert_dir must not be empty"
  else
    None

let controller_sample_interval_error (controller : controller_config) =
  if controller.sample_interval_ms <= 0 then
    Some "controller sample_interval_ms must be positive"
  else
    None

let controller_busy_threshold_error (controller : controller_config) =
  if controller.busy_threshold < 0.0 || controller.busy_threshold > 1.0 then
    Some "controller busy_threshold must be between 0.0 and 1.0"
  else
    None

let controller_saturated_threshold_error (controller : controller_config) =
  if
    controller.saturated_threshold < 0.0
    || controller.saturated_threshold > 1.0
  then
    Some "controller saturated_threshold must be between 0.0 and 1.0"
  else
    None

let controller_recovery_threshold_error (controller : controller_config) =
  if controller.recovery_threshold < 0.0 || controller.recovery_threshold > 1.0 then
    Some "controller recovery_threshold must be between 0.0 and 1.0"
  else
    None

let controller_busy_vs_saturated_threshold_error (controller : controller_config) =
  if controller.busy_threshold > controller.saturated_threshold then
    Some "controller busy_threshold must be less than or equal to saturated_threshold"
  else
    None

let controller_recovery_vs_busy_threshold_error (controller : controller_config) =
  if controller.recovery_threshold > controller.busy_threshold then
    Some "controller recovery_threshold must be less than or equal to busy_threshold"
  else
    None

let controller_busy_samples_error (controller : controller_config) =
  if controller.busy_samples <= 0 then
    Some "controller busy_samples must be positive"
  else
    None

let controller_saturated_samples_error (controller : controller_config) =
  if controller.saturated_samples <= 0 then
    Some "controller saturated_samples must be positive"
  else
    None

let controller_recovery_samples_error (controller : controller_config) =
  if controller.recovery_samples <= 0 then
    Some "controller recovery_samples must be positive"
  else
    None

let validate_tls_config (tls : tls_config) =
  let*? () = tls_cert_chain_path_error tls in
  let*? () = tls_private_key_path_error tls in
  let*? () = tls_security_policy_conflict_error tls in
  let*? () = tls_security_policy_error tls in
  let*? () = tls_alpn_empty_protocol_error tls in
  let*? () = tls_alpn_protocol_format_error tls in
  let*? () = tls_ocsp_staple_path_error tls in
  let*? () = tls_max_early_data_non_negative_error tls in
  let*? () = tls_max_early_data_range_error tls in
  let*? () = tls_session_ticket_keys_path_error tls in
  let*? () = tls_session_ticket_encrypt_decrypt_lifetime_error tls in
  let*? () = tls_session_ticket_decrypt_lifetime_error tls in
  let*? () = tls_session_ticket_initial_count_error tls in
  let*? () = tls_server_keying_material_lifetime_error tls in
  let*? () = tls_resumption_requires_session_tickets_error tls in
  let*? () = tls_early_data_requires_session_tickets_error tls in
  let*? () = tls_client_auth_ca_cert_path_error tls in
  let*? () = tls_client_auth_ca_cert_dir_error tls in
  None

let validate_controller_config (controller : controller_config) =
  let*? () = controller_sample_interval_error controller in
  let*? () = controller_busy_threshold_error controller in
  let*? () = controller_saturated_threshold_error controller in
  let*? () = controller_recovery_threshold_error controller in
  let*? () = controller_busy_vs_saturated_threshold_error controller in
  let*? () = controller_recovery_vs_busy_threshold_error controller in
  let*? () = controller_busy_samples_error controller in
  let*? () = controller_saturated_samples_error controller in
  let*? () = controller_recovery_samples_error controller in
  None

let tls
    ~cert_chain_path
    ~private_key_path
    ?security_policy
    ?minimum_protocol_version
    ?(alpn_protocols = [])
    ?ocsp_staple_path
    ?(client_auth = tls_client_auth Client_auth_none)
    ?session_tickets_enabled
    ?session_ticket_keys_path
    ?session_ticket_encrypt_decrypt_lifetime_secs
    ?session_ticket_decrypt_lifetime_secs
    ?session_ticket_initial_count
    ?server_keying_material_lifetime_secs
    ?max_early_data_size
    () =
  {
    cert_chain_path;
    private_key_path;
    security_policy;
    minimum_protocol_version;
    alpn_protocols;
    ocsp_staple_path;
    client_auth;
    session_tickets_enabled;
    session_ticket_keys_path;
    session_ticket_encrypt_decrypt_lifetime_secs;
    session_ticket_decrypt_lifetime_secs;
    session_ticket_initial_count;
    server_keying_material_lifetime_secs;
    max_early_data_size;
  }

let create
    ?(host = default_host)
    ?(backlog = default_backlog)
    ?(max_request_size = default_max_request_size)
    ?(request_timeout_ms = default_request_timeout_ms)
    ?(idle_timeout_ms = default_idle_timeout_ms)
    ?tls
    ?(controller = default_controller)
    ~port
    router =
  {
    host;
    port;
    backlog;
    max_request_size;
    request_timeout_ms;
    idle_timeout_ms;
    tls;
    controller;
    router;
  }

let validate (config : t) =
  let*? () = backlog_error config in
  let*? () = max_request_size_error config in
  let*? () = request_timeout_error config in
  let*? () = idle_timeout_error config in
  let*? () =
    match config.tls with
    | None -> None
    | Some tls -> validate_tls_config tls
  in
  let*? () = validate_controller_config config.controller in
  None
