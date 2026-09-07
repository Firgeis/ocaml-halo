let default_router = Halo.Router.empty

let default_config () =
  Halo.Server.Config.create ~port:8080 default_router

let check_valid label config =
  Alcotest.(check (option string)) label None (Halo.Server.Config.validate config)

let check_invalid label expected config =
  Alcotest.(check (option string))
    label
    (Some expected)
    (Halo.Server.Config.validate config)

let test_default_config_is_valid () =
  check_valid "default config is valid" (default_config ())

let test_default_request_limits () =
  let config = default_config () in
  Alcotest.(check int) "request line" (8 * 1024) config.max_request_line_size;
  Alcotest.(check int) "headers" (32 * 1024) config.max_header_size;
  Alcotest.(check int) "body" (1024 * 1024) config.max_body_size

let test_backlog_must_be_positive () =
  check_invalid "backlog validation" "server backlog must be positive"
    (Halo.Server.Config.create ~port:8080 ~backlog:0 default_router)

let test_max_request_line_size_must_be_positive () =
  check_invalid "max request line size validation"
    "server max_request_line_size must be positive"
    (Halo.Server.Config.create ~port:8080 ~max_request_line_size:0 default_router)

let test_max_header_size_must_be_positive () =
  check_invalid "max header size validation"
    "server max_header_size must be positive"
    (Halo.Server.Config.create ~port:8080 ~max_header_size:0 default_router)

let test_max_body_size_must_be_positive () =
  check_invalid "max body size validation"
    "server max_body_size must be positive"
    (Halo.Server.Config.create ~port:8080 ~max_body_size:0 default_router)

let test_request_timeout_must_be_non_negative () =
  check_invalid "request timeout validation"
    "server request_timeout_ms must be non-negative"
    (Halo.Server.Config.create ~port:8080 ~request_timeout_ms:(-1) default_router)

let test_idle_timeout_must_be_non_negative () =
  check_invalid "idle timeout validation"
    "server idle_timeout_ms must be non-negative"
    (Halo.Server.Config.create ~port:8080 ~idle_timeout_ms:(-1) default_router)

let replay_store ?timeout_seconds () =
  Halo.Server.Config.Early_data_replay_store.create ?timeout_seconds
    (fun ~identity:_ ~ttl_secs:_ ->
      Halo.Server.Config.Early_data_replay_store.Unavailable)

let test_replay_store_timeout_must_be_positive () =
  Alcotest.check_raises "replay store timeout validation"
    (Invalid_argument
       "early-data replay-store timeout must be finite and positive")
    (fun () -> ignore (replay_store ~timeout_seconds:0.0 ()))

let test_blocking_replay_store_max_in_flight_must_be_positive () =
  let module Store = Halo.Server.Config.Early_data_replay_store in
  Alcotest.check_raises "blocking replay store concurrency validation"
    (Invalid_argument
       "early-data blocking replay-store max_in_flight must be positive")
    (fun () ->
      ignore
        (Store.create_blocking ~max_in_flight:0 (fun ~identity:_ ~ttl_secs:_ ->
             Store.Unavailable)))

let test_replay_store_requires_early_data () =
  check_invalid "replay store requires early data"
    "server early_data_replay_store requires TLS max_early_data_size to be positive"
    (Halo.Server.Config.create ~port:8080
       ~early_data_replay_store:(replay_store ()) default_router)

let test_replay_store_with_early_data_is_valid () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~session_tickets_enabled:true ~max_early_data_size:1024 ()
  in
  check_valid "replay store with early data"
    (Halo.Server.Config.create ~port:8080 ~tls
       ~early_data_replay_store:(replay_store ()) default_router)

let test_controller_busy_threshold_range () =
  let controller =
    {
      Halo.Server.Config.default_controller with
      busy_threshold = 1.1;
    }
  in
  check_invalid "busy threshold range"
    "controller busy_threshold must be between 0.0 and 1.0"
    (Halo.Server.Config.create ~port:8080 ~controller default_router)

let test_controller_threshold_ordering () =
  let controller =
    {
      Halo.Server.Config.default_controller with
      busy_threshold = 0.95;
      saturated_threshold = 0.90;
    }
  in
  check_invalid "busy threshold ordering"
    "controller busy_threshold must be less than or equal to saturated_threshold"
    (Halo.Server.Config.create ~port:8080 ~controller default_router)

let test_tls_cert_chain_path_must_not_be_empty () =
  let tls =
    Halo.Server.Config.tls ~cert_chain_path:"" ~private_key_path:"key.pem" ()
  in
  check_invalid "tls cert chain path"
    "server tls cert_chain_path must not be empty"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_private_key_path_must_not_be_empty () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"" ()
  in
  check_invalid "tls private key path"
    "server tls private_key_path must not be empty"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_security_policy_conflicts_with_minimum_protocol_version () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~security_policy:"default_tls13"
      ~minimum_protocol_version:Halo.Server.TLS13 ()
  in
  check_invalid "tls security policy conflict"
    "server tls security_policy cannot be combined with minimum_protocol_version"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_alpn_protocols_must_not_contain_empty_values () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~alpn_protocols:["h2"; ""] ()
  in
  check_invalid "tls alpn empty value"
    "server tls alpn_protocols must not contain empty values"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_alpn_protocols_must_not_contain_nul () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~alpn_protocols:["h2\000http/1.1"] ()
  in
  check_invalid "tls alpn nul"
    "server tls alpn_protocols entries must be at most 255 bytes and cannot contain NUL"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_client_auth_ca_cert_path_must_not_be_empty () =
  let client_auth =
    Halo.Server.Config.tls_client_auth
      ~ca_cert_path:""
      Halo.Server.Client_auth_required
  in
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~client_auth ()
  in
  check_invalid "tls client auth ca cert path"
    "server tls client_auth ca_cert_path must not be empty"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_max_early_data_size_must_be_non_negative () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~max_early_data_size:(-1) ()
  in
  check_invalid "tls max early data"
    "server tls max_early_data_size must be non-negative"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_session_ticket_keys_path_must_not_be_empty () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~session_ticket_keys_path:"" ()
  in
  check_invalid "tls session ticket keys path"
    "server tls session_ticket_keys_path must not be empty"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_session_ticket_initial_count_must_fit_uint8 () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~session_ticket_initial_count:256 ()
  in
  check_invalid "tls session ticket initial count"
    "server tls session_ticket_initial_count must fit in uint8"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_server_keying_material_lifetime_must_fit_uint32 () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~server_keying_material_lifetime_secs:(0x1_0000_0000) ()
  in
  check_invalid "tls server keying material lifetime"
    "server tls server_keying_material_lifetime_secs must fit in uint32"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_resumption_settings_require_session_tickets () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~session_tickets_enabled:false
      ~session_ticket_keys_path:"tickets.txt" ()
  in
  check_invalid "tls resumption requires session tickets"
    "server tls resumption settings require session_tickets_enabled to be true"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let test_tls_early_data_requires_session_tickets () =
  let tls =
    Halo.Server.Config.tls
      ~cert_chain_path:"cert.pem" ~private_key_path:"key.pem"
      ~max_early_data_size:1024 ()
  in
  check_invalid "tls early data requires session tickets"
    "server tls max_early_data_size requires session_tickets_enabled to be true"
    (Halo.Server.Config.create ~port:8080 ~tls default_router)

let () =
  Alcotest.run "server_config"
    [
      ( "validate",
        [
          Alcotest.test_case "default config is valid" `Quick
            test_default_config_is_valid;
          Alcotest.test_case "default request limits" `Quick
            test_default_request_limits;
          Alcotest.test_case "backlog must be positive" `Quick
            test_backlog_must_be_positive;
          Alcotest.test_case "max request line size must be positive" `Quick
            test_max_request_line_size_must_be_positive;
          Alcotest.test_case "max header size must be positive" `Quick
            test_max_header_size_must_be_positive;
          Alcotest.test_case "max body size must be positive" `Quick
            test_max_body_size_must_be_positive;
          Alcotest.test_case "request timeout must be non-negative" `Quick
            test_request_timeout_must_be_non_negative;
          Alcotest.test_case "idle timeout must be non-negative" `Quick
            test_idle_timeout_must_be_non_negative;
          Alcotest.test_case "replay store timeout must be positive" `Quick
            test_replay_store_timeout_must_be_positive;
          Alcotest.test_case
            "blocking replay store max in flight must be positive" `Quick
            test_blocking_replay_store_max_in_flight_must_be_positive;
          Alcotest.test_case "replay store requires early data" `Quick
            test_replay_store_requires_early_data;
          Alcotest.test_case "replay store with early data is valid" `Quick
            test_replay_store_with_early_data_is_valid;
          Alcotest.test_case "controller busy threshold range" `Quick
            test_controller_busy_threshold_range;
          Alcotest.test_case "controller threshold ordering" `Quick
            test_controller_threshold_ordering;
          Alcotest.test_case "tls cert chain path must not be empty" `Quick
            test_tls_cert_chain_path_must_not_be_empty;
          Alcotest.test_case "tls private key path must not be empty" `Quick
            test_tls_private_key_path_must_not_be_empty;
          Alcotest.test_case
            "tls security policy conflicts with minimum protocol version"
            `Quick test_tls_security_policy_conflicts_with_minimum_protocol_version;
          Alcotest.test_case
            "tls alpn protocols must not contain empty values"
            `Quick test_tls_alpn_protocols_must_not_contain_empty_values;
          Alcotest.test_case "tls alpn protocols must not contain NUL" `Quick
            test_tls_alpn_protocols_must_not_contain_nul;
          Alcotest.test_case
            "tls client auth ca cert path must not be empty"
            `Quick test_tls_client_auth_ca_cert_path_must_not_be_empty;
          Alcotest.test_case
            "tls max early data size must be non-negative"
            `Quick test_tls_max_early_data_size_must_be_non_negative;
          Alcotest.test_case
            "tls session ticket keys path must not be empty"
            `Quick test_tls_session_ticket_keys_path_must_not_be_empty;
          Alcotest.test_case
            "tls session ticket initial count must fit uint8"
            `Quick test_tls_session_ticket_initial_count_must_fit_uint8;
          Alcotest.test_case
            "tls server keying material lifetime must fit uint32"
            `Quick test_tls_server_keying_material_lifetime_must_fit_uint32;
          Alcotest.test_case
            "tls resumption settings require session tickets"
            `Quick test_tls_resumption_settings_require_session_tickets;
          Alcotest.test_case
            "tls early data requires session tickets"
            `Quick test_tls_early_data_requires_session_tickets;
        ] );
    ]
