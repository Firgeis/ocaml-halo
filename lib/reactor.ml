exception Client_disconnected
exception Runtime_error of string

let trace_enabled = Sys.getenv_opt "HALO_BRIDGE_TRACE" = Some "1"

let trace fmt =
  Printf.ksprintf
    (fun message ->
      if trace_enabled then prerr_endline ("halo bridge: " ^ message))
    fmt

type raw_event = {
  kind : int;
  request_id : int64;
  listener_id : int64;
  method_text : string;
  target : string;
  headers : (string * string) list;
  raw_headers : (string * string) list;
  http_major : int;
  http_minor : int;
  keep_alive : bool;
  body_expected : bool;
  is_early_data : bool;
  replay_ttl_secs : int64;
  payload : string;
}

type request_event = {
  request_id : int64;
  listener_id : int64;
  method_text : string;
  target : string;
  headers : (string * string) list;
  raw_headers : (string * string) list;
  http_major : int;
  http_minor : int;
  keep_alive : bool;
  body_expected : bool;
  is_early_data : bool;
}

type body_chunk_event = { request_id : int64; chunk : string }
type metrics_event = { query_id : int64; payload : string }

type replay_claim_event = {
  claim_id : int64;
  listener_id : int64;
  identity : string;
  ttl_secs : int64;
}

type unknown_event = {
  kind : int;
  request_id : int64;
  payload : string;
}

type event =
  | Request of request_event
  | Body_chunk of body_chunk_event
  | Body_end of int64
  | Response_complete of int64
  | Disconnected of int64
  | Stopped
  | Write_complete of int64
  | Metrics of metrics_event
  | Replay_claim of replay_claim_event
  | Drained
  | Unknown of unknown_event

type command_status = Enqueued | Full | Closed | Unknown_status of int

external create_runtime : unit -> (Server_backend.t, string) result
  = "ocaml_halo_bridge_runtime_create"

external close_runtime : Server_backend.t -> string option
  = "ocaml_halo_runtime_close"

external enable_raw : Server_backend.t -> int -> int -> (unit, string) result
  = "ocaml_halo_bridge_enable"

external event_fd : Server_backend.t -> Unix.file_descr
  = "ocaml_halo_bridge_event_fd"

external drain_wakeup : Server_backend.t -> unit = "ocaml_halo_bridge_drain_wakeup"

external poll_event_raw : Server_backend.t -> raw_event option
  = "ocaml_halo_bridge_poll_event"

external send_response_raw : Server_backend.t -> int64 -> string -> int
  = "ocaml_halo_bridge_send_response"

external request_body_raw : Server_backend.t -> int64 -> int
  = "ocaml_halo_bridge_read_body"

external close_request_raw : Server_backend.t -> int64 -> int
  = "ocaml_halo_bridge_close_request"

external stop_raw : Server_backend.t -> int = "ocaml_halo_bridge_stop"

external begin_drain_raw : Server_backend.t -> int
  = "ocaml_halo_bridge_begin_drain"

external send_informational_raw : Server_backend.t -> int64 -> string -> int
  = "ocaml_halo_bridge_send_informational"

external start_response_stream_raw : Server_backend.t -> int64 -> string -> int
  = "ocaml_halo_bridge_start_response_stream"

external write_response_chunk_raw : Server_backend.t -> int64 -> string -> int
  = "ocaml_halo_bridge_write_response_chunk"

external finish_response_stream_raw : Server_backend.t -> int64 -> int
  = "ocaml_halo_bridge_finish_response_stream"

external abort_response_stream_raw : Server_backend.t -> int64 -> int
  = "ocaml_halo_bridge_abort_response_stream"

external request_metrics_raw : Server_backend.t -> int64 -> int
  = "ocaml_halo_bridge_read_metrics"

external resolve_replay_claim_raw : Server_backend.t -> int64 -> bool -> int
  = "ocaml_halo_bridge_resolve_replay_claim"

external run : Server_backend.t -> (unit, string) result
  = "ocaml_halo_bridge_start"

external listener_id : Server_backend.server -> int64
  = "ocaml_halo_server_bridge_id"

let enable runtime ~max_in_flight_requests ~command_capacity =
  enable_raw runtime max_in_flight_requests command_capacity

let event_of_raw (raw : raw_event) =
  match raw.kind with
  | 0 ->
      Request
        {
          request_id = raw.request_id;
          listener_id = raw.listener_id;
          method_text = raw.method_text;
          target = raw.target;
          headers = raw.headers;
          raw_headers = raw.raw_headers;
          http_major = raw.http_major;
          http_minor = raw.http_minor;
          keep_alive = raw.keep_alive;
          body_expected = raw.body_expected;
          is_early_data = raw.is_early_data;
        }
  | 1 -> Body_chunk { request_id = raw.request_id; chunk = raw.payload }
  | 2 -> Body_end raw.request_id
  | 3 -> Response_complete raw.request_id
  | 4 -> Disconnected raw.request_id
  | 5 -> Stopped
  | 6 -> Write_complete raw.request_id
  | 7 -> Metrics { query_id = raw.request_id; payload = raw.payload }
  | 8 ->
      Replay_claim
        {
          claim_id = raw.request_id;
          listener_id = raw.listener_id;
          identity = raw.payload;
          ttl_secs = raw.replay_ttl_secs;
        }
  | 9 -> Drained
  | kind -> Unknown { kind; request_id = raw.request_id; payload = raw.payload }

let poll_event runtime = Option.map event_of_raw (poll_event_raw runtime)

let command_status = function
  | 0 -> Enqueued
  | 1 -> Full
  | 2 -> Closed
  | status -> Unknown_status status

let send_response_status runtime request_id response =
  command_status (send_response_raw runtime request_id response)

let request_body_status runtime request_id =
  command_status (request_body_raw runtime request_id)

let close_request_status runtime request_id =
  command_status (close_request_raw runtime request_id)

let stop_status runtime = command_status (stop_raw runtime)
let begin_drain_status runtime = command_status (begin_drain_raw runtime)

let send_informational_status runtime request_id response =
  command_status (send_informational_raw runtime request_id response)

let start_response_stream_status runtime request_id response =
  command_status (start_response_stream_raw runtime request_id response)

let write_response_chunk_status runtime request_id response =
  command_status (write_response_chunk_raw runtime request_id response)

let finish_response_stream_status runtime request_id =
  command_status (finish_response_stream_raw runtime request_id)

let abort_response_stream_status runtime request_id =
  command_status (abort_response_stream_raw runtime request_id)

let request_metrics_status runtime query_id =
  command_status (request_metrics_raw runtime query_id)

let resolve_replay_claim_status runtime claim_id accept =
  command_status (resolve_replay_claim_raw runtime claim_id accept)

let rec enqueue command =
  match command () with
  | Enqueued -> ()
  | Full ->
      Eio.Fiber.yield ();
      enqueue command
  | Closed -> raise Client_disconnected
  | Unknown_status status ->
      raise
        (Runtime_error
           (Printf.sprintf "unknown bridge command status %d" status))

let send_response runtime request_id response =
  enqueue (fun () -> send_response_status runtime request_id response)

let request_body runtime request_id =
  enqueue (fun () -> request_body_status runtime request_id)

let close_request runtime request_id =
  enqueue (fun () -> close_request_status runtime request_id)

let stop runtime = enqueue (fun () -> stop_status runtime)
let begin_drain runtime = enqueue (fun () -> begin_drain_status runtime)

let send_informational runtime request_id response =
  enqueue (fun () -> send_informational_status runtime request_id response)

let start_response_stream runtime request_id response =
  enqueue (fun () -> start_response_stream_status runtime request_id response)

let write_response_chunk runtime request_id response =
  enqueue (fun () -> write_response_chunk_status runtime request_id response)

let finish_response_stream runtime request_id =
  enqueue (fun () -> finish_response_stream_status runtime request_id)

let abort_response_stream runtime request_id =
  enqueue (fun () -> abort_response_stream_status runtime request_id)

let request_metrics runtime query_id =
  enqueue (fun () -> request_metrics_status runtime query_id)

let resolve_replay_claim runtime claim_id ~accept =
  enqueue (fun () -> resolve_replay_claim_status runtime claim_id accept)

let controller_state_of_int = function
  | 0 -> Server_backend.Normal
  | 1 -> Server_backend.Busy
  | 2 -> Server_backend.Saturated
  | 3 -> Server_backend.Recovering
  | state ->
      raise
        (Runtime_error
           (Printf.sprintf "unknown runtime controller state %d" state))

let metrics_of_payload payload =
  match String.split_on_char ' ' payload with
  | [
   utilization;
   active_connections;
   active_requests;
   accepted_connections;
   rejected_connections;
   completed_requests;
   tls_full_handshakes;
   tls_resumed_handshakes;
   tls_handshake_failures;
   controller_state;
   backpressure_enabled;
  ] ->
      {
        Server_backend.utilization = float_of_string utilization;
        active_connections = Int64.of_string active_connections;
        active_requests = Int64.of_string active_requests;
        accepted_connections = Int64.of_string accepted_connections;
        rejected_connections = Int64.of_string rejected_connections;
        completed_requests = Int64.of_string completed_requests;
        tls_full_handshakes = Int64.of_string tls_full_handshakes;
        tls_resumed_handshakes = Int64.of_string tls_resumed_handshakes;
        tls_handshake_failures = Int64.of_string tls_handshake_failures;
        controller_state = controller_state_of_int (int_of_string controller_state);
        backpressure_enabled = int_of_string backpressure_enabled <> 0;
      }
  | _ -> raise (Runtime_error "malformed runtime metrics event")
