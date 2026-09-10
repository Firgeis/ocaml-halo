exception Client_disconnected
exception Runtime_error of string

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
  (** Zero for a one-shot request; stable across a keep-alive connection. *)
  connection_id : int64;
  (** Present only on the first request of a cached connection, or on a
      one-shot request. *)
  connection_info : Router.connection_info option;
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
  | Connection_closed of int64
  | Unknown of unknown_event

val create_runtime : unit -> (Server_backend.t, string) result
val close_runtime : Server_backend.t -> string option

val enable :
  Server_backend.t ->
  max_in_flight_requests:int ->
  command_capacity:int ->
  (unit, string) result

val event_fd : Server_backend.t -> Unix.file_descr
val drain_wakeup : Server_backend.t -> unit
val poll_event : Server_backend.t -> event option
val send_response : Server_backend.t -> int64 -> string -> unit
val request_body : Server_backend.t -> int64 -> unit
val close_request : Server_backend.t -> int64 -> unit
val stop : Server_backend.t -> unit
val begin_drain : Server_backend.t -> unit
val send_informational : Server_backend.t -> int64 -> string -> unit
val start_response_stream : Server_backend.t -> int64 -> string -> unit
val write_response_chunk : Server_backend.t -> int64 -> string -> unit
val finish_response_stream : Server_backend.t -> int64 -> unit
val abort_response_stream : Server_backend.t -> int64 -> unit
val request_metrics : Server_backend.t -> int64 -> unit
val resolve_replay_claim : Server_backend.t -> int64 -> accept:bool -> unit
val metrics_of_payload : string -> Server_backend.runtime_metrics
val run : Server_backend.t -> (unit, string) result
val listener_id : Server_backend.server -> int64
val trace : ('a, unit, string, unit) format4 -> 'a
