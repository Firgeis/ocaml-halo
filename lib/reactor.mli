exception Client_disconnected
exception Runtime_error of string

type event_kind =
  | Request
  | Body_chunk
  | Body_end
  | Response_complete
  | Disconnected
  | Stopped
  | Write_complete
  | Metrics
  | Unknown of int

type event = {
  kind : event_kind;
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
  payload : string;
}

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
val send_informational : Server_backend.t -> int64 -> string -> unit
val start_response_stream : Server_backend.t -> int64 -> string -> unit
val write_response_chunk : Server_backend.t -> int64 -> string -> unit
val finish_response_stream : Server_backend.t -> int64 -> unit
val request_metrics : Server_backend.t -> int64 -> unit
val metrics_of_payload : string -> Server_backend.runtime_metrics
val run : Server_backend.t -> (unit, string) result
val listener_id : Server_backend.server -> int64
val trace : ('a, unit, string, unit) format4 -> 'a
