type _ Effect.t +=
  Await_request_body : string option Effect.t
type _ Effect.t +=
  Sleep_ms : int -> unit Effect.t
type _ Effect.t +=
  Start_response_stream : Router.response_head -> unit Effect.t
type _ Effect.t +=
  Write_response_chunk : string -> unit Effect.t
type _ Effect.t +=
  Finish_response_stream : 'a Effect.t
type _ Effect.t +=
  Send_informational : Router.informational_response -> unit Effect.t
type _ Effect.t +=
  On_client_disconnect : (unit -> unit) -> unit Effect.t
type _ Effect.t +=
  Check_client_connected : unit Effect.t

exception Request_cancelled
exception Client_disconnected

val fail : Error.t -> 'a
val finalize_runtime : (unit -> unit) -> unit
val await_request_body : unit -> string option
val sleep_ms : int -> unit
val start_response_stream : Router.response_head -> unit
val write_response_chunk : string -> unit
val finish_response_stream : unit -> 'a
val send_informational : Router.informational_response -> unit
val on_client_disconnect : (unit -> unit) -> unit
val check_client_connected : unit -> unit
val or_fail_runtime : ('a, string) result -> 'a
val or_fail_runtime_status : string option -> unit
val run : (unit -> ('a, Error.t) result) -> ('a, Error.t) result
