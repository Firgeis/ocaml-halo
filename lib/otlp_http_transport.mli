(** Minimal Eio HTTP transport used by the OTLP exporter. *)

type error = { message : string; retryable : bool; retry_after : float option }

val post :
  net:_ Eio.Net.t ->
  endpoint:Uri.t ->
  headers:(string * string) list ->
  content_type:string ->
  body:string ->
  (unit, error) result
(** POST [body] to an unencrypted HTTP endpoint. A 2xx response succeeds;
    connection failures and transient OTLP status codes are marked retryable.
    Invalid endpoints raise [Invalid_argument]. *)
