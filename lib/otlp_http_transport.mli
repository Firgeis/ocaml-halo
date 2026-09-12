(** Minimal Eio HTTP transport used by the OTLP exporter. *)

val post :
  net:_ Eio.Net.t ->
  endpoint:Uri.t ->
  headers:(string * string) list ->
  content_type:string ->
  body:string ->
  (unit, string) result
(** POST [body] to an unencrypted HTTP endpoint. A 2xx response succeeds;
    invalid endpoints raise [Invalid_argument]. *)
