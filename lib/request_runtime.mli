type listener
type t

val listener :
  early_data_policy:Server_config.early_data_policy ->
  early_data_replay_store:Server_config.Early_data_replay_store.t ->
  Router.t ->
  listener

val create :
  runtime:Server_backend.t ->
  listeners:(int64, listener) Hashtbl.t ->
  bridge_switch:Eio.Switch.t ->
  on_error:(int64 -> exn -> Printexc.raw_backtrace -> unit) ->
  telemetry:Telemetry.t ->
  t

val run : t -> unit
val await_idle : t -> unit
val begin_drain : t -> unit
val metrics : t -> Server_backend.runtime_metrics
