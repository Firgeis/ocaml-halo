type producer = yield:(string -> unit) -> unit

val respond :
  ?headers:Router.Headers.t ->
  status:Router.Status.t ->
  producer ->
  Router.response

type buffered_producer = yield:(string -> unit) -> flush:(unit -> unit) -> unit

val respond_buffered :
  ?headers:Router.Headers.t ->
  ?max_buffered_bytes:int ->
  status:Router.Status.t ->
  buffered_producer ->
  Router.response
(** Coalesce yields into batches of at most [max_buffered_bytes] payload bytes
    (default 64 KiB; must be positive). Full batches and explicit [flush] calls
    wait for transport write completion before producing more output. Large
    yields are split into bounded batches. No background writer is started.

    The remaining bytes are flushed on successful producer return and discarded
    on failure. There is no timer: call [flush] before waiting for another event
    when prompt delivery matters. An empty flush does nothing. Empty yields are
    invalid, as with [respond]. Wire chunk boundaries may change.

    Calls are serialized. Keep both callbacks within the producer's lifetime;
    join any child fibers before returning. A failed write makes later calls
    fail too. Buffer and transport copies use memory proportional to the limit,
    in addition to strings supplied by the application. *)
