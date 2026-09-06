type producer = yield:(string -> unit) -> unit

val respond :
  ?headers:Router.Headers.t ->
  status:Router.Status.t ->
  producer ->
  Router.response
