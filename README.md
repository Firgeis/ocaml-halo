# Halo

Halo is an HTTP/1.1 server for OCaml 5 with Eio-based request handlers. It
supports incremental request bodies, streaming responses, server-sent events,
TLS, and multiple listeners.

## Quick start

```ocaml
let router =
  let open Halo.Router in
  let open Matcher in
  route
    (let+ () = request_method_is GET
     and+ () = path_end in
     fun _request -> text 200 "hello world\n")

let () =
  Eio_main.run @@ fun _env ->
  let config = Halo.Server.Config.create ~port:8080 router in
  Halo.Server.run [ config ]
```

Add `ocaml-halo` and `eio_main` to the executable's Dune libraries:

```lisp
(executable
 (name main)
 (libraries ocaml-halo eio_main))
```

`Server.run` installs a SIGINT handler by default and shuts down cleanly when
Ctrl-C is received. Pass several configurations to `Server.run` to serve
several listeners.

## Routes and requests

Routes use applicative matchers and compose with `<|>`:

```ocaml
let users =
  let open Halo.Router in
  let open Matcher in
  route
    (let+ () = request_method_is GET
     and+ () = path_segment "users"
     and+ id = int_parameter
     and+ verbose = queryparam "verbose"
     and+ () = path_end in
     fun request ->
       let agent = Request.header request "user-agent" in
       json 200
         (Printf.sprintf {|{"id":%d,"verbose":%S,"agent":%S}|}
            id verbose agent))
```

`Router.Request` exposes request metadata:

- `request_method`, `request_method_text`, `raw_target`, and `target`
- case-insensitive `header`, `headers`, and `fold_headers`
- `id`, a unique request ID for the runtime
- `switch`, the request-scoped Eio switch
- `body`, an incremental `Halo.Body.t`
- `send_informational` and `on_disconnect`

Use `Request.switch request` when starting child fibers. They are cancelled with
the request if the client disconnects.

Middleware may wrap matched handlers with `Router.use`. Pre-middleware added
with `Router.use_pre` can continue, respond immediately, or rewrite the request
before matching.

## Request bodies

`Body.read` returns the next available chunk, or `None` after the complete body
has been read. `Body.read_all` drains the body and returns the complete value,
including chunks already returned by `read`.

```ocaml
route
  Matcher.(
    let+ () = request_method_is POST
    and+ () = path_segment "echo"
    and+ () = path_end
    and+ () = require_body in
    fun request ->
      text 200 (Halo.Body.read_all (Request.body request)))
```

If a handler responds before draining the body, Halo sends the response and
then closes the connection.

## Streaming and server-sent events

Streaming responses are values, just like fixed responses:

```ocaml
Halo.Stream.respond ~status:200
  ~headers:[ "Content-Type", "text/plain; charset=utf-8" ]
  (fun ~yield ->
    yield "hello ";
    Eio.Time.sleep clock 0.1;
    yield "world\n")
```

Each `yield` waits until its chunk is written, applying response backpressure.
`Halo.Sse.respond` creates a server-sent event stream with the same interface.
Streaming requires HTTP/1.1; Halo returns 500 if a handler attempts it on an
HTTP/1.0 request.

Handlers can emit 1xx responses explicitly:

```ocaml
Halo.Router.Request.send_informational request
  (Halo.Router.informational 103
     [ "Link", "</style.css>; rel=preload; as=style" ])
```

## Lifecycle and metrics

For application-controlled shutdown, pass a shared control:

```ocaml
let shutdown = Halo.Server.Shutdown.create ()

(* From another fiber or signal handler: *)
Halo.Server.Shutdown.request shutdown;

Halo.Server.run ~shutdown ~handle_sigint:false [ config ]
```

The optional `on_ready` callback receives a `Server.running` handle after all
listeners are bound. Use the handle with `Server.metrics` to get a consistent
metrics snapshot or with `Server.shutdown` to request shutdown.

```ocaml
Halo.Server.run
  ~on_ready:(fun running ->
    let metrics = Halo.Server.metrics running in
    Printf.printf "active requests: %Ld\n%!" metrics.active_requests)
  [ config ]
```

In practice, keep `on_ready` short and hand the handle to another fiber before
waiting for work. Handler exceptions are reported through `on_error` and become
a 500 response if no response has started. Disconnects raise
`Halo.Server.Client_disconnected` in the affected request fiber.

The admission bound defaults to 4096 in-flight requests. It can be changed with
`~max_in_flight_requests`; excess requests receive 503.

## TLS

Enable TLS by passing a TLS configuration to `Server.Config.create`:

```ocaml
let tls =
  Halo.Server.Config.tls
    ~cert_chain_path:"cert.pem"
    ~private_key_path:"key.pem"
    ~minimum_protocol_version:Halo.Server.TLS13
    ~session_tickets_enabled:true
    ()

let config = Halo.Server.Config.create ~port:8443 ~tls router
```

Configuration also supports ALPN, OCSP stapling, optional or required client
certificates, explicit session-ticket keys, ticket lifetimes, TLS 1.3 early
data, and server keying-material lifetime limits. See `Halo.Server.Config` for
the complete typed configuration surface.

## Development

From a configured local switch:

```sh
make build
make test
make run
```
