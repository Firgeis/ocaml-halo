# Halo

Halo is an HTTP/1.1 server for OCaml 5 with Eio-based request handlers. It
supports incremental request bodies, streaming responses, server-sent events,
TLS, and multiple listeners.

## Quick start

```ocaml
let router =
  let open Halo.Router in
  let open Matcher in
  route ~template:"/"
    (let+ () = request_method_is GET *> path_end in
     fun _request -> text Status.ok "hello world\n")

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

`Server.run` installs a SIGINT handler by default. On shutdown, Halo stops
accepting connections, closes idle connections, and lets active requests and
responses finish for up to 30 seconds before force-closing them. Configure the
deadline with `~shutdown_timeout_ms`; zero requests an immediate stop. Pass
several configurations to `Server.run` to serve several listeners.

Listeners default to IPv4 loopback (`127.0.0.1`). `~host` accepts unbracketed
IPv4 or IPv6 literals; hostnames and IPv6 scope IDs are unsupported. IPv6
listeners default to IPv6-only. Select dual-stack explicitly to accept both
families on all interfaces:

```ocaml
let config =
  Halo.Server.Config.create ~host:"::" ~port:8080
    ~ipv6_mode:Halo.Server.Dual_stack router
```

Use `~ipv6_mode:Halo.Server.IPv6_only` to require IPv6-only, or `~host:"::1"`
for IPv6 loopback. An explicit IPv6 mode requires an IPv6 host. Socket endpoints
on a dual-stack listener retain IPv4-mapped IPv6 addresses in request metadata.

`~reuse_port:true` enables OS distribution of connections between listeners
sharing an address and port. Each listener sharing that port must enable it;
the option does not start worker processes. Libuv supports it on Linux 3.9+,
FreeBSD 12+, DragonFlyBSD 3.6+, Solaris 11.4, and AIX 7.2.5+. Unsupported
reuseport or IPv6 modes fail at startup with the endpoint and failed operation.
Ports must be between 0 and 65535; zero lets the OS choose a port.

Existing `Config.create` calls remain valid. Direct construction of the
`Config.t` record now requires `ipv6_mode` and `reuse_port` fields.

Request limits are configured independently on `Server.Config.create`:
`max_request_line_size` caps the combined method and request-target bytes,
`max_header_size` caps the combined bytes in all header names and values, and
`max_body_size` caps decoded body bytes (including chunked requests). The
defaults are 8 KiB, 32 KiB, and 1 MiB, respectively. Requests exceeding them
receive 414, 431, and 413.

Trusted proxy forwarding is opt-in. See the [forwarding guide](docs/forwarded.md)
for policy configuration, trust boundaries, and effective request metadata.

## Routes and requests

Routes use applicative matchers and compose with `<|>`:

```ocaml
let users =
  let open Halo.Router in
  let open Matcher in
  route ~template:"/users/{id}"
    (let+ id =
       request_method_is GET *> path_segment "users" *> int_parameter
       <* path_end
     and+ verbose = queryparam "verbose" in
     fun request ->
       let agent = Request.header request "user-agent" in
       json Status.ok
         (Printf.sprintf {|{"id":%d,"verbose":%S,"agent":%S}|}
            id verbose agent))
```

Use `*>` and `<*` to sequence matchers while keeping the value on the side
indicated by the angle bracket. Reserve `let+` and `and+` bindings for values
used by the handler.

An explicit `HEAD` route takes precedence. Otherwise, a `HEAD` request can
match a `GET` route. Handlers and middleware still see `HEAD`, and the HTTP
transport omits the response body. `Allow` includes `HEAD` wherever `GET` is
available.

Use `Matcher.host "api.example.com"` in a route or scope to match a host
case-insensitively, alongside method and path matchers. Request ports are
ignored unless the expected host includes one. Use brackets for IPv6 literals,
for example `Matcher.host "[::1]:8080"`.

`Router.Request` exposes request metadata:

- `request_method`, `request_method_text`, `raw_target`, and `target`
- `http_version`, `remote_address`, and `local_address`
- `is_tls` and `tls`, including SNI, ALPN, protocol version, and cipher
- case-insensitive `header`, `headers`, and `fold_headers`
- `host` for a parsed hostname and optional port; `header request "Host"` for
  the original value
- `id`, a unique request ID for the runtime
- `switch`, the request-scoped Eio switch
- `body`, an incremental `Halo.Body.t`
- `send_informational` and `on_disconnect`

Use `Request.switch request` when starting child fibers. They are cancelled with
the request if the client disconnects.

`Request.connection_info` returns both socket endpoints as `Unix.sockaddr`
values and an optional TLS record. A missing SNI or ALPN value is represented by
`None`; the whole TLS record is `None` on a plaintext connection. These
connection endpoints and the HTTP version are available on requests dispatched
by a running server, not synthetic requests passed directly to
`Router.dispatch`. Synthetic requests report `is_tls = false` and `tls = None`.

Middleware may wrap matched handlers with `Router.use`. Pre-middleware added
with `Router.use_pre` can continue, respond immediately, or rewrite the request
before matching.

Middleware that needs the final transport outcome can register a lifecycle
callback before calling the next handler:

```ocaml
let observe next request =
  Halo.Router.Request.on_response_terminated request (function
    | Halo.Router.Completed -> log "response completed"
    | Halo.Router.Aborted (exn, backtrace) -> log_stream_error exn backtrace
    | Halo.Router.Client_disconnected -> log "client disconnected");
  next request
```

The callback runs exactly once for buffered and streamed responses, including
after the handler has returned.

## Response compression

Enable buffered gzip compression on a router or scope:

```ocaml
let router =
  Halo.Router.use
    (Halo.Compression.middleware ~min_body_size:1024 ~level:6 ())
    router
```

Compression negotiates all `Accept-Encoding` fields, honors quality values and
exclusions, and merges `Vary: Accept-Encoding`. Explicit `identity` preferences
participate in negotiation; implicit identity is a fallback. A missing or empty
header uses identity. If no available representation is acceptable, the
middleware returns an empty 406 response.

The defaults compress text, JavaScript, JSON, XML and SVG bodies of at least
1 KiB. Streams, server-sent events, partial responses, existing content encodings,
and `Cache-Control: no-transform` are excluded. Compression keeps the original
when gzip would increase its size, unless the client excludes identity. Strong
ETags become weak and original digest headers are removed. HEAD selects the
same buffered representation as GET, while the transport omits its body.
Use `~compressible:(fun media_type -> ...)` to customize the media-type rules.

For an explicit asset path, use the precompressed file handler:

```ocaml
let css_handler cwd =
  Halo.Compression.file
    ~headers:(Halo.Router.Headers.of_list_exn
      [ "Cache-Control", "public, max-age=3600" ])
    ~content_type:"text/css; charset=utf-8"
    Eio.Path.(cwd / "public/app.css")
```

The handler serves GET/HEAD and chooses `app.css`, `app.css.gz` or `app.css.br`
according to client preferences. Ties prefer Brotli, then gzip. Generate
sidecars during the asset build; missing sidecars or ones older than the original
are ignored. Each variant preserves the original MIME type and gets its own
strong ETag. `If-None-Match` is checked against the selected variant.

Assets and dynamic responses are buffered in memory. The file helper sends full
representations for Range requests and does not map request URLs to paths; pass
an application-selected path from a confined Eio directory. Runtime compression
uses system zlib. Serving `.br` sidecars requires no Brotli library; runtime
Brotli and streaming compression are not provided.

## Telemetry

`Halo.Telemetry` provides no-op-by-default OpenTelemetry integration. Use its
local tracer or pass a custom server-request observer to `Server.run`:

```ocaml
let telemetry = Halo.Telemetry.of_tracer tracer

let () = Halo.Server.run ~telemetry [ config ]
```

The observer receives the parsed W3C `request.remote_parent`, runs the handler
and response write inside its span scope, and receives the final route, status,
transport outcome, and C-reactor timestamps. Associate stable route templates
with routes using
`Router.route ~template:"/users/{id}"`; never use a concrete request path as a
template.

See [docs/telemetry.md](docs/telemetry.md) for local tracing, batching,
OTLP/HTTP export, the observer contract, and semantic-convention mapping.

Request and runtime metrics use the companion collector:

```ocaml
let metrics = Halo.Metrics.create ()

Halo.Server.run ~metrics
  ~on_ready:(fun running ->
    let snapshot = Halo.Server.collect_metrics running in
    let prometheus = Halo.Metrics.Prometheus.encode snapshot in
    publish prometheus)
  [ config ]
```

It includes OTEL HTTP latency and body-size histograms, status-class request
totals, active requests, and `halo.*` reactor gauges/counters. Prometheus text
and OTLP/HTTP protobuf adapters consume the same snapshot; the telemetry guide
lists the instruments and exporter setup.

## Request bodies

`Body.read` consumes and returns the next available chunk, or `None` after the
complete body has been read. Consumed chunks are released rather than retained.
`Body.read_all` drains and returns only the unread remainder, so incremental
consumers use memory proportional to the current chunk instead of the complete
request body.

```ocaml
route
  Matcher.(
    let+ () =
      request_method_is POST
      *> path_segment "echo"
      *> path_end
      *> require_body
    in
    fun request ->
      text Status.ok (Halo.Body.read_all (Request.body request)))
```

If a handler responds before draining the body, Halo sends the response and
then closes the connection.

## Streaming and server-sent events

Streaming responses are values, just like fixed responses. Header construction
returns a result so applications can choose how to handle invalid input:

```ocaml
let streamed_response clock =
  Halo.Router.Headers.of_list
    [ "Content-Type", "text/plain; charset=utf-8" ]
  |> Result.map (fun headers ->
       Halo.Stream.respond ~status:Halo.Router.Status.ok ~headers
         (fun ~yield ->
           yield "hello ";
           Eio.Time.sleep clock 0.1;
           yield "world\n"))
```

Each `yield` waits until its chunk is written, applying response backpressure.
`Halo.Sse.respond` creates a server-sent event stream with the same interface.
Streaming requires HTTP/1.1; Halo returns 500 if a handler attempts it on an
HTTP/1.0 request.

For producers that emit many small chunks, opt into bounded batching:

```ocaml
Halo.Stream.respond_buffered ~status:Halo.Router.Status.ok
  ~max_buffered_bytes:65536 (fun ~yield ~flush ->
    List.iter yield chunks;
    flush ())
```

Buffered yields combine into payload batches of at most 64 KiB by default.
Each full batch waits for write completion, preserving backpressure for slow
readers. Larger yields are split. Successful producer return flushes remaining
bytes; an exception discards pending bytes and aborts the stream. Buffer and
transport copies use memory proportional to the limit, in addition to the
application's strings. There is no background writer or flush timer.

`Halo.Sse.respond_buffered` supplies `~send` and `~flush` callbacks. Flush after
an event or group of events that needs prompt delivery, before waiting for more
events. Existing `Stream.respond` and `Sse.respond` calls keep their immediate
write behavior.

Starting a streamed body enables TCP_NODELAY on its connection to avoid
delayed-ACK pauses between data and the final chunk terminator. It remains
enabled if the connection is reused.

Handlers can emit 1xx responses explicitly:

```ocaml
let send_early_hints request =
  Halo.Router.Headers.of_list
    [ "Link", "</style.css>; rel=preload; as=style" ]
  |> Result.map (fun headers ->
       Halo.Router.Request.send_informational request
         (Halo.Router.informational Halo.Router.Status.early_hints headers))
```

Response statuses, headers, and responses are validated types. Use a named
`Router.Status` value when possible. For dynamically selected codes,
`Router.Status.of_int` returns a result. Build dynamic header collections with
`Router.Headers.of_list` and handle its result explicitly. Invalid field names
and values containing prohibited control characters are rejected before a
response can be serialized.

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
waiting for work. Handler exceptions and their raw backtraces are reported
through `on_error` and become a 500 response if no response has started. The
default callback prints both to standard error. Custom reporters receive
`request_id`, `exn`, and `Printexc.raw_backtrace`; enable backtrace recording
with `Printexc.record_backtrace true` or `OCAMLRUNPARAM=b` to ensure the
backtrace is populated. Disconnects raise
`Halo.Server.Client_disconnected` in the affected request fiber.

The admission bound defaults to 4096 in-flight requests. It can be changed with
`~max_in_flight_requests`; excess requests receive 503. The request lookup
table allocates one pointer-sized bucket per configured slot at startup (32 KiB
for the default 4096 slots on a 64-bit system), plus an entry per active request.

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

Early-data requests are rejected with `425 Too Early` by default. Applications
that enable `~max_early_data_size` must explicitly allow replay-safe requests
through `Server.Config.create ~early_data_policy`. Accepted handlers can use
`Router.Request.is_early_data` to distinguish early data from ordinary traffic.
The in-process replay cache is shared by all listeners and runtimes, uses a
bounded hash table, and rejects new early data when its 65,536-entry or 64 MiB
identity budget is exhausted. Separate processes still require coordinated
replay protection at the deployment layer. Install a shared adapter with
`~early_data_replay_store` for those deployments:

```ocaml
module Replay_store = Halo.Server.Config.Early_data_replay_store

let early_data_tls =
  Halo.Server.Config.tls
    ~cert_chain_path:"cert.pem"
    ~private_key_path:"key.pem"
    ~minimum_protocol_version:Halo.Server.TLS13
    ~session_tickets_enabled:true
    ~max_early_data_size:4096
    ()

let replay_store =
  Replay_store.create_blocking ~timeout_seconds:0.1 ~max_in_flight:64
    (fun ~identity ~ttl_secs ->
      (* Atomically claim the binary identity for this TTL. For Redis this is
         equivalent to SET identity 1 NX EX ttl_secs. *)
      match claim_in_shared_store ~identity ~ttl_secs with
      | `Claimed -> Replay_store.Fresh
      | `Exists -> Replay_store.Already_seen
      | `Unavailable -> Replay_store.Unavailable)

let config =
  Halo.Server.Config.create ~port:8443 ~tls:early_data_tls
    ~early_data_replay_store:replay_store router
```

The identity is an opaque 32-byte SHA-256 digest of the TLS PSK identity. The
claim must be atomic across every process or instance sharing ticket keys.
`Already_seen`, `Unavailable`, exceptions, and timeouts reject early data but
allow the TLS handshake to continue. Without a custom adapter, Halo uses the
bounded process-wide cache described above, exposed as
`Early_data_replay_store.process_local`, which is also the reference
implementation of the contract. Use `Early_data_replay_store.create` for a
cooperative Eio client, or `create_blocking` for a blocking client. Blocking
callbacks run in system threads and are bounded by `max_in_flight`; they may
finish after Halo has rejected early data on timeout, so the store client must
also have a finite I/O timeout.

## Development

From a configured local switch:

```sh
make build
make test
make run
```

Compression tests require the `gzip` command for independent decompression and
checksum verification.
