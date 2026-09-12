# Server-request telemetry observer

`Halo.Telemetry` is the local boundary between Halo's C/OCaml request runtime
and an observability implementation. It is disabled by default and has no
runtime dependency on an OpenTelemetry package.

An observer implements `Halo.Telemetry.Observer` and is installed once
for all listeners passed to `Halo.Server.run`:

```ocaml
module Otel_observer : Halo.Telemetry.Observer = struct
  type span = My_otel.span

  let start_server_request request =
    (* Extract traceparent/tracestate from request.headers, then start a SERVER
       span at request.start_time_unix_ns. *)
    My_otel.start_server_span request

  let with_span span run = My_otel.with_span span run

  let finish_server_request span result =
    My_otel.finish_server_span span result
end

let telemetry =
  Halo.Telemetry.of_observer (module Otel_observer)
let () = Halo.Server.run ~telemetry [ config ]
```

`with_span` must call its callback exactly once. Start, scope, and finish run on
the Eio request domain, so a network exporter should only enqueue bounded work
there. Batching, retry, and OTLP I/O belong in separate fibers or domains.
Ordinary observer exceptions are isolated from HTTP processing; Eio
cancellation is preserved.

This first boundary covers requests dispatched from the C reactor to an OCaml
handler or built-in method response. Rejections completed entirely in C before
dispatch (for example, a malformed request line or an admission-limit 503) do
not yet produce request spans. Their aggregate counters remain available from
`Halo.Server.metrics`.

## Request timing

The C reactor captures two clocks at the first plaintext HTTP byte:

- `start_time_unix_ns` is the absolute span start time.
- `start_monotonic_ns` is the duration clock.

The terminal `end_monotonic_ns` is captured after the final response write, or
when the connection is lost. This means the duration includes request parsing,
middleware, the handler, and response transmission. Derive the end wall time
without trusting a second wall-clock read:

```text
end_unix_ns = start_time_unix_ns
            + (end_monotonic_ns - start_monotonic_ns)
```

## OpenTelemetry mapping

Use the current HTTP server semantic conventions when implementing an observer:

| Halo value | OpenTelemetry field |
|---|---|
| `method_` | `http.request.method` |
| path parsed from `target` | `url.path` |
| TLS presence | `url.scheme` (`https` or `http`) |
| `http_version` | `network.protocol.version` |
| remote socket address | `client.address` and, when useful, `client.port` |
| local socket or Host header | `server.address` and `server.port` |
| `result.route` | `http.route` |
| `result.status_code` | `http.response.status_code` |
| monotonic difference | `http.server.request.duration`, in seconds |

Create a `SERVER` span. When a route is known, name it
`<method> <route-template>`; otherwise use the method only. A server response in
the 5xx range sets span status to error. A disconnect or aborted stream should
set a low-cardinality `error.type`; do not use exception messages as metric
attributes.

`Router.route ~template` is deliberately explicit. Use a stable value such as
`/users/{id}`. Halo never substitutes the raw URL path, which prevents unbounded
span names and metric cardinality. Unmatched requests have no route value.

Relevant specifications:

- [HTTP spans](https://opentelemetry.io/docs/specs/semconv/http/http-spans/)
- [HTTP metrics](https://opentelemetry.io/docs/specs/semconv/http/http-metrics/)
- [Trace API](https://opentelemetry.io/docs/specs/otel/trace/api/)
- [W3C Trace Context](https://www.w3.org/TR/trace-context/)
- [OTLP](https://opentelemetry.io/docs/specs/otlp/)
