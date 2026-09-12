# Telemetry

`Halo.Telemetry` connects Halo's C/OCaml request runtime to a local tracing SDK.
It is disabled by default and has no dependency on a third-party OpenTelemetry
package.

`Halo.Metrics` is the matching metrics collector. It records the standard HTTP
server duration, active-request, request-body-size, and response-body-size
instruments, plus low-cardinality request totals and reactor measurements under
`halo.*`.

## Metrics

Create one collector, pass it to the server, and collect a snapshot from the
live handle. `Server.collect_metrics` refreshes the reactor gauges and counters
before returning the immutable snapshot:

```ocaml
let metrics = Halo.Metrics.create ()

let on_ready running =
  Eio.Fiber.fork ~sw @@ fun () ->
  let snapshot = Halo.Server.collect_metrics running in
  let body = Halo.Metrics.Prometheus.encode snapshot in
  serve_metrics_response
    ~content_type:Halo.Metrics.Prometheus.content_type body

let () = Halo.Server.run ~metrics ~on_ready [ config ]
```

The Prometheus adapter emits text exposition format 0.0.4. Dotted OTEL names
become underscores, duration and byte units get conventional suffixes, and
monotonic sums get `_total`.

For OTLP/HTTP protobuf, create an exporter and send snapshots at the interval
chosen by the application:

```ocaml
let exporter =
  Halo.Metrics.Otlp_http.exporter ~net:env#net
    ~service_name:"my-service" ()
in
let snapshot = Halo.Server.collect_metrics running in
match Halo.Metrics.Otlp_http.export exporter snapshot with
| Ok () -> ()
| Error message -> log_export_error message
```

The default endpoint is `http://localhost:4318/v1/metrics`. The exporter uses
cumulative temporality and the same bounded retry behavior as the trace
exporter. Call `Metrics.Otlp_http.shutdown` when no further exports should be
accepted.

Request measurements are grouped by method, scheme, explicit route template,
status code, terminal outcome, and low-cardinality status class. A raw URL path
is never used as a metric attribute. Body-size histograms count body octets and
exclude headers and HTTP/1 framing.

Runtime snapshots expose these instrument families:

| Instrument | Kind |
|---|---|
| `halo.runtime.utilization` | gauge |
| `halo.runtime.connections.active` | gauge |
| `halo.runtime.requests.active` | gauge |
| `halo.runtime.connections.accepted` | cumulative sum |
| `halo.runtime.connections.rejected` | cumulative sum |
| `halo.runtime.requests.completed` | cumulative sum |
| `halo.runtime.tls.handshakes` | cumulative sum by result/session mode |
| `halo.runtime.queue.depth` | gauge by event/command queue |
| `halo.runtime.controller.state` | gauge labeled by current state |
| `halo.runtime.backpressure` | boolean gauge |

`Metrics.create` accepts custom duration and size histogram boundaries. The
defaults cover typical HTTP latencies and payload sizes.

## Local tracer

The usual production setup uses the bounded batch processor and OTLP/HTTP
exporter:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun telemetry_switch ->
  let exporter =
    Halo.Telemetry.Otlp_http.exporter ~net:env#net
      ~service_name:"my-service" ()
  in
  let processor =
    Halo.Telemetry.Batch_processor.create ~sw:telemetry_switch exporter
  in
  let tracer = Halo.Telemetry.Tracer.create ~processor () in
  let telemetry = Halo.Telemetry.of_tracer tracer in
  Fun.protect
    ~finally:(fun () ->
      ignore (Halo.Telemetry.Processor.shutdown processor))
    (fun () -> Halo.Server.run ~telemetry [ config ])
```

The exporter defaults to `http://localhost:4318/v1/traces` and sends standard
OTLP protobuf. The built-in network transport currently accepts unencrypted
`http` endpoints, which fits a collector or sidecar on the local network. Put a
TLS proxy in front of it when the collector requires HTTPS, or pass an existing
TLS-capable HTTP client through `Otlp_http.exporter_with_transport`.

Transient exports use bounded exponential backoff with jitter. By default an
export is attempted at most five times, starting at 500 ms and capping delays
at five seconds. The built-in transport retries connection failures and HTTP
429, 502, 503, and 504 responses, including integer `Retry-After` delays. Use
`Otlp_http.Retry.create` to tune the policy or
`Otlp_http.Retry.disabled` to make a single attempt. An application-supplied
transport's `Error` results are considered transient.

The batch processor holds at most 2048 finished spans, exports up to 512 at a
time, flushes every five seconds, and applies a 30-second export timeout. Its
request-facing path only performs a bounded atomic enqueue. New spans are
dropped when the queue is full; inspect `Processor.dropped_spans` and
`Processor.last_error` for operational health. Call `Processor.force_flush`
before a checkpoint and `Processor.shutdown` during application shutdown.

For synchronous or local collection, combine `Processor.simple` with
`In_memory_exporter` or another `Exporter.t`.

Application spans use the same tracer:

```ocaml
let span = Halo.Telemetry.Tracer.start_span tracer "load-profile" in
Fun.protect
  ~finally:(fun () -> Halo.Telemetry.Span.finish span)
  (fun () ->
    Halo.Telemetry.Context.with_span span @@ fun () ->
    load_profile ())
```

The active context is fiber-local and is inherited by Eio child fibers. Trace
and span identifiers remain in binary form internally. The default generator
uses OpenSSL's cryptographic RNG; `Id_generator.create` accepts deterministic
generators when an application needs one.

## Custom observer

An observer implements `Halo.Telemetry.Observer` and is installed once
for all listeners passed to `Halo.Server.run`:

```ocaml
module Otel_observer : Halo.Telemetry.Observer = struct
  type span = My_otel.span

  let start_server_request request =
    (* Start a SERVER span with request.remote_parent as its parent and
       request.start_time_unix_ns as its start time. *)
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

## W3C trace context

Halo extracts `traceparent` and `tracestate` before invoking the observer. A
valid upstream context is available as `request.remote_parent`. Header names
are matched case-insensitively, while trace and span identifiers must use the
lowercase hexadecimal form required by W3C Trace Context. An invalid or
duplicated `traceparent` is ignored. An invalid `tracestate` discards only the
vendor state, leaving a valid remote parent usable.

`Telemetry.Trace_context` exposes the trace ID, upstream span ID, trace flags,
sampled bit, and ordered tracestate entries. After the observer creates a
server span, it can construct that span's context and serialize downstream
headers:

```ocaml
let propagation_headers ~server_span_id request =
  let open Halo.Telemetry.Trace_context in
  match request.remote_parent with
  | None -> []
  | Some parent ->
      make ~trace_id:(trace_id parent) ~span_id:server_span_id
        ~trace_flags:(trace_flags parent) ~tracestate:(tracestate parent)
      |> Option.fold ~none:[] ~some:to_headers
```

`make` validates locally created identifiers and tracestate. `to_headers`
emits a version `00` `traceparent` plus `tracestate` when it is non-empty.
`Trace_state.add` updates vendor state at the front while preserving the W3C
entry and size limits.

The tracing and HTTP-metric lifecycle boundary covers requests dispatched from
the C reactor to an OCaml handler or built-in method response. Rejections
completed entirely in C before dispatch (for example, a malformed request line
or an admission-limit 503) do not produce request spans or HTTP histogram
points. Their aggregate runtime counters remain available from
`Halo.Server.metrics` and `Halo.Server.collect_metrics`.

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
| `remote_parent` | remote parent `SpanContext` |
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
