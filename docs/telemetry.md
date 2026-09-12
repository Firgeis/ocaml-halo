# Telemetry

`Halo.Telemetry` connects Halo's C/OCaml request runtime to a local tracing SDK.
It is disabled by default and has no dependency on a third-party OpenTelemetry
package.

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
