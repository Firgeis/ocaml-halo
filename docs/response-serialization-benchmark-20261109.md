# Response serialization and streaming comparison

Baseline: commit `4ccda77`. Both versions were compiled from their respective
sources with the same Dune release profile and the same OCaml toolchain. The
servers and workloads used the probe sources from
`/tmp/halo-gc-pooling-20261109`. The server was pinned to CPU 0; the fixed-body
C client was pinned to CPUs 4–7. Streaming used a Python HTTP client with 32
connections, 20 requests per connection. Each fixed-body run used 32
connections and 1,000 warmup requests; the measured counts were 30,000 for
small responses, 15,000 for 1 KiB, and 2,000 for 64 KiB. The body cases POST
and echo the payload, so they include request-body processing. No Memprof
callbacks were enabled. Tables show medians of three separate runs, not a
statistical confidence interval.

## Component allocation

The component harness extracted the baseline and current `Http` serializer
source and ran it against an immutable response, with the buffer pool warmed.
The baseline returns an OCaml wire string; the current version passes its
header bytes and immutable body to a no-op consumer. The current measurement
does **not** include the subsequent C-owned copy or any bridge costs. The
prepared measurement covers only a prepared-wire lookup, after preparation.

| Body | Baseline B/call | Current B/call |
| --- | ---: | ---: |
| 12 B | 2,360 | 552 |
| 1 KiB | 4,392 | 552 |
| 64 KiB | 133,416 | 552 |
| Prepared 12 B wire lookup | — | 16 |

The prepared lookup has a different scope from serialization: its immutable
wire was constructed ahead of time. The component figures do not represent
whole-request savings.

## End-to-end allocation and collection

| Workload | Allocated B/request baseline → current | Promoted B/request baseline → current | Direct-major B/request baseline → current | Minor GCs baseline → current | Major GCs baseline → current |
| --- | ---: | ---: | ---: | ---: | ---: |
| Small fixed | 9,944 → 8,295 | 266 → 370 | ~0 → ~0 | 163 → 140 | 37 → 35 |
| Reused constant, prepared in current | 9,694 → 7,474 | 311 → 232 | ~0 → ~0 | 163 → 126 | 40 → 32 |
| Dynamic response headers | 15,137 → 12,835 | 304 → 245 | ~0 → ~0 | 245 → 206 | 48 → 39 |
| 1 KiB echo | 19,072 → 15,431 | 344 → 304 | ~0 → ~0 | 163 → 133 | 37 → 31 |
| 64 KiB echo | 344,149 → 211,473 | 2,117 → 1,731 | 328,024 → 196,567 | 155 → 144 | 154 → 143 |
| 64 KiB stream, 64 chunks | 299,754 → 56,203 | 3,391 → 1,061 | 132,097 → ~0 | 112 → 22 | 109 → 6 |
| 64 KiB buffered stream | 416,788 → 34,633 | 2,274 → 684 | 389,255 → 6,858 | 36 → 13 | 35 → 6 |

Counters include server startup, warmup, and shutdown. Per-request figures use
warmup plus measured requests as the divisor for fixed responses, and 640 for
streams. `Gc.quick_stat` may lag, which produced small negative estimates of
direct-major allocation; these are shown as approximately zero. Allocation
counts do not include native allocations.

The constant-response case reuses the same response object in both versions;
the current version additionally calls `Router.prepare` once before serving.

## End-to-end performance

| Workload | Requests/s baseline → current | p95 latency ms baseline → current | p99 latency ms baseline → current |
| --- | ---: | ---: | ---: |
| Small fixed | 100,077 → 105,343 | 0.524 → 0.473 | 0.683 → 0.663 |
| Reused constant, prepared in current | 107,464 → 106,757 | 0.460 → 0.476 | 0.608 → 0.651 |
| Dynamic response headers | 88,408 → 91,475 | 0.570 → 0.602 | 0.709 → 0.738 |
| 1 KiB echo | 71,673 → 73,394 | 0.678 → 0.665 | 0.816 → 0.823 |
| 64 KiB echo | 8,650 → 10,008 | 6.234 → 5.159 | 7.467 → 6.541 |
| 64 KiB stream | 1,289 → 2,702 | 29.154 → 12.464 | 31.002 → 12.847 |
| 64 KiB buffered stream | 6,861 → 13,068 | 8.182 → 4.713 | 8.942 → 6.526 |

These are short local runs. The Python streaming client and its scheduling are
part of the streaming timing. Dynamic-header tail latency was slightly higher
in the median current run; the three runs are too few to establish a stable
timing difference. No performance run used Memprof.

## Memory after large responses

A separate matching pair of servers each served 128 one-MiB fixed responses
with 16 concurrent clients, then performed two full major collections. In two
runs, baseline live OCaml heap was 147,790 words and current was 148,959 words;
heap capacity ranged from 248,357 to 252,453 words baseline and was 240,165
words current. Process RSS after collection ranged from 44,024 to 59,456 KiB
baseline and 17,872 to 18,060 KiB current. RSS includes native allocator and
runtime memory and can vary with allocation and page-return behavior; it is not
a direct measurement of the buffer pool. The current pool retains at most 16
buffers of at most 64 KiB per domain and discards larger buffers on release.

Raw run data and the local harnesses are in
`/tmp/halo-gc-pooling-20261109/compare-run-{1,2,3}.json`,
`/tmp/halo-gc-pooling-20261109/component-integrated-results.txt`,
`/tmp/halo-gc-pooling-20261109/compare-constant-{1,2,3}.json`,
`/tmp/halo-pooling-compare.py`, and `/tmp/halo-pooling-retained.py`.

## Benchmark rerun

Three more unprofiled repetitions used the same matched release binaries,
workloads, CPU pinning, warmup and clients described above. All completed
without request errors. The following figures are medians; throughput and
latency are short-run measurements and should be treated as directional.

| Workload | Allocated B/request baseline → current | Requests/s baseline → current | p95 ms baseline → current |
| --- | ---: | ---: | ---: |
| Small fixed | 9,936 → 8,282 | 99,509 → 105,490 | 0.518 → 0.470 |
| Reused constant, prepared in current | 9,707 → 7,476 | 104,242 → 106,056 | 0.487 → 0.471 |
| Dynamic response headers | 15,138 → 12,837 | 84,609 → 90,989 | 0.629 → 0.594 |
| 1 KiB echo | 19,069 → 15,419 | 71,977 → 73,511 | 0.655 → 0.674 |
| 64 KiB echo | 344,027 → 211,384 | 8,357 → 9,621 | 6.717 → 5.272 |
| 64 KiB stream, 64 chunks | 300,260 → 56,288 | 1,248 → 2,738 | 30.761 → 12.374 |
| 64 KiB buffered stream | 416,716 → 32,709 | 6,814 → 13,183 | 7.921 → 4.539 |

The streaming benchmarks include the Python client's scheduling overhead. The
GC counters and latency distributions for each run, along with the isolated
benchmark scripts and logs, are in `/tmp/halo-response-bench-nb9vohsf/`.
Component and post-collection memory measurements above were not rerun.
