#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DUNE_BIN="${DUNE_BIN:-$ROOT_DIR/_opam/bin/dune}"
DUNE_TARGET="${DUNE_TARGET:-./test/demo_server.exe}"
DUNE_PROFILE="${DUNE_PROFILE:-release}"
SERVER_BIN="${SERVER_BIN:-$ROOT_DIR/_build/default/test/demo_server.exe}"
BENCH_BIN="${BENCH_BIN:-$ROOT_DIR/_build/default/scripts/http_bench.exe}"
RESULT_DIR="${RESULT_DIR:-$ROOT_DIR/bench-results}"
SERVER_LOG="$RESULT_DIR/server.log"
SERVER_TIME_LOG="$RESULT_DIR/server-time.log"
PIDSTAT_LOG="$RESULT_DIR/server-pidstat.log"
RESULT_JSON="$RESULT_DIR/baseline.json"
PIDSTAT_BIN="${PIDSTAT_BIN:-}"
PROCESS_SAMPLE_INTERVAL_SECONDS="${PROCESS_SAMPLE_INTERVAL_SECONDS:-0.1}"
HOST="${HOST:-127.0.0.1}"
PORT="${PORT:-8080}"
REQUESTS="${REQUESTS:-5000}"
CONCURRENCY="${CONCURRENCY:-32}"
WARMUP_REQUESTS="${WARMUP_REQUESTS:-200}"
REPETITIONS="${REPETITIONS:-3}"
COOLDOWN_SECONDS="${COOLDOWN_SECONDS:-1.2}"
BENCH_SEED="${BENCH_SEED:-$RANDOM}"

if [[ ! "$REPETITIONS" =~ ^[1-9][0-9]*$ || ! "$BENCH_SEED" =~ ^[0-9]{1,5}$ || ! "$COOLDOWN_SECONDS" =~ ^[0-9]+([.][0-9]+)?$ ]]; then
  echo "REPETITIONS must be positive, BENCH_SEED between 0 and 32767, and COOLDOWN_SECONDS non-negative" >&2
  exit 2
fi
BENCH_SEED=$((10#$BENCH_SEED))
if ((BENCH_SEED > 32767)); then
  echo "BENCH_SEED must be between 0 and 32767" >&2
  exit 2
fi
mkdir -p "$RESULT_DIR"
RUN_DIR="$(mktemp -d "$RESULT_DIR/baseline-runs.XXXXXX")"
PROCESS_SAMPLE_JSON="$RUN_DIR/server-process-sample.json"
MANIFEST="$RUN_DIR/manifest.tsv"
: > "$MANIFEST"
: > "$PIDSTAT_LOG"

# Stop the server before waiting for the sampler, which follows its lifetime.
# Bound each wait so failed startup or shutdown cannot hang the harness.
stop_child() {
  local pid="$1" signal="$2" i
  kill -s "$signal" "$pid" 2>/dev/null || true
  for ((i = 0; i < 100; i++)); do
    kill -0 "$pid" 2>/dev/null || break
    sleep 0.1
  done
  kill -KILL "$pid" 2>/dev/null || true
  wait "$pid" 2>/dev/null || true
}
cleanup() {
  if [[ -n "${SERVER_PID:-}" ]]; then
    stop_child "$SERVER_PID" INT
    unset SERVER_PID
  fi
  if [[ -n "${PIDSTAT_PID:-}" ]]; then
    stop_child "$PIDSTAT_PID" TERM
    unset PIDSTAT_PID
  fi
  if [[ -n "${PROCESS_SAMPLE_PID:-}" ]]; then
    # The sampler exits after observing the stopped server.
    for ((i = 0; i < 100; i++)); do
      kill -0 "$PROCESS_SAMPLE_PID" 2>/dev/null || break
      sleep 0.1
    done
    stop_child "$PROCESS_SAMPLE_PID" TERM
    unset PROCESS_SAMPLE_PID
  fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

make -C "$ROOT_DIR" bench-tools >/dev/null
"$DUNE_BIN" build --profile "$DUNE_PROFILE" "$DUNE_TARGET" >/dev/null
if [[ ! -x "$SERVER_BIN" || ! -x "$BENCH_BIN" ]]; then
  echo "server or benchmark binary not found or not executable" >&2
  exit 1
fi
if [[ -z "$PIDSTAT_BIN" ]]; then
  if [[ -x /usr/bin/pidstat ]]; then PIDSTAT_BIN=/usr/bin/pidstat
  elif command -v pidstat >/dev/null 2>&1; then PIDSTAT_BIN="$(command -v pidstat)"
  fi
fi

"$SERVER_BIN" >"$SERVER_LOG" 2>"$SERVER_TIME_LOG" &
SERVER_PID=$!
if [[ -n "$PIDSTAT_BIN" ]]; then
  "$PIDSTAT_BIN" -r -u -p "$SERVER_PID" 1 >"$PIDSTAT_LOG" 2>&1 &
  PIDSTAT_PID=$!
fi
"$ROOT_DIR/_build/default/scripts/process_sample.exe" \
  --pid "$SERVER_PID" --interval-seconds "$PROCESS_SAMPLE_INTERVAL_SECONDS" \
  --output "$PROCESS_SAMPLE_JSON" &
PROCESS_SAMPLE_PID=$!
"$BENCH_BIN" --wait-ready --host "$HOST" --port "$PORT"

headers=(--header 'X-Header-01: alpha' --header 'X-Header-02: beta'
  --header 'X-Header-03: gamma' --header 'X-Header-04: delta'
  --header 'X-Header-05: epsilon' --header 'X-Header-06: zeta'
  --header 'X-Header-07: eta' --header 'X-Header-08: theta'
  --header 'X-Header-09: iota' --header 'X-Header-10: kappa')
printf -v body '%1024s' ''
body="${body// /a}"
scenarios=(root_get header_heavy_get post_submit_1k root_get_keep_alive header_heavy_get_keep_alive post_submit_1k_keep_alive)
RANDOM="$BENCH_SEED"
for ((trial = 1; trial <= REPETITIONS; trial++)); do
  order=("${scenarios[@]}")
  for ((i = ${#order[@]} - 1; i > 0; i--)); do
    j=$((RANDOM % (i + 1)))
    tmp="${order[i]}"; order[i]="${order[j]}"; order[j]="$tmp"
  done
  for ((position = 0; position < ${#order[@]}; position++)); do
    scenario="${order[position]}"
    options=()
    case "$scenario" in
      root_get*) options+=(--path /) ;;
      header_heavy_get*) options+=(--path /inspect/headers "${headers[@]}") ;;
      post_submit_1k*) options+=(--method POST --path /submit --body "$body") ;;
    esac
    [[ "$scenario" != *_keep_alive ]] || options+=(--keep-alive)
    sleep "$COOLDOWN_SECONDS"
    output="$RUN_DIR/$trial-$position.json"
    error="$RUN_DIR/$trial-$position.stderr"
    if "$BENCH_BIN" --scenario "$scenario" --host "$HOST" --port "$PORT" \
      --requests "$REQUESTS" --concurrency "$CONCURRENCY" --warmup-requests "$WARMUP_REQUESTS" \
      --expect-status 200 "${options[@]}" >"$output" 2>"$error"; then
      code=0
    else
      code=$?
    fi
    printf '%s\t%s\t%s\t%s\t%s\t%s\n' "$scenario" "$trial" "$((position + 1))" "$code" "$output" "$error" >> "$MANIFEST"
  done
done

cleanup
"$ROOT_DIR/_build/default/scripts/bench_report.exe" baseline-runs \
  "$RESULT_JSON" "$SERVER_TIME_LOG" "$PIDSTAT_LOG" "$PROCESS_SAMPLE_JSON" \
  "$MANIFEST" "$REPETITIONS" "$BENCH_SEED" "$COOLDOWN_SECONDS" \
  "$REQUESTS" "$CONCURRENCY" "$WARMUP_REQUESTS" "$HOST" "$PORT" "$DUNE_PROFILE"
