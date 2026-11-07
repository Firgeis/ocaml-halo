#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DUNE_BIN="${DUNE_BIN:-$ROOT_DIR/_opam/bin/dune}"
DUNE_TARGET="${DUNE_TARGET:-./test/demo_server.exe}"
SERVER_BIN="${SERVER_BIN:-$ROOT_DIR/_build/default/test/demo_server.exe}"
RESULT_DIR="${RESULT_DIR:-$ROOT_DIR/bench-results}"
SERVER_LOG="$RESULT_DIR/server.log"
SERVER_TIME_LOG="$RESULT_DIR/server-time.log"
PIDSTAT_LOG="$RESULT_DIR/server-pidstat.log"
PROCESS_SAMPLE_JSON="$RESULT_DIR/server-process-sample.json"
RESULT_JSON="$RESULT_DIR/baseline.json"
TIME_BIN="${TIME_BIN:-}"
PIDSTAT_BIN="${PIDSTAT_BIN:-}"
PROCESS_SAMPLE_INTERVAL_SECONDS="${PROCESS_SAMPLE_INTERVAL_SECONDS:-0.1}"
HOST="${HOST:-127.0.0.1}"
PORT="${PORT:-8080}"
REQUESTS="${REQUESTS:-5000}"
CONCURRENCY="${CONCURRENCY:-32}"
WARMUP_REQUESTS="${WARMUP_REQUESTS:-200}"

mkdir -p "$RESULT_DIR"

cleanup() {
  if [[ -n "${PROCESS_SAMPLE_PID:-}" ]] && kill -0 "$PROCESS_SAMPLE_PID" 2>/dev/null; then
    wait "$PROCESS_SAMPLE_PID" || true
  fi
  if [[ -n "${PIDSTAT_PID:-}" ]] && kill -0 "$PIDSTAT_PID" 2>/dev/null; then
    kill "$PIDSTAT_PID" 2>/dev/null || true
    wait "$PIDSTAT_PID" || true
  fi
  if [[ -n "${SERVER_PID:-}" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
    kill -INT "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" || true
  fi
}

trap cleanup EXIT

make -C "$ROOT_DIR" bench-tools >/dev/null
"$DUNE_BIN" build "$DUNE_TARGET" >/dev/null

if [[ ! -x "$SERVER_BIN" ]]; then
  echo "server binary not found or not executable: $SERVER_BIN" >&2
  exit 1
fi

if [[ -z "$PIDSTAT_BIN" ]]; then
  if [[ -x /usr/bin/pidstat ]]; then
    PIDSTAT_BIN=/usr/bin/pidstat
  elif command -v pidstat >/dev/null 2>&1; then
    PIDSTAT_BIN="$(command -v pidstat)"
  fi
fi

"$SERVER_BIN" >"$SERVER_LOG" 2>"$SERVER_TIME_LOG" &
SERVER_PID=$!

if [[ -n "$PIDSTAT_BIN" ]]; then
  "$PIDSTAT_BIN" -r -u -p "$SERVER_PID" 1 >"$PIDSTAT_LOG" 2>&1 &
  PIDSTAT_PID=$!
fi

"$ROOT_DIR/_build/default/scripts/process_sample.exe" \
  --pid "$SERVER_PID" \
  --interval-seconds "$PROCESS_SAMPLE_INTERVAL_SECONDS" \
  --output "$PROCESS_SAMPLE_JSON" &
PROCESS_SAMPLE_PID=$!

"$ROOT_DIR/_build/default/scripts/http_bench.exe" --wait-ready --host "$HOST" --port "$PORT"

ROOT_REQUEST=$("$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario root_get \
  --host "$HOST" \
  --port "$PORT" \
  --path / \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS")

HEADER_REQUEST=$("$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario header_heavy_get \
  --host "$HOST" \
  --port "$PORT" \
  --path /inspect/headers \
  --header "X-Header-01: alpha" \
  --header "X-Header-02: beta" \
  --header "X-Header-03: gamma" \
  --header "X-Header-04: delta" \
  --header "X-Header-05: epsilon" \
  --header "X-Header-06: zeta" \
  --header "X-Header-07: eta" \
  --header "X-Header-08: theta" \
  --header "X-Header-09: iota" \
  --header "X-Header-10: kappa" \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS")

POST_REQUEST=$("$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario post_submit_1k \
  --host "$HOST" \
  --port "$PORT" \
  --method POST \
  --path /submit \
  --body "$(printf 'a%.0s' $(seq 1 1024))" \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS")

ROOT_KEEP_ALIVE_REQUEST=$("$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario root_get_keep_alive \
  --host "$HOST" \
  --port "$PORT" \
  --path / \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS" \
  --keep-alive)

HEADER_KEEP_ALIVE_REQUEST=$("$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario header_heavy_get_keep_alive \
  --host "$HOST" \
  --port "$PORT" \
  --path /inspect/headers \
  --header "X-Header-01: alpha" \
  --header "X-Header-02: beta" \
  --header "X-Header-03: gamma" \
  --header "X-Header-04: delta" \
  --header "X-Header-05: epsilon" \
  --header "X-Header-06: zeta" \
  --header "X-Header-07: eta" \
  --header "X-Header-08: theta" \
  --header "X-Header-09: iota" \
  --header "X-Header-10: kappa" \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS" \
  --keep-alive)

POST_KEEP_ALIVE_REQUEST=$("$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario post_submit_1k_keep_alive \
  --host "$HOST" \
  --port "$PORT" \
  --method POST \
  --path /submit \
  --body "$(printf 'a%.0s' $(seq 1 1024))" \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS" \
  --keep-alive)

kill -INT "$SERVER_PID" 2>/dev/null || true
wait "$SERVER_PID" || true
unset SERVER_PID

if [[ -n "${PIDSTAT_PID:-}" ]]; then
  wait "$PIDSTAT_PID" || true
  unset PIDSTAT_PID
fi
if [[ -n "${PROCESS_SAMPLE_PID:-}" ]]; then
  wait "$PROCESS_SAMPLE_PID" || true
  unset PROCESS_SAMPLE_PID
fi

"$ROOT_DIR/_build/default/scripts/bench_report.exe" baseline \
  "$RESULT_JSON" "$SERVER_TIME_LOG" "$PIDSTAT_LOG" "$PROCESS_SAMPLE_JSON" \
  "$ROOT_REQUEST" "$HEADER_REQUEST" "$POST_REQUEST" \
  "$ROOT_KEEP_ALIVE_REQUEST" "$HEADER_KEEP_ALIVE_REQUEST" "$POST_KEEP_ALIVE_REQUEST"
