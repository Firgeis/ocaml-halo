#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DUNE_BIN="${DUNE_BIN:-$ROOT_DIR/_opam/bin/dune}"
DUNE_TARGET="${DUNE_TARGET:-./test/demo_server.exe}"
SERVER_BIN="${SERVER_BIN:-$ROOT_DIR/_build/default/test/demo_server.exe}"
RESULT_DIR="${RESULT_DIR:-$ROOT_DIR/bench-results}"
RESULT_JSON="${RESULT_JSON:-$RESULT_DIR/body-stress.json}"
SERVER_LOG="${SERVER_LOG:-$RESULT_DIR/body-stress-server.log}"
PROCESS_SAMPLE_JSON="${PROCESS_SAMPLE_JSON:-$RESULT_DIR/body-stress-process-sample.json}"
HOST="${HOST:-127.0.0.1}"
PORT="${PORT:-8080}"
MAX_BODY_SIZE="${MAX_BODY_SIZE:-1048576}"
ACCEPTED_BODY_SIZE="${ACCEPTED_BODY_SIZE:-$((MAX_BODY_SIZE - 1024))}"
OVERSIZED_BODY_SIZE="${OVERSIZED_BODY_SIZE:-$((MAX_BODY_SIZE + 1024))}"
REQUESTS="${REQUESTS:-12}"
CONCURRENCY="${CONCURRENCY:-2}"
WARMUP_REQUESTS="${WARMUP_REQUESTS:-2}"
TIMEOUT_SECONDS="${TIMEOUT_SECONDS:-15}"
CHUNK_SIZE="${CHUNK_SIZE:-8192}"
PROCESS_SAMPLE_INTERVAL_SECONDS="${PROCESS_SAMPLE_INTERVAL_SECONDS:-0.02}"
KEEP_ALIVE="${KEEP_ALIVE:-0}"

KEEP_ALIVE_ARGS=()
case "$KEEP_ALIVE" in
  0|false|no|off) KEEP_ALIVE_ENABLED=0 ;;
  1|true|yes|on)
    KEEP_ALIVE_ENABLED=1
    KEEP_ALIVE_ARGS+=(--keep-alive)
    ;;
  *)
    echo "KEEP_ALIVE must be a boolean" >&2
    exit 1
    ;;
esac

mkdir -p "$RESULT_DIR"

run_bench() {
  local __result_var="$1"
  shift

  local output
  local exit_code
  if output=$("$@" 2>&1); then
    exit_code=0
  else
    exit_code=$?
  fi

  printf -v "$__result_var" '%s' "$output"
  return "$exit_code"
}

if (( MAX_BODY_SIZE <= 0 )); then
  echo "MAX_BODY_SIZE must be positive" >&2
  exit 1
fi

if (( ACCEPTED_BODY_SIZE <= 0 )); then
  echo "ACCEPTED_BODY_SIZE must be positive" >&2
  exit 1
fi

if (( ACCEPTED_BODY_SIZE > MAX_BODY_SIZE )); then
  echo "ACCEPTED_BODY_SIZE must be less than or equal to MAX_BODY_SIZE" >&2
  exit 1
fi

if (( OVERSIZED_BODY_SIZE <= MAX_BODY_SIZE )); then
  echo "OVERSIZED_BODY_SIZE must be greater than MAX_BODY_SIZE" >&2
  exit 1
fi

cleanup() {
  if [[ -n "${SERVER_PID:-}" ]] && kill -0 "$SERVER_PID" 2>/dev/null; then
    kill -INT "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" || true
  fi
  if [[ -n "${PROCESS_SAMPLE_PID:-}" ]] && kill -0 "$PROCESS_SAMPLE_PID" 2>/dev/null; then
    wait "$PROCESS_SAMPLE_PID" || true
  fi
  rm -f "${ACCEPTED_BODY_FILE:-}" "${OVERSIZED_BODY_FILE:-}"
}

trap cleanup EXIT

make -C "$ROOT_DIR" bench-tools >/dev/null
"$DUNE_BIN" build "$DUNE_TARGET" >/dev/null

if [[ ! -x "$SERVER_BIN" ]]; then
  echo "server binary not found or not executable: $SERVER_BIN" >&2
  exit 1
fi

ACCEPTED_BODY_FILE="$(mktemp)"
OVERSIZED_BODY_FILE="$(mktemp)"

"$ROOT_DIR/_build/default/scripts/bench_report.exe" body-files \
  "$ACCEPTED_BODY_FILE" "$ACCEPTED_BODY_SIZE" "$OVERSIZED_BODY_FILE" "$OVERSIZED_BODY_SIZE"

HALO_MAX_BODY_SIZE="$MAX_BODY_SIZE" "$SERVER_BIN" >"$SERVER_LOG" 2>&1 &
SERVER_PID=$!

"$ROOT_DIR/_build/default/scripts/process_sample.exe" \
  --pid "$SERVER_PID" \
  --interval-seconds "$PROCESS_SAMPLE_INTERVAL_SECONDS" \
  --output "$PROCESS_SAMPLE_JSON" &
PROCESS_SAMPLE_PID=$!

"$ROOT_DIR/_build/default/scripts/http_bench.exe" --wait-ready --host "$HOST" --port "$PORT"

if run_bench ACCEPTED_RESULT \
  "$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario accepted_large_body \
  --host "$HOST" \
  --port "$PORT" \
  --method POST \
  --path /submit \
  --body-file "$ACCEPTED_BODY_FILE" \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS" \
  --timeout-seconds "$TIMEOUT_SECONDS" \
  "${KEEP_ALIVE_ARGS[@]}" \
  --expect-status 200; then
  ACCEPTED_EXIT_CODE=0
else
  ACCEPTED_EXIT_CODE=$?
fi

if run_bench ACCEPTED_CHUNKED_RESULT \
  "$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario accepted_large_body_chunked \
  --host "$HOST" \
  --port "$PORT" \
  --method POST \
  --path /submit \
  --body-file "$ACCEPTED_BODY_FILE" \
  --chunked \
  --chunk-size "$CHUNK_SIZE" \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS" \
  --timeout-seconds "$TIMEOUT_SECONDS" \
  "${KEEP_ALIVE_ARGS[@]}" \
  --expect-status 200; then
  ACCEPTED_CHUNKED_EXIT_CODE=0
else
  ACCEPTED_CHUNKED_EXIT_CODE=$?
fi

if run_bench OVERSIZED_RESULT \
  "$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario oversized_large_body \
  --host "$HOST" \
  --port "$PORT" \
  --method POST \
  --path /submit \
  --body-file "$OVERSIZED_BODY_FILE" \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests 0 \
  --timeout-seconds "$TIMEOUT_SECONDS" \
  --expect-status 413; then
  OVERSIZED_EXIT_CODE=0
else
  OVERSIZED_EXIT_CODE=$?
fi

if run_bench OVERSIZED_CHUNKED_RESULT \
  "$ROOT_DIR/_build/default/scripts/http_bench.exe" \
  --scenario oversized_large_body_chunked \
  --host "$HOST" \
  --port "$PORT" \
  --method POST \
  --path /submit \
  --body-file "$OVERSIZED_BODY_FILE" \
  --chunked \
  --chunk-size "$CHUNK_SIZE" \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests 0 \
  --timeout-seconds "$TIMEOUT_SECONDS" \
  --expect-status 413; then
  OVERSIZED_CHUNKED_EXIT_CODE=0
else
  OVERSIZED_CHUNKED_EXIT_CODE=$?
fi

kill -INT "$SERVER_PID" 2>/dev/null || true
wait "$SERVER_PID" || true
unset SERVER_PID

if [[ -n "${PROCESS_SAMPLE_PID:-}" ]]; then
  wait "$PROCESS_SAMPLE_PID" || true
  unset PROCESS_SAMPLE_PID
fi

"$ROOT_DIR/_build/default/scripts/bench_report.exe" body-report \
  "$RESULT_JSON" "$PROCESS_SAMPLE_JSON" "$MAX_BODY_SIZE" "$ACCEPTED_BODY_SIZE" \
  "$OVERSIZED_BODY_SIZE" "$CHUNK_SIZE" "$ACCEPTED_EXIT_CODE" "$ACCEPTED_CHUNKED_EXIT_CODE" \
  "$OVERSIZED_EXIT_CODE" "$OVERSIZED_CHUNKED_EXIT_CODE" "$ACCEPTED_RESULT" \
  "$ACCEPTED_CHUNKED_RESULT" "$OVERSIZED_RESULT" "$OVERSIZED_CHUNKED_RESULT" "$KEEP_ALIVE_ENABLED"
