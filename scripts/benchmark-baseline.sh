#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
DUNE_BIN="${DUNE_BIN:-$ROOT_DIR/_opam/bin/dune}"
PYTHON_BIN="${PYTHON_BIN:-python3}"
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

"$PYTHON_BIN" "$ROOT_DIR/scripts/process_sample.py" \
  --pid "$SERVER_PID" \
  --interval-seconds "$PROCESS_SAMPLE_INTERVAL_SECONDS" \
  --output "$PROCESS_SAMPLE_JSON" &
PROCESS_SAMPLE_PID=$!

"$PYTHON_BIN" - "$HOST" "$PORT" <<'PY'
import socket
import sys
import time

host = sys.argv[1]
port = int(sys.argv[2])
deadline = time.time() + 5.0

while True:
    try:
        with socket.create_connection((host, port), timeout=0.2):
            break
    except OSError:
        if time.time() >= deadline:
            raise SystemExit("server did not start listening in time")
        time.sleep(0.05)
PY

ROOT_REQUEST=$("$PYTHON_BIN" "$ROOT_DIR/scripts/http_bench.py" \
  --scenario root_get \
  --host "$HOST" \
  --port "$PORT" \
  --path / \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS")

HEADER_REQUEST=$("$PYTHON_BIN" "$ROOT_DIR/scripts/http_bench.py" \
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

POST_REQUEST=$("$PYTHON_BIN" "$ROOT_DIR/scripts/http_bench.py" \
  --scenario post_submit_1k \
  --host "$HOST" \
  --port "$PORT" \
  --method POST \
  --path /submit \
  --body "$(printf 'a%.0s' $(seq 1 1024))" \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS")

ROOT_KEEP_ALIVE_REQUEST=$("$PYTHON_BIN" "$ROOT_DIR/scripts/http_bench.py" \
  --scenario root_get_keep_alive \
  --host "$HOST" \
  --port "$PORT" \
  --path / \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS" \
  --keep-alive)

HEADER_KEEP_ALIVE_REQUEST=$("$PYTHON_BIN" "$ROOT_DIR/scripts/http_bench.py" \
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

POST_KEEP_ALIVE_REQUEST=$("$PYTHON_BIN" "$ROOT_DIR/scripts/http_bench.py" \
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

"$PYTHON_BIN" - "$RESULT_JSON" "$SERVER_TIME_LOG" "$PIDSTAT_LOG" "$PROCESS_SAMPLE_JSON" "$ROOT_REQUEST" "$HEADER_REQUEST" "$POST_REQUEST" "$ROOT_KEEP_ALIVE_REQUEST" "$HEADER_KEEP_ALIVE_REQUEST" "$POST_KEEP_ALIVE_REQUEST" <<'PY'
import json
import sys
from pathlib import Path

result_json = Path(sys.argv[1])
server_time_log = Path(sys.argv[2])
pidstat_log = Path(sys.argv[3])
process_sample_json = Path(sys.argv[4])
root_result = json.loads(sys.argv[5])
header_result = json.loads(sys.argv[6])
post_result = json.loads(sys.argv[7])
root_keep_alive_result = json.loads(sys.argv[8])
header_keep_alive_result = json.loads(sys.argv[9])
post_keep_alive_result = json.loads(sys.argv[10])

server_metrics = {}
for line in server_time_log.read_text().splitlines():
    if "=" not in line:
        continue
    key, value = line.split("=", 1)
    server_metrics[key] = value
if not server_metrics:
    server_metrics["timing_tool"] = "unavailable"

if pidstat_log.exists():
    cpu_samples = []
    rss_samples_kb = []
    for raw_line in pidstat_log.read_text().splitlines():
        line = raw_line.strip()
        if not line:
            continue
        if line.startswith("Linux ") or line.startswith("Average:"):
            continue
        if "UID" in line and "%usr" in line:
            continue
        parts = line.split()
        if len(parts) < 8:
            continue
        try:
            cpu_percent = float(parts[-1])
            rss_kb = float(parts[-3])
        except ValueError:
            continue
        cpu_samples.append(cpu_percent)
        rss_samples_kb.append(rss_kb)

    if cpu_samples:
        server_metrics["pidstat_cpu_percent_mean"] = f"{sum(cpu_samples) / len(cpu_samples):.2f}"
        server_metrics["pidstat_cpu_percent_max"] = f"{max(cpu_samples):.2f}"
        server_metrics["pidstat_sample_count"] = str(len(cpu_samples))
    if rss_samples_kb:
        server_metrics["pidstat_rss_kb_mean"] = f"{sum(rss_samples_kb) / len(rss_samples_kb):.2f}"
        server_metrics["pidstat_rss_kb_max"] = f"{max(rss_samples_kb):.0f}"

if process_sample_json.exists():
    process_sample = json.loads(process_sample_json.read_text())
    server_metrics["process_sample_interval_seconds"] = str(
        process_sample.get("sample_interval_seconds", 0.0)
    )
    server_metrics["process_sample_count"] = str(process_sample.get("sample_count", 0))
    server_metrics["process_cpu_percent_mean"] = f"{process_sample.get('cpu_percent_mean', 0.0):.2f}"
    server_metrics["process_cpu_percent_max"] = f"{process_sample.get('cpu_percent_max', 0.0):.2f}"
    server_metrics["process_rss_kb_mean"] = f"{process_sample.get('rss_kb_mean', 0.0):.2f}"
    server_metrics["process_rss_kb_max"] = f"{process_sample.get('rss_kb_max', 0.0):.0f}"

payload = {
    "server_metrics": server_metrics,
    "scenarios": [
        root_result,
        header_result,
        post_result,
        root_keep_alive_result,
        header_keep_alive_result,
        post_keep_alive_result,
    ],
}
result_json.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")
print(json.dumps(payload, indent=2, sort_keys=True))
PY
