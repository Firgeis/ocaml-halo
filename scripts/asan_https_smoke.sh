#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SERVER_BIN="${SERVER_BIN:-$ROOT_DIR/_build/default/test/demo_server.exe}"
CERT_FILE="${CERT_FILE:-$ROOT_DIR/test/fixtures/tls-cert.pem}"
KEY_FILE="${KEY_FILE:-$ROOT_DIR/test/fixtures/tls-key.pem}"
HOST="${HOST:-127.0.0.1}"
PORT="${PORT:-8080}"
REQUESTS="${REQUESTS:-100}"
CONCURRENCY="${CONCURRENCY:-4}"
WARMUP_REQUESTS="${WARMUP_REQUESTS:-10}"
ASAN_OPTIONS_VALUE="${ASAN_OPTIONS:-detect_leaks=0:abort_on_error=1:halt_on_error=1}"

SERVER_PID=""

cleanup() {
  local exit_status=$?
  local server_status=0
  trap - EXIT INT TERM
  if [[ -n "$SERVER_PID" ]]; then
    if kill -0 "$SERVER_PID" 2>/dev/null; then
      kill -INT "$SERVER_PID" 2>/dev/null || true
    fi
    # Sanitizers can report failures during server shutdown.
    wait "$SERVER_PID" || server_status=$?
    if (( exit_status == 0 )); then
      exit_status=$server_status
    fi
  fi
  exit "$exit_status"
}

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

DYLD_LIBRARY_PATH="$ROOT_DIR/vendor/s2n-tls/lib" \
LD_LIBRARY_PATH="$ROOT_DIR/vendor/s2n-tls/lib" \
ASAN_OPTIONS="$ASAN_OPTIONS_VALUE" \
HALO_TLS_CERT_CHAIN="$CERT_FILE" \
HALO_TLS_PRIVATE_KEY="$KEY_FILE" \
"$SERVER_BIN" &
SERVER_PID=$!

python3 - "$CERT_FILE" "$HOST" "$PORT" <<'PY'
import socket
import ssl
import sys
import time

cert_file = sys.argv[1]
host = sys.argv[2]
port = int(sys.argv[3])

ctx = ssl.create_default_context(cafile=cert_file)
ctx.check_hostname = False
ctx.minimum_version = ssl.TLSVersion.TLSv1_3
deadline = time.time() + 5.0

while True:
    try:
        with socket.create_connection((host, port), timeout=0.2) as raw:
            with ctx.wrap_socket(raw, server_hostname="localhost"):
                pass
        break
    except OSError:
        if time.time() >= deadline:
            raise SystemExit("server did not start listening in time")
        time.sleep(0.05)
PY

python3 "$ROOT_DIR/scripts/https_bench.py" \
  --scenario tls_smoke \
  --host "$HOST" \
  --port "$PORT" \
  --server-name localhost \
  --ca-file "$CERT_FILE" \
  --path / \
  --requests "$REQUESTS" \
  --concurrency "$CONCURRENCY" \
  --warmup-requests "$WARMUP_REQUESTS" \
  --reuse-mode full
