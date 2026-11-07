#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TEST_DIR="$(mktemp -d)"
trap 'rm -rf "$TEST_DIR"' EXIT

cat > "$TEST_DIR/failing-bench" <<'BENCH'
#!/usr/bin/env bash
if [[ "$1" == --wait-ready ]]; then
  exec "$REAL_BENCH" "$@"
fi
echo '{"errors":["synthetic connection failure"],"completed_requests":0}'
echo 'synthetic client diagnostic' >&2
exit 17
BENCH
chmod +x "$TEST_DIR/failing-bench"

export REAL_BENCH="$ROOT_DIR/_build/default/scripts/http_bench.exe"
export BENCH_BIN="$TEST_DIR/failing-bench" RESULT_DIR="$TEST_DIR/results"
export REPETITIONS=1 COOLDOWN_SECONDS=0 BENCH_SEED=5
status=0
timeout 25s bash "$ROOT_DIR/scripts/benchmark-baseline.sh" >"$TEST_DIR/output" 2>"$TEST_DIR/error" || status=$?
if [[ "$status" != 1 ]]; then
  echo "expected a reported benchmark failure, got exit $status" >&2
  cat "$TEST_DIR/error" >&2
  exit 1
fi
report="$RESULT_DIR/baseline.json"
[[ -s "$report" ]] || { echo 'missing failure report' >&2; exit 1; }
[[ "$(grep -c '"exit_code": 17' "$report")" == 6 ]] || { echo 'missing trial exit codes' >&2; exit 1; }
[[ "$(grep -c 'synthetic connection failure' "$report")" == 6 ]] || { echo 'missing client errors' >&2; exit 1; }
[[ "$(grep -c 'synthetic client diagnostic' "$report")" == 6 ]] || { echo 'missing client diagnostics' >&2; exit 1; }
echo 'baseline failure reporting and cleanup passed'

export BENCH_BIN="$REAL_BENCH" RESULT_DIR="$TEST_DIR/success"
export REPETITIONS=2 REQUESTS=32 WARMUP_REQUESTS=0 CONCURRENCY=4
timeout 25s bash "$ROOT_DIR/scripts/benchmark-baseline.sh" >"$TEST_DIR/success-output"
report="$RESULT_DIR/baseline.json"
[[ "$(grep -c '"successful_runs": 2' "$report")" == 6 ]] || { echo 'missing successful repetitions' >&2; exit 1; }
[[ "$(grep -c '"trial": 2' "$report")" == 6 ]] || { echo 'missing second trial' >&2; exit 1; }
echo 'baseline repeated trials passed'
