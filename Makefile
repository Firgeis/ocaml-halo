OPAM = opam
DUNE = $(OPAM) exec --switch=. -- dune
VENDOR_FORCE_FETCH = 0
VERSION ?= $(shell git describe --tags --always 2>/dev/null | sed 's/^v//')

VENDOR_DIR = vendor

S2N_TLS_DIR = $(VENDOR_DIR)/s2n-tls
S2N_TLS_BUILD_DIR = $(S2N_TLS_DIR)/lib
S2N_TLS_LIB = $(S2N_TLS_BUILD_DIR)/libs2n.a
S2N_TLS_S2NC = $(S2N_TLS_DIR)/bin/s2nc
S2N_TLS_RUNTIME_DIR = $(S2N_TLS_BUILD_DIR)
S2N_TLS_FETCH_URL = https://github.com/aws/s2n-tls/archive/refs/tags/v1.7.12.tar.gz
S2N_TLS_FETCH_REF = v1.7.12
S2N_TLS_FETCH_SHA256 = 76d3e6125ad8f7d960a62bf1e18c451fb75ed6ab640247b046b5b0f55d39d099

LIBUV_DIR = $(VENDOR_DIR)/libuv
LIBUV_FETCH_URL = https://github.com/libuv/libuv/archive/refs/tags/v1.53.0.tar.gz
LIBUV_FETCH_REF = v1.53.0
LIBUV_FETCH_SHA256 = 279f3f67a24bb9921fe999ca6cd5e332fade8d515873ef9ba054b70e70a31d9e

LLHTTP_DIR = $(VENDOR_DIR)/llhttp
LLHTTP_FETCH_URL = https://github.com/nodejs/llhttp/archive/refs/tags/release/v9.4.3.tar.gz
LLHTTP_FETCH_REF = release/v9.4.3
LLHTTP_FETCH_SHA256 = 1eb813c7437b31a87496a1cd3ed79f00746720f5e7e29c79b42c02cb69f36c39

OTELCOL_DIR = $(VENDOR_DIR)/otelcol
OTELCOL_BIN = $(OTELCOL_DIR)/otelcol
OTELCOL_VERSION = 0.160.0
OTELCOL_FETCH_URL = https://github.com/open-telemetry/opentelemetry-collector-releases/releases/tag/v$(OTELCOL_VERSION)

VENDORED_FETCH_DEPS = s2n-tls libuv llhttp
VENDORED_BUILT_DEPS = s2n-tls
VENDORED_BUILT_TARGETS = $(S2N_TLS_LIB)
VENDORED_TEST_TARGETS = $(S2N_TLS_S2NC) $(OTELCOL_BIN)
VENDORED_RUNTIME_LIBRARY_DIRS = $(S2N_TLS_RUNTIME_DIR)
LIBCRYPTO_ROOT_DEFAULT = /usr

vendored_runtime_library_path = "`pwd`/$(S2N_TLS_RUNTIME_DIR)"
VENDORED_RUNTIME_ENV = \
	LIBRARY_PATH=$(vendored_runtime_library_path) \
	LD_LIBRARY_PATH=$(vendored_runtime_library_path) \
	DYLD_LIBRARY_PATH=$(vendored_runtime_library_path)

.PHONY: \
	build \
	release-build \
	release-source-artifact \
	run \
	release-run \
	body-stress \
	bench-tools \
	bench-tools-test \
	bench-baseline-test \
	router-bench \
	test \
	runtest \
	clean \
	asan-build \
	asan-run \
	asan-https-smoke \
	vendor-build \
	vendor-test-tools \
	vendor-clean \
	vendor-fetch \
	vendor-refetch \
	vendor-status \
	local-setup \
	vendor-fetch-s2n-tls \
	vendor-fetch-libuv \
	vendor-fetch-llhttp

$(S2N_TLS_LIB):
	@set -e; \
	if [ -f ./mk/local.env ]; then . ./mk/local.env; fi; \
	$(MAKE) -C $(S2N_TLS_DIR) libs LIBCRYPTO_ROOT="$${LIBCRYPTO_ROOT:-$(LIBCRYPTO_ROOT_DEFAULT)}"

$(S2N_TLS_S2NC):
	@set -e; \
	if [ -f ./mk/local.env ]; then . ./mk/local.env; fi; \
	$(MAKE) -C $(S2N_TLS_DIR) bin LIBCRYPTO_ROOT="$${LIBCRYPTO_ROOT:-$(LIBCRYPTO_ROOT_DEFAULT)}"

$(OTELCOL_BIN):
	bash ./scripts/fetch_otelcol.sh $(OTELCOL_VERSION) ./$(OTELCOL_DIR)

vendor-build: $(VENDORED_BUILT_TARGETS)

vendor-test-tools: $(VENDORED_TEST_TARGETS)

vendor-clean:
	bash ./scripts/vendor_reset.sh ./$(S2N_TLS_DIR)
	bash ./scripts/vendor_reset.sh ./$(LIBUV_DIR)
	bash ./scripts/vendor_reset.sh ./$(LLHTTP_DIR)
	bash ./scripts/vendor_reset.sh ./$(OTELCOL_DIR)

vendor-fetch: vendor-fetch-s2n-tls vendor-fetch-libuv vendor-fetch-llhttp

vendor-refetch:
	$(MAKE) vendor-fetch VENDOR_FORCE_FETCH=1

vendor-status:
	@printf '%s\n' \
		's2n-tls $(S2N_TLS_FETCH_REF) $(S2N_TLS_FETCH_URL)' \
		'libuv $(LIBUV_FETCH_REF) $(LIBUV_FETCH_URL)' \
		'llhttp $(LLHTTP_FETCH_REF) $(LLHTTP_FETCH_URL)' \
		'otelcol v$(OTELCOL_VERSION) $(OTELCOL_FETCH_URL)'

local-setup:
	bash ./scripts/detect_local_env.sh ./mk/local.env

vendor-fetch-s2n-tls:
	bash ./scripts/vendor_fetch.sh s2n-tls $(S2N_TLS_FETCH_URL) $(S2N_TLS_FETCH_REF) ./$(S2N_TLS_DIR) $(S2N_TLS_FETCH_SHA256) $(VENDOR_FORCE_FETCH)

vendor-fetch-libuv:
	bash ./scripts/vendor_fetch.sh libuv $(LIBUV_FETCH_URL) $(LIBUV_FETCH_REF) ./$(LIBUV_DIR) $(LIBUV_FETCH_SHA256) $(VENDOR_FORCE_FETCH)

vendor-fetch-llhttp:
	bash ./scripts/vendor_fetch.sh llhttp $(LLHTTP_FETCH_URL) $(LLHTTP_FETCH_REF) ./$(LLHTTP_DIR) $(LLHTTP_FETCH_SHA256) $(VENDOR_FORCE_FETCH)

build: vendor-build
	$(VENDORED_RUNTIME_ENV) $(DUNE) build

release-build: vendor-build
	$(VENDORED_RUNTIME_ENV) $(DUNE) build --profile release ./test/demo_server.exe

release-source-artifact: vendor-fetch
	bash ./scripts/package_release_source.sh "$(VERSION)" ./dist

asan-build: vendor-build
	$(VENDORED_RUNTIME_ENV) $(DUNE) build --profile asan ./test/demo_server.exe

run: vendor-build
	$(VENDORED_RUNTIME_ENV) $(DUNE) exec ./test/demo_server.exe

release-run: release-build
	DYLD_LIBRARY_PATH=$(vendored_runtime_library_path) \
	LD_LIBRARY_PATH=$(vendored_runtime_library_path) \
	./_build/release/test/demo_server.exe

bench-tools:
	$(DUNE) build ./scripts/http_bench.exe ./scripts/https_bench.exe ./scripts/process_sample.exe ./scripts/bench_report.exe

bench-tools-test:
	$(DUNE) build @test/runtest-bench_report_test @test/runtest-bench_tools_test --force

bench-baseline-test: bench-tools
	bash ./test/benchmark_baseline_test.sh

body-stress: vendor-build bench-tools
	$(VENDORED_RUNTIME_ENV) bash ./scripts/body_stress.sh

router-bench: vendor-build
	$(VENDORED_RUNTIME_ENV) $(DUNE) exec --profile release ./scripts/router_bench.exe

asan-run: asan-build
	DYLD_LIBRARY_PATH=$(vendored_runtime_library_path) \
	LD_LIBRARY_PATH=$(vendored_runtime_library_path) \
	ASAN_OPTIONS=detect_leaks=1:abort_on_error=1:halt_on_error=1 \
	./_build/default/test/demo_server.exe

asan-https-smoke: asan-build bench-tools
	bash ./scripts/asan_https_smoke.sh

test: vendor-build $(VENDORED_TEST_TARGETS)
	$(VENDORED_RUNTIME_ENV) $(DUNE) runtest -f

runtest: vendor-build $(VENDORED_TEST_TARGETS)
	$(VENDORED_RUNTIME_ENV) $(DUNE) runtest -f

clean:
	$(MAKE) vendor-clean
	$(DUNE) clean
