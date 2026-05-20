OPAM_BIN = ./_opam/bin
DUNE = $(OPAM_BIN)/dune
VENDOR_FORCE_FETCH = 0
VERSION ?= $(shell git describe --tags --always 2>/dev/null | sed 's/^v//')

VENDOR_DIR = vendor

S2N_TLS_DIR = $(VENDOR_DIR)/s2n-tls
S2N_TLS_BUILD_DIR = $(S2N_TLS_DIR)/lib
S2N_TLS_LIB = $(S2N_TLS_BUILD_DIR)/libs2n.a
S2N_TLS_S2NC = $(S2N_TLS_DIR)/bin/s2nc
S2N_TLS_RUNTIME_DIR = $(S2N_TLS_BUILD_DIR)
S2N_TLS_FETCH_URL = https://github.com/aws/s2n-tls/archive/refs/tags/v1.7.3.tar.gz
S2N_TLS_FETCH_REF = v1.7.3

# The historical vendored libuv snapshot exposed 1.52.2-dev in headers, but
# upstream currently publishes 1.52.x releases through v1.52.1.
LIBUV_DIR = $(VENDOR_DIR)/libuv
LIBUV_FETCH_URL = https://github.com/libuv/libuv/archive/refs/tags/v1.52.1.tar.gz
LIBUV_FETCH_REF = v1.52.1

LLHTTP_DIR = $(VENDOR_DIR)/llhttp
LLHTTP_FETCH_URL = https://github.com/nodejs/llhttp/archive/refs/tags/release/v9.3.0.tar.gz
LLHTTP_FETCH_REF = release/v9.3.0

VENDORED_FETCH_DEPS = s2n-tls libuv llhttp
VENDORED_BUILT_DEPS = s2n-tls
VENDORED_BUILT_TARGETS = $(S2N_TLS_LIB)
VENDORED_TEST_TARGETS = $(S2N_TLS_S2NC)
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

vendor-build: $(VENDORED_BUILT_TARGETS)

vendor-test-tools: $(VENDORED_TEST_TARGETS)

vendor-clean:
	bash ./scripts/vendor_reset.sh ./$(S2N_TLS_DIR)
	bash ./scripts/vendor_reset.sh ./$(LIBUV_DIR)
	bash ./scripts/vendor_reset.sh ./$(LLHTTP_DIR)

vendor-fetch: vendor-fetch-s2n-tls vendor-fetch-libuv vendor-fetch-llhttp

vendor-refetch:
	$(MAKE) vendor-fetch VENDOR_FORCE_FETCH=1

vendor-status:
	@printf '%s\n' \
		's2n-tls $(S2N_TLS_FETCH_REF) $(S2N_TLS_FETCH_URL)' \
		'libuv $(LIBUV_FETCH_REF) $(LIBUV_FETCH_URL)' \
		'llhttp $(LLHTTP_FETCH_REF) $(LLHTTP_FETCH_URL)'

local-setup:
	bash ./scripts/detect_local_env.sh ./mk/local.env

vendor-fetch-s2n-tls:
	bash ./scripts/vendor_fetch.sh s2n-tls $(S2N_TLS_FETCH_URL) $(S2N_TLS_FETCH_REF) ./$(S2N_TLS_DIR) $(VENDOR_FORCE_FETCH)

vendor-fetch-libuv:
	bash ./scripts/vendor_fetch.sh libuv $(LIBUV_FETCH_URL) $(LIBUV_FETCH_REF) ./$(LIBUV_DIR) $(VENDOR_FORCE_FETCH)

vendor-fetch-llhttp:
	bash ./scripts/vendor_fetch.sh llhttp $(LLHTTP_FETCH_URL) $(LLHTTP_FETCH_REF) ./$(LLHTTP_DIR) $(VENDOR_FORCE_FETCH)

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

body-stress: vendor-build
	$(VENDORED_RUNTIME_ENV) bash ./scripts/body_stress.sh

asan-run: asan-build
	DYLD_LIBRARY_PATH=$(vendored_runtime_library_path) \
	LD_LIBRARY_PATH=$(vendored_runtime_library_path) \
	ASAN_OPTIONS=detect_leaks=1:abort_on_error=1:halt_on_error=1 \
	./_build/default/test/demo_server.exe

asan-https-smoke: asan-build
	bash ./scripts/asan_https_smoke.sh

test: vendor-build $(VENDORED_TEST_TARGETS)
	$(VENDORED_RUNTIME_ENV) $(DUNE) runtest -f

runtest: vendor-build $(VENDORED_TEST_TARGETS)
	$(VENDORED_RUNTIME_ENV) $(DUNE) runtest -f

clean:
	$(MAKE) vendor-clean
	$(DUNE) clean
