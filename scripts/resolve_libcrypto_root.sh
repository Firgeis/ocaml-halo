#!/usr/bin/env bash

set -euo pipefail

detect_from_pkg_config() {
  local pkg

  if ! command -v pkg-config >/dev/null 2>&1; then
    return 1
  fi

  for pkg in openssl libcrypto libssl; do
    if pkg-config --exists "$pkg" 2>/dev/null; then
      pkg-config --variable=prefix "$pkg" 2>/dev/null
      return 0
    fi
  done

  return 1
}

detect_from_brew() {
  local formula

  if ! command -v brew >/dev/null 2>&1; then
    return 1
  fi

  for formula in openssl@3 openssl openssl@1.1; do
    if brew --prefix "$formula" >/dev/null 2>&1; then
      brew --prefix "$formula"
      return 0
    fi
  done

  return 1
}

detect_from_common_paths() {
  local candidate

  for candidate in \
    /opt/homebrew/opt/openssl@3 \
    /opt/homebrew/opt/openssl \
    /usr/local/opt/openssl@3 \
    /usr/local/opt/openssl \
    /usr/local/opt/openssl@1.1 \
    /usr
  do
    if [[ -f "$candidate/include/openssl/ssl.h" ]]; then
      printf '%s\n' "$candidate"
      return 0
    fi
  done

  return 1
}

if [[ -n "${LIBCRYPTO_ROOT:-}" ]]; then
  printf '%s\n' "$LIBCRYPTO_ROOT"
elif detect_from_pkg_config; then
  :
elif detect_from_brew; then
  :
elif detect_from_common_paths; then
  :
else
  printf '%s\n' /usr
fi
