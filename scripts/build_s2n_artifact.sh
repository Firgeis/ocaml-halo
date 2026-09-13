#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <lib|shared|s2nc> <target-path>" >&2
  exit 2
fi

artifact=$1
target_path=$2
script_dir=$(cd "$(dirname "$0")" && pwd)
repo_root=$(cd "${script_dir}/.." && pwd)
s2n_dir="${repo_root}/vendor/s2n-tls"
libcrypto_root=$(bash "${script_dir}/resolve_libcrypto_root.sh")
build_root=

cleanup() {
  if [[ -n "${build_root}" && -d "${build_root}" ]]; then
    rm -rf "${build_root}"
  fi
}

trap cleanup EXIT

build_root=$(mktemp -d "${PWD}/.s2n-build.XXXXXX")
cp -R "${s2n_dir}" "${build_root}/s2n-tls"
staged_s2n_dir="${build_root}/s2n-tls"
chmod -R u+w "${staged_s2n_dir}"

build_target() {
  local make_target=$1

  # Apple Clang 18 diagnoses legacy K&R-style declarations in this vendored
  # s2n release. Keep the compatibility exception isolated to that toolchain;
  # halo's own sources continue to build with their normal warnings.
  if [[ "$(uname -s)" == "Darwin" ]]; then
    make -C "${staged_s2n_dir}" "${make_target}" \
      LIBCRYPTO_ROOT="${libcrypto_root}" \
      CFLAGS+=" -Wno-error=strict-prototypes"
  else
    make -C "${staged_s2n_dir}" "${make_target}" LIBCRYPTO_ROOT="${libcrypto_root}"
  fi
}

copy_output() {
  local source_path=$1

  cp "${source_path}" "${target_path}"
}

case "${artifact}" in
  lib)
    build_target libs
    copy_output "${staged_s2n_dir}/lib/libs2n.a"
    ;;
  shared)
    build_target libs
    if [[ -f "${staged_s2n_dir}/lib/libs2n.so" ]]; then
      copy_output "${staged_s2n_dir}/lib/libs2n.so"
    elif [[ -f "${staged_s2n_dir}/lib/libs2n.dylib" ]]; then
      copy_output "${staged_s2n_dir}/lib/libs2n.dylib"
    else
      echo "Could not find a built shared s2n library" >&2
      exit 1
    fi
    ;;
  s2nc)
    build_target bin
    copy_output "${staged_s2n_dir}/bin/s2nc"
    ;;
  *)
    echo "unknown artifact kind: ${artifact}" >&2
    exit 2
    ;;
esac
