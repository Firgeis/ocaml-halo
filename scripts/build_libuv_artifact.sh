#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <static|shared> <target-path>" >&2
  exit 2
fi

artifact=$1
target_path=$2
script_dir=$(cd "$(dirname "$0")" && pwd)
repo_root=$(cd "${script_dir}/.." && pwd)
libuv_dir="${repo_root}/vendor/libuv"
build_root=

cleanup() {
  if [[ -n "${build_root}" && -d "${build_root}" ]]; then
    rm -rf "${build_root}"
  fi
}

trap cleanup EXIT

if ! command -v cmake >/dev/null 2>&1; then
  echo "cmake is required to build the vendored libuv source" >&2
  exit 1
fi

build_root=$(mktemp -d "${PWD}/.libuv-build.XXXXXX")
cp -R "${libuv_dir}" "${build_root}/libuv"
chmod -R u+w "${build_root}/libuv"

cmake -S "${build_root}/libuv" -B "${build_root}/build" \
  -DBUILD_TESTING=OFF \
  -DLIBUV_BUILD_SHARED=ON \
  -DCMAKE_POSITION_INDEPENDENT_CODE=ON \
  -DCMAKE_BUILD_TYPE=Release

case "${artifact}" in
  static)
    cmake --build "${build_root}/build" --target uv_a
    cp "${build_root}/build/libuv.a" "${target_path}"
    ;;
  shared)
    cmake --build "${build_root}/build" --target uv
    if [[ -f "${build_root}/build/libuv.so" ]]; then
      cp "${build_root}/build/libuv.so" "${target_path}"
    elif [[ -f "${build_root}/build/libuv.dylib" ]]; then
      cp "${build_root}/build/libuv.dylib" "${target_path}"
    else
      echo "Could not find the built shared libuv library" >&2
      exit 1
    fi
    ;;
  *)
    echo "unknown libuv artifact kind: ${artifact}" >&2
    exit 2
    ;;
esac
