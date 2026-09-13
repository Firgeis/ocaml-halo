#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <version> <output-dir>" >&2
  exit 2
fi

version=${1#v}
output_dir=$2
script_dir=$(cd "$(dirname "$0")" && pwd)
repo_root=$(cd "${script_dir}/.." && pwd)
package_name="ocaml-halo-${version}"
staging_root=$(mktemp -d)
staging_dir="${staging_root}/${package_name}"

cleanup() {
  rm -rf "${staging_root}"
}

trap cleanup EXIT

dir_has_downloaded_contents() {
  local dir=$1

  find "${dir}" -mindepth 1 -maxdepth 1 \
    ! -name .gitignore \
    ! -name .gitkeep \
    ! -name .vendor-fetch-ref \
    -print -quit | grep -q .
}

require_downloaded_vendor_dir() {
  local dir=$1
  local name=$2

  if [[ ! -d "${dir}" ]] || ! dir_has_downloaded_contents "${dir}"; then
    echo "Missing vendored source tree for ${name} in ${dir}. Run make vendor-fetch first." >&2
    exit 1
  fi
}

require_downloaded_vendor_dir "${repo_root}/vendor/s2n-tls" "s2n-tls"
require_downloaded_vendor_dir "${repo_root}/vendor/libuv" "libuv"
require_downloaded_vendor_dir "${repo_root}/vendor/llhttp" "llhttp"

mkdir -p "${output_dir}"

mkdir -p "${staging_dir}"

rsync -a \
  "${repo_root}/dune" \
  "${repo_root}/dune-project" \
  "${repo_root}/ocaml-halo.opam" \
  "${repo_root}/README.md" \
  "${repo_root}/Makefile" \
  "${repo_root}/LICENSE" \
  "${staging_dir}/"

rsync -a \
  "${repo_root}/lib/" \
  "${staging_dir}/lib/"

rsync -a \
  "${repo_root}/c/" \
  "${staging_dir}/c/"

mkdir -p "${staging_dir}/scripts"
rsync -a \
  "${repo_root}/scripts/build_s2n_artifact.sh" \
  "${repo_root}/scripts/build_libuv_artifact.sh" \
  "${repo_root}/scripts/resolve_libcrypto_root.sh" \
  "${staging_dir}/scripts/"

mkdir -p "${staging_dir}/vendor"
rsync -a \
  --exclude 'bindings/' \
  --exclude 'tests/' \
  --exclude 'docs/' \
  --exclude 'doc/' \
  --exclude 'examples/' \
  --exclude 'bench/' \
  --exclude 'benchmark/' \
  --exclude 'benchmarks/' \
  --exclude '.github/' \
  --exclude '.gitlab/' \
  --exclude 'img/' \
  --exclude 'images/' \
  --exclude 'cmake-toolchains/' \
  --exclude 'm4/' \
  "${repo_root}/vendor/s2n-tls/" \
  "${staging_dir}/vendor/s2n-tls/"

mkdir -p "${staging_dir}/vendor/s2n-tls/tests"
rsync -a \
  "${repo_root}/vendor/s2n-tls/tests/features/" \
  "${staging_dir}/vendor/s2n-tls/tests/features/"

rsync -a \
  --exclude 'test/' \
  --exclude 'tests/' \
  --exclude 'docs/' \
  --exclude 'doc/' \
  --exclude 'examples/' \
  --exclude 'bench/' \
  --exclude 'benchmark/' \
  --exclude 'benchmarks/' \
  --exclude '.github/' \
  --exclude '.gitlab/' \
  --exclude 'img/' \
  --exclude 'images/' \
  --exclude 'cmake-toolchains/' \
  --exclude 'm4/' \
  "${repo_root}/vendor/libuv/" \
  "${staging_dir}/vendor/libuv/"

rsync -a \
  --exclude 'test/' \
  --exclude 'tests/' \
  --exclude 'docs/' \
  --exclude 'doc/' \
  --exclude 'examples/' \
  --exclude 'bench/' \
  --exclude 'benchmark/' \
  --exclude 'benchmarks/' \
  --exclude '.github/' \
  --exclude '.gitlab/' \
  --exclude 'img/' \
  --exclude 'images/' \
  --exclude 'cmake-toolchains/' \
  --exclude 'm4/' \
  "${repo_root}/vendor/llhttp/" \
  "${staging_dir}/vendor/llhttp/"

find "${staging_dir}" -type f \
  \( -name '*.o' -o -name '*.a' -o -name '*.so' -o -name '*.dylib' -o -name '*.bc' \) \
  -delete

find "${staging_dir}/vendor/s2n-tls/bin" -maxdepth 1 -type f \
  ! -name '*.c' \
  ! -name '*.h' \
  ! -name 'Makefile' \
  -delete

tar -czf "${output_dir}/${package_name}.tar.gz" -C "${staging_root}" "${package_name}"

printf 'Wrote %s\n' "${output_dir}/${package_name}.tar.gz"
