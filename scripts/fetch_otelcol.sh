#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "usage: $0 <version> <destination-dir>" >&2
  exit 2
fi

version=$1
destination_dir=$2

case "$(uname -s)" in
  Linux) os=linux ;;
  Darwin) os=darwin ;;
  *)
    echo "unsupported otelcol operating system: $(uname -s)" >&2
    exit 1
    ;;
esac

case "$(uname -m)" in
  x86_64 | amd64) arch=amd64 ;;
  arm64 | aarch64) arch=arm64 ;;
  *)
    echo "unsupported otelcol architecture: $(uname -m)" >&2
    exit 1
    ;;
esac

archive="otelcol_${version}_${os}_${arch}.tar.gz"
release_url="https://github.com/open-telemetry/opentelemetry-collector-releases/releases/download/v${version}"
temporary_dir=$(mktemp -d)

cleanup() {
  rm -rf "$temporary_dir"
}
trap cleanup EXIT

curl --fail --location --retry 3 --output "$temporary_dir/$archive" \
  "$release_url/$archive"
curl --fail --location --retry 3 --output "$temporary_dir/$archive.sha256" \
  "$release_url/$archive.sha256"

expected_checksum=$(awk '{ print $1; exit }' "$temporary_dir/$archive.sha256")
if command -v sha256sum >/dev/null 2>&1; then
  actual_checksum=$(sha256sum "$temporary_dir/$archive" | awk '{ print $1 }')
else
  actual_checksum=$(shasum -a 256 "$temporary_dir/$archive" | awk '{ print $1 }')
fi

if [[ -z "$expected_checksum" || "$actual_checksum" != "$expected_checksum" ]]; then
  echo "otelcol archive checksum verification failed" >&2
  exit 1
fi

tar -xzf "$temporary_dir/$archive" -C "$temporary_dir"
if [[ ! -x "$temporary_dir/otelcol" ]]; then
  echo "otelcol executable was not present in $archive" >&2
  exit 1
fi

mkdir -p "$destination_dir"
install -m 0755 "$temporary_dir/otelcol" "$destination_dir/otelcol.new"
mv "$destination_dir/otelcol.new" "$destination_dir/otelcol"

