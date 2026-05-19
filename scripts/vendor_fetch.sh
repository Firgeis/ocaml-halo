#!/usr/bin/env bash

set -euo pipefail

if [[ $# -lt 4 || $# -gt 5 ]]; then
  echo "usage: $0 <name> <url> <ref> <dest-dir> [force]" >&2
  exit 2
fi

name=$1
url=$2
ref=$3
dest_dir=$4
force=${5:-0}
marker_file=

tmp_dir=
archive_path=

cleanup() {
  if [[ -n "${archive_path}" && -f "${archive_path}" ]]; then
    rm -f "${archive_path}"
  fi
  if [[ -n "${tmp_dir}" && -d "${tmp_dir}" ]]; then
    rm -rf "${tmp_dir}"
  fi
}

trap cleanup EXIT

marker_payload() {
  printf '%s\n%s\n' "$ref" "$url"
}

dir_has_downloaded_contents() {
  find "$dest_dir" -mindepth 1 -maxdepth 1 \
    ! -name .gitignore \
    ! -name .gitkeep \
    ! -name .vendor-fetch-ref \
    -print -quit | grep -q .
}

reset_dest_dir() {
  if [[ ! -d "$dest_dir" ]]; then
    mkdir -p "$dest_dir"
    return
  fi

  find "$dest_dir" -mindepth 1 -maxdepth 1 \
    ! -name .gitignore \
    ! -name .gitkeep \
    -exec rm -rf {} +
}

download() {
  local source_url=$1
  local output_path=$2

  if command -v curl >/dev/null 2>&1; then
    curl -fL "$source_url" -o "$output_path"
    return
  fi

  if command -v wget >/dev/null 2>&1; then
    wget -O "$output_path" "$source_url"
    return
  fi

  echo "vendor-fetch requires curl or wget" >&2
  exit 1
}

marker_file="${dest_dir}/.vendor-fetch-ref"

if [[ -d "$dest_dir" && "$force" != "1" ]]; then
  if [[ -f "$marker_file" ]] && dir_has_downloaded_contents && cmp -s <(marker_payload) "$marker_file"; then
    echo "Skipping ${name}: ${dest_dir} already matches ${ref}"
    exit 0
  fi

  if dir_has_downloaded_contents; then
    echo "Refusing to overwrite ${dest_dir}; use force to refresh ${name}" >&2
    exit 1
  fi
fi

mkdir -p "$(dirname "$dest_dir")"
reset_dest_dir
tmp_dir=$(mktemp -d)
archive_path="${tmp_dir}/${name}.tar.gz"

echo "Fetching ${name} (${ref})"
download "$url" "$archive_path"
tar -xzf "$archive_path" -C "$tmp_dir"

extracted_dir=$(find "$tmp_dir" -mindepth 1 -maxdepth 1 -type d | head -n 1)

if [[ -z "${extracted_dir}" ]]; then
  echo "Failed to unpack ${name} from ${url}" >&2
  exit 1
fi

find "$extracted_dir" -mindepth 1 -maxdepth 1 -exec mv {} "$dest_dir"/ \;
marker_payload > "$marker_file"
echo "Fetched ${name} into ${dest_dir}"
