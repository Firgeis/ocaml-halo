#!/usr/bin/env bash

set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <vendor-dir>" >&2
  exit 2
fi

vendor_dir=$1

if [[ ! -d "$vendor_dir" ]]; then
  exit 0
fi

find "$vendor_dir" -mindepth 1 -maxdepth 1 \
  ! -name .gitignore \
  ! -name .gitkeep \
  -exec rm -rf {} +
