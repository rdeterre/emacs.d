#!/usr/bin/env bash

set -euo pipefail

code_dir="${HOME}/code"
mkdir -p "$code_dir"

ensure() {
  local repo="$1"
  local relative_path
  local destination

  case "$repo" in
    *://*) relative_path="${repo#*://}" ;;
    *:*)
      relative_path="${repo%%:*}/${repo#*:}"
      relative_path="${relative_path#*@}"
      ;;
    *)
      printf 'Unsupported repository URL: %s\n' "$repo" >&2
      return 1
      ;;
  esac

  relative_path="${relative_path%.git}"
  destination="${code_dir}/${relative_path}"

  if [[ -d "$destination" ]]; then
    printf 'Already present: %s\n' "$destination"
    return
  fi

  mkdir -p "$(dirname "$destination")"
  git clone "$repo" "$destination"
}

ensure git@github.com:anomalyco/opencode.git
