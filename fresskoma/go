#!/bin/bash
set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

function error {
    echo -e >&2 "\033[31m${1}\033[0m";
    exit 1;
}

function notice {
    echo -e >&2 "\033[33m${1}\033[0m";
}

function ensure_env {
    command -v cargo >/dev/null 2>&1 || error "Please install cargo"
    command -v rustc >/dev/null 2>&1 || error "Please install rustc"
}

function cmd_build {
  cargo build
}

function cmd_test {
    cd "$SCRIPT_DIR"
    cmd_build

    error=0

    tempfile="$(mktemp)"
    for file in examples/*.lj1; do
      echo -n $file
      if target/debug/langjam0001 "$file" 1>"$tempfile" 2>&1; then
        echo " ✓"
      else
        echo " x"
        error=1
        cat "$tempfile"
      fi
    done

    exit $error
}

function cmd_usage {
    echo "Usage";
}

ensure_env

command=""
if (( $# > 0 )); then
    command="${1}"
    shift
fi

case "${command}" in
    test) cmd_test "$@" ;;
    *) cmd_usage
esac
