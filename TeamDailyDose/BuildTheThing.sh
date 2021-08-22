#!/usr/bin/env bash
set -euo pipefail

[[ ! -d Build ]] && mkdir Build
cd Build
cmake .. -GNinja
cmake --build .
