#!/usr/bin/env bash
# Pre-flight the Fletcher EJS screens (or any RDF JSON passed as an argument).
set -euo pipefail
cd "$(dirname "$0")/.."
if [ $# -gt 0 ]; then exec python3 tools/ejs-preflight.py "$@"; fi
exec python3 tools/ejs-preflight.py cfdemo/qddssrc/*eo.json
