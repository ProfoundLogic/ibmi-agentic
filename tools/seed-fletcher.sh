#!/usr/bin/env bash
# Load the J.H. Fletcher demo seed data into a target library.
#
#   tools/seed-fletcher.sh <LIBRARY>
#
# Idempotent: every table is DELETEd before insert, so this is safe to re-run.
# Re-run it after any codermake rebuild that recreates a physical file - CRTPF
# produces an empty file and takes the seeded data with it.
set -euo pipefail

LIB="${1:-${IBMI_BUILD_LIBRARY:-}}"
if [ -z "$LIB" ]; then
  echo "usage: $0 <LIBRARY>   (or set IBMI_BUILD_LIBRARY)" >&2
  exit 2
fi
LIB="$(echo "$LIB" | tr '[:lower:]' '[:upper:]')"
SQL="$(dirname "$0")/../cfdemo/seed/flseed.sql"
[ -f "$SQL" ] || { echo "seed file not found: $SQL" >&2; exit 2; }

echo "Seeding Fletcher demo data into $LIB"
python3 "$(dirname "$0")/seed-fletcher.py" "$LIB" "$SQL"
