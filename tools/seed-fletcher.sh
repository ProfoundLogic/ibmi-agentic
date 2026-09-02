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
SEEDS=("$(dirname "$0")/../cfdemo/seed/flseed.sql"
       "$(dirname "$0")/../cfdemo/seed/flsched.sql")
for SQL in "${SEEDS[@]}"; do
  [ -f "$SQL" ] || { echo "seed file not found: $SQL" >&2; exit 2; }
done

# Optionally load just one section: tools/seed-fletcher.sh FLTDEMO flsched
if [ -n "${2:-}" ]; then
  SEEDS=("$(dirname "$0")/../cfdemo/seed/$2.sql")
  [ -f "${SEEDS[0]}" ] || { echo "seed file not found: ${SEEDS[0]}" >&2; exit 2; }
fi

echo "Seeding Fletcher demo data into $LIB"
for SQL in "${SEEDS[@]}"; do
  echo "  $(basename "$SQL")"
  python3 "$(dirname "$0")/seed-fletcher.py" "$LIB" "$SQL"
done
