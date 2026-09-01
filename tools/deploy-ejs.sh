#!/usr/bin/env bash
# Deploy the repo's EJS screen assets to a Profound UI document root.
#
#   tools/deploy-ejs.sh [DOCROOT]
#
# codermake does NOT deploy htdocs/ - it only builds IBM i objects. Forgetting
# this step produces a blank screen and sends people debugging the RPG, which
# is the wrong end entirely. Run this after any .ejs / .css / .js edit.
set -euo pipefail
cd "$(dirname "$0")/.."

DOCROOT="${1:-/home/drusso/puidist/htdocs}"
SRC="htdocs/profoundui/userdata/ui"
DEST="$DOCROOT/profoundui/userdata/ui"

echo "Pre-flight before deploying"
tools/ejs-preflight.sh

echo
echo "Deploying $SRC -> dev:$DEST"
for d in fletcher flfleet flparts; do
  [ -d "$SRC/$d" ] || continue
  ssh dev "mkdir -p '$DEST/$d'"
  scp -q "$SRC/$d"/* "dev:$DEST/$d/"
  echo "  $d: $(ls "$SRC/$d" | wc -l) file(s)"
done

echo
echo "Verifying on the server"
for d in fletcher flfleet flparts; do
  [ -d "$SRC/$d" ] || continue
  for f in "$SRC/$d"/*; do
    n="$(basename "$f")"
    if ssh dev "test -s '$DEST/$d/$n'"; then echo "  ok   $d/$n"; else echo "  FAIL $d/$n"; fi
  done
done
