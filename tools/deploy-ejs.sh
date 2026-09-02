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

# Discover the asset directories rather than listing them. The list used to be
# hardcoded in two places, and adding an app to only one of them is exactly how
# the A7 duplicate-review assets (ui/fldup) went undeployed while the script
# still reported success.
DIRS=()
for d in "$SRC"/*/; do
  [ -d "$d" ] || continue
  DIRS+=("$(basename "$d")")
done
if [ ${#DIRS[@]} -eq 0 ]; then
  echo "no asset directories under $SRC" >&2
  exit 1
fi

echo
echo "Deploying $SRC -> dev:$DEST"
echo "  directories: ${DIRS[*]}"
for d in "${DIRS[@]}"; do
  ssh dev "mkdir -p '$DEST/$d'"
  scp -q "$SRC/$d"/* "dev:$DEST/$d/"
  echo "  $d: $(ls "$SRC/$d" | wc -l) file(s)"
done

echo
echo "Verifying on the server"
fails=0
for d in "${DIRS[@]}"; do
  for f in "$SRC/$d"/*; do
    n="$(basename "$f")"
    if ssh dev "test -s '$DEST/$d/$n'"; then
      echo "  ok   $d/$n"
    else
      echo "  FAIL $d/$n"
      fails=$((fails + 1))
    fi
  done
done

echo
if [ "$fails" -gt 0 ]; then
  echo "$fails file(s) failed to verify on the server" >&2
  exit 1
fi
echo "all files verified"
