#!/bin/sh
#
# GT Warehouse Mobile -- deploy the Genie EJS shim into one or more skins.
#
#   gtwms/tools/deploy-shim.sh [skin ...]        default: pls Classic
#
# Each skin's start.html carries its OWN independent copy of the template
# snapshot, so every skin has to be regenerated whenever a template changes.
# Testing under one skin proves nothing about another.
#
# Run this from the repo root, after copying the .ejs/.css to the document root.
# It backs up each start.html before touching it, and verifies the JavaScript
# parses before uploading -- a syntax error here would break every screen in
# that skin, including the other projects that share the file.

set -e

#  Standalone assets FIRST. The snapshot written below is verified against LOCAL
#  source, so this script passes every check while the docroot still serves an old
#  gt-scan.js -- which is precisely what happened. Set GTWMS_SKIP_ASSETS=1 to
#  re-shim only.
if [ "${GTWMS_SKIP_ASSETS:-0}" != 1 ]; then
  "$(dirname "$0")/deploy-assets.sh" || exit 1
  echo
fi

SKINS="$*"
[ -z "$SKINS" ] && SKINS="pls Classic"

REMOTE_SKINS="/home/drusso/puidist/htdocs/profoundui/userdata/genie skins"
STAMP=$(date +%Y%m%d%H%M 2>/dev/null || echo manual)
WORK=$(mktemp -d)

for SKIN in $SKINS; do
  echo "=== $SKIN ==="
  LOCAL="$WORK/$SKIN-start.html"

  scp -q "dev:$REMOTE_SKINS/$SKIN/start.html" "$LOCAL"
  node gtwms/tools/gen-gt-shim.js "$LOCAL"

  # Extract and syntax-check the generated block before it goes anywhere near
  # the server. A broken start.html takes down every screen in the skin.
  node -e "
    const fs=require('fs');
    const s=fs.readFileSync('$LOCAL','utf8');
    const b=s.indexOf('<!-- GTWMS EJS shim BEGIN');
    const e=s.indexOf('<!-- GTWMS EJS shim END -->');
    if (b<0||e<0) { console.error('block missing'); process.exit(1); }
    const blk=s.slice(b,e);
    const js=blk.slice(blk.indexOf('<script type=\"text/javascript\">')+31, blk.lastIndexOf('</script>'));
    fs.writeFileSync('$WORK/check.js', js);
  "
  node --check "$WORK/check.js"
  echo "  shim JavaScript parses"

  ssh dev "cp \"$REMOTE_SKINS/$SKIN/start.html\" \"$REMOTE_SKINS/$SKIN/start.html.bak-gtwms-$STAMP\""
  scp -q "$LOCAL" "dev:$REMOTE_SKINS/$SKIN/start.html"
  echo "  deployed (backup: start.html.bak-gtwms-$STAMP)"
done

rm -rf "$WORK"

echo
echo "Verifying against the deployed files..."
node gtwms/tools/test-gt-shim.js "$(echo $SKINS | tr ' ' ',')"
