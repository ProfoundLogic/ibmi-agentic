#!/bin/bash
# Render a captured Profound UI Rich Display screen locally, with the real runtime.
#
#   ./render.sh <screen.json> <out.png> [--as-hosted]
#
# Why this exists: genie_html.sh renders server side and answers the agent 401, so
# there is no way to actually SEE a Rich Display screen while working on it.  This
# runs Profound UI's own runtime.js against a screen captured by genie_get.sh, in
# headless Chromium, and screenshots the result.
#
# Default: links /profoundui/proddata/css/profoundui.css, i.e. a correctly hosted
# page.  With --as-hosted it instead links only the Genie pls skin on a black
# terminal body, reproducing the hosting page these screens actually run on - use
# that to prove a screen is self-sufficient.
set -e
cd "$(dirname "$0")"
SCREEN="${1:?usage: render.sh <screen.json> <out.png> [--as-hosted]}"
OUT="${2:?usage: render.sh <screen.json> <out.png> [--as-hosted]}"
MODE="${3:-}"
PORT=8777
ROOT=$(mktemp -d)
REPO=$(cd ../../.. && pwd)
PUI="${IBMI_PUI_SERVER:?IBMI_PUI_SERVER is not set}"

mkdir -p "$ROOT/profoundui/proddata/css" "$ROOT/profoundui/proddata/js" "$ROOT/profoundui/proddata/fonts"
curl -sk "$PUI/profoundui/proddata/js/runtime.js"        -o "$ROOT/profoundui/proddata/js/runtime.js"
curl -sk "$PUI/profoundui/proddata/css/profoundui.css"   -o "$ROOT/profoundui/proddata/css/profoundui.css"
for f in MaterialIcons-Regular.woff2 MaterialIcons-Regular.woff MaterialIcons-Regular.ttf \
         fa-solid-900.woff2 fa-regular-400.woff2 fa-brands-400.woff2; do
  curl -sk "$PUI/profoundui/proddata/fonts/$f" -o "$ROOT/profoundui/proddata/fonts/$f" || true
done
# the repo's own userdata assets take precedence, exactly as the task proxy does
cp -r "$REPO/htdocs/profoundui/userdata" "$ROOT/profoundui/" 2>/dev/null || true
mkdir -p "$ROOT/profoundui/userdata/genie skins/pls" "$ROOT/profoundui/userdata/custom/css" "$ROOT/profoundui/userdata/custom/fonts"
curl -sk "$PUI/profoundui/userdata/genie%20skins/pls/pls.css" -o "$ROOT/profoundui/userdata/genie skins/pls/pls.css" || true
curl -sk "$PUI/profoundui/userdata/custom/css/pls.css"        -o "$ROOT/profoundui/userdata/custom/css/pls.css" || true
for f in material_icons.woff2 arimo-v16-latin-regular.woff2 RobotoMono-Regular.ttf; do
  curl -sk "$PUI/profoundui/userdata/custom/fonts/$f" -o "$ROOT/profoundui/userdata/custom/fonts/$f" || true
done

cp "$SCREEN" "$ROOT/screen.json"
BASE="<link href='/profoundui/proddata/css/profoundui.css' rel='stylesheet'>"
SKIN="<link href='/profoundui/userdata/genie skins/pls/pls.css' rel='stylesheet'><link href='/profoundui/userdata/custom/css/pls.css' rel='stylesheet'>"
case "$MODE" in
  --as-hosted)
    # What these screens actually get on this instance: the Genie pls skin in the
    # page head, Genie's .genie-form--screen wrapper, and profoundui.css arriving
    # LATER because the screen names it in `external css` and the runtime appends
    # it at render time.  Do not link $BASE here - putting it ahead of the skin
    # lets the skin win on order and, among other things, paints input fields
    # with the dark theme background, which does not happen in a real browser.
    HEAD="$SKIN"; CLASS=""; STYLE=""; WRAP="genie-form--screen" ;;
  --no-base)
    # The broken state before the screens named profoundui.css themselves.
    HEAD="$SKIN"; CLASS="monoSpace"; STYLE="background:#000;color:#fff"; WRAP="genie-form--screen" ;;
  *)
    # A cleanly hosted Profound UI page, with no Genie skin at all.
    HEAD="$BASE"; CLASS=""; STYLE=""; WRAP="" ;;
esac
# EXTRA_CSS lets you try a stylesheet before wiring it into the screens
for extra in $EXTRA_CSS; do HEAD="$HEAD<link href='$extra' rel='stylesheet'>"; done
sed -e "s|<!-- PUI_HEAD -->|$HEAD<script src='/profoundui/proddata/js/runtime.js'></script>|" \
    -e "s|BODY_CLASS|$CLASS|" -e "s|BODY_STYLE|$STYLE|" \
    -e "s|WRAPPER_CLASS|$WRAP|" render.html > "$ROOT/render.html"

# A server left over from a previous run keeps the port and serves a temp dir
# that has since been deleted, which shows up as a 404 page in the screenshot.
pkill -f "http.server $PORT" 2>/dev/null || true
sleep 1
( cd "$ROOT" && exec python3 -m http.server $PORT >/dev/null 2>&1 ) &
SRV=$!
for _ in 1 2 3 4 5 6 7 8 9 10; do
  if curl -s -m 1 -o /dev/null -f "http://localhost:$PORT/render.html"; then break; fi
  sleep 0.4
done
PUI_COLOR_SCHEME="${PUI_COLOR_SCHEME:-light}" node "${RENDER_SCRIPT:-shot.js}" "$OUT" "http://localhost:$PORT/render.html" || true
kill $SRV 2>/dev/null || true
pkill -f "http.server $PORT" 2>/dev/null || true
rm -rf "$ROOT"
echo "wrote $OUT"
