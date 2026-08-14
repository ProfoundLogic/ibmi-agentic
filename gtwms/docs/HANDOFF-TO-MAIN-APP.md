# Handoff: what makes the main app work, and what to do every new task

Written 2026-08-14, the day option 4 and option 5 both worked on the iPad.

Paste this into the main-application task. It is short on purpose.

## The headline: no camera code changed

Option 4 started working without a single line of the application changing. The
fix had already shipped; the device had never loaded it.

Diffing the whole repo against the state it was in when the camera was last
reported broken, the only things that differ are the new option-5 files, the
sign-on menu, `Rules.mk`, one entry in `gen-gt-shim.js`, a bug in
`test-gt-shim.js`, `package.json`, and docs. **`gt-scan.js` is byte-for-byte
identical.** So are all sixteen display files and every application program.

```
gt-scan.js: patch-restored vs deployed now  ->  IDENTICAL, not touched
```

The real fix landed in `20260811k`, in the previous task: **the barcode decoder
travels inlined in the page as base64, not as a URL the browser has to fetch.**
It was deployed and verified server-side, and the access log showed the device
never loaded anything newer than `20260811f`. Two builds behind, twice.

## What today's access log proves

```
09:50:00  GET /profoundui/auth/genie?skin=classic   200  2,770,795     <- the whole shim
10:04:53  GET /profoundui/auth/genie                200  2,595,523     <- again, pls skin
gt-zxing.min.js fetches from the device today ....  0
simtest.ejs     fetches from the device today ....  0
```

Both screens worked while fetching neither the decoder nor the template. Both came
out of the base64 inside `start.html`. That ~2.6 MB page load is the delivery
mechanism now — one page load carries eleven templates, their CSS, every screen's
JavaScript, and 354 KB of decoder.

**So "it works now" means: the device finally did a fresh page load.** Nothing more
mysterious than that.

One refinement, stated honestly. `simtest.js` *was* fetched over the wire today
(`?v=20260811k`, 200, 23,666 bytes) by Profound UI's own screen-JS loader. So the
earlier blanket conclusion — "a dynamically appended `<script src>` never fires on
this device" — is **too strong**. What is certain is that the inlined route works
and the decoder is no longer fetched at all. Whether the runtime-appended script
tag is still broken is unsettled, and today's log cannot settle it, because
`gt-scan.js` now prefers the inlined copy and never reaches the fetch. Option 5's
`routes=` field measures it directly — read it off the screen once and the question
is closed.

## The per-task checklist

This is the part that actually costs time. None of it is a code fix; all of it is
delivery, and skipping any step makes a working application look broken.

### 1. Check the prior work still exists — before debugging anything

A fresh container resets the repo to the last **commit**. Everything uncommitted
is gone, and on 2026-08-14 that was five screens and seven rounds of camera work.

```bash
ls gtwms/qddssrc/            # expect 16 display files, not 6
grep -o "VERSION = '[0-9a-z]*'" htdocs/profoundui/userdata/ui/gtcommon/gt-scan.js
```

If it is short, recover from CoderFlow's per-turn patches — **not** from the IBM i:

```bash
ls /task-output/patches/                       # highest turn-N is the newest
git apply --check /task-output/patches/turn-51/ibmi-agentic.patch   # verify first
git apply         /task-output/patches/turn-51/ibmi-agentic.patch
```

The patch's checkpoint commit may not exist locally and it still applies cleanly to
the current HEAD. Binaries are excluded, so regenerate images/PDFs if needed.

**Then push the branch.** This whole step exists because the work was never
committed.

### 2. `npm install`

`node_modules` is gitignored. Without it `deploy-shim.sh` uploads the shim and
*then* dies with `MODULE_NOT_FOUND` on its verification step — which reads exactly
like a broken deploy.

### 3. Rebuild the sign-on menu shadow into THIS task's library

The task library changes every task. Option 4 does not appear at all until this is
done, and codermake stamps record *that* a target was built, not *which library* it
went to — so a stale stamp makes it a silent no-op.

```bash
rm -f build/menu.file build/menu.msgf build/menu.menu
codermake menu.menu          # default IBMI_BUILD_LIBRARY = the task library
```

Never `rm -f build/*` wholesale: `custp.file` would be rebuilt as an EMPTY CUSTP in
the task library, shadowing the populated base copy.

### 4. Application objects go to TIGERPOC, as an override

```bash
IBMI_BUILD_LIBRARY=TIGERPOC codermake <target>
```

Never a bare `codermake` with no target from this repo — it builds every other
project's objects into TIGERPOC too.

### 4b. After rebuilding programs, check the images

Rebuilding programs can re-run the seed (`gtseed.file` is an order-only
prerequisite), and that drops and recreates GTIMAGE. `image_id` is
`GENERATED ALWAYS AS IDENTITY`, so the ids move to a new block while the exported
`.jpg` files keep the old names -- **every image on every screen 404s** and no
build step reports it.

```bash
node gtwms/tools/test-images.js          # AFTER the rebuild, always
node gtwms/tools/export-images.js        # only if the ids 404
scp /tmp/gtimg-export/*.jpg dev:/home/drusso/puidist/htdocs/profoundui/userdata/ui/gtimg/
```

### 5. Deploy the web assets AND re-snapshot every skin

```bash
gtwms/tools/deploy-assets.sh     # htdocs -> Profound UI docroot
gtwms/tools/deploy-shim.sh       # base64 snapshot into pls AND Classic
```

**Step 5b is not optional and is the single most common cause of "my change did
nothing".** The shim holds a *copy*; uploading the asset alone changes nothing on
screen. Each skin holds its own copy, so testing one proves nothing about the
other.

### 6. One cache-defeating page load on the device

```
https://idev.profoundlogic.com:8103/profoundui/auth/genie?cb=<anything-new>
```

Then check the version chip reads the current stamp. A Genie session keeps talking
to the server by POST without re-requesting the page, and on iOS a plain reload
often comes entirely from cache — which is how a device sat two builds behind while
being "refreshed".

## Verify from the server, not from the device

This is the lesson that cost the most. Three of seven rounds were delivery
failures diagnosed as code failures, because delivery was checked by looking at
what the server had **on disk** instead of what the device had **fetched**.

```bash
# what did the device actually download, and which build?
ssh dev "grep -hE 'gt-scan|gt-zxing|gt-version' /www/drpuidev/logs/access_log.Q1YYMMDD00 | tail -20"

# which clients are real devices vs the screen-capture service?
#   the capture service also POSTs screen_visualization=1 -- do not mistake it for the iPad
ssh dev "grep -h 'screen_visualization' /www/drpuidev/logs/access_log.Q1YYMMDD00 | awk '{print \$4}' | sort -u"

# every page load, with byte counts -- a ~2.6 MB auth/genie is a full shim delivery
ssh dev "grep -hE 'start\.html|auth/genie' /www/drpuidev/logs/access_log.Q1YYMMDD00 | tail"
```

The log filename is `access_log.Q1YYMMDD00` — for 2026-08-14, `Q126081400`. It is
world-readable and records the full query string, which is why option 5 can report
device state just by fetching a URL.

## The skin owns the scroll

Fixed 2026-08-14, and worth knowing before touching any layout. Every box the skin
puts around us is pinned: on mobile `body` is `position: fixed`, and
`.genie-container` is `position: fixed; height: 100%` with `overflow: auto` only
inside `@supports (-moz-appearance: none)` -- effectively Firefox, though Chromium
honours it too. So on WebKit **nothing scrolls at all**, and the container also
hangs 38px below the viewport because it has no `top` and the skin's markup
indentation leaves two whitespace line boxes above it.

**The two skins are different shapes.** Classic's entire body is
`<div id="5250">` -- nothing pinned, document scrolls, one finger works. Every shell
rule is therefore qualified by `.genie-container`, which only the pls-family skins
have. Unscoped, those rules clipped the Classic page itself and Android needed a
pinch and two fingers to move anything.

Our screens are therefore an app shell, scoped to `html.gt-screen`: header pinned,
footer pinned, only `.gt-main` scrolls. Two consequences for new screens:

- **every screen needs `<main class="gt-main">` and `<footer class="gt-actions">`.**
  Without the main, `.gt-app { overflow: hidden }` clips it with no scrollbar.
  `test-skin-scroll.js` checks every screen for both.
- **never add `html`, `body` or `.genie-*` rules unscoped.** The theme has none,
  and the test fails if any appear.

Run `node gtwms/tools/test-skin-scroll.js` after any layout change. It fetches the
real deployed skin CSS and tests the webkit-like case, which is the only one that
reproduces the device.

## Three invariants not to regress

1. **The decoder must never be fetched at runtime.** It lives inlined in the shim.
   Guarded by `test-camera.js` ("a barcode decoded with every decoder fetch
   blocked") and by `test-simtest.js`. If you add a screen that scans, it gets the
   decoder from `window.__gtLoadZxing()`, never from a script tag.

2. **Wait for real video dimensions before decoding.** `video.play()` resolving
   does not mean `videoWidth > 0`, and a 0×0 frame makes every decoder fail
   silently for ever — a live picture that never reads.

3. **Crop to the reticle.** Full-frame analysis turns a barcode occupying a fifth
   of the picture into a few pixels per bar.

And one about testing, which is why these took seven rounds: **a test granted a
capability the device withholds cannot fail the way the device fails.** Chromium's
fake camera contains no barcode; a decoder fetch the harness allows and the device
drops; a snapshot compared against local source rather than against what was
served. All three passed while the device could not scan.

## Known false alarms

- **A QPGMR `AUTOBUILD` job ends the PUI dev instance at 22:00.** A verified
  endpoint can vanish mid-task and it is not your change.
- **Timeouts to `64.56.97.x` / `172.16.20.0/24`** are NAT hairpin from inside
  `profoundnet`, not proof a server is down.
- **`SYSSCHEMAS`/`SYSCOLUMNS` showing 0 rows** for a real `AITSKxxxxx` library is a
  catalog blind spot; use `OBJECT_STATISTICS` or `DSPFFD` over SSH.

## If the camera ever misbehaves again

Do not debug it inside the application. Take **option 5**, read its details line,
and follow the decision tree in `SIMPLE-CAMERA-TEST.md`. It answers "is it the
build, the delivery, the picture, or the code" in one screen, and it takes about
fifteen seconds.
