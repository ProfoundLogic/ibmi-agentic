# Simple camera test — option 5

A deliberately minimal RPG + Rich Display File screen whose only job is to
answer one question: **can this device read a barcode at all?**

It exists because the same question could not be answered from inside the main
application. Seven rounds of fixes went into the shared scan component and the
iPad still opened the camera, showed a live picture and read nothing — and every
round had to be diagnosed through eleven screens, a service program, commitment
control, a cache stamp and a Genie snapshot. Too many moving parts to isolate one
browser's behaviour.

## What it deliberately does not have

| The application has | This has |
|---|---|
| `gtcommon/gt-scan.js`, shared by 11 screens | its own `gtsimd/simtest.js`, used by nothing else |
| GTBAR service program + binding directory | no parsing at all — the payload is echoed raw |
| SQL, tables, journalling, commitment control | an array in the program |
| images, navigation, subfile paging | one format, one subfile |
| `.sqlrpgle` through the SQL precompiler | `.rpgle` through `CRTBNDRPG` |

The short prerequisite list is the point. When this screen fails, there are only
a handful of things it can be.

## Reaching it

Option **5** on the sign-on menu, `5. Simple Camera Test`.

```
MENU -> 5  ->  TIGERPOC/GTSIMST  ->  ADDLIBLE TIGERPOC  ->  GTSIMR
```

`GTSIMST` mirrors `GTSTART`: option 5 calls it fully qualified, so it is
reachable whatever the library list holds at sign-on, and everything after it
resolves through `*LIBL`.

The sign-on menu is a **shadow** `MENU` built from `../cfdemo` into the CoderFlow
**task** library, which changes every task. Rebuild it each task, deleting the
stamps first — codermake records *that* a target was built, not *which library*
it went to, so a stale stamp makes this a silent no-op and option 5 simply is not
there:

```bash
rm -f build/menu.file build/menu.msgf build/menu.menu
codermake menu.menu                      # default IBMI_BUILD_LIBRARY = task library
```

The application objects go to TIGERPOC as an override:

```bash
IBMI_BUILD_LIBRARY=TIGERPOC codermake gtsimd.file
IBMI_BUILD_LIBRARY=TIGERPOC codermake gtsimr.pgm
IBMI_BUILD_LIBRARY=TIGERPOC codermake gtsimst.pgm
gtwms/tools/deploy-assets.sh             # htdocs -> Profound UI docroot
gtwms/tools/deploy-shim.sh               # snapshot into every skin -- NOT optional
```

## Reading the details line

Under the camera there is one line of state, always visible. **Camera details →
Send details to the server** pushes it into the `DIAG` field, which puts it in the
screen data — the one channel that has reached us from the device every time, with
no screenshot and no log access needed.

```
v=20260811k dec=inline routes=fetch:ok,tag:no vid=1280x720 crop=1203x324 frames=41 focus=not adjustable
```

| Key | Meaning |
|---|---|
| `v` | build stamp. If it is not the current one, the device is running old code and nothing else in the line means anything. |
| `dec` | which decoder is in use: `native` (BarcodeDetector), `inline` (the base64 copy the Genie shim carries in the page), `page`, `fetch`, `tag`, or `none`. |
| `routes` | measured delivery routes, against a 249-byte file: `fetch:ok,tag:no` means fetch works and a dynamically appended `<script src>` does not. |
| `vid` | what the camera gave us. `0x0` means no picture, whatever is on screen. |
| `crop` | what the decoder was handed, after cropping to the reticle. |
| `frames` | frames examined. **`frames=0` with a live `vid` means the loop, not the optics.** |
| `focus` | whether the browser will let us control focus. iOS reports `not adjustable`. |
| `err` | the last unexpected error, if any. |

## The decision tree

Start from the details line, not from the symptom.

```
v= is not the current stamp
    -> the device is running old code. Load ?cb=<something> once. Nothing else is meaningful yet.

dec=none
    -> no decoder could be obtained. Look at routes=.
       tag:no and fetch:no  -> nothing dynamic reaches this device. The shim's inlined copy
                               is the only route; check the skin snapshot is current.
       fetch:ok             -> the fetch+eval path should have worked; read err=.

vid=0x0
    -> the camera opened and produced no picture. Not a decoder problem.

frames=0 with a real vid
    -> the frame loop never ran. A code fault, not the device.

frames climbing, nothing read
    -> the only remaining explanation is the picture: focus, distance, glare or the
       barcode itself. Try "Take a photo instead" -- the native camera focuses and
       exposes where a getUserMedia stream on iOS may not. If the photo reads and the
       live stream does not, that is a focus problem, and it is worth making the photo
       path the primary route on iOS in the main application.

a value appears in the frame below
    -> camera, decoder and the full RPG round trip all work on this device, and any
       remaining fault in the main application is in its own plumbing.
```

## What carries back to the main application

Whatever `routes=` reports here is true of every EJS screen in the application,
because it is the same browser in the same Genie page. In particular:

- `tag:no` confirms the root cause already fixed in `gt-scan.js` — the decoder
  must travel inlined in the page, never as a `<script src>`.
- if the live stream never reads and a photo does, the fix is architectural, not
  a decoder tweak: make the still-photo path primary on iOS.

## Tests

```bash
node gtwms/tools/test-simtest.js
```

27 checks, offline, in headless Chromium. The three that matter, and that were
missing from the main application for seven rounds:

- a **real barcode** in a **real `<video>`** (via `canvas.captureStream`), filling
  42% of the frame, decoded end to end into `pui.submit`. Chromium's fake camera
  emits a rolling colour pattern — a camera with no barcode in it — so every
  earlier check could only prove the decoder *started*.
- with **every network fetch of the decoder blocked**. A test granted a capability
  the device withholds cannot fail the way the device fails.
- with **no screen JavaScript at all**, because a screen that goes blank when its
  JS fails cannot tell us why its JS failed.

It also asserts both directions of the template↔JS handler contract, both
directions of the template↔display-file field contract, and that the details line
is still live after a read — it was not, and the test caught it.

### Known gap

Every check runs in **Chromium**. The device in question is **WebKit**. A real
WebKit build cannot be installed in this container: Playwright downloads it and
the binary resolves its libraries, but launching needs GStreamer and there is no
root here. So these results do not cover the actual iOS engine, and this screen
exists precisely because that gap has to be closed on the device itself.

## Disposable

Delete `gtsimd.json`, `gtsimr.rpgle`, `gtsimst.clle`, `htdocs/…/gtsimd/`, the
`gtsimd`/`gtsimr`/`gtsimst` rules, the `gtsimd-simtest` entry in
`gen-gt-shim.js`, `test-simtest.js`, and menu option 5 + `usr0005`, once the iPad
question is closed. Nothing in the application depends on any of it.
