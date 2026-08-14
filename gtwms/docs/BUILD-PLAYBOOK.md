# GT Warehouse Mobile — build playbook

Everything learned building the first three screens (Main Menu, Scan Lab, Item
Lookup with camera capture), written as rules for the ones still to come.

`GT-WAREHOUSE-POC.md` is the design document and the narrative build log — *why*
each decision was taken, with the evidence. **This file is the operational
distillation: what to do, in what order, and what will bite.** When they
disagree, this file is newer.

Nearly every rule below is here because it cost time. Treat "this seems
unnecessary" as a signal to read the reason first.

---

## 1. Where things live

| | |
|---|---|
| Repo root | `/workspace/workspace/ibmi-agentic/docs` — **not** one level up |
| Branch | `TIGERPOC` |
| Application source | `gtwms/` — `qrpglesrc` `qddssrc` `qclsrc` `qsqlsrc` `qsrvsrc` `tools` `docs` |
| Screen assets | `htdocs/profoundui/userdata/ui/{gtcommon,gtmnud,gtscnd,gtitmd,gtitdd}/` |
| Application objects | **`TIGERPOC`** — persistent, survives tasks |
| The `MENU` shadow | **the CoderFlow task library** (`AITSK000nn`) — per task, see §3 |
| Profound UI docroot | `dev:/home/drusso/puidist/htdocs/profoundui/userdata/ui/` |
| Genie skins | `dev:"/home/drusso/puidist/htdocs/profoundui/userdata/genie skins"/{pls,Classic}` |

**Never hardcode a library in RPG, CL or DDS.** One CL names it once
(`GTSTART` does `ADDLIBLE TIGERPOC`), everything after resolves through `*LIBL`.

---

## 2. Build: `codermake` only

```bash
IBMI_BUILD_LIBRARY=TIGERPOC codermake <target>
```

- **Never a bare `codermake`** with no target. It builds every other project in
  the repo into `TIGERPOC`, including empty copies of populated data files.
- **Never a manual `CRT*` command.** Not over SSH, not in a session.
- Only ever edit `Rules.mk`, never a generated Makefile.
- Build logs: `tmp/logs/<target>.log`. **`codermake` clears `tmp/logs/` at the
  start of every invocation**, so a driver script that logs there deletes its own
  log as it runs, and only the last target's detail survives. `rebuild-all.sh`
  logs to `tmp/rebuild-logs/` for exactly this reason.

### The whole application, in order, one command

```bash
gtwms/tools/rebuild-all.sh           # objects only — the data is left alone
gtwms/tools/rebuild-all.sh --full    # + drop, recreate and reseed the database
```

The order below is prose, and prose is not executable — the script is the order.
It clears each stamp before its target (so nothing is a silent no-op), stops at
the first failure, and finishes with the launcher, the `htdocs` copy, the shim
and the offline suite.

**`--full` is destructive and that is the point.** Tables are dropped and
recreated and `gtseed` regenerates the demo data, so anything captured on a
device since the last rebuild — operator photographs, posted receipts, moves,
counts — is gone. Verified end to end on 2026-08-11: 33 targets, then a clean
seeded baseline of 8 open receipts, 4 open counts, 0 movements.

**How to prove the imagery ordering actually held:** after a `--full`,
`GTIMAGE` must be **102** and `GTVITEMIMG` **816** (408 items × 2 family
images). If imagery had loaded before the seed, `GTIMAGE` would be **0** — and
nothing else would complain.

### The stamp trap — read this before believing a build did nothing

`build/<target>` stamps record **that** a target was built, **not which library
it went to**. Consequences:

- Changing the target library does **not** invalidate the stamps. The build
  reports success and does nothing.
- On a fresh container the stamps may be missing, so order-only `.pf`/`.table`
  prerequisites get **recreated**, emptying a populated file.

So: `rm -f build/<specific targets>` when the target library changes, and
**never blanket `rm -rf build/`** — that rebuilds `custp.file` and friends, which
creates empty shadows of the cfdemo demo data.

### Build order that matters

1. `gtjrn.file` — **journals before tables.** A `codermake`-created library has
   no `QSQJRN`, so SQL tables are not auto-journaled, and DB2 refuses DML on an
   unjournaled table under commitment control (`SQL7008`). `SQL7905` at `CREATE`
   time is expected; `STRJRNPF` runs immediately after.
2. tables → indexes → views → `gtseed.file`
3. **imagery after `gtseed.file`.** `gtseed.table.sql` opens with
   `DELETE FROM GTIMAGE`; load imagery first and the seed wipes it. Invisible on
   an incremental build, only bites on a fresh clone.
3b. **export the image BLOBs to static files, every single time the imagery is
   reloaded — and it reloads far more often than you would think.** `GTIMAGE.image_id` is `GENERATED ALWAYS AS IDENTITY`, so a reload
   issues a **new block of ids** — one generation ran 103..204, the next
   205..306 — and every template builds its URL from that integer
   (`/profoundui/userdata/ui/gtimg/<image_id>.jpg`). Skip this and the server is
   still holding the *previous* generation's filenames: **every image on every
   screen 404s.** Nothing in the build reports it — `codermake` succeeds, the
   shim test passes byte-for-byte, and the pre-flights pass because they serve
   their own fixtures against a server that is not the real one. It is visible
   only by looking at a screen, which is how it was found: by the user, right
   after a rebuild that reported "everything ok". `rebuild-all.sh` does this
   unconditionally at stage 6a and `test-images.js` guards it.

   **The cascade that makes this bite on ordinary builds.** `gtseed.file` lists
   `gtviews.file` as a **normal** prerequisite, and the eight imagery targets
   list `gtseed.file`. So rebuilding any shared view — adding one column to
   `GTVCNTVAR`, say — makes `make` re-run the seed **and** reload all the
   imagery, wiping the transactional tables back to the seeded baseline and
   moving every `image_id`. Measured: ids went **205..306 → 307..408** on a
   build whose only source change was one column in one view. Nothing warns you.

   Two consequences worth holding onto:

   - **Editing a view is a data-losing operation here.** If demo state matters,
     check before rebuilding a view, not after.
   - The image export is therefore **not** a `--full`-only step. `rebuild-all.sh`
     runs it on every deploy, because it is cheap and idempotent and that is
     better than expecting anyone to predict the cascade.
4. service programs → display files → programs
5. the `MENU` shadow
6. `htdocs` assets — **`codermake` does not deploy these**

---

## 3. Getting the operator in — the per-task chore

Sign-on resolves its initial menu through `*LIBL/MENU`, and `PUISETENV` puts the
**CoderFlow task library ahead of `AIDEMOBASE`**. So:

- The shadow `MENU` (display file + message file + menu) is built into the
  **task library**, from `cfdemo`.
- Option 4's command is `call tigerpoc/gtstart` — **fully qualified**, so it does
  not care about the library list.
- `GTSTART` does `ADDLIBLE TIGERPOC *FIRST`, then `CALL GTMNUR`.

**Everything else is in `TIGERPOC`. This launcher is the one exception, and it
is built into BOTH libraries:**

```bash
gtwms/tools/rebuild-menu.sh      # TIGERPOC + the task library, both verified
```

- **`TIGERPOC`** — the permanent home. The launcher travels with the application,
  so a save, or a restore onto a box with a normal library list, just works.
- **the task library** — the copy sign-on actually resolves *here*, and the only
  reason this is a per-task step at all.

The script deletes the stamps first, builds both, and checks all three objects
landed in each. Doing it by hand is two `rm`s and two builds and is easy to get
half-right.

Skipping the `rm` makes this a silent no-op and option 4 simply is not there.

**This has now happened twice, and the symptom is always the same: "we've lost
the menu."** Check `$IBMI_BUILD_LIBRARY` first — if it has changed
(`AITSK00072` → `AITSK00051` the second time), that is the whole diagnosis. The
persistent library is unaffected: `TIGERPOC` still held all 16 programs and 53
files both times, and the deployed web assets and Genie shim live on the server
rather than in the container, so they survive too. **Only the menu shadow is
per-task.** One `rm` and one `codermake menu.menu`, then sign on and look.

Three dead ends, so nobody spends the afternoon again:

- `CHGJOBD JOBD(AIDEMO/AIDEMO)` → **`CPD1602 Not authorized`**. `aidemo` cannot
  change its own job description.
- Writing to `AIDEMOBASE` → off limits, shared.
- `CHGUSRPRF USRPRF(AIDEMO) CURLIB(TIGERPOC)` → **`CPF2292`, *SECADM required**.
  The current library *is* searched ahead of the user portion, so this would work
  — it just needs authority we do not have.
- `QGPL` is on the library list and is writable, but it is shared by every user
  on the box. Shadowing `MENU` there to avoid a per-task step in our own task
  library would be strictly worse.

**The measured sign-on library list**, so nobody has to infer it again:

```
AITSK000nn (SYS) · PLSYS · QSYS · QSYS2 · QHLPSYS · QUSRSYS
AIDEMOBASE · QGPL · QTEMP · DRPUIDEV                          (USR)
```

`TIGERPOC` is not on it and there is no current library — which is the whole
reason the task-library copy exists. **A `TIGERPOC/MENU` alone is never found at
sign-on today**; it is built there so it is ready the moment somebody with
*SECADM runs one of the two commands above, at which point the per-task step
disappears for good.

Verify by signing on and reading the menu. Not by checking an object exists.

---

## 4. Deploying a screen — three steps, all mandatory

Editing an `.ejs`, `.css` or `.js` and rebuilding changes **nothing** on screen
until all three are done.

```bash
# 1. assets to the Profound UI docroot
scp htdocs/profoundui/userdata/ui/<dir>/* \
    dev:/home/drusso/puidist/htdocs/profoundui/userdata/ui/<dir>/

# 2. the shim, for EVERY skin
./gtwms/tools/deploy-shim.sh

# 3. only if the display-file JSON changed
IBMI_BUILD_LIBRARY=TIGERPOC codermake <pgm>.pgm
```

**Why step 2 is not optional.** Profound UI fetches the EJS template over XHR,
and that request does not reach the browser in this environment — the screen
renders as a blank white page with no error. So the template is base64
snapshotted into each skin's `start.html`. The shim holds a **copy**: step 1
alone leaves the browser reading the old snapshot.

- **Each skin holds its own copy.** Testing under `pls` proves nothing about
  `Classic`, which is the skin actually in use. `deploy-shim.sh` does both.
- **Screen JS must be snapshotted too** — a `<script src>` to `userdata/*` is
  fine for tag-based loads, but the shim carries the JS as well so a stale
  snapshot is a stale handler.
- **The stale-snapshot trap:** a shim generated before your edit will pass every
  "is the shim armed" check while serving the previous template.
  `test-gt-shim.js` compares the snapshot to the source **byte for byte** —
  trust that, not the absence of errors.
- **Cache-bust when you touch css/js.** Version-stamp the URLs in the display
  file JSON (`...css?v=20260807b`). Phones cache these hard. Note this
  recompiles the display file *and* its program.
- The render hook on this Profound UI build is **`pui.QD`** (older builds:
  `pui.vu`). The XHR patch does the real work.

---

## 5. Screen architecture rules

### One record format per display file. Always.

Two formats in one display file both stay rendered at once — the second covers
the first, its buttons look dead, and only F3 works because that goes straight to
the program rather than through a button. **To move to another screen, call
another program with its own display file.**

Corollaries learned the hard way:

- **Every `WRITE`/`EXFMT`-target format needs `OVERLAY`.** A `WRITE`-only record
  (a footer, say) without it clears an already-loaded subfile from the device's
  view — `CPF5006` / `RNX1255`, which reads like "two subfiles are unsupported"
  and is not. Two subfiles are fine once every `WRITE` has `OVERLAY`.
- `SFLDSPCTL` with an input field below the subfile anchor row → `CPD7812` at
  compile time.
- A message subfile's `QMHSNDPM` call must pass **the DDS `SFLMSGKEY` field**,
  not a private variable, or every message crashes at runtime.

### READC DOES NOT WORK IN AN EJS SCREEN. Read this before adding subfile input.

**The subfile rows are rendered by your template, not by Profound UI grid
widgets.** So every input in every row carries the same `name="lqty"` with no
record number attached anywhere, and the runtime builds its
`{SUBFILE}.rrn` changed-record marker from *its own* grid widgets — of which
there are none. `READC` therefore returns nothing, however the value was
changed, and the program reports "nothing changed" every single time.

It looks like it works if you test it by POSTing by hand
(`LINSFL.LQTY.2=92&LINSFL.rrn=2`) because you are supplying the row index the
browser cannot. **That is not a test of the screen.** Drive it from a real
browser, or through the collector below.

**The pattern that does work:** send the changed rows in one field with the row
index encoded — `"seq:qty;seq:qty;"` — the same way this application already
encodes a row in its `PICKnn` action codes and a photograph in `IMGDATA`. Client
JS collects only boxes whose value differs from a `data-gt-orig` attribute; RPG
parses the string.

Two traps in the RPG parser, both of which arrive as an **inquiry message on the
operator's screen** rather than as anything a program can report:

- **A trailing separator overruns.** After the last token the scan position sits
  *on* the final character, and `%subst(str : pos + 1)` is then `RNX0100`
  "length or start position out of range". Check `pos >= %len(str)` first.
- **Wrap the numeric conversion in `monitor`.** `%dec` of a malformed token is
  `RNX0105`; skipping the token keeps the operator's other edits.

### TWO SUBFILES IN ONE EJS FORMAT DO NOT COMPILE

The RDF-to-DDS conversion emits malformed DDS — `CPD7836` "subfile control
record not found" and `CPD7835` "subfile record not found", and the display file
is not created at all. **Carry the second list as a JSON string field** the
template parses (`JSON.parse` in the template, plain concatenation in RPG), or
give it its own display file and program.

A short reference list — reason codes, statuses — is a string. A list the
operator scrolls or types into is a second display file.

### Subfile mechanics that cost time

- **The subfile CLEAR writes the RECORD FORMAT, not the subfile record.**
  `sflclear = '1'; write <format>;` — writing the *subfile* record while the
  RRN is still 0 gives **`CPF5021` "incorrect relative record number"**, which
  arrives as an inquiry message on the operator's screen, not as a compile
  error. Set `rrn = 0` before the clear, too.
- **The clear field is a character indicator, not a number.** `sflclear = 1`
  fails to compile with `RNF7416`; it is `sflclear = '1'`.
- **Order a scan-to-confirm list by line number and nothing else.** Ordering by
  status (untouched first, variances next) reads well on paper and is wrong in
  the hand: every scan moved the line that had just been scanned to a different
  position, so the flash-and-scroll landed on a row that had jumped. The view
  still exposes `sort_group` for a future "variances first" review toggle.
- **Audit columns are `VARCHAR(18)` and `opername` is `char(40)`.** Stamp the
  operator *id*, not the name, or the insert fails on length.
- **The SQL precompiler will not take an ARRAY ELEMENT as a host variable.**
  `:items(i).sku` gives `SQL0312 "variable ITEMS not defined or not usable"`
  followed by `SQL0104` on the subscript. Copy it to a scalar first. Same
  restriction as the subfields of a `likeds()` parameter.
- **Move the header status when a detail row moves.** Nothing lifted the receipt
  out of `OPEN`, so it read "Not started" on the screen and in the list however
  many lines had been confirmed — only the final post ever changed it. Do it
  inside the same commit boundary as the line.

### The RPG trap that cost a compile

- **A work field must not share a name with a DDS field.** RPG is
  case-insensitive, so `dcl-s wType char(4)` **is** the externally described
  subfile field `WTYPE`. Redeclaring it produces a storm of `RNF6033`
  "length of externally-described field is not the same", `RNF6299` and
  `RNF7456` — none of which says "you have redeclared a DDS field". The
  Supervisor View's work fields are named `x*` for exactly this reason.

### Shared screen JS must anchor on the COMPONENT, never on a screen class

`gt-scan.js` bound relative to `.gt-scan-screen` — a class only Scan Lab and Item
Lookup's scan screen were ever given. Every screen added afterwards carries its
own wrapper, so `bind()` returned false and **the camera button was dead on nine
of the eleven screens that have one**: both Receiving screens, all three
Inventory screens, both Cycle Count screens, both Putaway screens.

**Why it survived every test in this repo for weeks.** The failure is partial and
silent — the keyed box beside the camera needs no JavaScript at all, because its
Go button is an inline `pui.submit`. So those screens scanned perfectly well *as
long as you typed*, and typing is exactly what every check did:

| Check | What it asserted | Why it passed |
|---|---|---|
| pre-flights | the button is present, sized, hit-testable | it was all three |
| `test-gt-shim` | the JS is delivered, byte for byte | it was |
| live sessions | scanning resolves | fed `SCANVAL` directly — the keyed path |

Present, delivered, and **never once clicked**. `test-camera.js` now clicks it on
every screen that has one, discovering the list by scanning the templates rather
than holding one.

Two rules out of it:

- **Anchor shared JS on the thing it operates.** `#gt-scan-camera` is the scan
  band; anchoring there means "wherever there is a band, wire it up", which
  cannot drift when screen twelve invents a new wrapper class. Put the bound
  marker on the band too, so a re-rendered screen arrives unmarked and rebinds.
- **Genie is a single page, so test the SECOND screen.** The DOM is replaced
  while the window lives on, and the reported failure was always "it works on
  Scan Lab, not on Receiving". `test-camera.js` swaps the DOM and re-runs the
  snapshot to prove a rebind.

**And a harness note that cost a false failure:** `gt-scan.js` checks for
`navigator.mediaDevices.getUserMedia` before doing anything, and on an insecure
origin that property does not exist — so the first run of the new test failed on
*all eleven* screens, including the two that demonstrably work. Serve the harness
over `http://127.0.0.1` (a secure context) with Chromium's
`--use-fake-device-for-media-stream`, and **wrap** `getUserMedia` rather than
replacing it. Stubbing it does not stick.

### NEVER take a fixed-length substring of a scanned payload

```rpg
wKey = %upper(%subst(payload : 1 : 10));   // RNX0100 waiting to happen
wKey = %upper(%trim(payload));             // correct — assignment truncates
```

`%subst(x : 1 : n)` requires the string to be **at least n characters**. A
payload is whatever the scanner handed over, and **every location id here is
six characters** — so `%subst(payload : 1 : 10)` is an escape message, not a
truncation. It shipped in `GTCNHR` and `GTIMVR`.

**Why it hid, and what finally reached it.** The line is only executed when
`gtbar_parse` did **not** resolve the scan — the branch no happy-path test takes,
because every test typed a location that resolved. Enabling the camera reached it
on the operator's first unresolved scan, and an unmonitored `RNX0100` looks like
this from the browser:

```json
{ "name": "CNTHOME", "active": true, "subfiles": {} }     // no data object at all
```

**An empty format with no `data` is a crashed program, not a redisplay.** Go
straight to the joblog:

```sql
SELECT message_id, from_program, substr(message_text,1,80)
  FROM TABLE(qsys2.joblog_info('<job>/AIDEMO/<dev>'))
 WHERE message_id LIKE 'RNX%' OR message_id LIKE 'RNQ%' OR message_id LIKE 'MCH%'
 ORDER BY ordinal_position DESC
```

It named the statement: *"RNX0100 unmonitored by GTCNHR at statement 356"*. Find
the active job with `qsys2.active_job_info(subsystem_list_filter => 'QINTER')`
filtered on `authorization_name`.

**The trap was already guarded in `GTRCLR`, with a comment explaining it** — and
not applied in the two programs written afterwards. Knowing a rule and applying
it are different things, so `lint-scan-handlers.js` checks instead: it flags a
fixed-length `%subst` of `payload`/`scanval`/`work` with no `%len()` guard in the
eight lines above. It accepts the `len > 16 and …` short-circuit form `GTBAR`
uses, because a lint that cries wolf gets switched off.

### Two more things scanners bolt on

`normalise()` in `GTBAR` is the only place that should ever know about these:

- **A leading FNC1.** See above — it broke GS1 detection entirely.
- **Code 39 start/stop asterisks.** `*D01011*` is eight characters that match no
  location, so a rack label was rejected while the operator stood in front of
  that rack. ZXing and Android's `BarcodeDetector` strip them; keyboard wedges
  often do not. A matched leading/trailing pair is now removed — nothing in this
  application has data that legitimately begins and ends with an asterisk.

**A service program change needs a fresh job to take effect.** After rebuilding
`GTBAR` I re-tested in the session that was already open and the fix "did not
work" — the job still had the old `*SRVPGM` activated. Sign off, sign on, retest.
This wasted a cycle and will waste yours.

### An overlay must be pinned to the VIEWPORT, and `position: fixed` is not available

`.gt-photo-overlay` is `position: absolute; inset: 0`, so it covers its nearest
**positioned ancestor**. On the Cycle Count variance screen that ancestor is a
26-row page several thousand pixels tall, and the panel is centred in it with
`margin: auto` — so the camera opened **halfway down the document** and the
operator had to scroll to find it. Measured: with the page scrolled to the
bottom, the panel sat at `top: -3108px`.

`position: fixed` is the obvious answer and is **not usable here**: Genie owns
the page and a fixed element anchors to whatever transformed or positioned
container the skin has wrapped us in, which differs between skins.

So compute the viewport rectangle instead, from the overlay's own
`offsetParent`, which is right whatever Genie does:

```js
var r = o.offsetParent.getBoundingClientRect();
o.style.top = (-r.top) + 'px';        /* parent's viewport offset, negated */
o.style.left = (-r.left) + 'px';
o.style.right = 'auto';               /* inset:0 set these; they fight width */
o.style.bottom = 'auto';
o.style.width = window.innerWidth + 'px';
o.style.height = window.innerHeight + 'px';
```

With no positioned ancestor the `offsetParent` is `<body>`, whose `rect.top` is
`-scrollY`, so this reduces to "top = current scroll position" — the same answer
by the same arithmetic. Re-pin on `scroll`, `resize` and `orientationchange`
while open, and clear the inline geometry on close so nothing stale carries into
the next screen.

**Why no test saw it: every photo fixture was a SHORT page.** Two rows, opened
from the top, where absolute and viewport-pinned look identical. The condition is
a tall page *scrolled down*. Both photo tests now scroll to the bottom, open from
the last row, and assert the panel's rect is inside the viewport — and both were
confirmed to fail when the pinning is removed.

### `video.play()` resolving does NOT mean the video has a picture

Metadata can arrive *after* `play()` resolves, so `videoWidth` is still **0**.
Start decoding then and every decoder gets a 0×0 frame: `BarcodeDetector.detect()`
rejects or returns nothing for ever, ZXing does the same, and the operator sees a
**live picture that never reads a barcode**. That is exactly the report "the
camera opens but will not scan".

`gt-scan.js` now polls for `videoWidth > 0 && videoHeight > 0 && readyState >= 2`
before starting either decoder, gives up after ~6 s, and says so rather than
pretending to scan.

**No desk test could see this**, which is the important part: Chromium's
`--use-fake-device-for-media-stream` gives a stream with dimensions
*immediately*. `test-camera.js` now pins `videoWidth`/`videoHeight` to 0 via
`Object.defineProperty` on `HTMLVideoElement.prototype` to reproduce the real
device, and asserts both that the operator is told and that nothing burns CPU
decoding blank frames.

### The self-test page: stop diagnosing from a description

`gtcommon/gt-camtest.html` — a standalone page, no Genie, no Profound UI, no
display file, no shim. Open it on the failing device:

```
/profoundui/userdata/ui/gtcommon/gt-camtest.html
```

It reports secure context, `getUserMedia`, whether `BarcodeDetector` exists,
whether its constructor works, which formats it admits to, the camera label and
track settings, **the video dimensions**, live decode attempt/error counters for
*both* decoders, and the decoded value. One screenshot, or the Copy report
button, and there is nothing left to guess.

It exists because one symptom was reported **four times** and diagnosed from a
description each time — iPad, then cache, then capability check, then frame
readiness. Every fix was real; none was the whole answer, and each round cost a
build and a deploy. **When a device-only symptom survives one remote fix, stop
fixing and start instrumenting.**

**The page had the same bug it was built to find.** It used
`decodeFromVideoElementContinuously` too, so its "ZXing callbacks: 0" was reading
its own broken loop rather than the device's — which is still exactly the
evidence that cracked it, but by luck. It now runs the same self-owned frame loop
as `gt-scan.js`, and the row is labelled **"ZXing frames examined"** because a
count of callbacks from a callback that is never called tells you nothing.

### "BarcodeDetector exists" does NOT mean "BarcodeDetector works"

On **Chrome for Android** the API is present but backed by a Google Play Services
module that is **downloaded on demand**. On a device where that module is
missing, one of these happens:

| Device state | What the API does |
|---|---|
| Safari, desktop Chrome | `BarcodeDetector` is `undefined` |
| Play module missing | the **constructor throws** |
| module present but empty | constructs, then `getSupportedFormats()` → `[]` |
| module broken at runtime | constructs, then every `detect()` **rejects** |

`beginDetect` originally tested only for **existence**, took the native branch,
and showed *"this browser cannot decode in-camera"* — **without ever trying the
ZXing fallback sitting right beside it**. That is why the message survived a hard
refresh on a browser that definitely was Chrome.

All four states now route to the fallback, and the status text carries the reason
in brackets (`[detector init threw / decoder not loaded]`) so a photo from the
floor says which branch was taken instead of one message meaning four things.

`test-camera.js` simulates all four by overriding `window.BarcodeDetector`, and
each was confirmed to fail before the fix.

**The general rule: a capability check is not a capability.** Feature detection
tells you an API is *present*. Only using it tells you it *works* — so any
failure of the preferred path must fall through to the fallback rather than
reporting defeat.

### iOS has no BarcodeDetector — Safari needs a JavaScript decoder

`BarcodeDetector` is a **Chromium** API. WebKit does not implement it, and on
iOS every browser is WebKit — so on an iPad the camera opened, showed a live
picture, and reported *"This browser cannot decode in-camera."* Honest, and
useless to somebody holding the device.

`gt-scan.js` now falls back to **`@zxing/library`** (vendored at
`gtcommon/gt-zxing.min.js`, 353 KB, Apache-2.0), attached to the same `<video>`
element this file is already streaming into, driven by **our own frame loop** —
see the next section, because the library's continuous helper does not work
here — so the component keeps ownership of the camera and ZXing only reads
frames.

Three constraints that decided the design:

- **Pure JavaScript, not WebAssembly.** The Genie proxy drops XHR to
  `userdata/*`, so a wasm build could not fetch its `.wasm` — while a
  `<script src>` to the same path **is** served normally. That asymmetry is the
  whole reason this is the JS port and not `zxing-wasm`.
- **Loaded lazily, only where needed.** Android has the native API and pays
  nothing.
- **Released with the camera.** `stopCamera()` clears our decode timer *and*
  calls `reader.reset()` to drop the library's capture canvas.

**The bug that made the fallback invisible even after it was written:**
`startCamera()` had its *own* `if (supportsDetector())` and showed the warning in
the `else`, so `beginDetect()` — which now picks the decoder — was never
reached. One decision, one place.

**Deployment is not covered by the shim test.** The decoder is fetched by a
script tag at runtime, so it is not part of any snapshot: `gt-scan.js` can ship
correctly with no decoder beside it and the iPad is exactly where it started.
`test-camera.js` fetches the URL from the real server, and simulates Safari by
**deleting `BarcodeDetector`**, then checks the fallback loads *and decodes a
real UPC-A* — a fallback that loads but cannot read a barcode is no better than
the message it replaced.

### `decodeFromVideoElementContinuously` NEVER STARTS on a video that is already playing

The iPad reported `zxLoaded: true` with **`zxTries: 0`**. The decoder had loaded
and its loop had never run a single frame. The reason is four lines inside the
library:

```js
playVideoOnLoad(el, cb) {
  el.addEventListener('playing', cb);   // it waits for a 'playing' event
  this.tryPlayVideo(el);
}
tryPlayVideo(v) {
  if (this.isVideoPlaying(v)) { return; }   // ...and returns early if playing
  await v.play();                            // the only thing that fires it
}
```

We hand it an already-playing element **deliberately** — `whenVideoReady()` waits
for real dimensions before decoding anything, which is a fix from the round
before. So `tryPlayVideo` bails, `'playing'` never fires again, and the callback
is never invoked. Two correct fixes that combine into a dead decoder. Its setup
also calls `reset()`, which stops the very stream we opened.

**Drive the frames yourself:**

```js
var tick = function () {
  if (!state.running || !state.zxing) return;
  if (!video.videoWidth) { state.zxTimer = setTimeout(tick, 200); return; }
  diag.zxTries++;
  try {
    var result = state.zxing.decode(video);      // NOT createBinaryBitmap+decodeBitmap
    if (result) { submitPayload(result.getText()); return; }
  } catch (e) { /* NotFoundException on every frame with no barcode */ }
  state.zxTimer = setTimeout(tick, 120);
};
```

- **`decode()`, not `createBinaryBitmap()` + `decodeBitmap()`.** `decode()` is
  those two calls plus a retry of the *same frame* with a different binarizer
  (`GlobalHistogramBinarizer` instead of `HybridBinarizer`). Measured on one
  identical video frame: `decode()` read it, the pair did not. Twice the work per
  frame, still cheap against a 120 ms interval, and a read on the second attempt
  is a read.
- **`setTimeout`, not `requestAnimationFrame`.** A JS decoder at 60 fps cooks the
  battery for nothing, and rAF stops dead when the tab is backgrounded.
- **`clearTimeout` in `stopCamera()`**, or the loop outlives the camera.
- Minified builds **rename `NotFoundException`** — the device reported
  `lastErr: "N"`. Match exception names loosely (`indexOf('NotFound') < 0`) and
  never on identity.

### A synthetic camera must carry a real barcode, or the test proves nothing

Every camera test before this one used Chromium's `--use-fake-device-for-media-stream`,
which emits a rolling colour pattern — **a camera with no barcode in it.** So they
could only ever prove the decoder *started*. The device's failure was that it
never started, and no test could see it.

The fixture that catches it: draw a bwip-js barcode onto a canvas, hand
`canvas.captureStream(10)` back from a stubbed `getUserMedia`, delete
`BarcodeDetector` to force the iOS path, and assert `pui.submit` receives the
payload. It covers **all 11 symbologies the cheat sheet prints**. Reverted against
the old code it reproduces `zxTries: 0` exactly.

Three fixture traps, all of which cost time here:

- **Repaint on a timer.** A canvas that never changes stops producing frames, and
  a frozen stream passes for the wrong reason.
- **1280 × 720**, because that is what `gt-scan.js` asks the camera for. At
  640 × 480 a wide 1D symbol spread across the frame puts its narrow bars under a
  pixel and nothing decodes — a decoder bug that does not exist.
- **Compute GS1 check digits** (`bwip-js` rejects a wrong one) and **omit**
  `height` for 2D symbologies rather than passing `undefined` (it throws).

### A getUserMedia stream is NOT the camera app — it may never focus

The loop was fixed, delivery was verified byte-for-byte on both skins, and the
iPad still would not read a barcode. What is left when the code is running and the
frames are being examined is **the picture itself**, and the likeliest cause on
iOS is focus: a `getUserMedia` stream commonly stays at a fixed focus, so a
barcode held where a person naturally holds it is blurry — and blur is unreadable
no matter how good the decoder is. WebKit exposes no focus control, so
`applyConstraints({advanced:[{focusMode:'continuous'}]})` is worth attempting
(Chrome on Android honours it) but cannot be relied on.

**So give the operator a path that does not use the stream at all:**

```html
<input type="file" accept="image/*" capture="environment">
```

`capture` hands the job to the **native camera**, which focuses, exposes and
shoots at full resolution; then decode the still. It is also strictly better
input than a video frame — the still that comes back here is ~860 px of clean
barcode. This is the escape hatch for any device whose live stream will not
cooperate, and it costs one button.

Built in JavaScript — the markup, the CSS and the wiring — so **eleven templates
did not have to change**. `gt-scan.js` creates the control inside the scan band on
every screen that has one; the tests assert it appears on a screen whose template
knows nothing about it.

### Own the pixels: crop to the reticle, and run three binarizers

Handing the `<video>` element to the library gives up too much:

- its capture canvas is created **once**, sized from `videoWidth` at that instant,
  and reused for ever — an orientation change leaves it the wrong shape;
- it always analyses the **whole frame**. A barcode filling a fifth of a
  1080-line picture is a handful of pixels per bar by the time the binarizer sees
  it, and nobody fills the viewfinder with the barcode;
- failures inside it surface as one opaque exception per frame.

Draw the reticle region into your own canvas at a capped size (1024 px long edge —
a JS decoder walks every row, and more resolution buys nothing once each bar is a
few pixels across), then build the bitmap yourself from the exported classes the
library uses internally:

```js
var lum = new Z.HTMLCanvasElementLuminanceSource(canvas);
new Z.BinaryBitmap(new Z.HybridBinarizer(lum))                          // local threshold
new Z.BinaryBitmap(new Z.GlobalHistogramBinarizer(lum))                 // flat, high contrast
new Z.BinaryBitmap(new Z.HybridBinarizer(new Z.InvertedLuminanceSource(lum)))  // light on dark
```

Three attempts per frame are far cheaper than the 120 ms between frames, and a
read on the third is still a read. Every fourth frame skips the crop and takes the
whole picture, so a barcode the operator has not centred still reads.

`willReadFrequently: true` on the context keeps the surface on the CPU — without
it every `getImageData` round-trips from the GPU. Older WebKit **throws** on the
options object rather than ignoring it, so wrap it.

The test that proves the crop earns its keep renders the barcode at **42% of the
frame** and asserts both the decode and that the analysed frame really was cropped
and capped.

### The application must report on itself

Five rounds of one symptom were diagnosed from a description. A standalone
self-test page fixed that once — but asking somebody on a warehouse floor to open a
second URL is a poor answer, and the page had the same bug as the app anyway.

The scan band now carries a **Camera details** button. One tap prints version,
decoder path, frames examined, video and analysed-frame dimensions, focus state,
camera label and the last non-routine error. It stays `display:none` until a scan
fails to resolve or it is asked for, so it costs nothing when things work. After
7 seconds with no read it appears by itself, alongside distance/glare advice and
the photo-scan offer.

**A report that does not name the version it came from is ambiguous** — that
ambiguity is what made two of these five rounds inconclusive. The version is the
first field, and a test asserts it.

### An ITF-14 bearer bar cannot be read by a camera

`bwip-js` draws ITF-14 with a bearer bar by default. It is correct for a printed
carton — the thick frame stops a laser reading a partial symbol — and it sits hard
against the quiet zone, which through a camera frame is **fatal**: ZXing rejects
the symbol outright. Same value, 40 frames and no read with the bearer bar, read
on the *first* frame with `borderwidth: 0`.

Two cheat-sheet cards (demo steps 1.3 and 2.2) would have failed live while every
check we had said the sheet was good, because **verifying a rendered barcode as a
still image is not verifying it through a camera.** At natural resolution the
bearer bar decodes fine; `verify-demo-sheet.js` passes it before *and* after the
fix. It is the downscale into a video frame that closes the quiet zone.

`gen-demo-sheet.js` now forces `borderwidth: 0` for `itf14`, and `test-camera.js`
asserts that it still does.

### Option 4 started working with NO camera code change. Read this before debugging delivery again.

On 2026-08-14 both option 4 and option 5 read barcodes on the iPad. Diffing the
whole repo against the state it was in when the camera was last reported broken,
**`gt-scan.js` is byte-for-byte identical** -- as are all sixteen display files and
every application program. The fix had shipped in `20260811k` in the previous task;
the device had simply never loaded it, and the access log said so all along.

Today's log settles the delivery question:

```
09:50:00  GET /profoundui/auth/genie?skin=classic  200  2,770,795
gt-zxing.min.js fetches from the device today ....  0
simtest.ejs     fetches from the device today ....  0
```

Both screens worked while fetching neither the decoder nor the template: both came
from the base64 in `start.html`. One ~2.6 MB page load now carries eleven
templates, their CSS, every screen's JavaScript and 354 KB of decoder.

**One earlier conclusion needs narrowing.** `simtest.js` WAS fetched over the wire
today by Profound UI's own screen-JS loader (200, 23,666 bytes), so "a dynamically
appended `<script src>` never fires on this device" is too strong. What is certain
is that the inlined route works and the decoder is no longer fetched. Whether the
runtime-appended tag is still broken is unsettled and today's log cannot settle it,
because `gt-scan.js` now prefers the inlined copy and never reaches the fetch.
Option 5's `routes=` measures it directly.

The full per-task checklist -- recover uncommitted work from
`/task-output/patches/`, `npm install`, rebuild the menu shadow into the current
task library, build to TIGERPOC, deploy assets AND re-snapshot every skin, then one
cache-defeating page load -- is in `HANDOFF-TO-MAIN-APP.md`. Skipping any step
makes a working application look broken.

### When a bug outlives three fixes, build the smallest thing that can still fail

Seven rounds went into the shared scan component and the iPad still would not
read. Every round was diagnosed through eleven screens, a service program,
commitment control, a cache stamp and a Genie snapshot -- so every round had a
dozen candidate explanations, and picking the wrong one cost a full deploy cycle.

Option 5 of the sign-on menu is the answer to that: `GTSIMR` + `GTSIMD`, RPG and a
Rich Display File and nothing else. No SQL, no tables, no journalling, no GTBAR,
no shared JavaScript, one format, one subfile. See `SIMPLE-CAMERA-TEST.md`.

The value is not that it is small, it is that **the prerequisite list is the
diagnosis**. When it fails there are only a handful of things it can be, and its
details line names which one:

```
v=20260811k dec=inline routes=fetch:ok,tag:no vid=1280x720 crop=1203x324 frames=41
```

`routes=` is the part worth copying into any future screen: it MEASURES which
delivery mechanisms reach the device, against a 249-byte file, rather than
assuming. That distinction -- "the decoder is broken" versus "the decoder was
never delivered" -- is what the seven rounds were actually about.

Two things it changed on the way in, both caught by its own test rather than by a
device:

- **A field declared and never rendered is dead weight.** `nscans` went into the
  display file out of habit; the row count already gave the same number where it
  was used. The test now fails on any declared-but-unrendered field.
- **The details line went stale on the one path that mattered.** A successful read
  leaves the frame loop without going through the refresh, so after a read the
  line still said `dec=none frames=0` -- a readout that lies exactly when someone
  is finally looking at it. If a screen reports on itself, test the reporting on
  the success path too, not just the failure paths.

### THE ROOT CAUSE, after seven rounds: a dynamically appended `<script src>` NEVER FIRES on the device

On the iPad, inside Genie, appending

```js
var el = document.createElement('script');
el.src = '/profoundui/userdata/ui/gtcommon/gt-zxing.min.js';
document.head.appendChild(el);
```

produces **no network request at all.** Not a 404, not an error — nothing reaches the
server. Proven from the access log: that file was fetched from the device **once all
day**, and that fetch came from the standalone self-test page, never from inside the
application. Three independent files behaved identically (the decoder, the version
probe, the access-log ping) and all three work in Chromium.

**iOS has no `BarcodeDetector`, so ZXing is the only thing that can read a barcode
there.** No decoder means a camera that opens, shows a live picture and can never
resolve anything — the symptom reported seven times, through five real but
insufficient fixes.

Why WebKit-inside-Genie drops those requests is still unknown, and **it does not
matter**: the shim's base64 route demonstrably reaches the device, because the
screen JS it carries is running there. So the decoder travels the same way.

- `gen-gt-shim.js` embeds `gt-zxing.min.js` as **one global** `ZXING_B64` (not per
  screen — 354 KB inlined once; `start.html` goes 1.96 MB → 2.5 MB) and exposes
  `window.__gtLoadZxing()`, which decodes and evaluates it **lazily**, so a 5250
  screen that will never scan pays nothing.
- `loadZxing()` in `gt-scan.js` calls that first and keeps the `<script src>` route
  only as a fallback for skins with no shim. `diag.zxVia` reports which was used.

**THE TEST THAT WOULD HAVE CAUGHT THIS IN ROUND ONE:** block every network route to
the decoder and require a real barcode to decode anyway.

```js
await pg.route('**/gt-zxing.min.js*', (r) => r.abort());
```

Every camera test until now let that fetch succeed, so all of them passed while the
device could not scan at all. The general lesson is bigger than this bug: **a test
that grants the code a capability the device withholds cannot fail the way the
device fails.** The fake camera with no barcode in it was the same mistake; so was
comparing local source to a local snapshot to "verify" deployment.

Also: run the file the way the SHIM runs it — `new Function(src)()` — not as a
`<script>` tag. Every test used the tag route for seven rounds; the device uses the
other one.

### THE ACCESS LOG IS ALSO A TELEMETRY SINK. The URL is the message.

The log records every request **with its query string**, so the client can report
its own state by fetching a static file with a long query:

```
GET /profoundui/userdata/ui/gtcommon/gt-ping.js?v=20260811i&via=snapshot&why=noread
    &path=zxing&frames=312&video=720x1280&read=1024x460&det=0&zx=1&photos=0
    &focus=not+adjustable&err=-&ua=Mozilla%2F5.0+(iPad...CriOS%2F152...)  200
```

Read it back with:

```bash
ssh dev "grep gt-ping /www/drpuidev/logs/access_log.Q1YYMMDD00 | tail"
```

`gtcommon/gt-ping.js` is an empty, served file — nothing executes; only the URL
matters. **No CGI program, no database table, no RPG change, no authority we do not
already have.** Sent at most three times per page (install, first successful read,
and once if a scan does not resolve), user agent truncated.

This is what to build INSTEAD of asking for a screenshot. It also resolves an
ambiguity that wasted a round: **the log has no user-agent column, and not every IP
is a device.** `172.16.1.212` made 109 requests without fetching a single asset,
and `172.16.1.95` fetched assets *and* posted `screen_visualization=1` — the
screen-capture service renders screens, so it looks exactly like a browser. Until a
ping arrives carrying a real user agent, "which IP is the operator's iPad" is
guesswork.

`via=` distinguishes the two delivery routes: the shim sets `window.__gtVia =
'snapshot'` before running its base64 copy, so a report tells you whether a cached
`start.html` is pinning old code or the stamped `<script src>` tag is in charge.

**The bootstrap limit, and it is real:** telemetry and self-upgrade both live IN the
build. A device stuck on an older build has neither, and nothing server-side can
push to it. Exactly one genuine cache-defeating load is required to get out —
`…/profoundui/auth/genie?cb=<anything>` in a new tab — and from that build onward
the device reports itself and heals itself. Every mechanism of this kind pays off
only from the version that introduces it.

### READ THE ACCESS LOG FIRST. It knows what the device fetched.

```bash
ssh dev "grep -h 'gt-scan.js' /www/drpuidev/logs/access_log.Q1YYMMDD00 | tail -25"
```

The instance serving 8103 is `/www/drpuidev`, its logs are world-readable, and
**every asset request is in there with a timestamp, the client IP, the query string
and the byte count.** Six rounds of "is the fix on the device?" were argued from
descriptions while this file sat on disk with the answer.

What it settled in one command:

```
15:14:04  172.16.1.95   GET .../gt-scan.js?v=20260811f  200 24420
15:15:39  172.16.1.133  GET .../gt-scan.js?v=20260811f  304 -
15:26:09  10.3.96.23    GET .../gt-scan.js?cb=102562    200 36799   <- my own check
```

The fix went out at 15:26. **Neither device made a single request after 15:15**, and
`now` was 15:47. The operator refreshed, nothing was re-fetched, and the old code
kept running — so the report "still broken after refreshing" was accurate and the
new code had never been on the device.

Three things worth knowing when reading it:

- The **byte count identifies the build** even when the URL does not. `20891` vs
  `24420` vs `36799` distinguished three generations at a glance.
- `?v=` in the log proves the tag route fired — meaning a real page load happened.
  No request at all means everything came from cache.
- **Not every IP is a browser.** `172.16.1.212` made 109 requests and never asked
  for a single asset: it is the screen-capture service that posts
  `screen_visualization=1`. Match the IP to asset fetches before drawing
  conclusions about "the device".

### A stamp only works on a real page load — so the code checks its own age

The `?v=` stamp is only consulted when the browser actually loads the page. A page
that is never reloaded never asks the server anything, and no number of stamp bumps
will reach it. That is a different failure from the cache-stamp trap below: there,
the browser asked and got a stale answer; here, it never asked.

`gt-scan.js` now settles it from the inside. On install it pulls
`gtcommon/gt-version.js?cb=<timestamp>` — generated by `deploy-assets.sh`, holding
nothing but `window.__gtStamp` — and if that stamp is newer than its own `VERSION`
it loads the current build by URL and lets the newer-wins guard hand over. A stale
page heals within a second of a screen appearing, with no reload and nothing asked
of the operator.

- **A `<script>` tag, not XHR.** The Genie proxy drops XHR to `userdata/*`; tag
  fetches to the same path are served normally.
- **Cache-buster is a timestamp, not the stamp.** The point is to ask a question
  whose answer we do not already know.
- `lint-cache-stamps.js` asserts `gt-version.js` agrees with the display files, so
  it cannot advertise a build that was never deployed.

**THE TRAP IN THE UPGRADE PATH, which cost a red test before it cost a demo:**
`bind()` marks the band `data-gt-scan-bound` and returns early when it sees the
mark. A newer copy therefore installs itself, attaches **nothing**, and leaves the
Start button calling the *old* `startCamera` — upgraded and inert. The takeover now
clears the mark, removes the controls it created, and **clones the Start/Stop/keyed
controls to drop listeners** registered by a copy of the file we no longer hold a
reference to. Without the clone, one click opens two camera streams; a test asserts
exactly one.

Everything injected goes inside **one** container (`#gt-scan-extra`) so the
takeover is a single removal. The first version appended the version chip and the
diagnostic line to the band separately, the takeover removed two of three, and the
surviving old chip — sharing an id with the new one — reported the version that was
no longer running. The element that exists to end version confusion caused it.

### The version chip

Ten grey pixels under the scan band reading `v20260811h`. "Which version is your
device running?" was the pivotal question in five of six rounds and was never
answerable from a screenshot. It is now, without a tap.

### THE CACHE STAMP. Three fixes were "deployed" and none of them ran.

Screen assets reach the browser by **two routes at once**:

1. the display file lists them in `"css"`/`"js"`, so the browser loads a
   `<script src="...gt-scan.js?v=stamp">` tag — **served from cache** if the
   stamp has not changed;
2. the Genie shim injects a base64 snapshot, which is **always current**.

`gt-scan.js` installs a singleton and guarded its own re-entry, so **whichever
ran first won** — and the second copy became a no-op. `gtitmd.json`,
`gtmnud.json` and `gtscnd.json` carried **no `?v=` stamp at all**, so their copy
could never be evicted: a months-old August file installed itself and the fresh
snapshot politely stepped aside.

Result: the camera anchor fix, the viewport fix and the iOS decoder were all
verified byte-for-byte in the deployed snapshot, and the device kept running
August's code. The operator reported the same error three times while every
check passed.

**Two changes, and both are needed:**

- **One stamp for the whole application, bumped on every asset change.** Not
  per-file: a per-file stamp means remembering which screens changed, which is
  how three got left behind. `lint-cache-stamps.js` fails on any unstamped URL
  and on any disagreement.
- **Newer wins, inside the file.** `gt-scan.js` and `gt-photo.js` declare
  `var VERSION` matching the stamp, and on re-entry compare it: an older copy
  arriving second steps aside, a newer copy shuts the old one down and takes
  over. Order no longer matters, so this survives the next time somebody forgets
  the stamp. The lint checks the declared VERSION equals the stamp.

**The full deploy after touching any css/js:**

```bash
# 1. bump the stamp in EVERY display-file JSON, and VERSION in the guarded files
node gtwms/tools/lint-cache-stamps.js          # must be clean before building
# 2. the JSON is a prerequisite of the program, so this rebuilds BOTH
for p in gtitmr gtitdr gtscnr gtrchr gtrclr gtinhr gtinlr gtimvr \
         gtcnhr gtcner gtcnvr gtsvr gtpur gtpdr gtstr gtmnur; do
  IBMI_BUILD_LIBRARY=TIGERPOC codermake $p.pgm
done
# 3. assets to the docroot, then the shim for EVERY skin
gtwms/tools/deploy-shim.sh     # runs deploy-assets.sh first -- see below
```

**`deploy-shim.sh` alone was NOT enough, and that is a trap worth naming.** It
writes a base64 *snapshot* of every template and screen script into each skin's
`start.html`, and verifies that snapshot **against local source** — so it reports
"matches source byte for byte" while the docroot still serves the old file. The
standalone copies under `userdata/ui` used to be uploaded only by
`rebuild-all.sh` stage 6b, so this recipe skipped them.

gt-scan.js was fixed, stamped, rebuilt, shimmed and verified green while the
device was still downloading **20891 bytes of August**. The upload is now
`gtwms/tools/deploy-assets.sh`, called automatically by `deploy-shim.sh`, and it
finishes by comparing **local byte count against what the server serves over
HTTPS** — because comparing local source to a local snapshot cannot detect this
class of failure at all.

Anything fetched by URL rather than from the snapshot comes from the docroot and
nowhere else: `gt-zxing.min.js`, `gt-camtest.html`, and **every** file on the
seven skins that carry no shim.

**Only 2 of the 9 skins on this box carry the GTWMS shim** (`pls`, the default,
and `Classic`). On the other seven the shim is absent, so the `"js"` array is the
*only* delivery route — which makes the stamp the only thing standing between the
operator and a stale file.

### These screens exit by ACTION value, and the values are NOT uniform

There are no response indicators or shortcut keys in the metadata — the EJS
templates drive everything through `pui.submit({action: ...})`, so a scripted
session leaves a screen only by sending the value that screen's RPG accepts. They
differ: `GTMENU` and `ITMSCAN` take `EXIT`, while `ITMDETL` takes `MENU` or
`BACK`. Guessing `EXIT` on Item Detail does nothing at all, silently — a session
was ended still sitting inside the application rather than signed off.

Read the values from the source before navigating:

```bash
grep -in "when action = " gtwms/qrpglesrc/<pgm>.sqlrpgle
```

### Every screen needs a visible way out

An F-key alone is not enough — on a phone there is no keyboard. Every screen
carries visible buttons (`data-fkey` mirrors the key for desktop) **and** the
F-key. Item Lookup shipped once with only "Scan another", which was covered by
the format underneath; the lesson is in §32 of the design doc.

### THE VIEWPORT. Read this before debugging any "it looks wrong on the phone"

**Genie's `start.html` declares no `<meta name="viewport">`.** A mobile browser
with no viewport meta lays the page out in a **~980px layout viewport** and then
zooms the finished page out to fit the screen. Measured on an S24 Ultra profile:

| | layout viewport | what the operator sees |
|---|---|---|
| no meta | **980px** | the ≥835px desktop layout, scaled to ~42% |
| with meta | **412px** | the phone layout, full size |

So every media query in the application was reading a desktop width, and the app
was a shrunken desktop on a phone. **Nothing was wrong with the CSS.** The page
was never told the device existed.

The shim now sets the meta, but **only while a `.gt-app` wrapper is on the page**,
and restores the previous state when it leaves — `start.html` is shared with the
other projects in this skin and with Genie's own 5250 screens, where 980px scaled
to fit is arguably right for a fixed 80-column terminal. `test-device.js` checks
the deployed shim carries it, that the phone breakpoint really is in effect, and
**the negative case** — that without the meta the layout is 980px, because
otherwise the check proves nothing.

**Why five green pre-flights missed it:** each injected its own
`<meta name="viewport" content="width=412">` before measuring. A harness that
supplies what the real page is missing cannot discover that the real page is
missing it. The engine now injects exactly what the shim injects, emulates a
phone (`isMobile`), and asserts the layout viewport *is* the device width.

### Two ways to crush your own layout

- **`min-width: 0` on a flex item lets it be squeezed below min-content**, and
  once it is, even `overflow-wrap: break-word` must break mid-word. That is how
  the header rendered **"GT / Wareho / use"**. Put `min-width: 0` on the item that
  should *give way* (the operator name, which truncates), never on the one that
  must stay readable.
- **`overflow-wrap: anywhere` also collapses min-content to one character.** Use
  `break-word`: it still breaks a genuinely unbreakable string, but prefers the
  space.

### Colour tokens are colours. Metric tokens are lengths.

`--gt-tap` is 48px and `--gt-action` is **64px**. `background: var(--gt-action)`
is invalid CSS — silently dropped — and with `color: #fff` on top it produces an
element that is **white text on a white card**: in the DOM, correctly sized,
hit-testable, screenshot-perfect, invisible. Six had shipped across three screens
before anyone saw the missing chip. `test-device.js` greps for it, because no
rendering check catches it reliably.

### Responsive means more columns, not wider ones

Breakpoints at 640 / 835 / 1280. Small screens get key facts only; wide screens
get additional columns. Target device is a **Samsung S24 Ultra ≈ 412×915 CSS px**;
also check 360 for the narrowest realistic phone.

**A breakpoint must never be the only thing standing between an operator and a
control.** Both faults found so far were exactly this: the *Add photo* button
appeared only ≥640px, and the carousel arrows only ≥835px with `hover`. If a
control matters on a phone, it is present at every width.

---

## 6. EJS and CSS

- **Field names arrive lowercase.** Profound UI lowercases DDS field names for
  the template. `<%= DSKU %>` renders blank, silently. Use `dsku`.
- **An undefined field reference throws and aborts the whole render** — one
  `<%= foo %>` for a field not declared in that format's JSON gives a blank
  screen. When cloning a template, audit every identifier;
  `preflight-*.js` does this automatically.
- **When a second screen needs a component, MOVE it to `gt-theme.css` — do not
  copy it, and do not leave it where it was.** This has now cost time three
  times. The camera block was copied, then shared. The stepper was duplicated,
  then shared. And the **manual scan-entry row was left in `scnhome.css` while
  four screens used its class names** — so Receiving and Inventory both rendered
  a bare, unstyled **25px** input where a 64px one belonged, for days, because
  nothing measured it. If a screen uses a class it does not own, the styling
  belongs in the theme.
- **Scope every rule to the wrapper.** No `html`/`body` rules, no
  `position: fixed`, no `100vh`, no global reset. Genie owns the page.
- **The Genie reset must stay at element specificity.** Written as
  `.gt-app div {...}` it scores (0,1,1) and silently beats your own component
  classes — that is how the header and footer lost their padding. Write
  `:where(.gt-app) div {...}` → (0,0,1).
- **This trap has now bitten twice, the second time for months without being
  noticed.** `.gt-app button { color: inherit }` (0,1,1) beat
  `.gt-btn-primary { color: #FFFFFF }` (0,1,0), so *every primary button in the
  application* rendered near-black text on GT red. It looks plausible in a
  screenshot, which is why it survived. **Assert computed colour, don't eyeball
  it** — `getComputedStyle(el).color` in the pre-flight page is two lines.
- **State `z-index` above 10 explicitly for anything overlaying an image.** The
  skin sets `div { z-index: 10 }` on bare divs; `gt-theme.css` resets it, but
  both selectors score (0,0,1), so the winner depends on stylesheet order. A
  button at `z-index: 3` is clickable or not depending on the skin.
- **`white-space: nowrap` is the skin's default** and it breaks French, which
  runs ~20% longer than English. Reset it, and use `minmax(0, 1fr)` in grids.
- **Screen JS must use inline handler attributes** calling window globals. A
  file in the `js` array runs **before** the template renders, so
  `addEventListener` binds to nothing and fails silently. (`gt-carousel.js` and
  `gt-scan.js` get away with listeners only because they poll for their root
  first.)
- **The screen must render usably with no JS at all.** A JS-added visibility
  class plus a container Genie hides until the JS lands is a plain white screen.
- **A native `<input type="number">`'s spinner arrows are unusable on touch** —
  a few pixels tall, stacked on each other. Suppress them
  (`appearance: textfield` plus `::-webkit-inner/outer-spin-button`) and supply
  real 48px `−` / `+` buttons either side of the box. Step by something
  meaningful — the item's **case pack**, sent down in the subfile — because a
  line expecting 864 eaches is not counted up one tap at a time. Adjust
  client-side and commit once; a tap per case must not be a round trip per case.
- **Never put a localised number in an `<input type="number">`'s `value`.** A
  number input **silently empties itself** when handed anything that is not a
  valid floating-point literal, and French formats 24.00 as `24,00` — so every
  quantity box on the French screen came up blank while English was fine, and
  saving would have written zeroes. A number input's value is *data*; format for
  display only in text. Assert `input.value !== ''` in the pre-flight.
- **A control squeezed until its own value is invisible still passes an overflow
  check.** Nothing crosses the viewport, so `scrollWidth === clientWidth`. Give
  flexible controls a real `minmax()` floor, and assert both a minimum width and
  `scrollWidth <= clientWidth` on the control itself.
- **Assert the touch target, not just the presence.** A control can be visible,
  hit-testable and still too small to hit — that is exactly what the spinners
  were. The pre-flight now fails anything under 44px, and caught one of my own
  buttons at 40px.
- **`position: sticky` fails silently.** One ancestor with `overflow` anything
  other than `visible` and it simply does not stick — no warning, nothing in the
  console, and it looks fine until you scroll. A pinned band also needs an
  **opaque background** (or content shows through as it passes beneath) and a
  **z-index above whatever the rows claim** — the cards here use 20–21 to beat
  Genie's `div { z-index: 10 }`, so the band takes 30. **Assert it by actually
  scrolling** and measuring: `window.scrollTo(...)`, then check the control is
  still in the viewport and that `elementFromPoint` just under the band finds a
  row rather than the band. Both failure modes are covered in
  `test-rcv-stepper.js`.
- **Never let a form control fall below 16px.** iOS zooms the page on focus
  below that and stays zoomed, which is what made the app feel clunky. 16px is a
  floor: `font-size: max(16px, 1rem)`.
- The `Classic` skin ships **no viewport meta** (`pls` does), so the shim adds
  `width=device-width, initial-scale=1` when missing. Never
  `user-scalable=0` — that takes pinch-zoom from everyone.
- **Resolve i18n and theme in the template, not the JS.** Templates render
  client-side, so they can read `localStorage`. Every screen carries the EN|FR
  toggle.

---

## 7. RPG and SQL

The house pattern, copied from `gtitdr.sqlrpgle`:

```rpgle
**free
ctl-opt dftactgrp(*no) actgrp(*new);
ctl-opt bnddir('GTIMG');                       // in SOURCE, not Rules.mk
dcl-f gtxxxd workstn sfile(xxsfl : rrn) handler('PROFOUNDUI(HANDLER)');
exec sql set option commit = *chg, closqlcsr = *endmod;
/copy gtxxx_pr.rpgle
```

- **`codermake` passes no `BNDDIR` to `CRTSQLRPGI`.** A `.srvpgm` prerequisite in
  `Rules.mk` controls build *order*, not the binder's search path. Without
  `ctl-opt bnddir(...)` the bind fails `CPD5D02: Definition not found` even
  though `DSPSRVPGM` shows the export.
- **Binder export symbols must be uppercase** even when the procedure name is
  mixed case.
- **`commit = *chg` is correct here** because `TIGERPOC` tables are journaled.
  In a library where they are not, an `INSERT` silently rolls back at exit and
  you need `commit = *none`.
- **One statement per line, everywhere** — `RNF5508` "end of free-format
  statement is not blank", pointing at the column *after* the first semicolon.
  Free-form RPG has no C-style separator, so `nrcpts = 0; nrlines = 0;` is two
  statements on one line and will not compile. **This rule was already written
  here and was still broken twice in one program**, because `a = 0; b = 0;`
  reads like tidy grouping in every other language — so: resist the grouping.
- **`/copy` members must match the house layout**: 7-space indent, no tabs.
- **The SQL precompiler rejects subfields of a `likeds()` procedure parameter as
  host variables.** Relatedly, `dcl-ds row like(templateDS)` used as a whole-row
  `FETCH INTO` target produces a bogus `SQL0314 "host variable not unique"`
  blaming an unrelated variable — use `likeds()` for scalar DS host variables.
- **Identity values are not rolled back.** A rolled-back `INSERT` consumes the
  id, so ids have gaps and nothing may infer a count from the highest one.
- DDL: `FOR COLUMN` before the data type; `FOR SYSTEM NAME` is **rejected when it
  equals the SQL name** (`SQL7029`); `_by` columns `VARCHAR(18)` for
  `DEFAULT USER`; object names ≤ 10 chars.
- **`GENERATED ALWAYS AS (expression)` does not work on this box** — every
  variant fails. `GENERATED ALWAYS AS IDENTITY` is fine. Put derived values in a
  **view**, which is why all display formatting lives in `GTV*`.
- **`CURRENT SCHEMA` resolves to `*LIBL` inside `RUNSQLSTM`**, and `codermake`
  does not substitute `$LIBRARY` in `.sql` (that is a `.msgf`/`.bnddir`
  feature). A build script cannot discover its own library: name it once, then
  `ADDLIBLE`.
- **DDS `TEXT` past column 80 is silently truncated** — `CPD7508` "closing
  apostrophe missing", keyword dropped. Budget ~28 characters.
- **`EDTCDE(3)` on every numeric screen field.** Never a blank-on-zero code
  (`Z`, `2`, `4`, `B`, `D`, `K`, `M`) — a zero balance must read `0`, not blank.
- Runtime traps that compile clean: the `Date`/`DATFMT` triangle (`RNQ0114`),
  `%subst` on a `VARCHAR` (`RNQ0100`), and `%editc(int:'X')` returning hex.
- A numeric input field takes a **trailing** minus: `100.00-`, not `-100.00`.

---

## 8. Test before showing the user

In this order. Each catches something the next one cannot.

```bash
node gtwms/tools/preflight-item.js        # or the screen's own preflight
node gtwms/tools/test-photo-capture.js    # behaviour, real JS, fake camera
node gtwms/tools/test-gt-shim.js          # every deployed skin
node gtwms/tools/test-images.js           # the REAL server, the REAL ids
node gtwms/tools/test-device.js           # the phone: viewport, breakpoint, tokens
node gtwms/tools/verify-demo-sheet.js     # every barcode on the demo sheet decodes
node gtwms/tools/test-camera.js           # CLICKS the camera on all 11 screens
node gtwms/tools/lint-scan-handlers.js    # unguarded %subst of a scanned payload
node gtwms/tools/lint-cache-stamps.js     # unstamped/mismatched asset URLs
```

- **Pre-flight renders the screen offline** against the real template, real CSS
  and the declared field list, injecting Genie's hostile
  `div{white-space:nowrap;z-index:10;padding:1px}`, at 360/412/1280 in both
  languages. It audits identifiers and compares `scrollWidth` to `clientWidth`.
  It has caught real bugs a clean compile did not.
- **Hit-test anything that must persist.** Presence is not enough: a
  visible-but-covered button screenshots perfectly and does nothing. The check
  taps each control's centre point and fails if something else answers.
- **A check you have not seen fail proves nothing.** Both branches of the
  control check were confirmed by re-introducing the original faults. Do this.
- **Derive test expectations from source, never hardcode them.** This has now
  failed in both directions. A hardcoded handler list in `test-gt-shim.js` first
  reported a *false* failure when the API legitimately grew; then its hardcoded
  screen list silently reported **all checks passed while covering neither new
  Receiving screen**. A false pass is far worse than a false failure. Both lists
  are now derived — the screen list from `gen-gt-shim.js`'s exported `SCREENS`,
  the handler list from the template and the module.
- **A test harness that does not serve a shared file reports the product as
  broken.** Moving receiving's collector into `gt-edits.js` made
  `test-rcv-stepper.js` fail five checks — the harness served only
  `gt-rcvlines.js`, so `window.gtEdits` was absent. The failure looked exactly
  like a product bug. When a screen gains a shared dependency, add it to every
  harness that serves that screen, and confirm against the deployed shim
  (`test-gt-shim.js` lists the JS each screen actually gets).
- **A green suite that never touches the real server is not a green screen.**
  Every image in the application 404'd after a full rebuild while the build, the
  shim test and five pre-flights all reported success — because the ids moved
  and nothing fetched from the server the browser fetches from. `test-images.js`
  now does, and it asserts an absent id **404s**, because a check that only ever
  looks for 200 would be satisfied by a server that answers 200 to everything.
- **A leading FNC1 broke GS1 parsing, and only the demo sheet found it.**
  `GTBAR` stripped the AIM identifier (`]C1`) and converted a literal `{GS}`,
  but never a **leading** FNC1 byte — and GS1 detection tests the first two
  characters for an application identifier, so a payload beginning with `x'1D'`
  fell through to an anonymous `CODE-128` with nothing parsed. Scanners disagree
  about emitting it. Found by feeding the demo's own showpiece barcode through
  the running application in **all three prefix forms**: no prefix parsed, `]C1`
  parsed, a leading FNC1 did not. All four forms now do. The lesson is the
  method: one representative input is not a test of a normaliser, because a
  normaliser exists precisely because callers disagree.
- **A gap in a shared service shows up as a bug in its first new caller.**
  `GTBAR` only populated `result.sscc` from AI 00 inside a GS1-128, so a *bare*
  18-digit pallet label was classified `SSCC-18` and then resolved as if it were
  an item barcode. It was invisible until Receiving scanned one. Fixed in the
  service, with a unit-test case, rather than worked around in the caller.
- Chromium for headless work:
  `/home/coder/.cache/ms-playwright/chromium-1228/chrome-linux64/chrome`
  (`chrome-linux64`, not `chrome-linux`), launched with `--no-sandbox`.
  `--use-fake-device-for-media-stream` plus `--use-fake-ui-for-media-stream`
  gives a synthetic camera. Serve over `http://127.0.0.1`, because
  `getUserMedia` needs a secure context and `about:blank` is not reliably one.
  `npm i --no-save ejs playwright-core` — a fresh container has no
  `node_modules`.
- **Driving these screens headlessly: every response needs `FORMAT.ACTION=`.**
  None of these formats declares response indicators — the buttons are
  hyperlinks that set `ACTION` and press Enter. A POST carrying only the field
  value (`ITMSCAN.SCANVAL=0614141002252&aid=241`) falls through the `select`'s
  `other;` branch and the screen comes back **unchanged, with no message** —
  which reads exactly like a broken program. Pair the value with its code:
  `ITMSCAN.ACTION=SCAN&ITMSCAN.SCANVAL=…`. Codes are the `when action = '…'`
  lines in the program; row actions are `PICKnn`, and `nn` is the subfile
  sequence, not the RRN.
- Then **live on IBM i**: sign on, walk the path, sign off cleanly. Renderings
  via `genie_html.sh` embed in `summary.md`; they are 401 to the agent, so to
  actually *look* at an EJS screen, render it locally with real field values
  taken from the session JSON.

---

## 9. Ceilings and constraints — don't rediscover these

- **No CGI can reach `TIGERPOC`.** Every `/cgi-bin` ScriptAlias on this server is
  hard-wired to a library (`drpuidev` → `DRUSSO`, `puidev` → `PUICGI`) and none
  can be repointed without editing a shared `httpd.conf`. So: **DB2 is the
  system of record** (`GTIMAGE` holds journaled BLOBs) and images are **served
  as static files** exported from those BLOBs — a generated cache, safe to
  delete.
- **There is no upload endpoint either**, so a photograph travels on a
  display-file field. `IMGDATA` is `char(24000)`; base64 costs 4 bytes per 3, so
  the JPEG must come in under ~17 KB. The shrink ladder drops resolution before
  quality and refuses below 340px rather than storing mush.
- **Carousel capacity is 8 image slots.** Raising it is `MAX_IMGS` plus eight
  more field triples.
- **A nightly `AUTOBUILD` job stops the PUI dev instances at 22:00.** A verified
  endpoint can vanish mid-task and it is not your fault.
- The container sits inside `profoundnet` behind a NAT hairpin, so timeouts to
  Profound public IPs are routing, **not** proof a server is down.
- `aitool sql` needs an explicit `connection: "dev"`, and inline SQL containing
  quoted literals gets mangled — use `--input @file.json`.

---

## 10. Adding the next option — the checklist

1. `Rules.mk`: display file, program, and any service program. Order-only for
   binding directories and tables.
2. DDS JSON in `gtwms/qddssrc/` — **one format**, `EDTCDE(3)` on numerics,
   lowercase field names in mind for the template.
3. EJS + CSS in `htdocs/.../<dir>/`, EN|FR strings in the template, EN|FR toggle
   in the header, visible way out in the footer.
4. RPG in `gtwms/qrpglesrc/` following §7's house pattern.
5. Wire the menu: flip `built: false` → `true` in `menu.ejs`, add the
   `when action = '<CODE>'` branch in `gtmnur.sqlrpgle`.
6. A pre-flight for the new screen, with a `controls` list for anything that must
   persist at 360px.
7. Build, deploy all three steps, re-shim **both** skins.
8. Pre-flight → shim test → live session → sign off cleanly.
9. Record what surprised you here, in this file.

---

## 11. Status

| Option | Code | State |
|---|---|---|
| Item Lookup | `ITEM` | **built** — carousel, camera capture, 3 locations |
| Scan Lab | `SCAN` | **built** — GS1 parsing, live `BarcodeDetector` |
| Main Menu | — | **built** — live DB2 badge counts, bilingual |
| Receiving | `RECV` | **built** — pallet scan, scan-to-confirm, inline adjust, commit-controlled post. `rcvdetl` (reason codes, damage photos) and `rcvconf` (signature) still to come |
| Inventory & Move | `INV` | **built** — one scan box for location/item/pallet, bulk-vs-pick-face, replenish shortcut, commit-controlled move |
| Cycle Count | `CNT` | **built** — blind entry, variance reveal, declarative photo rule, commit-controlled post |
| Supervisor View | `SUPV` | **built** — six live figures, receiving/counting progress bars, per-zone strip, and a combined work queue where every row drills into the real screen (`GTRCLR` for a receipt, `GTCNVR` for a count) |
| Putaway | `PUTA` | **built** — the other half of Receiving: staged stock from `STAGE01` into storage, with a ranked destination suggestion, the alternatives shown, scan-to-override, and a commit-controlled book |
| Settings | `SETT` | **built** — the three preferences that have real columns (`lang_pref`, `default_zone`, `large_touch`), validated and committed, with a live large-touch preview |
