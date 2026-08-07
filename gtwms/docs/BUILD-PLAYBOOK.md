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
- Build logs: `tmp/logs/<target>.log`.

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

**The task library changes every task, so rebuild the shadow every task:**

```bash
rm -f build/menu.file build/menu.msgf build/menu.menu
codermake menu.menu          # default IBMI_BUILD_LIBRARY = the task library
```

Skipping the `rm` makes this a silent no-op and option 4 simply is not there.

Three dead ends, so nobody spends the afternoon again:

- `CHGJOBD JOBD(AIDEMO/AIDEMO)` → **`CPD1602 Not authorized`**. `aidemo` cannot
  change its own job description.
- Building the shadow into `TIGERPOC` → never found. `TIGERPOC` joins the
  library list only *after* option 4 runs `GTSTART`.
- Writing to `AIDEMOBASE` → off limits, shared.

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
- **Move the header status when a detail row moves.** Nothing lifted the receipt
  out of `OPEN`, so it read "Not started" on the screen and in the list however
  many lines had been confirmed — only the final post ever changed it. Do it
  inside the same commit boundary as the line.

### Every screen needs a visible way out

An F-key alone is not enough — on a phone there is no keyboard. Every screen
carries visible buttons (`data-fkey` mirrors the key for desktop) **and** the
F-key. Item Lookup shipped once with only "Scan another", which was covered by
the format underneath; the lesson is in §32 of the design doc.

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
- **One statement per line, everywhere** — `RNF5508`.
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
| Inventory & Move | `INV` | not started |
| Cycle Count | `CNT` | not started |
| Supervisor View | `SUPV` | not started |
| Putaway | `PUTA` | not started |
| Settings | `SETT` | not started |
