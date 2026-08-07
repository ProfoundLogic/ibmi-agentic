# Build note — camera capture (2026-08-06)

*Originally the task summary for the session that built this feature. Kept in
the repo because the reasoning behind the design decisions, and the three build
traps that cost real time, are worth not rediscovering.*

*The screen renderings in the original were harness-scoped iframes that do not
resolve outside it. The raw screen state they were generated from is committed
under `../session-screens/` and linked in their place.*

---

## Camera capture — operator photographs in the product carousel

**IBM i task library: `TIGERPOC`**

## What was asked

> Add the facility to use the device camera to add additional images that can be
> recalled when inquiring on the product again. Once uploaded the additional
> images should be visible in the carousel along with the images that already
> existed.

## What was built

An operator standing in front of the stock can now photograph it from the Item
Lookup detail screen. The photograph is stored as a journaled BLOB against that
SKU and appears in the carousel **on the same round trip**, beside the
catalogue imagery, badged so nobody mistakes a picture of a crushed carton for
what the product is meant to look like.

The catalogue image shows what a product is *supposed* to look like. This adds
what is *actually* on the pallet — damage, relabelling, how the skid is built.

### The path a photograph takes

| Stage | What happens |
|---|---|
| Tap the camera button | `<input type="file" capture="environment">` hands off to the platform camera app |
| `gt-photo.js` | decodes with EXIF orientation honoured, then resizes down a ladder until the base64 fits |
| `IMGDATA` `char(24000)` | the payload channel — there is no upload endpoint |
| `GTITDR` action `ADDIMG` | calls `GTIMG_ADD` |
| `GTIMG_ADD` | `BASE64_DECODE` → journaled BLOB in `GTIMAGE`, `ref_key` = the SKU |
| `GTIMG_PUBLISH` | writes the servable JPEG through the C runtime `open`/`write` |
| detail reloads | the photo returns through `GTVITEMIMG` like any other image |

There is **no special case in the render path**. The screen is simply re-read
after the insert, so the new photograph arrives through the same view the
catalogue imagery does and lands in the next free carousel slot.

### New objects in `TIGERPOC`

| Object | Type | Purpose |
|---|---|---|
| `GTIMG` | `*SRVPGM` | `gtimg_add`, `gtimg_publish` |
| `GTIMGSTG` | `*FILE` | test fixture — staging for the capture-test payload |
| `GTIMGTST` | `*PGM` | end-to-end test: decode → BLOB → IFS → HTTP |

### Changed

- **`GTVITEMIMG`** now unions two sources: catalogue imagery (`img_source` `'C'`,
  `ref_key` `'FAMnnnn'`, shared across a product family) with operator
  photographs (`'P'`, `ref_key` = the SKU itself). `img_group` orders them so
  the carousel still **opens on the clean product shot**. A SKU never looks like
  `FAMnnnn`, so the two arms cannot claim each other's rows.
- **`GTITDD`** — carousel grows from 4 slots to 8, each now carrying a source
  flag, plus the `IMGDATA char(24000)` payload field.
- **`GTITDR`** — handles `ADDIMG`, generates a dated bilingual caption, then
  reloads. Clears `IMGDATA` immediately so 24 KB is not shipped back to the
  browser on every subsequent screen write.
- **`gt-photo.js`** (new), **`itmdetl.ejs`/`.css`** — camera button on the
  carousel, confirmation overlay with preview and size readout, "Your photo"
  badge, image count. Bilingual EN/FR throughout.

## Design decisions worth knowing

**The payload rides on a display-file field.** Section 29 of the design doc
established that every `/cgi-bin` alias on this server is hard-wired to a
specific library, none can be pointed at `TIGERPOC`, and putting our objects in
someone else's library was rejected. So the photograph travels the only channel
that already exists. That imposes a hard ~17 KB limit on the JPEG, and forces
base64 — its alphabet is entirely invariant characters, so it survives the
field's EBCDIC translation. **The first thing verified, before any other work,
was that a display file accepts a 24,000-byte field.**

**Resolution drops before quality.** A slightly soft 800px photo of a damaged
carton is useful; a 340px one at quality 0.30 is not. The first rung that fits
wins, so a small photo keeps its full quality. Past the last rung it refuses
rather than storing mush.

**`<input type="file" capture>` rather than `getUserMedia`.** `gt-scan.js` uses
`getUserMedia` because a barcode scanner needs a live frame loop; a photograph
does not. Handing off to the platform camera app buys autofocus, exposure, HDR,
flash and tap-to-focus for free, produces a far better picture than a `<video>`
frame grab, and degrades to a file picker on a laptop with no camera.

**The RPG writes the servable file itself.** "Upload, then wait for a batch job"
is not a demo. The export tool remains for bulk rebuilds.

## Issues encountered

**The bind failed even though the export was present.** `CPD5D02: Definition
not found for symbol 'GTIMG_ADD'`, while `DSPSRVPGM` showed it exported.
`codermake` passes no `BNDDIR` to `CRTSQLRPGI`, so a Rules.mk prerequisite
controls build *order* but not the binder's *search path*. The fix belongs in
the source, where the rest of the project already does it:
`ctl-opt bnddir('GTIMG')`.

**The first capture test stored nothing, silently.** It passed base64 as a CL
`PARM('...')` literal against a `char(30000)` parameter — and CL does not pad a
literal to the receiver's declared size, so the program read 30,000 bytes out of
a 4,560-byte buffer, the length guard saw garbage, and `GTIMG_ADD` returned 0
with no error anywhere. `GTIMGTST` now takes no parameters; the payload is
staged in `GTIMGSTG` with SQL, which is also the only practical way to get
17,000 characters of base64 onto the machine.

**This container had no `codermake` build stamps, so make rebuilt the database
from scratch and the demo data was wiped.** Recovered by re-running
`gtseed.file` and reloading the imagery. That exposed a **real latent bug**: the
imagery targets depended on `gttables.file`, but `gtseed.table.sql` opens with
`DELETE FROM GTIMAGE` — so on a fresh clone make would load 102 images and the
seed would then wipe every one of them. Invisible on an incremental build,
because neither target re-runs. Corrected to depend on `gtseed.file`.

Because the identity column does not reset, reloading gave the images new IDs
(103–204), which orphaned every exported file on the IFS. Re-exported and
re-deployed; item coverage is back to **408/408**.

**`gtimgspk.file` was a dead Rules.mk target** — its source was never committed,
so any build that reached it failed outright. Removed.

## Test results

**Build:** all targets compile and bind clean.

**Shim (`test-gt-shim.js`) — all checks pass on both deployed skins (`pls`,
`Classic`).** The test was extended, because a stale JS snapshot would have
killed the camera silently while the screen still looked fine:

- `itmdetl js snapshot matches gt-carousel.js + gt-photo.js`
- `gtPhoto exposes the handlers the template calls`
- `template calls only handlers gt-photo.js defines` — `pick, chosen, use, close`

**Offline pre-flight (`preflight-item.js`)** — updated for the new fields and
the `gtitdd` path. Identifier audit clean; no overflow at 360 / 412 / 1280 px in
both EN and FR, with the overlay both closed and forced open.

**The real resize ladder**, driven through `gtPhoto.chosen()` in headless
Chromium on a 2400×1800 / 908 KB photograph: **600×450, 12.5 KB, 17,124 base64
characters** — rung 5 of 8, comfortably inside the field.

**Round trip:** the file served from `gtimg/206.jpg` is **byte-identical** to
what the client encoded — 12,841 bytes, correct JPEG magic, `Content-Type:
image/jpeg`.

## Exploratory Verification Results

Driven on a live session against `TIGERPOC`, signed off cleanly afterwards.

### Test Case 1: The captured photo appears in the carousel

Navigated main menu → Item Lookup → searched `GROC-000004` → opened the detail.
The screen returns three images where it previously held two: slots 1 and 2 are
catalogue imagery (`DSRC1`/`DSRC2` = `C`, image IDs 129 and 130), and slot 3 is
the operator photograph stored earlier by `GTIMGTST` (`DSRC3` = `P`, image ID
205, caption `Photo d'entrepôt - essai de capture`). Catalogue imagery comes
first, exactly as `img_group` specifies, so the carousel still opens on the
clean product shot. **Pass.**

> Raw screen state: [`../session-screens/screen-005.json`](../session-screens/screen-005.json)
> — the live `ITMDETL` format exactly as the program returned it.

### Test Case 2: Capturing a new photo end to end

Submitted `ACTION=ADDIMG` with a live 17,124-character base64 payload through
the real `IMGDATA` field — the same route the browser uses. The payload survived
the RDF datastream intact, `GTIMG_ADD` stored it as image ID 206, and the
reloaded detail shows it in slot 4 with `DSRC4` = `P` and an auto-generated
dated caption, `Photo d'entrepôt 2026-08-06`. The confirmation message reads
`Photo ajoutée à cet article.` `IMGDATA` came back with length 0, confirming the
24 KB payload is not shipped back on subsequent writes. Fetching
`gtimg/206.jpg` over HTTPS returned 12,841 bytes byte-identical to what the
client encoded. **Pass.**

> Raw screen state: [`../session-screens/screen-006.json`](../session-screens/screen-006.json)
> — the live `ITMDETL` format exactly as the program returned it.

### Test Case 3: An empty payload is rejected, not stored

Submitted `ACTION=ADDIMG` with `IMGDATA` blank — the shape a truncated or failed
capture would take. The program rejected it with `Aucune photo reçue. Veuillez
réessayer.` and stored nothing: `DIMG4` still holds 206 and `DIMG5` is 0, so no
stub row was created. **Pass.**

> Raw screen state: [`../session-screens/screen-007.json`](../session-screens/screen-007.json)
> — the live `ITMDETL` format exactly as the program returned it.

### Test Case 4: Captured photos are bilingual

Toggled to English on the detail screen. Both operator photographs re-read their
English captions — `Warehouse photo - capture test` and `Warehouse photo
2026-08-06` — while keeping their `P` source flags and carousel positions. The
captions are stored in both languages at capture time, not translated on
display. **Pass.**

> Raw screen state: [`../session-screens/screen-008.json`](../session-screens/screen-008.json)
> — the live `ITMDETL` format exactly as the program returned it.

### Test Case 5: Navigation is unaffected

`ACTION=BACK` from the detail returned to the search results with the query and
result count intact (`SEARCH=GROC-000004`, `NFOUND=1`) — confirming the added
capture path did not regress the one-format-per-display-file fix from section
32. Exited to the GT menu, then to the host menu, and signed off. **Pass.**

## Recommendations and next steps

1. **Carousel capacity is 8 slots.** With two catalogue images that leaves room
   for six photographs before the oldest stop appearing. Raising it is a
   `MAX_IMGS` constant and eight more field triples — deliberately not done
   until there is a reason to.
2. **No delete.** An operator can add a photograph but not remove one. For a POC
   that is arguably the safer default, but a demo audience will ask.
3. **`GTIMGSTG` is a test fixture** and could be dropped before any handover.
4. **The ~17 KB ceiling is a consequence of having no upload endpoint**, not a
   design preference. In a real deployment with a `/cgi-bin` alias — or Profound
   UI's own file-upload widget — the photograph could go up at full resolution.
   Worth saying out loud if the image quality is questioned in the demo.
5. **Next application: Receiving**, per the delivery waves in section 21.
