# Hornady POC — EJS Rich Display options (11–15)

The original Hornady-menu options (1–6) target classic 5250 displays or
Profound UI's rich Display File widgets.  Options **4 (`PICKBATR`)** and
**5 (`PICKERR`)** render blank on this PUI install due to a server/
client fix-pack mismatch — see `rebuild-guide.md §15`.  Options 11–15
are EJS Rich Display clones of options 1–5 that bypass the
widget-runtime parser and render correctly out-of-the-box.

This document is the operating manual for that EJS layer.  Read it
end-to-end if you need to rebuild, extend, or debug any of the EJS
options.

| Option | Program     | Mirrors    | Function                                   |
|---|---|---|---|
|  1 | HYR0600     | —          | Shipment Processing (5250, partial)        |
|  2 | HYR0138     | —          | Pallet Contents Maint. (5250, blocked)     |
|  3 | HYC0606     | —          | Shipment Lot Inquiry (5250, working)       |
|  4 | PICKBATR    | —          | Pick Batch Dashboard (PUI Rich, broken)    |
|  5 | PICKERR     | —          | Picker Workflow (PUI Rich, broken)         |
| **11** | **HYR0600EO** | Option 1  | Shipment Processing (EJS) ✅              |
| **12** | **HYR0138EO** | Option 2  | Pallet Contents Maint. (EJS) ✅           |
| **13** | **HYR0606EO** | Option 3  | Shipment Lot Inquiry (EJS) ✅             |
| **14** | **PICKBATEO** | Option 4  | Pick Batch Dashboard (EJS) ✅             |
| **15** | **PICKEREO**  | Option 5  | Picker Workflow (EJS) ✅                  |

Every EJS option follows the same source / build / deploy pattern.

---

## 1. Anatomy of an EJS option

Each EJS option is three things on the IBM i side and three things on
the web server side:

```
src/                                  htdocs/profoundui/userdata/ui/
├── {name}.json     ← RDF metadata    └── {name}/
├── {name}.sqlrpgle ← screen driver     ├── {name}.ejs   ← HTML template
└── Rules.mk entry                      ├── {name}.css   ← styling
                                        ├── {name}.js    ← screen-side JS
                                        └── hornady-logo.png
```

| Layer | What it does | Where it runs |
|---|---|---|
| `.json` | Declares the EJS Rich Display File: record formats, fields, subfile fields, and the URLs of the template / CSS / JS. | Compiled into a `*FILE` on IBM i by codermake (`rdf-to-dds.js` → DDS → `CRTDSPF`). |
| `.sqlrpgle` | Workstn-handler RPG that issues SQL, fills control + subfile records, and calls `exfmt`. Uses `HANDLER('PROFOUNDUI(HANDLER)')`. | Compiled into a `*PGM` on IBM i; runs in the PUI session's interactive job. |
| `.ejs` | Server-rendered HTML template. Iterates control fields and subfile rows via `<%= field %>` and `forEach`. | Rendered by the PUI client runtime in the user's browser. |
| `.css` | Hornady-branded styling (red `#C8102E`, black `#0A0A0A`, brass `#B08D57`, Oswald + Roboto via Google Fonts). | Loaded by the browser. |
| `.js` | Screen-side behaviour: search submit, modals, map render, scan dispatch. | Loaded by the browser. |

---

## 2. Build flow

```
src/{name}.json
   │  codermake (rdf-to-dds.js)
   ▼
tmp/{name}.dspf
   │  CREATE_SOURCE_MEMBER + crtdspf
   ▼
$LIBRARY/{name}.FILE   (display file with screen metadata baked in)

src/{name}.sqlrpgle    +   $LIBRARY/{name}.FILE
   │  CRTSQLRPGI (compile-and-bind)
   ▼
$LIBRARY/{name}.PGM
```

`Rules.mk` declarations for the five EJS options:

```makefile
hyr0600eo.file:  hyr0600eo.json
hyr0600eo.pgm:   hyr0600eo.sqlrpgle hyr0600eo.file | hypsgcu.file hypsghd.file hypsgdt.file

hyr0138eo.file:  hyr0138eo.json
hyr0138eo.pgm:   hyr0138eo.sqlrpgle hyr0138eo.file | hyppllt.file hdcust.file

hyr0606eo.file:  hyr0606eo.json
hyr0606eo.pgm:   hyr0606eo.sqlrpgle hyr0606eo.file | hypsgld.file hypsgcu.file

pickbateo.file:  pickbateo.json
pickbateo.pgm:   pickbateo.sqlrpgle pickbateo.file | pickbathp.file

whloc.file:      whloc.table.sql
pickereo.file:   pickereo.json
pickereo.pgm:    pickereo.sqlrpgle pickereo.file | pickbathp.file pickbatdp.file whloc.file
```

Drop + rebuild a single option after edits:

```bash
ssh dev '/usr/bin/qsh -c "system \"dltf  AITSK00054/PICKEREO\" 2>&1 | head -1"'
ssh dev '/usr/bin/qsh -c "system \"dltobj OBJ(AITSK00054/PICKEREO) OBJTYPE(*PGM)\" 2>&1 | head -1"'
rm -f /workspace/build/pickereo.file /workspace/build/pickereo.pgm \
      /workspace/ibmi-agentic/build/pickereo.file /workspace/ibmi-agentic/build/pickereo.pgm
codermake pickereo.pgm
```

(Codermake leaves marker files in both `/workspace/build/` and
`/workspace/ibmi-agentic/build/`; deleting just one isn't enough to force
a rebuild when both are present.)

---

## 3. **`SET OPTION COMMIT=*NONE`** — required for non-journaled INSERTs

This is the single most painful gotcha in the EJS layer.  Read it once
and never burn an hour to it.

`codermake`'s `BUILD_SQLRPGLE_PGM` recipe runs `CRTSQLRPGI` without an
explicit `COMMIT` parameter, so the IBM default `COMMIT(*CHG)` applies.
Our seed tables are **not journaled**, so every `INSERT` / `UPDATE` /
`DELETE` goes into a commitment scope that is **never explicitly
committed**.  The symptom is unmistakable:

* the program's own cursor reads the uncommitted row fine
  (read-your-own-writes within the session), so the screen *looks*
  correct mid-flow,
* a singleton `SELECT INTO ... WHERE PICKBAT = :x` for the same row
  returns `SQLCODE=100` (no row found),
* the moment the program exits, IBM i rolls everything back, and the
  next session opens to an unchanged table.

**Fix:** put this block immediately after the `dcl-f` for the workstn
file in every EJS SQLRPGLE that mutates non-journaled data.  Placement
matters — it must come **after** `dcl-f` so the SQL precompiler can see
workstn host variables, but **before** any other `exec sql`.

```rpgle
dcl-f pickereo workstn sfile(batsfl : rrn) handler('PROFOUNDUI(HANDLER)');

exec sql
  set option
    naming    = *sys,
    commit    = *none,
    datfmt    = *iso,
    dynusrprf = *user,
    closqlcsr = *endmod;
```

Currently in place: `pickbateo.sqlrpgle`, `pickereo.sqlrpgle`.
**Not yet applied** (open follow-up): `hyr0138eo.sqlrpgle` mutates
`HYPPLLT` and will silently lose CREATE/CLOSE/REOPEN/TOGGLE/DELETE
writes the moment that table is ever journaled.

Saved as memory `[[sqlrpgle-commit-default]]` for future tasks.

---

## 4. Asset deployment — *not* via codermake

codermake compiles the IBM i objects but does **not** push the
`htdocs/profoundui/userdata/ui/{name}/` assets.  The user's browser hits
`$IBMI_PUI_SERVER` (`https://idev.profoundlogic.com:8103` on this
environment) whose DocumentRoot is `/home/drusso/puidist/htdocs`.  Push
manually after every edit:

```bash
scp htdocs/profoundui/userdata/ui/pickereo/{picker.ejs,picker.css,picker.js} \
    dev:/home/drusso/puidist/htdocs/profoundui/userdata/ui/pickereo/

# Always verify the new asset is reachable before declaring it done.
curl -sIk "$IBMI_PUI_SERVER/profoundui/userdata/ui/pickereo/picker.js?v=12" | head -1
# HTTP/1.1 200 OK
```

For options that scaffold a new directory, create it first:

```bash
ssh dev '/usr/bin/qsh -c "mkdir -p /home/drusso/puidist/htdocs/profoundui/userdata/ui/pickereo"'
```

Saved as memory `[[ejs-deployment-needs-correct-puidocroot]]`.

### Cache-busting

The `drpuidev` Apache instance does **not** set `Cache-Control: no-cache`
on static `.ejs/.css/.js`, so the user's browser will happily serve a
stale copy even after `curl` confirms the new content is live.  Every
URL in `{name}.json` carries a `?v=N` suffix:

```json
"template": "/profoundui/userdata/ui/pickereo/picker.ejs?v=7",
"css":      ["/profoundui/userdata/ui/pickereo/picker.css?v=8"],
"js":       ["/profoundui/userdata/ui/pickereo/picker.js?v=12"]
```

**Bump the version of every asset you edit.**  Because the URLs are
compiled into the screen metadata inside the `*FILE`, you must also
rebuild the `.file` (and therefore the `.pgm`) so the new URL ships on
the next handler call.  See §2 for the drop + rebuild commands.

Saved as memory `[[ejs-template-needs-cache-bust]]`.

### Other deployment gotchas

* **No inline `<script>` blocks inside `.ejs`** — the PUI runtime
  injects the template via `innerHTML`, and browsers do not execute
  `<script>` inserted that way.  Put behaviour in the separate `.js`
  file referenced from the JSON.  Inline `onclick="..."` attributes DO
  work because the browser parses them when the element is added to
  the DOM.  Memory: `[[ejs-inline-script-doesnt-run]]`.
* **`<link rel=stylesheet>` and `<script src>` load fine** through the
  CoderFlow proxy, but **XHR fetches to `userdata/*` are blocked**.
  This is why the template lives in the JSON's `template` field
  (proxy-friendly) rather than being fetched at runtime.  Memory:
  `[[ejs-userdata-path]]`, `[[feedback-pui-ejs-proxy-xhr]]`.

---

## 5. Known generator + DSPF caveats

| Caveat | Where it bites |
|---|---|
| **Two subfiles per format** — codermake's `rdf-to-dds.js` emits both `SFL` records first and both `SFLCTL` records last; `CRTDSPF` then errors `CPD7835 / CPD7836`.  | pickbateo uses one `BATSFL` + a `detjson` *string* field to ship the View modal's drill-in data (no second subfile).  Memory: `[[codermake-rdf-two-subfiles-bug]]`. |
| **Field name length 10** — the generator silently truncates JSON field names to 10 chars in the DDS, so RPG references to longer names fail `RNF7030`. | `completedct` → `donect` in pickbateo.json. |
| **Action field length** — `action char(10)` truncates `CREATE_MEGA` (11) and `DISSOLVE_MEGA` (13).  `when action = 'CREATE_MEGA'` then silently never matches. | pickbateo / pickereo use `"action": { "type": "char", "length": 15 }`. |
| **DSPF constant overlap** — `CRTDSPF` does *not* error when two text constants overlap on the same row; the second is silently dropped from the rendered screen. | `hornady.dspf` right-column entries (options 11–15) must start at col 52 to clear the longest left-column constant.  Memory: `[[dspf-constant-overlap-silent-drop]]`. |
| **Free-form RPG one-statement-per-line** — `when stat='O'; return 'Open';` on a single line fails `RNF5508`. | Every per-action branch in pickbateo / pickereo / hyr0138eo is on its own line. |
| **SQL host-variable array refs** — `:array(i).field` is rejected in `FETCH INTO`; use a single-row DS intermediate then assign into the array. | All `FETCH INTO :row;` patterns followed by `array(i) = row;`. |
| **`EXISTS` inside `CASE WHEN`** — DB2 for i precompiler rejects this in column position.  | Rewrite as `LEFT JOIN (... GROUP BY)` with `CASE WHEN COALESCE(KIDCT,0) > 0` on the join column. |

---

## 6. Architecture features per option

### Option 11 — `HYR0600EO` Shipment Processing

* Dashboard over `HYPSGCU` joined with `HYPSGHD` for status + DC.
* Stat row: total / open / completed / DC counts / items + weight.
* Filter row: status (O/C), DC (WEST/ALDA/GI), free-text matching group #,
  customer name, or ship-via.
* Grid (scrollable, sticky header, 12 rows visible) with a row-level
  View icon.
* View modal pulls `HYPSGDT` lines for the selected group into a JSON
  field and `picker.js` renders them client-side.
* Bypasses HYR0600's stuck Employee Number Prompt loop.

### Option 12 — `HYR0138EO` Pallet Contents Maintenance

* CRUD over `HYPPLLT` joined with `HDCUST` for the ship-to name.
* Stat row: total / open / closed / hazmat / total cubic feet.
* Per-row actions: 🔒 Close, 🔓 Reopen, ⚠️ Toggle hazmat, 🗑 Delete.
* "+ New Pallet" modal: ship-to dropdown sourced from `HDCUST`,
  L × W × H inputs (default 48 × 40 × 48), hazmat checkbox.  Serials
  auto-assigned in the 9001–9999 range.
* Replaces the 5250 option 2 which is blocked by a missing `HYD0138.DSPF`.
* **TODO:** add `SET OPTION COMMIT=*NONE` (see §3).

### Option 13 — `HYR0606EO` Shipment Lot Inquiry

* Single-page lot inquiry over `HYPSGLD` with a group # selector +
  optional item filter (default group 1001 matches the demo path).
* Stat row: group / lot count / distinct items / total qty.
* Grid columns: group, ship-to, turn, sequence, item, lot, qty, SSCC
  barcode.

### Option 14 — `PICKBATEO` Pick Batch Dashboard

* Dashboard over `PICKBATHP` with status / DC / picker filters.
* Per-row icon cluster: 🔍 View detail, ✎ Assign picker, ▶ Start, ⏸ Pause,
  ✓ Complete, 🗑 Delete.  Buttons disable themselves when the row's
  status doesn't permit the action.
* **Mega batches:** check 2+ rows in the grid → floating action bar
  appears → "Bundle into Mega Batch" inserts an umbrella PICKBATHP row
  (9001+) and links each child via PICKMBATDP.  The cursor's
  `NOT IN (SELECT PICKBAT FROM PICKMBATDP)` clause then hides the
  children automatically; the umbrella shows with a brass `MEGA ×N`
  badge.
* View on a mega umbrella renders a modal with the constituent batch
  list (sourced from PICKMBATDP) and the COMBINED pick lines from all
  children's PICKBATDP.  Dissolve releases the children.
* Detail rows are passed as a 3000-byte `detjson` string because the
  generator can't emit a second subfile (see §5).

### Option 15 — `PICKEREO` Picker Workflow

A three-phase mobile-style workflow:

| Phase | View | Actions |
|---|---|---|
| **L** Login | Picker code + DC quick-pick (PICKER01-08 tiles, WEST/ALDA/GI buttons) | LOGIN, EXIT |
| **B** My Batches | Card-per-batch list filtered to logged-in picker, hides mega children, shows umbrella with `MEGA ×N` badge, per-card Picked / Pieces progress bar | OPEN, START, COMPLETE, LOGOUT |
| **D** Batch Detail | Per-line stop circle (walk position), tote badge (`T01`–`T0N` in mega), item, location, R/B/S coordinate chip, qty input + Scan / Scan all | SCAN, SCANFULL, COMPLETE, BACK |

#### Tote routing (mega-aware SCAN)

* When a mega umbrella is opened, `loadDetail` builds one JSON line per
  child batch's `PICKBATDP` row, ordered by walk position, with each
  line carrying its child batch number and a tote ID
  (`T01`+ based on the child's position in the mega via
  `ROW_NUMBER() OVER (PARTITION BY PICKMBAT ORDER BY PICKBAT)`).
* SCAN passes `selchild` (the constituent batch # that owns the line)
  alongside `selseq`.  The RPG updates `PICKBATDP` keyed on
  `(:scanTarget, :selSeq)` so the right child's row is incremented even
  though the umbrella is on screen.

#### 3D warehouse location & route map

`WHLOC` (`AITSK00054.WHLOC`) — 12 row × 60 bay × 4 shelf coordinate
master keyed `(WHITEM, WHDC)`.  `loadDetail` `LEFT JOIN`s `WHLOC` on
`(PICKITEM, PICKINVLOC)` to emit `whRow / whBay / whShelf` per pick
line.

Each detail line on screen shows a brass-pill R/B/S coordinate chip.

The **Show Map** button on the detail header opens an overlay with two
modes:

* **Route** — SVG 12 × 60 grid with paired-row aisles (rows 1+2 share
  aisle A1, rows 3+4 share A2, …), brass-shaded aisle lanes between
  each pair, wider inter-aisle gaps between pair groups.  Picker enters
  at a green **S** dot at R01 / B01.  Path runs along the **aisle
  lanes** (the brass bands) with short perpendicular spurs to each pick
  dot inside its rack cell.  Stops are tote-coloured circles numbered
  in walk order.  U-turns at the rack ends connect consecutive aisles.
* **Heatmap** — same grid, cells coloured white → red by sum of `need`
  quantities binned per (row, bay).

The walk order is computed client-side in `planRoute()` (a small DP
over `(aisle, entry edge, traversal type)`) and consumed by both
`renderLines()` (which reorders the item list to match) and
`renderMap()` (which draws the path).  Same numbering across both
views.  The DP chooses between:
* **Through** — walk the full aisle from one edge to the other.
* **In-and-out** — walk in to the deepest stop and back out the same
  side.  Worth it when stops cluster near one edge.
The picker always enters the warehouse at top-left (R01 / B01), so
`startEdge = 0` is forced; subsequent direction choices are
DP-optimised.

---

## 7. Seed data

Three idempotent SQL files in `Hornady/documentation/`:

| File | What it loads | Required for |
|---|---|---|
| `sample-data.sql` | Original 5-batch / 30-shipment / 6-SKU seed (Hornady-themed). | All EJS options. |
| `sample-data-more.sql` | Expansion: +5 employees → 8 total, +12 customers → 20, +14 SKUs → 20, +50 shipments → 80, +25 pick batches → 30, +84 pick lines → 98. | Volume for the option-11 / 14 / 15 grids and the picker workflow. |
| `sample-data-whloc.sql` | 60 rows in `WHLOC` — every SKU × every DC mapped to a deterministic (row, bay, shelf) coordinate clustered by category (handgun rows 1–3, rifle 4–7, shotgun 8–9, rimfire 10, components 11–12). | Option 15's R/B/S chips + warehouse map. |

Load them via `runsqlstm` over SSH:

```bash
for f in sample-data.sql sample-data-more.sql sample-data-whloc.sql; do
  cat Hornady/documentation/$f | ssh dev "cat > /tmp/hornady/$f"
  ssh dev "/usr/bin/qsh -c \"setccsid 1208 /tmp/hornady/$f > /dev/null 2>&1;
    system 'runsqlstm srcstmf(\\\"/tmp/hornady/$f\\\")
                       commit(*none) dftrdbcol(AITSK00054) errlvl(40)'\""
done
```

Final row counts after all three: see the table above.

### One-time pallet seed (option 12)

`HYPPLLT` is empty after the table is created; the original
`sample-data.sql` does **not** load it.  The smaller script in
`rebuild-guide.md §5` adds 5 demo pallets (serials 9001–9005, mixed
status, one hazmat).  Run once on a fresh `AITSK00054`.

---

## 8. End-to-end rebuild on a fresh task library

```bash
# 1. Build everything
cd /workspace/ibmi-agentic
codermake                                            # ~120s on a cold libl

# 2. One-time IBM i setup (DATABASEID, HYHDSDATA, GS1COMP, pallet seed)
#    See rebuild-guide.md §5.

# 3. Seed data
for f in sample-data.sql sample-data-more.sql sample-data-whloc.sql; do
  cat Hornady/documentation/$f | ssh dev "cat > /tmp/hornady/$f"
  ssh dev "/usr/bin/qsh -c \"setccsid 1208 /tmp/hornady/$f > /dev/null 2>&1;
    system 'runsqlstm srcstmf(\\\"/tmp/hornady/$f\\\")
                       commit(*none) dftrdbcol(AITSK00054) errlvl(40)'\""
done

# 4. Deploy htdocs to the PUI server (note: NOT done by codermake)
for d in hyr0600eo hyr0138eo hyr0606eo pickbateo pickereo; do
  ssh dev "/usr/bin/qsh -c \"mkdir -p /home/drusso/puidist/htdocs/profoundui/userdata/ui/$d\""
  scp htdocs/profoundui/userdata/ui/$d/* \
      dev:/home/drusso/puidist/htdocs/profoundui/userdata/ui/$d/
done

# 5. Verify
curl -sIk "$IBMI_PUI_SERVER/profoundui/userdata/ui/pickereo/picker.js?v=12" | head -1
# HTTP/1.1 200 OK
```

Sign in to Genie at `$IBMI_PUI_SERVER`, navigate `MENU → 5 → 11..15`,
each EJS option should render.

---

## 9. Open follow-ups

| Item | Status |
|---|---|
| `hyr0138eo.sqlrpgle` add `SET OPTION COMMIT=*NONE` | Open — without it pallet CREATE/DELETE could silently roll back if `HYPPLLT` is ever journaled. |
| Codermake `rdf-to-dds.js` two-subfile SFL/SFLCTL ordering bug | Reported via memory `[[codermake-rdf-two-subfiles-bug]]`; until fixed, every EJS option needing a "second list" uses the JSON-string-field workaround. |
| Persist tote IDs to `HYPSGDT.GDTOTE` at CREATE_MEGA time | Tote ID is currently derived at SELECT time via `ROW_NUMBER()`; persisting makes it durable for downstream pack/ship. |
| Replace `HYR9960`/`HYR9962` stubs with real barcode parsing | Promote `BARDATA` / `BARCUST` PFs from the HornadyDemo package, then swap in the real source bodies. |
| Snake-shape Manhattan route (4-aisle middle cross) | Current DP only supports entry at warehouse ends.  If a real warehouse has cross-aisles in the middle, the DP needs to expand its state space. |
| Map dot hover → item-code tooltip | Suggested UX add — `picker.js` can attach a `<title>` element to each stop circle. |

---

## 10. Quick reference — version stamps in source today

| Asset | Current `?v=` |
|---|---|
| `pickereo/picker.ejs` | 7 |
| `pickereo/picker.css` | 8 |
| `pickereo/picker.js`  | 12 |
| `pickbateo/dashboard.ejs` | 3 |
| `pickbateo/dashboard.css` | 3 |
| `pickbateo/dashboard.js`  | 3 |
| `hyr0600eo/shipments.{ejs,css,js}` | 2 (template), 2, 2 |
| `hyr0138eo/pallets.{ejs,css,js}` | 1 |
| `hyr0606eo/lots.{ejs,css,js}` | 1 |

Bump these every time you edit the corresponding file.
