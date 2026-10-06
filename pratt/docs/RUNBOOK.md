# Runbook: building Profound UI Rich Display files for Pratt Industries

Everything learned while making Pratt's `puitests` bundle build and run on the
Profound Logic demo system. Written so a new CoderFlow task can pick this up cold.

If you only read one thing: **you cannot see a Rich Display screen from inside the
container unless you render it yourself.** `tools/render-screen` does that. Use it
before you tell anyone a screen is fixed. Several hours were lost here reasoning
about CSS that could not be observed.

And the corollary, learned the hard way three times: **anything that must survive
the hosting page belongs in the screen JSON, not in a separate asset file.** Text
colour goes in a `color` widget property (§8b); helper JavaScript goes in the
`onload` prelude (§7.1). Assets under `htdocs/profoundui/userdata/` do not exist
on the IBM i until the task is approved, and naming a missing script in
`external javascript` stops the screen's `onload` running at all.

| | |
|---|---|
| **1** | [Who and what](#1-who-and-what) — the ask, the conversion template, the four house mechanisms |
| **2** | [Environment and build](#2-environment-and-build) |
| **3** | [Rebuilding the database from the code](#3-rebuilding-the-database-from-the-code) |
| **4** | [Rich Display File anatomy](#4-rich-display-file-anatomy-and-how-to-edit-one-safely) — the DDS-embedded JSON |
| **5** | [Look and feel](#5-look-and-feel-pratts-design-system) — geometry, panels, buttons, grids, type, colour |
| **6** | [How a screen is wired to the RPG](#6-how-a-screen-is-wired-to-the-rpg) — bound values, behaviour properties, events |
| **7** | [Asset wiring](#7-asset-wiring-make-the-screen-self-sufficient) |
| **8** | [Two hosting-page problems](#8-two-hosting-page-problems-and-how-to-recognise-them) |
| **9** | [Seeing a screen](#9-seeing-a-screen-toolsrender-screen) |
| **10** | [Verifying against the running system](#10-verifying-against-the-running-system) |
| **11** | [Demo data](#11-demo-data) |
| **12** | [Building a new screen from scratch](#12-building-a-new-rich-display-screen-from-scratch) — **start here for new work** |
| **13** | [Known traps, condensed](#13-known-traps-condensed) |
| **14** | [Still missing from Pratt](#14-still-missing-from-pratt--ask-for-these) |
| **15** | [Reconstructed helpers](#15-reconstructed-helpers) |

---

## 1. Who and what

**Pratt Industries** — America's 5th largest corrugated packaging company, "the
green box", vertically integrated: 19 recycling facilities feed six 100% recycled
paper mills (Conyers GA, Staten Island NY, Valparaiso IN, Shreveport LA,
Wapakoneta OH, Henderson KY), which feed 21 corrugating plants, 25 converting
plants and 17 retail-display facilities. Seven named markets: automotive,
beverage, cold chain, ecommerce, agriculture, pizza, protein.

**Contact:** Harel Davolt (`hdavolt@prattindustries.com`), to Isaac Thibault,
2 Oct 2026. cc Todd McCutcheon, Corey Cranmer, Ken J. Chouinard.

**The ask, verbatim:** *"If we could see the old managers converted into the new
apps in a consistent way, that would be the perfect use case."*

**They have already defined the target.** Do not invent a structure:

| | What | Maturity |
|---|---|---|
| `OMMGR` | 1992 RPG III "Manager" green screen (search / detail / update), used across the whole WMS, plus Genie overlays | Level 0 — the input |
| `PMSELUI` | New Profound UI search screen only; detail macros back to OMMGR through Genie | Level 1 — halfway |
| `POAPPUI` | Fully converted: native search (`POSELUI`) **and** native detail/update (`PODTLUI`) | **Level 2 — the target** |

### The conversion template is PODTLUI's procedure list

Two mirrored lifecycles, header and line. Reuse these names:

| Header | Line |
|---|---|
| `LoadRecord` | `LoadLines` / `ReadSubfile` |
| `SetAllowedModes` | — |
| `UpdateMode` | `LineUpdateMode` |
| `SetUpdateFields` | `SetLineUpdateFields` |
| `ShowChanges` | `ShowLineChanges` |
| `VerifyRecord` | `VerifyLine` |
| `PreSaveRecord` | `PreSaveLine` |
| `SaveRecord` | `SaveLine` |
| `PostSaveRecord` | `PostSaveLine` |
| `SaveHeaderText` | `SaveLineText` |
| `CheckModifiedRecord` | `CheckModifiedLines` |
| `NewRecord` | `NewLine` |

Business operations stay outside the lifecycle: `CopyPO`, `SendPO`, `PO2WIP`,
`CRTWIP`, `UPDVY`, `UPDVO`, `UPDVP`, `MQUPD`.

### The four house mechanisms to preserve

1. **Field-level pre-save validation by naming convention.** `VerifyRecord`
   documents it in its own header comment:

   ```
   ...ERR  show error for field        ...ERF  ERFINFO (warn) or ERFERROR (block)
   ...ERM  error message               ...UPD  field is on in update mode
   ...TT   tool tip                    ...DIS  disabled
   ...CHG  changed                     ..SAV   DS holding the pre-change record
   VFYERR  fatal - do not allow update
   ```
   PODTLUI carries 55 `*DIS`, 43 `*CHG`, 30 `*ERR`/`*ERM`, 24 `*TT`. The search
   screens barely use it — that density is what distinguishes a Level 2 app.

2. **Read-only vs update-capable is ONE widget.** `styles.css` defines
   `.comboInput` to look like PUI's `input` when enabled and like `outputField`
   when disabled (`:disabled.comboInput` + `appearance:none` + `unset`). The
   widget's `disabled` property binds to a `*DIS` indicator the RPG drives. No
   paired widgets, no visibility juggling.

3. **One prepared statement, many optional filters.** Every search is a single
   fixed `WHERE` with parameter markers, prepared once:

   ```sql
   AND OHPP   = COALESCE(NULLIF(?, ''), OHPP)
   AND ((? = 1 AND OHCODE = 'I') OR (? = 1 AND OHCODE = 'D'))
   AND OHRDTE BETWEEN COALESCE(NULLIF(?,'0001-01-01'), OHRDTE)
                  AND COALESCE(NULLIF(?,'0001-01-01'), OHRDTE)
   AND LOCATE(TRIM(COALESCE(?, '')), COALESCE(ORSORDR, '')) > 0
   ```
   POSELUI has 32 markers, PMSELUI 33. Only `ORDER BY` is dynamic, built by
   `SQLORDER` from three drop-downs fed by the `HELP` table. Results capped at
   `SFLMAX = 1000` with a tooltip when the cap is hit.

4. **Saved searches with zero per-field code.** `SRSAVE.onclick` does
   `Array.from(document.querySelectorAll(".search")).map(n => saveobj[n.id] = get(n.id,true))`
   and stores the JSON in `PSRVAL`. **Any field tagged with the `search` CSS class
   joins the saved search automatically.** `restoreSearch()` in `settings.js`
   reverses it. `PUISRCHUI` is the service.

### The pager

`CTLPREV`/`CTLNEXT` (PageUp/PageDown) sit on `@RET` (ordinal within the *search
result set*) and `@SIZ` (total), passed in from the search screen. The detail
shows `n/N` in `CTLCTR`, wraps at both ends, greys the buttons when `@SIZ <= 1`,
and returns `@RET` so the caller re-positions.

---

## 2. Environment and build

- Repo: `/workspace/ibmi-agentic` (**verify** — the path has varied between task
  containers; `find /workspace -maxdepth 4 -name .git` first).
- Pratt work lives in `pratt/`, a sibling of `cfdemo/`. Web assets live in the
  repo-wide `htdocs/profoundui/userdata/`, which mirrors the PUI document root.
- Target library: `$IBMI_BUILD_LIBRARY` (was `AITSK00086`). **Check it is set
  before planning any build work** — a retried task can start with it unset.

```bash
./pratt/build.sh                 # build everything under pratt/
./pratt/build.sh --list-targets  # what is outstanding, pratt only
./pratt/data/load.sh             # regenerate and load the demo data
```

**Never run bare `codermake` in this repo.** The shared `build/` stamp directory
has no stamps for `cfdemo` and `awdemo`, so a bare run would also create
`cfdemo`'s `CUSTP` as an **empty** file in the task library, where it shadows the
populated copy in `AIDEMOBASE`. `build.sh` always names pratt targets explicitly.

Other build facts:

- `CREATE OR REPLACE TABLE` handles compatible DDL changes in place. An
  incompatible column type change gives **SQL0190** — drop the object first, and
  **drop dependents before tables** (logical files and display files before the
  physical they are built over, or `DLTF` fails and you will not notice).
- `CRTDSPF` has no `REPLACE`, so rebuilding a display file whose object exists
  fails with **CPF5813**. Delete the object and remove its `build/` stamp.
- `codermake` has **no `*CMD` build type.** If a CL needs a command that does not
  exist here, you cannot supply it — compiling by hand is off limits. Comment the
  dependency out and document it.
- A source with embedded SQL must be named `.sqlrpgle`, not `.rpgle`, or it is
  built with `CRTBNDRPG` and fails. Pratt ship `PMSELUI` as `.rpgle` with 12
  `EXEC SQL` statements; it is renamed here.

---

## 3. Rebuilding the database from the code

Pratt sent **no database definitions.** Thirty tables, one view and seven DDS
logical files were reconstructed from how the supplied code uses them.

### Where the field types come from, in increasing authority

1. **Literal comparisons.** `OHSTAT = 'H'` ⇒ `CHAR(1)`. Beware names that lie:
   `VRCCOD` / `VOCCOD` / `MQCCOD` look like a CHAR(3) "currency code" but are
   compared against `1, 4, 5, 6, 7, 10` ⇒ `DECIMAL(2,0)`.
2. **The program's own `DCL-PR` prototypes.** A non-`CONST` parameter must match
   the actual exactly, so a prototype is a type declaration for the caller's
   field. `SAPPOINT(... @OH# ZONED(6))` called as `SAPPOINT(OHYY:OHPP:OH#)` proves
   `ORHDR.OH#` is `NUMERIC(6,0)`, not `DECIMAL`. **CONST char parameters accept a
   shorter actual**, so `@OIPART CHAR(30) CONST` does *not* prove `OIPART` is 30 —
   it is 25. A `DCL-S xxxP PACKED(n) // for conversion into YYY` comment means the
   file field is zoned.
3. **The compiler, iteratively.** `EXTPGM` prototypes need no object at compile
   time, so compile the big program immediately and harvest every
   `RNF7416` (type mismatch), `RNF7421` (bad operand), `RNF7535` (parameter
   mismatch) and `RNF7030` (undefined name — that is a *field of a file you have
   not modelled yet*). About twenty field types came from this loop alone, and
   `RNF7030` revealed five columns (`STREET`, `STCTST`, `PAYOTQ`, `ORG`, `LCACCT`)
   that exist only in the code.

### Byte positions are load bearing

`PODTLUI` declares externally described data structures with `POS()` overlays:

```
DCL-DS VRDS EXTNAME('VENDPART':'VRFMT':*ALL) INZ;
  MAJMIN CHAR(6)       POS(103);
  UOM    CHAR(4) DIM(5) POS(310);
  QTY    PACKED(7) DIM(5) POS(372);
  CST    PACKED(11:2) DIM(5) POS(392);
  VRF    PACKED(11:2) DIM(5) POS(437);
  VRA    PACKED(11:2) DIM(5) POS(467);
END-DS;
```

Four tables must place fields on exactly those bytes. The DDL achieves it with
explicit `*FILL*` columns, verified with `DSPFFD`:

| File | Record length | Overlay offsets |
|---|---|---|
| `VENDPART` | 496 | 103, 310, 372, 392, 437, 467 |
| `VENDPACO` | 193 | 68, 88, 133, 163 |
| `VENDPABLF` | 535 | 152, 411, 431, 476, 506 |
| `VENDSAZZ` | 410 | 21 |

Buffer sizes on IBM i, confirmed by `DSPFFD` rather than assumed:

| SQL type | Bytes |
|---|---|
| `CHAR(n)` | n |
| `DATE` | 10 |
| `TIME` | 8 |
| `DECIMAL(p,s)` (packed) | `CEIL((p+1)/2)` |
| `NUMERIC(p,s)` (zoned) | p |

Changing one such column's size shifts everything after it — grow the preceding
filler by the same number of bytes, then re-check `DSPFFD`.

```bash
ssh dev 'system "DSPFFD FILE(AITSK00086/VENDPART) OUTPUT(*PRINT)"' | \
  grep -E "Record length|^ +VR"
```

### Other DDL facts

- `#` and `$` are valid in ordinary SQL identifiers on IBM i: `OH#`, `OMEST$`,
  `MQ#OUT` all work in `CREATE TABLE`.
- `RCDFMT xxx` pins the record format name so native RPG I/O resolves unchanged.
- Declare everything `NOT NULL WITH DEFAULT`; native RPG I/O on nullable columns
  needs `ALWNULL(*USRCTL)` and will otherwise fail.
- **Keyed access paths that RPG opens natively stay as DDS logical files.** An SQL
  index cannot be opened for record-level I/O and cannot rename a record format.
  Base tables and views are SQL DDL; `ORHDRL4`, `ORITML5`, `ORMSRR`, `ORECT`,
  `VENDPABL6`, `WIPJOBH`, `LOCXREFLF` are `.lf`.
- `DSPFFD`'s "Format level identifier" matching is proof of byte-identical layout
  where the SQL catalog is unhelpful.

### Drop-downs are a second source of truth

`"choices database file"` on a widget names a table the PUI *runtime* queries in
the browser. These revealed tables and real column names that the RPG never
mentions: `HELP` (`HLPAPP/HLPSUB/HLPVAL/HLPSEQ/HLPTXT`, self-joined `SORTT`→text
and `SORTV`→column name), `COADDRNAME`, `LOCGEO` (`L2WH/PRTM/L2DEL`), `CUSTOMEP`
(`CUNN/CUID/CUNAME/CUDEL`), and corrections to `COADDRES` (`COAPLT`, not `COANN`)
and `SAPINCO` (`INCOCDE`/`INCODES`). **Mine them before writing DDL.**

Note: a database-driven grid or drop-down is resolved **in the browser**, so a
headless session shows it empty or "Loading...". That is expected, not a defect.

---

## 4. Rich Display File anatomy, and how to edit one safely

A Rich Display File's design source is **DDS whose `HTML()` keywords carry the
screen definition as JSON**. The rules:

- One `.dspf` member holds one screen object **per record format**
  (`PODTLUI` has four: `CTLFMT`, `NOTEFMT`, `ASKFMT`, `ERRFMT`).
- Each screen's JSON is split across **several `HTML()` keywords** (~2,500 chars
  each) which the runtime simply concatenates.
- Each keyword is split across **DDS continuation lines**: content occupies
  columns 45–79 (exactly 35 characters) with `-` in column 80. The prefix is 44
  characters: `     A                                  1  2`.
- A closing line has all 36 columns but must fit the trailing `')` too, so its
  content limit is **34**, not 35.
- Single quotes inside the JSON are doubled, DDS style. **PUI's own generator
  splits a doubled quote across a continuation** (9–12 times per file) and the
  compiler joins before scanning, so that is safe — and a "don't split a doubled
  quote" guard is actively harmful, because it forces a short chunk which then
  gets padded with spaces that become string data.

**Do not hand-edit this.** Use `tools/rdf_edit.py`:

```python
import sys; sys.path.insert(0, 'tools')
import rdf_edit
lines, screens = rdf_edit.parse('qddssrc/podtlui.dspf')   # -> [(start, end, obj)]
rdf_edit.rewrite('qddssrc/podtlui.dspf', lambda obj: mutate(obj))
```

**Always assert parse → emit → parse yields identical JSON, and that no line
exceeds 80 columns, before letting it touch a real file.** That test caught two
bugs in the first version of the writer that the compiler would not have.

`tools/wire_assets.py` is the worked example: it adds the `external css` /
`external javascript` properties described next.

Useful field facts for authoring:

- Widget `css class` values used by Pratt: `label`, `input`, `outputField`,
  `outputFieldClear`, `outputFieldOv`, `comboInput`, `search`,
  `hybrid-link`, `hybrid-grid`, `hybrid-constant`. The `hybrid-*` ones are
  **Pratt's own and are defined in a stylesheet they have not sent** — ask.
- `search` is functional, not cosmetic: it is what makes a field part of a saved
  search.
- Icons are `"icon": "material:navigate_before"`, rendered as a ligature in
  `.pui-material-icons`. If you see the icon *name* as literal text, the Material
  Icons font is not being applied — see §8.

---

## 5. Look and feel: Pratt's design system

Everything below is measured from the four screens they sent, not invented. Match
it and a new screen will sit beside theirs without comment.

### 5.1 Page geometry

A Pratt screen is a fixed-pixel absolute layout on a **1200 x 600 canvas** — all
three of their main screens come out at exactly that extent — built from
`css panel` layout widgets. Two established arrangements:

**Search screen** (`POSELUI`, `PMSELUI`) — three columns:

```
left=0    top=50   165 x 550   LayoutBTN     function-key column, blue
left=170  top=50   670 x 410   LayoutSFL     the subfile grid
left=170  top=460  670 x 140   LayoutLIN     secondary grid (PO lines), POSELUI only
left=845  top=50   295 x  30   LayoutSRCH    plant / warehouse
left=845  top=80   295 x 100   LayoutSRCH1   type + status checkboxes
left=845  top=180  295 x  75   LayoutSRCH2   vendor / order / freight
left=845  top=255  295 x  70   LayoutSRCH3   created-by + date ranges
left=845  top=330  295 x 110   LayoutSRCH4   line-item searches
left=845  top=440  295 x  70   LayoutSORT    three sort drop-downs
left=845  top=510  295 x  90   LayoutPSR     saved searches + save/edit/refresh
```

**Detail screen** (`PODTLUI`) — a header band of four panels over one wide grid:

```
left=0    top=50   165 x 550   LayoutBTN     function-key column, blue
left=170  top=50   375 x 150   LayoutHDR1    key fields, status, totals
left=550  top=50   310 x 150   LayoutHDR2    ship-to address
left=865  top=50   230 x 150   LayoutHDR3    text and instructions
left=1100 top=50   100 x 150   LayoutHDR4    created / changed audit
left=170  top=200 1030 x 400   LayoutSFL     the line-item grid
```

Note `top=50` on everything: the first 50px is left clear for the screen title.

### 5.2 Panels

Every container is `"field type": "layout"`, `"template": "css panel"`. Themes are
PUI's lettered set:

| Use | `header theme` | `body theme` |
|---|---|---|
| Function-key column | *(none — no header)* | `B - Blue` |
| Content and search panels | *(none)* | `D - Light Gray` |
| Window: normal | `B - Blue` | `D - Light Gray` |
| Window: prompt / "enter a key" | `F - Green` | `D - Light Gray` |
| Window: error | `G - Red` | `D - Light Gray` |

A panel with `"has header": "true"` plus `"header text"` draws a title bar; the
header text can be **bound to a field** so the RPG sets it (`ASKHEADER`,
`ERRHEADER`, `NOTEHDR` all do this). Other panel properties in use:
`"straight edge": "all"`, `"z index": "100"`, `"text align": "left"`.

Widgets are placed **inside** a panel with `"layout": "<panel id>"` and
`"container": "1"`, and their `left`/`top` then become panel-relative.

### 5.3 The function-key column

The signature Pratt element. A 165 x 550 blue panel down the left, holding
graphic buttons 145px wide, 30px high, on a 35px pitch, stacked from `top=10`,
with the exits pinned to the bottom:

```
top=10   Go To Record (F1)      material:search
top=45   View Detail  (F2)      material:remove_red_eye
top=80   New PO       (F4)      material:create_new_folder
   ... gap for screen-specific actions ...
top=400  Help (Alt-F1)          material:help
top=435  <download>             material:file_download     (40px wide)
top=440  "POs"  [SFLSIZ]        record counter, right aligned, 14px
top=470  Back (F12)             material:navigate_before
top=505  Exit to Menu (F3)      material:exit_to_app
```

Keep `Back` and `Exit` last and in that order — users rely on the position.

### 5.4 Buttons

```json
{ "id": "CTLEXIT", "field type": "graphic button", "css class": "hybrid-link",
  "value": "Exit to Menu (F3)", "icon": "material:exit_to_app",
  "icon position": "left", "shortcut key": "F3",
  "left": "10px", "top": "505px", "width": "145px", "height": "30px",
  "auto arrange": "false", "tab index": "-1",
  "layout": "LayoutBTN", "container": "1",
  "response": { "fieldName": "CTLEXIT", "dataType": "indicator", "indFormat": "1 / 0" } }
```

- **The label always carries its key in brackets** — `Save (F11)`, `Back (F12)`,
  `Refresh (Enter)`. Do not drop it; it is how the green-screen users navigate.
- `shortcut key` and the `response` indicator are both required: the key fires it,
  the indicator tells the RPG which button was pressed.
- `auto arrange: false` is on **all 57** buttons across the four screens — treat it
  as mandatory. `tab index: -1` is used selectively (14 of 57) to keep a button out
  of the tab order; apply it to the navigation buttons, not to ones a keyboard user
  should reach.
- Two sizes only: `145px x 30px` for a labelled button, `40px x 30px` for an
  icon-only one.
- Icons are Material ligature names. Ones already in use, so reuse rather than
  invent: `search`, `remove_red_eye`, `create_new_folder`, `edit`, `add`,
  `receipt`, `history`, `content_copy`, `send`, `work`, `store`, `schedule`,
  `note_add`, `speaker_notes`, `speaker_notes_off`, `delete`, `save`, `check`,
  `cancel`, `refresh`, `file_download`, `help`, `navigate_before`,
  `skip_previous`, `skip_next`, `exit_to_app`, `subdirectory_arrow_right`,
  `edit_location`, `widgets`. One Font Awesome is used:
  `fontAwesome-solid:box-open`.
- Function keys in use: F1 F2 F3 F4 F5 F6 F7 F10 F11 F12 F17 F18 F22, Alt-F1,
  Enter, PageUp, PageDown.

### 5.5 Grids (subfiles)

```json
{ "id": "SFLFMT", "field type": "grid", "css class": "hybrid-grid",
  "record format name": "SFLFMT",
  "number of rows": "10", "number of columns": "5",
  "column widths": "156,239,74,86,100",
  "column headings": "PO,Vendor/Order,Info,Created,Updated",
  "header height": "18", "row height": "42",
  "width": "656px", "height": "396px",
  "subfile return rrn": { "fieldName": "SFLR#", "dataType": "zoned", ... } }
```

- **Row height is the tell.** Pratt stack two or three lines of data in one grid
  row: 23px for a single line, 42–62px for multi-line. `PODTLUI` uses 62px to fit
  part number / description / SKU in one cell.
- Column headings may be empty for an options column (`",Status,Part/Desc,..."`).
- `"scrollbar": "sliding"` on the big detail grid.
- `"load all rows": "true"` for small fixed lists.
- A grid can be **database-driven** (`"database file"`, `"selection criteria"`
  with `?` markers, `"load all rows"`) — the PUI runtime queries it in the
  **browser**. `PUISRCHUI`'s grid does this. It will look empty in a headless
  session; that is expected.
- Cells hold ordinary widgets positioned relative to the cell, each carrying
  `"grid": "SFLFMT"` and `"column": "2"`.

### 5.6 Type, colour and classes

| | |
|---|---|
| Screen title | `hybrid-constant`, 18px, white, top band |
| Section headings in panels | `label`, 14–16px |
| Field labels | `label` |
| Read-only values | `outputField` (`outputFieldClear` / `outputFieldOv` for overlays) |
| Entry fields | `input`, or `comboInput` where it toggles read-only |
| Search filter fields | **also** `search` — this is functional, see §1 |
| Buttons and links | `hybrid-link` |
| Grids | `hybrid-grid` |

Font sizes in use: 12, 14, 15, 16, 18px. Explicit colours in the sources are only
`white` (3), `darkred` (2), `darkgreen` (1), `red` (1) — status emphasis. Everything
else now carries `#333333` (labels) or `#212121` (values) set by
`tools/wire_assets.py`; see §8 for why that is a widget property and not CSS.

**`color` can be bound to a field.** `SFLSIZ` binds its colour to `SFLSIZCOL` so
the program drives it — which is exactly why `wire_assets.py` skips any widget
that already has a `color`.

`hybrid-link`, `hybrid-grid` and `hybrid-constant` are **Pratt's own classes and
we do not have the stylesheet that defines them** (§14). Screens still look right
because Profound UI's base styling carries them; if Pratt send that file, expect
the look to change.

### 5.7 Windows

Prompt, note and error screens are separate record formats shown as windows:

```json
"screen": { "record format name": "ASKFMT", "show as window": "true",
            "center window": "true", "overlay screens": "true",
            "mask screen": "true" }
```

`window top` / `window left` override centring. The window's panel carries the
coloured header (blue / green / red per §5.2) and its `header text` is usually
bound so the RPG writes the title.

### 5.8 Which Genie skin

These screens are built for the stock **`Hybrid`** skin, or something very like
it. Two pieces of evidence:

- `Hybrid.css` is 9 KB and contains `div { padding: 1px; z-index: 10;
  text-align: left; white-space: nowrap; }` — the classic Genie blanket rule.
  `pratt_profound.css` opens with `div { padding: 0px; }`, which is a direct
  counter to it. Their house stylesheet is written against this skin.
- It has **no `@media (prefers-color-scheme: dark)` block at all**, so the
  white-on-white problem in §8b simply cannot happen under it.

Open a session with `?skin=hybrid`. The demo system's default is `pls`, a 180 KB
skin with a dark palette that these screens were never designed for.

Two things to watch under `Hybrid`, both consequences of its blanket `div` rule:
`white-space: nowrap` stops text wrapping anywhere on the screen, and
`z-index: 10` on every div can trap a dropdown inside a stacking context.
`pratt_profound.css` only counters the `padding` part — and it is currently not
reaching the browser at all (§7.1), so even that counter is inert.

---

## 6. How a screen is wired to the RPG

### 6.1 The RPG side

```rpgle
DCL-F PODTLUI WORKSTN HANDLER('PROFOUNDUI(HANDLER)')
  SFILE(SFLFMT:SFLR#);
```

That is the whole difference from a green screen: the `HANDLER` keyword routes
the display file through Profound UI's Open Access handler. `SFILE` names the
subfile record format and its RRN field exactly as DDS always did. Everything
else — `EXFMT`, `WRITE`, `CHAIN`, `READC` — is unchanged.

The CL entry point passes the key fields and a paging contract:

```cl
CALL PGM(PODTLUI) PARM((&EOJ) (&RET) (&SIZE) (&OHYY) (&OH#))
IF COND(&RET *NE '00000') THEN(GOTO START)
```

`@RET` is the ordinal within the search result set and `@SIZ` the total; see the
pager note in §1.

### 6.2 Bound values

Any widget property can be a literal **or** bound to a display-file field. A bound
property is an object, and the `dataType` / `formatting` pair must match the DDS
field or the screen will not compile:

```json
"value": { "fieldName": "SFLSIZ", "dataType": "zoned", "formatting": "Number",
           "dataLength": "5", "decPos": "0", "negNum": "-999.00",
           "numSep": "false", "zeroBalance": "true", "designValue": "[SFLSIZ]" }
```

| `dataType` | `formatting` | Notes |
|---|---|---|
| `char` | `Text` | plus `trimLeading` / `trimTrailing` / `textTransform` |
| `zoned`, `packed` | `Number` | plus `decPos`, `numSep`, `curSym`, `negNum` |
| `indicator` | `Indicator` | `indFormat` is `"1 / 0"` or `"true / false"` |
| `reference` | `Text` | `"refField": "OIDESC ORITM"` — takes type from the file |
| `date` | `Date` | |

`reference` is how Pratt bind to database fields: the DDS emits
`REFFLD(OIDESC ORITM)` and the table must exist at compile time.

### 6.3 Properties that drive behaviour from the program

Per editable widget, matching the `*DIS` / `*CHG` / `*ERR` naming convention
from §1:

| Widget property | Bound to | Effect |
|---|---|---|
| `disabled` | `xxxxDIS` indicator, `indFormat: "true / false"` | read-only — with `comboInput` this is the whole read-only/update switch |
| `changed` | `xxxxCHG` indicator | tells the runtime the value changed |
| `error condition` | `xxxxERR` indicator | flags the field |
| `error response` | `xxxxERR` indicator | usually the same field |
| `error message` | `xxxxERM` char(100) | the text shown |
| `error message css class` | `xxxxERF` char(15) | `ERFINFO` (warn, still saveable) or `ERFERROR` (blocks) |
| `tool tip` | `xxxxTT` char(100) | hover text, also used to explain *why* a button is disabled |
| `visibility` | `xxxxVIS` | show/hide |
| `color` | a char field | per-record colouring, as `SFLSIZ`/`SFLSIZCOL` do |
| `response` | an indicator | which button was pressed |
| `error enhanced mode` | `"1"` | required alongside the error set |

So adding one editable field to a screen means adding **six or seven hidden DDS
fields** alongside it. That is not optional decoration — `VerifyRecord` drives all
of them, and the screen is where the user sees the result.

### 6.4 Screen-level error plumbing

Each record format that can report an error carries:

```json
"error messages": "1 message",
"error message":   { "fieldName": "CTLFMTERM", "dataType": "char", "dataLength": "100" },
"error condition": { "fieldName": "CTLFMTERR", "dataType": "indicator" },
"error response":  { "fieldName": "CTLFMTERR", "dataType": "indicator" },
"error enhanced mode": "1"
```

Convention: `<FORMAT>ERM` and `<FORMAT>ERR`.

### 6.5 Client-side JavaScript

Pratt put real logic in widget event handlers. The patterns worth copying:

- **`onchange` marks a filter as touched** — `applyProperty(this.id + "LBL", "color", "red")`
  turns the field's label red so the user can see which filters are active.
- **Checkbox groups cannot all be off** — the last one clicked is forced back on.
- **A row click sets the RRN and enables/disables actions**:
  `pui.set('SFLR#', row)` then `applyPropertyCSS('.no-selection', 'disabled', 'false')`,
  then per-row checks via `grid.getDataValue(row, 'PONUM')`.
- **`pui.click(this.id, true)` submits**, after the handler has staged values.
- **`sessionStorage` carries values across a screen change**, including into a
  green screen (see the Genie macro in §1).
- `confirm()` / `prompt()` guard destructive actions (Copy PO, Cancel changes,
  Clear notes) and name a saved search.

These come from `settings.js`: `applyPropertyCSS`, `restoreSearch`, `pressWait`,
`sanitizeFilename`, `serverReady`, `sleep`, `applyPropertyCSSfromPUI`. The screen
must name that file — §7.

**A handler that runs on load must survive the file not being there yet.** The
runtime defers `onload` until `external javascript` has loaded, which is why that
property matters; see §8a.

---

## 7. Asset wiring: make the screen self-sufficient

> **Read 7.1 before adding `external javascript` to anything.** Naming a file that
> 404s there stops the screen's `onload` from running at all.

Pratt's screens assume *their* hosting page loads `settings.js` and the house CSS
globally. Nothing here does.

### 7.1 `external javascript` is a trap unless the file really exists

Profound UI's runtime evaluates a screen's `onload` **inside the callback of the
last `external javascript` file it loads**:

```js
if (0 < q)
  for (...) c.addJSFile(p, function () { --q; if (0 == q) { ... eval(onload) ... } })
```

If that file 404s the callback never fires, so **`onload` never runs**. The screen
still renders; it just silently loses its initialisation. The tell is that an
error moves *later* in the lifecycle — ours went from

```
Onload Error: applyPropertyCSS is not defined
```

to

```
Onrowclick Error: applyPropertyCSS is not defined
```

which looks like partial progress and is actually the opposite: `onload` had
stopped running, so it could no longer even fail.

**Assets in `htdocs/profoundui/userdata/` exist only in this repo.** They are not
on the IBM i until CoderFlow copies them there when the task is approved, and you
cannot check from the container because the proxy answers the agent HTTP 401. So
treat any userdata asset as **absent at runtime** while you are working.

### 7.2 Database-driven drop-downs do not work through the task proxy

A widget with `choices database file` is resolved **by the browser**, which POSTs
to `PUI0009103.PGM`. `pui.getProgramURL` builds that as an **absolute** path from
the site root (`/profoundui/auth/PUI0009103.PGM`), and the task proxy serves the
app under `/tasks/<id>/app/proxy/...`, so the call does not land where it should.
Every database-driven drop-down comes up empty.

The data is not the problem - running the widget's own query against the task
library returns the right rows. Check that first, then stop looking at the data.

**It is worse than an empty list.** While the query is outstanding the runtime puts
a single `Loading...` option in the box, so a static `choices` list is *replaced*
rather than used as a fallback. To get a working drop-down you must remove the
database-choice properties, not merely add static ones.

`tools/static_choices.py` does that: it generates `choices` / `choice values` from
the live tables and strips `choices database file`, `choices database file 2`,
`choices database join`, `choices selection criteria`, `choices parameter value*`,
`choice options field`, `choice values field` and `order by` from those widgets.
It prints what it removed, so restoring Pratt's behaviour once the callback works
is a matter of putting them back.

Static choices are not foreign to these screens - Pratt already ship them on
`OHCODE`, `OITXTU1-8` and `ASKRETURN`.

What it covers and why:

| Widget | Treatment |
|---|---|
| `OHDC`, `OHFRT`, `SRWH`, `SRDC`, `SRFRT`, `SRNN` | select boxes - unusable when empty, so they must have a list |
| `CTLORDR1/2/3` | select boxes feeding `SQLORDER`; without them you cannot sort |
| `OHVEND`, `OIPART`, `SRVEND`, `SRPART`, `SRPOIT` | textboxes - typable without a list, but you cannot look a vendor or part up, which is most of raising a PO |
| `OHCUID`, `OHLOC` | left alone - their criteria depend on another field, so a flat list would offer invalid combinations |
| `CTLVAL` | left alone - per-user saved searches, a build-time snapshot would be wrong |
| `SRDESC` | left alone - every description in ORITM/ORMSR, volatile and long |

Re-run it after changing reference data, then rebuild the display files.

### 7.3 What to do instead

| Need | Do this | Not this |
|---|---|---|
| Helper JS (`settings.js`) | Inline it into each screen's `onload` prelude | `external javascript` |
| Text colour | A `color` widget property — inline style, §8b | A stylesheet rule |
| Base widget styling | `external css` to `/profoundui/proddata/css/profoundui.css` — a **real IFS file**, so it loads | — |
| House CSS not yet deployed | Keep the `external css` entry; it is inert now and starts working on approval. A 404 stylesheet is harmless — unlike a 404 script, nothing is gated on it | — |

`tools/wire_assets.py` does all of this: it sets the `external css` list, deletes
any `external javascript`, generates the `onload` prelude from
`htdocs/profoundui/userdata/custom/settings.js` (rewriting `function foo(` as
`window.foo = function(` so the eval'd body reaches global scope, wrapped in a
`typeof` guard), and sets the `color` properties.

Re-run it after editing `settings.js` — the prelude is generated from that file,
which stays the single source of truth.

```
external css    /profoundui/proddata/css/profoundui.css   (base widget styling)
external css 2  /profoundui/userdata/css/pratt_profound.css
external css 3  /profoundui/userdata/css/styles.css        (only where .comboInput is used)
external css N  /profoundui/userdata/css/pratt_theme_fix.css  (always last)
```

`profoundui.css` goes **first** so house overrides still win, and is referenced at
its real `proddata` URL so its relative font URLs resolve. Loading it twice is
harmless if the hosting page does supply it. The property is **per record format**,
and **CSS files not listed on the current screen are actively removed** from the
DOM by the runtime.

### Where the files live

| Repo path | Served as |
|---|---|
| `htdocs/profoundui/userdata/css/*.css` | `/profoundui/userdata/css/*.css` |
| `htdocs/profoundui/userdata/custom/*.js` | `/profoundui/userdata/custom/*.js` |

**Never `scp` them to the IBM i** — that copy is owned by CoderFlow's approval
step. Because they are served from the working tree once deployed, editing one of
these files needs no rebuild; changing which files a *screen names*, or editing
`settings.js` (which feeds the prelude), does.

## 8. Two hosting-page problems, and how to recognise them

### a) No widget styling at all

**Symptom:** black page; buttons reading `searchGo To Record (F1)` — the Material
icon ligature *name* concatenated with the label; the subfile grid collapsed to
bare column dividers; panels with no chrome.

**Cause:** `/profoundui/proddata/css/profoundui.css` is not being applied to the
hosting page. `.pui-material-icons` lives in that file, so no base stylesheet
means no icon font, which is the fastest tell. Reproduced exactly by removing
only that file from the render harness.

**Fix:** have the screens name it themselves (§7). That is a workaround, not a
cure — if other PUI screens in the environment look the same way, check whether
the file returns 200 with `Content-Type: text/css` in a real browser.

### b) White on white

**Symptom:** everything renders, but text is near-invisible on the white panels.

**Cause:** the Genie `pls` skin switches palette under
`@media (prefers-color-scheme: dark)` — so **it only happens for users whose
machine is in dark mode**, and a headless render defaults to light and will never
show it. In that block the skin repaints everything:

```css
.genie-form--screen p, .genie-form--screen div { color: var(--color--text--primary) }
.genie-form--screen span                       { color: var(--color--text--primary--alt) }
```

`--color--text--primary` is `#dedede` dark, `#212121` light. The selectors score
**(0,1,1)**, outranking a plain `.label { color: #333 }` at **(0,1,0)**. Panels
stay white because their background comes from `profoundui.css`, not the skin.

Measured with `tools/render-screen/diffmode.js`: **189 of 192 text elements** on
the PO search screen compute a different colour in dark mode than in light.

**Fix - and this is the important part: NOT with CSS.** A stylesheet at (0,2,1)
*should* win, and `pratt_theme_fix.css` does exactly that. It had no effect in the
real browser: after shipping it the search-panel labels were still white. Either
the file is not reaching the page or Genie's real wrapper class differs from the
one the skin's own CSS implies, and neither can be determined from inside the
container - the proxy answers the agent HTTP 401.

So the colour is set **as a widget property instead**. Profound UI declares
`color` as a `g:"css"` property, so the runtime renders it as an **inline style**
on the element, which nothing in any stylesheet can outrank:

```json
{ "id": "SRNNLBL", "field type": "output field", "css class": "label",
  "color": "#333333" }
```

`tools/wire_assets.py` sets it on every `label`, `outputField`, `outputFieldClear`,
`outputFieldOv`, `input`, `search` and `comboInput` widget that does **not already
carry one** - Pratt set a few deliberately (two whites on the blue side panel, one
bound to a field so the program drives it) and those are left alone.

Verified by serving a 404 for the stylesheet and confirming the labels still
compute `rgb(51,51,51)`.

**Check the backgrounds first.** `tools/render-screen/labelbg.js` reports the
effective background behind every widget of a class. All 32 labels and every
output field sit on a light background in both schemes here, so a dark colour is
safe; do not inline a colour without checking that.

`pratt_theme_fix.css` is kept as a net for anything added without going through
`wire_assets.py`, with a header saying plainly that it is not doing the work.

**Deliberately left alone** — all white-on-coloured and correct as they are, and
none of them change between light and dark: grid column headers, the selected
row, `.hybrid-link` buttons, the `.hybrid-constant` screen title. Input
*backgrounds* are also left alone: forcing one would outrank
`styles.css`'s `:disabled.comboInput` and break the read-only look.

**General lesson: prefer a widget property to a stylesheet** for anything that has
to survive an unknown hosting page. CSS here competes with a 180 KB skin you did
not write, through a wrapper you cannot inspect, over a transport you cannot
verify. A widget property goes into the compiled display file and arrives in the
RDF stream — you can assert it with `genie_get.sh` without a browser at all.

**Two broader fixes that look right and are not** (both tried and rejected
against the harness): re-pinning the skin's whole light palette turned the
function-key buttons into blank blocks; re-pinning `--color--text--primary` for
the whole subtree forced dark text onto the skin's genuinely dark surfaces.
Custom properties inherit, so you cannot scope them to "only the light bits".

---

## 9. Seeing a screen: `tools/render-screen`

`genie_html.sh` renders server side and answers the agent 401. This runs Profound
UI's own `runtime.js` against a screen captured by `genie_get.sh`, in headless
Chromium.

```bash
S=/tmp/genie-sessions/MySession/history/screen-003.json

# a cleanly hosted Profound UI page, no Genie skin
tools/render-screen/render.sh $S out.png

# what the screens actually get here: base stylesheet + pls skin + Genie wrapper
tools/render-screen/render.sh $S out.png --as-hosted

# the broken state before the screens named profoundui.css themselves
tools/render-screen/render.sh $S out.png --no-base

# dark mode, trying a stylesheet before wiring it into the screens
PUI_COLOR_SCHEME=dark EXTRA_CSS=/profoundui/userdata/css/pratt_theme_fix.css \
  tools/render-screen/render.sh $S out.png --as-hosted

# measured instead of eyeballed
RENDER_SCRIPT=audit.js    ...   # WCAG contrast failures
RENDER_SCRIPT=diffmode.js ...   # every element's colour, to diff light vs dark
RENDER_SCRIPT=inputs.js   ...   # input/select colours and backgrounds
RENDER_SCRIPT=labelbg.js  ...   # effective background behind every .label
RENDER_SCRIPT=prove.js    ...   # inline style vs stylesheet, and 4xx asset loads
```

### Getting `pui.render` to work headlessly

- It reads `b.container.style`, so set `json.container = <div>` before calling.
- Also set `pui.runtimeContainer`, and have a `<div id="middle">` present.
- **Do not name the container `pui`.** The skin has `body, #pui { background: ... }`,
  so a `#pui` container takes the dark page colour and panels come out dark when
  they should be white. Genie's container is not called that.
- Wrap the content in `.genie-form--screen` for `--as-hosted`; most skin rules are
  scoped to it.
- Model `prefers-color-scheme` or you will never see a whole class of bug.
- Kill any stale `python3 -m http.server` on the port first, or it serves a
  deleted temp dir and you screenshot a 404 page and chase a phantom.

### What it does NOT reproduce

The skin's **button chrome**, and **input backgrounds**. Genie wraps a button in
more DOM than the harness's single wrapper div, so buttons can render blank there
while being fine in a real browser; and the skin paints entry fields with its dark
theme background in the harness while the user's browser shows them white. Judge
both in the browser. Where the harness and a user's screenshot disagree, **the
screenshot wins** — it is the real page.

---

## 10. Verifying against the running system

Use the `ibmi-interactive-session` skill. The menu is 5250; the Pratt apps switch
the stream to RDF.

```bash
cd /home/coder/.claude/skills/ibmi-interactive-session
./genie_start.sh Sess
./genie_get.sh Sess | jq -r '.["5250"].buffer[]'       # 5250
./genie_put.sh Sess "0=4&crow=22&ccol=7&aid=241"       # menu option 4
./genie_get.sh Sess > screen.json                      # now RDF
```

Driving an RDF screen headlessly:

- **Every** response indicator in the active format must be submitted, `0` except
  the one being pressed. Build the string rather than typing it:

```bash
IND=$(./genie_get.sh Sess | python3 -c "
import json,sys; d=json.load(sys.stdin)
f=[x for x in d['layers'][0]['formats'] if x.get('active')][0]
inds=sorted({it['response']['fieldName'].upper() for it in f['metaData']['items'] if it.get('response')})
print('&'.join('CTLFMT.%s=%s' % (i, '1' if i=='CTLDTL' else '0') for i in inds))")
./genie_put.sh Sess "${IND}&CTLFMT.SFLR#=3&aid=50&row=10&column=5&toprrn.1=1&toprrn.2=1"
```

- Format and field names go **UPPERCASE** in POST data regardless of how the JSON
  shows them; `aid`, `row`, `column`, `toprrn`, `rrn` stay lowercase.
- `toprrn.n=1` for each subfile. POSELUI has two.
- Selecting a subfile row headlessly means setting the RRN field the grid's
  `onrowclick` would set (`CTLFMT.SFLR#=3`), because the JS does not run.
- aid codes: Enter 241, F1–F12 49–60, F13–F24 177–188, PageUp 244, PageDown 245.
- **Sign off properly** (F3 out, then option 90) and `./genie_end.sh`.

Things worth asserting from the stream, with no browser: the `external css` /
`external javascript` properties on `metaData.screen`; `SFLSIZ`; subfile row
counts and values; `CTLCTR` for the pager; any `*ERM` field with content.

---

## 11. Demo data

`data/catalog.py` (reference data) + `data/gen_seed.py` (deterministic generator)
→ `data/seed.sql` → `data/load.sh`.

```bash
./data/load.sh            # into $IBMI_BUILD_LIBRARY
./data/load.sh MYLIB      # or a named library
```

`seed.sql` uses **unqualified** table names; the library is supplied at run time
through `RUNSQLSTM DFTRDBCOL`, so no `AITSKxxxxx` name is hardcoded anywhere.

### Content

63 vendor parts modelled on what a recycled-containerboard business actually
buys: corn and ethylated starch, borax, caustic soda, AKD sizing, retention aid,
defoamer, biocide; press felts, forming fabrics, dryer screens, doctor blades;
A/C/E-flute corrugating rolls, double-backer belts, slitter and scoring blades;
water-based flexo inks, photopolymer plates, anilox rolls, hot melt, stitching
wire, carton tape; high-tensile baling wire, bale ties, OCC grade 11 and DLK 12;
pallets, stretch film, PET strapping, edge protectors, protective foam; display
corner posts, shelf trays, litho labels; plant MRO and PPE. **Vendor names are
invented** so the demo implies no trading relationship with a real supplier.

Transactions use **archetypes, not noise**: open and partially received, closed,
overdue, on hold above the approval limit, cancelled, drop-ship, e-commerce,
entered-today-not-transmitted, rush with customer pickup. Dates anchor on a
reference date and never fall at a weekend. Activity is weighted toward plant 94
(Conyers) because that is the default the search screens open on — otherwise the
first screen a demo shows looks empty.

### Data rules the application enforces

These were all found by the app rejecting the data, not by reading code:

- `COADDRES.PAYOTQ` must be **numeric in 700000–810999** (6 digits). 7 digits and
  every PO shows *"Invalid or missing cost center for DC"*.
- `WIPJOBS.WPJOB` / `WIPSHIP.WSJOB` must be **exactly 8 digits**. PMSELUI matches
  `CHAR(wsjob)` against `SUBSTR(CHAR(omwo#),3,8)`, and DB2's `CHAR()` of a decimal
  is **left-justified**, so a shorter job silently never matches and the WP button
  stays greyed.
- `ORHDR.OHFRT` must exist in `SAPINCO` or the screen flags invalid freight terms.
- `COADDRES.COAPO#` for plant **'94'** is the next-PO-number counter: `PreSaveRecord`
  does `CHAIN '94' COAFMT`, increments it and uses it as `OH#`. Seed it to the
  highest PO number already in `ORHDR`, or the first PO a user creates comes out
  as `PR26<1>` instead of continuing the sequence.
- `OHX` / `OIX` / `WPX` / `WSX` / `VOX` / `MQX` are **audit flags, not delete
  flags**. PODTLUI only ever assigns them `'X'` on save and never tests them.
  Seed them as `'X'` so seeded rows match what the application writes, and never
  filter them out in a helper - doing so made every user-created PO total .00
  while the demo data totalled correctly.
- `VENDMST.VNCODE` other than blank means inactive, and the PO warns.
- A PO is placed with **one** vendor — all its lines must come from that vendor's
  catalogue.
- Freight lines must be quantity 1.

### SQL tooling notes

- `aitool sql` needs `connection:"dev"` explicitly, and its default library list
  hits `AIDEMOBASE` first — **qualify table names with the task library**.
- It caps at 500 rows silently, truncates at 64 KB into a pipe, and mangles
  inline single quotes — use `--input @file.json` and redirect to a file.
- It surfaces SQL **warnings** as errors. `SQL0434` ("correlation without
  qualification") is a warning; PMSELUI compiles clean at severity 0 with it.
- DB2 for i implicitly casts numerics in `CONCAT`, so `'PR' || OHYY || OH#` works.

---

## 12. Building a new Rich Display screen from scratch

A worked order of operations. Steps 1–4 are design, 5–7 are plumbing, 8–10 are
proof.

**1. Pick the shape.** Search screen or detail screen (§5.1)? Copy that geometry
rather than inventing one — the three-column search layout and the
header-band-over-grid detail layout are what Pratt's users already know. Decide
the record formats now: one screen object each, and every window
(`show as window`) is its own format.

**2. Design the database access first.** For a search screen that means the one
prepared statement with `COALESCE(NULLIF(?, ''), col)` optional filters and the
three-column `ORDER BY` fed by `SQLORDER` (§1). For a detail screen it means the
files `PODTLUI` already opens — check whether what you need is there before adding
a table.

**3. Create or extend the tables.** `CRTDSPF` resolves `REFFLD(FIELD FILE)` at
compile time, so every referenced table must exist and carry the field. If you add
a field to a table that an existing display file references, that display file has
to be deleted and rebuilt too (§2).

**4. Lay out the screen.** Start from a copy of the closest Pratt screen and edit
the JSON with `tools/rdf_edit.py`. Checklist:

   - Panels: `css panel`, themes from §5.2, `top=50` to leave the title band.
   - Widgets inside a panel carry `"layout": "<panel id>"` and `"container": "1"`.
   - Function-key column per §5.3 — 145x30 buttons on a 35px pitch, key name in
     the label, `Back` and `Exit` last.
   - Grid: row height sized for how many lines of data go in a cell (§5.5).
   - CSS classes per §5.6 — and put `search` on every field that should join a
     saved search, or the saved-search feature silently ignores it.
   - Every editable field gets its `*DIS` / `*CHG` / `*ERR` / `*ERM` / `*ERF` /
     `*TT` companions and the matching widget properties (§6.3). Six or seven
     hidden DDS fields per visible one is normal here.
   - Each format gets its `<FORMAT>ERM` / `<FORMAT>ERR` error plumbing (§6.4).

**5. Make it self-sufficient.** Run `python3 tools/wire_assets.py`. It sets the
`external css` list, **removes any `external javascript`** (§7.1), inlines
`settings.js` into the `onload` prelude, and sets the `color` widget properties
(§8b). Do not rely on a separate asset file for anything the screen needs to
function — until the task is approved, those files are not on the IBM i.

**6. Write the RPG.** `DCL-F x WORKSTN HANDLER('PROFOUNDUI(HANDLER)') SFILE(...)`
(§6.1). Follow PODTLUI's procedure names (§1) — `LoadRecord`, `SetAllowedModes`,
`UpdateMode`, `SetUpdateFields`, `ShowChanges`, `VerifyRecord`, `PreSaveRecord`,
`SaveRecord`, `PostSaveRecord`, and the line-level mirror. Validation goes in
`VerifyRecord` / `VerifyLine` and sets the `*ERR` / `*ERM` / `*ERF` fields;
`VFYERR` blocks the save.

**7. Add to `Rules.mk`.** Referenced tables are normal prerequisites (so a DDL
change rebuilds the display file) *and* order-only prerequisites (so they exist at
compile time):

   ```make
   myscreen.file: qddssrc/myscreen.dspf qsqlsrc/orhdr.table.sql | orhdr.file
   mypgm.pgm:     qrpglesrc/mypgm.sqlrpgle qddssrc/myscreen.dspf | myscreen.file orhdr.file
   ```

**8. Build.** `./pratt/build.sh`. On failure read `tmp/logs/<target>.log` and look
for severity ≥ 30. Remember a display file object has to be deleted before it can
be rebuilt (CPF5813), along with its `build/` stamp.

**9. Look at it.** `tools/render-screen/render.sh <screen.json> out.png --as-hosted`,
then again with `PUI_COLOR_SCHEME=dark`, then `RENDER_SCRIPT=audit.js` — expect
only white-on-coloured items below AA. Do not skip this; a screen that compiles
tells you nothing about whether it is readable (§9).

**10. Drive it.** A Genie session (§10): assert real values out of the stream —
row counts, computed totals, the pager, any populated `*ERM` — then sign off and
end the session. A render proves it looks right; only driving it proves it works.

### 12.1 Making it reachable

A Rich Display program is not reachable until something calls it. The chain in
place is:

```
MENU option 4  ->  PRATTPO (CL)  ->  POAPPUICL (Pratt's CL)  ->  POSELUI  ->  PODTLUI
MENU option 5  ->  PRATTREQ (CL) ->  PMAPPUICL (Pratt's CL)  ->  PMSELUI
```

**The menu** is `cfdemo/qddssrc/menu.dspf` (a free-format DDS menu) plus
`cfdemo/menu.msgf`. Option *n* runs message `USR00nn`:

```
addmsgd msgid(usr0004) msgf($LIBRARY/$NAME) msg('call prattpo') ...
addmsgd msgid(usr0005) msgf($LIBRARY/$NAME) msg('call prattreq') ...
```

Add the option text to the `.dspf` and the matching `USR00nn` to the `.msgf`, then
build all three targets together — the menu object is built from the display file
and the message file:

```bash
codermake cfdemo/menu.msgf cfdemo/menu.file cfdemo/menu.menu
```

The menu built from this source **has a command line**; the one that ships in
`AIDEMOBASE` does not. If your new option does not appear, you are looking at
`AIDEMOBASE/MENU` because it precedes the task library in the job's library list —
count the input fields on the screen: one means theirs, two means yours.

**The launcher CL** exists for one reason. Pratt's own entry CLs refuse to run
outside Genie:

```cl
RTVENVVAR  ENVVAR(PUI_IS_GENIE) RTNVAL(&ISGENIE)
IF COND(&ISGENIE ¬= '1') THEN(DO)
   SNDMSGDLY M('This program must be used from Genie') D(3)
   RETURN
ENDDO
```

Neither of those commands exists here (`QGPL/RTVENVVAR` has different parameter
names, `SNDMSGDLY` is absent) and `codermake` cannot build a `*CMD`, so the gate is
commented out and `PRATTPO` / `PRATTREQ` set the variable instead:

```cl
ADDENVVAR ENVVAR(PUI_IS_GENIE) VALUE('1') LEVEL(*JOB) REPLACE(*YES)
MONMSG MSGID(CPF0000)
CALL PGM(POAPPUICL) PARM((&OHYY) (&OH#P))
```

**The entry CL contract** is worth copying for a new app — one program, three
entry modes, decided by whether the key fields are blank:

```
year blank, key blank  ->  selection/search screen
year set,   key blank  ->  new record
year set,   key set    ->  straight to detail
```

It also issues `OVRDBF ... WAITRCD(5)` on the files the detail program updates, so
a locked record fails fast instead of hanging the browser.

**At runtime the job also needs** the helper programs (§15) and the task library
ahead of `AIDEMOBASE` in the library list.

---

## 13. Known traps, condensed

| Trap | Signal | Answer |
|---|---|---|
| Embedded SQL in a `.rpgle` | Compile fails oddly | Rename to `.sqlrpgle` |
| `CRTDSPF` on an existing object | CPF5813 | `DLTF` + remove the `build/` stamp |
| Incompatible column type change | SQL0190 | Drop dependents, then the table |
| `DLTF` on a PF with logicals | Silent failure, stale object | Delete LFs/views/DSPFs first |
| Bare `codermake` | Empty `CUSTP` shadowing `AIDEMOBASE` | Always `./pratt/build.sh` |
| Icon names shown as text | No `profoundui.css` | §8a |
| White on white | Dark mode + skin specificity | §8b |
| `applyPropertyCSS is not defined` | `settings.js` not loaded | §7 |
| Empty drop-down / grid headless | Database-driven choices | Expected; verify in a browser |
| Padding a short DDS continuation | Spaces injected into string data | Content is exactly 35 cols |
| Stale `http.server` on the harness port | Screenshot of a 404 page | `pkill` before starting |
| `#pui` as the harness container | Panels render dark | Use another id |
| Headless render looks fine, user's does not | Colour scheme | `PUI_COLOR_SCHEME=dark` |
| A correct, higher-specificity CSS rule has no effect | Unknown wrapper / unverifiable transport | Set a `color` widget property instead - inline wins |
| An error moves from `Onload Error` to `Onrowclick Error` | `external javascript` names a file that 404s, so onload never runs | Remove the property; inline the helpers in the onload prelude (§7.1) |
| A userdata asset 404s at runtime | It only exists in the repo until the task is approved | Carry what you need in the screen JSON, not in a separate file |
| Every database-driven drop-down is empty | The browser's query POSTs to an absolute `/profoundui/...` URL that misses the task proxy | `tools/static_choices.py` - and you must REMOVE the db-choice properties, not just add static ones (§7.2) |
| A drop-down shows one option, `Loading...` | Same cause; the static list was replaced by the pending-query placeholder | As above |
| A newly created PO is numbered 1 | `COADDRES.COAPO#` for plant '94' is the next-number counter and was seeded as 0 | Seed it to the highest number already issued (§11) |
| A user-created record totals .00 while demo data totals correctly | A helper filtered on `OIX`/`OHX`, which PODTLUI only ever ASSIGNS on save and never tests | They are audit flags, not delete flags - do not filter on them |

---

## 14. Still missing from Pratt — ask for these

1. **The real DDS for every file.** Everything in `qsqlsrc/` is a reconstruction.
   It is good enough that their own SQL and their 3,684-line detail program run
   against it unmodified, but their definitions would replace it in an afternoon.
2. **The `OMMGR` display file DDS.** `OMMGRCL.rpgle` (3,915 lines of RPG III) and
   `OMMGRCLO.clp` were supplied but not the display file, which is that program's
   primary `WORKSTN` file — so **OMMGR cannot be compiled at all**, and the Genie
   macro hand-off (PMSELUI → `sessionStorage` → the `.scn` `onload` →
   `pressKey("F8")` / `pressWait("F2")`) cannot be demonstrated end to end.

   **Visible consequence:** on the requisition screen the in-grid eyeball and
   `View Detail (F2)` both route to `OMMGRCLO`, a no-op stub here, so they return
   to the search screen having done nothing. Use `PO Detail (F6)` on that screen,
   or the eyeball on the PO screen, to reach a working detail screen (`PODTLUI`).
   That is not a defect in the conversion — it is the half Pratt have not
   converted yet, and it is the argument for converting it.
3. **Wherever `hybrid-link`, `hybrid-grid` and `hybrid-constant` are defined.**
   Used on all four screens, defined in neither stylesheet sent, and **not** in
   the stock `Hybrid` Genie skin either (checked). `search` is different — it
   needs no styling, it is the marker the saved-search code selects on.
4. Sample data, if the screens should show their own content.

### Small things found in their code, unchanged here

- A live `debugger;` statement in `PODTLUI`'s `SFLWH` button handler — it halts
  the browser for anyone with devtools open.
- The Help buttons point at an internal `http://10.0.100.189/OpenKM/...` URL.
- `PMSELUI` ships as `.rpgle` despite containing 12 `EXEC SQL` statements.
- `PMSELUI`'s WIP sub-select is fragile — see the 8-digit job rule in §11.

### Changes made to their source here, all commented in place

1. `podtlui.rpgle` — dropped `EXTDESC('ZCLIB/ZCCITY') EXTFILE(*EXTDESC)`; no
   `ZCLIB` on this system.
2. `poappuicl.clle` / `pmappuicl.clle` — the `PUI_IS_GENIE` gate is commented out.
   It needs `RTVENVVAR ... RTNVAL()` and `SNDMSGDLY`; `QGPL/RTVENVVAR` exists with
   different parameter names and `SNDMSGDLY` exists nowhere. `PRATTPO`/`PRATTREQ`
   set the variable instead, so restoring the gate is four uncommented lines.
3. `pmselui.rpgle` → `pmselui.sqlrpgle`.
4. All four `.dspf` members gained `external css` / `external javascript`.

---

## 15. Reconstructed helpers

Pratt's code calls these; none were supplied. Contracts are taken verbatim from
their `DCL-PR` prototypes.

**Real implementations:** `SQLORDER` (builds `ORDER BY` from three columns),
`USRRTV` (authority level from `USRLEVEL`; `NOV`/`INT`/`EXP`/`PGMR` map to
`USRMODE` 1–4, anything else 0), `RTVUNN` (default plant), `WHUSR` (default
warehouse), `RTVPOTOT` (PO total + approval limit, drives the "> max" warning),
`RTVPOCOST`, `RTVSAPACCT`, `OILEN`, `LOCKMSG`, `SQL2XLSXD`, `APPOPRTCL`
(marks the PO transmitted so Approve visibly does something).

**Documented no-op stubs**, so the green-screen hand-off buttons stay type safe
and non-fatal: `CXOHPP`, `WHSETCL`, `SAPPOINT`, `PRHDTLUI`, `PMDTLUI`,
`OHMGRCLO`, `OMMGRCLO`, `VNDMGRCL`, `MGRCL`, `VPMGRCL`, `WHMGRCLO`, `WHSELUI`,
`WPMGRCLO`.

`PMDTLUI` is a stub in Pratt's system too — requisition detail is reached by
macroing into OMMGR, which is why its prototype is commented out in `PMSELUI`.
