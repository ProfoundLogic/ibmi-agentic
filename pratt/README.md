# Pratt Industries `puitests` — reconstruction

> **Starting fresh?** Read [`docs/RUNBOOK.md`](docs/RUNBOOK.md). It is written to
> be enough on its own to build a Pratt Profound UI Rich Display screen from the
> ground up in a new task, with no other context: the house conventions, the
> database reconstruction method, how to edit the DDS-embedded widget JSON
> safely, asset wiring, the two hosting-page traps, how to actually see a screen,
> and how to drive one headlessly.

This directory makes the Profound UI application set Harel Davolt (Pratt Industries)
sent on 2 Oct 2026 build and run. Pratt supplied the programs and screens but not
the database, so everything under `qsqlsrc/` and every `.lf` member is **reverse
engineered from how the supplied code uses those files**.

## What came from Pratt, unchanged

| Path | Contents |
|---|---|
| `qddssrc/*.dspf` | The four Rich Display files (DDS with the widget JSON in `HTML()` keywords) |
| `qrpglesrc/podtlui.rpgle` | PO detail/update, 3,684 lines, 34 subprocedures |
| `qrpglesrc/poselui.sqlrpgle` | PO search |
| `qrpglesrc/pmselui.sqlrpgle` | Material requisition search |
| `qrpglesrc/puisrchui.sqlrpgle` | Saved-search service |
| `qrpglesrc/ommgrcl.rpgle` | The 1992 RPG III requisition manager (reference only, see below) |
| `qclsrc/poappuicl.clle`, `pmappuicl.clle` | Application entry points |
| `qclsrc/ommgrcl.clle`, `ommgrclo.clp` | Green-screen routing CL (reference only) |
| `qgeniesrc/*.scn` | Genie screen overlays for OMMGR |
| `../htdocs/profoundui/userdata/` | `css/pratt_profound.css`, `css/styles.css`, `custom/settings.js` |

Four edits were made to Pratt's own source. Each is commented in place:

1. `podtlui.rpgle` — `DCL-F ZCCITY` had `EXTDESC('ZCLIB/ZCCITY') EXTFILE(*EXTDESC)`.
   There is no `ZCLIB` here, so the qualifier was dropped and the file resolves
   through the library list.
2. `poappuicl.clle` / `pmappuicl.clle` — the `PUI_IS_GENIE` gate is commented out.
   It calls `RTVENVVAR ... RTNVAL()` and `SNDMSGDLY`, two site-specific commands;
   `QGPL/RTVENVVAR` exists here but with different parameter names, and
   `SNDMSGDLY` exists nowhere on this system.
3. `pmselui.rpgle` was renamed to `pmselui.sqlrpgle`. It contains 12 `EXEC SQL`
   statements, so it needs `CRTSQLRPGI`; built as `.rpgle` it fails.
4. All four `.dspf` members gained an `external javascript` screen property — see
   *Web assets* below.

## Web assets

The three files Pratt sent live at `htdocs/profoundui/userdata/` in this repo,
which mirrors the Profound UI document root:

| Repo path | Served as |
|---|---|
| `htdocs/profoundui/userdata/css/pratt_profound.css` | `/profoundui/userdata/css/pratt_profound.css` |
| `htdocs/profoundui/userdata/css/styles.css` | `/profoundui/userdata/css/styles.css` |
| `htdocs/profoundui/userdata/custom/settings.js` | `/profoundui/userdata/custom/settings.js` |

`settings.js` is not optional decoration: it defines `applyPropertyCSS`,
`restoreSearch`, `pressWait` and `sanitizeFilename`, which the search screens call
from their `onload` and button handlers. In Pratt's environment it is loaded
globally by their own Genie `start.html`. This instance's `start.html` loads a
different set (`pls.*.js`) and is shared, so the screens have to name the file
themselves. Without it the screens open with:

```
Onload Error: applyPropertyCSS is not defined
```

There was a second, larger problem with the same shape. The screens rendered with
**no widget styling at all** — black background, icon ligature names showing as
literal text (`searchGo To Record (F1)`), a collapsed grid. That is what Profound
UI's own base stylesheet, `/profoundui/proddata/css/profoundui.css`, being absent
from the hosting page looks like. It was reproduced exactly by removing only that
file from `tools/render-screen` (see below).

Profound UI's runtime supports `external css` / `external css 2` / … and
`external javascript` screen properties, and it evaluates a screen's `onload` only
in the callback *after* those files have loaded. `tools/wire_assets.py` therefore
makes each screen name everything it needs, in cascade order:

| Property | File |
|---|---|
| `external css` | `/profoundui/proddata/css/profoundui.css` (base widget styling) |
| `external css 2` | `pratt_profound.css` (Pratt's house overrides, as they had it) |
| `external css 3` | `styles.css` (only where `.comboInput` is used) |
| `external css N` | `pratt_theme_fix.css` (dark-mode label contrast, see below) |
| `external javascript` | `settings.js` |

`profoundui.css` goes first so Pratt's overrides still win, and it is referenced at
its real `proddata` URL so its relative font URLs keep resolving. Loading it twice
is harmless if the hosting page does supply it — which makes the screens correct on
any host, rather than only on one.

### Dark mode

`pratt_theme_fix.css` exists because the Genie skin here switches to a dark
palette under `@media (prefers-color-scheme: dark)` and repaints every div and
span on the screen:

```css
.genie-form--screen p, .genie-form--screen div { color: var(--color--text--primary) }
```

In dark mode that variable is `#dedede`. The selector scores (0,1,1), which
outranks Pratt's own `.label { color: rgb(51,51,51) }` at (0,1,0) — so for anyone
whose machine is in dark mode every label goes near-white, on panels that stay
white because their background comes from Profound UI's stylesheet rather than
the skin. `div.label` / `span.label` score (0,2,1) and win.

It is deliberately narrow — only `.label` is affected, every other widget class
on these screens renders fine. Two broader attempts were tried against the render
harness and rejected: re-pinning the skin's whole light palette turned the
function-key buttons into blank blocks, and re-pinning `--color--text--primary`
for the whole subtree forced dark text onto the skin's dark surfaces too.

## Seeing a screen

`genie_html.sh` renders server side and answers the agent with HTTP 401, so there
is no way to actually look at a Rich Display screen while working on it.
`tools/render-screen` closes that gap: it runs Profound UI's own `runtime.js`
against a screen captured by `genie_get.sh`, in headless Chromium, and screenshots
the result.

```bash
# a cleanly hosted Profound UI page, no Genie skin
tools/render-screen/render.sh /tmp/genie-sessions/X/history/screen-003.json out.png

# what these screens actually get here: base stylesheet + the pls skin + Genie's
# .genie-form--screen wrapper
tools/render-screen/render.sh screen-003.json out.png --as-hosted

# the state before the screens named profoundui.css themselves
tools/render-screen/render.sh screen-003.json out.png --no-base

# dark mode, and trying a stylesheet before wiring it into the screens
PUI_COLOR_SCHEME=dark EXTRA_CSS=/profoundui/userdata/css/pratt_theme_fix.css \
  tools/render-screen/render.sh screen-003.json out.png --as-hosted
```

It reproduces colour, cascade and layout faithfully. It does **not** reproduce
every detail of the skin's button chrome — Genie's real DOM has more structure
around a button than the harness's single wrapper div — so judge buttons in the
browser, not here.

Those properties live inside the widget JSON embedded in the DDS `HTML()`
keywords, so they cannot be hand-edited safely — the JSON is split across several
keywords and each keyword is split across continuation lines at exactly 35
characters with `-` in column 80. `tools/rdf_edit.py` parses that back to Python
and re-emits it byte-compatibly; it round-trips all four files to identical JSON
with no line over 80 columns. Re-run the wiring with:

```bash
python3 tools/wire_assets.py
```

## What was reconstructed

**Database** — 30 tables in modern SQL DDL (`qsqlsrc/*.table.sql`), one view, and
seven DDS logical files (`qddssrc/*.lf`). The logicals are DDS rather than SQL
indexes because `PODTLUI` opens them for record-level I/O and renames their record
formats, which an SQL index cannot do.

Field types were pinned down three ways: literal comparisons in the RPG
(`OHSTAT = 'H'` ⇒ `CHAR(1)`), the non-CONST parameters of Pratt's own `DCL-PR`
prototypes (`SAPPOINT`'s `@OH# ZONED(6)` is why `ORHDR.OH#` is `NUMERIC(6,0)` and
not `DECIMAL`), and the compiler itself — every `RNF7416`/`RNF7421`/`RNF7535`
named a column whose type was still wrong.

**Four tables are byte-position critical.** `PODTLUI` declares externally described
data structures with `POS()` overlays over `VENDPART`, `VENDPACO`, `VENDPABLF` and
`VENDSAZZ` — price-break arrays at fixed offsets such as `CST PACKED(11:2) DIM(5)
POS(392)`. Column order, sizes and the `*FILL*` columns in those four DDL members
exist to put those fields on exactly those bytes. Verified with `DSPFFD`:

| File | Record length | Overlays landed |
|---|---|---|
| `VENDPART` | 496 | 103, 310, 372, 392, 437, 467 |
| `VENDPACO` | 193 | 68, 88, 133, 163 |
| `VENDPABLF` | 535 | 152, 411, 431, 476, 506 |
| `VENDSAZZ` | 410 | 21 |

Do not reorder or resize columns in those members without re-checking `DSPFFD`.

**Helper programs** — eleven programs Pratt's code calls but did not send.
`SQLORDER`, `USRRTV`, `RTVUNN`, `WHUSR`, `RTVPOTOT`, `RTVPOCOST`, `RTVSAPACCT`,
`OILEN`, `LOCKMSG`, `SQL2XLSXD` and `APPOPRTCL` are real implementations against
the reconstructed schema. Thirteen more are documented no-op stubs that keep the
green-screen hand-off buttons type safe (`qrpglesrc/cxohpp.rpgle` and friends).

**`OMMGR` cannot be rebuilt.** Pratt sent `OMMGRCL.rpgle` and `OMMGRCLO.clp`, but
not the `OMMGR` display file DDS — and that display file is the program's primary
`WORKSTN` file. Both sources are kept here for reading; `OMMGRCLO` is a stub.
That also means the Genie macro hand-off (`PMSELUI` → `sessionStorage` → the
`.scn` `onload` → `pressKey("F8")`) cannot be demonstrated end to end.

## Demo data

`data/catalog.py` holds the reference data; `data/gen_seed.py` generates
`data/seed.sql` deterministically. The catalogue is modelled on Pratt's real
business as published on prattindustries.com: six 100% recycled paper mills
(Conyers GA, Staten Island NY, Valparaiso IN, Shreveport LA, Wapakoneta OH,
Henderson KY) feeding corrugating, converting and retail-display plants, plus the
recycling division. The 63 vendor parts are what such an operation buys — corn
starch and wet-end chemicals, paper machine clothing, corrugating rolls, flexo
inks and plates, baling wire, pallets and strapping, display components and plant
MRO. Vendor names are invented so the demo does not imply a trading relationship
with any real supplier.

Transactions are built from archetypes rather than noise: open/partially received,
closed, overdue, on hold above the approval limit, cancelled, drop-ship, ecom,
entered-today, and rush-with-customer-pickup. Dates are anchored on a reference
date (default today) and never fall at a weekend.

```bash
./data/load.sh                 # regenerate and load into $IBMI_BUILD_LIBRARY
./data/load.sh MYLIB           # or into a named library
```

`seed.sql` uses unqualified table names; the target library is supplied at run
time through `RUNSQLSTM DFTRDBCOL`, so no `AITSKxxxxx` name is ever hardcoded.

## Building

```bash
./build.sh                  # build everything under pratt/
./build.sh --list-targets   # what is outstanding, pratt only
```

**Do not run bare `codermake` in this repo** while this work is in progress. The
shared `build/` stamp directory no longer holds stamps for `cfdemo` and `awdemo`
(they were stale — they claimed objects that do not exist in the task library), so
a bare run would also create `cfdemo`'s `CUSTP` as an empty file in the task
library, where it would shadow the populated copy in `AIDEMOBASE`.

`CREATE OR REPLACE TABLE` handles compatible DDL changes in place. A change that
alters a column's type incompatibly needs the object dropped first — drop the
dependent logicals and display files before the table.

## Menu

`cfdemo/qddssrc/menu.dspf` and `cfdemo/menu.msgf` gain two options:

- **4** → `PRATTPO` → `POAPPUICL` → `POSELUI` (PO search) → `PODTLUI` (PO detail)
- **5** → `PRATTREQ` → `PMAPPUICL` → `PMSELUI` (requisition search)

`PRATTPO`/`PRATTREQ` set `PUI_IS_GENIE` for the job before calling Pratt's CL, so
the gate can be restored without changing how the menu works.
