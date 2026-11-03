# PRFDEMOLIB web screens (menu options 7-12)

Agentic (EJS) display-file versions of the three PRFDEMOLIB green screens, in
two tiers. The green screens (options 4, 5, 6) are untouched and still run
exactly as before.

| Menu | Program | Display file | Same logic as | Assets |
|---|---|---|---|---|
| 7. Inventory by Store (Web) | `INV046CLE` -> `INV046E` | `INV046ED` | INV046 | `htdocs/profoundui/userdata/ui/vpinv046/` |
| 8. Item Inquiry (Web) | `INV502E` | `INV502ED` | INV502 | `.../vpinv502/` |
| 9. Ticket Item Inventory (Web) | `TS0110E` | `TS0110ED` | TS0110 | `.../vpts0110/` |
| 10. Inventory by Store (VW Web) | `INV046CLW` -> `INV046W` | `INV046WD` | INV046 (RPG III) | `.../vwinv046/` |
| 11. Item Inquiry (VW Web) | `INV502W` | `INV502WD` | INV502 | `.../vwinv502/` |
| 12. Ticket Item Inventory (VW Web) | `TS0110W` | `TS0110WD` | TS0110 | `.../vwts0110/` |

## Options 7-9: a web version of the green screen

Deliberately **no** improvements: each template is the 5250 screen, field for
field - same labels, order, typed codes, messages, F-key legend (including
the keys whose target programs are missing from the restore, which do
nothing, as on the green screen). Inputs are plain text boxes, tables are
plain tables, one stylesheet (`vpshared/plain.css`, Arial, neutral
colours, no logo), no screen JS at all: every button is an inline
`pui.submit(...)`. It only adds what any web page gives you: clickable keys,
inputs you can click into, and a layout that fits the window down to a phone.

`INV046E`, `INV502E`, `TS0110E` and `INV046CLE` are **generated** from the
`*W` members (`python3 prfdemolib/tools/gen-plain-clones.py`; `--check` to
verify) - identical RPG, only the display-file name (same length, so no
fixed-format column moves), member text and header comments differ. The
`*ED` JSON files have exactly the same field contract as the `*WD` ones (same
formats, fields, lengths, subfiles); only the template/CSS URLs differ.
Never edit the generated members; edit the `*W` source and regenerate.

## Options 10-12: the Variety Wholesalers demo

Shared design system and behaviour: `.../vwshared/` (theme.css with the logo
inlined, app.css, app.js, lookups.js).

## Same RPG under the covers

Each web program is a copy of the original with a small, tagged (`VW01`) set
of changes. Every other line - files, key lists, calculations, subroutines,
the F-key `SELECT`s - is the original.

| | Lines | Original lines replaced | Lines added (`VW01`) |
|---|---|---|---|
| INV502W | 1,215 | 12 | ~100 (two routines, one message wrapper, comments) |
| TS0110W | 1,071 | 6 | ~80 |
| INV046W | 1,191 | 4 (vs. the mechanical conversion) | ~80 |

What the changes do:

- **Display file** renamed to the EJS one, plus `HANDLER('PROFOUNDUI(HANDLER)')`
  (Profound UI Open Access) and, where the program had none, an `INFDS` so the
  AID byte is available.
- **`ejsOut` before each `EXFMT`** publishes `*IN01-99` to the template as one
  99-character field `INDS` (EJS formats have no indicators of their own), and
  clears `ACTION`.
- **`ejsIn` after each `EXFMT`** turns the button the user pressed (`ACTION` =
  `F01`..`F24`, `PAGEUP`, `PAGEDOWN`, `ENTER`) - or, for a real keyboard key,
  the INFDS AID the handler fills - into exactly what the program already
  tests: `KbdKey` in INV502, `*INKA..*INKJ` in TS0110, `*INKA..*INKH` plus the
  DDS `CHANGE(02)` and `ROLLUP(03)` response indicators in INV046.
- **Written-only formats** (INV046 `SCRN3`, INV502 `SCREEN2B`/`MSGCTL`/`DUMMY`,
  TS0110 `CFKEYS`) are folded into the format the program reads; their
  `WRITE`s are commented out.
- **Subfile clear**: EJS subfiles are cleared by a character field (`EJSCLR`),
  set from the original `SFLCLR` indicator.
- **INV502 messages**: `SndMsgW()` calls the original `SndMsg()` and also puts
  the message text in `MSG` (EJS screens have no message subfile).
- **TS0110 row styling**: `EjsRow` copies indicators 10-15 into the subfile
  field `SFIND` before each `WRITE SkuSfl`, so the template knows headings,
  totals and SKU rows apart.

### INV046 had to become ILE RPG first

Open Access handlers exist only in ILE RPG, and INV046 is RPG III.
`prfdemolib/tools/cvtrpg3.py` converts it mechanically (the CVTRPGSRC job:
column layout, `DEFN`->`DEFINE`, `SETOF`->`SETOFF`, `ARR,N`->`ARR(N)`, half
adjust -> `(H)`, E/I specs -> D specs). The pure conversion was compiled as a
temporary INV046X and driven side by side with the original INV046 through 12
steps (store group, inventory group, region and store searches, paging, F5,
three error cases): the 5250 screens were **byte-identical**. INV046W is that
conversion plus the `VW01` changes.

## Building

```bash
python3 prfdemolib/web/build.py            # src/ -> htdocs (templates assembled, logo inlined, ejs syntax-checked)
python3 prfdemolib/web/build.py --check    # fail if htdocs is stale
python3 prfdemolib/tools/gen-plain-clones.py   # *W -> *E members (options 7-9)
codermake prfdemolib/inv046wd.file prfdemolib/inv046w.pgm prfdemolib/inv046clw.pgm \
          prfdemolib/inv502wd.file prfdemolib/inv502w.pgm \
          prfdemolib/ts0110wd.file prfdemolib/ts0110w.pgm \
          prfdemolib/inv046ed.file prfdemolib/inv046e.pgm prfdemolib/inv046cle.pgm \
          prfdemolib/inv502ed.file prfdemolib/inv502e.pgm \
          prfdemolib/ts0110ed.file prfdemolib/ts0110e.pgm
codermake cfdemo/menu.file cfdemo/menu.msgf cfdemo/menu.menu     # menu options 4-12
```

Edit `prfdemolib/web/src/`, never the generated files under `htdocs/`.
`CRTDSPF` cannot replace a file, so delete `*WD`/`*ED` display files (and the
`MENU` *FILE/*MSGF/*MENU) before rebuilding them. The `?v=1` in the JSON is compiled into the display file;
bump it (and rebuild) when shipping changed assets to a server that caches.

**Deployment of `htdocs/`** is CoderFlow's job: during the task its proxy
serves these files from the working tree, and on approval CoderFlow copies
them to the Profound UI document root. Do not scp them.

`lookups.js` (the catalogue behind the pickers) is generated from the demo
tables: `python3 prfdemolib/tools/gen-vw-lookups.py <LIB> > prfdemolib/web/src/lookups.js`,
then rebuild. Regenerate after reloading or reseeding data.

## Testing

```bash
node prfdemolib/web/tools/drive.js                          # 145 behaviour checks on options 10-12 + negative control
node prfdemolib/web/tools/render.js prfdemolib/web/fixtures/<f>.json \
     [--widths 1440,1280,1024,768,390] [--theme dark] [--nojs]   # screenshots + overflow/JS errors
# (fixtures are option 10-12 captures; for options 7-9 capture a plain screen,
#  or point a copy's "template" at vp<app>/ and its "css" at vpshared/plain.css)
node ~/.claude/skills/dspf-to-ejs/scripts/preflight.js --json prfdemolib/qddssrc/<x>wd.json \
     --assets <dir with vwshared/* (or vpshared/*) and the app's files>   # identifier audit, render, overflow, padding
```

Fixtures are real screens captured from live sessions
(`node prfdemolib/web/tools/fixture.js /tmp/genie-sessions/<S>/history/screen-NNN.json out.json`).
Both harnesses mount the screen inside `<div id="5250">` with both forms of
Genie's skin rule (bare `div` and `[id="5250"] div`). `drive.js` opens every
picker at 1440/1024/390 px and proves with `document.elementFromPoint` probes that
nothing paints over the list, that no description is cut off and that no
container or the window clips it. Both harnesses also refuse
every network request, and render the template client-side exactly as
Profound UI does.

## Two formats in one response (the "stuck on Loading SKU information" fix)

Profound UI renders **every** EJS format of the screen layer into **one**
container, and each one as soon as its template arrives over XHR. So when a
response carries two formats, whichever template download finishes *last* is
what the user sees, not the active format. Writing a subfile-control format
keeps the format that was on screen, so three responses carry two formats:

| Response | Formats in the layer (active last) |
|---|---|
| TS0110 prompt -> SKU list | `WAITWIN` (written by `LoadSfl`), `SKUCTL` |
| TS0110 F1 on the confirm window / Enter on the notice | `DISTWIN` or `NONEWIN`, `SKUCTL` |
| INV502 SKU prompt -> detail | `SCREEN1`, `SCREEN2C` |

With the race lost, the screen showed "Loading SKU information..." while the
program sat in `EXFMT SkuCtl` waiting for input. `build.py` (`GUARDS`) now
generates each of those lingering formats' templates as "render the
list/detail screen when its subfile is in the data, else my own screen"
(Profound UI merges every format's data into the template data), and their
`js` arrays load the list/detail screen JS too (`VW.ready` only initialises
the screen that is actually present). Either arrival order now shows the
right screen. The RPG is unchanged: the `WRITE WaitWin` is what clears the
prompt off the layer. The dspf-to-ejs preflight identifier audit flags those
four guarded formats for using the other format's fields; that is expected.

## Design system

Brand tokens from vwstores.com's own theme stylesheet: blue `#0054a6`, dark
`#003f7f`, ink `#152235`, muted `#5f6d7c`, line `#dfe6ee`, soft `#f4f7fb`,
footer navy `#071c33`; Arial/Helvetica; 800-weight buttons and letter-spaced
eyebrows; 9 / 14 / 20 / 24 px radii; the site's soft blue shadow. The logo is
the real `VW_logo.png`, inlined as a data URI.

Rules that keep it working inside Genie (all learned the hard way on earlier
projects): every selector scoped under `.vw-app`; Genie's blanket `div` rule
neutralised with `.vw-app.vw-app :where(...)` - specificity (0,2,0), because the
Skyline-lineage skins scope it as `[id="5250"] div` (0,1,1) and a one-class reset
loses, turning every card into a `z-index:10` layer that paints over the
dropdowns above it; a card or field containing an open picker gets a higher
layer (`:focus-within`, z-index only - never positioning, the distribution bar
is a sticky card); picker lists are at least as wide as their field and wide
enough for a full description (`width: max-content`, capped to the window,
flipped to the right edge when needed); no `position:fixed`, no `100vh`;
`minmax(0,1fr)` grids; screen JS through inline handlers calling `window.VW`
(it can run before the template exists); a bootstrap `VW` defined by the
template itself so buttons work even if `app.js` never loads; every submit
passes its field values explicitly; theme read in the template so dark mode
never flashes.

## What the browser adds

- Type-ahead pickers for SKU, store, region, district, store group, inventory
  group and department (replacing the F3/F4 prompt programs missing from the
  restore); Enter on a typed name picks the match.
- Command palette (Ctrl+K), keyboard cheat sheet (?), `/` to search, Alt+Left/Right
  for previous/next SKU, every green-screen F-key bound to its button.
- Sortable, filterable tables (filter covers city names the 5250 never shows),
  CSV export, print styles, compact/comfortable density, dark mode - all
  remembered per browser.
- KPI strips, inline bars, heat-coded weeks of supply, supply-mix bands,
  sell-through bars, network position bars - all computed from fields the
  program already sends.
- Recently viewed SKUs and store/department lists.
- TS0110 distribution builder: live SKU/pack/unit/retail totals, +/- steppers,
  Up/Down between pack fields, over-allocation warnings, quick-filter chips,
  Smart fill (1 pack at 85%+ sell-through, 2 at 95%+, capped at packs
  available), a confirmation screen listing exactly what will ship, and a
  toast once the IBM i has created the member.
- Responsive from 1440 px down to a 390 px phone (tables become labelled cards).
