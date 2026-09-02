# EJS / Profound UI screen playbook

Standards and hard-won failure modes for building EJS Rich Display screens in this
project, on this IBM i, behind this proxy. Written while building the J.H. Fletcher
demo (`FLTDEMO`, Jira epic GJA-907); everything here is something that actually
broke or actually had to be decided.

**Read §1 and §2 before writing a screen.** The rest is reference.

Reference implementation: `cfdemo/qddssrc/fl*eo.json`, `cfdemo/qrpglesrc/fl*eo.rpgle`,
`htdocs/profoundui/userdata/ui/{fletcher,flfleet,flparts,fldup,flsched}/`.

---

## 1. The delivery chain, and why the shim exists

An EJS screen is not just an RDF plus a template. In this environment three separate
browser-side fetches are involved, and **two of them do not survive the proxy.**

`pui.QD` in `genie.js` renders EJS screens. It:

1. fetches the **template** with a plain `XMLHttpRequest`, driving `onreadystatechange`
2. loads each **stylesheet** in `ejs css` as a `<link>` tag via `pui.Mh.pz`
3. loads each **script** in `ejs js` as a `<script>` tag
4. calls an internal escape helper to break the container out of the Genie layout

| Step | Through the proxy | Consequence if you ignore it |
|---|---|---|
| Template XHR | **dropped** | Blank screen. `pui.QD` gets `null` and leaves the container empty. |
| CSS `<link>` | **dropped** | Structure with zero styling — raw text on the skin background. Looks worse than blank. |
| Script `<script>` | **dropped** | Silent. Nothing binds, no error. |
| Full-bleed escape | **no-ops on `pls`** | Screen renders inside the 5250-sized box, small and tucked top-left. |

The assets themselves are fine — `fleet.ejs`, the theme CSS and the logo all return
**HTTP 200 when fetched directly.** It is specifically the in-page fetches that fail.

### Therefore: everything goes through the shim

`tools/gen-fletcher-shim.py` snapshots templates and stylesheets into every Genie
skin's `start.html` between `BEGIN/END` markers, and:

- patches `XMLHttpRequest.prototype.open/send` so **only** the snapshotted template
  URLs resolve locally; every other request goes to the network untouched
- injects all stylesheets as one `<style>` element
- defines shared JS globals (`window.flPickRow`, `window.flSetField`)
- re-implements the full-bleed escape keyed on our own wrapper class
- sizes grids from their measured position
- initialises sortable headings

```bash
tools/gen-fletcher-shim.py            # install / refresh
tools/gen-fletcher-shim.py --check    # exit 1 if any skin is stale
```

> ### ⚠️ Four things that will catch you
> 1. **Re-run the generator after *every* template or CSS edit.** A stale snapshot
>    silently serves the previous version, which is far more confusing than a blank
>    screen. `--check` exits non-zero; wire it into anything automated.
> 2. **`start.html` is ~3 MB and aggressively cached. Hard-refresh** after every
>    shim change or you are looking at the old one.
> 3. **Install into all skins.** `pls` is the default here, but don't assume.
> 4. **Never put screen JS in the RDF `js` array.** Two independent reasons: the
>    fetch is dropped, *and* files in that array load **before** the template
>    renders, so `addEventListener` binds to nothing. That second one is the root
>    cause of GJA-891 — the reference screen's column sorting has never worked.

### Why `readState` needs shadowing

`readyState`, `status` and `responseText` are read-only getters on the native
`XMLHttpRequest` prototype. The shim shadows them per-instance with
`Object.defineProperty` before firing the callback. If you rewrite this, keep that.

### Why the escape has to be reimplemented

`genie.js` only applies its escape after finding an ancestor with one of *its own*
class names:

```js
function b(t){ var z=/\b(mainDiv|middleDiv|insideDiv|topDiv)\b/, w=!1, A=t;
  while(A && A!==document.documentElement){ if(z.test(A.className)){w=!0;break} A=A.parentElement }
  if(!w) return null;          // no marker class -> no escape at all
```

The `pls` skin carries none of those, so the escape returns `null`. The shim runs the
same algorithm and the same CSS overrides, keyed on `.fl-screen`.

---

## 2. Adding a new screen — the checklist

```
□ RDF JSON in cfdemo/qddssrc/<name>.json
    □ one subfile per format, never two (see §7)
    □ cache-bust every template/css/js URL (?v=N)
    □ size hint/description fields generously - char(80) (60 still truncated)
    □ any secondary list travels as a JSON string field (§7), not a 2nd subfile
□ Template in htdocs/profoundui/userdata/ui/<app>/<screen>.ejs
    □ wrapper <div class="fl-screen <prefix>-screen">
    □ every optional value guarded with typeof
    □ hidden <input type="hidden" name="action" value="">
    □ inline onclick/ondblclick only - no js array
    □ renders correctly with JS disabled
□ Stylesheet, every rule prefixed (§4)
□ RPGOA program in cfdemo/qrpglesrc/<name>.rpgle
□ Rules.mk: <name>.file and <name>.pgm
□ Register template + CSS in tools/gen-fletcher-shim.py (TEMPLATES / STYLES)
□ tools/ejs-preflight.sh          -> all templates render
□ codermake LIBRARY=FLTDEMO <name>.file <name>.pgm
□ tools/deploy-ejs.sh             -> assets copied and verified
□ tools/gen-fletcher-shim.py      -> snapshot refreshed
□ Verify in a session AND render offline (§11 - they see different things)
□ Hard-refresh the browser
```

---

## 3. Screen and file naming

| Thing | Convention | Example |
|---|---|---|
| RDF / program | `fl<area><n>eo` | `flparteo`, `flpart1eo` |
| List → detail | list calls detail via `extpgm` | `flfleeteo` → `flfleet1eo` |
| Wrapper class | `fl-screen <prefix>-screen` | `fl-screen flparts-screen` |
| CSS prefix | one per screen | `.flparts-` `.flfleet-` `.flmach-` `.fldup-` `.flsch-` `.flswo-` `.flsdq-` `.fllk-` `.flcl-` |
| Asset directory | per app, not per screen | `ui/flparts/`, `ui/flfleet/`, `ui/flsched/` |
| Shared assets | `ui/fletcher/` | theme, logo |

---

## 4. CSS standards

**Scope everything to the wrapper class.** No `html`/`body` rules, no
`position: fixed`, no `100vh` for layout. All of them fight the Genie skin.

**Never reuse a generic class name.** `.header`, `.toolbar`, `.btn`, `.message` and
`.footer` are declared **globally** in *both* `wrkcuste` reference stylesheets with
conflicting rules. The shim injects all CSS globally, so they collide and last-loaded
wins. Use a screen prefix.

**Keep your own reset weaker than your layout rules.**
`.x-screen div { padding:0 }` has specificity (0,1,1) and silently beats
`.x-body { padding:20px }` at (0,1,0). Prefix layout rules with the wrapper and
compound for modifiers.

**Neutralise Genie's global `div` rule** — `div { z-index:10; white-space:nowrap;
padding:1px }` traps dropdowns in stacking contexts and stops all text wrapping.
Reset all three inside the wrapper, deliberately narrowly.

**Images: data URI in the stylesheet, not `<img src>`.** An absolute
`/profoundui/userdata/...` path is exactly what the proxy fails to resolve, and a
broken logo is highly visible. The logo lives as a `background-image` data URI in
`fletcher-theme.css` and travels with the CSS.

**Brand tokens as custom properties on the wrapper**, so a screen can override one
value without forking the theme.

**Write responsive rules from the start.** Fletcher runs tablets on the warehouse
floor and has said wide screens present badly there. Retrofitting is worse.

---

## 5. Grid standards

The Genie container **does not scroll**. A long list therefore pushes the footer and
its F-key buttons out of reach entirely. Every grid that can run long gets
`.fl-grid-scroll`.

**Bound the height by both a row cap and the viewport:**

```css
max-height: min(var(--fl-grid-max, 601px), calc(100vh - var(--fl-grid-reserve, 300px)));
```

- `--fl-grid-max` is the row cap — `header 31px + N rows at 38px`, **measured in the
  browser, not derived from padding**
- `--fl-grid-reserve` accounts for chrome above the grid plus the footer
- screens with more chrome override both — Parts Finder uses `487px` / `400px` (12 rows)
  against the 15-row default

**The shim then refines it from the measured position** —
`innerHeight − grid.top − footerHeight − margin`, still capped by `--fl-grid-max`, and
re-runs on resize. The CSS is the JS-disabled fallback.

> **Why both:** a fixed row cap alone is wrong. Capping at 15 rows made the grid
> *taller* than before and pushed the footer off a 760px window — fixing the number
> asked for while leaving the actual bug. Measuring beats counting chrome, and this
> needed revisiting twice before it was done properly.

**Sticky header:** `position: sticky; top: 0` on `th`. The sort indicator must use
`float`, **not** absolute positioning — setting `position` on the `th` breaks sticky.

**Never silently truncate.** Show a row-count note when the data exceeds what fits:
*"43 rows — scroll the grid for the rest, or narrow the search."*

**Watch for heading wrap when you add columns.** Sort indicators cost ~18px per
heading; any column sized before indicators existed is at risk. Measure wrapping via
the text node's line-box count (`Range.getClientRects().length`), not element height —
all `th` in a row share the row height, so height tells you nothing.

---

## 6. Interaction standards

**Double-click a row performs that row's default action.** On most screens that is
option `5`; on the duplicate-cluster review it is `1` (keep this part), because keeping
is the primary decision there. State it as "the default action", not "sets 5" — the
gesture is the standard, the option number is per-screen. Typing the option always
works and is the only way to reach non-default options.

**If a screen's option scheme *is* the screen, document it on the screen.** The
duplicate review needs `1` to keep, `2` to merge and blank to leave alone; a one-line
hint was not enough and the screen was unusable until it carried a numbered how-to
panel. Instructions belong adjacent to the field, not in a tooltip.

**Read every changed subfile row, not just the first.** A screen taking one selection
can stop at the first non-blank option; a screen taking a *set* of marks must loop the
whole `readc`. And because the subfile is reloaded from the database each cycle, pending
marks must be held in the program and re-applied on redisplay.

**Don't force a whole-group decision when partial is the normal case.** The cluster
review originally merged every member into one survivor, which cannot express "these two
are duplicates, that third one is a separate part" — which is the common case.

**Inline handlers calling window globals.** `ondblclick="flPickRow(this,'5')"`, with
`flPickRow` defined in the shim. Never `addEventListener` from a `js`-array file.
For anything that is not a subfile option or a `<select>` — the board's clickable
work-centre lanes, the chip that clears a filter — use `flSetField(name, value,
action)`, also in the shim. Do not define the helper in the template: an inline
`<script>` tag does not execute here.

**Setting `.value` programmatically is sufficient.** `genie.js` collects field values
at submit time by scanning named inputs:

```js
function e(t){ for(var z={},w=["input","select","textarea"],A=0; ...
  F.name&&(z[F.name]=F.value||"") ... return z}
```

No synthetic `input`/`change` events needed.

**Affordance is mandatory.** `cursor: pointer`, a hover tint, and a `title` naming what
the action will do. Without it nobody discovers the double-click.

**Every screen must render and be usable with no JS.** None of the Fletcher screens
ship a `js` array. Double-click and sorting are enhancements; typing works regardless.

**Sortable headings — click to sort, click again to reverse.** Type-aware:

| Content | Sort key |
|---|---|
| `MM/DD/YYYY` | `YYYYMMDD` numeric — chronological, not alphabetical |
| Currency / counts | numeric, stripping `$` and thousands separators |
| Percentages | numeric, stripping `%` **and a leading `+`** |
| Anything else | lower-cased string |
| Blank / em-dash | always last |

Ties are stable on original row order. Columns containing inputs are skipped
automatically.

> **The `+` matters.** Positive trend values carry a `+`, which fails a
> `/^-?\d*\.?\d+$/` test and falls through to a *string* sort — and a string sort of
> signed numbers looks plausible enough at a glance to ship unnoticed. Found by
> clicking the column, not by reading the code.

**Lookups open unfiltered.** Seeding a lookup with the value already on screen returns
the record the user is trying to move away from, which makes it useless. **But** when
the user typed something that failed to resolve, carry that text into the lookup — it
is meaningful and retyping is annoying. Two entry paths, deliberately different;
comment the reason in the source so it doesn't get "tidied".

**Prefer a `<select>` that submits on change over a coded text field plus a button.**
`genie.js` collects values by scanning `input`, `select` and `textarea` and reading
`.value`, so a dropdown needs no special handling — `onchange="pui.submit({action:'X'})"`
is enough, and the RPG side is unchanged. If you find yourself writing a legend to
explain single-letter codes, the field should have been a dropdown. Put the count in
each option label and it doubles as a progress summary.

**Submit contract:** hidden `action` input, `pui.submit({action:'X'})` for named
actions, plain `pui.submit()` for a subfile selection, `data-fkey="F3"` on buttons.

---

## 7. RDF and field standards

**Field names arrive lowercase** in the template. PUI lowercases DDS field names.

**An undefined identifier aborts the entire render** — a white screen, no error.
Guard every optional value:

```
<% if (typeof msg !== 'undefined' && msg && msg.trim()) { %>
```

**Two subfiles in one format compile to malformed DDS** (SFL/SFLCTL ordering). One
program per screen; the list calls the detail via `extpgm`. That is also the house
pattern in `wrkcusteo` → `wrkcust1eo`.

**A screen needing a *second* list carries it as one JSON string field.** The
schedule board's work-centre lane strip, the confidence screen's category rollup
and the work order's own findings all travel this way, parsed in the template
inside a `try`/`catch` that degrades to an empty list. Three rules:

- **Size the field for the whole payload and count what you drop.** `char(900)`
  held 10 of 11 lanes; the screen said so out loud and the eleventh silently
  looked like it did not exist. `char(1400)` fits, and `CRTDSPF` accepts a field
  that size in an RDF format without complaint.
- **Every number goes through `fl_jsonNum`.** `%char()` on a packed value drops
  the leading zero — `0.0` comes out as `.0` and `-0.5` as `-.5`. Neither is valid
  JSON, `JSON.parse` throws, and the *entire* strip renders as nothing with no
  error anywhere. One work centre at zero load was enough to blank the lane strip.
- **Every string goes through `fl_jsonStr`**, and the parse is wrapped. A payload
  problem must cost you the list, never the screen.

**Size hint fields at `char(80)`, not `char(60)`.** `soptdesc` at 60 truncated
*"Double-click a finding to open the work order it is on, or type 5"* to
*"…or t"* — the fourth time this field has been too small.

**Cache-bust `template`, `css` and `js` URLs** whenever an asset changes. Strip `?v=`
when mapping a URL to a filesystem path.

**Size hint and description fields generously.** `soptdesc` at `char(20)` then
`char(40)` truncated option hints mid-word **three separate times**. Use `char(60)`
and stop trimming prose to fit a field.

**Dates arrive as `char(10)`, pre-formatted in RPG.** Keeps date handling out of the
template entirely. **Numerics stay numeric** — presentation is the screen's job, and
they must zero-suppress but still display zero (`EDTCDE(3)` semantics).

**Coded values arrive with their description text** from the service program, never
translated in the template. Translating in the template already produced a live
inconsistency — the same code reads "Retail" in EJS and "Resident" elsewhere
(GJA-893).

---

## 8. RPG and data-access standards

```rpg
exec sql set option commit = *none;      // CRTSQLRPGI defaults to COMMIT(*CHG);
exec sql set option closqlcsr = *endmod; // on non-journaled files inserts vanish
```

| Trap | Symptom | Rule |
|---|---|---|
| `like()` on a DS host variable | bogus `SQL0314 host variable not unique`, blaming an unrelated variable | use `likeds()`, or an explicit subfield list |
| Fixed → varchar host variable | `LIKE` matches nothing | `%trim()` on assignment before building the pattern |
| `like()` off a display-file field | `SQL0312` | declare host variables independently |
| DECIMAL ÷ DECIMAL | result scale truncated to **0**, so every ratio is `0` and `ORDER BY` becomes a no-op | cast with `DOUBLE()`, or compute in RPG |
| `DEC()` on a ratio | overflow on a large swing (+135%) | compute percentages in RPG |
| `CASE WHEN EXISTS` | `SQL0104 Token EXISTS was not valid` | rewrite as a `LEFT JOIN` |
| `%editc(int:'X')` | returns **hex** | build zero-padded strings from `%char` + `%subst` |
| `%subst(v : 1 : n)` on a VARCHAR shorter than `n` | `RNX0100` at runtime | guard with `if %len(v) > n`, or assign straight to a fixed-length field (it pads/truncates) |
| Appending past a VARCHAR's declared length | `RNX0100` | size the working buffer well over the column, then truncate explicitly on the way out |
| Procedure call or array-indexed DS subfield as an SQL host variable | `SQL0104` / `SQL0312` | stage every value into a plain scalar first |
| Global `dcl-s` placed between procedures | `RNF0256 Specification found between procedures` | all globals go before the first `dcl-proc` |
| `%date(num : *iso0)` | `RNF7514 The Date, Time, or Timestamp separator '0' is not allowed with numeric entry` | a number has no separator: use `*iso` when converting **from** a numeric. `%char(date : *iso0)` — the other direction — is correct and is what `fl_today` uses |
| A variable named `out` (or `in`) | `RNF7064 The Factor 2 operand of IN or OUT is not a data area`, plus `RNF7260`/`RNF5008` about Factor operands in a **free-form** program | `IN` and `OUT` are opcodes, so `out = x;` parses as a data-area operation. Rename it. The messages name nothing recognisable and mention Factor 1/2 in free-form source, which is the tell |
| `%char()` on a packed value | leading zero dropped: `.0`, `-.5` | fine for display, **fatal in JSON** — see §7 |
| Procedure called before it is defined, same module | `RNF7030` unresolved | define it earlier in the source or prototype it before the first call. `fl_dqScan` sits ahead of the loader that calls it for exactly this reason |

**One formatter, one rule, one place.** Date display goes through a single
`fl_fmtDate`; the modernization rule lives once in `fl_modFlag` and is used by both
the list and the summary. Before this, eight cursors each built their own
`YYYY-MM-DD` concat — changing the format would have been eight edits and a missed one.

**Business rules belong in the service program, not the screen**, so every screen
agrees.

---

## 9. DDS standards

**Keyword budget is 28 characters.** Keywords start at column 45, so
`TEXT('...')` content can only run to column 78. Overflow does **not** fail cleanly —
it truncates mid-string and reports `CPD7508 Closing apostrophe missing`, which points
at entirely the wrong problem. This hit 7 of 9 physical files. **Record-level `TEXT()`
has the same budget.** Menu constants get 34.

**Generate DDS with a hard column-80 assertion** rather than hand-typing it. Given the
above, this is worth the ten lines.

**`CRTPF` uses `REPLACE(*NO)`.** Rebuilding an existing physical file returns
`CPF5813` and writes **no stamp**, so make retries it on every later build and blocks
everything downstream — and a *successful* retry `CRTPF`s an empty file over your
seeded data. **Adopt the object by touching the stamp**; never let it rebuild.

**`.menu` needs both prerequisites normal:**

```make
fltmenu.menu: fltmenu.msgf fltmenu.file      # correct
menu.menu:    menu.msgf | menu.file          # WRONG - recipe silently dropped
```

The pattern rule only matches when both are normal. As order-only, codermake reports
**success** and builds no `*MENU` object at all. This was a live bug in the repo.

**`CRTMNU` leaves DSPF/MSGF references as `*LIBL`**, so library list order decides
which display file a menu actually uses.

**`RETURN` does not exit a DDS menu.** F3 does. Label the menu accordingly rather than
offering an option that does nothing.

**A signed-on session caches the menu it resolved at sign-on.** Sign off and back on
before concluding a menu change didn't work.

---

## 10. Build and tooling

**Library comes only from `IBMI_BUILD_LIBRARY`.** `codermake 3.1.1` has no CLI option
for it. A per-invocation make-variable override works and changes nothing persistent:

```bash
codermake LIBRARY=FLTDEMO <target>
```

**Stamps under `build/` are not keyed by library.** Point the build at a new library
with old stamps present and codermake concludes everything already exists, builds
**nothing**, and reports success. Clear stamps or use `CODERMAKE_LIBRARY_MAP`.

**codermake exits 0 when run outside the repo root** ("No Rules.mk files found"). A
scripted loop reads that as success. **Verify objects exist; never trust the exit
code.** This produced a false "9/9 built" when nothing had been created.

**Never hardcode the asset directory list.** `tools/deploy-ejs.sh` listed
`fletcher flfleet flparts` in two separate loops, so the A7 duplicate-review assets
under `ui/fldup` were never deployed and the script still reported success on
everything it did copy. It now discovers the directories and exits non-zero on a
verification failure.

**Delete objects from the task library** once they live in the persistent one — a task
library normally precedes it on the library list and a stale same-named object wins
every resolution.

**`aitool sql` executes only the LAST statement of a semicolon-separated batch** and
still returns `success: true`. A seed load reported *"applied 1232, failed 0"* and
inserted **12 rows**. Send one statement per call; fold consecutive same-table inserts
into multi-row `VALUES`. Also use `--input @file.json` — inline single quotes get
mangled.

**Seeding is a separate, re-runnable step**, never a build side effect. Physical files
are order-only prerequisites; a rebuild empties them.

**The catalog can be blind on `AITSK*` libraries** — `SYSCOLUMNS` returns 0 rows for
real, queryable files. Verify with a direct query or `DSPLIB`.

---

## 11. Verification — what each test can and cannot see

This is the section that cost the most to learn.

| Method | Proves | **Blind to** |
|---|---|---|
| Headless Genie session | RPG logic, RDF wiring, subfile data, navigation | **everything the browser does** — blank screens, missing CSS, layout, clicks |
| `tools/ejs-preflight.sh` | template ↔ RDF field contract, asset URLs resolve | runtime data, layout, styling |
| `tools/ejs-screenshot.py` | actual rendering with real data | server-side behaviour |
| Browser + stubbed `pui` | click handlers, sorting, grid sizing | RPG behaviour |
| `codermake` exit code | **almost nothing** | see §10 |

**Three separate rendering defects — blank, corner-boxed, unstyled — all passed the
headless session test.** That test drives the RDF data stream; it cannot see what a
browser does with the result. Leaning on it as broader evidence than it is was the
single most expensive mistake in this build.

**`aitool ejs-validate` truncates its stdout at about 64 KB.** A grid of 36 rows
produces a response cut mid-string, and the JSON will not parse. That is a limit of
the renderer, not a fault in the template — `tools/ejs-screenshot.py` now halves the
subfile and retries, and prints how many rows it dropped. A screenshot of a shorter
grid presented as the whole thing would be worse than no screenshot.

**Test the harness, not just the code.** A grid-sizing check reported 10 rows at every
window height and looked exactly like a CSS bug. The CSS was correct — `viewportSize`
wasn't being applied, so `window.innerHeight` was 720 in every case. Check the
measurement before doubting the thing measured.

**Click things.** The percent-sort bug and the double-click handler were both verified
by driving the UI, not by reading the source.

```bash
tools/ejs-preflight.sh                                   # contract test
tools/ejs-screenshot.py <rdf.json> <session-screen.json> out.png
tools/gen-fletcher-shim.py --check                       # snapshot currency
```

---

## 12. Demo-data standards

**The seed data is the deliverable, not the tables.** The tables took an hour; the data
is what makes or breaks a demo.

**Domain plausibility is not optional.** The first fitment BOM assigned parts to
assemblies at random, which put drill steel under HYDRAULIC SYSTEM and a water pump
under BOOM ASSEMBLY. A domain expert spots that in seconds and it undermines
credibility on a screen whose entire job is to be credible. Map every part to one
plausible assembly.

**Screens open populated**, on a record chosen **by query** — the steepest-declining
account, the richest fitment list — never a hardcoded ID. An empty entry form is a
weak opening even when it is styled correctly.

**Computed, not generated.** Every number in an insight panel must trace to a query.
Model-written prose over stub data is exactly what produced *"that was a great demo,
but what's actually there?"*

**Mark it.** A `DEMO DATA — FICTIONAL RECORDS` badge built into the screen, so nobody
has to remember to say it. **Invent customer names** — never guess at real ones.

**A useful side effect:** grouping duplicate families into one assembly put the
variants on consecutive rows, so the duplication is visible without opening the detail
panel at all. Data layout can do demo work that UI cannot.

---

## 13. Failure catalogue — symptom first

| Symptom | Cause | Fix |
|---|---|---|
| Blank screen, no error | template XHR dropped | shim snapshot; hard-refresh |
| Structure, no styling | CSS `<link>` dropped | shim inlines CSS |
| Screen small, top-left corner | `genie.js` escape no-ops on this skin | shim escape keyed on `.fl-screen` |
| White screen after a template edit | undefined identifier aborts render | `typeof` guard; run pre-flight |
| Old content after an edit | stale shim snapshot / cached `start.html` | regenerate; hard-refresh |
| Footer buttons unreachable | grid taller than the viewport allows | `.fl-grid-scroll` bounded by row cap **and** viewport |
| Heading wrapped to two lines | sort indicator added ~18px | widen that column |
| Sort looks wrong on percentages | leading `+` fails numeric test | strip `+` in the sort key |
| Column sorting never fires | handler bound from the `js` array | inline handler + shim global |
| Dropdown clipped / text won't wrap | Genie's global `div` rule | reset `z-index`/`white-space`/`padding` |
| Option hint truncated mid-word | `char(20)`/`char(40)` field | widen to `char(60)` |
| `CPD7508 Closing apostrophe missing` | DDS keyword past column 80 | 28-char budget |
| `CPF5813 File already exists` | `CRTPF REPLACE(*NO)` on a rebuild | adopt the stamp |
| Build says OK, no `*MENU` object | `.file` is an order-only prerequisite | make both normal |
| Build says OK, nothing created | run outside repo root, or stale stamps | verify objects by query |
| Seed reports success, no rows | `aitool sql` ran only the last statement | one statement per call |
| `ORDER BY` has no effect | DECIMAL division truncated to 0 | `DOUBLE()` casts |
| Inserts vanish at program exit | `COMMIT(*CHG)` default | `SET OPTION COMMIT = *NONE` |
| Bogus `SQL0314` on an unrelated field | `like()` on a DS host variable | `likeds()` |
| `RNX0100` at a statement number | VARCHAR overflow, or `%subst` past its length | check both; the second is easy to miss |
| Fuzzy match groups things that are genuinely different | short alphanumeric tokens (`A1`, `B3`) dropped as too-short words | treat any token containing a digit as a variant token, not a word |
| Rows attach to the wrong parent after a table rebuild | ids restart; orphaned child rows remain | sweep orphans at the start of any regeneration |
| An entire JSON-payload strip renders as nothing | `%char()` emitted `.0` for a zero and `JSON.parse` threw | `fl_jsonNum` on every number in the payload |
| A literal `&mdash;` on screen | an HTML entity written inside `<%= %>`, which escapes its output | use the character, not the entity |
| `Could not find matching close tag for "<%"` | an EJS close delimiter inside a JS comment in a scriptlet — the scanner is textual and ends the scriptlet there | describe the delimiters in prose instead of writing them |
| Every capacity bar pinned at full width | load expressed as percent-of-a-month, and the real loads were 150–406% | scale to the range the data actually spans (weeks against a 16-week horizon) |
| Almost every order projected late | per-order projection divided each centre's capacity by its queue depth, which double-counts contention — an order only competes at the centre it is in | a forward finite-capacity load: one clock per work centre, orders loaded in promised-date order |
| Menu option missing | session cached the menu from sign-on | sign off and back on |
| Object changes have no effect | stale copy in a preceding library | delete the shadow |

---

## 14. Tool reference

| Tool | Purpose |
|---|---|
| `tools/ejs-preflight.sh [rdf.json…]` | Renders every template against a data context built **from the RDF field list** — a contract test. Verifies `css`/`js` URLs resolve. |
| `tools/deploy-ejs.sh [docroot]` | Pre-flights, copies `htdocs/.../ui/**` to the PUI docroot, verifies each file server-side. |
| `tools/gen-fletcher-shim.py [--check]` | Installs/refreshes the shim in every skin. `--check` exits non-zero on a stale snapshot. |
| `tools/ejs-screenshot.py <rdf> <screen.json> <out.png>` | Renders a screen offline from captured session data and screenshots it in headless Chromium. |
| `tools/seed-fletcher.sh <LIBRARY>` | Idempotent demo seed; library passed in, never hardcoded. |
| `aitool ejs-validate` | Underlying template renderer; returns HTML or a file/line error. |

---

## 15. If you remember five things

1. **Nothing browser-side survives the proxy — it all goes through the shim, and the
   shim must be regenerated and hard-refreshed after every asset change.**
2. **A passing headless session says nothing about what a browser shows.** Render it
   and look at it.
3. **Measure, don't count.** Row heights, chrome heights, line wrapping — and check
   your measurement harness before doubting the code.
4. **`codermake` and `aitool sql` both report success while doing nothing.** Verify
   the object, verify the row count.
5. **Scope every CSS rule to the wrapper class and prefix every class name.** The
   shim injects all CSS globally, so an unprefixed name is a live collision.
