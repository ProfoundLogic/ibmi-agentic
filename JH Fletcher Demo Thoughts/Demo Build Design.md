# Fletcher Demo — Build Design

**Status: created in Jira.** Epic **GJA-907**, tickets **GJA-908 … GJA-915**.
Target library changed to the persistent **`FLTDEMO`** (was a task-scoped `AITSKxxxxx`).

Scope: menu navigation + **A1 Parts Finder** + **A2 Fleet 360**, built on mocked Fletcher data in the
existing `cfdemo` application, following its established patterns.

---

## 1. What's already there (the baseline we extend)

The `cfdemo` app currently has three menu options and one complete EJS reference implementation:

```
MENU                                    (menu.dspf + menu.msgf → menu.menu)
  1. Work with Customers                → WRKCUSTR    5250
  2. Work with Customers (RPGOA)        → WRKCUSTRO   RDF
  3. Work with Customers (EJS)          → WRKCUSTEO   EJS   ← the pattern to copy
 90. Sign off
```

**The EJS stack, end to end** (from `wrkcusteo`):

| Layer | File | Notes |
|---|---|---|
| Data | `qddssrc/custp.pf` | DDS physical file |
| Access | `qrpglesrc/custr.sqlrpgle` + `custr_pr.rpgle` | module → `custr.srvpgm` via `qsrvsrc/custr.bnd` |
| Binding | `cust.bnddir` | |
| Screen def | `qddssrc/wrkcusteo.json` | RDF JSON: formats → fields + subfiles, points at template/css/js URLs |
| Program | `qrpglesrc/wrkcusteo.rpgle` | `dcl-f ... handler('PROFOUNDUI(HANDLER)')` |
| Web assets | `htdocs/profoundui/userdata/ui/wrkcuste/*.ejs/.css/.js` | **not deployed by codermake** |
| Menu | `menu.msgf` (`usr0003 → call wrkcusteo`) + `menu.dspf` text | |
| Build | `Rules.mk` | one line per object |

**Everything below follows this pattern exactly.** No new architecture.

---

## 2. Menu navigation

```
MENU  (existing)                         FLTMENU  (new)
  1. Work with Customers                   1. Parts Finder (EJS)          → FLPARTEO
  2. Work with Customers (RPGOA)           2. Fleet 360 (EJS)             → FLFLEETEO
  3. Work with Customers (EJS)            90. Return                      → RETURN
  4. J.H. Fletcher Demo  ──── GO FLTMENU ──┘
 90. Sign off
```

New objects: `qddssrc/fltmenu.dspf`, `fltmenu.msgf`, `fltmenu.menu`.
Edits: one line in `menu.dspf`, one `usr0004` message in `menu.msgf`, `Rules.mk` entries.

> ### ⚠️ Known Rules.mk trap — do not copy the existing line
> The committed rule is `menu.menu: menu.msgf | menu.file` — with `menu.file` as an **order-only**
> prerequisite (after the `|`). The `.menu` pattern rule only matches when **both** `.msgf` and
> `.file` are *normal* prerequisites; as order-only, the recipe is dropped **silently** and the menu
> never builds.
>
> So write it as:
> ```make
> fltmenu.menu: fltmenu.msgf fltmenu.file
> ```
> Also note `CRTMNU` leaves the DSPF/MSGF references as `*LIBL`, and a signed-on session keeps the
> menu it resolved at sign-on — **sign off and back on** before concluding a menu change didn't work.

---

## 3. Mock data model

Ten DDS physical files, `FL` prefix, in `cfdemo/qddssrc/`. Dates are `8S0` YYYYMMDD to match the
existing `custp.pf` convention (`CLASTORD`, `CCREATED`).

### Shared foundation — both screens need these

**`flcustp.pf` — FLCUSTP** · mining companies
`FLCUST 6P0` key · `FLCNAME 40A` · `FLCCITY 30A` · `FLCSTATE 2A` · `FLCCNTRY 3A` · `FLCREGN 10A` ·
`FLCMGR 30A` acct manager · `FLCSTAT 1A` A/I · `FLCSINCE 8S0`

**`flsitep.pf` — FLSITEP** · mine sites (a customer has many)
`FLSITE 6P0` key · `FLSCUST 6P0` · `FLSNAME 40A` · `FLSSTATE 2A` · `FLSTYPE 1A` C/M/I (coal /
metal-non-metal / industrial minerals) · `FLSSEAM 5P1` seam height in · `FLSSTAT 1A`

**`flmodlp.pf` — FLMODLP** · machine models
`FLMODL 10A` key · `FLMDESC 40A` · `FLMLINE 3A` product line · `FLMINTRO 8S0` · `FLMSTAT 1A` C=current
L=legacy
Product lines: `RBC` roof bolter coal · `RBM` roof bolter metal/non-metal · `DJ` drill jumbo ·
`SCL` scaler · `MRS` mobile roof support · `PL` powder loader · `PMT` prime mover tractor

**`flmachp.pf` — FLMACHP** · ⭐ **the installed base — the centre of both demos**
`FLSER 8A` key (serial) · `FLMMODL 10A` · `FLMCUST 6P0` · `FLMSITE 6P0` · `FLMBUILD 8S0` ·
`FLMSHIP 8S0` · `FLMHOURS 9P0` · `FLMSTAT 1A` A/P/S · `FLMLSVC 8S0` last service ·
`FLMLMOD 8S0` last modernization · `FLMMODT 1A` N/P/F

**`floptnp.pf` — FLOPTNP** · options fitted per machine — **this is the ETO variance made visible**
`FLOSER 8A` · `FLOCODE 10A` · `FLODESC 40A` · `FLOFITD 8S0`

### A1 Parts Finder

**`flpartp.pf` — FLPARTP** · ⭐ **carries the planted duplicates**
`FLPART 15A` key · `FLPDESC 40A` · `FLPDESC2 40A` · `FLPLINE 3A` · `FLPUOM 3A` · `FLPPRICE 11P2` ·
`FLPSTOCK 7P0` · `FLPLEAD 3P0` lead days · `FLPSUPER 15A` supersede-to · `FLPSTAT 1A` A/S/O ·
`FLPCREAT 8S0`

**`flbomp.pf` — FLBOMP** · fitment: which parts fit which machine
`FLBMODL 10A` · `FLBASSY 20A` assembly · `FLBPART 15A` · `FLBQTY 5P0` · `FLBPOS 4P0` ·
`FLBOPTN 10A` option required (blank = fits all) · `FLBFSER 8A` / `FLBTSER 8A` effective serial range

### A2 Fleet 360

**`flsvcp.pf` — FLSVCP** · service events
`FLVSER 8A` · `FLVDATE 8S0` · `FLVTYPE 1A` A=audit R=repair P=PM W=warranty · `FLVTECH 30A` ·
`FLVDESC 60A` · `FLVDOWN 5P1` downtime hrs · `FLVCOST 11P2`

**`flpordp.pf` — FLPORDP** · parts order history (drives the spend-trend insight)
`FLOORD 8P0` · `FLOSER 8A` · `FLOCUST 6P0` · `FLODATE 8S0` · `FLOPART 15A` · `FLOQTY 5P0` ·
`FLOAMT 11P2`

*(Optional, phase 2: `flquotp` / `flquotdp` if we want quote persistence — see decision D5.)*

---

## 4. Seed data — where the demo is won or lost

The tables are trivial. **The seed data is the actual deliverable.** Three things must be true:

**a) Planted duplicate clusters.** The Parts Finder's whole point is showing Julia's duplicate part
problem. Seed deliberate near-duplicates:

| Part number | Description | Created | Stock |
|---|---|---|---|
| `HYD-PUMP-A1` | HYDRAULIC PUMP ASSY | 1998 | 4 |
| `HYDPUMPA1` | HYD PUMP ASSEMBLY | 2007 | 0 |
| `PUMP-HYD-A1` | PUMP, HYDRAULIC, ASSY | 2014 | 11 |
| `HP-A1-STD` | HYDRAULIC PUMP (STANDARD) | 2021 | 2 |

Four part numbers, one physical pump, four different eras of naming convention — which is exactly the
story Julia told us. Seed 6–8 such clusters across different assemblies so it doesn't look staged.

**b) Realistic Fletcher shape.** Model codes drawn from their public range (`HDDR`, `CHDDR-4`,
`N3114-AD/E`, `J352-LS`), machines 1–30 years old, coal and metal/non-metal, multiple sites per
customer, some machines overdue for modernization.

**c) Fictional customers, clearly marked.** Mining-company names must be **invented** — we should not
put guesses about real Fletcher customers on screen in front of Fletcher. Plus a visible
`DEMO DATA — NOT ACTUAL FLETCHER RECORDS` marker in the UI footer.

> Per `Pain Points and Demo Ideas.md` §7: Ray already spotted invented fleet data once and asked if it
> came off their website. **Volunteering that it's mocked is what buys trust for everything we don't
> caveat.** Building the marker into the screen means nobody has to remember to say it.

**Seeding mechanism:** there is no seed tooling in the repo today. Proposal: `cfdemo/seed/flseed.sql`
run via `RUNSQLSTM`, plus a `tools/seed-fletcher.sh` wrapper — idempotent (`DELETE` then `INSERT`), so
it can be re-run after any rebuild.

> ### ⚠️ Why seeding must be separate from the build
> Data files are order-only prerequisites in `Rules.mk`. On a fresh clone or a cleared stamp,
> `codermake` will happily `CRTPF` over a populated file and **silently empty it**. Seeding has to be
> a deliberate, re-runnable step, never a build side effect.

---

## 5. Code to be written

### Service program — `FLDATA` (shared by both screens)

`qrpglesrc/fldata.sqlrpgle` + `fldata_pr.rpgle` → `fldata.srvpgm` via `qsrvsrc/fldata.bnd`, plus a
`fletcher.bnddir`. Exported procedures:

| Procedure | Used by |
|---|---|
| `fl_getMachine(serial)` → machine DS | A1, A2 |
| `fl_listMachines(custno : filter)` → array | A2 |
| `fl_getCustomer(custno)` → customer DS | A2 |
| `fl_listOptions(serial)` → array | A1 |
| `fl_listPartsForMachine(serial : searchText)` → array *(joins FLBOMP → FLPARTP, honours option + serial-range fitment)* | A1 |
| `fl_findDuplicates(partno)` → cluster array *(normalised-token similarity)* | A1 |
| `fl_listServiceEvents(serial)` → array | A2 |
| `fl_getFleetSummary(custno)` → totals, 2-yr parts spend + trend, modernization candidates | A2 |

> ### ⚠️ Four SQLRPGLE traps to bake into acceptance criteria
> 1. **`SET OPTION COMMIT=*NONE`** — codermake's `CRTSQLRPGI` defaults to `COMMIT(*CHG)`. On
>    non-journaled files, inserts silently roll back at program exit.
> 2. **`likeds()` not `like()`** for data-structure host variables — `like()` on a whole-row DS
>    produces a bogus `SQL0314 "host variable not unique"` blaming an unrelated variable.
> 3. **Fixed-to-varchar trailing blanks** — a fixed `char` filter field assigned into a varchar host
>    variable keeps its blanks, so `LIKE` matches nothing. Trim on assignment.
> 4. **No `like()` off a display-file field** in SQL host variables → `SQL0312`.

### A1 — Parts Finder

- `qddssrc/flparteo.json` — RDF. Format `partctl`: fields `action`, `sserial`, `ssearch`, `smodel`,
  `scustname`, `ssitename`, `sbuilt`, `soptions`, `msg`; subfile `partsfl`: `sopt`, `spart`, `sdesc`,
  `sassy`, `sqty`, `sstock`, `sprice`, `slead`, `sdupflag`. Second format `dupwin` for the duplicate
  cluster panel.
- `qrpglesrc/flparteo.rpgle` — RPGOA driver, same shape as `wrkcusteo.rpgle`.
- `htdocs/profoundui/userdata/ui/flparts/parts.ejs` / `.css` / `.js`

**Screen:** enter serial → machine identity banner (model, customer, site, build year, options fitted)
→ searchable parts list filtered to *that machine's* fitment → duplicate badge inline on any part with
near-matches → click badge for the cluster panel → running quote basket.

### A2 — Fleet 360

- `qddssrc/flfleeteo.json` — RDF. Format `fleetctl`: fields `action`, `scustno`, `scustname`,
  `sregion`, `smachcnt`, `sspend`, `strend`, `smodcand`, `sinsight`, `msg`; subfile `machsfl`: `sopt`,
  `sserial`, `smodel`, `ssite`, `sbuilt`, `shours`, `sstatus`, `slastsvc`, `smodflag`. Second format
  `svcsfl` for the service-history panel.
- `qrpglesrc/flfleeteo.rpgle`
- `htdocs/profoundui/userdata/ui/flfleet/fleet.ejs` / `.css` / `.js`

**Screen:** customer → KPI strip (machines, sites, 2-yr parts spend + trend, modernization candidates)
→ machine list with modernization-due flags → drill to service history → AI insight panel in the style
Gary already demoed, but computed from the seeded data rather than invented.

> **Note on the AI panel:** in the last demo the insight text was model-generated prose over stub data.
> Here it should be **computed from the seeded tables** ("2 machines past full-modernization age; parts
> spend down 22% YoY") so that when someone asks "where did that come from?", the answer is a query and
> not a guess. That distinction is exactly what the suspicion after the last demo was about.

### Shared EJS scaffolding

Brand tokens (Fletcher colours/logo from jhfletcher.com) as a single `fletcher-theme.css`, an asset
deploy step, and the Genie shim regeneration.

> ### ⚠️ EJS/Genie gotchas — these have cost days before
> - **codermake does not deploy `htdocs/`.** Assets need a separate `scp` to the PUI docroot. Needs
>   its own step in the ticket, or the screens render blank and everyone debugs the RPG.
> - **The Genie EJS shim is required here.** Screens render blank until the template is
>   base64-snapshotted into the skin's `start.html`; the hook is `pui.QD` (not `pui.vu`). Beware the
>   stale-snapshot trap — re-run the generator after *every* template edit.
> - **Field names arrive lowercase.** PUI lowercases DDS field names for the template.
> - **An undefined field reference aborts the entire render.** Every `<%= x %>` must exist in that
>   format's JSON field list. Audit before deploying — a typo yields a white screen, not an error.
> - **Screen JS must use inline handler attributes.** Files in the `js` array run *before* the
>   template renders, so `addEventListener` binds to nothing and fails silently.
> - **Screens must render without their JS** — don't gate visibility on a JS-added class.
> - **Cache-bust template/css/js URLs** in the JSON on every asset edit.
> - **Scope all CSS to a wrapper class.** No `html`/`body` rules, no `position:fixed`, no `100vh`.
>   Also reset Genie's `div { z-index:10; white-space:nowrap; padding:1px }` or dropdowns get trapped
>   and text refuses to wrap.

---

## 6. Proposed epic and tickets

**Epic — `J.H. Fletcher onsite demo: Parts Finder & Fleet 360`**

| # | Ticket | Depends on | Size |
|---|---|---|---|
| F0 | Fletcher demo data model — 10 DDS physical files + `Rules.mk` | — | S |
| F1 | Fletcher demo seed data + re-runnable seed script (incl. planted duplicate clusters) | F0 | M |
| F2 | Menu navigation — `MENU` option 4 → new `FLTMENU` | — | S |
| F3 | `FLDATA` service program — shared data access | F0, F1 | M |
| F4 | **A1 — Parts Finder** (RDF + RPGOA + EJS assets) | F3, F6 | L |
| F5 | **A2 — Fleet 360** (RDF + RPGOA + EJS assets) | F3, F6 | L |
| F6 | Shared EJS scaffolding — Fletcher theme, asset deploy, Genie shim | — | M |

F0/F2/F6 can start immediately and in parallel. F4 and F5 are independent once F3 and F6 land.

**Why seven and not two:** F0/F1/F3/F6 are all shared by A1 and A2. Folded into the two feature
tickets they'd be duplicated, and whichever got built second would inherit a half-built foundation.
Splitting them also means the boring, low-risk work can be assigned to agents in parallel on day one.

Happy to collapse to three (`data+seed`, `menu`, `A1`, `A2`) if you'd rather keep the board thin.

---

## 7. Decisions — all resolved

| | Decision | Outcome |
|---|---|---|
| **D1** | Reuse `CUSTP` or new `FLCUSTP`? | **Everything new.** Nothing existing is modified except two additive menu lines. |
| **D2** | DDS `.pf` or SQL DDL? | **DDS `.pf`** — matches `custp.pf`, codermake-native. SQL only for seeding. |
| **D3** | Target library? | **`FLTDEMO`** — a persistent library that survives between tasks, so the demo is built and seeded once instead of per task. No library name is hardcoded anywhere in source regardless. |
| **D4** | Seven tickets or four? | **Seven.** |
| **D5** | Quote basket persistence? | **In-memory only.** No persistence, no journaling, no `COMMIT` exposure. |
| **D6** | Fictional customer names? | **Yes**, plus a visible DEMO DATA marker built into both screens. |
| **D7** | Fletcher branding? | **Yes** — palette and logo from jhfletcher.com. |
| **D8** | Fitment depth? | **Model + option code.** Serial-range columns exist but are lightly used. |

---

## 7b. Created tickets

| Ticket | Key | Blocked by |
|---|---|---|
| Epic — J.H. Fletcher onsite demo: Parts Finder & Fleet 360 | **GJA-907** | — |
| F00 — Create `FLTDEMO` persistent library, point the build at it | GJA-915 | — |
| F0 — Fletcher demo data model: 10 DDS physical files | GJA-908 | GJA-915 |
| F1 — Fletcher demo seed data + re-runnable seed script | GJA-909 | GJA-908 |
| F2 — Menu navigation: MENU option 4 to new FLTMENU | GJA-910 | GJA-915 |
| F3 — FLDATA service program: shared data access | GJA-911 | GJA-908, GJA-909 |
| F4 — A1: Parts Finder (EJS) — FLPARTEO | GJA-912 | GJA-911, GJA-914 |
| F5 — A2: Fleet 360 (EJS) — FLFLEETEO | GJA-913 | GJA-911, GJA-914 |
| F6 — Shared EJS scaffolding: theme, deploy, Genie shim | GJA-914 | — |

**Unblocked and startable now: GJA-915 (F00) and GJA-914 (F6).**
`FLTDEMO` has to exist and the build has to genuinely target it before any object work begins.

### ⚠️ The trap F00 exists to prevent

codermake's single-library stamps under `build/` are **not keyed by library**. Stamps currently exist
for `AITSK00013.lib`, `AITSK00028.lib` and ~33 objects. Point the build at `FLTDEMO` with those in
place and codermake concludes every object already exists, builds **nothing**, and reports success.
Clear the stamps or switch to `CODERMAKE_LIBRARY_MAP` first.

Related: a task library normally precedes `FLTDEMO` on the library list, so a stale same-named object
— `MENU` especially — wins every resolution no matter how often `FLTDEMO` is rebuilt.

### Scope consequence

Because the demo is entered via option 4 on `MENU`, and `MENU` is a `cfdemo` object, the **whole
`cfdemo` application** should build into `FLTDEMO` — not just the `FL` objects. Otherwise the menu
change has to be reapplied every task, which is the thing the persistent library was meant to fix.

---

## 7c. Findings from the existing EJS reference

Reviewed the original EJS demo committed at `9a0a750c` (`htdocs/profoundui/userdata/ui/wrkcuste/`,
~610 lines). Written into GJA-914 in full, with the screen-specific parts on GJA-912/913 and the
RPG-side contract on GJA-911.

**Reuse:** the wrapper-div convention · `pui.submit({action:'X'})` from inline `onclick` · hidden
`action` input · subfile naming `sfl.field.<%= row._rrn %>` · lowercase `row.` properties ·
`data-fkey` buttons · dates pre-formatted in RPG as `char(10)` · one program per screen (list calls
detail via `extpgm`) · and above all the guard idiom
`<% if (typeof msg !== 'undefined' && msg && msg.trim()) { %>`, which is the antidote to the
render-abort trap.

**Do not reuse:**

- **`custlist.js` sorting never binds** — it uses `DOMContentLoaded`, but `js` files load *before*
  the template renders, so nothing is found to attach to. Already logged as GJA-891. Copying this
  file inherits dead sorting.
- **The CSS is not wrapper-scoped.** `.header`, `.toolbar`, `.btn`, `.message`, `.footer` are global
  in *both* stylesheets with different rules. The Genie shim injects all snapshotted CSS globally, so
  they already collide — adding two more screens with the same names makes it worse. Hence the strict
  `.flparts-` / `.flfleet-` prefix convention.
- No Genie `div` reset, no media queries at all, code→label translation in the template (source of
  GJA-893), and zero-suppression hacked in JS.

**Tooling find — `aitool ejs-validate`.** Renders a template against a data context offline and fails
with file and line. Verified during this review: full field set → exit 0; one field removed → exit 4
pointing at `detail.ejs:17`. Now a mandatory pre-flight gate, with the data context generated *from
the RDF JSON field list* so it doubles as a contract test between template and RDF.

**Architectural correction.** The earlier F4/F5 outlines called for a second format in the same RDF
for the duplicate-cluster and service-history panels. Two subfiles in one EJS format compile to
malformed DDS, and the house pattern is one program per screen — so those become separate
programs/RDFs (preferred) or a JSON-string field. Corrected in both tickets; this would otherwise
have cost a build cycle.

---

## 8. What I'd build first

If you want something demoable fastest: **F0 → F1 → F2 → F3 → F5 (Fleet 360)**.

Fleet 360 is the better first build — it's the simpler screen, it needs no fitment logic, and it's the
one that carries the revenue story for Chuck and Rod. Parts Finder is the stronger demo but depends on
the duplicate-detection logic and the BOM join, so it benefits from the data model having settled.
