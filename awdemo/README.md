# AdelWiggins demo dataset (AWDEMO)

A BOM / inventory / shop-floor demo dataset built for the **AdelWiggins Group**
customer demo. It is **deployed in library `AWDEMO`**.

The schema is a faithful clone of the existing sample library **BOMSAMPLE**;
only the content is different. Anything written against BOMSAMPLE runs against
this library unchanged — the record format level identifiers are identical, so
there is no level check.

## What is in it

| Table                  | Rows | Content |
|------------------------|-----:|---------|
| `ITEM`                 |  155 | Part master: 68 purchased, 87 manufactured |
| `WAREHOUSE`            |    5 | AdelWiggins plants and distribution points |
| `SUPPLIER`             |   13 | Raw material, hardware, process and packaging suppliers |
| `SUPPLIER_ITEM`        |   91 | Approved source list, one primary source per purchased part |
| `BOM_HEADER`           |   91 | One header per manufactured item, plus 5 rev-B supersessions |
| `BOM_DETAIL`           |  468 | Component lines, three levels deep |
| `INVENTORY`            |  356 | On-hand / on-order / reorder point by location |
| `MANUFACTURING_ORDERS` | 2112 | 12 months of shop orders at a steady release rate |

The parts are AdelWiggins' real product families, taken from adelwiggins.com:

- **Aerospace tube connectors** — AW2010HP / AW2091N / AW2100 / W991 / W991N /
  W991F flexible clamshell couplings, AW971 / W700 / 3600 / 6300 flexible
  threaded couplings, AW979 / 8300 rigid couplings, G620 connectors, W902 /
  63052 swage ferrules, W922N bulkhead fittings
- **Quick disconnects, valves and hoses** — 200 and 6000 Series QDs, Preece
  9100 / 9400 / 9600 QDs, Preece check and fuse valves, frangible couplings,
  hose assemblies
- **Isolators** — dielectric fuel and hydraulic lightning isolators
- **Heater products** — ribbon heaters, blankets, gasket heaters, cartridge
  heaters, cuff and manifold assemblies, heated hoses
- **Adel clamps** — standard / top-locking / side-locking loop clamps, block
  clamps, wear sleeve clamps, specialty clamps, aircraft grommets
- **Wiggins mining products** — diesel fast fuelling nozzles and receivers, DEF
  automatic fill, lubrication quick disconnects

Sizes, materials (2024/7075 aluminium, 321/304/A286 CRES, Ti-6Al-4V), seal
callouts, process specs (MIL-A-8625, AMS2700, AS9102) and pressure/temperature
language all come from the published product data.

### Deliberate demo content

The data is seeded with situations worth clicking on during a demo:

- **Multi-level structures.** Finished goods consume machined sub-assemblies,
  which consume raw bar, sheet, tube and outside processing — a recursive
  explosion returns three levels.
- **Standard cost ties out.** The cost of every manufactured item is rolled up
  from its own structure times a conversion factor, so a cost roll-up in the
  demo reconciles instead of showing arbitrary numbers.
- **Engineering revisions.** `AW2100-24`, `ISOF-1000`, `W991-24`, `HTRBL-112`
  and `WMFFN-100` each carry a rev A and a superseding rev B with a later
  effective date and a different component list.
- **Shortages.** 85 inventory rows sit below their reorder point.
- **Late work.** 59 of the 288 open orders are past their expected completion
  date, including a handful of long-stuck jobs for an aging-WIP report.
- **Exceptions.** `CLPSTD-04` is a manufactured item with no BOM; four items
  and one supplier (`AWS013 Redline Metal Treating`) are flagged inactive but
  still carry history.
- **Second sources.** Several commodities have both a primary and an alternate
  approved supplier at different lead times and prices.

### What is real and what is invented

Product families, part-number series, materials, sizes and specification
callouts are drawn from AdelWiggins' published product pages, and the Los
Angeles plant is their real manufacturing site. Everything transactional is
fictional: supplier companies, contact names, phone numbers, e-mail addresses,
operator IDs, costs, quantities, dates and order notes. The Valencia, Miami and
El Paso locations are invented. No AdelWiggins pricing or supply-chain data was
used — none is public.

## Dates

The dataset is anchored to the **date you generate it**, not a constant, so a
demo always opens on a live shop floor: about 1,800 closed orders behind it,
roughly 170 released and on the floor now, and a 120-order forward backlog.

Regenerate before a demo. It takes about a minute end to end:

```bash
python3 awdemo/tools/gen-awdemo-data.py
python3 awdemo/tools/load-awdemo-data.py AWDEMO
```

Skip that and the data decays: 30 days after generation the late-order count
goes from 59 to over 120, and 40-odd *planned* orders are overdue before anyone
has started them. At +60 days every open order is late and the backlog is
empty.

`sql/data/*.sql` in git is therefore a dated snapshot and *will* show as a diff
when you regenerate — that is expected, not drift. The as-of date is stamped in
each file header. To reproduce an old snapshot exactly:

```bash
python3 awdemo/tools/gen-awdemo-data.py --as-of 2026-09-08
AWDEMO_AS_OF=2026-09-08 python3 awdemo/tools/gen-awdemo-data.py   # same thing
```

The RNG seed is fixed, so for a given anchor the output is byte-for-byte
reproducible. Changing the anchor moves the dates and leaves the item master,
supplier list and product structures untouched.

### How the order dates are built

Orders come off a **steady release rate** — about 7.7 per working day over 260
working days — and each order's status falls out of where it sits in its own
lifecycle: due date passed and finished is `C`, due date passed and still open
is a late `R`, due date ahead is a live `R`. Getting this wrong is easy and
looks bad: an earlier version picked a fixed quota per status, which spread
closed orders over a year while cramming every open order into the last seven
weeks, so the plant appeared to run at 10 orders a month and then release 149
in six. Orders per month now sits in a 154-177 band.

Some smaller rules that keep the dates believable:

- Lead times are counted in **working days**, so no order is raised, due or
  completed on a Saturday or Sunday. Neither are stock movement dates.
- The **planned backlog is generated separately** from the release stream — it
  is forward-looking work from recent MRP runs, not history. Its creation dates
  sit in the last fortnight, which puts a mild bump on the most recent full
  month. Spreading them wider makes the chart worse, not better: the current
  month is only part-elapsed, so everything pushed backwards lands on the
  previous one.
- **Discontinued parts only appear in old closed history.** An obsolete part
  with a live order on the floor reads as a data error rather than as an
  exception worth finding.
- BOM effective dates run 2019 to 2026 and are deliberately *not* anchored to
  the run date. Aerospace parts have decade-long lifecycles, so a coupling BOM
  effective in 2020 is right; making them all recent would look less authentic.


## Layout

```
awdemo/
  Rules.mk                          codermake targets for the 8 tables + 9 indexes
  sql/*.table.sql                   DDL, one file per table
  sql/*.index.sql                   DDL, one file per index
  sql/data/*.sql                    generated INSERT statements, unqualified
  tools/gen-awdemo-data.py          deterministic generator for sql/data
  tools/load-awdemo-data.py         loads sql/data into a chosen library
```

## Building and loading

Build the objects into whatever `IBMI_BUILD_LIBRARY` points at. Name the
targets explicitly — a bare `codermake` rebuilds every other project in the
repo as well:

```bash
codermake item.file warehouse.file supplier.file bomheader.file \
          bomdetail.file inventory.file supplitem.file mfgorders.file \
          ixitemtype.file ixbomhpar.file ixbomdcmp.file ixbomdbom.file \
          ixsuppname.file ixsuppitem.file ixmfoitem.file ixmfodate.file \
          ixmfostat.file
```

Then load the data:

```bash
python3 awdemo/tools/load-awdemo-data.py            # uses $IBMI_BUILD_LIBRARY
python3 awdemo/tools/load-awdemo-data.py AWDEMO     # or name the library
```

The loader empties each table first, so it is safe to re-run; pass `--keep` to
append instead. Table names in `sql/data/*.sql` are unqualified and the loader
rewrites them to `LIBRARY.TABLE` — an unqualified `INSERT` resolves against the
job library list and can land in the wrong library.

## Deploying to a library

`AWDEMO` is already built and loaded. To rebuild it, or to deploy into a
different library:

```bash
IBMI_BUILD_LIBRARY=AWDEMO codermake <the 17 targets above>
python3 awdemo/tools/load-awdemo-data.py AWDEMO
```

Two things to know before pointing this at a new library.

**The library must be journaled.** SQL only journals a new table if the library
already contains a journal called `QSQJRN`. A library made with plain `CRTLIB`
has none, so every `CREATE TABLE` comes back `SQL7905 - table created but was
not journaled` at severity 20, and the tables end up outside commitment
control. That does not stop the data loading, but it breaks any SQLRPGLE
program built later: codermake compiles with `CRTSQLRPGI COMMIT(*CHG)`, and an
unjournaled table under commitment control fails at run time. BOMSAMPLE is a
real SQL collection and is journaled, so match it:

```bash
ssh dev "system -i \"CRTJRNRCV JRNRCV(<LIB>/QSQJRN0001) TEXT('COLLECTION - created by SQL')\""
ssh dev "system -i \"CRTJRN JRN(<LIB>/QSQJRN) JRNRCV(<LIB>/QSQJRN0001) MNGRCV(*SYSTEM) DLTRCV(*YES) RCVSIZOPT(*RMVINTENT) TEXT('COLLECTION - created by SQL')\""
```

Create the journal *before* the tables. `CREATE SCHEMA <LIB>` does all of this
for you if the library does not exist yet. AWDEMO's journal uses
`DLTRCV(*YES)` so detached receivers are cleaned up automatically — BOMSAMPLE
uses `*NO` and has accumulated 20-plus receivers for a tiny dataset.

**codermake build stamps are keyed by target, not by library.** A target
already built into another library is skipped when you point at a new one.
Remove just these 17 stamps first:

```bash
for t in item warehouse supplier bomheader bomdetail inventory supplitem mfgorders \
         ixitemtype ixbomhpar ixbomdcmp ixbomdbom ixsuppname ixsuppitem \
         ixmfoitem ixmfodate ixmfostat; do rm -f build/$t.file; done
```

Do not blanket-delete `build/` — that forces every unrelated project in the
repo to rebuild.

## Regenerating the data

`sql/data/*.sql` is generated; edit the generator, not the SQL.

```bash
python3 awdemo/tools/gen-awdemo-data.py
```

Item descriptions, product structures, supplier assignments and the release
rate are hand-written constants at the top of the file; quantities, costs,
balances, dates and order history are derived. It asserts that every ID fits
its column, that every BOM component exists, and that every manufactured item
has a structure (except the deliberately BOM-less `CLPSTD-04`).

See **Dates** above for the anchor and how often to regenerate.

## Schema notes

- Long SQL names are pinned with `FOR COLUMN` / `FOR SYSTEM NAME` so the short
  system names match BOMSAMPLE exactly (`COMPO00001`, `MANUF00001`,
  `SUPPL00001`, `IX_BO00001`…). Without pinning, DB2 generates them from
  creation order and a program using the short names could break.
- Tables use `CREATE OR REPLACE TABLE`, which preserves existing rows, so
  rebuilding the DDL is idempotent. `CREATE INDEX` is not — rebuilding an index
  target on a library that already has it fails with SQL0601. Drop the index
  first if you need to.
- Column headings are reproduced with `LABEL ON COLUMN` so screens and query
  tools show the same headings as BOMSAMPLE.
