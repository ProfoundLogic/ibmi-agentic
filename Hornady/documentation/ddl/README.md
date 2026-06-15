# DDL drafts for the Hornady shipment work-file family

These are **best-effort skeletons** for the physical files referenced by `HYR0600` and its sibling shipment programs. They are **not** wired into `ibmi-agentic/src/Rules.mk` yet — promoting them is a deliberate next step once they're reviewed.

## How the drafts were derived

For each PF here, the field list is the **union** of three evidence sources from the in-package material:

1. **Logical-file keys.** Every `.LF` member in the package keys on its parent PF. The key fields appear in DDL `A K …` lines — see `data-model.md`. These fields *must* exist on the parent PF with matching types, so each draft starts here.
2. **DSPF `REFFLD()` references.** Every `HYD*.DSPF` member that uses a PF as its `REF()` declares hidden/visible fields like `S1ITEM R REFFLD(GDITEM)`. The referenced field name reveals that the PF must contain it — see e.g. `HYD0600A.DSPF` for `HYPSGCU`, `HYD0602A.DSPF` for `HYPSGDT`, `HYD0606.DSPF` for `HYPSGLD`.
3. **`HYR0600.SQLRPGLE` field references.** Every `GC*` / `GD*` / `GH*` / `LD*` token in `HYR0600` is a hint that the parent PF has that field. (Tokens that obviously aren't field names — `GDQ`, `GDR`, `GDS`, etc. — were filtered out.)

## Type inference

Field types are **inferred from IBM i naming conventions**, not observed. Common conventions used here:

| Suffix / pattern | Inferred type | Rationale |
|---|---|---|
| `…ORD#` | `DECIMAL(8,0)` | Order numbers — 7–8 digit packed |
| `…ORL#` | `DECIMAL(4,0)` | Order line numbers |
| `…TURN` | `DECIMAL(4,0)` | Turnaround / sequence |
| `…BLN#` | `DECIMAL(5,0)` | Bin number |
| `…ITEM` | `CHAR(15)` | Item codes are 15A in BARDATA |
| `…CUST` `…BLTO` `…SHTO` `…DSHP` | `DECIMAL(7,0)` | Customer numbers (matches `CBCUST` in BARCUST) |
| `…GRP#` | `DECIMAL(7,0)` | Shipment group |
| `…LOT#` | `CHAR(10)` | Lot number |
| `…SSCC` `…SSCCBC` | `CHAR(20)` | SSCC barcode (UCC-128 storage) |
| `…STNM` | `CHAR(30)` | Street / store name |
| `…STZP` | `CHAR(9)` | Zip + 4 |
| `…SHPV` `…SVDS` | `CHAR(6)` / `CHAR(30)` | Ship-via code / description |
| `…TSTP*` | `TIMESTAMP` | Timestamp |
| `…Q*` (quantity) | `DECIMAL(11,2)` | Quantities — over-sized for safety |
| `…STS` `…STAT` | `CHAR(1)` | Status flag |
| Other char | `CHAR(10)` default | When no convention suggests otherwise |

These are *educated guesses*. Before relying on these for production data you must reconcile against the real PFs on a Hornady IBM i system — likely by running `DSPFFD` against each and updating the DDL.

## Files in this folder

| File | What it defines |
|---|---|
| `fldref.pf` | DDS field-reference file — the "shared types" that every PF/LF references via `REFFLD()`. Required because every in-package PF declares `REF(FLDREF)`. |
| `hypsgcu.sql` | Shipment group / customer header — referenced by `HYR0600` main subfile |
| `hypsghd.sql` | Shipment group header — referenced by `HYR0600` for the shipment-group existence check |
| `hypsgdt.sql` | Shipment detail — referenced by `HYR0602` / `HYR0606` / `HYR0608` and the SQL `DELETE FROM HYPSGDT` in `HYR0600` |
| `hypsgld.sql` | Shipment lot detail — referenced by `HYR0606` (lot inquiry) |
| `hypsgtd.sql` | Shipment tote detail — referenced by `HYR0608` and as a key carrier in `HYR0600` |
| `hypsgsd.sql` | Shipment sequence detail — small, only `CDTURN` keyed |
| `hypsghd_create.sql` | Combined create script (drop + create all, in dependency order) |

## What still blocks `HYR0600` from compiling

Even with all of these PFs created, `HYR0600` still won't compile until:

1. **More external PFs** — `HYPSGTC`, `HYPSGTS`, `HYPSCWK`, `HYPSSCC`, `HYPPCLS`, `HYPRCLT`, `HYPSTRK`, `HYPSVCT`, `HYPTDTA`, `HYPIDTA`, `HYPMILG`, `HYPINTR` — would need skeletons too.
2. **Order / customer / inventory PFs** — `OEORHD`, `OEOCMT`, `OECMWK`, `OEDTWK`, `OEHDWK`, `HDCUST`, `HDCCMT`, `HDDSHP`, `HDIMST`, `HDIWHS`, `HDSHPV` — required by SQL `JOIN` and `DELETE` statements in `HYR0600`.
3. **The `OEORDP01`, `HDITSL01`, `HDSHPV01` indexes** — the SQL exists for these but the base tables (`OEORDP`, `HDITSL`, `HDSHPV`) aren't in the package.
4. **`/COPY` members** — `QPRPSRC,HYR0610` / `HYR0614` / `HYR9930`. The source for these is in the package as `HYR0610.SQLRPGLE` / `HYR0614.SQLRPGLE` / `HYR9930.RPGLE` but each needs the prototype block extracted into a separate source member named `HYR0610` (etc.) inside a source PF named `QPRPSRC`.
5. **Service programs** — `HHDCSS`, `HHDSVI`, `HOEDIP` — not in the package; stubs needed.
6. **Sibling Hornady programs** — `HYR0189`, `HYR0520`, `HYR0602`, `HYR0608`, `VPRBLDP`, `HYC0138` — source present in the package but each has the same data-dep problem; only the leaf programs (e.g. `HYR0520`) might compile with the data layer this folder defines.

## Promotion plan

When ready to actually compile `HYR0600`:

1. Move these `.sql` files to `ibmi-agentic/src/` (rename to e.g. `hypsgcu.table.sql`, etc., or `.pf` if you'd rather use DDS).
2. Move `fldref.pf` to `ibmi-agentic/src/`.
3. Add `Rules.mk` entries — see the bottom of this README for a starter block.
4. Build the foundation: `codermake fldref.file hypsgcu.file hypsghd.file …` — each will fail if a field-ref name is missing from FLDREF; iterate.
5. Add skeletons for the secondary external PFs (Phase 2).
6. Replace the `hyr0600.rpgle` stub with the real `HYR0600.SQLRPGLE` from `Hornady/src/HornadyDemo/Source/`. Build it and iterate on compile errors.

### Rules.mk starter block (do not paste verbatim — review first)

```makefile
# Hornady real data layer (DRAFT — needs review)
fldref.file: fldref.pf
hypsgcu.file: hypsgcu.table.sql | fldref.file
hypsghd.file: hypsghd.table.sql | fldref.file
hypsgdt.file: hypsgdt.table.sql | fldref.file
hypsgld.file: hypsgld.table.sql | fldref.file
hypsgtd.file: hypsgtd.table.sql | fldref.file
hypsgsd.file: hypsgsd.table.sql | fldref.file

# When the real HYR0600 replaces the stub:
# hyr0600.pgm: hyr0600.sqlrpgle hyd0600a.file hyd0600b.file | \
#     hypsgcu.file hypsghd.file hypsgdt.file hypsgld.file hypsgtd.file
```
