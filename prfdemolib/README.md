# PRFDEMOLIB: JDA/MMS retail inquiry demo

Three green-screen programs from a JDA/MMS retail system (Variety Wholesalers),
prepared for a green-screen-to-GUI demo. The source was imported from IBM i
library `PRFDEMOLIB`. The vendor's compile and run guide is in
[`docs/Profound-Demo_README_v2.md`](docs/Profound-Demo_README_v2.md).

**End users:** see the step-by-step [New Hire Guide](docs/USER-GUIDE.md)
(how to run and read each screen, troubleshooting) and the
[Sample Data Reference](docs/SAMPLE-DATA.md) (every value you can key in; the
screens have no working F4 prompts).

| Menu option | Entry point | What it does | Screen |
|---|---|---|---|
| 4. Inventory by Store (INV046) | `INV046CL` (CLP) -> `INV046` (RPG III) | Inventory balances for a SKU by store, district, region or store group | 24x80 |
| 5. Item Inquiry (INV502) | `INV502` (RPGLE) | SKU details, then warehouse balances | **27x132** |
| 6. Ticket Item Inventory (TS0110) | `TS0110` (RPGLE) | Ticket item inventory for one store and department, with F10 to create a store distribution | **27x132** |

**Three tiers of the same three programs.** The menu shows each program
three ways, so a demo can walk from the green screen, to a straight web
version of it, to the full browser experience:

| Tier | Options | Programs | Display files | Assets |
|---|---|---|---|---|
| Green screen | 4, 5, 6 | `INV046CL`->`INV046`, `INV502`, `TS0110` | DDS (5250) | - |
| Web version of the green (no improvements) | 7, 8, 9 | `INV046CLE`->`INV046E`, `INV502E`, `TS0110E` | `INV046ED`, `INV502ED`, `TS0110ED` (EJS) | `vpshared/`, `vpinv046/`, `vpinv502/`, `vpts0110/` |
| Variety Wholesalers demo ("pop") | 10, 11, 12 | `INV046CLW`->`INV046W`, `INV502W`, `TS0110W` | `INV046WD`, `INV502WD`, `TS0110WD` (EJS) | `vwshared/`, `vwinv046/`, `vwinv502/`, `vwts0110/` |

The web tiers are the same RPG (a handful of tagged `VW01` lines on top of the
originals); the `*E` members are **generated** from the `*W` ones by
`tools/gen-plain-clones.py` and differ only in the display-file name. The
plain tier's templates are a field-for-field web rendering of the 5250
screens: same labels, same order, same F-key legend, typed codes instead of
pickers, no sorting, filtering, export, KPIs or shortcuts. See
[`web/README.md`](web/README.md) for how both are built and tested.

The menu options are on the cfdemo `MENU` (`cfdemo/qddssrc/menu.dspf`,
`cfdemo/menu.msgf`, USR0004-USR0012).

## Build, load, run

```bash
# 1. Build everything in this directory (into $IBMI_BUILD_LIBRARY)
codermake $(grep -oE '^[a-z0-9]+\.(file|pgm|module|msgf|bnddir)' prfdemolib/Rules.mk | sed 's|^|prfdemolib/|')

# 2. Copy the sample data in from PRFDEMOLIB, create the runtime objects and
#    add the seeded demo items (--no-seed for the shipped sample only)
prfdemolib/tools/load-prfdemo-data.sh "$IBMI_BUILD_LIBRARY"

# 2a. Optional: refresh the keying reference from the loaded data
python3 prfdemolib/tools/gen-sample-data-doc.py "$IBMI_BUILD_LIBRARY" > prfdemolib/docs/SAMPLE-DATA.md

# 3. Menu (only if the cfdemo MENU is not already in the library)
codermake cfdemo/menu.file cfdemo/menu.msgf cfdemo/menu.menu
```

codermake's `build/` stamps are not keyed by library. When building into a
*different* library from a container that already built this one, delete only
the prfdemolib stamps (`build/<name>.<ext>` for the targets in
`prfdemolib/Rules.mk`), never `build/*`, which would also recreate cfdemo's
data files empty.

codermake creates the data files **empty**, and rebuilding a data file empties
it again. If a `.pf` is rebuilt, run step 2 again. The loader can be rerun
safely: it replaces the data and keeps an existing `MBRSEQNO`. It never writes
to PRFDEMOLIB.

Sample data to try (from the vendor guide):

| Screen | Try |
|---|---|
| INV046 | SKU 262657, Search For `G`, store group 740 (10 stores); or `S` with store 205 |
| INV502 | SKU 262657 or 166402 (pack and hold), 100001 (allocated qty), 248117 (Bundle Component). Page Up/Down steps through the SKUs. |
| TS0110 | Store 205 (warehouse 9002) or 406 (warehouse 9001), dept 12, "Ticketed only" and "Available only" set to N |

## Layout

| Path | Contents |
|---|---|
| `qrpgsrc/`, `qrpglesrc/`, `qclsrc/`, `qddssrc/*.dspf`, `mmcpy/` | Program, screen and copybook source as imported |
| `qddssrc/*.pf`, `qddssrc/*.lf` (except `invvenj2.lf`) | **Reconstructed** DDS for the 41 physical and 9 logical files PRFDEMOLIB shipped as objects only |
| `qddssrc/invvenj2.lf` | Original join LF source (imported) |
| `jdamsgf.msgf`, `rtcbnddir.bnddir` | Recreated from the PRFDEMOLIB `JDAMSGF` *MSGF and `RTCBNDDIR` *BNDDIR objects |
| `tools/fetch-metadata.sh`, `tools/reconstruct-dds.py` | Regenerate the reconstructed DDS from the shipped objects |
| `tools/load-prfdemo-data.sh` | Data and runtime-object loader |
| `tools/gen-seed-data.py` | Generates the extra demo data (see below) |
| `tools/gen-sample-data-doc.py` | Writes `docs/SAMPLE-DATA.md` from a loaded library |

### Reconstructed data files

The DDS was generated from `DSPFFD`/`DSPFD` metadata (fields, text, column
headings, edit codes and edit words, keys, UNIQUE, DESCEND, select/omit). After a
build, **all 54 record formats** (including INVVENJ2) have format level
identifiers identical to the PRFDEMOLIB objects, and the keys match too. So the
programs run against either copy without level checks, and `CPYF
FMTOPT(*NONE)` loads the data. Fields are written out in full, not as
`REF(JDAREF)`, so no field reference file is needed at build time. Edit codes
are kept because the display files take them through `REFFLD`.

Regenerate (identical output, verified):

```bash
prfdemolib/tools/fetch-metadata.sh "$IBMI_BUILD_LIBRARY" /tmp/prfmeta
python3 prfdemolib/tools/reconstruct-dds.py /tmp/prfmeta prfdemolib/qddssrc
```

### Seeded demo data

The shipped sample has 145 of its 162 SKUs in department 12 (Ladies Intimate
Basics), one SKU in each of 17 other departments, and **no inventory groups**,
so INV046 *Search For = I* could never succeed. `tools/gen-seed-data.py` adds,
on top of the CPYF'd sample:

- dept **86 SEASONAL CANDY**: 12 Christmas/Harvest SKUs (880101-880112)
- dept **59 BEDDING**: 12 sheet/comforter/throw SKUs (880201-880212)
- 5 vendors, 10 sub-department/class rows, pack-and-hold for 6 holiday SKUs
  (out date 12/01/26)
- inventory groups 10/20/30/40 (stores grouped by state)

These are modelled on the Roses departments listed on rosesdiscountstores.com
and vwstores.com (Seasonal, Home) and the Fall/Holiday 2026 season. Each
seeded row is a clone of a real sample row (balances and history from SKU
849528, item master from 503589/104027) with identity, hierarchy, vendor, pack,
cost and price replaced and quantities scaled, so the programs process them
exactly like shipped SKUs. All seeded SKUs carry style number `DEMOSEED`. The
SQL is generated on the fly by the loader and run with QSH `db2`; it deletes
what it adds first, so it can be rerun safely.

## Differences from the shipped library

| What | Shipped | Here | Why |
|---|---|---|---|
| Calls to programs not in the restore (INV046, INV502) | `CALL` | commented out, key redisplays (`PRF2`) | Avoid *Error calling program* inquiry messages; see Known limitations |
| TS0110 F10 `SBMJOB` | `JOBD(IMSJOB1) INLLIBL(*JOBD)` | `CURLIB(<library TS0110 runs from>)`, default job description (QGPL/QBATCH for AIDEMO) | The demo profile cannot create or duplicate a job description (CPD0032 on CRTJOBD, CPF2189 on PRFDEMOLIB/IMSJOB1), and IMSJOB1's fixed list `QTEMP PRFDEMOLIB QGPL` would never find the CLTS0111 and STR001WK built here. Tagged `PRF1` in the source. |
| `DFTACTGRP(*NO) ACTGRP(*CALLER)` for TS0110, GETSPK, TRF840A, NXTSEQ, DSPSIZ | CRTBNDRPG parameters in the guide | `H` spec in each source | codermake has no per-program compile parameters |
| ORDERBK | 8 members; INV502 reads OB01 | 1 member, loaded from OB01 | The shipped ORDERBK member holds the same 3 rows as OB01; ORDERBK1 over the single member returns the same data |
| STR001WK | 12 empty `T01177xx` members | default member only, `MAXMBRS(*NOMAX)` set by the loader | F10 adds `T9000001`, `T9000002`, ... |
| MBRSEQNO *DTAARA | object | created by the loader (`*DEC 7 0`, 9000000) | codermake has no data area recipe |
| SYSGET, CLTS0111 stubs | *PGM objects listed in the guide, but **not present** in PRFDEMOLIB | built from `qclsrc/` | |

Not built: `qclsrc/prfdup01-03` and `prfload01-03`. The vendor says they are
internal build utilities that reference production libraries.

## Known limitations (by design of the demo)

- Function keys and options that call programs not included in the demo are
  **switched off**: the CALL and its PARMs are commented out (tag `PRF2`, as
  `C*prf` in INV046 and `PRF2 c***` in INV502), so the key just redisplays the
  screen instead of raising *Error calling program*. That covers 12 calls in
  INV046 (F3 INV002, F4 TBL048/049/009/018/064, F6 INV860/861, F7 and exit
  INV417, F8 INV416, option I INV042) and 22 in INV502 (F3 on the prompt INV002,
  F2-F23 INQDPTBYR ... INQDSQ). One PARM that defined a field (`INREGX 3,0`) is
  replaced by an equivalent `Z-ADD 0`. To restore a call when the program becomes
  available, uncomment its lines. TS0110 calls only programs that are here.
  It still creates no real transfers: the CLTS0111 stub only logs the member
  and its record count.
- INV502FM is 27x132 only (the vendor guide says 24x80). INV502 has no
  fallback for a 24x80 device. TS0110 shows the DSPSIZ notice on 24x80.
- INV046 with *Search For* `G` (store group) or `I` (inventory group) applies
  the *Store Type* filter to the type of the **previous** store record (`STRHDO`
  is tested before the `CHAIN TBLSTR`), so `S`/`W` can return an empty or wrong
  list, typically on the first inquiry. Use Store Type `A`. This is the
  original code, deliberately left unfixed.
- After an invalid SKU, INV502 shows INV0297 but leaves the previous SKU's
  description on the line. This matches the original program.
