# Source Inventory

114 source members total in `HornadyDemo/Source/`. Counts by type:

| Type | Count | Purpose |
|---|---|---|
| `.RPGLE` | 19 | Fixed-form ILE RPG programs and service modules |
| `.SQLRPGLE` | 32 | ILE RPG with embedded SQL |
| `.CLLE` | 3 | ILE Control Language wrappers |
| `.DSPF` | 12 | Display files (5250 + Profound UI) |
| `.PF` | 4 | Physical files (data definitions) |
| `.LF` | 37 | Logical files (alternate access paths) |
| `.TABLE` | 2 | SQL CREATE TABLE statements |
| `.VIEW` | 2 | SQL CREATE VIEW statements |
| `.INDEX` | 3 | SQL CREATE INDEX statements |
| Total | **114** |  |

## Naming conventions

| Prefix | Meaning |
|---|---|
| `HYR0xxx` | Hornady RPG — main shipping/order flow (interactive + batch) |
| `HYR2xxx` `HYR3xxx` `HYR6xxx` | Hornady RPG — specialized reports / outbound functions |
| `HYR8xxx` | Hornady RPG — payment processing (CurbstoneCard C3) |
| `HYR9xxx` | Hornady RPG — shared subprocedure libraries (service programs) |
| `HYD0xxx` | Hornady display file paired with the `HYR0xxx` of the same number |
| `HYC0xxx` `HYC3xxx` | Hornady CL wrapper |
| `HYP*` | Hornady physical file family — referenced but **not in this package** |
| `HYL*` | Hornady logical file — overlay onto an `HYP*` parent |
| `VPR*` `VARHDINFD` | Varsity WMS integration |
| `VPCNTR` `VPCCRTCNT` `PKGUNT` | Varsity container / package-unit data |
| `PICKBAT*` `PICKERD/R` | Modern picking dashboard (Profound Logic) |
| `BARCUST` `BARDATA` | Barcode master files (item ↔ customer ↔ UCC) |
| `OEORDP01` `HDITSL01` `HDSHPV01` | SQL indexes over Order/Inventory/Ship-via tables |
| `LDLMORD*` | Lot-to-MO cross-reference (over external `LDPMORD`) |
| `EDR9900` | EDI subprocedures |
| `CHKDIGIT` | Modulus-10 check-digit service |

## RPG / SQLRPGLE programs

Verified titles from the header comments of each member (`* TITLE: …`):

| Member | Title (from header) | Type |
|---|---|---|
| `HYR0114.SQLRPGLE` | Order Ship (Scanner) Packing List Print | Batch / printer |
| `HYR0116.SQLRPGLE` | Order Ship (Scanner) Shipping Label Print | Batch / label |
| `HYR0120.SQLRPGLE` | Order Ship (Scanner) Build Work Records | Batch |
| `HYR0138.SQLRPGLE` | Pallet Contents Entry/Maintenance | Interactive (DSPF) |
| `HYR0139A.RPGLE` | Pallet Report subprocedures | Service |
| `HYR0142.SQLRPGLE` | Bill of Lading Print | Batch / printer |
| `HYR0148.RPGLE` | Add record to EDI 856 tickler file | Service |
| `HYR0150.RPGLE` | Write HYPSCWK Records and Copy to HYPSSCC | Service |
| `HYR0152.RPGLE` | Write HYPEDWK Records and Copy to HYPELD | Service |
| `HYR0185.SQLRPGLE` | Pick Ticket Print Selection Functions | Service |
| `HYR0189.SQLRPGLE` | Display customer/order comments | Interactive (DSPF) |
| `HYR0240.RPGLE` | Update Varsity files for inquiry | Service |
| `HYR0500.SQLRPGLE` | UCC-128 SSCC and Tracking Number Association | Interactive (DSPF) |
| `HYR0520.SQLRPGLE` | Display Sales-specified ship-via and comments | Interactive (DSPF) |
| `HYR0540.SQLRPGLE` | Print UCC-128 carton labels for UCC-128 pallet labels | Batch |
| `HYR0600.SQLRPGLE` | Shipment Processing | Interactive (DSPF) — **main entry** |
| `HYR0602.SQLRPGLE` | Shipment Detail Processing | Interactive (DSPF) |
| `HYR0606.RPGLE` | Shipment Lot Inquiry | Interactive (DSPF) |
| `HYR0606TV.RPGLE` | Shipment Lot Inquiry — Tote Verify | Interactive (DSPF) |
| `HYR0608.SQLRPGLE` | Shipment Tote Processing | Interactive (DSPF) |
| `HYR0610.SQLRPGLE` | Consolidated Shipping subprocedures | Service |
| `HYR0614.SQLRPGLE` | Consolidated Shipping subprocedures | **Orchestrator** |
| `HYR0620.RPGLE` | Create ZPL label string for UCC-128 labels | Service |
| `HYR0622.SQLRPGLE` | Print UCC-128 labels | Batch |
| `HYR0626.SQLRPGLE` | Shipment Detail Mixed/Partial Label Print | Interactive (DSPF) |
| `HYR0804C3.SQLRPGLE` | CurbstoneCard C3 Request Interface 2 | Service |
| `HYR0810C3.SQLRPGLE` | CurbstoneCard C3 Data Retrieval | Service |
| `HYR0812.SQLRPGLE` | CurbstoneCard Complex Requests | Service |
| `HYR2022.RPGLE` | Write HDS BOL data for non-truck EDI shipment | Service |
| `HYR3504.RPGLE` | Shipping BOL Powder Weight Summary | Batch / printer |
| `HYR3552.SQLRPGLE` | Pallet Banner Print — Automatic | Batch |
| `HYR6080.SQLRPGLE` | Order Status Email | Batch |
| `HYR9906.RPGLE` | Subprocedures — Orders | Service program |
| `HYR9916A.SQLRPGLE` | Subprocedures — Hazardous Items (2) | Service program |
| `HYR9930.RPGLE` | Subprocedures — Text Manipulation | Service program |
| `HYR9933.RPGLE` | Subprocedures — Phone number | Service program |
| `HYR9934.RPGLE` | Subprocedures — Trademark and Copyright | Service program |
| `HYR9937.RPGLE` | Subprocedures — Check Digit | Service program |
| `HYR9960.RPGLE` | Subprocedures — Item Barcode | Service program (over `BARDATA`) |
| `HYR9962.RPGLE` | Subprocedures — Customer Item Barcode | Service program (over `BARCUST`) |
| `CHKDIGIT.RPGLE` | Check Digit Retrieval and Validation | Service program |
| `EDR9900.SQLRPGLE` | EDI Subprocedures | Service program |
| `VARHDINFD.SQLRPGLE` | (no `TITLE:` header) — Varsity hardware/master interface | Service |
| `VPRBLDP.SQLRPGLE` | (no `TITLE:` header) — Varsity build / pick load | Service |
| `VPRDMWTCT.SQLRPGLE` | (no `TITLE:` header) — Varsity demand/weight control | Service |
| `VPRSWOGINF.RPGLE` | (no `TITLE:` header) — Varsity shipping/order/goods info | Service |
| `PICKBATR.SQLRPGLE` | Pick Batch Dashboard | Interactive (Profound UI) |
| `PICKBATDR.SQLRPGLE` | Pick Batch Dashboard — Detail | Interactive (Profound UI) |
| `PICKBATLR2.SQLRPGLE` | Load Pick List BATCH | Batch |
| `PICKBATSV.SQLRPGLE` | (no `TITLE:` header) — Pick Batch Service / Save | Service |
| `PICKERR.SQLRPGLE` | Load Pick List BATCH (picker-side) | Interactive (Profound UI) |

## CL programs

| Member | Title | What it does |
|---|---|---|
| `HYC0138.CLLE` | Pallet Contents Entry/Maintenance | Wraps `HYR0138`; sets activation group `HDSCTL` and reclaims `HDSOPT` afterwards. Single CALL. |
| `HYC3512.CLLE` | Shipping Pallet Content Report | Calls `HYR3512` (not in this package) for the pallet report. |
| `VPCCRTCNT.CLLE` | "Call The ShipSelect Interface after overriding the …" | Varsity container creation setup. |

## Display files

12 members, see `display-files.md` for record formats.

- **Classic 5250** (DDS-only, with `SFL`/`SFLCTL` subfiles and `WINDOW*` records): `HYD0189`, `HYD0520`, `HYD0600A`, `HYD0600B`, `HYD0602A`, `HYD0602B`, `HYD0604A`, `HYD0606`, `HYD0608A`, `HYD0608B`.
- **Profound UI** (DDS shell with embedded JSON in `HTML(...)` keywords, mobile/scan-gun layouts): `PICKBATD.DSPF`, `PICKERD.DSPF`.

## Database objects

See `data-model.md` for full field listings and the ERD. Quick listing:

**Physical files (in this package):**
- `BARCUST.PF` — Hornady-item → Customer-item / UCC mapping
- `BARDATA.PF` — Item master with UPC/UCC and ammunition ballistics data
- `PICKBATHP.PF` — Pick batch header (Profound UI dashboard backing store)
- `PICKBATDP.PF` — Pick batch detail lines

**Logical files (in this package):** 37 LFs. 5 over `BARCUST`, 4 over `BARDATA`, 4 over `PICKBAT*` (2 over `PICKBATDP`, 2 over `PICKBATHP`), plus 22 LFs over external `HYP*` files and 2 over external `LDPMORD` (key-only — useful as access-path documentation but the parent PFs aren't here).

**SQL tables:** `PKGUNT.TABLE` (5 cols, package-unit dimensions), `VPCNTR.TABLE` (59 cols, container master).

**SQL views:** `PKGUNTL1.VIEW` over `PKGUNT`, `VPCNTRL3.VIEW` over `VPCNTR`.

**SQL indexes** (all over files NOT in this package):
- `OEORDP01` on `OEORDP(IDTURN, IDORD#, IDORL#, IDBLN#)` — order detail by turnaround/order/line
- `HDITSL01` on `HDITSL(ISITEM, ISWHS, ISRATE, ISSTKR, ISAILE, ISSLOC)` — inventory by item/warehouse/location
- `HDSHPV01` on `HDSHPV(SVSVDS, SVSVSV)` — ship-via cross-reference

## Notes on encoding

The zipped members arrived as raw EBCDIC (code page 037), fixed-length 110-byte records, no record-separator bytes. Decoding required:

1. `iconv -f IBM037 -t UTF-8` to convert characters.
2. Split the resulting byte stream into 110-char records and `rstrip` trailing spaces.

The working copies under `Source/` have been replaced with the readable UTF-8 versions. If you need the original EBCDIC binaries they are preserved at `/tmp/ebcdic_backup/` for this task session.
