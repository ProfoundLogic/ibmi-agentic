# Hornady Demo — Source Documentation

This folder documents the IBM i source delivered in `HornadyDemo.zip` and extracted to `Hornady/src/HornadyDemo/Source/`. The source originates from **Hornady Manufacturing** (per code references and `RHORNADY` user IDs in header comments) and was authored primarily by **Midrange Computer Associates, Inc. (MCA)** with later additions by **Profound Logic Software**.

## Quick orientation

- **114 source members** — 1 directory (`Source/`) of mixed RPGLE, SQLRPGLE, CLLE, DSPF, PF, LF, TABLE, VIEW, INDEX. The sibling `Save Files/` directory in the zip is empty.
- **Domain** — order shipping, packing, UCC-128 labelling, pallet/BOL management, EDI 850/856, credit-card settlement (CurbstoneCard), and warehouse picking. Comment blocks call it the **"Shipping" / "Manufacturing, Shipping"** application.
- **Two generations of UI** — classic 5250 green-screens (`HYD*.DSPF` with subfiles and windows) and modern Profound UI display files (`PICKBATD.DSPF`, `PICKERD.DSPF`) with embedded JSON/HTML metadata.
- **Build targets** — the SQL DDL members reference target libraries `XXPVAR350F` (Varsity / package-unit data) and `HDSSTDPGM` (Hornady Distribution System "standard" library). Indexes are over external files (`OEORDP`, `HDITSL`, `HDSHPV`) that are **not in this package** — only their CREATE INDEX statements are.

## Files in this folder

| File | Purpose |
|---|---|
| `README.md` | This overview |
| `source-inventory.md` | Full file-by-file inventory grouped by type and prefix |
| `data-model.md` | Database objects — PF/LF/TABLE/VIEW/INDEX — with entity-relationship diagram |
| `program-architecture.md` | RPG/CL program catalog with call-chain and dependency diagrams |
| `display-files.md` | DSPF catalog with record formats and UI generation |

## About the source encoding

The members in the zip arrived as **raw EBCDIC** (code page 037) with no line separators, structured as fixed-length 110-byte records. They were converted in place to UTF-8 with newlines for analysis — see `Notes` in `source-inventory.md`. The originals are preserved at `/tmp/ebcdic_backup/` for this task only; the working tree under `Hornady/src/HornadyDemo/Source/` now holds readable text.

## What is *not* in this package

A lot. The source extensively references external files that aren't included:

- **Master/transactional files** — `OEORHD` (order header), `OEORDT` (order detail), `OEORHP` (in-process order header), `OEORDP` (in-process detail), `OEPKLH/OEPKLD` (packing list), `OEBOLH/OEBOLD` (BOL), `HDCUST`, `HDIMST`, `HDIWHS`, `HDFTCL`, `HDDSHP`, `HDSHPV`, `HDKIT`, `HDSHQT`, `HDCCMT`, `OECTRL`, `OEOCMT`, etc.
- **Shipment work files** — `HYPSGCU`, `HYPSGDT`, `HYPSGLD`, `HYPSGTD`, `HYPSGSD`, `HYPSGHD`, `HYPSGTS`, `HYPSGTC`, `HYPSCWK`, `HYPSCNT`, `HYPTDTA`, `HYPPLLT`, `HYPPLTR`, `HYPPCLC`, `HYPPCLS`, `HYPMPCD`, `HYPSSCC`, `HYPSLBL`, `HYPSLOC`, `HYJSLBL`, `HYJPLBL`, `HYPELBL`, `HYPEELD`, `HYPELG`, `HYPCLMS`, `HYPWSOQ`, `HYPCCRH`, `HYPCCSF`.
- **EDI files** — `EDOTPM`, `EDLTPXR1`, `EO56TK`, plus the `EDISAVE` library segment data.
- **Varsity (warehouse) files** — `VPCONT`, `VPDETL`, `VPPACK`, `MFH1MH`, `VPCNTRL3` (only the view's DDL is here).
- **Most called programs** — only ~15 of the called RPG/CL targets are present here; the rest (`HYR6900*`, `HHDRUC`, `HHDCSS`, `HHDSVI`, `HYR0156`, `HYC0114P`, `HYR3512`, `HYR2816`, `HYR0802C3`, `HYC3550`, `CSYRLO`, `CSYDTC`, `HSYSBM`, `BSTSDLY`, `COEPIN`, etc.) live in libraries on the target IBM i.
- **Field reference file `FLDREF`** — the PF members reference an external `FLDREF` for field definitions (`REFFLD(ORD#)`, `REFFLD(ITEM)`, `REFFLD(TURN)`, `REFFLD(IMQT)`, etc.). Without `FLDREF` the actual lengths/types of these fields aren't visible.

So this zip is a **slice** of a larger application — the parts that touch shipment processing, picking dashboards (the modern Profound UI additions), labels, BOL/EDI generation, credit-card settlement, and the bridge to a Varsity WMS. To compile any of it you'd need the rest of the libraries on a target IBM i.

## How to read the rest

Start with `program-architecture.md` for the **call graph** — `HYR0614` is the orchestrator that ties most of the shipping subprograms together. Then `data-model.md` for the **files**. `display-files.md` covers the **screens** that drive the interactive flows (`HYR0600`/`HYR0602`/`HYR0608` are the main shipment screens; `PICKBATD`/`PICKERD` are the modern picking dashboards).
