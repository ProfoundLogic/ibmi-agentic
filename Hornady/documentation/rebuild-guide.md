# Hornady POC — Full Rebuild Guide

This document is the complete recipe for reproducing the working state of the
Hornady POC in a fresh environment. It covers: what objects must exist, how to
build them, what data to load, and how to verify each menu option works.

Target library: **`AITSK00030`** (set via `IBMI_BUILD_LIBRARY`).

> If you are an AI agent picking this task up cold, read this file end to end
> before touching anything. Sections are in execution order — start at §1.

---

## Status of the six Hornady POC menu options

| # | Program | What it does | Status |
|---|---|---|---|
| **1** | `HYR0600` Shipment Processing | 5250 subfile of open shipments, 14/page | ⚠️ Compiles + launches; `HYPTDTA` schema correction (2026-06) cleared the original `SQL0206 (TDTABL)` blocker but the employee prompt loop still needs deeper validation. |
| 2 | `HYR0138` Pallet Contents Maint. | Pallet content editor | 🚫 Blocked — `HYD0138.DSPF` not in HornadyDemo package |
| **3** | `HYR0606` Shipment Lot Inquiry | Read-only subfile of lots for one detail line | ✅ **Working** — 15 lots paginate across 3 pages |
| **4** | `PICKBATR` Pick Batch Dashboard | Profound UI rich DSPF | ❌ **Blank screen — unresolved.**  The IBM i side is sound (program compiles cleanly with the `INZ` fix below, opens the DSPF, calls EXFMT on the `CTL` rich record, no joblog errors).  The PUI HANDLER call returns a populated `CTL` payload (17 field metadata entries + data array — same shape PICKERR's `LOGINR` returns).  But the user's browser paints a blank screen and the program stays in EXFMT forever, requiring the job to be killed to exit.  Multiple fix attempts (INZ, CSS-class rewrites, RDF JSON deployment under `/userdata/dspf/`, F-spec HANDLER patch) did not restore visible rendering.  See §15 for the full debug log; the root cause is on the PUI client side and would need browser-console / network-tab capture to pin down. |
| **5** | `PICKERR` Picker Workflow | Profound UI mobile | ✅ **Promoted (2026-06)** — compiles, opens, PUI Login screen renders (LOGINR record format with EMPCODE / EMPNAME / Alda/GI/West buttons / Login / Clear / Exit). |
| 6 | `HYR6080` Order Status Email | Batch | Stub (no DSPF needed; not yet promoted) |

Options 1, 3, and 5 are the demo path.  Option 4 is built and the program
runs, but its rich dashboard doesn't render in this PUI install.  Option 2
is blocked on a missing DSPF and shows an explicit blocker message.

---

## 1. Source layout

Everything lives in **`ibmi-agentic/`** (this repo). Key directories:

```
ibmi-agentic/
├── src/                 -- buildable source (.rpgle, .sqlrpgle, .clle, .pf,
│                          .lf, .dspf, .table.sql, .bnddir, .msgf …)
│   └── Rules.mk         -- codermake build rules (DO NOT edit Makefiles --
│                          only this file)
├── build/               -- per-object marker files (touched on success)
├── tmp/logs/            -- per-object compile listings (where to look first
│                          when a build fails)
└── Hornady/
    ├── src/HornadyDemo/Source/   -- the original HornadyDemo zip extracted
    │                                and EBCDIC-decoded (read-only reference)
    └── documentation/            -- this folder
        ├── rebuild-guide.md      -- (this file)
        ├── README.md             -- top-level project overview
        ├── source-inventory.md   -- per-member catalog
        ├── data-model.md         -- ERD + PF/LF/TABLE/VIEW/INDEX detail
        ├── program-architecture.md
        ├── display-files.md
        ├── hyr0600-gap-analysis.md
        ├── ddl/                  -- the original (pre-promotion) DDL drafts
        └── sample-data.sql       -- the seed-data script (idempotent inserts)
```

`src/` is what `codermake` consumes. `Hornady/src/HornadyDemo/Source/` is the
authoritative reference for the original Hornady source; it's not built directly.

---

## 2. Prerequisites

- **`codermake`** CLI installed and configured. Confirm with `codermake --help`.
- **SSH access** to the target IBM i. The agentic environment provides
  `~/.ssh/ibmi_key_dev` and an entry for `aidemo@idev.profoundlogic.com`. From
  inside a session: `ssh dev` should succeed without further configuration.
- **`aitool sql`** for `RUNSQLSTM`-equivalent queries against IBM i. Connection
  alias `dev` is preconfigured in `~/.aitool/config.json`.
- **Environment variable** `IBMI_BUILD_LIBRARY=AITSK00030` set in the shell that
  invokes `codermake`. (The harness sets this automatically.)

---

## 3. Build everything

From `ibmi-agentic/` (NOT from `src/`):

```bash
codermake
```

If you want to start fully from scratch, **don't** run `codermake clean` — its
cleanup only removes local `build/*` marker files; IBM i objects stay behind
and the next `codermake` then silently fails because `CRTPF`/`CRTLF`/`CRTBNDRPG`
won't overwrite an existing object. Instead, delete the IBM i objects first
(see §4), then build.

`codermake` will produce all 100+ targets across:

- 4 `.menu` objects (main MENU + HORNADY submenu + their MSGFs + DSPFs)
- ~9 main menu programs (hellor, wrkcustr, wrkcust1r, …, inq01l, tn510l)
- 4 EJS/RPGOA related programs and rich display files
- The Hornady block (§6 below)

Build logs land in `tmp/logs/<target>.log`. When a build fails, that's the
first place to look.

---

## 4. IBM i object cleanup (when needed)

If the source schema changes (e.g. you change a column type in
`hypsgcu.table.sql`), `codermake` re-uploads the source but `CRTPF` will fail
with `File X in Y already exists`. Drop the IBM i object explicitly, then
rebuild. Use the qsh `dltf`/`dltobj` commands via SSH:

```bash
ssh dev '/usr/bin/qsh -c "system \"dltf AITSK00030/HYPSGCU\""'
codermake hypsgcu.file
```

Logical files keep parent files locked, so drop LFs first. The shipment-family
group is interlinked — when changing `HYPSGCU` / `HYPSGDT` / `HYPSGLD`, drop in
this order:

```text
HYLSGCU1, HYLSGCU2, HYLSGCU3, HYLSGCU4,
HYLSGCU11, HYLSGCU12, HYLSGCU13, HYLSGCU14,
HYLSGDT3, HYLSSCC8, HYLSSCC9,
HYLSGLD1, HYLPLLT1, HYLMPCD2, HYLMPCD3, HRMEPL20,
HYPSGCU, HYPSGDT, HYPSGHD, HYPSGLD, HYPSGTD, HYPSGSD,
HYPINTR, HYPSCWK, HYPSGCV, HYPSGTC, HYPSGTS, HYPSSCC, …
```

A helper one-liner:

```bash
ssh dev '/usr/bin/qsh -c "
for f in HYLSGCU1 HYLSGCU2 HYLSGCU3 HYLSGCU4 HYLSGCU11 HYLSGCU12 HYLSGCU13 \
         HYLSGCU14 HYLSGDT3 HYLSSCC8 HYLSSCC9 HYLSGLD1 HYLPLLT1 HYLMPCD2 \
         HYLMPCD3 HREMPL20 HYPSGCU HYPSGDT HYPSGHD HYPSGLD HYPSGTD HYPSGSD \
         HYPINTR HYPSCWK HYPSGCV HYPSGTC HYPSGTS HYPSSCC HYPSTRK HYPSVCT \
         HYPTDTA HYPIDTA HYPMILG HYPPCLS HYPRCLT HYPPLLT HYPMPCD HYPPMLG \
         HYPWSOQ HDCUST HDCCMT HDDSHP HDIMST HDIWHS HDSHPV HDCTRY HDPCLS \
         OEORHD OEOCMT OECMWK OEDTWK OEHDWK OEBOLH HREMPL HYW0189 HYW0610A \
         GUPTDAT HYLTEMP2; do
  system \"dltf AITSK00030/\$f\" 2>&1 | grep CPC2191 | head -1
done
"'
```

Programs and display files don't have parent dependencies; just `dltobj` them.

After dropping, also delete the matching local marker files so `make` will
re-trigger:

```bash
rm -f build/<target>.file build/<target>.pgm
```

---

## 5. One-time IBM i setup

Two things `codermake` cannot create on its own:

### 5.1 `DATABASEID` data area

`HYR0600` (and other programs that share `HDSOPT`) read this on startup:

```bash
ssh dev '/usr/bin/qsh -c "
system \"crtdtaara AITSK00030/DATABASEID type(*char) len(2) value('"'"'XX'"'"')\"
"'
```

Without it, `HYR0600` raises *"Data area `*LIBL/DATABASEID` was not found."*

### 5.2 `HYHDSDATA` library + duplicate `HREMPL`

`HYR0138`'s source has an F-spec `FHREMPL20HY IF E K DISK EXTDESC('HYHDSDATA/HREMPL20')`.
That hard-codes the library `HYHDSDATA`. Create it and duplicate the file:

```bash
ssh dev '/usr/bin/qsh -c "
system \"crtlib lib(hyhdsdata) text('"'"'Hornady HDS data lib alias'"'"')\"
system \"crtdupobj obj(HREMPL)   fromlib(AITSK00030) objtype(*FILE) tolib(HYHDSDATA) data(*yes)\"
system \"crtdupobj obj(HREMPL20) fromlib(AITSK00030) objtype(*FILE) tolib(HYHDSDATA)\"
"'
```

### 5.3 GS1 data areas for `PICKERR`

`PICKERR` declares `wk_GS1Comp` / `wk_GS1Comp2` with `DTAARA(GS1COMP)` /
`DTAARA(GS1COMP2)`.  They hold the 6-digit GS1 company prefix used to parse
UCC-128 barcodes during scanning.  Without them the program halts on startup
with `RNQ0401 Data area *LIBL/GS1COMP was not found`.

```bash
ssh dev '/usr/bin/qsh -c "
system \"crtdtaara AITSK00030/GS1COMP  type(*char) len(6) value('"'"'054321'"'"')\" 2>&1 | head -1
system \"crtdtaara AITSK00030/GS1COMP2 type(*char) len(6) value('"'"'054322'"'"')\" 2>&1 | head -1
"'
```

The `PICKBATD` data area used by `PICKBATR` (`dcl-s lastUsedBatch packed(11:0)
DTAARA('PICKBATD')`) does NOT need to be pre-created -- the program creates it
on first call when needed.

### 5.4 ⚠️ Do NOT modify `AIDEMO/AIDEMO`'s job description

`AIDEMO` is a **shared user profile** — every concurrent CoderFlow task and
every other developer using this Profound UI install signs in as the same
user.  Changes to `AIDEMO/AIDEMO`'s `INLLIBL` (or to any other shared-profile
config) leak into every session.

An earlier version of this guide instructed `CHGJOBD JOBD(AIDEMO/AIDEMO)
INLLIBL(...)` to put `AITSK00030` and `AIPUI53001` ahead of `AIDEMOBASE` in
the library list.  **That instruction was wrong.**  It worked around two
specific symptoms (the main menu picking up the wrong `MENU` object, and a
`PUI0042` HANDLER-lookup error) but the proper fix for both is scoped to
the session, not the shared profile.

If you arrive in this environment and the main `Agentic Coding Demo Menu`
is missing the "5. Hornady Shipping Demo" entry, the task library
(`AITSK00030`) isn't in the library list of your session.  Resolve it
**in your session only**, not on the JOBD:

```bash
# Inside a 5250 / Genie session, BEFORE running GO MENU:
ADDLIBLE AITSK00030 *FIRST
GO MENU
```

For the PUI HANDLER reference in `PICKBATR` / `PICKERR`: the package
source ships `HANDLER('PROFOUNDUI(HANDLER)')` which assumes a library
called `PROFOUNDUI` containing `GENIE.SRVPGM`.  This Profound UI install
uses `AIPUI53001.LIB` instead.  The current `src/pickbatr.sqlrpgle` and
`src/pickerr.sqlrpgle` are patched to `HANDLER('GENIE(HANDLER)')` —
unqualified — which still requires the PUI install library to be in
`*LIBL`.  If your sessions don't already pick up `AIPUI53001` (or
whichever PUI install library applies), one of these is a cleaner long-term
fix than touching the JOBD:

* **Fully-qualified handler reference.** Change the F-spec to
  `HANDLER('AIPUI53001/GENIE(HANDLER)')` so no libl entry is needed.
  Source-portable across environments only if you templatise the library
  name; not ideal for a checked-in package.
* **Library alias.** Create a `PROFOUNDUI` library on this system that
  aliases (or holds duplicates of) the PUI install objects, so the
  package-original `HANDLER('PROFOUNDUI(HANDLER)')` resolves out of the
  box.  Then both `pickbatr.sqlrpgle` and `pickerr.sqlrpgle` can be
  reverted to the unmodified package source.

Order matters: `HREMPL` (PF) must be duplicated before `HREMPL20` (LF over it).

Both setup steps are idempotent — re-running just produces `already exists`
messages and proceeds.

---

## 6. The Hornady block in `src/Rules.mk`

`Rules.mk` already contains all the rules below — no need to add them on a
fresh checkout. This is documentation of what they declare and why, so the
order and dependencies are reproducible from scratch if `Rules.mk` ever needs
to be rebuilt.

### 6.1 Top-of-block — shared infrastructure

```makefile
fldref.file:        fldref.pf                       # field-reference file
hyrstubd.file:      hyrstubd.dspf                   # placeholder stub screen
hornady.file:       hornady.dspf                    # Hornady submenu DSPF
hornady.msgf:       hornady.msgf
hornady.menu:       hornady.file hornady.msgf
```

`menu.dspf` and `menu.msgf` (the main Agentic Coding Demo Menu) live higher up
in `Rules.mk` and aren't repeated.

### 6.2 Phase-1 PFs (shipment-family core)

Six `*.table.sql` skeletons derived from in-package LF keys + DSPF REFFLDs:

```makefile
hypsgcu.file: hypsgcu.table.sql
hypsghd.file: hypsghd.table.sql
hypsgdt.file: hypsgdt.table.sql
hypsgld.file: hypsgld.table.sql
hypsgtd.file: hypsgtd.table.sql
hypsgsd.file: hypsgsd.table.sql
```

### 6.3 LFs sourced from the HornadyDemo package

```makefile
hylsgcu1.file:  hylsgcu1.lf  | hypsgcu.file
hylsgcu2.file:  hylsgcu2.lf  | hypsgcu.file
hylsgcu3.file:  hylsgcu3.lf  | hypsgcu.file
hylsgcu4.file:  hylsgcu4.lf  | hypsgcu.file
hylsgcu11.file: hylsgcu11.lf | hypsgcu.file     # reverse-engineered, not in package
hylsgcu12.file: hylsgcu12.lf | hypsgcu.file
hylsgcu13.file: hylsgcu13.lf | hypsgcu.file
hylsgcu14.file: hylsgcu14.lf | hypsgcu.file
hylsgdt3.file:  hylsgdt3.lf  | hypsgdt.file
hylsscc8.file:  hylsscc8.lf  | hypsscc.file
```

### 6.4 Phase-2 PFs (the rest of HYR0600's references)

```makefile
hypintr.file: hypintr.table.sql
hypscwk.file: hypscwk.table.sql
hypsgcv.file: hypsgcv.table.sql
hypsgtc.file: hypsgtc.table.sql
hypsgts.file: hypsgts.table.sql
hypsscc.file: hypsscc.table.sql
hypstrk.file: hypstrk.table.sql
hypsvct.file: hypsvct.table.sql
hyptdta.file: hyptdta.table.sql
hypidta.file: hypidta.table.sql
hypmilg.file: hypmilg.table.sql
hyppcls.file: hyppcls.table.sql
hyprclt.file: hyprclt.table.sql
hdcust.file:  hdcust.table.sql
hdccmt.file:  hdccmt.table.sql
hddshp.file:  hddshp.table.sql
hdimst.file:  hdimst.table.sql
hdiwhs.file:  hdiwhs.table.sql
hdshpv.file:  hdshpv.table.sql
oeorhd.file:  oeorhd.table.sql
oeocmt.file:  oeocmt.table.sql
oecmwk.file:  oecmwk.table.sql
oedtwk.file:  oedtwk.table.sql
oehdwk.file:  oehdwk.table.sql
guptdat.file: guptdat.table.sql
hrempl.file:  hrempl.table.sql
hyw0189.file: hyw0189.table.sql
hyw0610a.file: hyw0610a.table.sql
```

### 6.5 HYR0138 additional tables (Phase-2 extension)

```makefile
hyppllt.file:  hyppllt.table.sql
hypmpcd.file:  hypmpcd.table.sql
hdctry.file:   hdctry.table.sql
oebolh.file:   oebolh.table.sql
hdpcls.file:   hdpcls.table.sql
hyltemp2.file: hyltemp2.table.sql
hypwsoq.file:  hypwsoq.table.sql
hyppmlg.file:  hyppmlg.table.sql
hylpllt1.file: hylpllt1.lf | hyppllt.file
hylsscc9.file: hylsscc9.lf | hypsscc.file
hylmpcd2.file: hylmpcd2.lf | hypmpcd.file
hrempl20.file: hrempl20.lf | hrempl.file
```

### 6.6 Display files

```makefile
hyd0600a.file: hyd0600a.dspf | hypsgcu.file
hyd0600b.file: hyd0600b.dspf | hypsgdt.file
hyd0606.file:  hyd0606.dspf  | hypsgld.file hypsgdt.file
```

(`HYD0138.DSPF` is the missing piece blocking option 2.)

### 6.7 Service programs (stubs)

```makefile
hhdcss.module:  hhdcss.rpgle
hhdcss.srvpgm:  hhdcss.module
hhdsvi.module:  hhdsvi.rpgle
hhdsvi.srvpgm:  hhdsvi.module
hoedip.module:  hoedip.rpgle
hoedip.srvpgm:  hoedip.module

hyr0600s.module: hyr0600s.rpgle
hyr0600s.srvpgm: hyr0600s.module

hyr0138s.module: hyr0138s.rpgle
hyr0138s.srvpgm: hyr0138s.module
```

`hyr0600s` exports `Center`, `BOLReq`, `RtvForeign`, `RtvCSPRO#`, `CompCustShip`.
`hyr0138s` exports the pallet/pallet-report procedures plus `HHDISI`/`HHDPCS`.

### 6.8 Sibling-program stubs (called by HYR0600 / HYR0138)

```makefile
hyr0602.pgm: hyr0602.rpgle
hyr0608.pgm: hyr0608.rpgle
hyr0189.pgm: hyr0189.rpgle
hyr0520.pgm: hyr0520.rpgle
vprbldp.pgm: vprbldp.rpgle
hyr0156.pgm: hyr0156.rpgle
hyc0138.pgm: hyc0138.clle
hyc3512.pgm: hyc3512.clle
hyc3550.pgm: hyc3550.clle
```

### 6.9 Binding directories

```makefile
hyr0600.bnddir: hyr0600.bnddir | hyr0600s.srvpgm hhdcss.srvpgm hhdsvi.srvpgm hoedip.srvpgm
hyr0606.bnddir: hyr0606.bnddir | hyr0600s.srvpgm
```

Both H-specs use `BNDDIR('HYR0600')` / `BNDDIR('HYR0606')` respectively. The
bnddir is what lets `CRTSQLRPGI` (source-based) resolve the EXTPROC symbols at
bind time. Without these, the program compiles 0-error but the bind step fails.

### 6.10 Real Hornady programs

```makefile
hyr0600.pgm: hyr0600.sqlrpgle hyr0600_pr.rpgle \
             hyd0600a.file hyd0600b.file \
             hhdcss.srvpgm hhdsvi.srvpgm hoedip.srvpgm hyr0600s.srvpgm \
             hyr0600.bnddir \
             | (all 30+ supporting .file targets and sibling .pgm targets)

hyr0606.pgm: hyr0606.rpgle hyr0606_pr.rpgle \
             hyd0606.file hyr0606.bnddir \
             | hylsgld1.file hypsgdt.file hyr0600s.srvpgm

hyc0606.pgm: hyc0606.clle | hyr0606.pgm        # menu wrapper that supplies 6 typed parms
```

The full `hyr0600.pgm` dependency line is long — see `Rules.mk` for the exact list.

### 6.11 Menu stubs (options 2, 6)

```makefile
hyr0138.pgm: hyr0138.rpgle hyrstubd.file
hyr6080.pgm: hyr6080.rpgle hyrstubd.file
```

These bind against the single shared display file `hyrstubd.file`.  The
RPG stubs are 12 lines each — they set 4 short text strings and `EXFMT stub`.
Options 4 and 5 (PICKBATR / PICKERR) used to be on the same stub pattern but
were promoted to the real Profound UI programs in 2026-06 — see §6.12.

### 6.12 Pick-batch data layer + real PICKBATR / PICKERR (2026-06)

```makefile
# Pick-batch SQL tables (new -- see Hornady/documentation/data-model.md).
pickbathp.file:  pickbathp.table.sql
pickbatdp.file:  pickbatdp.table.sql
pickbatlog.file: pickbatlog.table.sql
pickbatmp.file:  pickbatmp.table.sql
pickmbatdp.file: pickmbatdp.table.sql
oeorhp.file:     oeorhp.table.sql
oeordp.file:     oeordp.table.sql
oeordt.file:     oeordt.table.sql

# DDS-keyed PF for non-unique scan history.
pickbatsp.file:  pickbatsp.pf

# Logical files.
pickbathl1.file: pickbathl1.lf | pickbathp.file
pickbathl2.file: pickbathl2.lf | pickbathp.file
pickbatdl1.file: pickbatdl1.lf | pickbatdp.file
pickbatdl2.file: pickbatdl2.lf | pickbatdp.file
oeordp01.file:   oeordp01.lf   | oeordp.file
hylsgsd1.file:   hylsgsd1.lf   | hypsgsd.file

# Pick-batch service program + bnddir (real source from package).
pickbatsv.module:  pickbatsv.sqlrpgle pickbatsv_pr.rpgle
pickbatsv.srvpgm:  pickbatsv.module
picksvbdir.bnddir: picksvbdir.bnddir | pickbatsv.srvpgm

# PICKERR-specific bnddir (real srvpgms HYR9960/HYR9962 stubbed in /src/).
hyr9960.module:  hyr9960.rpgle pickerr_pr.rpgle
hyr9960.srvpgm:  hyr9960.module
hyr9962.module:  hyr9962.rpgle pickerr_pr.rpgle
hyr9962.srvpgm:  hyr9962.module
pickerr.bnddir:  pickerr.bnddir | hyr9960.srvpgm hyr9962.srvpgm

# Sibling-program stubs PICKBATR calls via EXTPGM (real bodies too heavy).
pickbatdr.pgm:   pickbatdr.rpgle
pickbatlr2.pgm:  pickbatlr2.rpgle

# PUI Rich Display Files.
pickbatd.file:   pickbatd.dspf | pickbathp.file pickbatdp.file
pickerd.file:    pickerd.dspf  | pickbathp.file pickbatdp.file

# Real PICKBATR (replaces the hyrstubd stub).
pickbatr.pgm: pickbatr.sqlrpgle pickbatsv_pr.rpgle pickbatd.file \
              picksvbdir.bnddir \
            | pickbathp.file pickbatdp.file pickbatlog.file pickbatmp.file \
              pickmbatdp.file pickbathl1.file pickbathl2.file \
              pickbatdl1.file pickbatdl2.file oeorhp.file oeordp.file \
              oeordp01.file oeordt.file hylsgsd1.file hdcust.file \
              hdccmt.file hdimst.file hdiwhs.file guptdat.file hypsgsd.file \
              pickbatdr.pgm pickbatlr2.pgm

# Real PICKERR (replaces the hyrstubd stub).
pickerr.pgm: pickerr.sqlrpgle pickerr_pr.rpgle pickerd.file pickerr.bnddir \
           | pickbathp.file pickbathl1.file pickbathl2.file pickbatdp.file \
             pickbatsp.file pickbatmp.file oeorhp.file oeordp.file \
             oeordp01.file hdcust.file hdccmt.file hdimst.file hdiwhs.file \
             guptdat.file hypsgdt.file pickbatlr2.pgm
```

Inline `/COPY` replacements (same pattern as `hyr0600_pr.rpgle`):

* `src/pickbatsv_pr.rpgle` — replaces `/COPY QCPYLESRC,PICKBATSVD` in
  `pickbatr.sqlrpgle` and `pickbatsv.sqlrpgle`.  Declares only
  `pickbatsv_setDefaultPickOrder` (the one procedure PICKBATR uses).
* `src/pickerr_pr.rpgle` — replaces `/COPY QPRPSRC,HYR9960` and
  `/COPY QPRPSRC,HYR9962` in `pickerr.sqlrpgle`.  Declares
  `BarcodeItem / BarcodeQty / BarcodeItemC / BarcodeQtyC / sd_HYR9960`.

The HYR9960 / HYR9962 implementations in `src/hyr9960.rpgle` /
`src/hyr9962.rpgle` are **stubs**: they return blank / 0 because the real
bodies need the `BARDATA` / `BARCUST` tables (in-package PFs we have not
promoted yet).  Wiring up real barcode parsing means promoting BARDATA /
BARCUST and replacing those stubs with the real `HYR9960.RPGLE` /
`HYR9962.RPGLE` from the HornadyDemo package.

---

## 7. The `/COPY` rewrites

The real Hornady `.sqlrpgle` / `.rpgle` sources contain `/COPY` directives that
pull in prototype blocks from `QPRPSRC` source-PF members we don't have. To
keep the codermake build working without rebuilding `QPRPSRC`, we ship two
inline replacements:

- `src/hyr0600_pr.rpgle` — replaces `/COPY QPRPSRC,HYR0610`, `HYR0614`, `HYR9930`
  in `hyr0600.sqlrpgle`. Contains only the prototypes / standalone variables /
  the one constant (`BOLREQ`) that HYR0600 actually references.

- `src/hyr0606_pr.rpgle` — replaces `/COPY QPRPSRC,HYR9930` in `hyr0606.rpgle`.
  Just the `Center` prototype.

The source files themselves were modified in place — the `/COPY *LIBL/QPRPSRC,…`
lines were changed to `/copy hyr0600_pr.rpgle` (or `hyr0606_pr.rpgle`) and the
remaining `/COPY` lines became RPG comments. If you re-extract from the
HornadyDemo zip, you need to redo this patching.

Codermake's `/COPY` preprocessor only treats operands ending in `.rpgle` /
`.sqlrpgle` as IFS file paths. `.rpgleinc` is *not* recognized — keep the
`.rpgle` extension.

---

## 8. Schema notes (where my reverse-engineering corrected itself)

These reflect lessons learned from iterative compile-error fixing. The
schemas as committed are correct; this is for your understanding if you ever
need to extend a table.

| Column | Type | Why |
|---|---|---|
| `HYPSGCU.GCGRP#`, `HYPSGDT.GDGRP#`, etc. | `DECIMAL(9,0)` | HYR0600 uses 9-digit zoned working variables (`wk_ShipGrp# S 9S 0`). 7 digits is too narrow. |
| `HYPSGDT.GDTURN`, `HYPSGLD.LDTURN` | `DECIMAL(9,0)` | Same — HYR0606's `wk_Turn#` is 9S 0. |
| `HYPSGDT.GDTSEQ`, `HYPSGLD.LDTSEQ` | `DECIMAL(3,0)` | `wk_TurnSeq# S 3S 0`. |
| `HYPSGDT.GDDVRT` | `DECIMAL(3,0)` | HYR0606 does `%editc(GDDVRT:'Z')` — only valid on numeric. |
| `HYPSGDT.GDSSCC`, `HYPSGLD.LDSSCC`, `HYPSSCC.SCSSCC`, `HYPSCWK.S1SSCC` | `DECIMAL(20,0)` | Used in `%editc(...:'Z')` — must be numeric. |
| `HYPSGCU.GCSHPV`, `HDSHPV.SVSVSV`, `HYPSVCT.VTSHPV` | `CHAR(2)` | HHDSVI prototype declares ShpVia as 2A. |
| `HYPSGCU.GCSTCT` | `CHAR(2)` | Used as a 2-char flag in RtvForeign(...:wk_GCSTCT). |
| `HYPSGDT` PK | `(GDGRP#, GDTURN, GDTSEQ, GDRSEQ, GDITEM, …)` | HYR0606 does `key_GD SETLL HYPSGDT` with a 3-field KLIST whose 3rd field is numeric Tseq. |
| `HREMPL` | key `EMEMPL` not `EMUSER` | HYR0600 looks up by `SELECT EMFNAM, EMLNAM FROM HREMPL WHERE EMEMPL=:wk_EMPL`. |
| `HYPINTR` keys | `(GIORD#=8, GITURN=9, GIAIV#=7)` | HYR0138's working vars set these widths. |

When changing any of the above on the real Hornady system, also rebuild
HYR0600 and HYR0606 — their `LIKE(GCGRP#)` etc. variables silently re-pick the
new type only on next compile.

---

## 9. Calling conventions (for CL → RPG)

`HYR0606` (and any other RPG program with zoned-decimal `S 0` parameters)
**cannot** be called from CL with `DCL TYPE(*DEC)`. CL `*DEC` is packed; the
RPG zoned PI expects a different byte layout, and you get **`MCH1202`
decimal-data error**.

The workaround used in `hyc0606.clle` is to declare `*CHAR` variables whose
EBCDIC zoned-character representation is the value you want:

```cl
DCL VAR(&SHIPGRP) TYPE(*CHAR) LEN(9) VALUE('000001001')
```

EBCDIC `'0'` is `0xF0`, `'1'` is `0xF1`, etc. — the first nibble (`F`) is the
positive zoned sign, the second nibble is the digit. So `'000001001'` reads
as the zoned 9-digit value `1001`.

If you need to pass a different value at runtime, use `CHGVAR` with `%EDITC`
to build the zoned string, *not* via `*DEC`.

---

## 10. Seed data

Run **`Hornady/documentation/sample-data.sql`** to populate the working tables.
The script is idempotent (each section deletes its target rows before
inserting) so it's safe to re-run.

### 10.1 Upload + execute

```bash
# Pipe the file to IFS, set its CCSID, then run via RUNSQLSTM SRCSTMF.
ssh dev '/usr/bin/qsh -c "mkdir -p /tmp/hornady"'
cat Hornady/documentation/sample-data.sql | ssh dev 'cat > /tmp/hornady/sample.sql'
ssh dev '/usr/bin/qsh -c "
setccsid 1208 /tmp/hornady/sample.sql > /dev/null 2>&1
system \"runsqlstm srcstmf('"'"'/tmp/hornady/sample.sql'"'"') commit(*none) dftrdbcol(AITSK00030) errlvl(40)\"
"'
```

The `setccsid 1208` is important. Without it, qsh's `cat >` translates the
incoming UTF-8 through the job's default CCSID and you get a binary-garbled
file that `RUNSQLSTM` can't parse.

`COMMIT(*NONE)` because the tables don't have journaling. The final
`COMMIT;` line in the script errors with `SQL7007` — harmless.

### 10.2 Expected row counts

| Table | Rows | Notes |
|---|---|---|
| `HREMPL` | 3 | employees 12345 (J Smith), 22001 (J Doe), 30099 (B Jones) |
| `HDCUST` | 8 | customer numbers 1000100..1000800 |
| `HDSHPV` / `HYPSVCT` | 5 each | UP, FX, YR active; PU, BW inactive |
| `HDIMST` | 6 | demo SKUs `DEMO-9MM-115`, etc. |
| `HYPSGHD` | 30 | shipment headers 1001..1030, `GHDCLOC='WEST'`, `GHSTS='O'` |
| `HYPSGCU` | 30 | shipment-customer rows — HYR0600's main subfile fodder |
| `HYPSGDT` | 33 | detail lines |
| `HYPSGLD` | 47 | 15 on `(1001,1,1,1)` for HYR0606 paging, 1–2 on every other shipment |
| `OEORHD` | 8 | sales-order headers 10000001..10000008 |

### 10.3 Filter rules baked into the seed values

Two non-obvious filter conditions inside `HYR0600` shape the seed values —
if these aren't right, the main subfile comes up empty:

- `HYPSGCU.GCTSTP2` must be `*Loval` (`0001-01-01-00.00.00.000000`) on rows
  you want shown. The program treats any non-Loval value as "shipment
  completed" and excludes it from the "Open Shipments Only" view (the default).
- `HYPSGHD.GHDCLOC` must be one of `WEST`, `ALDA`, or `GI`. The DC-location
  filter on the main screen rejects other values. The seed script uses
  `'WEST'`.

---

## 11. Verification — end-to-end smoke test

Open an interactive 5250 session (Genie or any TN5250 client) as
`aidemo@idev.profoundlogic.com`. The agent-environment harness has the
`ibmi-interactive-session` skill if you're running headless.

### 11.1 Main menu

You should land on **`Agentic Coding Demo Menu`** with options 1–5 and 90.
Option 5 is "Hornady Shipping Demo".

### 11.2 Option 5 → Hornady POC submenu

Six options:

```
Shipment processing:
  1. Shipment Processing    (HYR0600)
  2. Pallet Contents Maint. (HYR0138)
  3. Shipment Lot Inquiry   (HYR0606)

Picking dashboard (Profound UI):
  4. Pick Batch Dashboard   (PICKBATR)
  5. Picker Workflow        (PICKERR)

Batch / outbound:
  6. Order Status Email     (HYR6080)

 88. Return to main menu
 90. Sign off
```

### 11.3 Option 1 happy path

1. Pick option 1.
2. **"Employee Number Prompt"** window appears. Enter `12345`.
3. Main shipment screen renders — header `Shipper: J Smith`, subfile shows
   shipments 1002–1015 (14 rows), `More..` marker.
4. **Page Down** → shipments 1016–1029.
5. **Page Down again** → shipment 1030 alone, `Bottom` marker.
6. **F3** to exit back to the Hornady menu.

### 11.4 Option 3 happy path

1. Pick option 3 (no prompt — `HYC0606` supplies the parms).
2. Lot Inquiry header: `Group: 1001`, `Order: 10000001`, item `DEMO-9MM-115`.
3. Subfile shows lots `LOT240601`..`LOT240608` (10 rows), `More..` marker.
4. **Page Down** → `LOT240609`, `LOT240610`, more pending.
5. **Page Down again** → `LOT240611`, `LOT240612`, `LOT240613`, `Bottom`.
6. **F3** to exit.

### 11.5 Option 5 happy path (PICKERR Picker Workflow)

1. Pick option 5 from the Hornady submenu.
2. The PUI mobile-form login screen renders (record format `LOGINR`).  The
   genie session shows it as a `handler` block in the response JSON, not as
   a 5250 buffer -- in a real browser the user sees a phone-sized layout
   with a `Log In` button, an `Employee Number` text field, a `Clear Form`
   button, and three big warehouse buttons (Alda / GI / West).
3. Enter a valid employee code (e.g. `12345` -- John Smith from `HREMPL`),
   pick a warehouse, and tap `Log In`.  Subsequent screens (assigned-batch
   list / pick-line detail / scan capture) load from the same DSPF.

### 11.6 Option 4 known issue (PICKBATR Pick Batch Dashboard)

Selecting option 4 calls `PICKBATR`, which opens `PICKBATD.DSPF` through
the PUI HANDLER and reaches the `EXFMT` on the `CTL` (dashboard) record
format.  Validation against the IBM i side checks out:

- Program compiles cleanly (`Program PICKBATR placed in library AITSK00030.
  10 highest severity` — warnings only).
- `ACTIVE_JOB_INFO` shows the job running with `FUNCTION = 'PICKBATR'` and
  near-zero CPU (idle, waiting on EXFMT).
- No errors in the joblog.
- The PUI HANDLER call (via `genie_get`) returns a populated `handler[0]`
  block with `name = "CTL"`, 17 field metadata entries, and a 17-value
  `data` array — the same shape PICKERR's `LOGINR` returns (which is
  known to render correctly).

In a browser, however, the dashboard paints **blank** and the program
sits in EXFMT indefinitely, requiring `ENDJOB` from another session to
exit.  Multiple fix attempts didn't restore visible rendering — see §15
for the debug log.

Two IBM i-side fixes ARE load-bearing and need to stay applied:

> **Patch A — RPG `INZ` (required to compile + run):** the package
> source's `Dcl-DS Ctl_Fields_Out LikeRec(DisplyFile.Ctl:*OUTPUT)` allocates
> a DS with zoned-decimal hidden subfields but does not zoned-zero them.
> The first `WRITE Ctl_Fields_Out` errors `CPF27AF "Edit mask not valid"`
> because `X'00'` isn't a valid zoned digit.  `src/pickbatr.sqlrpgle`
> applies `INZ` to every `Dcl-DS LikeRec(...)` declaration.  Without this
> the program crashes before the screen is even attempted.

> **Patch B — F-spec HANDLER library (environment-specific):**
> package source uses `HANDLER('PROFOUNDUI(HANDLER)')` but this Profound UI
> install puts `GENIE.SRVPGM` in `AIPUI53001.LIB`, not `PROFOUNDUI.LIB`.
> `src/pickbatr.sqlrpgle` and `src/pickerr.sqlrpgle` are patched to
> `HANDLER('GENIE(HANDLER)')` (unqualified).  See §5.4 for cleaner
> alternatives — particularly creating a `PROFOUNDUI` library alias on
> this system so the package source can be reverted untouched.

### 11.7 Other options

- Options 2 and 6 still land on the shared stub display file (`hyrstubd`).
  Option 2's stub explicitly says it's blocked on `HYD0138.DSPF`. Press
  **F3/F12/Enter** to return to the Hornady menu.
- **Option 88** → back to the main menu.
- **Option 90** → sign off.

---

## 12. What's *not* covered

- **Compiling the real `HYR0138`** — needs `HYD0138.DSPF` (18 record formats)
  which is absent from the HornadyDemo package. All other prerequisites
  (tables, LFs, service-program stubs, sibling program stubs, prototype
  include) are in place from the previous turn — just drop the DSPF source
  into `src/hyd0138.dspf`, add a Rules.mk entry, and replace the
  `hyr0138.rpgle` stub with the real `HYR0138.SQLRPGLE`.
- **~~Compiling the real `PICKBATR` / `PICKERR`~~** — done in 2026-06.  Both
  programs now compile cleanly out of `codermake` with the data-layer
  promotion described in §6.12.  PICKERR's PUI login screen renders end-to-end
  through the Profound UI runtime; PICKBATR renders its CTL dashboard
  record format through the same handler (the earlier `CPF27AF` blocker
  was fixed by adding `INZ` to the `LikeRec(...)` data structures -- see
  §15).  The real bodies of `PICKBATDR` and `PICKBATLR2` are still stubs
  in `src/`, and the barcode utility srvpgms `HYR9960` / `HYR9962` are
  stub procedures (return blank / 0) -- replacing those with the real
  package source needs the `BARDATA` / `BARCUST` PFs promoted first.
- **Compiling the real `HYR6080`** — batch email program. Calls a bunch of
  external email-related EXTPROCs we don't have stubs for. Easy after the
  data-layer pattern is set, but not done.
- **Real Hornady security**. `HYR0600` references `QSYGETPH` / `QSYRLSPH` for
  profile-handle swaps in maintenance mode. Stubbing those would be required
  if you ever go past option-1 read-only mode.

---

## 13. Quick smoke test from a fresh agent

If you're an agent picking this up and just want to verify "is anything broken":

```bash
cd /workspace/ibmi-agentic
codermake                      # should produce no errors, ends with "make: Nothing to be done for 'all'." after the first clean run
echo "Build OK"

# Confirm critical objects exist on IBM i
ssh dev '/usr/bin/qsh -c "ls /qsys.lib/AITSK00030.lib/ | grep -E \"^(HYR0600|HYR0606|HYC0606|HYD0600A|HYD0606|HYPSGCU|HYPSGLD)\\.(PGM|FILE|MENU)\$\""'

# Confirm seed data is loaded
aitool sql --input '{connection:"dev",sql:"SELECT '"'"'HYPSGCU'"'"' AS TBL, COUNT(*) AS N FROM AITSK00030.HYPSGCU UNION SELECT '"'"'HYPSGLD'"'"', COUNT(*) FROM AITSK00030.HYPSGLD",format:"table"}'
# Expected: HYPSGCU=30, HYPSGLD=47
```

If any of those fail, walk back through the relevant section above.

---

## 14. File-by-file checklist

For convenience: the complete list of `src/` files that participate in the
Hornady POC (Hornady-specific only — the main demo menu / wrkcust / hello
sources are unchanged):

```
src/menu.dspf                 src/menu.msgf                src/hyrstubd.dspf
src/hornady.dspf              src/hornady.msgf
src/hyr0600.sqlrpgle          src/hyr0600_pr.rpgle         src/hyr0600s.rpgle
src/hyr0600.bnddir            src/hyd0600a.dspf            src/hyd0600b.dspf
src/hyr0606.rpgle             src/hyr0606_pr.rpgle         src/hyr0606.bnddir
src/hyd0606.dspf              src/hylsgld1.lf              src/hyc0606.clle
src/hyr0138.rpgle             src/hyr0138_pr.rpgle         src/hyr0138s.rpgle
src/hyr0156.rpgle             src/hyc0138.clle             src/hyc3512.clle
src/hyc3550.clle              src/hyr0602.rpgle            src/hyr0608.rpgle
src/hyr0189.rpgle             src/hyr0520.rpgle            src/vprbldp.rpgle
src/hhdcss.rpgle              src/hhdsvi.rpgle             src/hoedip.rpgle
src/pickbatr.rpgle            src/pickerr.rpgle            src/hyr6080.rpgle
src/fldref.pf
src/hypsgcu.table.sql         src/hypsghd.table.sql        src/hypsgdt.table.sql
src/hypsgld.table.sql         src/hypsgtd.table.sql        src/hypsgsd.table.sql
src/hypintr.table.sql         src/hypscwk.table.sql        src/hypsgcv.table.sql
src/hypsgtc.table.sql         src/hypsgts.table.sql        src/hypsscc.table.sql
src/hypstrk.table.sql         src/hypsvct.table.sql        src/hyptdta.table.sql
src/hypidta.table.sql         src/hypmilg.table.sql        src/hyppcls.table.sql
src/hyprclt.table.sql         src/hyppllt.table.sql        src/hypmpcd.table.sql
src/hyppmlg.table.sql         src/hypwsoq.table.sql        src/hyltemp2.table.sql
src/hdcust.table.sql          src/hdccmt.table.sql         src/hddshp.table.sql
src/hdimst.table.sql          src/hdiwhs.table.sql         src/hdshpv.table.sql
src/hdctry.table.sql          src/hdpcls.table.sql
src/oeorhd.table.sql          src/oeocmt.table.sql         src/oecmwk.table.sql
src/oedtwk.table.sql          src/oehdwk.table.sql         src/oebolh.table.sql
src/guptdat.table.sql         src/hrempl.table.sql
src/hyw0189.table.sql         src/hyw0610a.table.sql
src/hylsgcu1.lf  src/hylsgcu2.lf  src/hylsgcu3.lf  src/hylsgcu4.lf
src/hylsgcu11.lf src/hylsgcu12.lf src/hylsgcu13.lf src/hylsgcu14.lf
src/hylsgdt3.lf  src/hylsscc8.lf  src/hylsscc9.lf
src/hylpllt1.lf  src/hylmpcd2.lf  src/hrempl20.lf
src/Rules.mk
```

Plus the seed-data script:

```
Hornady/documentation/sample-data.sql
```

That's everything required to reconstruct the working state of options 1 and 3.

Plus the new pick-batch sources added in 2026-06 (see §6.12):

```
src/pickbatr.sqlrpgle      src/pickbatsv_pr.rpgle    src/pickbatsv.sqlrpgle
src/pickerr.sqlrpgle       src/pickerr_pr.rpgle      src/pickbatdr.rpgle
src/pickbatlr2.rpgle       src/hyr9960.rpgle         src/hyr9962.rpgle
src/pickbatd.dspf          src/pickerd.dspf
src/picksvbdir.bnddir      src/pickerr.bnddir
src/pickbathp.table.sql    src/pickbatdp.table.sql
src/pickbatlog.table.sql   src/pickbatmp.table.sql   src/pickmbatdp.table.sql
src/pickbatsp.pf
src/oeorhp.table.sql       src/oeordp.table.sql      src/oeordt.table.sql
src/pickbathl1.lf          src/pickbathl2.lf
src/pickbatdl1.lf          src/pickbatdl2.lf
src/oeordp01.lf            src/hylsgsd1.lf
```

That reconstructs the working state of options 1, 3, and 5 plus the
infrastructure for option 4.

---

## 15. Known follow-ups

| Item | Detail |
|---|---|
| `PICKBATR` `CPF27AF` (resolved, patch applied) | First build errored at the initial `WRITE DisplyFile.Ctl Ctl_Fields_Out` with `CPF27AF "Edit mask not valid" / RNX1299 I/O error`.  Root cause: zoned-decimal hidden fields in the `LikeRec(DisplyFile.Ctl:*OUTPUT)` data structure started as `X'00'` instead of the zoned-`'0'` (`X'F0'`) the runtime expects.  Fix: `INZ` on every `Dcl-DS LikeRec(...)` declaration in `pickbatr.sqlrpgle`.  **Patch is in place; required to compile + run.** |
| `PICKBATR` blank dashboard (UNRESOLVED) | After the `CPF27AF` fix the program reaches `EXFMT` cleanly and the PUI HANDLER returns a populated `CTL` payload, but the browser paints blank and the program sits in EXFMT until the job is killed.  Things I tried that did NOT restore rendering: (1) adding `*PUI CANVASHEIGHT(...) CANVASWIDTH(...)` to the DSPF (PICKERD has them, PICKBATD doesn't) — no change.  (2) Rewriting all `hornady-*` and `blueprint-*` CSS class strings in the DSPF to `pui-*` or blanks (preserving DDS column-80 boundaries via space padding) — no change; patch left in place because it can't hurt and the original references undefined classes anyway.  (3) Extracting the screen JSON and deploying it as a Rich Display File at `/www/profoundui/htdocs/profoundui/userdata/dspf/PICKBATD.json` — no change.  (4) Sending `CTL.BTNEXIT=1` as a response indicator to confirm the program responds to input — it doesn't; the EXFMT never returns.  The HANDLER call returns the same shape PICKERR's `LOGINR` returns (and `LOGINR` renders correctly for the user), so the IBM i side and PUI HANDLER protocol are sound.  The remaining failure is on the **PUI client-side render**, where we have no visibility from this headless harness.  Next step: open option 4 with browser DevTools attached and capture the JS console (any red errors) and Network tab (any 404/500 responses, particularly to `/profoundui/api/*` or `/userdata/dspf/*`). |
| `PICKBATR` subfile contents (deferred) | Even once the dashboard renders, the seeded batches may not show on first load — the C1 cursor filters by `showcomp`, `invloc`, MegaBatch exclusion, etc.  Next step: trace the SQL cursor's default WHERE clause and confirm at least one of the 5 seeded `PICKBATHP` rows matches.  Deferred until the blank-screen issue above is resolved. |
| `HYR0600` employee prompt | Schema corrections (`HYPTDTA` / `GUPTDAT` to TD-prefix; `HDCUST` to CM-prefix) cleared the original `SQL0206 (TDTABL)` blocker.  Employee Number Prompt opens cleanly but the next screen still doesn't paint -- needs another schema-vs-source pass. |
| `PICKBATDR` / `PICKBATLR2` real bodies | Currently stubs in `src/`.  Real bodies are large (700+ lines each) and touch additional tables (OEORDT, HDDSHP, plus several `HYP*` LFs).  Plug in by copying the real `.SQLRPGLE` source from the HornadyDemo package, adding any missing /COPY shims, and extending the schemas. |
| `HYR9960` / `HYR9962` real bodies | Currently return blank / 0 -- enough to bind PICKERR but not enough to parse real GS1 barcodes.  Promote `BARDATA.PF` and `BARCUST.PF` from the HornadyDemo package, then drop the real source bodies in to replace `src/hyr9960.rpgle` / `src/hyr9962.rpgle`. |
| `HANDLER('GENIE(HANDLER)')` hard-coded | The HANDLER reference in `pickbatr.sqlrpgle` / `pickerr.sqlrpgle` is patched from `'PROFOUNDUI(HANDLER)'` (the in-package default) to `'GENIE(HANDLER)'` because this environment's Profound UI install lives under `AIPUI53001.LIB` (no `PROFOUNDUI.LIB`).  If you re-extract from the HornadyDemo zip, re-apply the patch.  Long-term, consider a CL/QSH preprocessor step in `codermake` so the HANDLER library name is environment-configurable. |

---

## 16. Operational hygiene — what NOT to touch

This environment uses a **shared `AIDEMO` user profile** for every Profound
UI session, including concurrent CoderFlow tasks.  Any change to objects
under that profile leaks across every other session.  The following changes
should be **scoped to your session only** (via `ADDLIBLE`, `CHGCURLIB`,
session-local `SBMJOB`, etc.) and **never** committed to the shared profile:

| Object | Why it's shared | If you need a change, do this instead |
|---|---|---|
| `AIDEMO/AIDEMO` job description (especially `INLLIBL`) | Every AIDEMO sign-on inherits the JOBD's library list.  Adding the wrong library here can shadow real objects in unrelated sessions and is hard to debug. | `ADDLIBLE <lib> *FIRST` inside your own session, before `GO MENU` or before the program that needs it. |
| `AIDEMO` user-profile fields (`INLPGM`, `INLMNU`, `CURLIB`, `JOBD`, `SPCAUT`) | Same — global across sessions. | Override per-session via `SBMJOB` parms or via the PUI auth call's environment variables (see §5.5 below). |
| Shared library content (`AIDEMOBASE`, `DRPUIDEV`, etc.) | Multiple tasks use these libraries' MENU, CSS, JS, etc.  Renaming or replacing objects here can break unrelated demos. | Build into your own task library and ADDLIBLE it ahead of the shared lib for your session. |

### What I broke and you fixed during this task

During the 2026-06 PICKBATR / PICKERR promotion I twice ran
`CHGJOBD JOBD(AIDEMO/AIDEMO) INLLIBL(...)` to work around two symptoms:

1. **First change** — added `AITSK00030` to the front of `INLLIBL` because the
   default `GO MENU` was resolving to a stale `AITSK00035/MENU` (a previous
   task's leftover library was first in the list).  This made the
   "5. Hornady Shipping Demo" entry visible.
2. **Second change** — added `AIPUI53001` to `INLLIBL` because the PUI
   HANDLER reference in `PICKBATR` / `PICKERR` couldn't resolve
   `GENIE.SRVPGM`.

You reverted both — the JOBD is now back to its base state
`(AIDEMOBASE QGPL QTEMP DRPUIDEV)`.  This guide has been corrected so the
JOBD-edit recipe is no longer documented as the right approach; see §5.4
for the session-scoped alternatives.

### 5.5 hook — PUI's `PUI_AGENTIC_TASK_LIB` environment variable

The PUI auth POST passes `PUI_AGENTIC_TASK_LIB=${IBMI_BUILD_LIBRARY}` (see
`/home/coder/.claude/skills/ibmi-interactive-session/genie_start.sh`).
On this system, `AIDEMO`'s initial program is `PROFOUNDUI/PUISETENV` —
that program is the right place for any task-library-list manipulation,
because it runs per-session against the env vars the PUI auth call
populated.  If `PUISETENV` doesn't currently ADDLIBLE the
`PUI_AGENTIC_TASK_LIB` value, that's the cleanest fix for the
"task library missing from `*LIBL`" symptom — and it's local to each
session, not a global JOBD edit.

---

## 17. PICKBATR blank-screen debug log (2026-06)

For posterity, this is everything that was tried to make option 4 render,
and what each attempt produced:

1. **Initial `CPF27AF "Edit mask not valid"`** at the first `WRITE Ctl`.
   * Diagnosis: zoned-decimal hidden fields in `Ctl_Fields_Out` not
     zero-initialized.
   * Fix: `INZ` on `Dcl-DS Ctl_Fields_Out LikeRec(DisplyFile.Ctl:*OUTPUT)`
     and the other six `LikeRec(...)` data structures in `pickbatr.sqlrpgle`.
   * Result: ✅ `CPF27AF` gone, program reaches EXFMT.  PUI HANDLER call
     returns `handler[0].name = "CTL"` with 17 populated fields.  Browser
     paints blank.
2. **CANVAS-dimension PUI directive added** (`*PUI CANVASHEIGHT(800)`,
   `*PUI CANVASWIDTH(1400)` at the top of `pickbatd.dspf` — PICKERD has
   these, PICKBATD didn't).  Rebuilt the DSPF.
   * Result: ❌ no change in the genie response or the browser render.
     Patch reverted.
3. **CSS class strings rewritten in the DSPF JSON.**  The package source's
   `hornady-grid`, `hornady-btn`, `hornady-input`, `hornady-constant`,
   `hornady-solid-button-*`, `hornady-dark-header`, `hornady-white-body`,
   `blueprint-panel`, `blueprint-defaults`, `blueprint-alt-defaults`,
   `blueprint-no-label`, `blueprint-select-box`,
   `blueprint-wrapping-text`, `office-copy-checkbox` aren't defined
   anywhere in `/www/profoundui/htdocs/profoundui/userdata/css/` or
   `/proddata/css/`.  Replaced each with the closest PUI standard class
   (`pui-grid`, `pui-button`, `pui-input`) or with spaces (length-preserving
   so the DDS column-80 boundaries stay intact).
   * Result: ❌ no change in the genie response or the browser render.
     Patch left in place — it doesn't hurt, and the unmodified references
     to undefined classes wouldn't have rendered anyway.
4. **Rich Display File JSON deployment.**  Extracted the embedded screen
   JSON from `pickbatd.dspf`'s `HTML('…')` keywords, reassembled into a
   single JSON file with 8 `formats` entries (CTL/DETAIL/DELETE/MESSAGE/
   BATCH_MSG/ASSIGN_BAT/COMP_BATCH/SHIP_VIA), and wrote it as
   `/www/profoundui/htdocs/profoundui/userdata/dspf/PICKBATD.json` with
   CCSID 1208.
   * Result: ❌ no change.
5. **Direct `CTL.BTNEXIT=1` indicator** sent to the running EXFMT to see
   whether the program responds to user input.
   * Result: ❌ no response; the program stays in EXFMT, indicating the
     PUI client never delivered an aid+indicator round-trip back to the
     program.

What's known about the EXFMT state at this point:

* The job's `FUNCTION = 'PICKBATR'`, CPU under 30 ms, no joblog errors.
* The PUI auth call response (the same data the browser receives over
  XHR) contains the correct `handler[]` block — same shape as PICKERR's
  working `LOGINR`.
* The 5250 `buffer` is all blanks and the `5250.layers[0].fields` array
  is empty — that's correct for a rich PUI EXFMT (the screen is supposed
  to be rendered client-side from the handler payload + DSPF metadata).

What's NOT known (and what would unblock further debug):

* What the browser's JS console shows when option 4 loads (any red
  errors?  PUI-runtime warnings?).
* What the browser's Network tab shows (any 404 / 500 responses from
  `/profoundui/api/*`, `/profoundui/proddata/js/*`,
  `/profoundui/userdata/dspf/*`?).
* Whether other Hornady DSPFs that use a grid widget render in this
  environment (PICKERD's `HOMER` format would be the closest comparison
  — same grid pattern, same prefix — but it can only be reached by
  logging in past PICKERR's `LOGINR`, which I haven't done).

Once that browser-side telemetry is in hand the next move is probably one
of: (a) trim widgets from `pickbatd.dspf`'s CTL JSON to identify a
specific failing widget, (b) drop the `expand to layout`, `xlsx export`,
`filter option`, `allow drag` properties that may not be supported in
this PUI version, (c) confirm that PUI's runtime version actually supports
the in-DSPF HTML-keyword metadata path on this install.
