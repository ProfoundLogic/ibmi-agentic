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
| **1** | `HYR0600` Shipment Processing | 5250 subfile of open shipments, 14/page | ✅ **Working** — 30 rows paginate cleanly across 3 pages |
| 2 | `HYR0138` Pallet Contents Maint. | Pallet content editor | 🚫 Blocked — `HYD0138.DSPF` not in HornadyDemo package |
| **3** | `HYR0606` Shipment Lot Inquiry | Read-only subfile of lots for one detail line | ✅ **Working** — 15 lots paginate across 3 pages |
| 4 | `PICKBATR` Pick Batch Dashboard | Profound UI | Stub (DSPF in package; not yet promoted) |
| 5 | `PICKERR` Picker Workflow | Profound UI mobile | Stub (DSPF in package; not yet promoted) |
| 6 | `HYR6080` Order Status Email | Batch | Stub (no DSPF needed; not yet promoted) |

Options 1 and 3 are the demo path. Options 4–6 are next-step candidates. Option 2
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

### 6.11 Menu stubs (options 2, 4, 5, 6)

```makefile
hyr0138.pgm: hyr0138.rpgle hyrstubd.file
pickbatr.pgm: pickbatr.rpgle hyrstubd.file
pickerr.pgm:  pickerr.rpgle  hyrstubd.file
hyr6080.pgm:  hyr6080.rpgle  hyrstubd.file
```

These all bind against the single shared display file `hyrstubd.file`. The
RPG stubs are 12 lines each — they set 4 short text strings and `EXFMT stub`.

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

### 11.5 Other options

- Options 2, 4, 5, 6 all land on the shared stub display file (`hyrstubd`).
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
- **Compiling the real `PICKBATR` / `PICKERR`** — these are Profound UI
  programs. Source and JSON-based DSPF metadata are in the HornadyDemo
  package; promoting them is a separate task (different toolchain — needs
  Profound UI deployment of the JSON files into the userdata workspace).
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
