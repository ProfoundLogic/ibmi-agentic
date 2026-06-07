# WTAGTCFG Demo Shell — Rebuild Guide

A step-by-step recipe for recreating this project from scratch, plus
the full set of gotchas I hit so you don't have to find them again.

> Companion to `Sapiens-Analysis.md` (the narrative log). This file
> is the procedural how-to. Read both; this one is the one you
> execute against.

---

## 0. What this project is

A **reverse-engineered demo shell** built around the Sapiens Agency
module source drop (`agencysrc.zip` + `Agency Info updated.docx`).

- Goal: get `WTAGTCFG` (Agency Configuration) compiling, binding, and
  rendering through Profound UI with sample data — *without* the real
  Sapiens binding directories, runtime libraries, EXTPGMs, or
  database DDS.
- Mechanism: invent everything Sapiens didn't ship — placeholder PFs,
  stub service program, stub EXTPGMs, fake action panel — just deep
  enough that the build chain succeeds and the UI renders.
- Status when this guide was last touched:
  - Menu option 4 launches `WTAGTCFG`.
  - The program binds and runs.
  - Profound UI renders `rcdAgtCfg` with 35 sample agencies.
  - **Open issue:** no on-screen Exit. The JS shim attempt did not
    take in the user's browser (see §11.5).

---

## 1. Working directory and conventions

| Thing | Where |
|---|---|
| Linux dev sandbox | `/workspace/ibmi-agentic/` |
| Source root | `/workspace/ibmi-agentic/src/` |
| Sapiens drop + reverse-engineered files | `/workspace/ibmi-agentic/src/Sapiens/` |
| This documentation | `/workspace/ibmi-agentic/src/Sapiens/documentation/` |
| SQL seed scripts | `/workspace/ibmi-agentic/src/Sapiens/sqlScripts/` |
| IBM i target library | `AITSK00030` (env var `IBMI_BUILD_LIBRARY`) |
| Dev IBM i host | `idev.profoundlogic.com` (via SSH alias `dev`) |
| Build tool | `codermake` (Profound Logic) |
| Build user | `aidemo` |

All file paths in this guide are *Linux* paths unless prefixed with `/qsys.lib/` or `/www/`.

---

## 2. Prerequisites

On the Linux side:

- `codermake` CLI installed and configured (env vars `IBMI_HOST`, `IBMI_USER`, `IBMI_KEY`, `IBMI_BUILD_LIBRARY`).
- SSH alias `dev` configured for the IBM i.
- `python3` (used by the SQL seed generator and a few helper scripts).

On the IBM i:

- Profound UI installed and running (the user's product — this is given since the host is `*.profoundlogic.com`).
- Library `AITSK00030` exists and `aidemo` has authority.
- IFS access for the build user.

**Permissions reality check** (important — bit me twice):

| Path | Owner | aidemo can write? |
|---|---|---|
| `/www/profoundui/htdocs/profoundui/userdata/custom/css/` | qpgmr | **NO** |
| `/www/profoundui/htdocs/profoundui/userdata/custom/js/` | qpgmr | **NO** |
| `/www/profoundui/htdocs/profoundui/userdata/css/` | mixed | YES |
| `/www/profoundui/htdocs/profoundui/userdata/html/` | mixed | YES |
| `/www/profoundui/htdocs/profoundui/userdata/genie skins/*.css` | qpgmr | YES (world-writable) |

So custom CSS/JS gets deployed under `userdata/css/` and `userdata/html/` — not the standard custom path.

---

## 3. The source drop

### 3.1 What Sapiens ships

`agencysrc.zip` contains **57 members** plus the Word doc (`Agency Info updated.docx`). Categories:

| Group | Count | Examples |
|---|---:|---|
| Interactive 5250 driver `.rpgle` programs | 8 | `wtagtcfg`, `wtagtasn`, `wtagtprod`, `wtagtcon`, `wtagtsrvrt`, `wtagtrtfnd`, `wtvsttrk`, `witmplt` |
| `.dds` for those programs | 9 | One per driver + `wtagtpri.dds` (DDS-only, no RPGLE) |
| `SPR*` / `SCOPY*` prototype copybooks | ~18 | `SPRDATE`, `SPRstring`, `SPRsql`, `SPRtable`, `SPRtrigger`, `SCOPYCOMPI`, `SCOPYCOMPR`, `SCOPYGRPPR`, `SPRATRIUM`, `SPRCOMPANY`, `SPRGENERR`, `SPRGENPOP`, `SPRusrprms`, `SPRmessage`, `SPRmodule`, `SPRmask`, `SPRruncmd`, `SPRrtvbrnd`, `SPRteam`, `SPRtrig` |
| `wPr*` prototype copybooks | ~8 | `wpragent`, `wpremplr`, `wprclaims`, `wprgroup`, `wprpolerr`, `wprtable`, `wprusrprms`, `wpreft` |
| Visit-tracking / template data DS | ~4 | `wtsearchds`, `wtvsttrksd`, `wtvsttrksp`, `witmpltd` |
| Pre-imported DDS member files | 11 | `.MBR` files (data, can be ignored) |

All copybooks are **prototype-only** — they declare `PR`s but contain no implementation.

### 3.2 What Sapiens does NOT ship (and you'll need to invent or get)

| Category | Count | What |
|---|---:|---|
| Database PFs | 6 | `WMAGP`, `SMCOP`, `WMAAP`, `WDF2P`, `WMCZP`, `WDELP` — the tables WTAGTCFG actually opens |
| Database LFs | 6 | `WMAGL`, `WMAGL1`, `SMCOL`, `WMAAL`, `WDF2L`, `WMCZL` |
| Trigger-DS placeholder PFs | 13 | `SZQ1P`, `WMAHP`, `WMCMP`, `WMCDP`, `WDPAP`, `WAMCP`, `WDEHP`, `WMSHP`, `WXELP`, `WMEMP`, `SDCMP`, `WMEAP`, `WIBIFP` — referenced via `extName(...)` from `SPRtrigger.rpgle`. **Compile-time DS references only — no actual data ever read** |
| Trigger-DS file with underscore in name | 1 | `WDEAP_HLD` — must be created via SQL `CREATE TABLE`, DDS `CRTPF` can't create names with underscores |
| Shared action panel display files | 2 | `STACTPNL` (used by WTAGTCFG, WITMPLT), `WTACTPNL` (used by other drivers) |
| External programs (`EXTPGM`) | 5 | `RTVCLTID`, `SRCHKPGM`, `STRTVDTA`, `STWRTDTA`, `W4020R` — called dynamically from WTAGTCFG |
| Other EXTPGMs (for other drivers when you add them) | ~11 | `WA002R`, `WTSEARCH`, `TIMEDELAY`, `WTPOPUP`, `SRCCSC`, `SRNAME`, `STFILETRG`, `RTVBRND`, `W0040R`, `QCMDEXC`, `WTAGTFEIN` |
| Binding directories | 2 | `STBNDDIR`, `WTGRPCFG` — hold the compiled implementations of every `sPr*/wPr*/sCopy*` prototype |
| Profound UI theme | — | Whatever CSS Sapiens uses in production for these screens |

### 3.3 The Word doc is a deployment runbook

`Agency Info updated.docx` gives, per driver:
- The STLAUNCH URL pattern.
- The library each DDS originated from (`V26PNSRC/QDDSSRC` or `V6R0QA/QDDSSRC`).
- The full F-spec database file list per tab.
- The full `/COPY` list per program.

Read it before doing anything else.

---

## 4. Phased rebuild

### Phase A: Stage the drop

1. Create `src/Sapiens/` under your `ibmi-agentic` repo.
2. Unzip `agencysrc.zip` into it.
3. Copy `Agency Info updated.docx` alongside.
4. Sanitize the copybooks (see §6.1) — stray `0x82` bytes in column-1 positions break the IBM i RPG compiler partway through `/COPY` expansion. Replace any byte `>= 0x80` with ASCII space. Confirmed-affected files at our drop time:
   `SCOPYCOMPI, SCOPYCOMPR, SCOPYGRPPR, SPRATRIUM (no), SPRCOMPANY, SPRGENERR, SPRGENPOP, SPRmodule, SPRtable, wpragent, wprclaims, wpreft, wpremplr, wprgroup, wprpolerr, wprtable, wtagtasn, wtagtcon, wtagtprod, wtagtrtfnd, wtagtsrvrt, wtvsttrksd, wtvsttrksp`. Other files may be OK as-shipped; check with:
   ```bash
   python3 -c "
   import glob
   for f in sorted(glob.glob('*.rpgle')):
       d = open(f,'rb').read()
       n = sum(1 for b in d if b >= 0x80)
       if n: print(f, n)
   "
   ```
5. **Two file renames** — these matter:
   - `wtagtcfg.dds` → `wtagtcfg.dspf` (codermake recognizes display files by `.dspf`, not `.dds`).
   - `wtagtcfg.rpgle` → `wtagtcfg.sqlrpgle` (the program embeds `EXEC SQL`; needs `CRTSQLRPGI`, not `CRTBNDRPG`).
6. Same renames apply to the other 7 driver `.dds`/`.rpgle` files when you onboard them.

### Phase B: Pre-flight infrastructure on IBM i

1. Confirm `AITSK00030` exists. `codermake` creates it on first build if missing.
2. Create source physical file for `/COPY`-resolution fallback:
   ```cl
   CRTSRCPF FILE(AITSK00030/QRPGLESRC) RCDLEN(112) MBR(*NONE)
            TEXT('Sapiens copybooks')
   ```
3. Symlink at project root:
   ```bash
   cd /workspace/ibmi-agentic
   ln -s src/Sapiens qrpglesrc
   ```
   This is required by **codermake's local preprocessor preflight** — without it the build won't even start. Doesn't have to go anywhere useful; the preprocessor only needs to find files by name.

### Phase C: Load copybooks into AITSK00030/QRPGLESRC

Copybooks need to live in QRPGLESRC on the IBM i so that the `/COPY` directives resolve at compile time. Also list them as **explicit prerequisites** of the program target in `Rules.mk` — codermake's preprocessor rewrites `/COPY sprUsrPrms` to a local IFS path but **only uploads files listed in the recipe**, so they MUST be prerequisites.

```bash
cd /workspace/ibmi-agentic/src/Sapiens
# Tar everything except the 8 driver programs
tar cf /tmp/copybks.tar $(ls *.rpgle | grep -viE \
  '^(wtagtcfg|wtagtasn|wtagtprod|wtagtcon|wtagtsrvrt|wtagtrtfnd|wtvsttrk|witmplt)\.rpgle$')
scp /tmp/copybks.tar dev:/tmp/copybks.tar
ssh dev "rm -rf /tmp/sapcopy && mkdir /tmp/sapcopy && cd /tmp/sapcopy && tar xf /tmp/copybks.tar
for f in *.rpgle; do
  mbr=\$(echo \${f%.rpgle} | tr '[:lower:]' '[:upper:]')
  /usr/bin/system \"CPYFRMSTMF FROMSTMF('/tmp/sapcopy/\$f') \
     TOMBR('/QSYS.LIB/AITSK00030.LIB/QRPGLESRC.FILE/\$mbr.MBR') \
     MBROPT(*REPLACE) STMFCCSID(819) DBFCCSID(37) ENDLINFMT(*ALL)\" >/dev/null
done"
```

The `STMFCCSID(819) DBFCCSID(37)` is critical: source members in `QRPGLESRC` are CCSID 37 (US English EBCDIC). ASCII→EBCDIC conversion happens at copy time.

### Phase D: Reverse-engineer the database DDS

Six real PFs + their LFs, plus 13 trigger-DS placeholder PFs, plus a SQL CREATE TABLE for `WDEAP_HLD`.

The PF field set comes from scanning `wtagtcfg.sqlrpgle` for field references by prefix (`AG*`, `AA*`, `CO*`, etc.). Field **types** come from how the RPG uses them — `%EditC(field:'X')` means numeric, `Chain (k1:k2) file` means the key types match, `field = '1'` means char, etc. Watch for:

- Suffix `#` (e.g. `AGCO#`, `AGAGT#`) — usually packed numeric (3-9 digits).
- Suffix `DT`, `TIME`, `DD01..DD06` — usually `7P 0` (CYYMMDD).
- Phone fields (`AGPHN1`, `AGPHN2`, `AGFAX#`) — `7P 0` numeric (7-digit prefix+line).
- Zip (`AGMZIP`, `AGPZIP`) — **numeric** `9P 0` (the program divides by 10000 to get zip5).
- State (`AGMST`, `AGPST`) — `2A`.
- Status flags (`AGSTAT`, `AGACT`, `AG1099`, etc.) — `1A`.

Sample WMAGP definition shape:

```dds
     A          R PAG01
     A            AGCO#          3P 0       TEXT('Company Number')
     A            AGFEIN         9P 0       TEXT('Federal EIN')
     A            AGMOD          3P 0       TEXT('Modifier')
     A            AGNAME        40A         TEXT('Agency Name')
     A            AGAGT#         5P 0       TEXT('Agent Number')
     ...  about 80 fields total ...
```

The full set lives in `src/Sapiens/wmagp.pf`. Use that as your starting point.

LFs are trivial — they only declare keys (from the Word doc's "co#/fein/mod" notation):

```dds
     A          R PAG01                     PFILE(WMAGP)
     A          K AGCO#
     A          K AGFEIN
     A          K AGMOD
```

For trigger-DS placeholders (the 13 names in `SPRtrigger`'s `extName(...)`), a one-field PF satisfies the compiler. Generate them programmatically — see `/tmp/build_stactpnl.py` or the placeholder PFs in `src/Sapiens/`.

`WDEAP_HLD` has an underscore in the name → DDS `CRTPF` can't create that. Use SQL via `.table.sql`:

```sql
-- src/Sapiens/wdeap_hld.table.sql
CREATE TABLE WDEAP_HLD (
  EHCO#   DECIMAL(3,0)  NOT NULL DEFAULT 0,
  EHDUMMY CHAR(10)      NOT NULL DEFAULT ''
);
```

### Phase E: Reverse-engineer STACTPNL

`STACTPNL` is the shared action panel DSPF. It needs to expose:

- Format **`RCDSYSTBL`** with fields `Q1USER (10A)`, `Q1PGM (10A)`, `Q1PRMS (3S 0)`, `Q1JOB (10A)`, `Q1JOBNBR (6S 0)`, `SCCOMPNAME (40A)` — all hidden.
- A second format with fields `APNLHEIGHT`, `APFOOTHGHT`, `AMENHEIGHT`, `PNLWIDTH`, `PNLTITLE`. **Name it something other than `RCDPANEL`** — `wtagtcfg.dspf` has its own `RCDPANEL` and a name collision causes RPG to drop the second-format fields entirely. The current code uses `RCDACTPL`.
- **Profound UI markers** on each record format (or PUI raises `PUI0016` on OPEN):

  ```dds
       A          R RCDSYSTBL
       A                                  1  2HTML('QPUIREC1    ROVERLAY   0     -
       A                                      RASSUME    0     ')
       A                                  1  2HTML('{"screen":{"record format nam-
       A                                      e":"rcdSysTbl","overlay":"true","ass-
       A                                      ume":"true"},"items":[]}')
       A            Q1USER        10A  H      TEXT('Active user')
       ...
  ```

Full definition lives in `src/Sapiens/stactpnl.dspf`.

### Phase F: Stub EXTPGMs

WTAGTCFG calls 5 programs dynamically. Each gets a minimal one-file RPGLE stub with matching parm signature and safe-default behavior:

| EXTPGM | Behavior in stub |
|---|---|
| `RTVCLTID(outCltID)` | `outCltID = 'STB'; *INLR = *On;` |
| `SRCHKPGM(qqPgmName, qqFound)` | `qqFound = '0';` |
| `STRTVDTA(co#, type, key, n1, out1, n2, out2)` | `outData1 = ' '; outData2 = ' ';` |
| `STWRTDTA(...)` | no-op |
| `W4020R(@@kl, @@lc, @@mode, @@rrn2)` | no-op (don't modify parms) |

Get the exact parm types from `wtagtcfg.sqlrpgle`'s `extpgm(...)` prototypes.

### Phase G: Stub the binding directory

The `/COPY` files declare ~80 PR prototypes across all the `SPR*`/`wPr*`/`SCOPY*` members. WTAGTCFG only *calls* **13** of them. Identify by scanning `wtagtcfg.sqlrpgle` + every `/COPY`'d copybook for `name(` patterns where `name` matches a known prototype:

```python
# Python heuristic — see prior task output for the full script
# Result for WTAGTCFG:
# CHECK_EMAIL, CREATEGENURL, CREATEURL, DATETO7, DTCYMD, GETCOMPNAM,
# GETUSERINFO, ISVALIDFETCH, LOADACTIONBAR, SYSTEMDATE, SYSTEMTIME,
# TIMETO6, VALIDDATE
```

Build a NOMAIN module `sapstubs.rpgle` that:

- `/COPY`s the needed copybooks (`SPRGENERR`, `SPRATRIUM`, `SPRDATE`, `SPRCOMPANY`, `SPRusrprms`, `SPRsql`, `SCOPYCOMPR`, `wprpolerr`) so it gets the PR declarations.
- Implements each as a `B/PI/E` block with `Export`, returning a safe default. A few have trivial real implementations worth doing (date conversions: `DateTo7`, `dtCYMD`, `TimeTo6`).

Build it as a service program with `EXPORT(*ALL)`:

```makefile
sapstubs.module: sapstubs.rpgle \
  SPRGENERR.rpgle SPRATRIUM.rpgle SPRDATE.rpgle SPRCOMPANY.rpgle \
  SPRusrprms.rpgle SPRsql.rpgle SCOPYCOMPR.rpgle wprpolerr.rpgle
sapstubs.srvpgm: sapstubs.module
```

(`.bnd` files don't ship cleanly through codermake — use `EXPORT(*ALL)`.)

Then create the binding directories. Both `STBNDDIR` and `WTGRPCFG` point at the same `sapstubs` srvpgm:

```cl
# src/Sapiens/stbnddir.bnddir
CRTBNDDIR BNDDIR($LIBRARY/$NAME) TEXT('Sapiens stub binding dir')
ADDBNDDIRE BNDDIR($LIBRARY/$NAME) OBJ((SAPSTUBS *SRVPGM))
```

(Same for `wtgrpcfg.bnddir`.)

### Phase H: The Rules.mk

Single recipe driving everything. Layout:

```makefile
# --- Real-schema PFs ---
wmagp.file: wmagp.pf
... 5 more PFs ...

# --- LFs ---
wmagl.file: wmagl.lf | wmagp.file
... 5 more LFs ...

# --- Trigger-DS placeholder PFs ---
szq1p.file: szq1p.pf
... 12 more, plus wdeap_hld.table.sql ...

# --- Display files ---
stactpnl.file: stactpnl.dspf
wtagtcfg.file: wtagtcfg.dspf | wdelp.file

# --- Stub service program & binding dirs ---
sapstubs.module: sapstubs.rpgle <8 copybook deps>
sapstubs.srvpgm: sapstubs.module
stbnddir.bnddir: stbnddir.bnddir | sapstubs.srvpgm
wtgrpcfg.bnddir: wtgrpcfg.bnddir | sapstubs.srvpgm

# --- EXTPGM stubs ---
w4020r.pgm:   w4020r.rpgle
srchkpgm.pgm: srchkpgm.rpgle
rtvcltid.pgm: rtvcltid.rpgle
strtvdta.pgm: strtvdta.rpgle
stwrtdta.pgm: stwrtdta.rpgle

# --- The program itself ---
wtagtcfg.pgm: wtagtcfg.sqlrpgle \
  <18 copybooks WTAGTCFG /COPYs> \
  wtagtcfg.file stactpnl.file \
  | stbnddir.bnddir wtgrpcfg.bnddir \
    <all 12 PF/LF files> \
    <all 14 trigger placeholders incl wdeap_hld> \
    w4020r.pgm srchkpgm.pgm rtvcltid.pgm strtvdta.pgm stwrtdta.pgm
```

Important rules of thumb:
- Copybook prereqs MUST use the actual on-disk filename case (`SPRusrprms.rpgle` not `sprUsrPrms.rpgle`) — Linux is case-sensitive even though the IBM i side isn't.
- PF/LFs as order-only prereqs (`|`) — they only need to exist, not trigger rebuild.

### Phase I: Build

```bash
cd /workspace/ibmi-agentic
codermake wtagtcfg.pgm
```

Build order produced by codermake: PFs → LFs → trigger PFs → DSPFs → sapstubs module → sapstubs srvpgm → binding dirs → 5 EXTPGM stubs → `CRTSQLRPGI` for the program → `CRTPGM` link.

Final success line you're looking for:

```
Program WTAGTCFG placed in library AITSK00030. 00 highest severity.
```

### Phase J: Wire the menu

`src/menu.dspf`:
```dds
     A                                  8  7'4. Agency Configuration'
```

`src/menu.msgf`:
```
addmsgd msgid(usr0004) msgf($LIBRARY/$NAME)
        msg('call wtagtcfg parm('' '' ''C'' '' '' ''     '')')
        seclvl(*none) sev(00) fmt(*none)
```

The CALL parms (`@@rrn=' '`, `@@mode='C'`, `@@next=' '`, `@@TabInd='     '`) — see §10 for mode reference. Then:

```bash
codermake menu.menu
```

### Phase K: Sample data

```bash
ssh dev "mkdir -p /tmp/seed"
scp src/Sapiens/sqlScripts/*.sql src/Sapiens/sqlScripts/runall.sh dev:/tmp/seed/
ssh dev "chmod +x /tmp/seed/runall.sh && /tmp/seed/runall.sh"
```

The shell orchestrator clears each PF and runs each loader in dependency order. End state: 35 rows in every reverse-engineered PF. To regenerate the SQL with different data (different names, more rows, etc.):

```bash
python3 src/Sapiens/sqlScripts/gen_sample_data.py
```

(deterministic — `random.seed(42)` — so re-runs produce identical diffs.)

### Phase L: Profound UI deploy (CSS + JS shim)

```bash
scp src/Sapiens/wtagtcfg-fix.css \
    dev:/www/profoundui/htdocs/profoundui/userdata/css/wtagtcfg-fix.css
scp src/Sapiens/wtagtcfg-shim.js \
    dev:/www/profoundui/htdocs/profoundui/userdata/html/wtagtcfg-shim.js

# Patch start.html (back it up first)
ssh dev "cp /www/profoundui/htdocs/profoundui/userdata/html/start.html \
        /www/profoundui/htdocs/profoundui/userdata/html/start.html.bak"
ssh dev "cat /www/profoundui/htdocs/profoundui/userdata/html/start.html" \
    > /tmp/s.html
python3 -c "
s = open('/tmp/s.html').read()
inj = ('  <link href=\"/profoundui/userdata/css/wtagtcfg-fix.css\" '
       'rel=\"stylesheet\" type=\"text/css\">\n'
       '  <script src=\"/profoundui/userdata/html/wtagtcfg-shim.js\" defer></script>\n')
if 'wtagtcfg-fix.css' not in s:
    s = s.replace('</head>', inj + '</head>')
    open('/tmp/s.html','w').write(s)
"
scp /tmp/s.html dev:/www/profoundui/htdocs/profoundui/userdata/html/start.html
```

### Phase M: Verify

1. From a 5250 session against `AITSK00030`, type the menu name and pick option 4 → expect `PUI0016` (PUI handler can't open in a green-screen session). That's success — proves the program loaded and bound.
2. From a browser, hit `/profoundui/auth/start?pgm=AITSK00030/WTAGTCFG&p1= &p2= &p3=GetAgency&p4=` (or use the menu through a PUI Genie session) and confirm the `rcdAgtCfg` format renders with 35 agency rows.

---

## 5. File map

| Source path | Builds to | Purpose |
|---|---|---|
| `src/Sapiens/wtagtcfg.dspf` (renamed from `.dds`) | `WTAGTCFG *FILE` | Main display file |
| `src/Sapiens/wtagtcfg.sqlrpgle` (renamed from `.rpgle`) | `WTAGTCFG *PGM` | The program |
| `src/Sapiens/stactpnl.dspf` | `STACTPNL *FILE` | Reverse-engineered action panel |
| `src/Sapiens/wmagp.pf` ... `wdelp.pf` | 6 master PFs | Reverse-engineered schema |
| `src/Sapiens/wmagl.lf` ... `wmczl.lf` | 6 LFs | Keyed views |
| `src/Sapiens/szq1p.pf` ... `wibifp.pf` | 13 placeholder PFs | Trigger-DS targets |
| `src/Sapiens/wdeap_hld.table.sql` | `WDEAP_HLD *FILE` | Underscore-named via SQL |
| `src/Sapiens/sapstubs.rpgle` | `SAPSTUBS *SRVPGM` | 13-procedure stub |
| `src/Sapiens/stbnddir.bnddir`, `wtgrpcfg.bnddir` | 2 `*BNDDIR` | Wraps the srvpgm |
| `src/Sapiens/w4020r.rpgle` ... `stwrtdta.rpgle` | 5 EXTPGM stubs | Dynamic-call targets |
| `src/Sapiens/Rules.mk` | (build recipe) | codermake driver |
| `src/Sapiens/sqlScripts/*.sql` | (sample data) | `RUNSQLSTM` against each |
| `src/Sapiens/sqlScripts/runall.sh` | (PASE orchestrator) | Clear + reload |
| `src/Sapiens/sqlScripts/gen_sample_data.py` | (generator) | Re-emit SQL |
| `src/Sapiens/wtagtcfg-fix.css` | `/profoundui/userdata/css/` | PUI layout patches |
| `src/Sapiens/wtagtcfg-shim.js` | `/profoundui/userdata/html/` | Exit-button shim |
| `src/menu.dspf`, `src/menu.msgf` | demo menu + msgf | Adds option 4 |
| `qrpglesrc` symlink at project root | (preflight) | codermake's local `/COPY` resolver |

Also deployed on the dev IBM i (not in git):

| Path | Why |
|---|---|
| `AITSK00030/QRPGLESRC` | Source PF holding 32 copybook members |
| `/www/profoundui/htdocs/profoundui/userdata/css/wtagtcfg-fix.css` | Web-served CSS |
| `/www/profoundui/htdocs/profoundui/userdata/html/wtagtcfg-shim.js` | Web-served JS |
| `/www/profoundui/htdocs/profoundui/userdata/html/start.html` | Patched with `<link>` + `<script>` |
| `/www/profoundui/htdocs/profoundui/userdata/html/start.html.bak` | Original — keep for revert |

---

## 6. Gotchas — every one cost me at least an hour

### 6.1 Stray `0x82` bytes in copybook source

22 of the 38 copybooks shipped contained `0x82` bytes (EBCDIC→ASCII transfer artifact) in column-1 positions. The IBM i RPG compiler choked on `/COPY` expansion partway through and silently truncated the included content. Symptom: `ValidDate`, `getCompNam`, etc. reported "undefined" even though grep found the prototype in the copybook.

Fix: replace every byte `>= 0x80` with `0x20` (space). Doesn't touch semantic content — they're all in leading whitespace.

### 6.2 codermake doesn't know `.dds`

codermake's recipe table maps display files from `.dspf`. Sapiens ships `.dds` files. Rename them. Same applies to the remaining 7 driver DSPFs when you onboard them.

### 6.3 `CRTBNDRPG` vs `CRTSQLRPGI`

`wtagtcfg` embeds `EXEC SQL` — `CRTBNDRPG` errors on `EXEC` as an unknown opcode. Rename to `.sqlrpgle` so codermake uses `CRTSQLRPGI`.

### 6.4 `/COPY` resolution needs three things

1. Local file present (so the codermake preprocessor preflight succeeds).
2. Project-root `qrpglesrc` symlink → `src/Sapiens` (so the preprocessor finds `qrpglesrc/<member>`).
3. Each `/COPY` target listed as an explicit prerequisite of the program target (so codermake actually uploads it to IBM i alongside the source).

Plus loading them into `AITSK00030/QRPGLESRC` as belt-and-suspenders.

### 6.5 Linux case-sensitivity in Rules.mk

Prereq paths must match the actual on-disk filename case (`SPRusrprms.rpgle` not `sprUsrPrms.rpgle`). The `/COPY` resolver inside the program can be case-insensitive on IBM i, but the Linux `make` step is case-sensitive when resolving file dependencies.

### 6.6 Record-format name collisions

`STACTPNL`'s second format was originally named `RCDPANEL` — exactly matches `wtagtcfg.dspf`'s `RCDPANEL`. RPG silently dropped one of them and the included fields disappeared. Rename to `RCDACTPL` (or anything unique).

### 6.7 PUI `PUI0016` on OPEN

The Profound UI handler refuses to OPEN a display file that lacks PUI metadata. Each record format needs both:
```
HTML('QPUIREC<n>    ROVERLAY   0     RASSUME    0     ')
HTML('{"screen":{"record format name":"<name>","overlay":"true","assume":"true"},"items":[]}')
```

### 6.8 SQL `CREATE TABLE` for names with underscores

`WDEAP_HLD` can't be created via DDS `CRTPF` (CL doesn't accept underscores in object names without quoting tricks). Use a `.table.sql` script — codermake runs it via `RUNSQLSTM`.

### 6.9 `system` PASE wrapper exit code

`/usr/bin/system "<CL command>"` from PASE shell returns exit 0 even when the underlying CL command failed with `CPF`/`CPD`. Wrap with a function that scans output for those message IDs:

```sh
run_cl() {
  out=$(/usr/bin/system "$1" 2>&1); echo "$out"
  if echo "$out" | grep -qE 'CPF[0-9]{4}|CPD[0-9]{4}'; then
    return 1
  fi
}
```

This bit me on `CLRPFM` failing silently and the seed loader doubling rows.

### 6.10 Field-type inference

`AGSECY` in my schema is `1A` (a flag). In real Sapiens it's probably a 40-char "Secretary Name" string. My initial sample data assigned "Lee Admin" to it → `SQL0404 value too long`. **Every reverse-engineered field is a guess.** Don't seed it with anything you wouldn't want truncated.

### 6.11 PF dependent-files lock `DLTF`

To recompile a PF you have to drop its LFs first. Run `DLTF` on the LFs before the PF. Or use codermake's order-only deps to keep them in sync.

### 6.12 Quoted `'GetAgency '` in CL CALL

In `menu.msgf`, the CL `CALL ... PARM(' ' 'C' ' ' '     ')` requires literal spaces. CL string syntax wants `''` (two apostrophes) inside the quoted msg — that's what's in the file. Test by extracting the message from the compiled msgf with `DSPMSGD`.

### 6.13 PUI strips metadata from non-active record formats

I tried injecting CSS via an `html container` item inside `STACTPNL`'s `RCDSYSTBL` metadata. PUI only sends the **active** format's metadata.items to the browser — the inline CSS never reached the page. Workaround was deploying the CSS via `start.html` instead.

### 6.14 PUI drops response-indicator submissions for hidden buttons

The hidden `BtnCancel` button in `wtagtcfg.dspf` (`visibility:"hidden"`) is the only thing bound to the response indicator that `Main()`'s exit loop watches. I tried (a) CSS-overriding the visibility to make it clickable, and (b) submitting `RCDAGTCFG.BTNCANCEL=1` directly via the PUI protocol. **Neither worked.** PUI appears to filter out indicator submissions for items it sees as hidden in screen metadata.

The current shim attempt (`wtagtcfg-shim.js`) is documented as not working in the user's browser. See §11.5.

### 6.15 5250 session locks survive

If you run an interactive `genie_*.sh` test that doesn't end cleanly, the resulting `QPADEV*` job holds shared locks on `menu.msgf` and prevents `codermake menu.menu` from rebuilding. Find with `WRKOBJLCK OBJ(AITSK00030/MENU) OBJTYPE(*MSGF)`, end with `ENDJOB JOB(<num>/AIDEMO/<dev>) OPTION(*IMMED)`.

---

## 7. Menu invocation modes

The 4-parm signature is `(@@rrn 9a, @@mode 1a, @@next 10a, @@TabInd 5a)`. `@@next='GetAgency'` is special — it triggers a one-shot search popup and returns control to the caller. Anything else makes `Main()` run as the interactive grid+detail screen.

| Mode | Menu CALL `parm(...)` | Behavior |
|---|---|---|
| Change/edit (current) | `' ' 'C' ' ' '     '` | Full grid+detail, all fields editable |
| Display/read-only | `' ' 'D' ' ' '     '` | Full grid+detail, all fields read-only |
| Initial/Add | `' ' ' ' ' ' '     '` | Default (Main runs in initial state) |
| Search-select | `' ' ' ' 'GetAgency ' '     '` | Popup, returns to caller with selection |

(`'GetAgency '` has a trailing space to fill the 10-char parm.)

---

## 8. Procedures we stubbed and what they should really do

If you ever replace `sapstubs.rpgle` with real implementations (or bind against the real `STBNDDIR`), the 13 procedures are:

| Procedure | Source copybook | Stub returns | Real behavior |
|---|---|---|---|
| `Check_Email` | `SPRGENERR` | `*off` (always ok) | Validate email format, return `*on` if invalid |
| `CreateGenURL` | `SPRATRIUM` | `' '` | Build an iDARTS portal URL from up to 11 parms |
| `CreateURL` | `SPRATRIUM` | `' '` | Build a Profound UI launcher URL |
| `DateTo7` | `SPRDATE` | `%dec(%char(d:*iso0):7:0)` (real impl) | Convert `D` to CYYMMDD numeric |
| `dtCYMD` | `SPRDATE` | Real impl (YYYYMMDD→CYYMMDD) | Same |
| `getCompNam` | `SPRCOMPANY` | `'STUB Company <n>'` | Chain `SMCOL` by co# and return `COname` |
| `GetUserInfo` | `SPRusrprms` | Returns co#=1, blanks | Look up user profile by user id |
| `IsValidFetch` | `SPRsql` | `inSqlState='00000'` | Check SQL state for "row found" |
| `LoadActionBar` | `SCOPYCOMPR` | no-op | Render function-key labels in action panel |
| `systemDate` | `SPRDATE` | `%date()` (real impl) | Same |
| `systemTime` | `SPRDATE` | `%time()` (real impl) | Same |
| `TimeTo6` | `SPRDATE` | Real impl | Convert `T` to HHMMSS numeric |
| `ValidDate` | `wprpolerr` | `'1'` if non-zero | Validate `8P 0` date is real |

---

## 9. Sapiens artifact ask (production-readiness)

To replace the demo shell with real production behavior, you need from Sapiens:

1. **`STBNDDIR` + `WTGRPCFG` binding directories** (objects preferred; source for the procedures acceptable). Drop them on the LIBL and the stub srvpgm becomes unused. This is the biggest gap.
2. **The 5 EXTPGMs** (`RTVCLTID`, `SRCHKPGM`, `STRTVDTA`, `STWRTDTA`, `W4020R`). For all 8 drivers add ~11 more (`WA002R`, `WTSEARCH`, `TIMEDELAY`, `WTPOPUP`, `SRCCSC`, `SRNAME`, `STFILETRG`, `RTVBRND`, `W0040R`, `QCMDEXC` (system), `WTAGTFEIN`).
3. **Real DDS** for the 6 master PFs (`WMAGP`, `SMCOP`, `WMAAP`, `WDF2P`, `WMCZP`, `WDELP`) so field types/lengths match production. Plus DDS for the 13 trigger-target PFs (only their layout matters since they're only referenced via `extName(...)`).
4. **`STACTPNL` + `WTACTPNL` real DSPFs** with their full action-panel JSON metadata. This gets the F-key labels and the legitimate exit path.
5. **Their production PUI theme / custom CSS** (replaces our `wtagtcfg-fix.css`).

With (1) and (2), the stubs are gone and the program runs real logic. With (3), real data lands correctly. With (4), the user has a clean exit. With (5), the UI matches the real product.

---

## 10. Reference: what we tried that DIDN'T work

| Approach | Why it failed |
|---|---|
| Put CSS in `/profoundui/userdata/custom/css/` (the standard path) | `qpgmr`-owned, `aidemo` can't write |
| Embed CSS via an `html container` item in STACTPNL's `RCDSYSTBL` metadata | PUI strips items from non-active record formats |
| Make hidden `#BtnCancel` button visible via CSS to give the user an Exit click | PUI drops response-indicator submissions for items it sees as `visibility:"hidden"` — clicking does nothing |
| Submit `RCDAGTCFG.BTNCANCEL=1` over the PUI protocol directly | Same as above — PUI doesn't route the indicator to RPG |
| Use `pui.click("BtnCancel")` from a JS shim | Did not take in user's browser (open issue) |
| Inline `<style>` in STACTPNL via multiple `HTML(...)` keywords | PUI parses these as JSON metadata, not raw HTML |
| Use `.bnd` exports file with `EXPORT(*SRCFILE)` for the srvpgm | codermake's MODULE_TO_SRVPGM passes the .bnd path that aidemo's compile job can't open (`CPF5CAC`). Switched to `EXPORT(*ALL)` instead |
| `set -e` in the runall.sh shell orchestrator | `/usr/bin/system` exits 0 on CPF/CPD failures — wrapped with manual scan instead |

---

## 11. Known-open issues

### 11.1 No on-screen Exit (PUI handler drops hidden-button indicators)

In `Main()`'s `DoU BtnCancel = *On` loop, the only DSPF item bound to the `BtnCancel` indicator is hidden by design. The JS shim attempt (`wtagtcfg-shim.js`) was deployed to the dev box and the URL responds HTTP 200, but the user reports the button doesn't appear in their browser. Possible causes I'd investigate next:

- PUI's session re-renders the body after every screen, removing the injected button before `setInterval` re-creates it. (The shim does run `setInterval(makeExitButton, 1000)` but if the user has caching they may see an older version.)
- The user's PUI Genie skin loads a *different* `start.html` than the one we patched. Check which skin the session uses (`config.js` per-skin folder) and patch that skin's `start.html` instead of the userdata default.
- PUI may load `start.html` only at session start; the shim runs but the page might be served from a different entry point for the `auth/start?pgm=` URL.

Workarounds to try next:
- Modify `wtagtcfg.dspf`'s JSON metadata to flip `BtnCancel`'s `visibility` from `"hidden"` to `"visible"` and position it sensibly. This is invasive (editing Sapiens source) but it's a one-property change and PUI WILL register the response indicator if visibility is not "hidden".
- Build a real `LoadActionBar` implementation that actually renders an F3=Exit action panel. Bigger lift but matches the original design.

### 11.2 Hyperlink columns in `sflAgtCfg` (`Edit`, `Change FEIN`, `iDARTS`) overflow on narrow viewports

The CSS file gives the grid `overflow:auto` so the user can scroll, but the columns themselves are sized for full-width. On a smaller viewport they overflow visibly.

### 11.3 Stub procedures + reverse-engineered DDS mean any "Save" is a lie

All the Save/Add buttons appear functional but the validation procedures (`ValidDate`, `Check_Email`) return blanket-OK and the field types in our PFs probably don't match Sapiens production. Don't connect this to anything real.

### 11.4 Site-wide CSS/JS load

Both `wtagtcfg-fix.css` and `wtagtcfg-shim.js` are loaded via `start.html` so every PUI program on the box pulls them in. Selectors are scoped enough that they shouldn't visibly affect unrelated programs, but the cleaner per-screen loading via `userdata/custom/` requires qpgmr write access.

---

## 12. Quick rebuild from a fresh clone

If you have this repo and just need to push it to a fresh `AITSK00030`:

```bash
cd /workspace/ibmi-agentic

# 1. Ensure the symlink exists
[ -L qrpglesrc ] || ln -s src/Sapiens qrpglesrc

# 2. Build (codermake creates the library and all objects)
codermake wtagtcfg.pgm menu.menu

# 3. Load copybooks into AITSK00030/QRPGLESRC
ssh dev "/usr/bin/system 'CRTSRCPF FILE(AITSK00030/QRPGLESRC) RCDLEN(112)
                          MBR(*NONE) TEXT(''Sapiens copybooks'')' 2>&1 | tail -1"
cd src/Sapiens
tar cf /tmp/copybks.tar $(ls *.rpgle | grep -viE \
  '^(wtagtcfg|wtagtasn|wtagtprod|wtagtcon|wtagtsrvrt|wtagtrtfnd|wtvsttrk|witmplt)\.rpgle$')
scp /tmp/copybks.tar dev:/tmp/copybks.tar
ssh dev "rm -rf /tmp/sapcopy && mkdir /tmp/sapcopy && cd /tmp/sapcopy && \
  tar xf /tmp/copybks.tar && \
  for f in *.rpgle; do
    mbr=\$(echo \${f%.rpgle} | tr '[:lower:]' '[:upper:]')
    /usr/bin/system \"CPYFRMSTMF FROMSTMF('/tmp/sapcopy/\$f') \
       TOMBR('/QSYS.LIB/AITSK00030.LIB/QRPGLESRC.FILE/\$mbr.MBR') \
       MBROPT(*REPLACE) STMFCCSID(819) DBFCCSID(37) ENDLINFMT(*ALL)\" >/dev/null
  done"

# 4. Seed sample data
cd ../..
ssh dev "rm -rf /tmp/seed && mkdir /tmp/seed"
scp src/Sapiens/sqlScripts/*.sql src/Sapiens/sqlScripts/runall.sh dev:/tmp/seed/
ssh dev "chmod +x /tmp/seed/runall.sh && /tmp/seed/runall.sh"

# 5. Deploy CSS / JS shim (optional; reverts via start.html.bak)
scp src/Sapiens/wtagtcfg-fix.css \
    dev:/www/profoundui/htdocs/profoundui/userdata/css/wtagtcfg-fix.css
scp src/Sapiens/wtagtcfg-shim.js \
    dev:/www/profoundui/htdocs/profoundui/userdata/html/wtagtcfg-shim.js
ssh dev "test -f /www/profoundui/htdocs/profoundui/userdata/html/start.html.bak || \
        cp /www/profoundui/htdocs/profoundui/userdata/html/start.html \
           /www/profoundui/htdocs/profoundui/userdata/html/start.html.bak"
ssh dev "cat /www/profoundui/htdocs/profoundui/userdata/html/start.html" > /tmp/s.html
python3 -c "
s = open('/tmp/s.html').read()
inj = ('  <link href=\"/profoundui/userdata/css/wtagtcfg-fix.css\" '
       'rel=\"stylesheet\" type=\"text/css\">\n'
       '  <script src=\"/profoundui/userdata/html/wtagtcfg-shim.js\" defer></script>\n')
if 'wtagtcfg-fix.css' not in s:
    s = s.replace('</head>', inj + '</head>')
    open('/tmp/s.html','w').write(s)
"
scp /tmp/s.html dev:/www/profoundui/htdocs/profoundui/userdata/html/start.html
```

Done. Hit `/profoundui/auth/start?pgm=AITSK00030/STLAUNCH&p1=WTAGTCFG&l1=10&p2=LIV&l2=3&p3=(' ' 'C' ' ' '     ')*End&l3=53` in a browser to launch (or use a 5250 menu through PUI Genie).

---

## 13. Where to look when things break

| Symptom | Look here |
|---|---|
| `make: No rule to make target ...` | `codermake --print-rules` → check the dependency closure |
| `RNF0273 /COPY not found` | `AITSK00030/QRPGLESRC` membership + Rules.mk prereqs |
| `RNF2120 file not found` | DDS source file missing from `src/Sapiens/` or not in Rules.mk |
| `RNF3523 external description for DS not found` | Trigger-DS placeholder PF missing |
| `RNF7030 name not defined` | Field missing from one of the reverse-engineered DSPFs (likely STACTPNL) |
| `RNF7416/7421 type mismatch` | Field type in a reverse-engineered PF disagrees with how RPG uses it |
| `RNF7072 KFLD type mismatch` | LF key field type doesn't match the KFLD list |
| `CPD5248 REFFLD file not found` | `WDELP` missing (needed for DSPF REFFLD only) |
| `CPF5CAC stream file could not be opened` | codermake passed a `.bnd` path that the IBM i job can't read — use `EXPORT(*ALL)` instead |
| `CPF5D12 service program preparation` | Bind step failure — check `tmp/logs/<srvpgm>.srvpgm.log` |
| `Bind Diagnostic Messages: Errors were found` | A procedure WTAGTCFG calls isn't in any binding directory — likely a procedure we didn't stub |
| `PUI0016 OPEN failed` | DSPF (typically STACTPNL) lacks PUI metadata markers, OR launched from a green-screen session (use a PUI browser session) |
| `SQL0404 value too long` | Sample data assigns a string longer than the column allows — drop the column from the INSERT or fix the PF type |
| `Resource busy` when rebuilding `menu.msgf` | Stale 5250 session holds the lock — `WRKOBJLCK` + `ENDJOB *IMMED` |

---

## 14. What I'd do differently next time

1. **Read the Word doc fully before anything else.** It's a per-driver runbook with F-spec lists, copybook lists, and the STLAUNCH URL. Most of the dependency excavation I did in the first few hours was already documented there.
2. **Build STACTPNL first**, before attempting any compile. PUI0016 / RNF7030 noise from missing q1user/q1pgm/q1prms/APnlHeight fields is loud and distracting.
3. **Treat `.rpgle` → `.sqlrpgle` rename as a Day-1 step** if the source contains `EXEC SQL` anywhere. (Grep for `EXEC SQL` in the drop first.)
4. **Pre-load copybooks into AITSK00030/QRPGLESRC immediately**, even before adding them to Rules.mk. The `/COPY` source-PF fallback is much easier to debug than codermake's prereq mechanism.
5. **Make a one-shot script for "delete LFs, delete PF, rebuild"** — I dropped/rebuilt WMAGP three times during field-type debugging and the dependent-LF dance got tedious.
6. **For PUI deployment paths, check `userdata/html/` write access first**, before assuming `userdata/custom/` is going to work.
7. **The hidden-button exit thing.** I'd skip the JS shim and just edit `wtagtcfg.dspf`'s JSON to flip `BtnCancel.visibility` from `"hidden"` to `"visible"` on day one. It's a small surgical edit, doesn't require runtime injection, and PUI will actually register the indicator. Spending an afternoon trying every other workaround was a sunk cost.
