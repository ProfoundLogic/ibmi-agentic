# PRFDEMOLIB: Compile and Run Guide

This save file contains three IBM i green-screen programs prepared for a green-screen-to-GUI demo:

| Screen set | Entry point | What it does | Screen size |
|---|---|---|---|
| INV046 | `INV046CL` (CL) | Inventory inquiry by store, district, region and store group | 24x80 |
| INV502 | `INV502` (RPGLE) | Item inquiry: SKU details, then warehouse balances | 24x80 |
| TS0110 | `TS0110` (RPGLE) | Ticket item inventory for one store and department, with F10 to create a store distribution | **27x132** |

Everything is in a single library, **PRFDEMOLIB**: program and screen source, data files with a small sample of real data, and a few support objects. You compile the programs (section 3). The only objects shipped already compiled are the stubs listed in section 2.

---

## 1. Restore

```
CRTSAVF   FILE(QGPL/PRFDEMO3)
/* upload the save file to QGPL/PRFDEMO3 (binary FTP or Navigator) */
RSTLIB    SAVLIB(PRFDEMOLIB) DEV(*SAVF) SAVF(QGPL/PRFDEMO3)
```

Put the library at the top of your library list. **Every compile and every run below assumes this.** The programs find their files, copybooks, binding directory, message file, data area, job description and called programs through `*LIBL`.

```
ADDLIBLE  LIB(PRFDEMOLIB) POSITION(*FIRST)
```

---

## 2. What is in the library

### Source files

| Source file | Record length | Members |
|---|---|---|
| QCLSRC | 92 | INV046CL (CLP), RTC001CL (CLP), RTC002CL (CLP), SYSGET (CLLE stub), CLTS0111 (CLLE stub) |
| QRPGSRC | 92 | INV046 (RPG), TBLF04 (RPG) |
| QRPGLESRC | 112 | INV502, RTC001, GETSPK, TS0110, TRF840A, NXTSEQ, DSPSIZ (all RPGLE) |
| QDDSSRC | 92 | INV046FM, INV502FM, TS0110D, DSPSIZ (DSPF), INVVENJ2 (LF) |
| MMCPY | 92 | PGMSTSDS_D, KBDINFDS_D, MESSAGE_D (RPGLE copybooks) |

QCLSRC also contains **PRFDUP01-03 and PRFLOAD01-03**. These are our internal build utilities and they reference our production libraries. Do not compile or run them.

### Support objects

| Object | Type | Purpose |
|---|---|---|
| SYSGET | *PGM (compiled stub) | Returns the 256-byte system parameter structure (SYS001ED) with fixed values. Replaces a production program that reads control files not included here. |
| CLTS0111 | *PGM (compiled stub) | Run by TS0110's F10 in a submitted job. Production uses it to create transfers in the merchandising system. The stub only reports the distribution member and its record count in the job log. |
| IMSJOB1 | *JOBD | Job description TS0110's F10 submits with. Library list QTEMP PRFDEMOLIB QGPL, job queue QGPL/QBATCH, so the submitted job finds the CLTS0111 stub. |
| MBRSEQNO | *DTAARA (*DEC 7,0) | Sequence number NXTSEQ increments to name each distribution member (T9000001, T9000002, ...). |
| RTCBNDDIR | *BNDDIR | Named in INV502's H-spec. Contains a single entry, `*LIBL/RTC001 *MODULE`. |
| JDAMSGF | *MSGF | Holds the messages INV502 sends: RTC0001, RTC0002, INV0297. |

### Data files (already created, no compile needed)

All physical and logical files are shipped as objects, with no triggers and no constraints.

- **INV046:** INVMST, INVBAL, INVCBL, INVADI, INVUPC, TBLSTR, TBLSTRL1, TBLSTRL2, TBLREG, TBLDST, TBLADI, TBLIGH, TBLIGD, TBL800, POMHDR, POMDTL, POMRCD, OPTIONS, OPTTAB, MENUCTL, JDAREF, SYS001ED
- **INV502 adds:** TBLSTRZ1, INVBALZ1, INVDPT, APSUPP, ITMRTL, TBLSEA, TRFDTL, TRFDTLL2, ORDERBK, ORDERBK1, INVFSD, TBLBYR, TRFZST, TRFZSTL3, PHPF, PHPFW2, PHPFB1, PHPFB2, OPTSTR, DPTDIV, TBLFLD, TBLFLDL1, INVVEN, SKVAUX, INVVENJ2
- **TS0110 adds:** INVMSTZ3, INVBAL1, RPLPRF, STR001WK

Notes:
- **ORDERBK has 8 members.** INV502 reads the OB01 member through ORDERBK1, which is where the sample order-book rows are.
- **STR001WK** arrives with 12 empty members. F10 in TS0110 adds a new member per distribution.
- **Some files are intentionally empty** because they are needed only to compile or to satisfy a lookup: JDAREF, OPTTAB, MENUCTL, INVUPC, TBLIGH, TBLIGD, POMHDR, POMDTL, POMRCD, SYS001ED, TRFZST, DPTDIV, PHPFB1 and PHPFB2.
- **The INVVENJ2 DDS is included for reference.** The join logical file already exists over PRFDEMOLIB/SKVAUX and INVVEN. If you ever recreate it, do so with PRFDEMOLIB first in the library list, because its JFILE names are unqualified.

---

## 3. Compile

Run these in the order shown, with PRFDEMOLIB first in the library list.

### INV046

```
CRTDSPF   FILE(PRFDEMOLIB/INV046FM) SRCFILE(PRFDEMOLIB/QDDSSRC) SRCMBR(INV046FM)
CRTRPGPGM PGM(PRFDEMOLIB/INV046)    SRCFILE(PRFDEMOLIB/QRPGSRC) SRCMBR(INV046)
CRTCLPGM  PGM(PRFDEMOLIB/INV046CL)  SRCFILE(PRFDEMOLIB/QCLSRC)  SRCMBR(INV046CL)
```

### INV502

```
CRTDSPF   FILE(PRFDEMOLIB/INV502FM) SRCFILE(PRFDEMOLIB/QDDSSRC)   SRCMBR(INV502FM)
CRTCLPGM  PGM(PRFDEMOLIB/RTC001CL)  SRCFILE(PRFDEMOLIB/QCLSRC)    SRCMBR(RTC001CL)
CRTCLPGM  PGM(PRFDEMOLIB/RTC002CL)  SRCFILE(PRFDEMOLIB/QCLSRC)    SRCMBR(RTC002CL)
CRTRPGMOD MODULE(PRFDEMOLIB/RTC001) SRCFILE(PRFDEMOLIB/QRPGLESRC) SRCMBR(RTC001)
CRTRPGPGM PGM(PRFDEMOLIB/TBLF04)    SRCFILE(PRFDEMOLIB/QRPGSRC)   SRCMBR(TBLF04)
CRTBNDRPG PGM(PRFDEMOLIB/GETSPK)    SRCFILE(PRFDEMOLIB/QRPGLESRC) SRCMBR(GETSPK) +
          DFTACTGRP(*NO) ACTGRP(*CALLER)
CRTBNDRPG PGM(PRFDEMOLIB/INV502)    SRCFILE(PRFDEMOLIB/QRPGLESRC) SRCMBR(INV502)
```

INV502's H-spec sets `DFTACTGRP(*NO) BNDDIR('RTCBNDDIR')`, so the RTC001 module must exist first.

### TS0110

```
CRTDSPF   FILE(PRFDEMOLIB/TS0110D)  SRCFILE(PRFDEMOLIB/QDDSSRC)   SRCMBR(TS0110D)
CRTDSPF   FILE(PRFDEMOLIB/DSPSIZ)   SRCFILE(PRFDEMOLIB/QDDSSRC)   SRCMBR(DSPSIZ)
CRTBNDRPG PGM(PRFDEMOLIB/DSPSIZ)    SRCFILE(PRFDEMOLIB/QRPGLESRC) SRCMBR(DSPSIZ) +
          DFTACTGRP(*NO) ACTGRP(*CALLER)
CRTBNDRPG PGM(PRFDEMOLIB/TRF840A)   SRCFILE(PRFDEMOLIB/QRPGLESRC) SRCMBR(TRF840A) +
          DFTACTGRP(*NO) ACTGRP(*CALLER)
CRTBNDRPG PGM(PRFDEMOLIB/NXTSEQ)    SRCFILE(PRFDEMOLIB/QRPGLESRC) SRCMBR(NXTSEQ) +
          DFTACTGRP(*NO) ACTGRP(*CALLER)
CRTBNDRPG PGM(PRFDEMOLIB/TS0110)    SRCFILE(PRFDEMOLIB/QRPGLESRC) SRCMBR(TS0110) +
          DFTACTGRP(*NO) ACTGRP(*CALLER)
```

### Stubs (only if you need to recompile them)

```
CRTBNDCL  PGM(PRFDEMOLIB/SYSGET)    SRCFILE(PRFDEMOLIB/QCLSRC) SRCMBR(SYSGET)
CRTBNDCL  PGM(PRFDEMOLIB/CLTS0111)  SRCFILE(PRFDEMOLIB/QCLSRC) SRCMBR(CLTS0111)
```

### Compile notes

- **`/COPY MMCPY,...` (INV502)** is unqualified, so it resolves to PRFDEMOLIB/MMCPY through the library list.
- **`extName(SYS001ED)` (INV502) and the SYS001ED structure (INV046)** resolve to the empty PRFDEMOLIB/SYS001ED file at compile time.
- **INV502 bind errors:** if it fails to bind with unresolved `SNDMSG`/`CLRMSG` or `MSGF`/`MSGPRE`, the RTC001 module wasn't created or PRFDEMOLIB isn't on the library list.
- **TS0110D is a 27x132 display file.** It compiles on any system; it only needs a wide session at run time.

---

## 4. Run

```
ADDLIBLE  LIB(PRFDEMOLIB) POSITION(*FIRST)   /* if not already there */
CALL      PRFDEMOLIB/INV046CL
CALL      PRFDEMOLIB/INV502
CALL      PRFDEMOLIB/TS0110                  /* 27x132 session */
```

### Sample data to try

| Screen | Try |
|---|---|
| INV046 | Store group 740 (10 stores across 5 regions), or any of stores 188, 205, 215, 257, 262, 406, 458, 499, 535, 1838 |
| INV502 | SKU 262657 or 166402 (pack-and-hold quantities), 100001 (allocated quantity), 248117 (Bundle Component set type). Page Up and Page Down step through every SKU in the sample. |
| TS0110 | Store 205 (ships from warehouse 9002) or 406 (ships from 9001), department 12. Toggle "Ticketed only" and "Available only" to see the full department. |

INV502 also accepts an optional SKU parameter (packed 9,0), which skips the prompt screen:

```
CALL      PRFDEMOLIB/INV502 PARM(X'000262657F')
```

TS0110 needs a 27x132 session. On a 24x80 session it shows the DSPSIZ notice and ends.

### TS0110 F10 (Create Distribution)

F10 works end to end in this library:
1. Key a pack quantity on one or more lines, press F10, and confirm.
2. TS0110 gets the next number from MBRSEQNO, adds member T9000001 (and so on) to PRFDEMOLIB/STR001WK, writes the distribution records, and submits CLTS0111 with job description IMSJOB1.
3. The stub logs `Demo CLTS0111: distribution member T9000001 has n record(s)` and leaves the member in place, so you can inspect what F10 wrote.

The job goes to QGPL/QBATCH, so that subsystem must be active to see it run.

---

## 5. What will not work (by design)

Only the screens and the programs they need to display data are included. Function keys and options that launch other inquiry programs are not included. They will end with a "program not found" error.

- **INV502:** F2 (buyers), F3 (SKU search), F4 to F23 inquiries.
- **INV046:** any function keys or options that call programs outside this library.
- **TS0110:** F10 creates the distribution member, but no transfers are created (see the stub above).

The sample data covers a limited set of SKUs, stores and warehouses. Fields with no matching rows display as blank or zero. For example, pack-and-hold is 0 for department 12 in TS0110.
