# PERP SQL DDL Style Guide

The `ibmi-agentic/perp/` module is greenfield — it introduces SQL DDL, FKs,
CHECK constraints, and journaling into a repository that until now used only
DDS `.pf` sources with no referential integrity and no commitment control.

Every convention below is being **set** by this module. Every subsequent PERP
story must follow it. The one worked example — `example_reference.table.sql`
in `qddlsrc/` — demonstrates every convention in this document.

Design decisions and the rationale behind each choice live on the Confluence
page **[Design Decisions & Conventions](https://profoundlogicsupport.atlassian.net/wiki/spaces/CPP/pages/2511831043/)**.
This guide is the codebase-side rendering of that reference.

---

## 1. File conventions

- **One `.table.sql` file per table.** No multi-object files.
- **snake_case** file names matching the SQL table name — e.g.
  `inventory_master.table.sql` creates the table `inventory_master`.
- **Extensions match object type**, so `codermake` maps them to `RUNSQLSTM`:
  - `.table.sql` → `CRTPF` via `RUNSQLSTM` (SQL table)
  - `.index.sql` → `CRTLF` via `RUNSQLSTM` (SQL index)
  - `.view.sql`  → `CRTLF` via `RUNSQLSTM` (SQL view)
  - `.proc.sql`  → `CRTPGM` via `RUNSQLSTM` (SQL stored procedure)
- Sources live under **`ibmi-agentic/perp/qddlsrc/`**.

## 2. Short-name mapping

DB2 for i has a 128-byte SQL name **and** a 10-byte system name for every
table and column. If you don't declare the system name explicitly, DB2
generates one — typically a truncated, non-obvious string that RPG programs
must then reference. That defeats the point of readable names in RPG.

**Every table** carries an explicit `FOR SYSTEM NAME`. **Every column** carries
an explicit `FOR COLUMN`. The system name is uppercase and ≤ 10 chars.

**Exception — SQL name is already a valid system name.** DB2 for i rejects
`FOR SYSTEM NAME X` / `FOR COLUMN X` when the target SQL name is itself a
valid ≤10-char system name (all-alpha, or underscores allowed). In that case
providing a redundant *or different* system name raises `SQL7029: System name
X cannot be specified`. Two situations trigger this:

1. The SQL name is ≤ 10 chars and pure alphanumeric, and you ask for the
   same value as the auto-derivation — e.g. `CREATE TABLE company FOR SYSTEM
   NAME COMPANY` or `city FOR COLUMN CITY`.
2. The SQL name is ≤ 10 chars and *already* a valid system name (underscores
   allowed), and you ask for a *different* value — e.g. `perp_user FOR
   SYSTEM NAME PERPUSR`. DB2 has no way to store a second short name here;
   the SQL name is the short name.

Fix: either omit the `FOR SYSTEM NAME` / `FOR COLUMN` clause and let DB2
auto-derive, or rename the SQL identifier so it needs a short name (e.g.
`city` → `city_name FOR COLUMN CITY`, `perp_user` → keep the SQL name and
accept `PERP_USER` as the system name — RPG programs `dcl-f perp_user`).

**Don't trust the SQL7029 message text as a hint at the real system name.**
`SQL7029: System name X cannot be specified` echoes back whatever value
*you* proposed — it is not telling you what DB2 actually auto-derived.
(Learned in PERP-20: an explicit `FOR SYSTEM NAME ITMCLS` on `item_class`
raised `SQL7029: System name ITMCLS cannot be specified`, which reads like
confirmation that `ITMCLS` is correct — it isn't. The real auto-derived
name, confirmed via `DSPOBJD OBJ(PERPDEMO/*ALL) OBJTYPE(*FILE)`, is
`ITEM_CLASS` — DB2 only abbreviates when the SQL name exceeds 10 characters,
and `item_class`/`item_lot` are exactly 10/8.) After hitting this error,
omit the clause, build, then confirm the real object name with `DSPOBJD`
before writing anything downstream (Rules.mk targets, `PERPSJPF` calls,
RPG `dcl-f`) against it — don't guess an abbreviation.

Related: **`LABEL ON TABLE` text is capped at 50 characters** on DB2 for i;
longer text raises `SQL0107`. Column labels have the same cap.

**Clause order matters.** On DB2 for i, `FOR COLUMN` goes **between the
column name and the data type**, not after the data type. Placing it after
`CHAR(...)` / `VARCHAR(...)` triggers the CCSID-modifier grammar (the parser
expects `FOR BIT DATA` / `FOR SBCS DATA` / `FOR MIXED DATA`) and RUNSQLSTM
fails with `SQL0199: Keyword COLUMN not expected`.

```sql
CREATE TABLE inventory_master FOR SYSTEM NAME INVMSTR (
  company_code FOR COLUMN COMPCD CHAR(3)     NOT NULL,
  item_number  FOR COLUMN ITEMNO VARCHAR(20) NOT NULL,
  ...
)
```

## 3. Multi-tenancy — `company_code`

Every business table is multi-tenant:

- `company_code CHAR(3) NOT NULL` is the **leading column** of every
  composite primary key on a business table.
- Reference tables (UOM, code_master, etc.) may be global, but if a value
  is company-scoped, `company_code` leads the PK there too.
- Items are **not** shared across companies — each company gets its own item
  numbers. This lets demos show two companies with the same SKU carrying
  different attributes.

## 4. Standard audit block

Every table carries the same five columns at the end of its column list:

```sql
created_at FOR COLUMN CRTAT TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP,
created_by FOR COLUMN CRTBY VARCHAR(18) NOT NULL DEFAULT USER,
updated_at FOR COLUMN UPDAT TIMESTAMP   NOT NULL DEFAULT CURRENT_TIMESTAMP,
updated_by FOR COLUMN UPDBY VARCHAR(18) NOT NULL DEFAULT USER,
is_active  FOR COLUMN ISACT CHAR(1)     NOT NULL DEFAULT 'Y'
```

**Why `VARCHAR(18)` on `_by` columns, not `VARCHAR(10)`.** DB2 for i's `USER`
special register returns `VARCHAR(18)`. If the target column is narrower,
`CREATE TABLE ... DEFAULT USER` fails with `SQL0574: Column, sequence, or
variable attribute is not valid`. IBM i user profiles are still ≤10 chars, so
the extra width is unused padding in practice — but the column must be able
to hold the default the register produces.

The `CHECK (is_active IN ('Y','N'))` constraint is declared at table level
(as a named `CONSTRAINT`) rather than inline. Named constraints produce
readable messages in DB2 catalog errors and give RPG programs a stable
constraint name to reference.

RPG maintenance programs stamp `updated_at`/`updated_by` on every write —
`CURRENT_TIMESTAMP` / `USER` are only the safety-net defaults.

## 5. Referential integrity — declared in DDL

FKs and CHECK constraints are declared on the table, not enforced in
application code. If a change would violate integrity, we want DB2 to reject
it — full stop. RPG programs must handle the resulting SQLSTATE cleanly.

- Every FK is `ON DELETE RESTRICT ON UPDATE RESTRICT` unless there is a
  specific reason otherwise. Physical deletes are rare — prefer `is_active =
  'N'` for soft delete.
- CHECK constraints enforce enumerations and simple invariants (positive
  quantities, `IN (...)` value sets).
- **No `GENERATED ALWAYS AS IDENTITY`** unless the table has no natural key.
  In PERP that is only `reconciliation_log`.

## 6. Composite PKs

- Business tables: `PRIMARY KEY (company_code, ...natural_key...)`
- Reference tables: `PRIMARY KEY (code_type, code_value)` and similar
- Multi-column PKs are the norm — no surrogate `id INT` columns.

## 7. Journaling — day-one requirement

Every table in this module is journaled from the moment it is created.
Departs from `cfdemo/` which runs `SET OPTION COMMIT = *NONE`.

- Journal receiver: `PERPRCV001` in `PERPDEMO`
- Journal:         `PERPJRN`     in `PERPDEMO`
- Bootstrap CL:    `qclsrc/perpjrn.clle`  — creates receiver + journal. Run
  once at library setup: `CALL PGM(PERPDEMO/PERPJRN)`. Idempotent
  (monitors CPF7010).
- STRJRNPF wrapper: `qclsrc/perpsjpf.clle` — `CALL PERPSJPF PARM('MYTBL')`
  attaches a single physical file to `PERPJRN` with `IMAGES(*BOTH)
  OMTJRNE(*OPNCLO)`. Call after every new `.table.sql` build.
- RPG programs use real commitment control (`ACTGRP(*NEW)` or a named
  activation group; no `COMMIT(*NONE)`).

## 8. Effective-dated pricing

Pricing tables use `effective_from` as part of the PK and a nullable
`effective_to`. Current row: `effective_to IS NULL`. Historical rows are
immutable — new prices close the current row and insert a new one.

## 9. Document numbering

Doc numbers are integer sequences per `(company_code, doc_type)` with a
computed display column for demo aesthetics:

```sql
po_number  FOR COLUMN PONBR  BIGINT      NOT NULL,
po_display FOR COLUMN PODSPY VARCHAR(20) GENERATED ALWAYS AS
           (company_code CONCAT '-PO-' CONCAT LPAD(CHAR(po_number), 6, '0'))
```

A `document_sequence` table holds the high-water mark per company + doc type.

## 10. Generic lookup — `code_master`

Simple code-and-description lookups (statuses, priorities, roles, approval
sources) live in one `code_master` table keyed on `(code_type, code_value)`.
Business tables reference it via a `GENERATED ALWAYS AS ('POSTATUS')`
column so a real FK still works:

```sql
status_code FOR COLUMN STCODE VARCHAR(20) NOT NULL,
status_type FOR COLUMN STTYPE VARCHAR(20) NOT NULL DEFAULT 'POSTATUS'
            CHECK (status_type = 'POSTATUS'),
FOREIGN KEY (status_type, status_code)
  REFERENCES code_master (code_type, code_value)
```

**Why `DEFAULT + CHECK` instead of `GENERATED ALWAYS AS ('POSTATUS')`.** DB2
for i requires the generation expression of a computed column to reference
at least one column of the same table — a bare literal is rejected with
`SQL0104: Token '...' was not valid`. `DEFAULT + CHECK` achieves the same
guarantee (row always carries the discriminator value, cannot be overridden)
and lets the composite FK to `code_master` still be declared cleanly.

**Exceptions:** UOM and `item_class` keep their own tables because they
carry structural attributes (conversion factors, per-company scope) that
don't fit a global lookup.

## 11. File header

Every `.table.sql` file starts with the same header block:

```sql
-- ---------------------------------------------------------------------------
-- Table:   <sql_name>  (system name <SYSNAME>)
-- Module:  perp
-- Purpose: <one-line purpose>
-- Epic:    PERP-<n>
-- ---------------------------------------------------------------------------
```

## 12. Build ordering

Rules.mk expresses FK ordering with normal prerequisites (not order-only):

```makefile
item.file: qddlsrc/item.table.sql company.file item_class.file uom.file
```

A parent table must have been (re-)built before a child that FKs to it. Use
`|` (order-only) for the STRJRNPF hook and other dependencies that shouldn't
trigger a rebuild.

---

## Reference example

**`qddlsrc/example_reference.table.sql`** in this module demonstrates every
one of these conventions in a single file. When adding a new table, copy that
file, rename it, and edit — don't start from scratch.

---

## 13. RPG / SQLRPGLE conventions

Style rules that came out of PERP-19 (docseq service program) and the PERP-2
maintenance programs. These apply to every RPG or SQLRPGLE source under
`perp/qrpglesrc/`.

- **Modern `**FREE`**, first column of the file — no leading whitespace on
  the `**free` directive itself (RPG `RNF0257`/`RNF7503` cascade otherwise).
- **`ctl-opt dftactgrp(*no) actgrp(*new)`** — real activation-group scoping;
  no default-actgrp fallback.
- **No `SET OPTION COMMIT = *NONE`.** Every PERP table is journaled, so RPG
  runs under real commitment control (`commit(*chg)` — the default of
  `CRTSQLRPGI`). Callers issue `EXEC SQL COMMIT` / `ROLLBACK`; leaf modules
  don't.
- **Schema-qualified table names** in embedded SQL — `perpdemo.company` etc.
  `CRTSQLRPGI` on this environment does not accept `DFTRDBCOL` via the
  codermake recipe, and the aitool / SSH invocation paths run outside the
  PERPDEMO library list. Hard-qualifying keeps every path working.
- **Host-variable names must not collide with column names.** DB2 for i's
  SQLRPGLE precompiler raises `SQL0314` ("host variable X not unique")
  when an unqualified `:name` in a WHERE clause matches a column of the
  referenced table AND a subprocedure parameter. Prefix parms (`nx_`, `pk_`,
  `in_`).
- **Host-variable scope is *module*, not *subprocedure*.** The precompiler
  does not respect `dcl-proc` scope when collecting host variables — two
  subprocedures with parameter names in common raise `SQL0314`. Give every
  subprocedure a distinct prefix.
- **`FROM FINAL TABLE (…)` supports `INSERT` only on DB2 for i V7R4.**
  Precompiler rejects `UPDATE`/`DELETE` variants with `SQL0199`. Use
  `UPDATE` + subsequent `SELECT` inside the same unit-of-work (row lock is
  held under `commit(*chg)`).
- **Service programs**: one prototype `.rpgle` per module, referenced via
  `/copy`; explicit `.bnd` export list in `perp/qsrvsrc/`; binding
  directory qualifies srvpgm names with `$LIBRARY` so callers do not need
  PERPDEMO on their library list at activation time.
- **Data-area handles**: use `dcl-ds NAME dtaara(*lda) len(1024) qualified`
  and reserve positions in the LDA — every job has an LDA automatically, so
  no runtime `CRTDTAARA` is needed. PERP session state (currently just the
  selected company code at positions 1-3) lives in the LDA.

## 14. DSPF conventions

- **`DSPSIZ(24 80 *DS3)`** — 5250 24×80 baseline (not 27×132).
- **`SFLPAG`** must be conservative enough to fit the display size minus
  header rows minus footer minus one for the `SFLEND(*MORE)` indicator.
  `CPD7817` (value on SFLPAG too large) will bite otherwise. `SFLPAG(0007)`
  is the safe default this module has been using.
- **Do not repeat file-level command-attention keys on a record**
  (`CPD7597` "keyword not allowed at both file and record level"). Declare
  `CA03/CA05/CA06/CA12` once at file level.
- **Standard F-key legend on every DSPF**: F3=Exit, F5=Refresh, F6=Add,
  F12=Cancel — declared at file level and echoed in the footer line.
- **Message subfile** (`R xMSGSFL` / `R xMSGCTL`) attached at row 24 on
  every screen; RPG uses `QMHSNDPM` to post messages.
- **Every numeric field on screen (subfile column or edit-panel field,
  input or output) gets `EDTCDE(3)`.** Without an edit code, a zoned
  numeric field displays every leading zero (e.g. `000000001500000` for
  150.0000), which is unreadable and reads as an error to anyone glancing
  at the screen. `EDTCDE(3)` zero-suppresses and inserts the decimal
  point (a no-op if `DEC=0`) while still displaying a literal `0` for a
  true zero value — critical for balance/quantity fields where "zero" is
  a meaningful, common state (e.g. a freshly-added item's `qty_on_hand`)
  that must read as `0`, not blank or a run of zeros.
  - **Do not use `EDTCDE(Z)`, `EDTCDE(2)`, `EDTCDE(4)`, or any other
    "blank when zero" code for business quantity/balance fields** — those
    codes zero-suppress by blanking the field entirely when the value is
    zero, which is indistinguishable from "no value" and defeats the
    purpose for anything the user needs to read as an explicit zero.
    (`Z`/`2`/`4`/`B`/`D`/`K`/`M` blank zero; `1`/`3`/`A`/`C`/`J`/`L` show
    it as `0` — this module standardizes on `3`, the plainest of the
    zero-showing codes, since these are quantities/counts, not currency
    needing comma grouping.)
  - Adding `EDTCDE(3)` to a `DEC>0` field grows its display width by
    exactly one column (the decimal point) — leave at least that much
    headroom between adjacent fields packed onto the same row, or the
    compile will overlap/truncate.
- **Never issue `READC` against a subfile that was not written to this
  cycle.** If the load routine finds 0 rows this pass (e.g. an empty
  filter result, or the "enter a key value to begin" state before any
  scoping value has been typed), the subfile-options `READC` loop must
  be skipped entirely — guard it with `if numRows > 0; ... endif;`.
  Unconditionally issuing `READC` against a subfile that has never been
  `WRITE`n this invocation raises a runtime **"Session or device error
  occurred in file &1"** (CPF5006-class) the instant the user presses
  Enter on an empty list — it does not just return `*EOF` the way a
  populated-then-cleared subfile would. Found across every PERP
  work-with program (`PERPSELR`, `WRKCMR`, `WRKUSRR`, `WRKUOMR`,
  `WRKCNVR`, `WRKICLR`, `WRKITMR`, `WRKLOTR`) since they all share the
  same load/fill/read-options skeleton; fixed in all eight at once.
  Corollary: if a program skips the load routine entirely when its scope
  key is blank (e.g. `WRKCNVR`/`WRKLOTR` before an item number is
  entered), explicitly reset the row counter to 0 in that branch too —
  otherwise a stale nonzero count from a *previous* scope value lets the
  guard pass even though nothing was loaded this cycle.

## 15. codermake gotchas

- **`.menu` recipe needs `.file` as a *normal* prerequisite**, not
  order-only. `foo.menu: foo.msgf foo.file` builds; `foo.menu: foo.msgf |
  foo.file` silently drops the recipe (make says "Nothing to be done" and
  the menu is never created).
- **`CRTSQLRPGI` does not accept `DFTRDBCOL` through codermake's compile
  options** — the recipe hard-codes its flags. Qualify table references
  in RPG source instead of relying on a runtime library list.
- **Binding directory sources** should qualify service-program references
  with `$LIBRARY` (`addbnddire … obj(($LIBRARY/mysrvpgm *srvpgm *immed))`)
  so activation-time lookup does not depend on the caller's library list.
- **Menu DSPF record format name must match the menu object name.**
  `CRTMNU TYPE(*DSPF)` looks for a record format whose name equals the
  menu name (e.g. menu `PERPMNU` requires `A R PERPMNU` in the DSPF).
  Wrong name compiles fine but calling `GO PERPMNU` fails at runtime with
  `Record format for menu definition not found. Problem displaying menu
  PERPMNU in library PERPDEMO.`
- **`CRTSRCPF … TEXT(...)` caps at 50 characters** — CPD0074 fires on
  longer strings. Keep source-PF text descriptions terse.

## 16. Source physical files in PERPDEMO

Every source type used by the module needs a matching source PF on the
IBM i target. `codermake` copies sources into these files at sync time.
Create them once when standing up a new task/build library:

```
CRTSRCPF FILE(PERPDEMO/QDDLSRC)  RCDLEN(112) TEXT('PERP - SQL DDL source')
CRTSRCPF FILE(PERPDEMO/QDDSSRC)  RCDLEN(112) TEXT('PERP - DDS source')
CRTSRCPF FILE(PERPDEMO/QRPGLESRC) RCDLEN(112) TEXT('PERP - RPG ILE / SQLRPGLE')
CRTSRCPF FILE(PERPDEMO/QCLSRC)   RCDLEN(112) TEXT('PERP - CL / CLLE source')
CRTSRCPF FILE(PERPDEMO/QSRVSRC)  RCDLEN(112) TEXT('PERP - Binder/Service pgm')
CRTSRCPF FILE(PERPDEMO/QMENUSRC) RCDLEN(112) TEXT('PERP - Menu source')
CRTSRCPF FILE(PERPDEMO/QCMDSRC)  RCDLEN(112) TEXT('PERP - Command source')
CRTSRCPF FILE(PERPDEMO/QPNLSRC)  RCDLEN(112) TEXT('PERP - Panel group source')
CRTSRCPF FILE(PERPDEMO/QSQLSRC)  RCDLEN(112) TEXT('PERP - SQL source')
```

`QDDLSRC` was missing at PERP-2 initial cutover and was created after
the fact; keep it as part of the PERPDEMO bootstrap for future clones.
