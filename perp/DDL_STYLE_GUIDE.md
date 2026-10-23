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

**A short column name can trip the same rule with a different error.**
Learned in PERP-33: `notes FOR COLUMN NOTES CLOB(16K)` raises `SQL0612:
NOTES is a duplicate column name`, not the `SQL7029` shown above for tables
— same root cause (the SQL name `notes` is already a valid ≤10-char system
name, so the explicit `FOR COLUMN NOTES` collides with the auto-derivation),
just a different diagnostic at the column level. Fix is identical: drop the
redundant `FOR COLUMN` clause and let it auto-derive.

**Auto-derived short names for >10-char SQL names are not a simple
truncation — they can be a sequential counter with no relation to the SQL
name at all.** Learned in PERP-28: `item_vendor` (11 chars) and
`item_vendor_price` (18 chars) both share the `ITEM` prefix with the
already-existing `item`/`item_class`/`item_lot` tables, and DB2 for i's
abbreviation algorithm produced `ITEM_00001` and `ITEM_00003` (not
`ITMVND`/`ITMVPRC` or any other intuitive abbreviation) to avoid a
collision. There is no way to predict this from the SQL name — always
confirm with `DSPOBJD OBJ(PERPDEMO/*ALL) OBJTYPE(*FILE)` right after the
build and use the *real* object name in every downstream reference
(`PERPSJPF` calls, `DSPFD`, RPG `dcl-f`/embedded-SQL is unaffected since it
resolves by SQL name, but any native/CL-level reference needs the real
short name).

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

**Partial (filtered) unique indexes work as `.index.sql` on DB2 for i.**
Confirmed in PERP-28: `CREATE UNIQUE INDEX ... ON tbl (cols) WHERE
predicate` runs clean through `RUNSQLSTM` and produces a normal `*FILE` LF
object. Use this for "at most one flagged row per group" constraints
(e.g. one preferred vendor per item) instead of application-code
enforcement — DB2 rejects the second `WHERE`-matching row with
`SQL0803`/`SQLSTATE 23505` just like any other unique-index violation.

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

Doc numbers are integer sequences per `(company_code, doc_type)`:

```sql
requisition_number FOR COLUMN REQNBR BIGINT NOT NULL,
```

A `document_sequence` table holds the high-water mark per company + doc type
(bumped via the `docseq` service program, PERP-19).

**No computed display column.** An earlier draft of this section showed a
`GENERATED ALWAYS AS (expression)` computed column for demo-friendly display
numbers (e.g. `ACM-PO-000123`). That pattern **does not build on this
target** — confirmed in PERP-33. Every variant tried raised a parser error
pointing at the open paren or the first identifier inside it, with the same
oddly specific hint list (`Valid tokens: . ACCTNG USERID APPLNAME PROGRAMID
WRKSTNNAME`) regardless of what the expression contained:

| Attempt | Error |
|---|---|
| `company_code CONCAT '-REQ-' CONCAT LPAD(...)` | SQL0199 at the first `CONCAT` |
| `company_code \|\| '-REQ-' \|\| LPAD(...)` | SQL0104 at the first `\|\|` |
| `(company_code)` — bare column reference | SQL0104 at the closing `)` |
| `(UPPER(company_code))` | SQL0104 at `(` after `UPPER` |
| `(tablename.company_code)` | SQL0104, hinted `QSYS2 SYSIBM` |
| IBM's own reference example, `bonus DEC(9,2) GENERATED ALWAYS AS (salary * .10)` | SQL0104 at `*` |

`GENERATED ALWAYS AS IDENTITY` (no expression) works fine on this target —
only the computed-column expression form fails, on every shape tested. If a
future story needs a demo-friendly display number, build it in RPG/DSPF (or
a view) instead of a generated column; don't re-attempt this in DDL without
budgeting time to re-verify it against whatever DB2 for i PTF/config is live
at the time.

**Worked example — PO open_qty via a view (PERP-37).** `po_line` needs a
per-row `open_qty = ordered_qty - received_qty` for the browse screen and
receipt allocation. Rather than fight the computed-column parser, the epic
ships a companion view `po_line_open` (`qddlsrc/po_line_open.view.sql`) that
`SELECT ordered_qty - received_qty AS open_qty, ordered_qty * unit_price AS
extended_price ... FROM po_line`. Programs that need the derived columns
join the view; programs that don't read `po_line` directly. Views are
`.view.sql` and build as normal `RUNSQLSTM` → `*FILE` LFs (short name auto-
derives; `po_line_open` came out as `PO_LI00002`, confirmed via `SYSTABLES`
after build — same auto-derivation rule as tables).

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
- **The SQLRPGLE precompiler does not accept `:ds(i).field` as a host
  variable.** Found in PERP-39 (`poreqr.sqlrpgle`): a WHERE clause of
  `and rl.requisition_number = :reqs(i).reqnbr` fails with `SQL0312
  Variable REQS not defined or not usable` and `SQL0104 Token ( not
  valid`. Same rule applies to nested DS arrays like
  `:plan(v).lines(L).item`. Fix: copy the array element to a plain
  scalar host variable (`xReqNbr = reqs(i).reqnbr; ... where
  ... = :xReqNbr`). This is why `poreqr` has an `x`-prefixed staging
  block near the top of its declarations.
- **`dcl-s` inside a `begsr` subroutine is illegal.** Found in the same
  program: local variables declared with `dcl-s` inside a `begsr` block
  raise `RNF0724 The statement type is out of sequence for the main
  procedure`, because `begsr` runs in the main procedure's scope (not
  its own like `dcl-proc`). Hoist all `dcl-s`/`dcl-ds` to the main
  declaration section; only executable statements go in a `begsr`.
- **Free-format RPG allows only one statement per line.** Two statements
  separated by whitespace on the same line (e.g.
  `*in60 = *off;  *in61 = *off;`) raise `RNF5508 End of free-format
  statement is not blank`. Put each on its own line.
- **`%editc(int : 'X')` returns hex, not decimal.** Every PO number
  in `pobrwr`'s first live browse rendered as `0000000000` because
  `bsponbr = %editc(rows(i).ponbr : 'X')` — edit code `'X'` is
  documented as "hex representation of a zoned decimal", not "plain
  string". For displaying a numeric doc-number (PO number,
  requisition number, line number) as a plain string, use `%char()`.
  Use `%editc` only when you deliberately want an RPG edit-code
  formatted output (comma grouping, sign, decimal shift, etc.) and
  never `'X'` for business display.

- **`%subst(varchar : 1 : N)` fails at runtime when `N` exceeds the
  current data length, not just when it exceeds the declared max.**
  Found in `pobrwr` after the DATFMT fix landed — `bsstat = %subst(
  rows(i).stat : 1 : 10)` where `stat` is `VARCHAR(20)` holding
  `'OPEN'` (4 chars) raised `RNQ0100 Length or start position is out
  of range for the string operation (C G D F)` at runtime, even
  though the declared max (20) is well above the requested 10. Same
  applies to `%subst(vendor_name : 1 : 25)` when the vendor name
  happens to be shorter than 25.

  **Fix:** just use direct assignment from VARCHAR into a fixed CHAR
  field. RPG right-pads or truncates automatically — no `%subst` is
  needed for "fit into the display field". Use `%subst` only when
  you actually need a middle slice, and even then guard with
  `%min(%len(...), N)`. Fixed across `pobrwr`, `poreqr`, `poschr`
  in one pass; captured here so downstream epics don't repeat it.

- **Sentinel/filter Date values in embedded SQL must stay within the
  job DATFMT range (`*MDY`, 1940-2039, on this env).** The SQL
  precompiler generates its intermediate host variables (the ones
  named `SQL_00020`, `SQL_00021`, ... that back every `:var`
  reference in an EXEC SQL) with `DATFMT(*MDY/)` — the JOB DATFMT,
  IGNORING `ctl-opt datfmt(*iso)`. Confirmed by inspecting the
  compile listing after the "fix":

  ```
  D  SQL_00020            192    199D   DATFMT(*MDY/)   FFRDT
  D  SQL_00021            200    207D   DATFMT(*MDY/)   FTODT
  ...
  SQL_00020 = FFRDT;   //SQL  <-- assigns *ISO FFRDT into *MDY SQL_00020
  ```

  So even when the RPG Date variable's *storage* format is `*ISO`
  (thanks to `ctl-opt datfmt(*iso)`), the precompiler-generated host
  variable it gets assigned to is `*MDY`, and any value outside
  1940-2039 crashes with `RNQ0114 The year portion of a Date or
  Timestamp value is not in the correct range (C G D F)`. Found in
  `pobrwr` (browse-filter from/to sentinels), 2026-07-22, after two
  earlier fix attempts (adding `:*ISO` to the parse, then
  `datfmt(*iso)` to `ctl-opt`) both compiled clean but ran the same
  crash at the exact same statement.

  **What works:**
  1. Put `datfmt(*iso)` on `ctl-opt` — sets RPG Date variable
     storage to *ISO (0001-9999); needed for anything that assigns
     to/from the DSPF's `L DATFMT(*ISO)` fields.
  2. Parse literals with explicit `%date('yyyy-mm-dd' : *ISO)` so
     the parse step doesn't use the job DATFMT.
  3. **Keep any Date value that will be assigned to an SQL host
     variable within `1940-2039`.** For the pobrwr filter, that
     meant swapping `0001-01-01` / `9999-12-31` sentinels for
     `1940-01-01` / `2039-12-31` — still functionally "no filter"
     for realistic PO dates, but doesn't fail the *MDY range check.

  All three steps are needed; the third is what stopped the runtime
  crash for good. If you truly need out-of-range Date values in SQL
  (unlikely in PERP), the only escape is to bypass the host-variable
  path — use dynamic SQL with the date rendered as a CHAR literal
  inside the statement text, or move the Date column comparison out
  of the WHERE clause entirely. `%date()` with no args (returns
  today) is always safe.
- **A correctly-ranged sentinel default does not protect against a
  user blanking the field.** Found in `pobrwr` again, 2026-08-07
  (PERP-89): even with the `1940-2039` sentinel fix above already in
  place, clearing the From/To date filter field on screen and
  pressing Enter crashes with the identical `RNQ0114`. Blanking a
  native DDS `L`-type field does not behave like clearing a character
  field — it still produces *some* date value that has to pass the
  *MDY range check, and no PERP program currently guards for it.
  There is also no equivalent, for a blanked date field, of the
  character-filter idiom (`:fstat = '' or ...`) that already lets
  `fvnd`/`fbuy`/etc. mean "no filter" — `loadPOs`-style code applies
  the date bounds unconditionally. Any optional/filter date field
  needs to explicitly detect blank input before it reaches an SQL host
  variable and decide what blank should mean (most likely: reapply
  the sentinel), rather than assume 5250 will always hand back an
  in-range value.
- **Program/module/file object names cap at 10 characters — same as
  journal receivers (§7).** Learned again in PERP-32: naming a smoke-test
  caller `itmvprcqsmk.sqlrpgle` (11 chars) failed `CRTSQLRPGI` with
  `CPD0074: Value 'ITMVPRCQSM' for OBJ exceeds 10 characters` — codermake
  does *not* auto-truncate the source basename to fit. Renamed the file
  itself to `ivprcqsmk.sqlrpgle` (9 chars). Unlike SQL table/column short
  names (§2), there is no separate "system name" escape hatch for RPG
  program objects — the source file basename *is* the object name, so it
  must fit within 10 chars from the start.

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
- **F12 means "back one screen in this program's own flow," never "end
  the program."** Only F3 ends the program and returns to the menu. An
  edit/add panel's `EXFMT` should `return` from its subroutine on
  `*in12` (unwinding to redisplay the list/caller) — most PERP
  programs already get this right for their edit panels (`WRKITMR`,
  `WRKIVNR`, `WRKCNVR`, etc.). The mistake is at the *list* level:
  `if *in03 or *in12; leave; endif;` on a top-level list/subfile
  screen is correct ONLY when that screen is the true entry point of
  the flow with nothing earlier to go back to (e.g. `PERPSELR`'s
  company picker, or a program's own header-entry screen). Found live
  on three programs where a *second* screen in the same flow made
  this mistake instead — `reqentr`'s Requisition Lines list (PERP-85),
  `poentr`'s Purchase Order Lines list (PERP-94), and `poschr`'s
  Schedule list (PERP-95) — each exits straight to the menu on F12
  from a screen that has an obvious "back" target (the header screen),
  discarding the user's place even though the parent record is
  already saved. Audit every `if *in03 or *in12` in a new or changed
  program and ask: is this genuinely screen #1 of the flow?
- **After a terminal action (Submit, Approve, Reject), navigate the
  user somewhere useful — don't leave them staring at the now-locked
  record.** Two programs `iter`ed back to redisplay the very screen
  that had just been locked, with no way to start the next document
  without exiting the whole program (`reqentr`'s F8=Submit, PERP-86;
  `poentr`'s F8=Submit, PERP-93). Decide, for every finalize action,
  what screen the user lands on next (a list of similar documents, or
  a fresh entry screen for the same document type), and carry the
  confirmation `writeMsg()` forward to whatever screen they land on —
  see the `clearMsgs()` ordering note above; don't let it get wiped in
  the transition.
- **Message subfile** (`R xMSGSFL` / `R xMSGCTL`) attached at row 24 on
  every screen; RPG uses `QMHSNDPM` to post messages.
- **`QMHSNDPM`'s message-key output parameter must be the DDS field bound
  to `SFLMSGKEY`** (e.g. `SMSGKEY`), not a separate RPG variable of your
  own — even one also named `msgkey`. Found in PERP-34/PERP-35 (`reqentr`/
  `reqaprr`): a `writeMsg` helper passed its own local `msgkey` to
  `QMHSNDPM` instead of the DDS field `smsgkey`, so `smsgkey` stayed
  uninitialized; the subsequent `WRITE` to the message subfile record then
  crashed at runtime (`CPF9999`-class exception, "The call to <PROC> ended
  in error") because the device driver couldn't resolve a message using a
  garbage key. Caught only by a live user hitting the very first
  `writeMsg` call in a fresh program (the "no company selected" guard) —
  it reproduces on *every* call, not just that one path, so a single typo
  here breaks every message the program ever shows. Always pass the
  record's own `SFLMSGKEY` field, and double-check this any time you
  copy the `writeMsg`/`clearMsgs` boilerplate into a new program.
- **`clearMsgs()` must run *before* an action handler's `writeMsg()`
  in the same pass, never after.** The standard boilerplate calls
  `clearMsgs()` once per loop iteration, ahead of that iteration's own
  `EXFMT`, so a message queued during the *previous* pass survives to
  be shown on the *next* one. If an F6/F8/etc. handler instead calls
  `writeMsg()` and then `iter`s straight back to the top of the loop,
  and `clearMsgs()` sits unconditionally at that same top, it wipes
  `msgrrn` and clears the message subfile before the next `EXFMT` ever
  displays the message the handler just queued — confirmation or error
  text is queued and destroyed in the same pass, and the action looks
  like it silently did nothing. Found in `wrkivpr` (PERP-74: the
  F6=Add-with-blank-item warning never reaches the screen) and
  suspected as a contributing factor in `reqaprr` (PERP-87:
  Approve/Reject confirmations). `reqentr`'s line-list loop gets this
  right — `clearMsgs()` sits *between* the F3/F12 check and the F6/F8
  dispatch, not folded into the very top of the loop after them — use
  that ordering as the reference shape when copying the boilerplate.
- **The record format shown alongside (or right before) the message
  subfile must have `OVERLAY`**, or displaying/writing it clears the
  screen and erases the just-written message subfile before the user
  ever sees it. Every PERP work-with screen's primary `SFLCTL` format
  has had `OVERLAY` since PERP-3, but a plain (non-subfile) entry screen
  — e.g. `reqentd.dspf`'s header-entry format `RHEAD` (PERP-34) — is easy
  to miss since there's no subfile keyword nearby as a reminder. Give
  every format that gets `WRITE`/`EXFMT`'d `OVERLAY`, subfile or not.
- **A second real subfile in `reqaprr`/`reqaprd.dspf` (list `ASFL`/`ASCTL`
  plus a review-detail `ALSFL`/`ALCTL`) reliably crashed live with
  "Session or device error occurred in file &1" (`CPF5006`) / an
  unmonitored `RNX1255` at `EXFMT ALCTL`, on this environment
  (Profound UI Genie, "classic" skin) — across **four** independent,
  each-textbook-correct implementations, and the true cause is still
  not confirmed. Recorded here in full because the investigation
  produced two plausible-looking "root causes" in a row that both
  turned out to be wrong once retested live; don't repeat either as a
  first assumption.
  1. `ASFL`/`ASCTL` (list) + `ALSFL`/`ALCTL` (detail), format switch
     issued from inside the driving `READC asfl` loop — crashed.
  2. Same design, format switch deferred until after the `READC` loop
     fully drains — crashed identically.
  3. Detail screen split into a separately called program (`reqapdtl`)
     with its own device file, the same pattern `wrkitmr` uses calling
     `wrkcnvr`/`wrklotr` — crashed too, cascading errors across both
     files. (This attempt did surface one real, unrelated, independently
     confirmed DDS compile-time bug, kept below: `SFLDSPCTL` combined
     with a below-anchor input field raises `CPD7812`.)
  4. Same two-subfile design as #1, this time built carefully against
     the documented `CPF5006`/`RNX1255` failure mode (see
     [code400.com](https://code400.com/forum/forum/iseries-programming-languages/rpg-rpgle/7971-session-or-device-error)
     and [midrangenews.com](http://www.midrangenews.com/view?id=1788):
     the error fires when `SFLDSP` is on but the subfile has 0 rows from
     the *device's* perspective, typically because a non-`OVERLAY`
     `WRITE` in between cleared the screen) — `OVERLAY` added to every
     format including the `RDFOOT` footer, separate `SFLCLR`/`SFLDSP`
     indicator pairs per subfile, no `SFLDSPCTL`. This looked like a
     confirmed fix (compiled clean, matched every documented rule) and
     was reported as such — but a genuine fresh-session live retest by
     the user showed the **identical crash at the identical statement**.
     The "missing `OVERLAY`" theory is therefore wrong, or at least
     incomplete, as an explanation for this specific crash.
  Every one of these four is standard, previously-working RPG/DDS —
  #4 in particular matches the exact pattern every other PERP work-with
  screen (`PERPSELR`, `WRKCMR`, `WRKITMR`, etc.) already uses safely for
  a single subfile. The one common thread across every failure is
  *two subfiles in this one program*; the one design that has ever
  rendered successfully for the user is `reqaprr`'s current shape:
  **one real subfile (`ASFL`/`ASCTL`) for the list, and a plain
  (non-subfile) `RDETAIL` record for the review/approve screen**, with
  up to `MAXDTLLINES` (6) line-item rows represented as individually
  named fields (`L1ITEM`/`L1QTY`/`L1UOM`/`L1COST` through `L6...`), each
  group conditioned on its own indicator (`*in60`-`*in65`) so an unused
  row is genuinely blank rather than a confusing `0.0000` (do not use
  indicators as a stand-in for real subfile scrolling in general — this
  is a workaround for a shape that is known to fail here, not a new
  default pattern to reach for elsewhere). This is a deliberate
  deviation from the two-subfile pattern used everywhere else in this
  module, kept **only** for `reqaprr`, and it caps review detail at 6
  lines with no scrolling — acceptable for now since requisition line
  counts are small, but revisit if that stops being true or if the
  underlying cause is ever identified. **Do not re-attempt a second
  business subfile in `reqaprr` without an actual interactive retest on
  this environment proving it renders** — a clean compile and a
  textbook-correct design have both already failed to predict this.

  **Addendum, PERP-87 (2026-08-07):** live testing reports F6=Approve,
  F7=Reject, *and* F12 all appearing to return to the menu instead of
  back to the `ASFL`/`ASCTL` list, from the current (workaround)
  `RDETAIL` design described above. Read in isolation, the RPG source
  for all three (`reqaprr.sqlrpgle`'s `reviewReq` subroutine) looks
  correct — each just `return`s to the outer loop, which should
  redisplay the list, not end the program — so if this reproduces, it
  is likely the *same* unconfirmed device-error class documented
  above, now apparently reachable on the *return* path out of the
  plain `RDETAIL` format rather than only on entry into a second
  subfile. That would mean the current workaround has not fully
  closed the underlying issue. Needs a live joblog check for an
  escape message (`RNX1255`/`CPF5006`-class) at the point of return
  before assuming this needs an RPG logic fix — don't spend a
  redesign cycle on `reviewReq`'s branching logic until that's ruled
  out.
- **A `SFLCTL` record's `SFLDSPCTL` keyword combined with an
  input-capable (`B`) field positioned *below* the subfile's anchor row
  raises `CPD7812`: "Subfile control record overlaps subfile record"**
  at DDS compile time — even though the field's row/col is nowhere near
  the subfile's visible `SFLPAG` rows. Confirmed empirically while
  building `reqapdtl.dspf` (PERP-35 follow-up): the same field
  (`ENOTES2`, an entry field at row 15, well below `SFLPAG(0005)`'s
  visible rows 10-14) compiled clean once `SFLDSPCTL` was removed and
  the field was repositioned *above* the subfile's anchor row instead.
  The overlap check appears to reserve rows through `SFLSIZ` (which can
  be much larger than `SFLPAG`, e.g. `50` vs `5` here), not just the
  visible page, for any input-capable control-record field, when
  `SFLDSPCTL` is present. Fix: position input-capable fields on a
  `SFLCTL` record *above* the subfile's anchor row (matching every
  other PERP `SFLCTL` record, e.g. `RLCTL` in `reqentd.dspf`, none of
  which have `SFLDSPCTL` combined with a below-anchor input field), or
  omit `SFLDSPCTL` if it isn't actually needed (it wasn't, here — the
  existing `SFLDSP` conditioning indicator already controls visibility).
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
  - **That one-column allowance is not the whole story — compute the
    field's *maximum* rendered width, including comma insertion,
    before spacing columns.** `EDTCDE(3)` inserts a comma every 3
    integer digits. A `15Y 4` field (13 integer digits) can render up
    to 20 characters wide (13 digits + 4 commas + 1 decimal point + 4
    decimals) — not just "16 digits + 1". Found overlapping in three
    places once realistic (larger) values were entered: `pobrwd.dspf`'s
    PO Detail line columns (Ord/Rcv/Open/Price only 9-10 columns
    apart, PERP-90), `poentd.dspf`'s PO Lines Qty column running into
    the UOM column (PERP-92), and `poreqd.dspf`'s Est Cost column
    starting at column 73 with a 20-character max width — 12 columns
    past the right edge of an 80-column screen, so it silently wraps
    onto the next physical row (PERP-97, and suspected of breaking F6
    option detection there since the wrap likely misaligns how the
    web renderer maps keystrokes back to the right subfile record).
    Work out the worst-case width for every `Y`-type field's digit
    count before choosing its column, not just "+1 for the decimal."
  - **Right-align the column heading over the field, not over its
    start column.** A numeric field's value is right-justified within
    its width; a heading positioned at the field's left edge reads as
    "too far left" once real data appears (PERP-90, PERP-92, and
    `wrkcnvd.dspf`'s Factor heading). Position heading text so it ends
    at (or near) the field's rightmost column instead.
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

## 14a. Location codes are alphanumeric, not numeric ordinals

`item.aisle_code` / `bay_code` / `shelf_code` (`PERP-3`/`PERP-20`) are free-text
`VARCHAR(10)` columns. Real seed data mixes a letter prefix with the ordinal
(e.g. `'A1'`, `'B2'`, `'S3'` -- confirmed against the `WIDGET1` seed row).
`CAST(aisle_code AS INTEGER)` rejects that outright with `SQL0420`.

Any service that needs the numeric ordinal out of one of these codes (e.g.
`whcoord`, `PERP-27`) must parse it in RPG rather than casting the whole
string in SQL -- pull out the digit characters and convert those
(`ordinalFromCode()` in `qrpglesrc/whcoord.sqlrpgle` is the reference
implementation). Don't assume these columns hold pure numeric strings.

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

## 16a. A message subfile only updates when its SFLCTL record is re-WRITE'n

**Toggling the `*in40`-style RPG indicator that conditions `SFLDSP` has no
visible effect unless the `SFLCTL` record itself is `WRITE`'n again** --
changing the RPG variable is not enough; the device only re-evaluates a
record's conditioning indicators when that specific record is the target
of a `WRITE`/`EXFMT` operation. Found in `rcventr` (PERP-43): the header
validation loop and the `receiveLine` edit-panel loop each set `*in40`
based on `msgrrn` and then `EXFMT`'d a *different* format (`RHEAD` /
`RLEDIT`) without an explicit `WRITE rmsgctl` first. Confirmed live: an
invalid-PO-number error never appeared on screen at all, even though
`writeMsg` had correctly written a row into the message subfile and
`msgrrn` was correctly nonzero.

Every PERP work-with program's *subfile* loop already gets this right
(`write rlctl` / `write plctl` / `write bsctl` immediately before the
`exfmt` of the same cycle) -- the bug only shows up in a program's
*header entry* or *plain edit-panel* loop, which don't otherwise need to
re-`WRITE` their own record before `EXFMT` (since `EXFMT` both writes and
reads that record). The message subfile control record is a second,
separate record that needs its own explicit `WRITE` every cycle if its
indicators changed, regardless of what other record is being `EXFMT`'d
in that same iteration.

**Fix:** any loop that both (a) conditionally shows/hides the message
subfile via an indicator and (b) `EXFMT`s a record other than the message
`SFLCTL` itself must `WRITE` the message `SFLCTL` record explicitly,
every iteration, before that `EXFMT` -- not just toggle the indicator
variable. `poentr`'s and `reqentr`'s header-entry loops follow the same
toggle-without-write shape as `rcventr`'s did before this fix and were
likely never live-tested against a failing header validation (PERP-7's
own verification notes cite the AIDEMO-menu-no-cmdline block as the
reason interactive testing was skipped in favor of state-based SQL
checks) -- worth a live retest and matching fix there if anyone is in
that code again, but out of scope to change opportunistically here.

## 17. CHECK constraints cannot cross tables

A `CHECK` constraint's expression may only reference columns of the table
being defined — DB2 for i (like every SQL-standard implementation) has no
concept of a cross-table `CHECK`. Found in PERP-42: the ticket for
`po_receipt_line` asked for "if `item.lot_controlled = 'Y'` then
`lot_number IS NOT NULL`", which reads like an ordinary `CHECK` but
`item.lot_controlled` lives on a different table than `lot_number`. There
is no DDL syntax that expresses this — a subquery inside `CHECK` is
rejected outright, and there's no cross-table trigger-like `CHECK` variant
on this platform.

**Fix: enforce it in the RPG program that inserts the row, not in DDL.**
`rcventr` (PERP-43) looks up `item.lot_controlled` before `INSERT`ing into
`po_receipt_line` and rejects the entry interactively if a lot-controlled
item has no lot number entered. The column itself stays nullable in DDL
(`lot_number` on `po_receipt_line`) — the invariant is real and enforced,
just at the application layer instead of the database layer. This is a
general rule, not specific to lots/receipts: any "if column A on table X
then column B on table Y must be Z" business rule in a future PERP story
needs the same treatment — don't spend time trying to express it as a
table-level `CONSTRAINT` first.

## 18. Screen-to-source coverage

Every interactive screen a user can reach should have matching
`.dspf`/`.sqlrpgle` source in this repo — the repo is the source of
truth, and live testing/fixes are only possible for what's actually
checked in here.

**Found a gap, PERP-98 (2026-08-07):** the live "Receipt Entry" screen
(PO Number, Vendor, PO Status, Receipt Date, Received By, Notes
fields) has no corresponding source anywhere under `qddssrc/` or
`qrpglesrc/` in this module — every other screen exercised during
Phase 2 testing had matching source; this one didn't. Whatever object
is actually running for it was not built from (or was never checked
into) this repo.

Before treating any live-reported bug as a source-level fix, confirm
the screen's source actually exists here first. If it doesn't, that's
its own finding — flag it rather than guessing at a fix, and don't
pull the live object's source off the IBM i host to fill the gap (see
the top-level environment rules); the gap itself needs a decision
(locate the real source, or write a new one into this repo) before
code work can start.
