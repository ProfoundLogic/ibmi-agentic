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
