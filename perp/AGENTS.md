# Instructions for Agents — PERP module

This directory holds the **PreSales ERP (PERP)** module — a greenfield ERP
demo playground built alongside `cfdemo/` in this repo.

Tracked in Jira project **PERP**; design reference lives in Confluence page
[PreSales ERP Development Project (PERP)](https://profoundlogicsupport.atlassian.net/wiki/spaces/CPP/pages/2511470593/).

## Before touching anything

Read **[DDL_STYLE_GUIDE.md](DDL_STYLE_GUIDE.md)** first. It sets the conventions
for every table, index, view, and CL in this module. In particular:

- One `.table.sql` file per table, snake_case
- Explicit `FOR SYSTEM NAME` / `FOR COLUMN` on every table and column
- Standard audit block on every table (`created_at/by`, `updated_at/by`, `is_active`)
- FKs and CHECKs declared in DDL, not enforced in app code
- Composite PKs with `company_code` leading on multi-tenant tables
- All tables journaled from day one (see `qclsrc/perpjrn.clle`)
- No `SET OPTION COMMIT = *NONE` in RPG — real commitment control against the
  perp journal

## Runtime-only gotchas that clean compiles won't catch

Three gotchas on this environment routinely compile clean at severity 00
and then crash at runtime on the first exercise. Always factor these in
when adding a new RPG or SQLRPGLE program under `perp/qrpglesrc/`:

- **`ctl-opt datfmt(*iso)` is mandatory for any program that touches
  Date values.** The job DATFMT is `*MDY` (year range 1940-2039).
  Without the ctl-opt override every RPG Date variable inherits that
  narrow range as its storage format and any value outside it (a
  `0001-01-01` sentinel, an SQL fetch of an out-of-range date, a DSPF
  `L DATFMT(*ISO)` read) crashes with `RNQ0114`. Three-layer fix:
  - Put `datfmt(*iso)` on `ctl-opt`.
  - Parse every literal with `%date('yyyy-mm-dd' : *ISO)`.
  - Keep any Date value that will be assigned to an SQL host variable
    inside 1940-2039 (e.g. sentinels `1940-01-01`/`2039-12-31`) — the
    SQL precompiler generates its intermediate host vars with the JOB
    DATFMT and ignores `ctl-opt datfmt(*iso)`, so the SQL side stays
    capped at *MDY even after the first two fixes.

  Full analysis in `DDL_STYLE_GUIDE.md` §13. This bit every PO program
  during PERP-7 and burned three round-trips before the third layer
  landed — don't repeat.

- **`%subst(varchar : 1 : N)` fails when `N > %len(current data)`, not
  just when `N > declared max`.** A VARCHAR(20) column holding
  `'OPEN'` (4 chars) can't answer `%subst(..., 1, 10)` — `RNQ0100`.
  For "fit into a fixed-width display field", drop the `%subst` and
  use direct assignment: RPG right-pads or truncates automatically.
  Only reach for `%subst` when you actually need a middle slice, and
  guard with `%min(%len(...), N)` when you do.

- **`%editc(int : 'X')` produces hex, not decimal.** For displaying a
  numeric doc-number as a plain string (PO number, requisition
  number, line number), use `%char()`. `%editc` with `'X'` returns
  the hex representation of the internal byte pattern — PERP-7's
  first live browse rendered every PO number as `0000000000` before
  this was caught.

## Build target

Objects build into the **`PERPDEMO`** library on IBM i. `PERPDEMO` is a
shared demo library (not the per-task library) and has been created for this
purpose. When you build from this module, point `IBMI_BUILD_LIBRARY` at
`PERPDEMO` for these sources.

Source physical files in `PERPDEMO` (all `RCDLEN(112)`):
`QRPGLESRC`, `QDDSSRC`, `QSQLSRC`, `QCLSRC`, `QCMDSRC`, `QMENUSRC`, `QSRVSRC`,
`QPNLSRC`.

## Directory layout

| Subdir       | Holds                                                          |
|--------------|----------------------------------------------------------------|
| `qddlsrc/`   | SQL DDL — `.table.sql`, `.index.sql`, `.view.sql`, `.proc.sql` |
| `qrpglesrc/` | RPG ILE / SQLRPGLE programs and modules                        |
| `qddssrc/`   | DDS — `.pf`, `.lf`, `.dspf`, `.prtf` (5250 baseline only)      |
| `qclsrc/`    | CL / CLLE — library setup, journaling, hooks                   |
| `.codermake/config.json` | Module-scoped compile options (adds `runsqlstm`)   |
| `Rules.mk`   | Module build rules — always edit this, never the Makefile      |

## Building

Run `codermake` from the repo root as usual. The module-local
`.codermake/config.json` layers on top of the repo-level one and adds the
`runsqlstm` compile options used to build `.table.sql` / `.index.sql` /
`.view.sql` / `.proc.sql` sources into `PERPDEMO`.
