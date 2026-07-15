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
