# PreSales ERP (PERP) module build rules.
#
# All rules for the /perp module go here. Sources live under:
#   qddlsrc/  — SQL DDL (.table.sql, .index.sql, .view.sql, .proc.sql)
#   qrpglesrc/ — RPG / SQLRPGLE programs and modules
#   qddssrc/  — DSPF / PF / LF / PRTF for the 5250 baseline
#   qclsrc/   — CL / CLLE (library setup, journal, hooks)
#
# See perp/DDL_STYLE_GUIDE.md for the SQL DDL conventions this module uses.
#
# Naming: one .sql file per table, snake_case, extension .table.sql so
# codermake maps the source to CRTPF via RUNSQLSTM. Parent tables are declared
# as normal prerequisites of tables that FK to them so the build order is
# correct.

# --- SQL DDL --------------------------------------------------------------

# Style-guide reference table. Not a business table — kept in the build so
# that (a) the codermake .table.sql recipe is exercised on every build, and
# (b) the file can be picked up by new-table authors as a copy-paste starter.
#
# perpsjpf.pgm is declared as an order-only prereq so STRJRNPF is available
# for the post-create step below; the perp journal itself (PERPJRN) must
# already exist (created by CALL PERPJRN — see qclsrc/perpjrn.clle).
example_reference.file: qddlsrc/example_reference.table.sql | perpsjpf.pgm

# (First real business tables land under PERP-2 Company & System Reference.)


# --- CL setup -------------------------------------------------------------

# PERP journal + receiver bootstrap. Run once at library setup time.
perpjrn.pgm: qclsrc/perpjrn.clle

# STRJRNPF wrapper called after each SQL table create.
perpsjpf.pgm: qclsrc/perpsjpf.clle
