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

# --- PERP-2: Company & System Reference tables ---------------------------
# Multi-tenant root + generic lookup + per-tenant configuration.
# FK order: company is created first; document_sequence, company_config,
# and perp_user follow (perp_user FKs code_master(USERROLE), so
# code_master is a normal prereq).
company.file:           qddlsrc/company.table.sql           | perpsjpf.pgm
code_master.file:       qddlsrc/code_master.table.sql       | perpsjpf.pgm
document_sequence.file: qddlsrc/document_sequence.table.sql company.file     | perpsjpf.pgm
company_config.file:    qddlsrc/company_config.table.sql    company.file     | perpsjpf.pgm
perp_user.file:         qddlsrc/perp_user.table.sql         code_master.file | perpsjpf.pgm


# --- PERP-19: Document sequence service ----------------------------------
# Atomic per-(company, doc_type) sequence allocator. Module + srvpgm + bnddir.
docseq.module: qrpglesrc/docseq.sqlrpgle qrpglesrc/docseq_pr.rpgle | company_config.file document_sequence.file
docseq.srvpgm: docseq.module qsrvsrc/docseq.bnd
perp.bnddir:   perp.bnddir

# Smoke-test caller for docseq — CALL PERPDEMO/DOCSEQSMK PARM('ACM' 'PO ').
docseqsmk.pgm: qrpglesrc/docseqsmk.sqlrpgle qrpglesrc/docseq_pr.rpgle docseq.srvpgm | perp.bnddir document_sequence.file


# --- PERP-16: Company selection utility ----------------------------------
# Writes the picked company code to *LDA[1:3] for downstream PERP programs.
perpseld.file: qddssrc/perpseld.dspf
perpselr.pgm:  qrpglesrc/perpselr.sqlrpgle qddssrc/perpseld.dspf | perpseld.file company.file


# --- PERP-17: Code-master maintenance program ----------------------------
wrkcmd.file: qddssrc/wrkcmd.dspf
wrkcmr.pgm:  qrpglesrc/wrkcmr.sqlrpgle qddssrc/wrkcmd.dspf | wrkcmd.file code_master.file


# --- PERP-18: perp_user maintenance program ------------------------------
wrkusrd.file: qddssrc/wrkusrd.dspf
wrkusrr.pgm:  qrpglesrc/wrkusrr.sqlrpgle qddssrc/wrkusrd.dspf | wrkusrd.file perp_user.file code_master.file


# --- PERP main menu (glue for exploratory verification) ------------------
# Ties the PERP-16/17/18/19 programs together into a single 5250 menu:
#   GO PERPDEMO/PERPMNU
perpmnu.file: qddssrc/perpmnu.dspf
perpmnu.msgf: perpmnu.msgf
# .file MUST be a normal prereq (not order-only) or codermake silently drops
# the CRTMNU recipe. See DDL_STYLE_GUIDE § "codermake menu gotcha".
perpmnu.menu: perpmnu.msgf perpmnu.file | perpselr.pgm wrkcmr.pgm wrkusrr.pgm docseqsmk.pgm


# --- CL setup -------------------------------------------------------------

# PERP journal + receiver bootstrap. Run once at library setup time.
perpjrn.pgm: qclsrc/perpjrn.clle

# STRJRNPF wrapper called after each SQL table create.
perpsjpf.pgm: qclsrc/perpsjpf.clle
