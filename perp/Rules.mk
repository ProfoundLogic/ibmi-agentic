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


# --- PERP-20: Inventory Master tables --------------------------------------
# Core item master. FK order: uom and item_class (both FK company) before
# item; item_uom_conversion and item_lot FK item + uom.
uom.file:                 qddlsrc/uom.table.sql                                       | perpsjpf.pgm
item_class.file:          qddlsrc/item_class.table.sql          company.file          | perpsjpf.pgm
item.file:                qddlsrc/item.table.sql                company.file item_class.file uom.file | perpsjpf.pgm
item_uom_conversion.file: qddlsrc/item_uom_conversion.table.sql item.file uom.file     | perpsjpf.pgm
item_lot.file:            qddlsrc/item_lot.table.sql             item.file             | perpsjpf.pgm


# --- PERP-25: 3D Warehouse Map — warehouse_layout table -------------------
# One row per company; FKs company. Drives coordinate derivation in whcoord
# (PERP-27) and the maintenance screen in wlmr (PERP-26).
warehouse_layout.file: qddlsrc/warehouse_layout.table.sql company.file | perpsjpf.pgm


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


# --- PERP-21: UOM & UOM conversion maintenance ----------------------------
wrkuomd.file: qddssrc/wrkuomd.dspf
wrkuomr.pgm:  qrpglesrc/wrkuomr.sqlrpgle qddssrc/wrkuomd.dspf | wrkuomd.file uom.file

# Scoped by *LDA company (perpselr) + item number entered on screen.
wrkcnvd.file: qddssrc/wrkcnvd.dspf
wrkcnvr.pgm:  qrpglesrc/wrkcnvr.sqlrpgle qddssrc/wrkcnvd.dspf | wrkcnvd.file item_uom_conversion.file


# --- PERP-22: Item class maintenance --------------------------------------
# Scoped by *LDA company (perpselr).
wrkicld.file: qddssrc/wrkicld.dspf
wrkiclr.pgm:  qrpglesrc/wrkiclr.sqlrpgle qddssrc/wrkicld.dspf | wrkicld.file item_class.file


# --- PERP-23: Item master maintenance --------------------------------------
# Scoped by *LDA company (perpselr). Option 6 on the subfile calls wrkcnvr
# pre-scoped to the selected item (dynamic CALL via EXTPGM, not compile-time
# bound -- wrkcnvr.pgm listed as order-only so build order still makes sense).
wrkitmd.file: qddssrc/wrkitmd.dspf
wrkitmr.pgm:  qrpglesrc/wrkitmr.sqlrpgle qddssrc/wrkitmd.dspf | wrkitmd.file item.file wrkcnvr.pgm wrklotr.pgm


# --- PERP-24: Item lot maintenance & inquiry --------------------------------
# Scoped by *LDA company (perpselr) + item number entered on screen, same
# idiom as wrkcnvr. Discrepancy indicator: item.qty_on_hand vs
# SUM(item_lot.qty_on_hand) for the scoped item.
wrklotd.file: qddssrc/wrklotd.dspf
wrklotr.pgm:  qrpglesrc/wrklotr.sqlrpgle qddssrc/wrklotd.dspf | wrklotd.file item_lot.file


# --- PERP-27: Warehouse coordinate query service --------------------------
# Iterator service program: located items in a company joined with x/y/z
# coordinates derived from warehouse_layout. Module + srvpgm + bnddir.
whcoord.module: qrpglesrc/whcoord.sqlrpgle qrpglesrc/whcoord_pr.rpgle | item.file warehouse_layout.file
whcoord.srvpgm: whcoord.module qsrvsrc/whcoord.bnd

# Smoke-test caller -- CALL PERPDEMO/WHCOORDSMK PARM('ACM').
whcoordsmk.pgm: qrpglesrc/whcoordsmk.sqlrpgle qrpglesrc/whcoord_pr.rpgle whcoord.srvpgm | perp.bnddir item.file warehouse_layout.file


# --- PERP-26: Warehouse layout maintenance --------------------------------
# Single-record display + edit of warehouse_layout, scoped by *LDA company
# (perpselr). No subfile list -- one row per company.
wlmd.file: qddssrc/wlmd.dspf
wlmr.pgm:  qrpglesrc/wlmr.sqlrpgle qddssrc/wlmd.dspf | wlmd.file warehouse_layout.file


# --- PERP-28: Vendor, Item-Vendor, Pricing tables --------------------------
# FK order: vendor (company, perp_user, code_master) before item_vendor
# (item, vendor) before item_vendor_price (item_vendor, code_master).
# The partial-unique preferred-vendor index is a separate .index.sql object,
# a normal (not order-only) prereq of item_vendor so it always rebuilds
# alongside the table it indexes.
vendor.file:                   qddlsrc/vendor.table.sql            company.file perp_user.file code_master.file | perpsjpf.pgm
item_vendor.file:              qddlsrc/item_vendor.table.sql       item.file vendor.file                        | perpsjpf.pgm
item_vendor_preferred_ak.file: qddlsrc/item_vendor_preferred_ak.index.sql item_vendor.file
item_vendor_price.file:        qddlsrc/item_vendor_price.table.sql item_vendor.file code_master.file            | perpsjpf.pgm


# --- PERP-29: Vendor master maintenance -----------------------------------
# Scoped by *LDA company (perpselr). Subfile filters by active-only and
# buyer_code.
wrkvndd.file: qddssrc/wrkvndd.dspf
wrkvndr.pgm:  qrpglesrc/wrkvndr.sqlrpgle qddssrc/wrkvndd.dspf | wrkvndd.file vendor.file perp_user.file code_master.file


# --- PERP-30: Item-vendor profile maintenance -----------------------------
# Scoped by *LDA company (perpselr) plus an item OR vendor entered on
# screen (item wins if both are entered).
wrkivnd.file: qddssrc/wrkivnd.dspf
wrkivnr.pgm:  qrpglesrc/wrkivnr.sqlrpgle qddssrc/wrkivnd.dspf | wrkivnd.file item_vendor.file


# --- PERP-31: Item-vendor price maintenance (effective-dated) --------------
# Scoped by *LDA company (perpselr) plus an item AND vendor entered on
# screen. Read-only history list; F6=Add closes the current row and
# inserts a new one dated today.
wrkivpd.file: qddssrc/wrkivpd.dspf
wrkivpr.pgm:  qrpglesrc/wrkivpr.sqlrpgle qddssrc/wrkivpd.dspf | wrkivpd.file item_vendor_price.file


# --- PERP-32: Pricing history query service --------------------------------
# View joins item_vendor_price + vendor; module/srvpgm/bnddir/prototype
# follow the docseq (PERP-19) pattern. Smoke-test caller proves binding.
item_vendor_price_history.file: qddlsrc/item_vendor_price_history.view.sql item_vendor_price.file vendor.file
itmvprcq.module: qrpglesrc/itmvprcq.sqlrpgle qrpglesrc/itmvprcq_pr.rpgle | item_vendor_price_history.file
itmvprcq.srvpgm: itmvprcq.module qsrvsrc/itmvprcq.bnd
# perp.bnddir target already declared above (PERP-19 docseq section);
# adding a new addbnddire entry there for itmvprcq is enough.

# Smoke-test caller for itmvprcq -- CALL PERPDEMO/IVPRCQSMK PARM('ACM' 'WIDGET1').
# Named ivprcqsmk, not itmvprcqsmk (11 chars) -- IBM i object names cap at 10.
ivprcqsmk.pgm: qrpglesrc/ivprcqsmk.sqlrpgle qrpglesrc/itmvprcq_pr.rpgle itmvprcq.srvpgm | perp.bnddir item_vendor_price_history.file


# --- PERP-6: Requisitioning — requisition_header / requisition_line -------
# FK order: requisition_header (company, perp_user, code_master -- all
# already built in PERP-2) before requisition_line (requisition_header,
# item, uom).
requisition_header.file: qddlsrc/requisition_header.table.sql company.file perp_user.file code_master.file | perpsjpf.pgm
requisition_line.file:   qddlsrc/requisition_line.table.sql   requisition_header.file item.file uom.file    | perpsjpf.pgm

# Requisition entry program (header + line subfile). Calls docseq_next('REQ')
# for numbering; defaults line UOM from item, est_unit_cost from the
# preferred vendor's current item_vendor_price row.
reqentd.file: qddssrc/reqentd.dspf
reqentr.pgm:  qrpglesrc/reqentr.sqlrpgle qrpglesrc/docseq_pr.rpgle qddssrc/reqentd.dspf | reqentd.file perp.bnddir requisition_header.file requisition_line.file item.file item_vendor.file item_vendor_price.file uom.file perp_user.file

# Requisition approval program (list of SUBMITTED reqs -> detail w/ up to
# 6 lines as plain fields + confidence badge -> Approve/Reject stamping
# approved_by/approved_at/approval_source=HUMAN/approval_notes). Detail
# is a single plain (non-subfile) record in this SAME file/program --
# two earlier designs (a second SFLCTL subfile in this file; a separate
# called program with its own device file) both crashed at runtime with
# "Session or device error" the moment the detail screen was reached,
# even though both compiled clean and matched seemingly-reasonable RPG
# patterns. This shape -- one subfile (ASFL) plus plain output/entry
# fields, all in one program -- is the simplest one that's actually
# proven not to crash (matches reqentr's own header/line-edit panels).
reqaprd.file: qddssrc/reqaprd.dspf
reqaprr.pgm:  qrpglesrc/reqaprr.sqlrpgle qddssrc/reqaprd.dspf | reqaprd.file requisition_header.file requisition_line.file

# CoderFlow auto-approval hook. Skeleton service program: no-op scorer,
# reads company_config thresholds, auto-approves (status/approved_by=
# CODERFLOW/approved_at/approval_source_code=CODERFLOW/approval_notes)
# when both the confidence and total-cost thresholds pass. Module +
# srvpgm + bnddir + prototype, same pattern as docseq (PERP-19).
reqauto.module: qrpglesrc/reqauto.sqlrpgle qrpglesrc/reqauto_pr.rpgle | requisition_header.file company_config.file
reqauto.srvpgm: reqauto.module qsrvsrc/reqauto.bnd
# perp.bnddir target already declared above (PERP-19 docseq section);
# adding a new addbnddire entry there for reqauto is enough.

# Smoke-test caller for reqauto -- CALL PERPDEMO/REQAUTOSMK PARM('ACM' '3       ').
reqautosmk.pgm: qrpglesrc/reqautosmk.sqlrpgle qrpglesrc/reqauto_pr.rpgle reqauto.srvpgm | perp.bnddir requisition_header.file


# --- PERP-7: Purchasing — po_header / po_line / po_line_schedule ---------
# FK order: po_header (company, vendor, perp_user (buyer), code_master)
# before po_line (po_header, item, uom, code_master, requisition_line for
# optional back-link) before po_line_schedule (po_line). The po_line_open
# view (ordered_qty - received_qty computed via view rather than a stored
# GENERATED ALWAYS AS column — see DDL_STYLE_GUIDE.md Sec.9) hangs off
# po_line as a normal (not order-only) prereq so it always rebuilds
# alongside its base table.
po_header.file:        qddlsrc/po_header.table.sql       company.file vendor.file perp_user.file code_master.file | perpsjpf.pgm
po_line.file:          qddlsrc/po_line.table.sql         po_header.file item.file uom.file code_master.file requisition_line.file | perpsjpf.pgm
po_line_schedule.file: qddlsrc/po_line_schedule.table.sql po_line.file                                                              | perpsjpf.pgm
po_line_open.file:     qddlsrc/po_line_open.view.sql     po_line.file

# Manual PO entry program (header + line subfile). Calls docseq_next('PO')
# for numbering; snapshots buyer_code from vendor at header commit;
# defaults line UOM from item, unit_price from the entered vendor's
# current item_vendor_price row. Same shape as reqentr (PERP-34).
poentd.file: qddssrc/poentd.dspf
poentr.pgm:  qrpglesrc/poentr.sqlrpgle qrpglesrc/docseq_pr.rpgle qddssrc/poentd.dspf | poentd.file perp.bnddir po_header.file po_line.file item.file item_vendor.file item_vendor_price.file uom.file vendor.file perp_user.file

# PO from requisition (consolidate/split). Selects APPROVED requisitions,
# groups their lines by preferred vendor (one PO per vendor -> splitting),
# consolidates multiple selected reqs into a single PO per vendor. Each
# generated po_line carries source_requisition_number +
# source_requisition_line_number so "converted" status is derivable by
# joining po_line back to requisition_line's PK. Approved reqs flip to
# CONVERTED once all their lines have been placed on a PO.
poreqd.file: qddssrc/poreqd.dspf
poreqr.pgm:  qrpglesrc/poreqr.sqlrpgle qrpglesrc/docseq_pr.rpgle qddssrc/poreqd.dspf | poreqd.file perp.bnddir po_header.file po_line.file requisition_header.file requisition_line.file item_vendor.file item_vendor_price.file vendor.file

# Blanket PO schedule maintenance. Subfile of po_line_schedule rows for
# a given (company, po_number, line_number). Add/Change/Delete rows.
# Warn (not block) when total scheduled_qty exceeds po_line.ordered_qty.
poschd.file: qddssrc/poschd.dspf
poschr.pgm:  qrpglesrc/poschr.sqlrpgle qddssrc/poschd.dspf | poschd.file po_line.file po_line_schedule.file

# PO browse & inquiry. Filterable subfile of PO headers (status, vendor,
# buyer, order-date range); Option 5 drills to detail (header + all
# lines, joined to po_line_open for open_qty + extended_price); Option 8
# on a line drills to source requisition (if any) + schedule (if blanket).
pobrwd.file: qddssrc/pobrwd.dspf
pobrwr.pgm:  qrpglesrc/pobrwr.sqlrpgle qddssrc/pobrwd.dspf | pobrwd.file po_header.file po_line.file po_line_open.file po_line_schedule.file requisition_line.file vendor.file perp_user.file


# --- PERP menus (glue for exploratory verification) -----------------------
# GO PERPDEMO/PERPMNU is the single entry point. PERPMNU itself only holds
# "Select company" + one option per child menu + Sign off -- the child
# menus group the actual maintenance/diagnostic programs so the top level
# doesn't grow one line per story as new epics land (PERP-6/7/8
# Requisitioning/Purchasing/Receiving will each get their own child menu
# under PERPMNU the same way, instead of more top-level numbers).
#
# .file MUST be a normal prereq (not order-only) or codermake silently drops
# the CRTMNU recipe. See DDL_STYLE_GUIDE § "codermake menu gotcha" -- this
# bit cfdemo/menu.menu too (fixed there in the same commit as this section).

# PERP-16/17/18: company selector + system maintenance (code_master, users)
perpsysm.file: qddssrc/perpsysm.dspf
perpsysm.msgf: perpsysm.msgf
perpsysm.menu: perpsysm.msgf perpsysm.file | wrkcmr.pgm wrkusrr.pgm

# PERP-20/21/22/23/24/26: inventory master data (+ warehouse layout, PERP-4)
perpinvm.file: qddssrc/perpinvm.dspf
perpinvm.msgf: perpinvm.msgf
perpinvm.menu: perpinvm.msgf perpinvm.file | wrkuomr.pgm wrkcnvr.pgm wrkiclr.pgm wrkitmr.pgm wrklotr.pgm wlmr.pgm

# PERP-28/29/30/31: vendor & pricing master data
perpvndm.file: qddssrc/perpvndm.dspf
perpvndm.msgf: perpvndm.msgf
perpvndm.menu: perpvndm.msgf perpvndm.file | wrkvndr.pgm wrkivnr.pgm wrkivpr.pgm

# PERP-19/27/32: service-program smoke testers
perpdiag.file: qddssrc/perpdiag.dspf
perpdiag.msgf: perpdiag.msgf
perpdiag.menu: perpdiag.msgf perpdiag.file | docseqsmk.pgm ivprcqsmk.pgm whcoordsmk.pgm reqautosmk.pgm

# PERP-6/7/8: Requisitioning / Purchasing / Receiving each get their own
# child menu under PERPMNU (see codermake menu note below). PERP-34 adds
# option 1 (reqentr); PERP-35 adds option 2 (reqaprr) once it lands.
perpreqm.file: qddssrc/perpreqm.dspf
perpreqm.msgf: perpreqm.msgf
perpreqm.menu: perpreqm.msgf perpreqm.file | reqentr.pgm reqaprr.pgm

# PERP-7 Purchasing child menu. Options: 1 manual PO (poentr), 2 PO from
# requisition (poreqr), 3 blanket schedule (poschr), 4 browse/inquiry
# (pobrwr).
perppom.file: qddssrc/perppom.dspf
perppom.msgf: perppom.msgf
perppom.menu: perppom.msgf perppom.file | poentr.pgm poreqr.pgm poschr.pgm pobrwr.pgm

# Top-level menu. Order-only on perpselr.pgm (called directly) and on the
# 6 child .menu targets (routed to via GO PERPDEMO/<name>, not CALLed).
perpmnu.file: qddssrc/perpmnu.dspf
perpmnu.msgf: perpmnu.msgf
perpmnu.menu: perpmnu.msgf perpmnu.file | perpselr.pgm perpsysm.menu perpinvm.menu perpvndm.menu perpdiag.menu perpreqm.menu perppom.menu


# --- CL setup -------------------------------------------------------------

# PERP journal + receiver bootstrap. Run once at library setup time.
perpjrn.pgm: qclsrc/perpjrn.clle

# STRJRNPF wrapper called after each SQL table create.
perpsjpf.pgm: qclsrc/perpsjpf.clle
