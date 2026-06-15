# Display files
hellod.file: hellod.dspf
menu.file: menu.dspf
wrkcustd.file: wrkcustd.dspf
wrkcust1d.file: wrkcust1d.dspf
inq01d.file: inq01d.dspf

# Rich Display Files
wrkcustdo.file: wrkcustdo.json
wrkcust1do.file: wrkcust1do.json

# Physical files
custp.file: custp.pf
ordhdrp.file: ordhdrp.pf
orddtlp.file: orddtlp.pf
prodp.file: prodp.pf
productsp.file: productsp.pf
categp.file: categp.pf
prodfeatp.file: prodfeatp.pf
featuresp.file: featuresp.pf

# Logical files
products1l.file: products1l.lf | productsp.file
products2l.file: products2l.lf | productsp.file

# Message file and menu
menu.msgf: menu.msgf
menu.menu: menu.file menu.msgf

# Simple program
hellor.pgm: hellor.rpgle hellod.file


# Simple COBOL program
tn510l.pgm: tn510l.cblle

# COBOL inquiry program
inq01l.pgm: inq01l.cblle inq01d.file | productsp.file products1l.file products2l.file categp.file prodfeatp.file featuresp.file

# Module and service program
custr.module: custr.sqlrpgle custr_pr.rpgle | custp.file
custr.srvpgm: custr.module custr.bnd

# Binding directory
cust.bnddir: cust.bnddir | custr.srvpgm

# Programs with service program and copy member dependencies
wrkcustr.pgm: wrkcustr.rpgle custr_pr.rpgle wrkcustd.file custr.srvpgm | cust.bnddir
wrkcust1r.pgm: wrkcust1r.rpgle custr_pr.rpgle wrkcust1d.file custr.srvpgm | cust.bnddir

# RPGOA Programs
wrkcustro.pgm: wrkcustro.rpgle custr_pr.rpgle wrkcustdo.file custr.srvpgm | cust.bnddir
wrkcust1ro.pgm: wrkcust1ro.rpgle custr_pr.rpgle wrkcust1do.file custr.srvpgm | cust.bnddir

# EJS Rich Display Files
wrkcusteo.file: wrkcusteo.json
wrkcust1eo.file: wrkcust1eo.json

# EJS RPGOA Programs
wrkcusteo.pgm: wrkcusteo.rpgle custr_pr.rpgle wrkcusteo.file custr.srvpgm | cust.bnddir
wrkcust1eo.pgm: wrkcust1eo.rpgle custr_pr.rpgle wrkcust1eo.file custr.srvpgm | cust.bnddir

# ----------------------------------------------------------------------------
# Hornady Shipping POC
# ----------------------------------------------------------------------------
# Shared stub display file used by every Hornady program stub.
hyrstubd.file: hyrstubd.dspf

# Hornady submenu (DSPF + MSGF -> *MENU).  The DSPF object must share the
# menu's name so CRTMNU TYPE(*DSPF) can find the record format by default.
hornady.file: hornady.dspf
hornady.msgf: hornady.msgf
hornady.menu: hornady.file hornady.msgf

# Hornady stub programs -- each launched from a Hornady-menu option.
# NOTE: hyr0600.pgm rule below has been retargeted to compile the real source.
hyr0138.pgm: hyr0138.rpgle hyrstubd.file
# hyr0606.pgm is now the REAL HYR0606 -- see block at the bottom of this file.
pickbatr.pgm: pickbatr.rpgle hyrstubd.file
pickerr.pgm: pickerr.rpgle hyrstubd.file
hyr6080.pgm: hyr6080.rpgle hyrstubd.file

# ----------------------------------------------------------------------------
# Hornady real data layer (reverse-engineered skeletons -- see
# Hornady/documentation/ddl/README.md for evidence and caveats)
# ----------------------------------------------------------------------------
fldref.file: fldref.pf
hypsgcu.file: hypsgcu.table.sql
hypsghd.file: hypsghd.table.sql
hypsgdt.file: hypsgdt.table.sql
hypsgld.file: hypsgld.table.sql
hypsgtd.file: hypsgtd.table.sql
hypsgsd.file: hypsgsd.table.sql

# Logical files over the shipment family (sourced from HornadyDemo;
# hylsgcu11 reverse-engineered since the original isn't in the package).
hylsgcu1.file: hylsgcu1.lf | hypsgcu.file
hylsgcu2.file: hylsgcu2.lf | hypsgcu.file
hylsgcu3.file: hylsgcu3.lf | hypsgcu.file
hylsgcu4.file: hylsgcu4.lf | hypsgcu.file
hylsgcu11.file: hylsgcu11.lf | hypsgcu.file
hylsgcu12.file: hylsgcu12.lf | hypsgcu.file
hylsgcu13.file: hylsgcu13.lf | hypsgcu.file
hylsgcu14.file: hylsgcu14.lf | hypsgcu.file
hylsgdt3.file: hylsgdt3.lf | hypsgdt.file

# Secondary external PFs (reverse-engineered minimal skeletons).
hypintr.file: hypintr.table.sql
hypscwk.file: hypscwk.table.sql
hypsgcv.file: hypsgcv.table.sql
hypsgtc.file: hypsgtc.table.sql
hypsgts.file: hypsgts.table.sql
hypsscc.file: hypsscc.table.sql
hypstrk.file: hypstrk.table.sql
hypsvct.file: hypsvct.table.sql
hyptdta.file: hyptdta.table.sql
hypidta.file: hypidta.table.sql
hypmilg.file: hypmilg.table.sql
hyppcls.file: hyppcls.table.sql
hyprclt.file: hyprclt.table.sql
hdcust.file: hdcust.table.sql
hdccmt.file: hdccmt.table.sql
hddshp.file: hddshp.table.sql
hdimst.file: hdimst.table.sql
hdiwhs.file: hdiwhs.table.sql
hdshpv.file: hdshpv.table.sql
oeorhd.file: oeorhd.table.sql
oeocmt.file: oeocmt.table.sql
oecmwk.file: oecmwk.table.sql
oedtwk.file: oedtwk.table.sql
oehdwk.file: oehdwk.table.sql
guptdat.file: guptdat.table.sql
hrempl.file: hrempl.table.sql
hyw0189.file: hyw0189.table.sql
hyw0610a.file: hyw0610a.table.sql

# Now wire the SSCC logical file (needed by sibling programs).
hylsscc8.file: hylsscc8.lf | hypsscc.file

# Additional tables for HYR0138 (Pallet Contents Maintenance).
hyppllt.file: hyppllt.table.sql
hypmpcd.file: hypmpcd.table.sql
hdctry.file: hdctry.table.sql
oebolh.file: oebolh.table.sql
hdpcls.file: hdpcls.table.sql
hyltemp2.file: hyltemp2.table.sql
hypwsoq.file: hypwsoq.table.sql
hyppmlg.file: hyppmlg.table.sql

# Logical files for HYR0138.
hylpllt1.file: hylpllt1.lf | hyppllt.file
hylsscc9.file: hylsscc9.lf | hypsscc.file
hylmpcd2.file: hylmpcd2.lf | hypmpcd.file
hrempl20.file: hrempl20.lf  | hrempl.file

# ----------------------------------------------------------------------------
# REAL HYR0606 -- Shipment Lot Inquiry (replaces the previous menu stub)
# ----------------------------------------------------------------------------
hyd0606.file: hyd0606.dspf | hypsgld.file hypsgdt.file
hylsgld1.file: hylsgld1.lf | hypsgld.file
hyr0606.bnddir: hyr0606.bnddir | hyr0600s.srvpgm
hyr0606.pgm: hyr0606.rpgle hyr0606_pr.rpgle hyd0606.file hyr0606.bnddir | hylsgld1.file hypsgdt.file hyr0600s.srvpgm
hyc0606.pgm: hyc0606.clle | hyr0606.pgm

# Real HYR0600 display files
hyd0600a.file: hyd0600a.dspf | hypsgcu.file
hyd0600b.file: hyd0600b.dspf | hypsgdt.file

# Stubs for service programs HYR0600 calls (real programs not in package)
hhdcss.module: hhdcss.rpgle
hhdsvi.module: hhdsvi.rpgle
hoedip.module: hoedip.rpgle
hhdcss.srvpgm: hhdcss.module
hhdsvi.srvpgm: hhdsvi.module
hoedip.srvpgm: hoedip.module

# Service program for BOLReq / RtvForeign / RtvCSPRO# / CompCustShip / Center.
hyr0600s.module: hyr0600s.rpgle
hyr0600s.srvpgm: hyr0600s.module

# Binding directory used by HYR0600's H-spec BNDDIR('HYR0600')
hyr0600.bnddir: hyr0600.bnddir | hyr0600s.srvpgm hhdcss.srvpgm hhdsvi.srvpgm hoedip.srvpgm

# Stubs for sibling Hornady programs HYR0600 chains into.  Named
# distinctly from the menu-option stubs (hyr0138 / hyr0606 etc) -- these
# are the EXTPGM targets of HYR0600's CALLPs.
hyr0602.pgm: hyr0602.rpgle
hyr0608.pgm: hyr0608.rpgle
hyr0189.pgm: hyr0189.rpgle
hyr0520.pgm: hyr0520.rpgle
hyc0138.pgm: hyc0138.clle
vprbldp.pgm: vprbldp.rpgle

# REAL HYR0600 -- the Hornady Shipment Processing program.  Replaces the
# earlier stub.  Pulls in all the data layer, display files, sibling
# program stubs, service program stubs, and the inline-prototype include.
hyr0600.pgm: hyr0600.sqlrpgle hyr0600_pr.rpgle hyd0600a.file hyd0600b.file hhdcss.srvpgm hhdsvi.srvpgm hoedip.srvpgm hyr0600s.srvpgm hyr0600.bnddir | hypsgcu.file hypsghd.file hypsgdt.file hypsgld.file hypsgtd.file hypsgsd.file hypintr.file hypscwk.file hypsgcv.file hypsgtc.file hypsgts.file hypsscc.file hypstrk.file hypsvct.file hyptdta.file hypidta.file hypmilg.file hyppcls.file hyprclt.file hdcust.file hdccmt.file hddshp.file hdimst.file hdiwhs.file hdshpv.file oeorhd.file oeocmt.file oecmwk.file oedtwk.file oehdwk.file guptdat.file hrempl.file hyw0189.file hyw0610a.file hylsgcu1.file hylsgcu2.file hylsgcu3.file hylsgcu4.file hylsgcu11.file hylsgcu12.file hylsgcu13.file hylsgcu14.file hylsgdt3.file hylsscc8.file hyr0602.pgm hyr0608.pgm hyr0189.pgm hyr0520.pgm hyc0138.pgm vprbldp.pgm
