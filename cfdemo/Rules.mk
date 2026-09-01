# Display files
hellod.file: qddssrc/hellod.dspf
menu.file: qddssrc/menu.dspf
wrkcustd.file: qddssrc/wrkcustd.dspf
wrkcust1d.file: qddssrc/wrkcust1d.dspf
inq01d.file: qddssrc/inq01d.dspf

# Rich Display Files
wrkcustdo.file: qddssrc/wrkcustdo.json
wrkcust1do.file: qddssrc/wrkcust1do.json

# Physical files
custp.file: qddssrc/custp.pf
ordhdrp.file: qddssrc/ordhdrp.pf
orddtlp.file: qddssrc/orddtlp.pf
prodp.file: qddssrc/prodp.pf
productsp.file: qddssrc/productsp.pf
categp.file: qddssrc/categp.pf
prodfeatp.file: qddssrc/prodfeatp.pf
featuresp.file: qddssrc/featuresp.pf

# --- J.H. Fletcher demo: physical files -------------------------------------
flcustp.file: qddssrc/flcustp.pf
flsitep.file: qddssrc/flsitep.pf
flmodlp.file: qddssrc/flmodlp.pf
flmachp.file: qddssrc/flmachp.pf
floptnp.file: qddssrc/floptnp.pf
flpartp.file: qddssrc/flpartp.pf
flbomp.file:  qddssrc/flbomp.pf
flsvcp.file:  qddssrc/flsvcp.pf
flpordp.file: qddssrc/flpordp.pf

# Logical files
products1l.file: qddssrc/products1l.lf | productsp.file
products2l.file: qddssrc/products2l.lf | productsp.file

# Message file and menu
menu.msgf: menu.msgf
# menu.file must be a NORMAL prerequisite: the .menu pattern rule only matches
# when both .msgf and .file are normal, and as order-only the recipe is
# silently dropped - codermake reports success and builds no *MENU object.
menu.menu: menu.msgf menu.file

# --- J.H. Fletcher demo: menu ------------------------------------------------
# NOTE: the .menu pattern rule only matches when BOTH the .msgf and the .file
# are NORMAL prerequisites. Making .file order-only (after a |) silently drops
# the recipe - see the existing menu.menu rule above.
fltmenu.file: qddssrc/fltmenu.dspf
fltmenu.msgf: fltmenu.msgf
fltmenu.menu: fltmenu.msgf fltmenu.file

# Simple program
hellor.pgm: qrpglesrc/hellor.rpgle qddssrc/hellod.dspf | hellod.file

# Simple COBOL program
tn510l.pgm: qcbllesrc/tn510l.cblle

# COBOL inquiry program
inq01l.pgm: qcbllesrc/inq01l.cblle qddssrc/inq01d.dspf qddssrc/productsp.pf qddssrc/products1l.lf qddssrc/products2l.lf qddssrc/categp.pf qddssrc/prodfeatp.pf qddssrc/featuresp.pf | inq01d.file productsp.file products1l.file products2l.file categp.file prodfeatp.file featuresp.file

# Module and service program
custr.module: qrpglesrc/custr.sqlrpgle qrpglesrc/custr_pr.rpgle qddssrc/custp.pf | custp.file
custr.srvpgm: custr.module qsrvsrc/custr.bnd

# --- J.H. Fletcher demo: data access service program -------------------------
fldata.module: qrpglesrc/fldata.sqlrpgle qrpglesrc/fldata_pr.rpgle | flcustp.file flsitep.file flmodlp.file flmachp.file floptnp.file flpartp.file flbomp.file flsvcp.file flpordp.file
fldata.srvpgm: fldata.module qsrvsrc/fldata.bnd
fletcher.bnddir: fletcher.bnddir

# Binding directory
cust.bnddir: cust.bnddir

# Programs with service program and copy member dependencies
wrkcustr.pgm: qrpglesrc/wrkcustr.rpgle qddssrc/wrkcustd.dspf qrpglesrc/custr_pr.rpgle custr.srvpgm | wrkcustd.file cust.bnddir
wrkcust1r.pgm: qrpglesrc/wrkcust1r.rpgle qddssrc/wrkcust1d.dspf qrpglesrc/custr_pr.rpgle custr.srvpgm | wrkcust1d.file cust.bnddir

# RPGOA Programs
wrkcustro.pgm: qrpglesrc/wrkcustro.rpgle qddssrc/wrkcustdo.json qrpglesrc/custr_pr.rpgle custr.srvpgm | wrkcustdo.file cust.bnddir
wrkcust1ro.pgm: qrpglesrc//wrkcust1ro.rpgle qddssrc/wrkcust1do.json qrpglesrc/custr_pr.rpgle custr.srvpgm | wrkcust1do.file cust.bnddir

# EJS Rich Display Files
wrkcusteo.file: qddssrc/wrkcusteo.json
wrkcust1eo.file: qddssrc/wrkcust1eo.json

# EJS RPGOA Programs
wrkcusteo.pgm: qrpglesrc/wrkcusteo.rpgle qddssrc/wrkcusteo.json qrpglesrc/custr_pr.rpgle custr.srvpgm | wrkcusteo.file cust.bnddir
wrkcust1eo.pgm: qrpglesrc/wrkcust1eo.rpgle qddssrc/wrkcust1eo.json qrpglesrc/custr_pr.rpgle custr.srvpgm | wrkcust1eo.file cust.bnddir

# --- J.H. Fletcher demo: EJS rich display files ------------------------------
flfleeteo.file:  qddssrc/flfleeteo.json
flcusteo.file:   qddssrc/flcusteo.json
flfleet1eo.file: qddssrc/flfleet1eo.json
flparteo.file:   qddssrc/flparteo.json
flmacheo.file:   qddssrc/flmacheo.json
flpart1eo.file:  qddssrc/flpart1eo.json

# --- J.H. Fletcher demo: EJS RPGOA programs ----------------------------------
flfleeteo.pgm:  qrpglesrc/flfleeteo.rpgle  qddssrc/flfleeteo.json  qrpglesrc/fldata_pr.rpgle fldata.srvpgm | flfleeteo.file  fletcher.bnddir
flfleet1eo.pgm: qrpglesrc/flfleet1eo.rpgle qddssrc/flfleet1eo.json qrpglesrc/fldata_pr.rpgle fldata.srvpgm | flfleet1eo.file fletcher.bnddir
flparteo.pgm:   qrpglesrc/flparteo.rpgle   qddssrc/flparteo.json   qrpglesrc/fldata_pr.rpgle fldata.srvpgm | flparteo.file   fletcher.bnddir
flpart1eo.pgm:  qrpglesrc/flpart1eo.rpgle  qddssrc/flpart1eo.json  qrpglesrc/fldata_pr.rpgle fldata.srvpgm | flpart1eo.file  fletcher.bnddir
flmacheo.pgm:   qrpglesrc/flmacheo.rpgle   qddssrc/flmacheo.json   qrpglesrc/fldata_pr.rpgle fldata.srvpgm | flmacheo.file   fletcher.bnddir
flcusteo.pgm:   qrpglesrc/flcusteo.rpgle   qddssrc/flcusteo.json   qrpglesrc/fldata_pr.rpgle fldata.srvpgm | flcusteo.file   fletcher.bnddir
