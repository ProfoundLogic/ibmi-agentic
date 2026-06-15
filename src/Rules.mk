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
hyr0600.pgm: hyr0600.rpgle hyrstubd.file
hyr0138.pgm: hyr0138.rpgle hyrstubd.file
hyr0606.pgm: hyr0606.rpgle hyrstubd.file
pickbatr.pgm: pickbatr.rpgle hyrstubd.file
pickerr.pgm: pickerr.rpgle hyrstubd.file
hyr6080.pgm: hyr6080.rpgle hyrstubd.file
