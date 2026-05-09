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
