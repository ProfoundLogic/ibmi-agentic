# Display files
hellod.file: hellod.dspf
menu.file: menu.dspf
wrkcustd.file: wrkcustd.dspf
wrkcust1d.file: wrkcust1d.dspf

# Rich Display Files
wrkcustdo.file: wrkcustdo.json
wrkcust1do.file: wrkcust1do.json

# Physical files
custp.file: custp.pf
ordhdrp.file: ordhdrp.pf
orddtlp.file: orddtlp.pf
prodp.file: prodp.pf

# Message file and menu
menu.msgf: menu.msgf
menu.menu: menu.file menu.msgf

# Simple program
hellor.pgm: hellor.rpgle hellod.file

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
