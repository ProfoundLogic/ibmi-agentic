# ---------------------------------------------------------------------------
# Pratt Industries "puitests" reconstruction
#
# Sources supplied by Pratt (Harel Davolt, 2 Oct 2026):
#   qddssrc/*.dspf      Profound UI Rich Display files (DDS + embedded widget JSON)
#   qrpglesrc/*.rpgle   PODTLUI / PMSELUI / POSELUI / PUISRCHUI / OMMGRCL
#   qclsrc/*.clle,*.clp entry-point CL
#   qgeniesrc/*.scn     Genie screen overlays for the OMMGR green screen
#
# Everything under qsqlsrc/ and the *.lf members are REVERSE ENGINEERED from
# field usage in those sources - Pratt did not send the database definitions.
# ---------------------------------------------------------------------------

# --- Base tables (modern SQL DDL) ------------------------------------------
coaddres.file:  qsqlsrc/coaddres.table.sql
customep.file:  qsqlsrc/customep.table.sql
custshpp.file:  qsqlsrc/custshpp.table.sql
help.file:      qsqlsrc/help.table.sql
locgeo.file:    qsqlsrc/locgeo.table.sql
locxref.file:   qsqlsrc/locxref.table.sql
mastsazz.file:  qsqlsrc/mastsazz.table.sql
mastspep.file:  qsqlsrc/mastspep.table.sql
orecomm.file:   qsqlsrc/orecomm.table.sql
orhdr.file:     qsqlsrc/orhdr.table.sql
orhtx.file:     qsqlsrc/orhtx.table.sql
oritm.file:     qsqlsrc/oritm.table.sql
oritx.file:     qsqlsrc/oritx.table.sql
ormsr.file:     qsqlsrc/ormsr.table.sql
puisrch.file:   qsqlsrc/puisrch.table.sql
sapinco.file:   qsqlsrc/sapinco.table.sql
sappohdcp.file: qsqlsrc/sappohdcp.table.sql
userids.file:   qsqlsrc/userids.table.sql
usrlevel.file:  qsqlsrc/usrlevel.table.sql
vendmst.file:   qsqlsrc/vendmst.table.sql
vendpablf.file: qsqlsrc/vendpablf.table.sql
vendpaco.file:  qsqlsrc/vendpaco.table.sql
vendpart.file:  qsqlsrc/vendpart.table.sql
vendsazz.file:  qsqlsrc/vendsazz.table.sql
warehous.file:  qsqlsrc/warehous.table.sql
wipjobs.file:   qsqlsrc/wipjobs.table.sql
wipship.file:   qsqlsrc/wipship.table.sql
xlslist.file:   qsqlsrc/xlslist.table.sql
zccity.file:    qsqlsrc/zccity.table.sql

# --- Views ------------------------------------------------------------------
coaddrname.file: qsqlsrc/coaddrname.view.sql qsqlsrc/coaddres.table.sql | coaddres.file

# --- Keyed access paths -----------------------------------------------------
# These stay as DDS logical files rather than SQL indexes: PODTLUI opens them for
# record level I/O and renames their record formats, which an SQL index cannot do.
orhdrl4.file:   qddssrc/orhdrl4.lf   qsqlsrc/orhdr.table.sql     | orhdr.file
oritml5.file:   qddssrc/oritml5.lf   qsqlsrc/oritm.table.sql     | oritm.file
ormsrr.file:    qddssrc/ormsrr.lf    qsqlsrc/ormsr.table.sql     | ormsr.file
orect.file:     qddssrc/orect.lf     qsqlsrc/orecomm.table.sql   | orecomm.file
vendpabl6.file: qddssrc/vendpabl6.lf qsqlsrc/vendpablf.table.sql | vendpablf.file
wipjobh.file:   qddssrc/wipjobh.lf   qsqlsrc/wipjobs.table.sql   | wipjobs.file
locxreflf.file: qddssrc/locxreflf.lf qsqlsrc/locxref.table.sql   | locxref.file

# --- Profound UI Rich Display files -----------------------------------------
# The .dspf members carry the widget JSON in HTML() keywords; the field level
# REFFLDs mean the referenced tables must exist before CRTDSPF runs.
puisrchui.file: qddssrc/puisrchui.dspf qsqlsrc/puisrch.table.sql \
                | puisrch.file
pmselui.file:   qddssrc/pmselui.dspf qsqlsrc/orhdr.table.sql qsqlsrc/ormsr.table.sql \
                qsqlsrc/puisrch.table.sql qsqlsrc/vendmst.table.sql \
                qsqlsrc/vendpart.table.sql qsqlsrc/xlslist.table.sql \
                | orhdr.file ormsr.file puisrch.file vendmst.file vendpart.file xlslist.file
poselui.file:   qddssrc/poselui.dspf qsqlsrc/orecomm.table.sql qsqlsrc/orhdr.table.sql \
                qsqlsrc/oritm.table.sql qsqlsrc/puisrch.table.sql \
                qsqlsrc/vendmst.table.sql qsqlsrc/xlslist.table.sql \
                | orecomm.file orhdr.file oritm.file puisrch.file vendmst.file xlslist.file
podtlui.file:   qddssrc/podtlui.dspf qsqlsrc/orecomm.table.sql qsqlsrc/orhdr.table.sql \
                qsqlsrc/orhtx.table.sql qsqlsrc/oritm.table.sql qsqlsrc/oritx.table.sql \
                qsqlsrc/vendpart.table.sql \
                | orecomm.file orhdr.file orhtx.file oritm.file oritx.file vendpart.file

# --- Application programs ---------------------------------------------------
# pmselui is shipped by Pratt as .rpgle but contains 12 EXEC SQL statements, so it
# is built here as .sqlrpgle (CRTSQLRPGI).  Building it with CRTBNDRPG fails.
puisrchui.pgm: qrpglesrc/puisrchui.sqlrpgle qddssrc/puisrchui.dspf \
               | puisrchui.file puisrch.file
poselui.pgm:   qrpglesrc/poselui.sqlrpgle qddssrc/poselui.dspf \
               | poselui.file orhdr.file oritm.file orecomm.file vendmst.file \
                 userids.file puisrch.file help.file xlslist.file
pmselui.pgm:   qrpglesrc/pmselui.sqlrpgle qddssrc/pmselui.dspf \
               | pmselui.file ormsr.file vendmst.file wipship.file \
                 userids.file puisrch.file help.file vendpart.file xlslist.file
podtlui.pgm:   qrpglesrc/podtlui.rpgle qddssrc/podtlui.dspf \
               | podtlui.file orhdrl4.file orhtx.file oritml5.file oritx.file \
                 vendpart.file vendpaco.file vendpabl6.file mastspep.file \
                 wipjobs.file wipship.file coaddres.file ormsrr.file orect.file \
                 vendmst.file sapinco.file warehous.file mastsazz.file \
                 custshpp.file vendsazz.file userids.file sappohdcp.file \
                 locxref.file locxreflf.file orecomm.file wipjobh.file zccity.file

# --- Reconstructed helper programs ------------------------------------------
# Called by Pratt's programs but not supplied in the bundle.  Contracts are taken
# verbatim from the DCL-PR prototypes in PODTLUI / POSELUI / PMSELUI.
sqlorder.pgm:   qrpglesrc/sqlorder.rpgle
usrrtv.pgm:     qrpglesrc/usrrtv.sqlrpgle     | usrlevel.file
rtvunn.pgm:     qrpglesrc/rtvunn.sqlrpgle     | userids.file
whusr.pgm:      qrpglesrc/whusr.sqlrpgle      | userids.file coaddres.file locgeo.file
rtvpotot.pgm:   qrpglesrc/rtvpotot.sqlrpgle   | oritm.file
rtvpocost.pgm:  qrpglesrc/rtvpocost.sqlrpgle  | oritm.file vendpart.file
rtvsapacct.pgm: qrpglesrc/rtvsapacct.sqlrpgle | vendpart.file coaddres.file
oilen.pgm:      qrpglesrc/oilen.sqlrpgle      | oritm.file
sql2xlsxd.pgm:  qrpglesrc/sql2xlsxd.sqlrpgle  | xlslist.file
lockmsg.pgm:    qrpglesrc/lockmsg.rpgle
appoprtcl.pgm:  qrpglesrc/appoprtcl.rpgle

# --- Reconstruction stubs ---------------------------------------------------
# These keep the hand-off buttons type safe and non-fatal.  OMMGRCLO in
# particular cannot be rebuilt: Pratt sent the RPG III source but not the OMMGR
# display file DDS, which is that program's primary WORKSTN file.
cxohpp.pgm:    qrpglesrc/cxohpp.rpgle
whsetcl.pgm:   qrpglesrc/whsetcl.rpgle
sappoint.pgm:  qrpglesrc/sappoint.rpgle
prhdtlui.pgm:  qrpglesrc/prhdtlui.rpgle
pmdtlui.pgm:   qrpglesrc/pmdtlui.rpgle
ohmgrclo.pgm:  qrpglesrc/ohmgrclo.rpgle
ommgrclo.pgm:  qrpglesrc/ommgrclo.rpgle
vndmgrcl.pgm:  qrpglesrc/vndmgrcl.rpgle
mgrcl.pgm:     qrpglesrc/mgrcl.rpgle
vpmgrcl.pgm:   qrpglesrc/vpmgrcl.rpgle
whmgrclo.pgm:  qrpglesrc/whmgrclo.rpgle
whselui.pgm:   qrpglesrc/whselui.rpgle
wpmgrclo.pgm:  qrpglesrc/wpmgrclo.rpgle

# --- Entry points -----------------------------------------------------------
poappuicl.pgm: qclsrc/poappuicl.clle | poselui.pgm podtlui.pgm rtvunn.pgm whusr.pgm
pmappuicl.pgm: qclsrc/pmappuicl.clle | pmselui.pgm pmdtlui.pgm podtlui.pgm rtvunn.pgm whusr.pgm
prattpo.pgm:   qclsrc/prattpo.clle   | poappuicl.pgm
prattreq.pgm:  qclsrc/prattreq.clle  | pmappuicl.pgm
