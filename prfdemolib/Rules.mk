# PRFDEMOLIB - JDA/MMS retail inquiry demo (INV046, INV502, TS0110)
#
# Source imported from IBM i library PRFDEMOLIB. The data files shipped in
# PRFDEMOLIB as objects only; their DDS in qddssrc/*.pf and the non-join
# qddssrc/*.lf were reconstructed by tools/reconstruct-dds.py and carry the
# same record format level identifiers as the shipped objects.
#
# codermake builds the files EMPTY. After building, load the sample data and
# create the runtime support objects (MBRSEQNO, IMSJOB1, STR001WK MAXMBRS):
#     prfdemolib/tools/load-prfdemo-data.sh <LIBRARY>
#
# Not built on purpose: qclsrc/prfdup01-03 and prfload01-03 are the vendor's
# internal build utilities and reference their production libraries.

# ---------------------------------------------------------------------------
# Physical files (reconstructed DDS)
# ---------------------------------------------------------------------------
apsupp.file: qddssrc/apsupp.pf
dptdiv.file: qddssrc/dptdiv.pf
invadi.file: qddssrc/invadi.pf
invbal.file: qddssrc/invbal.pf
invbal1.file: qddssrc/invbal1.pf
invcbl.file: qddssrc/invcbl.pf
invdpt.file: qddssrc/invdpt.pf
invfsd.file: qddssrc/invfsd.pf
invmst.file: qddssrc/invmst.pf
invupc.file: qddssrc/invupc.pf
invven.file: qddssrc/invven.pf
itmrtl.file: qddssrc/itmrtl.pf
jdaref.file: qddssrc/jdaref.pf
menuctl.file: qddssrc/menuctl.pf
options.file: qddssrc/options.pf
optstr.file: qddssrc/optstr.pf
opttab.file: qddssrc/opttab.pf
orderbk.file: qddssrc/orderbk.pf
phpf.file: qddssrc/phpf.pf
phpfb1.file: qddssrc/phpfb1.pf
phpfb2.file: qddssrc/phpfb2.pf
phpfw2.file: qddssrc/phpfw2.pf
pomdtl.file: qddssrc/pomdtl.pf
pomhdr.file: qddssrc/pomhdr.pf
pomrcd.file: qddssrc/pomrcd.pf
rplprf.file: qddssrc/rplprf.pf
skvaux.file: qddssrc/skvaux.pf
str001wk.file: qddssrc/str001wk.pf
sys001ed.file: qddssrc/sys001ed.pf
tbl800.file: qddssrc/tbl800.pf
tbladi.file: qddssrc/tbladi.pf
tblbyr.file: qddssrc/tblbyr.pf
tbldst.file: qddssrc/tbldst.pf
tblfld.file: qddssrc/tblfld.pf
tbligd.file: qddssrc/tbligd.pf
tbligh.file: qddssrc/tbligh.pf
tblreg.file: qddssrc/tblreg.pf
tblsea.file: qddssrc/tblsea.pf
tblstr.file: qddssrc/tblstr.pf
trfdtl.file: qddssrc/trfdtl.pf
trfzst.file: qddssrc/trfzst.pf

# ---------------------------------------------------------------------------
# Logical files
# ---------------------------------------------------------------------------
invbalz1.file: qddssrc/invbalz1.lf qddssrc/invbal.pf | invbal.file
invmstz3.file: qddssrc/invmstz3.lf qddssrc/invmst.pf | invmst.file
orderbk1.file: qddssrc/orderbk1.lf qddssrc/orderbk.pf | orderbk.file
tblfldl1.file: qddssrc/tblfldl1.lf qddssrc/tblfld.pf | tblfld.file
tblstrl1.file: qddssrc/tblstrl1.lf qddssrc/tblstr.pf | tblstr.file
tblstrl2.file: qddssrc/tblstrl2.lf qddssrc/tblstr.pf | tblstr.file
tblstrz1.file: qddssrc/tblstrz1.lf qddssrc/tblstr.pf | tblstr.file
trfdtll2.file: qddssrc/trfdtll2.lf qddssrc/trfdtl.pf | trfdtl.file
trfzstl3.file: qddssrc/trfzstl3.lf qddssrc/trfzst.pf | trfzst.file
# Join logical file - original DDS from the import
invvenj2.file: qddssrc/invvenj2.lf qddssrc/skvaux.pf qddssrc/invven.pf | skvaux.file invven.file

# ---------------------------------------------------------------------------
# Display files (REFFLD references need the referenced files to exist)
# ---------------------------------------------------------------------------
inv046fm.file: qddssrc/inv046fm.dspf | tblstr.file invbal.file pomdtl.file pomrcd.file menuctl.file invmst.file jdaref.file
inv502fm.file: qddssrc/inv502fm.dspf | menuctl.file
ts0110d.file: qddssrc/ts0110d.dspf
dspsiz.file: qddssrc/dspsiz.dspf

# ---------------------------------------------------------------------------
# Message file and binding directory (recreated from the PRFDEMOLIB objects)
# ---------------------------------------------------------------------------
jdamsgf.msgf: jdamsgf.msgf
rtcbnddir.bnddir: rtcbnddir.bnddir | rtc001.module

# ---------------------------------------------------------------------------
# Demo stubs (replace production programs not included in the demo)
# ---------------------------------------------------------------------------
sysget.pgm: qclsrc/sysget.clle
clts0111.pgm: qclsrc/clts0111.clle

# ---------------------------------------------------------------------------
# INV046 - inventory inquiry by store / district / region / store group
# ---------------------------------------------------------------------------
inv046.pgm: qrpgsrc/inv046.rpg qddssrc/inv046fm.dspf | inv046fm.file options.file invmst.file invbal.file tblstr.file tblstrl1.file tblstrl2.file tblreg.file tbldst.file tbladi.file invadi.file tbligh.file tbligd.file invcbl.file tbl800.file sys001ed.file
inv046cl.pgm: qclsrc/inv046cl.cl | inv046.pgm sysget.pgm

# ---------------------------------------------------------------------------
# INV502 - item inquiry: SKU details, then warehouse balances
# ---------------------------------------------------------------------------
rtc001cl.pgm: qclsrc/rtc001cl.cl
rtc002cl.pgm: qclsrc/rtc002cl.cl
rtc001.module: qrpglesrc/rtc001.rpgle
tblf04.pgm: qrpgsrc/tblf04.rpg | tblfld.file tblfldl1.file
getspk.pgm: qrpglesrc/getspk.rpgle | invvenj2.file invmst.file
trf840a.pgm: qrpglesrc/trf840a.rpgle | trfdtll2.file trfzstl3.file
inv502.pgm: qrpglesrc/inv502.rpgle qddssrc/inv502fm.dspf mmcpy/pgmstsds_d.rpgle mmcpy/kbdinfds_d.rpgle mmcpy/message_d.rpgle | inv502fm.file rtcbnddir.bnddir rtc001.module jdamsgf.msgf rtc001cl.pgm rtc002cl.pgm tblf04.pgm getspk.pgm trf840a.pgm sysget.pgm tblstrz1.file invmst.file invcbl.file invbal.file invbalz1.file invdpt.file apsupp.file itmrtl.file tblsea.file trfdtll2.file orderbk1.file pomhdr.file invfsd.file tblbyr.file trfzstl3.file phpf.file phpfw2.file phpfb1.file phpfb2.file optstr.file dptdiv.file sys001ed.file

# ---------------------------------------------------------------------------
# TS0110 - ticket item inventory by store/department (27x132), F10 creates a
# store distribution member in STR001WK and submits CLTS0111
# ---------------------------------------------------------------------------
dspsiz.pgm: qrpglesrc/dspsiz.rpgle qddssrc/dspsiz.dspf | dspsiz.file
nxtseq.pgm: qrpglesrc/nxtseq.rpgle
ts0110.pgm: qrpglesrc/ts0110.rpgle qddssrc/ts0110d.dspf | ts0110d.file tblstr.file invmstz3.file invbal.file invbal1.file invdpt.file itmrtl.file rplprf.file str001wk.file phpf.file phpfw2.file phpfb1.file phpfb2.file dspsiz.pgm nxtseq.pgm trf840a.pgm clts0111.pgm

# ===========================================================================
# Web (EJS / agentic display file) versions - menu options 10, 11, 12 (Variety Wholesalers demo)
# Same RPG as INV046 / INV502 / TS0110, display I/O through the Profound UI
# Open Access handler. Templates, CSS and JS live in the repo-root htdocs:
#   htdocs/profoundui/userdata/ui/vwshared/   Variety Wholesalers design system
#   htdocs/profoundui/userdata/ui/vwinv046|vwinv502|vwts0110/
# ===========================================================================
inv502wd.file: qddssrc/inv502wd.json
inv502w.pgm: qrpglesrc/inv502w.rpgle qddssrc/inv502wd.json mmcpy/pgmstsds_d.rpgle mmcpy/kbdinfds_d.rpgle mmcpy/message_d.rpgle | inv502wd.file rtcbnddir.bnddir rtc001.module jdamsgf.msgf rtc001cl.pgm rtc002cl.pgm tblf04.pgm getspk.pgm trf840a.pgm sysget.pgm tblstrz1.file invmst.file invcbl.file invbal.file invbalz1.file invdpt.file apsupp.file itmrtl.file tblsea.file trfdtll2.file orderbk1.file pomhdr.file invfsd.file tblbyr.file trfzstl3.file phpf.file phpfw2.file phpfb1.file phpfb2.file optstr.file dptdiv.file sys001ed.file
ts0110wd.file: qddssrc/ts0110wd.json
ts0110w.pgm: qrpglesrc/ts0110w.rpgle qddssrc/ts0110wd.json | ts0110wd.file tblstr.file invmstz3.file invbal.file invbal1.file invdpt.file itmrtl.file rplprf.file str001wk.file phpf.file phpfw2.file phpfb1.file phpfb2.file dspsiz.pgm nxtseq.pgm trf840a.pgm clts0111.pgm
inv046wd.file: qddssrc/inv046wd.json
inv046w.pgm: qrpglesrc/inv046w.rpgle qddssrc/inv046wd.json | inv046wd.file options.file invmst.file invbal.file tblstr.file tblstrl1.file tblstrl2.file tblreg.file tbldst.file tbladi.file invadi.file tbligh.file tbligd.file invcbl.file tbl800.file sys001ed.file sysget.pgm
inv046clw.pgm: qclsrc/inv046clw.cl | inv046w.pgm

# ---------------------------------------------------------------------------
# Plain web clones (menu options 7-9): generated from the *W members by
# tools/gen-plain-clones.py - same RPG, EJS display files *ED with plain
# templates (web/src/vp*).
# ---------------------------------------------------------------------------
inv502ed.file: qddssrc/inv502ed.json
inv502e.pgm: qrpglesrc/inv502e.rpgle qddssrc/inv502ed.json mmcpy/pgmstsds_d.rpgle mmcpy/kbdinfds_d.rpgle mmcpy/message_d.rpgle | inv502ed.file rtcbnddir.bnddir rtc001.module jdamsgf.msgf rtc001cl.pgm rtc002cl.pgm tblf04.pgm getspk.pgm trf840a.pgm sysget.pgm tblstrz1.file invmst.file invcbl.file invbal.file invbalz1.file invdpt.file apsupp.file itmrtl.file tblsea.file trfdtll2.file orderbk1.file pomhdr.file invfsd.file tblbyr.file trfzstl3.file phpf.file phpfw2.file phpfb1.file phpfb2.file optstr.file dptdiv.file sys001ed.file
ts0110ed.file: qddssrc/ts0110ed.json
ts0110e.pgm: qrpglesrc/ts0110e.rpgle qddssrc/ts0110ed.json | ts0110ed.file tblstr.file invmstz3.file invbal.file invbal1.file invdpt.file itmrtl.file rplprf.file str001wk.file phpf.file phpfw2.file phpfb1.file phpfb2.file dspsiz.pgm nxtseq.pgm trf840a.pgm clts0111.pgm
inv046ed.file: qddssrc/inv046ed.json
inv046e.pgm: qrpglesrc/inv046e.rpgle qddssrc/inv046ed.json | inv046ed.file options.file invmst.file invbal.file tblstr.file tblstrl1.file tblstrl2.file tblreg.file tbldst.file tbladi.file invadi.file tbligh.file tbligd.file invcbl.file tbl800.file sys001ed.file sysget.pgm
inv046cle.pgm: qclsrc/inv046cle.cl | inv046e.pgm
