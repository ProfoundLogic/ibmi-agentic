# Sapiens Agency module
#
# REVERSE-ENGINEERED scaffolding to get WTAGTCFG building & running
# without the real Sapiens DDS / objects / binding directories. Throw
# away when Sapiens delivers real artifacts.

# --- Real-schema PFs that WTAGTCFG opens ----------------------------------
wmagp.file: wmagp.pf
smcop.file: smcop.pf
wmaap.file: wmaap.pf
wdf2p.file: wdf2p.pf
wmczp.file: wmczp.pf
wdelp.file: wdelp.pf

# --- LFs over the real-schema PFs ----------------------------------------
wmagl.file:  wmagl.lf  | wmagp.file
wmagl1.file: wmagl1.lf | wmagp.file
smcol.file:  smcol.lf  | smcop.file
wmaal.file:  wmaal.lf  | wmaap.file
wdf2l.file:  wdf2l.lf  | wdf2p.file
wmczl.file:  wmczl.lf  | wmczp.file

# --- Trigger-DS placeholder PFs (extName targets in SPRtrigger.rpgle) ----
szq1p.file:  szq1p.pf
wmahp.file:  wmahp.pf
wmcmp.file:  wmcmp.pf
wmcdp.file:  wmcdp.pf
wdpap.file:  wdpap.pf
wamcp.file:  wamcp.pf
wdehp.file:  wdehp.pf
wmshp.file:  wmshp.pf
wxelp.file:  wxelp.pf
wmemp.file:  wmemp.pf
sdcmp.file:  sdcmp.pf
wmeap.file:  wmeap.pf
wibifp.file: wibifp.pf
wdeap_hld.file: wdeap_hld.table.sql

# --- Display files -------------------------------------------------------
stactpnl.file: stactpnl.dspf
wtagtcfg.file: wtagtcfg.dspf | wdelp.file

# --- Stub service program for the copybook procedure prototypes ---------
sapstubs.module: sapstubs.rpgle \
  SPRGENERR.rpgle SPRATRIUM.rpgle SPRDATE.rpgle SPRCOMPANY.rpgle \
  SPRusrprms.rpgle SPRsql.rpgle SCOPYCOMPR.rpgle wprpolerr.rpgle
sapstubs.srvpgm: sapstubs.module

# --- Binding directories that WTAGTCFG (and other drivers) reference ---
stbnddir.bnddir: stbnddir.bnddir | sapstubs.srvpgm
wtgrpcfg.bnddir: wtgrpcfg.bnddir | sapstubs.srvpgm

# --- EXTPGM stubs for runtime program calls -----------------------------
w4020r.pgm:   w4020r.rpgle
srchkpgm.pgm: srchkpgm.rpgle
rtvcltid.pgm: rtvcltid.rpgle
strtvdta.pgm: strtvdta.rpgle
stwrtdta.pgm: stwrtdta.rpgle

# --- WTAGTCFG program ---------------------------------------------------
wtagtcfg.pgm: wtagtcfg.sqlrpgle \
  SPRusrprms.rpgle SPRGENPOP.rpgle SPRDATE.rpgle SPRstring.rpgle \
  SPRsql.rpgle SPRmessage.rpgle SPRmodule.rpgle SCOPYCOMPR.rpgle \
  SPRCOMPANY.rpgle SPRtable.rpgle SPRtrigger.rpgle SPRtrig.rpgle \
  wprtable.rpgle SPRATRIUM.rpgle wprpolerr.rpgle wprgroup.rpgle \
  SPRGENERR.rpgle SCOPYCOMPI.rpgle \
  wtagtcfg.file stactpnl.file \
  | stbnddir.bnddir wtgrpcfg.bnddir \
    wmagp.file wmagl.file wmagl1.file smcop.file smcol.file \
    wmaap.file wmaal.file wdf2p.file wdf2l.file wmczp.file wmczl.file \
    wdelp.file \
    szq1p.file wmahp.file wmcmp.file wmcdp.file wdpap.file wamcp.file \
    wdehp.file wmshp.file wxelp.file wmemp.file sdcmp.file wmeap.file \
    wibifp.file wdeap_hld.file \
    w4020r.pgm srchkpgm.pgm rtvcltid.pgm strtvdta.pgm stwrtdta.pgm
