# Sapiens Agency module - Phase A (real DDS) + Phase B (real SPI* service programs)
#
# Layout follows codermake source-physical-file convention:
#     - RPG sources (programs, modules, /COPY copybooks)
#       - DDS sources (PFs, LFs, DSPFs, Rich Display File JSON)
#       - Binder source (.bnd) and binding directory specs (.bnddir)

# --- Field reference files (every PF REF()s one of these) -----------------
srefp.file: srefp.pf
wrefp.file: wrefp.pf  | srefp.file
screfp.file: screfp.pf | srefp.file
syrefp.file: syrefp.pf

# --- Real-schema PFs that WTAGTCFG opens ---------------------------------
wmagp.file: wmagp.pf | wrefp.file
wmaap.file: wmaap.pf | wrefp.file
wdf2p.file: wdf2p.pf | wrefp.file
wmczp.file: wmczp.pf | wrefp.file
wdelp.file: wdelp.pf | wrefp.file
smcop.file: smcop.pf | srefp.file

# LFs over each PF
wmagl.file: wmagl.lf  | wmagp.file
wmagl1.file: wmagl1.lf | wmagp.file
wmagl2.file: wmagl2.lf | wmagp.file
wmagl3.file: wmagl3.lf | wmagp.file
wmagl4.file: wmagl4.lf | wmagp.file
wmaal.file: wmaal.lf  | wmaap.file
wmaal1.file: wmaal1.lf | wmaap.file
wmaal2.file: wmaal2.lf | wmaap.file
wmaal3.file: wmaal3.lf | wmaap.file
wmaal4.file: wmaal4.lf | wmaap.file
wmaal5.file: wmaal5.lf | wmaap.file
wmaal6.file: wmaal6.lf | wmaap.file
wmaal7.file: wmaal7.lf | wmaap.file
smcol.file: smcol.lf  | smcop.file
smcol1.file: smcol1.lf | smcop.file
wdf2l.file: wdf2l.lf  | wdf2p.file
wdf2l1.file: wdf2l1.lf | wdf2p.file
wmczl.file: wmczl.lf  | wmczp.file
wmczl1.file: wmczl1.lf | wmczp.file
wdell.file: wdell.lf   | wdelp.file
wdell1.file: wdell1.lf  | wdelp.file
wdell1q.file: wdell1q.lf | wdelp.file
wdell2.file: wdell2.lf  | wdelp.file
wdell2q.file: wdell2q.lf | wdelp.file
wdell3.file: wdell3.lf  | wdelp.file
wdell4.file: wdell4.lf  | wdelp.file
wdell5.file: wdell5.lf  | wdelp.file
wdell6.file: wdell6.lf  | wdelp.file
wdell7.file: wdell7.lf  | wdelp.file
wdell8.file: wdell8.lf  | wdelp.file
wdell8q.file: wdell8q.lf | wdelp.file
wdell9.file: wdell9.lf  | wdelp.file
wdella.file: wdella.lf  | wdelp.file
wdellb.file: wdellb.lf  | wdelp.file
wdellq.file: wdellq.lf  | wdelp.file

# Trigger-buffer PFs (real Sapiens source)
szq1p.file: szq1p.pf  | wrefp.file
wmahp.file: wmahp.pf  | wrefp.file
wmcmp.file: wmcmp.pf  | wrefp.file
wmcdp.file: wmcdp.pf  | wrefp.file
wdpap.file: wdpap.pf  | wrefp.file
wamcp.file: wamcp.pf  | wrefp.file
wdehp.file: wdehp.pf  | wrefp.file
wmshp.file: wmshp.pf  | wrefp.file
wxelp.file: wxelp.pf  | wrefp.file
wmemp.file: wmemp.pf  | wrefp.file
sdcmp.file: sdcmp.pf  | screfp.file
wmeap.file: wmeap.pf  | wrefp.file
wibifp.file: wibifp.pf
wdeap_hld.file: wdeap_hld.pf

# Display files
stactpnl.file: stactpnl.dspf | wrefp.file szq1p.file
wtagtcfg.file: wtagtcfg.dspf | wrefp.file wdelp.file

# --- SAPSTUBS fallback service program -----------------------------------
sapstubs.module: sapstubs.rpgle \
  SPRGENERR.rpgle SPRATRIUM.rpgle SPRDATE.rpgle SPRCOMPANY.rpgle \
  SPRusrprms.rpgle SPRsql.rpgle SCOPYCOMPR.rpgle wprpolerr.rpgle
sapstubs.srvpgm: sapstubs.module

# --- Binding directories ---------------------------------------------------
stbnddir.bnddir: stbnddir.bnddir | sapstubs.srvpgm
wtgrpcfg.bnddir: wtgrpcfg.bnddir | sapstubs.srvpgm

# --- EXTPGM stubs for runtime program calls -----------------------------
w4020r.pgm: w4020r.rpgle
srchkpgm.pgm: srchkpgm.clp
rtvcltid.pgm: rtvcltid.clle
strtvdta.pgm: strtvdta.rpgle
stwrtdta.pgm: stwrtdta.rpgle

# --- WTAGTCFE (EJS rendition for menu option 5) -------------------------
wtagtcfe.file: wtagtcfe.json
wtagtcfe.pgm: wtagtcfe.sqlrpgle wtagtcfe.file \
  | wmagp.file wmagl.file wmagl1.file wmaap.file

# --- Files used by WTAGTASN's embedded SQL queries ---------------------
wmefp.file:   wmefp.pf   | wrefp.file
wmefl2.file:  wmefl2.lf  | wmefp.file
wmefl7.file:  wmefl7.lf  | wmefp.file
waqtp.file:   waqtp.pf   | wrefp.file
waqtl8.file:  waqtl8.lf  | waqtp.file
waqtl9.file:  waqtl9.lf  | waqtp.file
waqtl12.file: waqtl12.lf | waqtp.file
wmnbp.file:   wmnbp.pf   | wrefp.file
wmnbl.file:   wmnbl.lf   | wmnbp.file
wtqsp.file:   wtqsp.pf   | wrefp.file
wtqsl1.file:  wtqsl1.lf  | wtqsp.file
wmshlq.file:  wmshlq.lf  | wmshp.file
ssypf.file:   ssypf.pf   | syrefp.file
spcpf.file:   spcpf.pf   | syrefp.file
swcpf.file:   swcpf.pf   | syrefp.file
sbcpf.file:   sbcpf.pf   | syrefp.file
scspf.file:   scspf.pf   | syrefp.file
sdrpf.file:   sdrpf.pf   | syrefp.file
sfbpf.file:   sfbpf.pf   | syrefp.file
sglpf.file:   sglpf.pf   | syrefp.file
srcpf.file:   srcpf.pf   | syrefp.file
sp2pf.file:   sp2pf.pf   | syrefp.file
ssylf.file:   ssylf.lf   | ssypf.file spcpf.file swcpf.file sbcpf.file scspf.file sdrpf.file sfbpf.file sglpf.file srcpf.file sp2pf.file

# --- WTAGTASN (option 6 — Agency Assignment) --------------------------
# Self-contained: no extpgm callees, all required PFs/LFs available.
wtactpnl.file:  wtactpnl.dspf  | wrefp.file szq1p.file wmemp.file
wtagtasn.file:  wtagtasn.dspf  | wrefp.file wdelp.file wmemp.file wmahp.file
wmfnp.file:     wmfnp.pf       | wrefp.file
wmfnl.file:     wmfnl.lf       | wmfnp.file
wmahl3.file:    wmahl3.lf      | wmahp.file
wmahl.file:     wmahl.lf       | wmahp.file
smupp.file:     smupp.pf       | srefp.file
smupl.file:     smupl.lf       | smupp.file
wtagtasn.pgm:   wtagtasn.sqlrpgle \
  SPRusrprms.rpgle SPRGENPOP.rpgle SPRDATE.rpgle SPRCOMPANY.rpgle \
  wprpolerr.rpgle SPRstring.rpgle SPRGENERR.rpgle wpragent.rpgle \
  SPRmodule.rpgle witmpltd.rpgle SPRmask.rpgle wpreft.rpgle \
  SPRruncmd.rpgle wpremplr.rpgle SCOPYGRPPR.rpgle SCOPYCOMPI.rpgle \
  wtagtasn.file wtactpnl.file \
  | stbnddir.bnddir wtgrpcfg.bnddir \
    wmagp.file wmaap.file wmaal.file wmaal1.file wmahl.file wmahl3.file \
    wmfnl.file smcol.file smupl.file wdelp.file wdell.file \
    wmefp.file wmefl2.file wmefl7.file waqtp.file waqtl8.file waqtl9.file \
    waqtl12.file wmnbp.file wmnbl.file wtqsl1.file wmshlq.file ssylf.file

# --- WTAGTCFG (option 4) -----------------------------------------------
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
