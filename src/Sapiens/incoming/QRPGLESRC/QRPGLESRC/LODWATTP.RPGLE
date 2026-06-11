     Hcopyright('(c) 2009 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no)
    H*********************************************************************
    H/TITLE LODWATTP - Load data for WATTP
    H*********************************************************************
    H* Created: 10/01/09    C:33    Project: 114WUO                      *
    H*********************************************************************
     H* * * * Allow the process to add the suffix to the libraries  * * * *
    H*********************************************************************
     H* Revised:                                                          *
     H*                                                                   *
    H*********************************************************************
     Fwattp     uf a e           k disk    rename(ptt01:ptt0p)

     D #ofFiles        c                   203
     D fileDesc        s             88    dim(#ofFiles) ctdata perrcd(1)

     DfileInfo       e ds                  extname(WATTP)

     Di1               s              3  0
     Dsuffix           s              3
     Dpdsfil           s                   like(TTFLIB) inz('PDSFIL')
     Dpdstst           s                   like(TTTLIB) inz('PDSTST')
     DsysTime          s                   like(TTTIME)
     DsysDate          s                   like(TTDT)

     D psds          esds                  extname(szq1p)

     DRtvSuffix        PR                  extpgm('RTVSUFFIX')
     D suffix                         3

     D/copy sPrDate

     C                   eval      sysTime = TimeTo6(SystemTime)
     C                   eval      sysDate = DateTo7(SystemDate)
     C                   callp     RtvSuffix(suffix)
     C*
     C                   exsr      Process
     C*
     C                   eval      *inlr = *on
     C*
     C*====================================================================
     C* Process each company
     C*====================================================================
     C     Process       begsr
     C*
     C     1             do        #ofFiles      i1
     C                   eval      fileInfo = fileDesc(i1)
     C     TTFILE        chain     ptt0p
     C                   if        not %found
     C                   exsr      WriteRecord
     C                   endif
     C                   enddo
     C*
     C                   endsr
     C*
     C*====================================================================
     C* Write records to WATTP
     C*====================================================================
     C     WriteRecord   begsr
     C*
     C                   eval      fileInfo = fileDesc(i1)
     C*
     C                   select
     C                   when      TTFLIB = *blanks
     C                   eval      TTFLIB = pdsfil
     C                   endsl
     C*
     C                   select
     C                   when      TTTLIB = *blanks
     C                   eval      TTTLIB = pdstst
     C                   endsl
     C*
     C                   eval      TTFLIB = %trim(TTFLIB) + suffix
     C                   eval      TTTLIB = %trim(TTTLIB) + suffix
     C*
     C                   eval      TTSTAT = 'A'
     C                   eval      TTUSER = q1user
     C                   eval      TTPGM  = q1pgm
     C                   eval      TTTIME = sysTime
     C                   eval      TTDT   = sysDate
     C                   eval      TTIUSR = TTUSER
     C                   eval      TTIPGM = TTPGM
     C                   eval      TTITIM = TTTIME
     C                   eval      TTIDT  = TTDT
     C*
     C                   write     ptt0p
     C*
     C                   endsr

**  File  |  Key1   |  Key 2  |  Key 3  |   From  |    To    |Cl
FADAP     DACO#     DAFND     DASUB#    PDSFIL    PDSTST
FAEAP     EACO#     EAFND     EASUB#    PDSFIL    PDSTST
FAEDP     EDCO#     EDFND     EDSUB#    PDSFIL    PDSTST
FAEEP     EECO#     EEFND     EESUB#    PDSFIL    PDSTST
FAEHP     EHCO#     EHFND     EHSUB#    PDSFIL    PDSTST
FAEOP     EOCO#     EOFND     EOSUB#    PDSFIL    PDSTST
FAEPP     EPCO#     EPFND     EPSUB#    PDSFIL    PDSTST
FAESP     ESCO#     ESFND     ESSUB#    PDSFIL    PDSTST
FAEUP     EUCO#     EUFND     EUSUB#    PDSFIL    PDSTST
FAL2P     L2CO#     L2FND     L2SUB#    PDSFIL    PDSTST
FALTP     LTCO#     LTFND     LTSUB#    PDSFIL    PDSTST
FAMSP     MSCO#     MSFND     MSSUB#    PDSFIL    PDSTST
FAPPP     PPCO#     PPFND     PPSUB#    PDSFIL    PDSTST
FATDP     TDCO#     TDFND     TDSUB#    PDSFIL    PDSTST
FATHP     THFCO#    THFFND    THFSUB#   PDSFIL    PDSTST
FATIP     TICO#     TIFND     TISUB#    PDSFIL    PDSTST
FATRP     TRCO#     TRFND     TRSUB#    PDSFIL    PDSTST
FAWHP     WHCO#     WHFND     WHSUB#    PDSFIL    PDSTST
FDCAP     CACO#     CAFND     CASUB#    PDSFIL    PDSTST
FDCHP     CHCO#     CHFND     CHSUB#    PDSFIL    PDSTST
FDDAP     DACO#     DAFND     DASUB#    PDSFIL    PDSTST
FDDDP     DDCO#     DDFND     DDSUB#    PDSFIL    PDSTST
FDDEP     DECO#     DEFND     DESUB#    PDSFIL    PDSTST
FDDHP     DHCO#     DHFND     DHSUB#    PDSFIL    PDSTST
FDDPP     DPCO#     DPFND     DPSUB#    PDSFIL    PDSTST
FDDUP     DUCO#     DUFND     DUSUB#    PDSFIL    PDSTST
FDF2P     F2CO#     F2FND     F2SUB#    PDSFIL    PDSTST
FDF3P     F3CO#     F3FND     F3SUB#    PDSFIL    PDSTST
FDGXP     GXCO#     GXFND     GXSUB#    PDSFIL    PDSTST
FDMGP     MGCO#     MGFND     MGSUB#    PDSFIL    PDSTST
FDOSP     OSCO#     OSFND     OSSUB#    PDSFIL    PDSTST
FDSXP     SXCO#     SXFND     SXSUB#    PDSFIL    PDSTST
FEDDP     DDCO#     DDFND     DDSUB#    PDSFIL    PDSTST
FEDHP     DHCO#     DHFND     DHSUB#    PDSFIL    PDSTST
FEDPP     DPCO#     DPFND     DPSUB#    PDSFIL    PDSTST
FEDUP     DUCO#     DUFND     DUSUB#    PDSFIL    PDSTST
FEEDP     EDCO#     EDFND     EDSUB#    PDSFIL    PDSTST
FEEHP     EHCO#     EHFND     EHSUB#    PDSFIL    PDSTST
FEEPP     EPCO#     EPFND     EPSUB#    PDSFIL    PDSTST
FEEUP     EUCO#     EUFND     EUSUB#    PDSFIL    PDSTST
FELGP     LGCO#     LGFND     LGSUB#    PDSFIL    PDSTST
FMCHP     CHCO#     CHFND     CHSUB#    PDSFIL    PDSTST
FMCMP     CMCO#     CMFND     CMSUB#    PDSFIL    PDSTST
FMCXP     CXCO#     CXFND     CXSUB#    PDSFIL    PDSTST
FMEMP     EMCO#     EMFND     EMSUB#    PDSFIL    PDSTST
FMF1P     F1CO#     F1FND     F1SUB#    PDSFIL    PDSTST
FMFNP     FNCO#     FNFND     FNSUB#    PDSFIL    PDSTST
FMFRP     FRCO#     FRFND     FRSUB#    PDSFIL    PDSTST
FMLGP     LGCO#     LGFND     LGSUB#    PDSFIL    PDSTST
FMSBP     SBCO#     SBFND     SBSUB#    PDSFIL    PDSTST
NDTMP     TMCO#     TMFND     TMSUB#    PDSFIL    PDSTST
NMARP     ARCO#     ARFND     ARSUB#    PDSFIL    PDSTST
NMIMP     IMCO#     IMFND     IMSUB#    PDSFIL    PDSTST
UDPOP     POCO#     POFND     POSUB#    PDSFIL    PDSTST
UMCHP     CHCO#     CHFND     CHSUB#    PDSFIL    PDSTST
UMPCP     PCCO#     PCFND     PCSUB#    PDSFIL    PDSTST
WABLP     BLCO#     BLFND               PDSFIL    PDSTST
WAC2P     C2CO#     C2FND               PDSFIL    PDSTST
WAC3P     CPCO#     CPFND               PDSFIL    PDSTST
WAC6P     C6CO#     C6FND               PDSFIL    PDSTST
WACMP     CMCO#     CMFND               PDSFIL    PDSTST
WACPP     CPCO#     CPFND               PDSFIL    PDSTST
WADHP     DHCO#     DHFND               PDSFIL    PDSTST
WADQP     DQCO#     DQFND               PDSFIL    PDSTST
WADSP     DSCO#     DSFND               PDSFIL    PDSTST
WADTP     DTCO#     DTFND               PDSFIL    PDSTST
WAEAP     EACO#     EAFND               PDSFIL    PDSTST
WAEDP     EDCO#     EDFND               PDSFIL    PDSTST
WAEEP     EECO#     EEFND               PDSFIL    PDSTST
WAEPP     EPCO#     EPFND               PDSFIL    PDSTST
WAETP     ETCO#     ETFND               PDSFIL    PDSTST
WAFPP     FPCO#     FPFND               PDSFIL    PDSTST
WAGCP     GCCO#     GCFND               PDSFIL    PDSTST
WAIDP     IDCO#     IDFND               PDSFIL    PDSTST
WAILP     ILCO#     ILFND               PDSFIL    PDSTST
WAL1P     L1CO#     L1FND               PDSFIL    PDSTST
WALTP     LTCO#     LTFND               PDSFIL    PDSTST
WAMCP     MCCO#     MCFND               PDSFIL    PDSTST
WAOFP     OFCO#     OFFND               PDSFIL    PDSTST
WAQTP     QTCO#     QTFND               PDSFIL    PDSTST
WAR2P     R2CO#     R2FND               PDSFIL    PDSTST
WAR3P     RPCO#     RPFND               PDSFIL    PDSTST
WAR4P     R4CO#     R4FND               PDSFIL    PDSTST
WARPP     RPCO#     RPFND               PDSFIL    PDSTST
WARPPM    RPCO#     RPFND               PDSFIL    PDSTST
WASRP     SRCO#     SRFND               PDSFIL    PDSTST
WDABP     ABCO#     ABFND               PDSFIL    PDSTST
WDACP     ACCO#     ACFND               PDSFIL    PDSTST
WDAEP     AECO#     AEFND               PDSFIL    PDSTST
WDCDP     CDCO#     CDFND               PDSFIL    PDSTST
WDCRP     CRCO#     CRFND               PDSFIL    PDSTST
WDCXP     CXCO#     CXFND               PDSFIL    PDSTST
WDDDP     DDCO#     DDFND               PDSFIL    PDSTST
WDDSP     DSCO#     DSFND               PDSFIL    PDSTST
WDEAP     EACO#     EAFND               PDSFIL    PDSTST
WDECP     ECCO#     ECFND               PDSFIL    PDSTST
WDEHP     EHCO#     EHFND               PDSFIL    PDSTST
WDELP     ELCO#     ELFND               PDSFIL    PDSTST
WDF1P     F1CO#     F1FND               PDSFIL    PDSTST
WDF2P     F2CO#     F2FND               PDSFIL    PDSTST
WDHEP     HECO#     HEFND               PDSFIL    PDSTST
WDILP     ILCO#     ILFND               PDSFIL    PDSTST
WDMPP     MPCO#     MPFND               PDSFIL    PDSTST
WDNTP     NTCO#     NTFND               PDSFIL    PDSTST
WDPAP     PACO#     PAFND               PDSFIL    PDSTST
WDPBP     PBCO#     PBFND               PDSFIL    PDSTST
WDPFP     PFCO#     PFFND               PDSFIL    PDSTST
WDPPP     PPCO#     PPFND               PDSFIL    PDSTST
WDRBP     RBCO#     RBFND               PDSFIL    PDSTST
WDRCP     RCCO#     RCFND               PDSFIL    PDSTST
WDRPP     RPCO#     RPFND               PDSFIL    PDSTST
WDRRP     RRCO#     RRFND               PDSFIL    PDSTST
WDRTP     RTCO#     RTFND               PDSFIL    PDSTST
WDSCP     SCCO#     SCFND               PDSFIL    PDSTST
WDSEP     SECO#     SEFND               PDSFIL    PDSTST
WDSEP2    SECO#     SEFND               PDSFIL    PDSTST
WDSIP     SICO#     SIFND               PDSFIL    PDSTST
WDSLP     SLCO#     SLFND               PDSFIL    PDSTST
WDSLP2    SLCO#     SLFND               PDSFIL    PDSTST
WDSTP     STCO#     STFND               PDSFIL    PDSTST
WDVHP     VHCO#     VHGRP               PDSFIL    PDSTST
WDXHP     XHCO#     XHFND               PDSFIL    PDSTST
WDXSP     XSCO#     XSFND               PDSFIL    PDSTST
WFEUP     EUCO#     EUFND               PDSFIL    PDSTST
WHUPP     UPCO#     UPFND               PDSFIL    PDSTST
WHXAP     XACO#     XAFND               PDSFIL    PDSTST
WMA2P     A2CO#     A2FND               PDSFIL    PDSTST
WMA3P     A3CO#     A3FND               PDSFIL    PDSTST
WMAAP     AACO#     AAFND               PDSFIL    PDSTST
WMACP     ACCO#     ACFND               PDSFIL    PDSTST
WMADP     ADCO#     ADFND               PDSFIL    PDSTST
WMAHP     AHCO#     AHFND               PDSFIL    PDSTST
WMALP     ALCO#     ALFND               PDSFIL    PDSTST
WMANP     ANCO#     ANFND               PDSFIL    PDSTST
WMARP     ARCO#     ARFND               PDSFIL    PDSTST
WMAUP     AUCO#     AUFND               PDSFIL    PDSTST
WMAVP     AVCO#     AVFND               PDSFIL    PDSTST
WMAWP     AWCO#     AWFND               PDSFIL    PDSTST
WMAXP     AXCO#     AXFND               PDSFIL    PDSTST
WMBIP     BICO#     BIFND               PDSFIL    PDSTST
WMBXP     BXCO#     BXFND               PDSFIL    PDSTST
WMBYP     BYCO#     BYFND               PDSFIL    PDSTST
WMCCP     CCCO#     CCFND               PDSFIL    PDSTST
WMCEP     CECO#     CEFND               PDSFIL    PDSTST
WMCFP     CFCO#     CFFND               PDSFIL    PDSTST
WMCHP     CHCO#     CHFND               PDSFIL    PDSTST
WMCIP     CICO#     CIFND               PDSFIL    PDSTST
WMCMP     CMCO#     CMFND               PDSFIL    PDSTST
WMCPP     CPCO#     CPFND               PDSFIL    PDSTST
WMCXP     CXCO#     CXFND               PDSFIL    PDSTST
WMDDP     DDCO#     DDFND               PDSFIL    PDSTST
WMDPP     DPCO#     DPFND               PDSFIL    PDSTST
WMDSP     DSCO#     DSFND               PDSFIL    PDSTST
WMDXP     DXCO#     DXFND               PDSFIL    PDSTST
WMEAP     EACO#     EAFND               PDSFIL    PDSTST
WMECP     ECCO#     ECFND               PDSFIL    PDSTST
WMEMP     EMCO#     EMFND               PDSFIL    PDSTST
WMEPP     EPCO#     EPFND               PDSFIL    PDSTST
WMEWP     EWCO#     EWFND               PDSFIL    PDSTST
WMEXP     EXCO#     EXFND               PDSFIL    PDSTST
WMFNP     FNCO#     FNFND               PDSFIL    PDSTST
WMGCP     GCCO#     GCFND               PDSFIL    PDSTST
WMGRP     GRCO#     GRFND               PDSFIL    PDSTST
WMIHP     IHCO#     IHFND               PDSFIL    PDSTST
WMLGP     LGCO#     LGFND               PDSFIL    PDSTST
WMLLP     LLCO#     LLFND               PDSFIL    PDSTST
WMMGP     MGCO#     MGFND               PDSFIL    PDSTST
WMMUP     MUCO#     MUFND               PDSFIL    PDSTST
WMNBP     NBCO#     NBFND               PDSFIL    PDSTST
WMNIP     NICO#     NIFND               PDSFIL    PDSTST
WMNXP     NXCO#     NXFND               PDSFIL    PDSTST
WMOOP     OOCO#     OOFND               PDSFIL    PDSTST
WMPCP     PCCO#     PCFND               PDSFIL    PDSTST
WMPFP     PFCO#     PFFND               PDSFIL    PDSTST
WMREP     RECO#     REFND               PDSFIL    PDSTST
WMRFP     RFCO#     RFFND               PDSFIL    PDSTST
WMRHP     RHCO#     RHFND               PDSFIL    PDSTST
WMRIP     RICO#     RIFND               PDSFIL    PDSTST
WMRPP     RPCO#     RPFND               PDSFIL    PDSTST
WMRRP     RRCO#     RRFND               PDSFIL    PDSTST
WMSBP     SBCO#     SBFND               PDSFIL    PDSTST
WMSHP     SHCO#     SHFND               PDSFIL    PDSTST
WMSQP     SQCO#     SQFND               PDSFIL    PDSTST
WMSQP2    SQCO#     SQFND               PDSFIL    PDSTST
WMSRP     SRCO#     SRFND               PDSFIL    PDSTST
WMSRP2    SRCO#     SRFND               PDSFIL    PDSTST
WMSRP3    SRCO#     SRFND               PDSFIL    PDSTST
WMSSP     SSCO#     SSFND               PDSFIL    PDSTST
WMSTP     STCO#     STFND               PDSFIL    PDSTST
WMTHP     THCO#     THFND               PDSFIL    PDSTST
WMTIP     TICO#     TIFND               PDSFIL    PDSTST
WMTPP     TPCO#     TPFND               PDSFIL    PDSTST
WMWHP     WHCO#     WHFND               PDSFIL    PDSTST
WMWVP     WVCO#     WVFND               PDSFIL    PDSTST
WMXAP     XACO#     XAFND               PDSFIL    PDSTST
WMXRP     XRCO#     XRFND               PDSFIL    PDSTST
WTC2P     C2CO#     C2FND               PDSFIL    PDSTST
WTC3P     C3CO#     C3FND               PDSFIL    PDSTST
WTDDP     DDCO#     DDFND               PDSFIL    PDSTST
WTENP     ENCO#     ENFND               PDSFIL    PDSTST
WTLMP     LMCO#     LMFND               PDSFIL    PDSTST
WTMYP     MYCO#     MYFND               PDSFIL    PDSTST
WTXCP     XCCO#     XCFND               PDSFIL    PDSTST
