     H copyright('(C) Copyright Progressive Data Solutions, Inc., 2001-03')
      *****************************************************************
     H/TITLE W8126R2 - CLARENDON - BUILD ACTIVE POLICY EXTRACT FILES
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *****************************************************************
      *  REVISIONS:                                                   *
      *                                                               *
      *                                                               *
      * 10/11/07 C:190 SUPPORT WA099 54504                            *
      *                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
      *                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
      *                                                               *
      * 11/15/04 C:136 Support: WA121 44531                           *
      *                Ensure that duplicate POLMP records aren't     *
      *                written.                                       *
      *                                                               *
      * 12/18/03  C:33   Project: WC970   WA109/39646                 *
      *                  Break out Terrorism risk.                    *
      *                  (Quickest code available, no time for rework)*
      *                                                               *
      * 08/12/02  C:142  PROJECT: WC970                               *
      *                  Fixed. Was not processing last record in     *
      *                  file Wfagl.                                  *
      *****************************************************************
     *
     * Earned/Unearned File
     FWfeul     IF   E           K Disk
     *
     *  File used for group selection
     FWxpgl     IF   E           K Disk
     *
     * A/R Transaction Type Code Table File
     FWtttl1    IF   E           K Disk
     *
     * Employer Detail File
     FWdell3    IF   E           K Disk
     F                                     Rename(Pel01:Wdelr3)
     *
     * Employer Detail File
     FWdell1    IF   E           K Disk
     F                                     Rename(Pel01:Wdelr1)
     F                                     Prefix(L1:2)
     *
     * Employer Detail File
     FWdell2    IF   E           K Disk
     F                                     Rename(Pel01:Wdelr2)
     F                                     Prefix(L2:2)
     *
     * Deductible Detail File
     FWdddl     IF   E           K Disk
     *
     * Employer Detail File
     FWdell8    IF   E           K Disk
     F                                     Rename(Pel01:Wdelr8)
     F                                     Prefix(L8:2)
     *
     * Employer Master File
     FWmeml     IF   E           K Disk
     *
     * Carrier/State Reporting Master File
     FWmrpl4    IF   E           K Disk
     *
     * Rate Set Code Master File
     FWmrsl     IF   E           K Disk
     *
     * Group Carrier Information File
     FWmgcl     IF   E           K Disk
     *
     * Workers' Comp Employer Detail History File
     FWdehl1    IF   E           K Disk
     *
     * Fund master File
     FWmfnl     IF   E           K Disk
     *
     *  Cancellation Detail File
     FWdcxl     IF   E           K Disk
     *
     * Reserve Type Code Table File
     FWtrtp     IF   E           K Disk
     *
     * Generic Table File
     FWtabl     IF   E           K Disk
     F                                     Ignore(PPA01)
     F                                     Ignore(PRT01)
     F                                     Ignore(PTT01)
     F                                     Ignore(PRS01)
     F                                     Ignore(PTR01)
     *
     * Generic Table File 2
     FWtgtl     IF   E           K Disk
     *
     * Claimant Demographic File
     FWmcdl     IF   E           K Disk
     *
     *
     * Deductible Discount Code File
     FWtddl3    IF   E           K Disk
     F                                     Rename(PDD01:WTDDLR)
     *
     * Limits Table File
     FWtlml     IF   E           K Disk
     F                                     Rename(PLM01:WTLMLR)
     F                                     Prefix(LL:2)
     * Statistical Reporting/Rating File
     FWdrrl     IF   E           K Disk
     *
     *
     FPolml1    IF   E           K DISK
     F                                     Rename(PLM01:POLMLR)
     F                                     Prefix(L2:2)
     *
     FPolmp     IF A E           K DISK
     *
     * Logical Premium Master For Clarendon Pre Processing
     FPpreml    IF   E           K Disk
     F                                     Rename(PPP01:PPREMLR)
     *
     * Logical Inforce Premium For Clarendon Pre Processing
     FInfcl     IF   E           K Disk
     F                                     Rename(IFC01:INFCLR)
     *
     FPpremp    O  A E             DISK
     FInfcp     O  A E             DISK
     *
      /eject
     D DATUSA          S               D   DATFMT(*USA)
     D USAEFF          S               D   DATFMT(*USA)
     D USACNX          S               D   DATFMT(*USA)
      *
      * Amount Fields used to Accum totals for Alpha output
     D Ppwrt#          S             20  2
     D Ppded#          S             20  2
     D Fcpre#          S             20  2
     D Fcuep#          S             20  2
     D FirstL2         S              1n
     D First           S              1n
     D SvEuPfusa       S               D   Datfmt(*Usa)
     D EuPfusa         S               D   Datfmt(*Usa)
     D SvEuPtusa       S               D   Datfmt(*Usa)
     D EuPtusa         S               D   Datfmt(*Usa)
      *
     D Occurs          S              3  0
     D Editzip         C                    '0     -    '
      *
     D                 DS
     D Effdates                1     35  0 Dim(5)
     D Emefd1                  1      7  0
     D Emefd2                  8     14  0
     D Emefd3                 15     21  0
     D Emefd4                 22     28  0
     D Emefd5                 29     35  0
      *
     D                 DS
     D Cxdates                 1     35  0 Dim(5)
     D Emcad1                  1      7  0
     D Emcad2                  8     14  0
     D Emcad3                 15     21  0
     D Emcad4                 22     28  0
     D Emcad5                 29     35  0
      *
     D OcPrems         DS                  Occurs(55)
     D  OcState                       2
     D  OcEfdt                        7  0
     D* OcIncLim                     11  2
     D  OcDedOcr                     11  2
      *
     D OcPol           DS                  Occurs(55)
     D  OcPefdt                       7  0
     D  OcPexdt                       7  0
     D  OcPcndt                      10
     D  OcPname                      40
     D  OcPaddr                      50
     D  OcPcity                      30
     D  OcPst                         2
     D  OcPzip                       10
     D  OcPcnty                      25
     D  OcPterr                       3
     D  OcPrst                        2
     D  OcPrzip                      10
     D  OcPsysd                      10
     D  OcPacdt                      10
     D  OcPactive                     1
      *
     D Policy#         DS
     D  Pol1                   1      3
     D  Pol2                   4     12
     D  Pol3                  13     15
      *
     D                 DS
     D  XXPOL                  1     15
     D  X1FND                  1      3  0
     D  X1EMP#                 4     12  0
     D  X1FYR                 13     15  0
      *
     D Hold28          S             28  2
     D Hold20          S             20  2
     D Commpd          S              1n
     D Active          S              1n
     D Tab10           S             10
     D Tabcod          S              4
     D H2              S              2  0
     D H4              S              4  0
     D H6              S              6  0
     D H3              S              3  0
     D SvEupfdt7       S              7  0
     D SvEuptdt7       S              7  0
     D Eupfdt7         S              7  0
     D Euptdt7         S              7  0
     D H2A             S              2
     D H4A             S              4
     D H11A            S             11
     D Date10          S             10
     D I               S              2  0
     D Rragg#          S             28  2
     D Fmtefd          S               D   DATFMT(*USA)
     D Fmtcdt          S               D   DATFMT(*USA)
     D GT999           S              7  0 INZ(9999999)
     D Efdt999         S              7  0 INZ(9999999)
      *
     D PrPpWrtn        S             20  2
     D PrFcPrem        S             20  2
     D PrFcUepr        S             20  2
     D ExPpWrtn        S             20  2
     D ExFcPrem        S             20  2
     D ExFcUepr        S             20  2
     D***  Begin Add  ***  12/18/03  **************************************
     D TrPpWrtn        S             20  2
     D TrFcPrem        S             20  2
     D TrFcUepr        S             20  2
     D***  Begin Add  ***  12/18/03  **************************************
     D PrmCo#          S              3  0
     D PrmCo           S              3
     D PrmMdy          S              6  0
     D PrmMdyA         S              6
     D FromDt          S               D   DATFMT(*USA)
     D ThruDt          S               D   DATFMT(*USA)
     D Today           S               D   DATFMT(*USA)
     D Datymd          S               D   DATFMT(*YMD)
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      /eject
      *
      *
     C     POKY          KLIST
     C                   KFLD                    XXGRP
     C                   KFLD                    XXPOL#
     C                   KFLD                    Date10
      *
      *  EMPLOYER HISTORY FILE
      *
     C     EHKY          KLIST
     C                   KFLD                    SvEuco#
     C                   KFLD                    SvEufnd
     C                   KFLD                    SvEuemp#
     C                   KFLD                    SvEudiv
      *
     C     Wdell8_KeyF   Klist
     C                   KFLD                    SvEuco#
     C                   Kfld                    SvEuFnd
     C                   Kfld                    SvEuEmp#
     C                   Kfld                    SvEuFyr
      *
     C     Wdell8_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    Svemp#
     C                   Kfld                    Svfyr
      *
     C     Wdell8_Key3   Klist
     C                   Kfld                    SvEuco#
     C                   Kfld                    SvEuFnd
     C                   Kfld                    SvEuemp#
     C                   Kfld                    SvEufyr
      *
     C     Wdell2_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    SvEmp#
      *
     C     Wmrpl4_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Svfnd
     C                   Kfld                    Svfyr
     C                   Kfld                    Svemp#
      *
     C     Wmeml_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    SvEmp#
     C                   Kfld                    SvDiv
      *
     C     Wdell1_Key    Klist
     C                   Kfld                    PrmCo#
     C                   Kfld                    XxFnd
     C                   Kfld                    XxFyr
     C                   Kfld                    XxEmp#
     C                   Kfld                    XxDiv
      *
     C     ELKY          KLIST
     C                   KFLD                    SvEuCO#
     C                   KFLD                    SvEuFND
     C                   KFLD                    SvEuFYR
     C                   KFLD                    SvEuEMP#
     C                   KFLD                    SvEuDIV
      *
     C     ELKY1         KLIST
     C                   KFLD                    SvEuCO#
     C                   KFLD                    SvEuFND
     C                   KFLD                    SvEuFYR
     C                   KFLD                    SvEuEMP#
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Kyrtcd
      *
     C     Wmgcl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
      *
     C     Wdehl1_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    Svemp#
     C                   Kfld                    Svdiv
     C                   Kfld                    Svfyr
      *
     C     Wdehl1_SGT    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    Svemp#
     C                   Kfld                    Svdiv
     C                   Kfld                    Svfyr
     C                   Kfld                    GT999
      *
     C     Wfeul_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    Svemp#
     C                   Kfld                    Svfyr
     C                   Kfld                    OcState
      *
     C     Wdrrl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    OcState
     C                   Kfld                    Elpfdt
      *
     C     Wdrrl_Key2    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    OcState
      *
     C     Wdrrl_Keyf    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvEuFnd
     C                   Kfld                    SvEujrst
     C                   Kfld                    SvEupfdt7
      *
     C     Wdrrl_Key2f   Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvEuFnd
     C                   Kfld                    SvEujrst
      *
     C     Wtlml_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    Tab10
      *
     C     Wtddl3_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    OcState
     C                   Kfld                    L8dcde
     C                   Kfld                    L8pfdt
      *
     C     Wtddl3_Key2   Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvFnd
     C                   Kfld                    OcState
     C                   Kfld                    L8dcde
      *
     C     Wtddl3_Keyf   Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvEuFnd
     C                   Kfld                    SvEujrst
     C                   Kfld                    L8dcde
     C                   Kfld                    SvEupfdt7
      *
     C     Wtddl3_Key2f  Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    SvEuFnd
     C                   Kfld                    SvEujrst
     C                   Kfld                    L8dcde
      *
     C     Wtabl_key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Tab10
      *
     C     Polml1_Key    Klist
     C                   Kfld                    Lmgrp
     C                   Kfld                    Lmpol
     C                   Kfld                    Lmfyr
     C                   Kfld                    Lmrst
      *
     C     Ppreml_Key    Klist
     C                   Kfld                    Pppgid
     C                   Kfld                    Pppol
     C                   Kfld                    Pprst
     C                   Kfld                    Ppfyr
     C                   Kfld                    Pptype
      *
     C     Infcl_Key     Klist
     C                   Kfld                    Fcpgid
     C                   Kfld                    Fcpol
     C                   Kfld                    Fcrst
     C                   Kfld                    Fcfyr
     C                   Kfld                    Fctype
      *
     C     *Like         Define    OcPcndt       HldCndt
     C     *Like         Define    Rpcap#        Svpol#
     C     *Like         Define    Pptrcd        Svtrcd
     C     *Like         Define    LMGRP         XXGRP
     C     *Like         Define    LMPOL         XXPOL#
     C     *Like         Define    EMFND         XXFND
     C     *Like         Define    EMEMP#        XXEMP#
     C     *Like         Define    EMDIV         XXDIV
     C     *Like         Define    ELFYR         XXFYR
     C     *Like         Define    EMFND         SVFND
     C     *Like         Define    EMEMP#        SVEMP#
     C     *Like         Define    EMDIV         SVDIV
     C     *Like         Define    ELFYR         SVFYR
     C     *Like         Define    ELPFDT        XXEFDT
     C     *Like         Define    ELPFDT        Taredt
     C     *Like         Define    OcPcndt       HldCnd
     C     *Like         Define    Ememp#        Kyemp#
     C     *Like         Define    Elrtcd        Kyrtcd
     C     *Like         Define    Eldiv         Kydiv
     C     *Like         Define    ElFyr         FirstFyr
     C     *Like         Define    ElFyr         Fyear
     C     *Like         Define    Elpfdt        FirstEfd
     C     *Like         Define    Elptdt        LastExd
     C     *Like         Define    RsJrst        Risk_St
     C     *Like         Define    Ehtrdt        Svtrdt
     C     *Like         Define    Elprtd        Xxexpc
     C     *Like         Define    EuCo#         SvEuCo#
     C     *Like         Define    EuFnd         SvEuFnd
     C     *Like         Define    EuFyr         SvEuFyr
     C     *Like         Define    EuEmp#        SvEuEmp#
     C     *Like         Define    EuDiv         SvEuDiv
     C     *Like         Define    EuJrst        SvEuJrst
      /eject
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   Parm                    PrmCo
     C                   PARM                    QQTDT             8
     C                   PARM                    QQPOL#           15
     C                   PARM                    QQDIV             5 0
      *
     C                   Move      Prmco         Prmco#
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   MOVE      *BLANKS       XXECFL            1
     C                   MOVE      *BLANKS       ZZPOL#           15
      *
     C                   MOVE      QQTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C     *YMD          Move      XXtdt         DatUsa
     C                   Move      DatUsa        Date10
     C     *Mdy0         Move      DatUsa        Prmmdya
     C                   Move      DatUsa        ThruDt
     C                   Move      Prmmdya       Prmmdy
      *
      * Check if parm for single employer
      *
     C     Q1PRMS        IFGT      2
      *
     C                   Eval      XxDiv = *Zero
     C                   Eval      KyDiv = *Zero
     C                   Move      QqDiv         XxDiv
     C                   Move      QqDiv         KyDiv
      *
     C     QQPOL#        IFNE      *BLANKS
     C                   MOVE      QQPOL#        ZZPOL#
     C                   MOVE      ZZPOL#        XXPOL
     C                   MOVE      X1FND         XXFND
     C                   MOVE      X1EMP#        XXEMP#
     C                   MOVE      X1FYR         XXFYR
     C                   MOVE      X1FND         XXFND
     C                   MOVE      X1EMP#        XXEMP#
     C                   MOVE      X1FYR         XXFYR
     C                   ENDIF
      *
     C                   ENDIF
      *
      * CLEAR SAVE VALUES.
      *
     C                   Clear                   SVFND
     C                   Clear                   SVEMP#
     C                   Clear                   SVDIV
     C                   Clear                   SVFYR
     /eject
     **********************************************************************
     *                                                                    *
     * M A I N L I N E   R O U T I N E                                    *
     *                                                                    *
     **********************************************************************
      *
      * If single employer check, verify no record, then build one
      * Else build file.
      *
     C                   If        ZzPol# = *Blanks
     C                   Exsr      Proc_File
     C                   Else
     C                   Exsr      Proc_Record
     C                   Endif
      *
     C                   MOVE      *ON           *INLR
      *
      /eject
      **********************************************************************
      * Process one policy only
      **********************************************************************
      *
     C     Proc_Record   Begsr
      *
     C                   MOVE      Zzpol#        Xxpol
     C                   MOVE      X1fnd         Xxfnd
     C                   MOVE      X1emp#        Xxemp#
     C                   MOVE      X1emp#        Kyemp#
     C                   MOVE      X1fyr         Xxfyr
     C                   MOVE      X1fnd         Svfnd
     C                   MOVE      X1EMP#        SVEMP#
     C                   MOVE      X1FYR         SVFYR
     C                   Eval      Fyear = X1fyr
      *
     C                   Exsr      Get_Pol#
     C                   Exsr      Get_State
      *
      * Check to see if records already exists
      *
      *** beg add ***** 11/15/04 *******************************************************
     c                   move(p)   xxfnd         lmgrp
     c                   eval      lmpol = svpol#
     c                   eval      lmfyr = xxfyr
     c                   eval      lmrst = risk_st
      *** end add ***** 11/15/04 *******************************************************
     C     Polml1_Key    Chain     Polml1
      *
     C                   If        Not %Found
      *
      * Get program id for first field in each file
     C                   Exsr      Get_Pgmid
     C                   Exsr      Get_Pol#
     C                   Exsr      Bld_Pol_Ary
     C                   Exsr      Proc_Pol_Ary
     C                   Endif
      *
     C                   Endsr                                                  ZZPOL# IFNE
      *
      /eject
      *
      **********************************************************************
      * Build entire file
      **********************************************************************
      *
     C     Proc_File     Begsr                                                  ZZPOL# IFNE
      *
      * Read Report Workfile
      *
     C                   Eval      First = *On
      *
     C                   Dou       %Eof(Wfeul)
     C                   Read      Wfeul
      *
     C                   If        %Eof(Wfeul)
      *** Begin Add ****** 08/12/02 ****************************************
     C                   Exsr      Write_Files
      *** End   Add ****** 08/12/02 ****************************************
     C                   Leave
     C                   Endif
      *
     C                   If        First
     C                   Eval      SvEuCo#  = EuCo#
     C                   Eval      SvEuFnd  = EuFnd
     C                   Eval      SvEuEmp# = EuEmp#
     C                   Eval      SvEuDiv  = EuDiv
     C                   Eval      SvEuFyr  = EuFyr
     C                   Eval      SvEuJrst = EuJrst
     C                   Eval      First = *Off
     C                   Endif
      *
     C                   Clear                   Ppp01
     C                   Clear                   Plm01
     C                   Clear                   Ifc01
      *
     C                   If        SvEuCo#  = EuCo#  and
     C                             SvEuFnd  = EuFnd  and
     C                             SvEuEmp# = EuEmp# and
     C                             SvEuFyr  = EuFyr  and
     C                             SvEuJrst = EuJrst
     C***  Begin Chg  ***  12/18/03  **************************************
     C**********         Eval      PrPpwrtn = PrPpwrtn + EuMdwr
     C**********         Eval      PrFcPrem = PrFcPrem + EuInpr
     C**********         Eval      PrFcUepr = PrFcUepr + EuUnea
     C                   Eval      PrPpwrtn = PrPpwrtn + EuMdwr - EUMDTR
     C                   Eval      PrFcPrem = PrFcPrem + EuInpr - EUITER
     C                   Eval      PrFcUepr = PrFcUepr + EuUnea - EUUNTR
     C***  End   Chg  ***  12/18/03  **************************************
     C                   Eval      ExPpwrtn = ExPpwrtn + EuMwex
     C                   Eval      ExFcPrem = ExFcPrem + EuInex
     C                   Eval      ExFcUepr = ExFcUepr + EuUnex
     C***  Begin Add  ***  12/18/03  **************************************
     C                   Eval      TrPpWrtn = TrPpWrtn + EUMDTR
     C                   Eval      TrFcPrem = TrFcPrem + EUITER
     C                   Eval      TrFcUepr = TrFcUepr + EUUNTR
     C***  End   Add  ***  12/18/03  **************************************
      *
     C                   Else
      *
     C                   Exsr      Write_Files
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr                                                  ZZPOL# IFNE
      *
      /eject
      *** Begin Add ****** 08/12/02 ****************************************
     **********************************************************************
     * Get the key and state for parm received.
     *
     **********************************************************************
      *
     C     Write_Files   Begsr
      *
      * Get program ID for first field in each file
      *
     C                   Eval      Svfnd   = SvEufnd
     C                   Eval      Svfyr   = SvEufyr
     C                   Eval      Fyear   = SvEufyr
     C                   Eval      Risk_St = SvEuJrst
     C                   Eval      Svemp#  = SvEuemp#
     C                   Eval      Svdiv   = SvEudiv
     C                   Exsr      Get_Pgmid
     C                   Exsr      Get_Pol#
     C                   Exsr      Get_Wdelp
     C                   Exsr      Get_Cnx_Dt
     C                   Exsr      New_Rnw_Year
     C                   Exsr      Get_Tr_Cd                                    PPremp
     C                   Exsr      Eff_Date_St
     C                   Exsr      Write_Polmp_F
     C                   Exsr      Write_Premp_F
     C                   Exsr      Write_Infcp_F
      *
     C***  Begin Chg  ***  12/18/03  **************************************
     C**********         Eval      PrPpwrtn = EuMdwr
     C**********         Eval      PrFcPrem = EuInpr
     C**********         Eval      PrFcUepr = EuUnea
     C                   Eval      PrPpwrtn = EuMdwr - EUMDTR
     C                   Eval      PrFcPrem = EuInpr - EUITER
     C                   Eval      PrFcUepr = EuUnea - EUUNTR
     C***  End   Chg  ***  12/18/03  **************************************
     C                   Eval      ExPpwrtn = EuMwex
     C                   Eval      ExFcPrem = EuInex
     C                   Eval      ExFcUepr = EuUnex
     C***  Begin Add  ***  12/18/03  **************************************
     C                   Eval      TrPpWrtn = EUMDTR
     C                   Eval      TrFcPrem = EUITER
     C                   Eval      TrFcUepr = EUUNTR
     C***  End   Add  ***  12/18/03  **************************************
      *
     C                   Move      Eupfusa       SvEupfusa
     C                   Move      Euptusa       SvEuptusa
     C                   Eval      SvEuCo#  = EuCo#
     C                   Eval      SvEuFnd  = EuFnd
     C                   Eval      SvEuEmp# = EuEmp#
     C                   Eval      SvEuDiv  = EuDiv
     C                   Eval      SvEuFyr  = EuFyr
     C                   Eval      SvEuJrst = EuJrst
      *
     C                   Endsr                                                  ZZPOL# IFNE
      *
      *** End   Add ****** 08/12/02 ****************************************
      /eject
     **********************************************************************
     * Get the key and state for parm received.
     *
     **********************************************************************
      *
     C     Get_State     Begsr
      *
     C     Wdell1_Key    Chain     Wdell1
      *
     C                   If        %Found
     C                   Eval      Kyrtcd = L1rtcd
     C                   Exsr      Get_Risk_St
     C                   Endif
      *
     C                   Endsr                                                  ZZPOL# IFNE
      *
      /eject
     **********************************************************************
     * Get detail record for correct state, when processing by file
     *
     **********************************************************************
      *
     C     Get_Wdelp     Begsr
      *
      * Read Wdell8 for Co, Fund, Employer, and Fyr
      *
     C     Wdell8_Key    Setll     Wdell8
      *
     C                   Dou       %eof(Wdell8)
      *
     C     Wdell8_Key    Reade     Wdell8
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
      * Get Risk State from Rate Set Code Master File
      *
     C                   Eval      Kyrtcd = L8rtcd
     C                   Exsr      Get_Risk_St
      *
     C                   If        Risk_St = SvEujrst
     C                   Leave
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr                                                  ZZPOL# IFNE
      *
      /eject
     **********************************************************************
     * GET TRANSACTION CODE AND DATE
     * DETERMINE IF NEW, RENEW, OR AUDITED.
     **********************************************************************
      *
     C     Get_Tr_Cd     Begsr
      *
     C                   If        Elfaf = 'A'
      *                        * Audited *
     C                   Eval      Svtrcd = '17'
     C                   Exsr      Get_Audit_Dt
     C                   Else
      *
     C                   If        Elfyr <> FirstFyr
      *                        * Renew *
     C                   Eval      Svtrcd = '11'
     C                   Eval      Svtrdt = Elpfdt
     C                   Else
      *                        * New *
     C                   Eval      Svtrcd = '10'
     C                   Eval      Svtrdt = Elpfdt
     C                   Endif
      *
     C                   Endif
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Read Employer Master File to determine if ACTIVE.
     **********************************************************************
      *
     C     Get_Cnx_Dt    Begsr
      *
     C                   Eval      Active = *Off
      *
     C     Wmeml_Key     Chain     Wmeml
      *
     C                   If        Not %Eof
      *
     C                   If        %Found
     C                   Eval      i = 1
      *
     C                   If        Effdates(I) = *zero
     C                   Eval      Active = *On
     C                   Eval      HldCndt = *Blanks
     C                   Else
      *
      * Get the most recent cancel date.
      *
     C     *Zero         Lookup    Effdates(I)                            58
      *
     C                   If        *in58 = *on
     C                   Eval      I = I - 1
     C                   Else
     C                   Eval      I = 5
     C                   Endif
      *
      * At this point, we have the most current Cancel Date
      *
     C                   If        Cxdates(I) > *Zero
     C     *Ymd          Move      Cxdates(I)    Datusa
     C     *Ymd          Move      Cxdates(I)    Datymd
      *
     C                   If        Cxdates(I) <= L8ptdt and
     C                             (DatYmd <= ThruDt)
     C                   Eval      Active = *Off
     C                   Move      Datusa        HldCndt
     C                   Else
     C                   Eval      Active = *On
     C                   Eval      HldCndt = *Blanks
     C                   Endif
     C                   Else
     C                   Eval      Active = *On
     C                   Eval      HldCndt = *Blanks
     C                   Endif
      *
     C                   Endif
      *
     C                   Endif
      *
     C                   Else
     C                   Eval      Active = *On
     C                   Endif
      *
     C                   Endsr
      *
      /eject
     **********************************************************************
     *  Get Audit Date From Workers' Comp Employer Detail History File
     **********************************************************************
      *
     C     Get_Audit_Dt  Begsr
      *
     C                   Clear                   Svtrdt
     C     Wdehl1_SGT    Setgt     Wdehl1
      *
     C                   Dou       %eof(Wdehl1)
      *
     C     Wdehl1_Key    Readpe    Wdehl1
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        Ehfaf = 'A'
     C                   Eval      Svtrdt = Ehtrdt

     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Get First Year for Employer New-Renewal across all states
     **********************************************************************
      *
     C     New_Rnw_Year  Begsr
      *
     C                   Clear                   FirstFyr
     C                   Clear                   FirstEfd
     C                   Eval      FirstL2 = *On
      *
     C     Wdell2_Key    Setll     Wdell2
      *
     C                   Dou       %eof(Wdell2)
      *
     C     Wdell2_Key    Reade     Wdell2
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        FirstL2
     C                   Eval      FirstEfd = L2Pfdt
     C                   Eval      FirstFyr = L2Fyr
     C                   Eval      FirstL2 = *Off
     C                   Endif
      *
     C                   If        L2pfdt < FirstEfd
     C                   Eval      FirstEfd = L2Pfdt
     C                   Eval      FirstFyr = L2Fyr
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Get Earliest Effective Date for State /Year
     **********************************************************************
      *
     C     Eff_Date_St   Begsr
      *
     C                   Clear                   FirstEfd
     C                   Clear                   LastExd
      *
     C     Wdell2_Key    Setll     Wdell2
      *
     C                   Dou       %eof(Wdell2)
      *
     C     Wdell2_Key    Reade     Wdell2
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        L2Fyr <> Fyear
     C                   Iter
     C                   Endif
      *
     C                   Eval      Kyrtcd = L2rtcd
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
      *
     C                   If        RsJrst = Risk_St
      *
     C                   If        L2pfdt < FirstEfd or
     C                             FirstEfd = *Zero
     C                   Eval      FirstEfd = L2Pfdt
     C                   Endif
      *
     C                   If        L2ptdt > LastExd or
     C                             LastExd = *Zero
     C                   Eval      LastExd = L2ptdt
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Build Policy Array for each state from Wdelp
     **********************************************************************
      *
     C     Bld_Pol_Ary   Begsr
      *
     C                   Eval      Occurs = 1
      *
     C                   Do        55
     C     Occurs        Occur     OcPol
     C                   Clear                   OcPol
     C                   Eval      Occurs = Occurs + 1
     C                   Enddo
      *
      * Read Wdell8 for Co, Fund, Employer, and Fyr
      *
     C     Wdell8_Key    Setll     Wdell8
      *
     C                   Dou       %eof(Wdell8)
      *
     C     Wdell8_Key    Reade     Wdell8
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
      * Get Risk State from Rate Set Code Master File
      *
     C                   Eval      Kyrtcd = L8rtcd
     C                   Exsr      Get_Risk_St
      *
     C                   Eval      Kyemp# = L8emp#
     C                   Eval      Kydiv  = L8div
     C                   Eval      Svfyr  = L8fyr
     C                   Eval      Svdiv  = L8div
     C                   Eval      Svemp# = L8emp#
      *
     C                   Eval      Occurs = 1
     C                   Exsr      Get_Cnx_Dt
      *
     C                   Exsr      Eff_Date_St
      *
     C     *Mdy          Move      PrmMdy        Datusa
      *
     C                   Do        55
     C     Occurs        Occur     OcPol
      *
     C                   If        OcPrst  = *Blank
      *
     C                   Eval      OcPname = Emname
      *
     C                   If        Emmpfl = 'M'
     C                   Eval      OcPaddr = Emmad1
     C                   Eval      OcPcity = Emmcty
     C                   Eval      OcPst   = Emmst
     C                   Eval      H11A    = %Editw(Emmzip:Editzip)
     C                   Move      H11A          OcPzip
     C                   Movel     Emmst         H4a
     C                   Else
     C                   Eval      OcPaddr = Empad1
     C                   Eval      OcPcity = Empcty
     C                   Eval      OcPst   = Empst
     C                   Eval      H11A    = %Editw(Empzip:Editzip)
     C                   Move      H11A          OcPzip
     C                   Movel     Empst         H4a
     C                   Endif
     C                   Move      Emcnty        H4a
     C                   Movel(p)  H4a           Tab10
      *
     C     Wtabl_Key     Chain     Pcn01
      *
     C                   If        %Found
     C                   Movel     Cndesc        OcPcnty
     C                   Else
     C                   Clear                   OcPcnty
     C                   Endif
      *
     C                   Eval      H11A   = %Editw(Emmzip:Editzip)
     C                   Move      H11A          OcPrzip
      *
     C                   Eval      OcPefdt = Firstefd
     C                   Eval      OcPexdt = LastExd
      *
     C                   Eval      OcPterr = *blank
     C     *Usa          Move      Today         OcPsysd
     C     *Usa          Move      ThruDt        OcPacdt
     C                   Eval      OcPrst = Risk_St
      *
     C                   If        Emmzip = *Zero
     C                   Eval      H11A   = %Editw(Empzip:Editzip)
     C                   Move      H11A          OcPrzip
     C                   Else
     C                   Eval      H11A   = %Editw(Emmzip:Editzip)
     C                   Move      H11A          OcPrzip
     C                   Endif
      *
     C                   If        Active
     C                   Eval      OcPactive = 'Y'
     C                   Eval      OcPcndt = *Blank
     C                   Else
     C                   Eval      OcPactive = 'N'
     C                   Eval      OcPcndt = HldCndt
      *
     C                   If        OcPefdt <> *Zero
     C     *Ymd          Move      Ocpefdt       Fmtefd
     C     *Usa          Move      OcPcndt       Fmtcdt
      *
     C                   If        Fmtcdt < Fmtefd
     C                   Move      FmtEfd        OcPcndt
     C                   Endif
     C                   Endif
      *
     C                   Endif
      *
     C                   Leave
     C                   Endif
      *
     C                   If        OcPrst  = Risk_St
      *
     C                   If        Active
     C                   Eval      OcPactive = 'Y'
     C                   Else
     C                   Eval      OcPactive = 'N'
     C                   Endif
      *
     C                   Leave
     C                   Endif
      *
     C                   Eval      Occurs = Occurs + 1
     C                   Enddo
      *
     C                   Enddo
     C                   Endsr
      *
      /eject
     **********************************************************************
     *  Process Policy Array for each state.
     **********************************************************************
      *
     C     Proc_Pol_Ary  Begsr
      *
     C                   Eval      Occurs = 1
      *
     C                   Do        55
     C     Occurs        Occur     OcPol
      *
     C                   If        OcPrst <> *blank
      *
     C                   Eval      Kyemp# = Elemp#
     C                   Exsr      Get_Pol#
     C                   Exsr      Write_Polmp
      *
     C                   Else
     C                   Leave
     C                   Endif
      *
     C                   Eval      Occurs = Occurs + 1
     C                   Enddo
      *
     C                   Endsr
      *
      /eject
     **********************************************************************
     *  Get File WMRPL4 for Policy Number
     **********************************************************************
      *
     C     Get_Pol#      Begsr
      *
     C     Wmrpl4_Key    Chain     Wmrpl4
      *
     C                   If        %Found and
     C                             Rpcap# <> *blank
     C                   Clear                   Svpol#
     C                   Eval      Svpol# = Rpcap#
      *
     C                   Else
     C                   Move      SvFnd         Pol1
     C                   Move      Svemp#        Pol2
     C                   Move      Svfyr         Pol3
     C                   Movel(p)  Policy#       Svpol#
     C                   Endif
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Get Risk State from Rate Set Code Master File
     **********************************************************************
      *
     C     Get_Risk_St   Begsr
      *
     C     Wmrsl_Key     Chain     Wmrsl
      *
     C                   If        %Found
     C                   Eval      Risk_St = Rsjrst
     C                   Else
     C                   Eval      Risk_St = *blank
     C                   Endif
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Get File Wmgcl
     **********************************************************************
      *
     C     Get_Pgmid     Begsr
      *
     C     Wmgcl_Key     Chain     Wmgcl
      *
     C                   If        not %Found
     C                   Eval      Gcpgid  = *blank
     C                   Endif
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Write Policy Master For Clarendon Pre Processing
     **********************************************************************
     C     Write_Polmp   Begsr
      *
     C                   Movel(p)  Gcpgid        Lmpgid
     C                   Move(p)   Svfnd         Lmgrp
     C                   Movel(p)  Svpol#        Lmpol
     C                   Eval      Lmname = OcPname
     C                   Eval      Lmaddr = OcPaddr
     C                   Eval      Lmcity = OcPcity
     C                   Eval      Lmst   = OcPst
     C                   Eval      Lmzip  = OcPzip
     C                   Eval      Lmcnty = OcPcnty
     C                   Eval      Lmrzip = OcPrzip
      *
     C                   If        OcPefdt = *Zero
     C                   Eval      Lmefdt  = *Blanks
     C                   Else
     C     *Ymd          Move      OcPefdt       Datusa
     C                   Move      Datusa        Lmefdt
     C                   Endif
      *
     C                   If        OcPexdt = *Zero
     C                   Eval      Lmexdt  = *Blanks
     C                   Else
     C     *Ymd          Move      OcPexdt       Datusa
     C                   Move      Datusa        Lmexdt
     C                   Endif
      *
     C                   Eval      Lmterr = OcPterr
     C                   Eval      Lmsysd = OcPsysd
     C                   Eval      Lmacdt = OcPacdt
     C                   Eval      Lmrst  = OcPrst
      *
     C                   If        OcPactive = 'Y'
     C                   Eval      Lmcndt = *Blanks
     C                   Else
     C                   Eval      Lmcndt = OcPcndt
     C                   If        Lmcndt <> *Blanks and
     C                             Lmefdt <> *Blanks
     C     *Usa          Move      Lmefdt        UsaEff
     C     *Usa          Move      Lmcndt        UsaCnx
     C                   If        UsaCnx < UsaEff
     C                   Move      Lmefdt        Lmcndt
     C                   Endif
     C                   Endif
     C                   Endif
      *
     C     Polml1_Key    Chain     Polml1
      *
     C                   If        Not %Found
     C                   Eval      Lmfyr  = Svfyr
     C                   Eval      Lmfnd  = Svfnd
     C                   Eval      Lmemp# = Svemp#
     C                   Write     Plm01
     C                   Endif
      *
     C                   Endsr
      *
      /eject
     **********************************************************************
     *  Write Policy Master For Clarendon Pre Processing by file
     **********************************************************************
      *
     C     Write_Polmp_F Begsr
      *
     C                   Movel(p)  Gcpgid        Lmpgid
     C                   Move(p)   SvEufnd       Lmgrp
     C                   Movel(p)  Svpol#        Lmpol
     C                   Eval      Lmfyr  = SvEufyr
     C                   Eval      Lmfnd  = SvEufnd
     C                   Eval      Lmemp# = SvEuemp#
     C                   Eval      Lmname = Emname
     C                   If        Emmpfl = 'M'
     C                   Eval      LmAddr  = Emmad1
     C                   Eval      LmCity  = Emmcty
     C                   Eval      Lmst    = Emmst
     C                   Eval      H11A    = %Editw(Emmzip:Editzip)
     C                   Move      H11A          Lmrzip
     C                   Move      H11A          Lmzip
     C                   Movel     Emmst         H4a
     C                   Else
     C                   Eval      LmAddr  = Empad1
     C                   Eval      LmCity  = Empcty
     C                   Eval      Lmst    = Empst
     C                   Eval      H11A    = %Editw(Emmzip:Editzip)
     C                   Move      H11A          Lmrzip
     C                   Eval      H11A    = %Editw(Empzip:Editzip)
     C                   Move      H11A          Lmzip
     C                   Movel     Empst         H4a
     C                   Endif
     C                   Move      Emcnty        H4a
     C                   Movel     Emmst         H4a
     C                   Movel(p)  H4a           Tab10
      *
     C     Wtabl_Key     Chain     Pcn01
      *
     C                   If        %Found
     C                   Movel     Cndesc        Lmcnty
     C                   Else
     C                   Clear                   Lmcnty
     C                   Endif
      *
     C                   Eval      H11A   = %Editw(Emmzip:Editzip)
     C                   Move      H11A          Lmrzip
      *
     C                   If        Firstefd = *Zero
     C                   Eval      Lmefdt  = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Lmefdt
     C                   Endif
      *
     C                   If        Lastexd = *Zero
     C                   Eval      Lmexdt  = *Blanks
     C                   Else
     C     *Ymd          Move      Lastexd       Datusa
     C                   Move      Datusa        Lmexdt
     C                   Endif
      *
     C                   Eval      Lmterr = *Blank
     C     *Usa          Move      Today         Lmsysd
     C     *Usa          Move      ThruDt        Lmacdt
     C                   Eval      Lmrst  = SvEujrst
      *
     C                   If        Active
     C                   Eval      Lmcndt = *Blanks
     C                   Else
     C                   Eval      Lmcndt = Hldcndt
     C                   If        Lmcndt <> *Blanks and
     C                             Lmefdt <> *Blanks
     C     *Usa          Move      Lmefdt        UsaEff
     C     *Usa          Move      Lmcndt        UsaCnx
     C                   If        UsaCnx < UsaEff
     C                   Move      Lmefdt        Lmcndt
     C                   Endif
     C                   Endif
     C                   Endif
      *
     C     Polml1_Key    Chain     Polml1
      *
     C                   If        Not %Found
     C                   Write     Plm01
     C                   Endif
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Write Policy Premium For Clarendon Pre Processing from file
     **********************************************************************
      *
     C     Write_Premp_F Begsr
      *
     C                   Clear                   Ppwrt#
     C                   Clear                   Ppded#
     C                   Clear                   Rragg#
      *
      * Get deductibles
      *
     C     Wtddl3_KeyF   Setgt     Wtddl3
     C     Wtddl3_Key2F  Readpe    Wtddl3
      *
     C                   If        not %Eof
     C*** BEG ADD *****  10/11/07  ************************************
     C                              and ddstat = 'A'
     C*** END ADD *****  10/11/07  ************************************
     C                   Eval      Ppded# = Ddampo
     C                   Endif
      *
     C                   Movel(P)  L8ilcd        Tab10
     C     Wtlml_key     Chain     Wtlml
      *
     C                   If        %Found
     C                   Eval      Rragg# = Llagg * 1000
     C                   Else
      *
      * Get Limits
      *
     C     Wdrrl_keyf    Setgt     Wdrrl
     C     Wdrrl_key2f   Readpe    Wdrrl
      *
     C                   If        %Found
     C                   Eval      Rragg# = Rragg * 1000
     C                   Endif
     C                   Endif
      *
     C                   Eval      Pplmts = %editc(Rragg#:'P')
      *
      * Write Primary Premium Record
      *
     C                   Movel(p)  Gcpgid        Pppgid
     C                   Movel(p)  Svpol#        Pppol
     C                   Eval      Pptype = '10'
     C                   Movel     '16'          Ppasln
     C                   Eval      Pplob  = 'WC'
     C                   Eval      Pptrcd = Svtrcd
      *
     C                   Eval      Ppwrtn = %editc(PrPpwrtn:'P')
     C                   Eval      Ppwpr# = PrPpwrtn
      *
     C                   Eval      Svtrdt = Elpfdt
      *
     C                   Eval      Ppdeds = %editc(Ppded#:'P')
     C                   Eval      Pprst  = SvEujrst
      *
     C                   If        Firstefd = *Zero
     C                   Eval      Ppefdt = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Ppefdt
     C                   Endif
      *
     C                   If        Svtrcd = '10' or
     C                             Svtrcd = '11'
      *
     C                   If        Firstefd = *Zero
     C                   Eval      Pptrdt = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Pptrdt
     C                   Endif
     C                   Else
      *
     C                   If        Svtrcd = '17'
     C     *Ymd          Move      Ehtrdt        Datusa
      *
     C                   If        Ehtrdt = *Zero
     C                   Eval      PPtrdt = *Blanks
     C                   Else
     C                   Move      Datusa        Pptrdt
     C                   Endif
      *
     C                   Endif
     ************************************************************
     ************************************************************
      *
      * FIND THE LATEST REINSTATEMENT DATE IF ALL DIVISIONS ARE REINSTATED.
      *
     C     Elky          Setgt     Wdcxl
     C     Elky          Readpe    Wdcxl
     C                   If        %Found
      *
     C                   If        Cxredt > Taredt
     C                   Z-add     Cxredt        Taredt
     C                   Endif
     C                   Else
     C                   Z-add     9999999       Taredt
     C                   Endif
      *
     C                   If        Elfaf = 'A'
     C                   Move      '17'          PPTRCD
      *
     C     Ehky          Setgt     Peh01
     C     Ehky          Readpe    Peh01
     C                   Dou       not %Found and
     C                             Ehfaf <> 'A'
     C     Ehky          Readpe    Peh01
     C                   Enddo
      *
     C                   If        %Found
     C     *Ymd          Move      Ehtrdt        Datusa
     C                   Move      Datusa        Pptrdt
     C                   Endif
      *
     C                   Else                                                   ELFAF IFEQ
      *
     C                   If        Cxdates(I) <> 9999999 and
     C                             Cxdates(I) <> *Zero
     C                   Move      '15'          Pptrdt
     C     *Ymd          Move      Cxdates(I)    Datusa
     C                   Move      Datusa        Pptrdt
     C                   Else
      *
     C                   If        Taredt <> 9999999 and
     C                             Taredt <> *Zero
     C                   Move      '23'          Pptrdt
     C     *Ymd          Move      Taredt        Datusa
     C                   Move      Datusa        Pptrdt
     C                   Else                                                   *IN60 IFEQ
      *
     C                   If        Elpfdt <> Emefd1
     C                   Move      '11'          PPTRCD
     C     *Ymd          Move      Elpfdt        DATUSA
     C                   Move      Datusa        PPTRDT
     C                   Else
     C                   Move      '10'          Pptrcd
     C     *Ymd          Move      Elpfdt        Datusa
     C                   Move      Datusa        Pptrdt
     C                   Endif                                                  ELPFDT IFNE
      *
     C                   Endif
     C                   Endif
     C                   Endif
     ************************************************************
     ************************************************************
     C                   Endif
      *
     C     *Usa          Move      Today         Ppsy
     C     *Usa          Move      ThruDt        Ppac
     C                   Eval      Ppfyr  = SvEufyr
     C                   Eval      Ppfnd  = SvEufnd
     C                   Eval      PpEmp# = SvEuemp#
      *
     C     Ppreml_Key    Chain     Ppreml
     C                   If        not %Found
     C                   Eval      Ppfyr  = SvEufyr
     C                   Eval      Ppfnd  = SvEufnd
     C                   Eval      Ppemp# = SvEuemp#
     C                   Write     Ppp01
     C                   Endif
      *
      * Write Expense Constant Record
      *
     C                   Movel(p)  Gcpgid        Pppgid
     C                   Movel(p)  Svpol#        Pppol
     C                   Eval      Pptype = '12'
     C                   Movel     '16'          Ppasln
     C                   Eval      Pplob  = 'WC'
     C                   Eval      Pptrcd = Svtrcd
     C                   Eval      Ppwrt# = Elprtd
     C                   Eval      Ppwrtn = %editc(ExPpWrtn:'P')
     C                   Eval      Ppwpr# = ExPpWrtn
     C                   Eval      Pplmts = *Zero
     C                   Eval      Pplmts = %editc(Rragg#:'P')
     C                   Eval      Ppdeds = *Zero
     C                   Eval      Ppdeds = %editc(Ppded#:'P')
      *
     C                   If        Firstefd = *Zero
     C                   Eval      Ppefdt = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Ppefdt
     C                   Endif
      *
     C                   If        Svtrcd = '10' or
     C                             Svtrcd = '11'
     C                   If        Firstefd = *Zero
     C                   Eval      Pptrdt = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Pptrdt
     C                   Endif
      *
     C                   Else
      *
     C                   If        Svtrcd = '17'
     C     *Ymd          Move      Ehtrdt        Datusa
      *
     C                   If        Ehtrdt = *Zero
     C                   Eval      Pptrdt = *Blanks
     C                   Else
     C                   Move      Datusa        Pptrdt
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C     *Usa          Move      Today         Ppsy
     C     *Usa          Move      ThruDt        Ppac
     C                   Eval      Ppfyr  = SvEufyr
     C                   Eval      Ppfnd  = SvEufnd
     C                   Eval      PpEmp# = SvEuemp#
      *
     C     Ppreml_Key    Chain     Ppreml
      *
     C                   If        not %Found
     C                   Eval      Ppfyr  = SvEufyr
     C                   Eval      Ppfnd  = SvEufnd
     C                   Eval      Ppemp# = SvEuemp#
     C                   Write     Ppp01
     C                   Endif
     C*
     C***  Begin Add  ***  12/18/03  **************************************
      *
      * Write Terrorism Record
      *
     C                   Movel(p)  Gcpgid        Pppgid
     C                   Movel(p)  Svpol#        Pppol
     C                   Eval      Pptype = '14'
     C                   Movel     '16'          Ppasln
     C                   Eval      Pplob  = 'WC'
     C                   Eval      Pptrcd = Svtrcd
     C                   Eval      Ppwrt# = Elprtd
     C                   Eval      Ppwrtn = %editc(TrPpWrtn:'P')
     C                   Eval      Ppwpr# = TrPpWrtn
     C                   Eval      Pplmts = *Zero
     C                   Eval      Pplmts = %editc(Rragg#:'P')
     C                   Eval      Ppdeds = *Zero
     C                   Eval      Ppdeds = %editc(Ppded#:'P')
      *
     C                   If        Firstefd = *Zero
     C                   Eval      Ppefdt = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Ppefdt
     C                   Endif
      *
     C                   If        Svtrcd = '10' or
     C                             Svtrcd = '11'
     C                   If        Firstefd = *Zero
     C                   Eval      Pptrdt = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Pptrdt
     C                   Endif
      *
     C                   Else
      *
     C                   If        Svtrcd = '17'
     C     *Ymd          Move      Ehtrdt        Datusa
      *
     C                   If        Ehtrdt = *Zero
     C                   Eval      Pptrdt = *Blanks
     C                   Else
     C                   Move      Datusa        Pptrdt
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C     *Usa          Move      Today         Ppsy
     C     *Usa          Move      ThruDt        Ppac
     C                   Eval      Ppfyr  = SvEufyr
     C                   Eval      Ppfnd  = SvEufnd
     C                   Eval      PpEmp# = SvEuemp#
      *
     C     Ppreml_Key    Chain     Ppreml
      *
     C                   If        not %Found
     C                   Eval      Ppfyr  = SvEufyr
     C                   Eval      Ppfnd  = SvEufnd
     C                   Eval      Ppemp# = SvEuemp#
     C                   Write     Ppp01
     C                   Endif
     C***  End   Add  ***  12/18/03  **************************************
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Write Inforce Premium For Clarendon Pre Processing for file
     **********************************************************************
      *
     C     Write_Infcp_F Begsr
      *
     C                   Clear                   Fcpre#
     C                   Clear                   Fcuep#
      *
     C                   Movel(p)  Gcpgid        Fcpgid
     C                   Movel(p)  Svpol#        Fcpol
     C                   Eval      Fctype = '10'
     C                   Movel     '16'          Fcasln
     C                   Eval      Fclob  = 'WC'
     C     *Usa          Move      ThruDt        Fceomd
      *
     C                   Eval      Fcprem = %editc(PrFcprem:'P')
     C                   Eval      Fcprm# = PrFcprem
      *
     C                   If        PrFcuepr < *Zero
     C                   Clear                   PrFcuepr
     C                   Endif
      *
     C                   Eval      Fcuepr = %editc(PrFcuepr:'P')
     C                   Eval      Fcue#  = PrFcuepr
      *
     C                   If        Firstefd = *Zero
     C                   Eval      Fcefdt = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Fcefdt
     C                   Endif
      *
     C                   Eval      Fcrst  = SvEujrst
     C                   Eval      Fcfyr  = SvEufyr
     C                   Eval      Fcfnd  = SvEufnd
     C                   Eval      Fcemp# = SvEuemp#
      *
     C     Infcl_Key     Chain     Infcl
     C                   If        not %Found
     C                   Write     Ifc01
     C                   Endif
      *
     C                   Movel(p)  Gcpgid        Fcpgid
     C                   Movel(p)  Svpol#        Fcpol
     C                   Eval      FCTYPE = '12'
     C                   Movel     '16'          Fcasln
     C                   Eval      FCLOB  = 'WC'
     C     *Usa          Move      Thrudt        FCEOMD
     C                   Eval      Fcprem = %editc(ExFcprem:'P')
     C                   Eval      Fcprm# = ExFcprem
      *
     C                   If        ExFcuepr < *Zero
     C                   Clear                   ExFcuepr
     C                   Endif
      *
     C                   Eval      Fcuepr = %editc(ExFcuepr:'P')
     C                   Eval      Fcue#  = ExFcuepr
      *
     C                   If        Firstefd = *Zero
     C                   Eval      Fcefdt = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Fcefdt
     C                   Endif
      *
     C                   Eval      Fcrst  = SvEujrst
     C                   Eval      Fcfyr  = SvEufyr
     C                   Eval      Fcfnd  = SvEufnd
     C                   Eval      Fcemp# = SvEuemp#
      *
     C     Infcl_Key     Chain     Infcl
     C                   If        not %Found
     C                   Write     Ifc01
     C                   Endif
      *
     C***  Begin Add  ***  12/18/03  **************************************
     C* Terrorisom
     C*
     C                   Movel(p)  Gcpgid        Fcpgid
     C                   Movel(p)  Svpol#        Fcpol
     C                   Eval      FCTYPE = '14'
     C                   Movel     '16'          Fcasln
     C                   Eval      FCLOB  = 'WC'
     C     *Usa          Move      Thrudt        FCEOMD
     C                   Eval      Fcprem = %editc(TrFcPrem:'P')
     C                   Eval      Fcprm# = TrFcPrem
      *
     C                   If        TrFcuepr < *Zero
     C                   Clear                   TrFcuepr
     C                   Endif
      *
     C                   Eval      Fcuepr = %editc(TrFcuepr:'P')
     C                   Eval      Fcue#  = TrFcuepr
      *
     C                   If        Firstefd = *Zero
     C                   Eval      Fcefdt = *Blanks
     C                   Else
     C     *Ymd          Move      Firstefd      Datusa
     C                   Move      Datusa        Fcefdt
     C                   Endif
      *
     C                   Eval      Fcrst  = SvEujrst
     C                   Eval      Fcfyr  = SvEufyr
     C                   Eval      Fcfnd  = SvEufnd
     C                   Eval      Fcemp# = SvEuemp#
      *
     C     Infcl_Key     Chain     Infcl
     C                   If        not %Found
     C                   Write     Ifc01
     C                   Endif
     C***  End   Add  ***  12/18/03  **************************************
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Initial Routine
     **********************************************************************
      *
     C     *Inzsr        Begsr
      *
     C                   Eval      Commpd = *Off
      *
     **********************************************************************
      *
      *  Check to see if commission paid on expense constant
      *  Indicator is "Commpd".
      *
     C                   Eval      Commpd = *Off
     C     PrmCo#        Setll     Wtttl1
      *
     C                   Dou       %Eof
     C     PrmCo#        Reade     Wtttl1
      *
     C                   If        not %eof and
     C                             Ttstat = 'A' and
     C                             Tttype = 'P' and
     C                             Ttctyp = 'E'
      *
     C                   If        Ttecfl <> 'Y'
     C                   Eval      Commpd = *On
     C                   Endif
      *
     C                   Leave
      *
     C                   Endif
      *
     C                   Enddo
      *
     C                   Move      Udate         Today
      *
     C                   Endsr
      *****************************************************************
