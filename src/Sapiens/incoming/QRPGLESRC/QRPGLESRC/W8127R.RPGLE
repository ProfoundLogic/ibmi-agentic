      *  (C) Copyright Progressive Data Solutions, Inc., 2001
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H DATFMT(*USA)
      *****************************************************************
      *  DESCRIPTION:  Clarendon preprocessor data extract            *
      *                Prompt Screen                                  *
      *                                                               *
      *  CALLS TO:                                                    *
      *                                                               *
      *  CREATED: 09/28/2001  JOHN STONE                              *
      *                                                               *
      *****************************************************************
      * Revised:
      *                                                                                    *
      * 08/13/18 C:53  Project: WC1917                                                     *
      *                Part code field change                                              *
      *
      * 10/11/07 C:190 SUPPORT WA099 54504                            *
      *                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
      *                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
      *                                                               *
      * 07/22/03  C:61  support wa099 32376
      *                 Change to use Wxpgl.
     *                                                               *
      * 05/22/03 C:15  Support: WC065/31676                           *
      *                Changed access to WDRPP.                       *
      *                                                               *
      * 08/01/02  C:142                                               *
      *                  Found wrong field on clear statement.        *
      *                                                               *
      * 07/15/02  C:142  Project: WC970                               *
      *                  Fixed Fcuepr to zero if negative.            *
      *                                                               *
      * 06/03/02  C:15   Project: WC970                               *
      *                  Corrected PMTRP amount sign.                 *
      *                                                               *
      * 04/25/02  C:142  PROJECT: WC970                               *
      *                  Zero expense constant when flat cancel.      *
      *                                                               *
      * 04/04/02  C:142  PROJECT: WC970                               *
      *                  Include numeric versions of premium fields   *
      *                  to facilitate testing and analysis. (2)      *
      *                                                               *
      * 04/03/02  C:142  PROJECT: WC970                               *
      *                  Check effective date against the thru date   *
      *                                                               *
      * 03/29/02  C:35   PROJECT: WC970                               *
      *                  Include numeric versions of premium fields   *
      *                  to facilitate testing and analysis. (1)      *
      *                                                               *
      * 03/28/02  C:35   PROJECT: WC970                               *
      *                  Only write expense constant record if value  *
      *                  is not 0.                                    *
      *                                                               *
      * 01/22/02 C:15  Support: WC095/30740                           *
      *                Corrected code that loads the payee name and   *
      *                address in PMTRP.                              *
      *****************************************************************
      *  INDICATOR USAGE:                                             *
      *                                                               *
      *  31 - Used on the PAGEDOWN keyword                            *
      *****************************************************************
      /eject
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
     * Employer Detail File
     FWdell8    IF   E           K Disk
     F                                     Rename(Pel01:Wdelr8)
     F                                     Prefix(L8:2)
     *
     * Employer Master File
     FWmeml     IF   E           K Disk
     *
     * CO#/ST/EFDT
     FWdecl     IF   E           K Disk
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
     * Claim file
     FWmcml5    IF   E           K Disk
     *
     * Deductible Detail File
     FWdddl     IF   E           K Disk
     *
     * Claims Checks Payment File
     FWdpal     IF   E           K Disk
     *
     * Claims Checks Payment File
     FWdpalb    IF   E           K Disk
     F                                     Rename(PPA01:PPA01B)
     F                                     Prefix(BB:2)
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
     * Statistical Reporting/Rating File
     FWdrrl     IF   E           K Disk
     *
     * Earned/Unearned File
     FWfeul     IF   E           K Disk
     *
     * Logical Policy Master For Clarendon Pre Processing
     FPolml1    IF   E           K Disk
     F                                     Rename(PLM01:POLMLR)
     *
     * Logical Premium Master For Clarendon Pre Processing
     FPpreml    IF   E           K Disk
     F                                     Rename(PPP01:PPREMLR)
     *
     * Logical Inforce Premium For Clarendon Pre Processing
     FInfcl     IF   E           K Disk
     F                                     Rename(IFC01:INFCLR)
     *
     * Limits Table File
     FWtlml     IF   E           K Disk
     F                                     Rename(PLM01:WTLMLR)
     F                                     Prefix(LL:2)
     *
     * Payee Demographics Master
     FWmpdl     IF   E           K Disk
     *
     * Deductible Discount Code File
     FWtddl3    IF   E           K Disk
     F                                     Rename(PDD01:WTDDLR)
     *
     * Claim History File
     FWmchl     IF   E           K Disk
     *
     * Part Table File
     FWtpal1    IF   E           K Disk
     F                                     Rename(PPA01:WTpalr)
     *
     * Check View File
     FWdrpl     IF   E           K Disk
     F                                     Rename(Prp01:Drp01)
    F*** Begin add *** 05/22/03 **************************************
     Fwdrpl3    if   e           k disk    Rename(prp01:prp03)
    F*** End add ***** 05/22/03 **************************************
     *
     * Policy Master For Clarendon Pre Processing
     FPolmp     O    E           K Disk
     *
     * Payment Master For Clarendon Pre Processing
     FPmtrp     O    E           K Disk
     *
     * Claim Master For Clarendon Pre Processing
     FClmmp     O    E           K Disk
     *
     * Claimant Master For Clarendon Pre Processing
     FClmnp     O    E           K Disk
     *
     * Inforce Premium For Clarendon Pre Processing
     FInfcp     O    E           K Disk
     *
     * Policy Premium For Clarendon Pre Processing
     FPPremp    O    E           K Disk
      *
     /eject
     *---------------------------------------------------------------------
     * Stand Alone Fields - TOP
     *---------------------------------------------------------------------
      *
      * Amount Fields used to Accum totals for Alpha output
     D Ppwrt#          S             20  2
     D Ppded#          S             20  2
     D Fcpre#          S             20  2
     D Fcuep#          S             20  2
     D Trcka#          S             20  2
     D Trinp#          S             20  2
     D Trmdp#          S             20  2
     D Trexp#          S             20  2
     D Mnexp#          S             20  2
     D Mnuap#          S             20  2
     D Mnsub#          S             20  2
     D Mnddr#          S             20  2
     D Mnded#          S             20  2
     D Mninp#          S             20  2
     D Mnmdp#          S             20  2
     D Mninr#          S             20  2
     D Mnmdr#          S             20  2
     D Mnexr#          S             20  2
     D Mnuar#          S             20  2
     D Zeros           S             20  2
     D Rragg#          S             28  2
      *
     D PrPpWrtn        S             20  2
     D PrFcPrem        S             20  2
     D PrFcUepr        S             20  2
     D ExPpWrtn        S             20  2
     D ExFcPrem        S             20  2
     D ExFcUepr        S             20  2
      *
     D Cod             S              6    dim(500)
     D X               S              3  0
     D Polkey          S             30
     D Grpkey          S             20
     D Efdkey          S             10
      *
     D PrmCo#          S              3  0
     D PrmCo           S              3
     D PrmMdy          S              6  0
     D PrmMdyA         S              6
     D PrmYmd          S              6  0
     D Mth             S              3  0
     D GT999           S              7  0 INZ(9999999)
     D Efdt999         S              7  0 INZ(9999999)
     D FromDt          S               D   DATFMT(*USA)
     D ThruDt          S               D   DATFMT(*USA)
     D Fmtefd          S               D   DATFMT(*USA)
     D Fmtcdt          S               D   DATFMT(*USA)
     D Mdy             S               D   DATFMT(*MDY)
     D ME_Date         S               D   DATFMT(*ISO)
     D Today           S               D   DATFMT(*USA)
     D Datymd          S               D   DATFMT(*YMD)
     D Datusa          S               D   DATFMT(*USA)
     D Trdt            S               D   DATFMT(*USA)
     D Timestmp        S             12  0
     D Commpd          S              1n
     D Active          S              1n
     D First           S              1n
     D FirstL2         S              1n
     D Wfeup           S              1n
     D Srco#           S              3
     D Srfnd           S              3
     D Srfyr           S              3
     D Sremp#          S              9
     D Srdiv           S              5
     D Srdate          S              8
     D SrdEfd          S              7  0
     D Sramt           S             11
     D Sramtunp        S             11  2
     D Sramtunx        S             11  2
     D Sramt2          S             11
     D Srterm          S              1
     D Tab10           S             10
     D Tabcod          S              4
     D H2              S              2  0
     D H4              S              4  0
     D H6              S              6  0
     D H2A             S              2
     D H4A             S              4
     D H11A            S             11
     D I               S              2  0
     D Occurs          S              3  0
     D Modfr           S              3  0 Inz(0)
     D  Sign           S             11  0
     D Editzip         C                    '0     -    '
      ****                                   '12345678901234567890.12'
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
     D  OcIncLim                     11  2
     D  OcDedOcr                     11  2
     D  OcExpcon                     20  2
      *
     D OcPol           DS                  Occurs(55)
     D  OcPefdt                       7  0
     D  OcPexdt                      10
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
     D Codkey          DS
     D  Codco#                        3
     D  Codtab                        2
     D  Codtyp                        1
      *
     D Claim#          DS
     D  Claim1                 1      3
     D                         4      4    Inz('-')
     D  Claim2                 5      7
     D                         8      8    Inz('-')
     D  Claim3                 9     15
      *
      *
     D Policy#         DS
     D  Pol1                   1      3
     D  Pol2                   4     12
     D  Pol3                  13     15
      *
      /eject
     D Psds          esds                  extname(szq1p)
     D Fids01        e ds                  extname(szq2p)
     C                   eval              *inlr = *off
     /eject
     **********************************************************************
     *                                                                    *
     * M A I N L I N E   R O U T I N E                                    *
     *                                                                    *
     **********************************************************************
      * Read Wxpgl to get all of the groups that have been selected in the
      * previous program. For each group input, process the files for that
      * group.
      *
     C     *Loval        Setll     Wxpgl
      *
     C                   Dou       %eof(Wxpgl)
     C                   Read      Wxpgl
      *
     C                   If        %eof
     C                   Leave
     C                   Endif
      *
     C                   Eval      First = *on
      *
     C                   If        Pgflag <> 'I'
     C                   Iter
     C                   Endif
      *
     C     Wmfnl_key     Chain     Wmfnl
     C                   If        %Found
     C                   Eval      Mth = Fnmxex
     C                   Endif
      *
     C                   If        Mth = *zero
     C                   Eval      Mth = 12
     C                   Endif
      *
     C     ThruDt        Subdur    Mth:*m        FromDt
      *
      * Get program id for first field in each file
     C                   Exsr      Get_Pgmid
      *
     C                   Exsr      Process_Empl
     C                   Exsr      Process_Clms
      *
     C                   Enddo
      *
     C                   Eval      *Inlr = *on
      /eject
     **********************************************************************
     *                                                                    *
     *  Process Employer detail for a group. Some selection is done on    *
     *  this record to determine whether to continue on read the next     *
     *  record.                                                           *
     *                                                                    *
     *  For each Employer Detail record processed, Wdell3-co,fnd,eff date *
     *                                                                    *
     *  - Get records from other files for additional information.        *
     *                                                                    *
      *  - For each empl-yr-effdt loop thru Wdell8 - summarize by risk     *
     *    state and write Polmp record for each risk state.               *
     *                                                                    *
     *  - When state reporting division loop thru Wdell1 by fyr, empl#    *
     *    Build array for premium information by risk state               *
     *                                                                    *
     *  - Process the information in these arrays and create records      *
     *    for files PPREMP and INFCP.                                     *
     *                                                                    *
     **********************************************************************
      *
     C     Process_Empl  Begsr
      *
      * Read Wdell3 by Company, Fund
      *
     C     Wdell3_Key    Setll     Wdell3
      *
     C                   Dou       %Eof(Wdell3)
     C                   Eval      Active = *off
     C     Wdell3_Key    Reade     Wdell3
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
      **** BEGIN ADD **** 04/03/02 *****************************************************************
     C     *Ymd          Move      Elpfdt        Datymd
     C                   If        Datymd > ThruDt
     C                   Iter
     C                   Endif
      **** End   ADD **** 04/03/02 *****************************************************************
      *
     C                   Clear                   Ppp01
     C                   Clear                   Plm01
     C                   Clear                   Ifc01
      *
     C                   If        Elptdt = *zero
     C                   Iter
     C                   Endif
      *
     C                   Eval      Kyrtcd = Elrtcd
     C                   Exsr      Get_Risk_St
      *
     C                   Eval      Kyemp# = Elemp#
     C                   Eval      Kydiv  = Eldiv
      *
     C                   Eval      Svemp# = Elemp#
     C                   Eval      Svdiv  = Eldiv
     C                   Eval      Svfyr  = Elfyr
     C                   Eval      Svpfdt = Elpfdt
      *
     C                   Eval      Fyear = Elfyr
     C                   Eval      KyEmp# = ElEmp#
      *
     C                   Exsr      Bld_Pol_Ary
     C                   Exsr      Proc_Pol_Ary
     C                   Exsr      New_Rnw_Year
     C                   Exsr      Get_Tr_Cd                                    PPremp
     C                   Exsr      Bld_Prem_Ary
     C                   Exsr      Proc_Prem_Ary
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     C*          ELFAF     IFEQ 'A'
      *
     C*                    Z-ADDELPRTD    TAM,A
      *
     C*                    ELSE
      *
     C*          ELEMNP    IFEQ 'Y'
     C*          ELPRTD    OREQ *ZERO
      *
     C*                    Z-ADDELPRTD    TAM,A
      *
     C*                    ELSE
      *
     C*
     C*                    Z-ADDELCO#     XXCO#
     C*                    Z-ADDELFND     XXFND
     C*                    Z-ADDELFYR     XXFYR
     C*
     C*                    Z-ADD0         XXEXCO
     C*                    MOVE ELPFDT    XXEFDT
      *
     C*          ELKY2     CHAINPEL02                60
      *
     C*          *IN60     DOWEQ*OFF
      *
     C*          RSKY      CHAINPRS01                60
      *
     C*                    MOVE RSJRST    XXST
      *
     C*          ECKY      SETGTPEC01
     C*          ECKY1     REDPEPEC01                    60
      *
     C*          *IN60     IFEQ *OFF
     C*          ECEXCO    ANDGTXXEXCO
     C*          ELPFDT    ANDNEELPTDT
     C*                    Z-ADDECEXCO    XXEXCO
     C*                    ENDIF                           *IN60 IFEQ
      *
     C*          ELKY2     READEPEL02                    60
      *
     C*                    ENDDO                           *IN60 DOWEQ
     C*
     C*                    Z-ADDXXEXCO    TAM,A
     C*
     C*          ELKEY     CHAINPEL02                60
     C*          RSKY      CHAINPRS01                60
      *
     C*                    MOVE RSJRST    XXST
     C*                    ENDIF                           ELEMNP=Y
      *
     C*                    ENDIF                           ELFAF=A
      *
      /eject
     **********************************************************************
     *  Read Employer Master File to determine if ACTIVE.
     **********************************************************************
      *
     C     Check_Active  Begsr
      *
     C     Wmeml_Key     Chain     Wmeml
     C                   Eval      Active = *Off
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
     C                   If        Cxdates(I) <= Elptdt and
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
     C                   Endsr
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
     C                   Clear                   FirstFyr
     C                   Clear                   FirstEfd
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
     C                   Endif
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Build Policy Array for each state.
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
     C                   Eval      Kyemp# = L8emp#
     C                   Eval      Kydiv  = L8div
     C                   Eval      Svfyr  = L8fyr
     C                   Eval      Svemp# = L8emp#
      *
     C                   Eval      Occurs = 1
     C                   Exsr      Check_Active
      *
     C                   Exsr      Eff_Date_St
      *
     C     *Mdy          Move      PrmMdy        Datusa
     C     *Usa0         Move      Datusa        Srdate
      *
     C                   Do        55
     C     Occurs        Occur     OcPol
      *
     C                   If        OcPrst  = *Blank
      *
     C                   Eval      OcPname = Emname
     C                   Eval      OcPaddr = Emmad1
     C                   Eval      OcPcity = Emmcty
     C                   Eval      OcPst   = Emmst
     C                   Eval      H11A    = %Editw(Emmzip:Editzip)
     C                   Move      H11A          OcPzip
     C                   Move      Emcnty        H4a
     C                   Movel     Emmst         H4a
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
      *
     C     *Ymd          Move      L8ptdt        Datusa
      *
     C                   If        L8ptdt = *Zero
     C                   Eval      OcPexdt = *Blanks
     C                   Else
     C                   Move      Datusa        OcPexdt
     C                   Endif
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
     *  Get State Reporting Division
     **********************************************************************
      *
     C     Get_SRD       Begsr
      *
     C                   Clear                   SrdEfd
     C     Wmeml_Key2    Setll     Wmeml
      *
     C                   Dou       %Eof(Wmeml)
     C     Wmeml_Key2    Reade     Wmeml
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        Emstrp = 'Y'
     C                   Eval      I = 1
     C     *Zero         Lookup    Effdates(I)                            58
      *
     C                   If        *in58 = *on
     C                   Eval      I = I - 1
     C                   Else
     C                   Eval      I = 5
     C                   Endif
      *
      * At this point, we have the most current Effective Date
      *
     C                   Eval      SrdEfd  = Effdates(I)
     C                   Eval      SrdPror = ElPror
     C                   Leave
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Build Premium Array for each state.
     **********************************************************************
      *
     C     Bld_Prem_Ary  Begsr
      *
     C                   Eval      Occurs = 1
      *
     C                   Do        55
     C     Occurs        Occur     OcPrems
     C                   Clear                   OcPrems
     C                   Eval      Occurs = Occurs + 1
     C                   Enddo
      *
      * Read Wdell for Co, Fund, Fyr, and Employer
      *
     C     Wdell1_Key    Setll     Wdell1
      *
     C                   Dou       %eof(Wdell1)
      *
     C     Wdell1_Key    Reade     Wdell1
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   Eval      Expconst = L1Prtd
      *
      **** BEGIN ADD **** 04/25/02 *****************************************************************
     C                   If        L1pfdt = L1ptdt
     C                   Clear                   Expconst
     C                   Endif
      **** END   ADD **** 04/25/02 *****************************************************************
      *
      * Get Risk State from Rate Set Code Master File
      *
     C                   Eval      Kyrtcd = L1rtcd
     C                   Exsr      Get_Risk_St
      *
     C                   Eval      Occurs = 1
      *
     C                   Do        55
     C     Occurs        Occur     OcPrems
      *
     C                   If        OcState = Risk_St
     C                   Eval      OcIncLim = OcIncLim + L1ain1
     C                   Eval      OcDedOcr = OcDedOcr + L1prtd
      *
     C                   If        L1pfdt < OcEfdt
     C                   Eval      OcEfdt  = L1pfdt
     C                   Endif
      *
     C                   Eval      OcExpcon = Expconst
      *
`    C                   Leave
     C                   Endif
      *
     C                   If        OcState = *Blank
     C                   Eval      OcEfdt  = L1pfdt
     C                   Eval      OcState  = Risk_St
     C                   Eval      OcIncLim = L1ain1
     C                   Eval      OcDedOcr = L1prtd
      *
     C                   Eval      OcExpcon = Expconst
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
     *  Process Premium Array for each state.
     **********************************************************************
      *
     C     Proc_Prem_Ary Begsr
      *
     C                   Eval      Occurs = 1
      *
     C                   Do        55
     C     Occurs        Occur     OcPrems
      *
      **** BEGIN ADD **** 04/03/02 *****************************************************************
     C                   Eval      Wfeup = *Off
      **** END   ADD **** 04/03/02 *****************************************************************
     C                   Exsr      Get_Wfeul
      *
      **** BEGIN ADD **** 04/03/02 *****************************************************************
     C                   If        Not Wfeup
     C                   Eval      OcExpcon = *Zero
     C                   Endif
      **** END   ADD **** 04/03/02 *****************************************************************
      *
     C                   If        OcState <> *blank and
      **** BEGIN ADD **** 04/03/02 *****************************************************************
     C                             Wfeup
      **** END   ADD **** 04/03/02 *****************************************************************
     C                   Exsr      Write_Premp
      *
     C                   If        Active or
     C                             PrFcPrem <> *Zero or
     C                             PrFcUepr <> *Zero or
     C                             ExFcPrem <> *Zero or
     C                             ExFcUepr <> *Zero
     C                   Exsr      Write_Infcp
     C                   Endif
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
     C                   Move      Pggrp         Pol1
     C                   Move      Svemp#        Pol2
     C                   Move      Svfyr         Pol3
     C                   Movel(p)  Policy#       Svpol#
     C                   Endif
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Get Payment Transaction Code for Pmtrp
     **********************************************************************
      *
     C     Get_Trpycd    Begsr
      *
     C                   If        Patype = 'R' and Paamt > 0
      *
     C                   movel(p)  pabr2         Tab10
     C     Wtabl_Key     Chain     Pb201
     C                   If        not %Found
     C                   Eval      B2plcd = *blank
     C                   Endif
      *
     C                   If        B2plcd = 'O' or
     C                             B2plcd = 'P'
     C                   Eval      Trpycd = '12'
     C                   Endif
      *
     C                   If        B2plcd = 'Q'
     C                   Eval      Trpycd = '28'
     C                   Endif
      *
     C                   If        Paretp = 'C' or Paretp = 'M'
     C                   If        B2plcd <> 'O' and
     C                             B2plcd <> 'P' and
     C                             B2plcd <> 'Q'
     C                   Eval      Trpycd = '29'
     C                   Endif
     C                   Endif
      *
     C                   If        Paretp = 'E' or Paretp = 'L'
     C                   If        B2plcd <> 'O' and
     C                             B2plcd <> 'P' and
     C                             B2plcd <> 'Q'
     C                   Eval      Trpycd = '13'
     C                   Endif
     C                   Endif
      *
     C                   Endif
      *
      *********************************************************************
      *
     C                   If        Patype = 'P' and paamt > 0 or
     C                             Patype = 'M' and paamt > 0 or
     C                             Patype = 'A' and paamt > 0 or
     C                             Patype = 'Z' and paamt > 0
      *
     C                   If        Paretp = 'C' or
     C                             Paretp = 'M'
     C                   If        Pafnal = 'Y'
     C                   Eval      Trpycd = '16'
     C                   Else
     C                   Eval      Trpycd = '17'
     C                   Endif
     C                   Endif
      *
     C                   If        Paretp = 'E' or
     C                             Paretp = 'L'
     C                   Eval      Trpycd = '27'
     C                   Endif
      *
     C                   Endif
      *
      *********************************************************************
      *
     C                   If        Patype = 'V' and paamt > 0 or
     C                             Patype = 'S' and paamt > 0 or
     C                             Patype = 'A' and paamt < 0 or
     C                             Patype = 'M' and paamt < 0
      *
     C                   If        Paretp = 'C' or
     C                             Paretp = 'M'
     C                   Eval      Trpycd = '29'
     C                   Endif
     C                   If        Paretp = 'E' or
     C                             Paretp = 'L'
     C                   Eval      Trpycd = '30'
     C                   Endif
      *
     C                   Endif
      *
      *********************************************************************
      *
     C                   If        Patype = 'R'
     C                   If        Paretp = 'C' or
     C                             Paretp = 'M'
      *
     C                   If        Paamt < *zero
     C                   Eval      Trpycd = '33'
     C                   Endif
     C                   Endif
     C                   Endif
      *
      *********************************************************************
      *
     C                   If        Patype = 'R'
     C                   If        Paretp = 'E' or
     C                             Paretp = 'L'
      *
     C                   If        Paamt < *zero
     C                   Eval      Trpycd = '34'
     C                   Endif
     C                   Endif
     C                   Endif
      *
      *********************************************************************
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
     *  Read Wfeul to get written prem, in force prem, and unearned prem
     **********************************************************************
      *
     C     Get_Wfeul     Begsr
      *
     C                   Clear                   PrPpWrtn
     C                   Clear                   PrFcPrem
     C                   Clear                   PrFcUepr
     C                   Clear                   ExFcPrem
     C                   Clear                   ExFcUepr
      *
     C     Wfeul_Key     Setll     Wfeul
      *
     C                   Dou       %Eof
      *
     C     Wfeul_Key     Reade     Wfeul
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
      **** BEGIN ADD **** 04/03/02 *****************************************************************
     C                   Eval      Wfeup = *on
      **** END   ADD **** 04/03/02 *****************************************************************
      *
     C                   Eval         PrPpWrtn = PrPpWrtn +
     C                                EuYdwr + EuPywr
     C                   Eval         PrFcPrem = PrFcPrem + EuInpr
     C                   Eval         PrFcUepr = PrFcUepr + EuUnea
      *
     C****************** Eval         ExPpWrtn = ExPpWrtn + EuMwex
     C                   Eval         ExFcPrem = ExFcPrem + EuInex
     C                   Eval         ExFcUepr = ExFcUepr + EuUnex
      *
     C                   Enddo
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
      *
     C     Write_Polmp   Begsr
      *
     C                   Movel(p)  Gcpgid        Lmpgid
     C                   Move(p)   Elfnd         Lmgrp
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
     C                   Eval      Lmexdt = OcPexdt
     C                   Eval      Lmterr = OcPterr
     C                   Eval      Lmsysd = OcPsysd
     C                   Eval      Lmacdt = OcPacdt
     C                   Eval      Lmrst  = OcPrst
      *
     C                   If        OcPactive = 'Y'
     C                   Eval      Lmcndt = *Blanks
     C                   Else
     C                   Eval      Lmcndt = OcPcndt
     C                   Endif
      *
     C     Polml1_Key    Chain     Polml1
      *
     C                   If        Not %Found
     C                   Eval      Lmfyr  = Elfyr
     C                   Eval      Lmfnd  = Elfnd
     C                   Eval      Lmemp# = Elemp#
     C                   Write     Plm01
     C                   Endif
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Write Policy Premium For Clarendon Pre Processing
     **********************************************************************
      *
     C     Write_Premp   Begsr
      *
     C                   Clear                   Ppwrt#
     C                   Clear                   Ppded#
     C                   Clear                   Rragg#
      *
      * Get deductibles
      *
     C     Wtddl3_Key    Setgt     Wtddl3
     C     Wtddl3_Key2   Readpe    Wtddl3
      *
     C                   If        not %Eof
     C*** BEG ADD *****  10/11/07  ************************************
     C                              and ddstat = 'A'
     C*** END ADD *****  10/11/07  ************************************
     C                   Eval      Ppded# = Ddampo
     C                   Endif
      *
     C                   Movel(P)  Elilcd        Tab10
     C     Wtlml_key     Chain     Wtlml
      *
     C                   If        %Found
     C                   Eval      Rragg# = Llagg * 1000
     C                   Else
      *
      * Get Limits
      *
     C     Wdrrl_key     Setgt     Wdrrl
     C     Wdrrl_key2    Readpe    Wdrrl
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
      **** BEGIN ADD **** 03/29/02 *****************************************************************
     C                   z-add     PrPpwrtn      Ppwpr#
      **** END   ADD **** 03/29/02 *****************************************************************
      *
     C                   Eval      Svtrdt = Elpfdt
      *
     C                   Eval      Ppdeds = %editc(Ppded#:'P')
     C                   Eval      Pprst  = OcState
      *
     C     *Ymd          Move      OcEfdt        Datusa
      *
     C                   If        OcEfdt = *Zero
     C                   Eval      Ppefdt = *Blanks
     C                   Else
     C                   Move      Datusa        Ppefdt
     C                   Endif
      *
     C                   If        Svtrcd = '10' or
     C                             Svtrcd = '11'
     C     *Ymd          Move      OcEfdt        Datusa
      *
     C                   If        OcEfdt = *Zero
     C                   Eval      Pptrdt = *Blanks
     C                   Else
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
     C                   Endif
      *
     C     *Usa          Move      Today         Ppsy
     C     *Usa          Move      ThruDt        Ppac
     C                   Eval      Ppfyr  = Elfyr
     C                   Eval      Ppfnd  = Elfnd
     C                   Eval      PpEmp# = Elemp#
      *
     C     Ppreml_Key    Chain     Ppreml
     C                   If        not %Found
     C                   Eval      Ppfyr  = Elfyr
     C                   Eval      Ppfnd  = Elfnd
     C                   Eval      Ppemp# = Elemp#
     C                   Write     Ppp01
     C                   Endif
      *
      * Write Expense Constant Record
      *
     C*                  If        Commpd
      **** BEGIN ADD **** 03/28/02 *****************************************************************
     C****************** If        ExPpWrtn <> 0
     C                   If        OcExpcon <> 0
      **** END   ADD **** 03/28/02 *****************************************************************
      *
     C                   Movel(p)  Gcpgid        Pppgid
     C                   Movel(p)  Svpol#        Pppol
     C                   Eval      Pptype = '12'
     C                   Movel     '16'          Ppasln
     C                   Eval      Pplob  = 'WC'
     C                   Eval      Pptrcd = Svtrcd
     C                   Eval      Ppwrt# = Elprtd
      **** BEGIN CHG **** 04/04/02 *****************************************************************
     C****************** Eval      Ppwrtn = %editc(ExPpWrtn:'P')
     C                   Eval      Ppwrtn = %editc(OcExpcon:'P')
      **** END   CHG **** 04/04/02 *****************************************************************
      **** BEGIN ADD **** 03/29/02 *****************************************************************
     C****************** z-add     ExPpWrtn      Ppwpr#
     C                   Z-add     OcExpcon      Ppwpr#
      **** END   ADD **** 03/29/02 *****************************************************************
     C                   Eval      Pplmts = *Zero
     C                   Eval      Pplmts = %editc(Rragg#:'P')
     C                   Eval      Ppdeds = *Zero
     C                   Eval      Ppdeds = %editc(Ppded#:'P')
      *
     C     *Ymd          Move      OcEfdt        Datusa
      *
     C                   If        OcEfdt = *Zero
     C                   Eval      Ppefdt = *Blanks
     C                   Else
     C                   Move      Datusa        Ppefdt
     C                   Endif
      *
     C                   If        Svtrcd = '10' or
     C                             Svtrcd = '11'
     C     *Ymd          Move      OcEfdt        Datusa
     C                   If        OcEfdt = *Zero
     C                   Eval      Pptrdt = *Blanks
     C                   Else
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
     C                   Eval      Ppfyr  = Elfyr
     C                   Eval      Ppfnd  = Elfnd
     C                   Eval      PpEmp# = Elemp#
      *
     C     Ppreml_Key    Chain     Ppreml
     C                   If        not %Found
     C                   Eval      Ppfyr  = Elfyr
     C                   Eval      Ppfnd  = Elfnd
     C                   Eval      Ppemp# = Elemp#
     C                   Write     Ppp01
     C                   Endif
      *
     C*                  Endif
      **** BEGIN ADD **** 03/28/02 *****************************************************************
     C                   Endif                                                  If ExPpWrtn <> 0
      **** END   ADD **** 03/28/02 *****************************************************************
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Write Inforce Premium For Clarendon Pre Processing
     **********************************************************************
      *
     C     Write_Infcp   Begsr
      *
     C                   Clear                   Fcpre#
     C                   Clear                   Fcuep#
      *
     C                   Movel(p)  Gcpgid        Fcpgid
     C                   Movel(p)  Svpol#        Fcpol
     C                   Eval      Fctype = '10'
     C                   Movel     '16'          Fcasln
     C                   Eval      Fclob  = 'WC'
     C     *Usa          Move      ThruDt        FCEOMD
      *
     C                   Eval      Fcprem = %editc(PrFcprem:'P')
      *
      **** BEGIN ADD **** 07/15/02 *****************************************************************
     C                   If        PrFcuepr < *Zero
     C                   Clear                   PrFcuepr
     C                   Endif
      **** END   ADD **** 07/15/02 *****************************************************************
      *
     C                   Eval      Fcuepr = %editc(PrFcuepr:'P')
      **** BEGIN ADD **** 03/29/02 *****************************************************************
     C                   z-add     PrFcprem      Fcprm#
     C                   z-add     PrFcuepr      Fcue#
      **** END   ADD **** 03/29/02 *****************************************************************
      *
     C     *Ymd          Move      OcEfdt        Datusa
      *
     C                   If        OcEfdt = *Zero
     C                   Eval      Fcefdt = *Blanks
     C                   Else
     C                   Move      Datusa        Fcefdt
     C                   Endif
      *
     C                   Eval      Fcrst  = OcState
     C                   Eval      Fcfyr  = Elfyr
     C                   Eval      Fcfnd  = Elfnd
     C                   Eval      Fcemp# = Elemp#
      *
     C     Infcl_Key     Chain     Infcl
     C                   If        not %Found
     C                   Write     Ifc01
     C                   Endif
      *
     C*                  If        Commpd
      **** BEGIN ADD **** 03/28/02 *****************************************************************
     C                   If        (ExFcprem <> 0) or (ExFcuepr <> 0)
      **** END   ADD **** 03/28/02 *****************************************************************
      *
     C                   Movel(p)  Gcpgid        Fcpgid
     C                   Movel(p)  Svpol#        Fcpol
     C                   Eval      Fctype = '12'
     C                   Movel     '16'          Fcasln
     C                   Eval      Fclob  = 'WC'
     C     *Usa          Move      Thrudt        FCEOMD
     C                   Eval      Fcprem = %editc(ExFcprem:'P')
      *
      **** BEGIN ADD **** 07/15/02 *****************************************************************
     C                   If        ExFcuepr < *Zero
     C                   Clear                   ExFcuepr
     C                   Endif
      **** END   ADD **** 07/15/02 *****************************************************************
      *
     C                   Eval      Fcuepr = %editc(ExFcuepr:'P')
      **** BEGIN ADD **** 03/29/02 *****************************************************************
     C                   z-add     ExFcprem      Fcprm#
     C                   z-add     ExFcuepr      Fcue#
      **** END   ADD **** 03/29/02 *****************************************************************
      *
     C     *Ymd          Move      OcEfdt        Datusa
      *
     C                   If        OcEfdt = *Zero
     C                   Eval      Fcefdt = *Blanks
     C                   Else
     C                   Move      Datusa        Fcefdt
     C                   Endif
      *
     C                   Eval      Fcrst  = OcState
     C                   Eval      Fcfyr  = Elfyr
     C                   Eval      Fcfnd  = Elfnd
     C                   Eval      Fcemp# = Elemp#
      *
     C     Infcl_Key     Chain     Infcl
     C                   If        not %Found
     C                   Write     Ifc01
     C                   Endif
      *
     C*                  Endif
      **** BEGIN ADD **** 03/28/02 *****************************************************************
     C                   Endif                                                  If (ExFcprem <> 0) o
      **** END   ADD **** 03/28/02 *****************************************************************
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Process Claims
     **********************************************************************
      *
     C     Process_Clms  Begsr
      *
     C     Wmcml5_Key    Setll     Wmcml5
     C                   Clear                   Svemp#
     C                   Clear                   Svfyr
     C                   Clear                   Svdiv
     C                   Eval      First  = *On
      *
     C                   Dou       %eof(Wmcml5)
     C                   Eval      Active = *off
     C     Wmcml5_Key    Reade     Wmcml5
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   Clear                   Cmm01
     C                   Clear                   Cmn01
     C                   Clear                   Ptr01
      *
     C                   If        Svemp# <> Cmemp# or
     C                             Svfyr  <> Cmfyr
      *
     C                   Eval      Svemp# = Cmemp#
     C                   Eval      Svfyr  = Cmfyr
      *
     C                   Endif
      *
     C                   Eval      Svdiv = Cmdiv
     C                   Exsr      Get_Pol#
     C                   Eval      Kyemp# = Cmemp#
     C                   Eval      Kydiv = Cmdiv
      *
  229C     Wdell1_Key2   Chain     Wdell1
     C                   Eval      Kyrtcd = L1rtcd
     C                   Exsr      Get_Risk_St
      *
     C     Cmss#         Chain     Wmcdl
      *
     C                   Exsr      Write_Clmmp
     C                   Exsr      Write_Clmnp
     C                   Exsr      Write_Pmtrp
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Write Payment Master For Clarendon Pre Processing
     **********************************************************************
      *
     C     Write_Pmtrp   Begsr
      *
     C     Wdpal_Key     Setll     Wdpal
      *
     C                   Dou       %Eof(Wdpal)
     C     Wdpal_Key     Reade     Wdpal
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C     *Ymd          Move      PaTrdt        Trdt
     C                   If        Trdt > Thrudt
     C                   Iter
     C                   Endif
      *
     C                   Clear                   Trcka#
     C                   Clear                   Trinp#
     C                   Clear                   Trmdp#
     C                   Clear                   Trexp#
     C                   Clear                   Ptr01
      *
     C                   Movel(p)  Gcpgid        Trpgid
     C                   Move      Pggrp         Claim1
     C                   Move      Svfyr         Claim2
     C                   Move      Cmcase        Claim3
     C                   Movel(p)  Claim#        Trclm#
     C                   Move(p)   '1'           Trclt#
      *
     C                   Exsr      Get_Trpycd
      *
     C     Wmpdl_Key     Chain     Wmpdl
      *
     C                   If        %Found
     C                   If        Pd1099 = 'Y'
     C                   Eval      Tr9flg = 'Y'
     C                   Else
     C                   Eval      Tr9flg = 'N'
     C                   Endif
     C                   Else
     C                   Eval      Tr9flg = 'N'
     C                   Endif
      *
     C                   Movel(p)  Pafein        Trfein
     C                   Move      Pachk#        Trck#
      *
     C                   If        Patrdt <> *zero
     C                   Move      Patrdt        H6
     C     *Ymd          Move      H6            Datusa
      *
     C                   If        H6 = *Zero
     C                   Eval      Trckdt = *Blanks
     C                   Else
     C                   Move      Datusa        Trckdt
     C                   Endif
      *
     C                   Endif
      *
     C*** Begin dlt *** 06/03/02 *******************************************************************
     C****************** If        Trpycd = '12' or
     C******************           Trpycd = '13' or
     C******************           Trpycd = '28' or
     C******************           Trpycd = '29' or
     C******************           Trpycd = '30' or
     C******************           Trpycd = '33' or
     C******************           Trpycd = '34'
     C*** End dlt ***** 06/03/02 *******************************************************************
     C*** Begin add *** 06/03/02 *******************************************************************
     C                   If        PATYPE = 'R' or
     C                             PATYPE = 'S' or
     C                             PATYPE = 'V'
     C*** End add ***** 06/03/02 *******************************************************************
     C                   Eval      Paamt = Paamt * -1
     C                   Endif
      *
     C                   Eval      Trcka# = Paamt
     C                   Eval      Trckam = %editc(Trcka#:'P')
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     C                   Eval      Trckam# = Trcka#
      **** END   ADD **** 04/04/02 *****************************************************************
      *
     C                   If        Paretp = 'C'
     C                   Eval      Trinp# = Paamt
     C                   Endif
     C                   Eval      Trinpd = %editc(Trinp#:'P')
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     C                   Eval      Trinpd# = Trinp#
      **** END   ADD **** 04/04/02 *****************************************************************
      *
     C                   If        Paretp = 'M'
     C                   Eval      Trmdp# = Paamt
     C                   Endif
     C                   Eval      Trmdpd = %editc(Trmdp#:'P')
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     C                   Eval      Trmdpd# = Trmdp#
      **** END   ADD **** 04/04/02 *****************************************************************
      *
     C                   If        Paretp = 'E' or
     C                             Paretp = 'L'
     C                   Eval      Trexp# = Paamt
     C                   Endif
     C                   Eval      Trexpd = %editc(Trexp#:'P')
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     C                   Eval      Trexpd# = Trexp#
      **** END   ADD **** 04/04/02 *****************************************************************
      *
     C     *Usa          Move      Today         Trsysd
     C     *Usa          Move      ThruDt        Tracdt
     C                   Eval      Trcom  = *blank
      *********************************************************************
     C                   If        Patype = 'P' or
     C                             Patype = 'M' or
     C                             Patype = 'R'
      *
    C*** Begin add *** 05/22/03 **************************************
     C     Wdrpl3_key    chain     prp03
     C                   If        not %Found
    C*** End add ***** 05/22/03 **************************************
     C     Wdrpl_Key     Chain     Wdrpl
    C*** Begin add *** 05/22/03 **************************************
     C                   endif
    C*** End add ***** 05/22/03 **************************************
      *
     C                   If        %Found
     C                   Movel(p)  Rppnam        Trnam1
     C                   Eval      Trnam2 = *blank
     C                   Eval      Tradr1 = Rpadr1
     C                   Eval      Tradr2 = Rpadr2
     C                   Eval      Trcity = Rpcty
     C                   Eval      Trst   = RPST
     C                   Eval      H11A   = %Editw(Rpzip:Editzip)
     C                   Move      H11A          Trzip
      *
     C                   Endif
      *
     C                   If        trnam1 = *blanks
      *
     C     Wmpdl_Key     Chain     Wmpdl
      *
     C                   If        %Found
     C                   If        Pdfsn = *Blanks
     C                   Movel(p)  Pdname        Trnam1
     *
     C                   Else
     C     pdfsn         Cat       pdname:1      Trnam1
     C                   Endif
      *
     *** The following will override the name loaded above if Dba1 = Y
     *
     C                   If        Pddba1 = 'Y'
     C                   Movel(p)  Pdadr1        Trnam1
     C                   Movel(p)  Pdadr2        Tradr1
     C                   Else
     C                   Movel(p)  Pdadr1        Tradr1
     C                   Movel(p)  Pdadr2        Tradr2
     C                   Endif
      *
     C                   Eval      Trcity = Pdcty
     C                   Eval      Trst   = Pdst
     C                   Eval      H11A   = %Editw(Pdzip:Editzip)
     C                   Move      H11A          Trzip
     C                   Else
      *
     C     Cmss#         Chain     Wmcdl
      *
     C                   If        %Found
     C                   Clear                   Trnam1
     C     Cdlsn         Cat       ', ':0        Trnam1
     C     Trnam1        Cat       Cdfsn:1       Trnam1
     C                   Eval      Trnam2 = *blank
     C                   Eval      Tradr1 = Cdadr1
     C                   Eval      Tradr2 = Cdadr2
     C                   Eval      Trcity = Cdcty
     C                   Eval      Trst   = Cdst
     C                   Eval      H11A   = %Editw(Cdzip:Editzip)
     C                   Move      H11A          Trzip
     C                   Endif
      *
     C                   Endif
     C                   Endif
     C                   Endif
      *
      ********************************************************************
      *
     C                   If        Patype = 'V' or
     C                             Patype = 'S' or
     C                             Patype = 'A' or
     C                             Patype = 'Z'
      *
     C     WdpalB_Key    Chain     Wdpalb
      *
     C                   If        %Found
      *
    C*** Begin add *** 05/22/03 **************************************
     C     Wdrpl3_key    chain     prp03
     C                   If        not %Found
    C*** End add ***** 05/22/03 **************************************
     C     Wdrpl_KeyB    Chain     Wdrpl
    C*** Begin add *** 05/22/03 **************************************
     C                   endif
    C*** End add ***** 05/22/03 **************************************
      *
     C                   If        %Found
     C                   Movel(p)  Rppnam        Trnam1
     C                   Eval      Trnam2 = *blank
     C                   Eval      Tradr1 = Rpadr1
     C                   Eval      Tradr2 = Rpadr2
     C                   Eval      Trcity = Rpcty
     C                   Eval      Trst   = RPST
     C                   Eval      H11A   = %Editw(Rpzip:Editzip)
     C                   Move      H11A          Trzip
     C                   Endif
      *
     C                   Endif
      *
     C                   If        trnam1 = *blanks
      *
     C     Wmpdl_Key     Chain     Wmpdl
      *
     C                   If        %Found(Wmpdl)
     C                   If        Pdfsn = *Blanks
     C                   Movel(p)  Pdname        Trnam1
     *
     C                   Else
     C     pdfsn         Cat       pdname:1      Trnam1
     C                   Endif
      *
     *** The following will override the name loaded above if Dba1 = Y
     *
     C                   If        Pddba1 = 'Y'
     C                   Movel(p)  Pdadr1        Trnam1
     C                   Movel(p)  Pdadr2        Tradr1
     C                   Else
     C                   Movel(p)  Pdadr1        Tradr1
     C                   Movel(p)  Pdadr2        Tradr2
     C                   Endif
      *
     C                   Eval      Trcity = Pdcty
     C                   Eval      Trst   = Pdst
     C                   Eval      H11A   = %Editw(Pdzip:Editzip)
     C                   Move      H11A          Trzip
     C                   Else
      *
     C     Cmss#         Chain     Wmcdl
      *
     C                   If        %Found
     C                   Clear                   Trnam1
     C     Cdlsn         Cat       ', ':0        Trnam1
     C     Trnam1        Cat       Cdfsn:1       Trnam1
     C                   Eval      Trnam2 = *blank
     C                   Eval      Tradr1 = Cdadr1
     C                   Eval      Tradr2 = Cdadr2
     C                   Eval      Trcity = Cdcty
     C                   Eval      Trst   = Cdst
     C                   Eval      H11A   = %Editw(Cdzip:Editzip)
     C                   Move      H11A          Trzip
     C                   Endif
     C                   Endif
     C                   Endif
     C                   Endif
      *                                       m
     C                   Eval      Trfyr  = Cmfyr
     C                   Eval      Trfnd  = Cmfnd
     C                   Eval      Tremp# = Cmemp#
     C                   Eval      TrCase = CmCase
     C                   Write     Ptr01
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Write Claimant Master For Clarendon Pre Processing
     **********************************************************************
      *
     C     Write_Clmmp   Begsr
      *
     C                   Movel(p)  Gcpgid        Mmpgid
     C                   Movel(p)  Svpol#        Mmpol
     C                   Move      Pggrp         Claim1
     C                   Move      Svfyr         Claim2
     C                   Move      Cmcase        Claim3
     C                   Movel(p)  Claim#        Mmclm#
      *
     C                   Eval      Fyear  = Cmfyr
     C                   Eval      KyEmp# = CmEmp#
      *
     C                   Exsr      Eff_Date_St
      *
     C                   If        FirstEfd <> *zero
     C     *Ymd          Move      FirstEfd      Datusa
     C                   Move      Datusa        Mmefdt
     C                   Endif
      *
     C                   If        Cmacdt <> *zero
     C     *Ymd          Move      Cmacdt        Datusa
     C                   Move      Datusa        Mmaccd
     C                   Endif
      *
     C                   If        Cmdtrp <> *zero
     C     *Ymd          Move      Cmdtrp        Datusa
     C                   Move      Datusa        Mmrpdt
     C                   Endif
      *
     C                   If        Cmstat = 'C'
     C     Wmchl_SGT     Setgt     Wmchl
     C     Wmchl_Key     Readpe    Wmchl
      *
     C                   If        Not %Eof
     C     *Ymd          Move      Chefdt        Datusa
      *
     C                   If        Chefdt = *Zero
     C                   Eval      Mmcldt = *Blanks
     C                   Else
     C                   Move      Datusa        Mmcldt
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   If        Cmstat = 'O'
     C     Wmchl_SGT     Setgt     Wmchl
     C     Wmchl_Key     Readpe    Wmchl
      *
     C                   If        Not %Eof
     C     *Ymd          Move      Chefdt        Datusa
      *
     C                   If        Chefdt = *Zero
     C                   Eval      Mmrodt = *Blanks
     C                   Else
     C                   Move      Datusa        Mmrodt
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   If        Cmidt <> *zero
     C     *Ymd          Move      Cmidt         Datusa
      *
     C                   If        Cmidt  = *Zero
     C                   Eval      Mmsysd = *Blanks
     C                   Else
     C                   Move      Datusa        Mmsysd
     C                   Endif
      *
     C                   Endif
      *
     C     *Ymd          Move      Prmymd        Datusa
      *
     C                   If        Prmymd = *Zero
     C                   Eval      MMacdt = *Blanks
     C                   Else
     C                   Move      Datusa        Mmacdt
     C                   Endif
      *
     C                   Eval      Mmcom  = *blank
     C                   Eval      Mmrst  = Risk_St
     C                   Eval      Mmcat# = *blank
      *
     C                   If        Cmodis = 'Y'
     C                   Eval      Mmtypl = 'OD'
     C                   Else
     C     Cmjrst        Cat(p)    'LC':0        Tabcod
     C                   Clear                   Tab10
      *
     C     Wtgtl_Key     Chain     Wtgtl
      *
     C                   If        not %Found
     C                   Movel(p)  '  LC'        Tabcod
     C     Wtgtl_Key     Chain     Wtgtl
     C                   Endif
      *
     C                   Movel     Gtkey         H4a
     C                   Move      H4a           H2a
      *
     C                   Select
     C                   When      H2a = '02'
     C                   Move      'OD'          Mmtypl
     C                   When      H2a = '03'
     C                   Move      'CT'          Mmtypl
     C                   Other
     C                   Move      'WC'          Mmtypl
     C                   Endsl
      *
     C                   Endif
      *
     C                   Movel(p)  Cmpart        Tab10
     C     Wtpal1_Key    Chain     Wtpal1
     C                   If        %Found
     C*** Beg Chg *** 05/01/18 ************************************
     C****************** Move      Paedip        Mmtypb
     C                   MoveL(P)  Paedip        Mmtypb
     C*** End Chg *** 05/01/18 ************************************
     C                   Endif
      *
     C                   Movel(p)  Cmnatr        Tab10
     C     Wtabl_Key     Chain     Pna01
      *
     C                   If        %Found
     C                   Move      Naedin        Mmnoi
     C                   Else
     C                   Clear                   Mmnoi
     C                   Endif
      *
     C                   Movel(p)  Cmcaus        Tab10
     C     Wtabl_Key     Chain     Pca01
      *
     C                   If        %Found
     C                   Move      Caedic        Mmcoi
     C                   Else
      **** BEGIN CHG **** 08/01/02 *****************************************************************
     C*                  Clear                   Mmnoi
     C                   Clear                   Mmcoi
      **** BEGIN CHG **** 08/01/02 *****************************************************************
     C                   Endif
      *
     C                   Eval      Mmfyr  = CmFyr
     C                   Eval      Mmfnd  = Cmfnd
     C                   Eval      Mmemp# = Cmemp#
     C                   Eval      MmCase = CmCase
     C                   Write     Cmm01
      *
     C                   Endsr
      /eject
     **********************************************************************
     *  Write Claim Master For Clarendon Pre Processing
     **********************************************************************
      *
     C     Write_Clmnp   Begsr
      *
     C                   Clear                   Mnexp#
     C                   Clear                   Mnuap#
     C                   Clear                   Mnsub#
     C                   Clear                   Mnddr#
     C                   Clear                   Mnded#
     C                   Clear                   Mninp#
     C                   Clear                   Mnmdp#
     C                   Clear                   Mninr#
     C                   Clear                   Mnmdr#
     C                   Clear                   Mnexr#
     C                   Clear                   Mnuar#
      *
     C                   Movel(p)  Gcpgid        Mnpgid
     C                   Move      Pggrp         Claim1
     C                   Move      Svfyr         Claim2
     C                   Move      Cmcase        Claim3
     C                   Movel(p)  Claim#        Mnclm#
     C                   Move(p)   '1'           Mnclt#
     C                   Movel     '16'          Mnasln
     C                   Eval      MNLOB  = 'WC'
      *
     C                   Exsr      Get_Deduct
     C                   Exsr      Accum_Paymts
      *
     C                   Exsr      Indem_Rsrvs
     C                   Exsr      Medcl_Rsrvs
     C                   Exsr      Legal_Rsrvs
     C                   Exsr      Exp_Rsrvs
      *
     C     *Ymd          Move      Prmymd        Datusa
      *
     C                   If        Prmymd = *Zero
     C                   Eval      Mneomd = *Blanks
     C                   Else
     C                   Move      Datusa        Mneomd
     C                   Endif
      *
     C                   Eval      Mncom  = *blank
     C                   Clear                   Mncltn
     C     Cdfsn         Cat       Cdmid:1       Mncltn
     C     Mncltn        Cat       Cdlsn:1       Mncltn
      *
     C                   Eval      Mnslvg = %editc(Zeros:'P')
     C                   Eval      Mnsant = %editc(Zeros:'P')
     C                   Eval      Mnsban = %editc(Zeros:'P')
     C                   Eval      Mnexpd = %editc(Mnexp#:'P')
     C                   Eval      Mnuapd = %editc(Mnuap#:'P')
     C                   Eval      Mnsubr = %editc(Mnsub# :'P')
     C                   Eval      Mndedr = %editc(Mnddr# :'P')
     C                   Eval      Mndeds = %editc(Mnded#:'P')
     C                   Eval      Mninpd = %editc(Mninp#:'P')
     C                   Eval      Mnmdpd = %editc(Mnmdp#:'P')
     C                   Eval      Mninrv = %editc(Mninr#:'P')
     C                   Eval      Mnmdrv = %editc(Mnmdr#:'P')
     C                   Eval      Mnexrv = %editc(Mnexr#:'P')
     C                   Eval      Mnuarv = %editc(Mnuar#:'P')
      *
      *
      **** BEGIN ADD **** 04/04/02 *****************************************************************
     C                   Eval      Mnslvg# = Zeros
     C                   Eval      Mnsant# = Zeros
     C                   Eval      Mnsban# = Zeros
     C                   Eval      Mnexpd# = Mnexp#
     C                   Eval      Mnuapd# = Mnuap#
     C                   Eval      Mnsubr# = Mnsub#
     C                   Eval      Mndedr# = Mnddr#
     C                   Eval      Mndeds# = Mnded#
     C                   Eval      Mninpd# = Mninp#
     C                   Eval      Mnmdpd# = Mnmdp#
     C                   Eval      Mninrv# = Mninr#
     C                   Eval      Mnmdrv# = Mnmdr#
     C                   Eval      Mnexrv# = Mnexr#
     C                   Eval      Mnuarv# = Mnuar#
      **** END   ADD **** 04/04/02 *****************************************************************
      *
     C                   Eval      Mnfyr  = CmFyr
     C                   Eval      Mnfnd  = Cmfnd
     C                   Eval      Mnemp# = Cmemp#
     C                   Eval      MnCase = CmCase
     C                   Write     Cmn01
      *
     C                   Endsr
      /eject
     **********************************************************************
     *
     *  Get Record from Deductible Detail File
     **********************************************************************
      *
     C     Get_Deduct    Begsr
      *
     C     Wdddl_Key     Setll     Wdddl
      *
     C                   Dou       %Eof(Wdddl)
      *
     C     Wdddl_Key     Reade     Wdddl
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C     *YMD          Move      Ddidt         Datymd
      *
     C                   If        Datymd <= ThruDt
     C                   Eval      Mnded# = Mnded# + Ddamtb
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *
     *  Get Record from  Claims Checks Payment File
     **********************************************************************
      *
     C     Accum_Paymts  Begsr
      *
     C     Wdpal_Key     Setll     Wdpal
      *
     C                   Dou       %Eof(Wdpal)
      *
     C     Wdpal_Key     Reade     Wdpal
      *
     C                   If        %Eof
     C                   Leave
     C                   Endif
      *
     C                   If        Patrdt = *zero
     C                   Iter
     C                   Endif
      *
     C                   Move      Patrdt        H6
     C     *YMD          Move      H6            Datymd
      *
     C                   If        Datymd <= ThruDt
      *
     C                   If        Paretp = 'C'
      *
     C                   If        Patype = 'P' or
     C                             Patype = 'M' or
     C                             Patype = 'A' or
     C                             Patype = 'Z'
     C                   Eval      Mninp# = Mninp# + Paamt
     C                   Endif
      *
     C                   If        Patype = 'S' or
     C                             Patype = 'R' or
     C                             Patype = 'V'
     C                   Eval      Mninp# = Mninp# - Paamt
     C                   Endif
     C                   Endif
      *
     C                   If        Paretp = 'M'
      *
     C                   If        Patype = 'P' or
     C                             Patype = 'M' or
     C                             Patype = 'A' or
     C                             Patype = 'Z'
     C                   Eval      Mnmdp# = Mnmdp# + Paamt
     C                   Endif
      *
     C                   If        Patype = 'S' or
     C                             Patype = 'R' or
     C                             Patype = 'V'
     C                   Eval      Mnmdp# = Mnmdp# - Paamt
     C                   Endif
     C                   Endif
      *
     C                   If        Paretp = 'L' or Paretp = 'E'
      *
     C                   If        Patype = 'P' or
     C                             Patype = 'M' or
     C                             Patype = 'A' or
     C                             Patype = 'Z'
     C                   Eval      Mnexp# = Mnexp# + Paamt
     C                   Endif
      *
     C                   If        Patype = 'S' or
     C                             Patype = 'R' or
     C                             Patype = 'V'
     C                   Eval      Mnexp# = Mnexp# - Paamt
     C                   Endif
     C                   Endif
      *
     C                   If        Paretp = '*'
      *
     C                   If        Patype = 'P' or
     C                             Patype = 'M' or
     C                             Patype = 'A' or
     C                             Patype = 'Z'
     C                   Eval      Mnuap# = Mnuap# + Paamt
     C                   Endif
      *
     C                   If        Patype = 'S' or
     C                             Patype = 'R' or
     C                             Patype = 'V'
     C                   Eval      Mnuap# = Mnuap# - Paamt
     C                   Endif
     C                   Endif
      *
     C                   If        Patype = 'R'
     C                   Movel(p)  Pabr2         Tab10
     C     Wtabl_Key     Chain     Pb201
      *
     C                   If        %Found
     C                   If        B2plcd = 'O' or
     C                             B2plcd = 'P'
      *
     C                   Eval      Mnsubr = %editc(Paamt:'P')
      *
     C                   Endif
     C                   Endif
     C                   Endif
      *
     C                   If        Patype = 'R'
     C                   Movel(p)  Pabr2         Tab10
     C     Wtabl_Key     Chain     pb201
      *
     C                   If        %Found
      *
     C                   If        B2plcd = 'Q'
     C                   Eval      Mnddr#  = Paamt + Mnddr#
     C                   Endif
      *
     C                   Endif
     C                   Endif
      *
     C                   Endif
      *
     C                   Enddo
      *
     C                   Endsr
      /eject
     **********************************************************************
     *                                                                *****
     *  Get total for Indemnity Reserves.
     *                                                                *****
     **********************************************************************
      *
     C     Indem_Rsrvs   Begsr
      *
     C                   Move      Prmco#        Codco#
     C                   Move      'C'           Codtyp
      *
     C                   Move      Cmrt01        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr01
     C                   Endif
      *
     C                   Move      Cmrt02        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr02
     C                   Endif
      *
     C                   Move      Cmrt03        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr03
     C                   Endif
      *
     C                   Move      Cmrt04        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr04
     C                   Endif
      *
     C                   Move      Cmrt05        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr05
     C                   Endif
      *
     C                   Move      Cmrt06        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr06
     C                   Endif
      *
     C                   Move      Cmrt07        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr07
     C                   Endif
      *
     C                   Move      Cmrt08        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr08
     C                   Endif
      *
     C                   Move      Cmrt09        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr09
     C                   Endif
      *
     C                   Move      Cmrt10        Codtab
     C     Codkey        Lookup    Cod                                    59
      *
     C                   If        %Equal
     C                   Eval      Mninr# = Mninr# + Cmrr10
     C                   Endif
      *
      *
     C                   Endsr
      /eject
     **********************************************************************
     *                                                                *****
     *  Get total for Medical Reserves.
     *                                                                *****
     **********************************************************************
      *
     C     Medcl_Rsrvs   Begsr
      *
     C                   Move      Prmco#        Codco#
     C                   Move      'M'           Codtyp
      *
     C                   Move      Cmrt01        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr01
     C                   Endif
      *
     C                   Move      Cmrt02        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr02
     C                   Endif
      *
     C                   Move      Cmrt03        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr03
     C                   Endif
      *
     C                   Move      Cmrt04        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr04
     C                   Endif
      *
     C                   Move      Cmrt05        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr05
     C                   Endif
      *
     C                   Move      Cmrt06        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr06
     C                   Endif
      *
     C                   Move      Cmrt07        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr07
     C                   Endif
      *
     C                   Move      Cmrt08        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr08
     C                   Endif
      *
     C                   Move      Cmrt09        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr09
     C                   Endif
      *
     C                   Move      Cmrt10        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnmdr# = Mnmdr# + Cmrr10
     C                   Endif
      *
      *
     C                   Endsr
      /eject
     **********************************************************************
     *                                                                *****
     *  Get total for Legal Reserves.
     *                                                                *****
     **********************************************************************
      *
     C     Legal_Rsrvs   Begsr
      *
     C                   Move      Prmco#        Codco#
     C                   Move      'L'           Codtyp
      *
     C                   Move      Cmrt01        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr01
     C                   Endif
      *
     C                   Move      Cmrt02        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr02
     C                   Endif
      *
     C                   Move      Cmrt03        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr03
     C                   Endif
      *
     C                   Move      Cmrt04        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr04
     C                   Endif
      *
     C                   Move      Cmrt05        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr05
     C                   Endif
      *
     C                   Move      Cmrt06        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr06
     C                   Endif
      *
     C                   Move      Cmrt07        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr07
     C                   Endif
      *
     C                   Move      Cmrt08        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr08
     C                   Endif
      *
     C                   Move      Cmrt09        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr09
     C                   Endif
      *
     C                   Move      Cmrt10        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr10
     C                   Endif
      *
      *
     C                   Endsr
      /eject
     **********************************************************************
     *                                                                *****
     *  Get total for Expense Reserves.
     *                                                                *****
     **********************************************************************
      *
     C     Exp_Rsrvs     Begsr
      *
     C                   Move      Prmco#        Codco#
     C                   Move      'E'           Codtyp
      *
     C                   Move      Cmrt01        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr01
     C                   Endif
      *
     C                   Move      Cmrt02        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr02
     C                   Endif
      *
     C                   Move      Cmrt03        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr03
     C                   Endif
      *
     C                   Move      Cmrt04        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr04
     C                   Endif
      *
     C                   Move      Cmrt05        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr05
     C                   Endif
      *
     C                   Move      Cmrt06        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr06
     C                   Endif
      *
     C                   Move      Cmrt07        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr07
     C                   Endif
      *
     C                   Move      Cmrt08        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr08
     C                   Endif
      *
     C                   Move      Cmrt09        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr09
     C                   Endif
      *
     C                   Move      Cmrt10        Codtab
     C     Codkey        Lookup    Cod                                    59
     C                   If        %Equal
     C                   Eval      Mnexr# = Mnexr# + Cmrr10
     C                   Endif
      *
      *
     C                   Endsr
      /eject
     **********************************************************************
     *                                                                *****
     *  *INZSR   INITIAL SUBROUTINE
     *                                                                *****
     **********************************************************************
      *
     C     *Inzsr        Begsr
      *
     C     *Entry        Plist
     C                   Parm                    PrmCo
     C                   Parm                    PrmMdya
      *
     C                   Move      Prmco         Prmco#
     C                   Move      Prmmdya       Prmmdy
     C                   Move      PrmMdy        H2
     C                   Movel     PrmMdy        H4
     C                   Move      H4            PrmYmd
     C                   Movel     H2            PrmYmd
      *
      * Move numeric Parm date to "date" fields
      *
     C     *Mdy          Move      PrmMdy        ThruDt
      *
     **********************************************************************
      *
     C     *Like         Define    OcPcndt       HldCndt
     C     *Like         Define    Cmemp#        Kyemp#
     C     *Like         Define    Elrtcd        Kyrtcd
     C     *Like         Define    Eldiv         Kydiv
     C     *Like         Define    ElFyr         FirstFyr
     C     *Like         Define    ElFyr         Fyear
     C     *Like         Define    Elpfdt        FirstEfd
     C     *Like         Define    RsJrst        Risk_St
     C     *Like         Define    ElPror        SrdPror
     C     *Like         Define    EcExco        SvExco
     C     *Like         Define    EcMpec        SvMpec
     C     *Like         Define    EcSful        SvSful
     C     *Like         Define    RsJrst        SvRst
      *
     C     Wtttl1_Key    Klist
     C                   Kfld                    Prmco#
      *
     C     Wdecl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Rsjrst
     C                   Kfld                    SrdEfd
      *
     C     Wdecl_Key2    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Rsjrst
      *
     C     Wdell3_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
      *
     C     Wdell1_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svfyr
     C                   Kfld                    Svemp#
      *
     C     Wdell1_Key2   Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Cmfyr
     C                   Kfld                    Cmemp#
     C                   Kfld                    Cmdiv
      *
     C     Wdell1_Key3   Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Elfyr
     C                   Kfld                    Elemp#
     C                   Kfld                    Eldiv
      *
     C     Wdell8_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svemp#
     C                   Kfld                    Svfyr
      *
     C     Wdell8_Key1   Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Kyemp#
      *
     C     Wdell2_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Kyemp#
      *
     C     Wmrpl4_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svfyr
     C                   Kfld                    Svemp#
      *
     C     Wmeml_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Kyemp#
     C                   Kfld                    Kydiv
      *
     C     Wmeml_Key2    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Elemp#
      *
     C     Wmrsl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Kyrtcd
      *
     C     Wmgcl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
      *
     C     Wdehl1_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svemp#
     C                   Kfld                    Svdiv
     C                   Kfld                    Svfyr
      *
     C     Wdehl1_SGT    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svemp#
     C                   Kfld                    Svdiv
     C                   Kfld                    Svfyr
     C                   Kfld                    GT999
      *
     C     Wmfnl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
      *
     C     Wmcml5_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
      *
     C     Wdddl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svfyr
     C                   Kfld                    Cmcase
      *
     C     Wdpal_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svfyr
     C                   Kfld                    Cmcase
      *
     C     WdpalB_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svfyr
     C                   Kfld                    Pacase
     C                   Kfld                    Pachk#
     C                   Kfld                    Pabasq
      *
     C     Wmpdl_Key     Klist
     C                   Kfld                    Pafein
     C                   Kfld                    Pamod
      *
     C     Wfeul_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svemp#
     C                   Kfld                    Svfyr
     C                   Kfld                    OcState
      *
     C     Wdrrl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    OcState
     C                   Kfld                    Elpfdt
      *
     C     Wdrrl_Key2    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    OcState
      *
     C     Wtlml_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Tab10
      *
     C     Wtddl3_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    OcState
     C                   Kfld                    L8dcde
     C                   Kfld                    L8pfdt
      *
     C     Wtddl3_Key2   Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    OcState
     C                   Kfld                    L8dcde
      *
     C     Wmchl_SGT     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svfyr
     C                   Kfld                    Cmcase
     C                   Kfld                    Efdt999
      *
     C     Wmchl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svfyr
     C                   Kfld                    Cmcase
      *
     C     Wtpal1_Key    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Tab10
      *
     C     Wtabl_key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Tab10
      *
     C     Wtgtl_key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Tabcod
     C                   Kfld                    Tab10
      *
     C     Wdrpl_Key     Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svfyr
     C                   Kfld                    Cmcase
     C                   Kfld                    Pachk#
     C                   Kfld                    Patrdt
      *
     C     Wdrpl_KeyB    Klist
     C                   Kfld                    Prmco#
     C                   Kfld                    Pggrp
     C                   Kfld                    Svfyr
     C                   Kfld                    Cmcase
     C                   Kfld                    Pachk#
     C                   Kfld                    BBtrdt
     *
    C*** Begin add *** 05/22/03 **************************************
     C     Wdrpl3_Key    Klist
     C                   kfld                    paco#
     C                   kfld                    pafnd
     C                   kfld                    pafyr
     C                   kfld                    pacase
     C                   kfld                    paseq#
    C*** End add ***** 05/22/03 **************************************
      *
     C     Polml_Key     Klist
     C                   Kfld                    Lmgrp
     C                   Kfld                    Lmpol
     C                   Kfld                    Lmefdt
     C                   Kfld                    Lmrst
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
     **********************************************************************
      *
     C     *Like         Define    Ememp#        Svemp#
     C     *Like         Define    Ememp#        Hldemp#
     C     *Like         Define    Elfyr         Hldfyr
     C     *Like         Define    Emdiv         Svdiv
     C     *Like         Define    Elfyr         Svfyr
     C     *Like         Define    Elpfdt        Svpfdt
     C     *Like         Define    Elpfdt        Hldpfdt
     C     *Like         Define    Rpcap#        Svpol#
     C     *Like         Define    Pptrcd        Svtrcd
     C     *Like         Define    Ehtrdt        Svtrdt
     C     *Like         Define    Elprtd        Expconst
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
     **********************************************************************
      *
     C                   Eval      X = 1
      *
     C                   Dou       %Eof
     C                   Read      Wtrtp
      *
     C                   If        not %eof and
     C                             Rtco# = Prmco#
     C                   If        Rttype = 'C' or
     C                             Rttype = 'M' or
     C                             Rttype = 'L' or
     C                             Rttype = 'E'
      *
     C                   Move      Rtco#         Codco#
     C                   Movel(p)  Rtkey         Codtab
     C                   Move      Rttype        Codtyp
     C                   Move      Codkey        Cod(X)
     C                   Eval      X = X + 1
     C                   Endif
     C                   Endif
      *
     C                   Enddo
     C                   Move      Udate         Today
      *
     C                   Endsr
      *
      /eject
