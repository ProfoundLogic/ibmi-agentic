     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BndDir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H* TITLE W7421R - AGENT COMMISSION STATEMENT
     H*****************************************************************
     H* Revisions:                                                    *
     H*
     H* 12/11/25 C:65  Project: Phw#120223 (Internal)
     H*                Submit output to Scriptura after all statements
     H*                have been generated.
     H*
     H* 09/26/25 C:062 Ticket 119676(146)
     H*                Fix process to generate csv and attachment to
     H*                darts.
     H*
     H* 09/04/24 C:079 Ticket 117604(140)
     H*                Do not duplicate headings or include other
     H*
     H* 11/04/24 C:079 Ticket 117604(140)
     H*                Do not duplicate headings or include other
     H*                agents in the current agent file
     H*
     H* 03/06/24 C:062 Ticket 115204(140)
     H*                Make sure to find policy by using policy year.
     H*
     H* 01/16/24 C:333 Project: WC2818 (Under ticket 108943)
     H*                Agent Commission-CSV Attached to iDARTS
     H*
     H* 03/02/22 C:136 Project: WC2818
     H*                Create CSV of WFACP and attach to iDarts with
     H*                the individual statement.
     H*                Changes by C: 079 starting 3/8/22
     H*
     H* 09/23/21 C:33  Seq#: 97393
     H*               -Load ACco# for WXACPSTMT1.
     H*               -Write to all workfiles regardless of using spooled files.
     H*
     H* 11/09/20 C:65  Project: WC2073/Phw#92739
     H*                Pass along CO#/GRP from entry parameters to STLETTER.
     H*          C:33  Do not process policy dates without a policy
     H*                Retrieve WMAGP rrn
     H*
     H* 07/09/20 C:65  Project: WC2073
     H*                Update Scriptura coding to current standards/design.
     H*
     H* 04/06/16 C:33  Project: SY371
     H*               -Scriptura Phase II. Create output file for XML.
     H*
     H* 02/17/16 C:33  38406
     H*               -Include premium for all years.
     H*
     H* 10/13/15 C:33  37824
     H*               -Expand "current activity" totals
     H*               -Expand "current earnings" totals
     H*
     H* 07/17/15 C:33  36099
     H*               -Don't print routing page when resolving
     H*
     H* 06/15/15 C:33  Project: 114WPE  Priss #21
     H*               -Reset premium without needing a employer detail rcd
     H*
     H* 06/08/15 C:33  Project: 114WPE  Priss #20
     H*               -Change column headings to Current Period Activity
     H*                 and Amount This Period.
     H*               -Remove page break before summary.
     H*               -Line up summary information.
     H*
     H* 05/20/15 C:33  PHW34913
     H*               -Remove producer break within agent.
     H*
     H* 05/06/15 C:33  PHW34673
     H*               -Keep previous balance and payment lines from
     H*                 overlaying.
     H*               -Do not print producers with zero balance.
     H*
     H* 04/20/15 C:33  Do not list a carrier # if no employer in file
     H*
     H* 04/13/15 C:33  PHW32188
     H*                Do not resolve statements for a mass runs
     H*
     H* 10/14/14 C:33  Project: 114WPE                                *
     H*                Account for multiple plan types
     H*
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 04/28/14 C:302 SUPPORT: WA114 85376                           *
     H*                Fix logic to Pass MOD# number correctly        *
     H*                to DA700R to Resolve Darts correctly.          *
     H*                                                               *
     H*                                                               *
     H* 01/08/14 C:136 Support: WA114 84453                           *
     H*                Correct build of WMLSP so that Agent Statements*
     H*                will resolve correctly at Check Print. The     *
     H*                Zero Balance flag was only being set on intial *
     H*                write.                                         *
     H*                                                               *
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H* 09/10/12 C:283 SUPPORT: WA114 80480                           *
     H*                Fix logic to Pass FEIN number correctly        *
     H*                to DA700R to Resolve Darts correctly.          *
     H*                                                               *
     H* 07/11/12 C:191 SUPPORT: WA114 80334                           *
     H*                Fix logic to resolve output to correct agent   *
     H*                Do not print routing page if resolving         *
     H*                                                               *
     H* 02/06/12 C:255 Project: 114WBD                                *
     H*                Add changes to write statement period date     *
     H*                range, group/agent to file WMSLP               *
     H*                                                               *
     H* 01/27/11 C:136 SUPPORT: WA146 75541                           *
     H*                CORRECT HOW ACPFDT IS POPULATED                *
     H*                                                               *
     H* 06/30/10  C:062  Project: 73378                               *
     H*                  Populate ACPFDT                              *
     H*                                                               *
     H*  06/10/10 C:251 PROJECT: 133WJC                               *
     H*                 ADD RATING STATE TO OUTPUT                    *
     H*                                                               *
     H*  01/28/10 C:249 Project: SY215                                *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  10/20/09 C:220 SUPPORT: 70024                                *
     H*                 PREVENT PRODUCER HEADING WITHOUT DETAILS      *
     H*                                                               *
     H*  06/18/09 C:220 SUPPORT: 102WEP/68898                         *
     H*                 NEED TO CORRECT PAGE BREAK BASED ON PROCUCER  *
     H*                                                               *
     H*  08/21/08 C:191 PROJECT: 102WEP                               *
     H*                 BACK TO BASE : BASE OPTION                    *
     H*                   - PRINT CHANGE - AGENT COMMISSION STMNT     *
     H*                                                               *
     H*  06/02/08 C:191 PROJECT: 132WKE                               *
     H*                 BUSINESS OBJ LINK TO CARRIER POL#             *
     H*                                                               *
     H*  02/12/07 C:180 SEQ# 55374
     H*                 Print 16 characters of policy number.
     H*
     H*  09/29/06 C:33  Seq#: WA091/55053                             *
     H*                -Do not print producer totals for producer     *
     H*                  zero.                                        *
     H*                -Create an extra space after the previous      *
     H*                  balance (per KK)                             *
     H*                                                               *
     H*  09/08/06 C:132  Project:  71WHI Back to Base                 *
     H*                - Print verbiage on report "Audit Credit       *
     H*                  Applied" if current Year if Commision code   *
     H*                  is "TA".                                     *
     H*                - Print verbiage on report "Audit Credit" if   *
     H*                  Prior Year if Commision code if "TA".        *
     H*                  is "TA".                                     *
     H*                - If not Current or Prior year, use default    *
     H*                  A/R transaction code description             *
     H*                                                               *
     H*  09/01/06 C:180 Project: 114WMF                               *
     H*                 Create Carrier# if blank.                     *
     H*                                                               *
     H*  01/16/06 C:33  Seq#: WA091/50346                             *
     H*                 Be sure to not print agents with zero balance *
     H*                  when selected.                               *
     H*                                                               *
     H*  06/24/05 C:33  Project: 109WDZ                               *
     H*                 {109CS}                                       *
     H*                 Add carrier policy # to statement.            *
     H*                                                               *
     H*  03/22/01 C:90  PROJECT:WA112/26447                           *
     H*                 ADDED REGION# AND DESCRIPTION TO WORKFILE.    *
     H*                                                               *
     H*  12/13/00 C:61  PROJECT: WC924                                *
     H*                 Output for all groups if QQFND=*ZERO.         *
     H*                                                               *
     H*  04/19/00 C:118 PROJECT: WC577                                *
     H*                 ADD OUTPUT FOR CREATING WORKFILE WFACP.       *
     H*                                                               *
     H*  11/17/98 C:33 SUPPORT: WA095/12630                           *
     H*                ACCUMULATE SEPARATE TOTAL FOR SECONDARY AGENTS *
     H*                                                               *
     H*  11/03/98 C:33 SUPPORT: OP031/14819                           *
     H*               -ACCOUNT FOR LAST AGENT WHEN CHECK FOR INACTIVE *
     H*                AGENTS                                         *
     H*               -ACCOUNT FOR INACTIVE AGENTS THAT HAVE A PAYMENT*
     H*                                                               *
     H*  10/19/98 C:33 SUPPORT: WA093/13382                           *
     H*                DO NOT CONVERT XXYEAR TO 4.0 FOR PRINTING      *
     H*                                                               *
     H*  08/19/98 C:09 PROJECT: SUPP  WA093  SEQ# 13309               *
     H*                IGNORE INACTIVE AGENT HISTORY RECORDS          *
     H*                                                               *
     H*  06/25/98 C:33 PROJECT: WC577                                 *
     H*                CORRECT REPORT ALLIGNMENT. (NO COMMENTS)       *
     H*                                                               *
     H*  06/15/98 C:33 SEQ#: WA095/12257                              *
     H*                CHECK ALL AGENTS FOR INACTIVITY.               *
     H*                                                               *
     H*  06/14/98 C:33 SUPPORT                                        *
     H*                USE CORRECT SIZE FOR EMP#/DIV                  *
     H*                                                               *
     H*  05/21/98 C:33 SEQ#: WA095/11712                              *
     H*                ONLY CALCLUATE THE BALANCE AS OF THE 'TO' DATE.*
     H*                                                               *
     H*  05/20/98 C:65 PROJECT: CM038                                 *
     H*                IF THE AGENT/PROD ISNT THE CURRENTLY ACTIVE,   *
     H*                USE 0 WRITTEN PREMIUM.                         *
     H*                                                               *
     H*  04/14/98 C:65 PROJECT: CM038                                 *
     H*                USE ELANPR/ELAAPR FOR ANNUAL PREMIUM FIGURES.  *
     H*                                                               *
     H*  04/22/98 C:77 PROJECT: WC815                                 *
     H*                MODIFY TO CORRECTLY PROCESS REQUESTS FOR       *
     H*                'PRINT IF 0 BAL' = 'N'                         *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS:                                                  *
     H*                                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAPP     uf   e             disk    rename(pap01:pap0p)
     FWMAGL     IF   E           K DISK
     F                                     infds(dbfds)
     FWDELL2    IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL00)
     Fwdehl6    if   e           k disk    rename(peh01:peh06)
     FWMEML     IF   E           K DISK
     FWMACL3    IF   E           K DISK
     FWMACL5    IF   E           K DISK
     F                                     RENAME(PAC01:PAC02)
     FWMAHL     IF   E           K DISK
     FWMRPL2    IF   E           K DISK
     F* TABLE FILE
     FWTABL     IF   E           K DISK
     F*
     F* AGENT COMMISSION WORKFILE
     FWFACP     UF A E             DISK
     F                                     RENAME(PAC01:PAC03)
     F                                     prefix(A3:2)
     FWMREL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F                                     RENAME(PRS01:PRS02)
     FWMLSL     UF A E           K DISK
     FWXACPstmtCo    e           k disk    usropn
     FWXACPstmt1o    e           k disk    usropn
     FWXACPstmt2o    e           k disk    usropn
     FWXACPstmt3o    e           k disk    usropn
     FWXACPstmt4o    e           k disk    usropn
     FWXACPstmt5o    e           k disk    usropn
     FWXACPstmt6o    e           k disk    usropn
     FXMLPREL1  UF   E           K DISK
       //*** beg add ***** 03/02/22 ************************
       Dcl-F WMEXL Keyed Rename(pex01:pex00);
       //*** end add ***** 03/02/22 ************************
     FQSYSPRT   O    F  180        PRINTER OFLIND(*INOA)
     F                                     USROPN

     D aryProdDtl      s              1n   dim(99999)
     D SZ              S              1    DIM(13)
     D ST              S              2    DIM(1)
     D YR              S              3  0 DIM(999)
     D AMT             S             11  2 DIM(999)
     D aryEmpPremYr    s              3  0 dim(999)
     D aryEmpPrem      s             15  2 dim(999)
     D aryEmpPremPrt   s              1n   dim(999)
     D SRT             S             14    DIM(999) ASCEND
     D RMK             S             34    DIM(3) CTDATA PERRCD(1)
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  AGMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D*
     D                 DS
     D  DSCAP#                 1     25
     D  ACFND                  1      3  0
     D  ACEMP#                 4     12  0
     D  ACFYR                 13     15  0
      *
     D                 DS
     D  XXCUDT                 1      7  0
     D  XXCFYR                 1      3  0
     D  XXCMD                  4      7  0
     D* Current Year Constant
     D TACUDS          C                   CONST('AUDIT CREDIT APPLIED')
     D* Prior Year Constant
     D TAPRDS          C                   CONST('AUDIT CREDIT ')
     D*
     D                 DS
     D  @@FRDT                 1      8
     D  @@MTH                  1      2
     D  @@DAY                  3      4
     D  @@FYR                  5      8
     D                 DS
     D  @@TODT                 1      8
     D  @@MTH1                 1      2
     D  @@DAY1                 3      4
     D  @@FYR1                 5      8
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      // Cash basis plan totals
        // Group
     D xxFAnnC         s             11  2
     D xxFColC         s             11  2
     D xxFAmtC         s             11  2
        // Agent
     D xxAAnnC         s             11  2
     D xxAColC         S             11  2
     D xxAAmtC         s             11  2
        // Producer
     D xxPAnnC         s             11  2
     D xxPColC         S             11  2
     D xxPAmtC         s             11  2
      // Written premium basis plan totals
        // Group
     D xxFAnnW         s             11  2
     D xxFColW         s             11  2
     D xxFAmtW         s             11  2
        // Agent
     D xxAAnnW         s             11  2
     D xxAColW         s             11  2
     D xxAAmtW         s             11  2
        // Producer
     D xxPAnnW         s             11  2
     D xxPColW         s             11  2
     D xxPAmtW         s             11  2

     D isAgtTotBrk     s              1n   inz
     D isCalcBal       s              1n   inz
     D isIncAllPrem    s              1n   inz('1')
     D isNoRcdsGrp     s              1n
     D isNoRcdsAgt     s              1n
     D isNoRcdsProd    s              1n
     D isBrkProd       s              1n
     D isFoundProd     s              1n
     D isPrtSplitAgt   s              1n
     D isPrtSplitGrp   s              1n
     D isPrtSplitProd  s              1n
     D isPrtZeroProd   s              1n   inz
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D saveACemp#      s                   like(ACemp#)
     D saveACdiv       s                   like(ACdiv)
     D saveACplan      s                   like(ACplan)
     D saveACprod      s                   like(ACprod)
     D savePrtProd     s                   like(ACprod)
     D xmlFile         s             60    inz
     D xmlTemplate     s             60    inz
     D xxPlanDesc      s             30
     D xxHoldPrem      s                   like(EHwAPr)
     D xxPrtPrem       s                   like(EHwAPr)
     D xxPrtPremYr     s                   like(ELfyr)
     D xx2nd3rd        s              1
     D rrn             s              9  0
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x@RRN           s              9  0 inz(0)
     D isMassReq       s              1n   inz
     d massFlag        s              1
     d massRun         s              1
     d xmlLink         s                   like(XPLink)
     d xmlFlowName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('AL')
     d xxProcessType   s            100
     d processType     c                   const('Commission Statements')
      //*****Beg Add *** 01/16/24 **************************
     d Data            s           5000
      //*****End Add *** 01/16/24 **************************

     D svlda           ds          1024    dtaara(*lda)
     D ldMassFile                    60    Overlay(svlda:129)
     D ldOutputFile                  60    Overlay(svlda:191)

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     d
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const

     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  isFound                       1
     d
     DObjExists        pr             1n
     D object                        10    const
     D objType                        7    const
       //*** beg add ***** 03/02/22 ***********************
       Dcl-S isWfacpData Char(1);
       Dcl-S chrRtCd Char(1);
       //***** Beg Chg *** 01/16/24 ************************
       //***Dcl-S strCommand Char(255);
       Dcl-S strCommand Char(700);
       //***** End Chg *** 01/16/24 ************************
       Dcl-S strDocId Char(40);
       Dcl-S strExists Char(80);
       Dcl-S strPath Char(80);
       Dcl-S strPath2 VarChar(80);
       Dcl-S strDcid2 VarChar(12);
       Dcl-S decLength Packed(15:5);
       Dcl-S strDocRoot Char(10);
       Dcl-S sqlStmtStr Char(200);
       Dcl-S intFein Like(ACfein);
       Dcl-S intMod Like(ACMod);
       Dcl-S AgtCount Zoned(5);
       // Procedures
       Dcl-Pr AddDocToDarts extPgm('ISEWMD');
        key Char(40);
        seq# Char(5);
        docID Char(40);
        docFolder Char(60);
        type Char(1) Const;
        desc Char(40) Const;
        remarkCode Char(5);
        systemName Char(20) Const;
        programName Char(10);
        user Char(10);
        returnCode Char(1) Const;
        exists Char(80) Const;
        directoryPath Char(80);
        buildPath Char(1) Const;
       End-Pr;
       Dcl-Pr GetNextDocID ExtPgm('SNXTDCID');
        docFolder Char(80);
        docID Char(12);
        pre Char(2) Const;
        co# Char(3);
        fnd Char(3);
        returnCode Char(1);
       End-Pr;
       Dcl-Pr RunCommand ExtPgm('QCMDEXC');
        //***** Beg Chg *** 01/16/24 ***********************
        //***strCommand Char(255) Const;
        strCommand Char(700) Const;
        //***** End Chg *** 01/16/24 ***********************
        intLength Packed(15:5) Const;
       End-Pr;
       Dcl-Pr RtvDocRoot ExtPgm('RTVDOCROOT');
        strDocRoot Char(10);
       End-Pr;
       //*** end add ***** 03/02/22 ***********************
     D/copy sPrModule
     D/copy sPrXML
     D/copy wPrAgent
     D/copy wPrProduce
     D/copy wPrTable
     D*** Begin Add *** 09/26/25 *******************************************
     D/copy sPrJobInfo
     D*** End   Add *** 09/26/25 *******************************************

     IPEL00
     I              ELAAPR                      E1AAPR
     I              ELANPR                      E1ANPR
     I              ELFYR                       E1FYR
     I              ELPFDT                      E1PFDT

     C*================================================================
     C*   LIKE DEFINES
     C*================================================================
     C     *LIKE         DEFINE    FNCO#         XXCO#
     C     *LIKE         DEFINE    FNFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELPTDT        XXTODT
     C     *LIKE         DEFINE    ELPFDT        XXFRDT
     C     *LIKE         DEFINE    ACEMP#        XXEMP#
     C     *LIKE         DEFINE    ACDIV         XXDIV
     C     *LIKE         DEFINE    ELFAF         XXZBF
     C     *LIKE         DEFINE    ELANPR        XXPREM
     C     *LIKE         DEFINE    ACAGT#        XXSVA#
     C     *LIKE         DEFINE    *IN60         LSTAGT
     C     *LIKE         DEFINE    CSKEY         XXKEY
     C     *LIKE         DEFINE    *IN60         EMPFL
     C     *LIKE         DEFINE    *IN60         PRDFL
     C     *LIKE         DEFINE    CONAME        XXCONM
     C     *LIKE         DEFINE    ACAGT#        XXAGT#
     C*
     C*================================================================
     C*   KEY FIELDS
     C*================================================================
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACAGT#
     C                   KFLD                    ACPROD
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C*
     C     ACKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACAGT#
     C*
     C     ACKY2A        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C     ACKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACAGT#
     C                   KFLD                    ACPROD
     C*
     C     ACKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ACKY7         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACAGT#
     C                   KFLD                    XXFRDT
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACAGT#
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    ACPROD
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C                   KFLD                    XXTODT
     C*
     C     ELKY          KLIST
     C                   KFLD                    A3CO#
     C                   KFLD                    A3FND
     C                   KFLD                    A3EMP#
     C                   KFLD                    A3DIV
     C                   KFLD                    A3FYR
     C*** Begin Add *** 03/06/24 *******************************************
     C     ELKY2         KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C                   KFLD                    ACFYR
     C*** End   Add *** 03/06/24 *******************************************
     C*
     C* AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    XXTODT
     C*
     C     AHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C                   KFLD                    ELFYR
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C*
     C     RPKY          KLIST
     C                   KFLD                    ACCO#
     C                   KFLD                    ACFND
     C                   KFLD                    ACEMP#
     C                   KFLD                    ACDIV
     C                   KFLD                    ACFYR
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     REKY          KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMRGN#
     C*
     C     LSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ACAGT#
     C*================================================================
     C*   ENTRY PARAMETERS
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQAGT#            5
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQZBF             1
     C                   PARM                    QQTYPO            1
     C                   PARM                    QQCRCD            5
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         sysDt             7 0
     C                   OPEN      QSYSPRT
     C*  READ USED TO ACCESS UPDATE FOR WORK FILE
     C*
     C  N60
     CAN 60              READ      PAC03                                  60
     C*
     C                   EXSR      INSR
     C*
     C*===============================================================
     C*  MAIN ROUTINE
     C*===============================================================
      *
      *  PROCESS FUND(S)
      *
     C     XXFND         IFEQ      *ZERO
     C     FNKY1         CHAIN     PFN01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     FNTEST        IFNE      'Y'
     C                   Z-ADD     FNFND         XXFND
     C                   Z-ADD     0             XXSVA#
     C                   Eval      isNoRcdsGrp = *on
     C                   Z-ADD     0             PAGE
      *
      *  Center Company name in display field
      *
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
      *
     C                   ELSE
      *
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
      *
     C                   MOVEL     ##DATA        XXCONM
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
      *
     C                   MOVEL     ##DATA        XCNAME           60
      *
     C                   Z-ADD     0             XXPANN           11 2
     C                   Z-ADD     0             XXPCOL           11 2
     C                   Z-ADD     *ZEROS        X2PCOL           11 2
     C                   Z-ADD     *ZEROS        X3PCOL           11 2
     C                   Z-ADD     0             XXPAMT           11 2
     C                   Z-ADD     0             XXAANN           11 2
     C                   Z-ADD     0             XXACOL           11 2
     C                   Z-ADD     *ZEROS        X2ACOL           11 2
     C                   Z-ADD     *ZEROS        X3ACOL           11 2
     C                   Z-ADD     0             XXAAMT           11 2
     C                   Z-ADD     0             XXFANN           11 2
     C                   Z-ADD     0             XXFCOL           11 2
     C                   Z-ADD     *ZEROS        X2FCOL           11 2
     C                   Z-ADD     *ZEROS        X3FCOL           11 2
     C                   Z-ADD     0             XXFAMT           11 2
      /Free
       // Clear plan type totals
         // Cash basis plan totals
       Clear xxFAnnC;
       Clear xxFColC;
       Clear xxFAmtC;
       Clear xxAAnnC;
       Clear xxAColC;
       Clear xxAAmtC;
       Clear xxPAnnC;
       Clear xxPColC;
       Clear xxPAmtC;
         // Written premium basis plan totals
       Clear xxFAnnW;
       Clear xxFColW;
       Clear xxFAmtW;
       Clear xxAAnnW;
       Clear xxAColW;
       Clear xxAAmtW;
       Clear xxPAnnW;
       Clear xxPColW;
       Clear xxPAmtW;
       isPrtSplitGrp = *off;
      /End-free
      *
     C                   EXSR      PRCFND
     C                   ENDIF
      *
     C     FNKY1         READE     PFN01                                  60
     C                   ENDDO
      *
     C                   ELSE
      *
     C                   EXSR      PRCFND
     C                   ENDIF
      *
      /Free
       xxPrt = '0';
       Chain orKy POR01;
       If %found and qqCrCd = *blanks and (qqTypo = 'P' or qqTypo = 'B');
          Except Route;
       Endif;
       Exsr WriteStmtC;
      /End-free
     C                   CLOSE     QSYSPRT
     C     QQCRCD        IFNE      *BLANKS
     C     QQAGT#        ANDNE     *ZEROS
     C                   EXSR      SRDRTS
     C                   ENDIF
      /Free

         If %open(WXACPSTMTC);
            Close WXACPSTMTC;
            Close WXACPSTMT1;
            Close WXACPSTMT2;
            Close WXACPSTMT3;
            Close WXACPSTMT4;
            Close WXACPSTMT5;
            Close WXACPSTMT6;
         Endif;

         Select;
         When xmlFlowName   = *Blanks;
       //*** begin add ***  12/11/25  ************************************
       //* Do not call Scriptura if processing in mass via W3032R
         When massFlag = 'Y';
       //*** end add *****  12/11/25  ************************************
         When objExists('PDSDEV':'*LIB');
            Monitor;
            S4401C(xmlFlowName + '$');
            On-error;
            Endmon;
         Other;
            S4401C(xmlFlowName + '$');
         Endsl;

       //** beg add ***** 03/02/22 **********************
       //*** Begin Chg *** 09/26/25 **************************************
       //*** Exsr SrDartsCSV;
       If qqAgt# <> '00000' And PgmInStack('W3032R');
          Exsr SrDartsCSV;
       EndIf;
       //*** End   Chg *** 09/26/25 **************************************
       //** end add ***** 03/02/22 **********************

      /End-free
     C                   SETON                                        LR
      *
      *===============================================================
      *  Process Fund
      *===============================================================
     C     PRCFND        BEGSR
      *
     C*  READ AGENT COMMISSION FILE
     C*
     C   37ACKY4         CHAIN     PAC01                              60
     C  N37ACKY2A        CHAIN     PAC01                              60
     C                   If        *in60
     C                   Eval      isNoRcdsGrp = *off
     C                   Endif
     C*
     C     *IN60         DOWEQ     *OFF
     C                   If        isCalcBal
     C     XXSVA#        IFNE      ACAGT#
     C                   EXSR      CALBAL
     C                   ENDIF
     C                   ENDIF
     C*
     C*   ONLY CALCULATE/PRINT DETAIL IF:
     C*       - USER REQUESTED  'PRINT IF 0 BAL' = 'Y'          (*IN36)
     C*    OR - USED REQUESTED  'PRINT IF 0 BAL' = 'N'
     C*         AND  BAL IS NOT 0
     C*
     C     *IN36         IFEQ      '1'
     C     *IN36         OREQ      '0'
     C     XXBAL         ANDNE     0
     C*
     C*   CLEAR EARNED TO DATE ARRAY FOR AGENT
     C*
     C                   Z-ADD     0             YR
     C                   Z-ADD     0             AMT
     C                   EXSR      ANAME
     C                   EXSR      ADDSR
     C                   EXSR      CHKSTS
     C*
     C*  Check output type
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C                   EXCEPT    HEAD1
     C                   Except    HEAD2a
     C                   EXCEPT    HEAD2
     C                   ENDIF
     C                   EXSR      CLCPBL
     C*
     C*   PRINT PREV. BAL. LINE AND ADD IT AGENT TOTAL
     C*
     C* HOLD PREVIOUS BALANCE FOR WORKFILE
     C*
     C                   Z-ADD     XXPRBL        X1PRBL            9 2
     C*
       // Add a condition to always produce a file if group set for CSV file
     C*  Check output type
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C                   EXCEPT    PBAL
     C*
     C                   ENDIF
     C                   ADD       XXPRBL        XXAAMT
     C                   Z-ADD     0             XXPRBL
     C*
     C*Check whether record exist in Agt/Grp last stmt file.
     C     LSKY          CHAIN     PLS01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVEL     XXCO#         LSCO#
     C                   MOVEL     XXFND         LSFND
     C                   MOVEL     ACAGT#        LSAGT#
     C                   MOVEL     XXFRDT        LSFRDT
     C                   MOVEL     XXTODT        LSTODT
     C                   MOVE      QQZBF         LSZBF
     C                   MOVE      QQTYPO        LSTYPO
       //*** beg add ***** 03/02/22 ******************
       //*** Begin chg *** 09/26/25 ******************
       //*** If LStypo = 'P' and EXacsrCsv = 'Y';
       If (LStypo = 'P' or LStypo ='X') and EXacsrCsv = 'Y';
       //*** End   chg *** 09/26/25 ******************
          LStypo = 'B';
       Endif;
       //*** end add ***** 03/02/22 ******************
     C                   WRITE     PLS01
     C                   ELSE
     C                   MOVEL     XXFRDT        LSFRDT
     C                   MOVEL     XXTODT        LSTODT
     C                   MOVE      QQZBF         LSZBF
     C                   MOVE      QQTYPO        LSTYPO
       //*** beg add ***** 03/02/22 ******************
       //*** Begin chg *** 09/26/25 ******************
       //*** If LStypo = 'P' and EXacsrCsv = 'Y';
       If (LStypo = 'P' or LStypo ='X') and EXacsrCsv = 'Y';
       //*** End   chg *** 09/26/25 ******************
          LStypo = 'B';
       Endif;
       //*** end add ***** 03/02/22 ******************
     C                   UPDATE    PLS01
     C                   ENDIF
     C                   EXSR      PRDBRK
     C*
     C                   EXSR      AGTBRK
     C*
     C                   ENDIF
     C*
     C   37              EXSR      NXTAGT
     C  N37              SETON                                        60
     C                   END
     C*
     C                   Eval      *in99 = x2FCol <> *zero or x3FCol <> *zero
     C*
     C*  Check output type
     C*
      /Free
      /End-free
       //***  Begin Add  ***  09/23/21  ***********************************
      /Free
       Exsr WriteStmt1;
      /End-free
       //***  End   Add  ***  09/23/21  ***********************************
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
      /Free
       //***  Begin Del  ***  09/23/21  ***********************************
       //***Exsr WriteStmt1;
      /End-free
       //***  End   Del  ***  09/23/21  ***********************************
     C     *IN37         IFEQ      *ON
     C*
     C                   EXCEPT    HEAD1
     C                   Select
     C                   When      isNoRcdsGrp
     C                   Except    NOFNDTOT
     C                   Other
     C                   Except    HeadTot
     C                   If        isPrtSplitGrp
     C                   Except    TOTGRPC
     C                   Except    TOTGRPW
     C                   Endif
     C                   Except    TOTGRP
     C                   Endsl
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
      *
      *
     C*===============================================================
     C*    CALBAL - CALCULATE AGENT BALANCE
     C*===============================================================
     C     CALBAL        BEGSR
     C*
     C                   Z-ADD     ACAGT#        XXSVA#
     C                   Z-ADD     0             XXBAL             9 2
      /Free
       Clear aryProdDtl;
       isNoRcdsAgt = *on;
      /End-free
     C*
     C     ACKY2         CHAIN     PAC02                              50
     C*
     C     *IN50         DOWEQ     '0'
     C     ACPODT        ANDLE     XXTODT
     C*
      /Free
       // Check for producer detail within the period
       If ACpoDt >= xxFrDt and ACprod <> *zero;
          aryProdDtl(ACprod) = *on;
       Endif;
       // Check for agent detail within the period
       isNoRcdsAgt = not (ACpoDt >= xxFrDt);
      /End-free
     C*
     C     ACCOTP        IFEQ      'C'
     C     ACCOTP        OREQ      'A'
     C     ACCOTP        OREQ      'V'
     C                   ADD       ACAMT         XXBAL             9 2
     C                   ELSE
     C     ACCOTP        IFEQ      'P'
     C                   SUB       ACAMT         XXBAL
     C                   ENDIF
     C                   ENDIF
     C*
     C  N50ACKY2         READE     PAC02                                  50
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C*   NXTAGT - GET NEXT AGENT
     C*================================================================
     CSR   NXTAGT        BEGSR
     C*
     C     *IN60         DOUEQ     *ON
     C*
     C                   Z-ADD     0             PAGE
     C     ACKY2         SETGT     PAC01
     C     ACKY4         READE     PAC01                                  60
     C*
     C                   If        isCalcBal
     C     XXSVA#        IFNE      ACAGT#
     C                   EXSR      CALBAL
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXZBF         IFEQ      'Y'
     C     XXBAL         ORNE      *ZERO
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C*   AGTBRK AGENT BREAK
     C*================================================================
     CSR   AGTBRK        BEGSR
     C*
     C*   PRINT AGENT TOTALS AND ACCUMULATE FUND TOTALS
     C*
     C                   ADD       XXAANN        XXFANN
     C                   ADD       XXACOL        XXFCOL
     C                   SUB       X2ACOL        XXFCOL
     C                   ADD       X2ACOL        X2FCOL
     C                   SUB       X3ACOL        XXFCOL
     C                   ADD       X3ACOL        X3FCOL
     C                   ADD       XXAAMT        XXFAMT
      /Free
       xxFAnnC += xxAAnnC;
       xxFColC += xxAColC;
       xxFAmtC += xxAAmtC;
       xxFAnnW += xxAAnnW;
       xxFColW += xxAColW;
       xxFAmtW += xxAAmtW;
      /End-free
     C*
     C*  Check producer flag before going to workfile
     C*
     C     PRDFL         IFEQ      *OFF
     C     QQTYPO        IFEQ      'F'
     C     QQTYPO        OREQ      'B'
     C                   EXSR      WRKFIL
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Check output type
     C*
      /Free
       Exsr WriteStmt2;
      /End-free
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
      /Free
       If isAgtTotBrk;
          *inoa = *on;
       Else;
          Except Line1;
       Endif;
      /End-free
     C                   EXSR      PRTHDS
     C                   If        isPrtSplitAgt
     C                   Except    TotAgtC
     C                   Except    TotAgtW
     C                   Endif
     C                   Except    TotAgt
     C*
     C                   ENDIF
     C                   MOVE      *OFF          PRDFL
     C                   Z-ADD     0             XXAANN
     C                   Z-ADD     0             XXACOL
     C                   Z-ADD     *ZEROS        X2ACOL
     C                   Z-ADD     *ZEROS        X3ACOL
     C                   Z-ADD     0             XXAAMT
      /Free
       Clear xxAAnnC;
       Clear xxAColC;
       Clear xxAAmtC;
       Clear xxAAnnW;
       Clear xxAColW;
       Clear xxAAmtW;
       isPrtSplitAgt = *off;
      /End-free
     C*
     C*  SORT ARRAY THAT ACCUMULATES AGENT EARNED TO DATE FOR EACH FYR
     C*
     C                   MOVE      '0'           *IN88
     C                   MOVE      *BLANKS       SRT
     C                   Z-ADD     1             X
     C     YR(X)         DOWNE     0
     C                   MOVEL     YR(X)         SRT(X)
     C                   MOVE      AMT(X)        SRT(X)
     C                   ADD       1             X
     C                   END
     C                   SORTA     SRT
     C*
     C*  PRINT FUND YEAR TOTAL FOR AGENT EARNED TO DATE.
     C*
     C                   Z-ADD     1             X
     C     *BLANK        LOOKUP    SRT(X)                             60
     C     *IN60         IFEQ      '1'
     C*
     C     X             DOWNE     1000
     C*
     C                   EXSR      PRTHDS
     C                   MOVEL     SRT(X)        XXWK30            3 0
     C*  IF THERE WAS A NEGATIVE AMOUNT IN THE SRT ARRAY FOR A FYR,
     C*  THE FYR MUST BE FLIPPED BACK TO A POSITIVE NUMBER BECAUSE
     C*  THE SRT ARRAY COMBINES BOTH NUMBERS.
     C     XXWK30        IFLT      0
     C                   MULT      -1            XXWK30
     C                   END
     C*
     C                   Z-ADD     XXWK30        XXYEAR            3 0
     C*
     C                   MOVE      SRT(X)        XXAMT            11 2
     C*
     C*  Check output type
     C*
      /Free
       Exsr WriteStmt6;
      /End-free
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C                   EXCEPT    AGTEA
     C*
     C                   ENDIF
     C                   ADD       1             X
     C  N88              MOVE      '1'           *IN88
     C*
     C                   END
     C*
     C                   END
     C*
     C                   Clear                   savePrtProd
     C*
     C                   ENDSR
     C*================================================================
     C*   PRTHDS PRINT HEADINGS
     C*================================================================
     CSR   PRTHDS        BEGSR
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   Except    HEAD2a
     C                   Except    HeadTot
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C*   PRTPHD PRINT PRODUCER HEADINGS
     C*================================================================
     CSR   PRTPHD        BEGSR
     C*
     C     *INOA         IFEQ      '1'
       //***  Begin Add  ***  09/23/21  ********************
     C                   If        qqTypO = 'P' or qqTypO = 'B'
       //***  End   Add  ***  09/23/21  ********************
     C                   EXCEPT    HEAD1
       //***  Begin Add  ***  09/23/21  ********************
     C                   Endif
       //***  End   Add  ***  09/23/21  ********************
     C                   Exsr      GetProdHead
       //***  Begin Add  ***  09/23/21  ********************
     C                   If        qqTypO = 'P' or qqTypO = 'B'
       //***  End   Add  ***  09/23/21  ********************
     C                   Except    HEAD2a
     C                   EXCEPT    HEAD2
       //***  Begin Add  ***  09/23/21  ********************
     C                   Endif
       //***  End   Add  ***  09/23/21  ********************
      /Free
       If ACprod <> 0
        and aryProdDtl(ACprod);
      /End-free
       //***  Begin Add  ***  09/23/21  ********************
     C                   If        qqTypO = 'P' or qqTypO = 'B'
       //***  End   Add  ***  09/23/21  ********************
     C                   EXCEPT    HEAD3
       //***  Begin Add  ***  09/23/21  ********************
     C                   Endif
       //***  End   Add  ***  09/23/21  ********************
     C                   Eval      savePrtProd = ACprod
     C                   END
     C                   MOVE      '0'           *INOA
     C                   ELSE
     C                   If        savePrtProd <> ACprod and ACprod <> *zero
     C                              and aryProdDtl(ACprod)
     C                   Exsr      GetProdHead
       //***  Begin Add  ***  09/23/21  ********************
     C                   If        qqTypO = 'P' or qqTypO = 'B'
       //***  End   Add  ***  09/23/21  ********************
     C                   Except    Head3
       //***  Begin Add  ***  09/23/21  ********************
     C                   Endif
       //***  End   Add  ***  09/23/21  ********************
     C                   Eval      savePrtProd = ACprod
     C                   Endif
     C                   END
     C*
     C                   ENDSR
     C*
      /Free
       //===============================================================
       // Get producer heading info
       //===============================================================
       Begsr GetProdHead;
        xxPNme = GetProdNm(AAfein:AAmod:ACprod:rrn:xxCo#);
        // Update only needed for clearing record format
        If 1 = 0;
           Update pap0p;
        Endif;
        Clear pap0p;
        Chain rrn WMAPP;
       Endsr;
      /End-Free
     C*
     C*================================================================
     C*   PRDBRK PRODUCER BREAK
     C*================================================================
     CSR   PRDBRK        BEGSR
     C*
     C*   GET THE AGENT PRODUCER FILE AND READ ALL PRODUCERS  *
     C*
     C                   Z-ADD     0             ACPROD
     C                   MOVE      *BLANKS       XXPNME           40
     C     ACKY3         CHAIN     PAC01                              61
     C     *IN61         IFEQ      '1'
     C     ACKY3         SETGT     PAC01
     C     ACKY2         READE     PAC01                                  61
     C                   END
     C*
     C                   If        not *in61
     C                   Eval      saveACplan = ACplan
     C                   Eval      isNoRcdsProd = *on
     C                   Endif
     C*
     C     *IN61         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXPNME           40
     C     ACPROD        IFNE      0
     C                   Exsr      GetProdHead
     C                   If        xxPNme <> *blanks
     C                   ELSE
     C                   GOTO      NEXTP
     C                   END
     C*
     C*
     C*  Check output type
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C                   Exsr      PrtPHd
     C*
     C                   ENDIF
     C                   END
     C*
     C*********************************************************
     C*               *** EMPLOYER BREAK ***                  *
     C*   READ EACH EMPLOYER FOR PRODUCER                     *
     C*********************************************************
     C                   MOVE      '0'           *IN58
     C     *IN58         DOWEQ     '0'
     C*
     C* CLEAR EARNED TO DATE FOR NEXT FYR UNDER AN EMPLOYER
     C* MOVE "Y" TO XXINIT TO INDICATE IF EMPLOYER HEADING S/B PRINTED
     C* MOVE "N" TO XXPRNT TO INDICATE IF EARNED TO DATE FIELD S/B PRINTED
     C*
     C                   Z-ADD     0             XXCPTD            9 2
     C                   MOVE      'Y'           XXINIT            1
     C                   MOVE      'N'           XXPRNT            1
     C                   MOVE      ACFYR         XXFYR
     C                   MOVE      '0'           *IN62
     C*
     C     *IN62         DOWEQ     '0'
     C*****************************************************************
     C*                    *** FUND YEAR BREAK ***
     C*  PRINT THE COMMISSION EARNED TO DATE FOR EACH FYR ONLY IF DATA
     C*  WAS PRINTED ON STATEMENT FOR AN EMP.  REGARDLESS OF WHETHER WE
     C*  PRINTED DETAIL RECORDS FOR AN EMP, WE NEED TO ACCUMULATE THE
     C*  THE AMOUNT FOR THE FYR IN THE CORRESPONDING ARRAY IN ORDER TO
     C*  GET CORRECT EARNED TO DATE TOTALS FOR THE AGENT.
     C*****************************************************************
     C     ACFYR         IFNE      XXFYR
     C*
     C                   Z-ADD     1             X                 4 0
     C     XXFYR         LOOKUP    YR(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *ZERO         LOOKUP    YR(X)                                  60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     XXFYR         YR(X)
     C                   END
     C                   END
     C                   ADD       XXCPTD        AMT(X)
     C*
     C     XXPRNT        IFEQ      'Y'
     C     ACEMP#        ANDNE     0
     C*
     C*  Check output type
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C                   EXCEPT    EARNED
     C*
     C                   ENDIF
     C                   MOVE      'N'           XXPRNT
     C                   END
     C*
     C                   Z-ADD     0             XXCPTD
     C*
     C                   END
     C*
     C*  ACCUMULATE PREVIOUS COMMISSION EARNED DURING FUND YEAR.
     C*
     C     ACPODT        IFLT      XXFRDT
     C     ACCOTP        IFEQ      'C'
     C     ACCOTP        OREQ      'A'
     C                   ADD       ACAMT         XXCPTD
     C                   END
     C*
     C                   ELSE
     C*
     C*  PRINT DETAIL LINE FOR EACH TRANSACTION WITHIN ENTERED DATES &
     C*  INCREASE COMMISSION EARNED TO DATE
     C*
     C     ACPODT        IFLE      XXTODT
     C                   MOVE      'Y'           XXPRNT
     C     ACCOTP        IFEQ      'C'
     C     ACCOTP        OREQ      'A'
     C                   ADD       ACAMT         XXCPTD                   61
     C                   END
     C*
     C*   INITIAL CHAIN TO EMP DETAIL AND MASTER FOR EMP AND DIV -
     C*   GET THE ANNUAL PREMIUM FOR THE FYR THAT WAS KEYED. IF THE EMP
     C*   HAS BEEN CANCELLED FOR THAT FYR, THEN USE THE PREVIOUS FYR.
     C*
     C                   Z-ADD     0             XXPREM
     C*
     C     XXINIT        IFEQ      'Y'
     C     ACEMP#        ANDNE     0

      /Free
       // Get premium from last history record prior to the ending run date
        Clear EHwAPr;
        Setgt (ACco#:ACfnd:ACemp#:ACdiv:ACfyr:xxToDt) wdehl6;
        Readpe (ACco#:ACfnd:ACemp#:ACdiv:ACfyr) wdehl6;
        xxPrem = EHwAPr;
        xxPrtPremYr = ACfyr;
        xxHoldPrem = xxPrem;
      /End-free
     C*
     C                   MOVE      *BLANKS       ELRTCD
     C     ELKEY         SETGT     PEL01
     C     EMKY          READPE    PEL01                                  50
     C*** Begin Add *** 03/06/24 *******************************************
     C                   If        *in50 = *On
     C     ELKy2         Chain     PEL00                              50
     C                   If        *in50 = *Off
     C                   Eval      ELAAPR = E1AAPR
     C                   Eval      ELANPR = E1ANPR
     C                   Eval      ELFYR  = E1FYR
     C                   Eval      ELPFDT = E1PFDT
     C                   EndIf
     C                   EndIf
     C*** End   Add *** 03/06/24 *******************************************
     C*
     C     *IN50         IFEQ      *OFF
     C     ELPTDT        ANDGE     XXFRDT
     C*
     C     AHKY          SETGT     PAH01
     C     AHKY1         READPE    PAH01                                  51
     C     *IN51         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C     AHKY1         READPE    PAH01                                  51
     C                   END
     C*
     C     *IN51         IFEQ      *OFF
     C     AHAGT#        ANDEQ     ACAGT#
     C     AHPROD        ANDEQ     ACPROD
     C*
      /Free
       // Get premium from last history record prior to the ending run date
        Clear EHwAPr;
        Setgt (ELco#:ELfnd:ELemp#:ELdiv:ELfyr:xxToDt) wdehl6;
        Readpe (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) wdehl6;
        xxPrem = EHwAPr;
        xxPrtPremYr = ELfyr;
        xxHoldPrem = xxPrem;
      /End-free
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      ELRTCD        XXRTCD            3
     C     RSKY          CHAIN     PRS02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXJRST            2
     C                   ELSE
     C                   MOVE      *BLANKS       XXJRST
     C                   ENDIF
     C*
     C     EMKY          CHAIN     PEM01                              50
     C     *IN50         IFEQ      '1'
     C                   MOVE      *BLANKS       EMNAME
     C                   END
     C*
     C*  Check output type
     C*
      /Free
       Exsr WriteStmt4;
      /End-free
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C                   EXSR      PRTPHD
     C*
      /Free
      /End-free
     C*
     C                   EXCEPT    DET1
     C                   ENDIF
     C                   MOVE      'N'           XXINIT
     C*
     C                   END
     C*
     C*  PRINT DETAIL LINE OF EACH TRANSACTION FOR THAT EMP & DIV
     C*
     C     *INOA         IFEQ      '1'
     C*
     C*  Check output type
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C                   EXCEPT    HEAD1
     C                   Except    HEAD2a
     C                   EXCEPT    HEAD2
     C     ACPROD        IFNE      0
     C                   Exsr      GetProdHead
     C                   EXCEPT    HEAD3
     C                   Eval      savePrtProd = ACprod
     C                   END
     C     ACEMP#        IFNE      0
     C                   EXCEPT    DET1
     C                   END
     C                   MOVE      '0'           *INOA
     C                   END
     C                   ENDIF
     C*
     C     ACCOTP        IFEQ      'P'
     C                   MULT      -1            ACAMT
     C                   END
     C*
     C*  Get Carrier policy #
     C*
     C                   CLEAR                   RPCAP#
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     RPCAP#        XXCAP#           16
     C                   ELSE
     C                   MOVE      *BLANKS       XXCAP#
     C                   ENDIF
      *
     C     XXCAP#        IFEQ      *BLANKS
     C     ACEMP#        ANDNE     *ZEROS
     C                   MOVEL     DSCAP#        XXCAP#
     C                   ENDIF
     C*
     C*  If printing to workfile turn on employer and producer flags
     C*
     C     QQTYPO        IFEQ      'F'
     C     QQTYPO        OREQ      'B'
     C                   MOVE      *ON           EMPFL
     C                   MOVE      *ON           PRDFL
     C                   EXSR      WRKFIL
     C                   ENDIF
     C*
     C*  Check output type
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
      *
     C     CLTID         IFEQ      '071'
      *
      * IF CM Code = 'TA', and Current fund year, chg description
     C     ACFYR         IFEQ      XXCFYR
     C     ACCOCD        ANDEQ     'TA'
     C                   MOVEL(P)  TACUDS        ACDESC
     C                   ENDIF
      * IF CM Code = 'TA', and Prior fund year, chg description
     C     ACFYR         IFEQ      XXPFYR
     C     ACCOCD        ANDEQ     'TA'
     C                   MOVEL(P)  TAPRDS        ACDESC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
      /Free
       //***  Begin Add  ***  09/23/21  ***************************
       Endif;
       //***  End   Add  ***  09/23/21  ***************************
        Select;
        When ACagt2 = 'Y';
           xx2nd3rd = '*';
        When ACagt3 = 'Y';
           xx2nd3rd = '^';
        Other;
           xx2nd3rd = ' ';
        Endsl;

        xxPlanDesc = GetPlanTypeDesc(ACplan);

        Clear xxPrtPrem;
       If saveACemp# <> ACemp# or saveACdiv <> ACdiv;
          Clear aryEmpPremYr;
          Clear aryEmpPrem;
          Clear aryEmpPremPrt;
          saveACemp# = ACemp#;
          saveACdiv  = ACdiv;
       Endif;
       Clear xxPrem;
       Clear xxHoldPrem;
       x = %lookup(ACfyr:aryEmpPremYr:1);
       If x = 0;
          x = %lookup(*zero:aryEmpPremYr:1);
          aryEmpPremYr(x) = ACfyr;
          // Get premium from last history record prior to the ending run date
          Clear EHwAPr;
          Setgt (ACco#:ACfnd:ACemp#:ACdiv:ACfyr:xxToDt) wdehl6;
          Readpe (ACco#:ACfnd:ACemp#:ACdiv:ACfyr) wdehl6;
          aryEmpPrem(x) = EHwAPr;
          aryEmpPremPrt(x) = *off;
       Endif;
       If x > 0;
          xxPrem = aryEmpPrem(x);
          Select;
          When not aryEmpPremPrt(x);
             aryEmpPremPrt(x) = *on;
             xxHoldPrem = xxPrem;
          Endsl;
       Endif;
       xxPrtPrem = xxPrem;

       xxPAnn += xxHoldPrem;
       Select;
       When ACplan = 'W';
          xxPAnnW += xxHoldPrem;
       Other;
          xxPAnnC += xxHoldPrem;
       Endsl;

        If ACplan <> saveACplan;
           isPrtSplitProd = *on;
           isPrtSplitAgt  = *on;
           isPrtSplitGrp  = *on;
        Endif;
        isNoRcdsProd = *off;
      /End-free
     C*
       //***  Begin Add  ***  09/23/21  ***************************
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
       //***  End   Add  ***  09/23/21  ***************************
     C     ACEMP#        IFEQ      0
     C                   EXCEPT    DET
     C                   ELSE
     C                   EXCEPT    DET2
     C                   END
     C                   ENDIF
      /Free
       Exsr WriteStmt5;
      /End-free
     C*
     C*   ACCUMULATE PRODUCER TOTALS
     C*
     C                   ADD       ACTOCL        XXPCOL
      /Free
       Select;
       When ACplan = 'W';
          xxPColW += ACtoCl;
       Other;
          xxPColC += ACtoCl;
       Endsl;
      /End-free
     C*
     C     ACAGT2        IFEQ      'Y'
     C                   ADD       ACTOCL        X2PCOL
     C                   ENDIF
     C     ACAGT3        IFEQ      'Y'
     C                   ADD       ACTOCL        X3PCOL
     C                   ENDIF
     C*
     C                   ADD       ACAMT         XXPAMT
      /Free
       Select;
       When ACplan = 'W';
          xxPAmtW += ACamt;
       Other;
          xxPAmtC += ACamt;
       Endsl;
      /End-free
     C                   END
     C                   END
     C                   MOVE      ACFYR         XXFYR
     C     ACKY          READE     PAC01                                  62
     C                   END
     C*
     C*  WE HAVE NOW HIT A DIFFERENT EMPLOYER WITHIN THE SAME PRODUCER,
     C*  WE NEED TO ACCUMULATE EARNED TO DATE AMOUNT IN THE AMT ARRAY
     C*  FOR THE FYR WE JUST CALCULATED - REGARDLESS OF WHETHER ANY
     C*  DETAIL LINES WERE PRINTED. IF DETAIL LINES WERE PRINTED, THEN
     C*  WE WANT TO PRINT EARNED TO DATE AMOUNT.
     C*
     C                   Z-ADD     1             X                 4 0
     C     XXFYR         LOOKUP    YR(X)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *ZERO         LOOKUP    YR(X)                                  60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     XXFYR         YR(X)
     C                   END
     C                   END
     C                   ADD       XXCPTD        AMT(X)
     C*
     C*  Check output type
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C*
     C     XXPRNT        IFEQ      'Y'
     C     ACEMP#        ANDNE     0
     C                   EXCEPT    EARNED
     C                   END
     C                   ENDIF
     C*
     C*   GET NEXT EMPLOYER
     C*
     C     ACKY          SETGT     PAC01
     C     ACKY3         READE     PAC01                                  58
     C                   END
     C*
      /Free
       isBrkProd = *off;
       If %eof or (ACprod <> saveACprod);
          isBrkProd = *on;
       Endif;
       If ACplan <> saveACplan;
          isPrtSplitProd = *on;
          isPrtSplitAgt  = *on;
          isPrtSplitGrp  = *on;
       Endif;

       saveACprod = ACprod;

       If isBrkProd;
      /End-free
     C*
     C*   WE HAVE NOW HIT A PRODUCER BREAK.
     C*   PRINT PRODUCER TOTALS AND ACCUMULATE AGENT TOTALS
     C*
     C                   ADD       XXPANN        XXAANN
     C                   ADD       XXPCOL        XXACOL
     C                   ADD       X2PCOL        X2ACOL
     C                   ADD       X3PCOL        X3ACOL
     C                   ADD       XXPAMT        XXAAMT
      /Free
       xxAAnnC += xxPAnnC;
       xxAColC += xxPColC;
       xxAAmtC += xxPAmtC;
       xxAAnnW += xxPAnnW;
       xxAColW += xxPColW;
       xxAAmtW += xxPAmtW;
      /End-free
     C                   EXSR      PRTPHD
     C*
     C     ACEMP#        IFNE      0
     C     ACPROD        ANDNE     0
     C*
     C*  IF NO DETAIL LINES WERE PRINTED
     C*
     C     EMPFL         IFEQ      *OFF
     C     QQTYPO        IFEQ      'F'
     C     QQTYPO        OREQ      'B'
     C                   MOVE      *ON           PRDFL
     C                   EXSR      WRKFIL
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Check output type
     C*
      //***  Begin Del  ***  09/23/21  ********************
     C*****QQTYPO        IFEQ      'P'
     C*****QQTYPO        OREQ      'B'
      //***  End   Del  ***  09/23/21  ********************
     C*
     C                   If        not isNoRcdsProd
     C                              and aryProdDtl(ACprod)
      /Free
       Exsr WriteStmt3;
      /End-free
      //***  Begin Add  ***  09/23/21  ********************
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
      //***  End   Add  ***  09/23/21  ********************
     C                   Except    TotProd1
     C                   If        isPrtSplitProd
     C                   Except    TotProdC
     C                   Except    TotProdW
     C                   Endif
     C                   Except    TotProd2
     C                   Endif
     C     CLTID         IFEQ      '102'
     C                   MOVE      *ON           *INOA
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   END
     C                   Z-ADD     0             XXPANN
     C                   Z-ADD     0             XXPCOL
     C                   Z-ADD     *ZEROS        X2PCOL
     C                   Z-ADD     *ZEROS        X3PCOL
     C                   Z-ADD     0             XXPAMT
      /Free
       Clear xxPAnnC;
       Clear xxPColC;
       Clear xxPAmtC;
       Clear xxPAnnW;
       Clear xxPColW;
       Clear xxPAmtW;
       Clear aryEmpPremYr;
       Clear aryEmpPrem;
       Clear aryEmpPremPrt;
       isPrtSplitProd = *off;
      /End-free
     C*
     C*  Initialize employer flag before getting next producer
     C*
     C                   MOVE      *OFF          EMPFL
      /Free
          isBrkProd = *off;
       Endif;
      /End-free
     C     NEXTP         TAG
     C*
     C*   GET NEXT PRODUCER
     C*
     C     ACKY3         SETGT     PAC01
     C     ACKY2         READE     PAC01                                  61
     C*
     C                   If        not *in61
     C                   Eval      isNoRcdsProd = *on
     C                   Endif
     C*
     C                   END
     C                   ENDSR
     C*================================================================
     C*   CLCPBL - CALCULATE PREVIOUS BALANCE
     C*================================================================
     CSR   CLCPBL        BEGSR
     C*
     C     ACKY2         CHAIN     PAC02                              50
     C     *IN50         DOWEQ     '0'
     C*
     C     ACPODT        IFLT      XXFRDT
     C*
     C     *IN35         IFEQ      *ON
     C     ACCOTP        IFEQ      'C'
     C     ACCOTP        OREQ      'A'
     C     ACCOTP        OREQ      'V'
     C                   ADD       ACAMT         XXPRBL            9 2
     C                   ELSE
     C     ACCOTP        IFEQ      'P'
     C                   SUB       ACAMT         XXPRBL
     C                   END
     C                   END
     C                   ELSE
     C*
     C     ACEMP#        IFEQ      XXEMP#
     C     ACDIV         ANDEQ     XXDIV
     C*
     C     ACCOTP        IFEQ      'C'
     C     ACCOTP        OREQ      'A'
     C     ACCOTP        OREQ      'V'
     C                   ADD       ACAMT         XXPRBL            9 2
     C                   ELSE
     C     ACCOTP        IFEQ      'P'
     C                   SUB       ACAMT         XXPRBL
     C                   END
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      '1'           *IN50
     C*
     C                   END
     C  N50ACKY2         READE     PAC02                                  50
     C                   END
     C                   ENDSR
     C*================================================================
     C*   CHKSTS - CHECK AGENT STATUS
     C*================================================================
     CSR   CHKSTS        BEGSR
     C*
     C* CHECK TO SEE IF AGENT IS INACTIVE.  IF INACTIVE, CHECK FOR
     C* ANY RECORDS IN COMMISSION FILE THAT ARE IN THE DATE RANGE
     C*
     C     AASTAT        DOWEQ     'I'
     C*
     C                   DO
     C*
     C                   Z-ADD     ACAGT#        XXSAGT            5 0
     C                   MOVE      '0'           *IN84
     C*
     C     ACKY7         SETLL     PAC02
     C     ACKY2         READE     PAC02                                  80
     C*
     C     *IN80         DOWEQ     '0'
     C*
     C     ACPODT        IFGE      XXFRDT
     C     ACPODT        ANDLE     XXTODT
     C*
     C     *IN35         IFEQ      *ON
     C*
     C                   MOVE      '1'           *IN84
     C*
     C                   ELSE
     C*
     C     ACEMP#        IFEQ      XXEMP#
     C     ACDIV         ANDEQ     XXDIV
     C                   MOVE      '1'           *IN84
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ACKY2         READE     PAC02                                  80
     C*
     C     ACPODT        IFGT      XXTODT
     C                   MOVE      '1'           *IN80
     C                   ENDIF
     C*
     C                   END
     C*
     C                   Z-ADD     XXSAGT        ACAGT#
     C*
     C     *IN84         IFEQ      '0'
     C                   EXSR      NXTAGT
     C                   MOVE      *IN60         LSTAGT
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   EXSR      ANAME
     C                   EXSR      ADDSR
     C*
     C     LSTAGT        IFEQ      *ON
     C     AASTAT        ANDEQ     'I'
     C     *IN84         OREQ      *ON
     C     AASTAT        ANDEQ     'I'
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C*   ADDSR - LOAD CITY STATE, AND ZIP FOR MAILING ADDRESS
     C*================================================================
     CSR   ADDSR         BEGSR
     C                   MOVE      *BLANKS       XXCSZ            40
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     AGMST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     AGMCTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C                   ENDSR
     C*================================================================
     C*   ANAME - GET THE AGENT NAME FROM MASTER FILE
     C*================================================================
     CSR   ANAME         BEGSR
     C     AAKY          CHAIN     PAA01                              61
     C     *IN61         IFEQ      '1'
     C                   Z-ADD     *ZEROS        AAFEIN
     C                   Z-ADD     *ZEROS        AAMOD
     C                   MOVE      *BLANKS       AASTAT
     C                   MOVE      *BLANKS       AAAGRD
     C                   END
     C                   Z-ADD     AAFEIN        XXFEIN            9 0
     C                   Z-ADD     AAMOD         XXMOD             3 0
     C*
     C     AGKY          CHAIN     PAG01                              61
     C     *IN61         IFEQ      '1'
     C                   MOVE      *BLANKS       AGNAME
     C                   MOVE      *BLANKS       AGMAD1
     C                   MOVE      *BLANKS       AGMST
     C                   MOVE      *BLANKS       AGMCTY
     C                   Z-ADD     0             AGMZIP
     C                   END
     C*** begin add ***  07/09/20  ******************************************
     C                   Eval      x@rrn = dbrrn
     C*** end add *****  07/09/20  ******************************************
     C                   ENDSR
     C*================================================================
     C*   INITIALIZATION ROUTINE
     C*================================================================
     CSR   INSR          BEGSR
     C*
      /Free

       xxProcessType = processType;

       isUseSpoolOut = UseSpoolOut(xxProcessType);
       isUseXML = UseScriptura(xxProcessType)
        or Use3rdXml(xxProcessType);

       If QQTYPo = 'F';
          isUseXml = *off;
       Endif;

       isMassReq = *off;
       massFlag = ' ';
       //*** begin add ***  12/11/25  ************************************
       If PgmInStack('W3032R');
          isMassReq = *On;
          massFlag = 'Y';
       Else;
          isMassReq = *Off;
          massFlag = ' ';
       EndIf;
       //*** end add *****  12/11/25  ************************************

       //* Create Scriptura request and setup files.

       Select;
       When qqTypO = 'P' and not isUseSpoolOut;
          qqTypO = 'X';
       When qqTypO = 'B' and not isUseSpoolOut;
          qqTypO = 'F';
       Endsl;
      /End-free
     C*
     C                   MOVEL     QQCO#         XXCO#
     C                   MOVEL     QQFND         XXFND
     C     XXFND         COMP      *ZERO                                  38
     C                   MOVEL     QQFDT         XXFDT             8 0
     C                   MOVEL     QQTDT         XXTDT             8 0
     C                   MOVEL     QQEMP#        XXEMP#
     C                   MOVEL     QQDIV         XXDIV
     C                   MOVEL     QQZBF         XXZBF
     C                   Z-ADD     0             XXSVA#
     C*
     C*  Initialize employer and producer flags
     C*
     C                   MOVE      *OFF          EMPFL
     C                   MOVE      *OFF          PRDFL
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQQUE         COMP      *BLANK                             080809
     C*
     C                   MOVE      QQCO#         RSCO#             3 0
     C                   MOVE      QQFND         RSFND             3 0
     C                   Z-ADD     XXFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     *ZEROS        XXAGT#
     C     QQAGT#        IFNE      *ZEROS
     C                   MOVE      QQAGT#        RSAGT#            5 0
     C                   MOVE      QQAGT#        XXAGT#
     C                   MOVE      *OFF          *IN37
     C                   ELSE
     C                   MOVE      '*ALL'        RSAGTA            5
     C                   MOVE      *ON           *IN37
     C                   END
     C     QQEMP#        IFNE      *ZEROS
     C                   MOVE      QQEMP#        RSEMP#            9 0
     C                   MOVE      QQDIV         RSDIV             5 0
     C                   MOVE      *OFF          *IN35
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   ELSE
     C                   MOVE      '*ALL'        RSEMPA            9
     C                   MOVE      *ON           *IN35
     C                   END
     C*
     C     XXZBF         IFEQ      'Y'
     C                   MOVE      *ON           *IN36
     C                   MOVEL     '*YES'        RSZBF             4
     C                   Eval      isCalcBal = *on
     C                   ELSE
     C                   MOVEL     '*NO'         RSZBF             4
     C                   ENDIF
     C*
     C* isPrtZeroProd is being set to always be *OFF until a project can
     C*  be done to all the user to select it as an option
     C                   If        not isPrtZeroProd
     C                   Eval      isCalcBal = *on
     C                   Endif
      *
      * RTVCLTID - Retrieve Client Number from Data Area
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3

      /Free
       // Break before agent summary page
       If cltId = '999';
          isAgtTotBrk = *on;
       Endif;
      /End-free

     C     CLTID         IFEQ      '071'
     C*
      * XXCFYR is the current fyr from XXTODT (Data Structure)
     C                   Z-ADD     XXTODT        XXCUDT
     C     XXCFYR        SUB       1             XXPFYR            3 0
      * XXCFYR is the current fyr from XXTODT
     C*
     C                   ENDIF
     C*
     C*  GET USER, COMPANY, AND GROUP INFORMATION
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     COKY          CHAIN     PCO01                              60
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Center Company name in display field
     C*
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
     C*
     C*  HOLD CONAME OR FNANAM FOR WORKFILE
     C*
     C                   MOVEL     ##DATA        XXCONM
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C                   ENDIF
        //*** Beg Add *** 03/02/22 *****************************************
        Chain (xxCo#:xxFnd) pex00;
        Exec Sql Set Option Commit = *None;
        //*** End Add *** 03/02/22 *****************************************

     C*
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C*  Left Justify Title in Field
     C*
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF
     C                   ENDIF
     C*
     C*  Check output type
     C*
     C     QQTYPO        IFEQ      'P'
     C     QQTYPO        OREQ      'B'
     C     QQCRCD        IFEQ      *BLANKS
      *
     C     ORDIS1        IFEQ      *BLANKS
     C                   MOVEL(P)  UPUSNM        ORDIS1
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      '0'           *INOA
     C*
     C*  CENTER TEXT IN FIELD
     C*
     C                   MOVEL     ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        ORTITL
     C*
     C*   INITIALIZE TOTAL FIELDS
     C*
     C                   Z-ADD     0             XXPANN           11 2
     C                   Z-ADD     0             XXPCOL
     C                   Z-ADD     *ZEROS        X2PCOL
     C                   Z-ADD     *ZEROS        X3PCOL
     C                   Z-ADD     0             XXPAMT
     C                   Z-ADD     0             XXAANN           11 2
     C                   Z-ADD     0             XXACOL
     C                   Z-ADD     *ZEROS        X2ACOL
     C                   Z-ADD     *ZEROS        X3ACOL
     C                   Z-ADD     0             XXAAMT
     C                   Z-ADD     0             XXFANN           11 2
     C                   Z-ADD     0             XXFCOL
     C                   Z-ADD     *ZEROS        X2FCOL
     C                   Z-ADD     *ZEROS        X3FCOL
     C                   Z-ADD     0             XXFAMT
       //*** Begin Chg *** 09/26/25 ****************************************
       //* Create Scriptura request and setup files.
       //*** Exsr ScriptXML;
       If isUseXml;
          Exsr ScriptXML;
       EndIf;
       //*** End   Chg *** 09/26/25 ****************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*   WRKFIL - SUBROUTINE TO WRITE RECORDS TO WORKFILE WFACP
     C*================================================================
     CSR   WRKFIL        BEGSR
     C*
     C                   CLEAR                   PAC03
     C*
     C*  If employer detail lines are being written all fields load
     C*
     C                   Z-ADD     ACCO#         A3CO#
     C                   MOVEL(P)  XXCONM        A3CONM
     C                   Z-ADD     ACFND         A3FND
     C                   MOVEL(P)  FNNAME        A3FNNM
     C                   Z-ADD     XXFDT         A3FDT
     C                   Z-ADD     XXTDT         A3TDT
     C                   Z-ADD     ACAGT#        A3AGT#
     C                   MOVEL(P)  AGNAME        A3AGNM
     C                   MOVEL(P)  AGMAD1        A3AGAD
     C                   MOVEL(P)  AGMCTY        A3AGCY
     C                   MOVEL(P)  AGMST         A3AGST
     C                   MOVEL     XXZP1         A3AGZ1            5 0
     C                   MOVEL     XXZP2         A3AGZ2            4 0
     C                   MOVEL(P)  AASTAT        A3STAT
     C                   MOVEL(P)  AAFEIN        A3FEIN
     C                   MOVEL(P)  AAMOD         A3MOD
     C                   Z-ADD     X1PRBL        A3PRBL
     C*
     C                   Z-ADD     ACPROD        A3PROD
     C     ACPROD        IFEQ      *ZEROS
     C                   MOVEL     *BLANKS       A3PRNM
     C                   ELSE
     C                   MOVEL(P)  XXPNME        A3PRNM
     C                   ENDIF
     C*
     C                   Z-ADD     ACEMP#        A3EMP#
     C     A3EMP#        IFEQ      *ZEROS
     C                   MOVEL(P)  *BLANKS       EMNAME
     C                   ELSE
     C                   MOVEL(P)  EMNAME        A3EMNM
     C                   ENDIF
     C*
     C                   Z-ADD     ACDIV         A3DIV
     C                   Z-ADD     ACFYR         A3FYR
     C     ELKY          CHAIN     PEL00                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     E1PFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A3PFDT
     C                   ELSE
     C                   MOVE      *ZEROS        A3PFDT
     C                   ENDIF
     C                   Z-ADD     XXPREM        A3EPPR           11 2
     C*
     C                   MOVEL(P)  ACCOCD        A3COCD
     C                   MOVEL(P)  *BLANKS       XXKEY
     C                   MOVEL(P)  ACCOCD        XXKEY
     C     TBKY          CHAIN     PCS01                              60
     C                   MOVEL(P)  CSDESC        A3CODS
      *
     C     CLTID         IFEQ      '071'
      *
      * IF CM Code = 'TA', and Current fund year, chg description
     C     ACFYR         IFEQ      XXCFYR
     C     ACCOCD        ANDEQ     'TA'
     C                   MOVEL(P)  TACUDS        A3CODS
     C                   ENDIF
      * IF CM Code = 'TA', and Prior fund year, chg description
     C     ACFYR         IFEQ      XXPFYR
     C     ACCOCD        ANDEQ     'TA'
     C                   MOVEL(P)  TAPRDS        A3CODS
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C                   MOVEL(P)  ACCOTP        A3COTP
     C                   MOVEL(P)  *BLANKS       XXKEY
     C                   MOVEL(P)  ACCOTP        XXKEY
     C     TBKY          CHAIN     PAT01                              60
     C                   MOVEL(P)  ATDESC        A3TYDS
     C*
     C                   Z-ADD     ACTOCL        A3TOCL
     C                   Z-ADD     ACRATE        A3RATE
     C                   Z-ADD     ACAMT         A3AMT
     C                   Z-ADD     XXCPTD        A3ETOD
     C*
     C                   Z-ADD     ACPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A3PSDT            8 0
     C*
     C                   Z-ADD     ACTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A3TNDT            8 0
     C*
     C                   Z-ADD     ACCHCK        A3CHCK
     C*
     C*  If no employers are written under a producer
     C*     Clear selected fields
     C*
     C     EMPFL         IFEQ      *OFF
     C*
     C                   Z-ADD     *ZERO         A3EMP#
     C                   MOVEL     *BLANKS       A3EMNM
     C                   Z-ADD     *ZERO         A3DIV
     C                   Z-ADD     *ZERO         A3FYR
     C                   Z-ADD     *ZERO         A3PFDT
     C                   Z-ADD     *ZERO         A3EPPR           11 2
     C*
     C                   MOVEL(P)  *BLANKS       A3COCD
     C                   MOVEL(P)  *BLANKS       A3CODS
     C*
     C                   MOVEL(P)  *BLANKS       A3COTP
     C                   MOVEL(P)  *BLANKS       A3TYDS
     C*
     C                   Z-ADD     *ZERO         A3TOCL
     C                   Z-ADD     *ZERO         A3RATE
     C                   Z-ADD     *ZERO         A3AMT
     C                   Z-ADD     *ZERO         A3ETOD
     C*
     C                   Z-ADD     *ZEROS        A3PSDT            8 0
     C*
     C                   Z-ADD     *ZERO         A3TNDT            8 0
     C                   Z-ADD     *ZERO         A3CHCK
     C*
     C                   ENDIF
     C*
     C*  If there are no producers or employers under an agent
     C*        clear selected fields
     C*
     C     EMPFL         IFEQ      *OFF
     C     PRDFL         ANDEQ     *OFF
     C*
     C                   Z-ADD     *ZEROS        A3PROD
     C                   MOVEL(P)  *BLANKS       A3PRNM
     C*
     C                   ENDIF
     C*
     C*  Load all maintenance fields
     C*
     C                   MOVEL(P)  Q1USER        A3USER
     C                   MOVEL(P)  Q1PGM         A3PGM
     C                   MOVEL(P)  DSTIME        A3TIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         A3DT
     C*
     C                   Z-ADD     EMRGN#        A3RGN#
     C     REKY          CHAIN     PRE01                              60
     C                   MOVEL     REDESC        A3RGDS
     C                   MOVEL     XXCAP#        A3CAP#
     C                   Z-ADD     ACCHCK        A3CHCK
     C                   Z-ADD     ACCKAM        A3CKAM
     C                   Z-ADD     ACCKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         A3CKDT
     C                   Z-ADD     ACSEQ#        A3SEQ#
     C                   MOVEL     XXJRST        A3JRST
     C*
     C                   WRITE     PAC03
     C*
     C                   ENDSR
       //*** beg add ***** 03/02/22 ******************
       //==============================================
       Begsr SrDartsCSV;
       //==============================================
       // Leave if not creating CSV.
        If EXacSrCsv <> 'Y';
           LeaveSr;
        Endif;

        //*** Beg Add ***** 01/16/24 ***************************
        Exsr CrtDartsCSV;
        //*** End Add ***** 01/16/24 ***************************

          // Attach CSV to DARTS Remark
           strDocId = #1DcId;
           AddDocToDarts(q#Key:
                         q#Seq#:
                         strDocId:
                         #1Fold:
                         'O':
                         'Statement Data':
                         q#RmCd:
                         'AS400':
                         q#pNam:
                         q#User:
                         chrRtCd:
                         strExists:
                         strPath:
                         'N');
       Endsr;
       //*** end add ***** 03/02/22 ******************
       //***** Beg Add *** 01/16/24 ********************
       //==============================================
       Begsr CrtDartsCSV;
       //==============================================
       // Get next available document ID and folder.
        #1RtCd = 'P';
        #1Pre = 'AL';
        #1co# = qqCo#;
        #1Fnd = qqFnd;
        GetNextDocID(
         #1Fold:
         #1DcID:
         #1Pre:
         #1Co#:
         #1Fnd:
         #1RtCd);
         #1DcID = %ScanRpl('.PDF':'.CSV':#1DcID);
        @@fein = %Editc(xxFein:'X');
        @@mod  = %Editc(xxMod:'X');
        strCommand =
         'CPYF FROMFILE(QTEMP/WFACP) ' +
         'TOFILE(QTEMP/WFACPCSV) ' +
         'MBROPT(*REPLACE) ' +
         'CRTFILE(*YES) ' +
         'INCREL((*IF ACFEIN *EQ ' + @@FEIN + ') ' +
         '(*AND ACMOD *EQ ' + @@Mod + ')) ';
        isWfacpData = 'Y';
        Monitor;
        RunCommand(%Trim(strCommand):%Len(%Trim(strCommand)));
        On-error;
           isWfacpData = 'N';
        Endmon;

        // If data was successfully copied from WFACP then create CSV file attac
        If isWfacpData = 'Y';

           // If no records in WFACP then indicate that in WFACPCSV
           intFein = %Dec(@@fein:9:0);
           intMod  = %Dec(@@mod:3:0);
           Exec Sql
            Select count(*) into :AgtCount from WFACP
             where ACfein = :intFein and ACmod = :intMod;
           If AgtCount = 0;
              Exec Sql insert into QTEMP/WFACPCSV (ACcoNm)
               Values('No Records Selected for this Agent');
           Endif;
           // Create another Doc ID field as .TXT since CPYTOIMPF does not accep
           strDcId2 = %ScanRpl('.CSV':'.TXT':#1Dcid);
           // Build the path name
           strDocRoot = *blanks;
           Callp RtvDocRoot(strDocRoot);
           If strDocRoot <> *blanks;
              strPath = '/' + %Trim(strDocRoot) + '/' + %Trim(#1Fold) +
               '/' + %Trim(strDcId2);
           Else;
              strPath = '/' + %Trim(#1Fold) + '/' + %Trim(strDcid2);
           Endif;
           strPath2 = %TrimR(strPath);

           Exsr SrCsvHeading;

           // Copy as .TXT file because CPYTOIMPF command does not work with .CS
           strCommand = 'CPYTOIMPF FROMFILE(QTEMP/WFACPX) TOSTMF(' +
            '''' + strPath2 + '''' + ')' +
            ' STMFCCSID(*PCASCII) RCDDLM(*CRLF) STRDLM(*DBLQUOTE)' +
            ' MBROPT(*ADD) STMFAUT(*INDIR)';
           RunCommand(%Trim(strCommand):%Len(%Trim(strCommand)));
           // Rename to .CSV file so it can be opened in Excel
           strDcid2 = #1Dcid;
           strCommand = 'RNM OBJ(' + '''' + strPath2 + '''' + ')' +
             ' NEWOBJ(' + '''' + strDcid2 + '''' + ')';
           RunCommand(%Trim(strCommand):%Len(%Trim(strCommand)));

        Endif;
       Endsr;

       //==============================================
       Begsr SrCsvHeading;
       //==============================================

        Data = 'GROUP,GROUP NAME,FROM DATE,TO DATE,AGENT # ASSIGNED BY FUND,'+
               'AGENT NAME,AGENT ADDRESS,AGENT CITY,STATE,ZIP1,ZIP2,'+
               'PREVIOUS BAL,PRODUCER NAME,EMPLOYER,EMPLOYER NAME,DIV,YEAR,'+
               'EST PRORATA PREM,COMM CODE DESC,TOTAL COLLECTIONS,RATE,'+
               'AMT THIS MONTH,EARNED TO DATE,CARRIER POLICY#,RATING STATE';

        //*** Beg Chg *** 11/06/24 *****************************************
        // creating a temporary flatfile with column headings
        // used in CPYTOIMPF
        //***Exec sql CREATE TABLE QTEMP/@OUTFILE
        //***                   (HEADINGS CHAR(5000)
        //***                   NOT NULL WITH DEFAULT) ;
        Exec sql CREATE OR REPLACE TABLE QTEMP/@OUTFILE
                           (HEADINGS CHAR(5000)
                           NOT NULL WITH DEFAULT) ;
        //*** End Chg *** 11/06/24 *****************************************
        //*** Beg Add *** 11/06/24 *****************************************
        strCommand = 'delete FROM QTEMP/@OUTFILE';
        Exec sql EXECUTE IMMEDIATE :StrCommand;
        //*** End Add *** 11/06/24 *****************************************

        strCommand = 'INSERT INTO QTEMP/@OUTFILE +
                        VALUES (''' + %trim(Data) + ''')' ;

        Exec sql EXECUTE IMMEDIATE :StrCommand;

        strCommand = 'CPYTOIMPF FROMFILE(QTEMP/@OUTFILE)'+
                     ' TOSTMF(''' + %trim(strPath2) + ''')'+
                     ' MBROPT(*REPLACE)'+
                     ' STMFCODPAG(*PCASCII)'+
                     ' RCDDLM(*CRLF) STRDLM(*NONE)' ;

        RunCommand(%Trim(strCommand):%Len(%Trim(strCommand)));

        //*** Beg Chg *** 11/06/24 *****************************************
        //***Exec sql CREATE TABLE QTEMP/WFACPX
        Exec sql CREATE OR REPLACE TABLE QTEMP/WFACPX
        //*** End Chg *** 11/06/24 *****************************************
                           (XACFND DEC (3,0),
                            XACFNNM CHAR (60),
                            XACFDT VARCHAR (10),
                            XACTDT VARCHAR (10),
                            XACAGT# DEC (5,0),
                            XACAGNM CHAR (40),
                            XACAGAD CHAR (40),
                            XACAGCY CHAR (40),
                            XACAGST CHAR (2),
                            XACAGZ1 DEC (5,0),
                            XACAGZ2 DEC (4,0),
                            XACPRBL DEC (9,2),
                            XACPRNM CHAR (40),
                            XACEMP# DEC (9,0),
                            XACEMNM CHAR (40),
                            XACDIV DEC (5,0),
                            XACFYR DEC (3,0),
                            XACEPPR DEC (11,2),
                            XACCODS CHAR (40),
                            XACTOCL DEC (11,2),
                            XACRATE DEC (5,2),
                            XACAMT  DEC (7,2),
                            XACETOD DEC (11,2),
                            XACCAP# CHAR (16),
                            XACJRST CHAR (2));
        //*** Beg Add *** 11/06/24 *****************************************
        strCommand = 'delete FROM QTEMP/WFACPX';
        Exec sql EXECUTE IMMEDIATE :StrCommand;
        //*** End Add *** 11/06/24 *****************************************

        Exec SQL INSERT INTO QTEMP/WFACPX (XACfnd,XACfnnm,XACfdt,XActdt,
                XACagt#,XACagnm,XACagad,XACagcy,XACagst,XACagz1,XACagz2,
                XACprbl,XACprnm,XACemp#,XACemnm,XACdiv,XACfyr,XACeppr,
                XACcods,XACtocl,XACrate,XACamt,XACetod,XACcap#,XACjrst)
         SELECT ACfnd,ACfnnm,VARCHAR_FORMAT(TO_DATE(LEFT(DIGITS(ACfdt),2) ||
                COALESCE(SUBSTRING(DIGITS(ACfdt),3,6),'00') ||
                COALESCE(SUBSTRING(DIGITS(ACfdt),9,4),'0000'), 'MMDDYYYY'),
                'MM/DD/YYYY'),
                VARCHAR_FORMAT(TO_DATE(LEFT(DIGITS(ACtdt),2) ||
                COALESCE(SUBSTRING(DIGITS(ACtdt),3,6),'00') ||
                COALESCE(SUBSTRING(DIGITS(ACtdt),9,4),'0000'), 'MMDDYYYY'),
                'MM/DD/YYYY'),ACagt#,ACagnm,ACagad,ACagcy,ACagst,
                ACagz1,ACagz2,ACprbl,ACprnm,ACemp#,ACemnm,ACdiv,ACfyr,ACeppr,
                ACcods,ACtocl,ACrate,ACamt,ACetod,ACcap#,ACjrst

         From QTEMP/WFACPCSV;

       Endsr;
       //***** End Add *** 01/16/24 ********************

     C     SRDRTS        BEGSR
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #1FOLD           80
     C                   PARM      *BLANKS       #1DCID           12
     C                   PARM      'AL'          #1PRE             2
     C                   PARM      QQCO#         #1CO#             3
     C                   PARM      QQFND         #1FND             3
     C                   PARM      'P'           #1RTCD            1
     C*Call program to resolve Agent commission stmt to .PDF
     C                   CALL      'SRESOLVE'
     C                   PARM      'QSYSPRT'     #1SPLF           10
     C                   PARM                    #1FOLD
     C                   PARM                    #1DCID
     C                   PARM      OROUTQ        #1OUTQ           10
     C*Call API to write a DARTS Remark
     C*
     C                   MOVEL     XXFEIN        @@FEIN            9
     C                   MOVEL     XXMOD         @@MOD             3
     C     @@FEIN        CAT       @@MOD         @@KEY            40
     C                   MOVEL     @@KEY         Q#KEY            40
     C                   Z-ADD     LSFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@FRDT            8
     C*
     C                   Z-ADD     LSTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@TODT            8
     C*
     C     RMK(1)        CAT(P)    QQFND:1       @@WKV1          256
     C     @@WKV1        CAT(P)    RMK(2):1      @@WKV1
     C*
     C     @@MTH         CAT       '/':0         @@DATE           10
     C     @@DATE        CAT       @@DAY:0       @@DATE
     C     @@DATE        CAT       '/':0         @@DATE
     C     @@DATE        CAT       @@FYR:0       @@DATE
     C*
     C     @@WKV1        CAT(P)    @@DATE:1      @@WKV1
     C     @@WKV1        CAT(P)    RMK(3):1      @@WKV1
     C*
     C                   MOVE      *BLANKS       @@DATE
     C     @@MTH1        CAT       '/':0         @@DATE
     C     @@DATE        CAT       @@DAY1:0      @@DATE
     C     @@DATE        CAT       '/':0         @@DATE
     C     @@DATE        CAT       @@FYR1:0      @@DATE
     C*
     C     @@WKV1        CAT(P)    @@DATE:1      @@WKV1
     C*
     C                   MOVE      @@WKV1        Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         Q#CDAT            8
     C                   MOVEL     #1DCID        Q#DCID           12
     C                   MOVEL     'W0040R'      Q#PNAM           10
     C                   MOVEL     *ZERO         Q#SECL            3
     C                   MOVEL     Q1USER        Q#USER           10
     C                   MOVEL     Q1PGM         Q#PGM            10
     C                   MOVEL     #1FOLD        Q#FOLD           80
     C                   MOVEL     *BLANKS       Q#DIFL            1
     C                   MOVEL     Q1PGM         Q#IPGM           10
     C                   MOVEL     *BLANKS       Q#FL01            1
     C                   MOVEL     *BLANKS       Q#FL02            1
     C                   MOVEL     QQCRCD        Q#RMCD            5
     C                   MOVEL     '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL     *BLANKS       Q#APID            2
     C                   MOVEL     *BLANKS       Q#IDOC           12
     C                   MOVEL     *BLANKS       Q#IQUE           10
     C                   MOVEL     'O'           Q#IMGF            1
     C                   MOVEL     *BLANKS       Q#IPTH           10
     C                   MOVE      *BLANKS       Q#PIDX            5
     C                   MOVEL     *BLANKS       Q#QPTY            1
     C                   MOVEL     *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C*
        //*** Beg Add *** 03/02/22 ********************************************
        // If Scriptura only then do not send in the docID and folder
        If Not isUseSpoolOut and isUseXml;
           Q#dcId = *blanks;
           Q#fold = *blanks;
        Endif;
        //*** End Add *** 03/02/22 ********************************************
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM          256
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C*
       // Inform Scriptura to attach the CSV to the DARTS
       // it creates.
       Select;
       When xmlLink = ' ';
          Chain xmlFlowname xp100;
       Other;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       EndSl;
       If %Found(XMLPREL1);
          XPdrseq# = %Int(Q#seq#);
          Update xp100;
       Endif;
       //** end add ***** 03/02/22 **********************
     C                   ENDSR
      /Free
       //==================================================================
       // Load XML statement file maintenance fields
       //==================================================================
       Begsr LoadStmtMaint;
        ACuser  = q1User;
        ACpgm   = q1Pgm;
        ACtime  = %dec(%char(%time:*hms0):6:0);
        ACdt    = %dec(%char(%date:*cymd0):7:0);
       Endsr;
       //==================================================================
       // Write records to statement workfile - criteria
       //==================================================================
       Begsr WriteStmtC;
        If not isUseXML;
           Leavesr;
        Endif;
        //***  Begin Add  ***  09/23/21  ***************
        Select;
        When not %open(WXACPSTMTC);
           Leavesr;
        Endsl;
        //***  End   Add  ***  09/23/21  ***************
        ACxml     = xmlTemplate;
        ACco#     = COco#;
        ACfndCrit = %editc(xxFnd:'Z');
        If xxFnd = *zero;
           ACfndCrit = '*ALL';
        Endif;
        ACagtCrit = %editc(xxAgt#:'Z');
        If xxAgt# = *zero;
           ACagtCrit = '*ALL';
        Endif;
        ACempCrit = %editc(xxEmp#:'Z') + ' / ' + %editc(xxDiv:'Z');
        If xxEmp# = *zero;
           ACempCrit = '*ALL';
        Endif;
        ACfromDate = xxFDt;
        ACtoDate   = xxTDt;
        ACzeroBal  = RSzbf;
        Select;
        When qqTypO = 'P';
           ACoutput = 'Print Only';
        When qqTypO = 'B';
           ACoutput = 'Print & File';
        When qqTypO = 'F';
           ACoutput = 'File Only';
        Endsl;
        ACrunDate = %dec(%char(%date:*usa0):8:0);
        ACrunTime = %dec(%char(%time:*hms0):6:0);
        ACrunUser = q1User;
        Exsr LoadStmtMaint;
        Write pacs0c;
       Endsr;
       //==================================================================
       // Write records to statement workfile 1
       //==================================================================
       Begsr WriteStmt1;
        If not isUseXML;
           Leavesr;
        Endif;
        //***  Begin Add  ***  09/23/21  ***************
        Select;
        When not %open(WXACPSTMT1);
           Leavesr;
        Endsl;
        ACco#     = COco#;
        //***  End   Add  ***  09/23/21  ***************
        ACco#     = COco#;
        ACxml     = xmlTemplate;
        ACcoName  = COname;
        ACcoAdr1  = COadr1;
        ACcoAdr2  = COadr2;
        ACcoCity  = COcty;
        ACcoSt    = COst;
        ACcoZip   = COzip;
        ACfnd     = FNfnd;
        ACfndName = FNname;
        Select;
        When FNmAd1 <> *blank;
           ACfndAdr1 = FNmAd1;
           ACfndAdr2 = FNmAd2;
           ACfndCity = FNmCty;
           ACfndSt   = FNmSt;
           ACfndZip  = FNmZip;
        Other;
           ACfndAdr1 = FNpAd1;
           ACfndAdr2 = FNpAd2;
           ACfndCity = FNpCty;
           ACfndSt   = FNpSt;
           ACfndZip  = FNpZip;
        Endsl;
        ACfromDate = xxFDt;
        ACtoDate   = xxTDt;
        ACgrpCPrem = xxFAnnC;
        ACgrpCAct  = xxFColC;
        ACgrpCEarn = xxFAmtC;
        ACgrpWPrem = xxFAnnW;
        ACgrpWAct  = xxFColW;
        ACgrpWEarn = xxFAmtW;
        ACgrpTPrem = xxFAnn;
        ACgrpTEarn = xxFAmt;
        ACgrpTAct  = xxFCol + x2FCol + x3FCol;
        ACgrpTAct1 = xxFCol;
        ACgrpTAct2 = x2FCol;
        ACgrpTAct3 = x3FCol;
        Exsr LoadStmtMaint;
        Write pacs01;
       Endsr;
       //==================================================================
       // Write records to statement workfile 2
       //==================================================================
       Begsr WriteStmt2;
        If not isUseXML;
           Leavesr;
        Endif;
        //***  Begin Add  ***  09/23/21  ***************
        Select;
        When not %open(WXACPSTMT2);
           Leavesr;
        Endsl;
        //***  End   Add  ***  09/23/21  ***************
        ACxml      = xmlTemplate;
        ACfromDate = xxFDt;
        ACtoDate   = xxTDt;
        ACagtName  = AGname;
        Select;
        When AGmAd1 <> *blank;
           ACagtAdr1 = AGmAd1;
           ACagtAdr2 = *blank;
           ACagtCity = AGmCty;
           ACagtSt   = AGmSt;
           ACagtZip  = AGmZip;
        Other;
           ACagtAdr1 = AGpAd1;
           ACagtAdr2 = *blank;
           ACagtCity = AGpCty;
           ACagtSt   = AGpSt;
           ACagtZip  = AGpZip;
        Endsl;
        ACagtFein  = AGfein;
        ACagtMod   = AGmod;
        ACagtPrBal = x1PrBl;
        ACagtCPrem = xxAAnnC;
        ACagtCAct  = xxAColC;
        ACagtCEarn = xxAAmtC;
        ACagtWPrem = xxAAnnW;
        ACagtWAct  = xxAColW;
        ACagtWEarn = xxAAmtW;
        ACagtTPrem = xxAAnn;
        ACagtTAct  = xxACol;
        ACagtTEarn = xxAAmt;
        Exsr LoadStmtMaint;
        Write pacs02;
       Endsr;
       //==================================================================
       // Write records to statement workfile 3
       //==================================================================
       Begsr WriteStmt3;
        If not isUseXML;
           Leavesr;
        Endif;
        //***  Begin Add  ***  09/23/21  ***************
        Select;
        When not %open(WXACPSTMT3);
           Leavesr;
        Endsl;
        //***  End   Add  ***  09/23/21  ***************
        ACxml      = xmlTemplate;
        ACprodName = xxPNme;
        ACprodAdr1 = APmAd1;
        ACprodAdr2 = *blank;
        ACprodCity = APmCty;
        ACprodSt   = APmSt;
        ACprodZip  = APmZip;
        ACprdCPrem = xxPAnnC;
        ACprdCAct  = xxPColC;
        ACprdCEarn = xxPAmtC;
        ACprdWPrem = xxPAnnW;
        ACprdWAct  = xxPColW;
        ACprdWEarn = xxPAmtW;
        ACprdTPrem = xxPAnn;
        ACprdTAct  = xxPCol;
        ACprdTEarn = xxPAmt;
        Exsr LoadStmtMaint;
        Write pacs03;
       Endsr;
       //==================================================================
       // Write records to statement workfile 4
       //==================================================================
       Begsr WriteStmt4;
        If not isUseXML;
           Leavesr;
        Endif;
        //***  Begin Add  ***  09/23/21  ***************
        Select;
        When not %open(WXACPSTMT4);
           Leavesr;
        Endsl;
        //***  End   Add  ***  09/23/21  ***************
        ACxml      = xmlTemplate;
        ACempName  = EMname;
        Select;
        When EMmAd1 <> *blank;
           ACempAdr1 = EMmAd1;
           ACempAdr2 = EMmAd2;
           ACempCity = EMmCty;
           ACempSt   = EMmSt;
           ACempZip  = EMmZip;
        Other;
           ACempAdr1 = EMpAd1;
           ACempAdr2 = EMpAd2;
           ACempCity = EMpCty;
           ACempSt   = EMpSt;
           ACempZip  = EMpZip;
        Endsl;
        Exsr LoadStmtMaint;
        Write pacs04;
       Endsr;
       //==================================================================
       // Write records to statement workfile 5
       //==================================================================
       Begsr WriteStmt5;
        If not isUseXML;
           Leavesr;
        Endif;
        //***  Begin Add  ***  09/23/21  ***************
        Select;
        When not %open(WXACPSTMT5);
           Leavesr;
        Endsl;
        //***  End   Add  ***  09/23/21  ***************
        ACxml      = xmlTemplate;
        Clear ACpfDt;
        Clear ACptDt;
        Clear ACjrSt;
        Clear ACcap#;
        If ACemp# <> 0;
           ACpfDt     = %dec(%char(%date(ELpfDt:*CYMD):*usa0):8:0);
           ACptDt     = %dec(%char(%date(ELptDt:*CYMD):*usa0):8:0);
           ACjrSt     = xxJrSt;
           ACcap#     = xxCap#;
        Endif;
        Select;
        When ACemp# = 0;
           Clear ACcommLvl;
        When ACagt3 = 'Y';
           ACcommLvl = 'Tertiary';
        When ACagt2 = 'Y';
           ACcommLvl = 'Secondary';
        Other;
           ACcommLvl = 'Primary';
        Endsl;
        ACcode     = ACcoCd;
        ACcodeDesc = ACdesc;
        ACtype     = ACcoTp;
        ACtypeDesc = *blank;
        ACplan     = ACplan;
        ACplanDesc = xxPlanDesc;
        ACprPrem   = xxHoldPrem;
        ACcurrPrem = xxPrtPrem;
        ACactivity = ACtoCl;
        ACrate     = ACrate;
        ACearnings = ACamt;
        ACeTD      = xxCPTD;
        ACpostDate = %dec(%char(%date(ACpoDt:*CYMD):*usa0):8:0);
        ACtranDate = %dec(%char(%date(ACtrDt:*CYMD):*usa0):8:0);
        Exsr LoadStmtMaint;
        Write pacs05;
       Endsr;
       //==================================================================
       // Write records to statement workfile 6
       //==================================================================
       Begsr WriteStmt6;
        If not isUseXML;
           Leavesr;
        Endif;
        //***  Begin Add  ***  09/23/21  ***************
        Select;
        When not %open(WXACPSTMT6);
           Leavesr;
        Endsl;
        //***  End   Add  ***  09/23/21  ***************
        ACxml      = xmlTemplate;
        ACyear     = xxYear;
        ACyearETD  = xxAmt;
        Exsr LoadStmtMaint;
        Write pacs06;
       Endsr;
       //==================================================================
       // Reserve/Update XML record for Scriptura output.
       //==================================================================
       Begsr ScriptXML;

       If not isUseXML or xmlTemplate <> ' ';
          Leavesr;
       Endif;

       Select;
       When isUseXML;
       Other;
          Leavesr;
       Endsl;

       //* xmlFile - contains the name of the template XML file, used for work f
       //* xmlFlowName - contains the name of the flow XML file, used for S4401C
       //* xmlLink - used to link individual records together in a mass run - XM

       xmlFile = ' ';
       message = %trim(processType);
       string = ' ';

       //*** Begin Add *** 12/11/25 ****************************************
       If isMassReq;

          In *Lock *dtaara;
          Out *dtaara;

          If ldMassFile <> ' ';
             xmlFile = %trim(ldMassFile);
          Else;
             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':
              ' ':'AgentStmts':'000':'000':massFlag:xmlLink);

             In *Lock *dtaara;
             ldMassFile = %trim(xmlFile);
             Out *dtaara;

             Chain (xmlFile) xp100;
             XPCat = 'AS';
             XPData = 'Y';
             If XPrmCd = ' ' And qqCrCd <> ' ';
                XPrmCd = qqCrCd;
                XPdart = 'Y';
             EndIf;
             Update xp100;

          Endif;

          isMassReq = *off;
          xmlFlowName = xmlfile;
          xmllink = xmlfile;
       EndIf;
       //*** End   Add *** 12/11/25 ****************************************

       //* Request record for Agent Commission Statement Report.
       If QQAgt# <> '00000';
         Clear dbrrn;
         Clear AAco#;
         Clear AAfein;
         Clear AAmod;
         Chain (xxCo#:xxFnd:xxAgt#) wmaal1;
         Chain (xxCo#:AAfein:AAmod) wmagl;
         x@rrn = dbrrn;
          StLetter('XMLREQUEST':xmlFile:message:'A':%editc(x@RRN:'X'):
           xxPre:string:QQCO#:QQFND:massFlag:xmlLink);
       Else;
          StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
           'AgtComStmt':'000':'000':massFlag:xmlLink);
       Endif;

       //* Flow XML file name is used for the S4401C call.
       If xmlFlowName = ' ';
          xmlFlowName = xmlFile;
       Endif;

       //* The template XML file name must be pulled from record for work files
       //* single request.  Otherwise it has been returned from the XML Request
       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       xmlTemplate = xmlFile;

       XPData = ' ';

       If QQAgt# = '00000';
          XPDart = ' ';
          XPRMCD = ' ';
       //*** Begin Add *** 09/26/25 ****************************************
       Else;
          If XPrmCd = ' ' And qqCrCd <> ' ';
             XPrmCd = qqCrCd;
             XPdart = 'Y';
          EndIf;
          XPfein = AGfein;
          XPmod  = AGmod;
       //*** End   Add *** 09/26/25 ****************************************
       Endif;

       //*** begin add *** 12/11/25 ****************************************
       XPNoComb = 'Y';
       //*** end add ***** 12/11/25 ****************************************
       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DSTime;
       XPDt   = sysDt;

       Update xp100;

       If not %open(WXACPSTMTC);
          Open WXACPSTMTC;
          Open WXACPSTMT1;
          Open WXACPSTMT2;
          Open WXACPSTMT3;
          Open WXACPSTMT4;
          Open WXACPSTMT5;
          Open WXACPSTMT6;
       Endif;

       Endsr;
      /End-free
     C*================================================================
     OQSYSPRT   E            ROUTE          1 06
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'COMPANY        :'
     O                       RSCO#         Z     33
     O          E            ROUTE          2
     O                                           26 'GROUP          :'
     O              N38      RSFND         Z     33
     O               38                          34 '*ALL'
     O          E            ROUTE          2
     O                                           26 'AGENT NUMBER   :'
     O              N37      RSAGT#        Z     35
     O               37      RSAGTA              35
     O          E            ROUTE          2
     O                                           26 'EMPLOYER / DIV :'
     O               35      RSEMPA              35
     O              N35      RSEMP#        Z     35
     O              N35                          37 '/'
     O              N35      RSDIV         Z     41
     O          E            ROUTE          2
     O                                           26 'FROM DATE      :'
     O                       XXFDT               38 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'TO DATE        :'
     O                       XXTDT               38 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'PRINT IF 0 BAL :'
     O                       RSZBF               35
     O          E            ROUTE          2
     O                                           26 'TYPE OF OUTPUT :'
     O                       QQTYPO              33
     O          E            ROUTE            58
     O                                           26 'RUN DATE . . . :'
     O                       DSDTTM              44 '  :  :  &  /  /  '
     O                                           47 '-'
     O                       CLTID               52
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2

     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XCNAME             120
     O                       DSDATE        Y    180
     O          E            HEAD1          2
     O                       Q1PGM               10
     O                       ORTITL             120
     O                                          174 'PAGE:'
     O                       PAGE          Z    180
     O          E            HEAD1          2
     O                                           82 'DATE RANGE:'
     O                       XXFDT         Y   +  1
     O                                         +  1 '-'
     O                       XXTDT         Y   +  1
     O          E            HEAD1          2
     O                                           06 'GROUP:'
     O                       FNFND         Z   +  1
     O                       FNNAME            +  1
     O          E            HEAD2A         1
     O                                           06 'AGENT:'
     O                       ACAGT#        Z     15
     O                       AGNAME              56
     O          E            HEAD2A         1
     O                       AGMAD1              56
     O          E            HEAD2A         2
     O                       XXCSZ               56
     O          E            HEAD2          1
     O                                            9 'EMPLOYER'
     O                                           15 'DIV'
     O                                           21 'NAME'
     O                                          128 'CURRENT PERIOD'
     O                                          158 ' EARNINGS  '
     O          E            HEAD2          1
     O                                            9 ' YR'
     O                                         +001 'ST'
     O                                         +001 'CARRIER POLICY#'
     O                                         +002 'DESCRIPTION'
     O                                           69 'CODE  TYPE'
     O                                         +003 'BASED ON'
     O                                          106 'ANNUAL PREM'
     O                                          128 '   ACTIVITY   '
     O                                          138 'RATE'
     O                                          158 'THIS PERIOD'
     O                                          178 'EARNED TO DATE'
     O          E            PBAL        1  1
     O                                           41 'PREVIOUS BALANCE'
     O                       XXPRBL        A    160
     O* Extra Line
     O          E            PBALX          1
     O          E            HEAD3       2  1
     O                                           09 'PRODUCER:'
     O                       ACPROD        Z     15
     O                       XXPNME              56
     O          E            DET         1
     O                       ACFYR         Z      9
     O                       XXJRST            +001
     O                       XXCAP#            +001
     O                       ACDESC            +001
     O                       ACCOCD              62
     O                       ACCOTP              67
     O                       xxPrtPrem     A    108
     O                       ACAMT         A    160
     O                       xx2nd3rd           162
     O          E            DET1        1
     O                       ACEMP#        Z     09
     O                       ACDIV         Z     15
     O                       EMNAME              57
     O          E            DET2        1
     O                       ACFYR         Z      9
     O                       XXJRST            +001
     O                       XXCAP#            +001
     O                       ACDESC            +001
     O                       ACCOCD              62
     O                       ACCOTP              67
     O                       xxPlanDesc        +005
     O                       xxPrtPrem     A    108
     O                       ACTOCL        A    130
     O                       ACRATE        K    140
     O                       ACAMT         A    160
     O                       xx2nd3rd           162
     O          E            EARNED      0  0
     O                       XXCPTD        A    180
     O          E            TOTPROD1    2  0
     O                                           55 'COMMISSION DETAILS:'
     O          E            TOTPRODC    2  0
     O                                           38 'Cash Basis'
     O                                          +01 'Commission Total:'
     O                       xxPAnnC       AB   108
     O                       xxPColC       AB   130
     O                       xxPAmtC       AB   160
     O          E            TOTPRODW    2  0
     O                                           38 'Written Premium'
     O                                          +01 'Commission Total:'
     O                       xxPAnnW       AB   108
     O                       xxPColW       AB   130
     O                       xxPAmtW       AB   160
     O          E            TOTPROD2    2  1
     O                                           55 'PRODUCER TOTAL:'
     O                       XXPANN        AB   108
     O                       XXPCOL        AB   130
     O                       XXPAMT        AB   160
     O          E            TOTAGTC        1
     O                                           38 'Cash Basis'
     O                                          +01 'Commission Total:'
     O                       xxAAnnC       AB   108
     O                       xxAColC       AB   130
     O                       xxAAmtC       AB   160
     O          E            TOTAGTW     1  1
     O                                           38 'Written Premium'
     O                                          +01 'Commission Total:'
     O                       xxAAnnW       AB   108
     O                       xxAColW       AB   130
     O                       xxAAmtW       AB   160
     O          E            TOTAGT      1  2
     O                                           55 'AGENT TOTAL:'
     O                       XXAANN        AB   108
     O                       XXACOL        AB   130
     O                       XXAAMT        AB   160
     O          E            AGTEA          1
     O                 N88                      151 'AGENT EARNED TO DATE'
     O                       XXYEAR        Z    162
     O                       XXAMT         A    180
     O          E            NOFNDTOT       2
     O                                           24 '* * * NO RECORDS MATCH'
     O                                         +001 'SELECTION CRITERIA'
     O          E            HeadTotA       2
     O          E            HeadTot        1
     O                                          106 '    Current    '
     O                                          128 'Current Period'
     O                                          158 ' Earnings  '
     O          E            HeadTot        2
     O                                           55 'COMMISSION DETAILS:'
     O                                          106 ' Period Premium'
     O                                          128 '   Activity   '
     O                                          158 'This Period'
     O          E            TOTGRPC        1
     O                                           37 'Cash Basis'
     O                                          +01 'Commission Total:'
     O                       xxFAnnC       A    108
     O                       xxFColC       A    130
     O                       xxFAmtC       A    160
     O          E            TOTGRPW        1
     O                                           37 'Written Premium'
     O                                          +01 'Commission Total:'
     O                       xxFAnnW       A    108
     O                       xxFColW       A    130
     O                       xxFAmtW       A    160
     O          E            TOTGRP         1
     O                                           55 '      Group Totals:'
     O                       xxFAnn        A    108
     O              N99      xxFCol        A    130
     O                       xxFAmt        A    160
     O          E    99      TOTGRP         1
     O                                           55 '    Primary Agents:'
     O                       xxFCol        A    130
     O          E    99      TOTGRP         1
     O                                           55 '* Secondary Agents:'
     O                       x2FCol        A    130
     O          E    99      TOTGRP         1
     O                                           55 '^  Tertiary Agents:'
     O                       x3FCol        A    130
     O          E            LINE1          1
     P*============================================
     P* Determine if the object exists
     P*============================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e
**  ARRAY RMK - DARTS DIARY REMARK
Agent Commission Statement - Group
Statement Date
thru
