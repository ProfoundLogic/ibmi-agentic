     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/TITLE SRONLI - API FOR ONLINE S/R TASKS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H*    THIS PROGRAM IS USED TO PERFORM VARIOUS TASKS FOR          *
     H*    ONLINE S/R PROCESSING.                                     *
     H*                                                               *
     H*    IF THE PROGRAM IS CALLED WITH A MODE OF '1' THEN IT        *
     H*    WILL RETURN THE STATUS OF THE POLICY IN REGARDS TO         *
     H*    ONLINE S/R PROCESSING.                                     *
     H*                                                               *
     H*    MODE '1' WILL RETURN THE FOLLOWING IN ERROR PARM:          *
     H*                                                               *
     H*    '0' - POLICY IS S/R, FLAGGED FOR ONLINE, AND HAS EFT OR    *
     H*          3RD PARTY                                            *
     H*    '1' - POLICY IS S/R, FLAGGED FOR ONLINE, NO VALID EFT OR   *
     H*          3RD PARTY                                            *
     H*    '2' - POLICY IS S/R BUT NOT FLAGGED FOR ONLINE.            *
     H*    '3' - POLICY IS NOT S/R.                                   *
     H*                                                               *
     H*    MODE '2' WILL CREATE A/R BATCH RECORDS FOR AMOUNTS.        *
     H*                                                               *
     H*    MODE '3' WILL CREATE S/R PAYROLL RECORDS FOR CLASSES.      *
     H*                                                               *
     H*    MODE '4' WILL WRITE RECORDS TO SRODTAQ                     *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    03/28/08            PROJECT: CI019                *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:
     H*                                                               *
     H* 06/24/24 C:333 Project: 146WKR                                *
     H*                Collateral Processing.                         *
     H*                                                               *
     H* 06/23/23 C:65  Project: Phw#107685  (Prescient)               *
     H*                Look for payroll class with specific effective *
     H*                date when writing s/r payrolls with mode '3'.  *
     H*                This is a partial reversal of support change   *
     H*                done on 12/21/11 under ticket 78737.           *
     H*                                                               *
     H* 04/10/23 C:65  Project: Phw#107397  (Prescient)               *
     H*                Do not return error if no WDELP and bscd is    *
     H*                being passed in.                               *
     H*                                                               *
     H* 08/10/22 C:65  Project: Phw#103051  (Inhouse PayGo)           *
     H*                Add invoice# as optional parameter to allow    *
     H*                SRBAT to reuse it.                             *
     H*                                                               *
     H* 03/08/22 C:33  Project: WC1986   #101372                      *
     H*                Use xxDiv/xxFyr vs qqDiv/qqFyr for EFT check   *
     H*                                                               *
     H* 04/06/21 C:136 Project: WC1986                                *
     H*                Add EFT to Quote. Add starting policy year     *
     H*                functionality to EFT and 3rd Party.            *
     H*                                                               *
     H* 11/18/20 C:180 Project WC2757                                 *
     H*                Recompiled only.                               *
     H*                                                               *
     H* 10/27/20 C:136 Project: SY407                                 *
     H*                Add parm for invoice # for 3rd Party           *
     H*                processing                                     *
     H*                                                               *
     H* 08/28/20 C:33  Project: SY407                                 *
     H*                Return error information only if no years were *
     H*                 selected.                                     *
     H*                                                               *
     H* 11/04/19 C:136 Project: SY407 (DE573)                         *
     H*                3rd Party Payments are friends, not food       *
     H*                (shouldn't be EFT for S/R Online)              *
     H*                                                               *
     H* 10/21/19 C:33  Project: SY407                                 *
     H*                Return error information if QQERR is passed in *
     H*                 as 'Y'.                                       *
     H*                                                               *
     H* 07/19/19 C:136 Project: SY407                                 *
     H*                Cleaned up old comments. Added logic to Mode1  *
     H*                to return '4' if the policy is 3rd Party.      *
     H*                                                               *
     H* 05/13/19 C:33  Project: SY407                                 *
     H*                Include the group in the data queue info       *
     H*                                                               *
     H* 01/24/19 C:33  Project: SY407                                 *
     H*                Add alias (and member) for holding areas       *
     H*                                                               *
     H* 05/04/15 C:136 Support: WA046 34338                           *
     H*                Ensure that, in the case of a split month, the *
     H*                A/R is posted to the expiration date of the    *
     H*                prior year in order to synch up with Comp-     *
     H*                Suite S/R Edit. CompSuite won't adjust         *
     H*                subsequent billings correctly otherwise.       *
     H*                                                               *
     H* 08/05/14 C:136 Support: WA045 86263                           *
     H*                Correct 82607. It was causing S/R Online to    *
     H*                to possibly report payrolls and write A/R to   *
     H*                the wrong reporting period.                    *
     H*                                                               *
     H* 03/04/14 C:283 Support: CI045 82607                           *
     H*                Send back the Expiration Date of the reporting *
     H*                month end date is after the Expiration Date.   *
     H*                                                               *
     H* 08/28/13 C:136 Support: WA146 83053                           *
     H*                Don't wash payments for non state reporting    *
     H*                divisions with summarized A/R through cash     *
     H*                dissolve                                       *
     H*                                                               *
     H* 08/23/12 C:282 Project: WC2160                                *
     H*                S/R Online: Incld A/R Batch in Prem Paid       *
     H*                                                               *
     H* 12/21/11 C:136 Support: WA045 78737                           *
     H*               - Do not write new payrolls                     *
     H*               - Report payrolls to most current payroll eff   *
     H*                 date as of report date                        *
     H*               - Implement code to report to payroll extension *
     H*               - From 78541 - populate SRTODT and SRRPMY IAW   *
     H*                 billing frequency                             *
     H*                                                               *
     H* 08/15/11 C:136 Support: WA133 77680                           *
     H*                Do not include div's that are not set up for   *
     H*                S/R Online                                     *
     H*                                                               *
     H* 06/23/11 C:189 SUPPORT: WA133/76401                           *
     H*                Incorrect message regarding payroll due  being *
     H*                displayed when payroll had already been        *
     H*                reported.                                      *
     H*                                                               *
     H* 06/16/11 C:191 Project: WC2048                                *
     H*                Auto Cash/Dissolve/Pmts AR Batch               *
     H*                                                               *
     H* 02/22/11 C:136 SUPPORT: WA045/75883                           *
     H*                PREVENT CREDIT PAYMENTS FROM BEING WRITTEN     *
     H*                                                               *
     H* 10/27/10 C:136 SUPPORT: WA146 74385                           *
     H*                REIMPLEMENT EFT CHECK BY CURRENT DATE WHICH    *
     H*                WAS LOST BY 73572                              *
     H*                                                               *
     H* 10/22/10 C:255 Support: SEQ73572                              *
     H*                Checked object WA073R in library list instead  *
     H*                Call Stack.                                    *
     H*                                                               *
     H* 08/25/10 C:227 PROJECT: 74004                                 *
     H*                Default Frequency to 'M' if its blank          *
     H*                                                               *
     H* 07/27/10 C:257 Support: SEQ73572                                  *
     H*                If API WA073R exist in call stack then use         *
     H*                file WMEFL6 instead of WMDBL3                      *
     H*                                                               *
     H* 07/05/10 C:227 PROJECT: CI084                                 *
     H*                SR Online performance enhancement              *
     H*                                                               *
     H* 05/31/10 C:255 PROJECT: WC1806 (PRISS#47)                     *
     H*                USING FILE WMEFL6 INSTEAD FILE WMDBL3          *
     H*                                                               *
     H*                                                               *
     H* 02/19/09 C:65  SUPPORT: CI019/SUPPORT                         *
     H*                VALIDATE THE ACH AS OF THE CURRENT DATE        *
     H*                INSTEAD OF THE DATE PASSED INTO PROGRAM.       *
     H*                                                               *
     H* 09/09/08 C:136 SUPPORT: WC1619                                *
     H*                ADD PARM FOR CALL TO SREFFD                    *
     H*****************************************************************
     FWTABL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWMEFL6    IF   E           K DISK
     FWMDBL3    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTC3L     IF   E           K DISK
     FWDCDL1    IF   E           K DISK
     FWASRL6E   UF A E           K DISK    RENAME(PSR01:PSR06E)
     FWASRL8E   UF A E           K DISK    RENAME(PSR01:PSR08E)
     FWMEPL7E   UF   E           K DISK    RENAME(PEP01:PEP07E)
     FWASRL2    IF   E           K DISK    RENAME(PSR01:PSRL2)
     FWASRL10   IF   E           K DISK    RENAME(PSR01:PSR10)
     FWMEML     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWDEAL5    IF   E           K DISK
     FWMP3L     IF   E           K DISK    RENAME(PP301:PP300)
      *** Beg Add ***** 06/24/24 ***************************
     FWMEXL     IF   E           K DISK
      *** End Add ***** 06/24/24 ***************************


     D @@YR            S              3  0 DIM(3)
     D @@BS            S              3  0 DIM(3)
     D @@PF            S              7  0 DIM(3)
     D @@PT            S              7  0 DIM(3)
     D @@FQ            S              1    DIM(3)
     D @@DV            S              5  0 DIM(3)
     D BSD             S              7  0 DIM(52)
     D STRDT           S               D
     D ENDDT           S               D
     D XXSDAY          s              4
     D SRLDAY          s              9
     D SRDTE           s              8  0
     D useAutoCash     s               n
     d mthlyRpting     s               n
     d xxDiffPayAmt    s                   like(SRAMTP)
     d isInStack       s              1a
     d ix              s              2  0
     d maxPgmCount     c                   10
     d stkPgm          s             10a   Dim(maxPgmCount) CtData PerRcd(1)
     d chrPayType      s              1
     d intInv#         s             10  0
     C*** begin add ***  08/10/22  ****************************************
     d chrInv          s             10    inz(' ')
     C*** end add *****  08/10/22  ****************************************
      *** beg add ***** 04/06/21 **********************************
     d hasEFT          s               n
     d has3rdParty     s               n
       Dcl-S chrEnv Char(1);
      /Copy wPrBank
      *** end add ***** 04/06/21 **********************************

     D                 DS
     D KKRPMY                  1      6  0
     D KKRPMM                  1      2
     D KKRPYY                  3      6
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  X1MODE                 1    100
     D  X1BSCD                 1      3  0
     D  X1ABCD                 1      3
     D                 DS
     D  X2MODE                 1    100
     D  X2TYP1                 1      2
     D  X2AMT1                 3     13  2
     D  X2DAT1                14     21  0
     D  X2TYP2                22     23
     D  X2AMT2                24     34  2
     D  X2DAT2                35     42  0
     D  X2TYP3                43     44
     D  X2AMT3                45     55  2
     D  X2DAT3                56     63  0
     D  X2TYP4                64     65
     D  X2AMT4                66     76  2
     D  X2DAT4                77     84  0
     D                 DS
     D  X3MODE                 1    100
     D  X3CLS1                 1      5  0
     D  X3ACL1                 1      5
     D  X3SPC1                 6      6
     D  X3EFD1                 7     14  0
     D  X3RMY1                15     20  0
     D  X3RTO1                21     28  0
     D  X3AMT1                29     39  2
     D  X3LAT1                40     40
     D  X3EXT1                41     45
     D  X3CLS2                46     50  0
     D  X3ACL2                46     50
     D  X3SPC2                51     51
     D  X3EFD2                52     59  0
     D  X3RMY2                60     65  0
     D  X3RTO2                66     73  0
     D  X3AMT2                74     84  2
     D  X3LAT2                85     85
     D  X3EXT2                86     90
     D                 DS
     D  X4MODE                 1    300
     D  X4ID                   1     25
     D  X4EMP#                 1      9
     D  X4TIME                10     15
     D  X4USER                16     25
     D  X4AEMP                26     34
     D  X4ADIV                35     39
     D  X4AFYR                40     42
     D  X4FREQ                43     43
     D  X4SPLT                44     44
     D  X4DT                  45     60
     D  X4SRIN                61     61
     D  X4AFND                62     64
     D*
     D getAutoCash     pr              n
     D inCo#                          3  0
     D inFnd                          3  0
     d inEmp#                              like(ELemp#)
     d inDiv                               like(ELdiv)
     d inFyr                               like(ELfyr)
     d ChkCallStack    pr                  extpgm('SRCHKPGM')
     d  outProgram                   10    Const
     d  rtnFlag                       1                                         '1' if pgm in stack
      /Copy WPREMPLR
     D xxCXDT          s                   like(ELPTDT)
     D xxCFDT          s                   like(ELPTDT)
     D xxREDT          s                   like(ELPTDT)

      /copy sPrFiles
     C*
     C*  EMPLOYER DETAIL FILE KEY LIST
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C* ACH DIRECT DEBIT FILE KEY LIST
     C*
     C     DBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     DBKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  TABLE FILE KEY LIST
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C*  GROUP DETAIL FILE.
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  PREMIUM CASH DISSOLVE FILE.
     C*
     C     C3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  AUTO CASH DISSOLVE FILE.
     C*
     C     CDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCTYP
     C*
     C* S/R PAYROLL FILE KEY LIST
     C*

     c     srky6e        klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     c                   kfld                    x1rpmy
     c                   kfld                    xxclas
     c                   kfld                    xxspec
     c                   kfld                    xxext
     c                   kfld                    x1efdt

     c     srky8e        klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     c                   kfld                    x1todt
     c                   kfld                    xxclas
     c                   kfld                    xxspec
     c                   kfld                    xxext
     c                   kfld                    x1efdt
     C*
     C     SRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDV
     C                   KFLD                    X1RPMY
     C*
     C     SRKY10        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDV
     C                   KFLD                    X1TODT
     C*
     C* EMPLOYER PAYROLL FILE KEY LIST
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    X1EFDT
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC

     c     epky7e        klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     c                   kfld                    xxclas
     c                   kfld                    xxspec
     c                   kfld                    xxext

     c     epky7e1       klist
     c                   kfld                    xxco#
     c                   kfld                    xxfnd
     c                   kfld                    xxfyr
     c                   kfld                    xxemp#
     c                   kfld                    xxdiv
     c                   kfld                    xxclas
     c                   kfld                    xxspec
     c                   kfld                    xxext
     C*** begin chg ***  06/23/23  *********************************************
     c*********          kfld                    x1todt
     c                   kfld                    x1efdt
     C*** end chg *****  06/23/23  *********************************************
     C*
     C*  PAYROLL RATE TABLE FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    X2EFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C*
     C* EMPLOYER MASTER FILE KEY LIST
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
      * WORKERS' COMPENSATION EMPLOYER A/R BATCH FILE
      *
     C     keyWDEAL5     Klist
     C                   Kfld                    xxco#
     C                   Kfld                    xxfnd
     C                   Kfld                    xxemp#
     C                   Kfld                    xxdiv
     C                   Kfld                    xxfyr
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    CDCTYP        XXCTYP
     C     *LIKE         DEFINE    ELPFDT        X1EFDT
     C     *LIKE         DEFINE    SRRPMY        X1RPMY
     C     *LIKE         DEFINE    ELPFDT        X1TODT
     C     *LIKE         DEFINE    ELPFDT        X2EFDT
     C     *LIKE         DEFINE    ELPFDT        XXCURD
     C     *LIKE         DEFINE    ELPFDT        XXBGDT
     C     *LIKE         DEFINE    ELPFDT        XXENDT
     C     *LIKE         DEFINE    ELPFDT        XOBGDT
     C     *LIKE         DEFINE    ELPFDT        XOENDT
     C     *LIKE         DEFINE    ELPFDT        XXWKDT
     C     *LIKE         DEFINE    ELFYR         XXYR
     C     *LIKE         DEFINE    ELDIV         XXDV
     C     *LIKE         DEFINE    ELBSCD        XXBS
     C     *LIKE         DEFINE    ELPFDT        XXPF
     C     *LIKE         DEFINE    ELPFDT        XXPT
     C     *LIKE         DEFINE    BSFREQ        XXFQ
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCMD2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3             1
     C                   PARM                    QQFND             3             2
     C                   PARM                    QQEMP#            9             3
     C                   PARM                    QQDIV             5             4
     C                   PARM                    QQFYR             3             5
     C                   PARM                    QQEFDT            8             6
     C                   PARM                    QQMODE            1             7
     C                   PARM                    QQERR             1             8
     C                   PARM                    QQDATA          100             9
     C                   PARM                    qqPayType         1            10
      *** Beg Add ***** 04/06/21 ******************************************
     C                   PARM                    qqEnv             1            11
     C*** begin add ***  08/10/22  ****************************************
     C                   PARM                    qqInv            10            12

       If Q1prms >= 12;
          chrInv = qqInv;
       Endif;
       If chrInv <> ' ';
          intInv# = %int(chrInv);
       Endif;
     C*** end add *****  08/10/22  ****************************************

       If Q1prms >= 11;
          chrEnv = qqEnv;
       Endif;
      *** End Add ***** 04/06/21 ******************************************

      /Free
       If Q1Prms >= 10;
          chrPayType = qqPayType;
       Endif;
      /End-Free
      *** End Add ***** 11/04/19 ******************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      *BLANK        XXERR             1
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQEFDT        IFNE      *BLANKS
     C     QQEFDT        ANDNE     *ZEROS
     C                   MOVE      QQEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C*
     C* SELECT MODE OF PROCESSING.
     C*
     C                   SELECT
     C     QQMODE        WHENEQ    '1'
     C                   EXSR      MODE1
     C     QQMODE        WHENEQ    '2'
     C                   EXSR      MODE2
     C     QQMODE        WHENEQ    '3'
     C                   EXSR      MODE3
     C     QQMODE        WHENEQ    '4'
     C                   EXSR      MODE4
     C                   ENDSL                                                  QQMODE SELECT
     C*
     C* MOVE RESULTS INTO PARM FIELDS.
     C*
     C                   MOVE      XXERR         QQERR
     C                   MOVE      *ON           *INLR

      /Free
       //--------------------------------------------------
       // Mode 1 - Returns status of policy
       //--------------------------------------------------
       Begsr Mode1;
       // Don't populate x1Mode when Mode1 is called from Mode4
       If qqMode = '1';
          x1Mode = QQdata;
       Endif;
       Chain elKy pel01;
       //*** begin chg ***  04/10/23  ************************************
       //*****If Not(%Found);
       If Not(%Found) and x1abcd = ' ';
       //*** end chg *****  04/10/23  ************************************
          xxErr = 'E';
          LeaveSr;
       Endif;
       Select;
       When x1abcd <> *Blanks;
          xxKey = x1abcd;
       Other;
          xxKey = %EditC(ELbscd:'X');
       EndSl;
       Chain tbky pbs01;
       If Not(%Found);
          xxErr = 'E';
          LeaveSr;
       Endif;
       //*** beg chg ***** 04/06/21 *************
       //Select;
       //When BSsefl <> 'S';
       //   xxErr = '3';
       //When BSonli <> 'Y';
       //   xxErr = '2';
       //When IsEFT();
       //   xxErr = '0';
       //When IsThirdParty();
       //   xxErr = '4';
       //   For ix = 1 to maxPgmCount;
       //      //***  Begin Add  ***  10/21/19  ************
       //      If stkPgm(ix) = *blanks;
       //         Iter;
       //      Endif;
       //      //***  End   Add  ***  10/21/19  ************
       //      ChkCallStack(stkPgm(ix):isInStack);
       //      If isInStack = '1';
       //         xxErr = '0';
       //         LeaveSr;
       //      Endif;
       //   EndFor;
       //Other;
       //   xxErr = '1';
       //Endsl;
       Select;
       When BSsefl <> 'S';
          xxErr = '3';
       When BSonli <> 'Y';
          xxErr = '2';
       When isEFT() Or isThirdParty();
          xxErr = '0';
       Other;
          xxErr = '1';
       Endsl;
       //*** end chg ***** 04/06/21 *************
       If xxErr = *Blank;
          xxErr = 'E';
       Endif;
       Endsr;
      /End-Free

     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE A/R BATCH RECORDS THROUGH SRBAT API.
     C*
     C     MODE2         BEGSR
     C*
     C                   MOVE      QQDATA        X2MODE
     C                   Z-ADD     0             XXAMT            11 2
     C                   Z-ADD     0             XXDATE            8 0
     C                   MOVE      *BLANK        XXTYPE            2
      /free
       useAutoCash = GetAutoCash(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr);
      /end-free
     C*
     C* FIRST RECORD.
     C*
     C     X2TYP1        IFNE      *BLANKS
     C*
     C                   MOVE      X2TYP1        XXTYPE
     C                   MOVE      X2AMT1        XXAMT
     C                   MOVE      X2DAT1        XXDATE
     C*
     C                   EXSR      SRCBAT
     C*
     C                   ENDIF                                                  X2TYP1 IFNE
     C*
     C* SECOND RECORD.
     C*
     C     X2TYP2        IFNE      *BLANKS
      *** Beg Add ***** 06/24/24 ***************************
     C     X2TYP2        ANDNE     'CB'
     C     X2TYP2        ANDNE     'CP'
      *** End Add ***** 06/24/24 ***************************
     C*
     C                   MOVE      X2TYP2        XXTYPE
     C                   MOVE      X2AMT2        XXAMT
     C                   MOVE      X2DAT2        XXDATE
     C                   if        useAutoCash and X2TYP4 <> *blanks
     C                   eval      XXAMT = XXAMT + X2AMT4
     C                   endif
     C*
     C                   EXSR      SRCBAT
     C*
     C                   ENDIF                                                  X2TYP2 IFNE
     C*
     C* THIRD RECORD.
     C*
     C     X2TYP3        IFNE      *BLANKS
     C*
     C                   MOVE      X2TYP3        XXTYPE
     C                   MOVE      X2AMT3        XXAMT
     C                   MOVE      X2DAT3        XXDATE
     C*
     C                   EXSR      SRCBAT
     C*
     C                   ENDIF                                                  X2TYP3 IFNE
     C*
     C* FOURTH RECORD.
     C*
     C                   if        X2TYP4 <> *blanks and not useAutoCash
      *** Beg Add ***** 06/24/24 ***************************
     C                             and X2TYP4 <> 'CB' and X2TYP4 <> 'CP'
      *** End Add ***** 06/24/24 ***************************
     C*
     C                   MOVE      X2TYP4        XXTYPE
     C                   MOVE      X2AMT4        XXAMT
     C                   MOVE      X2DAT4        XXDATE
     C*
     C                   EXSR      SRCBAT
     C*
     C                   ENDIF                                                  X2TYP4 IFNE
     C*
     C                   ENDSR                                                  MODE2 SUBR
     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE S/R PAYROLL RECORDS AND WMEPP IF NECESSARY.
     C*
     C     MODE3         BEGSR
     C*
     C     ELKY          CHAIN     PEL01                              60

     c                   movel(p)  ELBSCD        xxkey
     c     tbky          chain     pbs01
     c                   select
     c                   when      BSFREQ = 'M' or BSFREQ = *blank
     c                   eval      mthlyRpting = *on
     c                   other
     c                   eval      mthlyRpting = *off
     c                   endsl

     C                   MOVE      QQDATA        X3MODE
     C                   Z-ADD     0             XXCLAS            5 0
     C                   MOVE      *BLANK        XXSPEC            1
     C                   Z-ADD     0             XXAMT            11 2
     C                   Z-ADD     0             XXDATE            8 0
     C                   Z-ADD     0             XXRPMY            6 0
     C                   Z-ADD     0             XXTODT            8 0
     C                   MOVE      *BLANK        XXLATE            1
     C                   MOVE      *BLANK        XXEXT             5
     C*
     C* FIRST RECORD.
     C*
     C     X3ACL1        IFNE      *BLANKS
     C*
     C                   Z-ADD     X3CLS1        XXCLAS
     C                   MOVE      X3SPC1        XXSPEC
     C                   Z-ADD     X3AMT1        XXAMT
     C                   Z-ADD     X3EFD1        XXDATE
     C                   Z-ADD     X3RMY1        XXRPMY
     C                   Z-ADD     X3RTO1        XXTODT
     C                   MOVE      X3LAT1        XXLATE
     C                   MOVE      X3EXT1        XXEXT
     C*
     C                   EXSR      SRCPAY
     C*
     C                   ENDIF                                                  X3CLS1 IFNE
     C*
     C* SECOND RECORD.
     C*
     C     X3ACL2        IFNE      *BLANKS
     C*
     C                   Z-ADD     X3CLS2        XXCLAS
     C                   MOVE      X3SPC2        XXSPEC
     C                   Z-ADD     X3AMT2        XXAMT
     C                   Z-ADD     X3EFD2        XXDATE
     C                   Z-ADD     X3RMY2        XXRPMY
     C                   Z-ADD     X3RTO2        XXTODT
     C                   MOVE      X3LAT2        XXLATE
     C                   MOVE      X3EXT2        XXEXT
     C*
     C                   EXSR      SRCPAY
     C*
     C                   ENDIF                                                  X3CLS2 IFNE
     C*
     C                   ENDSR                                                  MODE3 SUBR
     C*****************************************************************
     C*
     C* SUBROUTINE TO CALL SRBAT API.
     C*
     C     SRCBAT        BEGSR
     C*
     C                   MOVE      *BLANKS       XXARCD            2
     C*
     C                   SELECT
     C*
     C* PREMIUM BILLING CODE FROM GROUP DETAIL.
     C*
     C     XXTYPE        WHENEQ    'PB'
     C*
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      *OFF
     C     F1SRBB        ANDNE     *BLANKS
     C                   MOVE      F1SRBB        XXARCD
     C                   ENDIF
     C*
     C* PREMIUM PAYMENT CODE FROM PREMIUM CASH DISSOLVE.
     C*
     C     XXTYPE        WHENEQ    'PP'
     C     xxamt         andgt     0
     C*
     C     C3KY          CHAIN     PC301                              60
     C     *IN60         IFEQ      *OFF
     C     C3SPCD        ANDNE     *BLANKS
     C                   MOVE      C3SPCD        XXARCD
     C                   ENDIF
     C*
     C* TAX BILLING CODE FROM A/R TABLE FILE.
     C*
     C     XXTYPE        WHENEQ    'TB'
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      'T'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXARCD
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* TAX PAYMENT CODE FROM AUTO CASH DISSOLVE.
     C*
     C     XXTYPE        WHENEQ    'TP'
     C     xxamt         andgt     0
     C*
     C                   MOVE      'T'           XXCTYP
     C     CDKY          CHAIN     PCD01                              60
     C     *IN60         IFEQ      *OFF
     C     CDCPCD        ANDNE     *BLANKS
     C                   MOVE      CDCPCD        XXARCD
     C                   ENDIF
     C*
      *** Beg Add ***** 06/24/24 ********************************
      * Collateral Billed code from WMEXP
      *
        When XXtype = 'CB';
         Chain C3KY pex01;
         If EXcolb <> ' ';
            xxARCD = EXcolb;
         Endif;
      *
      * Collateral Paid code from WMEXP
      *
        When XXtype = 'CP' and xxamt > 0;
         Chain C3KY pex01;
         If EXcolp <> ' ';
            xxARCD = EXcolp;
         Endif;
      *
      *** End Add ***** 06/24/24 ********************************
     C                   ENDSL                                                  XXTYPE SELEC
     C*
     C* CALL SRBAT IF AN A/R CODE HAS BEEN FOUND.
     C*
     C     XXARCD        IFNE      *BLANKS
     C*
     C                   MOVE      XXDATE        Q1PODT
     C                   MOVE      XXAMT         Q1AMT
     C                   MOVE      XXARCD        Q1ARCD
     C                   Z-ADD     0             XXBAT#            9 0
     C                   MOVE      XXBAT#        Q1BAT#
     C                   MOVE      'O'           Q1MODE
      /Free
       // Pass a 'P' for 3rd Party Payments
       If chrPayType <> *Blanks;
          Q1Mode = chrPayType;
       Endif;
      /End-Free
      /Free
       // q1Inv# will always be zero for EFT and non-zero
       //  for 3rd Party.
       q1Inv# = intInv#;
      /End-Free
     C*
     C                   CALL      'SRBAT'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    Q1PODT            8
     C                   PARM                    Q1ARCD            2
     C                   PARM                    Q1AMT            11
     C                   PARM                    Q1BAT#            9
     C                   PARM                    Q1MODE            1
     C                   PARM                    q1Inv#           10 0

      /Free
       intInv# = q1Inv#;
      /End-Free

     C*
     C                   ENDIF                                                  XXARCD IFNE
     C*
     C                   ENDSR                                                  SRCBAT SUBR
     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE RECORDS IN SRODATQ
     C*
     C     MODE4         BEGSR
     C*
     C                   Z-ADD     0             XXCNT             6 0
     C*
     C     EMKY          SETLL     PEM01
     C     EMKY          READE     PEM01                                  70
     C*
     C     *IN70         DOWEQ     *OFF
     C*
     C                   Z-ADD     EMDIV         XXDIV
     C*
     C                   EXSR      INTVAR
     C*
     C                   Z-ADD     1             I                 2 0
     C*
     C                   Z-ADD     XXCURD        XXBGDT
     C                   Z-ADD     XXCURD        XXENDT
     C*
     C     ELKY2         SETGT     PEL01
     C     ELKY2         READPE    PEL01                                  71
     C*
     C     *IN71         DOWEQ     *OFF
     C*
     C                   MOVEL(P)  ELFYR         XXFYR
     C*
     C*
     C                   EXSR      MODE1
     C*
     C     XXERR         IFEQ      '0'
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C*
     C                   IF        BSFREQ = ' '
     C                   MOVEL     'M'           @@FQ(I)
     C                   ELSE
     C                   MOVEL     BSFREQ        @@FQ(I)
     C                   ENDIF
     C*
     C                   IF        @@FQ(1) <> @@FQ(2)
     C                   EXSR      GETDT
     C                   ENDIF                                                  CURBS <> PRVBS

      * Prevent the policy from reporting if cx date is prior to reporting period
      /Free
         xxCFDT = *Zeros;
         xxREDT = *Zeros;
         xxCXDT = GetCxDate1(ELCO#:ELFND:ELEMP#:ELDIV:ELFYR:xxCFDT:xxREDT);
         if xxCxDt <> *Zeros and xxReDt = *zeros;
            if xxCXDT < ELptdt;
               ELptdt = xxCXDT;
            endif;
         endif;
      /End-Free

     C     XXBGDT        IFLT      ELPTDT
     C     XXENDT        ANDGE     ELPFDT
     C*
     C                   IF        I < 2
     C                   MOVE      XXBGDT        XOBGDT
     C                   MOVE      XXENDT        XOENDT
     C                   ELSE
     C                   IF        XXBGDT >= ELPTDT
     C                   MOVE      XOBGDT        XXBGDT
     C                   ELSE
     C                   MOVE      'Y'           XXSPLT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   IF        (XXSPLT = 'Y') OR (I = 1)
     C                   Z-ADD     ELFYR         @@YR(I)
     C                   MOVEL     ELBSCD        @@BS(I)
     C                   Z-ADD     ELPFDT        @@PF(I)
     C                   Z-ADD     ELPTDT        @@PT(I)
     C                   Z-ADD     ELDIV         @@DV(I)
     C                   ENDIF
     C*
     C                   ADD       1             I
     C*
     C                   ENDIF                                                  XXENDT >= ELPFDT
     C                   ENDIF                                                  XXERR=0
     C*
     C*
     C     I             IFLE      2
     C     ELKY2         READPE    PEL01                                  71
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN71=*OFF
     C*
     C                   CLEAR                   XXB               7 0
     C                   CLEAR                   XXE               7 0
     C     XXSPLT        IFNE      *BLANKS
     C*
     C                   EXSR      LODBSD
     C*
     C                   Z-ADD     1             J                 2 0
     C*
     C                   DOW       (J <= 52) AND BSD(J) <> 0
     C*
     C                   IF        J = 1
     C                   IF        BSD(J) > XXENDT
     C     *CYMD         MOVEL     XXENDT        STRDT
     C                   ADDDUR    1:*D          STRDT
     C     *CYMD         MOVEL     STRDT         XXB
     C                   Z-ADD     BSD(J)        XXE
     C                   ELSE
     C                   Z-ADD     XXBGDT        XXB
     C                   Z-ADD     BSD(J)        XXE
     C                   ENDIF
     C                   ELSE                                                   J=1
     C                   EVAL      I = J-1
     C                   IF        BSD(I) > XXENDT
     C     *CYMD         MOVEL     XXENDT        STRDT
     C     *CYMD         MOVEL     STRDT         XXE
     C                   Z-ADD     XXBGDT        XXB
     C                   ELSE
     C                   Z-ADD     BSD(J)        XXB
     C                   Z-ADD     BSD(I)        XXE
     C                   ENDIF                                                  J=1
     C                   ENDIF                                                  J=1
     C*
     C                   Z-ADD     @@YR(2)       XXYR
     C                   Z-ADD     @@BS(2)       XXBS
     C                   Z-ADD     @@PF(2)       XXPF
     C                   Z-ADD     @@PT(2)       XXPT
     C                   MOVE      @@FQ(2)       XXFQ
     C                   MOVE      @@DV(2)       XXDV
     C                   EXSR      DTAQ
     C*
     C                   EVAL      J = J+1
     C*
     C                   ENDDO                                                  J>=1
     C*
     C                   Z-ADD     XOBGDT        XXB
     C                   Z-ADD     XOENDT        XXE
     C                   Z-ADD     @@YR(1)       XXYR
     C                   Z-ADD     @@BS(1)       XXBS
     C                   Z-ADD     @@PF(1)       XXPF
     C                   Z-ADD     @@PT(1)       XXPT
     C                   MOVE      @@FQ(1)       XXFQ
     C                   MOVE      @@DV(1)       XXDV
     C                   EXSR      DTAQ
     C*
     C                   ELSE                                                   XXSPLT<> ' '
     C*
     C     @@YR(1)       IFNE      0
     C                   Z-ADD     XOBGDT        XXB
     C                   Z-ADD     XOENDT        XXE
     C                   Z-ADD     @@YR(1)       XXYR
     C                   Z-ADD     @@BS(1)       XXBS
     C                   Z-ADD     @@PF(1)       XXPF
     C                   Z-ADD     @@PT(1)       XXPT
     C                   MOVE      @@FQ(1)       XXFQ
     C                   MOVE      @@DV(1)       XXDV
     C                   EXSR      DTAQ
     C                   ENDIF                                                  @@YR(1) <> ' '
     C*
     C                   ENDIF                                                  XXSPLT<> ' '
     C*
     C     EMKY          READE     PEM01                                  70
     C*
     C                   ENDDO                                                  *IN70=*OFF
     C*
     C                   If        qqErr <> 'Y'
     C                              or xxCnt > 0
     C                   CLEAR                   XXERR
     C                   Endif
     C                   CLEAR                   QQDATA
     C                   MOVEL     XXCNT         QQDATA
     C                   ENDSR
     C*****************************************************************
     C*
     C* SUBROUTINE TO GET LOAD REPORITNG DATE FOR PRIOR POLICY YEAR
     C*
     C     LODBSD        BEGSR
     C*
     C                   Z-ADD     1             I
     C*
     C                   IF        XXENDT < @@PT(2)
     C                   MOVE      @@PT(2)       BSD(I)
     C                   ADD       1             I
     C                   ENDIF
     C*
     C                   MOVE      XXENDT        XXWKDT
     C*
     C                   SELECT
     C*
     C                   WHEN      @@FQ(2) = 'M'
     C*
     C                   DOW       (XXWKDT >= XXBGDT) AND (XXWKDT <= XXENDT)
     C                   MOVE      XXWKDT        BSD(I)
     C                   MOVE      '01'          XXWKDT
     C     *CYMD         MOVE      XXWKDT        ENDDT
     C                   SUBDUR    1:*D          ENDDT
     C     *CYMD         MOVE      ENDDT         XXWKDT
     C                   ADD       1             I
     C                   ENDDO
     C*
     C                   WHEN      @@FQ(2) = 'T'
     C*
     C                   DOW       (XXWKDT >= XXBGDT) AND (XXWKDT <= XXENDT)
     C                   MOVE      XXWKDT        BSD(I)
     C                   ADD       1             I
     C     *CYMD         MOVE      XXWKDT        STRDT
     C                   EXTRCT    STRDT:*D      XXDAY
     C*
     C                   IF        XXDAY >= 16
     C                   MOVE      '15'          XXWKDT
     C                   ELSE
     C                   MOVE      '01'          XXWKDT
     C     *CYMD         MOVE      XXWKDT        ENDDT
     C                   SUBDUR    1:*D          ENDDT
     C     *CYMD         MOVE      ENDDT         XXWKDT
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C                   WHEN      @@FQ(2) = 'W'
     C*
     C                   DOW       (XXWKDT >= XXBGDT) AND (XXWKDT <= XXENDT)
     C*
     C                   MOVE      XXWKDT        BSD(I)
     C                   ADD       1             I
     C*
    C     *CYMD         MOVE      XXWKDT        ENDDT
     C                   SUBDUR    7:*D          ENDDT
     C     *CYMD         MOVE      ENDDT         XXWKDT
     C*
     C                   ENDDO
     C*
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C* SUBROUTINE TO GET REPORTING PERIOD
     C*
     C     GETDT         BEGSR
     C*
     C
     C                   SELECT
     C*
     C                   WHEN      @@FQ(I) = 'M'
     C*
     C     *CYMD         MOVE      XXCURD        STRDT
     C                   SUBDUR    1:*M          STRDT
     C     *CYMD         MOVE      STRDT         XXBGDT
     C                   MOVE      '01'          XXBGDT
     C*
     C     *CYMD         MOVE      XXBGDT        ENDDT
     C                   ADDDUR    1:*M          ENDDT
     C                   SUBDUR    1:*D          ENDDT
     C     *CYMD         MOVE      ENDDT         XXENDT
     C*
     C                   WHEN      @@FQ(I) = 'T'
     C*
     C     *CYMD         MOVE      XXCURD        STRDT
     C                   EXTRCT    STRDT:*D      XXDAY             2 0
     C*
     C                   IF        XXDAY >= 16
     C*
     C     *CYMD         MOVE      STRDT         XXBGDT
     C                   MOVE      '01'          XXBGDT
     C                   MOVE      XXCURD        XXENDT
     C                   MOVE      '15'          XXENDT
     C*
     C                   ELSE
     C*
     C     *CYMD         MOVE      XXCURD        STRDT
     C                   SUBDUR    1:*M          STRDT
     C     *CYMD         MOVE      STRDT         XXBGDT
     C                   MOVE      '16'          XXBGDT
     C     *CYMD         MOVE      STRDT         XXENDT
     C                   MOVE      '01'          XXENDT
     C     *CYMD         MOVE      XXENDT        ENDDT
     C                   ADDDUR    1:*M          ENDDT
     C                   SUBDUR    1:*D          ENDDT
     C     *CYMD         MOVE      ENDDT         XXENDT
     C*
     C                   ENDIF
     C*
     C                   WHEN      @@FQ(I) = 'W'
     C
    C*
    C* Find the day of the week (Mon., Tue., Wed., ect.)
    C*
     C                   MOVE      XXCURD        XXENDT
     C                   EVAL      SRDTI = XXENDT
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKDATE            8 0
    C*
     C                   CALL      'SRDAY8'
     C                   PARM      WKDATE        SRDTE
     C                   PARM      *BLANKS       XXSDAY
     C                   PARM                    SRLDAY
     C                   PARM                    SRERR
     C*
     C                   DOW       %SUBST(SRLDAY:1:2) <> BSDOWK
     C*
     C     *CYMD         MOVE      XXENDT        STRDT
     C                   SUBDUR    1:*D          STRDT
     C     *CYMD         MOVE      STRDT         XXENDT
    C*
     C                   EVAL      SRDTI = XXENDT
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKDATE            8 0
    C*
     C                   CALL      'SRDAY8'
     C                   PARM      WKDATE        SRDTE
     C                   PARM      *BLANKS       XXSDAY
     C                   PARM                    SRLDAY
     C                   PARM                    SRERR
     C*
     C                   ENDDO
     C*
     C     *CYMD         MOVE      XXENDT        STRDT
     C                   SUBDUR    6:*D          STRDT
     C     *CYMD         MOVE      STRDT         XXBGDT
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C* SUBROUTINE TO INITIALIZE VARIABLES
     C*
     C     INTVAR        BEGSR
     C*
     C                   Z-ADD     0             XXYR
     C                   Z-ADD     0             XXBS
     C                   Z-ADD     0             XXPF
     C                   Z-ADD     0             XXPT
     C*
     C                   Z-ADD     1             I
     C     I             DOWLE     3
     C                   Z-ADD     0             @@YR(I)
     C                   Z-ADD     0             @@BS(I)
     C                   Z-ADD     0             @@PF(I)
     C                   Z-ADD     0             @@PT(I)
     C                   Z-ADD     0             @@DV(I)
     C                   MOVE      *BLANK        @@FQ(I)
     C                   ADD       1             I
     C                   ENDDO
     C*
     C                   Z-ADD     1             I
     C     I             DOWLE     52
     C                   Z-ADD     0             BSD(I)
     C                   ADD       1             I
     C                   ENDDO
     C*
     C                   MOVE      *BLANK        XXSPLT            1
     C*
     C                   ENDSR
     C*****************************************************************
     C*
     C* SUBROUTINE TO WRITE VALUES IN DATA QUEUE
     C*
     C     DTAQ          BEGSR
     C*
     C                   CLEAR                   X4MODE
     C*
     C                   MOVEL     QQDATA        X4ID
     C                   MOVEL     QQFND         X4AFND
     C                   MOVEL     QQEMP#        X4AEMP
     C                   MOVEL     EMDIV         X4ADIV
     C                   MOVE      XXYR          X4AFYR
     C*
     C                   MOVE      XXFQ          X4FREQ
     C                   MOVE      XXSPLT        X4SPLT
     C*
     C                   Z-ADD     XXB           XXWKB             7 0
     C                   Z-ADD     XXE           XXWKE             7 0
     C*
     C                   IF        XXPF > XXWKB
     C                   Z-ADD     XXPF          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         @@DTF             8 0
     C                   ELSE
     C                   Z-ADD     XXWKB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         @@DTF
     C                   ENDIF
     C*
     C                   IF        XXPT <= XXWKE
     C     *CYMD         MOVE      XXPT          ENDDT
     C     *CYMD         MOVE      ENDDT         XXWKE
     C                   Z-ADD     XXWKE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         @@DTT             8 0
     C                   ELSE
     C                   Z-ADD     XXWKE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         @@DTT
     C                   ENDIF
     C*
     C                   MOVEL     @@DTF         X4DT
     C                   MOVE      @@DTT         X4DT
     C*
     C                   IF        XXFQ = 'M'
     C*
     C                   MOVEL     @@DTT         KKRPMM            2
     C                   MOVE      @@DTT         KKRPYY            4
     C                   Z-ADD     KKRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         X1RPMY
     C     SRKY2         CHAIN     PSRL2                              90
     C*
     C                   ELSE
     C*
     C                   MOVEL     @@DTT         KKTODT            8 0
     C                   Z-ADD     KKTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TODT
     C     SRKY10        CHAIN     PSR10                              90
     C*
     C                   ENDIF
     C*
     C                   IF        *IN90 = *OFF
     C                   EVAL      X4SRIN = 'Y'
      /free

       Else;
          Chain keyWDEAL5 pea01;
          If %Found(wdeal5) And
             (EAtfap <> 'Y' And EAtffl <> 'Y') And
             (EActyp= 'P' Or EActyp= 'I' Or EActyp= 'Q');
                x4SrIn = 'Y';
          EndIf;

      /end-free
     C                   ENDIF
     C*
     C                   CALL      'QSNDDTAQ'                           99
     C                   PARM      'SRODATQ'     XXQNM            10
     C                   PARM      '*LIBL'       XXLIB            10
     C                   PARM      300           XXFLDL            5 0
     C                   PARM                    X4MODE
     C                   PARM      25            XXKEYL            3 0
     C                   PARM                    X4ID
     C*
     C                   ADD       1             XXCNT
     C*
     C                   ENDSR
     C*
     C*
     C* SUBROUTINE TO CREATE WASRP/WMEMP PAYROLL FILES.
     C*
     C     SRCPAY        BEGSR
     C*
     C* WRITE/UPDATE THE WASRP RECORD.
     C*
     C                   Z-ADD     0             SVAMT            11 2

      *
      *  Account for payroll extensions. S/R Online shouldn't write new payroll master records.
      *
     c                   z-add     xxtodt        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         x1todt

     C*** begin chg ***  06/23/23  *********************************************
     c*****epky7e1       setgt     pep07e
     c*****epky7e        readpe    pep07e
     c*****              if        %eof
     c*****              leavesr
     c*****              endif
     c*****              eval      x1efdt = EPEFDT
     C*
     C* Verify payroll master record exists for eff date passed in.
     C*
     c                   Z-add     xxdate        srdti
     c                   Call      'SRCVTDAT'    dtcymd
     c                   Movel     srdto         x1efdt
     C     epky7e1       Chain     pep07e
     C                   If        not %found
     C                   Leavesr
     C                   Endif
     C*** end chg *****  06/23/23  *********************************************
     c                   time                    dsdttm

     c                   if        mthlyRpting
     c                   z-add     xxrpmy        srdti
     c                   call      'SRCVTMY'     dtcymd
     c                   movel     srdto         x1rpmy
     c     srky6e        chain     psr06e
     c                   else
     c     srky8e        chain     psr08e
     c                   endif

     c                   if        not(%found)

     c                   if        mthlyRpting
     c                   z-add     0             SRTODT
     c                   z-add     x1rpmy        SRRPMY
     c                   else
     c                   z-add     0             SRRPMY
     c                   z-add     x1todt        SRTODT
     c                   endif

     c                   z-add     xxco#         SRCO#
     c                   z-add     xxfnd         SRFND
     c                   z-add     xxemp#        SREMP#
     c                   z-add     xxdiv         SRDIV
     c                   z-add     xxfyr         SRFYR
     c                   z-add     x1efdt        SREFDT
     c                   z-add     xxclas        SRCLAS
     c                   z-add     xxamt         SRAMTP
     c                   eval      xxDiffPayAmt = xxamt
     c                   movel     xxspec        SRSPEC
     c                   movel     xxext         SREXT
     c                   movel     'S'           SRTYFL
     c                   movel     q1user        SRIUSR
     c                   movel     q1pgm         SRIPGM
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         SRIDT
     c                   movel     dstime        SRITM
     c                   eval      SRDT   = SRIDT
     c                   eval      SRTIME = SRITM
     c                   eval      SRPGM  = SRIPGM
     c                   eval      SRUSER = SRIUSR
     c                   write     psr08e

     c                   else                                                   not(%found)

     c                   eval      xxDiffPayAmt = xxamt - SRAMTP
     c                   z-add     xxamt         SRAMTP
     c                   movel     q1user        SRUSER
     c                   movel     q1pgm         SRPGM
     c                   z-add     dsdate        srdti
     c                   call      'SRCVTDAT'    dtcymd
     c                   movel     srdto         SRDT
     c                   movel     dstime        SRTIME

     c                   if        mthlyRpting
     c                   update    psr06e
     c                   else
     c                   update    psr08e
     c                   endif

     c                   endif                                                  not(%found)

     c                   eval      EPSAPR = EPSAPR + xxDiffPayAmt
     C                   movel     q1user        EPUSER
     C                   movel     q1pgm         EPPGM
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         EPDT
     C                   movel     dstime        EPTIME
     C                   update    pep07e
     C*
     C                   ENDSR                                                  SRCPAY SUBR
     C*****************************************************************
     p getAutoCash     b
     d getAutoCash     pi              n
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                              like(ELemp#)
     d inDiv                               like(ELdiv)
     d inFyr                               like(ELfyr)
     d*
     d xpco#           s              3  0
     d xpfnd           s              3  0
     d xpFlag          s               n
     d*
     c     f2ky          klist
     c                   kfld                    xpco#
     c                   kfld                    xpfnd
     c*
     c                   eval      xpco# = inCo#
     c                   eval      xpfnd = inFnd
     C                   eval      xpFlag = *off
      /free
       // Don't wash non state rpt div with summarized A/R
       // through auto cash dissolve
       Chain (inCo#:inFnd:inEmp#:inDiv:inFyr) pel01;
       Chain (inCo#:inFnd:inEmp#:inDiv) pem01;
       If ELarsm = 'Y' and EMstrp <> 'Y';
          Return *off;
       Endif;
      /end-free
     c*
     c     f2ky          chain     pf201
     c                   if        %found(wdf2l) and
     c                             (F2CPMT = *blanks and F2CCYF = 'Y')
     c                   eval      xpFlag = *on
     c                   endif
     c*
     c                   return    xpflag
     c*
     p getAutoCash     e
       //===================================================
       // Returns *On if policy is set up for EFT
       //===================================================
      /Free
       Dcl-Proc IsEFT;
       Dcl-Pi *N Ind;
       End-Pi;
       Dcl-S strEfDt Char(7);
       Dcl-S chrEftType Char(1);
       Dcl-S strEftKey Char(30);
       Dcl-S chrPre Char(1);
       Dcl-S strRrn Char(9);
       Dcl-S strEobTyp Char(3);
       //***  Begin Add  ***  03/08/22  ********************
       Dcl-S strFyr Char(3);
       //***  End   Add  ***  03/08/22  ********************
       Dcl-S strCkTp Char(10);
       //*** beg chg ***** 04/06/21 ***********************
       //Setgt (xxCo#:xxFnd:xxEmp#:xxDiv) pef01;
       //Dou %Eof(WMEFL6);
       //   Readpe (xxCo#:xxFnd:xxEmp#:xxDiv) pef01;
       //   If %Eof;
       //       Leave;
       //   Endif;
       //   Select;
       //   When xxCurd >= EFefdt
       //    And (EFexdt = 0 Or xxCurd < EFexdt);
       //      Return *On;
       //   EndSl;
       //Enddo;
       //Return *Off;
       chrEftType = 'E';
       strEfDt = %Editc(xxEfDt:'X');
       //***  Begin Chg  ***  03/08/22  ********************
       //strEftKey = qqEmp# + qqDiv;
       //hasEFT =  GetEftStatus(qqCo#:chrEftType:QQfnd:
       // strEftKey:strEfDt:chrPre:strRrn:strEobTyp:
       // strCkTp:qqFyr:chrEnv);
       strEftKey = qqEmp# + %editc(xxDiv:'X');
       strFyr = %editc(xxFyr:'X');
       hasEFT =  GetEftStatus(qqCo#:chrEftType:QQfnd:
        strEftKey:strEfDt:chrPre:strRrn:strEobTyp:
        strCkTp:strFyr:chrEnv);
       //***  End   Chg  ***  03/08/22  ********************
       Return hasEFT;
       //*** end chg ***** 04/06/21 ***********************
       End-Proc;
       //===================================================
       // Returns *On if policy is set up for 3rd Party
       //===================================================
       Dcl-Proc IsThirdParty;
       Dcl-Pi *N Ind;
       End-Pi;
       //*** beg chg ***** 04/06/21 ***********************
       //Setgt (xxCo#:xxFnd:xxEmp#:xxDiv:'S/R Online':xxCurd) pp300;
       //Dou %Eof(WMP3L);
       //   Readpe (xxCo#:xxFnd:xxEmp#:xxDiv:'S/R Online') pp300;
       //   If %Eof;
       //       Leave;
       //   Endif;
       //   Select;
       //   When P3enbld <> 'Y';
       //   When (P3exdt = 0 Or xxCurd < P3exdt);
       //      Return *On;
       //   EndSl;
       //Enddo;
       //Return *Off;
         Dcl-S chrSrd Char(1);
         Dcl-S strType Char(100) Inz('S/R Online');
         has3rdParty = Has3rdPartyAcct(xxCo#:xxFnd:xxEmp#:
          xxDiv:strType:xxCurd:xxFyr:chrEnv:chrSrd);
         Return has3rdParty;
       //*** end chg ***** 04/06/21 ***********************
       End-Proc;
      /End-Free
** stkPgms
WTPOLDTL
WPILOCMST
WTPOLESTBL
WIALERTS
WISRMAIL
