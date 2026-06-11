     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4076R - Unearned Premium Report
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 USE ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                 FIELD FNANAM IF AVAILABLE.                    *
     H*                                                               *
     H*  02/10/99 C:113 PROJECT: SUPPORT WA025 #16400                 *
     H*                 Changed to evaluate all policy effective and  *
     H*                 cancellation date ranges when determining     *
     H*                 cancelled status.                             *
     H*                                                               *
     H* 12/24/98  C:35   PROJECT: SUPPORT WA033 15563                 *
     H*                  TOTALS APPEARED INCORRECT DUE TO NEGATIVES   *
     H*                  NOT PRINTING.                                *
     H*                                                               *
     H*  08/25/98 C:09 PROJECT SUPPORT  WA093  SEQ# 13075             *
     H*                EXCLUDE ANY ACCOUNTS WITH POLICY EFFECTIVE     *
     H*                DATES AFTER THE RUN DATE.  ACCOUNT FOR ANY     *
     H*                ROUNDING ISSUES (+/- 1.00) BY ADJUSTING        *
     H*                THE UNEARNED PREMIUM NUMBER.  IF MINIMUM PREM  *
     H*                DO NOT PRORATE NORMAL PREMIUM.                 *
     H*                                                               *
     H*  08/27/93 C:10 PROJ: WC260                                    *
     H*                CHANGED TO ALLOW FOR UP TO 999 BILLING SCHEDULE*
     H*                CODES                                          *
     H*****************************************************************
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* Output Routing Master File
     FSMORL     IF   E           K DISK
     F*
     F* User Parameter Master File                                    Z
     FWMUPL     IF   E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F* Employer Detail File
     FWDELL1    IF   E           K DISK
     F*
     F* Employer Master File
     FWMEML     IF   E           K DISK
     F*
     FWTABL     IF   E           K DISK
     F* Output Report File
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

     D CD              S              3  0 DIM(999)                             BILLING SCHED CODES
     D NP              S             11  2 DIM(999)                             NORMAL PREM TL
     D EP              S             11  2 DIM(999)                             EARNED PREM TL
     D UP              S             11  2 DIM(999)                             UNEARN PREM TL
     D PP              S             11  2 DIM(999)                             PRORATED PREM TTL
     D BT              S              1    DIM(999)                             BS TYPE
     D*** BEGIN ADD *** 02/10/99 **************************************
     D*
     D*  Cancel & Effective date arrays
     D*
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*** END   ADD *** 02/10/99 **************************************
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D* External Program File Information
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Define New Field XXKEY
     C*
     C     *LIKE         DEFINE    BSKEY         XXKEY
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQQUE            10
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
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C* Set-Up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDATE        XXRNDT            8 0
     C*** BEGIN ADD *** 02/10/99 **************************************
     C                   Z-ADD     0             I1                3 0
     C                   Z-ADD     0             XXCAND            8 0
     C*** END   ADD *** 02/10/99 **************************************
     C*
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        Demand Mode
     C                   ELSE
     C                   SETON                                        08        Batch Mode
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the Output Routing File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Fund Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Employer Detail File
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* KEY LIST FOR BSCODE
     C*
     C     TBKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C* Get the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* Get the Output Routing Record
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C***** BEGIN DEL **** 04/10/00 ***********************************
     C* CENTER TEXT IN FIELD CONAME
     C*
     C**********           MOVELCONAME    ##DATA
     C*
     C**********           CALL 'SRCENTER'
     C**********           PARM           ##DATA 80
     C**********           PARM 40        ##LENG  30
     C*
     C**********           MOVEL##DATA    XXNAME 40
     C***** END   DEL **** 04/10/00 ***********************************
     C*
     C* CENTER TEXT IN FIELD ORTITL
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C*
     C* Print the Distribution Information
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   MOVE      '1'           *INOA
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* Get the Fund Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*  CHECK FUND MASTER FOR ALTERNATE COMPANY NAME
     C*
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL(P)  FNANAM        CONAME
     C                   ENDIF
     C*
     C* CENTER TEXT IN FIELD CONAME
     C*
     C                   MOVEL     CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXNAME           40
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C* Get the Billing Schedule Code
     C*
     C     TBKEY         CHAIN     PBS01                              60
     C*
     C* Initialize Detail Fields
     C*
     C                   Z-ADD     0             CD                             CODE ARRAY
     C                   Z-ADD     0             XXENMP           11 2          Est Ann Prem
     C                   Z-ADD     0             XXFNPR           11 2          Fin Ern Prem
     C                   Z-ADD     0             XXUNPR           11 2          Unearnd Prem
     C                   Z-ADD     0             XXPRON           11 2          Prorated Nor
     C*
     C* Initialize Total Fields
     C*
     C                   Z-ADD     0             NP                             NORMAL PREM
     C                   Z-ADD     0             EP                             EARNED PREM
     C                   Z-ADD     0             UP                             UNEARND PREM
     C                   Z-ADD     0             PP                             PRORATE PREM
     C                   Z-ADD     0             XXANTL           11 2          Est Ann Prem
     C                   Z-ADD     0             XXFNTL           11 2          Fin Ern Prem
     C                   Z-ADD     0             XXUNTL           11 2          Unearnd Prem
     C                   Z-ADD     0             XXPRTL           11 2          Prorate Prem
     C*
     C*   LOAD BILLING SCHEDULE CODES INTO ARRAY
     C*
     C                   Z-ADD     1             X                 3 0
     C     TBKEY         CHAIN     PBS01                              50
     C     *IN50         DOWEQ     '0'
     C     X             ANDLT     1000
     C                   MOVEL     BSKEY         XXCODE            3 0
     C                   Z-ADD     XXCODE        CD(X)
     C                   ADD       1             X
     C     TBKEY         READE     PBS01                                  50
     C                   END
     C*
     C* Get the Employer Detail Record
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C****** BEGIN ADD ***** 08/25/98 *********************************
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRND2            7 0
     C     ELPFDT        IFLE      XXRND2
     C****** END ADD ******* 08/25/98 *********************************
     C*
     C* Get the Employer Master Record
     C*
     C     EMKEY         CHAIN     PEM01                              61
     C     *IN61         IFEQ      '0'
     C*
     C* Converts Effective Date To Proper Format
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFDT            8 0
     C*
     C* Convert Cancel Date
     C*
     C*** BEGIN CHG *** 02/10/99 **************************************
     C                   Z-ADD     1             I1
     C*
     C     *ZERO         LOOKUP    EF(I1)                                 50
     C*
     C     *IN50         IFEQ      *ON
     C                   SUB       1             I1
     C                   ELSE
     C                   Z-ADD     5             I1
     C                   ENDIF                                                  *IN50 IFEQ *ON
     C*
     C     I1            IFNE      0
     C*
     C                   Z-ADD     CX(I1)        SRDTI
     C**********           Z-ADDEMCAD1    SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C**********           MOVELSRDTO     XXCAD1  80
     C                   MOVEL     SRDTO         XXCAND
     C*
     C                   ENDIF                                                  I1 IFNE 0
     C*** END   CHG *** 02/10/99 **************************************
     C*
     C* Convert System Date
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C* If employer has been auditted use the auditted annual premium;
     C* otherwise, use the estimated annual premium
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELANMP        XXENMP
     C                   ELSE
     C                   Z-ADD     ELENMP        XXENMP
     C                   END
     C*
     C* GET RESPECTIVE BS CODE TYPE FOR EMPLOYER
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKEY1        CHAIN     PBS01                              50
     C     *IN50         IFNE      '0'
     C                   MOVEL     *BLANKS       BSSEFL
     C                   ENDIF                                                  ENDIF *IN50
     C*
     C* Call Sub-program to Calculate the Financial Earned Prem to Date
     C*
     C                   MOVE      *BLANKS       SRCO#             3
     C                   MOVE      *BLANKS       SRFND             3
     C                   MOVE      *BLANKS       SRFYR             3
     C                   MOVE      *BLANKS       SREMP#            9
     C                   MOVE      *BLANKS       SRDIV             5
     C                   MOVE      *BLANKS       SRDATE            8
     C                   MOVE      *ZEROS        SRAMT            11
     C                   MOVE      *ZEROS        SRAMT2           11
     C                   MOVE      *BLANK        SRTERM            1
     C*
     C                   MOVE      XXCO#         SRCO#
     C                   MOVE      XXFND         SRFND
     C                   MOVE      XXFYR         SRFYR
     C                   MOVE      ELEMP#        SREMP#
     C                   MOVE      ELDIV         SRDIV
     C                   MOVE      XXRNDT        SRDATE
     C*
     C                   CALL      'SRPRT2'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    SRDATE
     C                   PARM                    SRAMT
     C                   PARM                    SRAMT2
     C                   PARM                    SRTERM
     C*
     C* Move Calculated Financial Earned Prem to Detail Field
     C*
     C                   MOVE      SRAMT         XXFNPR
     C*
     C* Call Sub-program to Calculate the Unearned Earned Prem to Date
     C*
     C                   MOVE      *BLANKS       SRCO#
     C                   MOVE      *BLANKS       SRFND
     C                   MOVE      *BLANKS       SRFYR
     C                   MOVE      *BLANKS       SREMP#
     C                   MOVE      *BLANKS       SRDIV
     C                   MOVE      *BLANKS       SRDATE
     C                   MOVE      *ZEROS        SRAMT
     C                   MOVE      *BLANK        SRTERM
     C*
     C                   MOVE      XXCO#         SRCO#
     C                   MOVE      XXFND         SRFND
     C                   MOVE      XXFYR         SRFYR
     C                   MOVE      ELEMP#        SREMP#
     C                   MOVE      ELDIV         SRDIV
     C                   MOVE      XXRNDT        SRDATE
     C*
     C                   CALL      'SRUEPR'
     C                   PARM                    SRCO#
     C                   PARM                    SRFND
     C                   PARM                    SRFYR
     C                   PARM                    SREMP#
     C                   PARM                    SRDIV
     C                   PARM                    SRDATE
     C                   PARM                    SRAMT
     C                   PARM                    SRTERM
     C*
     C* Move Calculated Unearned Prem to Detail Field
     C*
     C                   MOVE      SRAMT         XXUNPR
     C*
     C* Calculate Prorated Normal Premium (XXPRON)
     C*
     C                   MOVE      ELPROR        XXPROR            5 3
     C*** BEGIN ADD ***  08/04/98  ************************************
     C     ELFAF         IFEQ      'A'
     C****** BEGIN ADD ***** 08/25/98 *********************************
     C     ELEMNP        OREQ      'Y'
     C****** BEGIN ADD ***** 08/25/98 *********************************
     C                   Z-ADD     XXENMP        XXPRON
     C                   ELSE
     C*** END ADD *****  08/04/98  ************************************
     C     XXENMP        MULT      XXPROR        XXPRON           11 2
     C*** BEGIN ADD ***  08/04/98  ************************************
     C                   ENDIF                                                  ELFAF IFEQ
     C*** END ADD *****  08/04/98  ************************************
     C*
     C* Check for Page Overflow
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C* Print the Detail Line
     C*
     C****** BEGIN ADD ***** 08/25/98 *********************************
     C*
     C*  ADJUST FOR ROUNDING
     C*
     C     XXFNPR        ADD       XXUNPR        XXTTL            11 2
     C     XXTTL         SUB       XXPRON        XXDIFF           11 2
     C     XXDIFF        IFLE      1.50
     C     XXDIFF        ANDGE     -1.50
     C                   SUB       XXDIFF        XXUNPR
     C                   END
     C****** END ADD ******* 08/25/98 *********************************
     C                   EXCEPT    DETL
     C*
     C* Add Detail Fields to Total Fields
     C*
     C                   ADD       XXENMP        XXANTL
     C                   ADD       XXFNPR        XXFNTL
     C                   ADD       XXUNPR        XXUNTL
     C                   ADD       XXPRON        XXPRTL
     C                   Z-ADD     1             X
     C     ELBSCD        LOOKUP    CD(X)                                  50
     C     *IN50         IFEQ      '1'
     C                   ADD       XXENMP        NP(X)
     C                   ADD       XXFNPR        EP(X)
     C                   ADD       XXUNPR        UP(X)
     C                   ADD       XXPRON        PP(X)
     C                   END
     C*
     C                   END                                                    Emp Mast End
     C*
     C* Clear Detail Fields
     C*
     C                   Z-ADD     0             XXENMP
     C                   Z-ADD     0             XXFNPR
     C                   Z-ADD     0             XXUNPR
     C*
     C****** BEGIN ADD ***** 08/25/98 *********************************
     C                   END                                                    ELPFDT<=XXDATE
     C****** END ADD ******* 08/25/98 *********************************
     C*
     C* Get the Next Employer Detail Record
     C*
     C     ELKEY         READE     PEL01                                  60
     C                   END
     C*
     C* Check for Page Overflow
     C*
     C                   MOVE      '1'           *IN12
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C*
     C* Print Report Totals
     C*
     C                   EXCEPT    TOTL
     C*
     C                   Z-ADD     1             X
     C     X             DOWLT     1000
     C     CD(X)         ANDNE     *ZERO
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C     NP(X)         IFNE      0
     C     EP(X)         ORNE      0
     C     UP(X)         ORNE      0
     C     PP(X)         ORNE      0
     C                   EXCEPT    TOTB
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C* End the Program
     C*
      /Free
       // ***** Beg Add ***** 08/26/14 ***********************************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ***********************************************
      /End-free
     C                   MOVE      '1'           *INLR
     O*
     O* Output File Specs
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O* Heading Line Specs
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXNAME             119
     O                       XXDATE        Y    197
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          191 'PAGE:'
     O                       PAGE          Z    197
     O          E            HEAD           1
     O                                          186 'RUN DATE:'
     O                       XXRNDT             197 '  /  /    '
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O                                          192 'POLICY YEAR:'
     O                       XXFYR              197
     O          E            HEAD           1
     O                                           73 'BILLING'
     O                                          120 'FINANCIAL'
     O                                          157 'PRORATED'
     O                 N12                      169 'AUDIT'
     O          E            HEAD           2
     O                 N12                        9 'EMPLR'
     O                 N12                       18 'DIV'
     O                 N12                       35 'EMPLOYER NAME'
     O                                           76 'SCHED CODE'
     O                 N12                       84 'TYPE'
     O                                          103 'NORMAL PREM'
     O                                          122 'EARNED PREM'
     O                                          141 'UNEARNED PREM'
     O                                          160 'NORMAL PREM'
     O                 N12                      168 'FLAG'
     O                 N12                      182 'EFF DATE'
     O                 N12                      197 'CANCEL DATE'
     O*
     O* Detail Line Specs
     O*
     O          E            DETL           1
     O                       ELEMP#        Z     09
     O                       ELDIV         Z     18
     O                       EMNAME              62
     O                       ELBSCD        Z     73
     O                       BSSEFL              83
      **** BEGIN CHG **** 12/24/98 *********************************************
     O************************ XXENMP1  103
     O************************ XXFNPR1  122
     O************************ XXUNPR1  141
     O************************ XXPRON1  160
     O                       XXENMP        J    104
     O                       XXFNPR        J    123
     O                       XXUNPR        J    142
     O                       XXPRON        J    161
      **** END   CHG **** 12/24/98 *********************************************
     O                       ELFAF              167
     O                       XXPFDT             182 '  /  /    '
     O                       XXCAND             197 '  /  /    '
     O*
     O* Total Line Specs
     O*
     O          E            TOTL           2 15
     O                                           60 'GROUP TOTALS:'
      **** BEGIN CHG **** 12/24/98 *********************************************
     O************************ XXANTL1  103
     O************************ XXFNTL1  122
     O************************ XXUNTL1  141
     O************************ XXPRTL1  160
     O                       XXANTL        J    103
     O                       XXFNTL        J    122
     O                       XXUNTL        J    141
     O                       XXPRTL        J    160
      **** END   CHG **** 12/24/98 *********************************************
     O          E            TOTL           1
     O                                           52 'BILLING SCHEDULE CODE'
     O                                           60 'TOTALS:'
     O*
     O          E            TOTB           1
     O                       CD(X)         Z     73
      **** BEGIN CHG **** 12/24/98 *********************************************
     O************************ NP,X  1  103
     O************************ EP,X  1  122
     O************************ UP,X  1  141
     O************************ PP,X  1  160
     O                       NP(X)         J    103
     O                       EP(X)         J    122
     O                       UP(X)         J    141
     O                       PP(X)         J    160
      **** END   CHG **** 12/24/98 *********************************************
