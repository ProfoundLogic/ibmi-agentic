     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4076R1 - A/R Unearned Premium Report
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS REPORT IS SIMILAR TO THE REGULAR UNEARNED PREMIUM  **
     H**     REPORT EXCEPT THAT UNEARNED PREMIUM IS CALCULATED       **
     H**     AS FOLLOWS:                                             **
     H**        ACCUMULATION OF ALL 'B' (EXCL EXP CONST), 'D', AND   **
     H**        'A' A/R TRANSACTIONS FOR THE SPECIFIED EMPLR/FYR     **
     H**        MINUS FINANCIAL EARNED PREMIUM.                      **
     H**                                                             **
     H**   CALLED FROM: W4075R1                                      **
     H**   CALLS TO:    W4076C1                                      **
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*     08   -      BATCH MODE INDICATOR                          *
     H*     09   -      DEMAND MODE INDICATOR                         *
     H*     10   -      NO RECORD MEETS SELECTION CRITERIA            *
     H*     50   -      GENERIC TABLE SEARCH                          *
     H*     60   -      GENERAL CHAINING                              *
     H*     61   -      CHAIN TO EMPLOYEE MASTER                      *
     H*     65   -      LOKUP INDICATOR FOR CANCEL DATE               *
     H*****************************************************************
     H** FILE USAGE:                                                  *
     H*                                                               *
     H*     WMUPL  (PUP01) USER                                       *
     H*     WMFNL  (PFN01) CO#/FND                                    *
     H*     SMCOL (PCO01): CO#                                        *
     H*     SMORL (POR01): NAM/FIL#                                   *
     H*     WDELL1(PEL01): CO#/FND/FYR/EMP#/DIV                       *
     H*     WMEAL4(PEA01): CO#/FND/EMP#/DIV/FYR/ARTY/PODT             *
     H*     WMBXL1(PBX01): CO#/BSCD                                   *
     H*     WMEML (PEM01): CO#/FND/EMP#/DIV                           *
     H*     WDF1L (PF101): CO#/FND/FYR                                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/10/00 C:118 PROJECT: WC987                                *
     H*                 USE ALTERNATE COMPANY NAME FROM THE FUND      *
     H*                 MASTER IF AVAILABLE.                          *
     H*                                                               *
     H*  01/27/97 C:46 PROJECT: WC622                                 *
     H*                CORRECT CHAIN TO CHAIN TO BILLING SCHEDULE     *
     H*                XREF FILE TO INCLUDE FUND.                     *
     H*                                                               *
     H*****************************************************************
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* Output Routing Master File
     FSMORL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Fund Master File
     FWMFNL     IF   E           K DISK
     F*
     F* Employer Detail File
     FWDELL1    IF   E           K DISK
     F*
     F* Employer A/R File
     FWMEAL4    IF   E           K DISK
     F*
     F* Billing Schd File
     FWMBXL1    IF   E           K DISK
     F*
     F* Employer Master File
     FWMEML     IF   E           K DISK
     F*
     FWTABL     IF   E           K DISK
     F*
     F* Fund Detail File
     FWDF1L     IF   E           K DISK
     F* Output Report File
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

     D CD              S              3  0 DIM(999)                             BILLING SCHED CODES
     D NP              S             11  2 DIM(999)                             NORMAL PREM TL
     D PN              S             11  2 DIM(999)                             PRORATED PREM TL
     D EP              S             11  2 DIM(999)                             EARNED PREM TL
     D UP              S             11  2 DIM(999)                             UNEARN PREM TL
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5) ASCEND
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CN                     1    035  0
     D                                     DIM(5) ASCEND
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D*
     D* External Program File Information
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Entry Parameters
     C*
     C     *LIKE         DEFINE    ELFAF         XXFAF
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    ELPFDT        XXCNDT
     C     *LIKE         DEFINE    ELPFDT        XXRUDT
     C     *LIKE         DEFINE    ELPFDT        XXDATE
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
     C                   PARM      *BLANKS       SRTFMT
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Set-Up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDATE        X1RNDT            8 0
     C*
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXRNDT            8 0
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        Demand Mode
     C                   ELSE
     C                   SETON                                        08        Batch Mode
     C                   END
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDAT             8 0
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
     C* Key List for the Fund Detail File
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* Key List for the Employer Detail File
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  BILLING SCHEDULE X-REF FILE
     C*
     C     BXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBSCD           10
     C*
     C*  ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C* * * BILLING SCHEDULE TABLE FILE
     C     BSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBSCD
     C*
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
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
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
     C* Print the Distribution Information
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
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
     C***** BEGIN DEL **** 04/10/00 ***********************************
     C**If alternate name (FNANAM) isn't blank, print it in lieu of fund name.
     C**********
     C********** FNANAM    IFNE *BLANKS
     C**********
     C**********           MOVELFNANAM    XXFNNM 50
     C**********
     C**********           ELSE
     C***** END   DEL **** 04/10/00 ***********************************
     C*
     C                   MOVEL     FNNAME        XXFNNM           50
     C*
     C***** BEGIN DEL **** 04/10/00 ***********************************
     C**********           ENDIF                           FNANAM IFEQ *BLANKS
     C***** END   DEL **** 04/10/00 ***********************************
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*
     C*  IF ALTERNATE COMPANY NAME EXISTS IN FUND MASTER USE
     C*     INSTEAD OF COMPANY NAME
     C*
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C*
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C* Get the Fund Detail File
     C*
     C     F1KEY         CHAIN     PF101                              60
     C*
     C* Center Company Name and Screen Title
     C*
     C                   MOVEL     CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXCONM           40
     C*
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C*
     C* Initialize Detail Fields
     C*
     C                   Z-ADD     0             CD                             CODE ARRAY
     C                   Z-ADD     0             XXENMP           11 2          Normal Prem
     C                   Z-ADD     0             XXPRON           11 2          Prorated Normal Prem
     C                   Z-ADD     0             XXFNPR           11 2          Fin Ern Prem
     C                   Z-ADD     0             XXUNPR           11 2          Unearnd Prem
     C*
     C* Initialize Total Fields
     C*
     C                   Z-ADD     0             NP                             NORMAL PREM
     C                   Z-ADD     0             PN                             PRORATED NORMAL PREM
     C                   Z-ADD     0             EP                             EARNED PREM
     C                   Z-ADD     0             UP                             UNEARND PREM
     C                   Z-ADD     0             XXANTL           11 2          Est Ann Prem
     C                   Z-ADD     0             XXPROT           11 2          PRORATED NORMAL PREM
     C                   Z-ADD     0             XXFNTL           11 2          Fin Ern Prem
     C                   Z-ADD     0             XXUNTL           11 2          Unearnd Prem
     C                   Z-ADD     0             EF                             EFF DATE
     C                   Z-ADD     0             CN                             CANCEL DATE
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
     C*
     C* If no record found.
     C*
     C     *IN60         IFEQ      *ON
     C*
     C                   MOVE      *ON           *IN10
     C*
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C* Get the Employer Master Record
     C*
     C     EMKEY         CHAIN     PEM01                              61
     C     *IN61         IFEQ      '0'
     C*
     C     ELFAF         IFEQ      'A'
     C                   Z-ADD     ELANMP        XXENMP
     C                   ELSE
     C                   Z-ADD     ELENMP        XXENMP
     C                   END
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
     C                   MOVE      X1RNDT        SRDATE
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
     C* Calculate Prorated Normal Premium (XXPRON)
     C*
     C                   MOVE      ELPROR        XXPROR            5 3
     C     XXENMP        MULT      XXPROR        XXPRON           11 2
     C*
     C* Move Calculated Financial Earned Prem to Detail Field
     C*
     C                   MOVE      SRAMT         XXFNPR
     C*
     C*  CHECK IF EMPLOYER IS CANCELLED
     C*
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    EF(Y)                                  65
     C     *IN65         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   ENDIF
     C     Y             DOWNE     0
     C     ELPTDT        COMP      EF(Y)                              65  65
     C  N65              SUB       1             Y
     C  N65              ENDDO
     C*
     C                   Z-ADD     EF(Y)         XXEFDT
     C                   Z-ADD     CN(Y)         XXCNDT
     C*
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRUDT
     C*
     C     XXCNDT        IFLT      XXRUDT
     C     XXCNDT        ANDNE     *ZERO
     C                   Z-ADD     XXCNDT        XXDATE
     C                   ELSE
     C                   Z-ADD     XXRUDT        XXDATE
     C                   END
     C*
     C*
     C                   MOVE      'B'           XXARTY
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     EAPODT        IFLE      XXDATE
     C     TTCTYP        ANDNE     'E'
     C                   ADD       EAAMT         XXEAMT           11 2
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO                                                   *IN60 = 0
     C*
     C                   MOVE      'A'           XXARTY
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XXEAMT
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO
     C*
     C                   MOVE      'D'           XXARTY
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C     EAPODT        IFLT      XXDATE
     C                   ADD       EAAMT         XXEAMT
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO
     C*
     C                   MOVE      *BLANKS       XXBSCD
     C                   MOVEL     ELBSCD        XXBSCD
     C     BSKY1         CHAIN     PBS01                              60
     C   60              MOVE      *BLANKS       BSSEFL
     C     BXKEY         CHAIN     PBX01                              60
     C   60              MOVE      *BLANKS       BXTYPE
     C*
     C     ELFAF         IFEQ      'A'
     C     BSSEFL        OREQ      'S'
     C                   Z-ADD     *ZEROS        XXUNPR
     C                   ELSE
     C     XXEAMT        SUB       XXFNPR        XXUNPR
     C                   END
     C*
     C     ELFAF         IFNE      'A'
     C                   MOVE      *BLANKS       XXFAF
     C                   ELSE
     C                   MOVE      ELFAF         XXFAF
     C                   END
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFD2            8 0
     C*
     C                   Z-ADD     XXCNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCND2            8 0
     C*
     C*****************************************************************
     C*
     C* Check for Page Overflow
     C*****************************************************************
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C* Print the Detail Line
     C*
     C                   EXCEPT    DETL
     C*
     C* Add Detail Fields to Total Fields
     C*
     C                   ADD       XXENMP        XXANTL
     C                   ADD       XXPRON        XXPROT
     C                   ADD       XXFNPR        XXFNTL
     C                   ADD       XXUNPR        XXUNTL
     C                   Z-ADD     1             X
     C     ELBSCD        LOOKUP    CD(X)                                  50
     C     *IN50         IFEQ      '1'
     C                   ADD       XXENMP        NP(X)
     C                   ADD       XXPRON        PN(X)
     C                   ADD       XXFNPR        EP(X)
     C                   ADD       XXUNPR        UP(X)
     C                   END
     C*
     C                   END                                                    Emp Mast End
     C*
     C* Clear Detail Fields
     C*
     C                   Z-ADD     0             XXEAMT
     C                   Z-ADD     0             XXENMP
     C                   Z-ADD     0             XXFNPR
     C                   Z-ADD     0             XXUNPR
     C*
     C* Get the Next Employer Detail Record
     C*
     C     ELKEY         READE     PEL01                                  60
     C                   END                                                    *IN60 DOWEQ *ZERO
     C*
     C* Check for Page Overflow
     C*
     C                   MOVE      '1'           *IN12
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C*
     C* If no record was found
     C*
     C     *IN10         IFEQ      *ON
     C*
     C                   EXCEPT    NOREC
     C                   MOVE      *OFF          *IN10
     C*
     C                   ENDIF                                                  *IN10 IFEQ *ON
     C*
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
     C     PN(X)         ORNE      0
     C     EP(X)         ORNE      0
     C     UP(X)         ORNE      0
     C                   EXCEPT    TOTB
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C* End the Program
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*****************************************************************
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
     O                       ORTITL              80
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
     O          E            ROUTE          2
     O                                           18 'POLICY YEAR:'
     O                       XXFYR               22
     O                                           40 'RUN DATE:'
     O                       X1RNDT        Y     51
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O                                         +000 '(C)'
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
     O                       XXCONM             119
     O                       XXDAT              190 '  /  /    '
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       XXTITL             129
     O                                          179 'PAGE:'
     O                       PAGE          Z    190
     O          E            HEAD           1
     O                                          179 'RUN DATE:'
     O                       X1RNDT        Y    190
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       XXFNNM              71
     O                                          179 'POLICY YEAR:'
     O                       XXFYR              190
     O          E            HEAD           1
     O                                          109 'BILLING SCHEDULE'
     O                 N12                      119 'AUDIT'
     O                                          153 'PRORATED'
     O                                          172 'FINANCIAL'
     O                                          186 'UNEARNED'
     O          E            HEAD           2
     O                 N12                        9 'EMPLR'
     O                 N12                       16 'DIV'
     O                 N12                       31 'EMPLOYER NAME'
     O                 N12                       72 'EFF DT'
     O                 N12                       87 'CX DT'
     O                                           97 'CODE'
     O                 N12                      109 'TYPE  PLAN'
     O                 N12                      118 'FLAG'
     O                                          138 'NORMAL PREM'
     O                                          156 'NORMAL PREM'
     O                                          174 'EARNED PREM'
     O                                          190 'A/R BILLINGS'
     O*
     O          E            NOREC          1
     O                                           25 'NO RECORDS MEET'
     O                                           44 'SELECTION CRITERIA'
     O*
     O* Detail Line Specs
     O*
     O          E            DETL           1
     O                       EMEMP#        Z      9
     O                       EMDIV         Z     16
     O                       EMNAME              58
     O                       XXEFD2              72 '  /  /    '
     O                       XXCND2              87 '  /  /    '
     O                       ELBSCD        Z     97
     O                       BSSEFL             102
     O                       BXTYPE             108
     O                       XXFAF              117
     O                       XXENMP        B    138
     O                       XXPRON        B    156
     O                       XXFNPR        B    174
     O                       XXUNPR        B    190
     O*
     O* Total Line Specs
     O*
     O          E            TOTL           2 15
     O                                           83 'GROUP TOTALS:'
     O                       XXANTL        B    138
     O                       XXPROT        B    156
     O                       XXFNTL        B    174
     O                       XXUNTL        B    190
     O          E            TOTL           1
     O                                           75 'BILLING SCHEDULE CODE'
     O                                           83 'TOTALS:'
     O*
     O          E            TOTB           1
     O                       CD(X)         Z     97
     O                       NP(X)         B    138
     O                       PN(X)         B    156
     O                       EP(X)         B    174
     O                       UP(X)         B    190
