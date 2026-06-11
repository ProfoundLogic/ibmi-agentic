     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H/TITLE W4106R - STD PREM AT VARIABLE PERCENTAGES
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  12/14/99  C:118 PROJECT: WC577                               *
     H*                  POLICY YEAR AND STANDARD PREMIUM PERCENTAGE  *
     H*                  AMOUNT IS NOT PRINTING.                      *
     H*                                                               *
     H*  10/27/97  C:86  VERSION 3 PROJECT: WC577                     *
     H*            REVISED TO PRINT THE ALTERNATE FUND NAME IN PLACE  *
     H*            OF COMPANY NAME, IF ONE EXISTS.                    *
     H*****************************************************************
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Output Routing Master File
     FSMORL     IF   E           K DISK
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
     F* Printer Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*
     D* Employers' Effective and Cancel Dates
     D*
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* Cancel Date Array
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D*
     D* Effective Date Array
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D*
     D*
     D*
     D* External Program Data Structures
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Output Routing File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
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
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRPCT            7
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
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
     C* Set-up and Initialize
     C*
     C                   MOVE      QQCO#         XXCO#             3 0          Company
     C                   MOVE      QQFND         XXFND             3 0          Fund
     C                   MOVE      QQFYR         XXFYR             3 0          Fund Year
     C***** BEGIN DEL **** 12/14/99 ***********************************
     C**********           MOVE QQFYR     XXFYR2  20       Output Fn Yr
     C***** END   DEL **** 12/14/99 ***********************************
     C                   MOVE      QQRPCT        XXRPCT            7 2          Requested %
     C                   MOVE      QQRPCT        XXPER             7 2          Output %
     C                   MOVE      QQMODE        XXMODE            1            Mode
     C     XXRPCT        DIV       100           XXRPCT                         Work field %
     C                   TIME                    DSDTTM                         Date & Time
     C                   Z-ADD     0             TTLPRM           11 2          Total Prem
     C                   Z-ADD     0             TTLEMP            7 0          Total Emplyr
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        Demand Mode
     C                   ELSE
     C                   SETON                                        08        Batch Mode
     C                   END
     C*
     C* Get the User Parameter Master Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* Get the Company Master Record
     C*
     C     COKEY         CHAIN     SMCOL                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C* Get the Routing Master Record
     C*
      ***** Beg Del ***** 08/26/14 *****************************
     C**********         MOVE      '0'           XXPRT
     C**********     ORKEY         CHAIN     POR01                              60
     C**********     *IN60         IFEQ      '0'
     C*
     C* Print the Distribution Information
     C*
     C************       EXCEPT    ROUTE
     C************       END
      ***** End Del ***** 08/26/14 *****************************
     C*
     C                   MOVE      '1'           *INOF
     C*
     C* Get the Fund Master Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*** BEGIN ADD *** 10/27/97 *******************************
     C     *IN60         IFEQ      '0'
     C     FNANAM        IFNE      *BLANK
     C                   MOVEL(P)  FNANAM        ##DATA
     C                   ELSE
     C                   MOVEL(P)  CONAME        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XCNAME           60
     C                   ENDIF
     C*
     C*** END ADD ***** 10/27/97 ********************************
     C*
     C* Begin Reading the Employer Detail File
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Initialize Work Fields
     C*
     C                   Z-ADD     0             XXPREM           11 2
     C                   Z-ADD     0             XXCNDT            6 0
     C*
     C* Get the Employer Master Record
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* Check for a Cancel Date for this Employer.
     C*
     C                   Z-ADD     1             X                 3 0
     C     *ZERO         LOOKUP    ED(X)                                  55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   END
     C*
     C                   Z-ADD     1             Y                 3 0
     C     *ZERO         LOOKUP    CD(Y)                                  55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C*
     C     Y             IFNE      0
     C     CD(Y)         ANDGE     ED(X)
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCNDT
     C                   END
     C*
     C* IF MINIMUM PREM, DO NOT MULT BY PERCENTAGE
     C*
     C* If employer is auditted use the auditted standard premium field
     C* otherwise use estimate auditted premium field.
     C* Take the standard premium field and multiply by the requested
     C* percentage to get the output premium field xxprem.
     C*
     C     ELFAF         IFEQ      'A'                                          JJJ 9-29-90
     C                   Z-ADD     ELASPR        XXPREM                         |
     C                   ELSE                                                   |
     C                   Z-ADD     ELSTPR        XXPREM                         |
     C                   END                                                    |
     C*                                                    |
     C     ELEMNP        IFNE      'Y'                                          |
     C                   MULT(H)   XXRPCT        XXPREM                         |
     C                   END                                                    |
     C*** BEGIN ADD *** JJJ 12-21-90
     C*
     C*  ROUND PREMIUM
     C*
     C     XXPREM        MULT(H)   1             XXWK90            9 0
     C                   Z-ADD     XXWK90        XXPREM
     C*** END ADD *** JJJ 12-21-90
     C*
     C* Add 1 to the total number of members.
     C*
     C                   ADD       1             TTLEMP
     C*
     C* Add calculated premium field to total field
     C*
     C                   ADD       XXPREM        TTLPRM
     C*
     C* Print detail line after checking for overflow
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DETL
     C*
     C                   END
     C*
     C* Get the Next Employer Detail Record
     C*
     C     ELKEY         READE     PEL01                                  60
     C     *IN60         IFEQ      '1'
     C     TTLEMP        ANDEQ     0
     C                   EXCEPT    DETL2
     C                   END
     C                   END
     C*
     C* Print total line after check for page overflow.
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    TOTL
     C*
     C* End the Program
     C*
      /Free
       //***** Beg Add ***** 08/26/14 *****************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 *****************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*****************************************************************
     O*
     O* Routing Page Specs
     O*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *****************************
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
      ***** End Del ***** 08/26/14 *****************************
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
      ***** Beg Del ***** 08/26/14 *****************************
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
      ***** End Del ***** 08/26/14 *****************************
     O          E            ROUTE          2
     O                                           18 'POLICY YEAR:'
     O                       XXFYR         Z     22
     O          E            ROUTE          2
     O                                           28 'PERCENTAGE OF STANDARD'
     O                                         +  1 'PREMIUM:'
     O                       XXPER         K   +  2
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
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
     O                       XCNAME              96
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                              'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              72
     O                                          128 'POLICY YEAR:'
     O***** BEGIN CHG **** 12/14/99 ***********************************
     O**********               XXFYR2Z  132
     O                       XXFYR         Z    132
     O***** END   CHG **** 12/14/99 ***********************************
     O          E            HEAD           1
     O                                           16 'EMPLOYER'
     O                                           31 'DIVISION'
     O                                           98 'CANCELLATION'
     O                                          120 'STANDARD PREM'
     O          E            HEAD           2
     O                                           15 'NUMBER'
     O                                           30 'NUMBER'
     O                                           49 'MEMBER NAME'
     O                                           94 'DATE'
     O***** BEGIN CHG **** 12/14/99 ***********************************
     O**********                        117 '  AT % '
     O                                          110 'AT '
     O***** END   CHG **** 12/14/99 ***********************************
     O***** BEGIN ADD **** 12/14/99 ***********************************
     O                       XXPER         K    120
     O                                          120 '%'
     O***** END   ADD **** 12/14/99 ***********************************
     O* Detail Line
     O*
     O          E            DETL2          1
     O                                           34 '***   NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA   ***'
     O*
     O* Detail Line Specs
     O*
     O          E            DETL           1
     O                       ELEMP#        Z     15
     O                       ELDIV         Z     28
     O                       EMNAME              78
     O                       XXCNDT              96 '  /  /  '
     O                       XXPREM        J    121
     O*
     O* Total Line Specs
     O*
     O          E            TOTL        2  1
     O                       TTLEMP        J     16
     O                                           30 'TOTAL MEMBERS'
     O                                           98 'TOTAL:'
     O                       TTLPRM        J    121
