     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7255R - Report -  Agent Commission Edit
     H*
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM PRODUCES A EDIT REPORT FOR ALL ADJUSTMENTS **
     H**     AND VOIDS BY USER FOR THE SYSTEM DATE.                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7255C - VIA   WEDITQ                                   **
     H**   CALLS TO:                                                 **
     H**     None                                                    **
     H**   PROGRAMMER: Lori J. Quinn                                 **
     H**   CREATED:    04/24/92     PROJECT: AMW   - CLIENT #3       **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**      60                General Chaining Indicator           **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO#                                       **
     H**    WMUPL (PUP01): USER                                      **
     H**    SMORL (POR01): PNAM/FIL#                                 **
     H**    WMACL9(PAC01): CO#/USER/TRDT/FND/FYR/AGT#/PROD/EMP/DIV   **
     H*****************************************************************
     H*  REVISIONS:                                                   *
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
     H*  10/27/92 C:09 USER NAME THAT PRINTS ON THE ROUTING PAGE      *
     H*                SHOULD BE RETRIEVED FROM THE VALUE THAT IS     *
     H*                PASSED IN FROM THE PARM SECTION (QQUSER).      *
     H*                                                               *
     H*  09/30/93 C:15 PROJECT                                        *
     H*                ADDED EDIT CODE TO REPORT AMOUNT.              *
     H*
     H*  01/09/98  C:80  PROJECT# WC420
     H*                  MODIFIED PROGRAM TO PROCESS 8 DIGIT DATE
     H*                  FORMAT.
     H*
     H*****************************************************************
     F* Output Routing Master File
     FSMORL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* Agent Commission Master File
     FWMACL9    IF   E           K DISK
     F*
     F* Output Spool File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*

     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D* External Program Described Data Structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    ACCO#         XXCO#                          Company #
     C     *LIKE         DEFINE    ACFND         XXFND                          Fund #
     C     *LIKE         DEFINE    ACUSER        XXUSER                         User
     C     *LIKE         DEFINE    ACTRDT        XXTRDT                         Trans Date
     C*
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the Output Routing Master File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the User PArameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER
     C*
     C* Key List for the Agent Commission Master File
     C*
     C     ACKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXUSER
     C                   KFLD                    XXTRDT
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQUSER           10
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** PARM           QQDATE  6
     C                   PARM                    QQDATE            8
     C*** END   CHG *** 01/09/98 **************************************
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
     C* Initialize and Set-Up
     C*
     C                   Z-ADD     0             XXCO#
     C                   MOVE      *BLANKS       XXUSER
     C                   Z-ADD     0             XXTRDT
     C                   Z-ADD     0             XXMDY
     C                   MOVE      QQCO#         XXCO#                          Company #
     C                   MOVE      QQUSER        XXUSER                         User
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** MOVE QQDATE    XXMDY   60
     C                   MOVE      QQDATE        XXMDY             8 0
     C*** END   CHG *** 01/09/98 **************************************
     C*
     C                   Z-ADD     XXMDY         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTRDT
     C*
     C                   Z-ADD     0             XXAMT            11 2          Amount
     C                   Z-ADD     0             XXTOCL           11 2          Ttl Collecti
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE            8 0
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C* Retrieve the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C* Retrieve the Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* Retrieve the Output Routing Record
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C                   MOVE      *BLANKS       XXTITL           56
     C                   MOVEL     ORTITL        XXTITL
     C*
     C* Print the Routing Page
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      '1'           *INOA
     C*
     C* Read All the Agent Commission Records by CO#/USER/TRDT
     C*
     C     ACKEY         CHAIN     PAC01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* Check Code Type - Only Process 'A' & 'V' Types
     C*
     C     ACCOTP        IFEQ      'A'
     C     ACCOTP        OREQ      'V'
     C*
     C* Print the Detail Line
     C*
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** Z-ADDACPODT    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDY
     C******************** MOVELSRDTO     XXPODT  60
     C                   Z-ADD     ACPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C*** END   CHG *** 01/09/98 **************************************
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
     C                   EXCEPT    DETAIL
     C*
     C* Add to the Totals
     C*
     C                   ADD       ACAMT         XXAMT            11 2          Amount
     C                   ADD       ACTOCL        XXTOCL           11 2          Ttl Collecti
     C*
     C                   END                                                    A/V Types End
     C*
     C* Get the Next Agent Commission Record
     C*
     C     ACKEY         READE     PAC01                                  60
     C                   END
     C*
     C* Check for Page Overflow
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C* Print the Total Line
     C*
     C                   EXCEPT    TOTAL
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
     C*
     C*
     O* Routing Page
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O***8    E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O* Heading Page
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              82
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ DSDATEY  127
     O                       XXDATE             132 '  /  /    '
     O*** END   CHG *** 01/09/98 **************************************
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              92
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD        1  2
     O                                           10 'PROCESSOR:'
     O                       UPUSR               22
     O                       UPUSNM              73
     O                                           84 'TRANSACTION DATE:'
     O                       XXMDY         Y     94
     O          E            HEAD           2
     O*** BEGIN CHG *** 01/09/98 **************************************
     O********************************** 11 'FUND  FYR'
     O********************************** 18 'AGT#'
     O********************************** 32 'PROD  EMP #'
     O********************************** 37 'DIV'
     O********************************** 48 'POST DATE'
     O********************************** 60 'CODE  TYPE'
     O********************************** 73 'DESCRIPTION'
     O**********************************111 'TOTAL COLLECTIONS'
     O**********************************128 'AMOUNT'
     O                                            6 'GROUP'
     O                                           11 'PYR'
     O                                           18 'AGT#'
     O                                           25 'PROD#'
     O                                           36 'EMP#'
     O                                           43 'DIV'
     O                                           55 'POST DATE'
     O                                           61 'CODE'
     O                                           66 'TYP'
     O                                           79 'DESCRIPTION'
     O                                          116 'TOTAL COLLECTIONS'
     O                                          130 'AMOUNT'
     O*** END   CHG *** 01/09/98 **************************************
     O*
     O* Detail Line
     O          E            DETAIL         1
     O                       ACFND         Z     06
     O                       ACFYR         Z     11
     O                       ACAGT#        Z     18
     O                       ACPROD        Z     25
     O                       ACEMP#        Z     36
     O                       ACDIV         Z     43
     O                       XXPODT              55 '  /  /    '
     O                       ACCOCD              60
     O                       ACCOTP              65
     O                       ACDESC              98
     O                       ACTOCL        J    117
     O                       ACAMT         J    131
     O*
     O* Total Line
     O          E            TOTAL       1  2
     O                                           92 'TOTAL:'
     O                       XXTOCL        J    117
     O                       XXAMT         J    131
