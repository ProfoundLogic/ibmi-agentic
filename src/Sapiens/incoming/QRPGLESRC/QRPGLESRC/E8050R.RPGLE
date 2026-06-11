     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H**
     H/TITLE E8050R - LOAD EXP MOD TO WDELP
     H*****************************************************************
     H**                                                             **
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM WILL READ THE CALC'D EXP MOD FILE AND      **
     H**     LOAD THE EXP MOD TO WDELP                               **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     E8050C                                                  **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H**   PROGRAMMER: LORI J. QUINN                                 **
     H**   CREATED:    09/21/03     PROJECT: WC1156                  **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** MODIFICATIONS:                                              **
     H**                                                             **
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H**  08/26/14 C:298 Project: SY354                              **
     H**                -Implement Routing Page Changes.             **
     H**
     H**  01/28/10  C:XXX  Project: SY215                            **
     H**            C:189  Retrieve brand Name                       **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H**                                                             **
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H**     *IN                                                     **
     H**      08     DEMAND MODE                                     **
     H**      09     BATCH  MODE                                     **
     H**      25     "MA" SPECIAL PROCESSING FO STATE OF MASS
     H**      50     DOWEQ EXXTP                                     **
     H**      60     DOWEQ WDELP                                     **
     H**                                                             **
     H**                                                             **
     H** * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * **
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SMCOL (PCO01): CO FILE                                   **
     H**    EXXTL2(PXX01): CO/FND/EFDT/EMP#  (SELECT REC = "C")      **
     H**    WDELL1(PEL01): CO/FND/FYR/EMP/DIV                        **
     H*****************************************************************
     H*
     F*
     F* Company Master File
     F*
     FSMCOL     IF   E           K DISK
     F*
     F* EXP MOD FILE
     FEXXTL2    IF   E           K DISK
     F*
     F* WC EMP DETAIL
     FWDELL1    UF   E           K DISK
     FSMORL     IF   E           K DISK
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOA)

     D*
     D* System Date and Time Data Structure
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* INCOMING EFF DT
     D                 DS
     D  INEFDT                 1      7
     D  INCT                   1      1
     D  INYY                   2      3
     D  INMM                   4      5
     D  INDD                   6      7
     D*
     D* PRINTED EFF DT
     D                 DS
     D  OTEFDT                 1      8
     D  OTMM                   1      2
     D  XXDSH1                 3      3
     D  OTDD                   4      5
     D  XXDSH2                 6      6
     D  OTYY                   7      8
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST MANUAL RATE FILE
     C*
     C     XTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEFDT
     C*
     C*   WORK COMP WDELP FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEFDT            7
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
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C* Define Work Fields
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELPFDT        XXEFDT
     C     *LIKE         DEFINE    XTSURF        XXARAP
     C     *LIKE         DEFINE    QQMODE        XXMODE
     C                   Z-ADD     0             XXMOD             3 2
     C*
     C* SET UP AND INITIALIZE
     C*
     C                   MOVE      QQMODE        XXMODE
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEFDT        XXEFDT
     C                   MOVE      QQEFDT        INEFDT
     C                   MOVE      INYY          OTYY
     C                   MOVE      INMM          OTMM
     C                   MOVE      INDD          OTDD
     C                   MOVE      '/'           XXDSH1
     C                   MOVE      '/'           XXDSH2
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C* CHAIN TO THE COMPANY FILE
     C     COKY          CHAIN     PCO01                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C* GET ROUTING INFO
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     ORTITL        ##DATA           80
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        ORTITL
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C*
     C*   PRINT PAGE HEADING
     C                   EXCEPT    HEAD
     C*
     C****   READ EXP MOD FILE FOR THE SELECT GRP AND EFDT
     C*
     C     XTKY          CHAIN     PXX01                              50
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C                   Z-ADD     0             XXMOD
     C                   Z-ADD     0             XXARAP
     C                   SETOFF                                       25
     C*
     C     XTMODA        IFNE      0
     C     XTMODA        MULT(H)   1             XXMOD
     C                   ELSE
     C     XTMOD         MULT(H)   1             XXMOD
     C                   END
     C*
     C     XTOVR3        IFEQ      '*'
     C                   Z-ADD     1.00          XXMOD
     C                   END
     C*
     C* "MA" SPECIAL PROCESSING
     C     XTST          IFEQ      'MA'
     C     XTSURF        MULT      1             XXARAP
     C                   SETON                                        25
     C                   END
     C*
     C* NOW GO UPDATE WDELP
     C*
     C                   Z-ADD     XEMP#         XXEMP#
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXMOD         ELEXM1
     C                   Z-ADD     0             ELEXM2
     C                   Z-ADD     0             ELEXM3
     C                   Z-ADD     XXEFDT        ELEMD1
     C                   Z-ADD     0             ELEMD2
     C                   Z-ADD     0             ELEMD2
     C*
     C* "MA" SPECIAL PROCESSING
     C     XTST          IFEQ      'MA'
     C                   Z-ADD     XXARAP        ELARP1
     C                   Z-ADD     0             ELARP2
     C                   Z-ADD     0             ELARP3
     C                   END
     C*
     C                   MOVEL(P)  Q1USER        ELUSER
     C                   MOVEL(P)  Q1PGM         ELPGM
     C                   Z-ADD     DSTIME        ELTIME
     C                   Z-ADD     XXDT          ELDT
     C                   UPDATE    PEL01
     C*
     C* PRINT DETAIL
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    DET
     C*
     C* GET NEXT RECORD
     C     ELKY          READE     PEL01                                  60
     C                   END
     C*
     C     XTKY          READE     PXX01                                  50
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O*  MLS 1/19/88  ADD INDICATORS
     O**********     08      ORDIS1              60
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/19/88  ADD INDICATORS
     O               08      ORDIS1              60
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
      ***** Beg Del ***** 08/26/14 *********************************************
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
      ***** End Del ***** 08/26/14 *********************************************
      *** BEG ADD ***** 08/14/01 *****************************************
     O          E            ROUTE          2
     O                                           26 'COMPANY . . . . :'
     O                       XXCO#         4   +001
      *
     O          E            ROUTE          2
     O                                           26 'GROUP . . . . . :'
     O                       XXFND         4   +001
      *
     O          E            ROUTE          2
     O                                           26 'EFF DT. . . . . :'
     O                       OTEFDT            +001
      ****** BEG DLT ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                18 '(C) COPYRIGHT'
     O***                                35 'PROGRESSIVE DATA'
     O***                                51 'SOLUTIONS, INC.'
     O***                      *YEAR     56
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              60
     O                       DSDATE        Y     80
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              70
     O                                           76 'PAGE:'
     O                       PAGE          Z     80
     O          E            HEAD           2
     O                                           07 'GRP'
     O                                           21 'EMPLOYER/DIV'
     O                                           33 'EFF DT'
     O                                           47 'MODIFIER'
     O                                           65 'ARAP (MA ONLY)'
     O          E            DET            1
     O                       ELFND               07
     O                       ELEMP#        Z     18
     O                       ELDIV         Z     24
     O                       OTEFDT              35
     O                       ELEXM1              46
     O               25      ELARP1              58
