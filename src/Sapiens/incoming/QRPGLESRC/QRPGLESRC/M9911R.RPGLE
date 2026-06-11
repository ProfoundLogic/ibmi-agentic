     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* REVISIONS:                                                    *
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
     H* 09/17/92 KRR    REQ/PROJ: 1/SY001                             *
     H*                 1. ADDED CODE TO PRINT AVTIVE MENU OPTIONS    *
     H*                    ONLY.                                      *
     H*****************************************************************
     H/TITLE W9911R - MENU OPTION LIST
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FMMOPL     IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE MENU OPTION FILE
     C*
     C     OPKEY         KLIST
     C                   KFLD                    XXMNAM
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQMNAM           10
     C                   PARM                    QQQUE            10            MLS 1/20/88
     C**
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQMNAM        XXMNAM           10
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           *INOF
     C*                                                    MLS 1/20/88
     C*   SET UP AND INITIALIZE                                 "
     C*                                                         "
     C     QQQUE         IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     COKEY         CHAIN     SMCOL                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C*******            MOVE      '0'           XXPRT
     C*******     ORKEY         CHAIN     POR01                              60
     C*******     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*******                   EXCEPT    ROUTE
     C*******            END
      ***** End Del ***** 08/26/14 *********************************************
     C                   EXCEPT    HEAD
     C*
     C*
     C*  GET THE MENU OPTION LIST
     C*
     C     XXMNAM        IFEQ      *BLANK
     C                   READ      MMOPL                                  60
     C                   ELSE
     C     OPKEY         CHAIN     MMOPL                              60
     C                   END
     C*
     C     *IN60         DOWEQ     '0'
     C*** BEGIN ADD *** 09/17/92 **************************************
     C     OPSTAT        IFNE      'I'
     C*** END ADD *** 09/17/92 ****************************************
     C*
     C                   MOVEL     OPEXCL        XXEXCL          119
     C                   MOVEL     OPTY01        XXTY01           01
     C                   MOVEL     OPTY02        XXTY02           01
     C                   MOVEL     OPTY03        XXTY03           01
     C                   MOVEL     OPTY04        XXTY04           01
     C                   MOVEL     OPTY05        XXTY05           01
     C   OF              EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   EXCEPT    DET
     C*** BEGIN ADD *** 09/17/92 **************************************
     C                   END                                                    END OPSTAT
     C*** END ADD *** 09/17/92 ****************************************
     C     XXMNAM        IFEQ      *BLANK
     C                   READ      MMOPL                                  60
     C                   ELSE
     C     OPKEY         READE     MMOPL                                  60
     C                   END
     C                   END
     C*
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C**
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
     O*  MLS 1/20/88  ADD INDICATORS FOR DISTRIBUTION INFORMATION
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
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
     O*  MLS 1/20/88  ADD INDICATORS FOR DISTRIBUTION INFORMATION
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
     O*  MLS 1/20/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                56 ' 1991,1992'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             115
     O                       DSDATE        Y    196
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             126
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           2
     O          E            HEAD           1
     O                                           06 'MENU'
     O                                           27 'SEC'
     O                                           31 'LVL'
     O                                           36 'SEC'
     O          E            HEAD           2
     O                                           06 'NAME'
     O                                           17 'OPTION'
     O                                           21 'APP'
     O                                           27 'REQ'
     O                                           31 'REQ'
     O                                           37 'TYPE'
     O                                           49 'DESCRIPTION'
     O                                           91 'EXECUTED CLP'
     O          E            DET            1
     O                       OPMNAM              10
     O                       OPOPTN              15
     O                       OPMYN               17
     O                       OPAPP               21
     O                       OPYNO               23
     O                       OPMMSC        Z     27
     O                       OPMINS        Z     31
     O                       XXTY01              33
     O                       XXTY02              34
     O                       XXTY03              35
     O                       XXTY04              36
     O                       XXTY05              37
     O                       OPMNTT              78
     O                       XXEXCL             198
     O          E            DET            1
     O                       OPTX01              20
     O                       OPTX02              41
     O                       OPTX03              63
     O                       OPTX04              84
     O                       OPTX05             105
     O                       OPTX06             126
     O          E            DET            2
     O                       OPTX07              20
     O                       OPTX08              41
     O                       OPTX09              63
     O                       OPTX10              84
     O                       OPTX11             105
     O                       OPTX12             126
