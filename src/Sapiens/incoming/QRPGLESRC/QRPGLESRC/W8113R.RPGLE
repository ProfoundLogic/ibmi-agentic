     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8113R - PURGE PAYMENT/RESERVE FILES
     H****************************************************************
     H*                                                              *
     H*  PROGRAM DESCRIPTION:                                        *
     H*       THIS PROGRAM WILL PURGE ALL RECORDS FOR THE PAYMENT    *
     H*       AND RESERVE RELATED FILES FOR THE SELECTED FUND        *
     H*                                                              *
     H*  CALLED FROM:                                                *
     H*    W8112C - PAYMENT/RESERVE PURGE OVERRIDE CL                *
     H*                                                              *
     H*  CALLS TO:                                                   *
     H*    NONE                                                      *
     H*                                                              *
     H*  PROGRAMMER: C:46                                            *
     H*  CREATED:    03/13/96    PROJECT: WC494                      *
     H****************************************************************
     H*                                                              *
     H* REVISIONS:                                                   *
     H*                                                              *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  02/03/98 C:61 PROJECT 60APK                                 *
     H*                Convert for V3.                               *
     H*                                                              *
     H****************************************************************
     H*                                                              *
     H* INDICATOR USAGE:                                             *
     H*                                                              *
     H*                                                              *
     H****************************************************************
      *
      *
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
      *
     FWDPAL     UF   E           K DISK
     FWMRHL     UF   E           K DISK
     FWMCHL     UF   E           K DISK
     FWMTHL     UF   E           K DISK
     FWDRPL     UF   E           K DISK
     FWARPL     UF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWMLPL     UF   E           K DISK
      *
      ***** BEG ADD ******* 02/03/98 *********************************
     FWAGCL     UF   E           K DISK
     FWDAJL     UF   E           K DISK
     FWAR3L7    UF   E           K DISK
     F                                     RENAME(PRP01:PRP03)
     FWARPPML   UF   E           K DISK
     F                                     RENAME(PRP01:PRP04)
      *ZZZ
      ***** END ADD ******* 02/03/98 *********************************
      *
     FQSYSPRT   O    F  139        PRINTER OFLIND(*INOA)
      *

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *

     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@FND             3
     C                   PARM                    X@QUE            10
     C                   MOVE      X@CO#         XXCO#             3 0
     C                   MOVE      X@FND         XXFND             3 0
     C                   MOVE      X@QUE         QQQUE            10
      *
      *   SET-UP AND INITIALIZE
      *
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
      *   KEY LIST FOR ALL FILES
      *
     C     XXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE REPORT OVERRIDE
      *
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
      *
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *   CHAIN TO THE USER PARAMETER FILE
      *
     C     UPKEY         CHAIN     PUP01                              60
     C                   TIME                    DSDTTM
     C                   MOVE      X@CO#         XXCO#
     C                   MOVE      UDATE         XXDATE            6 0
      *
      *   CHAIN TO THE COMPANY FILE
      *
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
      *
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
      *
      *   GET THE DISTRIBUTION INFORMATION
      *
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
      *
      *   PRINT THE DISTRIBUTION INFORMATION
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C                   MOVE      *ON           *INOF
      *
     C                   EXCEPT    HEAD
      *
     C                   Z-ADD     *ZERO         XX#REC            6 0
     C                   Z-ADD     *ZERO         XXTREC            8 0
      *
      *  READ/DELETE ALL RECORDS IN THE FILE FOR THE SELECTED FUND
      *
      *  CHECK PAYMENT DETAIL FILE
      *
     C     XXKEY         CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PPA01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PPA01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDPAP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  RESERVE HISTORY FILE
      *
     C     XXKEY         CHAIN     PRH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMRHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  CLAIM HISTORY FILE
      *
     C     XXKEY         CHAIN     PCH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PCH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PCH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMCHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  TRANSFER HISTORY LOST TIME
      *
     C     XXKEY         CHAIN     PTH01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PTH01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PTH01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMTHP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  VIEW CHECK FILE
      *
     C     XXKEY         CHAIN     PRP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDRPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  REQUEST PAYMENT TRANSACTION FILE
      *
     C     XXKEY         CHAIN     PRP02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRP02
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRP02                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WARPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  LATE PAYMENT FILE
      *
     C     XXKEY         CHAIN     PLP01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PLP01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PLP01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WMLPP'       XXFILE           10
     C                   EXCEPT    DET
      *
      ***** BEG ADD ******* 02/03/98 *********************************
      *
      *  Group Check Pend V/S/Z File
      *
     C     XXKEY         CHAIN     PGC01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PGC01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PGC01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAGCP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Payment/Adjustment Management Detail
      *
     C     XXKEY         CHAIN     PAJ01                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PAJ01
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PAJ01                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WDAJP'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  Group Request Payment File
      *
     C     XXKEY         CHAIN     PRP03                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRP03
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRP03                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WAR3P'       XXFILE           10
     C                   EXCEPT    DET
      *
      *  WARPPM - Request Payment, Monthly Transactions
      *
     C     XXKEY         CHAIN     PRP04                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PRP04
     C                   ADD       1             XX#REC
     C                   ADD       1             XXTREC
     C     XXKEY         READE     PRP04                                  60                R
     C                   END
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   MOVEL     'WARPPM'      XXFILE           10
     C                   EXCEPT    DET
      *
      *ZZZ
      ***** END ADD ******* 02/03/98 *********************************
      *
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    TOTL
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
      *
      *
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
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           20 'CO#:'
     O                       XXCO#               25
     O          E            ROUTE          2
     O                                           20 'DATE:'
     O                       XXDATE              30 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1996'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
      ****************************************************************
      *     HEADING
      ****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       XXDATE        Y    130
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              94
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O          E            HEAD           2
     O                                           18 'FILE NAME'
     O                                           44 '# DELETED RECORDS'
      ****************************************************************
      *     DETAIL LINE WITH FILE NAME AND COUNT
      ****************************************************************
     O          E            DET            1
     O                       XXFILE              19
     O                       XX#REC        ZB    40
      ****************************************************************
      *     TOTAL RECORD COUNT FOR ALL FILES
      ****************************************************************
     O          E            TOTL        2  2
     O                                           18 'ALL FILES'
     O                       XXTREC        Z     40
