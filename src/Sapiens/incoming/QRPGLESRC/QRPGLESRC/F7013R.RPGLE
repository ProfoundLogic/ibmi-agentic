     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE F7013R - TABLE FILE REPORT
     H*
     H*  Called by: F7013C
     H*  Calls to : None
     H*
     H*****************************************************************
     H* Created on 10/24/2001 by C:62 for Project FSMNT #160
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354
     H*                -Implement Routing Page Changes.
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFTMIL2    IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D*
     D*
     D                 DS
     D  FL                     1     15
     D                                     DIM(15)
     D  SCMICD                 1      1
     D  SC02CD                 2      2
     D  SC03CD                 3      3
     D  SC04CD                 4      4
     D  SC05CD                 5      5
     D  SC06CD                 6      6
     D  SC07CD                 7      7
     D  SC08CD                 8      8
     D  SC09CD                 9      9
     D  SC10CD                10     10
     D  SC11CD                11     11
     D  SC12CD                12     12
     D  SC13CD                13     13
     D  SC14CD                14     14
     D  SC15CD                15     15
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C* Parameter Lists
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FLG            15
     C                   PARM                    @@QUE            10
     C                   PARM                    @@MODE            1
     C                   PARM                    @@ST              2
     C                   PARM                    @@STNM           40
     C*****************************************************************
     C* Key Lists
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE.
     C*
     C     COKY          KLIST
     C                   KFLD                    KKCO#
     C*
     C*   KEY LIST FOR THE ERROR MESSAGE FILE
     C*
     C     MIKY          KLIST
     C                   KFLD                    KKST
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*****************************************************************
     C* Like Defines
     C*****************************************************************
     C     *LIKE         DEFINE    COCO#         KKCO#
     C     *LIKE         DEFINE    MIST          KKST
     C     *LIKE         DEFINE    MIST          XXST
     C     *LIKE         DEFINE    MIMSG#        XXMSG#
     C     *LIKE         DEFINE    MIMSG#        HDMSG#
     C*****************************************************************
     C* Main Processing
     C*****************************************************************
     C                   MOVE      @@CO#         KKCO#
     C                   MOVEL     @@ST          KKST
     C                   MOVEL     @@ST          XXST
     C                   MOVEL     @@STNM        XXSTNM           40
     C                   MOVEA     @@FLG         FL
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZEROS        X                 2 0
     C*
     C     @@ST          IFEQ      *BLANKS
     C                   SETON                                        16
     C                   ELSE
     C                   SETOFF                                       16
     C                   ENDIF
     C*
     C*   SET UP AND INITIALIZE
     C*
     C     @@MODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   ENDIF
     C*
     C*   CHAIN TO THE COMPANY FILE.
     C*
     C     COKY          CHAIN     PCO01                              99
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           KKPRT
     C     ORKY          CHAIN     POR01                              99
     C*
     C* SETON REPORT CONDITIONS FOR ROUTING SHEET
     C*
     C     FL(1)         IFNE      *BLANKS
     C                   SETON                                        21
     C                   ENDIF
     C*
     C     FL(2)         IFNE      *BLANKS
     C                   SETON                                        22
     C                   ENDIF
     C*
     C     FL(3)         IFNE      *BLANKS
     C                   SETON                                        23
     C                   ENDIF
     C*
     C     FL(4)         IFNE      *BLANKS
     C                   SETON                                        24
     C                   ENDIF
     C*
     C     FL(5)         IFNE      *BLANKS
     C                   SETON                                        25
     C                   ENDIF
     C*
     C     FL(6)         IFNE      *BLANKS
     C                   SETON                                        26
     C                   ENDIF
     C*
     C     FL(7)         IFNE      *BLANKS
     C                   SETON                                        27
     C                   ENDIF
     C*
     C     FL(8)         IFNE      *BLANKS
     C                   SETON                                        28
     C                   ENDIF
     C*
     C     FL(9)         IFNE      *BLANKS
     C                   SETON                                        29
     C                   ENDIF
     C*
     C     FL(10)        IFNE      *BLANKS
     C                   SETON                                        30
     C                   ENDIF
     C*
     C     FL(11)        IFNE      *BLANKS
     C                   SETON                                        31
     C                   ENDIF
     C*
     C     FL(12)        IFNE      *BLANKS
     C                   SETON                                        32
     C                   ENDIF
     C*
     C     FL(13)        IFNE      *BLANKS
     C                   SETON                                        33
     C                   ENDIF
     C*
     C     FL(14)        IFNE      *BLANKS
     C                   SETON                                        34
     C                   ENDIF
     C*
     C     FL(15)        IFNE      *BLANKS
     C                   SETON                                        35
     C                   ENDIF
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
     C*
     C     *IN99         IFEQ      *OFF
     C     ORSTAT        ANDEQ     'A'
     C     Q1USER        CHAIN     PUP01                              99
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   ENDIF
     C*
     C*   PRINT THE REPORT FOR ERROR MESSAGES  - Position 1
     C*
     C     *IN21         IFEQ      *ON
     C                   EXSR      PRTMI
     C                   ENDIF
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       kkPrt = '0';
       Chain Orky Por01;
       If %found and Orstat = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*   END OF JOB.
     C*
     C                   SETON                                        LR
     C*****************************************************************
     C*   PRINT THE BILL ENTRY ERROR MESSAGES                         *
     C*****************************************************************
     C     PRTMI         BEGSR
     C*
     C     MIKY          CHAIN     PMI01                              99
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C*
     C                   Z-ADD     *ZERO         XXRCD             5 0
     C*
     C*** ONLY PROCESS THE FILE IF WE HAVE LEAST ONE RECORD.
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  MIMSG#        XXMSG#
     C*
     C*** READ THE EOMB CODE FILE UNTIL THE COMPANY CHANGES.
     C*
     C     *IN99         DOWEQ     *OFF
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   EXCEPT    BLKLIN
     C                   SETOFF                                       OA
     C                   MOVEL     MIMSG#        XXMSG#
     C     HDMSG#        IFNE      MIMSG#
     C                   MOVEL     MIMSG#        HDMSG#
     C                   ADD       1             XXRCD
     C                   ENDIF
     C                   ELSE
     C*
     C     HDMSG#        IFEQ      MIMSG#
     C                   MOVEL     *BLANKS       XXMSG#
     C                   ELSE
     C                   MOVEL     MIMSG#        XXMSG#
     C                   MOVEL     MIMSG#        HDMSG#
     C                   ADD       1             XXRCD
     C                   EXCEPT    BLKLIN
     C                   ENDIF
     C                   ENDIF
     C*
     C                   EXCEPT    DETMI
     C*
     C     MIKY          READE     PMI01                                  99
     C                   ENDDO
     C                   ELSE
     C                   EXCEPT    ERRMSG
     C                   ENDIF
     C*
     C                   EXCEPT    TOTL
     C*
     C                   ENDSR
     C*****************************************************************
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
     O**********        09   @@QUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE          2
     O              N16                          23 'STATE:'
     O              N16      XXST                26
     O              N16      XXSTNM              68
     O*
     O          E    21      ROUTE          1
     O                                           41 'Bill Entry Error Message'
     O                                           42 's'
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                58 'INC. 2001       '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    122
     O          E            HEAD           3
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          116 'PAGE'
     O                       PAGE          Z    122
     O          E            HEAD1          0
     O                                           73 'ERROR MESSAGES'
     O          E            HEAD1          1
     O                                           73 'ERROR MESSAGES'
     O          E            HEAD1          2
     O                       XXST                62
     O                       XXSTNM             104
     O          E            HEAD1          1
     O                                            7 '  Msg #'
     O                                         +002 'Seq#'
     O                                         +002 'Description'
     O                                         +061 'Sts'
     O          E            DETMI          1
     O                       XXMSG#               7
     O                       MIMSEQ            +003 ' 0 '
     O                       MIDESC            +002
     O                       MISTAT            +003
     O*****************************************************************
     O          E            BLKLIN      1
     O          E            TOTL        2
     O                       XXRCD         Z     54
     O                                           69 'TOTAL        '
     O              N21                          82 'RECORDS      '
     O               21                          83 'ERROR MESSAGES'
     O          E            TOTL
     O                       XXRCD         Z     54
     O                                           69 'TOTAL        '
     O              N21                          82 'RECORDS      '
     O               21                          83 'ERROR MESSAGES'
     O          E            ERRMSG
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
