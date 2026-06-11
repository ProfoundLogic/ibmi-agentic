     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE F7810R - USER POSSIBLE DUPLICATE CLAIMS RPT - PRINT PGM
     H* ***********************************************************
     H* REVISED:                                                  *
     H**                                                             **
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 11/16/93  C:20   REQ/PROJ: FSMNT                          *
     H*                  1. ADDED SUBSCRIBER NUMBER TO PTINTING   *
     H* ***********************************************************
     F*** FADUP FILE - OPEN QUERY: DUFLG1 EQ "Y"     SEQUENCE: DUUSER
     FFADUP     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFDCALB    IF   E           K DISK
     FQSYSPRT   O    F  198        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXCS#                  1     10
     D  XXFYR                  1      2  0
     D  XXHYP1                 3      3
     D  XXCASE                 4     10  0
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D                 DS
     D  XXSS#                  1      9
     D  XXPRE                  1      3
     D  XXMID                  4      5
     D  XXEND                  6      9
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSER           10
     C*
     C*  CLAIM DETAIL FILE
     C*
     C     CAKEY         KLIST
     C                   KFLD                    DUCO#
     C                   KFLD                    DUSUB#
     C                   KFLD                    DUFND
     C                   KFLD                    DUFYR
     C                   KFLD                    DUCASE
     C                   KFLD                    DUDSEQ
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
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
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      '-'           XXHYP1            1
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*** GET COMPANY RECORD
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     CONAME        XCNAME           40
     C                   END
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*** READ DUPLICATE CLAIMS FILE (OPEN QUERY)
     C*
     C                   READ      PDU01                                  50
     C*
     C     *IN50         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
     C*
     C*** GET ROUTING INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      Q1USER        XXUSER           10
     C                   MOVEL     *BLANKS       Q1USNM           50
     C     UPKEY         CHAIN     PUP01                              61
     C     *IN61         IFEQ      '0'
     C                   MOVEL     UPUSNM        Q1USNM
     C                   END
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   END
     C*
     C                   MOVE      DUUSER        XXUSER           10
     C                   MOVEL     *BLANKS       XXUSNM           50
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM
     C                   END
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*** IF USER CHANGES, PRINT TOTAL AND GET NEW USER NAME
     C*
     C     DUUSER        IFNE      XXUSER
     C     XXUSER        ANDNE     *BLANKS
     C                   ADD       XXUTOT        XXATOT            7 0
     C                   EXCEPT    USRTOT
     C                   MOVE      DUUSER        XXUSER
     C                   MOVEL     *BLANKS       XXUSNM           50
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     UPUSNM        XXUSNM
     C                   END
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   END
     C*
     C*  LOAD OUTPUT FIELDS FROM FILE FIELDS
     C*
     C                   MOVEL     DUSS#         XXSS#             9
     C*
     C                   Z-ADD     DUACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   Z-ADD     DUSUB#        XXSUB#            5 0
     C                   Z-ADD     DUFND         XXFND             3 0
     C                   Z-ADD     DUFYR         XXFYR             2 0
     C                   Z-ADD     DUCASE        XXCASE            7 0
     C                   Z-ADD     DUCASE        XECASE            6 0
     C                   Z-ADD     DUFEIN        XXFEIN            9 0
     C                   Z-ADD     DUMOD         XXMOD             5 0
     C                   Z-ADD     DUSVCD        XXRVCD            3 0
     C                   MOVE      DUCODE        XXHCPC            5
     C                   Z-ADD     DUUNTS        XXUNTS            3 0
     C*
     C                   Z-ADD     DUFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFDAT            6 0
     C*
     C                   Z-ADD     DUTDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTDAT            6 0
     C*
     C                   MOVEL     DUCODE        XXCODE            5
     C                   MOVEL     DUPMOD        XXPMOD            2
     C                   Z-ADD     DUAMT         XXAMT             9 2
     C                   Z-ADD     DUDSEQ        XXDSEQ            5 0
     C                   Z-ADD     DUEOB#        XXFSQ#            5 0
     C*
     C*** GET PAID AMOUNT & TRANS. DATE FROM DETAIL CHARGE FILE
     C*
     C     CAKEY         CHAIN     PCA01                              53
     C     *IN53         IFEQ      '0'
     C                   Z-ADD     CATOTP        XXPAMT            9 2
     C*
     C                   Z-ADD     CARDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C                   ELSE                                                   ELS 0 *IN53
     C                   Z-ADD     *ZERO         XXPAMT
     C                   Z-ADD     *ZERO         XXTRDT
     C                   END                                                    END 0 *IN53
     C*
     C*** SET ANY CONTROLING INDICATOR HERE
     C*
     C     CAPRTY        COMP      '4'                                    20
     C     XXMOD         COMP      *ZERO                              9292
     C     XXPMOD        COMP      *BLANKS                            9393
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DETAIL
     C                   ADD       1             XXUTOT            6 0
     C*
     C*  READ THE NEXT DUPLICATE CHARGE RECORD
     C*
     C                   READ      PDU01                                  50
     C*
     C                   END                                                    ENDDOWEQ 50
     C*
     C*  PRINT ALL USER TOTALS
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD2
     C                   MOVE      '0'           *INOF
     C                   END
     C                   ADD       XXUTOT        XXATOT
     C                   EXCEPT    USRTOT
     C*
     C                   EXCEPT    HEAD1
     C                   EXCEPT    ALLTOT
     C*
     C* END OF PROGRAM
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain Orkey Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
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
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                63 'INC. 1990, 1991, 1992'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT   E            HEAD1          1 06
     O                       DSDATE        Y      8
     O                       XCNAME              84
     O                       DSTIME             132 '  :  :  '
     O          E            HEAD1          2
     O                       ORPNAM              10
     O                       ORTITL              94
     O                       PAGE               132
     O                                          127 'PAGE:'
     O          E            HEAD2          2
     O                                            5 'USER:'
     O                       XXUSER              16
     O                                           18 '-'
     O                       XXUSNM              69
     O          E            HEAD2          1
     O                                            5 'TRANS'
     O                                           29 'ACCIDENT'
     O                                           84 'SERVICE DATES'
     O          E            HEAD2          2
     O                                            5 'DATE'
     O                                           18 'SOC SEC #'
     O                                           27 'DATE'
     O                                           37 'SUB #'
     O                                           41 'FND'
     O                                           45 'YR'
     O                                           50 'CASE'
     O                                           67 'PROVIDER  MOD'
     O                                           83 'FROM      TO'
     O                                          101 'CODE    MOD'
     O                                          109 'SEQ #'
     O                                          115 'EOB #'
     O                                          128 'CHARGES'
     O                                          140 'PAYMENTS'
     O          E            DETAIL         1
     O                       XXTRDT               8 '  /  /  '
     O                       XXPRE               12
     O                                           13 '-'
     O                       XXMID               15
     O                                           16 '-'
     O                       XXEND               20
     O                       XXACDT              29 '  /  /  '
     O                       XXSUB#        Z     37
     O                       XXFND               41
     O                       XXCS#               52
     O                       XXFEIN        Z     62
     O               92      XXMOD         Z     67
     O                       XXFDAT              76 '  /  /  '
     O                                           77 '-'
     O                       XXTDAT              85 '  /  /  '
     O              N20      XXCODE              92
     O               20      XXCMSV              98
     O               93                          99 '-'
     O               93      XXPMOD             101
     O                       XXDSEQ        Z    108
     O                       XXFSQ#        Z    115
     O                       XXAMT         2    128
     O                       XXPAMT        2    140
     O          E            USRTOT      2  1
     O                                           12 'USER TOTALS:'
     O                                           35 'NUMBER DETAIL CHARGES:'
     O                       XXUTOT        ZB    42
     O          E            ALLTOT         2
     O                                           73 'ALL USER TOTALS'
     O          E            ALLTOT         2
     O                                           73 'NUMBER DETAIL CHARGES:'
     O                       XXATOT        ZB    81
