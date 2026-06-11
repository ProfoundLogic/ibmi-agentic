     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H/TITLE N7100R - DELETED A/R TRANS WITHIN DATE RANGE - REPORT PGM
     H**************************************************************************
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*   01/28/10 C:249 Project: SY215                               *
     H*            C:189 Retrieve brand Name                          *
     H*                                                               *
     H* 01/27/00 C:62  Project: Y2K                                   *
     H*                CHANGE DATES TO MDYY.                          *
     H*                                                               *
     H* 07/05/95 C:35  REQ:PROJ: NW008                                *        *
     H*                1. ALLOW SYSTEM TYPE 'M'                       *        *
     H*                                                               *
     H**************************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNMARL4    IF   E           K DISK
     FFMSML     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)

     D*** BEGIN CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     D**********          TL      1   3 20               REPORT TITLE
     D TL              S             20    DIM(5) CTDATA PERRCD(1)              REPORT TITLE
     D*** END   CHG ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     D                 DS
     D*** BEGIN CHG *** 01/27/00 **************************************
     D**********                              1  120DSDTTM
     D**********                              1   60DSTIME
     D**********                              7  120DSDATE
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** END   CHG *** 01/27/00 **************************************
     D                 DS
     D  XXCASE                 1     10
     D  XXFYR                  1      2  0
     D  XXHYP1                 3      3
     D  ARCASE                 4     10  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C     *ENTRY        PLIST
     C                   PARM                    QQSYS             1
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
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
     C                   MOVE      QQSYS         XXSYS             1
     C                   MOVE      QQFRDT        X1FRDT            8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1FRDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXFRDT  60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQTODT        X1TODT            8 0
     C*
     C*** BEGIN DEL *** 01/27/00 **************************************
     C**********           Z-ADDX1TODT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDY1
     C**********           MOVELSRDTO     XXTODT  60
     C*** END   DEL *** 01/27/00 **************************************
     C*
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C*  COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#             3 0
     C*
     C*  SUBSCIBER FILE
     C*
     C     SMKEY         KLIST
     C                   KFLD                    UPCO#             3 0
     C                   KFLD                    UPSUB#
     C*
     C*  A/R FILE
     C*
     C     ARKEY         KLIST
     C                   KFLD                    UPCO#
     C                   KFLD                    UPSUB#
     C                   KFLD                    XXSYS
     C*
     C*  ROUTING FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      '-'           XXHYP1            1
     C*
     C* * * DETERMINE SYSTEM ID FOR REPORT HEADING
     C*
     C     XXSYS         IFEQ      'P'
     C                   MOVEL     TL(1)         XXTITL           20
     C                   END
     C*
     C     XXSYS         IFEQ      'H'
     C                   MOVEL     TL(2)         XXTITL
     C                   MOVE      '1'           *IN10
     C                   END
     C*
     C     XXSYS         IFEQ      'N'
     C                   MOVEL     TL(3)         XXTITL
     C                   END
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C     XXSYS         IFEQ      'U'
     C                   MOVEL     TL(4)         XXTITL
     C                   END
     C*
     C     XXSYS         IFEQ      'M'
     C                   MOVEL     TL(5)         XXTITL
     C                   END
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *ZERO         UPCO#
     C                   MOVE      *ZERO         UPSUB#
     C                   END
     C*
     C                   MOVE      UPCO#         XXCO#             3 0
     C                   MOVE      UPSUB#        XXSUB#            5 0
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR REPORT
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         MOVE      '0'           XXPRT                           QSYSPRT
     C**********     ORKEY         CHAIN     POR01                              60
     C**********     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION FOR REPORT
     C*
     C**********         EXCEPT    ROUTE
     C**********         END                                                    END IFEQ '0'
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C* * * GET COMPANY RECORD (CARRIER INFO)
     C*
     C     COKEY         CHAIN     PCO01                              50
     C*
     C*   GET THE SUBSCRIBER INFORMATION
     C*
     C     SMKEY         CHAIN     PSM01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANKS       XXSBNM           40
     C                   ELSE
     C                   MOVEL     SMNAME        XXSBNM
     C                   END
     C*
     C*  READ A/R FILE
     C*
     C     ARKEY         CHAIN     PAR01                              30
     C     *IN30         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C     *IN30         DOWEQ     '0'
     C*
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C     ARSYS         IFEQ      XXSYS
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*  CHECK FOR DATE LEVEL BREAK
     C*
     C     ARTRDT        IFNE      HLTRDT
     C*
     C     HLTRDT        IFNE      *ZEROS
     C                   EXSR      SRDATE                                       DATE TOTALS
     C                   END
     C*
     C                   Z-ADD     ARTRDT        HLTRDT            6 0
     C*
     C                   EXCEPT    HEAD
     C*
     C                   END                                                    IFNE HLTRDT
     C*
     C                   Z-ADD     ARTRDT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXTRDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C                   Z-ADD     ARPODT        SRDTI
     C*** BEGIN CHG *** 01/27/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXPODT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C*** END   CHG *** 01/27/00 **************************************
     C                   Z-ADD     ARFYR         XXFYR
     C*
     C                   MOVEL     ARDESC        XXDESC           30
     C*
     C**** DETERMINE HBA LEVEL FROM DESCRIPTION
     C     *IN10         IFEQ      '1'
     C                   MOVEL     ARDESC        HLDESC           20
     C                   MOVE      HLDESC        XXLVL             3
     C                   ELSE
     C                   MOVE      *BLANKS       XXLVL
     C                   END
     C*
     C                   ADD       ARAMT         XXDAMT           10 2
     C*
     C     ARAMT         IFGE      *ZERO
     C                   ADD       1             XXDCNT            4 0
     C                   ELSE
     C                   SUB       1             XXDCNT
     C                   END
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DETAIL
     C*** BEGIN ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C                   END                                                    END ARSYS EQ XXSYS
     C*** END   ADD ***** 07/05/95 *** PROJ: NW008 **** PGMR: 35 ******
     C*
     C*  READ THE NEXT A/R RECORD
     C*
     C     ARKEY         READE     PAR01                                  30
     C*
     C                   END                                                    ENDDOW *IN30
     C*
     C*  PRINT FINAL TOTALS
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXSR      SRDATE                                       DATE TOTALS
     C                   EXCEPT    LRTOTL                                       FINAL TOTALS
     C*
     C*** END PROGRAM
     C*
     C     TERM          TAG
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
     C*****************************************************************
     C     SRDATE        BEGSR                                                  S R C O T L*
     C*****************************************************************
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   ADD       XXDAMT        XXTAMT           11 2
     C                   ADD       XXDCNT        XXTCNT            5 0
     C*
     C                   EXCEPT    DTTOTL
     C*
     C                   Z-ADD     *ZERO         XXDAMT
     C                   Z-ADD     *ZERO         XXDCNT
     C*
     C                   ENDSR
     C****
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
      **** BEGIN ADD **** 05/12/98 ************************************
     O          E            ROUTE       2  2
     O                                           19 'SYSTEM:'
     O                       QQSYS               21
     O          E            ROUTE          2
     O                                           19 'DATE RANGE:'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDT    28 '  /  /  '
     O**********                         31 'TO'
     O**********               XXTODT    40 '  /  /  '
     O                       X1FRDT              30 '  /  /    '
     O                                           33 'TO'
     O                       X1TODT              44 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
      **** END   ADD **** 05/12/98 ************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O                                         +000 '(C)'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      **** BEGIN CHG **** 05/07/98 ************************************
     O********************************** 57 'INC. 1997'
      ****** BEG DLT ***** 01/28/10 **********************************
     O****                               46 'INC.'
     O****                     *YEAR   +  1
      ****** END DLT ***** 01/28/10 **********************************
      **** END   CHG **** 05/07/98 ************************************
     OQSYSPRT   E            HEAD           1 06
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               DSDATEY    8
     O                       DSDATE              10 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O                       CONAME              82
     O                       DSTIME             126 '  :  :  '
     O          E            HEAD           1
     O                       ORPNAM              10
     O                       ORTITL              92
     O                       PAGE               126
     O                                          121 'PAGE:'
     O          E            HEAD           1
     O                       XXTITL              73
     O          E            HEAD           2
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDTY   61
     O**********                         63 '-'
     O**********               XXTODTY   72
     O                       X1FRDT              63 '  /  /    '
     O                                           65 '-'
     O                       X1TODT              76 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            HEAD           2
     O                                           11 'SUBSCRIBER:'
     O                       XXSUB#              18
     O                       XXSBNM              60
     O          E            HEAD           1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                          9 'TRANS'
     O**********                         23 'POST'
     O**********                         95 'HBA'
     O                                           11 'TRANS'
     O                                           27 'POST'
     O                                           99 'HBA'
     O*** END   CHG *** 01/27/00 **************************************
     O          E            HEAD           2
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********                          9 'DATE'
     O**********                         23 'DATE'
     O**********                         35 'GROUP'
     O**********                         47 'CASE'
     O**********                         67 'DESCRIPTION'
     O**********                         96 'LEVEL'
     O**********                        108 'EOB #'
     O**********                        124 'AMOUNT'
     O                                           11 'DATE'
     O                                           27 'DATE'
     O                                           39 'GROUP'
     O                                           51 'CASE'
     O                                           72 'DESCRIPTION'
     O                                          100 'LEVEL'
     O                                          112 'EOB #'
     O                                          128 'AMOUNT'
     O*** END   CHG *** 01/27/00 **************************************
     O          E            DETAIL         1
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXTRDTY   11
     O**********               XXPODTYB  25
     O**********               ARFND  B  34
     O**********               XXCASE    50
     O**********               XXDESC B  86
     O**********               XXLVL  B  95
     O**********               AREOB#ZB 108
     O**********               ARAMT KB 125
     O                       XXTRDT              13 '  /  /    '
     O                       XXPODT         B    29 '  /  /    '
     O                       ARFND          B    38
     O                       XXCASE              54
     O                       XXDESC         B    90
     O                       XXLVL          B    99
     O                       AREOB#        ZB   112
     O                       ARAMT         KB   129
     O*** END   CHG *** 01/27/00 **************************************
     O          E            DTTOTL      2  2
     O                                           11 'TRANS. DATE'
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXTRDTYB  20
     O**********                         28 'TOTALS:'
     O**********                         47 'NUMBER OF RECORDS:'
     O**********               XXDCNTZB  53
     O**********               XXDAMTKB 125
     O                       XXTRDT         B    22 '  /  /    '
     O                                           30 'TOTALS:'
     O                                           49 'NUMBER OF RECORDS:'
     O                       XXDCNT        ZB    55
     O                       XXDAMT        KB   127
     O*** END   CHG *** 01/27/00 **************************************
     O          E            LRTOTL      2  1
     O                                           18 'SYSTEM'
     O                       XXSYS               20
     O                                           28 'TOTALS:'
     O                                           47 'NUMBER OF RECORDS:'
     O                       XXTCNT        ZB    53
     O                       XXTAMT        KB   125
     O          E            ERRMSG         1 06
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               DSDATEY    8
     O                       DSDATE              10 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O                       CONAME              82
     O                       DSTIME             126 '  :  :  '
     O          E            ERRMSG         1
     O                       ORPNAM              10
     O                       ORTITL              92
     O                       PAGE               126
     O                                          121 'PAGE:'
     O          E            ERRMSG         1
     O                       XXTITL              73
     O          E            ERRMSG         2
     O*** BEGIN CHG *** 01/27/00 **************************************
     O**********               XXFRDTY   61
     O**********                         63 '-'
     O**********               XXTODTY   72
     O                       X1FRDT              62 '  /  /    '
     O                                           65 '-'
     O                       X1TODT              76 '  /  /    '
     O*** END   CHG *** 01/27/00 **************************************
     O          E            ERRMSG         2
     O                                           21 '*** NO ACTIVITY MEETS'
     O                                           40 'SELECTION CRITERIA'
** TL (REPORT TITLE)
PROVIDER BILL REVIEW
HOSPITAL BILL AUDIT
 PPO NETWORK REVIEW
UTILIZATION REVIEW
MEDICAL MANGEMENT
