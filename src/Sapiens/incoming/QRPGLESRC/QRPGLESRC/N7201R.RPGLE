     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE N7201R - CONTRACT AND BILLING CHANGE REPORT
     H*****************************************************************
     H**   PROGRAM DESCRIPTION: PROGRAM IS USED TO PRINT THE THE     **
     H**                       CONTRACT AND BILLING CHANGE REPORT    **
     H**                       BASED ON THE REPORT TYPE 'C' 'B' OR ''**
     H*                                                              **
     H* PROGRAMMER C: 197                                            **
     H* CREATED     : 08/18/08                                       **
     H* PROJECT     : 114MPX                                         **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FNXBDL     IF   E           K DISK
     FFXS4L     IF   E           K DISK
     FNMBDL1    IF   E           K DISK
     F                                     RENAME(PBD01:PBD02)
     F*
     FQSYSPRT   O    F  132        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOV)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D FRTODS          DS
     D  F1VAL1                 1     30
     D  T1VAL1                31     60
     D  N1VAL1                61     80
     D*
     D FRT2DS          DS
     D  F2VAL2                 1     30
     D  T2VAL2                31     60
     D  N2VAL2                61     80
     D*
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    UPFYR         XXFYR
     C     *LIKE         DEFINE    Q1USER        XXUSER
     C*
     C*   KEY LIST FOR THE BILLING FILE
     C*
     C     BDKY          KLIST
     C                   KFLD                    BDCODE
     C                   KFLD                    BDTYPE
     C                   KFLD                    BDEFDT
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*
     C*   KEY LIST FOR THE PROVIDER NETWORK CONTRACT TRACKING FILE
     C*
     C     S4KEY         KLIST
     C                   KFLD                    XXDT              7 0
     C*
     C*   KEY LIST FOR THE NETWORK BILLING TRACKING FILE
     C*
     C     BDKEY         KLIST
     C                   KFLD                    XXDT
     C*
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFROM            8
     C                   PARM                    QQTO              8
     C                   PARM                    QQRPTY            1
     C*
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
     C*
     C                   MOVE      QQFROM        XXFRDT            8 0
     C                   MOVE      QQTO          XXTODT            8 0
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRD7            7 0
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTOD7            7 0
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQRPTY        XXRPTY            1
     C*
     C                   SETOFF                                       0809
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C                   TIME                    DSDTTM
     C*
     C*
     C*   GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        CONAME
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   SETOFF                                       3132
     C     XXRPTY        IFEQ      'C'
     C     XXRPTY        OREQ      ' '
     C                   MOVE      '1'           *IN31
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C*
     C                   MOVE      ORDIS1        O1DIS1           40
     C                   MOVE      ORDIS2        O1DIS2           40
     C                   MOVE      ORDIS3        O1DIS3           40
     C                   MOVE      ORDIS4        O1DIS4           40
      *
     C                   MOVE      ORTITL        O1TITL           60
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        O1TTLH           60
      *
     C                   MOVE      ORPNAM        O1PNAM           10
     C                   MOVE      ORFIL#        O1FIL#            1
     C                   MOVE      ORCOPY        O1COPY            2 0
     C                   MOVE      ORHOLD        O1HOLD            4
     C                   MOVE      OROUTQ        O1OUTQ           10
     C                   MOVE      ORFORM        O1FORM           10
     C                   MOVE      ORSAVE        O1SAVE            4
     C*
     C                   ENDIF
     C*
     C     XXRPTY        IFEQ      'B'
     C     XXRPTY        OREQ      ' '
     C                   MOVE      '1'           *IN32
     C                   MOVE      '1'           XXPRT             1
     C*
     C     ORKEY         CHAIN     POR01                              60
     C                   MOVE      ORDIS1        O2DIS1           40
     C                   MOVE      ORDIS2        O2DIS2           40
     C                   MOVE      ORDIS3        O2DIS3           40
     C                   MOVE      ORDIS4        O2DIS4           40
     C*
      *
     C                   MOVE      ORTITL        O2TITL           60
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL     ##DATA        O2TTLH           60
     C                   MOVE      ORPNAM        O2PNAM           10
     C                   MOVE      ORFIL#        O2FIL#            1
     C                   MOVE      ORCOPY        O2COPY            2 0
     C                   MOVE      ORHOLD        O2HOLD            4
     C                   MOVE      OROUTQ        O2OUTQ           10
     C                   MOVE      ORFORM        O2FORM           10
     C                   MOVE      ORSAVE        O2SAVE            4
     C                   ENDIF
     C*
     C     *IN31         IFEQ      *ON
     C                   EXSR      SRPRT1
     C                   ENDIF
     C*
     C     *IN32         IFEQ      *ON
     C                   EXSR      SRPRT2
     C                   ENDIF
     C*   END PROGRAM
     C*
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       If xxrpty = 'C' or xxrpty = ' ';
          Except Route1;
       Endif;
       If xxrpty = 'B' or xxrpty = ' ';
          Except Route2;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C******************************************************************
     C*          BILLING CHANGE REPORT
     C******************************************************************
     C     SRPRT1        BEGSR
     C*
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********         EXCEPT    ROUTE1
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C     *LOVAL        SETLL     PS401
     C                   READ      PS401                                  60
     C*   PRINT HEADINGS
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD11
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C*           MOVE DB FIELDS TO PRINTER FILE FIELDS
     C*
     C     S4DT          IFGE      XXFRD7
     C     S4DT          ANDLE     XXTOD7
     C*
     C                   Z-ADD     S4FEIN        X1FEIN            9 0
     C                   Z-ADD     S4MOD         X1MOD             3 0
     C*
     C                   Z-ADD     S4BGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1BGDT            8 0
     C*
     C                   MOVE      S4FLD         X1FLD            10
     C                   MOVEL     S4FVAL        F1VAL1           30
     C                   MOVEL     S4TVAL        T1VAL1           30
     C                   MOVE      S4USER        X1USER           10
     C*
     C                   Z-ADD     S4DT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1DT              8 0
     C*
     C                   MOVE      S4NWCD        X1NWCD            5
     C*
     C*
     C                   EXCEPT    DET1
      *
     C                   ADD       1             COUNT1            5 0
      *
     C                   ENDIF
     C*
     C* READ NEXT RECORD
     C                   READ      PS401                                  60
     C*
     C* CHECK OVERFLOW
     C*
     C     *INOF         IFEQ      '1'
     C     *IN60         ANDNE     '1'
     C                   EXCEPT    HEAD1
     C                   EXCEPT    HEAD11
     C                   MOVE      '0'           *INOF
     C                   ENDIF
     C                   ENDDO
      *
     C     COUNT1        IFNE      0
     C                   EXCEPT    TOT11
     C                   ELSE
     C                   EXCEPT    NORCD1
     C                   ENDIF
     C*
     C                   ENDSR
     C******************************************************************
     C*          CONTRACT CHANGE REPORT
     C******************************************************************
     C     SRPRT2        BEGSR
     C*
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C********           EXCEPT    ROUTE2
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C     *LOVAL        SETLL     PBD01
     C                   READ      PBD01                                  60
     C*   PRINT HEADINGS
     C                   EXCEPT    HEAD2
     C                   EXCEPT    HEAD21
     C                   EXCEPT    HEAD22
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     BDDT          IFGE      XXFRD7
     C     BDDT          ANDLE     XXTOD7
     C*          MOVE DB FIELDS TO PRINTER FILE FIELDS
     C*
     C                   MOVE      BDCODE        X2CODE            3
     C                   MOVE      BDTYPE        X2TYPE            2
     C*
     C                   Z-ADD     BDEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2EFDT            8 0
     C*
     C                   MOVE      BDFLD         X2FLD            10
     C                   MOVEL     BDFVAL        F2VAL2           30
     C                   MOVEL     BDTVAL        T2VAL2           30
     C                   MOVE      BDUSER        X2USER           10
     C*
     C                   Z-ADD     BDDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X2DT              8 0
     C*
     C                   MOVE      BDCPT4        X2CPT4            5
     C                   MOVE      BDPMOD        X2PMOD            2
     C                   MOVE      BD10A1        X210A1           10
     C                   MOVE      BD10A2        X210A2           10
     C                   MOVE      BD10A3        X210A3           10
     C                   MOVE      BD10A4        X210A4           10
     C*
     C     BDKY          CHAIN     PBD02                              99
     C                   MOVEL     BDCOM1        X2DESC           15
     C*
     C                   EXCEPT    DET21
     C                   EXCEPT    DET22
      *
     C                   ADD       1             COUNT2            5 0
      *
     C*
     C                   ENDIF
     C*
     C* READ NEXT RECORD
     C*
     C                   READ      PBD01                                  60
     C*
     C* CHECK OVERFLOW
     C*
     C     *INOV         IFEQ      '1'
     C     *IN60         ANDNE     '1'
     C                   EXCEPT    HEAD2
     C                   EXCEPT    HEAD21
     C                   EXCEPT    HEAD22
     C*
     C                   MOVE      '0'           *INOV
     C                   ENDIF
     C                   ENDDO
      *
     C     COUNT2        IFNE      0
     C                   EXCEPT    TOT22
     C                   ELSE
     C                   EXCEPT    NORCD2
     C                   ENDIF
     C*
     C                   ENDSR
     C******************************************************************
     C*
     OQSYSPRT   E            ROUTE1         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE1         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE1         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE1         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE1         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE1         1
     O**********                                 15 'ROUTE TO:'
     O**********             UPUSNM              67
     O**********E            ROUTE1         1
     O**********E            ROUTE1         1
     O**********E            ROUTE1         2
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE1         1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE1         2
     O                                           19 'REPORT TITLE:'
     O                       O1TITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE1         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             O1PNAM              30
     O**********                                 36 'FILE:'
     O**********             O1FIL#              38
     O**********                                 46 'COPIES:'
     O**********             O1COPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             O1HOLD              61
     O**********E            ROUTE1         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             O1OUTQ              30
     O**********                                 37 'FORMS:'
     O**********             O1FORM              48
     O**********                                 56 'SAVE:'
     O**********             O1SAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE1         2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE1         2
     O                                           17 'COMPANY   :'
     O                       XXCO#               22
     O          E            ROUTE1         1
     O                                           17 'DATE RANGE:'
     O                       XXFRDT              28 '  /  /    '
     O                                           31 '-'
     O                       XXTODT              42 '  /  /    '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE1
     O****                               20 '(C) COPYRIGHT FISERV'
     O****                               41 'INSURANCE SOLUTIONS,'
     O****                               63 'INC. 2008'
     O          E            ROUTE1         1 59
     O                       #BRND1
     O          E            ROUTE1         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD1          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              98
     O                       DSDATE        Y    122
     O          E            HEAD1          3
     O                       Q1PGM               10
     O                       O1TTLH             108
     O                                          116 'PAGE:'
     O                       PAGE          Z    122
     O          E            HEAD11         3
     O                                           11 'DATE RANGE:'
     O                       XXFRDT              22 '  /  /    '
     O                                           25 '-'
     O                       XXTODT              36 '  /  /    '
     O          E            HEAD11         2
     O                                            5 'FEIN '
     O                                           14 'MOD'
     O                                           24 'N/W CODE'
     O                                           36 'BEGIN DATE'
     O                                           48 'FIELD NAME'
     O                                           60 'FROM VALUE'
     O                                           90 'TO VALUE'
     O                                          117 'USER'
     O                                          126 'DATE'
     O          E            DET1           2
     O                       X1FEIN               9
     O                       X1MOD               14
     O                       X1NWCD              21
     O                       X1BGDT              36 '  /  /    '
     O                       X1FLD               48
     O                       F1VAL1              80
     O                       T1VAL1             112
     O                       X1USER             123
     O                       X1DT               132 '  /  /    '
     O          E            TOT11          2
     O                                           14 'REPORT TOTALS:'
     O                       COUNT1              21
     O*
     O          E            NORCD1
     O                                           76 'NO RECORDS MATCHED'
     O*
     O*
     O*
     OQSYSPRT2  E            ROUTE2         1 06
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE2         1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE2         1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE2         1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE2         3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE2         1
     O**********                                 15 'ROUTE TO:'
     O**********             UPUSNM              67
     O**********E            ROUTE2         1
     O**********E            ROUTE2         1
     O**********E            ROUTE2         2
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                       O2TITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
     O**********E            ROUTE2         2
     O**********                                 19 'PROGRAM NAME:'
     O**********             O2PNAM              30
     O**********                                 36 'FILE:'
     O**********             O2FIL#              38
     O**********                                 46 'COPIES:'
     O**********             O2COPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             O2HOLD              61
     O**********E            ROUTE2         2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             O2OUTQ              30
     O**********                                 37 'FORMS:'
     O**********             O2FORM              48
     O**********                                 56 'SAVE:'
     O**********             O2SAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE2         2
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE2         2
     O                                           17 'COMPANY   :'
     O                       XXCO#               22
     O          E            ROUTE2         1
     O                                           17 'DATE RANGE:'
     O                       XXFRDT              28 '  /  /    '
     O                                           31 '-'
     O                       XXTODT              42 '  /  /    '
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E  160           ROUTE2
     O***                                20 '(C) COPYRIGHT FISERV'
     O***                                41 'INSURANCE SOLUTIONS,'
     O***                                63 'INC. 2008'
     O          E            ROUTE2         1 59
     O                       #BRND1
     O          E            ROUTE2         1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              98
     O                       DSDATE        Y    122
     O          E            HEAD2          2
     O                       Q1PGM               10
     O                       O2TTLH             108
     O                                          116 'PAGE:'
     O                       PAGE          Z    122
     O          E            HEAD21         2
     O                                           11 'DATE RANGE:'
     O                       XXFRDT              22 '  /  /    '
     O                                           25 '-'
     O                       XXTODT              36 '  /  /    '
     O          E            HEAD21         2
     O                                            4 'CODE'
     O                                         +001 'TP'
     O                                         +001 'DESCRIPTION    '
     O                                         +001 ' EFF. DATE'
     O                                         +001 'FIELD NAME'
     O                                         +001 'FROM VALUE           '
     O                                         +000 '          '
     O                                         +001 'TO VALUE             '
     O                                         +000 '          '
     O                                         +001 'USER    '
     O                                         +001 '      DATE'
     O*
     O          E            HEAD22         2
     O                                           14 'PROCEDURE CODE'
     O                                           39 'PROCEDURE CODE MODIFIER'
     O                                           59 'REVENUE CODE RANGE'
     O                                           86 'PROCEDURE CODE RANGE'
     O*
     O          E            DET21          2
 3   O                       X2CODE               4
 2   O                       X2TYPE            +001
 15  O                       X2DESC            +001
 10  O                       X2EFDT            +001 '  /  /    '
 10  O                       X2FLD             +001
 30  O                       F2VAL2            +001
 30  O                       T2VAL2            +001
 10  O                       X2USER            +001
 10  O                       X2DT              +001 '  /  /    '
     O*
     O          E            DET22          3
     O                       X2CPT4               5
     O                       X2PMOD              18
     O                       X210A1              51
     O                                           53 '-'
     O                       X210A2              64
     O                       X210A3              76
     O                                           78 '-'
     O                       X210A4              89
     O          E            TOT22          2
     O                                           14 'REPORT TOTALS:'
     O                       COUNT2              21
     O*
     O          E            NORCD2
     O                                           76 'NO RECORDS MATCHED'
     O*
     O*
