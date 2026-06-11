     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H**********************************************************************
     H* REVISIONS:                                                         *
     H*                                                                    *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987
     H*                Load alternate company name from fund master
     H*
     H* 05/18/92 LPK  PROJECT WC145                                        *
     H*               COMMENTED OUT THE $5.00 MINIMUM AMOUNT.              *
     H**********************************************************************
     H/TITLE W7402R - AGENT LATE NOTICE REPORT
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMAPL     IF   E           K DISK
     FWXLNL     IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOA)

     D COD             S              2    DIM(30) ASCEND                       A/R CODES
     D DES             S             30    DIM(30)                              A/R CODE DESC
     D CDE             S              2    DIM(30)                              AR CDE FOR OUTPUT
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D                 DS
     D  AGMZIP                 1      9  0
     D  XXZPM1                 1      5  0
     D  XXZPM2                 6      9  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDATE            8            JJJ 9-2-88
     C                   PARM                    QQCODE           60            |
     C                   PARM                    QQQUE            10            MLS 1/19/88
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDATE        XXRNDT            8 0          JJJ 9-2-88
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1RNDT            6 0
     C                   MOVEA     QQCODE        COD                            |
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*                                                    MLS 1/19/88
     C*  USER PARAMETER FILE                                    "
     C*                                                         "
     C     UPKEY         KLIST                                                       "
     C                   KFLD                    Q1USER                              "
     C*
     C*   KEY LIST FOR THE GROUP
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    LNEMP#
     C                   KFLD                    LNDIV
     C*
     C*   KEY LIST FOR THE LATE NOTICE WORK FILE
     C*
     C     LNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE AGENT PRODUCER FILE
     C*
     C     APKEY         KLIST
     C                   KFLD                    XXCO#                            "         T
     C                   KFLD                    AAFEIN                           "
     C                   KFLD                    AAMOD                            "
     C                   KFLD                    LNPROD                           "
     C*
     C*   KEY LIST FOR THE AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*   KEY LIST FOR THE AGENT ASSIGNMENT FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    LNAGT#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* KEY LIST FOR TABLE FILE
     C*
     C     TBKEY         KLIST                                                        "
     C                   KFLD                    XXCO#                                "
     C                   KFLD                    XXKEY            10                  "
     C*
     C     XXFYR         ADD       1900          XXYEAR            4 0
     C                   TIME                    DSDTTM
     C*                                                    MLS 1/19/88
     C*   SET UP AND INITIALIZE                                 "
     C*                                                         "
     C     QQQUE         IFNE      ' '                                               "
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE                                                        "
     C                   SETON                                        08        BATCH MODE
     C                   END                                                         "
     C*                                                         "
     C*   CHAIN TO THE USER PARAMETER FILE                      "
     C*                                                         "
     C     UPKEY         CHAIN     PUP01                              60             "
     C*
     C*   CHAIN TO THE COMPANY FILE
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
     C*   CHAIN TO THE GROUP FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C***** END   ADD **** 04/10/00 ***********************************
     C*                                                    JJJ 9-2-88
     C*   SORT THE CODE ARRAY                              |
     C*                                                    |
     C                   SORTA     COD                                          |
     C*                                                    |
     C*   FILL THE CODE DESCRIPTION ARRAY CORRESPONDING TO CODE ARRAY
     C*                                                    |
     C                   MOVE      *BLANKS       CDE                            |
     C                   MOVE      *BLANKS       DES                            |
     C     QQCODE        IFNE      *BLANKS
     C                   Z-ADD     1             Y                 2 0          |
     C                   Z-ADD     1             Z                 2 0          |
     C     Y             DOUEQ     31                                           |
     C*
     C*   INDICATORS 20 TO 49 ARE USED FOR OUTPUT PURPOSES.
     C*                                                    |
     C     COD(Y)        IFNE      *BLANKS                                      |
     C     Z             ADD       19            Z1                2 0          |
     C                   MOVE      '1'           *IN(Z1)                        |
     C                   MOVE      *BLANKS       XXKEY                          |
     C                   MOVEL     COD(Y)        XXKEY                          |
     C     TBKEY         CHAIN     PTT01                              60        |
     C     *IN60         IFEQ      '0'                                          |
     C                   MOVEL     COD(Y)        CDE(Z)                         |
     C                   MOVEL     TTDESC        DES(Z)                         |
     C                   ADD       1             Z                              |
     C                   END                                                    |
     C                   END                                                    |
     C*                                                    |
     C                   ADD       1             Y                              |
     C                   END                                                    |
     C*
     C*
     C                   ELSE                                                   COD = BLANKS
     C*
     C* PRINT *ALL CODES ON ROUTING PAGE
     C*
     C                   MOVE      '*ALL'        RSCODE            4
     C*
     C                   END                                                    COD = BLANKS
     C*   GET THE DISTRIBUTION INFORMATION AND PRINT
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C     *LIKE         DEFINE    LNAGT#        XXAGT#
     C     *LIKE         DEFINE    LNPROD        XXPROD
     C                   Z-ADD     0             XXPRTL            9 2
     C                   Z-ADD     0             XXAGTL            9 2
     C                   Z-ADD     0             XXPCNT            5 0
     C                   Z-ADD     0             XXACNT            5 0
     C*
     C*   START READING THE WORK FILE
     C*
     C     LNKEY         CHAIN     PLN01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     LNAMT         IFGT      0
     C*
     C  N80              EXSR      SR1ST
     C*
     C     XXAGT#        IFNE      LNAGT#
     C*
     C*
     C*   PRINT PRODUCER AND AGENT TOTALS
     C                   ADD       XXPRTL        XXAGTL
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET3
     C                   EXCEPT    DET4
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        61
     C*
     C                   Z-ADD     0             PAGE
     C                   Z-ADD     0             XXPCNT
     C                   Z-ADD     0             XXPRTL
     C                   Z-ADD     0             XXAGTL
     C                   Z-ADD     0             XXACNT
     C*
     C                   MOVE      *BLANKS       APPROD                         LJQ 5-2-88
     C                   MOVE      *BLANKS       XXNAME                             "
     C*
     C*   GET NEXT AGENT & PRODUCER NAME
     C     AAKEY         CHAIN     PAA01                              60
     C     APKEY         CHAIN     PAP01                              60        LJQ 4-29-88
     C     AGKEY         CHAIN     PAG01                              60
     C  N60              EXSR      SRADDR
     C                   EXCEPT    HEAD
     C                   EXSR      SRPNAM
     C                   EXCEPT    DET
     C*
     C                   Z-ADD     LNAGT#        XXAGT#
     C                   Z-ADD     LNPROD        XXPROD
     C*
     C                   ELSE
     C*
     C*
     C     XXPROD        IFNE      LNPROD
     C*
     C                   MOVE      *BLANKS       APPROD                         LJQ 5-2-88
     C                   MOVE      *BLANKS       XXNAME                             "
     C*
     C*   PRINT PRODUCER COUNT AND TOTALS
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET3
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        61
     C*
     C                   ADD       XXPRTL        XXAGTL
     C                   Z-ADD     0             XXPRTL
     C                   Z-ADD     0             XXPCNT
     C*
     C*   GET NEXT PRODUCER NAME
     C     APKEY         CHAIN     PAP01                              60
     C                   EXSR      SRPNAM
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET
     C*
     C                   Z-ADD     LNPROD        XXPROD
     C*
     C                   END
     C*
     C                   END
     C*
     C                   ADD       LNAMT         XXPRTL
     C*   GET EMPLOYER NAME
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET2
     C                   ADD       1             XXPCNT
     C                   ADD       1             XXACNT
     C*
     C                   END
     C*
     C     LNKEY         READE     PLN01                                  60
     C                   END
     C*
     C*   PRINT PRODUCER AND AGENT TOTALS
     C*
     C                   ADD       XXPRTL        XXAGTL
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C*  SELECTION CRITERIA NOT FOUND
     C*
     C     *IN61         IFEQ      '1'
     C                   EXCEPT    DET3
     C                   EXCEPT    DET4
     C                   ELSE
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOREC
     C                   ENDIF
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
     C****************************************************
     C*   SUBROUTINE TO LOAD THE AGENT CITY STATE & ZIP  *
     C****************************************************
     C     SRADDR        BEGSR
     C                   MOVE      *BLANKS       SZ
     C                   MOVE      *BLANKS       ST
     C                   MOVE      *BLANKS       XXZP1
     C                   MOVE      *BLANKS       XXZP2
     C                   MOVEL     AGMST         XXST             13
     C                   MOVEA     XXST          SZ
     C                   MOVE      XXZPM1        XXZP1             5
     C                   MOVEA     XXZP1         SZ(4)
     C     XXZPM2        IFNE      0
     C                   MOVE      '-'           SZ(9)
     C                   MOVE      XXZPM2        XXZP2             4
     C                   MOVEA     XXZP2         SZ(10)
     C                   END
     C*
     C                   MOVE      *BLANKS       XXSZ             13
     C                   MOVEA     SZ            XXSZ
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     AGMCTY        XXLAST
     C                   MOVEL     XXSZ          XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXCSZ            40
     C                   ENDSR
     C*
     C*
     C****************************************************
     C*   SUBROUTINE TO LOAD THE PRODUCER NAME           *
     C****************************************************
     C     SRPNAM        BEGSR
     C                   MOVE      *BLANKS       XXNAME                         LJQ 5-2-88
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     APFSN         XXFRST
     C                   MOVEL     APLSN         XXLAST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNAME           40
     C                   ENDSR
     C*
     C*
     C****************************************************
     C*   SR1ST SUBROUTINE - INITAIL SETUP               *
     C****************************************************
     C     SR1ST         BEGSR
     C*
     C*   GET NEXT AGENT & PRODUCER NAME
     C     AAKEY         CHAIN     PAA01                              60
     C     APKEY         CHAIN     PAP01                              60        LJQ 5-2-88
     C     AGKEY         CHAIN     PAG01                              60
     C  N60              EXSR      SRADDR
     C                   EXCEPT    HEAD
     C                   EXSR      SRPNAM
     C                   EXCEPT    DET
     C*
     C                   MOVE      '1'           *IN80
     C*
     C                   Z-ADD     LNAGT#        XXAGT#
     C                   Z-ADD     LNPROD        XXPROD
     C*
     C*
     C                   ENDSR
     C*
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
     O*  MLS 1/19/88  ADD INDICATORS
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
     O*  MLS 1/19/88  ADD INDICATORS
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
     O**********E            ROUTE          3
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O*  MLS 1/19/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O*  SELECTION CRITERIA ADDED TO ROUTING PAGE
     O*
     O          E            ROUTE          2
     O                                           25 'SELECTION CRITERIA:'
     O          E            ROUTE          1
     O                                           18 'GROUP  . . :'
     O                       FNFND         Z     22
     O                       FNNAME            +  2
     O          E            ROUTE          1
     O                                           18 'POLICY YEAR:'
     O                       XXFYR         Z     22
     O          E            ROUTE          2
     O                                           18 'RUN DATE   :'
     O                       SRDTI         Y     29
     O*
     O          E            ROUTE          1 38
     O                                           30 'A/R CODES INCLUDED IN'
     O                                           42 'CALCULATION'
     O          E      N20   ROUTE          1
     O                       RSCODE              26
     O          E            ROUTE          1
     O                  20                       11 '1.'
     O                       CDE(1)              14
     O                       DES(1)              45
     O                  35                       50 '16.'
     O                       CDE(16)             53
     O                       DES(16)             84
     O          E            ROUTE          1
     O                  21                       11 '2.'
     O                       CDE(2)              14
     O                       DES(2)              45
     O                  36                       50 '17.'
     O                       CDE(17)             53
     O                       DES(17)             84
     O          E            ROUTE          1
     O                  22                       11 '3.'
     O                       CDE(3)              14
     O                       DES(3)              45
     O                  37                       50 '18.'
     O                       CDE(18)             53
     O                       DES(18)             84
     O          E            ROUTE          1
     O                  23                       11 '4.'
     O                       CDE(4)              14
     O                       DES(4)              45
     O                  38                       50 '19.'
     O                       CDE(19)             53
     O                       DES(19)             84
     O          E            ROUTE          1
     O                  24                       11 '5.'
     O                       CDE(5)              14
     O                       DES(5)              45
     O                  39                       50 '20.'
     O                       CDE(20)             53
     O                       DES(20)             84
     O          E            ROUTE          1
     O                  25                       11 '6.'
     O                       CDE(6)              14
     O                       DES(6)              45
     O                  40                       50 '21.'
     O                       CDE(21)             53
     O                       DES(21)             84
     O          E            ROUTE          1
     O                  26                       11 '7.'
     O                       CDE(7)              14
     O                       DES(7)              45
     O                  41                       50 '22.'
     O                       CDE(22)             53
     O                       DES(22)             84
     O          E            ROUTE          1
     O                  27                       11 '8.'
     O                       CDE(8)              14
     O                       DES(8)              45
     O                  42                       50 '23.'
     O                       CDE(23)             53
     O                       DES(23)             84
     O          E            ROUTE          1
     O                  28                       11 '9.'
     O                       CDE(9)              14
     O                       DES(9)              45
     O                  43                       50 '24.'
     O                       CDE(24)             53
     O                       DES(24)             84
     O          E            ROUTE          1
     O                  29                       11 '10.'
     O                       CDE(10)             14
     O                       DES(10)             45
     O                  44                       50 '25.'
     O                       CDE(25)             53
     O                       DES(25)             84
     O          E            ROUTE          1
     O                  30                       11 '11.'
     O                       CDE(11)             14
     O                       DES(11)             45
     O                  45                       50 '26.'
     O                       CDE(26)             53
     O                       DES(26)             84
     O          E            ROUTE          1
     O                  31                       11 '12.'
     O                       CDE(12)             14
     O                       DES(12)             45
     O                  46                       50 '27.'
     O                       CDE(27)             53
     O                       DES(27)             84
     O          E            ROUTE          1
     O                  32                       11 '13.'
     O                       CDE(13)             14
     O                       DES(13)             45
     O                  47                       50 '28.'
     O                       CDE(28)             53
     O                       DES(28)             84
     O          E            ROUTE          1
     O                  33                       11 '14.'
     O                       CDE(14)             14
     O                       DES(14)             45
     O                  48                       50 '29.'
     O                       CDE(29)             53
     O                       DES(29)             84
     O          E            ROUTE          1
     O                  34                       11 '15.'
     O                       CDE(15)             14
     O                       DES(15)             45
     O                  49                       50 '30.'
     O                       CDE(30)             53
     O                       DES(30)             84
     O          E            ROUTE          1
     O          E            ROUTE          1
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              62
     O                       DSDATE        Y     85
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              65
     O                                           72 'PAGE:'
     O                       PAGE          Z     85
     O          E            HEAD           1
     O                                           76 'RUN DATE:'
     O                       X1RNDT        Y     85
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O                                           79 'POLICY YEAR:'
     O                       XXFYR         Z     85
     O          E            HEAD           1 18
     O                                           06 'AGENT:'
     O                       AAAGT#              12
     O                       AGNAME              53
     O          E            HEAD           1
     O                       AGMAD1              53
     O          E            HEAD           1
     O                       XXCSZ               53
     O          E            HEAD           2 24
     O                                           18 'EMPLOYER'
     O                                           34 'DIV      NAME'
     O                                           84 'AMOUNT DUE'
     O          E            DET            2
     O                                           09 'PRODUCER:'
     O                       APPROD        Z     15
     O                       XXNAME              56
     O          E            DET2           1
     O                       EMEMP#        Z     18
     O                       EMDIV         Z     24
     O                       EMNAME              70
     O                       LNAMT         J     85
     O          E            DET3        1  2
     O                                           45 'PRODUCER COUNT:'
     O                       XXPCNT        Z     51
     O                                           71 'TOTAL:'
     O                       XXPRTL        J     85
     O          E            DET4           1
     O                                           45 'AGENT COUNT   :'
     O                       XXACNT        Z     51
     O                                           71 'TOTAL:'
     O                       XXAGTL        J     85
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*
