     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7403R - AGENT PREMIUM LATE NOTICE REPORT
     H****************************************************************
     H**  MODIFICATIONS:
     H**
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H**   04/10/00  C:118 PROJECT: WC987
     H**                   LOAD ALTERNATE COMPANY NAME FROM FUND MASTER
     H**
     H**   08/05/91  TAA   LOAD BLANKS FOR A/R TRANSACTION CODES.
     H**   PROJ: AJC       USER CAN SELECT DESIRED CODES OR LEAVE
     H**                   BLANK FOR ALL CODES.
     H**
     H****************************************************************
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
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

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
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCODE           60
     C                   PARM                    QQQUE            10
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
     C                   MOVE      QQDATE        XXRNDT            8 0
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1RNDT            6 0
     C                   MOVEA     QQCODE        COD
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
     C*
     C     APKEY         KLIST
     C                   KFLD                    XXCO#                                      T
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    LNPROD
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
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     XXFYR         ADD       1900          XXYEAR            4 0
     C                   TIME                    DSDTTM
     C*
     C*   SET UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
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
     C*
     C*   SORT THE CODE ARRAY
     C*
     C                   SORTA     COD
     C*
     C*   FILL THE CODE DESCRIPTION ARRAY CORRESPONDING TO CODE ARRAY
     C*
     C                   MOVE      *BLANKS       CDE
     C                   MOVE      *BLANKS       DES
     C     QQCODE        IFNE      *BLANKS
     C                   Z-ADD     1             Y                 2 0
     C                   Z-ADD     1             Z                 2 0
     C     Y             DOUEQ     31
     C*
     C     COD(Y)        IFNE      *BLANKS
     C     Z             ADD       19            Z1                2 0
     C                   MOVE      '1'           *IN(Z1)
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     COD(Y)        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     COD(Y)        CDE(Z)
     C                   MOVEL     TTDESC        DES(Z)
     C                   ADD       1             Z
     C                   END
     C                   END
     C*
     C                   ADD       1             Y
     C                   END
     C*
     C                   ELSE                                                   COD = BLANKS
     C*
     C* PRINT *ALL CODES ON ROUTING PAGE
     C*
     C                   MOVE      '*ALL'        RSCODE            4
     C*
     C                   END                                                    COD = BLANKS
     C*
     C*   GET THE DISTRIBUTION INFORMATION &PRINT
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
     C                   Z-ADD     0             XXPCNT            5 0
     C                   Z-ADD     0             XXACNT            5 0
     C                   Z-ADD     0             XXPCUR            9 2
     C                   Z-ADD     0             XXACUR            9 2
     C                   Z-ADD     0             XXPR30            9 2
     C                   Z-ADD     0             XXAG30            9 2
     C                   Z-ADD     0             XXPR60            9 2
     C                   Z-ADD     0             XXAG60            9 2
     C                   Z-ADD     0             XXPR90            9 2
     C                   Z-ADD     0             XXAG90            9 2
     C                   Z-ADD     0             XXPRTL            9 2
     C                   Z-ADD     0             XXAGTL            9 2
     C*
     C*   START READING THE WORK FILE
     C*
     C     LNKEY         CHAIN     PLN01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C  N80              EXSR      SR1ST
     C*
     C     XXAGT#        IFNE      LNAGT#
     C*
     C*   PRINT PRODUCER AND AGENT TOTALS
     C*
     C                   ADD       XXPCUR        XXACUR
     C                   ADD       XXPR30        XXAG30
     C                   ADD       XXPR60        XXAG60
     C                   ADD       XXPR90        XXAG90
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
     C                   Z-ADD     0             XXACNT
     C                   Z-ADD     0             XXPCUR
     C                   Z-ADD     0             XXACUR
     C                   Z-ADD     0             XXPR30
     C                   Z-ADD     0             XXAG30
     C                   Z-ADD     0             XXPR60
     C                   Z-ADD     0             XXAG60
     C                   Z-ADD     0             XXPR90
     C                   Z-ADD     0             XXAG90
     C                   Z-ADD     0             XXPRTL
     C                   Z-ADD     0             XXAGTL
     C*
     C                   MOVE      *BLANKS       APPROD
     C                   MOVE      *BLANKS       XXNAME
     C*
     C*   GET NEXT AGENT & PRODUCER NAME
     C*
     C     AAKEY         CHAIN     PAA01                              60
     C     APKEY         CHAIN     PAP01                              60
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
     C                   MOVE      *BLANKS       APPROD
     C                   MOVE      *BLANKS       XXNAME
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
     C                   ADD       XXPCUR        XXACUR
     C                   ADD       XXPR30        XXAG30
     C                   ADD       XXPR60        XXAG60
     C                   ADD       XXPR90        XXAG90
     C                   ADD       XXPRTL        XXAGTL
     C                   Z-ADD     0             XXPCNT
     C                   Z-ADD     0             XXPCUR
     C                   Z-ADD     0             XXPR30
     C                   Z-ADD     0             XXPR60
     C                   Z-ADD     0             XXPR90
     C                   Z-ADD     0             XXPRTL
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
     C                   ADD       LNCURR        XXPCUR
     C                   ADD       LN30DY        XXPR30
     C                   ADD       LN60DY        XXPR60
     C                   ADD       LN90DY        XXPR90
     C                   ADD       LNAMT         XXPRTL
     C*   GET EMPLOYER NAME
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   MOVEL     EMNAME        XXENME           29
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
     C     LNKEY         READE     PLN01                                  60
     C                   END
     C*
     C*   PRINT PRODUCER AND AGENT TOTALS
     C*
     C                   ADD       XXPCUR        XXACUR
     C                   ADD       XXPR30        XXAG30
     C                   ADD       XXPR60        XXAG60
     C                   ADD       XXPR90        XXAG90
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
     O                  30                       50 '11.'
     O                       CDE(11)             53
     O                       DES(11)             84
     O                  40                       89 '21.'
     O                       CDE(21)             92
     O                       DES(21)            123
     O          E            ROUTE          1
     O                  21                       11 '2.'
     O                       CDE(2)              14
     O                       DES(2)              45
     O                  31                       50 '12.'
     O                       CDE(12)             53
     O                       DES(12)             84
     O                  41                       89 '22.'
     O                       CDE(22)             92
     O                       DES(22)            123
     O          E            ROUTE          1
     O                  22                       11 '3.'
     O                       CDE(3)              14
     O                       DES(3)              45
     O                  32                       50 '13.'
     O                       CDE(13)             53
     O                       DES(13)             84
     O                  42                       89 '23.'
     O                       CDE(23)             92
     O                       DES(23)            123
     O          E            ROUTE          1
     O                  23                       11 '4.'
     O                       CDE(4)              14
     O                       DES(4)              45
     O                  33                       50 '14.'
     O                       CDE(14)             53
     O                       DES(14)             84
     O                  43                       89 '24.'
     O                       CDE(24)             92
     O                       DES(24)            123
     O          E            ROUTE          1
     O                  24                       11 '5.'
     O                       CDE(5)              14
     O                       DES(5)              45
     O                  34                       50 '15.'
     O                       CDE(15)             53
     O                       DES(15)             84
     O                  44                       89 '25.'
     O                       CDE(25)             92
     O                       DES(25)            123
     O          E            ROUTE          1
     O                  25                       11 '6.'
     O                       CDE(6)              14
     O                       DES(6)              45
     O                  35                       50 '16.'
     O                       CDE(16)             53
     O                       DES(16)             84
     O                  45                       89 '26.'
     O                       CDE(26)             92
     O                       DES(26)            123
     O          E            ROUTE          1
     O                  26                       11 '7.'
     O                       CDE(7)              14
     O                       DES(7)              45
     O                  36                       50 '17.'
     O                       CDE(17)             53
     O                       DES(17)             84
     O                  46                       89 '27.'
     O                       CDE(27)             92
     O                       DES(27)            123
     O          E            ROUTE          1
     O                  27                       11 '8.'
     O                       CDE(8)              14
     O                       DES(8)              45
     O                  37                       50 '18.'
     O                       CDE(18)             53
     O                       DES(18)             84
     O                  47                       89 '28.'
     O                       CDE(28)             92
     O                       DES(28)            123
     O          E            ROUTE          1
     O                  28                       11 '9.'
     O                       CDE(9)              14
     O                       DES(9)              45
     O                  38                       50 '19.'
     O                       CDE(19)             53
     O                       DES(19)             84
     O                  48                       89 '29.'
     O                       CDE(29)             92
     O                       DES(29)            123
     O          E            ROUTE          1
     O                  29                       11 '10.'
     O                       CDE(10)             14
     O                       DES(10)             45
     O                  39                       50 '20.'
     O                       CDE(20)             53
     O                       DES(20)             84
     O                  49                       89 '30.'
     O                       CDE(30)             92
     O                       DES(30)            123
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
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
     O                       CONAME              93
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          119 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           1
     O                                          123 'RUN DATE:'
     O                       X1RNDT        Y    132
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O                                          126 'POLICY YEAR:'
     O                       XXFYR         Z    132
     O          E            HEAD           1 18
     O                                           06 'AGENT:'
     O                       AAAGT#              12
     O                       AGNAME              53
     O          E            HEAD           1
     O                       AGMAD1              53
     O          E            HEAD           1
     O                       XXCSZ               53
     O          E            HEAD           1 24
     O                                          101 '30 DAYS        60 DAYS'
     O                                          115 '90 DAYS OR'
     O          E            HEAD           2
     O                                           18 'EMPLOYER'
     O                                           33 'DIV     NAME'
     O                                           86 'CURRENT       PAST DUE'
     O                                          116 'PAST DUE  MORE PAST DUE'
     O                                          131 'TOTAL DUE'
     O          E            DET            2
     O                                           09 'PRODUCER:'
     O                       APPROD        Z     15
     O                       XXNAME              56
     O          E            DET2           1
     O                       EMEMP#        Z     18
     O                       EMDIV         Z     24
     O                       XXENME              58
     O                       LNCURR        J     72
     O                       LN30DY        J     87
     O                       LN60DY        J    102
     O                       LN90DY        J    117
     O                       LNAMT         J    132
     O          E            DET3        1  2
     O                                           44 'PRODUCER COUNT:'
     O                       XXPCNT        Z     50
     O                                           58 'TOTAL:'
     O                       XXPCUR        J     72
     O                       XXPR30        J     87
     O                       XXPR60        J    102
     O                       XXPR90        J    117
     O                       XXPRTL        J    132
     O          E            DET4           1
     O                                           44 'AGENT COUNT   :'
     O                       XXACNT        Z     50
     O                                           58 'TOTAL:'
     O                       XXACUR        J     72
     O                       XXAG30        J     87
     O                       XXAG60        J    102
     O                       XXAG90        J    117
     O                       XXAGTL        J    132
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*
