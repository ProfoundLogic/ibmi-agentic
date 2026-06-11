     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W4092R - FINANCIAL EARNED PREMIUM REPORT
     H*****************************************************************
     H*  REVISIONS:                                                   *
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
     H*  04/08/91 JJJ  BASE MODIFICATION: CHG TOTALS FIELDS FROM      *
     H*  PROJ WC052    9.2 TO 11.2 FIELDS.                            *
     H*                                                               *
     H*****************************************************************
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWAFPL     IF   E           K DISK
     FQSYSPRT   O    F  199        PRINTER OFLIND(*INOA)
     FQSYSPRT1  O    F  199        PRINTER OFLIND(*INOF)

     D MNTH            S              9    DIM(12) CTDATA PERRCD(1)             NAME OF MNTH
     D MON             S              9    DIM(12)                              HEADING MNTH
     D SEQ             S              2  0 DIM(12)                              ORDER OF MNTH
     D AMT             S              9  2 DIM(12)                              AMOUNT
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  AM                     1    108  2
     D                                     DIM(12)
     D  FPAM01                 1      9  2
     D  FPAM02                10     18  2
     D  FPAM03                19     27  2
     D  FPAM04                28     36  2
     D  FPAM05                37     45  2
     D  FPAM06                46     54  2
     D  FPAM07                55     63  2
     D  FPAM08                64     72  2
     D  FPAM09                73     81  2
     D  FPAM10                82     90  2
     D  FPAM11                91     99  2
     D  FPAM12               100    108  2
     D                 DS
     D  YM                     1     60  0
     D                                     DIM(12)
     D  FPYM01                 1      5  0
     D  FPYM02                 6     10  0
     D  FPYM03                11     15  0
     D  FPYM04                16     20  0
     D  FPYM05                21     25  0
     D  FPYM06                26     30  0
     D  FPYM07                31     35  0
     D  FPYM08                36     40  0
     D  FPYM09                41     45  0
     D  FPYM10                46     50  0
     D  FPYM11                51     55  0
     D  FPYM12                56     60  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    FPEMP#        XXEMP#
     C     *LIKE         DEFINE    FPDIV         XXDIV
     C     *LIKE         DEFINE    FNANAM        XXCONM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDATE            6
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDATE        XXRNDT            6 0
     C                   MOVE      QQMODE        XXMODE            1
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
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    FPEMP#
     C                   KFLD                    FPDIV
     C*
     C*   KEY LIST FOR THE FINANCIAL EARNED PREMIUM WORK FILE
     C*
     C     FPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     FPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C                   TIME                    DSDTTM
     C*
     C*   SET UP AND INITIALIZE
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
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C     FNANAM        IFNE      *BLANKS
     C*
     C                   MOVEL     FNANAM        XXCONM
     C*
     C                   ELSE
     C*
     C                   MOVEL     CONAME        XXCONM
     C*
     C                   ENDIF                                                  FNANAM
     C*
     C                   MOVEL(P)  XXCONM        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      50            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXCONM
     C*
     C*
     C*   GET THE DISTRIBUTION INFORMATION AND PRINT
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C*******            MOVE      '0'           XXPRT
     C*******ORKEY         CHAIN     POR01                              60
     C********IN60         IFEQ      '0'
     C*******            EXCEPT    ROUTE
     C*******            END
      ***** End Del ***** 08/26/14 ************************************
     C*
     C*   START READING THE WORK FILE
     C*
     C     FPKEY         CHAIN     PFP01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C  N80              EXSR      SR1ST
     C*
     C     FPEMP#        IFNE      0
     C     FPDIV         ORNE      0
     C*
     C                   MOVE      *ZEROS        AMT
     C*
     C*   LOAD THE AMT ARRAY IN CORRECT SEQUENCE
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLT     13
     C     AM(X)         IFNE      0
     C                   MOVE      YM(X)         M                 2 0
     C                   Z-ADD     1             Y                 2 0
     C     M             LOOKUP    SEQ(Y)                                 60
     C                   Z-ADD     AM(X)         AMT(Y)
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C*   GET THE EMPLOYER NAME
     C*
     C                   MOVE      *BLANKS       XXNAME           15
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     EMNAME        XXNAME
     C                   END
     C*
     C*   ACCUMULATE QUARTERLY AND YEAR TO DATE TOTALS
     C*
     C     AMT(1)        ADD       AMT(2)        XXQRT1           11 2
     C                   ADD       AMT(3)        XXQRT1
     C     AMT(4)        ADD       AMT(5)        XXQRT2           11 2
     C                   ADD       AMT(6)        XXQRT2
     C     AMT(7)        ADD       AMT(8)        XXQRT3           11 2
     C                   ADD       AMT(9)        XXQRT3
     C     AMT(10)       ADD       AMT(11)       XXQRT4           11 2
     C                   ADD       AMT(12)       XXQRT4
     C                   Z-ADD     XXQRT1        XXYTD1           11 2
     C     XXYTD1        ADD       XXQRT2        XXYTD2           11 2
     C     XXYTD2        ADD       XXQRT3        XXYTD3           11 2
     C     XXYTD3        ADD       XXQRT4        XXYTD4           11 2
     C*
     C                   ADD       XXQRT1        XXQ1TL
     C                   ADD       XXQRT2        XXQ2TL
     C                   ADD       XXQRT3        XXQ3TL
     C                   ADD       XXQRT4        XXQ4TL
     C                   ADD       XXYTD1        XXY1TL
     C                   ADD       XXYTD2        XXY2TL
     C                   ADD       XXYTD3        XXY3TL
     C                   ADD       XXYTD4        XXY4TL
     C*
     C*   PRINT DETAIL LINE
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET
     C*
     C                   END
     C     FPKEY         READE     PFP01                                  60
     C                   END
     C*
     C*   PRINT FUND TOTALS
     C*
     C                   Z-ADD     0             XXEMP#
     C                   Z-ADD     0             XXDIV
     C                   MOVE      *ZEROS        AM
     C     FPKY1         CHAIN     PFP01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLT     13
     C     AM(X)         IFNE      0
     C                   MOVE      YM(X)         M                 2 0
     C                   Z-ADD     1             Y                 2 0
     C     M             LOOKUP    SEQ(Y)                                 60
     C                   Z-ADD     AM(X)         AMT(Y)
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C                   END
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    TOTL
     C*
      /Free
       // ***** Beg Add ***** 08/26/14 ********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       // ***** End Add ***** 08/26/14 ********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C****************************************************
     C*   SR1ST SUBROUTINE - INITAIL SETUP               *
     C****************************************************
     C     SR1ST         BEGSR
     C*
     C*   GET NAMES OF MONTHS
     C*
     C     F1KEY         CHAIN     PF101                              60
     C                   MOVEL     F1STPO        XXYYMM            5 0
     C                   MOVE      XXYYMM        M                 2 0
     C                   MOVE      *BLANK        MON
     C                   Z-ADD     1             X                 2 0
     C     X             DOWLT     13
     C                   MOVEA     MNTH(M)       MON(X)
     C                   MOVE      M             SEQ(X)
     C                   ADD       1             M
     C     M             IFEQ      13
     C                   Z-ADD     1             M
     C                   END
     C                   ADD       1             X
     C                   END
     C*
     C*   INITIALIZE TOTAL FIELDS
     C*
     C                   Z-ADD     0             XXQ1TL           11 2
     C                   Z-ADD     0             XXQ2TL           11 2
     C                   Z-ADD     0             XXQ3TL           11 2
     C                   Z-ADD     0             XXQ4TL           11 2
     C                   Z-ADD     0             XXY1TL           11 2
     C                   Z-ADD     0             XXY2TL           11 2
     C                   Z-ADD     0             XXY3TL           11 2
     C                   Z-ADD     0             XXY4TL           11 2
     C*
     C                   EXCEPT    HEAD
     C*
     C                   MOVE      '1'           *IN80
     C*
     C                   ENDSR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14  **********************************
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
      ***** End Del ***** 08/26/14  **********************************
      ***** Beg Add ***** 08/05/15 ***********************************
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
      ***** Beg Add ***** 08/05/15 ***********************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14  **********************************
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
      ***** End Del ***** 08/26/14  **********************************
     O          E            ROUTE       2  2
     O                                           24 'SELECTION CRITERIA'
     O          E            ROUTE          2
     O                                           18 'YEAR . . . :'
     O                       XXFYR         Z     27
     O          E            ROUTE          2
     O                                           18 'MONTH/YEAR :'
     O                       XXRNDT              27 '  /    '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                47 'VE DATA SOLUTIONS. INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             120
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             126
     O                                          190 'PAGE:'
     O                       PAGE               196
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O                                           84 'YEAR:'
     O                       XXFYR         Z     89
     O                                          103 'RUN DATE:'
     O                       XXRNDT             109 '  /    '
     O          E            HEAD           2
     O                                            9 'EMPLOYER'
     O                                           15 'DIV'
     O                                           31 'EMPLOYER NAME'
     O                       MON(1)              43
     O                       MON(2)              58
     O                       MON(3)              73
     O                                           87 'QUARTERLY'
     O                                          103 'YEAR TO DATE'
     O                       MON(4)             137
     O                       MON(5)             152
     O                       MON(6)             167
     O                                          181 'QUARTERLY'
     O                                          197 'YEAR TO DATE'
     O          E            DET            1
     O                       FPEMP#        Z     09
     O                       FPDIV         Z     15
     O                       XXNAME              31
     O                       AMT(1)        J     45
     O                       AMT(2)        J     60
     O                       AMT(3)        J     75
     O                       XXQRT1        J     90
     O                       XXYTD1        J    105
     O                       AMT(4)        J    137
     O                       AMT(5)        J    153
     O                       AMT(6)        J    168
     O                       XXQRT2        J    183
     O                       XXYTD2        J    198
     O          E            TOTL        1  1
     O                                           29 'TOTALS:'
     O                       AMT(1)        J     44
     O                       AMT(2)        J     59
     O                       AMT(3)        J     74
     O                       XXQ1TL        J     89
     O                       XXY1TL        J    104
     O                       AMT(4)        J    138
     O                       AMT(5)        J    153
     O                       AMT(6)        J    168
     O                       XXQ2TL        J    183
     O                       XXY2TL        J    198
     OQSYSPRT1  E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14  **********************************
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
      ***** End Del ***** 08/26/14  **********************************
      ***** Beg Add ***** 08/05/15  **********************************
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
      ***** End Add ***** 08/05/15  **********************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14  **********************************
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
      ***** End Del ***** 08/26/14  **********************************
     O          E            ROUTE       2  2
     O                                           24 'SELECTION CRITERIA'
     O          E            ROUTE          2
     O                                           18 'YEAR . . . :'
     O                       XXFYR         Z     27
     O          E            ROUTE          2
     O                                           18 'MONTH/YEAR :'
     O                       XXRNDT              27 '  /    '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                47 'VE DATA SOLUTIONS, INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM             120
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             126
     O                                          190 'PAGE:'
     O                       PAGE1              196
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O                                           84 'YEAR:'
     O                       XXFYR         Z     89
     O                                          103 'RUN DATE:'
     O                       XXRNDT             109 '  /    '
     O          E            HEAD           2
     O                                            9 'EMPLOYER'
     O                                           15 'DIV'
     O                                           31 'EMPLOYER NAME'
     O                       MON(7)              43
     O                       MON(8)              58
     O                       MON(9)              73
     O                                           87 'QUARTERLY'
     O                                          103 'YEAR TO DATE'
     O                       MON(10)            137
     O                       MON(11)            152
     O                       MON(12)            167
     O                                          181 'QUARTERLY'
     O                                          197 'YEAR TO DATE'
     O          E            DET            1
     O                       FPEMP#        Z     09
     O                       FPDIV         Z     15
     O                       XXNAME              31
     O                       AMT(7)        J     45
     O                       AMT(8)        J     60
     O                       AMT(9)        J     75
     O                       XXQRT3        J     90
     O                       XXYTD3        J    105
     O                       AMT(10)       J    138
     O                       AMT(11)       J    153
     O                       AMT(12)       J    168
     O                       XXQRT4        J    183
     O                       XXYTD4        J    198
     O          E            TOTL        1  1
     O                                           29 'TOTALS:'
     O                       AMT(7)        J     44
     O                       AMT(8)        J     59
     O                       AMT(9)        J     74
     O                       XXQ3TL        J     89
     O                       XXY3TL        J    104
     O                       AMT(10)       J    138
     O                       AMT(11)       J    153
     O                       AMT(12)       J    168
     O                       XXQ4TL        J    183
     O                       XXY4TL        J    198
**
JANUARY
FEBRUARY
MARCH
APRIL
MAY
JUNE
JULY
AUGUST
SEPTEMBER
OCTOBER
NOVEMBER
DECEMBER
