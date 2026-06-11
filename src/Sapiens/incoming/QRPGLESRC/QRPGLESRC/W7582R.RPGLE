     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7582R - Agent Late Notice Report Self Reporting Employers
     H*
     H*================================================================
     H* REVISIONS
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/10/00  C:118  PROJECT: WC987
     H*                  LOAD ALTERNATE COMPANY NAME FROM FUND MASTER
     H*
     H*================================================================
     F* Company Master File
     FSMCOL     IF   E           K DISK
     F*
     F* User Parameter Master File
     FWMUPL     IF   E           K DISK
     F*
     F* Output Routing File
     FSMORL     IF   E           K DISK
     F*
     F* GROUP Master File
     FWMFNL     IF   E           K DISK
     F*
     F* Employer Master File
     FWMEML     IF   E           K DISK
     F*
     F* Agent Late Notice Work File  - Built from W7581R
     FWXL1L     IF   E           K DISK
     F*
     F* Agent Producer Master FIle
     FWMAPL     IF   E           K DISK
     F*
     F* Agent Master File
     FWMAGL     IF   E           K DISK
     F*
     F* Agent Assignment Master File
     FWMAAL1    IF   E           K DISK
     F*
     F* Agent Late Notice Report Spool File
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOA)

     D*
     D* Define Table, Arrays and Data Structures
     D*
     D SZ              S              1    DIM(13)                              STATE & ZIP
     D ST              S              2    DIM(1)                               STATE
     D*
     D* Agent Zip Code
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
     C* Key List for the Company Master File
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C* Key List for the User Parameter Master File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C* Key List for the Output Routing File
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C* Key List for the GROUP Master File
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Employer Master File
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    L1EMP#
     C                   KFLD                    L1DIV
     C*
     C* Key List for the Agent Late Notice Work File
     C*
     C     L1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C* Key List for the Agent Producer Master File
     C*
     C     APKEY         KLIST
     C                   KFLD                    XXCO#                                      T
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C                   KFLD                    L1PROD
     C*
     C* Key List for the Agent Master File
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C* Key List for the Agent Assignment Master File
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    L1AGT#
     C*
     C* Entry Parameters
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE           10
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQDTE         XXDTE             8 0
     C*
     C* Set Up and Initialize
     C*
     C     XXFYR         ADD       1900          XXYEAR            4 0
     C                   TIME                    DSDTTM
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C* Get the User Parameter Record
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* Get the Company Master Record
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
     C* Get the GROUP Record
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
     C* Get the Distribution/Routing Record
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C* Print the Distribution Information
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C     *LIKE         DEFINE    L1AGT#        XXAGT#
     C     *LIKE         DEFINE    L1PROD        XXPROD
     C                   Z-ADD     0             XXPCNT            5 0
     C                   Z-ADD     0             XXACNT            5 0
     C*
     C*   Start Reading the Work File
     C*
     C     L1KEY         CHAIN     PL101                              60
     C     *IN60         DOWEQ     '0'
     C*
     C  N80              EXSR      SR1ST
     C*
     C     XXAGT#        IFNE      L1AGT#
     C*
     C* Print Producer and Agent Totals
     C*
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET3
     C                   EXCEPT    DET4
     C                   Z-ADD     0             PAGE
     C                   Z-ADD     0             XXPCNT
     C                   Z-ADD     0             XXACNT
     C*
     C                   MOVE      *BLANKS       APPROD
     C                   MOVE      *BLANKS       XXNAME
     C*
     C* Get the Next Agent & Producer Name
     C*
     C     AAKEY         CHAIN     PAA01                              60
     C     APKEY         CHAIN     PAP01                              60
     C     AGKEY         CHAIN     PAG01                              60
     C  N60              EXSR      SRADDR
     C                   EXCEPT    HEAD
     C                   EXSR      SRPNAM
     C                   EXCEPT    DET
     C*
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        61
     C*
     C*
     C                   Z-ADD     L1AGT#        XXAGT#
     C                   Z-ADD     L1PROD        XXPROD
     C*
     C                   ELSE
     C*
     C*
     C     XXPROD        IFNE      L1PROD
     C*
     C                   MOVE      *BLANKS       APPROD
     C                   MOVE      *BLANKS       XXNAME
     C*
     C* Print Producer Count
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET3
     C                   Z-ADD     0             XXPCNT
     C*
     C* Get the Next Producer Name
     C*
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
     C*  SELECTION CRITERIA FOUND
     C*
     C                   SETON                                        61
     C*
     C                   Z-ADD     L1PROD        XXPROD
     C*
     C                   END
     C*
     C                   END
     C*
     C* Get the Employer's Name
     C*
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
     C     L1KEY         READE     PL101                                  60
     C                   END
     C*
     C* Print Producer and Agent Totals
     C*
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
     C*
     C*****************************************************************
     C* SRADDR - Subroutine to Load the Agent City State & Zip        *
     C*****************************************************************
     C*
     C     SRADDR        BEGSR
     C*
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
     C*
     C                   ENDSR
     C*
     C*
     C*****************************************************************
     C* SRPNAM - Subroutine to Load the Producer Name                 *
     C*****************************************************************
     C*
     C     SRPNAM        BEGSR
     C*
     C                   MOVE      *BLANKS       XXNAME
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
     C*
     C                   ENDSR
     C*
     C*
     C****************************************************************
     C* SR1ST - Subroutine for Initial Setup                         *
     C****************************************************************
     C*
     C     SR1ST         BEGSR
     C*
     C* Get the Agent's and Producer's Names
     C*
     C     AAKEY         CHAIN     PAA01                              60
     C     APKEY         CHAIN     PAP01                              60
     C     AGKEY         CHAIN     PAG01                              60
     C  N60              EXSR      SRADDR
     C                   EXCEPT    HEAD
     C                   EXSR      SRPNAM
     C                   EXCEPT    DET
     C*
     C                   MOVE      '1'           *IN80
     C*
     C                   Z-ADD     L1AGT#        XXAGT#
     C                   Z-ADD     L1PROD        XXPROD
     C*
     C*
     C                   ENDSR
     C*
     C*
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
     O                       XXDTE         Y     29
     O*
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               23 '(c) Copyright Progressi'
     O****                                  've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              68
     O                       DSDATE        Y     84
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              72
     O                                           73 'PAGE:'
     O                       PAGE          Z     84
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O                                           80 'POLICY YEAR:'
     O                       XXFYR         Z     84
     O          E            HEAD           1 18
     O                                           06 'AGENT:'
     O                       AAAGT#        Z     12
     O                       AGNAME              53
     O          E            HEAD           1
     O                       AGMAD1              53
     O          E            HEAD           1
     O                       XXCSZ               53
     O          E            HEAD           2 24
     O                                           18 'EMPLOYER'
     O                                           34 'DIV   NAME'
     O          E            DET            2
     O                                           09 'PRODUCER:'
     O                       APPROD        Z     15
     O                       XXNAME              56
     O          E            DET2           1
     O                       EMEMP#        Z     18
     O                       EMDIV         Z     29
     O                       EMNAME              70
     O          E            DET3        1  2
     O                                           48 'PRODUCER COUNT:'
     O                       XXPCNT        Z     54
     O          E            DET4           1
     O                                           48 'AGENT COUNT:'
     O                       XXACNT        Z     54
     O*
     O*  SELECTION CRITERIA NOT FOUND
     O*
     O          E            NOREC
     O                                           19 '*** NO RECORDS MEET'
     O                                           42 'SELECTION CRITERIA ***'
     O*
