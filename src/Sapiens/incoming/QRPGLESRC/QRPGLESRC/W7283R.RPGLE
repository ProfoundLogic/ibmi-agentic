     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7283R - PREMIUM DUE (SELF-REPORTING) - AGENT SEQUENCE
     H*================================================================
     H* REVISIONS                                                     *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 05/26/00  C:35   PROJECT: WC987                               *
     H*                  MOVE COMPANY NAME PROCESSING TO AFTER LOAD   *
     H*                  OF ALTERNATE NAME.                           *
     H*                                                               *
     H* 04/10/00  C:118  PROJECT: WC987                               *
     H*                  Load alternate company name from fund master *
     H*                                                               *
     H*================================================================
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     FWASRL2    IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWXPDL1    IF   E           K DISK
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    PDDT          XXCDAT
     C     *LIKE         DEFINE    SRRPMY        XXRPYM
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRPMY            6
     C                   PARM                    QQQUE            10
     C                   PARM                    QQFAGT            5            JJJ 3-1-89
     C                   PARM                    QQTAGT            5            |
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQRPMY        XXRPMY            6 0
     C                   MOVE      QQRPMY        XXRMY1            6 0
     C*
     C                   Z-ADD     XXRMY1        SRDTI
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         XXRPYM
     C*
     C                   MOVE      QQFAGT        XXFAGT            5 0          JJJ 3-1-89
     C                   MOVE      QQTAGT        XXTAGT            5 0          |
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQQUE         IFNE      ' '
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08
     C                   END
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    PDEMP#
     C                   KFLD                    PDDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*
     C     SRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    XXRPYM
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE WORK FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE BILLING SCHEDULE FILE
     C*
     C     ABKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C                   TIME                    DSDTTM
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
      **** BEGIN DEL **** 05/26/00 *********************************************
     C**********
     C*** REMOVE LEADING BLANKS FROM COMPANY NAME
     C**********
     C**********           Z-ADD1         Y       20
     C********** *BLANK    LOKUPCONM,Y               60
     C**********           MOVEACONM,Y    XXCONM 40
      **** END   DEL **** 05/26/00 *********************************************
     C*
     C*   CHAIN TO THE FUND FILE
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
      **** BEGIN ADD **** 05/26/00 *********************************************
     C*
     C*   REMOVE LEADING BLANKS FROM COMPANY NAME
     C*
     C                   Z-ADD     1             Y                 2 0
     C     *BLANK        LOOKUP    CONM(Y)                            60
     C                   MOVEA     CONM(Y)       XXCONM           40
      **** END   ADD **** 05/26/00 *********************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C*  INITIALIZE EMPLOYER TOTAL
     C*
     C                   EXCEPT    HEAD
     C                   Z-ADD     0             XXECNT            5 0
     C*
     C*   START READING WORK FILE BY AGENT#
     C*
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*   GET THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVEL     ELBSCD        XXKEY
     C     ABKEY         CHAIN     PBS01                              60
     C     BSSEFL        IFEQ      'S'
     C*
     C*   GET THE EMPLOYER PAYROLL FILE
     C*
     C     SRKEY         CHAIN     PSR01                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     0             SRRPMY
     C*
     C*   FIND THE LATEST DATE
     C*
     C     SRKEY         SETGT     PSR01
     C                   READP     PSR01                                  60
     C*
     C     *IN60         IFEQ      '0'
     C     XXCO#         ANDEQ     SRCO#
     C     XXFND         ANDEQ     SRFND
     C     XXFYR         ANDEQ     SRFYR
     C     ELEMP#        ANDEQ     SREMP#
     C     ELDIV         ANDEQ     SRDIV
     C     XXRPYM        ANDGE     SRRPMY
     C*
     C     *IN60         OREQ      '0'
     C     XXCO#         ANDNE     SRCO#
     C*
     C     *IN60         OREQ      '0'
     C     XXFND         ANDNE     SRFND
     C*
     C     *IN60         OREQ      '0'
     C     XXFYR         ANDNE     SRFYR
     C*
     C     *IN60         OREQ      '0'
     C     ELEMP#        ANDNE     SREMP#
     C*
     C     *IN60         OREQ      '0'
     C     ELDIV         ANDNE     SRDIV
     C*
     C     *IN60         OREQ      '1'
     C*
     C     XXCO#         IFNE      SRCO#
     C     XXFND         ORNE      SRFND
     C     XXFYR         ORNE      SRFYR
     C     ELEMP#        ORNE      SREMP#
     C     ELDIV         ORNE      SRDIV
     C                   Z-ADD     0             SRRPMY
     C                   END
     C*
     C*   GET THE NAME OF THE EMPLOYER AND PRINT THE REPORT
     C*
     C                   ADD       1             XXECNT
     C     EMKEY         CHAIN     PEM01                              60
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   Z-ADD     SRRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         XXMMYY            6 0
     C*
     C*   CHECK THE EFFCTIVE AND CANCELLATION DATES
     C*
     C                   Z-ADD     1             Y
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C     Y             DOWNE     0
     C     ELPTDT        COMP      ED(Y)                              60  60
     C  N60              SUB       1             Y
     C  N60              END
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCDAT
     C*
     C     Y             IFNE      *ZERO
     C     CD(Y)         IFGE      XXCDAT
     C     CD(Y)         OREQ      0
     C                   MOVE      '0'           *IN21
     C                   ELSE
     C                   MOVE      '1'           *IN21
     C                   END
     C                   ELSE
     C                   SETON                                        21
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C*
     C                   END
     C                   END
     C                   END
     C                   END
     C     PDKEY         READE     PPD01                                  60
     C                   END
     C*
     C                   EXCEPT    TOTAL
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
     C***********************************************************
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
     O*  MLS 1/18/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Chg ***** 08/05/15 ***********************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O*  MLS 1/18/88 ADD INDICTORS AND DISTRIBUTION INFORMATION
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Chg ***** 08/05/15 ***********************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** End Del ***** 08/26/14 ************************************
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
     O*  MLS 1/18/88 ADD INDICATORS AND OUT QUEUE
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE          2 35
     O                                           49 'SELECTION CRITERIA:'
     O          E            ROUTE          2
     O                                           21 'YEAR  . . . . . . . :'
     O                       XXFYR         X     25
     O          E            ROUTE          2
     O                                           21 'REPORTING MONTH/YEAR:'
     O                       XXRPMY              29 '0  /    '
     O          E            ROUTE          2
     O                                           21 'SORT  . . . . . . . :'
     O                                           23 'A'
     O          E            ROUTE          2
     O                                           21 'AGENT RANGE . . . . :'
     O                       XXFAGT        X     27
     O                                           29 '-'
     O                       XXTAGT        X     35
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                47 'VE DATA SOLUTIONS. INC.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              62
     O                       DSDATE        Y     84
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              72
     O                                           75 'PAGE:'
     O                       PAGE          Z     84
     O          E            HEAD           2
     O                                           05 'YEAR:'
     O                       XXFYR         Z     09
     O                                           29 'AGENT RANGE:'
     O                       XXFAGT        Z     35
     O                                           37 '-'
     O                       XXTAGT        Z     42
     O                                           75 'RUN DATE:'
     O                       XXRPMY              84 '0  /    '
     O          E            HEAD           2
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              71
     O          E            HEAD           2
     O                                           23 'EMPLOYER   DIV    NAME'
     O                                           64 'AGENT#'
     O                                           84 'DT OF LAST REPORT'
     O          E            DET            1
     O                       EMEMP#        Z     09
     O               21                          11 'CX'
     O                       EMDIV         Z     16
     O                       EMNAME              59
     O                       PDAGT#        Z     64
     O                       XXMMYY              78 '0  /    '
     O          E            TOTAL       1  1
     O                                           12 'EMPLR TOTAL:'
     O                       XXECNT        Z     18
