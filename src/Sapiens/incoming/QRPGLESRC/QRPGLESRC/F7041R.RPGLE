     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE F7041R - SERVICE CODE FILE LISTING
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
     H*  07/16/90 DVM   - ADDED EFFECTIVE DATE FOR SERVICE CODE       *
     H*                   TO THE REPORT.                              *
     H*  04/21/92 KRR   - CHANGED THE SVCD(4N) TO CMSV(11A)           *
     H*  06/15/92 KRR   - FIX TO USE EFFECTIVE DATE IF ENTERED        *
     H*  08/12/92 KRR   - ADDED TYPE OF SERVICE TO LISTING            *
     H*  06/30/93 C:20  - FIX COPYRIGHT DATES                         *
     H*                                                               *
     H*  03/25/97 C:43   REQ/PROJ: SUPPORT MC068, SEQ=7031            *
     H*                  1. ADD RECORD TO COMPILE TIME TABLE=TYPE     *
     H*                  2. DEFENSE COMPILE TIME TABLE INDEX VALUE    *
     H*****************************************************************
     F*
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FFMSVL1    IF   E           K DISK    USROPN
     F*
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
     F*

     D SD              S             16    DIM(6) CTDATA PERRCD(1)              SERV TYPE DESCRIPTIO
     D QCA             S             55    DIM(2) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*** QCMDEXC CMD STRING MAPPED
     D                 DS
     D  @@CMD                  1     55
     D  @@ST                  40     41
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE SERVICE CODE FILE
     C*
     C     SVKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     SVKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCMSV           11
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    SVDT          XFEFDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFROM           11
     C                   PARM                    QQTO             11
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQST              2
     C                   PARM                    QQEFDT            8
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
     C**
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFROM        XXFROM           11
     C                   MOVE      QQTO          XXTO             11
     C                   MOVE      QQST          XXST              2
     C     QQEFDT        IFNE      *BLANKS
     C                   MOVE      QQEFDT        X1EFDT            8 0
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         XXEFDT            6 0
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XFEFDT
     C*
     C                   ELSE
     C                   Z-ADD     0             XFEFDT
     C                   END
     C                   Z-ADD     0             XXRCDS            7 0
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C                   TIME                    DSDTTM
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
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
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   DETERMINE REPORT SELECTIONS
     C*
     C     XXFROM        IFEQ      *BLANK
     C                   MOVE      '1'           *IN81
     C                   MOVEL     '*ALL'        RSFCDE           11
     C                   ELSE
     C                   MOVEL     XXFROM        RSFCDE           11
     C                   END
     C     XXTO          IFEQ      *BLANK
     C                   MOVE      '1'           *IN81
     C                   MOVEL     '*ALL'        RSTCDE           11
     C                   ELSE
     C                   MOVEL     XXTO          RSTCDE           11
     C                   END
     C     QQEFDT        IFEQ      *BLANK
     C                   MOVE      '1'           *IN80
     C                   ELSE
     C*
     C                   Z-ADD     X1EFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         RSEFDT            6 0
     C*
     C                   END
     C*
     C*   DETERMINE LOOP CONDITIONS
     C*
     C     XXFROM        IFEQ      *BLANKS
     C     XXTO          ANDEQ     *BLANKS
     C                   MOVE      *HIVAL        XXTO
     C                   END
     C*
     C*** CLOSE FILE IF OPENED
     C     STOPN         IFEQ      '1'
     C                   CLOSE     FMSVL1
     C                   MOVE      ' '           STOPN             1
     C                   END
     C*
     C*** DELETE OVERRIDE
     C     STOVR         IFEQ      '1'
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCMDEXC'                              60
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      ' '           STOVR             1
     C                   END
     C*
     C*** OVERRIDE FMPRL FILE TO SELECTED STATE CODE
     C     STOVR         IFEQ      ' '
     C                   Z-ADD     55            LENGTH
     C                   MOVEL     QCA(2)        @@CMD
     C                   MOVEL     XXST          @@ST
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '1'           STOVR             1
     C                   END
     C*
     C*** OPEN FILE FOR PROCESSING
     C     STOPN         IFNE      '1'                                            NOT CURRENTLY OPEN
     C                   OPEN      FMSVL1
     C                   MOVE      '1'           STOPN
     C                   END
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********   N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C*
     C*   PRINT HEADINGS
     C*
     C                   EXCEPT    HEAD
     C*
     C*
     C*  GET STARTING RECORD AND START LOOP
     C*
     C                   MOVEL     XXFROM        XXCMSV
     C     SVKEY1        SETLL     PSV01
     C     SVKEY         READE     PSV01                                  60
     C*
     C     *IN60         DOWEQ     '0'
     C     SVCMSV        ANDLE     XXTO
     C*
     C*   IF EFFECTIVE DATE IS SELECTED, ONLY PRINT RECORDS FOR THAT DATE
     C*
     C     XFEFDT        IFNE      *ZERO
     C     SVEFDT        ANDEQ     XFEFDT
     C     XFEFDT        OREQ      *ZERO
     C*
     C*   GET SERVICE TYPE DESCRIPTION
     C*
     C                   MOVE      SVTYPE        X                 1 0
     C     X             IFGT      0
     C     X             ANDLE     6
     C                   MOVEL     SD(X)         XXSVDS           13
     C                   ELSE
     C                   MOVEL(P)  'NO TABLE'    XXSVDS
     C                   ENDIF
     C*
     C                   Z-ADD     SVEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C*   PRINT THE DETAIL
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    DET
     C*
     C                   ADD       1             XXRCDS
     C                   END
     C*
     C     SVKEY         READE     PSV01                                  60
     C                   END
     C*
     C*   END PROGRAM
     C*
     C                   MOVE      '1'           *INLR
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C                   EXCEPT    TOT
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
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTION:'
     O          E            ROUTE          2
     O                                           24 'STATE:'
     O                       XXST                28
     O          E            ROUTE          2
     O                                           24 'FROM CODE:'
     O                       RSFCDE              37
     O          E            ROUTE          2
     O                                           24 'TO CODE:'
     O                       RSTCDE              37
     O          E            ROUTE          2
     O                                           24 'EFFECTIVE DATE:'
     O               80                          30 '*ALL'
     O                       RSEFDT              34 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               63 'INC. 1990, 1991, 1992'
     O****                               69 ', 1993'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              85
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                           31 'STATE:'
     O                       XXST                34
     O                                           51 'RANGE:'
     O                       XXFROM              64
     O               81                          57 '*ALL'
     O                                           69 'TO:'
     O                       XXTO                82
     O               81                          76 '*ALL'
     O                                          105 'EFFECTIVE DATE:'
     O               80                         110 '*ALL'
     O                       RSEFDT             114 '  /  /  '
     O          E            HEAD           1
     O                                           19 'EFFECTIVE'
     O                                           85 'TYPE OF'
     O          E            HEAD           2
     O                                           06 'CODE'
     O                                           17 'DATE'
     O                                           42 'D E S C R I P T I O N'
     O                                           76 'SERVICE TYPE'
     O                                           84 'CHARGE'
     O                                           95 'ALLOWABLE'
     O                                          108 'INPATIENT %'
     O                                          122 'OUTPATIENT %'
     O                                          131 'STATUS'
     O          E            DET            2
     O                       SVCMSV              11
     O                       XXEFDT              19 '  /  /  '
     O                       SVDESC              61
     O                       SVTYPE              64
     O                                           65 '-'
     O                       XXSVDS              78
     O                       SVTSRV              83
     O                       SVALOW        J     96
     O                       SVINPC        J    106
     O                       SVOTPC        J    119
     O                       SVSTAT             129
     O          E            TOT         2  1
     O                                           69 '# OF RECORDS LISTED:'
     O                       XXRCDS        Z     78
**  TABLE SD
Hosp Room Charge
Misc Charge
Sur/Ane Charge
Lab/X-Ray
Other
Hospital Outpatient Services
**  QCMDEXC TABLE
DLTOVR FILE(FMSVL1)
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
