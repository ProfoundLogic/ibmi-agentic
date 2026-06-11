     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE CS902R - OPTIMIZE CLAIM SCAN FILE
     H*****************************************************************
     H*    THIS OPTION WILL DELETE ALL RECORDS FROM THE CLAIM SCAN
     H*    FILE IF THE ACCIDENT DATE IS LESS THAN OR EQUAL TO THE
     H*    ENTERED DATE.  A COUNT OF DELETED RECORDS IS PRINTED ALONG
     H*    WITH THE USER NAME, DATE, & TIME.  THE FILE IS THEN
     H*    REORGANIZED IN THE OVERRIDE CL(CS902C).
     H*****************************************************************
     H*    INDICATOR USAGE
     H*    71       USED AS AN EXCEPTION/ERROR INDICATOR
     H*****************************************************************
     H* REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*             189  Retrieve brand Name                          *
     H*                                                               *
     H*  02/10/98  C:62  PROJECT#: MC145
     H*                  ** USED CONVERSION FROM MDYY YO CYMD
     H*
     H*****************************************************************
     FCMCSL1    UF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    CSACDT        XXDATE
     C     *LIKE         DEFINE    CSACDT        XXACDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQDATE            8            MMDDYY
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
     C                   MOVE      QQDATE        XXDTE             8 0
     C*
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     0             XXCNT             9 0
     C                   Z-ADD     0             XXERR             9 0
     C                   Z-ADD     0             XXERRD            9 0
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END Add ***** 01/28/10 **************************************
     C*
     C*   PRINT PAGE HEADING
     C*
      ***** Beg Del ***** 08/26/14 ****************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ****************************************
     C                   EXCEPT    HEAD
     C*
     C*   START READING THE CLAIM SCAN FILE
     C*
     C                   READ      PCS01                                7150
     C     *IN50         DOWEQ     '0'
     C*
     C*   IF FILE EXCEPTION/ERROR OCURRED, ADD 1 TO ERROR COUNT
     C*
     C   71              ADD       1             XXERR
     C*
     C*   FLIP ACCIDENT DATE (FROM FILE) FROM MMDDYY TO YYMMDD
     C*
     C*** BEGIN CHG *** 02/10/98 **************************************
     C**********           Z-ADD0         XXACDT
     C*
     C                   Z-ADD     CSACDT        XXACDT
     C*
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     XXACDT
     C*** END   CHG *** 02/10/98 **************************************
     C*
     C*   IF ACC DATE IS < OR = TO DATE ENTERED, DELETE RECORD.
     C*
     C     XXACDT        IFLE      XXDATE
     C     XXACDT        ANDNE     0
     C                   ADD       1             XXCNT
     C   71              ADD       1             XXERRD
     C                   DELETE    PCS01
     C                   END
     C*
     C                   READ      PCS01                                7150
     C                   END
     C*
     C                   EXCEPT    DET
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ********************************************
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
     O**********             Q1USER              27
      ***** End Del ***** 08/26/14 ********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       Q1USER              27
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                                           80 'OPTIMIZE CLAIM SCAN FILE'
      ***** Beg Del ***** 08/26/14 ********************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             Q1PGM               30
      ***** End Del ***** 08/26/14 ********************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O***                                20 '(C) COPYRIGHT PROGRE'
     O***                                41 'SSIVE DATA SOLUTIONS,'
     O***                                51 'INC. 1990'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      *
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                                           80 'OPTIMIZE CLAIM SCAN FILE'
     O                       DSDATE        Y    132
     O          E            HEAD           1
     O                       Q1PGM               10
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            DET            1
     O                                           29 'OPTIMIZE CLAIM SCAN'
     O                                           43 'SUBMITTED BY:'
     O                       Q1USER              54
     O                                           64 'DATE:'
     O                       DSDATE        Y     73
     O                                           82 'TIME:'
     O                       DSTIME              91
     O          E            DET            1
     O                                           33 'TOTAL NUMBER OF RECORDS'
     O                                           54 'DELETED WHICH HAD AN'
     O                                           75 'ACCIDENT DATE BEFORE'
     O                       XXDTE               86 '  /  /    '
     O                                           87 ':'
     O                       XXCNT         J    100
     O          E            DET            1 18
     O                                           33 'TOTAL NUMBER OF RECORDS'
     O                                           49 'WHICH HAVE DATA'
     O                                           67 'EXCEPTION/ERRORS:'
     O                       XXERR         J     79
     O                                           91 'DELETED:'
     O                       XXERRD        J    103
