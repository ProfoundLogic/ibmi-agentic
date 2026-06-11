     H*
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H* BAR CODE TRACKING SYSTEM
     H*
     H/TITLE R7070R - REPORT LISTING BY DATE/USER
     H****************************************************************
     H** PROGRAM DESCRIPTION:                                       **
     H**    This program prints in order by date and user for the   **
     H**    Bar Code System.                                        **
     H**                                                            **
     H** CALLED FROM:                                               **
     H** CALLS TO:                                                  **
     H**    NONE                                                    **
     H** PROGRAMMER:  David R. Moncrief                             **
     H** CREATED:      2-27-90        PROJECT:  WCMS-Client 0003    **
     H****************************************************************
     H*REVISIONS:                                                     *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H*   11/18/98 C:112 Project: WC577                               *
     H*                 Correct Program to conform with PDS standards *
     H*                 Ensure dates are accurately handled (i.e.     *
     H*                 dates are 7.0 Numeric (used in date           *
     H*                 comparisons) or 8.0 Numeric (used for display)*
     H*****************************************************************
     H**                                                            **
     H** INDICATOR USAGE:                                           **
     H**   Indicator               Function Description             **
     H**                                                            **
     H**      08                     Demand Mode                    **
     H**      09                     Batch Mode                     **
     H**      50                     Read File Indicator            **
     H**      60                     File Chaining Indicator        **
     H**      OF                     OverFlow Indicator             **
     H**      LR                     Last Record - End Program      **
     H****************************************************************
     H**  FILE USAGE:                                               **
     H**    SMORL  (POR01): CO#/Q1PGM/XXPRT                         **
     H**    SMCOL  (PCA01): CO#                                     **
     H**    WMUPL  (PCR01): Q1USER                                  **
     H**    BATRL1 (PAR01): DATE/USER                               **
     H**    QSYSPRT:        Printer  Only                           **
     H****************************************************************
     H*
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FBATRL1    IF   E           K DISK
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)
     F*
     D*
     D*** BEGIN CHANGE *** 11/18/98 ***********************************
     D                 DS
     D  DSDTTM                 1     14  0
     D*************************************** 1  120DSDTTM
     D  DSTIME                 1      6  0
     D*************************************** 7  120DSDATE
     D  DSDATE                 7     14  0
     D*** END CHANGE ***** 11/18/98 ***********************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     I*
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    UPCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   DEFINE WORK FIELDS
     C*
     C*          *LIKE     DEFN TRTRDT    XXDTE
     C     *LIKE         DEFINE    TRTRTM        XXTIME
     C     *LIKE         DEFINE    TRUSER        XXUSER
     C     *LIKE         DEFINE    TRKEY         XXKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*** BEGIN CHANGE *** 11/18/98 ***********************************
     C******************** PARM           QQFDT   6
     C******************** PARM           QQTDT   6
     C                   PARM                    QQFDT             8
     C                   PARM                    QQTDT             8
     C*** END CHANGE ***** 11/18/98 ***********************************
     C                   PARM                    QQUSER           10
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
     C*  MOVE ROUTING SELECTIONS
     C*
     C     QQUSER        IFNE      *BLANKS
     C                   MOVE      QQUSER        RSUSER           10
     C                   ELSE
     C                   MOVE      'ALL'         RSUSER
     C                   END
     C*
     C                   MOVE      QQFDT         XXFDT             8 0
     C                   MOVE      QQTDT         XXTDT             8 0
     C                   MOVE      QQMODE        XXMODE            1
     C*
     C                   TIME                    DSDTTM
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C     XXMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C                   Z-ADD     0             COUNT             3 0          COUNT FOR # REC
     C*
     C*   CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 **************************************
     C**********         MOVE      '0'           XXPRT
     C**********     ORKEY         CHAIN     POR01                              60
     C**********     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
     C**********         EXCEPT    ROUTE
     C**********         END
      ***** End Del ***** 08/26/14 **************************************
     C*
     C*   START THE BAR CODE TRANSACTION FILE
     C*
     C                   READ      PTR01                                  50
     C*
     C*   GET COMPANY NAME
     C*
     C                   MOVE      *BLANKS       XXCONM           40
     C     COKEY         CHAIN     PCO01                              60
     C  N60              MOVEL     CONAME        XXCONM
     C*
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   MOVE      '1'           *INLR
     C                   GOTO      TERM
     C                   END
     C*
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C*
     C     *IN50         DOWEQ     '0'
     C*
     C*   MOVE FILE FIELDS TO WORK FIELDS
     C*
     C                   Z-ADD     TRTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDTE             8 0
     C                   Z-ADD     TRTRTM        XXTIME
     C                   MOVE      *BLANKS       XXUSER
     C                   MOVEL     TRUSER        XXUSER
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     TRKEY         XXKEY
     C*
     C*   CHECK TRANSACTION TYPE FOR I,O, OR D
     C*
     C                   MOVE      *BLANKS       XXTYPE            6
     C*
     C     TRTRTP        IFEQ      'I'
     C                   MOVE      'IN'          XXTYPE
     C                   END
     C     TRTRTP        IFEQ      'O'
     C                   MOVE      'OUT'         XXTYPE
     C                   END
     C     TRTRTP        IFEQ      'D'
     C                   MOVE      'DELETE'      XXTYPE
     C                   END
     C*
     C*   PRINT THE DETAIL LINE
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DETAIL
     C*
     C*   ADD THE NUMBER OF RECORDS PRINTED
     C*
     C                   ADD       1             COUNT
     C*
     C* READ NEXT RECORD
     C                   READ      PTR01                                  50
     C                   END
     C*
     C*   PRINT THE TOTAL NUMBER OF RECORDS PROCESSED
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEAD1
     C                   MOVE      '0'           *INOF
     C                   END
     C                   Z-ADD     COUNT         XCOUNT            3 0
     C                   EXCEPT    DET1
     C*
     C*
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     O*****************************************************************
     O*   ROUTING INFORMATION                                         *
     O*****************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 **************************************
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
      ***** End Del ***** 08/26/14 **************************************
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
      ***** Beg Del ***** 08/26/14 **************************************
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
      ***** End Del ***** 08/26/14 **************************************
     O*
     O          E            ROUTE          2
     O                                           11 'USER:'
     O                       RSUSER              26
     O          E            ROUTE          2
     O                                           17 'DATE RANGE:'
     O                       XXFDT         Y     26
     O                                           28 '-'
     O                       XXTDT         Y     37
     O          E            ROUTE            60
     O                                           20 '(C) COPYRIGHT PROGRE'
     O                                           41 'SSIVE DATA SOLUTIONS,'
     O                                           51 'INC. 1990'
     O*****************************************************************
     O*      STANDARD HEADING                                         *
     O*****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              62
     O                       DSDATE        Y     84
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              72
     O                                           81 'PAGE:'
     O                       PAGE          Z     84
     O          E            HEAD           2
     O                       XXFDT         Y     40
     O                                           42 '-'
     O                       XXTDT         Y     53
     O*
     O*****************************************************************
     O*      REPORT HEADING                                           *
     O*****************************************************************
     O          E            HEAD1       1  1
     O                                           57 'KEY'
     O                                           79 'TRANSACTION'
     O          E            HEAD1          2
     O                                           10 'DATE'
     O                                           21 'TIME'
     O                                           33 'USER'
     O                                           61 'INFORMATION'
     O                                           75 'TYPE'
     O*
     O          E            DETAIL         1
     O                       XXDTE         Y     12
     O                       XXTIME              23 '  :  :  '
     O                       XXUSER              39
     O                       XXKEY               65
     O                       XXTYPE              76
     O*
     O          E            DET1        3  0
     O                                           19 'TOTAL NUMBER OF'
     O                                           38 'RECORDS PROCESSED:'
     O                       XCOUNT        Z     43
     O*****************************************************************
     O*   ERROR HEADING
     O*****************************************************************
     O          E            ERRMSG         1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              62
     O                       DSDATE        Y     84
     O          E            ERRMSG         1
     O                       Q1PGM               10
     O                       ORTITL              72
     O                                           81 'PAGE:'
     O                       PAGE          Z     84
     O          E            ERRMSG         3
     O                       XXFDT         Y     40
     O                                           42 '-'
     O                       XXTDT         Y     53
     O          E            ERRMSG         1
     O                                           57 'KEY'
     O                                           79 'TRANSACTION'
     O          E            ERRMSG         2
     O                                           10 'DATE'
     O                                           21 'TIME'
     O                                           33 'USER'
     O                                           61 'INFORMATION'
     O                                           75 'TYPE'
     O          E            ERRMSG      2  1
     O                                           42 'NO RECORDS MEET'
     O                                           60 'SELECTED CRITERIA'
