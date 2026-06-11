     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H****************************************************************
     H/TITLE SC751R - DARTS Document Printout by File - Prt Pgm
     H****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**       This program prints a report of all of the documents  **
     H**       that were printed for the key that was entered.       **
     H**       It also calls SC752C which prints the actual          **
     H**       documents that are attached to the DARTS records      **
     H**       for the particular key.                               **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**       SC751C                                                **
     H**   CALLS TO:                                                 **
     H**       SC752C                                               **
     H**   PROGRAMMER: W. Denehy                                     **
     H**   CREATED:    03/05/93     PROJECT: ANA                     **
     H****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**                                                             **
     H*       OF      OVERFLOW FOR PRINTING FOR REPORT                *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       20      DOCUMENT DOESN'T EXIST  MESSAGE                 *
     H*       21      NOT PRINTED (AUTHORITY) MESSAGE                 *
     H*       22      OPTICAL RETREIVE ERROR                          *
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H** FILES:                                                      **
     H**    SDCMLI03 (PCM01) :                                       **
     H**    SMORL    (POR01) : USER                                  **
     H**    SMUPL    (PUP01) : USER                                  **
     H*****************************************************************
     H*  REVISIONS:                                                  **
     H*                                                              **
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                **
     H*                -Implement Routing Page Changes.              **
     H*                                                              **
     H*  10/17/11 C:277 Project: SY195                               **
     H*                 QDLS migration to IFS                        **
     H*                 Add two parameters with command              **
     H*                                                              **
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  02/04/94 C:09 PROJECT 03AYJ                                 **
     H*                ADD ADDITIONAL PARAMETERS NEEDED FOR CALL TO  **
     H*                SC752C.  THIS PROGRAM WILL NOW NEED TO PULL   **
     H*                DOCUMENTS FROM OPTICAL BEFORE PRINTING, IF    **
     H*                NECESSARY.                                    **
     H*                                                              **
     H*****************************************************************
     FSDCMLL    IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     FQSYSPRT1  O    F  132        PRINTER OFLIND(*INOF)

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE REMARK FILE.
     C*
     C     CMKY          KLIST
     C                   KFLD                    QQKEY
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQUDTA           16
     C                   PARM                    QQOUTQ           10
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
     C                   TIME                    DSDTTM
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   Get the Distribution Information
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C*
     C*   Print the distribution information if a valid record is foundD.
     C*
      ***** BEG ADD ***** 01/28/10 ***********************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      ***** END ADD ***** 01/28/10 ***********************************
     C*
     C     *IN60         IFEQ      '0'
     C     ORSTAT        ANDEQ     'A'
     C     Q1USER        CHAIN     PUP01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C*********          EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C                   EXCEPT    HEAD
     C*
     C*  Read SDCMP (By Remark Date)
     C*
     C                   MOVE      '0'           XXPFLG            1
     C                   Z-ADD     *ZEROS        XXCNT             5 0
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         DOWEQ     '0'
     C     CMDCID        IFNE      *BLANKS
     C*
     C*  Call program to print the actual document
     C*
     C                   MOVE      *BLANKS       QQRTCD
     C                   MOVE      CMSEQ#        QQSEQ#
     C*
     C                   CALL      'SC752C'
     C                   PARM                    QQUDTA
     C                   PARM                    CMFOLD
     C                   PARM                    CMDCID
     C                   PARM                    QQOUTQ
     C                   PARM                    CMKEY
     C                   PARM                    QQSEQ#            5
     C                   PARM                    CMFL05
     C                   PARM                    QQRTCD            1
     C*** Begin Add *** 10/17/11 **************************************
     C                   PARM                    CMUNCN
     C                   PARM                    CMIFSP
     C*** End   Add *** 10/17/11 **************************************
     C*
     C*  Print document name to output file only if it exists
     C*
     C     QQRTCD        IFNE      '3'
     C     QQRTCD        ANDNE     '4'
     C*
     C                   MOVE      '1'           XXPFLG
     C                   ADD       1             XXCNT
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C     QQRTCD        IFEQ      '1'
     C                   MOVE      '1'           *IN20
     C                   ELSE
     C                   MOVE      '0'           *IN20
     C                   END
     C*
     C     QQRTCD        IFEQ      '2'
     C                   MOVE      '1'           *IN21
     C                   ELSE
     C                   MOVE      '0'           *IN21
     C                   END
     C*
     C     QQRTCD        IFEQ      '5'
     C                   MOVE      '1'           *IN22
     C                   ELSE
     C                   MOVE      '0'           *IN22
     C                   END
     C*
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCDAT            6 0
     C*
     C                   MOVEL     CMFOLD        XXFOLD           63
     C*
     C                   EXCEPT    DET
     C*
     C                   END
     C                   END
     C*
     C     CMKY          READE     PCM01                                  60
     C                   END
     C*
     C*  Print Total Count
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    TOT
     C*
     C*   If no records selected, print error message and end program
     C*
     C     XXPFLG        IFEQ      '0'
     C                   EXCEPT    ERRMSG
     C                   END
     C*
     C*** END PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKy POR01;
       If %found and orStat = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     O*************************************************************************
     OQSYSPRT1  E            ROUTE          1 06
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
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********     08      OROUTQ              30
     O**********        09   QQOUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O**
     O**   SELECTION CRITERIA
     O**
     O          E            ROUTE          1
     O                                           20 'KEY          :'
     O          E            ROUTE          1
     O                       QQKEY               80
     O          E            ROUTE          1
     O                                           20 'OUT QUEUE    :'
     O          E            ROUTE          1
     O                       QQOUTQ              80
     O*
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               13 '(C) COPYRIGHT'
     O****                               30 'PROGRESSIVE DATA'
     O****                               51 'SOLUTIONS, INC. 1997'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     OQSYSPRT1  E            HEAD           1 06
     O                       DSDATE        Y      8
     O                                           68 'DATABASE AND REMARKS'
     O                                           84 'TRACKING SYSTEM'
     O                       DSTIME             132 '  :  :  '
     O          E            HEAD           2
     O                                            6 'SC751R'
     O                                           66 'DARTS DOCUMENT '
     O                                           82 'PRINTOUT BY FILE'
     O                                          126 'PAGE:'
     O                       PAGE               132
     O          E            HEAD           2
     O                                            4 'KEY:'
     O                       QQKEY               45
     O                                           63 'OUTQ:'
     O                       QQOUTQ              74
     O          E            HEAD           1
     O                                           31 'REMARK'
     O                                           40 'REMARK'
     O          E            HEAD           2
     O                                           30 'DATE'
     O                                           39 'CODE'
     O                                           51 'DOCUMENT'
     O                                           64 'FOLDER'
     O          E            DET            1
     O               20                          22 'DOCUMENT DOESN''T EXIST'
     O               21                          22 'NOT PRINTED(AUTHORITY)'
     O               22                          22 'OPTICAL RETRIEVE ERROR'
     O                       XXCDAT              32 '  /  /  '
     O                       CMRMCD              40
     O                       CMDCID              55
     O                       XXFOLD             121
     O          E            TOT         1  1
     O                                           30 'TOTAL:'
     O                       XXCNT         Z     39
     O*
     O**** ERROR MESSAGE, IF NO RECORDS TO BE PROCESSED *************
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
