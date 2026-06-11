     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE SC740R - PRINT SINGLE DARTS REMARK
      *****************************************************************
      *  Program Description:                                         *
      *                                                               *
      *    This program will print a single DARTS remark that was     *
      *    requested from either program SC010R or SC015R.            *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    SC740C                                                     *
      *                                                               *
      *  CALLS TO:                                                    *
      *    none                                                       *
      *                                                               *
      *  Created:  09/25/95   C:30   Project:  DR015                  *
      *                                                               *
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
      * 01/28/10  C:XXX  Project: SY215                               *
      *           C:189  Retrieve brand Name                          *
      **                                                             **
      *                                                               *
      *                                                               *
      *****************************************************************
      * INDICATOR USAGE                                               *
      *                                                               *
      *   08 - Batch Mode  'B'                                        *
      *   09 - Demand Mode 'D'                                        *
      *   20 - *OFF means XXNAME is blanks                            *
      *   21 - *ON  means Diary Remark                                *
      *   50 - DARTS Remark file access                               *
      *   60 - Routing file access & User Parameter file access       *
      *   61 - Additional Remark file access                          *
      *                                                               *
      *****************************************************************
     F*
     F* DARTS Remark file              KEY/SEQ#
     FSDCMLA    IF   E           K DISK
     F*
     F* DARTS Additional Remark file   KEY/SEQ#/ASEQ#
     FSMARL     IF   E           K DISK
     F*
     F* Output Routing file            PGM/FILE#
     FSMORL     IF   E           K DISK
     F*
     F* User Parameter file            USRPRF
     FSMUPL     IF   E           K DISK
     F*
     F* Remark Report file
     FQSYSPRT   O    F   95        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     F*

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*  Remarks file
     C*
     C     CMKY          KLIST
     C                   KFLD                    HLKEY
     C                   KFLD                    HLSEQ#
     C*
     C*  Additional Remarks file
     C*
     C     ARKY          KLIST
     C                   KFLD                    HLKEY
     C                   KFLD                    HLSEQ#
     C                   KFLD                    HLASQ#
     C*
     C     ARKY2         KLIST
     C                   KFLD                    HLKEY
     C                   KFLD                    HLSEQ#
     C*
     C*   Key for the Override file
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*===================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQKEY            40
     C                   PARM                    QQSEQ#            5
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
     C*
     C*
     C     *LIKE         DEFINE    ARKEY         HLKEY
     C     *LIKE         DEFINE    ARSEQ#        HLSEQ#
     C     *LIKE         DEFINE    ARASQ#        HLASQ#
     C     *LIKE         DEFINE    ARCM75        XXCM75
     C     *LIKE         DEFINE    CMIUSR        XXUSER
     C     *LIKE         DEFINE    CMIUSR        RSUSER
     C     *LIKE         DEFINE    CMKEY         SVKEY
     C*
     C                   MOVE      *BLANK        XXKEY            79
     C                   MOVE      *BLANK        XXNAME           79
     C*
     C                   MOVEL     Q1USER        XXUSER
     C*
     C* Establish Routing Sheet Information
     C*
     C*
     C                   MOVE      *BLANK        RSKEY            80
     C                   MOVE      *BLANK        RSKYDF           80
     C                   MOVE      *BLANK        RSUSER
     C                   MOVEL     Q1USER        RSUSER
     C                   MOVEL     QQKEY         RSKEY
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C                   TIME                    DSDTTM
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*   Access Remark
     C*
     C                   MOVE      *BLANK        HLKEY
     C                   MOVEL     QQKEY         HLKEY
     C                   MOVE      QQSEQ#        HLSEQ#
     C*
     C     CMKY          CHAIN     PCM01                              50
     C*
     C*   Build Key Definition here
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      *BLANKS       X#NAME
     C                   MOVE      ' '           X#RTCD
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#NAME           79
     C                   PARM                    X#RTCD            1
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         XXKEY
     C     RSKEY         IFNE      '*ALL'
     C                   MOVEL     X#KEY         RSKEY
     C                   END
     C                   ELSE
     C                   MOVEL     CMKEY         XXKEY
     C                   END
     C*
     C                   MOVE      *BLANKS       SVKEY
     C                   MOVEL     CMKEY         SVKEY
     C*
     C                   MOVEL     X#NAME        XXNAME
     C     XXNAME        IFEQ      *BLANKS
     C                   MOVE      '0'           *IN20
     C                   ELSE
     C                   MOVE      '1'           *IN20
     C                   END
     C     RSKEY         IFNE      '*ALL'
     C                   MOVEL     X#NAME        RSKYDF
     C                   END
     C*
     C*   Determine if this remark is a Diary Remark
     C*
     C                   MOVE      *OFF          *IN21
     C     CMDIFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN21
     C*
     C                   Z-ADD     CMDCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDCDT            6 0
     C*
     C                   ENDIF
      ***** BEG ADD ***** 01/28/10 ***********************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      ***** END ADD ***** 01/28/10 ***********************************
     C*
     C*   Get & Print the Distribution Information, if a Valid Record is Found
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C     *IN60         IFEQ      '0'
     C     ORSTAT        ANDEQ     'A'
     C     Q1USER        CHAIN     PUP01                              60
      ***** Beg Del ***** 08/26/14 ************************************
     C********           EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   EXCEPT    HEAD
     C*
     C*** IF NO RECORDS SELECTED, PRINT ERROR MESSAGE AND END PROGRAM
     C     *IN50         IFEQ      '1'
     C                   EXCEPT    ERRMSG
     C                   GOTO      TERM
     C                   END
     C*
     C*** Build Key Definition here
     C*
     C                   MOVEL     CMPNAM        X#PNAM
     C                   MOVEL     CMKEY         X#KEY
     C                   MOVE      *BLANKS       X#NAME
     C                   MOVE      ' '           X#RTCD
     C                   CALL      'SRKCVT1'
     C                   PARM                    X#PNAM           10
     C                   PARM                    X#KEY            79
     C                   PARM                    X#NAME           79
     C                   PARM                    X#RTCD            1
     C*
     C                   MOVE      *BLANKS       XXKEY
     C*
     C     X#RTCD        IFNE      'N'
     C                   MOVEL     X#KEY         XXKEY
     C                   ELSE
     C                   MOVEL     CMKEY         XXKEY
     C                   END
     C*
     C                   MOVEL     X#NAME        XXNAME
     C     XXNAME        IFEQ      *BLANKS
     C                   MOVE      '0'           *IN20
     C                   ELSE
     C                   MOVE      '1'           *IN20
     C                   END
     C*
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCDAT            6 0
     C*
     C                   MOVE      *BLANKS       X1COMM
     C                   MOVE      *BLANKS       X2COMM
     C                   MOVE      *BLANKS       XXCM1
     C                   MOVE      *BLANKS       XXCM2
     C                   MOVE      *BLANKS       XXCM3
     C                   MOVE      *BLANKS       XXCM4
     C*
     C                   MOVEL     CMCOMM        X1COMM          150
     C                   MOVE      CMCOMM        X2COMM          125
     C*
     C                   MOVEL     X1COMM        XXCM1            75
     C                   MOVE      X1COMM        XXCM2            75
     C*
     C                   MOVEL     X2COMM        XXCM3            75
     C                   MOVE      X2COMM        XXCM4            50
     C*
     C     CMKEY         IFNE      SVKEY
     C     *INOF         ANDNE     '1'
     C                   EXCEPT    NKEY
     C                   END
     C*
     C                   MOVE      *BLANKS       SVKEY
     C                   MOVEL     CMKEY         SVKEY
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DET1
     C                   EXCEPT    DET2
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C     XXCM2         IFNE      *BLANKS
     C                   EXCEPT    DET3
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C     XXCM3         IFNE      *BLANKS
     C                   EXCEPT    DET4
     C                   END
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C     XXCM4         IFNE      *BLANKS
     C                   EXCEPT    DET5
     C                   END
     C*
     C* Get Additional Remarks and Print them
     C*
     C                   MOVE      '0'           XXONE
     C                   MOVE      *BLANKS       HLKEY
     C                   MOVE      *ZEROS        HLSEQ#
     C                   MOVE      *ZEROS        HLASQ#
     C                   MOVEL     CMKEY         HLKEY
     C                   MOVE      CMSEQ#        HLSEQ#
     C     ARKY          SETLL     PAR01
     C     ARKY2         READE     PAR01                                  61
     C     *IN61         DOWEQ     '0'
     C*
     C     XXONE         IFEQ      '0'
     C     ARCM75        ANDNE     *BLANKS
     C                   MOVE      '1'           XXONE             1
     C                   MOVEL     ARCM75        XXCM75
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DET6
     C                   ELSE                                                    IFEQXXONE
     C*
     C     ARCM75        IFNE      *BLANKS
     C                   MOVEL     ARCM75        XXCM75
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    DET1
     C                   EXCEPT    DET6
     C                   MOVE      '0'           *INOF
     C                   ELSE
     C                   EXCEPT    DET7
     C                   END                                                    IFEQ*INOF
     C*
     C                   END                                                    IFEQXXCM75
     C                   END                                                    IFEQXXONE
     C*
     C     ARKY2         READE     PAR01                                  61
     C                   END                                                    *IN61DOWEQ
     C*
     C*** END PROGRAM
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found and Orstat = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     O*************************************************************************
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
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O*
     O*    SELECTION CRITERIA
     O*
     O          E            ROUTE          1
     O                                           20 'KEY          :'
     O          E            ROUTE          1
     O                       RSKEY               80
     O          E   N20      ROUTE          1
     O          E    20      ROUTE          2
     O                       RSKYDF              80
     O*
     O          E            ROUTE          2
     O                                           20 'USER         :'
     O                       RSUSER              31
     O*
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                13 '(C) COPYRIGHT'
     O***                                30 'PROGRESSIVE DATA'
     O***                                51 'SOLUTIONS, INC. 1995'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
      ***** END CHG ****** 02/02/06 ***********************************
     OQSYSPRT   E            HEAD           1 06
     O                       DSDATE        Y      8
     O                                           43 'Database and Remarks'
     O                                           59 'Tracking System'
     O                       DSTIME              80 '  :  :  '
     O          E            HEAD           2
     O                                            6 'SC740R'
     O                                           48 'Print Single DARTS Remar'
     O                                           49 'k'
     O                                           75 'Page:'
     O                       PAGE                80
     O          E            HEAD           2
     O                                            5 'User:'
     O                       XXUSER              16
     O*
     O*  Heading for NON Diary Remark
     O*
     O          E   N21      HEAD           1
     O                                            4 'Key:'
     O                       XXKEY               79
     O          E   N20N21   HEAD           1
     O          E    20N21   HEAD           2
     O                       XXNAME              79
     O          E   N21      HEAD           1
     O                                            7 'Remarks'
     O                                           17 'Remarks'
     O                                           28 'Document'
     O          E   N21      HEAD           1
     O                                            6 'Date'
     O                                           16 'Type'
     O                                           24 'ID'
     O          E   N21      NKEY        1  1
     O                       XXKEY               79
     O          E   N20N21   NKEY           1
     O          E    20N21   NKEY           2
     O                       XXNAME              79
     O          E   N21      DET1        1  1
     O                       XXCDAT               8 '  /  /  '
     O                       CMRMCD              16
     O                       CMDCID              32
     O*
     O*  Heading for Diary Remark
     O*
     O          E    21      HEAD           1
     O                                           07 'Diary'
     O                                           18 'Diary'
     O                                           28 'Remark'
     O          E    21      HEAD           1
     O                                           06 'Date'
     O                                           20 'Completed'
     O                                           28 'Type'
     O                                           43 'Document ID'
     O                                           65 'Seq #'
     O          E    21      DET1        1  1
     O                       XXCDAT              08 '  /  /  '
     O                       XXDCDT              20 '  /  /  '
     O                       CMRMCD              28
     O                       CMDCID              43
     O                       CMSEQ#        Z     65
     O          E    21      DET1        1  1
     O                                            4 'Key:'
     O                       XXKEY               85
     O          E    21      DET1           1
     O                       XXNAME              79
     O*
     O*  Detail for both Diary and NON Diary Remark
     O*
     O          E            DET2           1
     O                       XXCM1               80
     O          E            DET3           1
     O                       XXCM2               80
     O          E            DET4           1
     O                       XXCM3               80
     O          E            DET5           1
     O                       XXCM4               55
     O          E            DET6           1
     O                       XXCM75              80
     O          E            DET7           1
     O                       XXCM75              80
     O*
     O**** ERROR MESSAGE, IF NO RECORDS TO BE PROCESSED *************
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
