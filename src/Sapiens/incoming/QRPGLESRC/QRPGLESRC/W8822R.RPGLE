     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8822R - EXCESS CLAIMS REPORT (VARIABLE % OF RETENTION)
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*  CREATED:              PROGRAMMER: C:         PROJECT: WC577  *
     H*****************************************************************
     H*                                                               *
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
     H*  11/13/98 C:61 PROJECT: WC577                                 *
     H*              - Produce routing page when NO RECORDS MEET      *
     H*                SELECTION CRITERIA.                            *
     H*              - Correct print of routing page                  *
     H*              - Correct passing of FYR parm.                   *
     H*                                                               *
     H*  01/28/98 C:85 PROJECT: WC770                                 *
     H*                VERSION 3 STANDARDS.                           *
     H*                                                               *
     H*  07/07/92  JEI  PROJECT: WC157                                *
     H*                 CHANGED CASE NUMBER BACK TO 7 NUMERIC FOR ALL *
     H*                 'BASE' FILES (XXCASE) - CASE NUMBER REMAINS   *
     H*                 14 ALPHA FOR REINSURANCE FILES (X@CASE)       *
     H*                                                               *
     H*  04/03/92  LPK  PROJECT: MD027                                *
     H*                 CHANGED CASE NUMBER FROM 7 NUMERIC TO 14      *
     H*                 ALPHA FIELD.                                  *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*    *IN11 - INITIALIZATION                                     *
     H*    *IN60 - FILE INDICATOR                                     *
     H*    *IN25 - ENTERED FYR = 0                                    *
     H*    *IN26 - LAST RESERVE CHG DURING MONTH OF RUN DATE          *
     H*    *IN09 - RPT BEING RUN IN DEMAND MODE                       *
     H*    *IN08 - RPT BEING RUN IN BATCH MODE                        *
     H*    *IN79 - MULTIPLE CLAIM FLAG                                *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     F*****************************************************************
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWXECL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D*                                                               *
     D*** BEGIN ADD *** 01/28/98 ***********************************************
     D OPTNM           C                   CONST('Excess Claims Report-
     D                                      (Variable % of Rete-
     D                                     ntion)')
     D*** END   ADD *** 01/28/98 ***********************************************
     D*
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXDATE                 1      7  0
     D  XXRNYM                 2      5  0
     D                 DS
     D  ECRSDT                 1      7  0
     D  XXYYMM                 2      5  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*  FUND FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     *LIKE         DEFINE    ECCO#         XXCO#
     C     *LIKE         DEFINE    ECFND         XXFND
     C     *LIKE         DEFINE    ECFYR         XXFYR
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
      ***** BEG DEL ******* 11/13/98 **********************************
     C*********            PARM           QQFYR   2
      ***** END DEL ******* 11/13/98 **********************************
      ***** BEG ADD ******* 11/13/98 **********************************
     C                   PARM                    QQFYR             3
      ***** END ADD ******* 11/13/98 **********************************
     C                   PARM                    QQDTE             8
     C                   PARM                    QQPCT             5
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
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQDTE         X1RNDT            8 0
     C*
     C                   Z-ADD     X1RNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRNDT            6 0
     C*
     C                   MOVE      QQPCT         XXPCT             5 2
      ***** BEG DEL ******* 11/13/98 **********************************
     C*********  QQFYR     COMP '00'                     25
      ***** END DEL ******* 11/13/98 **********************************
      ***** BEG ADD ******* 11/13/98 **********************************
     C     QQFYR         COMP      '000'                                  25
      ***** END ADD ******* 11/13/98 **********************************
     C                   TIME                    DSDTTM
     C*
      ***** BEG ADD ******* 11/13/98 **********************************
     C  N11              EXSR      SR1ST
      ***** END ADD ******* 11/13/98 **********************************
      *
     C                   READ      PEC01                                  60
     C   60              EXCEPT    NONE
     C     *IN60         DOWEQ     '0'
     C*
     C*  FIRST TIME INITIALIZATION
     C*
      ***** BEG DEL ******* 11/13/98 **********************************
     C**** N11 ****        EXSR SR1ST
      ***** END DEL ******* 11/13/98 **********************************
     C*
     C*  CHECK FOR FUND YEAR BREAK
     C*
     C     XXFYR         IFNE      ECFYR
     C                   MOVE      '1'           *INOF
     C                   Z-ADD     ECFYR         XXFYR
     C                   END
     C*
     C*  FLAG MULTIPLE CLAIMS
     C*
     C                   MOVE      '0'           *IN79
     C     ECCASE        IFEQ      *ZEROS
     C     ECCLM#        ANDNE     0
     C                   MOVE      '1'           *IN79
     C                   END
     C*
     C*  IF DATE OF LAST RESERVE CHANGE IS WITHIN SAME MONTH OF
     C*  ENTERED RUN DATE, FLAG WITH AN '*'.
     C*
     C     XXYYMM        COMP      XXRNYM                                 26
     C*
     C                   Z-ADD     0             XXCASE            7 0
     C                   MOVE      ECCASE        XXCASE
     C*
     C                   Z-ADD     ECACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACDT            6 0
     C*
     C                   Z-ADD     ECRTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRTDT            6 0
     C*
     C                   Z-ADD     ECRSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRSDT            6 0
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    DET
     C*
     C                   READ      PEC01                                  60
     C                   END
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C                   MOVE      '1'           *INLR
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     C     SR1ST         BEGSR
     C*
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*** BEGIN ADD *** 01/28/98 ***********************************************
     C*
     C*   CENTER COMPANY NAME
     C*
     C                   MOVEL(P)  CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL(P)  ##DATA        CONAME
     C*
     C*** END   ADD *** 01/28/98 ***********************************************
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C                   MOVEL     FNNAME        XFNAME           60
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
      *
      ***** BEG DEL ******* 11/13/98 **********************************
     C*** BEGIN ADD *** 01/28/98 ***********************************************
     C*************
     C***CENTER*PROGRAM TITLE
     C*************
     C*************        MOVELOPTNM     XXTITL 60 P
     C*************
     C*************        MOVELXXTITL    ##DATA    P
     C*************
     C*************        CALL 'SRCENTER'             99
     C*************        PARM           ##DATA 80
     C*************        PARM 40        ##LENG  30
     C*************
     C*************        MOVEL##DATA    XXTITL    P
     C*************                                                   *
     C*** END   ADD *** 01/28/98 ***********************************************
      ***** END DEL ******* 11/13/98 **********************************
      ***** BEG ADD ******* 11/13/98 **********************************
     C                   MOVEL(P)  OPTNM         XXTITL           60
      *
      * Left Justify Title in Field
      *
     C     XXTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   XXTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      XXTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      *
      ***** END ADD ******* 11/13/98 **********************************
     C*
      ***** Beg Del ***** 08/26/14 *************************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *************************************************
     C*
      ***** BEG ADD ******* 11/13/98 **********************************
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  XXTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXTITL
      *
      ***** END ADD ******* 11/13/98 **********************************
     C*
     C                   END
     C*
     C                   Z-ADD     ECFYR         XXFYR
     C                   MOVE      '1'           *IN11
     C*
     C* * * PRINT 1ST PAGE HEADINGS
     C*
      ***** BEG DEL ******* 11/13/98 **********************************
     C**********           EXCPTHEAD
      ***** END DEL ******* 11/13/98 **********************************
     C*
     C                   ENDSR
     C*
     C**
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *************************************************
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
      ***** End Del ***** 08/26/14 *************************************************
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
     O*** CHG 01/28/98 ***     ORTITL    80
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 *************************************************
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *************************************************
     O*
     O*** BEGIN ADD *** 01/28/98 ***********************************************
     O          E            ROUTE       1  2
     O                                           23 'REPORT SELECTIONS'
     O*** END   ADD *** 01/28/98 ***********************************************
     O*
     O*** CHG 01/28/98 ***
     O          E            ROUTE          2
     O*** CHG 01/28/98 ***               11 'FUND:'
     O                                           21 'GROUP . . . . :'
     O*** CHG 01/28/98 ***     XXFND Z   27
     O                       XXFND         Z     25
     O          E            ROUTE          2
     O*** CHG 01/28/98 ***               14 'FUND YR:'
     O                                           21 'POLICY YEAR   :'
     O*** CHG 01/28/98 **N25   QQFYR     27
      ***** BEG CHG ******* 11/13/98 **********************************
     O*********          N25   QQFYR     25
     O                 N25   QQFYR               26
      ***** END CHG ******* 11/13/98 **********************************
     O*** CHG 01/28/98 ***25             28 '*ALL'
     O                  25                       26 '*ALL'
     O          E            ROUTE          2
     O*** CHG 01/28/98 ***               15 'RUN DATE:'
     O                                           21 'RUN DATE  . . :'
     O*** CHG 01/28/98 ***     XXRNDTY   32
     O                       XXRNDT        Y     30
     O          E            ROUTE          2
     O                                           21 '% OF RETENTION:'
     O*** CHG 01/28/98 ***     XXPCT 1   30
     O                       XXPCT         1     28
     O*
     O*
     O*** CHG 01/28/98 ***               23 '(C) COPYRIGHT PROGRESSI'
     O*** CHG 01/28/98 ***               46 'VE DATA SOLUTIONS. INC,'
     O*** CHG 01/28/98 ***               51 '.1997'
     O*
     O*** BEGIN CHG *** 01/28/10 **************************************
     O*****   E  160           ROUTE
     O***                                23 '(c) Copyright Progressi'
     O***                                   've Data Solutions, Inc.,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
     O          E            NONE           1 06
     O                       DSTIME              08 '  :  :  '
     O*** CHG 01/28/98 ***     CONAME    87
     O                       CONAME              86
     O                       DSDATE        Y    132
     O          E            NONE           3
     O                       Q1PGM               10
     O*** CHG 01/28/98 ***     ORTITL    96
      ***** BEG CHG ******* 11/13/98 **********************************
     O*********                XXTITL   104
     O                       XXTITL              96
      ***** END CHG ******* 11/13/98 **********************************
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            NONE           2
     O                                           33 'NO OUTPUT FOR SELECTION'
     O                                           42 'CRITERIA'
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O*** CHG 01/28/98 ***     CONAME    87
     O                       CONAME              86
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O*** CHG 01/28/98 ***     ORTITL    96
      ***** BEG CHG ******* 11/13/98 **********************************
     O**********               XXTITL   104
     O                       XXTITL              96
      ***** END CHG ******* 11/13/98 **********************************
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O*** CHG 01/28/98 ***                5 'FUND:'
     O                                            6 'GROUP:'
     O*** CHG 01/28/98 ***     XXFND Z    9
     O                       XXFND         Z     10
     O*** CHG 01/28/98 ***     XFNAME    70
     O                       XFNAME              71
     O                                          123 'RUN DATE:'
     O                       XXRNDT        Y    132
     O          E            HEAD           2
     O*** CHG 01/28/98 ***               10 'FUND YEAR:'
     O                                           12 'POLICY YEAR:'
     O*** CHG 01/28/98 ***     XXFYR Z   14
     O                       XXFYR         Z     16
     O                                           35 '% OF RETENTION:'
     O                       XXPCT         1     42
     O                                           93 '* APPEARS NEXT TO CLAIM'
     O                                          115 'S WITH RESERVE CHANGES'
     O                                          132 'IN CURRENT MONTH'
     O          E            HEAD           1
     O                                           69 'MULTIPLE   ACCIDENT'
     O                                          125 'DATE RETN   DT OF LAST'
     O                                          130 'CLM'
     O          E            HEAD           2
     O                                           13 'CLAIMANT NAME'
     O                                           36 'CLAIM#'
     O                                           67 'CLAIM#      DATE'
     O                                           86 'TOTAL INCURRED'
     O                                           97 'RETENTION'
     O                                          112 '% REACHED'
     O                                          130 'RSV CHANGE  STS'
     O          E            DET            1
     O                 N79   ECNAME              30
     O                  79                       21 'MULTIPLE CLAIM'
     O                       XXCASE        Z     36
     O                       ECCLM#        Z     57
     O                       XXACDT        Y     69
     O                       ECTOTI        J     85
     O                       ECRETN        J     99
     O                       XXRTDT             111 '  /  /  '
     O                       XXRSDT             124 '  /  /  '
     O                       ECOCST             129
     O                  26                      131 '*'
