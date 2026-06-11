     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W7009R1 - SUPERVISORS EOMB PEND REPORT
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     THIS PROGRAM ALLOWS THE USER TO PRINT THE SUPERVISORS   **
     H**     EOMB PEND REPORT  FOR ALL EOMB PAYMNETS WHICH ARE OLDER **
     H**     THAN X NUMBER OF DAYS.                                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     W7009C1                                                 **
     H**   CALLS TO:                                                 **
     H**     NONE                                                    **
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               **
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  11/25/98  C:94  PROJECT: 48MBV                               *
     H*                  MAKE YR2000 COMPLIANT                        *
     H*                                                               *
     H*****************************************************************
     H*
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*     11   -      CONDITION FIRST TIME THROUGH PROGRAM          *
     H*     60   -      GENERAL CHAINING                              *
     H*                                                               *
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**                                                             **
     H**    WXRLL  (PRL01): CO#/SORT/FND/FYR/CASE/SEQ                **
     H**    SMUPL1 (PUP01): STAFF                                    **
     H**    SMCOL (PCO01): CO#                                       **
     H*     SMORL  (POR01) ORPNAM,ORFIL#                              *
     H**    WMUPL (PUP01): USER                                      **
     H**    WMUPL1 (PUP02): STAFF                                    **
     H*****************************************************************
     F*
     FWXRLL     IF   E           K DISK
     F* * * Work file built from W7009R2
     F*
     FSMORL     IF   E           K DISK
     F* * * Batch Routing File
     F*
     FSMCOL     IF   E           K DISK
     F* * * Company File
     F*
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     F* * * User Parameter File
     F*
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOF)
     F* * * Output File

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*   LIKE DEFINES
     C*****************************************************************
     C*
     C     *LIKE         DEFINE    RLCO#         XXCO#
     C     *LIKE         DEFINE    RLFND         XXFND
     C     *LIKE         DEFINE    RLFYR         XXFYR
     C     *LIKE         DEFINE    UPSTAF        XXSTAF
     C     *LIKE         DEFINE    RLSORT        XXSORT
     C*** BEG DEL *** 11/25/98 ***************************************
     C********** *LIKE     DEFN RLFRDT    XXFRDT
     C********** *LIKE     DEFN RLTODT    XXTODT
     C********** *LIKE     DEFN RLTRDT    XXTRDT
     C*** END DEL *** 11/25/98 ***************************************
     C*
     C*****************************************************************
     C*                   KEY LISTING
     C*****************************************************************
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE STAFF PARAMETER FILE
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXSTAF
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C*** BEG CHG *** 11/25/98 ***************************************
     C                   PARM                    QQDATE            7
     C**********           PARM           QQDATE  6
     C*** END CHG *** 11/25/98 ***************************************
     C                   PARM                    QQSRP             1
     C                   PARM                    QQDAYS            5
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
     C                   MOVE      QQCO#         XXCO#
     C*** BEG CHG *** 11/25/98 ****************************************
     C                   MOVE      QQDATE        XXDTE2            7 0
     C**********           MOVE QQDATE    XXDTE2  60
     C*** END CHG *** 11/25/98 ****************************************
     C                   MOVE      QQDAYS        XXDAYS            5 0
     C**
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C**
     C**
     C* START TO READ THE WORK FILE
     C*
     C                   READ      PRL01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C**** FIRST SAVE
     C     *IN70         IFEQ      '0'
     C                   MOVE      RLSORT        XXSORT
     C                   MOVEL     RLSORT        XXSTAF
     C     UPKY1         CHAIN     PUP02                              60
     C                   EXCEPT    HEAD
     C                   MOVE      '1'           *IN70
     C                   END
     C*
     C**  NEW SUPERVISOR
     C*
     C     RLSORT        IFNE      XXSORT
     C                   Z-ADD     *ZERO         PAGE
     C                   MOVE      RLSORT        XXSORT
     C                   MOVEL     RLSORT        XXSTAF
     C     UPKY1         CHAIN     PUP02                              60
     C                   EXCEPT    HEAD
     C                   END
     C**
     C**  DETAIL
     C**
     C                   Z-ADD     RLFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C                   Z-ADD     RLTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C                   Z-ADD     RLTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTRDT            8 0
     C                   EXCEPT    DETAIL
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   END
     C                   READ      PRL01                                  60
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     UPKY          CHAIN     PUP01                              60
     C*
     C* * * INITIALIZE FIELDS
     C                   TIME                    DSDTTM
     C*
     C     COKY          CHAIN     SMCOL                              60
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C*
     C                   ENDSR
     C*****************************************************************
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
     O**********E            ROUTE          2
     O**********                                 15 'ROUTE TO:'
     O**********             UPUSNM              67
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
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
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE       2  2
     O                                           32 'Supervisors Report (Y/N)'
     O                       QQSRP               36
     O          E            ROUTE          1
     O                                           28 'Payments older than:'
     O                       XXDAYS              36
     O*** BEG ADD *** 11/25/98 ***************************************
     O                                           42 'Days'
     O*** END ADD *** 11/25/98 ***************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O*****   E  160           ROUTE
     O****                               24 '(C) PROGRESSIVE DATA '
     O****                               45 'SOLUTIONS, INC. 1995'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ***** END CHG ****** 01/28/10 ***********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             116
     O                       DSDATE        Y    198
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             127
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O          E            HEAD           1
     O                                           11 'SUPERVISOR:'
     O                       XXSTAF              15
     O                       UPUSNM              67
     O          E            HEAD           2
     O                                           20 'Payments older than:'
     O                       XXDAYS        Z     26
     O*** BEG ADD *** 11/25/98 ***************************************
     O                                           31 'Days'
     O*** END ADD *** 11/25/98 ***************************************
     O          E            HEAD           1
     O*** BEG CHG *** 11/25/98 ***************************************
     O                                           98 'TR'
     O                                          103 'RC'
     O                                          108 'RE'
     O                                          113 'RE'
     O                                          161 'ST'
     O                                          180 ' INITIAL '
     O                                          186 'LATE'
     O**********
     O**********                         77 'TR'
     O**********                         80 'RC'
     O**********                         83 'RE'
     O**********                         86 'RE'
     O**********                        130 'ST'
     O**********                        144 ' INITIAL '
     O**********                        150 'LATE'
     O*** END CHG *** 11/25/98 ***************************************
     O          E            HEAD           2
     O*** BEG CHG *** 11/25/98 ***************************************
     O                                            3 'GRP'
     O                                            9 'FYR'
     O                                           19 'CASE'
     O                                           29 'SEQ#'
     O                                           42 'TRANS DATE'
     O                                           56 'AMOUNT'
     O                                           62 'P-1'
     O                                           67 'P-2'
     O                                           80 'FROM DATE'
     O                                           91 'TO DATE'
     O                                           98 'CD'
     O                                          103 'TP'
     O                                          108 'TP'
     O                                          113 'CD'
     O                                          120 'NAME'
     O                                          161 'CD'
     O                                          167 'GRP'
     O                                          180 'REQUESTOR'
     O                                          186 'CODE'
     O**********
     O**********                          3 'FND'
     O**********                         13 'CASE'
     O**********                         34 'TR DATE'
     O**********                         47 'AMOUNT'
     O**********                         52 'P-1'
     O**********                         56 'P-2'
     O**********                         65 'FR DATE'
     O**********                         74 'TO DATE'
     O**********                         77 'CD'
     O**********                         80 'TP'
     O**********                         83 'TP'
     O**********                         86 'CD'
     O**********                         92 'NAME'
     O**********                        130 'CD'
     O**********                        134 'GRP'
     O**********                        144 'REQUESTOR'
     O**********                        150 'CODE'
     O*** END CHG *** 11/25/98 ***************************************
     O*** BEG CHG *** 11/25/98 ***************************************
     O          E            DETAIL         1
     O                       RLFND         Z      3
     O                       RLFYR         Z      9
     O                       RLCASE        Z     19
     O                       RLSEQ#        Z     29
     O                       XXTRDT        Y     42
     O                       RLAMT         J     57
     O                       RLBR2               62
     O                       RLBR7               67
     O                       XXFRDT        Y     80
     O                       XXTODT        Y     93
     O                       RLTRCD              98
     O                       RLTYPE             103
     O                       RLRETP             108
     O                       RLRECD             113
     O                       RLPNAM             156
     O                       RLSTS              161
     O                       RLPGTP             167
     O                       RLRQST             180
     O                       RLLTCD             185
     O**********
     O********E* 1             DETAIL
     O**********               RLFND Z    3
     O**********               RLFYR Z    7
     O**********               RLCASEZ   15
     O**********               RLSEQ#Z   25
     O**********               XXTRDTY   34
     O**********               RLAMT J   48
     O**********               RLBR2     51
     O**********               RLBR7     55
     O**********               XXFRDTY   65
     O**********               XXTODTY   74
     O**********               RLTRCD    77
     O**********               RLTYPE    80
     O**********               RLRETP    83
     O**********               RLRECD    86
     O**********               RLPNAM   127
     O**********               RLSTS    130
     O**********               RLPGTP   134
     O**********               RLRQST   145
     O**********               RLLTCD   150
     O*** END CHG *** 11/25/98 ***************************************
