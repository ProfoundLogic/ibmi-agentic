     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*** SY266_Qry dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H****************************************************************
     H/TITLE W8600R - CHECKS ISSUED
     H****************************************************************
     H*    CALLED FROM:                                              *
     H*      W8600C - OVERRIDE                                       *
     H*    CALLS TO:                                                 *
     H*      NONE                                                    *
     H****************************************************************
     H*  REVISIONS:                                                  *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*
     H* 01/11/13 C:287 Project: SY266_QRY                             *
     H*                                                              *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  08/08/96  C:33  PROJECT# 25WGM                              *
     H*                  CHANGED ROUTING FOR DEMAND OR BATCH RUN.    *
     H****************************************************************
     F*
     F* * * Batch Routing File
     FSMORL     IF   E           K DISK
     F*
     F* * * Company File
     FSMCOL     IF   E           K DISK
     F*
     F* * * Fund File
     FWMFNL     IF   E           K DISK
     F*
     F* * * CLAIMS CHECK PAYMENT FILE
     FWDPAL     IF   E           K DISK
     F*
     F* * * User Parameter File
     FWMUPL     IF   E           K DISK
     F*
     F* * * Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PAFND
     C*
     C*   KEY LIST FOR THE PAYMENT FILE
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  USER PARAMETER FILE
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQQUE            10
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
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
      *
     C                   MOVEL     QQDATE        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1DATE            6 0
     C*
     C*  GET THE COMPANY RECORD.
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C                   TIME                    DSDTTM
     C     PAKEY         CHAIN     PPA01                              40
     C*
     C* * * INITIAL SET-UP
     C*
     C  N11              EXSR      SR1ST
     C     *IN40         DOWEQ     '0'
     C*
     C* * * CHECK FOR LEVEL BREAKS
     C     PAFND         IFNE      XXFND
     C                   EXSR      SRFBRK
     C                   END
     C*
     C* * *  DETAIL ROUTINE * * *
     C     PATYPE        IFEQ      'M'
     C                   ADD       1             XXMFND
     C                   END
     C     PATYPE        IFEQ      'P'
     C                   ADD       1             XXPFND
     C                   END
     C**********           END
     C*
     C     PAKEY         READE     PPA01                                  40
     C                   END
     C*
     C* * * PRINT LAST FUND TOTALS
     C* * * CHECK TO SEE IF OVERFLOW OCCURED ON LAST OUTPUT
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DET
     C                   ADD       XXPFND        XXPTOT
     C                   ADD       XXMFND        XXMTOT
     C     XXMTOT        ADD       XXPTOT        XXGTOT
     C* * * AT END PRINT FINAL FUND TOTAL AND GRAND TOTAL
     C* * *    PRINTING TOTALS
     C                   EXCEPT    TOT
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
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     CSR   SR1ST         BEGSR
     C* * * INITIALIZE LEVEL BREAK HOLD FIELDS
     C                   MOVE      PAFND         XXFND             3 0
     C                   Z-ADD     0             XXPFND            7 0
     C                   Z-ADD     0             XXMFND            7 0
     C                   Z-ADD     0             XXPTOT            7 0
     C                   Z-ADD     0             XXMTOT            7 0
     C                   Z-ADD     0             XXGTOT            9 0
     C* * * GET FUND NAME FROM FUND MASTER FILE
     C     FNKEY         CHAIN     PFN01                              42
     C     *IN42         IFEQ      '0'
     C                   MOVEL     FNSHNM        XXNAME           30
     C                   ELSE
     C                   MOVEL     *BLANKS       XXNAME
     C                   END
     C* * * GET USER NAME FROM USER PARM FILE
     C     UPKEY         CHAIN     PUP01                              43
     C*
      *  CONVERT SYSTEM DATE TO 8 DIGIT FORMAT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXDATE            8 0
      *
     C* * * PRINT 1ST PAGE HEADINGS
     C*   GET THE DISTRIBUTION INFORMATION
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*
     C*   IF THIS REPORT IS A DEMAND, BLANK OUT ALL DISBURSEMENT INFO
     C*   FIELDS AND PRINT THE USER'S NAME INSTEAD. IF A BATCH REPORT,
     C*   PRINT NORMAL DISTRIBUTION.
     C*
     C     QQQUE         IFNE      *BLANKS
     C                   MOVEL     UPUSNM        ORDIS1
     C                   MOVE      *BLANK        ORDIS2
     C                   MOVE      *BLANK        ORDIS3
     C                   MOVE      *BLANK        ORDIS4
     C                   MOVEL     QQQUE         OROUTQ
     C                   ENDIF                                                  QQQUE<>*BLANKS
     C*
     C*
      *   CENTER TEXT IN FIELDS
     C*
     C* CENTER TEXT IN FIELD CONAME
     C*
     C                   MOVEL     CONAME        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXNAM1           40
     C*
     C* CENTER TEXT IN FIELD ORTITL
     C*
     C                   MOVEL     ORTITL        ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTITL           60
     C*
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ***************************************
     C***********        EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ***************************************
     C                   END
     C                   EXCEPT    HEAD
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C                   ENDSR
      *==================================================================*
      *  Level Break on Fund #             ** Subroutine **  S R F B R K *
      *==================================================================*
     CSR   SRFBRK        BEGSR
     C* * * CHECK TO SEE IF OVERFLOW OCCURED ON LAST OUTPUT
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C* * * PRINT FUND TOTALS
     C                   EXCEPT    DET
     C* * * ACCUMULAT GRAND TOTALS * *
     C                   ADD       XXPFND        XXPTOT
     C                   ADD       XXMFND        XXMTOT
     C* * * CLEAR FUND TOTALS
     C                   Z-ADD     0             XXPFND
     C                   Z-ADD     0             XXMFND
     C* * * GET NEW FUND INFORMATION
     C     FNKEY         CHAIN     PFN01                              42
     C     *IN42         IFEQ      '0'
     C                   MOVEL     FNNAME        XXNAME
     C                   ELSE
     C                   MOVEL     *BLANKS       XXNAME
     C                   END
     C* * * INITIALIZE LEVEL BREAK HOLD FIELDS
     C                   MOVE      PAFND         XXFND
     C                   ENDSR
      *==================================================================*
      *  Print Fund Totals                 ** Subroutine **  S R F P R T *
      *==================================================================*
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ***************************************
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
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ***************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              96
      ***** Beg Del ***** 08/26/14 ***************************************
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
      ***** End Del ***** 08/26/14 ***************************************
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O*
      *----------- REPORT OUTPUT STARTS HERE ------------------------*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXNAM1              86
     O                       XXDATE        Y    131
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       XXTITL              96
     O                                          125 'PAGE:'
     O                       PAGE          Z    131
     O          E            HEAD           1
     O                                           47 'COMPUTER'
     O                                           60 'MANUALLY'
     O          E            HEAD           2
     O                                            5 'GROUP'
     O                                           17 'GROUP NAME'
     O                                           48 'GENERATED'
     O                                           60 'PRODUCED'
     O                                           79 'RUN DATE:'
     O                       X1DATE        Y     90
     O          E            DET            1
     O                       XXFND                5
     O                       FNSHNM              37
     O                       XXPFND        1     48
     O                       XXMFND        1     60
     O          E            TOT         2  2
     O                                           33 'TOTAL:'
     O                       XXPTOT        1     48
     O                       XXMTOT        1     60
     O          E            TOT            1
     O                                           33 'GRAND TOTAL:'
     O                       XXGTOT        1     48
