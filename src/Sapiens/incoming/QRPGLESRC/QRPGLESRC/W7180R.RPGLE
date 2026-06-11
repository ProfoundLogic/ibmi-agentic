     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W7180R - DAILY ADJUSTMENT EDIT BY PROCESSOR
     H*****************************************************************
     H*  REVISIONS:                                                   *
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
     H*  12/10/99 C:90 PROJECT: WC577                                 *
     H*                Y2K CHANGE - DISPLAY YR AS 3 CHAR.             *
     H*                                                               *
     H*  08/20/99 C:90 SUPPORT:WA096/19360                            *
     H*                CHANGED TOTALS TO SUBTRACT PAAMT FOR PATYPE    *
     H*                = 'V', 'S', OR 'R'.                            *
     H*                                                               *
     H*  04/14/93 C04  PROJECT: WC211                                 *
     H*                FIX ENDLESS LOOP POSSIBILITY IF A BAD USER     *
     H*                PROFILE IS FOUND                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWDPAL2    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    PADT          XXDATE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C*   KEY LIST FOR THE PAYMENT FILE
     C*
     C     PAKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXDATE
     C*
     C*   KEY LIST FOR THE PAYMENT FILE BY USER
     C*
     C     PAKEY1        KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXDATE
     C                   KFLD                    XXUSER
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*   KEY LIST FOR THE REPORT OVERRIDE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    PAUSER
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQDATE        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY1
     C                   MOVEL     SRDTO         X1DATE            6 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
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
     C     *IN60         IFEQ      '0'
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C*   POSITION @ THE BEGINNING OF THE FILE FOR THIS DATE.
     C*
     C     PAKEY         SETLL     PPA01
     C*
     C     TOP           TAG
     C                   SETON                                            22
     C*
     C*   READ THE RECORDS BY THE COMPANY & TX DATE.
     C*
     C     PAKEY         READE     PPA01                                  60
     C     *IN60         CABEQ     '1'           END                      LR
     C     PATYPE        CABEQ     'P'           TOP
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      *BLANKS       XXNAME
     C  N60              MOVEL     UPUSNM        XXNAME           40
     C                   MOVE      *BLANKS       XXUSER
     C                   MOVEL     PAUSER        XXUSER           10
     C*
     C     LOOP          TAG
     C*
     C                   Z-ADD     PAFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     PATODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   Z-ADD     PAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXPODT            6 0
     C*
      **** BEGIN ADD **** 08/20/99 ************************************
     C     PATYPE        IFEQ      'V'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'R'
     C     PAAMT         MULT      -1            PAAMT
     C                   ENDIF
      **** END   ADD **** 08/20/99 ************************************
     C                   ADD       PAAMT         XXAMTS           11 2
     C                   MOVEL     PADESC        XXDESC           30
     C                   ADD       PAAMT         LRAMTS           11 2
     C                   ADD       1             XXRCDS            5 0
     C                   ADD       1             LRRCDS            5 0
     C*
     C*   PRINT THE HEADINGS IF OVERFLOW IS ON.
     C*
     C   22              EXCEPT    HEAD
     C                   SETOFF                                           22
     C*
     C*   PRINT THE DETAIL LINE ITEM.
     C*
     C                   EXCEPT    DET
     C   OF              SETON                                            22
     C*
     C*** READ THE NEXT PAYMENTS RECORD
     C*
     C     READ          TAG
     C     PAKEY1        READE     PPA01                                  60
     C  N60PATYPE        CABEQ     'P'           READ
     C   60              DO
     C*
     C*   PRINT THE HEADINGS IF OVERFLOW IS ON.
     C*
     C   22              EXCEPT    HEAD
     C                   SETOFF                                           22
     C*
     C*   PRINT THE DETAIL LINE ITEM.
     C*
     C                   EXCEPT    DETT
     C   OF              SETON                                            22
     C*
     C     PAKEY1        SETGT     PPA01
     C                   Z-ADD     *ZERO         XXRCDS            5 0
     C                   Z-ADD     *ZERO         XXAMTS           11 2
     C                   GOTO      TOP
     C                   END
     C*
     C*   GOTO READ & GET THE NEXT RECORD FOR THIS USER.
     C*
     C                   GOTO      LOOP
     C*
     C*   PRINT THE REPORT TOTALS & END THE JOB
     C*
     C     END           TAG
     C                   MOVE      '1'           *INLR
     C     XXRCDS        IFGT      *ZERO
     C*
     C*   PRINT THE HEADINGS IF OVERFLOW IS ON.
     C*
     C   22              EXCEPT    HEAD
     C                   SETOFF                                           22
     C*
     C*   PRINT THE DETAIL LINE ITEM.
     C*
     C                   EXCEPT    DETT
     C   OF              SETON                                            22
     C*
     C                   END
     C*
     C*
     C*   PRINT THE HEADINGS IF OVERFLOW IS ON.
     C*
     C   22              EXCEPT    HEAD
     C                   SETOFF                                           22
     C                   EXCEPT    TOTL
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
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
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
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
      ****** BEG CHG ***** 01/28/10 **********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O***                                51 '.1996'
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
     O                                           11 'PROCESSOR:'
     O                       XXUSER              22
     O                       XXNAME              63
     O                                           85 'TRANSACTION DATE:'
     O                       X1DATE        Y     93
     O          E            HEAD           1
     O                                           83 '-PAY-'
     O                                           89 '-RSV-'
     O          E            HEAD           1
     O                                           15 'FND YR  CASE  '
      **** BEGIN CHG **** 12/10/99 ************************************
     O******************************     21 'CHECK#'
     O******************************     28 'POS DT'
     O******************************     36 'PAYEE'
     O******************************     43 'MOD'
     O******************************     47 'BR2'
     O******************************     51 'BR7'
     O******************************     60 'FRM DATE'
     O******************************     69 'TO DATE '
     O******************************     73 'WKS'
     O                                           22 'CHECK#'
     O                                           29 'POS DT'
     O                                           37 'PAYEE'
     O                                           44 'MOD'
     O                                           48 'BR2'
     O                                           52 'BR7'
     O                                           61 'FRM DATE'
     O                                           70 'TO DATE '
     O                                           74 'WKS'
      **** END   CHG **** 12/10/99 ************************************
     O                                           77 'DS'
     O                                           83 'CD TP'
     O                                           89 'CD TP'
     O                                          101 'DESCRIPTION'
     O                                          130 'AMOUNT'
     O          E            DET         1
     O                       PAFND         ZB     3
      **** BEGIN CHG **** 12/10/99 ************************************
     O*********************    PAFYR ZB   6
     O*********************               4 '-'
     O*********************               7 '-'
     O*********************    PACASE B  14
     O*********************    PACHK#ZB  21
     O*********************    XXPODT B  30 '  /  /  '
     O*********************    PAFEIN B  40
     O*********************    PAMOD ZB  44
     O*********************    PABR2  B  47
     O*********************    PABR7  B  51
     O*********************    XXFRDT B  60 '  /  /  '
     O*********************    XXTODT B  69 '  /  /  '
     O*********************    PAWKS ZB  73
     O                       PAFYR          B     7
     O                                            4 '-'
     O                                            8 '-'
     O                       PACASE         B    15
     O                       PACHK#        ZB    22
     O                       XXPODT         B    31 '  /  /  '
     O                       PAFEIN         B    41
     O                       PAMOD         ZB    45
     O                       PABR2          B    48
     O                       PABR7          B    52
     O                       XXFRDT         B    61 '  /  /  '
     O                       XXTODT         B    70 '  /  /  '
     O                       PAWKS         ZB    74
      **** END   CHG **** 12/10/99 ************************************
     O                       PADAYS        ZB    76
     O                       PATRCD         B    80
     O                       PATYPE         B    83
     O                       PARECD         B    86
     O                       PARETP         B    89
     O                       XXDESC         B   120
     O                       PAAMT         BB   132
     O          E            DETT        1  1
     O                       XXRCDS        AB   108
     O                                          115 'TOTAL:'
     O                       XXAMTS        AB   132
     O          E            TOTL        1  1
     O                       LRRCDS        AB   108
     O                                          115 'TOTAL:'
     O                       LRAMTS        AB   132
