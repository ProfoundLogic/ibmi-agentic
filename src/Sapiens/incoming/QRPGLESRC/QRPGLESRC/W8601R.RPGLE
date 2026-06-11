     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
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
     H* 08/05/99 C:118 PROJECT: WC577
     H*                Made V3 verbiage changes.
     H*
     H* 01/07/99 C:15 Support: WC095/15781                            *
     H*               Changed the pgm to prevent an error when the    *
     H*               wrong date is passed into the pgm.              *
     H*****************************************************************
     H/TITLE W8601R - PAY-LAG REPORT
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMCML3    IF   E           K DISK
     FWMLPL1    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)

     D FND             S              3  0 DIM(50)                              ALL FUND#'S
     D NEW             S              5  0 DIM(50)                              NEW CLMS/FND
     D LT              S              5  0 DIM(50)                              #LATE PMTS/FND
     D LCD             S              2    DIM(50)                              LATE CODES
     D CNT             S              5  0 DIM(50)                              CNT/CODE
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
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE CLAIM FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXENTD            7 0
     C*
     C*   KEY LIST FOR THE LATE PAYMENT FILE
     C*
     C     LPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFRDT
     C*
     C     LPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  USER PARAMETER FILE
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
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TBKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *LIKE         DEFINE    LPDT          XXFRDT
     C     *LIKE         DEFINE    LPDT          XXTODT
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
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQDATE        XXDT8             8 0
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   Z-ADD     XXTODT        XXFRDT
     C                   MOVE      '01'          XXFRDT
     C                   TIME                    DSDTTM
     C*
     C                   EXSR      SR1ST
     C*
     C*  LOAD ALL LATE CODES INTO ARRAY
     C*
     C                   Z-ADD     1             X                 3 0
     C     TBKY1         CHAIN     PLR01                              60
     C     *IN60         DOWEQ     '0'
     C     X             ANDLE     50
     C                   MOVEL     LRKEY         XXCODE            2
     C                   MOVE      XXCODE        LCD(X)
     C                   ADD       1             X
     C     TBKY1         READE     PLR01                                  60
     C                   END
     C*
     C*  LOAD ALL FUND #'S INTO ARRAY
     C*
     C                   Z-ADD     1             X                 3 0
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         DOWEQ     '0'
     C     X             ANDLE     50
     C                   MOVE      FNFND         FND(X)
     C*******    FNFND     IFNE 999
     C                   ADD       1             X
     C*******              END
     C     FNKEY         READE     PFN01                                  60
     C                   END
     C*
     C*  CALCULATE # OF NEW CLAIMS
     C*
     C                   MOVE      XXFRDT        XXENTD
     C     CMKY1         SETLL     PCM01
     C*** BEGIN CHG *** 01/07/99 **************************************
     C*********            READEPCM01                    66
     C     CMKEY         READE     PCM01                                6666
     C*** END CHG ***** 01/07/99 **************************************
     C*
     C     *IN66         DOWEQ     *OFF
     C     CMENTD        ANDLE     XXTODT
     C     CMLTFL        IFEQ      'I'
     C                   Z-ADD     1             X
     C     CMFND         LOOKUP    FND(X)                                 60
     C   60              ADD       1             NEW(X)
     C                   ENDIF                                                  CMLTFL IFEQ 'I'
     C*
     C     CMKEY         READE     PCM01                                  66
     C                   ENDDO                                                  *IN66 DOWEQ*OFF
     C*
     C                   XFOOT     NEW           XXNWTL            7 0
     C*
     C*  GET ALL LATE PAYMENTS
     C*
     C                   Z-ADD     1             X
     C                   MOVE      FND(X)        XXFND             3 0
     C     XXFND         DOWNE     0
     C     X             ANDLE     50
     C*
     C     LPKEY         SETLL     PLP01
     C     LPKY1         READE     PLP01                                  60
     C     *IN60         DOWEQ     '0'
     C     LPTRDT        ANDLT     XXTODT
     C                   ADD       1             LT(X)
     C     LPCODE        IFNE      *BLANKS
     C                   Z-ADD     1             Y                 3 0
     C     LPCODE        LOOKUP    LCD(Y)                                 60
     C   60              ADD       1             CNT(Y)
     C                   END
     C     LPKY1         READE     PLP01                                  60
     C                   END
     C*
     C                   Z-ADD     0             XXFND
     C                   ADD       1             X
     C     X             IFLE      50
     C                   Z-ADD     FND(X)        XXFND
     C                   END
     C                   END
     C*
     C                   EXCEPT    DET1
     C                   EXCEPT    HEADB
     C*
     C*  READ THRU LATE CODE ARRAY & PRINT TOTALS FOR EACH CODE
     C*
     C                   Z-ADD     1             X
     C     X             DOWLE     50
     C     LCD(X)        ANDNE     *BLANKS
     C*
     C     CNT(X)        IFNE      0
     C                   MOVE      *BLANKS       LRDESC
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     LCD(X)        XXKEY
     C     TBKEY         CHAIN     PLR01                              60
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADB
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    CDET
     C                   END
     C*
     C                   ADD       1             X
     C                   END
     C*
     C*  CALCULATE & PRINT LATE PAYMENT CODE GRAND TOTAL
     C*
     C                   XFOOT     CNT           XXLATE            7 0
     C                   EXCEPT    CTOT
     C*
     C*   CALCULATE PAY-LAG REPORT & PRINT
     C*
     C                   EXSR      PYLG
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C     SR1ST         BEGSR
     C*
     C*  GET THE COMPANY RECORD.
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
     C*  CHAIN TO USER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*  GET DISTRIBUTION INFO & PRINT
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         MOVE      '0'           XXPRT
     C**********     ORKEY         CHAIN     POR01                              60
     C**********     *IN60         IFEQ      '0'
     C**********         EXCEPT    ROUTE
     C**********         END
      ***** End Del ***** 08/26/14 ************************************
     C                   EXCEPT    HEAD
     C*
     C                   ENDSR
      *==================================================================*
      *  CALC & PRINT PAY-LAG              ** Subroutine **  P Y L G   K *
      *==================================================================*
     CSR   PYLG          BEGSR
     C*
     C*  CALCULATE & PRINT PAY-LAG INFO
     C*
     C                   EXCEPT    HEADP
     C                   Z-ADD     1             X
     C     X             DOWLE     50
     C     FND(X)        ANDNE     0
     C*
     C*  LOAD FUND NAME
     C*
     C                   MOVE      *BLANKS       XXSHNM
     C                   Z-ADD     FND(X)        XXFND
     C     FNKY1         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNSHNM        XXSHNM           20
     C                   END
     C*
     C                   Z-ADD     0             XXPYLG            5 2
     C                   Z-ADD     0             XXWK52            5 2
     C     NEW(X)        IFNE      0
     C     LT(X)         DIV       NEW(X)        XXWK54            5 4
     C     XXWK54        MULT      100           XXWK52
     C     100           SUB       XXWK52        XXPYLG
     C                   END
     C     NEW(X)        MULT      XXPYLG        XXWK90            9 0
     C     XXWK90        DIV       100           XXTMLY            5 0
     C     XXPYLG        COMP      0                                      78
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    HEAD
     C                   EXCEPT    HEADP
     C                   MOVE      '0'           *INOF
     C                   END
     C                   EXCEPT    DET
     C*
     C                   ADD       1             X
     C                   END
     C                   ENDSR
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
     O**********             UPUSNM              67
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
     O                       UPUSNM              67
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
     O****    E   60           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
     O****                               46 'VE DATA SOLUTIONS. INC,'
     O****                               51 '.1990'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              84
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              93
     O                                          127 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           3
     O                                           16 'RUN DATE: '
     O                       XXDT8               26 '  /  /    '
     O          E            DET1           2
     O                                           27 'NUMBER OF NEW LOST TIME'
     O                                           34 'CLAIMS'
     O                       XXNWTL        J     48
     O          E            HEADB          2
     O                                           21 'BREAKDOWN OF LATE'
     O                                           40 'PAYMENTS - OVERALL'
     O          E            CDET           1
     O                       LRDESC              40
     O                       CNT(X)        J     47
     O          E            CTOT        1  3
     O                                           23 'LATE PAYMENTS GRAND'
     O                                           30 'TOTAL:'
     O                       XXLATE        J     47
     O          E            HEADP          2
     O                                           13 'PAY - LAG'
     O          E            HEADP          1
     O                                           59 'NEW LOST      LATE'
     O                                           86 'TIMELY'
     O          E            HEADP          2
     O***** BEGIN CHG **** 08/05/99 ***********************************
     O**********                         18 'FUND   FUND  NAME'
     O                                           20 'GROUP  GROUP NAME'
     O***** END   CHG **** 08/05/99 ***********************************
     O                                           51 'PAY-LAG  TIME CLAIMS'
     O                                           70 'PAYMENTS  RECHECK'
     O                                           98 'PAID    PAY - LAG'
     O                                          111 'CALCULATIONS'
     O          E            DET            1
     O                       FND(X)        Z     08
     O                       XXSHNM              30
     O                       XXPYLG        1     37
     O                                           38 '%'
     O                       NEW(X)        J     47
     O                       LT(X)         J     59
     O                 N78   NEW(X)        1     68
     O                 N78                       69 '('
     O                 N78   XXPYLG        1     75
     O                 N78                       76 '%'
     O                 N78                       79 ') ='
     O                 N78   XXTMLY        J     86
     O                 N78   LT(X)         J     95
     O                 N78                       97 '/'
     O                 N78   NEW(X)        J    104
     O                 N78                      106 '='
     O                 N78   XXWK52        1    113
     O          E            DET            1
     O                 N78                       97 '100 -'
     O                 N78   XXWK52        1    104
     O                 N78                      106 '='
     O                 N78   XXPYLG        1    113
     O                    N78                   114 '%'
