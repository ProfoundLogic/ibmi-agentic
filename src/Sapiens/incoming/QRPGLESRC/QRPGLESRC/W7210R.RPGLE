     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7210R - DAILY CASH POSTING EDIT BY PROCESSOR
     H****************************************************************
     H*  REVISIONS:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  04/17/98  C:78  PROJECT# WC577
     H*                  PRINT EMPLOYER NAME IN OUTPUT
     H*
     H*  01/09/98  C:80  PROJECT# WC420
     H*                  MODIFIED PROGRAM TO PROCESS 8 DIGIT DATE
     H*                  FORMAT.
     H*
     H****************************************************************
     F*
     F* Added code to Accumulate A/R by fund year and A/R code
     F*                                                    LJQ 9-21-88
     FSMCOL     IF   E           K DISK
     FWXUEL     UF A E           K DISK
     FWMEAL2    IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     F****** BEGIN ADD ***** 04/17/98 *********************************
     FWMEML     IF   E           K DISK
     F****** END   ADD ***** 04/17/98 *********************************
     F****** BEGIN CHG ***** 04/17/98 *********************************
     F*****QSYSPRT O   F     132     OF     PRINTER
     FQSYSPRT   O    F  145        PRINTER OFLIND(*INOF)
     F****** END   CHG ***** 04/17/98 *********************************

     D XCOD            S              5    DIM(999)                              Fyr/ A/R Code
     D XAMT            S             11    DIM(999)                              Total Amount
     D COMB            S             16    DIM(999)                              Combination
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D* Combination Array                                  LJQ 9-21-88
     D                 DS
     D  XCOMB                  1     16
     D  XYEAR                  1      3  0
     D  XCODE                  4      5
     D  X2AMT                  6     16  2
     D*
     D* Key for Fund year / code array
     D                 DS
     D  XXKEY                  1      5
     D  XXYR                   1      3
     D  XXCD                   4      5
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    EAPODT        XXDATE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    X@CO#             3
     C                   PARM                    X@USER           10
     C                   PARM                    X@TIME            6
     C*
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** PARM           X@DATE  6
     C                   PARM                    X@DATE            8
     C*** END   CHG *** 01/09/98 **************************************
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
     C                   PARM      '*MDYY'       SRTFMT
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
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
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
     C                   KFLD                    X@USER
     C*
     C*   KEY LIST FOR A/R FILE - WITH TIME.
     C*
     C     EAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X@USER
     C                   KFLD                    XXDATE
     C                   KFLD                    UETIME
     C*
     C*   KEY LIST FOR A/R FILE - WITHOUT TIME.
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X@USER
     C                   KFLD                    XXDATE
     C*
     C*   KEY LIST FOR EMPLOYER USER FILE.
     C*
     C     UEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X@USER
     C                   KFLD                    XXFILE
     C*** BEGIN ADD *** 04/17/98 **************************************
     C*
     C*   KEY LIST FOR EMPLOYER MASTER FILE.
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C*** END   ADD *** 04/17/98 **************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVE      X@CO#         XXCO#             3 0
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** MOVE X@DATE    QQDATE  60
     C                   MOVE      X@DATE        QQDATE            8 0
     C*** END   CHG *** 01/09/98 **************************************
     C*
     C*** BEGIN ADD *** 01/09/98 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XXSYDT            8 0
     C*** END   ADD *** 01/09/98 **************************************
     C                   Z-ADD     QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVEL     'WMEAP'       XXFILE           10
     C*
     C* Initialize A/R Summary Fields                      LJQ 9-21-88
     C*                                                          "
     C                   MOVEA     *BLANKS       XAMT                                 "
     C                   MOVEA     *BLANKS       XCOD                                 "
     C                   MOVEA     *BLANKS       COMB                                 "
     C*
     C*   GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
      *
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   EXCEPT    HEAD
     C*
     C*   GET THE TIME TO START PROCESSING THE TRANSACTION FILE.
     C*
     C     UEKEY         CHAIN     PUE01                              59
     C   59              Z-ADD     *ZERO         UETIME
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C   60              GOTO      EOJ
     C*
     C     TOP           TAG
     C     EATIME        IFGT      UETIME
     C     EATIME        ANDLE     DSTIME
     C*
     C                   Z-ADD     EAPODT        SRDTI
     C*** BEGIN CHG *** 01/09/98 **************************************
     C******************** CALL 'SRCVTDAT'DTMDY
     C******************** MOVELSRDTO     XXPODT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPODT            8 0
     C*** END   CHG *** 01/09/98 **************************************
     C                   Z-ADD     EATRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C*
     C*  TEST TRANSACTION CODE VALUE ENTERED.
     C*
     C                   EXSR      ARSUM                                        LJQ 9-21-88
     C*
     C*   PRINT THE LINE.
     C*
     C   22              EXCEPT    HEAD
     C                   SETOFF                                       22
     C**** BEGIN ADD ******* 04/17/98 *********************************
     C*
     C* GET EMPLOYER NAME TO PRINT IN OUTPUT
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C**** END   ADD ******* 04/17/98 *********************************
     C                   EXCEPT    DET
     C   OF              SETON                                        22
     C                   SETOFF                                       OF
     C*
     C*   ADD TO THE TOTALS.
     C*
     C                   ADD       EAAMT         XXAMT            11 2
     C                   ADD       1             XXRCDS            5 0
     C*
     C                   END
     C     EAKEY2        READE     PEA01                                  60
     C  N60              GOTO      TOP
     C*
     C*  PRINT THE FINAL TOTALS AND UPDATE THE WORKER'S COMPENSATION
     C*  USER FILE WITH THE "LAST EDIT'S ENDING TIME".
     C*
     C     END           TAG
     C*
     C   22              EXCEPT    HEAD
     C                   SETOFF                                       22
     C*
     C     EOJ           TAG
     C                   EXCEPT    TOTL
     C                   EXSR      ARDETL                                       LJQ 9-21-88
     C                   MOVE      DSTIME        UETIME
     C*
     C     *IN59         IFEQ      '1'
     C                   MOVE      X@CO#         UECO#
     C                   MOVE      X@USER        UEUSER
     C                   MOVE      XXFILE        UEFILE
     C                   MOVE      *BLANK        UESTAT
     C                   WRITE     PUE01
     C*
     C                   ELSE
     C                   UPDATE    PUE01
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
     C*
     C*
     C*
     C*================================================================
     C* SUBROUTINE    **  ARSUM  **  LOAD A/R TYPES INTO ARRAYS       *
     C*================================================================
     C*                                                    LJQ 9-21-88
     CSR   ARSUM         BEGSR
     C*
     C*
     C*  CHECK TO SEE IF CODE IS ALREADY IN ARRAY
     C*
     C                   Z-ADD     1             V                 5 0
     C                   MOVE      '1'           *IN33
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVE      EAFYR         XXYR
     C                   MOVE      EAARCD        XXCD
     C     XXKEY         LOOKUP    XCOD(V)                                33
     C*
     C     *IN33         IFEQ      '0'                                          New code ADD
     C                   Z-ADD     1             V
     C*
     C*  FIND NEXT AVAILABLE POSITION
     C*
     C     XCOD(V)       DOWNE     *BLANKS
     C                   ADD       1             V
     C                   END
     C*
     C*  ADD FIELDS TO THE ARRAY
     C*
     C                   Z-ADD     EAAMT         X2AMT
     C                   MOVE      *BLANKS       X1AMT
     C*
     C                   MOVEA     XXKEY         XCOD(V)
     C                   MOVE      X2AMT         X1AMT            11
     C                   MOVEA     X1AMT         XAMT(V)
     C*
     C*   IF CODE IS IN ARRAY ADD TO EXISTING TOTALS
     C*
     C                   ELSE
     C*
     C                   Z-ADD     0             X2AMT
     C                   MOVE      *BLANKS       X1AMT
     C                   MOVEA     XAMT(V)       X1AMT
     C                   MOVE      X1AMT         X2AMT
     C                   ADD       EAAMT         X2AMT
     C                   MOVE      X2AMT         X1AMT
     C*
     C                   MOVEA     X1AMT         XAMT(V)
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*==================================================================*
     C*   PRINT A/R CODE SUMMARY               Subroutine  ** ARDETL  =  *
     C*==================================================================*
     C*
     CSR   ARDETL        BEGSR
     C*
     C* MOVE CODE AND TOTAL ARRAYS TO COMBINATION ARRAY
     C*
     C                   Z-ADD     1             V
     C                   Z-ADD     0             XXTTL            11 2
     C*
     C     XCOD(V)       DOWNE     *BLANKS
     C                   MOVEL     XCOD(V)       COMB(V)
     C                   MOVE      XAMT(V)       COMB(V)
     C                   ADD       1             V
     C                   END
     C*
     C* IF THERE ARE RECORDS IN THE FILE CONTINUE.
     C*
     C     V             IFNE      1
     C*
     C* PRINT SUMMARY HEADING.                                         DES
     C*
     C                   EXCEPT    ARHEAD
     C*
     C*  SORT THE ARRAY
     C*
     C                   SORTA     COMB
     C*
     C*  FIND THE FIRST RECORD
     C*
     C                   Z-ADD     1             X1                5 0
     C     COMB(X1)      DOWEQ     *BLANKS
     C                   ADD       1             X1
     C                   END
     C*
     C     X1            DOWLE     999
     C                   MOVE      *BLANKS       XCOMB
     C                   MOVEA     COMB(X1)      XCOMB
     C*
     C*  PRINT DETAIL LINE
     C*
     C*
     C     *INOF         IFEQ      '1'
     C                   EXCEPT    ARHEAD
     C                   MOVE      '0'           *INOF
     C                   END
     C*
     C                   EXCEPT    ARDTL
     C                   ADD       1             X1
     C                   ADD       X2AMT         XXTTL
     C                   END
     C*
     C                   EXCEPT    ARTTL
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
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
     O**********             UPUSNM              70
     O**********E            ROUTE          1
     O**********E            ROUTE          1
     O**********E            ROUTE          2
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              70
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
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***                                46 'VE DATA SOLUTIONS. INC,'
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *****END   CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O*** BEGIN CHG *** 04/17/98 **************************************
     O*********************    CONAME    85
     O                       CONAME              96
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ DSDATEY  132
     O*********************    XXSYDT   132 '  /  /    '
     O                       XXSYDT             145 '  /  /    '
     O*** END   CHG *** 01/09/98 **************************************
     O          E            HEAD           2
     O                                            6 'W7210R'
     O*** BEGIN CHG *** 04/17/98 **************************************
     O************             ORTITL    96
     O************                      126 'PAGE:'
     O************             PAGE  Z  132
     O                       ORTITL             101
     O                                          139 'PAGE:'
     O                       PAGE          Z    145
     O*** END   CHG *** 04/17/98 **************************************
     O          E            HEAD           2
     O                                           11 'PROCESSOR:'
     O                       UPUSR               22
     O                       UPUSNM              73
     O                                           84 'TRANSACTION DATE:'
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ DSDATEY   93
     O                       XXSYDT              95 '  /  /    '
     O*** END   CHG *** 01/09/98 **************************************
     O          E            HEAD           1
     O*** BEGIN CHG *** 01/09/98 **************************************
     O*********************************** 4 'FUND'
     O                                            3 'GRP'
     O*** END   CHG *** 01/09/98 **************************************
     O                                            7 'YR'
     O*** BEGIN CHG *** 01/09/98 **************************************
     O********************************** 17 'EMPLOYER'
     O********************************** 23 'DIV'
     O********************************** 34 'POST DATE'
     O********************************** 40 'CODE'
     O********************************** 45 'TYPE'
     O********************************** 54 'CHECK #'
     O********************************** 69 'DESCRIPTION'
     O**********************************100 'AMOUNT'
     O                                           18 'EMPLOYER'
     O                                           25 'DIV'
     O***** BEGIN CHG ***** 04/17/98 **********************************
     O*************                      36 'POST DATE'
     O*************                      42 'CODE'
     O*************                      47 'TYPE'
     O*************                      56 'CHECK #'
     O*************                      71 'DESCRIPTION'
     O*************                     115 'AMOUNT'
     O***** BEGIN ADD ***** 04/17/98 **********************************
     O                                           41 'EMPLOYER NAME'
     O***** END   ADD ***** 04/17/98 **********************************
     O                                           78 'POST DATE'
     O                                           84 'CODE'
     O                                           89 'TYPE'
     O                                           98 'CHECK #'
     O                                          110 'DESCRIPTION'
     O                                          135 'AMOUNT'
     O***** END   CHG ***** 04/17/98 **********************************
     O*** END   CHG *** 01/09/98 **************************************
     O          E            DET         1
     O                       EAFND         ZB     3
     O                       EAFYR         ZB     7
     O                       EAEMP#        ZB    18
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ EADIV ZB  29
     O************************ XXPODT B  39 '  /  /  '
     O************************ EAARCD B  45
     O************************ EAARTY B  49
     O************************ EACHK#ZB  60
     O************************ EADESC B  94
     O************************ EAAMT A  108
     O                       EADIV         ZB    25
     O***** BEGIN CHG ******** 04/17/98 *******************************
     O*********************    XXPODT B  36 '  /  /    '
     O*********************    EAARCD B  41
     O*********************    EAARTY B  46
     O*********************    EACHK#ZB  56
     O*********************    EADESC B  90
     O*********************    EAAMT A  117
     O***** BEGIN ADD ******** 04/17/98 *******************************
     O                       EMNAME              68
     O***** END   ADD ******** 04/17/98 *******************************
     O                       XXPODT         B    79 '  /  /    '
     O                       EAARCD         B    84
     O                       EAARTY         B    89
     O                       EACHK#        ZB    98
     O                       EADESC         B   129
     O                       EAAMT         A    141
     O***** END   CHG ******** 04/17/98 *******************************
     O*** END   CHG *** 01/09/98 **************************************
     O          E            TOTL        1  1
     O                       XXRCDS        ZB    77
     O                                           84 'TOTAL:'
     O                       XXAMT         BB   117
     O*
     O* A/R SUMMARY HEADING                                 LJQ 9-21-88
     O*
     O          E            ARHEAD         1 06
     O                       DSTIME              08 '  :  :  '
     O*** BEGIN CHG *** 04/17/98 **************************************
     O***********************  CONAME    85
     O                       CONAME              96
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ DSDATEY  132
     O*************************XXSYDT   132 '  /  /    '
     O                       XXSYDT             145 '  /  /    '
     O*** END   CHG *** 01/09/98 **************************************
     O*** END   CHG *** 04/17/98 **************************************
     O          E            ARHEAD         2
     O                                            6 'W7210R'
     O***** BEGIN CHG ***** 04/17/98 **********************************
     O*************            ORTITL    96
     O*************                     126 'PAGE:'
     O*************            PAGE  Z  132
     O                       ORTITL             101
     O                                          139 'PAGE:'
     O                       PAGE          Z    145
     O***** BEGIN CHG ***** 04/17/98 **********************************
     O          E            ARHEAD         2
     O                                           11 'PROCESSOR:'
     O                       UPUSR               22
     O                       UPUSNM              73
     O                                           84 'TRANSACTION DATE:'
     O*** BEGIN CHG *** 01/09/98 **************************************
     O************************ DSDATEY   93
     O                       XXSYDT              95 '  /  /    '
     O*** END   CHG *** 01/09/98 **************************************
     O          E            ARHEAD         2
     O                                           51 'A/R CODE SUMMARY'
     O          E            ARHEAD         2
     O*** BEGIN CHG *** 01/09/98 **************************************
     O********************************** 20 'FUND YEAR'
     O                                           20 'GROUP YEAR'
     O*** END   CHG *** 01/09/98 **************************************
     O                                           40 'A/R CODE'
     O                                           66 'AMOUNT'
     O*
     O* A/R SUMMARY DETAIL                                  LJQ 9-21-88
     O*
     O          E            ARDTL          1
     O                       XYEAR         Z     16
     O                       XCODE               37
     O                       X2AMT         B     68
     O*
     O* A/R SUMMARY TOTAL                                   LJQ 9-21-88
     O*
     O          E            ARTTL       2  2
     O                                           49 'TOTAL:'
     O                       XXTTL         B     68
