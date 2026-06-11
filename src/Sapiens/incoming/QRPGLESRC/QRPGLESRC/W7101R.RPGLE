     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7101R - EMPLOYEE PAYROLL EDIT
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  12/22/97  C:33  PROJECT: WC577                               *
     H*                  V3 CHANGES                                   *
     H*****************************************************************
     FWASRL1    IF   E           K DISK
     FWXUEL     UF A E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    SREFDT        XXDATE
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQUSR            10
     C                   PARM                    QQTIME            6
     C***  BEGIN CHG  ***  12/22/97  *********************************
     C**********           PARM           QQDATE  6
     C                   PARM                    QQDATE            8
     C***  END   CHG  ***  12/22/97  *********************************
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
     C*   KEY LIST FOR THE COMPANY FILE.
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE USER PARAMETERS FILE.
     C*
     C     UPKEY         KLIST
     C                   KFLD                    XXUSR
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE.
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR EMPLOYER PAYROLL FILE - WITH TIME.
     C*
     C     SRKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXUSR
     C                   KFLD                    XXDATE
     C                   KFLD                    UETIME
     C*
     C*   KEY LIST FOR EMPLOYER PAYROLL FILE - WITHOUT TIME.
     C*
     C     SRKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXUSR
     C                   KFLD                    XXDATE
     C*
     C*   KEY LIST FOR EMPLOYER USER FILE.
     C*
     C     UEKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXUSR
     C                   KFLD                    XXFILE
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQUSR         XXUSR            10
     C                   MOVE      QQTIME        XXTIME            6 0
     C***  BEGIN CHG  ***  12/22/97  *********************************
     C**********           MOVE QQDATE    X1DATE  60
     C                   MOVE      QQDATE        X1DATE            8 0
     C***  END   CHG  ***  12/22/97  *********************************
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   TIME                    DSDTTM
     C                   MOVEL     'WASRP'       XXFILE           10
     C*
     C*   GET THE COMPANY FILE.
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
     C*   GET THE DISTRIBUTION INFORMATION.
     C*
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
     C*
     C     *IN60         IFEQ      '0'
     C     ORSTAT        ANDEQ     'A'
     C*
     C*   GET THE USER PARAMETERS FILE.
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        UPUSNM
     C                   END
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C*
     C                   END
     C*
     C*   PRINT THE REPORT HEADINGS.
     C*
     C                   MOVE      '1'           *INOA
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C*
     C*   GET THE TIME TO START PROCESSING THE TRANSACTION FILE.
     C*
     C     UEKEY         CHAIN     PUE01                              59
     C     *IN59         IFEQ      '1'
     C                   MOVE      *ZERO         UETIME
     C                   END
     C*
     C     SRKEY2        CHAIN     PSR01                              60
     C     *IN60         DOWEQ     '0'
     C     SRTIME        IFGT      UETIME
     C     SRTIME        ANDLE     XXTIME
     C*
     C                   Z-ADD     SREFDT        SRDTI
     C***  BEGIN CHG  ***  12/22/97  *********************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXEFDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C***  END   CHG  ***  12/22/97  *********************************
     C                   Z-ADD     SRCADT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCADT            6 0
     C***  BEGIN CHG  ***  12/22/97  *********************************
     C**********           MOVELSRRPMY    XXYY    20
     C**********           MOVE SRRPMY    XXMM    20
     C**********           MOVELXXMM      SRRPMY
     C**********           MOVE XXYY      SRRPMY
     C                   Z-ADD     SRRPMY        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY
     C                   MOVEL     SRDTO         XXRPMY            6 0
     C***  END   CHG  ***  12/22/97  *********************************
     C*
     C*   PRINT THE LINE.
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DET
     C*
     C*   ADD TO THE TOTALS.
     C*
     C                   ADD       SRORGP        XXFNOR           11 2
     C                   ADD       SRAMTP        XXFNAM           11 2
     C                   ADD       1             XXFNIM            5 0
     C*
     C                   END
     C*
     C     SRKEY2        READE     PSR01                                  60
     C                   END
     C*
     C*  PRINT THE FINAL TOTALS AND UPDATE THE WORKER'S COMPENSATION USER
     C*  FILE WITH THE "LAST EDIT'S ENDING TIME".
     C*
     C     EOJ           TAG
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    FNTOT
     C*
     C     *IN59         IFEQ      '1'
     C                   MOVE      QQCO#         UECO#
     C                   MOVE      QQUSR         UEUSER
     C                   MOVE      XXFILE        UEFILE
     C                   MOVE      *BLANK        UESTAT
     C                   END
     C                   MOVE      XXTIME        UETIME
     C*
     C     *IN59         IFEQ      '0'
     C                   UPDATE    PUE01
     C                   ELSE
     C                   WRITE     PUE01
     C                   END
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain ORKEY POR01;
       If %found and ORSTAT = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      '1'           *INLR
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
     O**********E            ROUTE          3
     O**********                                 15 'ROUTE TO:'
     O**********             UPUSNM              70
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          3
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
     O***  BEGIN DLT  ***  01/28/10  *********************************
     O***     E   60           ROUTE
     O***                                23 '(C) COPYRIGHT PROGRESSI'
     O***  BEGIN CHG  ***  12/22/97  *********************************
     O**********                         46 'VE DATA SOLUTIONS. INC,'
     O**********                         51 '.1990'
     O***                                47 'VE DATA SOLUTIONS. INC,.'
     O***                      *YEAR     51
     O***  END DLT    ***  01/28/10  *********************************
     O***  END   CHG  ***  12/22/97  *********************************
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                                            6 'W7101R'
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O***  BEGIN CHG  ***  12/22/97  *********************************
     O**********                          4 'FUND'
     O**********                          7 'YR'
     O**********                         16 'EMPLOYER'
     O**********                         20 'DIV'
     O**********                         26 'CLASS'
     O**********                         35 'EFF DATE'
     O**********                         44 'RPT M/Y'
     O**********                         48 'SUB'
     O**********                         65 'ORIGINAL AMT'
     O**********                         82 'REPORTED AMT'
     O                                            4 'GRP'
     O                                            7 'YR'
     O                                           20 'EMPLOYER'
     O                                           26 'DIV'
     O                                           32 'CLASS'
     O                                           44 'EFFECT DATE'
     O                                           55 'RPT M/YY'
     O                                           59 'SUB'
     O                                           74 'ORIGINAL AMT'
     O                                           91 'REPORTED AMT'
     O***  END   CHG  ***  12/22/97  *********************************
     O          E            DET            1
     O                       SRFND         Z      4
     O                       SRFYR         Z      7
     O                       SREMP#        Z     20
     O                       SRDIV         Z     26
     O                       SRCLAS        Z     32
     O                       SRSPEC              33
     O***  BEGIN CHG  ***  12/22/97  *********************************
     O**********               XXEFDT    42 '  /  /  '
     O**********               SRRPMY    51 '   /  '
     O**********               SRSC      55
     O**********               SRORGPA   72
     O**********               SRAMTPA   89
     O                       XXEFDT              44 '  /  /    '
     O                       XXRPMY              55 '  /    '
     O                       SRSC                58
     O                       SRORGP        A     76
     O                       SRAMTP        A     93
     O***  END   CHG  ***  12/22/97  *********************************
     O          E            FNTOT       1  2
     O                                           22 'FINAL TOTAL:'
     O                       XXFNIM        1     29
     O                                           35 'ITEMS'
     O***  BEGIN CHG  ***  12/22/97  *********************************
     O**********               XXFNORA   65
     O**********               XXFNAMA   82
     O                       XXFNOR        A     76
     O                       XXFNAM        A     93
     O***  END   CHG  ***  12/22/97  *********************************
