     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7590R - EMPLOYER PAYROLL EDIT (ALL PAYROLLS)
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
     H*  04/17/98  C:78  PROJECT: WC577                               *
     H*                  EMPLOYER NAME ADDED TO OUTPUT REPORT         *
     H*
     H*  12/22/97  C:33  PROJECT: WC577                               *
     H*                  V3 CHANGES                                   *
     H*****************************************************************
     FWMEPL5    IF   E           K DISK
     FWXUEL     UF A E           K DISK
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     F***** BEGIN ADD ***** 04/17/98 **********************************
     FWMEML     IF   E           K DISK
     F***** END   ADD ***** 04/17/98 **********************************
     F***** BEGIN CHG ***** 04/17/98 **********************************
     F****QSYSPRT O   F     133     OA     PRINTER
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)
     F***** END   CHG ***** 04/17/98 **********************************

     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    EPDT          XXDATE
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
     C*   KEY LIST FOR EMPLOYER PAYROLL FILE - WITHOUT TIME.
     C*
     C     EPKEY2        KLIST
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
     C****** BEGIN ADD ***** 04/17/98 *********************************
     C*
     C*   KEY LIST FOR EMPLOYER MASTER FILE.
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EPFND
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C****** END   ADD ***** 04/17/98 *********************************
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
     C                   MOVEL     'WMEPP'       XXFILE           10
     C*
     C*   GET THE COMPANY FILE.
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
     C     EPKEY2        CHAIN     PEP01                              60
     C     *IN60         DOWEQ     '0'
     C     EPTIME        IFGT      UETIME
     C     EPTIME        ANDLE     XXTIME
     C*
     C****** BEGIN ADD ****** 04/17/98 ********************************
     C* GET EMPLOYER NAME
     C     EMKEY         CHAIN     PEM01                              60
     C****** END   ADD ****** 04/17/98 ********************************
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C***  BEGIN CHG  ***  12/22/97  *********************************
     C**********           CALL 'SRCVTDAT'DTMDY
     C**********           MOVELSRDTO     XXEFDT  60
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C***  END   CHG  ***  12/22/97  *********************************
     C*
     C*   PRINT THE LINE.
     C*
     C   OA              EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   EXCEPT    DET
     C*
     C                   END
     C*
     C     EPKEY2        READE     PEP01                                  60
     C                   END
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
      ****** BEG ADD ***** 01/28/10 **********************************
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O****    E   60           ROUTE
     O****                               23 '(C) COPYRIGHT PROGRESSI'
      ****** END DLT ***** 01/28/10 **********************************
     O***  BEGIN CHG  ***  12/22/97  *********************************
     O**********                         46 'VE DATA SOLUTIONS. INC,'
     O**********                         51 '.1990'
      ****** BEG DLT ***** 01/28/10 **********************************
     O****                               47 'VE DATA SOLUTIONS. INC,.'
     O****                     *YEAR     51
      ****** END DLT ***** 01/28/10 **********************************
     O***  END   CHG  ***  12/22/97  *********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O***** BEGIN CHG **** 04/17/98 ***********************************
     O********************     CONAME    87
     O********************     DSDATEY  132
     O                       CONAME             107
     O                       DSDATE        Y    198
     O***** END   CHG **** 04/17/98 ***********************************
     O          E            HEAD           2
     O                                            6 'W7590R'
     O***** BEGIN CHG **** 04/17/98 ***********************************
     O**********************   ORTITL    96
     O**********************            126 'PAGE:'
     O**********************   PAGE  Z  132
     O                       ORTITL             118
     O                                          192 'PAGE:'
     O                       PAGE          Z    198
     O***** END   CHG **** 04/17/98 ***********************************
     O          E            HEAD           1
     O**** BEGIN CHG ***** 04/17/98 **********************************
     O***  BEGIN CHG  ***  12/22/97  *********************************
     O**********                         34 'SPEC'
     O**********                         58 'SELF-RPT'
     O**********                         72 'EST'
     O**********                         91 'AUDITED'
     O**********                        105 'SELF-RPT/'
     O**********                        122 'ADDL'
     O**********************             36 'SPEC'
     O**********************             64 'SELF-RPT'
     O**********************             79 'EST'
     O**********************             96 'AUDITED'
     O**********************            108 'SELF-RPT/'
     O**********************            132 'ADDL'
     O                                           79 'SPEC'
     O                                          107 'SELF-RPT'
     O                                          118 'EST'
     O                                          138 'AUDITED'
     O                                          149 'SELF-RPT/'
     O                                          174 'ADDL'
     O***  END   CHG  ***  12/22/97  *********************************
     O**** END   CHG ***** 04/17/98 **********************************
     O          E            HEAD           2
     O***  BEGIN CHG  ***  12/22/97  *********************************
     O**********                          4 'FUND'
     O**********                          8 'YR'
     O**********                         17 'EMPLOYER'
     O**********                         21 'DIV'
     O**********                         28 'CLASS'
     O**********                         34 'DES'
     O**********                         44 'EFF DATE'
     O**********                         57 'PAYROLL'
     O**********                         74 'PAYROLL'
     O**********                         91 'PAYROLL'
     O**********                        107 'ANNUAL FLAG'
     O**********                        113 'STAT'
     O**********                        122 'LOAD'
     O                                            4 'GRP'
     O                                            8 'YR'
     O                                           20 'EMPLOYER'
     O                                           25 'DIV'
     O**** BEGIN ADD ***** 04/17/98 **********************************
     O                                           39 'EMPLOYER NAME'
     O**** END   ADD ***** 04/17/98 **********************************
     O**** BEGIN CHG ***** 04/17/98 **********************************
     O*********************              32 'CLASS'
     O*********************              36 'DES'
     O*********************              49 'EFFECT DATE'
     O*********************              64 'PAYROLL'
     O*********************              80 'PAYROLL'
     O*********************              96 'PAYROLL'
     O*********************             109 'ANNUAL FLAG'
     O*********************             116 'STAT'
     O*********************             132 'LOAD'
     O                                           74 'CLASS'
     O                                           78 'DES'
     O                                           91 'EFFECT DATE'
     O                                          106 'PAYROLL'
     O                                          122 'PAYROLL'
     O                                          138 'PAYROLL'
     O                                          151 'ANNUAL FLAG'
     O                                          158 'STAT'
     O                                          174 'LOAD'
     O**** END   CHG ***** 04/17/98 **********************************
     O***  END   CHG  ***  12/22/97  *********************************
     O*
     O          E            DET            1
     O                       EPFND         Z      4
     O                       EPFYR         Z      8
     O                       EPEMP#        Z     19
     O**** BEGIN CHG ***** 12/22/97 **********************************
     O**********               EPDIV Z   27
     O**********               EPCLASZ   34
     O**********               EPSPEC    39
     O**********               XXEFDT    50 '  /  /  '
     O**********               EPSAPRJ   67
     O**********               EPEAPRJ   84
     O**********               EPAAPRJ  101
     O**********               EPSAFL   108
     O**********               EPSTAT   118
     O                       EPDIV         Z     25
     O**** BEGIN ADD ***** 04/17/98 ***********************************
     O                       EMNAME              66
     O**** END   ADD ***** 04/17/98 ***********************************
     O**** BEGIN CHG ***** 04/17/98 ***********************************
     O**********               EPCLASZ   32
     O**********               EPSPEC    35
     O**********               XXEFDT    48 '  /  /    '
     O**********               EPSAPRJ   65
     O**********               EPEAPRJ   81
     O**********               EPAAPRJ   97
     O************             EPSAFL   104
     O************             EPSTAT   115
     O***  END   CHG  ***  12/22/97  *********************************
     O************             EPADLDJ  133
     O                       EPCLAS        Z     74
     O                       EPSPEC              77
     O                       XXEFDT              90 '  /  /    '
     O                       EPSAPR        J    107
     O                       EPEAPR        J    123
     O                       EPAAPR        J    139
     O                       EPSAFL             146
     O                       EPSTAT             157
     O                       EPADLD        J    175
     O**** END   CHG ***** 04/17/98 ***********************************
