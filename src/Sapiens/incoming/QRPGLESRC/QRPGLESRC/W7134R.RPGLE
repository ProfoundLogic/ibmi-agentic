     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7134R - UNDERWRITERS REPORT ACCIDENT DATE SELECTION
     H*****************************************************************
     H** REVISIONS:                                                  **
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
     H*** 09/26/91*** TAA *** PER         *** PROJ #: WC073 ***********
     H**  1.  CORRECTED REPORT PROBLEM ASSOCIATED WITH DATE RECEIVED **
     H*                                                               *
     H* 05/25/07 C:191 PROJECT: WC1515                                *
     H*                 Check claims authority                        *
     H*                 for no-auth, do not include claimant name     *
     H*                                                               *
     H* 04/10/00 C:118 PROJECT: WC987                                 *
     H*                LOAD ALTERNATE COMPANY NAME FROM FUND MASTER   *
     H*                IF AVAILABLE.                                  *
     H*                                                               *
     H* 12/16/99 C:90 PROJECT: WC577                                  *
     H*               Y2K FIX - CORRECTED POLICY YR FORMAT, CHG 'FUND'*
     H*               TO 'GROUP', MOVED REC. DATE, LEFT JUSTIFIED     *
     H*               TITLE ON ROUTING PAGE.                          *
     H*                                                               *
     H* 02/11/99 C:90 PROJECT WA034/16348                             *
     H*               CHANGED ACCDTE AND RECDTE DATE FORMAT FROM      *
     H*               MDYY TO MDY TO FIT XX/XX/XX OUTPUT FORMAT.      *
     H*                                                               *
     H* 08/12/97 C:78 PROJECT WC577                                   *
     H*               FIELDS SVEMP# AND SVDIV FORCED INTO SMALLER     *
     H*               FIELDS FOR OUTPUT PURPOSES.                     *
     H*                                                               *
     H* 08/27/97 C:78 PROJECT WC577                                   *
     H*               OUTPUT CHANGED TO MEET NEW STANDARDS            *
     H*                                                               *
     H*****************************************************************
     F*
     F* * * CLAIMS LOGICAL FILE
     FWMCML6    IF   E           K DISK
     F*
     F* * * CLAIMANT DEMOGRAPHICS FILE
     FWMCDL     IF   E           K DISK
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
     F* * * FUND DETAIL FILE
     FWDF1L     IF   E           K DISK
     F*
     F* * * CLAIM HISTORY FILE
     FWMCHL     IF   E           K DISK
     F*
     F* * * CLAIM HISTORY FILE
     FWTABL     IF   E           K DISK
     F*
     F* * * USER PARAMETER FILE
     FWMUPL     IF   E           K DISK
     F*
     F* * * Output File
     FQSYSPRT   O    F  196        PRINTER OFLIND(*INOF)
     F*

     D* * * NATURE, PART, CAUSE ARRAYS
     D NA              S              2    DIM(9)                               NATURE CODES
     D NAD             S             30    DIM(9)                               NATURE DESC.
     D PA              S              2    DIM(9)                               PART CODES
     D PAD             S             30    DIM(9)                               PART DESC.
     D CA              S              2    DIM(9)                               CAUSE CODES
     D CAD             S             30    DIM(9)                               CAUSE DESC.
     D*
     D                 DS
     D  CMT                    1     20
     D                                     DIM(10)                              RESERVE TYPES
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  CMA                    1     90  2
     D                                     DIM(10)                              PMT AMOUNT
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  CMR                    1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D                 DS
     D  DSDATE                 1      6  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** BEG ADD ****** 05/25/07 **********************************
     D XXATTX          C                   CONST('*NO AUTHORITY*')
      *** END ADD ****** 05/25/07 **********************************

     I*
     I*

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
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    F1FYR
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#             9
     C*
     C*   KEY LIST FOR THE HISTORY FILE
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    FNFND
     C                   KFLD                    F1FYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXDATE
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRES            10
     C*
     C     *LIKE         DEFINE    CHEFDT        XXDATE
     C     *LIKE         DEFINE    CHEFDT        XXFRDT
     C     *LIKE         DEFINE    CHEFDT        XXTODT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDTE             8
     C                   PARM                    QQDT2             8
     C                   PARM                    QQDT3             8
     C                   PARM                    QQQUE            10            MLS 1/15/88
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
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*** BEG ADD ****** 05/25/07 ********************************************
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C*** END ADD ****** 05/25/07 ********************************************
     C**
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQDTE         X1DATE            8 0
     C                   MOVEL     QQDTE         DSDATE
     C                   MOVE      QQMODE        XXMODE            1
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*  QQDT2, QQDT3 ARE THE DATES USED FOR SELECTION
     C*
     C                   MOVE      QQDT2         X2FRDT            8 0
     C                   MOVE      QQDT3         X2TODT            8 0
     C*
     C                   Z-ADD     X2FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   Z-ADD     X2TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      QQDT2         X3FRDT            8 0
     C                   Z-ADD     X3FRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1FRDT            6 0
     C*
     C                   MOVE      QQDT3         X3TODT            8 0
     C                   Z-ADD     X3TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X1TODT            6 0
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C  N11              EXSR      SR1ST
     C*
     C*  GET FUND DESCRIPTION
     C*
     C     XXFND         IFEQ      *ZERO
     C     XXCO#         SETLL     WMFNL
     C     TAG03         TAG
     C                   MOVE      '1'           *IN30
     C     XXCO#         READE     WMFNL                                  60
     C                   MOVEL     FNNAME        XFNAME           60
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C***** END   ADD **** 04/10/00 ***********************************
     C     *IN60         IFEQ      '1'
     C                   SETON                                        LR
     C                   GOTO      END
     C                   END
     C                   ELSE
     C     FNKEY         CHAIN     WMFNL                              60
     C     *IN60         IFEQ      '0'
     C                   MOVEL     FNNAME        XFNAME           60
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C***** END   ADD **** 04/10/00 ***********************************
     C                   ELSE
     C                   MOVE      *BLANKS       XFNAME           60
     C                   END
     C                   END
     C*
     C***** BEGIN ADD **** 04/10/00 ***********************************
     C*
     C*  CENTER COMPANY NAME IN FIELD
     C*
     C                   MOVEL     CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        CONAME
     C***** END   ADD **** 04/10/00 ***********************************
     C*
     C*  READ FUND DETAIL FILE
     C*
     C     F1KEY         SETLL     PF101
     C     TAG01         TAG
     C     F1KEY         READE     PF101                                  60
     C     *IN60         IFEQ      '1'
     C     *IN30         IFEQ      '1'
     C                   GOTO      TAG03
     C                   ELSE
     C                   SETON                                        LR
     C                   GOTO      END
     C                   END
     C                   END
     C                   MOVE      F1FYR         XXYR              2 0
     C                   MOVEL     F1FYR         XX1               1 0
     C     XX1           IFEQ      *ZERO
     C                   MOVE      '19'          XXCENT            2 0
     C                   END
     C                   MOVE      *BLANK        HPART
     C                   MOVE      *BLANK        HNATR
     C**
     C*  READ CLAIM FILE
     C*
     C     CMKEY         SETLL     WMCML6                                 67
     C     *IN67         IFNE      '1'
     C                   GOTO      TAG01
     C                   END
     C                   MOVE      'N'           XXPRNT            1            JJJ 11-30-89
     C     TAG02         TAG
     C     CMKEY         READE     WMCML6                                 66
     C**
     C*  THE ACCIDENT DATE MUST BE GREATER THAN OR EQUAL TO THE
     C*  FROM DATE AND LESS THAN OR EUQL TO THE TO DATE, EVERYTHING
     C*  ELSE GOES BACK TO THE TOP
     C*
     C**
     C     *IN66         IFEQ      '0'
     C     CMACDT        CABLT     XXFRDT        TAG02
     C     CMACDT        CABGT     XXTODT        TAG02
     C                   END
     C**
     C     *IN66         IFEQ      '1'
     C     XXPRNT        IFEQ      'Y'
     C                   MOVE      '1'           *IN22
     C                   ADD       L1COMP        L2COMP            9 2
     C                   ADD       L1MED         L2MED             9 2
     C                   ADD       L1OTHR        L2OTHR            9 2
     C                   ADD       L1TOTL        L2TOTL            9 2
     C                   ADD       L1TINC        L2TINC            9 2
     C                   ADD       L1ORES        L2ORES            9 2
     C     L1OPN         ADD       L1CLS         L1TCAS            5 0
     C                   ADD       L1TCAS        L2TCAS            5 0
     C                   ADD       L1OPN         L2OPN             5 0
     C                   ADD       L1CLS         L2CLS             5 0
     C     *INOF         IFEQ      '1'
     C                   EXSR      SUB01
     C                   EXCEPT    HEAD
     C                   END
     C                   EXCEPT    TOTL1
     C                   Z-ADD     *ZERO         L1OPN
     C                   Z-ADD     *ZERO         L1CLS
     C                   Z-ADD     *ZERO         L1TCAS
     C     *INOF         IFEQ      '1'
     C                   EXSR      SUB01
     C                   EXCEPT    HEAD
     C                   END
     C**
     C                   ADD       L2COMP        L3COMP           11 2
     C                   ADD       L2MED         L3MED            11 2
     C                   ADD       L2OTHR        L3OTHR           11 2
     C                   ADD       L2TOTL        L3TOTL           11 2
     C                   ADD       L2TINC        L3TINC           11 2
     C                   ADD       L2ORES        L3ORES           11 2
     C     L2OPN         ADD       L2CLS         L2TCAS            5 0
     C                   ADD       L2TCAS        L3TCAS            5 0
     C                   ADD       L2OPN         L3OPN             5 0
     C                   ADD       L2CLS         L3CLS             5 0
     C                   EXCEPT    TOTL2
     C                   Z-ADD     *ZERO         L2OPN
     C                   Z-ADD     *ZERO         L2CLS
     C                   Z-ADD     *ZERO         L2TCAS
     C     *INOF         IFEQ      '1'
     C                   EXSR      SUB01
     C                   EXCEPT    HEAD
     C                   END
     C**
     C                   MOVE      '1'           *INL3
     C                   ADD       L3COMP        LRCOMP           11 2
     C                   ADD       L3MED         LRMED            11 2
     C                   ADD       L3OTHR        LROTHR           11 2
     C                   ADD       L3TOTL        LRTOTL           11 2
     C                   ADD       L3TINC        LRTINC           11 2
     C                   ADD       L3ORES        LRORES           11 2
     C     L3OPN         ADD       L3CLS         L3TCAS            5 0
     C                   ADD       L3TCAS        LRTCAS            5 0
     C                   ADD       L3OPN         LROPN             5 0
     C                   ADD       L3CLS         LRCLS             5 0
     C                   EXCEPT    TOTL3
     C                   Z-ADD     *ZERO         L3OPN
     C                   Z-ADD     *ZERO         L3CLS
     C                   Z-ADD     *ZERO         L3TCAS
     C**
     C                   EXSR      SUB01
     C                   END                                                    JJJ 11-30-89
     C                   GOTO      TAG01
     C**
     C**
     C                   ELSE
     C**
     C**
     C     CMNATR        IFNE      HNATR
     C     HNATR         ANDNE     *BLANK
     C                   MOVE      '1'           *INL1
     C                   ADD       L1COMP        L2COMP            9 2
     C                   ADD       L1MED         L2MED             9 2
     C                   ADD       L1OTHR        L2OTHR            9 2
     C                   ADD       L1TOTL        L2TOTL            9 2
     C                   ADD       L1TINC        L2TINC            9 2
     C                   ADD       L1ORES        L2ORES            9 2
     C     L1OPN         ADD       L1CLS         L1TCAS            5 0
     C                   ADD       L1TCAS        L2TCAS            5 0
     C                   ADD       L1OPN         L2OPN             5 0
     C                   ADD       L1CLS         L2CLS             5 0
     C     *IN22         IFEQ      '1'
     C     *INOF         OREQ      '1'
     C                   EXSR      SUB01
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *IN22
     C                   END
     C     HPART         IFNE      *BLANKS
     C                   EXCEPT    TOTL1
     C                   END
     C                   Z-ADD     *ZERO         L1OPN
     C                   Z-ADD     *ZERO         L1CLS
     C                   Z-ADD     *ZERO         L1TCAS
     C**
     C                   MOVE      '1'           *INL2
     C                   ADD       L2COMP        L3COMP           11 2
     C                   ADD       L2MED         L3MED            11 2
     C                   ADD       L2OTHR        L3OTHR           11 2
     C                   ADD       L2TOTL        L3TOTL           11 2
     C                   ADD       L2TINC        L3TINC           11 2
     C                   ADD       L2ORES        L3ORES           11 2
     C     L2OPN         ADD       L2CLS         L2TCAS            5 0
     C                   ADD       L2TCAS        L3TCAS            5 0
     C                   ADD       L2OPN         L3OPN             5 0
     C                   ADD       L2CLS         L3CLS             5 0
     C     *IN22         IFEQ      '1'
     C     *INOF         OREQ      '1'
     C                   EXSR      SUB01
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *IN22
     C                   END
     C                   EXCEPT    TOTL2
     C                   Z-ADD     *ZERO         L2OPN
     C                   Z-ADD     *ZERO         L2CLS
     C                   Z-ADD     *ZERO         L2TCAS
     C     *INOF         IFEQ      '1'
     C                   EXSR      SUB01
     C                   EXCEPT    HEAD
     C                   END
     C**
     C                   GOTO      ENDA
     C                   END
     C**
     C     CMPART        IFNE      HPART
     C     HPART         ANDNE     *BLANK
     C                   MOVE      '1'           *INL1
     C                   ADD       L1COMP        L2COMP            9 2
     C                   ADD       L1MED         L2MED             9 2
     C                   ADD       L1OTHR        L2OTHR            9 2
     C                   ADD       L1TOTL        L2TOTL            9 2
     C                   ADD       L1TINC        L2TINC            9 2
     C                   ADD       L1ORES        L2ORES            9 2
     C     L1OPN         ADD       L1CLS         L1TCAS            5 0
     C                   ADD       L1TCAS        L2TCAS            5 0
     C                   ADD       L1OPN         L2OPN             5 0
     C                   ADD       L1CLS         L2CLS             5 0
     C     *IN22         IFEQ      '1'
     C     *INOF         OREQ      '1'
     C                   EXSR      SUB01
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOF
     C                   MOVE      '0'           *IN22
     C                   END
     C                   EXCEPT    TOTL1
     C                   Z-ADD     *ZERO         L1OPN
     C                   Z-ADD     *ZERO         L1CLS
     C                   Z-ADD     *ZERO         L1TCAS
     C     *INOF         IFEQ      '1'
     C                   EXSR      SUB01
     C                   EXCEPT    HEAD
     C                   END
     C                   END
     C**
     C                   END
     C**
     C     ENDA          TAG
     C**
     C*  THE ACCIDENT DATE MUST BE GREATER THAN OR EQUAL TO THE
     C*  FROM DATE AND LESS THAN OR EUQL TO THE TO DATE, EVERYTHING
     C*  ELSE GOES BACK TO THE TOP
     C*
     C**
     C*          CMACDT    CABLTXXFRDT    TAG02
     C*          CMACDT    CABGTXXTODT    TAG02
     C**
     C*** BEG ADD ****** 05/25/07 **********************************************
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *OFF          XXAUTH            1
     C                   MOVE      *BLANKS       XXSS#
     C                   ELSE
     C                   MOVE      *ON           XXAUTH
     C                   MOVE      CMSS#         XXSS#
     C                   ENDIF                                                     RTCD = '2'
     C*** END ADD ****** 05/25/07 **********************************************
      *
     C                   MOVEL     XXSS#         SS4               4
     C                   MOVE      '-'           SS4
     C                   MOVE      XXSS#         SS6               6
     C                   MOVEL     SS6           SS2               2
     C                   MOVE      SS6           SS5               5
     C                   MOVEL     '-'           SS5
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C**** BEGIN CHG **** 02/11/99 ************************************
     C******************** MOVELSRDTO     ACCDTE  60
     C                   MOVEL     SRDTO         XXACDT            8 0
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         ACCDTE            6 0
     C**** END   CHG **** 02/11/99 ************************************
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C**** BEGIN CHG **** 02/11/99 ************************************
     C******************** MOVELSRDTO     RECDTE  60
     C                   MOVEL     SRDTO         XXRCDT            8 0
     C                   Z-ADD     XXRCDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         RECDTE            6 0
     C**** END   CHG **** 02/11/99 ************************************
      **** BEGIN ADD ***** 08/12/97 ***********************************
     C                   Z-ADD     CMEMP#        CMEMP2            5 0
     C                   Z-ADD     CMDIV         CMDIV2            3 0
      **** END   ADD ***** 08/12/97 ***********************************
     C**
     C*  GET CLAIMANT DEMOGRAPHICS FILE
     C*
     C     CDKEY         CHAIN     PCD01                              60
     C**
     C**    CALL PGM TO RE-ARRANGE CLAIMANT NAME
     C**
     C                   MOVEL     CDLSN         QQLAST           56
     C                   MOVEL     CDFSN         QQFRST           15
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL     QQLAST        CNAME
     C*
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        CNAME            15
     C                   END
     C*** BEG ADD ****** 05/25/07 **********************************************
     C     XXAUTH        IFEQ      *OFF
     C                   MOVE      *BLANKS       CNAME
     C                   MOVEL(P)  XXATTX        CNAME
     C                   ENDIF
     C*** END ADD ****** 05/25/07 **********************************************
     C**
     C     CMNATR        LOOKUP    NA                                     31
     C     *IN31         IFEQ      '0'
     C     X             IFLT      9
     C                   ADD       1             X
     C                   MOVEL     CMNATR        NA(X)
     C                   ELSE
     C                   SETON                                        81  88
     C                   END
     C                   END
     C**
     C     CMPART        LOOKUP    PA                                     31
     C     *IN31         IFEQ      '0'
     C     Y             IFLT      9
     C                   ADD       1             Y
     C                   MOVEL     CMPART        PA(Y)
     C                   ELSE
     C                   SETON                                        82  88
     C                   END
     C                   END
     C**
     C     CMCAUS        LOOKUP    CA                                     31
     C     *IN31         IFEQ      '0'
     C     Z             IFLT      9
     C                   ADD       1             Z
     C                   MOVEL     CMCAUS        CA(Z)
     C                   ELSE
     C                   SETON                                        83  88
     C                   END
     C                   END
     C**
     C*  GET HISTORY FILE, DETERMINE IF OPEN OR CLOSED
     C*
     C     CHKEY         SETGT     PCH01
     C                   READP     PCH01                                  60
     C     *IN60         IFEQ      '0'
     C     CHCO#         ANDEQ     XXCO#
     C     CHFND         ANDEQ     FNFND
     C     CHFYR         ANDEQ     F1FYR
     C     CHCASE        ANDEQ     CMCASE
     C     CHOCST        IFEQ      'C'
     C                   ADD       1             L1CLS             5 0
     C                   ELSE
     C                   ADD       1             L1OPN             5 0
     C                   END
     C**
     C                   ADD       1             PARTL1            5 0
     C                   ADD       1             NATRL1            5 0
     C**
     C                   Z-ADD     1             R
     C                   DO        10            R                 2 0
     C                   MOVEL     CMT(R)        XXRES
     C**
     C*  GET RESERVE TYPE CODE FILE
     C*
     C                   MOVE      '0'           *IN20
     C                   MOVE      '0'           *IN21
     C     TBKEY         CHAIN     PRT01                              60
     C     *IN60         IFEQ      '0'
     C     RTTYPE        IFEQ      'C'
     C                   MOVE      '1'           *IN20
     C                   END
     C     RTTYPE        IFEQ      'M'
     C                   MOVE      '1'           *IN21
     C                   END
     C                   END
     C**
     C                   MOVE      CMPART        HPART             2
     C                   MOVE      CMNATR        HNATR             2
     C**
     C   20              ADD       CMA(R)        COMP              9 2
     C   21              ADD       CMA(R)        MED               9 2
     C  N20
     CANN21              ADD       CMA(R)        OTHR              9 2
     C                   ADD       CMR(R)        OUTRES            9 2
     C**
     C                   END
     C**
     C     COMP          ADD       MED           TOTALP            9 2
     C                   ADD       OTHR          TOTALP
     C     TOTALP        ADD       OUTRES        TOTINC            9 2
     C**
     C                   ADD       COMP          L1COMP            9 2
     C                   ADD       MED           L1MED             9 2
     C                   ADD       OTHR          L1OTHR            9 2
     C                   ADD       TOTALP        L1TOTL            9 2
     C                   ADD       TOTINC        L1TINC            9 2
     C                   ADD       OUTRES        L1ORES            9 2
     C                   MOVE      'Y'           XXPRNT
     C**
     C   22              EXCEPT    HEAD
     C                   SETOFF                                       22
     C                   EXCEPT    DETAIL
     C     *INOF         IFEQ      '1'
     C                   SETON                                        22
     C                   EXSR      SUB01
     C                   END
     C*
     C                   ELSE
     C*
     C     PARTL1        IFEQ      0
     C                   MOVE      *BLANKS       HPART             2
     C                   END
     C     NATRL1        IFEQ      0
     C                   MOVE      *BLANKS       HNATR             2
     C                   END
     C*
     C                   END
     C**
     C                   GOTO      TAG02
     C**
     C     END           TAG
     C**
     CLR 22              EXCEPT    HEAD2
     CLR                 EXCEPT    TOTALS
     CLR                 EXSR      SUB01
      ***** Beg Add ***** 08/26/14 ***********************************
      /Free
       xxPrt = '0';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
      /End-free
      ***** End Add ***** 08/26/14 ***********************************
     C**
      *==================================================================*
      *   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
      *==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C     XXMODE        IFEQ      'D'                                          DEMAND MODE
     C                   SETON                                        09
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C                   TIME                    DSDTTM
     C     UPKEY         CHAIN     PUP01                              60
     C* * * INITIALIZE FIELDS
     C                   Z-ADD     *ZERO         X                 1 0
     C                   Z-ADD     *ZERO         Y                 1 0
     C                   Z-ADD     *ZERO         Z                 1 0
     C*
     C     COKEY         CHAIN     SMCOL                              60
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
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
      **** BEGIN ADD **** 12/16/99 ************************************
      *
      * Left Justify Text in Field
      *
     C     ORTITL        IFNE      *BLANKS                                      FIELD
     C                   MOVE(P)   ORTITL        XXDATA           60            FIELD, LENGTH
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      ORTITL                         FIELD
     C                   ENDIF
     C                   ENDIF
      **** END   ADD **** 12/16/99 ************************************
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
      ***** Beg Del ***** 08/26/14 ************************************
     C************       EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   END
     C*
     C                   MOVE      '1'           *IN11                          1ST TIME INDIC.
     C                   MOVE      '1'           *IN22
     C*
     C* * * PRINT 1ST PAGE HEADINGS
     C*
     C*
     C                   ENDSR
     C*
      *==================================================================*
      *   Print arrays / bottom of each page Subroutine    ** S U B 0 1  *
      *==================================================================*
     C*
     CSR   SUB01         BEGSR
     C*
     C     NA(1)         IFEQ      *BLANK
     C     PA(1)         IFEQ      *BLANK
     C     CA(1)         IFEQ      *BLANK
     C                   GOTO      ENDSR
     C                   END
     C                   END
     C                   END
     C**
     C                   SORTA     NA
     C                   SORTA     PA
     C                   SORTA     CA
     C                   MOVE      '0'           *IN41
     C                   MOVE      '0'           *IN42
     C                   MOVE      '0'           *IN43
     C                   EXCEPT    TOTALH
     C**
     C                   Z-ADD     *ZERO         K                 2 0
     C                   Z-ADD     *ZERO         L                 2 0
     C                   Z-ADD     *ZERO         M                 2 0
     C     TAG10         TAG
     C     K             IFGE      9
     C                   MOVE      '1'           *IN41
     C                   GOTO      KTAG
     C                   ELSE
     C                   ADD       1             K
     C                   END
     C**
     C     NA(K)         DOWEQ     *BLANK
     C     K             IFLT      9
     C                   ADD       1             K
     C                   END
     C                   END
     C                   MOVEL     NA(K)         XXRES
     C     TBKEY         CHAIN     PNA01                              60
     C                   MOVEL     NADESC        NAD(K)
     C     KTAG          TAG
     C**
     C     L             IFGE      9
     C                   MOVE      '1'           *IN42
     C                   GOTO      LTAG
     C                   ELSE
     C                   ADD       1             L
     C                   END
     C     PA(L)         DOWEQ     *BLANK
     C     L             IFLT      9
     C                   ADD       1             L
     C                   END
     C                   END
     C                   MOVEL     PA(L)         XXRES
     C     TBKEY         CHAIN     PPA01                              60
     C                   MOVE      PADESC        PAD(L)
     C     LTAG          TAG
     C**
     C     M             IFGE      9
     C                   MOVE      '1'           *IN43
     C                   GOTO      MTAG
     C                   ELSE
     C                   ADD       1             M
     C                   END
     C     CA(M)         DOWEQ     *BLANK
     C     M             IFLT      9
     C                   ADD       1             M
     C                   END
     C                   END
     C                   MOVEL     CA(M)         XXRES
     C     TBKEY         CHAIN     PCA01                              60
     C                   MOVE      CADESC        CAD(M)
     C     MTAG          TAG
     C**
     C                   EXCEPT    TOTAL
     C     *IN41         IFEQ      '0'
     C     *IN42         OREQ      '0'
     C     *IN43         OREQ      '0'
     C                   GOTO      TAG10
     C                   END
     C**
     C                   EXCEPT    TOTALE
     C                   Z-ADD     *ZERO         R
     C                   Z-ADD     *ZERO         X
     C                   Z-ADD     *ZERO         Y
     C                   Z-ADD     *ZERO         Z
     C                   MOVE      '0'           *IN88
     C                   MOVE      '0'           *INOF
     C                   MOVE      *BLANK        NA
     C                   MOVE      *BLANK        PA
     C                   MOVE      *BLANK        CA
     C                   MOVE      *BLANK        NAD
     C                   MOVE      *BLANK        PAD
     C                   MOVE      *BLANK        CAD
     C     ENDSR         ENDSR
     C**
     C**
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ***********************************
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
      ***** End Del ***** 08/26/14 ***********************************
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
      ***** Beg Del ***** 08/26/14 ***********************************
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
     O*   MLS 1/15/88 ADD OUT QUEUE AND INDICATORS
     O**********        09   QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ***********************************
     O***  BEGIN CHG  ***  01/28/10  *********************************
     O*****   E  160           ROUTE
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      *****END   CHG ***** 01/28/10 **********************************
      ***** BEG   DLT ****** 01/28/10 *********************************
     O***                                23 '(C) COPYRIGHT PROGRESSI'
      ***** END   DLT ****** 01/28/10 *********************************
      ***** BEGIN CHG ****** 08/27/97 *********************************
     O***********************************46 'VE DATA SOLUTIONS. INC,'
     O***********************************51 '.1990'
      ***** BEG   DLT ****** 01/28/10 *********************************
     O***                                47 'VE DATA SOLUTIONS. INC.,'
      ***** END   DLT ****** 01/28/10 *********************************
      ***** END   CHG ****** 08/27/97 *********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             115
     O                       DSDATE        Y    196
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL             126
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           1
      ****** BEGIN CHG ****** 08/27/97 ********************************
     O***********************************09 'FUND:'
     O                                           09 'GROUP:'
      ****** END   CHG ****** 08/27/97 ********************************
     O                       CMFND         Z     13
     O                       XFNAME              75
     O          E            HEAD           2
      ****** BEGIN CHG ****** 08/27/97 ********************************
     O***********************************14 'FUND YEAR:'
     O*************************XXYR      19
     O*************************XXCENT    17
     O                                           15 'POLICY YEAR:'
      **** BEGIN CHG **** 12/16/99 ************************************
     O************************ XXYR      20
     O************************ XXCENT    18
     O                       F1FYR               19
      **** END   CHG **** 12/16/99 ************************************
      ****** END   CHG ****** 08/27/97 ********************************
     O                                           35 'SELECTION:'
     O                       X1FRDT        Y     45
     O                                           47 '-'
     O                       X1TODT        Y     57
     O          E            HEAD           1
     O                                           61 'INJURY'
     O                                           79 'INJURY CODES'
     O                                           91 'LOST   DATE'
     O          E            HEAD           2
      ******** BEGIN CHG ***** 08/12/97 *******************************
     O***********************************15 'EMPLR DIV  CASE'
     O***********************************26 'CLAIMANT'
     O                                            9 'EMPLR DIV'
     O                                           34 'CASE  CLAIMANT'
      ******** END   CHG ***** 08/12/97 *******************************
     O                                           51 'SOC SEC'
     O                                           66 'DATE   O/C'
     O                                           79 'NAT PRT CAU'
     O                                           93 'TIME RECEIVED'
     O                                          106 'COMPENSATION'
     O                                          124 'M E D I C A L'
     O                                          137 'O T H E R'
     O                                          155 'TOTAL PAID'
     O                                          176 'TOTAL INCURRED'
     O                                          194 'OUTSTANDING RES'
     O          E            DETAIL         1
      ******* BEGIN CHG ****** 08/12/97 *******************************
     O*************************CMEMP#Z    9
     O*************************CMDIV Z   15
     O                       CMEMP2        Z      5
     O                       CMDIV2        Z      9
      ******* END   CHG ****** 08/12/97 *******************************
     O                       CMCASE        Z     24
     O                       CNAME               41
     O                       SS4                 46
     O                       SS2                 48
     O                       SS5                 53
     O**** BEGIN CHG **** 02/11/99 ************************************
     O**********************   ACCDTE    62 '  /  /  '
     O                       ACCDTE        Y     62
     O**** END   CHG **** 02/11/99 ************************************
     O                       CHOCST              65
     O                       CMNATR              70
     O                       CMPART              74
     O                       CMCAUS              78
     O                       CMLTFL              82
     O**** BEGIN CHG **** 02/11/99 ************************************
     O***********************  RECDTE    92 '  /  /  '
      **** BEGIN CHG **** 12/16/99 ************************************
     O************************ RECDTEY   92
     O                       RECDTE        Y     93
      **** END   CHG **** 12/16/99 ************************************
     O**** END   CHG **** 02/11/99 ************************************
     O                       COMP          AB   108
     O                       MED           AB   125
     O                       OTHR          AB   142
     O                       TOTALP        AB   159
     O                       TOTINC        AB   176
     O                       OUTRES        AB   193
     O          E            TOTL1       1  2
     O                       PARTL1        1B    68
     O                                           88 'CASES FOR PART CODE'
     O                       HPART               91
     O                       L1COMP        AB   108
     O                                          109 '*'
     O                       L1MED         AB   125
     O                                          126 '*'
     O                       L1OTHR        AB   142
     O                                          143 '*'
     O                       L1TOTL        AB   159
     O                                          160 '*'
     O                       L1TINC        AB   176
     O                                          177 '*'
     O                       L1ORES        AB   193
     O                                          194 '*'
     O          E            TOTL2          3
     O                       NATRL1        1B    68
     O                                           90 'CASES FOR NATURE CODE'
     O                       HNATR               93
     O                       L2COMP        AB   108
     O                                          110 '**'
     O                       L2MED         AB   125
     O                                          127 '**'
     O                       L2OTHR        AB   142
     O                                          144 '**'
     O                       L2TOTL        AB   159
     O                                          161 '**'
     O                       L2TINC        AB   176
     O                                          178 '**'
     O                       L2ORES        AB   193
     O                                          195 '**'
     O          E            TOTL3          3
     O                                           17 'TOTALS FOR YEAR'
     O                       L3OPN         1     25
     O                                           36 'CASES OPEN'
     O                       L3CLS         1     43
     O                                           56 'CASES CLOSED'
     O                       L3TCAS        1     68
     O                                           80 'TOTAL CASES'
     O                       L3COMP        AB   108
     O                       L3MED         AB   125
     O                       L3OTHR        AB   142
     O                       L3TOTL        AB   159
     O                       L3TINC        AB   176
     O                       L3ORES        AB   193
     O          E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME             115
     O                       DSDATE        Y    196
     O          E            HEAD2          2
     O                       Q1PGM               10
     O                       ORTITL             126
     O                                          190 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD2          1
      **** BEGIN CHG **** 12/16/99 ************************************
     O*********************************  09 'FUND:'
     O                                           09 'GROUP:'
      **** END   CHG **** 12/16/99 ************************************
     O                       CMFND         Z     13
     O                       XFNAME              75
     O          E            HEAD2          2
     O                                           35 'SELECTION:'
     O                       X1FRDT        Y     45
     O                                           47 '-'
     O                       X1TODT        Y     57
     O          E            HEAD2          1
     O                                           61 'INJURY'
     O                                           79 'INJURY CODES'
     O                                           91 'LOST   DATE'
     O          E            HEAD2          2
     O                                           15 'EMPLR DIV  CASE'
     O                                           26 'CLAIMANT'
     O                                           51 'SOC SEC'
     O                                           66 'DATE   O/C'
     O                                           79 'NAT PRT CAU'
     O                                           93 'TIME RECEIVED'
     O                                          106 'COMPENSATION'
     O                                          124 'M E D I C A L'
     O                                          137 'O T H E R'
     O                                          155 'TOTAL PAID'
     O                                          176 'TOTAL INCURRED'
     O                                          194 'OUTSTANDING RES'
     O          E    LR      TOTALS      2  2
     O                                           14 'FINAL TOTALS'
     O                       LROPN         1     22
     O                                           33 'CASES OPEN'
     O                       LRCLS         1     40
     O                                           53 'CASES CLOSED'
     O                       LRTCAS        1     68
     O                                           80 'TOTAL CASES'
     O                       LRCOMP        A    108
     O                       LRMED         A    125
     O                       LROTHR        A    142
     O                       LRTOTL        A    159
     O                       LRTINC        A    176
     O                       LRORES        A    193
     O          E            TOTALH         2 78
     O                                           16 'NATURE OF INJURY'
     O                                           54 'PART OF BODY INJURED'
     O                                           83 'CAUSE OF INJURY'
     O          E            TOTAL          1
     O                       NA(K)          B    02
     O                       NAD(K)         B    33
     O                       PA(L)          B    36
     O                       PAD(L)         B    67
     O                       CA(M)          B    70
     O                       CAD(M)         B   101
     O          E       81 88TOTALE         1
     O                                           15 'TOO MANY CODES,'
     O                                           37 'REFERENCE NATURE LIST'
     O          E       82 88TOTALE         1
     O                                           48 'TOO MANY CODES,'
     O                                           70 'REFERENCE PART LIST'
     O          E       83 88TOTALE         1
     O                                           81 'TOO MANY CODES,'
     O                                          103 'REFERENCE CAUSE LIST'
