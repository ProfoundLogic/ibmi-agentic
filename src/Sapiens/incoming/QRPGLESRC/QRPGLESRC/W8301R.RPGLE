     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8301R - SELF-INSURER PAYROLL REPORT
     H*************************************************************
     H** PROGRAM DESCRIPTION:
     H**   THIS PROGRAM GENERATES A WORK FILE FOR SUBMITTING BSI-5
     H**   STATE REPORTS.  BSI-5'S SHOW AUDITED PAYROLL AMOUNTS AND
     H**   RATES FOR EACH ACTIVE PAYROLL CLASS FOR AND EMPLOYER WITHIN
     H**   THE ENTERED FYR.
     H**   CALLED FROM:
     H**         W8301R - PROMPT
     H**   CALLS TO:
     H**         NONE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/17/00  C:35   PROJECT: SUPPORT WA091                       *
     H*                  GET POLICY DATES FROM STATE-REPORTING        *
     H*                  DIVISION.                                    *
     H*                                                               *
     H* 08/26/98  C:35  PROJECT: SUPPORT WA033 13400                  *
     H*                 CORRECTED PRINTING OF POLICY EXPIRATION DATE. *
     H*                                                               *
     H* 06/24/98  C:35  PROJECT: SUPPORT WA091                        *
     H*                 1. A LINE OF CODE HAD BEEN COMMENTED OUT,     *
     H*                    RESULTING IN ALL PAYROLL BEING REPORTED    *
     H*                    UNDER THE LAST CLASS CODE FOR EACH         *
     H*                    EMPLOYER.                                  *
     H*                 2. PREMIUM WAS NOT SHOWING ON THE PRINTED     *
     H*                    OUTPUT.                                    *
     H*                 3. CHANGED POLICY NUMBER PRINT TO CONFORM     *
     H*                    CHANGES MADE FOR PROJECT WC829, 03/30/98.  *
     H*                                                               *
     H* 04/17/98  C:35  PROJECT: SUPPORT WA033 11185                  *
     H*                 SINCE XXARAT FIELD WAS EXPANDED TO 7 DIGITS,  *
     H*                 NO NEED TO PAD ARAT WITH ZEROS IN LEFT 2      *
     H*                 BYTES.                                        *
     H*                                                               *
     H* 03/30/98  C:35  PROJECT: WC829                                *
     H*                 USE OLD FORMAT FOR POLICY DATE IF POLICY      *
     H*                 EFFECTIVE DATE < F2DT01.                      *
     H*                                                               *
     H*****************************************************************
     FWXB5P     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL1    IF   E           K DISK
      **** BEGIN ADD **** 04/17/00 *********************************************
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
      **** BEGIN ADD **** 04/17/00 *********************************************
     FWMPRL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FQTAPE     O    F  150        SEQ
     FWDF2L     IF   E           K DISK
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)

     D ST              S              2    DIM(54) CTDATA PERRCD(27)
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D OUTTP           DS
     D  FUND#                  1      3
     D  ACCT#                  4     12
     D  FYR                   13     15
     D  FILL1                 16     18
      **** BEGIN ADD **** 03/30/98 *********************************************
     D  ACCTX                  4      8
     D  FYRX                   9     11
     D  FILLX                 12     18
      **** END   ADD **** 03/30/98 *********************************************
     D  BEGDT                 19     24
     D  ENDDT                 25     30
     D  JOBCLS                31     34
     D  PAYROL                35     44
     D  AMPR                  45     52
     D  ARAT                  53     59
     D  FLAGF                 60     60
     D  EMPNM                 61    103
     D  TPEXMD               104    107
     D  TPEXDT               108    113
     D  TPRTDT               114    119
     D  FILL2                120    120
     D  STCD                 121    122
     D  CARIER               123    127
     D  RISKID               128    136
     D  FILL3                137    150
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      **** BEGIN ADD **** 04/17/00 *********************************************
     C     ELKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
      **** END   ADD **** 04/17/00 *********************************************
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR PAYROLL WORK FILE
     C*
     C     B5KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*   KEY LIST FOR THE PAYROLL FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
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
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     *LIKE         DEFINE    B5EFDT        XXEFDT
     C     *LIKE         DEFINE    B5CLAS        XXCLAS
     C     *LIKE         DEFINE    B5SPEC        XXSPEC
     C     *LIKE         DEFINE    B5AAPR        XXAAPR
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    ELPTDT        XXPTDT
     C     *LIKE         DEFINE    ELPFDT        X1FROM
     C     *LIKE         DEFINE    ELPTDT        X1TO
     C     *LIKE         DEFINE    ELADC1        XXADC1
     C     *LIKE         DEFINE    B5AMPR        XXAMPR
     C**** BEGIN ADD **** 10/28/97 ************************************
     C     *LIKE         DEFINE    B5AMPR        SVAMPR
     C     *LIKE         DEFINE    B5AMPR        S1AMPR
     C     *LIKE         DEFINE    ELAMNP        XXAMNP
     C**** END   ADD **** 10/28/97 ************************************
      **** BEGIN ADD **** 04/17/00 *********************************************
     C     *LIKE         DEFINE    ELEMP#        XHEMP#
     C     *LIKE         DEFINE    ELPFDT        XHPFDT
     C     *LIKE         DEFINE    ELPTDT        XHPTDT
     C     *LIKE         DEFINE    ELADC1        XHADC1
      **** END   ADD **** 04/17/00 *********************************************
      **** BEGIN ADD **** 03/30/98 *********************************************
      *
      * NOTE: THIS FIELD MUST REMAIN IN THE OLD FORMAT - 5 DIGITS
      *       FOR THE EMPLOYER #
      *
     C                   Z-ADD     0             XXEMPO            5 0
      **** END   ADD **** 03/30/98 *********************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQHRDC            1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YMD'        SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YMD'        SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQFND         FUND#
     C                   MOVE      QQFYR         FYR
     C                   MOVE      QQHRDC        XXHRDC            1
     C                   MOVE      *BLANKS       FILL1
     C                   MOVE      *BLANKS       FILL2
     C                   MOVE      *BLANKS       FILL3
     C                   MOVE      'N'           LAST              1            LAST EMPLYR
     C*
     C                   Z-ADD     0             XXECT             5 0
     C                   Z-ADD     0             XXTPR            11 2
     C                   Z-ADD     0             XXERR             5 0
     C                   Z-ADD     0             XXARAT            7 3
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     F2KY          CHAIN     PF201                              60
     C                   MOVE      F2CAR#        CARIER
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   TURNS INDICATOR 80 ON IF "PRINT HARD COPY" OPTION = 'Y'
     C*
     C                   SETOFF                                       80
     C     XXHRDC        IFEQ      'Y'
     C                   SETON                                        80
     C                   ENDIF                                                  SCHRDC
     C*
     C*   FIND POSITION IN FILE AND THEN READ UNTIL EMPLOYER BREAK
     C*   GET POLICY DATES
     C*
     C     ELKEY         SETLL     PEL01
     C     ELKEY         READE     PEL01                                  60
     C                   Z-ADD     ELEMP#        XXEMP#
      **** BEGIN ADD **** 04/17/00**********************************************
     C                   Z-ADD     ELEMP#        XXHEMP            9 0
      **** END   ADD **** 04/17/00**********************************************
     C                   Z-ADD     ELPFDT        XXPFDT
     C                   Z-ADD     ELPTDT        XXPTDT
     C**** BEGIN ADD **** 10/28/97 ************************************
     C                   MOVEL     ELAMNP        XXAMNP
     C**** END   ADD **** 10/28/97 ************************************
     C                   Z-ADD     0             XXADC1
     C     *IN60         DOWEQ     '0'
     C*
      **** BEGIN CHG **** 04/17/00**********************************************
     C********** ELEMP#    IFNE XXEMP#
     C     ELEMP#        IFNE      XXHEMP
     C                   Z-ADD     ELEMP#        XXHEMP
      **** END   CHG **** 04/17/00**********************************************
     C*
     C     FINAL         TAG
     C*
     C*
     C*   CHAIN OUT TO THE EMPLOYER MASTER FILE AND GET EMPLOYER/DIV
     C*   THAT HAS THE STATE FLAG SET ELSE CHAIN TO EMPLOYER/DIV=0
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     '0'
     C     EMSTRP        ANDNE     'Y'
     C     EMKEY         READE     PEM01                                  60
     C                   END
     C*
     C     EMSTRP        IFNE      'Y'
     C     EMKEY         CHAIN     PEM01                              60
     C                   END
      **** BEGIN ADD **** 04/17/00 *********************************************
     C                   Z-ADD     ELEMP#        XHEMP#
     C                   Z-ADD     ELPFDT        XHPFDT
     C                   Z-ADD     ELPTDT        XHPTDT
     C                   Z-ADD     ELADC1        XHADC1
     C     ELKY2         CHAIN     PEL02                              60
     C                   Z-ADD     ELPFDT        XXPFDT
     C                   Z-ADD     ELPTDT        XXPTDT
      **** END   ADD **** 04/17/00 *********************************************
     C*
     C*   FLIP POLICY DATES FROM YMD TO MDY
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         X2TO              6 0
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTO              8 0
     C*
     C*   TURNS INDICATOR 81 ON IF "FNCGFL" ='Y'
     C*
     C     FNCGFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN81
     C                   ELSE
     C     FNCGFL        IFEQ      'N'
     C                   MOVE      *OFF          *IN81
     C                   ENDIF                                                  FNCGFL
     C                   ENDIF                                                  FNCGFL
     C*
     C*
     C*   GET POLICY END DATE FOR WRITING OF CONTR CLASS CREDIT
     C*   IF APPLICABLE
     C                   MOVE      XXTO          SRDTE             8 0
     C                   Z-ADD     1             SRPST             5 0
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTE             8 0
     C                   PARM                    SRPST
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
     C*
     C                   MOVE      SRDTE         X1PTDT            8 0
     C*
     C                   Z-ADD     X1PTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPTDT
     C*
     C*   CHAIN OUT TO THE PAYROLL WORK FILE AND START LOOP FOR FUND
     C*   ADD ALL " " SPEC TOGETHER FOR EACH CLASS EACH "F" SPEC IS
     C*   PRINTED SEPERATELY WITH AN F NEXT TO CLASS
     C*
     C     B5KEY         CHAIN     PB501                              60
     C*
     C*   ONLY UPDATE FIELDS AND PRINT HEADING IF EMPLOYER HAS
     C*   PAYROLL RECORDS ELSE SKIP OVER AND GET NEXT EMPLOYER
     C*
     C     *IN60         IFEQ      '0'
     C*
     C*   GET PUT EMPLOYER NAME AND NUMBER INTO TAPE OUTPUT FIELDS
     C*
      **** BEGIN ADD **** 03/30/98 *********************************************
     C     ELPFDT        IFLT      F2DT01
     C                   Z-ADD     EMEMP#        XXEMPO
     C                   MOVE      XXEMPO        ACCTX
     C                   MOVE      B5FYR         FYRX
     C                   MOVE      *BLANKS       FILLX
      **** BEGIN ADD **** 06/24/98 *********************************************
     C                   MOVE      *ON           *IN42
      **** END   ADD **** 06/24/98 *********************************************
     C                   ELSE
      **** END   ADD **** 03/30/98 *********************************************
     C                   MOVE      EMEMP#        ACCT#
      **** BEGIN ADD **** 03/30/98 *********************************************
     C                   MOVE      B5FYR         FYR
     C                   MOVE      *BLANKS       FILL1
      **** BEGIN ADD **** 06/24/98 *********************************************
     C                   MOVE      *OFF          *IN42
      **** END   ADD **** 06/24/98 *********************************************
     C                   ENDIF                                                  ELPFDT IFLT F2DT01
      **** END   ADD **** 03/30/98 *********************************************
     C                   MOVEL     EMNAME        EMPNM
     C                   Z-ADD     1             I                 2 0
     C*
     C     EMPST         IFNE      *BLANK
     C*
     C     EMPST         LOOKUP    ST(I)                                  61
     C*
     C                   ELSE
     C*
     C     EMMST         LOOKUP    ST(I)                                  61
     C*
     C                   ENDIF
     C*
     C                   MOVE      I             STCD
     C                   MOVEL     EMCOM3        RISKID
     C*
     C                   ADD       1             XXECT
     C*
     C*   MOVE PAYROLL DATES INTO TAPE OUTPUT FIELDS
     C*
     C                   MOVE      XXPFDT        BEGDT
     C                   MOVE      XXPTDT        ENDDT
     C                   MOVE      'Y'           XXFRST            1
     C                   Z-ADD     B5EFDT        XXEFDT
     C                   Z-ADD     0             XXFROM            6 0
     C                   Z-ADD     0             X2TO              6 0
     C*
     C*   PRINT HEADING
     C*
     C*
     C                   Z-ADD     B5CLAS        XXCLAS
     C                   MOVE      B5SPEC        XXSPEC
     C                   Z-ADD     0             XXAAPR
     C                   Z-ADD     0             XXARAT
     C                   MOVE      B5RDAT        TPRTDT
     C                   MOVE      B5EXDT        TPEXDT
     C                   MOVE      B5EXMD        TPEXMD
     C                   Z-ADD     0             XXAMPR
     C*
     C*  CLASSES IN WMEPP WITH SPEC DES = S HAVE BEEN WRITTEN TO
     C*  WXB5P WITH B5SPEC = BLANK.  WE WILL ACCUMULATE ALL CLASSES
     C*  WITH THE SAME SPEC DES AND EFFECTIVE DATE.
     C*  EACH HARBOR EXP (SPEC DES = F) WILL WRITE A SEPERATE RECORD
     C*  ON EACH OCCURANCE
     C*
     C     *IN60         DOWEQ     '0'
     C*
     C     B5CLAS        IFNE      XXCLAS
     C     XXSPEC        ANDEQ     ' '
     C     B5SPEC        ORNE      XXSPEC
     C     XXSPEC        ANDEQ     ' '
     C     B5EFDT        ORNE      XXEFDT
     C     XXSPEC        ANDEQ     ' '
     C*
     C******************** EXSR PRTSR
      **** BEGIN ADD **** 06/24/98 *********************************************
     C                   EXSR      PRTSR
      **** END   ADD **** 06/24/98 *********************************************
     C*
     C     B5SPEC        IFEQ      ' '
     C                   ADD       B5AAPR        XXAAPR
     C                   MOVE      B5SPEC        XXSPEC
     C                   Z-ADD     B5CLAS        XXCLAS
     C                   Z-ADD     B5ARAT        XXARAT
     C                   Z-ADD     B5EFDT        XXEFDT
     C                   MOVE      XXPFDT        BEGDT
     C                   MOVE      XXPTDT        ENDDT
     C                   MOVE      B5RDAT        TPRTDT
     C                   MOVE      B5EXDT        TPEXDT
     C                   MOVE      B5EXMD        TPEXMD
     C                   ADD       B5AMPR        XXAMPR
     C                   END
     C*
     C                   ELSE
     C*
     C     B5SPEC        IFEQ      ' '
     C                   ADD       B5AAPR        XXAAPR
     C                   MOVE      B5SPEC        XXSPEC
     C                   Z-ADD     B5CLAS        XXCLAS
     C                   Z-ADD     B5ARAT        XXARAT
     C                   Z-ADD     B5EFDT        XXEFDT
     C                   ADD       B5AMPR        XXAMPR
     C                   END
     C*
     C                   END
     C*
     C     B5SPEC        IFEQ      'F'
     C                   Z-ADD     B5AAPR        XXAAPR
     C                   MOVE      B5SPEC        XXSPEC
     C                   Z-ADD     B5CLAS        XXCLAS
     C                   Z-ADD     B5ARAT        XXARAT
     C                   Z-ADD     B5EFDT        XXEFDT
     C                   MOVE      XXPFDT        BEGDT
     C                   MOVE      XXPTDT        ENDDT
     C                   Z-ADD     B5AMPR        XXAMPR
     C                   EXSR      PRTSR
     C                   END
     C*
     C     B5KEY         READE     PB501                                  60
     C                   END
     C*
     C     XXSPEC        IFEQ      ' '
     C                   EXSR      PRTSR
     C                   END
     C*
     C     XXADC1        IFNE      0
     C                   Z-ADD     XXADC1        XXAAPR
     C                   Z-ADD     9046          XXCLAS
     C                   MOVE      ' '           XXSPEC
     C                   Z-ADD     0             XXARAT
     C                   Z-ADD     0             XXADC1
     C     X1FROM        IFNE      XXEFDT
     C                   MOVE      '1'           *INOA
     C                   END
     C                   Z-ADD     0             XXAMPR
     C                   EXSR      PRTSR
     C                   END
     C*
     C     *IN80         IFEQ      *ON
     C*
     C                   EXCEPT    BOTT
     C*
     C                   ENDIF
     C*
     C                   END                                                    PAYROLL REC
     C*
     C     LAST          CABEQ     'Y'           TERM
     C*
      **** BEGIN ADD **** 04/17/00 *********************************************
     C                   Z-ADD     XHEMP#        XXEMP#
     C                   Z-ADD     XHPFDT        XXPFDT
     C                   Z-ADD     XHPTDT        XXPTDT
     C                   ADD       XHADC1        XXADC1
      *
     C                   ELSE
      *
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELPFDT        XXPFDT
     C                   Z-ADD     ELPTDT        XXPTDT
     C                   ADD       ELADC1        XXADC1
      **** END   ADD **** 04/17/00 *********************************************
     C                   END                                                    EMP# LOOP
     C*
      **** BEGIN DEL **** 04/17/00 *********************************************
     C******************** Z-ADDELEMP#    XXEMP#
     C******************** Z-ADDELPFDT    XXPFDT
     C******************** Z-ADDELPTDT    XXPTDT
     C******************** ADD  ELADC1    XXADC1
      **** END   DEL **** 04/17/00 *********************************************
     C*
     C     ELKEY         READE     PEL01                                  60
     C                   END
     C*
     C*   GO BACK AND PROCESS THE LAST EMPLOYER
     C*
     C                   MOVE      'Y'           LAST
     C                   GOTO      FINAL
     C*
     C*   END OF PROGRAM
     C*
     C     TERM          TAG
     C*
      **** BEGIN DEL **** 04/17/00 *********************************************
     C******************** EXCPTHEAD
      **** END   DEL **** 04/17/00 *********************************************
     C                   EXCEPT    TOTL
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*   SUBROUTINE TO PRINT EACH DETAIL LINE AND GETS DESCRIPTION
     C*
     C     PRTSR         BEGSR
     C*
     C* GET PAYROLL DESCRIPTION
     C*
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '1'
     C     PRCO#         ORNE      XXCO#
     C     PRCLAS        ORNE      XXCLAS
     C                   MOVEL     *BLANKS       PRDESC
     C                   END
     C     XXCLAS        IFEQ      9046
     C     XXADC1        ANDNE     0
     C                   MOVE      XXPFDT        BEGDT
     C                   MOVE      XXPTDT        ENDDT
     C                   END
     C*
     C                   Z-ADD     XXCLAS        X1CLAS            4 0
     C**** BEGIN ADD **** 10/28/97 ************************************
     C     XXAMNP        IFEQ      'Y'
     C*
     C                   Z-ADD     XXAMPR        SVAMPR
     C*
     C     XXARAT        DIV       100           XXRATE            7 5
     C*
     C     XXAAPR        MULT      XXRATE        XXAMPR
     C*
     C                   Z-ADD     XXAMPR        S1AMPR
     C*
     C                   ENDIF
     C**** END   ADD **** 10/28/97 ************************************
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   MOVE      XXAAPR        XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXAAPR
     C                   END
     C                   MOVE      '00'          XXAAPR
     C*
     C                   Z-ADD     XXAAPR        X1AAPR            9 0
     C                   MOVE      XXAMPR        XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXAMPR
     C                   END
     C                   MOVE      '00'          XXAMPR
     C*
     C                   Z-ADD     XXAMPR        X1AMPR            9 0
     C*
     C                   MOVE      BEGDT         XXBGDT            6 0
     C*
     C                   Z-ADD     XXBGDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     X1FROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFROM
     C*
     C                   MOVE      ENDDT         XXEDDT            6 0
     C*
     C                   Z-ADD     XXEDDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         X1TO
     C*
     C                   Z-ADD     X1TO          SRDTI
      **** BEGIN CHG **** 08/26/98 *********************************************
     C******************** CALL 'SRCVTDAT'DTMDY8
     C                   CALL      'SRCVTDAT'    DTMDY
      **** END   CHG **** 08/26/98 *********************************************
     C                   MOVEL     SRDTO         X2TO
     C*
     C     *IN80         IFEQ      *ON
     C*
     C     XXFRST        IFEQ      'Y'
     C*
     C                   EXCEPT    HEAD
     C*
     C                   MOVE      'N'           XXFRST
     C                   END
     C*
     C*
     C     *INOA         IFEQ      '1'
     C                   EXCEPT    BOTT
     C                   EXCEPT    HEAD
     C                   MOVE      '0'           *INOA
     C                   END
     C*
     C                   EXCEPT    DET
     C*
     C                   ENDIF
     C*
     C*  IF THERE IS A CHANGE IN RATING PERIOD, EFF DT WILL BE DIFF
     C*  AND WE WILL NEED TO PAGE BREAK FOR NEW DATE HEADING ON THE
     C*  NEXT DETAIL PRINT
     C*
     C                   Z-ADD     XXFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FROM
     C*
     C     X1FROM        IFNE      B5EFDT
     C                   MOVE      '1'           *INOA
     C                   END
     C*
     C*   MOVE DATA INTO TAPE OUTPUT FIELDS
     C*
     C                   MOVE      X1CLAS        JOBCLS            4
     C                   MOVE      *ALL'0'       PAYROL
     C                   MOVE      X1AAPR        PAYROL
     C                   MOVE      XXSPEC        FLAGF             1
     C                   MOVE      XXARAT        ARAT
      **** BEGIN DEL **** 04/17/98 *********************************************
     C******************** MOVEL'00'      ARAT
      **** END   DEL **** 04/17/98 *********************************************
     C                   MOVE      X1AMPR        AMPR
     C*   WRITE TO TAPE
     C                   WRITE     QTAPE         OUTTP
     C*
     C     XXARAT        IFEQ      0
     C     XXCLAS        ANDNE     9046
     C                   ADD       1             XXERR
     C                   END
     C**** BEGIN ADD **** 10/28/97 ************************************
     C*
     C     XXAMNP        IFEQ      'Y'
     C*
     C                   EXSR      MPDIFF
     C*
     C                   ENDIF
     C*
     C**** END   ADD **** 10/28/97 ************************************
     C                   ADD       XXAAPR        XXTPR
     C*
     C                   Z-ADD     0             XXAAPR
     C                   Z-ADD     0             XXAMPR
     C*
     C                   ENDSR
     C**** BEGIN ADD **** 10/28/97 ************************************
     C*****************************************************************
     C* MPDIFF - PRINTS DIFFERENCE BETWEEN MINIMUM PREMIUM
     C*****************************************************************
     C     MPDIFF        BEGSR
     C*
     C* CALCULATE DIFFERENCE
     C*
     C     SVAMPR        SUB       S1AMPR        XXAMPR
     C*
     C* ROUND TO THE NEAREST DOLLAR
     C*
     C                   MOVE      XXAMPR        XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXAMPR
     C                   END
     C                   MOVE      '00'          XXAMPR
     C*
     C                   Z-ADD     XXAMPR        X1AMPR            9 0
     C*
     C* MOVE PREMIUM DIFFERENCE AND CLASS CODE TO OUTTP
     C*
     C                   MOVE      X1AMPR        AMPR
     C                   MOVE      F2MPCD        JOBCLS
     C                   MOVE      *BLANKS       PAYROL
     C                   MOVE      *BLANKS       ARAT
     C*
     C* WRITE TO TAPE
     C*
     C                   WRITE     QTAPE         OUTTP
     C*
     C                   ENDSR
     C*****************************************************************
     C**** END   ADD **** 10/28/97 ************************************
     C*
     OQSYSPRT   E            HEAD           1 02
     O                                           27 'STATE OF FLORIDA'
     O                                           51 'DEPARTMENT OF LABOR AND'
     O                                           71 'EMPLOYMENT SECURITY'
     O          E            HEAD           1
     O                                           50 'DIVISION OF LABOR'
     O          E            HEAD           1
     O                                           15 'LES Form WCI-5'
     O                                           41 'Bureau of Workman'
     O                                           42 ''''
     O                                           56 's Compensation'
     O          E            HEAD           1
     O                                           10 '(R-10/78)'
     O                                           37 'Tallahassee, Florida'
     O                                           57 '32301 / Phone (904)'
     O                                           66 '488-8888'
     O          E            HEAD           3
     O                                           48 'SELF-INSURER PAYROLL'
     O                                           55 'REPORT'
     O          E            HEAD        1  1
     O                                           57 'EMPLOYER NO.'
     O                                           74 'PERIOD COVERED'
     O          E            HEAD           1
     O                       EMSEMP              54
     O                       XXFROM        Y     65
     O                                           67 '-'
     O                       X2TO          Y     78
     O          E            HEAD           1
     O                       EMNAME              45
     O          E            HEAD           1
     O                                           67 'EXPERIENCE MODIFICATION'
     O          E            HEAD           2
     O                       FNNAME              65
     O          E            HEAD           2
     O                                           14 'ACCOUNT #'
     O                 N81   XXFND               19
     O                 N81                       20 '-'
      **** BEGIN CHG **** 06/24/98 *********************************************
     O****************** N81   EMEMP#    29
     O              N42N81   EMEMP#              29
     O               42N81   XXEMPO              25
      **** END   CHG **** 06/24/98 *********************************************
     O                  81   XXFND               19
      **** BEGIN CHG **** 06/24/98 *********************************************
     O******************* 81   EMEMP#    28
     O******************* 81   XXFYR     31
     O              N42 81   EMEMP#              28
     O              N42 81   XXFYR               31
     O               42 81   XXEMPO              24
     O               42 81   XXFYR               27
      **** END   CHG **** 06/24/98 *********************************************
     O          E            HEAD           1
     O                                           36 'AMOUNT OF PAYROLL BY'
     O                                           49 'OCCUPATIONAL'
     O                                           64 'CLASSIFICATION'
     O          E            HEAD           1
     O                                           44 'Manual'
      **** BEGIN CHG **** 06/24/98 *********************************************
     O********************************** 64 'Rate Per'
     O                                           66 'Rate Per'
      **** END   CHG **** 06/24/98 *********************************************
     O          E            HEAD           2
     O                                           24 'Occupation'
     O                                           44 'Class'
      **** BEGIN CHG **** 06/24/98 *********************************************
     O********************************** 54 'Payroll'
     O********************************** 62 '$100'
     O********************************** 74 'Premium'
     O                                           56 'Payroll'
     O                                           64 '$100'
     O                                           79 'Premium'
      **** END   CHG **** 06/24/98 *********************************************
     O          E            DET            2
     O                       PRDESC              35
     O                       X1CLAS              43
     O                       XXSPEC              44
     O                       X1AAPR        4     56
      **** BEGIN CHG **** 06/24/98 *********************************************
     O************************ XXARAT4   63
     O                       XXARAT        4     66
     O                       X1AMPR        4     79
      **** END   CHG **** 06/24/98 *********************************************
     O          E            BOTT           1 59
     O                                           27 'Please return form to:'
     O                                           50 'Florida Department of'
     O                                           71 'Labor and Employment'
     O                                           80 'Security'
     O          E            BOTT           1
     O                                           50 'Division of Worker''s '
     O                                           62 'Compensation'
     O          E            BOTT           1
     O                                           52 'Bureau of Worker''s Comp'
     O                                           75 'ensation Self-Insurance'
     O          E            BOTT           1
     O                                           51 '100 West Gaines Street'
     O          E            BOTT           1
     O                                           52 'Suite 562 Fletcher Buil'
     O                                           56 'ding'
     O          E            BOTT           1
     O                                           49 'Tallahassee, Florida'
     O                                           60 '32399-4103'
     O          E            BOTT           0
     O                                           39 'ASSESSMENT COMPUTATION'
     O                                           57 'WILL BE SENT WITH'
     O                                           65 'BILLING'
     O*
     O          E            TOTL           1 02
     O                                           40 'BSI - 5 TAPE'
     O          E            TOTL        2  1
     O                                           30 'EMPLOYERS'
     O                                           54 'PAYROLL'
     O          E            TOTL           1  7
     O                                           15 'TOTALS:'
     O                       XXECT         Z     30
     O                       XXTPR         J     55
     O                                           65 'ERRORS:'
     O                       XXERR         4     70
**  TABLE ST - STATES IN ENDORSEMENT NUMERICAL ORDER **** ADD 06/06/96
ALAZARCACOCTDEDCFLGAIDILINIAKSKYLAMEMDMAMIMNMSMOMTNENV
NHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIWYPRZZHIZZAK
