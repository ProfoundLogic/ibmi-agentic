     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W8210R - BSI-17 REPORT & TAPE OUTPUT
     H*************************************************************
     H** PROGRAM DESCRIPTION:
     H**   THIS PROGRAM GENERATES A REPORT & TAPE FOR SUBMITTING BSI-17
     H**   STATE REPORTS.
     H**
     H**   A RECORD IS WRITTEN FOR EACH EXCESS CLAIM SHOWING AMOUNT
     H**   INCURRED.
     H**
     H**   ANOTHER RECORD IS WRITTEN FOR ALL NON-EXCESS CLAIMS WITH THE
     H**   SAME NCCI, CLASS CODE, AND PAYROLL EFFECTIVE DATE WITH THE
     H**   TOTAL INCURRED.
     H**
     H**   EACH NON-EXCESS RECORD IS USED TO ESTABLISH THE EMPLOYERS
     H**   PAYROLL DATE RANGES (ANNIVERSARY RATING MAY CAUSE MORE THAN
     H**   ONE PAYROLL DATE RANGE).  EACH DATE RANGE MUST BE SPECIFIED
     H**   EVEN THOUGH NO NON-EXCESS CLAIMS EXISTED.
     H**
     H**   IF QQRPT = '1', A RECORD WILL BE WRITTEN FOR EACH PAYROLL
     H**   DATE RANGE FOR EMPLOYERS WITH NO CLAIMS.
     H**
     H**   CALLED FROM:
     H**         W8200C - OVERRIDE, LOAD TAPE
     H**   CALLS TO:
     H**         NONE
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/09/20 C:318 Project: SY420
     H*                Mask SSN
     H*
     H* 02/09/01  C:35   PROJECT: SUPPORT WA094 #26257                *
     H*                  DO NOT PRINT EMPLOYER HEADER ON TOTALS PAGE. *
     H*                                                               *
     H* 02/09/99  C:35   PROJECT: SUPPORT WA051 16263                 *
     H*                  BEGINNING AND ENDING DATES WERE NOT PRINTING *
     H*                  CORRECTLY ON PAGE 2 AND UP FOR EACH POLICY.  *
     H*                                                               *
     H* 01/29/99  C:35   PROJECT: SUPPORT WA051 16263                 *
     H*                  CORRECTED PAGE BREAKING.                     *
     H*                                                               *
     H* 06/25/98  C:35  PROJECT: SUPPORT WA091 12394                  *
     H*                 CHANGE PRINT OF POLICY NUMBER TO BE           *
     H*                 CONSISTENT WITH THE BSI-5.                    *
     H*                                                               *
     H* 03/30/98  C:35  PROJECT: WC829                                *
     H*                 USE OLD FORMAT FOR POLICY NUMBER IF POLICY    *
     H*                 EFFECTIVE DATE < F2DT01.                      *
     H*                                                               *
     H*****************************************************************
     H** INDICATOR USAGE:
     H**
     H**    45, 47 INDICATES PRINTING OF NON-EXCESS RECORD
     H**    46     PRINT PAGE END TOTALS
     H**    60     GLOBAL FILE CHAIN
     H**    85     FIRST TIME INDICATOR
     H**
     H*************************************************************
     FSMCOL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWXB7L2    IF   E           K DISK
      **** BEGIN ADD **** 09/19/97 ************************************
     FWXB7L     IF   E           K DISK
     F                                     RENAME(PB701:PB702)
      **** END   ADD **** 09/19/97 ************************************
     FWDF2L     IF   E           K DISK
      **** BEGIN ADD **** 06/25/98 ************************************
     FWMFNL     IF   E           K DISK
      **** END   ADD **** 06/25/98 ************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     FQTAPE     O    F  100        SEQ

     D ST              S              2    DIM(54) CTDATA PERRCD(27)
      **** END   ADD **** 09/19/97 ************************************
     D*
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5)                               CXL.DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  B7SS#                  1      9
     D  XXSS1                  1      3
     D  XXSS2                  4      5
     D  XXSS3                  6      9
     D*
     D                 DS
     D  XXZIP                  1      9  0
     D  XXZIP1                 1      5  0
     D  XXZIP2                 6      9  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D OUTTP           DS
     D  TPFND                  1      3
     D  TPACCT                 4     12
     D  TPFYR                 13     15
     D  TPFIL1                16     18
      **** BEGIN ADD **** 03/30/98 *********************************************
     D  TPACCX                 4      8
     D  TPFYRX                 9     11
     D  TPFILX                12     18
      **** END   ADD **** 03/30/98 *********************************************
     D  TPBGDT                19     24
     D  TPTODT                25     30
     D  TPCLAS                31     34
     D  TPRPT                 35     35
     D  TPSTAT                36     36
     D  TPNCCI                37     38
     D  TPSSCS                39     47
     D  TPMEDA                48     56
     D  TPINDE                57     65
     D  TPCATA                66     66
     D  TPFIL2                67     67
     D  TPSTCD                68     69
     D  TPCAR#                70     74
     D  TPRSK#                75     83
     D  TPFIL3                84    100
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      **** BEGIN ADD **** 09/19/97 ************************************

     IPB702
     I              B7CO#                       B2CO#
     I              B7FND                       B2FND
     I              B7FYR                       B2FYR
     I              B7ACCD                      B2ACCD
     I              B7EOPD                      B2EOPD
     I              B7EMP#                      B2EMP#
     I              B7DIV                       B2DIV
     I              B7SS#                       B2SS#
     I              B7#CLM                      B2#CLM
     I              B7NCCI                      B2NCCI
     I              B7CLAS                      B2CLAS
     I              B7REP#                      B2REP#
     I              B7STAT                      B2STAT
     I              B7MEDA                      B2MEDA
     I              B7INDA                      B2INDA
     I              B7ALAE                      B2ALAE
     I              B7CATA                      B2CATA
     I              B7#PAG                      B2#PAG
     I              B7CASE                      B2CASE
     I              B7PFDT                      B2PFDT
     I              B7EXCS                      B2EXCS

     C*
     C*   KEY LIST FOR THE COMPANY FILE.
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  BSI-17 WORK FILE
     C*
     C     B7KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR             3 0
     C*
     C*
     C     B7KY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    B7EMP#
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
      **** BEGIN ADD **** 06/25/98 *********************************************
      *
      * KEY LIST FOR THE FUND MASTER FILE
      *
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      **** END   ADD **** 06/25/98 *********************************************
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EMDT          XXPFDT
      **** BEGIN ADD **** 03/30/98 *********************************************
      *
      * NOTE: THIS FIELD MUST REMAIN IN THE OLD FORMAT - FIVE DIGITS
      *       FOR THE EMPLOYER #
      *
     C                   Z-ADD     0             XXEMPO            5 0
      **** END   ADD **** 03/30/98 *********************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQRPT#            1
      ***** BEGIN ADD ***** 09/19/97 **********************************
     C                   PARM                    QQHRDC            1
      ***** END   ADD ***** 09/19/97 **********************************
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQFYR         TPFYR
     C*
     C                   TIME                    DSDTTM
     C*
     C*   CHAIN TO THE COMPANY FILE.
     C*
     C     COKEY         CHAIN     PCO01                              60
     C   60              MOVE      *BLANK        CONAME
     C                   MOVEL     CONAME        XXCONM           40
     C                   Z-ADD     0             XXEMP#
      **** BEGIN ADD **** 09/19/97 ************************************
     C                   Z-ADD     0             XXPFDT
      **** END   ADD **** 09/19/97 ************************************
     C                   MOVE      *ALL'0'       XXSS#             9
      **** BEGIN ADD **** 06/25/98 ************************************
     C     FNKY          CHAIN     PFN01                              60
     C     FNCGFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN81
     C                   ELSE
     C                   MOVE      *OFF          *IN81
     C                   ENDIF                                                  FNCGFL
      **** END   ADD **** 06/25/98 ************************************
     C     F2KY          CHAIN     PF201                              60
     C                   MOVE      F2CAR#        TPCAR#
     C                   MOVE      *BLANKS       TPFIL1
     C                   MOVE      *BLANKS       TPFIL2
     C                   MOVE      *BLANKS       TPFIL3
      ***** BEGIN ADD **** 09/19/97 ***********************************
     C*
     C*   TURNS INDICATOR 80 ON IF "PRINT HARD COPY" OPTION = 'Y'
     C*
     C                   SETOFF                                       80
     C     QQHRDC        IFEQ      'Y'
     C                   SETON                                        80
     C                   ENDIF                                                  SCHRDC
      ***** END   ADD **** 09/19/97 ***********************************
     C*
     C*   START READING BSI-17 WORK RECORDS
     C*
     C     B7KEY         CHAIN     PB701                              60
     C     *IN60         DOWEQ     '0'
      **** BEGIN ADD **** 09/19/97 ************************************
     C*
      **** BEGIN DEL **** 02/09/99 *********************************************
     C********** *IN80     IFEQ *ON
      **** END   DEL **** 02/09/99 *********************************************
     C*
     C     B7EMP#        IFNE      XXEMP#
     C     B7ACCD        ORNE      XXPFDT
     C     B7SS#         ANDEQ     XXSS#
     C*
      **** BEGIN ADD **** 02/09/99 *********************************************
     C     *IN80         IFEQ      *ON
      **** END   ADD **** 02/09/99 *********************************************
     C     B7EMP#        IFNE      XXEMP#
     C*
     C*   IF EMPLOYER CHG, PRINT TOTALS & CALC NEW PAGE TOTAL
     C*
     C                   MOVE      '1'           *IN46
     C   85              EXCEPT    TOT
     C                   Z-ADD     0             XTMEDA
     C                   Z-ADD     0             XTINDA
     C                   Z-ADD     0             XTALAE
     C*
     C     B7KY2         CHAIN     PB702                              60
     C  N60              Z-ADD     B2#PAG        B7#PAG
     C     B7#PAG        DIV       20            XXWK30            3 0
     C                   MVR                     XXWK32            3 2
     C     XXWK32        IFGT      *ZERO
     C                   ADD       1             XXWK30
     C                   END
     C                   Z-ADD     1             XXCPAG            3 0
     C*
      **** BEGIN ADD **** 01/29/99 *********************************************
     C                   MOVE      '1'           *INOF
      **** END   ADD **** 01/29/99 *********************************************
     C                   END                                                    NEW EMP#
      **** BEGIN ADD **** 02/09/99 *********************************************
     C                   ENDIF                                                  *IN80 IFEQ *ON
      **** END   ADD **** 02/09/99 *********************************************
     C*
     C                   MOVE      '0'           *IN46
     C*
      **** BEGIN DEL **** 02/09/99 *********************************************
     C******************** ENDIF                           *IN80
      **** END   DEL **** 02/09/99 *********************************************
      **** END   ADD **** 09/19/97 ************************************
     C                   MOVE      B7EMP#        XXEMP#
     C                   MOVE      B7DIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              60
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
     C                   MOVE      I             TPSTCD
     C                   MOVEL     EMCOM3        TPRSK#
     C*
      **** BEGIN ADD **** 09/19/97 ************************************
     C*
     C     *IN80         IFEQ      *ON
     C*
      **** BEGIN DEL **** 01/29/99 *********************************************
     C******************** MOVE '1'       *INOF
      **** END   DEL **** 01/29/99 *********************************************
     C*
     C*  LOAD PAYROLL DATE RANGE
     C*
     C                   Z-ADD     B7ACCD        XXPFDT
     C*
     C                   Z-ADD     XXPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C*
     C                   Z-ADD     B7EOPD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C                   MOVE      '1'           *IN85
     C*
     C                   END                                                    BREAK
      **** BEGIN ADD **** 06/25/98 *********************************************
     C     B7PFDT        IFLT      F2DT01
     C                   MOVE      *ON           *IN42
     C                   Z-ADD     B7EMP#        XXEMPO
     C                   ELSE
     C                   MOVE      *OFF          *IN42
     C                   ENDIF                                                  B7PFDT IFLT F2DT01
      **** END   ADD **** 03/30/98 *********************************************
      **** END   ADD **** 06/25/98 *********************************************
     C*
     C*   PRINT DETAIL LINES
     C*
      **** BEGIN DEL **** 02/09/99 *********************************************
     C********** *INOF     IFEQ '1'
     C********** XXDET     ORGE 20
     C******************** EXCPTHEAD
     C******************** ADD  1         XXCPAG
     C******************** MOVE '0'       *INOF
     C******************** Z-ADD0         XXDET
     C******************** END
      **** END   DEL **** 02/09/99 *********************************************
     C*
     C                   ENDIF                                                  *IN80
     C*
      **** END   ADD **** 09/19/97 ************************************
     C     B7MEDA        MULT(H)   1             XXMEDA            9 0
     C     B7INDA        MULT(H)   1             XXINDA            9 0
      **** BEGIN ADD **** 09/19/97 ************************************
     C*
     C     *IN80         IFEQ      *ON
     C*
     C     B7ALAE        MULT(H)   1             XXALAE            9 0
     C                   ADD       XXMEDA        XTMEDA            9 0
     C                   ADD       XXINDA        XTINDA            9 0
     C                   ADD       XXALAE        XTALAE            9 0
     C                   MOVE      B7CLAS        XXCLAS            4 0
     C*
     C                   Z-ADD     B7ACCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXACCD            6 0
     C*
     C                   ENDIF                                                  *IN80
     C*
      **** END   ADD **** 09/19/97 ************************************
     C     B7#CLM        COMP      0                                      45
      **** BEGIN ADD **** 09/19/97 ************************************
     C*
     C     *IN80         IFEQ      *ON
      **** BEGIN ADD **** 02/09/99 *********************************************
     C     *INOF         IFEQ      '1'
     C     XXDET         ORGE      20
     C                   EXCEPT    HEAD
     C                   ADD       1             XXCPAG
     C                   MOVE      '0'           *INOF
     C                   Z-ADD     0             XXDET
     C                   END
      **** END   ADD **** 02/09/99 *********************************************
     C*
     C   45B7SS#         COMP      *BLANKS                                47    *LJQ 4-15-88
     C   45B7SS#         COMP      XXSS#                                  47    JJJ 9-21-89
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*MASK'       QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'B7SS#'       QQNAME           25
     C     B7SS#         PARM      B7SS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   EXCEPT    DET
     C***  Begin Add  ***  06/09/20  *******************************
     C                   CALL      'SPIMASK'
     C                   PARM      '*UNMASK'     QQMODE           10
     C                   PARM      'SSN'         QQTYPE           25
     C                   PARM      'B7SS#'       QQNAME           25
     C     B7SS#         PARM      B7SS#         QQVAL           100
     C***  End Add  ***  06/09/20  *******************************
     C                   MOVE      '0'           *IN47                          *LJQ 4-15-88
     C                   ADD       1             XXDET             5 0
     C*
     C                   ENDIF                                                  *IN80
     C*
      **** END   ADD **** 09/19/97 ************************************
     C*
     C                   ADD       B7MEDA        XFMEDA           11 2
     C                   ADD       B7INDA        XFINDA           11 2
     C                   ADD       B7ALAE        XFALAE           11 2
     C   45              ADD       1             XF#CLM            5 0
     C  N45              ADD       B7#CLM        XF#CLM
     C*
     C*
     C*
     C*  WRITE TAPE RECORDS
     C*
     C                   MOVE      B7FND         TPFND
      **** BEGIN ADD **** 03/30/98 *********************************************
     C     B7PFDT        IFLT      F2DT01
     C                   Z-ADD     B7EMP#        XXEMPO
     C                   MOVE      XXEMPO        TPACCX
     C                   MOVE      B7FYR         TPFYRX
     C                   MOVE      *BLANKS       TPFILX
      **** BEGIN ADD **** 06/25/98 *********************************************
     C                   MOVE      *ON           *IN42
      **** END   ADD **** 06/25/98 *********************************************
     C                   ELSE
      **** END   ADD **** 03/30/98 *********************************************
     C                   MOVE      B7EMP#        TPACCT
      **** BEGIN ADD **** 03/30/98 *********************************************
     C                   MOVE      B7FYR         TPFYR
     C                   MOVE      *BLANKS       TPFIL1
      **** BEGIN ADD **** 06/25/98 *********************************************
     C                   MOVE      *OFF          *IN42
      **** END   ADD **** 06/25/98 *********************************************
     C                   ENDIF                                                  B7PFDT IFLT F2DT01
      **** END   ADD **** 03/30/98 *********************************************
     C*
     C                   MOVE      *ZERO         TPBGDT
     C*
     C     B7EXCS        IFEQ      'Y'
     C                   MOVE      B7ACCD        TPBGDT                         Accident Date
     C                   ENDIF
     C*
     C                   MOVE      B7PFDT        TPTODT                         Policy Effect DT
     C*
     C                   MOVE      B7CLAS        TPCLAS
     C                   MOVE      B7REP#        TPRPT
     C                   MOVE      B7STAT        TPSTAT
     C                   MOVE      B7NCCI        TPNCCI
     C     B7#CLM        IFEQ      *ZERO
     C                   MOVE      B7SS#         TPSSCS
     C                   ELSE
     C                   MOVE      *ALL'0'       TPSSCS
     C                   MOVE      B7#CLM        TPSSCS
     C                   END
     C                   MOVE      XXMEDA        TPMEDA
     C                   MOVE      XXINDA        TPINDE
     C                   MOVE      B7CATA        TPCATA
     C                   WRITE     QTAPE         OUTTP
     C*
     C*   GET NEXT BSI-17 RECORD
     C*
     C     B7KEY         READE     PB701                                  60
     C                   END
      **** BEGIN ADD **** 09/19/97 ************************************
     C*
     C     *IN80         IFEQ      *ON
     C*
     C                   MOVE      '1'           *IN46
     C                   EXCEPT    TOT
     C*
     C                   ENDIF                                                  *IN80
      **** END   ADD **** 09/19/97 ************************************
      **** BEGIN DEL **** 02/09/01 *********************************************
     C******************** EXCPTHEAD
      **** END   DEL **** 02/09/01 *********************************************
     C                   EXCEPT    FTOT
     C*
     C     END           TAG
     C                   MOVE      '1'           *INLR
     C*
     OQSYSPRT   E            HEAD             03
     O                                           11 'REPORT NO.'
     O                       QQRPT#              13
     O                                           43 'SELF INSURANCE'
     O                                           74 'PAGE'
     O                       XXCPAG              78
     O                                           81 'OF'
     O                       XXWK30              85
     O          E            HEAD             04
     O                                           42 'UNIT STATISTICAL'
     O                                           49 'REPORT'
      **** BEGIN DEL **** 01/29/99 *********************************************
     O******* E 31             HEAD
     O********************************** 25 '# OF CLAIMS'
     O********************************** 39 'MEDICAL'
     O********************************** 54 'INDEMNITY'
     O********************************** 69 'ALL. LOSS ADJ'
      **** END   DEL **** 01/29/99 *********************************************
     O          E            HEAD             06
     O                                           16 'EMPLOYER NUMBER'
     O                                           35 'ACCOUNT NUMBER'
     O                                           53 'BEGINNING DATE'
     O                                           70 'ENDING DATE'
     O          E            HEAD             07
     O                       EMSEMP              15
     O                       EMFND         Z     25
      **** BEGIN CHG **** 06/25/98 *********************************************
     O********************************** 26 '-'
     O************************ EMEMP#    31
     O                 N81                       26 '-'
     O               42N81   XXEMPO              31
     O              N42N81   EMEMP#              35
     O              N42 81   EMEMP#              34
     O              N42 81   XXFYR               37
     O               42 81   XXEMPO              30
     O               42 81   XXFYR               33
      **** END   CHG **** 06/25/98 *********************************************
     O                       XXFRDT        Y     50
     O                       XXTODT        Y     70
     O          E            HEAD             09
     O                                           14 'EMPLOYER NAME'
     O          E            HEAD             10
     O                       EMNAME              45
     O          E            HEAD             12
     O                                           14 'SSN OR NUMBER'
     O                                           22 'STATUS'
     O                                           30 'INJURY'
     O                                           39 'PAYROLL'
     O                                           50 'DATE OF'
     O                                           73 'INCURRED LOSS'
     O          E            HEAD           2 13
     O                                           12 'OF CLAIMS'
     O                                           29 'CODE'
     O                                           41 'CLASS CODE'
     O                                           51 'ACCIDENT'
     O                                           63 'MEDICAL'
     O                                           79 'INDEMNITY'
     O          E            DET            2
     O               45      XXSS1               05
     O               45N47                       06 '-'
     O               45      XXSS2               08
     O               45N47                       09 '-'
     O               45      XXSS3               13
     O              N45      B7#CLM        Z     13
     O                       B7STAT              19
     O                       B7NCCI              28
     O              N47      XXCLAS              37
     O               45N47   XXACCD              51 '  /  /  '
     O                       XXMEDA        J     66
     O                       XXINDA        J     80
     O          E            TOT              55
     O                                           21 'TOTAL ALLOCATED LOSS'
     O                                           32 'ADJUSTMENT'
     O          E            TOT              56
     O                                           20 'EXPENSE INCURRED:'
     O          E            TOT              57
     O               46      XTALAE        J     29
     O                                           47 'TOTALS:'
     O               46      XTMEDA        J     66
     O               46      XTINDA        J     80
     O          E            TOT              59
     O                                           85 'Report completed by:'
     O          E            TOT              60
     O                       XXCONM              85
     O          E            TOT              61
     O                                           24 'Please return completed'
     O                                           35 'report to:'
     O                                           59 'Florida Dept. of Labor'
     O                                           83 'and Employment Security'
     O          E            TOT              62
     O                                           58 'Division of Worker''s '
     O                                           70 'Compensation'
     O          E            TOT              63
     O                                           60 'Bureau of Worker''s Comp'
     O                                           83 'ensation Self-Insurance'
     O          E            TOT              64
     O                                           23 'LES FORM BSI-17 (5/83)'
     O                                           59 '100 West Gaines Street'
     O          E            TOT              65
     O                                           60 'Suite 562 Fletcher Buil'
     O                                           64 'ding'
     O          E            TOT              66
     O                                           57 'Tallahassee, Florida'
     O                                           68 '32399-4103'
     O          E            FTOT             06
     O                                           15 'FUND TOTALS:'
     O                       XF#CLM        Z     25
     O                       XFMEDA        J     40
     O                       XFINDA        J     55
     O                       XFALAE        J     70
**  TABLE ST - STATES IN ENDORSEMENT NUMERICAL ORDER **** ADD 06/06/96
ALAZARCACOCTDEDCFLGAIDILINIAKSKYLAMEMDMAMIMNMSMOMTNENV
NHNJNMNYNCNDOHOKORPARISCSDTNTXUTVTVAWAWVWIWYPRZZHIZZAK
