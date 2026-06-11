     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE SRFLAT - FLAT RATE CALCULATION SUBROUTINE
     H*                                                               *
     H* PROGRAMER: 65                                                 *
     H* PROJECT:   WC844                                              *
     H* DATE:      6/30/00                                            *
     H*                                                               *
     H* AMOUNT OF FLAT RATE ADJUSTMENT IS PASSED OUT OF PROGRAM AS    *
     H* AN ALPHANUMERIC PARM OF LENGTH 9.  THE RESULT SHOULD BE MOVED *
     H* INTO A 9.0 FIELD.                                             *
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  02/02/09  C:220 Project: WC1635(Priss#7)                     *
     H*                  Correction in diff in NY adjustment value    *
     H*                                                               *
     H*  11/06/08  C:220 Project: WC1635                              *
     H*                  WCSTAT NY Rate Adjustment                    *
     H*                                                               *
     H*  08/28/00  C:65  PROJECT: WC844                               *
     H*                  PROCESS 9/1/2000 RATE ADJUSTMENTS.           *
     H*                  THE RATE CHANGE IS 3.6%.                     *
     H*                                                               *
     H*****************************************************************
     FWDELL     IF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
      *** BEGIN ADD ***  11/06/08  ************************************
     FWMEML     IF   E           K DISK
     FWDF1L     IF   E           K DISK

     D PDAT            S              7  0 DIM(6) ASCEND                        PAYROLL DATES
     D RDAT            S              7  0 DIM(6) ASCEND                        RATING ARRAY
     D MAN             S             11  2 DIM(6)                               MANUAL PREM
     D PRO             S              5  4 DIM(6)                               PRORATA
     D DYS             S              3  0 DIM(6)                               DAYS IN EACH PERIOD
      *** END   ADD ***  11/06/08  ************************************
      *
     D                 DS
     D  ELPTDT                 1      7  0
     D  XXPTM                  4      5  0
      *** BEG   ADD ***  11/06/08  ************************************
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
      *** END   ADD ***  11/06/08  ************************************
      *

     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      *
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
      *
     C*** BEG   ADD ***  11/06/08  ************************************
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
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
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C*** END   ADD ***  11/06/08  ************************************
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQAMT             9
      *
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQFYR         XXFYR             3 0
     C                   MOVE      QQEMP#        XXEMP#            9 0
     C                   MOVE      QQDIV         XXDIV             5 0
      *
      * ZERO OUT AMOUNT FIELD.
      *
     C                   Z-ADD     0             XXAMT             9 0
      *
     C     ELKY          CHAIN     PEL01                              60
      *
      * PROCESS IF EMPLOYER DETAIL EXISTS AND IS NOT MINIMUM PREMIUM.
      *
     C     *IN60         IFEQ      *OFF
     C     ELAMNP        ANDNE     'Y'
      *
     C                   MOVE      ELRTCD        XXRTCD            3
     C     RSKY          CHAIN     PRS01                              60
      *
      * MUST HAVE VALID RATE SET CODE.
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      RSJRST        XXST              2
     C                   Z-ADD     ELPFDT        XXEFDT            7 0
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *
      * MUST HAVE VALID STATISTICAL RECORD.
      *
     C     *IN60         IFEQ      *OFF
     C*
     C*** BEG   ADD ***  11/06/08  ************************************
     C*  CLEAR RATING ARRAYS
     C*
     C                   Z-ADD     0             RDAT
     C                   Z-ADD     0             PDAT
     C                   Z-ADD     0             PRO
     C                   Z-ADD     0             MAN
     C                   Z-ADD     0             DYS
     C                   Z-ADD     0             XXSTDT            7 0
     C                   Z-ADD     0             XXANDT
     C                   Z-ADD     0             X1ANDT            7 0
     C*
     C     F1KEY         CHAIN     PF101                              66
     C*
     C*  TO CHECK IF NOT PROCESSSING FOR STATE REORTING DIV
     C*
     C     ENKY          CHAIN     PEM01                              61
     C*
     C     EMSTRP        IFNE      'Y'
     C     EMKY          SETLL     PEM01
     C     EMKY          READE     PEM01                                  62
     C*
     C     *IN62         DOWEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   MOVE      EMDIV         XSDIV             5 0
     C                   ENDIF                                                  EMSTRP = 'Y'
     C*
     C     EMKY          READE     PEM01                                  62
     C                   ENDDO                                                  *IN62=*OFF
     C*
     C                   MOVE      XXDIV         XXWDV             5 0
     C                   MOVE      XSDIV         XXDIV
     C*
     C*  TO READ WITH STATE REPORTING DIV
     C*
     C     ELKY          CHAIN     PEL01                              60
     C                   MOVE      ELPFDT        X1PFDT            7 0
     C                   MOVE      ELANDT        X1ANDT            7 0
     C                   MOVE      XXWDV         XXDIV
     C*
     C*  TO READ WITH PROCESSING DIV
     C*
     C     ELKY          CHAIN     PEL01                              60
     C     ELANDT        IFEQ      0
     C     X1ANDT        IFNE      0
     C     X1ANDT        ADD       10000         X1ANDT
     C     X1ANDT        IFLE      ELPFDT
     C                   Z-ADD     X1ANDT        XXSTDT
     C                   ELSE
     C     X1ANDT        SUB       10000         XXSTDT
     C                   ENDIF                                                  X1ANDT < ELPFDT
     C                   ENDIF                                                  X1ANDT <> 0
     C     XXSTDT        IFEQ      0
     C     X1PFDT        ANDLT     ELPFDT
     C                   MOVE      X1PFDT        XXSTDT
     C                   ENDIF                                                  XXSTDT = 0
     C                   ENDIF                                                  ELANDT = 0
     C                   ENDIF                                                  EMSTRP = Y
     C                   MOVE      ELPFDT        RDAT(1)
     C                   MOVE      ELPFDT        PDAT(1)
     C     XXSTDT        IFNE      0
     C                   MOVE      XXSTDT        RDAT(1)
     C                   ENDIF                                                  XXSTDT <> 0
     C                   Z-ADD     2             X                 2 0
     C     ELANDT        IFNE      0
     C     ELANDT        ANDNE     ELPFDT
     C                   MOVE      ELANDT        XXANDT
     C                   ADD       1             XXANYR
     C     XXANDT        IFGT      ELPFDT
     C                   MOVE      ELANDT        RDAT(1)
     C                   ENDIF
     C     XXANDT        IFGT      ELPFDT
     C     XXANDT        ANDLT     ELPTDT
     C                   MOVE      XXANDT        RDAT(X)
     C                   MOVE      XXANDT        PDAT(X)
     C                   ADD       1             X
     C                   ENDIF                                                  XXANDT >ELPFDT
     C                   ENDIF                                                  ELANDT <> 0
     C                   MOVE      ELPTDT        X1ANDT
     C                   Z-ADD     X1ANDT        PDAT(X)
     C*
     C*********** DAY CALCULATION  BETWEEN ELPFDT & ELPTDT **************
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XAFROM            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XATO              8 0
     C*
     C                   Z-ADD     0             X1FROM            8 0
     C                   Z-ADD     0             X1TO              8 0
     C                   MOVE      X1FROM        QQFROM            8 0
     C                   MOVE      X1TO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM      XAFROM        QQFROM
     C                   PARM      XATO          QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XAFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM            7 0
     C*
     C                   Z-ADD     XATO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO              7 0
     C*
     C                   Z-ADD     XQFROM        XXLPDT            7 0
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XAFROM
     C*
     C                   Z-ADD     XAFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XAFROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF                                                  SRERR <> '1'
     C*
     C                   ENDIF                                                  XXLPDT <> 0
     C*
     C                   Z-ADD     QQDAYS        PDAYS             3 0
     C*********** TO POPULATE THE DYS AND PRO ARRAYS ****************
     C                   Z-ADD     1             X
     C*
     C     RDAT(X)       DOWNE     0
     C     X             ADD       1             Z                 2 0
     C*
     C*********** DAY CALCULATION  BETWEEN PDAT,X & PDAT,Z **************
     C*
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XAFROM            8 0
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XATO              8 0
     C*
     C                   MOVE      X1FROM        QQFROM            8 0
     C                   MOVE      X1TO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM      XAFROM        QQFROM
     C                   PARM      XATO          QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XAFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XATO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   Z-ADD     XQFROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XQFROM
     C                   Z-ADD     XQTO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XQFROM
     C     XXLPDT        ORGT      XQTO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XAFROM
     C*
     C                   Z-ADD     XAFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XAFROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF                                                  SRERR  <> '1'
     C*
     C                   ENDIF                                                  XXLPDT <> 0
     C*
     C                   Z-ADD     QQDAYS        DYS(X)
     C*
     C*
     C     PDAYS         IFNE      0
     C     DYS(X)        DIV(H)    PDAYS         PRO(X)
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     PRO           XXWK54            5 4
     C     1             SUB       XXWK54        XXWK54
     C                   ADD       XXWK54        PRO(1)
     C*
     C****** POPULATE MAN ARRAY FOR RATE CALCULATIONS *****************
     C*
     C                   Z-ADD     0             XXPREM
     C*
     C****** TO LOOP OVER PAYROLL FILE  *******************************
     C*
     C     EPKY          SETLL     PEP01
     C     EPKY          READE     PEP01                                  65
     C     *IN65         DOWEQ     *OFF
     C     EPSTAT        IFEQ      'A'
     C     RRDCCD        ANDNE     EPCLAS
     C                   ADD       EPAMPR        XXPREM
     C                   ENDIF                                                  RRDCCD <> EPCLAS
     C     EPKY          READE     PEP01                                  65
     C                   ENDDO                                                  *IN65 = *OFF
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
     C     PRO(X)        MULT      XXPREM        XXWK10           11 0
     C                   MOVE      XXWK10        MAN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X <> 0
     C                   XFOOT     MAN           XXWK10
     C     XXWK10        IFNE      XXPREM
     C     XXPREM        SUB       XXWK10        XXWK10
     C                   ADD       XXWK10        MAN(1)
     C                   ENDIF                                                  XXWK10 <> XXPREM
     C*** END   ADD ***  11/06/08  ************************************
     C*
     C* APPLY RATE INCREASE TO INDIANA ACCOUNT IF APPLICABLE.
     C*
     C     RRST          IFEQ      'IN'
     C     ELPTDT        ANDGE     990801
     C     ELPFDT        ANDLT     990701
     C*
     C                   Z-ADD     0             XXPREM           11 2
     C*
     C* TOTAL AUDITED MANUAL PREMIUM.
     C*
     C     EPKY          CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFEQ      'A'
     C     RRDCCD        ANDNE     EPCLAS
     C                   ADD       EPAMPR        XXPREM
     C                   ENDIF
     C*
     C     EPKY          READE     PEP01                                  60
     C                   ENDDO
     C*
     C* DETERMINE NUMBER OF MONTHS TO HAVE RATE INCREASE.
     C*
     C     ELPTDT        IFGE      1000101
     C     12            ADD       XXPTM         XXMON             2 0
     C                   ELSE
     C                   Z-ADD     XXPTM         XXMON
     C                   ENDIF
     C*
     C                   SUB       7             XXMON
     C*
     C                   SELECT
     C     XXMON         WHENEQ    1
     C                   Z-ADD     .2            XXMPC             2 1
     C     XXMON         WHENEQ    2
     C                   Z-ADD     .5            XXMPC
     C     XXMON         WHENEQ    3
     C                   Z-ADD     .7            XXMPC
     C     XXMON         WHENEQ    4
     C                   Z-ADD     .9            XXMPC
     C     XXMON         WHENEQ    5
     C                   Z-ADD     1.1           XXMPC
     C     XXMON         WHENEQ    6
     C                   Z-ADD     1.4           XXMPC
     C     XXMON         WHENEQ    7
     C                   Z-ADD     1.6           XXMPC
     C     XXMON         WHENEQ    8
     C                   Z-ADD     1.8           XXMPC
     C     XXMON         WHENEQ    9
     C                   Z-ADD     2.0           XXMPC
     C     XXMON         WHENEQ    10
     C                   Z-ADD     2.3           XXMPC
     C     XXMON         WHENEQ    11
     C                   Z-ADD     2.5           XXMPC
     C                   OTHER
     C                   Z-ADD     2.7           XXMPC
     C                   ENDSL
     C*
     C                   Z-ADD     XXMPC         XXRPCT            4 3
     C                   DIV       100           XXRPCT
     C                   ADD       1             XXRPCT
     C*
     C* APPLY RATE INCREASE TO PREMIUM TOTAL.
     C*
     C     XXPREM        MULT(H)   XXRPCT        XXAMT
     C                   SUB       XXPREM        XXAMT
     C*
     C                   ENDIF                                                  RRST IFEQ
     C*
     C*** BEGIN ADD ***  08/28/00  ************************************
     C     RRST          IFEQ      'IN'
     C     ELPTDT        ANDGE     1001001
     C     ELPFDT        ANDLT     1000901
     C*
     C                   Z-ADD     0             XXPREM           11 2
     C*
     C* TOTAL AUDITED MANUAL PREMIUM.
     C*
     C     EPKY          CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFEQ      'A'
     C     RRDCCD        ANDNE     EPCLAS
     C                   ADD       EPAMPR        XXPREM
     C                   ENDIF
     C*
     C     EPKY          READE     PEP01                                  60
     C                   ENDDO
     C*
     C* DETERMINE NUMBER OF MONTHS TO HAVE RATE INCREASE.
     C*
     C     ELPTDT        IFGE      1010101
     C     12            ADD       XXPTM         XXMON             2 0
     C                   ELSE
     C                   Z-ADD     XXPTM         XXMON
     C                   ENDIF
     C*
     C                   SUB       9             XXMON
     C*
     C                   SELECT
     C     XXMON         WHENEQ    1
     C                   Z-ADD     .3            XXMPC             2 1
     C     XXMON         WHENEQ    2
     C                   Z-ADD     .6            XXMPC
     C     XXMON         WHENEQ    3
     C                   Z-ADD     .9            XXMPC
     C     XXMON         WHENEQ    4
     C                   Z-ADD     1.2           XXMPC
     C     XXMON         WHENEQ    5
     C                   Z-ADD     1.5           XXMPC
     C     XXMON         WHENEQ    6
     C                   Z-ADD     1.8           XXMPC
     C     XXMON         WHENEQ    7
     C                   Z-ADD     2.1           XXMPC
     C     XXMON         WHENEQ    8
     C                   Z-ADD     2.4           XXMPC
     C     XXMON         WHENEQ    9
     C                   Z-ADD     2.7           XXMPC
     C     XXMON         WHENEQ    10
     C                   Z-ADD     3.0           XXMPC
     C     XXMON         WHENEQ    11
     C                   Z-ADD     3.3           XXMPC
     C                   OTHER
     C                   Z-ADD     3.6           XXMPC
     C                   ENDSL
     C*
     C                   Z-ADD     XXMPC         XXRPCT            4 3
     C                   DIV       100           XXRPCT
     C                   ADD       1             XXRPCT
     C*
     C* APPLY RATE INCREASE TO PREMIUM TOTAL.
     C*
     C     XXPREM        MULT(H)   XXRPCT        XXAMT
     C                   SUB       XXPREM        XXAMT
     C*
     C                   ENDIF                                                  RRST IFEQ
     C*** END ADD *****  08/28/00  ************************************
     C*** BEG ADD *****  11/06/08  ************************************
     C*
     C     XXST          IFEQ      'NY'
     C     ELPTDT        ANDGT     1071001
     C     RDAT(1)       ANDLT     1071001
     C     RDAT(1)       ANDNE     1061001
     C     F1NY07        ANDNE     'Y'
     C*** END ADD *****  11/06/08  ************************************
     C*
     C*********** DAY CALCULATION  BETWEEN RATE CAHNGE & SRDTO *********
     C*** BEG CHG *****  02/02/09  ************************************
     C                   CALL      'SRDDIF7'
     C                   PARM      1071001       SRFRDC            7 0
     C                   PARM      ELPTDT        SRTODC            7 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
      *
     C     SRDAYS        DIV(H)    366           X2FACT            7 6
     C     X2FACT        MULT(H)   .161          XXNYP             5 4
      *
     C     XXANDT        IFNE      0
     C     XXANDT        ANDGT     1071001
      *
     C                   CALL      'SRDDIF7'
     C                   PARM      ELPFDT        SRFRD1            7 0
     C                   PARM      XXANDT        SRTOD1            7 0
     C                   PARM                    SRDAY1            5 0
     C                   PARM      ' '           SRERR1            1
     C                   PARM      ' '           SRTER1            1
      *
     C     ELPTDT        IFGT      1070228
     C     SRDAY1        DIV(H)    366           X2FACT
     C                   ELSE
     C     SRDAY1        DIV(H)    365           X2FACT
     C                   ENDIF
      *
     C                   MULT(H)   X2FACT        XXNYP
      *
     C                   ELSE
      *
     C     ELPROR        IFNE      1
     C                   MULT(H)   ELPROR        XXNYP
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     1             SUB       XXNYP         XXNYP
      *
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
     C     RDAT(X)       IFLT      1071001
     C     XXNYP         MULT(H)   MAN(X)        XXWK10
     C                   Z-ADD     XXWK10        MAN(X)
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO
     C*
     C*                    MOVEL10012007  XAFROM
     C*                    Z-ADDELPTDT    SRDTI
     C*                    CALL 'SRCVTDAT'DTMDYY
     C*                    MOVELSRDTO     XATO
     C*                    Z-ADD*ZEROS    QQDAYS
     C*                    MOVE *OFF      QQERR
     C*                    CALL 'SRDDIF8'
     C*                    PARM XAFROM    QQFROM
     C*                    PARM XATO      QQTO
     C*                    PARM           QQDAYS
     C*                    PARM           QQERR
     C*
     C*          QQDAYS    DIV  366       X2FACT  76H
     C*          X2FACT    MULT .161      XXNYP   52H
     C*          XXANDT    IFNE 0
     C*          XXANDT    ANDGT1071001
     C*
     C*********** DAY CALCULATION  BETWEEN ELPFDT & XXANDT **************
     C*
     C*                    Z-ADDELPFDT    SRDTI
     C*                    CALL 'SRCVTDAT'DTMDYY
     C*                    MOVELSRDTO     XAFROM
     C*
     C*                    Z-ADDXXANDT    SRDTI
     C*                    CALL 'SRCVTDAT'DTMDYY
     C*                    MOVELSRDTO     XATO
     C*                    MOVE *BLANKS   QQDAYS
     C*                    MOVE *BLANKS   QQERR
     C*                    CALL 'SRDDIF8'
     C*                    PARM XAFROM    QQFROM
     C*                    PARM XATO      QQTO
     C*                    PARM           QQDAYS
     C*                    PARM           QQERR
     C*          QQDAYS    DIV  365       X2FACT
     C*          ELPTDT    IFGT 1080228
     C*          QQDAYS    DIV  366       X2FACT
     C*                    ENDIF                           ELPTDT > 1082802
     C*          X2FACT    MULT XXNYP     XXNYP     H
     C*                    ELSE
     C*          ELPROR    IFNE 1
     C*          ELPROR    MULT XXNYP     XXNYP     H
     C*                    ENDIF                           ELPROR <> 1
     C*                    ENDIF                           XXANDT <> 0
     C*          1         SUB  XXNYP     XXNYP     H
     C*                    Z-ADD1         Y       20
     C*                    DO   X
     C*          RDAT,Y    IFNE 0
     C*          RDAT,Y    ANDLT1071001
     C*          XXNYP     MULT MAN,Y     XXWK10    H
     C*                    Z-ADDXXWK10    MAN,Y
     C*                    ENDIF                           RDAT,X <> 0
     C*                    ADD  1         Y                ADD 1 TO Y
     C*                    ENDDO                           X TIMES
     C*** END CHG *****  02/02/09  ************************************
     C                   XFOOT     MAN           XXAMT
     C*
     C*** CHANGE OF THE FLAT RATE AFTER NY ADJUSTMENT *****************
     C*
     C                   SUB       XXPREM        XXAMT
     C                   ENDIF                                                  XXST = 'NY'
     C*
     C*** END ADD *****  11/06/08  ************************************
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* COPY AMOUNT TO PARAMETER FOR EXPORT.
     C*
     C                   MOVE      XXAMT         QQAMT
     C*
     C                   MOVE      *ON           *INLR
