     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**SY266_OVR*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H/TITLE SRINST - GENERATE INSTALLMENTS FOR ESTIMATING POLICY.
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                       *
     H**                                                              *
     H**      THIS PROGRAM BUILDS AND RETURNS DATE/AMOUNT ARRAYS      *
     H**      FOR ESTIMATING BILLING SCHEDULE CODE POLICIES.          *
     H**                                                              *
     H**      THE ARRAYS ARE RETURNED VIA 6 PARMS.  THE DATES AND     *
     H**      AMOUNTS ARE SPLIT INTO 3 GROUPS OF 18 ENTRIES EACH,     *
     H**      FOR A TOTAL OF 54 DATES/AMOUNTS.                        *
     H**                                                              *
     H**      THE DATES ARE IN CYMD FORMAT WITH 7 CHARACTERS EACH     *
     H**      WHILE THE AMOUNTS ARE IN A 11.2 FORMAT.                 *
     H**                                                              *
     H**      THE QQTYPE PARM RETURNS THE TYPE OF PLAN PRESENT:       *
     H**      M - MONTHLY   T - TWICE MONTHLY   W - WEEKLY            *
     H**                                                              *
     H*                                                               *
     H* PROGRAMMER: C:65                                              *
     H* CREATED:    08/01/06            PROJECT: SUPP#53145           *
     H*                                                               *
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60  GENERAL CHAIN (READ) INDICATOR                            *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/31/10 C:136 PROJECT: 71WKU                                 *
     H*                ADJUST RETURN AMT ARRAY FOR RETRO CREDIT       *
     H*                ADJUSTMENTS (QQTYPE = 'R')                     *
     H*****************************************************************
     FWTABL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWDF1L     IF   E           K DISK

     D*
     D DAY             S              2    DIM(13) CTDATA PERRCD(13)              DAYS OF WEEK
     D*
     D                 DS
     D  AB                     1    108  8
     D                                     DIM(12)
     D  BSAB01                 1      9  8
     D  BSAB02                10     18  8
     D  BSAB03                19     27  8
     D  BSAB04                28     36  8
     D  BSAB05                37     45  8
     D  BSAB06                46     54  8
     D  BSAB07                55     63  8
     D  BSAB08                64     72  8
     D  BSAB09                73     81  8
     D  BSAB10                82     90  8
     D  BSAB11                91     99  8
     D  BSAB12               100    108  8
     D                 DS
     D  XXWKDT                 1      7  0
     D  YY                     1      3  0
     D  MM                     4      5  0
     D  DD                     6      7  0
     D  YYMM                   1      5  0
     D                 DS
     D  WKD                    1    378  0
     D                                     DIM(54) ASCEND
     D  WD1                    1    126
     D  WD2                  127    252
     D  WD3                  253    378
     D                 DS
     D  WKA                    1    594  2
     D                                     DIM(54)
     D  WA1                    1    198
     D  WA2                  199    396
     D  WA3                  397    594

     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  ADMIN/DARTS FILE KEY LIST
     C*
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  TABLE FILE KEY LIST
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    EXCO#         XXCO#
     C     *LIKE         DEFINE    EXFND         XXFND
     C     *LIKE         DEFINE    F1FYR         XXFYR
     C     *LIKE         DEFINE    F1STPO        XXBSDT
     C     *LIKE         DEFINE    F1STPO        XXPFDT
     C     *LIKE         DEFINE    F1STPO        XXPTDT
     C     *LIKE         DEFINE    F1STPO        XXDATE
     C     *LIKE         DEFINE    BSKEY         XXKEY
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
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQBSCD            3
     C                   PARM                    QQBSDT            7
     C                   PARM                    QQPFDT            7
     C                   PARM                    QQPTDT            7
     C                   PARM                    QQDATE            7
     C                   PARM                    QQPREM           11
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQDA1           126
     C                   PARM                    QQDA2           126
     C                   PARM                    QQDA3           126
     C                   PARM                    QQAM1           198
     C                   PARM                    QQAM2           198
     C                   PARM                    QQAM3           198
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVEL(P)  QQBSCD        XXKEY
     C                   MOVE      QQBSDT        XXBSDT
     C                   MOVE      QQPFDT        XXPFDT
     C                   MOVE      QQPTDT        XXPTDT
     C                   MOVE      QQDATE        XXDATE
     C                   MOVE      QQPREM        XXPREM           11 2
      *** BEG ADD ***** 08/31/10 **************************
     C                   MOVE      QQTYPE        X1TYPE            1
      *** END ADD ***** 08/31/10 **************************
     C                   MOVE      *BLANK        XXTYPE            1
     C                   Z-ADD     0             WKD
     C                   Z-ADD     0             WKA
     C                   Z-ADD     *ZEROS        XXAMT            11 2
     C                   Z-ADD     *ZEROS        XXAMT2           15 4
     C                   Z-ADD     *ZEROS        XXWK10           11 0
     C                   Z-ADD     1             L                 2 0
     C*
     C     F1KY          CHAIN     PF101                              60
     C     EXKY          CHAIN     PEX01                              60
     C     TBKY          CHAIN     PBS01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     BSSEFL        ANDEQ     'E'
     C*
     C* SELECT BILLING PLAN TYPE.
     C*
     C                   SELECT
     C*
     C* MONTHLY PLANS
     C     BSDOWK        WHENEQ    *BLANKS
     C     BSFREQ        ANDNE     'T'
     C                   EXSR      SRMTH
     C*
     C* TWICE MONTHLY PLANS
     C     BSDOWK        WHENEQ    *BLANKS
     C     BSFREQ        ANDEQ     'T'
     C                   EXSR      SRTWM
     C*
     C* WEEKLY PLANS
     C     BSDOWK        WHENNE    *BLANKS
     C                   EXSR      SRWEK
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*****************************************************************
     C* MOVE RESULTS INTO PARM FIELDS.
     C*
     C                   MOVE      XXTYPE        QQTYPE
     C*
     C                   MOVE      WD1           QQDA1
     C                   MOVE      WD2           QQDA2
     C                   MOVE      WD3           QQDA3
     C*
     C                   MOVE      WA1           QQAM1
     C                   MOVE      WA2           QQAM2
     C                   MOVE      WA3           QQAM3
     C*
     C                   MOVE      *ON           *INLR
     C*****************************************************************
     C*  PROCESS MONTHLY BILLING SCHEDULE CODES.
     C*
     C     SRMTH         BEGSR
     C*
     C                   MOVE      'M'           XXTYPE
     C*
     C* BUILD MONTHLY DATE ARRAY.
     C*
     C                   Z-ADD     XXBSDT        XXWKDT
     C     1             DO        12            X                 2 0
     C                   Z-ADD     XXWKDT        WKD(X)
     C                   ADD       1             MM
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF                                                  MM IFGT
     C                   MOVE      '01'          DD
     C                   ENDDO                                                  DO 12 X
     C*
     C                   MOVE      XXBSDT        XXWKDT
     C                   MOVEL     XXDATE        YYMM2             5 0
     C*
     C* BUILD INSTALLMENT AMOUNTS ARRAY.
     C*
     C     1             DO        12            X
     C     AB(X)         MULT(H)   XXPREM        XXAMT
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT         XXWK10
     C                   Z-ADD     XXWK10        XXAMT
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT         XXAMT2
     C     YYMM          IFGE      YYMM2
      *** BEG CHG ***** 08/31/10 *************************************
     C***********          Z-ADDXXAMT2    WKA,L
     C***********          ADD  1         L
     C                   SELECT
     C     X1TYPE        WHENEQ    'R'
     C                   Z-ADD     XXAMT2        WKA(X)
     C                   OTHER
     C                   Z-ADD     XXAMT2        WKA(L)
     C                   ADD       1             L
     C                   ENDSL
      *** END CHG ***** 08/31/10 *************************************
     C                   Z-ADD     *ZEROS        XXAMT2
     C                   ENDIF                                                  YYMM IFGE
     C*
     C                   ADD       1             MM
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF                                                  MM > 12
     C*
     C                   ENDDO                                                  DO X 12
     C*
     C* ENSURE TOTAL OF INSTALLMENTS EQUALS TOTAL PREMIUM.
     C*
     C                   XFOOT     WKA           XXCOMP           15 4
     C*
     C     XXCOMP        IFNE      XXPREM
     C     XXPREM        SUB       XXCOMP        XXDIF            15 4
     C                   Z-ADD     1             X
     C                   MOVE      '0'           *IN60
     C*
     C     X             DOWLE     12
     C     *IN60         ANDEQ     '0'
     C*
     C     WKA(X)        IFNE      0
     C                   ADD       XXDIF         WKA(X)
     C                   MOVE      '1'           *IN60
     C                   ENDIF                                                  AM,X IFNE
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C  N60              ADD       XXDIF         WKA(1)
     C                   ENDIF                                                  XXCOMP IFNE
     C*
     C                   ENDSR                                                  SRMTH SUBR
     C*****************************************************************
      * PROCESS TWICE A MONTH BILLLING INSTALLMENTS.
      *
     C     SRTWM         BEGSR
     C*
     C                   MOVE      'T'           XXTYPE
     C*
     C                   MOVE      XXBSDT        XXWKDT
     C*
     C* CHECK FOR FIRST INSTALLMENT ON/AFTER BILLING START DATE.
     C*
     C     DD            IFEQ      1
     C     DD            OREQ      15
     C                   Z-ADD     XXBSDT        WKD(1)
     C                   ELSE
     C*
     C     DD            IFLT      15
     C                   Z-ADD     15            DD
     C                   Z-ADD     XXWKDT        WKD(1)
     C                   ELSE
     C*
     C                   Z-ADD     1             DD
     C                   ADD       1             MM
     C     MM            IFEQ      13
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF
     C*
     C                   Z-ADD     XXWKDT        WKD(1)
     C                   ENDIF                                                  DD IFLT
     C*
     C                   ENDIF                                                  DD IFEQ
     C*
     C                   Z-ADD     1             T                 3 0
     C*
     C     WKD(1)        IFGE      XXPTDT
     C                   Z-ADD     XXBSDT        WKD(1)
     C                   ELSE
     C*
     C* BUILD DATE ARRAY WITH EACH TWICE MONTHLY INSTALLMENT DATE.
     C*
     C     XXWKDT        DOWLT     XXPTDT
     C*
     C     DD            IFEQ      1
     C                   Z-ADD     15            DD
     C                   ELSE
     C*
     C                   Z-ADD     1             DD
     C                   ADD       1             MM
     C     MM            IFEQ      13
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF
     C*
     C                   ENDIF                                                  DD IFEQ
     C*
     C     XXWKDT        IFLT      XXPTDT
     C                   ADD       1             T
     C                   Z-ADD     XXWKDT        WKD(T)
     C                   ENDIF                                                  XXWKDT IFLT
     C*
     C                   ENDDO                                                  XXWKDT DOWLT
     C*
     C                   ENDIF                                                  WKD,1 IFGE
     C*
     C* CALCULATE EACH INSTALLMENT AMOUNT.
     C*
     C     XXPREM        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKA(U)
     C                   ENDDO
     C*
     C* ENSURE TOTAL OF INSTALLMENTS EQUALS TOTAL PREMIUM.
     C*
     C                   XFOOT     WKA           XXCOMP           15 4
     C     XXCOMP        IFNE      XXPREM
     C     XXPREM        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKA(1)
     C                   ENDIF
     C*
     C                   ENDSR                                                  SRTWM SUBR
     C*****************************************************************
     C*  PROCESS WEEKLY BILLING SCHEDULE CODES.
     C*
     C     SRWEK         BEGSR
     C*
     C                   MOVE      'W'           XXTYPE
     C*
     C                   Z-ADD     XXPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     XXPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C*
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   EXSR      SRLEAP
     C*
     C* TOTAL DAYS IN POLICY
     C*
     C                   Z-ADD     QQDAYS        TDAYS             5 0
     C*
     C                   Z-ADD     XXBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTE
     C*
     C                   MOVE      *BLANK        SRSDAY
     C                   MOVE      *BLANK        SRLDAY
     C                   MOVE      *BLANK        SRERR
     C*
     C                   CALL      'SRDAY8'
     C                   PARM                    SRDTE             8 0          M/D/YY
     C                   PARM                    SRSDAY            4            Abbreviation
     C                   PARM                    SRLDAY            9            Full Day Name
     C                   PARM                    SRERR                          Error Flag
     C*
     C                   MOVEL     SRSDAY        XXDAY             2
     C                   Z-ADD     0             XXCNT             3 0
     C*
     C* CHECK FOR FIRST DAY OF WEEK ON/AFTER BILLING START DATE.
     C*
     C     XXDAY         IFEQ      BSDOWK
     C                   Z-ADD     XXBSDT        WKD(1)
     C                   ELSE
     C*
     C                   Z-ADD     0             X1                3 0
     C                   Z-ADD     0             X2                3 0
     C                   Z-ADD     1             T                 3 0
     C*
     C     XXDAY         LOOKUP    DAY(T)                                 60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     T             X1
     C                   ENDIF                                                  *IN60
     C*
     C     BSDOWK        LOOKUP    DAY(T)                                 60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     T             X2
     C                   ENDIF                                                  *IN60
     C*
     C     X2            SUB       X1            XXCNT
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      SRDTE         QQDT              8 0
     C                   PARM      XXCNT         QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTE
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKD(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXDAY IFEQ
     C*
     C     WKD(1)        IFGE      XXPTDT
     C                   Z-ADD     XXBSDT        WKD(1)
     C                   ENDIF
     C*
     C                   Z-ADD     1             T                 3 0
     C                   ADD       7             XXCNT
     C*
     C* BUILD DATE ARRAY WITH EACH WEEKLY INSTALLMENT DATE.
     C*
     C     XXCNT         DOWLT     TDAYS
     C*
     C                   ADD       1             T
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      SRDTE         QQDT              8 0
     C                   PARM      7             QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTE
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         WKD(T)
     C                   ENDIF
     C*
     C                   ADD       7             XXCNT
     C                   ENDDO                                                  XXCNT IFLT
     C*
     C* CALCULATE EACH WEEKLY INSTALLMENT AMOUNT.
     C*
     C     XXPREM        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKA(U)
     C                   ENDDO
     C*
     C* ENSURE TOTAL OF INSTALLMENTS EQUALS TOTAL PREMIUM.
     C*
     C                   XFOOT     WKA           XXCOMP           15 4
     C*
     C     XXCOMP        IFNE      XXPREM
     C     XXPREM        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKA(1)
     C                   ENDIF
     C*
     C                   ENDSR                                                  SRWEK SUBR
     C*****************************************************************
     C*
     C* SUBROUTINE TO DETERMINE IF A DATE RANGE CONTAINS FEBRUARY 29TH.
     C*
     C     SRLEAP        BEGSR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FROM            7 0
     C*
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TO              7 0
     C*
     C                   Z-ADD     X1FROM        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      X1FROM
     C                   Z-ADD     X1TO          XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      X1FROM
     C     XXLPDT        ORGT      X1TO
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     SRERR         IFNE      '1'
     C     XXLPDT        ANDNE     X1FROM
     C                   SUB       1             QQDAYS
     C                   ELSE
     C                   Z-ADD     0             XXLPDT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  SRLEAP
     C*****************************************************************
** DAYS OF WEEK FOR ARRAY                                              ***
MOTUWETHFRSASUMOTUWETHFRSA
