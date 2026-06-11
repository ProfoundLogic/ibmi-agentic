     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  PROGRAM SUBROUTINE SRUEPR - UNEARNED PREMIUM TO DATE SUBPGM
     H*****************************************************************
     H*
     H*  THIS SUB-PROGRAM WILL CALCULATE THE UNEARNED PREMIUM TO DATE.
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 03/10/04 C:09 PROJECT: SUPP  WA025/SEQ# 41681                 *
     H*               CORRECT CALCULATION OF UNEARNED PREMIUM         *
     H*               INVOLVING LEAP YEARS.                           *
     H*                                                               *
     H* 02/15/99 C:09 PROJECT: SUPP  WA025/SEQ# 16596                 *
     H*               CORRECT CALCULATION OF UNEARNED PREMIUM FOR     *
     H*               NON-COMMERICAL GROUPS.                          *
     H*                                                               *
     H* 02/10/99 C:09 PROJECT: SUPP  WA048/SEQ# 16514                 *
     H*               EXPAND XXWK92/XXWKPR TO 11.2 TO HANDLE "LARGE"  *
     H*               OFFSETTING A/R ENTRIES.                         *
     H*                                                               *
     H* 08/25/98 C:09 PROJECT SUPPORT  WA093  SEQ# 13075              *
     H*               UNEARNED PREMIUM EQUALS ZERO IF THE RUN DATE    *
     H*               (AS OF DATE) IS GREATER THAN ELPTDT             *
     H*                                                               *
     H* 07/07/98 C:65 PROJECT SUPPORT                                 *
     H*               USE PRORATED NORMAL FOR UNEARNED CALCULATIONS.  *
     H*                                                               *
     H* 04/24/98 C:78 PROJECT WC745                                   *
     H*               CORRECT PROJ DONE ON 09/08/97. F2CT01 REPLACED  *
     H*               WITH WMFNP/FNCGFL.                              *
     H*                                                               *
     H**** ADD NEW REVISIONS ON TOP ***********************************
     H*                                                               *
     H* 05/23/91 JJJ  ACCOUNT FOR CANCEL DT LESS THAN FYR START DATE. *
     H*               PREMIUM IS NOT EARNED FOR 1 DAY IF CANCEL DATE  *
     H*               EQUALS THE START DATE.                          *
     H*                                                               *
     H* 01/02/92 JJJ  PROJECT WC109                                   *
     H*               ACCOUNT FOR LEAP YEAR.                          *
     H*                                                               *
     H* 08/27/93 C:10 PROJECT WC260                                   *
     H*               CHANGED CANCEL DATE PROCESSING BECAUSE THE      *
     H*               CANCEL DATES WEREN'T BEING IDENTIFIED           *
     H*               CORRECTLY.                                      *
     H*                                                               *
     H* 11/20/95 C:33 PROJECT CM010                                   *
     H*               ON ESTIMATING ACCOUNTS, BE SURE TO PRORATE      *
     H*               PROPERLY FOR POLICIES WITH NON-COMMON           *
     H*               EXPIRATION DATES.                               *
     H*                                                               *
     H* 08/15/96 C:36 PROJECT WC270                                   *
     H*               FIX NUMBER OF DAYS IN FEBRUARY IN LEAP YEAR TO  *
     H*               BE 28.                                          *
     H*                                                               *
     H* 09/08/97  C:78  PROJ:WC745                                    *
     H*                 F2CT01 REPLACED WITH WMFNP/FNCGFL             *
     H*                                                               *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     F**** BEGIN DEL ******* 04/24/98 *********************************
     F**** WDF2L   IF  E           K        DISK
     F**** END   DEL ******* 04/24/98 *********************************
      ****** BEGIN ADD ***** 09/08/97 *********************************
     FWMFNL     IF   E           K DISK
      ****** END   ADD ***** 09/08/97 *********************************

     D*
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CN                     1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0

     C*
     C* * * EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* * * EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C* * * FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* * * BILLING SCHEDULE TABLE FILE
     C*
     C     BSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXWK10           10
     C***** BEGIN DEL ***** 04/24/98 **********************************
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C****       F2KY      KLIST
     C****                 KFLD           XXCO#
     C****                 KFLD           XXFND
     C***** END   DEL ***** 04/24/98 **********************************
      ******* BEGIN ADD ***** 09/08/97 ********************************
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      ******* END   ADD ***** 09/08/97 ********************************
     C*
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDT          XXDATE
     C     *LIKE         DEFINE    ELDT          X1DT01
     C     *LIKE         DEFINE    ELDT          X1DT02
     C     *LIKE         DEFINE    ELDT          XXLPDT
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRCO#             3
     C                   PARM                    SRFND             3
     C                   PARM                    SRFYR             3
     C                   PARM                    SREMP#            9
     C                   PARM                    SRDIV             5
     C                   PARM                    SRDATE            8            COMES IN M/D/Y
     C                   PARM                    SRAMT            11            UNEARNED PRM
     C                   PARM                    SRTERM            1            SET LR FLAG.
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
     C                   MOVE      SRCO#         XXCO#             3 0
     C                   MOVE      SRFND         XXFND             3 0
     C                   MOVE      SRFYR         XXFYR             3 0
     C                   MOVE      SREMP#        XXEMP#
     C                   MOVE      SRDIV         XXDIV
     C                   MOVE      SRDATE        X1DATE            8 0
     C*
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
      ***** BEGIN ADD **** 09/08/97 ***********************************
     C*
     C*  GET THE FUND MASTER TO GET THE FNCGFL FLAG
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        XFNAME           60
     C**** BEGIN ADD ******* 04/24/98 *********************************
     C                   MOVE      *BLANK        FNCGFL
     C**** END   ADD ******* 04/24/98 *********************************
     C                   ELSE
     C                   MOVEL     FNNAME        XFNAME           60
     C                   END
      ***** END   ADD **** 09/08/97 ***********************************
     C**** BEGIN DEL ******* 04/24/98 *********************************
     C****                 MOVE *BLANK    XXFCT1  1
     C*****      F2KY      CHAINPF201                60
      ****** BEGIN CHG ******* 09/08/97 *******************************
     C**N60****************MOVELF2CT01    XXFCT1
     C**N60*****           MOVELFNCGFL    XXFCT1
      ****** END   CHG ******* 09/08/97 *******************************
     C**** END   DEL ******* 04/24/98 *********************************
     C*
     C* * * GET EMPLOYER DETAIL RECORD
     C     ELKEY         CHAIN     PEL01                              40
     C     *IN40         IFEQ      '1'
     C                   MOVE      *ZERO         SRAMT
     C                   ELSE
     C*
     C* * * IF THE FINAL AUDIT FLAG(ELFAF) IS 'Y', THIS EMPL. HAS GONE
     C* * * THRU THE FINAL AUDIT.
     C     ELFAF         IFEQ      'A'
     C**** BEGIN ADD ******* 08/25/98 *********************************
     C*
     C*  IF RUN DATE (AS OF DATE) IS GREATER THAN POLICY TO DATE
     C*  UNEARNED IS ZERO
     C*
     C     XXDATE        ORGT      ELPTDT
     C**** END ADD ********* 08/25/98 *********************************
     C                   MOVE      *ZERO         SRAMT
     C                   ELSE
     C*
     C* * * GET BILL SCHEDULE FILE TABLE RECORD TO DETERMINE TYPE OF EMPL.
     C* * * 'S' = SELF REPORTING 'E' = ESTIMATED.
     C                   MOVEL     ELBSCD        XXWK10
     C     BSKY1         CHAIN     PBS01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  IF EMPLOYER IS SELF-REPORTING, THERE WILL NO UNEARNED PREM
     C*
     C     BSSEFL        IFEQ      'S'
     C                   MOVE      *ZERO         SRAMT
     C                   ELSE
     C*
     C*  IF EMPLOYER IS CANCELLED, UNEARNED WILL BE ZERO.
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   MOVE      'N'           XXCANC            1
     C*
     C*   FIND THE EFFECTIVE DATE
     C*
     C                   Z-ADD     1             X                 1 0
     C     *ZERO         LOOKUP    EF(X)                                  50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     5             X
     C                   ELSE
     C                   SUB       1             X
     C                   END
     C*
     C*   CHECK CANCELLED DATE
     C*
     C     CN(X)         IFLT      XXDATE
     C     CN(X)         ANDNE     0
     C                   MOVE      'Y'           XXCANC
     C                   MOVE      *ZERO         SRAMT
     C                   END
     C*
     C                   ELSE
     C                   MOVE      *ZERO         SRAMT
     C                   MOVE      'Y'           XXCANC
     C                   END
     C*
     C*  IF EMPLOYER IS NOT AUDITED, CANCELLED, OR SELF-REPORTING,
     C*  CONTINUE.
     C*
     C*  UNEARNED = ESTIMATED ANNUAL PREMIUM DIVIDED BY DAYS IN
     C*             FUND YEAR TIMES DAYS REMAINING IN FUND YEAR.
     C*
     C     XXCANC        IFEQ      'N'
     C*
     C* * * GET THE FUND DETAIL RECORD TO DETERMINE THE FROM/TO POLICY
     C* * * POLICY DATES.
     C     F1KEY         CHAIN     PF101                              41
     C*
     C*  IF THIS IS A NON-COMMON EXPIRATION DATE POLICY,
     C*  FUND DATES WILL BE ELPFDT AND ELPTDT = 365 DAYS
     C*
     C**** BEGIN CHG **** 04/24/98 ************************************
     C*******    XXFCT1    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
     C**** END   CHG **** 04/24/98 ************************************
     C                   Z-ADD     ELPFDT        F1STPO
     C                   Z-ADD     ELPTDT        F1ENPO
     C                   END
     C*
     C* * * FIND THE NUMBER OF DAYS BETWEEN THE FUND DATES
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01            8 0
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02            8 0
     C*
     C                   MOVE      XXDT01        XXWK6             8 0
     C                   MOVE      XXDT02        XXW16             8 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXWK6
     C                   PARM                    XXW16
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C*
     C                   Z-ADD     XXWK50        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXWK50
     C*
     C                   Z-ADD     XXWK50        XXFDAY            5 0
     C*
     C*  FIND NUMBER OF DAYS REMAINING IN YEAR
     C*
     C                   MOVE      *ZERO         XXWK92           11 2
     C                   MOVE      *ZERO         XXWK50            5 0
     C                   MOVE      *ZERO         XXANPR           11 2
     C*
     C                   Z-ADD     X1DATE        XXDT01
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*
     C                   MOVE      XXDT01        XXWK6
     C                   MOVE      XXDT02        XXW16
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXWK6
     C                   PARM                    XXW16
     C                   PARM                    XXWK50
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C                   Z-ADD     0             XXRDAY
     C     XXWK50        IFNE      0
     C     XXWK50        SUB       1             XXRDAY            5 0          |
     C                   END
     C*
     C                   Z-ADD     XXRDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXRDAY
     C*
     C*  IF THE LEAPYR DATE WHICH IS 02/29 IS EQUAL TO THE RUN DATE
     C*  THEN WE DO NOT WANT TO SUBTRACT 1 DAY FROM THE DAYS REMAINING
     C*
     C*** BEGIN CHG ***  03/10/04  ************************************
     C***********XXLPDT****IFEQ XXDT01
     C     XXLPDT        IFEQ      XXDATE
     C*** END CHG *****  03/10/04  ************************************
     C                   ADD       1             XXRDAY
     C                   END
     C*** BEGIN ADD ***  07/07/98  ************************************
     C*** BEGIN ADD ***  02/15/99  ************************************
     C     FNCGFL        IFEQ      'Y'
     C*** END ADD *****  02/15/99  ************************************
     C                   MULT(H)   ELPROR        ELENMP
     C*** BEGIN ADD ***  02/15/99  ************************************
     C                   END
     C*** END ADD *****  02/15/99  ************************************
     C*** END ADD *****  07/07/98  ************************************
     C*
     C*  NOW CALCULATE THE UNEARNED PREMIUM
     C*
     C     XXFDAY        IFNE      0
     C     ELENMP        DIV(H)    XXFDAY        XXWK92
     C     XXWK92        MULT      XXRDAY        XXANPR
     C                   ELSE
     C                   Z-ADD     0             XXANPR
     C                   END
     C*
     C                   MOVE      XXANPR        SRAMT
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C     SRTERM        IFEQ      '1'
     C                   RETURN
     C                   ELSE
     C                   MOVE      '1'           *INLR
     C                   END
     C*
     C     LEAPYR        BEGSR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     0             XXLPDT
     C                   Z-ADD     XXDT01        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DT01
     C*
     C                   Z-ADD     XXDT02        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DT02
     C*
     C                   Z-ADD     X1DT01        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      X1DT01
     C                   Z-ADD     X1DT02        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      X1DT01
     C     XXLPDT        ORGT      X1DT02
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             XXWKDY            5 0
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
