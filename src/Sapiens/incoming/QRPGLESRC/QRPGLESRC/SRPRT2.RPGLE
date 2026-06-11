     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  PROGRAM SUBROUTINE SRPRT2 - EARNED PREMIUM TO DATE SUBPROGRAM
     H*****************************************************************
     H*
     H*  THIS SUB-PGM WILL CALCULATE THE FINANCIAL EARNED PREM TO DATE.
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/08/15-B C:180 SUPPORT: PHW28940                            *
     H*                  Return restrospective premium when applicable.
     H*                                                               *
     H* 06/08/05 C:65 PROJECT: 135WCE/WC1288                          *
     H*               WHEN CHECKING A/R FOR SUMMARIZED S/R EMPLRS,    *
     H*               CHECK THE STATE REPORTING DIVISION INSTEAD.     *
     H*                                                               *
     H* 02/10/99 C:09 PROJECT: SUPP  WA048/SEQ# 16514                 *
     H*               EXPAND XXWK92/XXWKPR TO 11.2 TO HANDLE "LARGE"  *
     H*               OFFSETTING A/R ENTRIES.                         *
     H*                                                               *
     H* 07/14/98 C:09 PROJECT: SUPP  WA095/SEQ# 11561                 *
     H*               APPLY CHANGES SIMILAR TO THOSE ALREADY MADE IN  *
     H*               S/R EARNED PREMIUM SECTION TO NOT INCLUDE CANCEL*
     H*               DATE AS PART OF EARNED PREMIUM.                 *
     H*                                                               *
     H* 04/24/98 C:78 PROJECT WC745                                   *
     H*               CORRECT PROJ DONE ON 09/08/97. F2CT01 REPLACED  *
     H*               WITH WMFNP/FNCGFL.                              *
     H*                                                               *
     H* 03/05/98 C:35 PROJECT: WC577                                  *
     H*               SRCVTDAT WAS BLOWING UP ON THE LEAP YEAR CHECK  *
     H*               BECAUSE THE WRONG PARAMETER LIST WAS BEING      *
     H*               USED.                                           *
     H*                                                               *
     H* 02/02/98 C:09 PROJECT WC577                                   *
     H*               AVOID CALCULATION OF A NEGATIVE EARNED PREMIUM. *
     H*               DO NOT PROCESS POLICY IF EFFECTIVE DATE IS      *
     H*               GREATER THAN "AS OF" DATE FOR EARNED PREMIUM.   *
     H*                                                               *
     H* 10/20/97 C:77 PROJECT WC577                                   *
     H*               IF POLICY HAS EXPIRED PRIOR TO SRDATE, USE      *
     H*               POLICY EXPIRATION DATE INSTEAD OF SRDATE.       *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H* 11/05/90 KWK  CHANGED CALCULATIONS TO USE EMPLOYERS ANNUAL    *
     H*               PREMIUM (POSSIBLY PRORATED) LESS THE EXPENSE    *
     H*               CONSTANT TO CALCULATE EARNED PREMIUM            *
     H*                                                               *
     H* 11/20/90 JJJ  USE ELPRTD INSTEAD OF F1EXCO                    *
     H*                                                               *
     H* 05/22/91 JJJ  CHECK FOR LEAP YEAR.  MAX DAYS = 365            *
     H*                                                               *
     H* 05/28/91 JJJ  ACCOUNT FOR CANCEL DT LESS THAN FYR START DATE. *
     H*               PREMIUM IS NOT EARNED FOR 1 DAY IF CANCEL DATE  *
     H*               EQUALS THE START DATE.
     H*                                                               *
     H* 08/06/92 KWK  PROJECT WC159                                   *
     H*               WHEN DETERMINING XXANPR WE NEED TO INCLUDE 'V'  *
     H*               TYPES ALONG WITH 'P', 'R', AND 'N' TYPES.       *
     H*                                                               *
     H* 11/28/95 C:33 PROJECT CM010                                   *
     H*               ON ESTIMATING ACCOUNTS, BE SURE TO PRORATE      *
     H*               PROPERLY FOR POLICIES WITH NON-COMMON           *
     H*               EXPIRATION DATES.                               *
     H*                                                               *
     H* 06/05/96 C:36 PROJECT WC270                                   *
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
     FWMEAL6    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     F***** BEGIN DEL ***** 04/24/98 **********************************
     F***** WDF2L   IF  E           K        DISK
     F***** END   DEL ***** 04/24/98 **********************************
      ****** BEGIN ADD ***** 09/08/97 *********************************
     FWMFNL     IF   E           K DISK
      ****** END   ADD ***** 09/08/97 *********************************

     D*
     D                 DS
     D  XXTODT                 1      7  0
     D  XXTOYR                 2      3  0
     D  XXTOMO                 4      5  0
     D  XXTOYM                 1      5  0
     D*
     D                 DS
     D  XXFRDT                 1      7  0
     D  XXFRYR                 2      3  0
     D  XXFRMO                 4      5  0
     D  XXFRYM                 1      5  0
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
     D                 DS
     D  XXDAT1                 1      7  0
     D  XXDAY1                 6      7  0
     D                 DS
     D  XXSTPO                 1      7  0
     D  XXYYSP                 1      3  0
     D  XXMMSP                 4      5  0
     D  XXMYSP                 1      5  0
     D  XXDYSP                 6      7  0
     D                 DS
     D  XXENPO                 1      7  0
     D  XXYYEP                 1      3  0
     D  XXMMEP                 4      5  0
     D  XXMYEP                 1      5  0
     D  XXDYEP                 6      7  0
     D                 DS
     D  XXENDM                 1      7  0
     D  XXYYEM                 1      3  0
     D  XXMMEM                 4      5  0
     D  XXMYEM                 1      5  0
     D  XXDDEM                 6      7  0
     D                 DS
     D  EAPODT                 1      7  0
     D  XXPOYM                 1      5  0
     D                 DS
     D  XXDATE                 1      7  0
     D  XXYM                   1      5  0

     C*
     C* * * EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*** BEGIN ADD ***  06/08/05  ************************************
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*** END ADD *****  06/08/05  ************************************
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
     C*
     C* * * TABLE FILE KEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C* * * A/R BILLING RECORDS
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*** BEGIN CHG ***  06/08/05  ************************************
     C*********            KFLD           XXDIV
     C                   KFLD                    X1DIV             5 0
     C*** END CHG *****  06/08/05  ************************************
     C                   KFLD                    XXFYR
     C                   KFLD                    XXPODT            7 0
     C*
     C     EAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*** BEGIN CHG ***  06/08/05  ************************************
     C*********            KFLD           XXDIV
     C                   KFLD                    X1DIV
     C*** END CHG *****  06/08/05  ************************************
     C                   KFLD                    XXFYR
     C****** BEGIN DEL ******* 04/24/98 *******************************
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C****       F2KY      KLIST
     C****                 KFLD           XXCO#
     C****                 KFLD           XXFND
     C****** END   DEL ******* 04/24/98 *******************************
      ******* BEGIN ADD ***** 09/08/97 ********************************
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      ******* END   ADD ***** 09/08/97 ********************************
     C*
     C     *LIKE         DEFINE    EAEMP#        XXEMP#
     C     *LIKE         DEFINE    EADIV         XXDIV
     C     *LIKE         DEFINE    EADT          X1DT01
     C     *LIKE         DEFINE    EADT          X1DT02
     C*
     C     *ENTRY        PLIST
     C                   PARM                    SRCO#             3
     C                   PARM                    SRFND             3
     C                   PARM                    SRFYR             3
     C                   PARM                    SREMP#            9
     C                   PARM                    SRDIV             5
     C                   PARM                    SRDATE            8            COMES IN M/D/Y
     C                   PARM                    SRAMT            11            ERN PRM ALL
     C                   PARM                    SRAMT2           11            ERN PRM 1 MTH
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
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
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*** BEGIN ADD ***  06/08/05  ************************************
     C                   Z-ADD     XXDIV         XSDIV             5 0
     C*
     C     EMKEY1        CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C  N60EMKEY1        READE     PEM01                                  60
     C                   ENDDO
     C*** END ADD *****  06/08/05  ************************************
     C*
     C* * * GET THE FUND DETAIL RECORD TO DETERMINE THE FROM/TO POLICY
     C* * * POLICY DATES.
     C     F1KEY         CHAIN     PF101                              41
     C*
     C* * * GET THE EMPLOYER MASTER RECORD
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C*
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
      **** BEGIN ADD **** 09/08/97 ************************************
     C*  GET THE FUND MASTER TO LOAD THE FNCGFL FLAG
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      '1'
     C                   MOVE      *BLANK        XFNAME           60
     C**** BEGIN ADD **** 04/24/98 ************************************
     C                   MOVE      *BLANK        FNCGFL
     C**** END   ADD **** 04/24/98 ************************************
     C                   ELSE
     C                   MOVEL     FNNAME        XFNAME           60
     C                   END
     C*
      **** END   ADD **** 09/08/97 ************************************
     C*
     C***** BEGIN DEL ****** 04/24/98 *********************************
     C*******              MOVE *BLANK    XXFCT1  1
     C*******    F2KY      CHAINPF201                60
      **** BEGIN CHG ***** 09/08/97 ***********************************
     C**N60****************MOVELF2CT01    XXFCT1
     C**N60**              MOVELFNCGFL    XXFCT1
      **** END   CHG ***** 09/08/97 ***********************************
     C***** END   DEL ****** 04/24/98 *********************************
     C*
     C* * * GET EMPLOYER DETAIL RECORD
     C     ELKEY         CHAIN     PEL01                              40
     C     *IN40         IFEQ      '1'
     C*** BEGIN ADD *** 02/02/98 *******************************************
     C     ELPFDT        ORGT      XXDATE
     C*** END ADD ***** 02/02/98 *******************************************
     C                   MOVE      *ZERO         SRAMT
     C                   MOVE      *ZERO         SRAMT2                                     H
     C                   ELSE
      *** BEG ADD ***** 06/08/15-B ***********************************
     C* USE RETROSPECTIVE PREMIUM IF AVAILABLE, NET OF EXPENSE CONST.
     C                   IF        ELSTAM > *zero
     C     ELSTAM        SUB       ELPRTD        ELSTAM
     C                   MOVE      ELSTAM        SRAMT
     C                   MOVE      *ZEROS        SRAMT2
     C                   GOTO      RETURN
     C                   ENDIF
      *** END ADD ***** 06/08/15-B ***********************************
     C     ELANPR        SUB       ELPRTD        ELANPR
     C     ELANPR        IFLT      *ZERO
     C                   Z-ADD     0             ELANPR
     C                   END
     C*
     C* * * IF THE FINAL AUDIT FLAG(ELFAF) IS 'Y', THIS EMPL. HAS GONE
     C* * * THRU THE FINAL AUDIT AND WE CAN USE THE ANNUAL AUDITED AMOUNT.
     C     ELFAF         IFEQ      'A'
     C*
     C*  GET THE FUND DETAIL RECORD TO DETERMINE THE EXPENSE CONSTANT
     C*
     C     F1KEY         CHAIN     PF101                              41        SAJ 5-9-88
     C*
     C*  CALCULATE NORMAL PREMIUM BY BACKING OUT EXPENSE CONSTANT
     C*
     C                   MOVE      ELAAPR        SRAMT
     C     ELAAPR        SUB       ELPRTD        ELAAPR
     C     ELAAPR        IFLT      *ZERO
     C                   Z-ADD     0             ELAAPR
     C                   END
     C                   MOVE      ELAAPR        SRAMT
     C                   MOVE      *ZEROS        SRAMT2
     C                   GOTO      RETURN
     C                   END
     C*
     C* * * SAVE THE BILLING SCHEDULE CODE FOR PROCESSING LATER.
     C*
     C                   MOVEL     ELBSCD        XXWK10
     C*
     C* * * DETERMINE OUR DATES, USE THE FUND POLICY FROM DATE
     C* * * AND THE FUND POLICY TO DATE, OR THE POLICY DATES FROM
     C* * * THE EMPLOYER DETAIL FILE.
     C*
     C     F1STPO        IFLT      ELPFDT
     C                   Z-ADD     ELPFDT        F1STPO
     C                   END
     C*
     C     F1ENPO        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        F1ENPO
     C                   END
     C*
     C                   MOVEL     F1STPO        XXSAVE            5 0
     C*
     C*
     C*  IF THIS IS A NON-COMMON EXPIRATION DATE POLICY,
     C*  FUND DATES WILL BE ELPFDT AND ELPTDT = 365 DAYS
     C*
     C****** BEGIN CHG ***** 04/24/98 *********************************
     C*****      XXFCT1    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
     C****** END   CHG ***** 04/24/98 *********************************
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
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C                   Z-ADD     XXWK50        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXWK50
     C*
     C                   Z-ADD     XXWK50        XXFDAY            5 0
     C*
     C* * * CLEAN UP
     C                   MOVE      *ZERO         XXWK92           11 2
     C                   MOVE      *ZERO         XXWK50
     C                   MOVE      *ZERO         XXANPR
     C                   MOVE      *ZERO         XXMNPR
     C                   MOVE      *ZERO         XXEDAY
     C                   MOVE      *ZERO         XXMDAY
     C*
     C* * * GET BILL SCHEDULE FILE TABLE RECORD TO DETERMINE TYPE OF EMPL.
     C* * * 'S' = SELF REPORTING 'E' = ESTIMATED.
     C     BSKY1         CHAIN     PBS01                              60
     C     *IN60         IFEQ      '0'
     C     BSSEFL        IFEQ      'E'
     C*
     C* * * IF THIS EMPLOYER IS ESTIMATED THEN WE NEED TO FIND THE
     C* * * NUMBER OF ACTIVE DAYS BETWEEN THE POLICY FROM DATE AND
     C* * * THE PASSED DATE, IF IT IS GREATER THAN THE DAYS IN THE
     C* * * POLICY, THEN USE THE FULL ESTIMATED PREMIUM
     C*
     C     1             DO        5             X1                5 0
     C*
     C     EF(X1)        IFGE      F1STPO
     C     EF(X1)        ANDLE     XXDATE
     C*
     C* * *  CHECK FOR A CANCELLATION
     C*
     C     CN(X1)        IFNE      *ZERO
     C     CN(X1)        IFLE      XXDATE
     C*
     C* * *  CALCULATE THE DIFFERENCE BETWEEN THE START DATE
     C* * *  AND THE END DATE
     C*
     C                   Z-ADD     EF(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C*
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXEDAY            5 0
     C*** BEGIN DEL *** 07/14/98 *******************************************
     C*
     C*  DO NOT INCLUDE CX DATE AS PART OF EARNED PREMIUM
     C*
     C***********CN,X1*****IFNE EF,X1
     C*********************ADD  1         XXEDAY
     C*********************END
     C*** END DEL ***** 07/14/98 *******************************************
     C                   Z-ADD     XXEDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXEDAY
     C                   END
     C                   END
     C*
     C                   ELSE
     C*
     C* * * IF THERE IS NO CANCELLATION THEN WE CALCULATE BETWEEN
     C* * * THE FROM DATE TO THE END DATE
     C                   Z-ADD     EF(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C*
     C*** BEGIN CHG *** 10/20/97 **************************************
     C******************** Z-ADDXXDATE    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     XXDT02
     C     ELPTDT        IFLT      XXDATE
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*
     C                   ELSE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*
     C                   ENDIF                                                  ELPTDT<SRDTO
     C*
     C                   MOVEL     SRDTO         XXDT02
     C*** END CHG ***** 10/20/97 **************************************
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXEDAY            5 0
     C                   ADD       1             XXEDAY
     C                   Z-ADD     XXEDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXEDAY
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   END
     C*
     C* * * IF WE DIDN'T HAVE A START
     C* * * DATE DURING OUR LOOP, CHECK FOR A CANCELLATION
     C*
     C     1             DO        5             X1                5 0
     C*
     C     CN(X1)        IFGE      F1STPO
     C     EF(X1)        ANDLT     F1STPO
     C     CN(X1)        IFLE      XXDATE
     C*
     C* * * CALCULATE THE DIFFERENCE BETWEEN THE START DATE AND
     C* * * THE CANCELLATION DATE
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C*
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXEDAY            5 0
     C*** BEGIN DEL *** 07/14/98 *******************************************
     C*
     C*  DO NOT INCLUDE CX DATE AS PART OF EARNED PREMIUM
     C*
     C***********CN,X1*****IFNE F1STPO
     C*********************ADD  1         XXEDAY
     C*********************END
     C*** END DEL ***** 07/14/98 *******************************************
     C                   Z-ADD     XXEDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXEDAY
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   END
     C*
     C     XXEDAY        IFEQ      *ZERO
     C*
     C* * * WE NEED TO FIND A EF DATE BEFORE THE F1STPO, AND
     C* * * A CN DATE AFTER THE CURRENT DATE
     C*
     C                   MOVE      '0'           XXPOL             1
     C     1             DO        5             X1
     C     EF(X1)        IFLE      F1STPO
     C     EF(X1)        ANDNE     0
     C     CN(X1)        IFEQ      *ZERO
     C     CN(X1)        ORGE      XXDATE
     C                   MOVE      '1'           XXPOL
     C                   END
     C                   END
     C                   END
     C*
     C* * * CALCULATE THE DIFFERENCE BETWEEN THE START DATE AND
     C* * * THE CURRENT DATE
     C*
     C     XXPOL         IFEQ      '1'
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C     SRERR         IFEQ      '0'
     C     1             ADD       XXWK50        XXEDAY            5 0
     C                   Z-ADD     XXEDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXEDAY
     C                   END
     C*
     C                   END
     C                   END
     C*
     C     ENDDAY        TAG
     C*
     C* * * TAKE THE NUMBER OF DAYS COVERED AND DIVIDE BY THE POLICY
     C* * * PERIOD
     C     XXFDAY        IFNE      *ZERO
     C     XXEDAY        DIV(H)    XXFDAY        XXPROR           10 9
     C                   ELSE
     C                   Z-ADD     0             XXPROR
     C                   END
     C*
     C* * * MULTIPLY THE PRORATA FACTOR TIMES THE ANNUAL PREMIUM
     C*
     C     XXPROR        MULT(H)   ELANPR        XXANPR           11 2          JJJ 10-18-88
     C*
     C* * * ESTIMATED PREMIUM CANNOT BE MORE THAN THE WHOLE
     C*
     C     XXANPR        IFGT      ELANPR                                       |
     C                   Z-ADD     ELANPR        XXANPR                         |
     C                   END
     C*
     C* * * NOW WE HAVE TO FIND THE PREMIUM FOR THIS MONTH
     C*
     C                   MOVE      XXDATE        XXDAT1
     C                   MOVE      01            XXDAY1
     C*
     C     XXDATE        IFGT      F1ENPO
     C     XXDATE        ORLT      F1STPO
     C                   Z-ADD     0             XXMDAY
     C                   GOTO      ENDMTH
     C                   END
     C*
     C                   MOVE      '0'           XXNTF
     C                   Z-ADD     5             X1
     C     X1            DOUEQ     0
     C     EF(X1)        IFNE      *ZERO
     C     EF(X1)        IFLE      XXDATE                                       |
     C     CN(X1)        IFGE      XXDAT1
     C     CN(X1)        OREQ      *ZERO
     C                   MOVE      '1'           XXNTF             1
     C                   END
     C                   END
     C                   END
     C                   SUB       1             X1
     C                   END
     C*
     C     XXNTF         IFEQ      '0'
     C                   Z-ADD     0             XXMDAY
     C                   GOTO      ENDMTH
     C                   END
     C*
     C     1             DO        5             X1                5 0
     C*
     C     EF(X1)        IFGE      XXDAT1
     C     EF(X1)        ANDLE     XXDATE
     C*
     C* * * CHECK FOR A CANCELLATION DATE
     C*
     C     CN(X1)        IFNE      *ZERO
     C     CN(X1)        IFLE      XXDATE
     C                   Z-ADD     EF(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C*
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXMDAY            5 0
     C     CN(X1)        IFNE      EF(X1)
     C                   ADD       1             XXMDAY
     C                   END
     C                   Z-ADD     XXMDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXMDAY
     C                   END
     C*
     C                   END
     C                   ELSE
     C*
     C* * * FIND THE DIFFERENCE BETWEEN THE EFFECTIVE DATE AND THE
     C* * * END OF THE MONTH
     C                   Z-ADD     EF(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C*
     C*** BEGIN CHG *** 10/20/97 **************************************
     C******************** Z-ADDXXDATE    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     XXDT02
     C     ELPTDT        IFLT      XXDATE
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*
     C                   ELSE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*
     C                   ENDIF                                                  ELPTDT<SRDTO
     C*
     C                   MOVEL     SRDTO         XXDT02
     C*** END CHG ***** 10/20/97 **************************************
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXMDAY            5 0
     C                   ADD       1             XXMDAY
     C                   Z-ADD     XXMDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXMDAY
     C                   END
     C*
     C                   END
     C                   ELSE
     C                   END
     C*
     C                   END
     C*
     C* * * CHECK OUR DAYS, IF ZERO THEN WE DIDN'T HAVE A START
     C* * * DATE DURING OUR LOOP, CHECK FOR A CANCELLATION
     C*
     C     XXMDAY        IFEQ      *ZERO
     C     1             DO        5             X1                5 0
     C*
     C     CN(X1)        IFGE      XXDAT1
     C     EF(X1)        ANDLT     XXDAT1
     C     CN(X1)        IFLT      XXDATE
     C*
     C* * * CALCULATE THE DIFFERENCE BETWEEN THE START DATE AND
     C* * * THE CANCELLATION DATE
     C*
     C                   Z-ADD     XXDAT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C*
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXMDAY            5 0
     C     CN(X1)        IFNE      XXDAT1
     C                   ADD       1             XXMDAY
     C                   END
     C                   Z-ADD     XXMDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXMDAY
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   END
     C                   END
     C*
     C* * * FIND THE DIFFERENCE BETWEEN THE FIRST OF THE MONTH AND
     C* * * THE END OF THE MONTH
     C*
     C     XXMDAY        IFEQ      *ZERO
     C*
     C* * * WE NEED TO FIND A EF DATE BEFORE THE XXDAT1, AND
     C* * * A CN DATE AFTER THE CURRENT DATE
     C*
     C                   MOVE      '0'           XXPOL             1
     C     1             DO        5             X1
     C     EF(X1)        IFLT      XXDAT1
     C     EF(X1)        ANDNE     0
     C     CN(X1)        IFEQ      *ZERO
     C     CN(X1)        ORGE      XXDATE
     C                   MOVE      '1'           XXPOL
     C                   END
     C                   END
     C                   END
     C*
     C     XXPOL         IFEQ      '1'
     C                   Z-ADD     XXDAT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C*
     C*** BEGIN CHG *** 10/20/97 **************************************
     C******************** Z-ADDXXDATE    SRDTI
     C******************** CALL 'SRCVTDAT'DTMDYY
     C******************** MOVELSRDTO     XXDT02
     C     ELPTDT        IFLT      XXDATE
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*
     C                   ELSE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C*
     C                   ENDIF                                                  ELPTDT<SRDTO
     C*
     C                   MOVEL     SRDTO         XXDT02
     C*** END CHG ***** 10/20/97 **************************************
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50
     C                   PARM                    SRERR
     C                   PARM                    SRTERM
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C     1             ADD       XXWK50        XXMDAY            5 0
     C                   Z-ADD     XXMDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXMDAY
     C                   END
     C                   END
     C*
     C                   END
     C*
     C     ENDMTH        TAG
     C*
     C* * * TAKE THE NUMBER OF DAYS COVERED AND DIVIDE BY THE POLICY
     C* * * PERIOD
     C     XXFDAY        IFNE      *ZERO
     C     XXMDAY        DIV(H)    XXFDAY        XXPROR           10 9
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPROR
     C                   END
     C*
     C* * * MULTIPLY THE PRORATA FACTOR TIMES THE ANNUAL PREMIUM
     C*
     C     XXPROR        MULT(H)   ELANPR        XXMNPR           11 2          JJJ 10-18-88
     C*
     C                   ELSE
     C*
     C* * * DO 'S' TYPE OF BILLING, SELF REPORTING.
     C* * * FIND THE AMOUNT OF A/R GENERATED; BILS + ADJUSTMENTS.
     C*
     C*
     C                   Z-ADD     0             XXWK92
     C*** BEGIN ADD ***  06/08/05  ************************************
     C     ELARSM        IFEQ      'Y'
     C                   Z-ADD     XSDIV         X1DIV
     C                   ELSE
     C                   Z-ADD     XXDIV         X1DIV
     C                   ENDIF
     C*** END ADD *****  06/08/05  ************************************
     C     EAKEY         SETLL     PEA01
     C     EAKEY1        READE     PEA01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*** BEGIN ADD ***  06/08/05  ************************************
     C     ELARSM        IFEQ      'Y'
     C     EARDIV        ANDEQ     XXDIV
     C     ELARSM        ORNE      'Y'
     C*** END ADD *****  06/08/05  ************************************
     C*
     C     EAPODT        IFLE      XXDATE
     C*
     C     EAARTY        IFEQ      'P'                                          |
     C     EAARTY        OREQ      'R'                                          |
     C     EAARTY        OREQ      'N'                                          |
     C     EAARTY        OREQ      'V'                                          |
     C*                                                    |
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C     EAARTY        IFEQ      'P'
     C                   ADD       EAAMT         XXANPR
     C                   ELSE
     C                   SUB       EAAMT         XXANPR
     C                   END
     C                   END
     C*
     C*  LOAD CURRENT MONTH AMOUNT IF POSTING Y/M SAME AS PARM Y/M.
     C*
     C     XXPOYM        IFEQ      XXYM
     C*
     C     XXPOYM        ORLE      XXYM                                         SAJ 11-11-88
     C     XXYM          ANDEQ     XXSAVE                                       SAJ 11-11-88
     C*
     C     TTCTYP        IFNE      'E'                                          SAJ 11-14-88
     C     EAARTY        IFEQ      'P'                                          |   11-11-88
     C                   ADD       EAAMT         XXMNPR
     C                   ELSE                                                   |
     C                   SUB       EAAMT         XXMNPR                         |
     C                   END                                                    |
     C                   END
     C                   ENDIF                                                  *IN60 IFEQ
     C                   END
     C*
     C                   END
     C*
     C                   END
     C*
     C*** BEGIN ADD ***  06/08/05  ************************************
     C                   ENDIF                                                  ELARSM IFEQ
     C*** END ADD *****  06/08/05  ************************************
     C*
     C     EAKEY1        READE     PEA01                                  60
     C*
     C                   END
     C*
     C                   END
     C*
     C*
     C                   END
     C*
     C* * * LOAD UP RETURN VALUES
     C*
     C                   MOVE      XXANPR        SRAMT
     C                   MOVE      XXMNPR        SRAMT2
     C                   END
     C*
     C     RETURN        TAG
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
     C                   Z-ADD     0             XXLPDT            8 0
     C                   Z-ADD     XXDT01        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXDT01
     C*
     C                   Z-ADD     XXDT02        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXDT02
     C*
     C                   Z-ADD     XXDT01        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C*
     C     XXLPDT        IFLT      XXDT01
     C                   Z-ADD     XXDT02        XXLPDT
     C                   MOVE      '0229'        XXLPDT
     C                   END
     C*
     C     XXLPDT        IFLT      XXDT01
     C     XXLPDT        ORGT      XXDT02
     C                   Z-ADD     *ZERO         XXLPDT
     C                   END
     C*
     C     XXLPDT        IFNE      *ZERO
     C*
     C                   Z-ADD     XXLPDT        SRDTI
      **** BEGIN CHG **** 03/05/98 *********************************************
     C******************** CALL 'SRCVTDAT'DTMDYY
     C                   CALL      'SRCVTDAT'    DTMDY2
      **** END   CHG **** 03/05/98 *********************************************
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C*********************Z-ADDXQFROM    SRDTI
     C*********************CALL 'SRCVTDAT'DATCHK
     C*********************MOVELSRDTO     XQFROM  80
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             XXWKDY            5 0
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C/EJECT
