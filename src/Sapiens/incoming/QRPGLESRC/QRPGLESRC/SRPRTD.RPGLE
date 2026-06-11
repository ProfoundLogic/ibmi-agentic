     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H/define CONVERT
     H*****************************************************************
     H*  PROGRAM SUBROUTINE SRPRTD - EARNED PREMIUM TO DATE SUBPROGRAM
     H*****************************************************************
     H*
     H*  THIS SUB-PROGRAM WILL CALCULATE THE EARNED PREMIUM TO DATE.
     H*  AND WILL ALSO RETURN THE EARNED PREMIUM FOR THE GIVEN PERIOD MNTH.
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/30/12 C:180 SUPPORT 79761
     H*                Prevent CPF4128 on pgm W8011R and file WAELL
     H*                when batch billing job runs.
     H*
     H* 01/24/06 C:65 PROJECT 114WHO                                  *
     H*               USE THE RETROSPECTIVE PREMIUM IF POPULATED FOR  *
     H*               PREMIUM BASIS.                                  *
     H*                                                               *
     H* 07/06/05 C:136 SUPPORT: CI135 47867                           *
     H*                PREVIOUS ITEM (06/08/05) EXECUTES TOO SLOW.    *
     H*                DIVIDE THE A/R UP INTO DIV/RDIV CHUNKS TO      *
     H*                READ FOR SUMMARIZED S/R EMPLRS.                *
     H*                                                               *
     H* 06/24/05 C:33 Project: 109WDZ                                 *
     H*              -Added code to calculate the earned premium      *
     H*                based on the S/R Frequency flag in WTBSPX.     *
     H*                 BSSRFQ = 'M':monthly but can be limited by    *
     H*                               the '# of months to be reported'*
     H*                 BSSRFQ = 'Q':quarterly (report every 3 months)*
     H*                              also can be limited to certain # *
     H*                              of reports                       *
     H*                 BSSRFQ = 'X':semi-annual( rprt every 6 months)*
     H*                              also can be limited to certain # *
     H*                              of reports                       *
     H*              -Call API (WA114R) to determine if month is to   *
     H*                be reported.                                   *
     H*                                                               *
     H* 06/08/05 C:65 PROJECT: 135WCE/WC1288                          *
     H*               WHEN CHECKING A/R FOR SUMMARIZED S/R EMPLRS,    *
     H*               CHECK THE STATE REPORTING DIVISION INSTEAD.     *
     H*                                                               *
     H* 11/19/03 C:136 PROJECT: WC970                                 *
     H*                ADD CODE TO BREAK OUT TERRORISM CHARGE IF      *
     H*                REQUESTED.                                     *
     H*                                                               *
     H* 02/10/99 C:09 PROJECT: SUPP  WA048/SEQ# 16514                 *
     H*               EXPAND XXWK92/XXWKPR TO 11.2 TO HANDLE "LARGE"  *
     H*               OFFSETTING A/R ENTRIES.                         *
     H*                                                               *
     H* 09/04/98 C:65 PROJECT: 48WLJ                                  *
     H*               IGNORE S/R REPORTS POSTED AFTER MONTHEND.       *
     H*                                                               *
     H* 07/14/98 C:09 PROJECT: SUPP  WA095/SEQ# 11561                 *
     H*               APPLY CHANGES SIMILAR TO THOSE ALREADY MADE IN  *
     H*               S/R EARNED PREMIUM SECTION TO NOT INCLUDE CANCEL*
     H*               DATE AS PART OF EARNED PREMIUM.                 *
     H*                                                               *
     H* 05/01/98 C:78 PROJECT: WC745                                  *
     H*              SINCE WE ARE USING WMFNP/FNCGFL INSTEAD OF WDF2P/*
     H*              F2CT01, WE DONT NEED THE FILE ANYMORE(WDF2P).    *
     H*              FNCGFL ALREADY A ONE CHAR FIELD. DONT NEED XXFCT1*
     H*                                                               *
     H* 04/13/98 C:77 PROJECT SUPP (WA095 #10676, WA031 #10035)       *
     H*               CHANGE RETRIEVAL OF COMMERCIAL FLAG TO USE V.3  *
     H*               METHOD INSTEAD OF V.2.                          *
     H*               (WMFNP/FNCGFL INSTEAD OF WDF2P/F2CT01).         *
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
     H* 11/20/90 JJJ  USE ELPRTD INSTEAD OF F1EXCO                    *
     H*                                                               *
     H* 05/28/91 JJJ  ACCOUNT FOR CANCEL DT LESS THAN FYR START DATE. *
     H*               PREMIUM IS NOT EARNED FOR 1 DAY IF CANCEL DATE  *
     H*               EQUALS THE START DATE.
     H*                                                               *
     H* 07/05/95 C:36 PROJECT WC270                                   *
     H*               FIX NUMBER OF DAYS IN FEBRUARY IN LEAP YEAR TO  *
     H*               BE 28.                                          *
     H*                                                               *
     H* 10/09/95 C:24 PROJECT WC270                                   *
     H*               SELF REPORTING ACCOUNTS - ESTIMATE EARNED       *
     H*               PREMIUM FOR ALL MISSING MONTHS, NOT JUST CURRENT*
     H*                                                               *
     H* 11/28/95 C:33 PROJECT CM010                                   *
     H*               ON ESTIMATING ACCOUNTS, BE SURE TO PRORATE      *
     H*               PROPERLY FOR POLICIES WITH NON-COMMON           *
     H*               EXPIRATION DATES.                               *
     H*                                                               *
     H* 04/18/96 C:36 SUPP - PROJECT 68AHO                            *
     H*               FIXED PROBLEM WITH CALCULATION OF EARNED        *
     H*               PREMIUM FOR CURRENT MONTH                       *
     H*                                                               *
     H* 04/11/97 C:09 PROJECT WC270                                   *
     H*               ANY ACCOUNT THAT IS S/R, NOT AUDITED, NOT       *
     H*               CANCELLED AND A PRIOR YEAR. (EX. CURR DT=       *
     H*               4/10/97, ACCT 1/1/96-1/1/97), THIS ROUTINE      *
     H*               WAS ADDING AN EXTRA DAY'S WORTH OF EARNED       *
     H*               PREMIUM.                                        *
     H*                                                               *
     H* 04/15/97 C:62 PROJECT WC270                                   *
     H*               ANY ACCOUNT THAT IS S/R, NOT AUDITED, NOT       *
     H*               CANCELLED AND A PRIOR YEAR. (EX. CURR DT=       *
     H*               4/10/97, ACCT 1/1/96-1/1/97), THIS ROUTINE      *
     H*               WAS ADDING AN EXTRA DAY'S WORTH OF EARNED       *
     H*               PREMIUM.                                        *
     H*                                                               *
     H* 06/19/97 C:09 PROJECT WC720                                   *
     H*               MODIFY S/R PROCESSING TO PROPERLY RECOGNIZE     *
     H*               A MISSING REPORT VS. A MONTH W/NO PAYROLL BEING *
     H*               REPORTED.  NOTE: DEFINITION OF NO PAYROLL       *
     H*               REPORTED IS AN A/R RECORD WITH EAAMT = ZEROS.   *
     H*****************************************************************
     FWMEML     IF   E           K DISK
     F*** BEGIN CHANGE ***** 04/30/12 *********************************
     F*DELL** IF  E           K        DISK
     FWDELL     IF   E           K DISK    USROPN
     F*** END   CHANGE ***** 04/30/12 *********************************
     FWTABL     IF   E           K DISK
     FWMEAL6    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     F**** BEGIN DEL ***** 05/01/98 ***********************************
     F***  BEGIN ADD  ***  11/28/95  *********************************
     F***** WDF2L   IF  E           K        DISK
     F***  END   ADD  ***  11/28/95  *********************************
     F**** END   DEL ***** 05/01/98 ***********************************
     F*** BEGIN ADD *** 04/13/98 *************************************
     FWMFNL     IF   E           K DISK
     F*** END ADD ***** 04/13/98 *************************************
     F*** BEG ADD ***** 07/06/05 *************************************
     FWMEAL24   IF   E           K DISK
     F                                     RENAME(PEA01:PEA24)
     F*** END ADD ***** 07/06/05 *************************************

     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
     D*
     D*** BEG ADD ***** 11/19/03 *************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** END ADD ***** 11/19/03 *************************************
     D*
     D                 DS
     D  XXTODT                 1      6  0
     D  XXTOYR                 1      2  0
     D  XXTOMO                 3      4  0
     D  XXTOYM                 1      4  0
     D*
     D                 DS
     D  XXFRDT                 1      6  0
     D  XXFRYR                 1      2  0
     D  XXFRMO                 3      4  0
     D  XXFRYM                 1      4  0
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
     D  XXDAY1                 5      7  0
     D                 DS
     D  XXSTPO                 1      7  0
     D  XXCYSP                 1      3  0
     D  XXMMSP                 4      5  0
     D  XXMYSP                 1      5  0
     D  XXDYSP                 6      7  0
     D                 DS
     D  XXENPO                 1      7  0
     D  XXCYEP                 1      3  0
     D  XXMMEP                 4      5  0
     D  XXMYEP                 1      5  0
     D  XXDYEP                 6      7  0
     D                 DS
     D  XXENDM                 1      7  0
     D  XXCYEM                 1      3  0
     D  XXMMEM                 4      5  0
     D  XXMYEM                 1      5  0
     D  XXDDEM                 6      7  0

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
     C* * * TABLE FILE
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
     C*** BEG CHG *****  07/06/05  ************************************
     C*********            KFLD           X1DIV   50
     C                   KFLD                    XXDIV
     C*** END CHG *****  07/06/05  ************************************
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
     C*** BEG CHG *****  07/06/05  ************************************
     C*********            KFLD           X1DIV   50
     C                   KFLD                    XXDIV
     C*** END CHG *****  07/06/05  ************************************
     C*** END CHG *****  06/08/05  ************************************
     C                   KFLD                    XXFYR
      *
     C*** BEG ADD *****  07/06/05  ************************************
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XSDIV             5 0
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EAKY2L        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XSDIV             5 0
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXPODT            7 0
     C*** END ADD *****  07/06/05  ************************************
     C**** BEGIN DEL ***** 05/01/98 **********************************
     C***  BEGIN ADD  ***  11/28/95  *********************************
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C*****      F2KY      KLIST
     C*****                KFLD           XXCO#
     C*****                KFLD           XXFND
     C***  END   ADD  ***  11/28/95  *********************************
     C**** END   DEL ***** 05/01/98 **********************************
     C*
     C*** BEGIN ADD *** 04/13/98 *******************************************
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*** END ADD ***** 04/13/98 *******************************************
     C*
     C*** BEGIN ADD *** 04/11/97 *******************************************
     C     *LIKE         DEFINE    F1ENPO        XXDTT2
     C*** END ADD ***** 04/11/97 *******************************************
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C*** BEG ADD ***** 11/19/03 *************************************
     C     *LIKE         DEFINE    XXPROR        XXFACT
     C     *LIKE         DEFINE    ELANPR        XXAETC
     C     *LIKE         DEFINE    ELANPR        XXMETC
     C     *LIKE         DEFINE    ELETER        XXETER
     C*** END ADD ***** 11/19/03 *************************************
      *
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
      *** BEG ADD ***** 11/19/03 *******************************************
     C                   PARM                    SRTRFL            1            'Y' = BRKOUT TERR CH
     C                   PARM                    SRAETC           11            ANNUAL TERR CHG
     C                   PARM                    SRMETC           11            MONTH'S TERR CHG
      *** END ADD ***** 11/19/03 *******************************************
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
     C***  Begin Add  ***  04/30/12  **************************************
     C                   MOVE      '0'           XXOPN             1
     C     XXOPN         IFEQ      '0'
     C                   OPEN      WDELL
     C                   MOVE      '1'           XXOPN
     C                   END
     C***  End   Add  ***  04/30/12  **************************************
     C***  Begin Add  ***  06/24/05  **************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C                   MOVE      SRCO#         XXCO#             3 0
     C                   MOVE      SRFND         XXFND             3 0
     C                   MOVE      SRFYR         XXFYR             3 0
     C                   MOVE      SREMP#        XXEMP#
     C                   MOVE      SRDIV         XXDIV
     C                   MOVE      SRDATE        X1DATE            8 0
     C                   Z-ADD     X1DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
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
     C***  BEGIN ADD  ***  11/28/95  *********************************
     C*
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C****** BEGIN DEL ****** 05/01/98 ********************************
     C**********           MOVE *BLANK    XXFCT1  1
     C********** F2KY      CHAINPF201                60
     C****** END   DEL ****** 05/01/98 ********************************
     C*
     C*** BEGIN CHG *** 04/13/98 *************************************
     C**N60****************MOVELF2CT01    XXFCT1
     C*
     C*  GET THE FUND MASTER
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C****** BEGIN CHG ****** 05/01/98 ********************************
     C**N60                MOVELFNCGFL    XXFCT1
     C   60              MOVE      *BLANK        FNCGFL
     C****** END   CHG ****** 05/01/98 ********************************
     C*** END CHG ***** 04/13/98 *************************************
     C***  END   ADD  ***  11/28/95  *********************************
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
     C*
     C*** BEGIN DEL *** JJJ 11-20-90 ********************************
     C*********  ELEMNP    IFEQ *BLANK
     C*********  ELPROR    MULT F1EXCO    F1EXCO
     C*********  F1EXCO    IFLT F1MPEC
     C*********            Z-ADDF1MPEC    F1EXCO
     C*********            END
     C*
     C*  ROUND THE EXPENSE CONSTANT                        SAJ 5-9-88
     C*                                                    |
     C*********            Z-ADDF1EXCO    XXWORK  70H      | RLS 2
     C*********            Z-ADDXXWORK    F1EXCO           |
     C*
     C*********  ELANPR    SUB  F1EXCO    ELANPR
     C*** END DEL *** JJJ 11-20-90 ********************************
     C*** BEGIN ADD *** JJJ 11-20-90 ********************************
     C     ELANPR        SUB       ELPRTD        ELANPR
     C*
     C*** BEG ADD ***** 11/19/03 *************************************
     C     Q1PRMS        IFGE      10
     C     SRTRFL        ANDEQ     'Y'
     C     ELANPR        SUB       ELETER        ELANPR
     C                   ENDIF
     C*** END ADD ***** 11/19/03 *************************************
     C*
     C*** END ADD *** JJJ 11-20-90 ********************************
     C     ELANPR        IFLT      *ZERO
     C                   Z-ADD     0             ELANPR
     C                   END
     C*** BEGIN DEL *** JJJ 11-20-90 ********************************
     C**********           END
     C*** END DEL *** JJJ 11-20-90 ********************************
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
     C*** BEGIN ADD ***  01/24/06  ************************************
     C     ELSTAM        IFNE      *ZERO
     C                   MOVE      ELSTAM        SRAMT
     C                   ENDIF
     C*** END ADD *****  01/24/06  ************************************
     C*** BEGIN DEL *** JJJ 11-20-90 ********************************
     C*********  ELAMNP    IFEQ *BLANK
     C*********  ELPROR    MULT F1EXCO    F1EXCO
     C*********  F1EXCO    IFLT F1MPEC
     C*********            Z-ADDF1MPEC    F1EXCO
     C*********            END
     C*
     C*  ROUND THE EXPENSE CONSTANT                        SAJ 5-9-88
     C*                                                    |
     C**********           Z-ADDF1EXCO    XXWORK  70H      | RLS 2
     C**********           Z-ADDXXWORK    F1EXCO           |
     C********** ELAAPR    SUB  F1EXCO    ELAAPR
     C*** END DEL *** JJJ 11-20-90 ********************************
     C*** BEGIN ADD *** JJJ 11-20-90 ********************************
     C     ELAAPR        SUB       ELPRTD        ELAAPR
     C*** END ADD *** JJJ 11-20-90 ********************************
     C*** BEGIN ADD ***  01/24/06  ************************************
     C     ELSTAM        IFNE      *ZERO
     C     ELSTAM        SUB       ELPRTD        ELAAPR
     C                   ENDIF
     C*** END ADD *****  01/24/06  ************************************
     C     ELAAPR        IFLT      *ZERO
     C                   Z-ADD     0             ELAAPR
     C                   END
     C                   MOVE      ELAAPR        SRAMT
     C*** BEGIN DEL *** JJJ 11-20-90 ********************************
     C************         END
     C*** END DEL *** JJJ 11-20-90 ********************************
     C                   MOVE      *ZEROS        SRAMT2
      *
     C*** BEG ADD ***** 11/19/03 *************************************
     C     Q1PRMS        IFGE      10
     C     SRTRFL        ANDEQ     'Y'
      *
     C     ELAAPR        SUB       ELATER        ELAAPR
     C*** BEGIN ADD ***  01/24/06  ************************************
     C     ELSTAM        IFNE      *ZERO
     C     ELSTAM        SUB       ELATER        ELAAPR
     C                   ENDIF
     C*** END ADD *****  01/24/06  ************************************
     C                   MOVE      ELATER        SRAETC
     C                   MOVE      *ZEROS        SRMETC
      *
     C                   ENDIF
     C*** END ADD ***** 11/19/03 *************************************
      *
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
     C***  BEGIN ADD  ***  11/28/95  *********************************
     C*
     C*  IF THIS IS A NON-COMMON EXPIRATION DATE POLICY,
     C*  FUND DATES WILL BE ELPFDT AND ELPTDT = 365 DAYS
     C*
     C****** BEGIN CHG ****** 05/01/98 ********************************
     C*******    XXFCT1    IFEQ 'Y'
     C     FNCGFL        IFEQ      'Y'
     C****** END   CHG ****** 05/01/98 ********************************
     C                   Z-ADD     ELPFDT        F1STPO
     C                   Z-ADD     ELPTDT        F1ENPO
     C                   END
     C***  END   ADD  ***  11/28/95  *********************************
     C*
     C* * * FIND THE NUMBER OF DAYS BETWEEN THE FUND DATES
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01            8 0
     C                   Z-ADD     F1ENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02            8 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C*** BEGIN DEL *** 07/05/95 *******************************************
     C*
     C* * * * IF WE DON'T HAVE AN ERROR CHECK FOR 365,366
     C*
     C*********  XXWK50    IFEQ 366
     C*********            Z-ADD365       XXWK50
     C*********            END
     C*** END DEL ***** 07/05/95 *******************************************
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   Z-ADD     XXWK50        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXWK50
     C*** END ADD ***** 07/05/95 *******************************************
     C*
     C                   Z-ADD     XXWK50        XXFDAY            5 0
     C*
     C* * * CLEAN UP
     C                   MOVE      *ZERO         XXWK92
     C                   MOVE      *ZERO         XXWK50
     C                   MOVE      *ZERO         XXANPR
     C                   MOVE      *ZERO         XXMNPR
     C                   MOVE      *ZERO         XXEDAY
     C                   MOVE      *ZERO         XXMDAY
     C***  Begin Add  ***  06/24/05  **************************************
     C                   MOVE      *ZERO         XXPDAY
     C***  End   Add  ***  06/24/05  **************************************
     C*** BEG ADD ***** 11/19/03 *************************************
     C                   MOVE      *ZERO         XXAETC
     C                   MOVE      *ZERO         XXMETC
     C*** END ADD ***** 11/19/03 *************************************
     C*
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
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
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
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   Z-ADD     XXEDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXEDAY
     C*** END ADD ***** 07/05/95 *******************************************
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
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXEDAY            5 0
     C                   ADD       1             XXEDAY
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   Z-ADD     XXEDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXEDAY
     C*** END ADD ***** 07/05/95 *******************************************
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
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
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
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   Z-ADD     XXEDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXEDAY
     C*** END ADD ***** 07/05/95 *******************************************
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
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C*
     C     SRERR         IFEQ      '0'
     C     1             ADD       XXWK50        XXEDAY            5 0
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   Z-ADD     XXEDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXEDAY
     C*** END ADD ***** 07/05/95 *******************************************
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
     C     XXPROR        MULT(H)   ELANPR        XXANPR           11 2
     C*
     C* * * ESTIMATED PREMIUM CANNOT BE MORE THAN THE WHOLE
     C*
     C     XXANPR        IFGT      ELANPR                                       SAJ 5-12-88
     C                   Z-ADD     ELANPR        XXANPR                         | UNDID
     C                   END                                                    | CHANGE
     C*
     C* * * NOW WE HAVE TO FIND THE PREMIUM FOR THIS MONTH
     C*
     C                   MOVE      XXDATE        XXDAT1            7 0
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
     C     EF(X1)        IFLE      XXDATE
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
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXMDAY            5 0
     C     CN(X1)        IFNE      EF(X1)
     C                   ADD       1             XXMDAY
     C                   END
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   Z-ADD     XXMDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXMDAY
     C*** END ADD ***** 07/05/95 *******************************************
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
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXMDAY            5 0
     C                   ADD       1             XXMDAY
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   Z-ADD     XXMDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXMDAY
     C*** END ADD ***** 07/05/95 *******************************************
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
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXMDAY            5 0
     C     CN(X1)        IFNE      XXDAT1
     C                   ADD       1             XXMDAY
     C                   END
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   Z-ADD     XXMDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXMDAY
     C*** END ADD ***** 07/05/95 *******************************************
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
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C     1             ADD       XXWK50        XXMDAY            5 0
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   Z-ADD     XXMDAY        XXWKDY
     C                   EXSR      LEAPYR
     C                   Z-ADD     XXWKDY        XXMDAY
     C*** END ADD ***** 07/05/95 *******************************************
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
     C     XXPROR        MULT(H)   ELANPR        XXMNPR           11 2
     C*
     C                   ELSE
     C*
     C* * * DO 'S' TYPE OF BILLING, SELF REPORTING
     C* * * FIND THE NUMBER OF DAYS COVERED IN THE MONTH, IF
     C* * * LESS THAN THE ACTUAL NUMBER OF DAYS, THEN WE PRORATE
     C* * * THE MONTH.  FIND THE AMOUNT OF A/R GENERATED FOR THE
     C* * * MONTH. IF THE MONTH WE ARE LOOKING FOR A/R IS EQUAL TO
     C* * * THE CURRENT MONTH OF THE REPORT, THEN IF WE DON'T FIND
     C* * * A/R WE CAN ESTIMATE THE PREMIUM, IF IT IS NOT THE
     C* * * CURRENT MONTH, THEN WE SHOULD REPORT NO PREMIUM.
     C*
     C     F1ENPO        IFLT      XXDATE
     C                   Z-ADD     F1ENPO        XXENPO            7 0
     C                   ELSE
     C                   Z-ADD     XXDATE        XXENPO
     C                   END
     C*
     C                   Z-ADD     F1STPO        XXSTPO
     C*
     C* * * NOW GO THROUGH EACH MONTH OF THE POLICY PERIOD AND
     C* * * DETERMINE THE EARNED PREMIUM
     C*
     C     XXMYSP        DOWLE     XXMYEP
     C*
     C                   Z-ADD     XXSTPO        XXENDM
     C*
     C                   Z-ADD     XXMMSP        X2                5 0
     C                   Z-ADD     MD(X2)        XXDDEM
     C     XXCYEM        DIV       4             XXWK50            5 0
     C                   MVR                     XXWK83            8 3
     C     XXWK83        IFEQ      *ZERO
     C     XXMMEM        IFEQ      2
     C                   Z-ADD     29            XXDDEM
     C                   END
     C                   END
     C*
     C*
     C                   Z-ADD     0             XXEDAY
     C     1             DO        5             X1                5 0
     C*
     C     EF(X1)        IFGE      XXSTPO
     C     EF(X1)        ANDLE     XXENDM
     C*
     C* * * CHECK FOR A CANCELLATION DATE
     C*
     C     CN(X1)        IFNE      *ZERO
     C     CN(X1)        IFLE      XXENDM
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C     CN(X1)        ANDLE     XXENPO
     C*** END ADD ***** 07/05/95 *******************************************
     C                   Z-ADD     EF(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXEDAY
     C     CN(X1)        IFNE      EF(X1)
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C     XXDDEM        ANDNE     29
     C     CN(X1)        ORNE      EF(X1)
     C     XXDDEM        ANDEQ     29
     C     CN(X1)        ANDLT     XXENDM
     C     CN(X1)        ANDLE     XXENPO
     C*** END ADD ***** 07/05/95 *******************************************
     C*** BEGIN CHG *** 04/11/97 *******************************************
     C     CN(X1)        ANDNE     F1ENPO
     C*** END CHG ***** 04/11/97 *******************************************
     C                   ADD       1             XXEDAY
     C                   END
     C                   END
     C*
     C*** BEGIN CHG *** 07/05/95 *******************************************
     C                   ELSE
     C*********            END
     C*** END CHG ***** 07/05/95 *******************************************
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C*
     C* * * FIND THE DIFFERENCE BETWEEN THE EFFECTIVE DATE AND THE
     C* * * END OF THE MONTH
     C*
     C                   Z-ADD     EF(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C     XXENPO        IFLE      XXENDM
     C                   Z-ADD     XXENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*** BEGIN ADD *** 04/15/97 *******************************************
     C                   Z-ADD     XXENPO        XXDTT2
     C*** END   ADD *** 04/15/97 *******************************************
     C                   ELSE
     C                   Z-ADD     XXENDM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*** BEGIN ADD *** 04/15/97 *******************************************
     C                   Z-ADD     XXENDM        XXDTT2
     C*** END   ADD *** 04/15/97 *******************************************
     C                   END
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXEDAY
     C     XXDDEM        IFNE      29
     C     XXDDEM        OREQ      29
     C     XXENPO        ANDLT     XXENDM
     C*** BEGIN ADD *** 04/15/97 *******************************************
     C     XXDTT2        ANDNE     F1ENPO
     C*** END   ADD *** 04/15/97 *******************************************
     C                   ADD       1             XXEDAY
     C                   END
     C                   END
     C*
     C                   END
     C*** END ADD ***** 07/05/95 *******************************************
     C                   ELSE
     C*
     C* * * FIND THE DIFFERENCE BETWEEN THE EFFECTIVE DATE AND THE
     C* * * END OF THE MONTH
     C*
     C                   Z-ADD     EF(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C     XXENPO        IFLE      XXENDM
     C                   Z-ADD     XXENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*** BEGIN ADD *** 04/15/97 **************************************
     C                   Z-ADD     XXENPO        XXDTT2
     C*** END   ADD *** 04/15/97 **************************************
     C                   ELSE
     C                   Z-ADD     XXENDM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*** BEGIN ADD *** 04/15/97 **************************************
     C                   Z-ADD     XXENDM        XXDTT2
     C*** END   ADD *** 04/15/97 **************************************
     C                   END
     C*** END ADD ***** 07/05/95 *******************************************
     C*** BEGIN DEL *** 07/05/95 *******************************************
     C*********  XXENDM    MULT 100.0001  XXDT02
     C*** END DEL ***** 07/05/95 *******************************************
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXEDAY
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C     XXDDEM        IFNE      29
     C     XXDDEM        OREQ      29
     C     XXENPO        ANDLT     XXENDM
     C*** BEGIN ADD *** 04/15/97 **************************************
     C     XXDTT2        ANDNE     F1ENPO
     C*** END   ADD *** 04/15/97 **************************************
     C*** END ADD ***** 07/05/95 *******************************************
     C                   ADD       1             XXEDAY
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   END
     C*** END ADD ***** 07/05/95 *******************************************
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
     C     1             DO        5             X1                5 0
     C*
     C     CN(X1)        IFGE      XXSTPO
     C     EF(X1)        ANDLT     XXSTPO
     C     CN(X1)        IFLE      XXENDM
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C     CN(X1)        ANDLE     XXENPO
     C*** END ADD ***** 07/05/95 *******************************************
     C*
     C* * * CALCULATE THE DIFFERENCE BETWEEN THE START DATE AND
     C* * * THE CANCELLATION DATE
     C*
     C                   Z-ADD     XXSTPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C                   Z-ADD     CN(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C*
     C     SRERR         IFEQ      '0'
     C                   ADD       XXWK50        XXEDAY
     C     CN(X1)        IFNE      XXSTPO
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C     XXDDEM        ANDNE     29
     C     CN(X1)        ORNE      XXSTPO
     C     XXDDEM        ANDEQ     29
     C     CN(X1)        ANDLT     XXENDM
     C     CN(X1)        ANDLE     XXENPO
     C*** END ADD ***** 07/05/95 *******************************************
     C*** BEGIN ADD *** 04/15/97 **************************************
     C     CN(X1)        ANDNE     F1ENPO
     C*** END   ADD *** 04/15/97 **************************************
     C                   ADD       1             XXEDAY
     C                   END
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   END
     C*
     C* * * FIND THE DIFFERENCE BETWEEN THE FIRST OF THE MONTH AND
     C* * * THE END OF THE MONTH
     C*
     C     XXEDAY        IFEQ      *ZERO
     C*
     C* * * WE NEED TO FIND A EF DATE BEFORE THE XXDAT1, AND
     C* * * A CN DATE AFTER THE CURRENT DATE
     C*
     C                   MOVE      '0'           XXPOL             1
     C     1             DO        5             X1
     C     EF(X1)        IFLT      XXSTPO
     C     EF(X1)        ANDNE     0
     C     CN(X1)        IFEQ      *ZERO
     C     CN(X1)        ORGE      XXENDM
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C     CN(X1)        ORGE      XXENPO
     C*** END ADD ***** 07/05/95 *******************************************
     C                   MOVE      '1'           XXPOL
     C                   END
     C                   END
     C                   END
     C*
     C     XXPOL         IFEQ      '1'
     C                   Z-ADD     XXSTPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT01
     C     XXENDM        IFLT      XXENPO
     C                   Z-ADD     XXENDM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*** BEGIN ADD *** 04/15/97 **************************************
     C                   Z-ADD     XXENDM        XXDTT2
     C*** END   ADD *** 04/15/97 **************************************
     C                   ELSE
     C                   Z-ADD     XXENPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT02
     C*** BEGIN ADD *** 04/15/97 **************************************
     C                   Z-ADD     XXENPO        XXDTT2
     C*** END   ADD *** 04/15/97 **************************************
     C                   END
     C                   CALL      'SRDDIF8'
     C                   PARM                    XXDT01
     C                   PARM                    XXDT02
     C                   PARM                    XXWK50            5 0
     C                   PARM                    SRERR             1
     C                   PARM                    SRTERM            1
     C* * * GET THE DAYS
     C*
     C     SRERR         IFEQ      '0'
     C     XXENDM        IFLT      XXENPO
     C*** BEGIN CHG *** 07/05/95 *******************************************
     C                   ADD       XXWK50        XXEDAY
     C*********  1         ADD  XXWK50    XXEDAY
     C*** END CHG ***** 07/05/95 *******************************************
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C     XXDDEM        IFNE      29
     C*** BEGIN ADD *** 04/15/97 **************************************
     C     XXDTT2        ANDNE     F1ENPO
     C*** END   ADD *** 04/15/97 **************************************
     C                   ADD       1             XXEDAY
     C                   END
     C*** END ADD ***** 07/05/95 *******************************************
     C                   ELSE
     C     XXENDM        IFEQ      XXENPO                                       SAJ 5-9-88
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C     XXDDEM        IFEQ      29
     C                   Z-ADD     28            XXEDAY
     C                   ELSE
     C*** END ADD ***** 07/05/95 *******************************************
     C                   Z-ADD     XXDDEM        XXEDAY                         |
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   END
     C*** END ADD ***** 07/05/95 *******************************************
     C                   ELSE                                                   |
     C                   ADD       XXWK50        XXEDAY
     C*** BEGIN ADD *** 04/15/97 **************************************
     C     XXDTT2        IFNE      F1ENPO
     C*** END   ADD *** 04/15/97 **************************************
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C                   ADD       1             XXEDAY
     C*** BEGIN ADD *** 04/15/97 **************************************
     C                   ENDIF
     C*** END   ADD *** 04/15/97 **************************************
     C*** END ADD ***** 07/05/95 *******************************************
     C                   END                                                    |
     C                   END
     C                   GOTO      ENDSPR
     C                   END
     C                   END
     C                   END
     C*
     C     ENDSPR        TAG
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C                   MOVE      *ON           RPTFLG            1
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C                   EXSR      CHKFRQ
     C                   ENDSL
     C*
     C*        if report month flag = *on
     C     RPTFLG        IFEQ      *ON
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C*** BEGIN ADD *** 06/19/97 *******************************************
     C                   MOVE      'N'           XXZRFL            1
     C*** END ADD ***** 06/19/97 *******************************************
     C*
     C* * * LOOK IN THE A/R FILE FOR THE AMOUNT OF BILLING WE HAVE
     C* * * GENERATED. BILLS + ADJUSTMENTS.  IF THE MONTH IS EQUAL
     C* * * TO OUR SUBMIT MONTH, THEN, IF WE DON'T FIND ANY PREMIUM
     C* * * WE CAN ESTIMATE THE PREMIUM.
     C*
     C                   Z-ADD     0             XXWK92
     C*** BEG CHG *****  07/06/05  ************************************
      *****************
     C*** BEGIN ADD ***  06/08/05  ************************************
     C***********ELARSM    IFEQ 'Y'
     C*****************    Z-ADDXSDIV     X1DIV
     C*****************    ELSE
     C*****************    Z-ADDXXDIV     X1DIV
     C*****************    ENDIF
     C****END*ADD******  06/08/05  ************************************
     C***********EAKEY*    SETLLPEA01
     C***********EAKEY1    READEPEA01                    60
      *
     C                   Z-ADD     XXSTPO        XXPODT
     C     ELARSM        IFEQ      'Y'
     C     EAKY2L        SETLL     PEA24
     C     EAKY2         READE     PEA24                                  60
     C                   ELSE
     C     EAKEY         SETLL     PEA01
     C     EAKEY1        READE     PEA01                                  60
     C                   ENDIF
     C*** END CHG *****  07/06/05  ************************************
     C     *IN60         DOWEQ     '0'
     C*
     C*** BEG DEL *****  07/06/05  ************************************
     C*** BEGIN ADD ***  06/08/05  ************************************
     C*********  ELARSM    IFEQ 'Y'
     C*********  EARDIV    ANDEQXXDIV
     C*********  ELARSM    ORNE 'Y'
     C*** END ADD *****  06/08/05  ************************************
     C*** END DEL *****  07/06/05  ************************************
     C*
     C     EAPODT        IFLE      XXENDM
     C     EAPODT        ANDGE     XXSTPO
     C*** BEGIN ADD ***  09/04/98  ************************************
     C     EATRDT        ANDLE     XXDATE
     C*** END ADD *****  09/04/98  ************************************
     C*
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C*
     C*  NO PAYROLLS FOR THE CURRENT MONTH (-NOT- A MISSING REPORT)
     C     EAAMT         IFEQ      *ZEROS
     C                   MOVE      'Y'           XXZRFL
     C                   END
     C                   ADD       EAAMT         XXWK92           11 2
     C*
     C                   ENDIF                                                  TTCTYP IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   END
     C*
     C                   END
     C*
     C*** BEG DEL *****  07/06/05  ************************************
     C*** BEGIN ADD ***  06/08/05  ************************************
     C*************        ENDIF                           ELARSM IFEQ
     C*** END ADD *****  06/08/05  ************************************
     C*** END DEL *****  07/06/05  ************************************
     C*** BEG CHG *****  07/06/05  ************************************
     C***********EAKEY1    READEPEA01                    60
      *
     C     ELARSM        IFEQ      'Y'
     C     EAKY2         READE     PEA24                                  60
     C                   ELSE
     C     EAKEY1        READE     PEA01                                  60
     C                   ENDIF
      *
     C     EAPODT        IFGT      XXENDM
     C                   LEAVE
     C                   ENDIF
     C*** END CHG *****  07/06/05  ************************************
     C*
     C                   END
     C*
     C*
     C* * * LOOK AT THE A/R FIELD, IF IT IS NOT ZERO, ADD IT TO
     C* * * AN ACCUMULATOR IF IT IS ZERO, THEN WE MIGHT NEED TO
     C* * * ESTIMATE
     C*
     C*** BEGIN CHG *** 06/19/97 *******************************************
     C***********XXWK92****IFEQ *ZERO
     C*
     C*  ESTIMATE IF TOTAL FOR MONTH IS ZERO AND NO ZERO REPORTS WERE
     C*  RECEIVED
     C*
     C     XXZRFL        IFNE      'Y'
     C     XXWK92        ANDEQ     *ZEROS
     C*** END CHG ***** 06/19/97 *******************************************
     C*
     C* * * TAKE THE SUBMIT MONTH AND YEAR AND COMPARE TO THE CURRENT
     C* * * MONTH AND YEAR, IF THEY ARE THE SAME THEN WE CAN ESTIMATE
     C* * * THE PREMIUM FOR THIS MONTH
     C*
     C*** BEGIN CHG *** 10/09/95 *******************************************
     C     XXMYSP        IFLE      XXMYEP
     C*********  XXMYSP    IFEQ XXMYEP
     C*** END CHG ***** 10/09/95 *******************************************
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C     BSSRFQ        IFEQ      'M'
     C     BSSRFQ        OREQ      ' '
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C     XXEDAY        IFNE      *ZERO
     C*
     C* * * TAKE THE NUMBER OF DAYS COVERED AND DIVIDE BY THE POLICY
     C* * * PERIOD
     C*
     C     XXFDAY        IFNE      *ZERO
     C     XXEDAY        DIV(H)    XXFDAY        XXPROR
     C                   ELSE
     C                   Z-ADD     0             XXPROR
     C                   END
     C*
     C* * * MULTIPLY THE PRORATA FACTOR TIMES THE ANNUAL PREMIUM
     C*
     C     XXPROR        MULT      ELANPR        XXWK92
     C                   ADD       XXWK92        XXANPR
     C*
     C                   END
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C                   ELSE
     C     XXPDAY        IFNE      *ZERO
     C*
     C* * * TAKE THE NUMBER OF DAYS COVERED AND DIVIDE BY THE POLICY
     C* * * PERIOD
     C*
     C     XXFDAY        IFNE      *ZERO
     C     XXPDAY        DIV(H)    XXFDAY        XXPROR
     C                   ELSE
     C                   Z-ADD     0             XXPROR
     C                   END
     C*
     C* * * MULTIPLY THE PRORATA FACTOR TIMES THE ANNUAL PREMIUM
     C*
     C     XXPROR        MULT      ELANPR        XXWK92
     C                   ADD       XXWK92        XXANPR
     C*
     C                   ENDIF                                                  XXPDAY<>0
     C                   ENDIF                                                  BSSRFQ='M'
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C                   END
     C                   ELSE
     C                   ADD       XXWK92        XXANPR
     C                   END
     C*
     C* * * TAKE THE PREMIUM FOR THE CURRENT MONTH ADD TO THE MONTH
     C* * * AMOUNT
     C*
     C*** BEGIN DLT *** 04/18/96 *******************************************
     C*** BEGIN CHG *** 10/09/95 *******************************************
     C*********  XXMYSP    IFLE XXMYEP
     C*********  XXMYSP    IFEQ XXMYEP
     C*** END CHG ***** 10/09/95 *******************************************
     C*** END DLT ***** 04/18/96 *******************************************
     C*** BEGIN ADD *** 04/18/96 *******************************************
     C     XXMYSP        IFEQ      XXMYEP
     C*** END ADD ***** 04/18/96 *******************************************
     C*
     C                   ADD       XXWK92        XXMNPR
     C*
     C                   END
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C                   Z-ADD     0             XXPDAY
     C                   ENDIF                                                  RPTFLG=*ON
     C***  End   Add  ***  06/24/05  **************************************
     C*
     C                   Z-ADD     1             XXDYSP
     C                   ADD       1             XXMMSP
     C     XXMMSP        IFGT      12
     C                   Z-ADD     1             XXMMSP
     C                   ADD       1             XXCYSP
     C                   END
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
     C*
     C                   MOVE      XXANPR        SRAMT
     C                   MOVE      XXMNPR        SRAMT2
     C*
     C*** BEG ADD ***** 11/19/03 *************************************
     C     Q1PRMS        IFGE      10
     C     SRTRFL        ANDEQ     'Y'
      *
     C     XXANPR        DIV(H)    ELANPR        XXFACT
     C     ELETER        MULT(H)   XXFACT        XXETER
     C                   MOVE      XXETER        SRAETC
      *
     C     XXMNPR        DIV(H)    ELANPR        XXFACT
     C     ELETER        MULT(H)   XXFACT        XXETER
     C                   MOVE      XXETER        SRMETC
      *
     C                   ENDIF
     C*** END ADD ***** 11/19/03 *************************************
     C*
     C                   END
     C*
     C     RETURN        TAG
     C*
     C***  Begin Add  ***  04/30/12  **************************************
     C     XXOPN         IFEQ      '1'
     C                   CLOSE     WDELL
     C                   MOVE      '0'           XXOPN
     C                   END
     C***  End Add   ****  04/30/12  **************************************
     C*
     C     SRTERM        IFEQ      '1'
     C                   RETURN
     C                   ELSE
     C                   MOVE      '1'           *INLR
     C                   END
     C*
     C***  Begin Add  ***  06/24/05  **************************************
     C*====================================================================
     C* Determine if this month should be reported based on the S/R freq
     C*====================================================================
     C     CHKFRQ        BEGSR
     C*
     C     *LIKE         DEFINE    *IN62         SVIN62
     C*
     C                   MOVE      *IN62         SVIN62
     C*
     C     BSKY1         CHAIN     PBS01                              62
     C*
     C     *IN62         IFEQ      *OFF
     C*
     C     BSSRFQ        IFEQ      'M'
     C     BSMSFL        IFEQ      'Y'
     C*            if month is not month to report - turn flag *off
     C                   MOVE      ELCO#         ##CO#
     C                   MOVE      ELPFDT        ##PFDT
     C                   MOVE      ELBSCD        ##BSCD
     C                   MOVE      XXMYSP        ##YYMM
     C*
     C                   CALL      'WA114R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##PFDT            7
     C                   PARM                    ##BSCD            3
     C                   PARM                    ##YYMM            5
     C                   PARM      *BLANK        ##EXFL            1
     C                   PARM      *BLANK        ##TDFL            1
     C*
     C     ##EXFL        IFEQ      'Y'
     C                   MOVE      *OFF          RPTFLG
     C                   ENDIF
     C*
     C                   ENDIF                                                  BSMSFL='Y'
     C                   ENDIF                                                  BSSRFQ='M'
     C*
     C     BSSRFQ        IFEQ      'Q'
     C     BSSRFQ        OREQ      'S'
     C*            if month is not month to report - turn flag *off
     C                   MOVE      ELCO#         ##CO#
     C                   MOVE      ELPFDT        ##PFDT
     C                   MOVE      ELBSCD        ##BSCD
     C                   MOVE      XXMYSP        ##YYMM
     C*
     C                   CALL      'WA114R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##PFDT            7
     C                   PARM                    ##BSCD            3
     C                   PARM                    ##YYMM            5
     C                   PARM      *BLANK        ##EXFL            1
     C                   PARM      *BLANK        ##TDFL            1
     C*
     C     ##EXFL        IFEQ      'Y'
     C                   MOVE      *OFF          RPTFLG
     C                   ENDIF
     C*
     C                   ENDIF                                                  BSSRFQ='Q'OR'S'
     C*
     C                   ENDIF                                                  *IN62=*OFF
     C*
     C                   ADD       XXEDAY        XXPDAY            5 0
     C*
     C                   MOVE      SVIN62        *IN62
     C*
     C                   ENDSR
     C***  End   Add  ***  06/24/05  **************************************
     C*** BEGIN ADD *** 07/05/95 *******************************************
     C*
     C     LEAPYR        BEGSR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     0             XXLPDT            8 0
     C                   Z-ADD     XXDT01        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXDT01
     C                   Z-ADD     XXDT02        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXDT02
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
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XQFROM            7 0
     C*********************MOVE *BLANK    QQERR   1
     C*********************MOVE '1'       QQTERM  1
     C*********************Z-ADDXQFROM    SRDTI
     C*********************CALL 'SRCVTDAT'DATCHK
     C*********************MOVELSRDTO     XQFROM
     C*
     C     SRERR         IFNE      '1'
     C                   SUB       1             XXWKDY            5 0
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
     C*** END ADD ***** 07/05/95 *******************************************
     C/EJECT
**
312831303130313130313031
