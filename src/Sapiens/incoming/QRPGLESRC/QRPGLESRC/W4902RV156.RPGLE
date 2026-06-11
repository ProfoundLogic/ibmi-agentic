     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H*****************************************************************
     H/TITLE W4902R - CALENDAR EARNED/UNEARNED PREM RPT - WORKFILE BLD
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM CREATES A WORKFILE OF EMPLOYERS TO FEED INTO   *
     H*   THE CALENDAR EARNED/UNEARNED REPORTED DATE RANGE.           *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W4901C                                                      *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: C:57                                              *
     H* CREATED: 12/04/96               PROJECT: 68AGB                *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/15/15 C:136 Support: 24504 (79634)                         *
     H*                Correct GPEA17 routine so that self report     *
     H*                transaction will report in the correct year.   *
     H*                                                               *
     H* 03/12/14 C:302 SUPPORT: WA114  81205                          *
     H*                Exclude retro cash transactions in all A/R     *
     H*                subroutines.  Converted subroutines to free    *
     H*                form. Changes not commented                    *
     H*                                                               *
     H* 05/02/12 C:279 SUPPORT: WA133  79634                          *
     H*                Created new version.                           *
     H*                                                               *
     H* 08/11/11 C:136 SUPPORT: WA099 74798                           *
     H*                CREATE W4902RV111                              *
     H*                DON'T ADJUST FROM DATE FOR FISCAL YEAR         *
     H*                PROCESSING ON POLICY TYPE RUN                  *
     H*                                                               *
     H* 09/03/10 C:190 SUPPORT: 74046                                 *
     H*                Created new version.                           *
     H*                                                               *
     H* 12/02/09 C:136 PROJECT: 25WPZ                                 *
     H*                ADD FISCAL YEAR PROCESSING                     *
     H*                                                               *
     H* 01/20/09 C:136 SUPPORT: WA139 65431                           *
     H*                NEW PGM SERIES TO TEST CHANGES WITH            *
     H*               -ADJUST LOGIC FOR SUMMARIZED FLAG TO ACCOUNT    *
     H*                FOR '@' VALUE.  (A)                            *
     H*               -PREVENT DUPLICATION OF 'EA' RECORDS IF NO      *
     H*                PREM HISTORY REC HAS BEEN WRITTEN  (B)         *
     H*               -REMOVE RESTRICTION THAT PREVENTED CX/RE        *
     H*                RECORDS FROM WRITING WHEN NO BILL HAS BEEN     *
     H*                RUN (C)                                        *
     H*                                                               *
     H* 10/30/08 C:220 SUPPORT: SY082  PRISS #34                      *
     H*                CONDITION THE 'SRCHKPGM' CALL IN W4902R        *
     H*                TO INVOKE ONLY IF THE CLARENDON PREPROCESSOR   *
     H*                MODULE IS INSTALLED.                           *
     H*                                                               *
     H* 11/12/07 C:136 SUPPORT: WA139 61045                           *
     H*                SUMMARIZED A/R WAS CHANGING KEY VALUE IN       *
     H*                WRITIT                                         *
     H*                                                               *
     H* 08/16/07 C:136 SUPPORT: WA139 59520                           *
     H*                SUMMARIZED A/R WAS CHANGING KEY VALUE IN       *
     H*                GPEA16 AND GPEA17                              *
     H*                                                               *
     H* 07/31/07 C:136 SUPPORT: WA114 59208                           *
     H*                ALLOW CEU TO PROCESS POLICIES IF THERE IS A    *
     H*                RETRO BILLING RECORD WITHIN THE VALUATION      *
     H*                DATE RANGE.                                    *
     H*                                                               *
     H* 04/19/07 C:136 SUPPORT: WA109 52632                           *
     H*                CODE THAT WRITES THE A/R RECORDS FOR SUMMARIZED*
     H*                EMPLOYERS CAUSES FIRST RECORD OF SUMMARIZED    *
     H*                EMPLOYER TO BE NOT WRITTEN.                    *
     H*                                                               *
     H* 05/17/06 C:136 SUPPORT: WA132 52173                           *
     H*                PREVENT A/R TRANSACTIONS WITH A POST DATE      *
     H*                <= 12/31/05 FROM ADJUSTING PREMIUM SO THAT     *
     H*                THE RDS CEU AND FISERV CEU CAN TIE TOGETHER.   *
     H*                                                               *
     H* 01/30/06 C:136 PROJECT: 114WHO                                *
     H*                ADD CODE (TEMPORARILY) THAT WILL EXCLUDE       *
     H*                RETRO DEDUCTIBLE PREMIUM HISTORY RECORDS.      *
     H*                                                               *
     H* 01/26/06 C:136 PROJECT: WC1293                                *
     H*                ADD FIELDS FOR DOMESTIC TERRORISM CHARGE       *
     H*                                                               *
     H* 04/08/05 C:136 PROJECT: WC1288                                *
     H*                IF SELF-REPORTING ACCOUNT IS SUMMARIZED,       *
     H*                WRITE TO WXEUP THE DIVISION THAT ORIGINATED    *
     H*                THE PAYROLL RECORD (EARDIV)                    *
     H*                                                               *
     H* 02/11/05 C:136 SUPPORT: WA109 45715                           *
     H*                If called from the Clarendon preprocessor,     *
     H*                capture policies that are two years back       *
     H*                instead of one year. This prevents errors      *
     H*                in the Clarendon preprocessor when it needs to *
     H*                calculate self-reported deferred amounts.      *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                Add code for voluntary labor charge.           *
     H*                                                               *
     H* 07/26/04 C:136 PROJECT: 93WEE                                 *
     H*                Add manual premium calculations.               *
     H*                                                               *
     H* 07/21/04 C:136 SUPPORT: WA093 42407                           *
     H*                Ensure that WXEUP records have zero values     *
     H*                in EUPRTD and EUPROR for cancellation and      *
     H*                reinstatement records.                         *
     H*                                                               *
     H* 01/08/04  C:33   Project: WC970                               *
     H*                  Correct expense constant processing.         *
     H*                                                               *
     H* 12/01/03  C:33   Project: WC970                               *
     H*                  Add processing for Terrorism Risk.           *
     H*                                                               *
     H* 10/31/02  C:142  SUPPORT                                      *
     H*                  WHEN TESTING PROJECT WC1094, FOUND THAT      *
     H*                  SOME WORKFILE RECORDS WERE BEING CREATED     *
     H*                  WHEN THE SHOULD NOT BE. SINCE THEY HAD       *
     H*                  ZERO AMOUNTS, THEY WERE NOT DETECTED BEFORE. *
     H*                  CHANGED TO NOT WRITE WORK FILE RECORD FROM   *
     H*                  WDXHP IF THE RECORD DID NOT EXIST IN WDEHP.  *
     H*                                                               *
     H* 10/22/02  C:142  PROJECT: WC1094                              *
     H*                  ADD STATE SORT PARM AND USE RISK STATE       *
     H*                                                               *
     H* 02/28/01  C:35   PROJECT: WC577                               *
     H*                  ADD CODE FOR INCL/EXCL EXPENSE CONSTANT      *
     H*                                                               *
     H* 07/24/00  C:65   PROJECT: WC609                               *
     H*                  DO NOT CONSIDER AUDIT REVERSAL RECORDS AS    *
     H*                  AUDITED.                                     *
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993                                  *
     H*                 Select only non-test groups.                  *
     H*                                                               *
     H* 05/11/00  C:65   PROJECT: WC609                               *
     H*                  ADD FLAG FOR AUDIT REVERSAL DESIGNATION.     *
     H*                                                               *
     H* 01/04/00  C:87   PROJECT: SUPP                                *
     H*                  Y2k issue: year(XXYY) was being represented  *
     H*                  as 2 digits instead of 3.                    *
     H*                                                               *
     H* 11/22/99  C:87   PROJECT: 96NCQ                               *
     H*                  Change process to run for all groups(except  *
     H*                  999) when the group passed in to the pgm     *
     H*                  equals zero (QQFND=0).                       *
     H*                                                               *
     H* 04/21/99  C:35   PROJECT: SUPPORT WA096 #16926                *
     H*                  IF AUDIT TRANSACTION DATE IS PRIOR TO THE    *
     H*                  POLICY EFFECTIVE DATE, EARN/INCLUDE THE      *
     H*                  AUDIT AS OF THE POLICY EFFECTIVE DATE.       *
     H*                                                               *
     H* 12/04/98  C:35   PROJECT: SUPPORT                             *
     H*                  ALWAYS START READING THE EMPLOYER DETAIL     *
     H*                  FILE 12 MONTHS BACK INSTEAD OF USING THE     *
     H*                  MAX EXTENDED POLICY MONTHS, SINCE THIS IS A  *
     H*                  CALENDAR REPORT.  ALSO, INCLUDE ALL          *
     H*                  EMPLOYERS WHOSE EFF DATE FALLS IN THE RANGE  *
     H*                  REGARDLESS OF THE EXPIRATION DATE, SO THAT   *
     H*                  AUDITED CANCELLED ACCOUNTS WILL STILL BE     *
     H*                  PICKED UP IF THE REPORT IS RUN AGAIN LATER.  *
     H*                                                               *
     H* 11/18/98  C:35   PROJECT: SUPPORT                             *
     H*                  ONCE AN EMPLOYER/DIV/FYR HAS MET THE         *
     H*                  CRITERIA FOR INCLUSION ON THE REPORT,        *
     H*                  RETRIEVE ALL CANCELLATION, REINSTATEMENT AND *
     H*                  A/R TRANSACTIONS UP TO THE VALUATION DATE.   *
     H*                                                               *
     H* 10/27/98  C:35   PROJECT: CM002                               *
     H*                  AUDITS SHOULD BE INCLUDED/EARNED BASED ON    *
     H*                  THE TRANSACTION DATE, REGARDLESS OF THE      *
     H*                  POSTING DATE.                                *
     H*                                                               *
     H* 10/19/98  C:35   PROJECT: CM002                               *
     H*                  USE EHFAF IN PLACE OF EHFAF2 FOR OLD DATA.   *
     H*                                                               *
     H* 08/06/98  C:35   PROJECT: CM002                               *
     H*                  CHANGES TO GET CODE IN SYNCH WITH HUMANA     *
     H*                  VERSION.                                     *
     H*                                                               *
     H*  04/29/98  C:78  PROJ:WC745                                   *
     H*                 F2CT01 IS NOT USED FOR ANYTHING IN PGM.       *
     H*                 REFERNCE FILE (WDF2P) DELETED.                *
     H*                                                               *
     H*  02/04/98 C:57 PROJECT CM036                                  *
     H*                ALLOW FOR SELECTION OF A/R RECORDS FROM WMEAP  *
     H*                WHEN EAARCD = F1SRBB. THIS CODE WAS ERRONEOUSLY*
     H*                REMOVED DURING THE V3 REIMPLEMENTATION.        *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  XX  XXXXXXXXXXXXXXXX                                         *
     H*****************************************************************
     F*
     FWDF1L     IF   E           K DISK
     FWDELL3    IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDXHL5    IF   E           K DISK
     FWDXHL6    IF   E           K DISK
     F                                     RENAME(PXH01:PXH02)
     FWDXHL7    IF   E           K DISK
     F                                     RENAME(PXH01:PXH03)
     FWDXHL8    IF   E           K DISK
     F                                     RENAME(PXH01:PXH04)
     FWDEHL3    IF   E           K DISK
     F                                     RENAME(PEH01:PEH03)
     FWDEHL6    IF   E           K DISK
     F                                     RENAME(PEH01:PEH06)
     FWDEHL7    IF   E           K DISK
     F                                     RENAME(PEH01:PEH07)
     FWDEHL8    IF   E           K DISK
     F                                     RENAME(PEH01:PEH08)
     FWMEAL12   IF   E           K DISK
     FWMEAL13   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
      **** BEGIN ADD 10/22/02 *****************************************
     FWMRSL     IF   E           K DISK
     F                                     RENAME(PRS01:PRS99)
      **** END   ADD 10/22/02 *****************************************
     FWXEUL     UF A E           K DISK
     FWTABL     IF   E           K DISK
     FWDXHL10   IF   E           K DISK
     F                                     RENAME(PXH01:PXH10)
     FWDXHL11   IF   E           K DISK
     F                                     RENAME(PXH01:PXH11)
     FWDXHL12   IF   E           K DISK
     F                                     RENAME(PXH01:PXH12)
     FWDXHL13   IF   E           K DISK
     F                                     RENAME(PXH01:PXH13)
     FWMEAL16   IF   E           K DISK
     F                                     RENAME(PEA01:PEA16)
     FWMEAL17   IF   E           K DISK
     F                                     RENAME(PEA01:PEA17)
      *** BEG ADD ***** 01/20/09 **************************************(B)
     FWMEAL23   IF   E           K DISK
     F                                     RENAME(PEA01:PEA23)
      *** END ADD ***** 01/20/09 **************************************(B)
      *** BEG ADD ***** 12/02/09 **************************************(B)
     FSMCOL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
      *** END ADD ***** 12/02/09 **************************************(B)
      *** BEG ADD ***** 10/30/08 *****************************************
     F* File to Read Clarendon Preprocessor module status
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
      *** END ADD ***** 10/30/08 *****************************************

     D*
     D                 DS
     D  XXSTDT                 1      7  0
     D  XXYY                   1      3  0
     D  XXMM                   4      5  0
      *** BEG ADD ***** 12/02/09 *****************************************
     D                 DS
     D  XXVLDT                 1      8  0
     D  XXVMTH                 1      2  0
     D  XXVDAY                 3      4  0
     D  XXVYR                  5      8  0
      *** END ADD ***** 12/02/09 *****************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELFYR         X1FYR
     C     *LIKE         DEFINE    EUPRDT        XXPRDT
     C     *LIKE         DEFINE    EUSORT        XXSORT
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    EAARCD        XXARCD
     C     *LIKE         DEFINE    EAAMT         XXAMT
     C     *LIKE         DEFINE    EATRDT        XXTRDT
     C     *LIKE         DEFINE    EAPODT        XXPODT
     C     *LIKE         DEFINE    EUFAF         XXFAF
     C     *LIKE         DEFINE    EUREVF        XXREVF
     C     *LIKE         DEFINE    EUIAPR        XXIAPR
     C     *LIKE         DEFINE    EUWAPR        XXWAPR
     C     *LIKE         DEFINE    EUWNMP        XXWNMP
     C     *LIKE         DEFINE    EUJAPR        XXJAPR
     C     *LIKE         DEFINE    EUXAPR        XXXAPR
     C     *LIKE         DEFINE    EUXNMP        XXXNMP
     C     *LIKE         DEFINE    EURWDT        XXRWDT
     C     *LIKE         DEFINE    EURWFL        XXRWFL
     C     *LIKE         DEFINE    EUIDT         XXIDT
     C     *LIKE         DEFINE    EUITM         XXITM
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    EUFAF2        XXFAF2
     C     *LIKE         DEFINE    EHPRTD        XXPRTD
     C     *LIKE         DEFINE    EHPROR        XXPROR
     C***  Begin Add  ***  12/01/03  **************************************
     C     *LIKE         DEFINE    EUITER        XXITER
     C     *LIKE         DEFINE    EUWTER        XXWTER
     C     *LIKE         DEFINE    EUXTER        XXXTER
     C     *LIKE         DEFINE    EUJTER        XXJTER
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 01/26/06 ******************************************
     C     *LIKE         DEFINE    EUIDTR        XXIDTR
     C     *LIKE         DEFINE    EUWDTR        XXWDTR
     C     *LIKE         DEFINE    EUXDTR        XXXDTR
     C     *LIKE         DEFINE    EUJDTR        XXJDTR
     C*** END ADD ***** 01/26/06 ******************************************
     C***  Begin Add  ***  07/26/04  **************************************
     C     *LIKE         DEFINE    EUWMPR        XXWMPR
     C     *LIKE         DEFINE    EUIMPR        XXIMPR
     C     *LIKE         DEFINE    EUJMPR        XXJMPR
     C     *LIKE         DEFINE    EUXMPR        XXXMPR
     C***  End   Add  ***  07/26/04  **************************************
     C***  BEG  ADD *****  09/01/04  **************************************
     C     *LIKE         DEFINE    EHWVLC        XXWVLC
     C     *LIKE         DEFINE    EHIVLC        XXIVLC
     C     *LIKE         DEFINE    EHJVLC        XXJVLC
     C     *LIKE         DEFINE    EHXVLC        XXXVLC
     C***  END  ADD *****  09/01/04  **************************************
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSTDT
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY3         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EAFND
     C                   KFLD                    EAEMP#
     C                   KFLD                    EADIV
     C                   KFLD                    EAFYR
     C*
     C*   KEY LIST FOR THE CANCELLATION HISTORY FILE
     C*
     C     XHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFDAT
     C*
     C     XHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     XHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE PREMIUM ENDORSEMENT HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFDAT
     C*
     C     EHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
      **** BEGIN ADD 10/31/02 *****************************************
     C     EHKY3         KLIST
     C                   KFLD                    XHCO#
     C                   KFLD                    XHFND
     C                   KFLD                    XHEMP#
     C                   KFLD                    XHDIV
     C                   KFLD                    XHFYR
      **** END   ADD 10/31/02 *****************************************
     C*
     C*
     C*   KEY LIST FOR THE WORKFILE
     C*
     C     EUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     FNKY1         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE A/R TRANSACTION FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFDAT
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X1FYR
     C*
     C*   KEY LIST FOR THE TABLE FILE
     C*
     C     TTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      **** BEGIN ADD 10/22/02 *****************************************
     C*
     C*   KEY LIST FOR THE RISK STATE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
      **** END   ADD 10/22/02 *****************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQVLDT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQPRNT            1
      **** BEGIN ADD 10/22/02 *****************************************
     C                   PARM                    QQSORT            1
      **** END   ADD 10/22/02 *****************************************
      **** BEGIN ADD **** 10/22/02 ************************************
     C     Q1PRMS        IFLT      09
     C                   MOVE      ' '           QQSRTS            1
     C                   ELSE
     C                   MOVE      QQSORT        QQSRTS
     C                   ENDIF
      **** END   ADD **** 10/22/02 ************************************
      **** BEGIN ADD **** 05/16/06 ************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      **** END   ADD **** 05/16/06 ************************************
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
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C*
     C*  CONVERT MMDDCCYY FROM DATE TO CYYMMDD
     C*
     C                   MOVE      QQFRDT        XXFRDT            8 0          MMDDCCYY
      *** BEG ADD ***** 12/02/09 *****************************************
      *** BEG CHG ***** 08/11/11 *****************************************
     C***********          EXSR GETFYR
     C     QQTYPE        IFNE      'P'
     C                   EXSR      GETFYR
     C                   ENDIF
      *** END CHG ***** 08/11/11 *****************************************
      *** END ADD ***** 12/02/09 *****************************************
      *** BEG ADD ***** 02/11/05 *****************************************
     C*
     C*   CHECK IF CALLED FROM PREPROCESSOR AND SUBTRACT 2 YEARS FROM THE FROM
     C*   DATE IF THIS IS TRUE
     C*
      *** BEG ADD ***** 10/30/08 *****************************************
     C*
     C* Read file SWCPF to check WCFL20 for the Clarendon Preprocessor
     C*
     C                   READ      SWC01                                  35
     C     WCFL20        IFEQ      'A'
     C     WCFL20        OREQ      'B'
     C*
      *** END ADD ***** 10/30/08 *****************************************
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W8126C'      XXPGM            10
     C                   PARM      *BLANKS       SRERR
      *** BEG ADD ***** 10/30/08 *****************************************
     C*
     C                   ENDIF
     C*
      *** END ADD ***** 10/30/08 *****************************************
     C*
     C     SRERR         IFEQ      *ON
     C                   SUB       2             XXFRDT
     C                   ENDIF                                                  SRERR=*ON
      *** END ADD ***** 02/11/05 *****************************************
     C*
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFDAT            7 0          CYYMMDD
     C*
     C*  CONVERT MMDDCCYY TO DATE TO CYYMMDD
     C*
     C                   MOVE      QQTODT        XXTODT            8 0          MMDDCCYY
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDAT            7 0          CYYMMDD
     C*
     C*  CONVERT MMDDCCYY VALUE DATE TO CYYMMDD
     C*
     C                   MOVE      QQVLDT        XXVLDT            8 0          MMDDCCYY
     C                   Z-ADD     XXVLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXVDAT            7 0          CYYMMDD
     C*
     C*  CALC FIRST DAY OF VALUE YEAR, BASED ON VALUE DATE.
     C*
     C                   MOVE      XXVDAT        XXJAN1            7 0          CYYMMDD
     C                   MOVE      0101          XXJAN1
     C*
     C*  CALC FIRST DAY OF VALUE MONTH, BASED ON VALUE DATE.
     C*
     C                   MOVE      XXVDAT        XXMTH1            7 0          CYYMMDD
     C                   MOVE      01            XXMTH1
     C*
     C* DETERMINE START DATE FOR EMPLOYER DETAIL FILE.  USE MAX
     C* EXTENDED YEAR POLICY VALUE FROM FUND MASTER FILE.
     C*
     C                   MOVE      QQFND         Q1FND             3 0
     C*
     C     Q1FND         IFNE      0
     C     FNKY          CHAIN     PFN01                              60
     C                   ELSE
     C     FNKY1         CHAIN     PFN01                              60
     C                   ENDIF                                                  QQFND=0
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     FNFND         XXFND
     C*
     C*
     C     FNTEST        IFNE      'Y'
     C*** END CHANGE ****  06/10/00  **********************************
     C     Q1FND         ORNE      0
     C*
      *
      * NEED TO ALWAYS USE 12 MONTHS, SINCE THIS IS A CALENDAR REPORT
      *
     C                   Z-ADD     12            FNMXEX
     C*
     C     FNMXEX        DIV       12            XXYEAR            2 0
     C                   MVR                     XXMNTH            2 0
     C                   MOVE      XXFDAT        XXSTDT
     C                   SUB       XXYEAR        XXYY
     C*
     C     XXMM          IFGT      XXMNTH
     C                   SUB       XXMNTH        XXMM
     C                   ELSE
     C                   SUB       1             XXYY
     C                   SUB       XXMM          XXMNTH
     C     12            SUB       XXMNTH        XXMM
     C                   ENDIF                                                  XXMM IFGT
     C*
     C*
     C*  LOAD THE FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS
     C*  SET UP FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C*  PROCESS EMPLOYER DETAIL FILE TO RETRIEVE RECORDS
     C*
     C                   EXSR      GPEL01
     C*
     C*  PROCESS EMPLOYER HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH EFFECTIVE DATE IN FROM/TO RANGE
     C*
     C                   EXSR      GPEH07
     C*
     C*  PROCESS EMPLOYER HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH TRANSACTION DATE IN FROM/TO RANGE
     C*
     C                   EXSR      GPEH08
     C*
     C*  PROCESS ACCTS RECEIVABLE FILE TO RETRIEVE RECORDS
     C*   WITH TRANSACTION DATE IN FROM/TO RANGE
     C*
     C                   EXSR      GPEA01
     C*
     C*  PROCESS ACCTS RECEIVABLE FILE TO RETRIEVE RECORDS
     C*   WITH POSTING DATE IN FROM/TO RANGE
     C*
     C                   EXSR      GPEA02
     C*
     C*  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH CANCEL ISSUE DATE IN FROM/TO RANGE
     C*
     C                   EXSR      GPXH01
     C*
     C*  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH CANCELLATION DATE IN FROM/TO RANGE
     C*
     C                   EXSR      GPXH02
     C*
     C*  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH REINSTATEMENT ISSUE DATE IN FROM/TO RANGE
     C*
     C                   EXSR      GPXH03
     C*
     C*  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH REINSTATEMENT DATE IN FROM/TO RANGE
     C*
     C                   EXSR      GPXH04
     C*
     C***  BEGIN ADD  ***  11/22/99  **********************************
     C                   ENDIF                                                  XXFND<>999
     C*
     C     Q1FND         IFEQ      0
     C     FNKY1         READE     PFN01                                  60
     C                   ELSE
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     TERM          TAG
     C                   MOVE      *ON           *INLR
      /EJECT
     C********************************************************************
     C*  PROCESS EMPLOYER DETAIL FILE TO RETRIEVE RECORDS
     C********************************************************************
     C     GPEL01        BEGSR
     C*
     C*  START EMPLR DETAIL AT F1MXEX MONTHS BEFORE FROM DATE PARM
     C*
     C     ELKY          SETLL     PEL01
     C     ELKY1         READE     PEL01                                  61
     C*
     C*  PROCESS UNTIL EOF OR EMPLR PFDT > TO DATE PARM
     C*
     C     *IN61         DOWEQ     *OFF
     C     ELPFDT        ANDLE     XXTDAT
     C*
     C*  EXCLUDE PEL01 RECS IF POLICY TO DATE < FROM DATE PARM
     C*
      **** BEGIN DEL **** 12/04/98 *********************************************
     C********** ELPTDT    IFGE XXFDAT
      **** END   DEL **** 12/04/98 *********************************************
     C*
     C*  THERE MUST BE AT LEAST ONE PREMIUM HISTORY RECORD.
     C*
     C                   MOVE      ELEMP#        XXEMP#
     C                   MOVE      ELDIV         XXDIV
     C                   MOVE      ELFYR         XXFYR
     C*
     C     EHKY          SETGT     PEH03
     C     EHKY          READPE    PEH03                                  62
     C*
     C     *IN62         IFEQ      *OFF
     C                   MOVE      '0'           XXSORT
     C*
     C*  WRITE WORKFILE RECORD
     C*
     C                   EXSR      EURITE
     C*
     C                   ENDIF                                                  *IN62 IFEQ
      **** BEGIN DEL **** 12/04/98 *********************************************
     C******************** ENDIF                           ELPTDT IFGT
      **** END   DEL **** 12/04/98 *********************************************
     C*
     C     ELKY1         READE     PEL01                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
     C*  PROCESS EMPLOYER HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH EFFECTIVE DATE IN FROM/TO RANGE
     C********************************************************************
     C     GPEH07        BEGSR
     C*
     C*  START EMPLOYER HISTORY FILE
     C*
     C     EHKY1         SETLL     PEH07
     C     EHKY2         READE     PEH07                                  61
     C*
     C*  PROCESS UNTIL EOF
     C*
     C     *IN61         DOWEQ     *OFF
      *** BEG ADD ***** 01/30/06 **********************************
     C     EHFAF         IFNE      'X'
      *** END ADD ***** 01/30/06 **********************************
     C     EHFAF         IFNE      'A'
     C     EHEFDT        ANDLE     XXVDAT
     C     EHFAF         OREQ      'A'
     C     EHTRDT        ANDLE     XXVDAT
     C*** BEGIN ADD ***  07/24/00  ************************************
     C     EHREVF        ANDNE     'Y'
     C     EHFAF         OREQ      'A'
     C     EHREVF        ANDEQ     'Y'
     C     EHEFDT        ANDLE     XXVDAT
     C*** END ADD *****  07/24/00  ************************************
     C*
     C                   MOVE      EHEMP#        XXEMP#
     C                   MOVE      EHDIV         XXDIV
     C                   MOVE      EHFYR         XXFYR
     C                   MOVE      '0'           XXSORT
      **** BEGIN ADD **** 04/21/99 *********************************************
     C                   MOVE      *OFF          *IN63
      *
     C     EHFAF         IFEQ      'A'
     C*** BEGIN ADD ***  07/24/00  ************************************
     C     EHREVF        ANDNE     'Y'
     C*** END ADD *****  07/24/00  ************************************
      *
     C     ELKY2         CHAIN     PEL02                              63
      *
     C     *IN63         IFEQ      *OFF
      *
     C     EHTRDT        IFLT      ELPFDT
     C     ELPFDT        ANDGT     XXVDAT
      *
     C                   MOVE      *ON           *IN63
      *
     C                   ENDIF                                                  EHTRDT IFLT ELPFDT
      *
     C                   ENDIF                                                  *IN63 IFEQ *OFF
      *
     C                   ENDIF                                                  EHFAF IFEQ 'A'
      **** END   ADD **** 04/21/99 *********************************************
     C*
     C*  WRITE WORKFILE RECORD
     C*
      **** BEGIN ADD **** 04/21/99 *********************************************
     C     *IN63         IFEQ      *OFF
      **** END   ADD **** 04/21/99 *********************************************
     C                   EXSR      EURITE
      **** BEGIN ADD **** 04/21/99 *********************************************
     C                   ENDIF                                                  *IN63 IFEQ *OFF
      **** END   ADD **** 04/21/99 *********************************************
     C*
      **** BEGIN ADD **** 10/27/98 *********************************************
     C                   ENDIF                                                  EHFAF IFNE 'A'
      **** END   ADD **** 10/27/98 *********************************************
      *** BEG ADD ***** 01/30/06 **********************************
     C                   ENDIF                                                  EHFAF <> 'X'
      *** END ADD ***** 01/30/06 **********************************
     C     EHKY2         READE     PEH07                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
     C*  PROCESS EMPLOYER HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH TRANSACTION DATE IN FROM/TO RANGE
     C********************************************************************
     C     GPEH08        BEGSR
     C*
     C*  START EMPLOYER HISTORY FILE
     C*
     C     EHKY1         SETLL     PEH08
     C     EHKY2         READE     PEH08                                  61
     C*
     C*  PROCESS UNTIL EOF
     C*
     C     *IN61         DOWEQ     *OFF
     C     EHTRDT        ANDLE     XXVDAT
     C*
      *** BEG ADD ***** 01/30/06 **********************************
     C     EHFAF         IFNE      'X'
      *** END ADD ***** 01/30/06 **********************************
     C*
     C                   MOVE      EHEMP#        XXEMP#
     C                   MOVE      EHDIV         XXDIV
     C                   MOVE      EHFYR         XXFYR
     C                   MOVE      '0'           XXSORT
     C*
     C*  WRITE WORKFILE RECORD
     C*
     C                   EXSR      EURITE
     C*
      *** BEG ADD ***** 01/30/06 **********************************
     C                   ENDIF                                                  EHFAF <> 'X'
      *** END ADD ***** 01/30/06 **********************************
     C*
     C     EHKY2         READE     PEH08                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
     C*  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH CANCEL ISSUE DATE IN FROM/TO RANGE
     C********************************************************************
     C     GPXH01        BEGSR
     C*
     C*  START CANCELLATION HISTORY FILE
     C*
     C     XHKY          SETLL     PXH01
     C     XHKY1         READE     PXH01                                  61
     C*
     C*  PROCESS UNTIL EOF
     C*
     C     *IN61         DOWEQ     *OFF
     C     XHCDAT        ANDLE     XXVDAT
     C*
     C     XHDT          IFLE      XXVDAT
     C                   MOVE      XHEMP#        XXEMP#
     C                   MOVE      XHDIV         XXDIV
     C                   MOVE      XHFYR         XXFYR
     C                   MOVE      '0'           XXSORT
     C*
     C*  WRITE WORKFILE RECORD
     C*
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C********** EHKY3     CHAINWDEHL3               64
     C********** *IN64     IFEQ *OFF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C                   EXSR      EURITE
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C**********           ENDIF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C                   ENDIF
     C*
     C     XHKY1         READE     PXH01                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
     C*  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH CANCELLATION DATE IN FROM/TO RANGE
     C********************************************************************
     C     GPXH02        BEGSR
     C*
     C*  START CANCELLATION HISTORY FILE
     C*
     C     XHKY          SETLL     PXH02
     C     XHKY1         READE     PXH02                                  61
     C*
     C*  PROCESS UNTIL EOF
     C*
     C     *IN61         DOWEQ     *OFF
     C     XHCXDT        ANDLE     XXVDAT
     C*
     C     XHDT          IFLE      XXVDAT
     C                   MOVE      XHEMP#        XXEMP#
     C                   MOVE      XHDIV         XXDIV
     C                   MOVE      XHFYR         XXFYR
      **** BEGIN CHG **** 11/18/98 ************************************
     C******************** MOVE XHCXDT    XXPRDT
     C******************** MOVE XHCDAT    XXTRDT
     C******************** MOVE XHCXDT    XXPODT
     C******************** MOVE '3'       XXSORT
     C******************** MOVE XHDT      XXIDT
     C******************** MOVE XHTIME    XXITM
     C                   MOVE      '0'           XXSORT
      **** END   CHG **** 11/18/98 ************************************
     C*
     C*  WRITE WORKFILE RECORD
     C*
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C********** EHKY3     CHAINWDEHL3               64
     C********** *IN64     IFEQ *OFF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C                   EXSR      EURITE
     C                   ENDIF
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C**********           ENDIF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C*
     C     XHKY1         READE     PXH02                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
     C*  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH REINSTATEMENT ISSUE DATE IN FROM/TO RANGE
     C********************************************************************
     C     GPXH03        BEGSR
     C*
     C*  START CANCELLATION HISTORY FILE
     C*
     C     XHKY          SETLL     PXH03
     C     XHKY1         READE     PXH03                                  61
     C*
     C*  PROCESS UNTIL EOF
     C*
     C     *IN61         DOWEQ     *OFF
     C     XHRDAT        ANDLE     XXVDAT
     C*
     C     XHDT          IFLE      XXVDAT
     C                   MOVE      XHEMP#        XXEMP#
     C                   MOVE      XHDIV         XXDIV
     C                   MOVE      XHFYR         XXFYR
      **** BEGIN CHG **** 11/18/98 ************************************
     C******************** MOVE XHRDAT    XXPRDT
     C******************** MOVE XHRDAT    XXTRDT
     C******************** MOVE XHREDT    XXPODT
     C******************** MOVE '4'       XXSORT
     C******************** MOVE XHDT      XXIDT
     C******************** MOVE XHTIME    XXITM
     C                   MOVE      '0'           XXSORT
      **** END   CHG **** 11/18/98 ************************************
     C*
     C*  WRITE WORKFILE RECORD
     C*
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C********** EHKY3     CHAINWDEHL3               64
     C********** *IN64     IFEQ *OFF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C                   EXSR      EURITE
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C**********           ENDIF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C                   ENDIF
     C*
     C     XHKY1         READE     PXH03                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
     C*  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH REINSTATEMENT DATE IN FROM/TO RANGE
     C********************************************************************
     C     GPXH04        BEGSR
     C*
     C*  START CANCELLATION HISTORY FILE
     C*
     C     XHKY          SETLL     PXH04
     C     XHKY1         READE     PXH04                                  61
     C*
     C*  PROCESS UNTIL EOF
     C*
     C     *IN61         DOWEQ     *OFF
     C     XHREDT        ANDLE     XXVDAT
     C*
     C     XHDT          IFLE      XXVDAT
     C                   MOVE      XHEMP#        XXEMP#
     C                   MOVE      XHDIV         XXDIV
     C                   MOVE      XHFYR         XXFYR
      **** BEGIN CHG **** 11/18/98 ************************************
     C******************** MOVE XHREDT    XXPRDT
     C******************** MOVE XHRDAT    XXTRDT
     C******************** MOVE XHREDT    XXPODT
     C******************** MOVE '4'       XXSORT
     C******************** MOVE XHDT      XXIDT
     C******************** MOVE XHTIME    XXITM
     C                   MOVE      '0'           XXSORT
      **** END   CHG **** 11/18/98 ************************************
     C*
     C*  WRITE WORKFILE RECORD
     C*
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C********** EHKY3     CHAINWDEHL3               64
     C********** *IN64     IFEQ *OFF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C                   EXSR      EURITE
     C                   ENDIF
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C**********           ENDIF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C*
     C     XHKY1         READE     PXH04                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
      **** BEGIN ADD **** 11/18/98 ************************************
      ********************************************************************
      *  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
      *   WITH CANCEL ISSUE DATE LESS THAN TO DATE
      ********************************************************************
     C     GPXH10        BEGSR
      *
      *  START CANCELLATION HISTORY FILE
      *
     C     XHKY2         CHAIN     PXH10                              61
      *
      *  PROCESS UNTIL EOF
      *
     C     *IN61         DOWEQ     *OFF
     C     XHCDAT        ANDLE     XXVDAT
      *
     C     XHDT          IFLE      XXVDAT
     C                   MOVE      XHEMP#        XXEMP#
     C                   MOVE      XHDIV         XXDIV
     C                   MOVE      XHFYR         XXFYR
     C                   MOVE      XHCDAT        XXPRDT
     C                   MOVE      XHCDAT        XXTRDT
     C                   MOVE      XHCXDT        XXPODT
     C                   MOVE      '3'           XXSORT
     C                   MOVE      XHDT          XXIDT
     C                   MOVE      XHTIME        XXITM
      *
      *  WRITE WORKFILE RECORD
      *
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C********** EHKY3     CHAINWDEHL3               64
     C********** *IN64     IFEQ *OFF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C***  Begin Add  ***  01/08/04  **************************************
      *** BEG DEL ***** 07/21/04 *****************************************
     C*****************    MOVE EHPRTD    XXPRTD
     C*****************    MOVE EHPROR    XXPROR
      *** END DEL ***** 07/21/04 *****************************************
     C***  End   Add  ***  01/08/04  **************************************
     C                   EXSR      WRITIT
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C**********           ENDIF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C                   ENDIF
      *
     C     XHKY2         READE     PXH10                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
      *
     C                   ENDSR                                                  GPXH10
      /EJECT
      ********************************************************************
      *  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
      *   WITH CANCELLATION DATE LESS THAN TO DATE
      ********************************************************************
     C     GPXH11        BEGSR
      *
      *  START CANCELLATION HISTORY FILE
      *
     C     XHKY2         CHAIN     PXH11                              61
      *
      *  PROCESS UNTIL EOF
      *
     C     *IN61         DOWEQ     *OFF
     C     XHCXDT        ANDLE     XXVDAT
      *
     C     XHDT          IFLE      XXVDAT
     C                   MOVE      XHEMP#        XXEMP#
     C                   MOVE      XHDIV         XXDIV
     C                   MOVE      XHFYR         XXFYR
     C                   MOVE      XHCXDT        XXPRDT
     C                   MOVE      XHCDAT        XXTRDT
     C                   MOVE      XHCXDT        XXPODT
     C                   MOVE      '3'           XXSORT
     C                   MOVE      XHDT          XXIDT
     C                   MOVE      XHTIME        XXITM
      *
      *  WRITE WORKFILE RECORD
      *
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C********** EHKY3     CHAINWDEHL3               64
     C********** *IN64     IFEQ *OFF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
      *
     C***  Begin Add  ***  01/08/04  **************************************
      *** BEG DEL ***** 07/21/04 *****************************************
     C***********          MOVE EHPRTD    XXPRTD
     C***********          MOVE EHPROR    XXPROR
      *** END DEL ***** 07/21/04 *****************************************
     C***  End   Add  ***  01/08/04  **************************************
     C                   EXSR      WRITIT
     C                   ENDIF
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C**********           ENDIF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
      *
     C     XHKY2         READE     PXH11                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
      *
     C                   ENDSR                                                  GPXH11
      /EJECT
      ********************************************************************
      *  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
      *   WITH REINSTATEMENT ISSUE DATE LESS THAN THE TO DATE
      ********************************************************************
     C     GPXH12        BEGSR
      *
      *  START CANCELLATION HISTORY FILE
      *
     C     XHKY2         CHAIN     PXH12                              61
      *
      *  PROCESS UNTIL EOF
      *
     C     *IN61         DOWEQ     *OFF
     C     XHRDAT        ANDLE     XXVDAT
      *
     C     XHDT          IFLE      XXVDAT
     C                   MOVE      XHEMP#        XXEMP#
     C                   MOVE      XHDIV         XXDIV
     C                   MOVE      XHFYR         XXFYR
     C                   MOVE      XHRDAT        XXPRDT
     C                   MOVE      XHRDAT        XXTRDT
     C                   MOVE      XHREDT        XXPODT
     C                   MOVE      '4'           XXSORT
     C                   MOVE      XHDT          XXIDT
     C                   MOVE      XHTIME        XXITM
      *
      *  WRITE WORKFILE RECORD
      *
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C********** EHKY3     CHAINWDEHL3               64
     C********** *IN64     IFEQ *OFF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
      *
     C***  Begin Add  ***  01/08/04  **************************************
      *** BEG DEL ***** 07/21/04 *****************************************
     C***********          MOVE EHPRTD    XXPRTD
     C***********          MOVE EHPROR    XXPROR
      *** END DEL ***** 07/21/04 *****************************************
     C***  End   Add  ***  01/08/04  **************************************
     C                   EXSR      WRITIT
     C                   ENDIF
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C**********           ENDIF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
      *
     C     XHKY2         READE     PXH12                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
      *
     C                   ENDSR                                                  GPXH12
      /EJECT
      ********************************************************************
      *  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
      *   WITH REINSTATEMENT DATE LESS THAN THE TO DATE
      ********************************************************************
     C     GPXH13        BEGSR
      *
      *  START CANCELLATION HISTORY FILE
      *
     C     XHKY2         CHAIN     PXH13                              61
      *
      *  PROCESS UNTIL EOF
      *
     C     *IN61         DOWEQ     *OFF
     C     XHREDT        ANDLE     XXVDAT
      *
     C     XHDT          IFLE      XXVDAT
     C                   MOVE      XHEMP#        XXEMP#
     C                   MOVE      XHDIV         XXDIV
     C                   MOVE      XHFYR         XXFYR
     C                   MOVE      XHREDT        XXPRDT
     C                   MOVE      XHRDAT        XXTRDT
     C                   MOVE      XHREDT        XXPODT
     C                   MOVE      '4'           XXSORT
     C                   MOVE      XHDT          XXIDT
     C                   MOVE      XHTIME        XXITM
      *
      *  WRITE WORKFILE RECORD
      *
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C********** EHKY3     CHAINWDEHL3               64
     C********** *IN64     IFEQ *OFF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
     C***  Begin Add  ***  01/08/04  **************************************
      *** BEG DEL ***** 07/21/04 *****************************************
     C***********          MOVE EHPRTD    XXPRTD
     C***********          MOVE EHPROR    XXPROR
      *** END DEL ***** 07/21/04 *****************************************
     C***  End   Add  ***  01/08/04  **************************************
     C                   EXSR      WRITIT
     C                   ENDIF
      **** BEGIN ADD 10/31/02 *****************************************
      **** BEG DEL ***** 01/20/09 ********************************* (C)
     C**********           ENDIF
      **** END DEL ***** 01/20/09 ********************************* (C)
      **** END   ADD 10/31/02 *****************************************
      *
     C     XHKY2         READE     PXH13                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
      *     `
     C                   ENDSR                                                  GPXH13
      /EJECT

       //*******************************************************************
       //  PROCESS ACCTS RECEIVABLE FILE TO RETRIEVE RECORDS
       //   WITH TRANSACTION DATE LESS THAN TO DATE
       //      CHECK TO SEE IF ANY SELF-REPORTING OR CLAIMS REPLENISHMENT BILL
       //      TRANSACTIONS EXIST FOR THE REPORTING PERIOD.
       //*******************************************************************
      /FREE
       Begsr GPEA16;

          X1fyr = 0;
          Setll EAKY2 PEA16;
          Reade EAKY2 PEA16;

          Dow Not %EOF And EAtrdt <= XXvdat;

           //  GET FUND DETAIL RECORD

             If EAfyr <> X1FYR;
                x1fyr = EAfyr;
                Chain F1KY PF101;
             Endif;

             TTctyp = *Blanks;
             If EActyp = *blanks And EAarcd <> F1srbb;
                Chain (xxCo#: EAarcd) PTT01;
             Else;
                 TTctyp = EActyp;
             Endif;

             If (TTctyp = 'C' And EAarty = 'B')
              Or EAarcd = F1srbb;

                xxEmp# = EAemp#;
                xxDiv = EAdiv;
                xxFyr = EAfyr;
                xxPrdt = EAtrdt;
                xxArcd = EAarcd;
                xxAmt = EAamt;
                xxTrdt = EAtrdt;
                xxPodt = EApodt;
                xxSort = '1';
                xxIdt = EAdt;
                xxItm = EAtime;

                Select;
                   When CLtid = '136'
                    And EAtrdt <= 1051231;

                   Other;
                      Exsr WRITIT;
                Endsl;

             EndIf;

             Reade EAKY2 PEA16;

          Enddo;

       Endsr;

       //*******************************************************************
       //  PROCESS ACCTS RECEIVABLE FILE TO RETRIEVE RECORDS
       //   WITH POSTING DATE LESS THAN TO DATE
       //      CHECK TO SEE IF ANY SELF-REPORTING OR CLAIMS REPLENISHMENT BILL
       //      TRANSACTIONS EXIST FOR THE REPORTING PERIOD.
       //*******************************************************************
       Begsr GPEA17;
          //
          x1Fyr = 0;
          Setll EAKY2 PEA17;
          Reade EAKY2 PEA17;

          //
          Dow Not %EOF And EApodt <= XXVdat;

             //  GET FUND DETAIL RECORD

             If EAfyr <> x1Fyr;
                x1Fyr = EAfyr;
                Chain F1KY PF101;
             Endif;

             TTctyp = *Blanks;
             If EActyp = *blanks And EAarcd <> F1srbb;
                Chain (xxCo#: EAarcd) PTT01;
             Else;
                 TTctyp = EActyp;
             Endif;

             If (TTCTYP = 'C' And EAarty = 'B')
              Or EAarcd = F1srbb;

                xxEmp# = EAemp#;
                xxDiv = EAdiv;
                xxFyr = EAfyr;
          //*** beg chg ***** 09/15/15 ********************
          //*** xxPodt = EAtrdt;
                xxPrdt = EAtrdt;
          //*** beg chg ***** 09/15/15 ********************
                xxArcd = EAarcd;
                xxAmt = EAamt;
                xxTrdt = EAtrdt;
                xxPodt = EApodt;
                xxSort = '1';
                xxIdt = EAdt;
                xxItm = EAtime;

                Select;
                   When CLtid = '136'
                    And EAtrdt <= 1051231;

                   Other;
                      Exsr WRITIT;
                Endsl;

             EndIf;

             Reade EAKY2 PEA17;

          Enddo;

       Endsr;

       //*******************************************************************
       //  PROCESS ACCTS RECEIVABLE FILE TO RETRIEVE RECORDS
       //   WITH TRANSACTION DATE IN FROM/TO RANGE
       //      CHECK TO SEE IF ANY SELF-REPORTING OR CLAIMS REPLENISHMENT BILL
       //      TRANSACTIONS EXIST FOR THE REPORTING PERIOD.
       //*******************************************************************
       Begsr GPEA01;

          X1fYR = 0;
          Setll EAKY PEA01;
          Reade EAKY1 PEA01;

          Dow Not %EOF And EAtrdt <= xxVdat;

             //  GET FUND DETAIL RECORD

             If EAfyr <> x1Fyr;
                x1Fyr = EAfyr;
                Chain F1KY PF101;
             Endif;

             TTctyp = *Blanks;
             If EActyp = *blanks And EAarcd <> F1srbb;
                Chain (xxCo#: EAarcd) PTT01;
             Else;
                 TTctyp = EActyp;
             Endif;

             If (TTctyp = 'C' And EAarty = 'B')
              Or EAarcd = F1srbb;

                xxEmp# = EAemp#;
                xxDiv = EAdiv;
                xxFyr = EAfyr;
                xxSort = '0';

                Select;
                   When CLtid = '136'
                    And EAtrdt <= 1051231;

                   Other;
                      Chain ELKY3 PEL02;
                      If ELarsm <> *BLANK;
                         xxDiv = EArdiv;
                      Endif;

                   Exsr EURITE;

                Endsl;

             Endif;

             Reade EAKY1 PEA01;

          Enddo;

       Endsr;

       //*******************************************************************
       //  PROCESS ACCTS RECEIVABLE FILE TO RETRIEVE RECORDS
       //   WITH POSTING DATE IN FROM/TO RANGE
       //      CHECK TO SEE IF ANY SELF-REPORTING OR CLAIMS REPLENISHMENT BILL
       //      TRANSACTIONS EXIST FOR THE REPORTING PERIOD.
       //*******************************************************************
       Begsr GPEA02;

          x1Fyr = 0;
          Setll EAKY PEA02;
          Reade EAKY1 PEA02;

          Dow Not %EOF And EApodt <= xxVdat;

             //  GET FUND DETAIL RECORD

             If EAfyr <> x1Fyr;
                x1Fyr = EAfyr;
                Chain F1KY PF101;
             Endif;

             TTctyp = *Blanks;
             If EActyp = *blanks And EAarcd <> F1srbb;
                Chain (xxCo#: EAarcd) PTT01;
             Else;
                 TTctyp = EActyp;
             Endif;

             If (TTctyp = 'C' And EAarty = 'B')
                Or EAarcd = F1srbb;

                xxEmp# = EAemp#;
                xxDiv = EAdiv;
                xxFyr = EAfyr;
                xxSort = '0';

                Select;
                   When CLtid = '136'
                    And EAtrdt <= 1051231;

                   Other;
                      Chain ELKY3 PEL02;
                      If ELarsm <> *BLANK;
                         xxDiv = EArdiv;
                      Endif;

                   Exsr EURITE;

                Endsl;

             Endif;

             Reade EAKY1 PEA02;

          Enddo;

       Endsr;
      /EJECT

      /END-FREE

     C*  WRITE WORKFILE RECORD
     C********************************************************************
     C     EURITE        BEGSR
     C*
     C*  GET EMPLOYER NAME
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMNAME        XXNAME
     C                   ELSE
     C                   MOVE      *BLANK        XXNAME
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  DETERMINE IF THIS POLICY HAS ALREADY BEEN WRITTEN INTO THE WORKFILE
     C*
      *** BEG CHG ***** 04/19/07 *****************************************
     C********** EUKY      CHAINPEU01                69
     C     EUKY          SETLL     PEU01
     C     EUKY          READE     PEU01                                  69
     C     *IN69         DOWEQ     *OFF
     C     EUSORT        IFEQ      '2'
     C     EUSORT        OREQ      '5'
      *** BEG ADD ***** 01/20/09 *****************************************
     C     EUSORT        OREQ      '1'                                                      (B)
     C     EUSORT        OREQ      '3'                                                      (C)
     C     EUSORT        OREQ      '4'                                                      (C)
      *** END ADD ***** 01/20/09 *****************************************
     C                   LEAVE
     C                   ENDIF
     C     EUKY          READE     PEU01                                  69
     C                   ENDDO
      *** END CHG ***** 04/19/07 *****************************************
     C*
     C                   EXSR      WRITIT
     C*
     C*  IF FIRST WRITE FOR EMPLOYER, GET ALL QUALIFYING EMPLOYER HISTORY
     C*
     C*** BEG ADD ***** 01/20/09 **********************************(B)
     C                   MOVE      'N'           XXARFL            1
     C*** END ADD ***** 01/20/09 **********************************(B)
     C     *IN69         IFEQ      *ON
     C*** BEG ADD ***** 01/20/09 **********************************(B)
     C     ELKY2         CHAIN     PEL02                              60
     C     ELARSM        IFNE      ' '
     C                   MOVE      'Y'           XXARFL
     C                   ELSE
     C                   MOVE      'N'           XXARFL
     C                   ENDIF
     C*** END ADD ***** 01/20/09 **********************************(B)
     C*
     C*  PROCESS EMPLOYER HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH EFFECTIVE LESS THAN TO TO DATE
     C*
     C                   EXSR      GPEH03
     C*
     C*  PROCESS EMPLOYER HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH TRANSACTION DATE LESS THAN TO DATE
     C*
     C                   EXSR      GPEH06
      **** BEGIN ADD **** 11/18/98 ************************************
      *
      *  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
      *   WITH CANCEL ISSUE DATE LESS THAN TO DATE
      *
     C                   EXSR      GPXH10
      *
      *  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
      *   WITH CANCELLATION DATE LESS THAN TO DATE
      *
     C                   EXSR      GPXH11
      *
      *  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
      *   WITH REINSTATEMENT ISSUE DATE LESS THAN TO DATE
      *
     C                   EXSR      GPXH12
      *
      *  PROCESS CANCELLATION HISTORY FILE TO RETRIEVE RECORDS
      *   WITH REINSTATEMENT DATE LESS THAN TO DATE
      *
     C                   EXSR      GPXH13
      *** BEG ADD ***** 01/20/09 ***********************************(B)
      *
      *  PROCESS ON EADIV IF DIVISION IS NOT SUMMARIZED, OTHERWISE,
      *  PROCESS ON EARDIV
      *
     C     XXARFL        IFEQ      'N'
      *** END ADD ***** 01/20/09 ***********************************(B)
      *
      *  PROCESS ACCTS RECEIVABLE FILE TO RETRIEVE RECORDS
      *   WITH TRANSACTION DATE LESS THAN TO DATE
      *
     C                   EXSR      GPEA16
      *
      *  PROCESS ACCTS RECEIVABLE FILE TO RETRIEVE RECORDS
      *   WITH POSTING DATE LESS THAN TO DATE
      *
     C                   EXSR      GPEA17
      *
      *** BEG ADD ***** 01/20/09 ***********************************(B)
     C                   ELSE
     C                   EXSR      GPEA23
     C                   ENDIF
      *** END ADD ***** 01/20/09 ***********************************(B)
      *
      **** END   ADD **** 11/18/98 ************************************
     C                   ENDIF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
     C*  PROCESS EMPLOYER HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH EFFECTIVE LESS THAN THE TO DATE
     C********************************************************************
     C     GPEH03        BEGSR
     C*
     C*  START EMPLOYER HISTORY FILE
     C*
     C     EHKY          SETLL     PEH03
     C     EHKY          READE     PEH03                                  61
     C*
     C*  PROCESS UNTIL EOF
     C*
     C     *IN61         DOWEQ     *OFF
      **** BEGIN CHG **** 10/27/98 *********************************************
     C********** EHEFDT    ANDLEXXVDAT
      *** BEG ADD ***** 01/30/06 **********************************
     C     EHFAF         IFNE      'X'
      *** END ADD ***** 01/30/06 **********************************
     C     EHFAF         IFNE      'A'
     C     EHEFDT        ANDLE     XXVDAT
     C     EHFAF         OREQ      'A'
     C     EHTRDT        ANDLE     XXVDAT
     C*** BEGIN ADD ***  07/24/00  ************************************
     C     EHREVF        ANDNE     'Y'
     C     EHFAF         OREQ      'A'
     C     EHREVF        ANDEQ     'Y'
     C     EHEFDT        ANDLE     XXVDAT
     C*** END ADD *****  07/24/00  ************************************
      **** END   CHG **** 10/27/98 *********************************************
      **** BEGIN ADD **** 04/21/99 *********************************************
     C                   MOVE      *OFF          *IN63
      *
     C     EHFAF         IFEQ      'A'
     C*** BEGIN ADD ***  07/24/00  ************************************
     C     EHREVF        ANDNE     'Y'
     C*** END ADD *****  07/24/00  ************************************
      *
     C                   MOVE      EHEMP#        XXEMP#
     C                   MOVE      EHDIV         XXDIV
     C                   MOVE      EHFYR         XXFYR
      *
     C     ELKY2         CHAIN     PEL02                              63
      *
     C     *IN63         IFEQ      *OFF
      *
     C     EHTRDT        IFLT      ELPFDT
     C     ELPFDT        ANDGT     XXVDAT
      *
     C                   MOVE      *ON           *IN63
      *
     C                   ENDIF                                                  EHTRDT IFLT ELPFDT
      *
     C                   ENDIF                                                  *IN63 IFEQ *OFF
      *
     C                   ENDIF                                                  EHFAF IFEQ 'A'
      *
     C     *IN63         IFEQ      *OFF
      **** END   ADD **** 04/21/99 *********************************************
     C*
     C     EHEFDT        IFGE      EHTRDT
     C     EHEFDT        ANDNE     *ZERO
     C     EHTRDT        ANDNE     *ZERO
      **** BEGIN CHG **** 10/27/98 *********************************************
     C******************** MOVE EHEFDT    XXPRDT
     C                   MOVE      EHTRDT        XXPRDT
      **** END   CHG **** 10/27/98 *********************************************
      **** BEGIN ADD **** 04/21/99 *********************************************
     C     EHFAF         IFEQ      'A'
     C*** BEGIN ADD ***  07/24/00  ************************************
     C     EHREVF        ANDNE     'Y'
     C*** END ADD *****  07/24/00  ************************************
     C     EHTRDT        ANDLT     ELPFDT
     C                   Z-ADD     ELPFDT        XXPRDT
     C                   ENDIF                                                  EHFAF IFEQ 'A'
      **** END   ADD **** 04/21/99 *********************************************
     C                   MOVE      EHEFDT        XXPODT
     C                   MOVE      EHTRDT        XXTRDT
     C*** BEGIN ADD ***  05/11/00  ************************************
     C                   MOVE      EHREVF        XXREVF
     C*** END ADD *****  05/11/00  ************************************
     C                   MOVE      EHFAF         XXFAF
      **** BEGIN ADD **** 10/19/98 *********************************************
     C     EHFAF2        IFNE      *BLANK
      **** END   ADD **** 10/19/98 *********************************************
      **** BEGIN ADD **** 08/06/98 *********************************************
     C                   MOVE      EHFAF2        XXFAF2
      **** END   ADD **** 08/06/98 *********************************************
      **** BEGIN ADD **** 10/19/98 *********************************************
     C                   ELSE
     C                   MOVE      EHFAF         XXFAF2
     C                   ENDIF                                                  EHFAF2 IFNE *BLANK
      **** END   ADD **** 10/19/98 *********************************************
     C                   MOVE      EHIAPR        XXIAPR
     C                   MOVE      EHWAPR        XXWAPR
      **** BEGIN ADD **** 02/28/01 ************************************
     C                   MOVE      EHPRTD        XXPRTD
     C                   MOVE      EHPROR        XXPROR
      **** END   ADD **** 02/28/01 ************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      EHITER        XXITER
     C                   MOVE      EHWTER        XXWTER
     C                   MOVE      EHXTER        XXXTER
     C                   MOVE      EHJTER        XXJTER
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 01/26/06 ******************************************
     C                   MOVE      EHIDTR        XXIDTR
     C                   MOVE      EHWDTR        XXWDTR
     C                   MOVE      EHXDTR        XXXDTR
     C                   MOVE      EHJDTR        XXJDTR
     C*** END ADD ***** 01/26/06 ******************************************
     C***  Begin Add  ***  07/26/04  **************************************
     C                   MOVE      EHWMPR        XXWMPR
     C                   MOVE      EHIMPR        XXIMPR
     C                   MOVE      EHJMPR        XXJMPR
     C                   MOVE      EHXMPR        XXXMPR
     C***  End   Add  ***  07/26/04  **************************************
     C*** BEG ADD *******  09/01/04  **************************************
     C                   MOVE      EHWVLC        XXWVLC
     C                   MOVE      EHIVLC        XXIVLC
     C                   MOVE      EHJVLC        XXJVLC
     C                   MOVE      EHXVLC        XXXVLC
     C*** END ADD *******  09/01/04  **************************************
     C                   MOVE      EHWNMP        XXWNMP
     C                   MOVE      EHJAPR        XXJAPR
     C                   MOVE      EHXAPR        XXXAPR
     C                   MOVE      EHXNMP        XXXNMP
     C                   MOVE      EHRWDT        XXRWDT
     C     EHPGM         IFEQ      'SRRVEH'
     C                   MOVE      'Y'           XXRWFL
     C                   ENDIF
     C     EHFAF         IFEQ      'A'
     C                   MOVE      '5'           XXSORT
     C                   ELSE
     C                   MOVE      '2'           XXSORT
     C                   ENDIF
     C                   MOVE      EHDT          XXIDT
     C                   MOVE      EHTIME        XXITM
     C*
     C*  WRITE WORKFILE RECORD
     C*
     C                   EXSR      WRITIT
     C                   ENDIF
      **** BEGIN ADD **** 04/21/99 *********************************************
     C                   ENDIF                                                  *IN63 IFEQ *OFF
      **** END   ADD **** 04/21/99 *********************************************
      **** BEGIN ADD **** 10/27/98 *********************************************
     C                   ENDIF                                                  EHFAF IFNE 'A'
      **** END   ADD **** 10/27/98 *********************************************
      *** BEG ADD ***** 01/30/06 **********************************
     C                   ENDIF                                                  EHFAF <> 'X'
      *** END ADD ***** 01/30/06 **********************************
     C*
     C     EHKY          READE     PEH03                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
     C*  PROCESS EMPLOYER HISTORY FILE TO RETRIEVE RECORDS
     C*   WITH TRANSACTION DATE LESS THAN TO DATE
     C********************************************************************
     C     GPEH06        BEGSR
     C*
     C*  START EMPLOYER HISTORY FILE
     C*
     C     EHKY          SETLL     PEH06
     C     EHKY          READE     PEH06                                  61
     C*
     C*  PROCESS UNTIL EOF
     C*
     C     *IN61         DOWEQ     *OFF
     C     EHTRDT        ANDLE     XXVDAT
     C*
      *** BEG ADD ***** 01/30/06 **********************************
     C     EHFAF         IFNE      'X'
      *** END ADD ***** 01/30/06 **********************************
     C     EHTRDT        IFGT      EHEFDT
     C     EHEFDT        ANDNE     *ZERO
     C     EHTRDT        ANDNE     *ZERO
     C                   MOVE      EHTRDT        XXPRDT
     C                   MOVE      EHEFDT        XXPODT
     C                   MOVE      EHTRDT        XXTRDT
     C*** BEGIN ADD ***  05/11/00  ************************************
     C                   MOVE      EHREVF        XXREVF
     C*** END ADD *****  05/11/00  ************************************
     C                   MOVE      EHFAF         XXFAF
      **** BEGIN ADD **** 10/19/98 *********************************************
     C     EHFAF2        IFNE      *BLANK
      **** END   ADD **** 10/19/98 *********************************************
      **** BEGIN ADD **** 08/06/98 *********************************************
     C                   MOVE      EHFAF2        XXFAF2
      **** END   ADD **** 08/06/98 *********************************************
      **** BEGIN ADD **** 10/19/98 *********************************************
     C                   ELSE
     C                   MOVE      EHFAF         XXFAF2
     C                   ENDIF                                                  EHFAF2 IFNE *BLANK
      **** END   ADD **** 10/19/98 *********************************************
     C                   MOVE      EHIAPR        XXIAPR
     C                   MOVE      EHWAPR        XXWAPR
      **** BEGIN ADD **** 02/28/01 ************************************
     C                   MOVE      EHPRTD        XXPRTD
     C                   MOVE      EHPROR        XXPROR
      **** END   ADD **** 02/28/01 ************************************
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      EHITER        XXITER
     C                   MOVE      EHWTER        XXWTER
     C                   MOVE      EHXTER        XXXTER
     C                   MOVE      EHJTER        XXJTER
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 01/26/06 ******************************************
     C                   MOVE      EHIDTR        XXIDTR
     C                   MOVE      EHWDTR        XXWDTR
     C                   MOVE      EHXDTR        XXXDTR
     C                   MOVE      EHJDTR        XXJDTR
     C*** END ADD ***** 01/26/06 ******************************************
     C***  Begin Add  ***  07/26/04  **************************************
     C                   MOVE      EHWMPR        XXWMPR
     C                   MOVE      EHIMPR        XXIMPR
     C                   MOVE      EHJMPR        XXJMPR
     C                   MOVE      EHXMPR        XXXMPR
     C***  End   Add  ***  07/26/04  **************************************
     C*** BEG ADD *******  09/01/04  **************************************
     C                   MOVE      EHWVLC        XXWVLC
     C                   MOVE      EHIVLC        XXIVLC
     C                   MOVE      EHJVLC        XXJVLC
     C                   MOVE      EHXVLC        XXXVLC
     C*** END ADD *******  09/01/04  **************************************
     C                   MOVE      EHWNMP        XXWNMP
     C                   MOVE      EHJAPR        XXJAPR
     C                   MOVE      EHXAPR        XXXAPR
     C                   MOVE      EHXNMP        XXXNMP
     C                   MOVE      EHRWDT        XXRWDT
     C     EHPGM         IFEQ      'SRRVEH'
     C                   MOVE      'Y'           XXRWFL
     C                   ENDIF
     C     EHFAF         IFEQ      'A'
     C                   MOVE      '5'           XXSORT
     C                   ELSE
     C                   MOVE      '2'           XXSORT
     C                   ENDIF
     C                   MOVE      EHDT          XXIDT
     C                   MOVE      EHTIME        XXITM
     C*
     C*  WRITE WORKFILE RECORD
     C*
     C                   EXSR      WRITIT
     C                   ENDIF
     C*
      *** BEG ADD ***** 01/30/06 **********************************
     C                   ENDIF                                                  EHFAF <> 'X'
      *** END ADD ***** 01/30/06 **********************************
     C     EHKY          READE     PEH06                                  61
     C                   ENDDO                                                  *IN61 DOWEQ *OFF
     C*
     C                   ENDSR
      /EJECT
     C********************************************************************
     C*  LOAD & WRITE THE WORKFILE RECORD, CLEAR THE WORK FIELDS
     C********************************************************************
     C     WRITIT        BEGSR
     C*
     C*  CLEAR OUT THE RECORD FORMAT
     C*
     C                   CLEAR                   PEU01
     C*
     C*  LOAD THE WORK DATA INTO THE WORKFILE
     C*
     C                   MOVE      XXCO#         EUCO#
     C                   MOVE      XXFND         EUFND
     C                   MOVE      XXEMP#        EUEMP#
     C                   MOVE      XXDIV         EUDIV
     C                   MOVE      XXFYR         EUFYR
     C                   MOVE      XXPRDT        EUPRDT
     C                   MOVE      XXSORT        EUSORT
     C                   MOVE      XXNAME        EUNAME
     C                   MOVE      XXARCD        EUARCD
     C                   MOVE      XXAMT         EUAMT
     C                   MOVE      XXTRDT        EUTRDT
     C                   MOVE      XXPODT        EUPODT
     C                   MOVE      XXFAF         EUFAF
     C                   MOVE      XXREVF        EUREVF
     C                   MOVE      XXIAPR        EUIAPR
     C                   MOVE      XXWAPR        EUWAPR
     C                   MOVE      XXPRTD        EUPRTD
     C                   MOVE      XXPROR        EUPROR
     C***  Begin Add  ***  12/01/03  **************************************
     C                   MOVE      XXITER        EUITER
     C                   MOVE      XXWTER        EUWTER
     C                   MOVE      XXXTER        EUXTER
     C                   MOVE      XXJTER        EUJTER
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 01/26/06 ******************************************
     C                   MOVE      XXIDTR        EUIDTR
     C                   MOVE      XXWDTR        EUWDTR
     C                   MOVE      XXXDTR        EUXDTR
     C                   MOVE      XXJDTR        EUJDTR
     C*** END ADD ***** 01/26/06 ******************************************
     C***  Begin Add  ***  07/26/04  **************************************
     C                   MOVE      XXWMPR        EUWMPR
     C                   MOVE      XXIMPR        EUIMPR
     C                   MOVE      XXJMPR        EUJMPR
     C                   MOVE      XXXMPR        EUXMPR
     C***  End   Add  ***  07/26/04  **************************************
     C*** BEG ADD *******  09/01/04  **************************************
     C                   MOVE      XXWVLC        EUWVLC
     C                   MOVE      XXIVLC        EUIVLC
     C                   MOVE      XXJVLC        EUJVLC
     C                   MOVE      XXXVLC        EUXVLC
     C*** END ADD *******  09/01/04  **************************************
     C                   MOVE      XXWNMP        EUWNMP
     C                   MOVE      XXJAPR        EUJAPR
     C                   MOVE      XXXAPR        EUXAPR
     C                   MOVE      XXXNMP        EUXNMP
     C                   MOVE      XXRWDT        EURWDT
     C                   MOVE      XXRWFL        EURWFL
     C                   MOVE      XXIDT         EUIDT
     C                   MOVE      XXITM         EUITM
     C                   MOVE      XXFAF2        EUFAF2
     C*
     C*  WRITE THE WORKFILE RECORD
     C*
B1   C     XXSORT        IFNE      '0'
      *
B2   C     QQTYPE        IFEQ      'P'
     C     ELKY2         CHAIN     PEL02                              60
     C*
B3   C     ELPFDT        IFGE      XXFDAT
     C     ELPFDT        ANDLE     XXTDAT
      *****BEGIN ADD 10/22/02 *****************************************
     C     QQSRTS        IFEQ      'Y'
     C                   EXSR      RISKST
C    C                   ELSE
     C                   MOVE      'XX'          EUST
C    C                   ENDIF
      *****END** ADD 10/22/02 *****************************************
      ****BEG ADD ***** 11/12/07 ******************************************
      *** BEG DEL ***** 01/20/09 ***************************************(C)
     C********** XXARFL    IFEQ 'Y'
     C**********           Z-ADDSVDIV     EUDIV
     C**********           MOVE 'N'       XXARFL
     C**********           ENDIF
      *** END DEL ***** 01/20/09 ***************************************(C)
      ****END ADD ***** 11/12/07 ******************************************
     C                   WRITE     PEU01
E3   C                   ENDIF
     C*
X2   C                   ELSE
      *****BEGIN ADD 10/22/02 *****************************************
     C     QQSRTS        IFEQ      'Y'
     C     ELKY2         CHAIN     PEL02                              60
     C*
     C                   EXSR      RISKST
C    C                   ELSE
     C                   MOVE      'XX'          EUST
     C                   ENDIF
      *****END   ADD 10/22/02 *****************************************
      ****BEG ADD ***** 11/12/07 ******************************************
      *** BEG DEL ***** 01/20/09 ***************************************(C)
     C********** XXARFL    IFEQ 'Y'
     C**********           Z-ADDSVDIV     EUDIV
     C**********           MOVE 'N'       XXARFL
     C**********           ENDIF
      *** END DEL ***** 01/20/09 ***************************************(C)
      ****END ADD ***** 11/12/07 ******************************************
     C                   WRITE     PEU01
E2   C                   ENDIF
     C*
E1   C                   ENDIF
     C*
     C                   CLEAR                   XXARCD
     C                   CLEAR                   XXAMT
     C                   CLEAR                   XXTRDT
     C                   CLEAR                   XXPODT
     C                   CLEAR                   XXFAF
     C                   CLEAR                   XXREVF
     C                   CLEAR                   XXIAPR
     C                   CLEAR                   XXWAPR
     C                   CLEAR                   XXPRTD
     C                   CLEAR                   XXPROR
     C***  Begin Add  ***  12/01/03  **************************************
     C                   CLEAR                   XXITER
     C                   CLEAR                   XXWTER
     C                   CLEAR                   XXXTER
     C                   CLEAR                   XXJTER
     C***  End   Add  ***  12/01/03  **************************************
     C*** BEG ADD ***** 01/26/06 ******************************************
     C                   CLEAR                   XXIDTR
     C                   CLEAR                   XXWDTR
     C                   CLEAR                   XXXDTR
     C                   CLEAR                   XXJDTR
     C*** END ADD ***** 01/26/06 ******************************************
     C***  Begin Add  ***  07/26/04  **************************************
     C                   CLEAR                   XXWMPR
     C                   CLEAR                   XXIMPR
     C                   CLEAR                   XXJMPR
     C                   CLEAR                   XXXMPR
     C***  End   Add  ***  07/26/04  **************************************
     C*** BEG ADD *******  09/01/04  **************************************
     C                   CLEAR                   XXWVLC
     C                   CLEAR                   XXIVLC
     C                   CLEAR                   XXJVLC
     C                   CLEAR                   XXXVLC
     C*** END ADD *******  09/01/04  **************************************
     C                   CLEAR                   XXWNMP
     C                   CLEAR                   XXJAPR
     C                   CLEAR                   XXXAPR
     C                   CLEAR                   XXXNMP
     C                   CLEAR                   XXRWDT
     C                   CLEAR                   XXRWFL
     C                   CLEAR                   XXIDT
     C                   CLEAR                   XXITM
     C                   CLEAR                   XXFAF2
     C*
     C                   ENDSR
      /EJECT
      **** BEGIN ADD 10/22/02 *****************************************
      ********************************************************************
      *   GET RISK STATE
      ********************************************************************
     C     RISKST        BEGSR
      *
      *** BEG CHG ***** 01/20/09 **************************************(B)
      *** BEG CHG ***** 11/12/07 *****************************************
     C********** RSKY      CHAINWMRSL                60
     C********** *IN60     IFEQ *ON
     C**********           MOVE 'XX'      EUST
     C**********           ELSE
     C**********           MOVE RSJRST    EUST
     C**********           ENDIF
     C**********           SELEC
     C********** XXARFL    WHNE 'Y'
     C********** RSKY      CHAINWMRSL                60
     C********** *IN60     IFEQ *ON
     C**********           MOVE 'XX'      EUST
     C**********           ELSE
     C**********           MOVE RSJRST    EUST
     C**********           ENDIF
     C********** XXARFL    WHEQ 'Y'
     C**********           Z-ADDSVDIV     XXDIV
     C********** ELKY2     CHAINPEL02                60
     C**N60***** RSKY      CHAINWMRSL                60
     C********** *IN60     IFEQ *ON
     C**********           MOVE 'XX'      EUST
     C**********           ELSE
     C**********           MOVE RSJRST    EUST
     C**********           ENDIF
     C**********           Z-ADDEUDIV     XXDIV
     C********** ELKY2     CHAINPEL02                60
     C**********           ENDSL
      *** END CHG ***** 11/12/07 *****************************************
     C     RSKY          CHAIN     WMRSL                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      'XX'          EUST
     C                   ELSE
     C                   MOVE      RSJRST        EUST
     C                   ENDIF
      *** END CHG ***** 01/20/09 **************************************(B)
      *
     C                   ENDSR
      **** END   ADD 10/22/02 *****************************************

      //===============================================================
      // GPEA23 - PROCESSES ON EARDIV, IT WILL GATHER ALL RECORDS FOR
      //          A POLICY IF NOT WRITTEN INTO THE WORKFILE ALREADY.
      //===============================================================
      /FREE
       Begsr GPEA23;

          x1Fyr = 0;
          Setll EAKY2 PEA23;
          Reade EAKY2 PEA23;

          Dow Not %EOF;

             If EAtrdt <= xxVdat And EApodt <= xxVdat;
                xxDiv = EArdiv;
                Exsr EURIT2;
                If EAfyr <> x1Fyr;
                   x1Fyr = EAfyr;
                   Chain F1KY PF101;
                Endif;

                TTctyp = *Blanks;
                If EActyp = *blanks And EAarcd <> F1srbb;
                   Chain (xxCo#: EAarcd) PTT01;
                Else;
                    TTctyp = EActyp;
                Endif;

                If (TTctyp = 'C' And EAarty = 'B')
                 Or EAarcd = F1srbb;

                   xxEmp# = EAemp#;
                   xxDiv = EArdiv;
                   xxFyr = EAfyr;
                   xxPrdt = EAtrdt;
                   xxArcd = EAarcd;
                   xxAmt = EAamt;
                   xxTrdt = EAtrdt;
                   xxPodt = EApodt;
                   xxSort = '1';
                   xxIdt = EAdt;
                   xxItm = EAtime;

                   Select;
                      When CLtid = '136'
                       And EAtrdt <= 1051231;
                      Other;
                         Exsr WRITIT;
                   Endsl;

                Endif;

             Endif;

             Reade EAKY2 PEA23;

          Enddo;

       Endsr;

      /END-FREE
      *===============================================================
      * EURIT2 - CHECKS POLICY AND GETS ALL TRANSACTIONS IF NOT ALREADY
      *          WRITTEN INTO THE WORKFILE
      *===============================================================
     C     EURIT2        BEGSR
     C     EUKY          SETLL     PEU01
     C     EUKY          READE     PEU01                                  69
     C     *IN69         DOWEQ     *OFF
     C     EUSORT        IFNE      '0'                                                      (B)
     C                   LEAVE
     C                   ENDIF
     C     EUKY          READE     PEU01                                  69
     C                   ENDDO
     C     *IN69         IFEQ      *ON
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     EMNAME        XXNAME
     C                   ELSE
     C                   MOVE      *BLANK        XXNAME
     C                   ENDIF                                                  *IN60 IFEQ
     C                   EXSR      GPEH03
     C                   EXSR      GPEH06
     C                   EXSR      GPXH10
     C                   EXSR      GPXH11
     C                   EXSR      GPXH12
     C                   EXSR      GPXH13
     C                   ENDIF
     C                   ENDSR
      *===============================================================
      *** END ADD ***** 01/20/09 **************************************(B)
      *** BEG ADD ***** 12/02/09 **********************************
      *============================================================
      * GETFYR - SET UP FOR FISCAL YEAR DATES
      *          IF FISCAL YEAR START MONTH IS PROVIDED, FIND
      *          FIRST DAY OF CURRENT FISCAL YEAR AND PRIOR
      *          FISCAL YEAR END DATE
      *============================================================
     C     GETFYR        BEGSR
     C     XXCO#         CHAIN     SMCOL                              60
     C                   Z-ADD     COFYSM        XXFYSM            2 0
     C     FNKY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *OFF
     C     F2FYSM        ANDNE     *ZERO
     C                   Z-ADD     F2FYSM        XXFYSM
     C                   ENDIF
     C     XXFYSM        IFNE      *ZERO
     C                   MOVEL     XXFYSM        XAFYSM            4
     C                   MOVE      '01'          XAFYSM
     C                   MOVEL     XAFYSM        XXFRDT
     C                   MOVE      QQVLDT        XXVLDT
     C     XXFYSM        IFGT      XXVMTH
     C                   SUB       1             XXFRDT
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR
      *** END ADD ***** 12/02/09 **********************************
