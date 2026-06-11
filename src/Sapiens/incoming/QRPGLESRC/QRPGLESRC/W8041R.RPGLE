     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8041R - BULK RERATING PROGRAM
     H*****************************************************************
     H**                                                              *
     H** PROGRAM DESCRIPTION:                                         *
     H**    THIS PROGRAM WILL RERATE A GROUP OF POLICIES BASED ON     *
     H**    THE RATE SET CODE AND AN EFFECTIVE DATE.                  *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**    W8041C                                                    *
     H**                                                              *
     H** CALLS TO:                                                    *
     H** W0090R                                                       *
     H**                                                              *
     H** PROGRAMMER: C:65                                             *
     H** CREATED:    07/16/99         PROJECT: WC850                  *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 06/22/18 C:65  Project: 150WAL                                *
     H*                Change rounding on territory factor to round   *
     H*                after applying both LCM and territory.         *
     H*                                                               *
     H* 07/26/17 C:62  Project: 150WAL                                *
     H*                Add Territorial Differential Factor Process.   *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 07/02/13 C:136 Project: 147WAI                                *
     H*                Add processing for Dividend Plan               *
     H*                                                               *
     H* 02/08/13 C:190 Project: 140WAU                                *
     H*                Process deposit adjustments for all plan types *
     H*                (i.e. include estimating plans also)           *
     H*                                                               *
     H* 11/16/11 C:65  Project: 139WPL                                *
     H*                CHANGE OVERRIDE COMMAND FOR HCN OVERRIDE.      *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Add overrides for HCN Program.                 *
     H*                                                               *
     H* 07/15/10 C:180 SUPPORT 73558 - POPULATE RATING STATE.
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 07/23/09 C:65  PROJECT: 139WJU                                *
     H*                DISABLE PROJECT BY CHANGING CLTID FOR 139 TO   *
     H*                999 AFTER CLTID IS SET.                        *
     H*                                                               *
     H* 02/12/09 C:65  PROJECT: 132WBW/SUPPORT                        *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES FOR NEW SECTIONS OF CODE.     *
     H*                                                               *
     H* 02/10/09 C:65  PROJECT: SUPPORT/WC1568/WC1769                 *
     H*                VARIOUS CORRECTIONS TO ASSESSMENT BY           *
     H*                INSTALLMENTS AND 6TH/7TH ASSESSMENTS.          *
     H*                                                               *
     H* 01/23/09 C:136 PROJECT: 139WJU                                *
     H*                BILL ASSESSMENTS BY INSTALLMENT FOR S/R        *
     H*                POLICIES FOR GIC.  THE ASSESSMENT WILL BE      *
     H*                BILLED IN EQUAL AMOUNTS FOR EACH MONTH THAT    *
     H*                GETS REPORTED ON THE POLICY, BILLED TO THE     *
     H*                LAST DAY OF THE MONTH.                         *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 11/07/08  C:65  PROJECT: SUPP# 65953                          *
     H*                 IGNORE PAYROLL CLASSES THAT ARE A-RATABLE     *
     H*                 ON AN A-RATED POLICY WHEN DETERMINING IF      *
     H*                 A POLICY HAS A RATE CHANGE NEEDED.            *
     H*                                                               *
     H* 03/10/08 C:136 PROJECT: WC1568 PRISS 13                       *
     H*                PRIOR INSTALLMENT A/R RECORDS NOT BEING        *
     H*                ACCUMULATED.                                   *
     H*                                                               *
     H* 01/11/08 C:65  PROJECT: WC1568                                *
     H*  COPY BY C:136 REWIRE THE ASSESSMENT BILLING TO PROCESS BOTH  *
     H*                ASSESSMENTS BY INSTALLMENT AND ASSESSMENTS IN  *
     H*                LUMP SUM AND POST ALL ADJUSTMENTS TO THE       *
     H*                ADJUSTMENT SCREENS.  LUMP SUM ASSESSMENTS WILL *
     H*                ALWAYS BE POSTED TO THE FIRST (CATCH-UP) SLOT. *
     H*                ALL CODING TO ADJUST ASSESSMENTS WILL THEN     *
     H*                BE REMOVED FROM W8011R BACK END PROCESING.     *
     H*                                                               *
     H*                ASSESSMENT FIELD DEFINITIONS:                  *
     H*                                                               *
     H*                XX1STB - 1ST ASSESSMENT AMOUNT TO BILL INTO    *
     H*                         INSTALLMENTS.                         *
     H*                XU1STB - 1ST ASSESSMENT AMOUNT TO BILL IN      *
     H*                         LUMP SUM.                             *
     H*                XT1STB - 1ST ASSESSMENT TOTAL FOR INSTALLMENTS.*
     H*                                                               *
     H*                XL1STB - 1ST ASSESSMENT TOTAL FOR LUMP SUM.    *
     H*                                                               *
     H*                (REPEATED FOR EACH ASSESSMENT 2ND THRU 5TH)    *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 10/11/07 C:183 PROJECT: 146WBC                                *
     H*                Added code to invoke the same processing that  *
     H*                occurs on a billing schedule code change if    *
     H*                the user changes the "Monthly Billings on      *
     H*                Policy Effective Day" from BLANK to 'Y' or 'Y' *
     H*                to blank.                                      *
     H*                                                               *
     H* 05/15/07 C:184 PROJECT WC1568                                 *
     H*                Charge assessments on the same basis as premium*
     H*                installments                                   *
     H*                                                               *
     H* 01/11/07 C:183 PROJECT: 139WGA                                *
     H*                IGNORE CATEGORY 'C' RECORDS WHERE A/R TYPE     *
     H*                IS 'B' OR 'A'. THIS APPLIES FOR REGULAR BI     *
     H*                LLING OR EXPENSE CONSTANT BILLINGS.            *
     H*                                                               *
     H* 01/08/07  C:65  PROJECT: SUPP# 56522                          *
     H*                 REMOVE EXPENSE CONSTANT FROM ANNUAL LEVEL     *
     H*                 DEPOSIT CALC, SINCE ITS BILLED UP FRONT.      *
     H*                                                               *
     H* 02/28/06 C:65  PROJECT: WC1327                                *
     H*                ADD PARM FOR SINGLE DIVISION CALL TO W8011R.   *
     H*                                                               *
     H* 02/20/06 C:65  PROJECT: 132WDP                                *
     H*                CHECK THE FLAG AT THE ADMIN/DARTS LEVEL FOR    *
     H*                ROUNDING INSTALLMENTS (PREMIUM AND TAX) TO     *
     H*                THE WHOLE DOLLAR.                              *
     H*                                                               *
     H* 08/10/05 C:136 PROJECT: WC1136                                *
     H*                ADD LOSS COST MULTIPLIER TO CALCULATION.       *
     H*                                                               *
     H* 04/07/05 C:65  PROJECT: WC1273                                *
     H*                ALLOW DEPOSIT TO BE APPLIED TO AN ANNUAL LEVEL.*
     H*                                                               *
     H* 11/30/04 C:65  PROJECT: 132WBW                                *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES.                              *
     H*                                                               *
     H* 04/10/00  C:118 PROJECT: WC987                                *
     H*                 LOAD ALTERNATE COMPANY NAME FROM FUND MASTER  *
     H*                                                               *
     H* 11/15/99  C:65  PROJECT: WC936                                *
     H*                 ADD CODE TO SUPPORT A/R SUMMARY AND ALSO      *
     H*                 PROCESS ALL DIVISIONS AT THE SAME TIME.       *
     H*                                                               *
     H*  12/13/99  C:65  PROJECT:  WC850                              *
     H*                  SET UP CODE TO ALLOW RESUBMITION TO AVOID    *
     H*                  THE MAX SPOOL FILES ERROR.                   *
     H*                  CHANGE THE PROCESSING TO CHECK PAYROLL       *
     H*                  RECORDS FOR CHANGED RATES INSTEAD OF THE     *
     H*                  PENDING RATE CHANGE ENDORSEMENT.             *
     H*                                                               *
     H* 12/09/99  C:65  PROJECT: WC850                                *
     H*                 PRODUCE THE ENDORSEMENT FOR THE STATE RPT     *
     H*                 DIVISON ONLY.                                 *
     H*                                                               *
     H* 11/24/99  C:65  PROJECT: WC850                                *
     H*                 FIND EMPLOYERS WHO ARE EFFECTIVE ON OR AFTER  *
     H*                 THE RATE CHANGE BY EITHER POLICY FROM DATE    *
     H*                 OR ANNIV RATING DATE IN EFFECT.  ALSO EMPLR   *
     H*                 MUST HAVE THE PENDING RATE CHANGE ENDORSEMNT. *
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H* 60 - GENERAL CHAIN                                            *
     H*                                                               *
     H*****************************************************************
     FWDF1L     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWAELL     IF A E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWMEML     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     FWMEXL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWXICP     O    E             DISK    USROPN
     FWMRSL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FWMEPL     IF   E           K DISK    USROPN
     FWAEPL     IF A E           K DISK
     F                                     RENAME(PEP01:PEP02)
     FWMEAL4    UF   E           K DISK
     FWMEAL3    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
     FWAEAL     O  A E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWMMUL     UF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWMEPL1    IF   E           K DISK    USROPN
     F                                     RENAME(PEP01:PEP03)
     FQSYSPRT4  O    F  132        PRINTER OFLIND(*INOA)
     F                                     USROPN
     F                                     INFDS(PINFDS)
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOF)
      *****************************************************************

      *** Begin Add ***** 07/26/17 ***********************************************
     d* Get the Territorial Differential Factor
     d GetTDF          pr                  extpgm('WA101R')
     d  inCompany#                    3  0
     d  inGroup#                      3  0
     d  inEmployer#                   9  0
     d  inDivision                    5  0
     d  inYear                        3  0
     d  outTDF                        5  4
     d  outTDF         s              5  4
      *** End   Add ***** 07/26/17 ***********************************************
      *** beg add ***** 07/02/13 **************************************************
     d/copy wprDivPlan
      *** end add ***** 07/02/13 **************************************************

     D AB1             S              9  8 DIM(12)
     D AB2             S              9  8 DIM(12)
     D QCA             S             55    DIM(30) CTDATA PERRCD(1)                           EC
     D BD              S              7  0 DIM(13)
     D DA              S              8  0 DIM(13)
     D PA              S              9  2 DIM(13)
     D DT              S              8  0 DIM(10)
     D PDAT            S              7  0 DIM(4) ASCEND                        PAYROLL DATES
     D RDAT            S              7  0 DIM(4) ASCEND                        RATING DATES
     D WKD             S              7  0 DIM(53) ASCEND
     D WKA             S              9  2 DIM(53)
     D WKT             S              9  2 DIM(53)
     D DAY             S              2    DIM(13) CTDATA PERRCD(13)              DAYS OF WEEK
     D*
     D A1              S              9  2 DIM(13)
     D A2              S              9  2 DIM(13)
     D A3              S              9  2 DIM(13)
     D A4              S              9  2 DIM(13)
     D A5              S              9  2 DIM(13)
     D A6              S              9  2 DIM(13)
     D A7              S              9  2 DIM(13)
     D*
     D WK1             S              9  2 DIM(53)
     D WK2             S              9  2 DIM(53)
     D WK3             S              9  2 DIM(53)
     D WK4             S              9  2 DIM(53)
     D WK5             S              9  2 DIM(53)
     D WK6             S              9  2 DIM(53)
     D WK7             S              9  2 DIM(53)
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
     c*** Beg   Add ***  02/08/13  ************************************
     d xxAB01          s              9  6
     d xxAB02          s              9  6
     c*** End   Add ***  02/08/13  ************************************
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
      *****************************************************************
     D*
     D PINFDS        E DS                  EXTNAME(SZQ4P)
     D*
     D XXFORM          C                   CONST('WC000404  ')
     D XXFRM           C                   CONST('WC000407  ')
     D*
     D                 DS
     D  YYMM                   1      5  0
     D  YY                     1      3  0
     D  MM                     4      5  0
     D                 DS
     D  X2PFDT                 1      7  0
     D  YY1                    1      3  0
     D  MM1                    4      5  0
     D  DD1                    6      7  0
     D  YYMM1                  1      5  0
     D                 DS
     D  P1                     1    117  2
     D                                     DIM(13)
     D  SCP101                 1      9  2
     D  SCP102                10     18  2
     D  SCP103                19     27  2
     D  SCP104                28     36  2
     D  SCP105                37     45  2
     D  SCP106                46     54  2
     D  SCP107                55     63  2
     D  SCP108                64     72  2
     D  SCP109                73     81  2
     D  SCP110                82     90  2
     D  SCP111                91     99  2
     D  SCP112               100    108  2
     D  SCP113               109    117  2
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
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  CX                     1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
      *
     D                 DS
     D  EF                     1    035  0
     D                                     DIM(5)
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  XXWKDT                 1      7  0
     D  WYY                    1      3  0
     D  WMM                    4      5  0
     D  WDD                    6      7  0
     D  WYYMM                  1      5  0
     D*
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C*****************************************************************
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND DETAIL FILE
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C     RSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C*
     C*  STAT RPT/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST              2
     C                   KFLD                    XXEFDT
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      *
      *  EMPLOYER MASTER FILE
      *
     C     EMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
      *
      *  EMPLOYER DETAIL FILE
      *
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPFDT
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER A/R TRANSACTION
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C                   KFLD                    XXPODT
     C*
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C*
     C     EAKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXARTY            1
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKY          KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE OVERRIDE FILE
      *
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TABLE FILE MAINTENANCE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  ENDORSEMENT FILE
     C*
     C     MUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XKFORM
     C*
     C     MUKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    XXCLAS            5 0
     C                   KFLD                    XXDT              7 0
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    XXCLAS            5 0
     C*
     C*   FIELD DEFINITIONS
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        SVEMP#
     C     *LIKE         DEFINE    ELFYR         SVFYR
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    EXREV         XXRMCD
     C     *LIKE         DEFINE    ELBSCD        XXBSCD
     C     *LIKE         DEFINE    EADT          XXCURD
     C     *LIKE         DEFINE    EADT          XXCUR2
     C     *LIKE         DEFINE    EADT          XXDTE
     C     *LIKE         DEFINE    EADT          XXDTE1
     C     *LIKE         DEFINE    EADT          XXDTE2
     C     *LIKE         DEFINE    EADT          XXEFD1
     C     *LIKE         DEFINE    EADT          XXPODT
     C     *LIKE         DEFINE    MUFORM        XKFORM
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRTCD            3
     C                   PARM                    QQPCT             5
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQBCM1            2
     C                   PARM                    QQBCM2            2
     C                   PARM                    QQBCM3            2
     C                   PARM                    QQICM1            2
     C                   PARM                    QQICM2            2
     C                   PARM                    QQICM3            2
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *   PLIST FOR CALLS TO SRDDIF8
      *
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
      *  INITIALIZE FIELDS BASED ON *ENTRY PARMS
      *
     C                   MOVE      *BLANK        XXFLAG            1
     C                   MOVE      *BLANK        XXMULT            1
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQRTCD        XXRTCD
     C                   MOVE      QQEFDT        X1PFDT            8 0
     C                   MOVE      QQPCT         XXPCT             5 2
     C                   MOVE      '0'           XXOPN             1
     C                   Z-ADD     0             SVEMP#
     C                   MOVE      *BLANK        MAXFLG            1
     C                   Z-ADD     9000          MAXSPL            4 0
     C                   Z-ADD     0             XXSPL#
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '139'
     C                   MOVE      '999'         CLTID
     C                   ENDIF
     C                   Z-ADD     X1PFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPFDT
     C*
     C     X1PFDT        SUB       1             SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXSTDT            7 0
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   EXSR      SR1ST
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C                   MOVE      *BLANK        XXSKIP            1
     C     EMEMP#        IFNE      SVEMP#
     C     ELKY1         SETGT     PEL01
     C     ELKY1         READPE    PEL01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELFYR         XXFYR
     C                   ELSE
     C                   MOVE      'Y'           XXSKIP
     C                   ENDIF
     C                   ENDIF
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXSKIP        ANDNE     'Y'
     C*
     C                   MOVE      *OFF          *IN70
     C     EMEMP#        IFEQ      SVEMP#
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      *ON           *IN70
     C                   ELSE
       //*** Begin Add *** 07/26/17 ************************************
      /free
       // Get the Territorial Differential Factor
       GetTDF(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:outTDF);
      /end-free
       //*** End   Add *** 07/26/17 ************************************
     C*
     C                   OPEN      WMEPL1
     C*
     C     ELKY2         CHAIN     PEP03                              60
     C                   MOVE      *BLANK        XX1ST             1
     C*
     C     *IN60         DOWEQ     *OFF
     C     *IN70         ANDEQ     *OFF
     C*
     C*
     C     EPDIV         IFNE      ELDIV
     C     XX1ST         OREQ      *BLANK
     C*
     C                   MOVE      'N'           XX1ST
     C     ELKY3         CHAIN     PEL01                              50
     C     RSKY1         CHAIN     PRS01                              50
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFDT            7 0
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  50
     C                   Z-ADD     0             PDAT
     C                   Z-ADD     0             RDAT
     C                   MOVE      '0'           *IN77
     C                   MOVEA     '11'          *IN(78)
     C                   Z-ADD     ELPFDT        PDAT(1)
     C                   Z-ADD     ELPFDT        RDAT(1)
     C                   Z-ADD     0             XXANDT
     C                   Z-ADD     2             X
     C                   Z-ADD     ELPFDT        XXEFDT
     C*
     C     ELANDT        IFNE      0
     C     ELANDT        ANDNE     ELPFDT
     C*
     C                   MOVE      ELANDT        XXANDT
     C                   ADD       1             XXANYR
     C*
     C     XXANDT        IFGT      ELPFDT
     C                   MOVE      ELANDT        RDAT(1)
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     C*
     C     XXANDT        IFGE      ELPTDT
     C     XXANDT        ORLE      ELPFDT
     C                   MOVE      *ON           *IN78
     C                   ELSE
     C                   MOVE      *OFF          *IN78
     C                   ENDIF
     C*
     C     *IN78         IFEQ      '0'
     C     ELEMD1        ANDEQ     ELANDT
     C     ELEMD2        ANDEQ     XXANDT
     C     ELEXM1        ANDNE     ELEXM2
     C                   MOVE      '0'           *IN79
     C                   ENDIF                                                  *IN78 IFEQ
     C*
     C     *IN78         IFEQ      '0'
     C                   Z-ADD     XXANDT        PDAT(X)
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ADD       1             X
     C                   ENDIF                                                    *IN78='0'
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     ELPTDT        PDAT(X)
     C                   ENDIF
     C*
     C     ELRTCD        IFEQ      XXRTCD
     C*
     C                   Z-ADD     EPCLAS        XXCLAS
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    PDAT(X)                            60
     C     *IN60         IFEQ      '1'
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   Z-ADD     RDAT(X)       XXDT
     C                   ELSE
     C                   Z-ADD     1             X
     C                   Z-ADD     ELPFDT        XXDT
     C                   END
     C                   Z-ADD     0             XXDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   XXCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DATE
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DATE        XXDT
     C*
     C     PRKEY         SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE(P)   ELCO#         Q2CO#
     C                   MOVE(P)   ELFND         Q2FND
     C                   MOVE(P)   PRRTCD        Q2RTCD
     C                   MOVE(P)   PRCLAS        Q2CLAS
     C                   MOVE(P)   XXDT          Q2EFDT
     C                   CALL      'SRLCM'
     C                   PARM                    Q2CO#             3
     C                   PARM                    Q2FND             3
     C                   PARM                    Q2RTCD            3
     C                   PARM                    Q2CLAS            5
     C                   PARM                    Q2EFDT            7
     C                   PARM      *BLANKS       QQLCM             5
      *
     C     QQLCM         IFNE      *BLANKS
     C                   MOVE      QQLCM         XXLCM             5 4
     C                   ENDIF
      *
     C*** begin chg ***  06/22/18  *************************************
     C*****PRRT          MULT(H)   XXLCM         PRRT
       //*** Begin Add *** 07/26/17 ************************************
      /free
       // Apply Territorial Differential Factor
       //***If outTDF > 0;
       //***   PRrt = %DecH(PRrt * outTDF:7:2);
       //***EndIf;
      /end-free
       //*** End   Add *** 07/26/17 ************************************
     C                   If        outTDF = 0
     C                   Eval(h)   prrt = xxlcm * prrt
     C                   Else
     C                   Eval(h)   prrt = outTDF * xxlcm * prrt
     C                   Endif
     C*** end chg *****  06/22/18  *************************************
     C                   ENDIF
      *
     C     *IN60         IFEQ      *OFF
     C     PRRT          ANDNE     EPARAT
     C     EPCLAS        ANDNE     RRDCCD
     C     EPSTAT        ANDEQ     'A'
     C     ELSTAT        IFNE      'Y'
     C     PRARFL        ORNE      'Y'
     C                   MOVE      *ON           *IN70
     C                   ENDIF                                                  ELSTAT IFNE
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELRTCD IFEQ
     C*
     C  N70ELKY2         READE     PEP03                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   CLOSE     WMEPL1
     C*
     C   70ELKEY         CHAIN     PEL01                              60
     C*
     C                   ENDIF                                                  EMEMP# IFEQ
     C*
     C     *IN60         IFEQ      *OFF
     C     ELPTDT        ANDGT     XXPFDT
     C     *IN70         ANDEQ     *ON
     C*
     C                   Z-ADD     ELFYR         XXFYR
     C*
     C     F1KY          CHAIN     PF101                              60
     C*
      *  MAKE SURE EMPLOYER IS ACTIVE
      *
     C                   MOVE      *OFF          *IN42
     C                   Z-ADD     5             X                 3 0
     C                   DO        5
     C     EF(X)         IFNE      *ZEROS
     C     CX(X)         IFNE      *ZEROS
     C                   MOVE      *ON           *IN42
     C                   LEAVE
     C                   ENDIF                                                  CX,X IFNE
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF                                                  EF,X IFNE
     C                   ENDDO                                                  DO 5
     C*
     C     *IN42         IFEQ      *OFF
     C*
     C*
     C*
     C     MAXFLG        IFEQ      'Y'
     C     XXMULT        ANDNE     'Y'
     C                   MOVE      *ON           *IN69
     C                   ENDIF                                                  MAXFLG IFEQ
     C*
     C     XXSPL#        IFGE      MAXSPL
     C                   MOVE      'Y'           MAXFLG
     C                   ENDIF                                                  Q4SPL# IFEQ
     C*
     C     *IN69         IFEQ      *OFF
     C*
     C                   Z-ADD     XXEMP#        SVEMP#
     C                   Z-ADD     XXFYR         SVFYR
     C                   EXSR      SPRD
     C*
     C                   MOVE      *BLANK        XXSUM             1
     C                   MOVE      *BLANK        XXWAR             1
     C                   Z-ADD     0             XXDIVC            5 0
     C                   Z-ADD     0             XTANPR           11 2
     C                   Z-ADD     0             XT1STB           11 2
     C                   Z-ADD     0             XT2NDB           11 2
     C                   Z-ADD     0             XT3RDB           11 2
     C                   Z-ADD     0             XT4THB           11 2
     C                   Z-ADD     0             XT5THB           11 2
     C                   Z-ADD     0             XT6THB           11 2
     C                   Z-ADD     0             XT7THB           11 2
     C                   Z-ADD     0             XL1STB           11 2
     C                   Z-ADD     0             XL2NDB           11 2
     C                   Z-ADD     0             XL3RDB           11 2
     C                   Z-ADD     0             XL4THB           11 2
     C                   Z-ADD     0             XL5THB           11 2
     C                   Z-ADD     0             XL6THB           11 2
     C                   Z-ADD     0             XL7THB           11 2
     C                   Z-ADD     0             SL1STB           11 2
     C                   Z-ADD     0             SL2NDB           11 2
     C                   Z-ADD     0             SL3RDB           11 2
     C                   Z-ADD     0             SL4THB           11 2
     C                   Z-ADD     0             SL5THB           11 2
     C                   Z-ADD     0             SL6THB           11 2
     C                   Z-ADD     0             SL7THB           11 2
     C                   Z-ADD     0             XTTXDU           11 2
     C*
     C                   EXSR      LDOVRF
     C                   EXSR      BILL
     C                   EXSR      DLTOVR
     C*
     C     ELKY2         CHAIN     PEL03                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     *IN60         OREQ      *ON
     C     XXSUM         ANDEQ     'Y'
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     ELDIV         XXDIV
     C                   ADD       1             XXDIVC
     C     ELARSM        IFEQ      'Y'
     C                   MOVE      'Y'           XXSUM
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     XSDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      'Y'           XXWAR
     C                   ENDIF
     C*
     C     ELKY1         CHAIN     PEM01                              60
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV             5 0
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN42
     C                   Z-ADD     5             X                 3 0
     C                   DO        5
     C     EF(X)         IFNE      *ZEROS
     C     CX(X)         IFNE      *ZEROS
     C                   MOVE      *ON           *IN42
     C                   LEAVE
     C                   ENDIF                                                  CX,X IFNE
     C                   ELSE
     C                   SUB       1             X
     C                   ENDIF                                                  EF,X IFNE
     C                   ENDDO                                                  DO 5
     C*
     C     *IN42         IFEQ      *OFF
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             XXACDT            8 0
     C*
     C                   Z-ADD     XXCO#         @@CO#
     C                   Z-ADD     XXFND         @@FND
     C                   Z-ADD     XXEMP#        @@EMP#
     C                   Z-ADD     XXDIV         @@DIV
     C                   Z-ADD     XXFYR         @@FYR
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         @@DATE
     C                   MOVEL     '*'           @@PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@AGT#            5 0
     C                   PARM                    @@ANAM           60
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM                    @@PLAN            1
     C                   PARM                    @@DATE            8 0
     C*
     C     @@PLAN        IFEQ      'A'
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     EXAOID        XXDAYS            5 0
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      XXDAYS        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          XXACDT            8 0
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  @@PLAN IFEQ
     C*
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFDT            7 0
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*  SUBTRACT EXPENSE CONSTANT FROM ANNUAL PREM & SAVE IT
     C*
     C     ELANPR        SUB       ELPRTD        SVANPR           11 2
     C                   Z-ADD     ELSTPR        SVSTPR           11 2
     C                   Z-ADD     ELMAPR        SVMAPR           11 2
     C                   Z-ADD     ELENMP        SVNMPR           11 2
     C     ELANPR        SUB       ELPRTD        S2ANPR           11 2
     C*
     C                   EXSR      EXEND
     C*
     C*
     C                   EXSR      CMD7
     C*
     C     ELARSM        IFNE      'Y'
     C     XXWAR         OREQ      'Y'
     C                   EXSR      UPDTE
     C                   ENDIF
     C*
     C     XXWAR         IFNE      'Y'
     C                   EXSR      PRINTD
     C                   ENDIF
     C*
     C*
     C     EMSTRP        IFEQ      'Y'
     C     XXWAR         ANDNE     'Y'
     C                   EXSR      ENDOR
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C                   OPEN      QSYSPRT4
     C                   Z-ADD     Q4SPL#        XXSPL#            4 0
     C     *ON           IFEQ      *OFF
     C                   EXCEPT    OUT
     C                   ENDIF
     C                   CLOSE     QSYSPRT4
     C*
     C                   ENDIF                                                  *IN42=*OFF
     C*
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      *BLANK        XXSUM
     C                   MOVE      *BLANK        XXWAR
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C     ELKY2         READE     PEL03                                  60
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   EXSR      CALLU
     C*
     C                   EXSR      PRINT
     C*
     C     XXDIVC        IFGT      1
     C                   EXSR      SUMM
     C                   MOVE      *BLANK        XXMULT
     C                   ENDIF
     C*
     C     EMKY1         SETGT     PEM01
     C                   ENDIF                                                  *IN69=*OFF
     C*
     C                   ENDIF                                                  *IN42=*OFF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     *IN69         IFEQ      *OFF
     C     EMKY          READE     PEM01                                  60
     C                   ELSE
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  *IN69 IFEQ
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN69         IFEQ      *ON
     C                   EXSR      RESBM
     C                   ENDIF
     C*
     C     XXFLAG        IFEQ      *BLANK
     C                   EXCEPT    NOREC
     C                   ENDIF
     C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       Except Route;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   MOVE      *ON           *INLR
      *
      *****************************************************************
      *
      *  SUBROUTINE TO PRINT THE BILL AND LIST EMPLOYER ON REPORT.
      *
     C     PRINT         BEGSR
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXEMP#        QQEMP#
     C                   MOVE      XXDIV         QQDIV
     C*
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         WKDATE            8 0
     C*
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* REVISED INTERIM DAYS PLUS THE PRESENT DATE
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          WKDATE
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        WKDATE
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     DA(1)         IFNE      *ZERO
     C                   MOVE      DA(1)         QQDATE
     C                   ELSE
     C                   MOVE      WKDATE        QQDATE            8
     C                   ENDIF                                                  DA,1 IFNE
     C                   MOVE      SCDT          QQDUE             8
     C                   MOVE      ' '           QQPRT             1
     C                   MOVE      ' '           QQRNB             1
     C                   MOVE      XXDART        QQDART            1
     C*
     C                   CALL      'W7615R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQDATE
     C                   PARM                    QQBCM1
     C                   PARM                    QQBCM2
     C                   PARM                    QQBCM3
     C                   PARM                    QQDUE
     C                   PARM      'Y'           QQDEP             1
     C                   PARM      'W'           QQQTE
     C                   PARM                    QQRNB
     C                   PARM                    QQDART
     C                   PARM      '0'           QQSTEP            1
     C                   PARM                    QQOUTQ
     C                   PARM                    QQPRT
     C                   PARM      'Y'           QQHARD            1
     C                   PARM      'N'           QQAGTO            1
     C                   PARM      ' '           QQFQT             1
     C                   PARM                    XXRMCD
     C                   PARM                    QQFCM1           30
     C                   PARM                    QQFCM2           30
     C                   PARM                    QQFCM3           30
     C*
     C                   ENDSR                                                  PRINT SR
     C*
     C     PRINTD        BEGSR
     C     *INOF         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   MOVE      *OFF          *INOF
     C                   ENDIF
     C*
     C                   EXCEPT    DET
     C*
     C                   MOVE      'Y'           XXFLAG
     C*
     C                   ENDSR                                                  PRINT SR
      *
      *****************************************************************
      *
      *  SUBROUTINE TO PRINT AN INSTALLMENT SCHEDULE FOR EMPLOYER.
      *
     C     SUMM          BEGSR
     C*
     C*
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         WKDATE            8 0
     C*
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* REVISED INTERIM DAYS PLUS THE PRESENT DATE
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          WKDATE
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        WKDATE
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     DA(1)         IFNE      *ZERO
     C                   MOVE      DA(1)         QQDATE
     C                   ELSE
     C                   MOVE      WKDATE        QQDATE            8
     C                   ENDIF                                                  DA,1 IFNE
     C*
     C                   CALL      'W7329R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDATE            8
     C                   PARM                    QQICM1
     C                   PARM                    QQICM2
     C                   PARM                    QQICM3
     C                   PARM                    QQOUTQ           10
     C                   PARM      'D'           QQMODE            1
     C                   PARM      ' '           QQCALL            1
     C*
     C                   ENDSR                                                  SUMM  SR
      *
      *****************************************************************
      *
      *  SUBROUTINE TO MARK PENDING RATE CHANGE ENDORSEMENT AS EXCLUDED
      *
     C     EXEND         BEGSR
     C*
     C     MUKY          CHAIN     PMU01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'E'           MUEXFL
     C                   UPDATE    PMU01
     C                   ENDIF
     C*
     C                   ENDSR                                                  EXEND SR
      *
      *****************************************************************
      *
      *  SUBROUNTINE TO GENERATE A RATE CHANGE ENDORSEMENT FOR EMPLOYER
      *
     C     ENDOR         BEGSR
     C*
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXEMP#        QQEMP#
     C                   MOVE      XXDIV         QQDIV
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      UPOUTQ        QQOUTQ
     C                   MOVE      XXDART        QQDART
     C                   MOVE      XXST          QQRST
     C*
     C* PRINT THE ENDORSEMENTS
     C*
     C                   CALL      'W8042R'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    QQOUTQ
     C                   PARM                    QQPCT
     C                   PARM                    QQEFDT
     C                   PARM                    QQDART
     C                   PARM                    QQRST             2
     C*
     C                   ENDSR                                                  ENDOR SR
      *
      *****************************************************************
      *
      *  SUBROUTINE TO CHECK AND PREPARE EMPLOYER FOR SPREAD DISCOUNT
      *
     C     SPRD          BEGSR
     C*
     C*  DETERMINE IF SPREAD IS IN EFFECT
     C*
     C                   MOVE      XXCO#         Q1CO#             3
     C                   MOVE      XXFND         Q1FND             3
     C                   MOVE      XXFYR         Q1FYR             3
     C                   MOVE      XXEMP#        Q1EMP#            9
     C*
     C*
     C*
     C                   MOVE      XXDIV         Q1DIV
     C                   Z-ADD     0             Q1PREM
     C                   MOVE      'E'           Q1TYPE
     C                   MOVE      '3'           Q1CALL
     C                   MOVE      ' '           Q1RNB
     C*
     C                   CALL      'SRMDIV'
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1PREM           11 2
     C                   PARM                    Q1TYPE            1
     C                   PARM                    Q1CALL            1
     C                   PARM                    Q1RNB             1
     C*
     C*
     C                   ENDSR                                                  SPRD SR
      *
      *****************************************************************
      *
      *  SUBROUTINE TO INITIALIZE DATA AND CHAIN TO FILES.
      *
     C     SR1ST         BEGSR
      *
     C                   MOVE      *OFF          *IN08
     C                   MOVE      *ON           *IN09
      *
     C     UPKY          CHAIN     PUP01                              60
      *
      * INITIALIZE FIELDS AND MOVE SELECTION CRITERIA
      *    TO ROUTING PAGE
      *
     C                   TIME                    DSDTTM
     C*
     C     COKY          CHAIN     PCO01                              60
     C*
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*
     C     FNKY          CHAIN(N)  PFN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     FNANAM        IFNE      *BLANKS
     C                   MOVEL     FNANAM        CONAME
     C                   ENDIF                                                  FNANAM<>BLK
     C                   ENDIF                                                  *IN60=OFF
     C*
     C*   GET THE DISTRIBUTION INFORMATION FOR PRINTER FILE 0
     C*
     C                   MOVE      '0'           XXPRT             1
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXTITL           60
     C                   MOVEL     ORTITL        XXTITL
     C                   MOVE      *BLANKS       XXDIS1           40
     C                   MOVEL     ORDIS1        XXDIS1
     C                   MOVE      *BLANKS       XXDIS2           40
     C                   MOVEL     ORDIS2        XXDIS2
     C                   MOVE      *BLANKS       XXDIS3           40
     C                   MOVEL     ORDIS3        XXDIS3
     C                   MOVE      *BLANKS       XXDIS4           40
     C                   MOVEL     ORDIS4        XXDIS4
     C                   MOVE      *BLANKS       XXPNAM           10
     C                   MOVEL     ORPNAM        XXPNAM
     C                   MOVE      *BLANKS       XXFIL#            1
     C                   MOVEL     ORFIL#        XXFIL#
     C                   MOVE      *BLANKS       XXCOPY            2 0
     C                   MOVEL     ORCOPY        XXCOPY
     C                   MOVE      *BLANKS       XXHOLD            4
     C                   MOVEL     ORHOLD        XXHOLD
     C                   MOVE      *BLANKS       XXOUTQ           10
     C                   MOVEL     OROUTQ        XXOUTQ
     C                   MOVE      *BLANKS       XXSAV             4
     C                   MOVEL     ORSAVE        XXSAV
     C                   END
     C*
     C*   PRINT THE DISTRIBUTION INFORMATION
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   EXCEPT    HEAD                                                     IC.
     C*
     C                   MOVE      'Y'           XXDART            1
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      EXREV         XXRMCD
     C                   ELSE
     C                   MOVE      *BLANKS       XXRMCD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXRMCD        IFEQ      *BLANKS
     C                   MOVE      'N'           XXDART
     C                   ENDIF                                                  XXRMCD IFEQ
     C*
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     SYOFIF        ORNE      'Y'
     C                   MOVE      'N'           XXDART
     C                   ENDIF
      *
     C                   MOVEL     XXFORM        XKFORM
     C                   MOVEL(P)  XXFRM         XXFRM1           15
      *
     C                   OPEN      WXICP
      *
     C     '.inc(END'    CAT(P)    'ORSE,':0     ICSTMT
     C                   CAT       'AMTEMP':0    ICSTMT
     C                   CAT       ',)':0        ICSTMT
     C*
     C                   WRITE     PIC01
     C*
     C     '.inc('       CAT(P)    XXFRM1:0      ICSTMT
     C                   CAT       ',':0         ICSTMT
     C                   CAT       'AMTEMP':0    ICSTMT
     C                   CAT       ',)':0        ICSTMT
     C*
     C                   WRITE     PIC01
     C*
     C                   CLOSE     WXICP
     C*
     C                   ENDSR                                                  SR1ST
      *
      *****************************************************************
      *
      *  SUBROUTINE TO PROCESS BILLING REVISIONS FOR EMPLOYER.
      *
     C     CMD7          BEGSR
     C*
     C*  LOAD PREVIOUS BILLING SCHEDULE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C  N60              MOVEA     AB            AB1
     C*
     C*  LOAD NEW BILLING SCHEDULE
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELBSCD        XXKEY
     C                   MOVEL     ELBSCD        XXBSCD
     C                   MOVEL     ELBLFL        XXBLFL            1
     C     TBKY          CHAIN     PBS01                              60
     C  N60              MOVEA     AB            AB2
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C*
     C     ELARSM        IFEQ      'Y'
     C     XXWAR         ANDNE     'Y'
     C*
     C     ELANPR        SUB       ELPRTD        X1ANPR           11 2
      *** beg add ***** 07/02/13 **************************************
      /free
       // Reduce annual premium (X1ANPR) by the amount of dividend premium
       x1anpr = x1anpr - ELdvpr;
      /end-free
      *** end add ***** 07/02/13 **************************************
     C                   ADD       X1ANPR        XTANPR
     C                   ADD       ELTXDU        XTTXDU
     C     RR1STB        IFEQ      'Y'
     C                   ADD       EL1AAM        XT1STB
     C                   ELSE
     C                   ADD       EL1AAM        XL1STB
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   ADD       EL2AAM        XT2NDB
     C                   ELSE
     C                   ADD       EL2AAM        XL2NDB
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   ADD       EL3AAM        XT3RDB
     C                   ELSE
     C                   ADD       EL3AAM        XL3RDB
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   ADD       EL4AAM        XT4THB
     C                   ELSE
     C                   ADD       EL4AAM        XL4THB
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   ADD       EL5AAM        XT5THB
     C                   ELSE
     C                   ADD       EL5AAM        XL5THB
     C                   ENDIF
     C     RR6THB        IFEQ      'Y'
     C                   ADD       EL6AAM        XT6THB
     C                   ELSE
     C                   ADD       EL6AAM        XL6THB
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   ADD       EL7AAM        XT7THB
     C                   ELSE
     C                   ADD       EL7AAM        XL7THB
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C*  CLEAR DATE AND AMOUNT ARRAYS TO BE USED ON ADJUSTMENT SCREEN
     C*
     C                   MOVEA     *ZEROS        DA
     C                   MOVEA     *ZEROS        PA
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C*
     C*  COMPUTE ADJUSTMENT IF NEW BILLING SCHEDULE DESIGNATES A
     C*  SELF-REPORTING ACCOUNT
     C*
     C     BSSEFL        IFEQ      'S'
     C                   MOVE      *ON           *IN81
     C*
     c*** Beg   Add ***  02/08/13  ************************************
      /free
       Select;
       When BSab01 <> 0;
          xxAB01 = BSab01;
       Other;
          xxAB01 = BSab02;
       Endsl;
       Exsr DepositAdj;
      /end-free
     c*** End   Add ***  02/08/13  ************************************
     c*** Beg   Dlt ***  02/08/13  ************************************
     C*************      Z-ADD     *ZEROS        XXADJ            11 2
     C*************      Z-ADD     *ZEROS        XXADJ2           11 2
     C*****F1DPS***      IFEQ      'N'
     C*****ELENMP**      SUB       SVNMPR        XXADJ
     C*************      ELSE
     C*****F1DPS***      IFEQ      'M'
     C*****ELMAPR**      SUB       SVMAPR        XXADJ
     C*************      ELSE
     C*****F1DPS***      IFEQ      'S'
     C*****ELSTPR**      SUB       SVSTPR        XXADJ
     C*************      ELSE
     C*****ELANPR**      SUB       ELPRTD        XXAM12           11 2
     C*****XXAM12**      SUB       S2ANPR        XXADJ
     C*************      END
     C*************      END
     C*************      END
     C*************
     C*************      Z-ADD     *ZEROS        XXARPR           11 2
     C*************
     C*************      MOVE      'H'           XXARTY
     C*****EAKY3***      CHAIN     PEA03                              50           ALL FYRS
     C**** *IN50***      DOWEQ     '0'
     C*****EAFYR***      ANDLE     XXFYR
     C*************      ADD       EAAMT         XXARPR
     C*****EAKY3***      READE     PEA03                                  50
     C*************      END                                                    *IN50
     C*************
     C*****BSAB01**      IFNE      *ZEROS
     C*****XXARPR**      DIV       BSAB01        XXARPR
     C*************      END
     C*************
     C*****F1DPS***      IFEQ      'N'
     C*****SVNMPR**      SUB       XXARPR        XXADJ2
     C*************      ELSE
     C*****F1DPS***      IFEQ      'M'
     C*****SVMAPR**      SUB       XXARPR        XXADJ2
     C*************      ELSE
     C*****F1DPS**       IFEQ      'S'
     C*****SVSTPR*       SUB       XXARPR        XXADJ2
     C*************      ELSE
     C*****S2ANPR**      SUB       XXARPR        XXADJ2
     C*************      END                                                    F1DPS = 'N'
     C*************      END                                                    F1DPS = 'M'
     C*************      END                                                    F1DPS = 'S'
     C*************      ADD       XXADJ2        XXADJ
     C*************
     C*  ROUND TO NEAREST DOLLAR
     C*************
     C*************      Z-ADD     XXADJ         XXSAVE           11 2
     C*****XXADJ***      IFLT      *ZERO
     C*************      MULT      -1            XXADJ
     C*************      END
     C*************      MOVE      XXADJ         XX                2 0
     C*****XX******      IFGE      50
     C*************      ADD       1             XXADJ
     C*************      END
     C*************      MOVE      '00'          XXADJ
     C*****XXSAVE**      IFLT      *ZERO
     C*****XXADJ***      MULT      -1            XXADJ
     C*************      END
     C*************
     C*****XXADJ***      MULT(H)   BSAB01        XXWK90            9 0
     C*****BSAB01**      IFEQ      *ZERO
     C*************      Z-SUB     XXARPR        XXWK90
     C*************      ENDIF                                                  BSAB01 IFEQ
     C*************      Z-ADD     XXWK90        SCTOT            11 2
     C*************      MOVE      *BLANK        XXTHRS            1
     C*****F1BVFL**      IFNE      'Y'
     C*************      Z-ADD     0             XXADJ
     C*************      Z-ADD     0             SCTOT
     C*************      MOVE      'Y'           XXTHRS
     C*************      ELSE
     C*************
     C*****XXWK90**      IFGE      0
     C*************
     C* DO NOT POST ADJ IF LESS THAN DEBIT THRESHOLD
     C*************
     C*****XXWK90*       IFLT      F1DADJ
     C*************      Z-ADD     0             XXADJ
     C*************      Z-ADD     0             SCTOT
     C*************      MOVE      'Y'           XXTHRS
     C*************      ENDIF                                                  XXWK90 IFLT
     C*************
     C*************      ELSE
     C*************
     C* DO NOT POST ADJ IF GREATER THAN CREDIT THRESHOLD
     C*************
     C*****XXWK90**      MULT      -1            XXTMP             9 0
     C*************
     C*****XXTMP***      IFLT      F1CADJ
     C*************      Z-ADD     0             XXADJ
     C*************      Z-ADD     0             SCTOT
     C*************      MOVE      'Y'           XXTHRS
     C*************      ENDIF                                                  XXTMP IFLT
     C*************
     C*************      ENDIF                                                  XXWK90 IFGE
     C*************
     C*************      ENDIF                                                  F1BVFL IFNE
     C*************      Z-ADD     SCTOT         XXDEPP           11 2
     C*************
     C*  CALCULATE THE STATE TAX ADJUSTMENT
     C*************
     C*************      Z-ADD     0             XXTXDU           11 2
     C*************      MOVE      'F'           XXARTY
     C*****EAKY2***      CHAIN     PEA01                              50
     C**** *IN50***      DOWEQ     '0'
     C*************
     C*************      MOVEL(P)  EAARCD        XXKEY
     C*****TBKY****      CHAIN     PTT01                              60
     C**** *IN60***      IFEQ      *OFF
     C*************
     C*****TTCTYP**      IFEQ      'T'
     C*****BSSEFL**      ANDEQ     'E'
     C*****TTCTYP**      OREQ      'D'
     C*****BSSEFL**      ANDEQ     'S'
     C*************      ADD       EAAMT         XXTXDU
     C*************      ENDIF                                                  TTCTYP IFEQ
     C*************
     C*************      ENDIF                                                  *IN60 IFEQ
     C*************
     C*****EAKY2***      READE     PEA01                                  50
     C*************      END                                                    *IN50
     C*************
     C*****RRTDFL**      IFNE      'Y'
     C*****XXTHRS**      OREQ      'Y'
     C*************      Z-ADD     0             XXTXDU
     C*************      ELSE
     C*************
     C*****BSAB01**      MULT(H)   XXADJ         XXWK90
     C*****BSAB01**      IFEQ      *ZERO
     C*************      Z-SUB     XXARPR        XXWK90
     C*************      ENDIF                                                  BSAB01 IFEQ
     C*************      MULT(H)   RRTXPC        XXWK90
     C*************      MULT(H)   .01           XXWK90
     C*************      Z-ADD     XXWK90        XXTXDU
     C*************
     C*************      ENDIF                                                  RRTDFL IFNE
     C*************
     C*************      ADD       XXTXDU        SCTOT
     C*************      MOVE      '0'           *IN80
     C*************      Z-ADD     0             X
     C*************      MOVE      *OFF          *IN89
     C*************
     C*** End   Dlt ***  02/08/13  ************************************
     C* BILL THE ASSESSMENTS INTO INSTALLMENTS IF FLAGGED TO DO SO,
     C* OTHERWISE BILL IN LUMP SUM.  ASSESSMENT CODING WILL BE HANDLED
     C* BY W8011R FOR S/R POLICIES FOR CLIENTS NOT FLAGGED TO BILL HERE.
     C*
     C     CLTID         IFEQ      '139'
     C*
     C                   MOVE      *ON           *IN89
     C*
     C* PROCESS CALCULATIONS OF ADJUSTMENTS TO THE ASSESSMENTS.
     C*
     C     XXWAR         IFEQ      'Y'
     C*
     C* SET SUMMARY TOTALS INTO WORKING ASSESSMENT FIELDS.
     C*
     C                   Z-ADD     XT1STB        XX1STB           11 2
     C                   Z-ADD     XT2NDB        XX2NDB           11 2
     C                   Z-ADD     XT3RDB        XX3RDB           11 2
     C                   Z-ADD     XT4THB        XX4THB           11 2
     C                   Z-ADD     XT5THB        XX5THB           11 2
     C                   Z-ADD     XL1STB        XU1STB           11 2
     C                   Z-ADD     XL2NDB        XU2NDB           11 2
     C                   Z-ADD     XL3RDB        XU3RDB           11 2
     C                   Z-ADD     XL4THB        XU4THB           11 2
     C                   Z-ADD     XL5THB        XU5THB           11 2
     C                   Z-ADD     XT6THB        XX6THB           11 2
     C                   Z-ADD     XT7THB        XX7THB           11 2
     C                   Z-ADD     XL6THB        XU6THB           11 2
     C                   Z-ADD     XL7THB        XU7THB           11 2
     C*
     C                   ELSE
     C*
     C* SET CURRENT AMOUNT FIELDS FOR EACH ASSESSMENT FOR NON-SUMMARY.
     C*
     C     RR1STB        IFEQ      'Y'
     C                   Z-ADD     EL1AAM        XX1STB
     C                   ELSE
     C                   Z-ADD     EL1AAM        XU1STB
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   Z-ADD     EL2AAM        XX2NDB
     C                   ELSE
     C                   Z-ADD     EL2AAM        XU2NDB
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   Z-ADD     EL3AAM        XX3RDB
     C                   ELSE
     C                   Z-ADD     EL3AAM        XU3RDB
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   Z-ADD     EL4AAM        XX4THB
     C                   ELSE
     C                   Z-ADD     EL4AAM        XU4THB
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   Z-ADD     EL5AAM        XX5THB
     C                   ELSE
     C                   Z-ADD     EL5AAM        XU5THB
     C                   ENDIF
     C     RR6THB        IFEQ      'Y'
     C                   Z-ADD     EL6AAM        XX6THB
     C                   ELSE
     C                   Z-ADD     EL6AAM        XU6THB
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   Z-ADD     EL7AAM        XX7THB
     C                   ELSE
     C                   Z-ADD     EL7AAM        XU7THB
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXWAR IFEQ 'Y'
     C*
     C* COUNT THE NUMBER OF MONTHS ON THE POLICY TO BE REPORTED.
     C*
     C                   MOVE      ELPFDT        X2PFDT
     C                   MOVEL     ELPTDT        XXYMX             5 0
     C                   Z-ADD     0             XXCNT
     C*
     C     YYMM1         DOWLT     XXYMX
     C                   ADD       1             XXCNT
     C                   ADD       1             MM1
     C*
     C     MM1           IFEQ      13
     C                   Z-ADD     1             MM1
     C                   ADD       1             YY1
     C                   ENDIF
     C*
     C                   ENDDO                                                  YYMM1 DOWLT XXYMX
     C*
     C                   MOVE      '01'          DD1
     C*
     C* IF POLICY DOES NOT END ON 1ST OF MONTH, THEN ADD IN EXTRA MONTH.
     C*
     C     X2PFDT        IFLT      ELPTDT
     C                   ADD       1             XXCNT
     C                   ENDIF
     C*
     C* IF NO REPORTINGS HAVE BEEN SET THEN BILL ALL TO ONE AMOUNT.
     C*
     C     XXCNT         IFEQ      0
     C                   ADD       1             XXCNT
     C                   ENDIF
     C*
     C* SET UP ARRAY OF POSTING DATES FOR S/R ASSESSMENT INSTALLMENTS.
     C*
     C                   CLEAR                   WKD
     C                   MOVE      ELPFDT        X2PFDT
     C*
     C     1             DO        XXCNT         L                 2 0
     C                   MOVE      X2PFDT        WKD(L)
     C                   MOVE      MD(MM1)       WKD(L)
     C*
     C* CHECK IF FEB 29TH IS A VALID DATE AND USE FOR THAT MONTH.
     C*
     C     MM1           IFEQ      2
     C                   Z-ADD     WKD(L)        XXDTE
     C                   MOVE      29            XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVE      29            WKD(L)
     C                   ENDIF
     C*
     C                   ENDIF                                                  MM1 IFEQ 2
     C*
     C                   ADD       1             MM1
     C*
     C     MM1           IFEQ      13
     C                   Z-ADD     1             MM1
     C                   ADD       1             YY1
     C                   ENDIF
     C*
     C                   ENDDO                                                  1 DO XXCNT
     C*
     C                   SUB       1             L
     C*
     C     WKD(L)        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        WKD(L)
     C                   ENDIF
     C*
     C                   ADD       1             L
     C                   MOVE      *HIVAL        WKD(L)
     C*
     C* CLEAR WORKING FIELDS.
     C*
     C                   Z-ADD     *ZEROS        W
     C                   Z-ADD     *ZEROS        Y
     C                   Z-ADD     *ZEROS        Z
     C                   Z-ADD     *ZEROS        XXDTE
     C                   Z-ADD     *ZEROS        XXDTE1
     C                   Z-ADD     *ZEROS        XXDTE2
     C                   Z-ADD     *ZEROS        XXDIFF
     C                   Z-ADD     *ZEROS        YRMO
     C                   Z-ADD     *ZEROS        YYMM
     C                   MOVE      '0'           XXFUT
     C                   Z-ADD     *ZEROS        X1ATOT
     C                   Z-ADD     *ZEROS        X1AA1
     C                   Z-ADD     *ZEROS        X1AA2
     C                   Z-ADD     *ZEROS        X1AA3
     C                   Z-ADD     *ZEROS        X2ATOT
     C                   Z-ADD     *ZEROS        X2AA1
     C                   Z-ADD     *ZEROS        X2AA2
     C                   Z-ADD     *ZEROS        X2AA3
     C                   Z-ADD     *ZEROS        X3ATOT
     C                   Z-ADD     *ZEROS        X3AA1
     C                   Z-ADD     *ZEROS        X3AA2
     C                   Z-ADD     *ZEROS        X3AA3
     C                   Z-ADD     *ZEROS        X4ATOT
     C                   Z-ADD     *ZEROS        X4AA1
     C                   Z-ADD     *ZEROS        X4AA2
     C                   Z-ADD     *ZEROS        X4AA3
     C                   Z-ADD     *ZEROS        X5ATOT
     C                   Z-ADD     *ZEROS        X5AA1
     C                   Z-ADD     *ZEROS        X5AA2
     C                   Z-ADD     *ZEROS        X5AA3
     C                   Z-ADD     *ZEROS        X6ATOT
     C                   Z-ADD     *ZEROS        X6AA1
     C                   Z-ADD     *ZEROS        X6AA2
     C                   Z-ADD     *ZEROS        X6AA3
     C                   Z-ADD     *ZEROS        X7ATOT
     C                   Z-ADD     *ZEROS        X7AA1
     C                   Z-ADD     *ZEROS        X7AA2
     C                   Z-ADD     *ZEROS        X7AA3
     C*
     C* CALCULATE MONTHLY CHARGE FOR ASSESSMENT.
     C*
     C     XX1STB        DIV(H)    XXCNT         X1AMT            11 2
     C     XX2NDB        DIV(H)    XXCNT         X2AMT            11 2
     C     XX3RDB        DIV(H)    XXCNT         X3AMT            11 2
     C     XX4THB        DIV(H)    XXCNT         X4AMT            11 2
     C     XX5THB        DIV(H)    XXCNT         X5AMT            11 2
     C     XX6THB        DIV(H)    XXCNT         X6AMT            11 2
     C     XX7THB        DIV(H)    XXCNT         X7AMT            11 2
     C*
     C* ROUND INSTALLMENTS TO WHOLE DOLLAR IF FLAGGED.
     C*
     C     EXRNDI        IFEQ      'Y'
     C*
     C                   Z-ADD(H)  X1AMT         XXWK10
     C                   Z-ADD     XXWK10        X1AMT
     C*
     C                   Z-ADD(H)  X2AMT         XXWK10
     C                   Z-ADD     XXWK10        X2AMT
     C*
     C                   Z-ADD(H)  X3AMT         XXWK10
     C                   Z-ADD     XXWK10        X3AMT
     C*
     C                   Z-ADD(H)  X4AMT         XXWK10
     C                   Z-ADD     XXWK10        X4AMT
     C*
     C                   Z-ADD(H)  X5AMT         XXWK10
     C                   Z-ADD     XXWK10        X5AMT
     C*
     C                   Z-ADD(H)  X6AMT         XXWK10
     C                   Z-ADD     XXWK10        X6AMT
     C*
     C                   Z-ADD(H)  X7AMT         XXWK10
     C                   Z-ADD     XXWK10        X7AMT
     C*
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C* SET THE POSTING DATE FOR THE DEPOSIT/TAX ADJUSTMENT.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDT
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             EXRVID
     C                   ENDIF
     C*
     C     SCTOT         IFGT      *ZERO
     C     EXRVID        ANDNE     *ZERO
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SCDT              8 0
     C                   ENDIF
     C*
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SCDT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*
     C* DO NOT ALLOW DEP/TAX POSTING DATE TO BE PRIOR TO BILLING START DATE.
     C*
     C     XXDT          IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C                   ENDIF                                                  XXDT IFLT
     C*
     C* MOVE DEPOSIT/TAX ADJUSTMENT INTO ARRAY.
     C*
     C     SCTOT         IFNE      *ZERO
     C                   Z-ADD     XXDEPP        PA(1)
     C                   Z-ADD     XXTXDU        P1(1)
     C                   Z-ADD     SCDT          DA(1)
     C                   ENDIF
     C*
     C* SET CURRENT DATE.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT
     C                   PARM      EXRVID        QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C* SET CURRENT DATE TO THE END OF THE MONTH TO MATCH INSTALLMENT DATE.
     C*
     C                   Z-ADD     XXCURD        X2PFDT
     C                   MOVE      MD(MM1)       X2PFDT
     C                   Z-ADD     X2PFDT        XXCURD
     C*
     C     MM1           IFEQ      2
     C                   Z-ADD     XXCURD        XXDTE
     C                   MOVE      29            XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVE      29            XXCURD
     C                   ENDIF
     C*
     C                   ENDIF                                                  MM1 IFEQ 2
     C*
     C                   Z-ADD     XXCURD        XXCUR2
     C*
     C* SET BEGINNING DATE TO THE END OF THE MONTH TO MATCH INSTALLMENT DATE.
     C*
     C                   Z-ADD     ELPFDT        X2PFDT
     C                   MOVE      MD(MM1)       X2PFDT
     C                   Z-ADD     X2PFDT        XXBSDT            7 0
     C*
     C     MM1           IFEQ      2
     C                   Z-ADD     XXBSDT        XXDTE
     C                   MOVE      29            XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C                   MOVE      29            XXBSDT
     C                   ENDIF
     C*
     C                   ENDIF                                                  MM1 IFEQ 2
     C*
     C                   MOVEL     XXCURD        YRMO1
     C                   MOVE      YRMO1         YRMO
     C                   MOVEL     ELPFDT        YYMM
     C                   Z-ADD     1             Y
     C                   Z-ADD     *ZEROS        XXTOT
     C*
     C* LOOP THROUGH ALL MONTHS ON POLICY PRIOR TO CURRENT POSTING DATE.
     C*
     C     Y             DOWLE     XXCNT
     C     YYMM          ANDLE     YRMO
     C*
     C     Y             ADD       1             W
     C*
     C                   Z-ADD     WKD(Y)        XXDTE1
     C*
     C     WKD(W)        IFGT      XXCURD
     C*
     C*  WE NEED TO INCLUDE ANY BILLINGS OR ADJUSTMENTS WITH TODAYS DATE
     C*
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*
     C                   Z-ADD     1             SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*
     C*  COMPUTE TODAYS DATE + 1 SO THAT SRWASB WHICH LOOKS FOR 'A'
     C*  OR 'B' A/R RECORDS WITH EAPODT >= XXDTE1 AND EAPODT < XXDTE2
     C*  WILL INCLUDE TODAYS DATE.
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   Z-ADD     XXCURD        XXDTE2
     C*
     C     SRRTCD        IFEQ      '0'
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE2
     C                   END
     C*
     C                   Z-ADD     XXDTE2        XXCUR2
     C*
     C                   ELSE
     C                   Z-ADD     WKD(W)        XXDTE2
     C                   END                                                    WKD,W >= XXCURD
     C*
     C* IF THE END DATE IS BEFORE THE FIRST POSTING DATE, MOVE IT UP TO THE
     C* FIRST POSTING DATE.
     C*
     C     XXDTE2        IFLT      XXBSDT
     C                   Z-ADD     XXBSDT        XXDTE2
     C*
     C* USE THE FIRST POSTING DATE + 1 DAY
     C*
     C                   Z-ADD     XXBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     1             SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C     SRRTCD        IFEQ      '0'
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE2
     C*
     C                   ENDIF                                                  SRRTCD IFEQ
     C*
     C                   ENDIF                                                  XXDTE2 IFLT
     C*
     C* CALL SUBROUTINE TO ACCUMULATE PRIOR BILLED FROM A/R.
     C*
     C                   EXSR      SRWASB
     C*
     C* 1ST ASSESSMENT
     C*
     C                   Z-ADD(H)  X1AMT         X1AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C*
     C     X1AA2         SUB       X1AA1         XXDIFF
     C                   ADD       XXDIFF        X1ATOT
     C                   ADD       X1AA2         X1AA3
     C*
     C* 2ND ASSESSMENT
     C*
     C                   Z-ADD(H)  X2AMT         X2AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C*
     C     X2AA2         SUB       X2AA1         XXDIFF
     C                   ADD       XXDIFF        X2ATOT
     C                   ADD       X2AA2         X2AA3
     C*
     C* 3RD ASSESSMENT
     C*
     C                   Z-ADD(H)  X3AMT         X3AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C*
     C     X3AA2         SUB       X3AA1         XXDIFF
     C                   ADD       XXDIFF        X3ATOT
     C                   ADD       X3AA2         X3AA3
     C*
     C* 4TH ASSESSMENT
     C*
     C                   Z-ADD(H)  X4AMT         X4AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C*
     C     X4AA2         SUB       X4AA1         XXDIFF
     C                   ADD       XXDIFF        X4ATOT
     C                   ADD       X4AA2         X4AA3
     C*
     C* 5TH ASSESSMENT
     C*
     C                   Z-ADD(H)  X5AMT         X5AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C*
     C     X5AA2         SUB       X5AA1         XXDIFF
     C                   ADD       XXDIFF        X5ATOT
     C                   ADD       X5AA2         X5AA3
     C*
     C* 6TH ASSESSMENT
     C*
     C                   Z-ADD(H)  X6AMT         X6AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU6THB        X6AA2
     C                   ENDIF
     C*
     C     X6AA2         SUB       X6AA1         XXDIFF
     C                   ADD       XXDIFF        X6ATOT
     C                   ADD       X6AA2         X6AA3
     C*
     C* 7TH ASSESSMENT
     C*
     C                   Z-ADD(H)  X7AMT         X7AA2
     C*
     C     Y             IFEQ      1
     C                   ADD       XU7THB        X7AA2
     C                   ENDIF
     C*
     C     X7AA2         SUB       X7AA1         XXDIFF
     C                   ADD       XXDIFF        X7ATOT
     C                   ADD       X7AA2         X7AA3
     C*
     C* INCREMENT TO NEXT MONTH.
     C*
     C                   ADD       1             Y
     C                   ADD       1             MM
     C*
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   ENDIF
     C*                                                     &  YYMM <= YRMO
     C                   ENDDO                                                  DOW Y <= 12
     C*                                                     &  YYMM <= YRMO
     C* MOVE CATCH UP ADJUSTMENT AMOUNT INTO ADJUSTMENT ARRAY.
     C*
     C     X1ATOT        IFNE      *ZEROS
     C     X2ATOT        ORNE      *ZEROS
     C     X3ATOT        ORNE      *ZEROS
     C     X4ATOT        ORNE      *ZEROS
     C     X5ATOT        ORNE      *ZEROS
     C     X6ATOT        ORNE      *ZEROS
     C     X7ATOT        ORNE      *ZEROS
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXDT8             8 0
     C*
     C     X1ATOT        IFGT      *ZEROS
     C     X2ATOT        ORGT      *ZEROS
     C     X3ATOT        ORGT      *ZEROS
     C     X4ATOT        ORGT      *ZEROS
     C     X5ATOT        ORGT      *ZEROS
     C     X6ATOT        ORGT      *ZEROS
     C     X7ATOT        ORGT      *ZEROS
     C*
     C     XX1STB        IFNE      *ZERO
     C     XX2NDB        ORNE      *ZERO
     C     XX3RDB        ORNE      *ZERO
     C     XX4THB        ORNE      *ZERO
     C     XX5THB        ORNE      *ZERO
     C     XX6THB        ORNE      *ZERO
     C     XX7THB        ORNE      *ZERO
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   ELSE
     C                   Z-ADD     SCDT          XXDT8
     C                   ENDIF                                                  XX1STB IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* IF THE POSTING DATE IS PRIOR TO THE BILLING START DATE,
     C* CHANGE IT.
     C*
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C*
     C     XXTDT         IFLT      XXBSDT
     C*
     C     X1ATOT        IFGT      *ZEROS
     C     X2ATOT        ORGT      *ZEROS
     C     X3ATOT        ORGT      *ZEROS
     C     X4ATOT        ORGT      *ZEROS
     C     X5ATOT        ORGT      *ZEROS
     C     X6ATOT        ORGT      *ZEROS
     C     X7ATOT        ORGT      *ZEROS
     C                   Z-ADD     XXBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   ELSE
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDT8
     C                   ENDIF                                                  X1ATOT IFGT
     C*
     C                   ENDIF                                                  XXTDT IFLT
     C*
     C* SET POSTING DATE FOR ASSESSMENT ADJUSTMENT.  IF THE DATE DOES
     C* NOT MATCH THE DEP/TAX ADJUSTMENT DATE THEN PUT IN SLOT #2.
     C*
     C     SCTOT         IFNE      *ZERO
     C     XXDT8         ANDNE     SCDT
     C                   ADD       2             X
     C                   ELSE
     C                   ADD       1             X
     C                   ENDIF
     C*
     C                   Z-ADD     XXDT8         DA(X)
     C*
     C     XX1STB        IFNE      *ZERO
     C                   Z-ADD     X1ATOT        A1(X)
     C                   ELSE
     C                   Z-ADD     X1ATOT        A1(1)
     C                   ENDIF
     C*
     C     XX2NDB        IFNE      *ZERO
     C                   Z-ADD     X2ATOT        A2(X)
     C                   ELSE
     C                   Z-ADD     X2ATOT        A2(1)
     C                   ENDIF
     C*
     C     XX3RDB        IFNE      *ZERO
     C                   Z-ADD     X3ATOT        A3(X)
     C                   ELSE
     C                   Z-ADD     X3ATOT        A3(1)
     C                   ENDIF
     C*
     C     XX4THB        IFNE      *ZERO
     C                   Z-ADD     X4ATOT        A4(X)
     C                   ELSE
     C                   Z-ADD     X4ATOT        A4(1)
     C                   ENDIF
     C*
     C     XX5THB        IFNE      *ZERO
     C                   Z-ADD     X5ATOT        A5(X)
     C                   ELSE
     C                   Z-ADD     X5ATOT        A5(1)
     C                   ENDIF
     C*
     C     XX6THB        IFNE      *ZERO
     C                   Z-ADD     X6ATOT        A6(X)
     C                   ELSE
     C                   Z-ADD     X6ATOT        A6(1)
     C                   ENDIF
     C*
     C     XX7THB        IFNE      *ZERO
     C                   Z-ADD     X7ATOT        A7(X)
     C                   ELSE
     C                   Z-ADD     X7ATOT        A7(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  X1ATOT <> 0
     C*
     C* PROCESS THE REMAINING MONTHS ON THE POLICY.
     C*
     C     Y             DO        XXCNT         Z
     C*
     C     Z             ADD       1             W
     C*
     C*  THE FOLLOWING MUST ONLY BE DONE FOR THE FIRST BILLING PERIOD
     C*  (I.E. FROM WKD,Y TO WKD,Y+1; ONLY THE FIRST TIME THIS CODE IS EXECUTED)
     C*  NEED TO BE SURE TO INCLUDE ANY 'B' OR 'A' A/R RECORDS THAT MAY
     C*  HAVE BEEN MANUALLY POSTED BETWEEN THE CURRENT DATE (OR CURRENT
     C*  DATE + 1) AND THE NEXT BILLING PERIOD (WKD,W)
     C*
     C     XXCUR2        IFNE      *ZEROS
     C                   Z-ADD     XXCUR2        XXDTE1
     C                   Z-ADD     *ZEROS        XXCUR2
     C                   ELSE
     C                   Z-ADD     WKD(Z)        XXDTE1
     C                   ENDIF                                                  XXCUR2 <> 0
     C*
     C                   Z-ADD     WKD(W)        XXDTE2
     C*
     C* IF THE START DATE IS PRIOR TO THE BILLING START DATE,
     C* CHANGE IT.
     C*
     C     XXDTE1        IFLT      XXBSDT
     C                   Z-ADD     WKD(Z)        XXDTE1
     C                   ENDIF                                                  XXDTE1 IFLT
     C*
     C                   EXSR      SRWASB
      *
     C* 1ST ASSESSMENT
      *
     C                   Z-ADD(H)  X1AMT         X1AA2
     C     Z             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C*
     C     X1AA2         SUB       X1AA1         X1ATOT
     C                   ADD       X1AA2         X1AA3
     C*
     C* 2ND ASSESSMENT
      *
     C                   Z-ADD(H)  X2AMT         X2AA2
     C     Z             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C*
     C     X2AA2         SUB       X2AA1         X2ATOT
     C                   ADD       X2AA2         X2AA3
     C*
     C* 3RD ASSESSMENT
      *
     C                   Z-ADD(H)  X3AMT         X3AA2
     C     Z             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C*
     C     X3AA2         SUB       X3AA1         X3ATOT
     C                   ADD       X3AA2         X3AA3
     C*
     C* 4TH ASSESSMENT
      *
     C                   Z-ADD(H)  X4AMT         X4AA2
     C     Z             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C*
     C     X4AA2         SUB       X4AA1         X4ATOT
     C                   ADD       X4AA2         X4AA3
     C*
     C* 5TH ASSESSMENT
      *
     C                   Z-ADD(H)  X5AMT         X5AA2
     C     Z             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C*
     C     X5AA2         SUB       X5AA1         X5ATOT
     C                   ADD       X5AA2         X5AA3
     C*
     C* 6TH ASSESSMENT
      *
     C                   Z-ADD(H)  X6AMT         X6AA2
     C     Z             IFEQ      1
     C                   ADD       XU6THB        X6AA2
     C                   ENDIF
     C*
     C     X6AA2         SUB       X6AA1         X6ATOT
     C                   ADD       X6AA2         X6AA3
     C*
     C* 7TH ASSESSMENT
      *
     C                   Z-ADD(H)  X7AMT         X7AA2
     C     Z             IFEQ      1
     C                   ADD       XU7THB        X7AA2
     C                   ENDIF
     C*
     C     X7AA2         SUB       X7AA1         X7ATOT
     C                   ADD       X7AA2         X7AA3
     C*
     C* MOVE NEXT ADJUSTMENT INTO ARRAY.
     C*
     C     XXTOT         IFNE      *ZEROS
     C     X1TOT         ORNE      *ZEROS
     C     X1ATOT        ORNE      *ZEROS
     C     X2ATOT        ORNE      *ZEROS
     C     X3ATOT        ORNE      *ZEROS
     C     X4ATOT        ORNE      *ZEROS
     C     X5ATOT        ORNE      *ZEROS
     C     X6ATOT        ORNE      *ZEROS
     C     X7ATOT        ORNE      *ZEROS
     C                   ADD       1             X
     C*
     C                   Z-ADD     WKD(Z)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C*
     C                   Z-ADD     X1ATOT        A1(X)
     C                   Z-ADD     X2ATOT        A2(X)
     C                   Z-ADD     X3ATOT        A3(X)
     C                   Z-ADD     X4ATOT        A4(X)
     C                   Z-ADD     X5ATOT        A5(X)
     C                   Z-ADD     X6ATOT        A6(X)
     C                   Z-ADD     X7ATOT        A7(X)
     C                   ENDIF                                                  XXTOT <> 0
     C*
     C                   ENDDO                                                  Y DO XXCNT Z
     C*
     C*  CHECK FOR ANY ROUNDING DISCREPANCIES BETWEEN CURRENT A/R AND
     C*  NEW ADJUSTMENTS THAT WILL BE POSTED TO A/R; BOTH SHOULD EQUAL
     C*  THE NEW PREMIUM AMOUNT TO THE PENNY
     C*
     C     XX1STB        SUB       X1AA3         X1AA3
     C     XX2NDB        SUB       X2AA3         X2AA3
     C     XX3RDB        SUB       X3AA3         X3AA3
     C     XX4THB        SUB       X4AA3         X4AA3
     C     XX5THB        SUB       X5AA3         X5AA3
     C     XX6THB        SUB       X6AA3         X6AA3
     C     XX7THB        SUB       X7AA3         X7AA3
     C                   ADD       XU1STB        X1AA3
     C                   ADD       XU2NDB        X2AA3
     C                   ADD       XU3RDB        X3AA3
     C                   ADD       XU4THB        X4AA3
     C                   ADD       XU5THB        X5AA3
     C                   ADD       XU6THB        X6AA3
     C                   ADD       XU7THB        X7AA3
     C*
     C*  DIFFERENCE SHOULD NEVER BE GREATER THAN OR LESS THAN $1.00
     C*
     C     X1AA3         IFLT      1.00
     C     X1AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A1(1)         IFNE      *ZERO
     C                   ADD       X1AA3         A1(1)
     C                   ELSE
     C                   ADD       X1AA3         A1(2)
     C                   ENDIF
     C                   END
     C*
     C     X2AA3         IFLT      1.00
     C     X2AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A2(1)         IFNE      *ZERO
     C                   ADD       X2AA3         A2(1)
     C                   ELSE
     C                   ADD       X2AA3         A2(2)
     C                   ENDIF
     C                   END
     C*
     C     X3AA3         IFLT      1.00
     C     X3AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A3(1)         IFNE      *ZERO
     C                   ADD       X3AA3         A3(1)
     C                   ELSE
     C                   ADD       X3AA3         A3(2)
     C                   ENDIF
     C                   END
     C*
     C     X4AA3         IFLT      1.00
     C     X4AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A4(1)         IFNE      *ZERO
     C                   ADD       X4AA3         A4(1)
     C                   ELSE
     C                   ADD       X4AA3         A4(2)
     C                   ENDIF
     C                   END
     C*
     C     X5AA3         IFLT      1.00
     C     X5AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A5(1)         IFNE      *ZERO
     C                   ADD       X5AA3         A5(1)
     C                   ELSE
     C                   ADD       X5AA3         A5(2)
     C                   ENDIF
     C                   END
     C*
     C     X6AA3         IFLT      1.00
     C     X6AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A6(1)         IFNE      *ZERO
     C                   ADD       X6AA3         A6(1)
     C                   ELSE
     C                   ADD       X6AA3         A6(2)
     C                   ENDIF
     C                   END
     C*
     C     X7AA3         IFLT      1.00
     C     X7AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C     A7(1)         IFNE      *ZERO
     C                   ADD       X7AA3         A7(1)
     C                   ELSE
     C                   ADD       X7AA3         A7(2)
     C                   ENDIF
     C                   END
     C*
     C                   XFOOT     A1            SCTOT2
     C                   XFOOT     A2            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A3            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A4            SCTOT3
     C                   XFOOT     A5            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A6            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A7            SCTOT4           11 2
     C*
     C                   Z-ADD     13            X
     C     X             DOWGT     0
     C     PA(X)         IFNE      *ZERO
     C     P1(X)         ORNE      *ZERO
     C     A1(X)         ORNE      *ZERO
     C     A2(X)         ORNE      *ZERO
     C     A3(X)         ORNE      *ZERO
     C     A4(X)         ORNE      *ZERO
     C     A5(X)         ORNE      *ZERO
     C     A6(X)         ORNE      *ZERO
     C     A7(X)         ORNE      *ZERO
     C                   LEAVE
     C                   ELSE
     C                   Z-ADD     0             DA(X)
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  CLTID IFEQ
     C*
     C*  PRINT BILL & RETURN TO SCREEN 1 IF THERE IS NO BILLING CHANGE
     C*
     C     SCTOT         IFEQ      *ZERO
     C     CLTID         ANDNE     '139'
     C     SCTOT         OREQ      *ZERO
     C     X             ANDEQ     0
     C     CLTID         ANDEQ     '139'
     C     F1UPAR        OREQ      'N'
     C*
     C*  DUE TO CHANGE MADE ON 01/19/92 (TO ALWAYS CREATE PLACE REVISED
     C*  RECORDS IN FILES WAELP AND WAEPP) WE MUST STILL GO TO
     C*  SCR04 TO PROCESS BILLING REVISION EVEN THOUGH NO ADJUSTMENTS
     C*  ARE TO BE POSTED.  THE BILLING REVISION FILES STILL NEED TO
     C*  BE MOVED INTO THE LIVE ENVIRONMENT. IN ADDITION, IF THERE WAS
     C*  A BILLING SCHEDULE CHANGE, FUTURE BILLINGS AND ADJUSTMENTS NEED
     C*  TO BE DELETED
     C*
     C                   MOVE      '1'           *IN80
     C                   MOVEA     *ZEROS        DA
     C                   MOVEA     *ZEROS        PA
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C                   MOVE      *BLANKS       QQAR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCEFDT            8 0
     C*
     C                   ELSE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCEFDT            8 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCDT
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     SCTOT         ANDGT     *ZERO
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SCDT              8 0
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SCDT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     CLTID         IFNE      '139'
     C     SCDT          IFNE      *ZERO
     C                   Z-ADD     SCDT          DA(1)
     C                   ELSE
     C                   Z-ADD     SCEFDT        DA(1)
     C                   ENDIF                                                  SCDT IFNE
     C     XXADJ         MULT(H)   BSAB01        XXWK90            9 0
     C     BSAB01        IFEQ      *ZERO
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  BSAB01 IFEQ
     C                   Z-ADD     XXWK90        XXADJ
     C                   Z-ADD     XXADJ         PA(1)
     C                   Z-ADD     XXTXDU        P1(1)
     C                   ENDIF                                                  CLTID IFNE
     C                   END
     C*
     C                   END                                                    BSSEFL = 'S'
     C*
     C*  COMPUTE ADJUSTMENT IF NEW BILLING SCHEDULE DESIGNATES AN
     C*  ESTIMATING ACCOUNT
     C*
     C     BSSEFL        IFEQ      'E'
     C                   MOVE      *OFF          *IN81
     c*** Beg   Add ***  02/08/13  ************************************
      /free
       Select;
       When BSesdp <> 0;
          xxAB02 = BSesdp;
       Other;
          xxab02 = BSesd2;
       Endsl;
       xxAB01 = xxAB02/100;
       Exsr DepositAdj;
      /end-free
     c*** End   Add ***  02/08/13  ************************************
     C*
     C                   Z-ADD     *ZEROS        W                 3 0
     C                   Z-ADD     *ZEROS        X                 3 0
     C                   Z-ADD     *ZEROS        Y                 3 0
     C                   Z-ADD     *ZEROS        Z                 3 0
     C                   Z-ADD     *ZEROS        XXDTE
     C                   Z-ADD     *ZEROS        XXDTE1
     C                   Z-ADD     *ZEROS        XXDTE2
     C                   Z-ADD     *ZEROS        XXDIFF           11 2
     C                   Z-ADD     *ZEROS        XXTOT            11 2
     C                   Z-ADD     *ZEROS        XXAMT1           11 2
     C                   Z-ADD     *ZEROS        XXAMT2           11 2
     C                   Z-ADD     *ZEROS        YRMO              5 0
     C                   Z-ADD     *ZEROS        XXAMT3           11 2
     C                   Z-ADD     *ZEROS        X1TOT            11 2
     C                   Z-ADD     *ZEROS        X1AMT1           11 2
     C                   Z-ADD     *ZEROS        X1AMT2           11 2
     C                   Z-ADD     *ZEROS        X1AMT3           11 2
     C                   MOVE      '0'           XXFUT             1
     C                   Z-ADD     *ZEROS        X1ATOT           11 2
     C                   Z-ADD     *ZEROS        X1AA1            11 2
     C                   Z-ADD     *ZEROS        X1AA2            11 2
     C                   Z-ADD     *ZEROS        X1AA3            11 2
     C*
     C                   Z-ADD     *ZEROS        X2ATOT           11 2
     C                   Z-ADD     *ZEROS        X2AA1            11 2
     C                   Z-ADD     *ZEROS        X2AA2            11 2
     C                   Z-ADD     *ZEROS        X2AA3            11 2
     C*
     C                   Z-ADD     *ZEROS        X3ATOT           11 2
     C                   Z-ADD     *ZEROS        X3AA1            11 2
     C                   Z-ADD     *ZEROS        X3AA2            11 2
     C                   Z-ADD     *ZEROS        X3AA3            11 2
     C*
     C                   Z-ADD     *ZEROS        X4ATOT           11 2
     C                   Z-ADD     *ZEROS        X4AA1            11 2
     C                   Z-ADD     *ZEROS        X4AA2            11 2
     C                   Z-ADD     *ZEROS        X4AA3            11 2
     C*
     C                   Z-ADD     *ZEROS        X5ATOT           11 2
     C                   Z-ADD     *ZEROS        X5AA1            11 2
     C                   Z-ADD     *ZEROS        X5AA2            11 2
     C                   Z-ADD     *ZEROS        X5AA3            11 2
     C*
     C                   Z-ADD     *ZEROS        X6ATOT           11 2
     C                   Z-ADD     *ZEROS        X6AA1            11 2
     C                   Z-ADD     *ZEROS        X6AA2            11 2
     C                   Z-ADD     *ZEROS        X6AA3            11 2
     C*
     C                   Z-ADD     *ZEROS        X7ATOT           11 2
     C                   Z-ADD     *ZEROS        X7AA1            11 2
     C                   Z-ADD     *ZEROS        X7AA2            11 2
     C                   Z-ADD     *ZEROS        X7AA3            11 2
     C*
     C     XXWAR         IFEQ      'Y'
     C                   Z-ADD     XTANPR        X1ANPR
     C                   Z-ADD     XTTXDU        ELTXDU
     C                   Z-ADD     XT1STB        XX1STB           11 2
     C                   Z-ADD     XT2NDB        XX2NDB           11 2
     C                   Z-ADD     XT3RDB        XX3RDB           11 2
     C                   Z-ADD     XT4THB        XX4THB           11 2
     C                   Z-ADD     XT5THB        XX5THB           11 2
     C                   Z-ADD     XT6THB        XX6THB           11 2
     C                   Z-ADD     XT7THB        XX7THB           11 2
     C                   Z-ADD     XL1STB        XU1STB           11 2
     C                   Z-ADD     XL2NDB        XU2NDB           11 2
     C                   Z-ADD     XL3RDB        XU3RDB           11 2
     C                   Z-ADD     XL4THB        XU4THB           11 2
     C                   Z-ADD     XL5THB        XU5THB           11 2
     C                   Z-ADD     XL6THB        XU6THB           11 2
     C                   Z-ADD     XL7THB        XU7THB           11 2
     C                   ELSE
     C     ELANPR        SUB       ELPRTD        X1ANPR           11 2
      *** beg add ***** 07/02/13 **************************************
      /free
       // Reduce annual premium (X1ANPR) by the amount of dividend premium
       x1anpr = x1anpr - ELdvpr;
      /end-free
      *** end add ***** 07/02/13 **************************************
     C     RR1STB        IFEQ      'Y'
     C                   Z-ADD     EL1AAM        XX1STB
     C                   ELSE
     C                   Z-ADD     EL1AAM        XU1STB
     C                   ENDIF
     C     RR2NDB        IFEQ      'Y'
     C                   Z-ADD     EL2AAM        XX2NDB
     C                   ELSE
     C                   Z-ADD     EL2AAM        XU2NDB
     C                   ENDIF
     C     RR3RDB        IFEQ      'Y'
     C                   Z-ADD     EL3AAM        XX3RDB
     C                   ELSE
     C                   Z-ADD     EL3AAM        XU3RDB
     C                   ENDIF
     C     RR4THB        IFEQ      'Y'
     C                   Z-ADD     EL4AAM        XX4THB
     C                   ELSE
     C                   Z-ADD     EL4AAM        XU4THB
     C                   ENDIF
     C     RR5THB        IFEQ      'Y'
     C                   Z-ADD     EL5AAM        XX5THB
     C                   ELSE
     C                   Z-ADD     EL5AAM        XU5THB
     C                   ENDIF
     C     RR6THB        IFEQ      'Y'
     C                   Z-ADD     EL6AAM        XX6THB
     C                   ELSE
     C                   Z-ADD     EL6AAM        XU6THB
     C                   ENDIF
     C     RR7THB        IFEQ      'Y'
     C                   Z-ADD     EL7AAM        XX7THB
     C                   ELSE
     C                   Z-ADD     EL7AAM        XU7THB
     C                   ENDIF
     C                   ENDIF
     C     BSDOWK        IFEQ      *BLANKS
     C     BSFREQ        ANDNE     'T'
     C*
     C*
     C*  FIRST LOAD BILLING SCHEDULE DATES INTO AN ARRAY FOR LATER
     C*  USE
     C*
     C                   Z-ADD     ELBSDT        XXDTE
     C                   MOVEL     XXDTE         YYMM
     C                   MOVE      XXDTE         DD                2 0
     C                   DO        12            X
     C     ELBLFL        IFEQ      'Y'
      *
     C                   MOVE      DD            XXDTE
      *
     C     MM            IFEQ      2
     C     DD            ANDGE     29
     C                   MOVE      29            XXDTE
     C                   Z-ADD     XXDTE         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *ON
     C                   MOVE      28            XXDTE
     C                   ENDIF                                                  SRERR IFEQ *ON
      *
     C                   ELSE
      *
     C     DD            IFGT      30
      *
     C     MM            IFEQ      4
     C     MM            OREQ      6
     C     MM            OREQ      9
     C     MM            OREQ      11
     C                   MOVE      30            XXDTE
     C                   ENDIF                                                  MM IFEQ 4
      *
     C                   ENDIF                                                  DD IFGT 30
      *
     C                   ENDIF                                                  MM IFEQ 2
      *
     C                   ENDIF                                                  ELBLFL IFEQ 'Y'
     C                   Z-ADD     XXDTE         BD(X)
     C                   ADD       1             MM
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   END
     C                   MOVEL     YYMM          XXDTE
     C     ELBLFL        IFNE      'Y'
     C                   MOVE      '01'          XXDTE
     C                   ENDIF                                                  ELBLFL IFNE
     C                   END
     C                   Z-ADD     *HIVAL        BD(13)
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     XXCURD        XXCUR2
     C*
     C                   MOVEL     XXCURD        YRMO1             5 0
     C                   MOVE      YRMO1         YRMO
     C                   MOVEL     ELBSDT        YYMM
     C                   Z-ADD     1             Y
     C                   Z-ADD     0             X
     C                   Z-ADD     *ZEROS        XXTOT
     C     Y             DOWLE     12
     C     YYMM          ANDLE     YRMO
     C     Y             ADD       1             W
     C                   Z-ADD     BD(Y)         XXDTE1
     C     BD(W)         IFGT      XXCURD
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C     ELBLFL        ANDEQ     XXBLFL
     C*
     C*  IF THE BILLING SCHEDULE CODE CHANGED WE WILL BE DELETING
     C*  ALL BILLINGS AND ADJUSTMENTS WITH TODAY'S DATE OR GREATER SO
     C*  WHEN CALCULATING WHAT WAS BILLED DO NOT INCLUDE TODAYS DATE.
     C*  IF THE BILLING SCHEDULE DOES NOT CHANGE, HOWEVER, WE NEED TO
     C*  INCLUDE ANY BILLINGS OR ADJUSTMENTS WITH TODAYS DATE
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SRDATE            8 0
     C*
     C                   Z-ADD     1             SRFUTR            5 0
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   ADD       EXRVID        SRFUTR
     C     XXACDT        IFNE      *ZERO
     C                   Z-ADD     1             SRFUTR            5 0
     C                   ADD       EXAOID        SRFUTR
     C                   ENDIF                                                  XXACDT IFNE
     C                   ENDIF
     C                   MOVE      ' '           SRRTCD            1
     C*
     C*  COMPUTE TODAYS DATE + 1 SO THAT SRWASB WHICH LOOKS FOR 'A'
     C*  OR 'B' A/R RECORDS WITH EAPODT >= XXDTE1 AND EAPODT < XXDTE2
     C*  WILL INCLUDE TODAYS DATE IF THE BILLING SCHEDULE CODE HAS
     C*  NOT CHANGED
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C                   Z-ADD     XXCURD        XXDTE2
     C     SRRTCD        IFEQ      '0'
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE2
     C*
     C                   END
     C                   Z-ADD     XXDTE2        XXCUR2
     C                   ELSE                                                   ELBSCD <> XXBSCD
     C                   Z-ADD     XXCURD        XXDTE2
     C                   END                                                    ELBSCD = XXBSCD
     C*
     C                   ELSE
     C                   Z-ADD     BD(W)         XXDTE2
     C                   END                                                    BD,W >= XXCURD
     C*
     C* IF THE END DATE IS BEFORE THE POLICY STARTS, MOVE IT UP TO THE
     C* EFFECTIVE DATE.
     C*
     C     XXDTE2        IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        XXDTE2
     C*
     C* USE THE BILLING START DATE + 1 DAY
     C*
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C                   Z-ADD     1             SRFUTR            5 0
     C                   MOVE      ' '           SRRTCD            1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRFUTR
     C                   PARM                    SRRTCD
     C*
     C     SRRTCD        IFEQ      '0'
     C*
     C                   Z-ADD     SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDTE2
     C*
     C                   ENDIF                                                  SRRTCD IFEQ
     C                   ENDIF                                                  XXDTE2 IFLT
     C*
     C                   EXSR      SRWASB
     C     AB2(Y)        MULT(H)   X1ANPR        XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     XXAMT2        SUB       XXAMT1        XXDIFF
     C                   ADD       XXDIFF        XXTOT
     C*
     C*  CALCULATE THE STATE TAX DUE FOR THIS PERIOD
     C*
     C     AB2(Y)        MULT      ELTXDU        X1AMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     X1AMT2        SUB       X1AMT1        XXDIFF
     C                   ADD       XXDIFF        X1TOT
     C                   ADD       X1AMT2        X1AMT3
     C*
     C     AB2(Y)        MULT      XX1STB        X1AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C*
     C     X1AA2         SUB       X1AA1         XXDIFF
     C                   ADD       XXDIFF        X1ATOT
     C                   ADD       X1AA2         X1AA3
     C*
     C     AB2(Y)        MULT      XX2NDB        X2AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X2AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X2AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C*
     C     X2AA2         SUB       X2AA1         XXDIFF
     C                   ADD       XXDIFF        X2ATOT
     C                   ADD       X2AA2         X2AA3
     C*
     C     AB2(Y)        MULT      XX3RDB        X3AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X3AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X3AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C*
     C     X3AA2         SUB       X3AA1         XXDIFF
     C                   ADD       XXDIFF        X3ATOT
     C                   ADD       X3AA2         X3AA3
     C*
     C     AB2(Y)        MULT      XX4THB        X4AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X4AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X4AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C*
     C     X4AA2         SUB       X4AA1         XXDIFF
     C                   ADD       XXDIFF        X4ATOT
     C                   ADD       X4AA2         X4AA3
     C*
     C     AB2(Y)        MULT      XX5THB        X5AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X5AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X5AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C*
     C     X5AA2         SUB       X5AA1         XXDIFF
     C                   ADD       XXDIFF        X5ATOT
     C                   ADD       X5AA2         X5AA3
     C*
     C     AB2(Y)        MULT      XX6THB        X6AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X6AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X6AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU6THB        X6AA2
     C                   ENDIF
     C*
     C     X6AA2         SUB       X6AA1         XXDIFF
     C                   ADD       XXDIFF        X6ATOT
     C                   ADD       X6AA2         X6AA3
     C*
     C     AB2(Y)        MULT      XX7THB        X7AA2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X7AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X7AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Y             IFEQ      1
     C                   ADD       XU7THB        X7AA2
     C                   ENDIF
     C*
     C     X7AA2         SUB       X7AA1         XXDIFF
     C                   ADD       XXDIFF        X7ATOT
     C                   ADD       X7AA2         X7AA3
     C*
     C*  MAINTAIN A TOTAL OF NEW PREMIUM AMOUNT ACCORDING TO CURRENT
     C*  A/R AND NEW ADJUSTMENTS SO THAT ROUNDING MAY BE CHECKED AT THE
     C*  END
     C*
     C                   ADD       XXAMT2        XXAMT3
     C                   ADD       1             Y
     C                   ADD       1             MM
     C     MM            IFGT      12
     C                   Z-ADD     1             MM
     C                   ADD       1             YY
     C                   END
     C                   END                                                    DOW Y <= 12
     C*                                                     &  YYMM <= YRMO
     C                   MOVE      *BLANK        X1FLAG            1
     C     Y             DO        12            Z
     C     AB2(Z)        IFNE      *ZEROS
     C                   MOVE      'Y'           X1FLAG
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     X1FLAG        IFEQ      *BLANK
     C*
     C                   Z-ADD     XXDTE2        XXDTE1
     C                   Z-ADD     *HIVAL        XXDTE2
     C                   EXSR      SRWASB
     C                   SUB       XXAMT1        XXTOT
     C                   SUB       X1AMT1        X1TOT
     C                   SUB       X1AA1         X1ATOT
     C                   SUB       X2AA1         X2ATOT
     C                   SUB       X3AA1         X3ATOT
     C                   SUB       X4AA1         X4ATOT
     C                   SUB       X5AA1         X5ATOT
     C                   SUB       X6AA1         X6ATOT
     C                   SUB       X7AA1         X7ATOT
     C*
     C                   ENDIF                                                  XXFLAG IFEQ
     C     XXTOT         IFNE      *ZEROS
     C     X1ATOT        ORNE      *ZERO
     C     X2ATOT        ORNE      *ZERO
     C     X3ATOT        ORNE      *ZERO
     C     X4ATOT        ORNE      *ZERO
     C     X5ATOT        ORNE      *ZERO
     C     X6ATOT        ORNE      *ZERO
     C     X7ATOT        ORNE      *ZERO
     C                   ADD       1             X
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         DA(X)
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXTOT         ANDGT     *ZERO
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          DA(X)
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        DA(X)
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* IF THE POSTING DATE IS PRIOR TO THE POLICY EFFECTIVE DATE,
     C* CHANGE IT.
     C*
     C                   Z-ADD     DA(X)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTDT             7 0
     C     XXTDT         IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C                   ENDIF                                                  XXTDT IFLT
     C*
     C                   Z-ADD     XXTOT         PA(X)
     C                   Z-ADD     X1TOT         P1(X)
     C                   Z-ADD     X1ATOT        A1(X)
     C                   Z-ADD     X2ATOT        A2(X)
     C                   Z-ADD     X3ATOT        A3(X)
     C                   Z-ADD     X4ATOT        A4(X)
     C                   Z-ADD     X5ATOT        A5(X)
     C                   Z-ADD     X6ATOT        A6(X)
     C                   Z-ADD     X7ATOT        A7(X)
     C                   END                                                    XXTOT <> 0
     C*
     C     ELBLFL        IFNE      'Y'
     C                   MOVE      '01'          XXDTE
     C                   ENDIF                                                  ELBLFL IFNE
     C     Y             DO        12            Z
     C     AB2(Z)        IFNE      *ZEROS
     C*
     C*  IF THE BILLING SCHEDLUE CODE DID NOT CHANGE WE MUST COMPARE
     C*  WHAT WAS BILLED FOR A GIVEN MONTH TO WHAT SHOULD BE BILLED
     C*  THAT MONTH UNDER THE NEW BILLING SCHEDULE CODE
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C*
     C     Z             ADD       1             W
     C*
     C*  THE FOLLOWING MUST ONLY BE DONE FOR THE FIRST BILLING PERIOD
     C*  (I.E. FROM BD,Y TO BD,Y+1; ONLY THE FIRST TIME THIS CODE IS EXECUTED)
     C*  NEED TO BE SURE TO INCLUDE ANY 'B' OR 'A' A/R RECORDS THAT MAY
     C*  HAVE BEEN MANUALLY POSTED BETWEEN THE CURRENT DATE (OR CURRENT
     C*  DATE + 1) AND THE NEXT BILLING PERIOD (BD,W)
     C*
     C     XXCUR2        IFNE      *ZEROS
     C                   Z-ADD     XXCUR2        XXDTE1
     C                   Z-ADD     *ZEROS        XXCUR2
     C                   ELSE
     C                   Z-ADD     BD(Z)         XXDTE1
     C                   END                                                    XXCUR2 <> 0
     C                   Z-ADD     BD(W)         XXDTE2
     C*
     C* IF THE START DATE IS PRIOR TO THE POLICY EFFECTIVE DATE,
     C* CHANGE IT.
     C*
     C     XXDTE1        IFLT      ELBSDT
     C                   Z-ADD     BD(Z)         XXDTE1
     C                   ENDIF                                                  XXDTE1 IFLT
     C*
     C                   EXSR      SRWASB
     C     AB2(Z)        MULT(H)   X1ANPR        XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     XXAMT2        SUB       XXAMT1        XXTOT
     C     AB2(Z)        MULT(H)   ELTXDU        X1AMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     X1AMT2        SUB       X1AMT1        X1TOT
     C                   ADD       X1AMT2        X1AMT3
     C*
     C     AB2(Z)        MULT(H)   XX1STB        X1AA2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X1AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X1AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       XU1STB        X1AA2
     C                   ENDIF
     C*
     C     X1AA2         SUB       X1AA1         X1ATOT
     C                   ADD       X1AA2         X1AA3
     C*
     C     AB2(Z)        MULT(H)   XX2NDB        X2AA2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X2AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X2AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       XU2NDB        X2AA2
     C                   ENDIF
     C*
     C     X2AA2         SUB       X2AA1         X2ATOT
     C                   ADD       X2AA2         X2AA3
     C*
     C     AB2(Z)        MULT(H)   XX3RDB        X3AA2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X3AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X3AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       XU3RDB        X3AA2
     C                   ENDIF
     C*
     C     X3AA2         SUB       X3AA1         X3ATOT
     C                   ADD       X3AA2         X3AA3
     C*
     C     AB2(Z)        MULT(H)   XX4THB        X4AA2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X4AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X4AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       XU4THB        X4AA2
     C                   ENDIF
     C*
     C     X4AA2         SUB       X4AA1         X4ATOT
     C                   ADD       X4AA2         X4AA3
     C*
     C     AB2(Z)        MULT(H)   XX5THB        X5AA2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X5AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X5AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       XU5THB        X5AA2
     C                   ENDIF
     C*
     C     X5AA2         SUB       X5AA1         X5ATOT
     C                   ADD       X5AA2         X5AA3
     C     AB2(Z)        MULT(H)   XX6THB        X6AA2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X6AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X6AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       XU6THB        X6AA2
     C                   ENDIF
     C*
     C     X6AA2         SUB       X6AA1         X6ATOT
     C                   ADD       X6AA2         X6AA3
     C*
     C     AB2(Z)        MULT(H)   XX7THB        X7AA2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  X7AA2         XXWK10           11 0
     C                   Z-ADD     XXWK10        X7AA2
     C                   ENDIF                                                  EXRNDI IFEQ
     C     Z             IFEQ      1
     C                   ADD       XU7THB        X7AA2
     C                   ENDIF
     C*
     C     X7AA2         SUB       X7AA1         X7ATOT
     C                   ADD       X7AA2         X7AA3
     C*
     C*  MAINTAIN A TOTAL OF NEW PREMIUM AMOUNT ACCORDING TO CURRENT
     C*  A/R AND NEW ADJUSTMENTS SO THAT ROUNDING MAY BE CHECKED AT THE
     C*  END
     C*
     C                   ADD       XXAMT2        XXAMT3
     C     XXTOT         IFNE      *ZEROS
     C     X1ATOT        ORNE      *ZEROS
     C     X2ATOT        ORNE      *ZEROS
     C     X3ATOT        ORNE      *ZEROS
     C     X4ATOT        ORNE      *ZEROS
     C     X5ATOT        ORNE      *ZEROS
     C     X6ATOT        ORNE      *ZEROS
     C     X7ATOT        ORNE      *ZEROS
     C                   ADD       1             X
     C*
     C                   Z-ADD     BD(Z)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C*
     C                   Z-ADD     XXTOT         PA(X)
     C                   Z-ADD     X1TOT         P1(X)
     C                   Z-ADD     X1ATOT        A1(X)
     C                   Z-ADD     X2ATOT        A2(X)
     C                   Z-ADD     X3ATOT        A3(X)
     C                   Z-ADD     X4ATOT        A4(X)
     C                   Z-ADD     X5ATOT        A5(X)
     C                   Z-ADD     X6ATOT        A6(X)
     C                   Z-ADD     X7ATOT        A7(X)
     C                   END                                                    XXTOT <> 0
     C*
     C                   ELSE                                                   ELBSCD <> XXBSCD
     C*
     C*  IF THE BILLING CODE WAS CHANGED ALL FUTURE BILLINGS AND
     C*  ADJUSTMENTS WILL BE DELETED SO THE ADUSTMENT AMOUNT IS
     C*  SIMPLY DETERMINED BY THE NEW BILLING SCHEDULE CODE
     C*
     C                   ADD       1             X
     C*
     C                   Z-ADD     BD(Z)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(X)
     C*
     C     AB2(Z)        MULT(H)   X1ANPR        PA(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  PA(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        PA(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C     AB2(Z)        MULT(H)   ELTXDU        P1(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  P1(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        P1(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       P1(X)         X1AMT3
     C*
     C     AB2(Z)        MULT(H)   XX1STB        A1(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A1(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A1(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A1(X)         X1AA3
     C*
     C     AB2(Z)        MULT(H)   XX2NDB        A2(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A2(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A2(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A2(X)         X2AA3
     C*
     C     AB2(Z)        MULT(H)   XX3RDB        A3(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A3(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A3(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A3(X)         X3AA3
     C*
     C     AB2(Z)        MULT(H)   XX4THB        A4(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A4(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A4(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A4(X)         X4AA3
     C*
     C     AB2(Z)        MULT(H)   XX5THB        A5(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A5(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A5(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A5(X)         X5AA3
     C*
     C     AB2(Z)        MULT(H)   XX6THB        A6(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A6(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A6(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A6(X)         X6AA3
     C*
     C     AB2(Z)        MULT(H)   XX7THB        A7(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A7(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A7(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       A7(X)         X7AA3
     C*
     C*  MAINTAIN A TOTAL OF NEW PREMIUM AMOUNT ACCORDING TO CURRENT
     C*  A/R AND NEW ADJUSTMENTS SO THAT ROUNDING MAY BE CHECKED AT THE
     C*  END
     C*
     C                   ADD       PA(X)         XXAMT3
     C*
     C                   END                                                    ELBSCD = XXBSCD
     C                   END                                                    AB2,Z <> 0
     C                   END                                                    Y DO 12 Z
      *
      * TAKE THE FIRST ADJUSTMENT, WHICH SHOULD BE OF ALL PAST AND
      * CURRENT ADJUSTMENTS, AND SPREAD IT ACROSS ALL FUTURE THE
      * FUTURE INSTALLMENTS.
      *
     C                   MOVE      ELFFL1        XXFFL1            1
      *
     C     CLTID         IFEQ      '109'
     C                   MOVE      'Y'           XXFFL1
     C                   ENDIF
      *
     C     PA(1)         IFGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     PA(1)         ORLT      *ZERO
     C     ELBSCD        ANDEQ     XXBSCD
     C     XXFFL1        ANDEQ     'Y'
     C     CLTID         ANDNE     '109'
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     DA(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT1             7 0
     C*
     C     XXDT1         IFLE      XXDT
     C     XXDT1         ANDNE     *ZERO
     C*
     C                   Z-ADD     0             XXINST            2 0
     C     2             DO        12            X
     C     PA(X)         IFNE      *ZEROS
     C                   ADD       1             XXINST
     C                   ENDIF
     C                   ENDDO
     C*
     C     XXINST        IFGE      1
     C*
     C                   SUB       PA(1)         XXAMT3
     C                   SUB       P1(1)         X1AMT3
     C                   SUB       A1(1)         X1AA3
     C                   SUB       A2(1)         X2AA3
     C                   SUB       A3(1)         X3AA3
     C                   SUB       A4(1)         X4AA3
     C                   SUB       A5(1)         X5AA3
     C                   SUB       A6(1)         X6AA3
     C                   SUB       A7(1)         X7AA3
     C     1             DO        XXINST        X
     C     X             ADD       1             W
     C     PA(1)         DIV(H)    XXINST        XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       XXAMT2        PA(W)
     C                   ADD       XXAMT2        XXAMT3
     C     P1(1)         DIV(H)    XXINST        XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C                   ADD       XXAMT2        P1(W)
     C                   ADD       XXAMT2        X1AMT3
     C* 1ST ASSESSMENT
     C     A1(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A1(W)
     C                   ADD       XXAMT2        X1AA3
     C*
     C* 2ND ASSESSMENT
     C     A2(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A2(W)
     C                   ADD       XXAMT2        X2AA3
     C*
     C* 3RD ASSESSMENT
     C     A3(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A3(W)
     C                   ADD       XXAMT2        X3AA3
     C*
     C* 4TH ASSESSMENT
     C     A4(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A4(W)
     C                   ADD       XXAMT2        X4AA3
     C*
     C* 5TH ASSESSMENT
     C     A5(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A5(W)
     C                   ADD       XXAMT2        X5AA3
     C*
     C* 6TH ASSESSMENT
     C     A6(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A6(W)
     C                   ADD       XXAMT2        X6AA3
     C*
     C* 7TH ASSESSMENT
     C     A7(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A7(W)
     C                   ADD       XXAMT2        X7AA3
     C*
     C                   ENDDO
     C*
     C     2             DO        12            X
     C     X             SUB       1             W
     C                   Z-ADD     PA(X)         PA(W)
     C                   Z-ADD     DA(X)         DA(W)
     C                   Z-ADD     0             PA(X)
     C                   Z-ADD     0             DA(X)
     C                   Z-ADD     P1(X)         P1(W)
     C                   Z-ADD     0             P1(X)
     C                   Z-ADD     A1(X)         A1(W)
     C                   Z-ADD     0             A1(X)
     C                   Z-ADD     A2(X)         A2(W)
     C                   Z-ADD     0             A2(X)
     C                   Z-ADD     A3(X)         A3(W)
     C                   Z-ADD     0             A3(X)
     C                   Z-ADD     A4(X)         A4(W)
     C                   Z-ADD     0             A4(X)
     C                   Z-ADD     A5(X)         A5(W)
     C                   Z-ADD     0             A5(X)
     C                   Z-ADD     A6(X)         A6(W)
     C                   Z-ADD     0             A6(X)
     C                   Z-ADD     A7(X)         A7(W)
     C                   Z-ADD     0             A7(X)
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXINST IFGE
     C*
     C                   ENDIF                                                  XXDT1 IFLE
     C*
     C                   ENDIF                                                  PA,1 IFGT
     C*
     C*
     C*
     C*  CHECK FOR ANY ROUNDING DISCREPANCIES BETWEEN CURRENT A/R AND
     C*  NEW ADJUSTMENTS THAT WILL BE POSTED TO A/R; BOTH SHOULD EQUAL
     C*  THE NEW PREMIUM AMOUNT TO THE PENNY
     C*
     C     X1ANPR        SUB       XXAMT3        XXAMT3
     C     ELTXDU        SUB       X1AMT3        X1AMT3
     C     XX1STB        SUB       X1AA3         X1AA3
     C     XX2NDB        SUB       X2AA3         X2AA3
     C     XX3RDB        SUB       X3AA3         X3AA3
     C     XX4THB        SUB       X4AA3         X4AA3
     C     XX5THB        SUB       X5AA3         X5AA3
     C     XX6THB        SUB       X6AA3         X6AA3
     C     XX7THB        SUB       X7AA3         X7AA3
     C                   ADD       XU1STB        X1AA3
     C                   ADD       XU2NDB        X2AA3
     C                   ADD       XU3RDB        X3AA3
     C                   ADD       XU4THB        X4AA3
     C                   ADD       XU5THB        X5AA3
     C                   ADD       XU6THB        X6AA3
     C                   ADD       XU7THB        X7AA3
     C*
     C*  DIFFERENCE SHOULD NEVER BE GREATER THAN OR LESS THAN $1.00
     C*
     C     XXAMT3        IFLT      1.00
     C     XXAMT3        ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C                   ADD       XXAMT3        PA(1)
     C                   END
     C     X1AMT3        IFLT      1.00
     C     X1AMT3        ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C                   ADD       X1AMT3        P1(1)
     C                   END
     C*
     C     X1AA3         IFLT      1.00
     C     X1AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C                   ADD       X1AA3         A1(1)
     C                   ENDIF
     C*
     C     X2AA3         IFLT      1.00
     C     X2AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C                   ADD       X2AA3         A2(1)
     C                   ENDIF
     C*
     C     X3AA3         IFLT      1.00
     C     X3AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C                   ADD       X3AA3         A3(1)
     C                   ENDIF
     C*
     C     X4AA3         IFLT      1.00
     C     X4AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C                   ADD       X4AA3         A4(1)
     C                   ENDIF
     C*
     C     X5AA3         IFLT      1.00
     C     X5AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C                   ADD       X5AA3         A5(1)
     C                   ENDIF
     C*
     C     X6AA3         IFLT      1.00
     C     X6AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C                   ADD       X6AA3         A6(1)
     C                   ENDIF
     C*
     C     X7AA3         IFLT      1.00
     C     X7AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C                   ADD       X7AA3         A7(1)
     C                   ENDIF
     C*
     C                   ELSE                                                   BSDOWK IFEQ
     C*
     C     BSFREQ        IFEQ      'T'
      *
      * PROCESS AND BILL TO A/R TWICE A MONTH BILLLING INSTALLMENTS.
      *
     C                   Z-ADD     0             WKD
     C                   Z-ADD     0             WKA
     C                   Z-ADD     0             WKT
     C                   Z-ADD     0             WK1
     C                   Z-ADD     0             WK2
     C                   Z-ADD     0             WK3
     C                   Z-ADD     0             WK4
     C                   Z-ADD     0             WK5
     C                   Z-ADD     0             WK6
     C                   Z-ADD     0             WK7
     C*
     C                   MOVE      ELBSDT        XXWKDT
     C*
     C* CHECK FOR FIRST INSTALLMENT ON/AFTER BILLING START DATE.
     C*
     C     WDD           IFEQ      1
     C     WDD           OREQ      15
     C                   Z-ADD     ELBSDT        WKD(1)
     C                   ELSE
     C*
     C     WDD           IFLT      15
     C                   Z-ADD     15            WDD
     C                   Z-ADD     XXWKDT        WKD(1)
     C                   ELSE
     C*
     C                   Z-ADD     1             WDD
     C                   ADD       1             WMM
     C     WMM           IFEQ      13
     C                   Z-ADD     1             WMM
     C                   ADD       1             WYY
     C                   ENDIF
     C*
     C                   Z-ADD     XXWKDT        WKD(1)
     C                   ENDIF                                                  DD IFLT
     C*
     C                   ENDIF                                                  DD IFEQ
     C*
     C                   Z-ADD     1             T                 3 0
     C*
     C     WKD(1)        IFGE      ELPTDT
     C                   Z-ADD     ELBSDT        WKD(1)
     C                   ELSE
     C*
     C* BUILD DATE ARRAY WITH EACH TWICE MONTHLY INSTALLMENT DATE.
     C*
     C     XXWKDT        DOWLT     ELPTDT
     C*
     C     WDD           IFEQ      1
     C                   Z-ADD     15            WDD
     C                   ELSE
     C*
     C                   Z-ADD     1             WDD
     C                   ADD       1             WMM
     C     WMM           IFEQ      13
     C                   Z-ADD     1             WMM
     C                   ADD       1             WYY
     C                   ENDIF
     C*
     C                   ENDIF                                                  WDD IFEQ
     C*
     C     XXWKDT        IFLT      ELPTDT
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
     C     X1ANPR        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKA(U)
     C                   ENDDO
     C*
     C                   XFOOT     WKA           XXCOMP           15 4
     C     XXCOMP        IFNE      X1ANPR
     C     X1ANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKA(1)
     C                   ENDIF
     C*
     C* STATE TAX
     C*
     C     ELTXDU        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKT(U)
     C                   ENDDO
     C*
     C                   XFOOT     WKT           XXCOMP           15 4
     C     XXCOMP        IFNE      ELTXDU
     C     ELTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKT(1)
     C                   ENDIF
     C     XX1STB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK1(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK1           XXCOMP           15 4
     C     XXCOMP        IFNE      XX1STB
     C     XX1STB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK1(1)
     C                   ENDIF
     C*
     C     XX2NDB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK2(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK2           XXCOMP           15 4
     C     XXCOMP        IFNE      XX2NDB
     C     XX2NDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK2(1)
     C                   ENDIF
     C     XX3RDB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK3(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK3           XXCOMP           15 4
     C     XXCOMP        IFNE      XX3RDB
     C     XX3RDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK3(1)
     C                   ENDIF
     C*
     C     XX4THB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK4(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK4           XXCOMP           15 4
     C     XXCOMP        IFNE      XX4THB
     C     XX4THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK4(1)
     C                   ENDIF
     C*
     C     XX5THB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK5(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK5           XXCOMP           15 4
     C     XXCOMP        IFNE      XX5THB
     C     XX5THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK5(1)
     C                   ENDIF
     C*
     C     XX6THB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK6(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK6           XXCOMP           15 4
     C     XXCOMP        IFNE      XX6THB
     C     XX6THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK6(1)
     C                   ENDIF
     C*
     C     XX7THB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK7(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK7           XXCOMP           15 4
     C     XXCOMP        IFNE      XX7THB
     C     XX7THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK7(1)
     C                   ENDIF
     C*
     C* BILL INSTALLMENTS INTO A/R, CATCHING UP TO CURRENT DATE
     C* IF NECESSARY.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* CALCULATE A/R ADJUSTMENTS.
     C*
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C                   ADD       XU1STB        WK1(1)
     C                   ADD       XU2NDB        WK2(1)
     C                   ADD       XU3RDB        WK3(1)
     C                   ADD       XU4THB        WK4(1)
     C                   ADD       XU5THB        WK5(1)
     C                   ADD       XU6THB        WK6(1)
     C                   ADD       XU7THB        WK7(1)
     C                   Z-ADD     0             Y
     C                   Z-ADD     0             Z
     C*
     C     1             DO        53            U
     C*
     C     WKA(U)        IFNE      *ZERO
     C*
     C* CATCH UP TO NEXT INSTALLMENT DUE.
     C*
     C     WKD(U)        IFLT      XXCURD
     C                   ADD       WKA(U)        XXAMT2
     C                   ADD       WKT(U)        X1AMT2
     C                   ADD       WK1(U)        X1AA2
     C                   ADD       WK2(U)        X2AA2
     C                   ADD       WK3(U)        X3AA2
     C                   ADD       WK4(U)        X4AA2
     C                   ADD       WK5(U)        X5AA2
     C                   ADD       WK6(U)        X6AA2
     C                   ADD       WK7(U)        X7AA2
     C                   ELSE
     C*
     C     Y             IFLT      2
     C*
     C                   ADD       1             Y
     C     Y             IFEQ      2
     C                   ADD       1             Z
     C                   ENDIF
     C     WKA(U)        ADD       XXAMT2        PA(Y)
     C     WKT(U)        ADD       X1AMT2        P1(Y)
     C     WK1(U)        ADD       X1AA2         A1(Y)
     C     WK2(U)        ADD       X2AA2         A2(Y)
     C     WK3(U)        ADD       X3AA2         A3(Y)
     C     WK4(U)        ADD       X4AA2         A4(Y)
     C     WK5(U)        ADD       X5AA2         A5(Y)
     C     WK6(U)        ADD       X6AA2         A6(Y)
     C     WK7(U)        ADD       X7AA2         A7(Y)
     C     XXAMT2        IFNE      *ZERO
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X6AA2
     C                   ENDIF
     C*
     C* SET DATE RANGE FOR PRIOR BILLED A/R.
     C*
     C                   Z-ADD     WKD(U)        XXDTE1
     C     U             ADD       1             T
     C                   Z-ADD     WKD(T)        XXDTE2
     C*
     C* IF CHANGING BSCD, DO NOT INCLUDE NEXT INSTALLMENT IN THE CATCH
     C* UP PRIOR A/R SINCE IT WILL BE DELETED.
     C*
     C     ELBSCD        IFNE      XXBSCD
     C     Y             ANDEQ     1
     C                   Z-ADD     XXCURD        XXDTE2
     C                   ENDIF                                                  ELBSCD IFNE
     C*
     C                   EXSR      SRWASB
     C*
     C* BILL FULL AMOUNT IF CHANGING BILLING SCHEDULE CODE.
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C     Y             OREQ      1
     C                   SUB       XXAMT1        PA(Y)
     C                   SUB       X1AMT1        P1(Y)
     C                   SUB       X1AA1         A1(Y)
     C                   SUB       X2AA1         A2(Y)
     C                   SUB       X3AA1         A3(Y)
     C                   SUB       X4AA1         A4(Y)
     C                   SUB       X5AA1         A5(Y)
     C                   SUB       X6AA1         A6(Y)
     C                   SUB       X7AA1         A7(Y)
     C                   ENDIF                                                  ELBSCD IFEQ
     C*
     C                   Z-ADD     WKD(U)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(Y)
     C*
     C                   ELSE
     C                   ADD       1             Z
     C                   ENDIF                                                  Y IFLT 2
     C*
     C                   ENDIF                                                  WKD,U IFLT
     C*
     C                   ENDIF                                                  WKA,U IFNE
     C*
     C                   ENDDO                                                  1 DO 53
     C*
     C                   MOVE      *ON           *IN62
     C                   MOVE      *ON           *IN64
     C*
     C     PA(1)         IFEQ      *ZERO
     C     P1(1)         ANDEQ     *ZERO
     C     A1(1)         ANDEQ     *ZERO
     C     A2(1)         ANDEQ     *ZERO
     C     A3(1)         ANDEQ     *ZERO
     C     A4(1)         ANDEQ     *ZERO
     C     A5(1)         ANDEQ     *ZERO
     C     A6(1)         ANDEQ     *ZERO
     C     A7(1)         ANDEQ     *ZERO
     C*
     C     PA(2)         IFNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C     A6(2)         ANDEQ     *ZERO
     C     A7(2)         ANDEQ     *ZERO
     C                   Z-ADD     PA(2)         PA(1)
     C                   Z-ADD     P1(2)         P1(1)
     C                   Z-ADD     A1(2)         A1(1)
     C                   Z-ADD     A2(2)         A2(1)
     C                   Z-ADD     A3(2)         A3(1)
     C                   Z-ADD     A4(2)         A4(1)
     C                   Z-ADD     A5(2)         A5(1)
     C                   Z-ADD     A6(2)         A6(1)
     C                   Z-ADD     A7(2)         A7(1)
     C                   Z-ADD     DA(2)         DA(1)
     C                   SUB       1             Z
     C*
     C     Z             IFGT      0
     C                   Z-ADD     1             U
     C                   Z-ADD     DA(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2DATE            7 0
     C     X2DATE        LOOKUP    WKD(U)                             60
     C                   Z-ADD     WKD(U)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(2)
     C                   ELSE
     C                   Z-ADD     0             PA(2)
     C                   Z-ADD     0             P1(2)
     C                   Z-ADD     0             A1(2)
     C                   Z-ADD     0             A2(2)
     C                   Z-ADD     0             A3(2)
     C                   Z-ADD     0             A4(2)
     C                   Z-ADD     0             A5(2)
     C                   Z-ADD     0             A6(2)
     C                   Z-ADD     0             A7(2)
     C                   Z-ADD     0             DA(2)
     C                   ENDIF                                                  Z IFGT
     C*
     C                   ENDIF                                                  PA,2 IFNE
     C*
     C                   ENDIF                                                  PA,1 IFEQ
     C*
     C*
     C     PA(1)         IFNE      *ZERO
     C     PA(2)         ORNE      *ZERO
     C     P1(1)         ORNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C     A1(1)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
     C     A2(1)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(1)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(1)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(1)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C     A6(1)         ORNE      *ZERO
     C     A6(2)         ORNE      *ZERO
     C     A7(1)         ORNE      *ZERO
     C     A7(2)         ORNE      *ZERO
     C*
     C                   Z-ADD     2             X
     C                   ENDIF
     C*
     C                   ELSE                                                   BSFREQ IFEQ
     C*
     C*
     C*  PROCESS WEEKLY BILLING SCHEDULE CODES.
     C*
     C                   Z-ADD     0             WKD
     C                   Z-ADD     0             WKA
     C                   Z-ADD     0             WKT
     C                   Z-ADD     0             WK1
     C                   Z-ADD     0             WK2
     C                   Z-ADD     0             WK3
     C                   Z-ADD     0             WK4
     C                   Z-ADD     0             WK5
     C                   Z-ADD     0             WK6
     C                   Z-ADD     0             WK7
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   MOVE      XQFROM        QQFROM            8 0
     C                   MOVE      XQTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
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
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTE
     C                   MOVE      *BLANK        SRSDAY
     C                   MOVE      *BLANK        SRLDAY
     C                   MOVE      *BLANK        SRERR
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
     C                   Z-ADD     ELBSDT        WKD(1)
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
     C     WKD(1)        IFGE      ELPTDT
     C                   Z-ADD     ELBSDT        WKD(1)
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
     C     X1ANPR        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKA(U)
     C                   ENDDO
     C*
     C                   XFOOT     WKA           XXCOMP           15 4
     C     XXCOMP        IFNE      X1ANPR
     C     X1ANPR        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKA(1)
     C                   ENDIF
     C*
     C* WEEKLY STATE TAX
     C*
     C     ELTXDU        DIV(H)    T             XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WKT(U)
     C                   ENDDO
     C*
     C                   XFOOT     WKT           XXCOMP           15 4
     C     XXCOMP        IFNE      ELTXDU
     C     ELTXDU        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WKT(1)
     C                   ENDIF
     C     XX1STB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK1(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK1           XXCOMP           15 4
     C     XXCOMP        IFNE      XX1STB
     C     XX1STB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK1(1)
     C                   ENDIF
     C*
     C     XX2NDB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK2(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK2           XXCOMP           15 4
     C     XXCOMP        IFNE      XX2NDB
     C     XX2NDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK2(1)
     C                   ENDIF
     C*
     C     XX3RDB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK3(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK3           XXCOMP           15 4
     C     XXCOMP        IFNE      XX3RDB
     C     XX3RDB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK3(1)
     C                   ENDIF
     C*
     C     XX4THB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK4(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK4           XXCOMP           15 4
     C     XXCOMP        IFNE      XX4THB
     C     XX4THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK4(1)
     C                   ENDIF
     C*
     C     XX5THB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK5(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK5           XXCOMP           15 4
     C     XXCOMP        IFNE      XX5THB
     C     XX5THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK5(1)
     C                   ENDIF
     C*
     C     XX6THB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK6(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK6           XXCOMP           15 4
     C     XXCOMP        IFNE      XX6THB
     C     XX6THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK4(1)
     C                   ENDIF
     C*
     C     XX7THB        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C     1             DO        T             U                 3 0
     C                   Z-ADD     XXAMT2        WK7(U)
     C                   ENDDO
     C*
     C                   XFOOT     WK7           XXCOMP           15 4
     C     XXCOMP        IFNE      XX7THB
     C     XX7THB        SUB       XXCOMP        XXDIF            15 4
     C                   ADD       XXDIF         WK7(1)
     C                   ENDIF
     C*
     C* BILL WEEKLY INSTALLMENTS INTO A/R, CATCHING UP TO CURRENT DATE
     C* IF NECESSARY.
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C* POSTING DATE (REVISED INTERIM DAYS PLUS THE PRESENT DATE)
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCURD
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C* CALCULATE A/R ADJUSTMENTS.
     C*
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C                   ADD       XU1STB        WK1(1)
     C                   ADD       XU2NDB        WK2(1)
     C                   ADD       XU3RDB        WK3(1)
     C                   ADD       XU4THB        WK4(1)
     C                   ADD       XU5THB        WK5(1)
     C                   ADD       XU6THB        WK6(1)
     C                   ADD       XU7THB        WK7(1)
     C                   Z-ADD     0             Y
     C                   Z-ADD     0             Z
     C*
     C     1             DO        53            U
     C*
     C     WKA(U)        IFNE      *ZERO
     C*
     C* CATCH UP TO NEXT INSTALLMENT DUE.
     C*
     C     WKD(U)        IFLT      XXCURD
     C                   ADD       WKA(U)        XXAMT2
     C                   ADD       WKT(U)        X1AMT2
     C                   ADD       WK1(U)        X1AA2
     C                   ADD       WK2(U)        X2AA2
     C                   ADD       WK3(U)        X3AA2
     C                   ADD       WK4(U)        X4AA2
     C                   ADD       WK5(U)        X5AA2
     C                   ADD       WK6(U)        X6AA2
     C                   ADD       WK7(U)        X7AA2
     C                   ELSE
     C*
     C     Y             IFLT      2
     C*
     C                   ADD       1             Y
     C     Y             IFEQ      2
     C                   ADD       1             Z
     C                   ENDIF
     C     WKA(U)        ADD       XXAMT2        PA(Y)
     C     WKT(U)        ADD       X1AMT2        P1(Y)
     C     WK1(U)        ADD       X1AA2         A1(Y)
     C     WK2(U)        ADD       X2AA2         A2(Y)
     C     WK3(U)        ADD       X3AA2         A3(Y)
     C     WK4(U)        ADD       X4AA2         A4(Y)
     C     WK5(U)        ADD       X5AA2         A5(Y)
     C     WK6(U)        ADD       X6AA2         A6(Y)
     C     WK7(U)        ADD       X7AA2         A7(Y)
     C     XXAMT2        IFNE      *ZERO
     C                   Z-ADD     0             XXAMT2
     C                   Z-ADD     0             X1AMT2
     C                   Z-ADD     0             X1AA2
     C                   Z-ADD     0             X2AA2
     C                   Z-ADD     0             X3AA2
     C                   Z-ADD     0             X4AA2
     C                   Z-ADD     0             X5AA2
     C                   Z-ADD     0             X6AA2
     C                   Z-ADD     0             X7AA2
     C                   ENDIF
     C*
     C* SET DATE RANGE FOR PRIOR BILLED A/R.
     C*
     C                   Z-ADD     WKD(U)        XXDTE1
     C     U             ADD       1             T
     C                   Z-ADD     WKD(T)        XXDTE2
     C*
     C* IF CHANGING BSCD, DO NOT INCLUDE NEXT INSTALLMENT IN THE CATCH
     C* UP PRIOR A/R SINCE IT WILL BE DELETED.
     C*
     C     ELBSCD        IFNE      XXBSCD
     C     Y             ANDEQ     1
     C                   Z-ADD     XXCURD        XXDTE2
     C                   ENDIF                                                  ELBSCD IFNE
     C*
     C                   EXSR      SRWASB
     C*
     C* BILL FULL WEEKLY AMOUNT IF CHANGING BILLING SCHEDULE CODE.
     C*
     C     ELBSCD        IFEQ      XXBSCD
     C     Y             OREQ      1
     C                   SUB       XXAMT1        PA(Y)
     C                   SUB       X1AMT1        P1(Y)
     C                   SUB       X1AA1         A1(Y)
     C                   SUB       X2AA1         A2(Y)
     C                   SUB       X3AA1         A3(Y)
     C                   SUB       X4AA1         A4(Y)
     C                   SUB       X5AA1         A5(Y)
     C                   SUB       X6AA1         A6(Y)
     C                   SUB       X7AA1         A7(Y)
     C                   ENDIF                                                  ELBSCD IFEQ
     C*
     C                   Z-ADD     WKD(U)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(Y)
     C*
     C                   ELSE
     C                   ADD       1             Z
     C                   ENDIF                                                  Y IFLT 2
     C*
     C                   ENDIF                                                  WKD,U IFLT
     C*
     C                   ENDIF                                                  WKA,U IFNE
     C*
     C                   ENDDO                                                  1 DO 53
     C*
     C                   MOVE      *ON           *IN62
     C*
     C     PA(1)         IFEQ      *ZERO
     C     P1(1)         ANDEQ     *ZERO
     C     A1(1)         ANDEQ     *ZERO
     C     A2(1)         ANDEQ     *ZERO
     C     A3(1)         ANDEQ     *ZERO
     C     A4(1)         ANDEQ     *ZERO
     C     A5(1)         ANDEQ     *ZERO
     C     A6(1)         ANDEQ     *ZERO
     C     A7(1)         ANDEQ     *ZERO
     C*
     C     PA(2)         IFNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C     A6(2)         ORNE      *ZERO
     C     A7(2)         ORNE      *ZERO
     C                   Z-ADD     PA(2)         PA(1)
     C                   Z-ADD     P1(2)         P1(1)
     C                   Z-ADD     A1(2)         A1(1)
     C                   Z-ADD     A2(2)         A2(1)
     C                   Z-ADD     A3(2)         A3(1)
     C                   Z-ADD     A4(2)         A4(1)
     C                   Z-ADD     A5(2)         A5(1)
     C                   Z-ADD     A6(2)         A6(1)
     C                   Z-ADD     A7(2)         A7(1)
     C                   Z-ADD     DA(2)         DA(1)
     C                   SUB       1             Z
     C*
     C     Z             IFGT      0
     C                   Z-ADD     1             U
     C                   Z-ADD     DA(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2DATE            7 0
     C     X2DATE        LOOKUP    WKD(U)                             60
     C                   Z-ADD     WKD(U)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         DA(2)
     C                   ELSE
     C                   Z-ADD     0             PA(2)
     C                   Z-ADD     0             P1(2)
     C                   Z-ADD     0             A1(2)
     C                   Z-ADD     0             A2(2)
     C                   Z-ADD     0             A3(2)
     C                   Z-ADD     0             A4(2)
     C                   Z-ADD     0             A5(2)
     C                   Z-ADD     0             A6(2)
     C                   Z-ADD     0             A7(2)
     C                   Z-ADD     0             DA(2)
     C                   ENDIF                                                  Z IFGT
     C*
     C                   ENDIF                                                  PA,2 IFNE
     C*
     C                   ENDIF                                                  PA,1 IFEQ
     C*
     C*
     C     PA(1)         IFNE      *ZERO
     C     PA(2)         ORNE      *ZERO
     C     P1(1)         ORNE      *ZERO
     C     P1(2)         ORNE      *ZERO
     C     A1(1)         ORNE      *ZERO
     C     A1(2)         ORNE      *ZERO
     C     A2(1)         ORNE      *ZERO
     C     A2(2)         ORNE      *ZERO
     C     A3(1)         ORNE      *ZERO
     C     A3(2)         ORNE      *ZERO
     C     A4(1)         ORNE      *ZERO
     C     A4(2)         ORNE      *ZERO
     C     A5(1)         ORNE      *ZERO
     C     A5(2)         ORNE      *ZERO
     C     A6(1)         ORNE      *ZERO
     C     A6(2)         ORNE      *ZERO
     C     A7(1)         ORNE      *ZERO
     C     A7(2)         ORNE      *ZERO
     C*
     C                   Z-ADD     2             X
     C                   ENDIF
     C*
     C                   ENDIF                                                  BSFREQ IFEQ
     C*
     C                   ENDIF                                                  BSDOWK IFEQ
     C*
     C*  TEST TO SEE IF ANY ADJUSTMENTS ARE BEING POSTED
     C*
     C                   XFOOT     PA            SCTOT
     C*
     C*  ADD THE STATE TAX ADJUSTMENTS TO TOTAL ADJUSTMENT
     C*
     C                   XFOOT     P1            XXTEST           11 2
     C                   ADD       XXTEST        SCTOT
     C                   MOVE      '0'           *IN80
     C*
     C                   XFOOT     A1            SCTOT2           11 2
     C                   XFOOT     A2            XXTEST
     C*
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A3            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A4            SCTOT3           11 2
     C*
     C                   XFOOT     A5            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A6            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A7            SCTOT4
     C*
     C     *IN62         IFEQ      *ON
     C*
     C     PA(2)         MULT      Z             SCTOT
     C                   ADD       PA(1)         SCTOT
     C     P1(2)         MULT      Z             XXTEST
     C                   ADD       P1(1)         XXTEST
     C                   ADD       XXTEST        SCTOT
     C*
     C     A1(2)         MULT      Z             SCTOT2
     C                   ADD       A1(1)         SCTOT2
     C     A2(2)         MULT      Z             XXTEST
     C                   ADD       A2(1)         XXTEST
     C                   ADD       XXTEST        SCTOT2
     C     A3(2)         MULT      Z             XXTEST
     C                   ADD       A3(1)         XXTEST
     C                   ADD       XXTEST        SCTOT2
     C     A4(2)         MULT      Z             SCTOT3
     C                   ADD       A4(1)         SCTOT3
     C     A5(2)         MULT      Z             XXTEST
     C                   ADD       A5(1)         XXTEST
     C                   ADD       XXTEST        SCTOT3
     C     A6(2)         MULT      Z             XXTEST
     C                   ADD       A6(1)         XXTEST
     C                   ADD       XXTEST        SCTOT3
     C     A7(2)         MULT      Z             XXTEST
     C                   ADD       A7(1)         XXTEST
     C                   ADD       XXTEST        SCTOT4
     C*
     C*
     C     Z             IFEQ      0
     C                   MOVE      *ON           *IN63
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN62 IFEQ
     C*
     C                   MOVE      '0'           *IN80
     C*
     C                   Z-ADD     12            X
     C     X             DOWGT     0
     C     PA(X)         IFNE      *ZERO
     C     P1(X)         ORNE      *ZERO
     C     A1(X)         ORNE      *ZERO
     C     A2(X)         ORNE      *ZERO
     C     A3(X)         ORNE      *ZERO
     C     A4(X)         ORNE      *ZERO
     C     A5(X)         ORNE      *ZERO
     C     A6(X)         ORNE      *ZERO
     C     A7(X)         ORNE      *ZERO
     C                   LEAVE
     C                   ELSE
     C                   Z-ADD     0             DA(X)
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
     C     X             IFEQ      *ZEROS
     C     F1UPAR        OREQ      'N'
     C*
     C*  DUE TO CHANGE MADE ON 01/19/92, OVERRIDE RECORDS WILL ALWAYS EXIST
     C*  AT THIS POINT IN WAELP AND WAEPP WHETHER OR NOT ANY REVISIONS WERE
     C*  MADE THEREFORE WE NEED TO SUBMIT A BILLING REVISION TO PROCESS THESE
     C*  RECORDS AND MOVE THEM TO LIVE.  IN ADDITION, IF NO ADJUSTMENT RECORDS
     C*  EXIST AND THERE WAS A BILLING SCHEDULE CHANGE WE STILL NEED TO SUBMIT
     C*  THE BILING REVISION TO DELETE ALL FUTURE BILLINGS AND ADJUSTMENTS
     C*
     C                   MOVE      '1'           *IN80
     C                   MOVEA     *ZEROS        DA
     C                   MOVEA     *ZEROS        PA
     C                   MOVEA     *ZEROS        P1
     C                   MOVEA     *ZEROS        A1
     C                   MOVEA     *ZEROS        A2
     C                   MOVEA     *ZEROS        A3
     C                   MOVEA     *ZEROS        A4
     C                   MOVEA     *ZEROS        A5
     C                   MOVEA     *ZEROS        A6
     C                   MOVEA     *ZEROS        A7
     C                   MOVE      *BLANKS       QQAR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCEFDT            8 0
     C*
     C                   END                                                    IF X = 0 OR
     C*                                                    F1UPAR = 'N'
     C*
     C                   END                                                    BSSEFL = 'E'
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C                   ENDSR                                                  CMD7
     C*
     C*================================================================
     C*   SUBROUTINE LDOVRF
     C*   LOAD DATA TO OVERRIDE FILES
     C*================================================================
     C*
     C     LDOVRF        BEGSR
     C*
     C     EPKY1         CHAIN     PEP02                              60
     C     *IN60         IFEQ      '1'
     C     XXOPN         IFEQ      '0'
     C                   OPEN      WMEPL
     C                   MOVE      '1'           XXOPN
     C                   END
     C     EPKY1         CHAIN     PEP01                              61
     C     *IN61         DOWEQ     '0'
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C                   Z-ADD     DSTIME        EPTIME
     C  N61              WRITE     PEP02
     C     EPKY1         READE     PEP01                                  61
     C                   END
     C     XXOPN         IFEQ      '1'
     C                   CLOSE     WMEPL
     C                   MOVE      '0'           XXOPN             1
     C                   END
     C                   END
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C     *IN60         IFEQ      '1'
     C     ELKEY         CHAIN     PEL01                              61
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C*
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C                   MOVE      XXRTCD        ELRTCD
     C*
     C  N61              WRITE     PEL02
     C                   END
     C*
     C*  EXECUTE OVERRIDE STATEMENTS
     C*
     C                   EXSR      BGNOVR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*   SUBROUTINE BGNOVR
     C*   EXECUTES THE OVERRIDE STATEMENTS
     C*================================================================
     C*
     C     BGNOVR        BEGSR
     C*
     C*  OVERRIDE DATABASE FILE WDELL TO WAELL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDELP TO WAELP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(9)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDELL1 TO WAELL1
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(15)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL1 TO WAEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(2)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL6 TO WAEPL6
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(13)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPL TO WAEPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WMEPP TO WAEPP
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(10)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDDSL TO WADSL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(7)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDILL TO WAILL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(17)
     C                   PARM                    LENGTH           15 5
     C*
     C*  OVERRIDE DATABASE FILE WDMPL TO WAMPL
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(19)
     C                   PARM                    LENGTH           15 5
     C*
     C*  Override HCD Program File (WDHCL)
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(21)
     C                   PARM      55            LENGTH           15 5
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*   SUBROUTINE BILL
     C*   CALLS THE BILL WITH THE OVERRIDES AND THEN DELETES THE
     C*   OVERRIDES
     C*================================================================
     C*
     C     BILL          BEGSR
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR             3
     C                   MOVE      XXEMP#        QQEMP#            9
     C                   MOVE      XXDIV         QQDIV             5
     C*
     C                   MOVE      DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         WKDATE            8 0
     C*
     C     FNKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* REVISED INTERIM DAYS PLUS THE PRESENT DATE
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM      DSDATE        QQDT              8 0
     C                   PARM      EXRVID        QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   MOVE      QQDT          WKDATE
     C                   ENDIF
     C     XXACDT        IFNE      *ZERO
     C                   MOVE      XXACDT        WKDATE
     C                   ENDIF                                                  XXACDT IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVE      WKDATE        QQDATE            8
     C                   MOVE      *BLANKS       QQESCD            2
     C                   MOVE      'Y'           QQOVR             1
     C                   MOVE      *BLANKS       QQSRCD            2
     C                   MOVE      *BLANKS       QQPRT             1
     C                   MOVE      *BLANKS       QQAJCD            2
     C                   MOVE      'W'           QQQTE             1
     C                   MOVE      ' '           QQRNB1            1
     C                   CALL      'W8010R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQDATE
     C                   PARM                    QQESCD
     C                   PARM                    QQOVR
     C                   PARM                    QQSRCD
     C                   PARM                    QQPRT
     C                   PARM                    QQAJCD
     C                   PARM                    QQQTE
     C                   PARM                    QQRNB1
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*   SUBROUTINE DLTOVR
     C*   DELETES ALL OVERRIDES
     C*================================================================
     C*
     C     DLTOVR        BEGSR
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(5)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(6)
     C                   PARM                    LENGTH           15 5
     C                   MOVE      '0'           *IN86
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(8)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(18)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(20)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(11)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(12)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(14)
     C                   PARM                    LENGTH           15 5
     C*
     C*  DELETE OVERRIDES
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(16)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(22)
     C                   PARM      55            LENGTH           15 5
     C*
     C                   ENDSR
     C*
      *****************************************************************
     C*
     C* UPDATE THE A/R FILE AND CALL W8011C
     C*
     C     UPDTE         BEGSR
     C*
     C*  WRITE TRANSACTION RECORD AND SUBMIT BATCH UPDATE
     C*
     C     *IN80         IFEQ      *OFF
     C                   Z-ADD     DA(1)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   Z-ADD     DA(2)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO02
     C*
     C                   Z-ADD     DA(3)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO03
     C*
     C                   Z-ADD     DA(4)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO04
     C*
     C                   Z-ADD     DA(5)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO05
     C*
     C                   Z-ADD     DA(6)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO06
     C*
     C                   Z-ADD     DA(7)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO07
     C*
     C                   Z-ADD     DA(8)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO08
     C*
     C                   Z-ADD     DA(9)         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO09
     C*
     C                   Z-ADD     DA(10)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO10
     C*
     C                   Z-ADD     DA(11)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO11
     C*
     C                   Z-ADD     DA(12)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO12
     C*
     C                   Z-ADD     PA(1)         EAAM01
     C                   Z-ADD     PA(2)         EAAM02
     C                   Z-ADD     PA(3)         EAAM03
     C                   Z-ADD     PA(4)         EAAM04
     C                   Z-ADD     PA(5)         EAAM05
     C                   Z-ADD     PA(6)         EAAM06
     C                   Z-ADD     PA(7)         EAAM07
     C                   Z-ADD     PA(8)         EAAM08
     C                   Z-ADD     PA(9)         EAAM09
     C                   Z-ADD     PA(10)        EAAM10
     C                   Z-ADD     PA(11)        EAAM11
     C                   Z-ADD     PA(12)        EAAM12
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C*
     C     BSSEFL        IFEQ      'E'
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'P'
     C     TTTYPE        ANDEQ     'A'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   ELSE
     C                   MOVEL(P)  EXDACD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   ENDIF                                                  BSSEFL IFEQ
     C*
     C                   MOVE      EAARCD        QQAR
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C*
     C                   MOVE      ELCO#         EACO#
     C                   MOVE      ELFND         EAFND
     C                   Z-ADD     ELFYR         EAFYR
     C                   MOVE      ELEMP#        EAEMP#
     C                   MOVE      ELDIV         EADIV
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DSTIME        EATIME
     C                   WRITE     PEA02
     C*
     C*  GET THE STATE TAX A/R TYPE
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'T'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     TTCTYP        OREQ      'D'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'S'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C                   Z-ADD     P1(1)         EAAM01
     C                   Z-ADD     P1(2)         EAAM02
     C                   Z-ADD     P1(3)         EAAM03
     C                   Z-ADD     P1(4)         EAAM04
     C                   Z-ADD     P1(5)         EAAM05
     C                   Z-ADD     P1(6)         EAAM06
     C                   Z-ADD     P1(7)         EAAM07
     C                   Z-ADD     P1(8)         EAAM08
     C                   Z-ADD     P1(9)         EAAM09
     C                   Z-ADD     P1(10)        EAAM10
     C                   Z-ADD     P1(11)        EAAM11
     C                   Z-ADD     P1(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
     C                   WRITE     PEA02
     C                   Z-ADD     EAPO01        SVDT7             7 0
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '1'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '1'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     EAARCD        IFNE      *BLANKS
     C                   Z-ADD     A1(1)         EAAM01
     C                   Z-ADD     A1(2)         EAAM02
     C                   Z-ADD     A1(3)         EAAM03
     C                   Z-ADD     A1(4)         EAAM04
     C                   Z-ADD     A1(5)         EAAM05
     C                   Z-ADD     A1(6)         EAAM06
     C                   Z-ADD     A1(7)         EAAM07
     C                   Z-ADD     A1(8)         EAAM08
     C                   Z-ADD     A1(9)         EAAM09
     C                   Z-ADD     A1(10)        EAAM10
     C                   Z-ADD     A1(11)        EAAM11
     C                   Z-ADD     A1(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A1(13)        IFNE      *ZERO
     C                   Z-ADD     A1(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EAARCD IFNE
     C*
     C*  GET THE STATE TAX A/R TYPE-SECOND ASSESSMENT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '2'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '2'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     EAARCD        IFNE      *BLANKS
     C                   Z-ADD     A2(1)         EAAM01
     C                   Z-ADD     A2(2)         EAAM02
     C                   Z-ADD     A2(3)         EAAM03
     C                   Z-ADD     A2(4)         EAAM04
     C                   Z-ADD     A2(5)         EAAM05
     C                   Z-ADD     A2(6)         EAAM06
     C                   Z-ADD     A2(7)         EAAM07
     C                   Z-ADD     A2(8)         EAAM08
     C                   Z-ADD     A2(9)         EAAM09
     C                   Z-ADD     A2(10)        EAAM10
     C                   Z-ADD     A2(11)        EAAM11
     C                   Z-ADD     A2(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A2(13)        IFNE      *ZERO
     C                   Z-ADD     A2(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EAARCD IFNE
     C*  GET THE STATE TAX A/R TYPE-THIRD ASSESSMENT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '3'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '3'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     EAARCD        IFNE      *BLANKS
     C                   Z-ADD     A3(1)         EAAM01
     C                   Z-ADD     A3(2)         EAAM02
     C                   Z-ADD     A3(3)         EAAM03
     C                   Z-ADD     A3(4)         EAAM04
     C                   Z-ADD     A3(5)         EAAM05
     C                   Z-ADD     A3(6)         EAAM06
     C                   Z-ADD     A3(7)         EAAM07
     C                   Z-ADD     A3(8)         EAAM08
     C                   Z-ADD     A3(9)         EAAM09
     C                   Z-ADD     A3(10)        EAAM10
     C                   Z-ADD     A3(11)        EAAM11
     C                   Z-ADD     A3(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A3(13)        IFNE      *ZERO
     C                   Z-ADD     A3(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EAARCD IFNE
     C*  GET THE STATE TAX A/R TYPE-FOURTH ASSESSMENT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '4'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '4'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     EAARCD        IFNE      *BLANKS
     C                   Z-ADD     A4(1)         EAAM01
     C                   Z-ADD     A4(2)         EAAM02
     C                   Z-ADD     A4(3)         EAAM03
     C                   Z-ADD     A4(4)         EAAM04
     C                   Z-ADD     A4(5)         EAAM05
     C                   Z-ADD     A4(6)         EAAM06
     C                   Z-ADD     A4(7)         EAAM07
     C                   Z-ADD     A4(8)         EAAM08
     C                   Z-ADD     A4(9)         EAAM09
     C                   Z-ADD     A4(10)        EAAM10
     C                   Z-ADD     A4(11)        EAAM11
     C                   Z-ADD     A4(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A4(13)        IFNE      *ZERO
     C                   Z-ADD     A4(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EAARCD IFNE
     C*  GET THE STATE TAX A/R TYPE-FIFTH ASSESSMENT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '5'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '5'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WRITE A RECORD FOR THE STATE TAX ADJUSTMENT
     C*
     C     EAARCD        IFNE      *BLANKS
     C                   Z-ADD     A5(1)         EAAM01
     C                   Z-ADD     A5(2)         EAAM02
     C                   Z-ADD     A5(3)         EAAM03
     C                   Z-ADD     A5(4)         EAAM04
     C                   Z-ADD     A5(5)         EAAM05
     C                   Z-ADD     A5(6)         EAAM06
     C                   Z-ADD     A5(7)         EAAM07
     C                   Z-ADD     A5(8)         EAAM08
     C                   Z-ADD     A5(9)         EAAM09
     C                   Z-ADD     A5(10)        EAAM10
     C                   Z-ADD     A5(11)        EAAM11
     C                   Z-ADD     A5(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A5(13)        IFNE      *ZERO
     C                   Z-ADD     A5(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EAARCD IFNE
     C*  GET THE A/R TYPE - SIXTH ASSESSMENT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '6'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '6'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WRITE A RECORD FOR THE SIXTH ASSESSMENT ADJUSTMENT
     C*
     C     EAARCD        IFNE      *BLANKS
     C                   Z-ADD     A6(1)         EAAM01
     C                   Z-ADD     A6(2)         EAAM02
     C                   Z-ADD     A6(3)         EAAM03
     C                   Z-ADD     A6(4)         EAAM04
     C                   Z-ADD     A6(5)         EAAM05
     C                   Z-ADD     A6(6)         EAAM06
     C                   Z-ADD     A6(7)         EAAM07
     C                   Z-ADD     A6(8)         EAAM08
     C                   Z-ADD     A6(9)         EAAM09
     C                   Z-ADD     A6(10)        EAAM10
     C                   Z-ADD     A6(11)        EAAM11
     C                   Z-ADD     A6(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A6(13)        IFNE      *ZERO
     C                   Z-ADD     A6(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EAARCD IFNE
     C*
     C*  GET THE A/R TYPE - SEVENTH ASSESSMENT
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '7'
     C     TTTYPE        ANDEQ     'F'
     C     BSSEFL        ANDEQ     'E'
     C     CLTID         ANDNE     '139'
     C     TTCTYP        OREQ      '7'
     C     TTTYPE        ANDEQ     'F'
     C     CLTID         ANDEQ     '139'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WRITE A RECORD FOR THE SEVENTH ASSESSMENT ADJUSTMENT
     C*
     C     EAARCD        IFNE      *BLANKS
     C                   Z-ADD     A7(1)         EAAM01
     C                   Z-ADD     A7(2)         EAAM02
     C                   Z-ADD     A7(3)         EAAM03
     C                   Z-ADD     A7(4)         EAAM04
     C                   Z-ADD     A7(5)         EAAM05
     C                   Z-ADD     A7(6)         EAAM06
     C                   Z-ADD     A7(7)         EAAM07
     C                   Z-ADD     A7(8)         EAAM08
     C                   Z-ADD     A7(9)         EAAM09
     C                   Z-ADD     A7(10)        EAAM10
     C                   Z-ADD     A7(11)        EAAM11
     C                   Z-ADD     A7(12)        EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C     BSDOWK        IFNE      *BLANKS
     C     BSFREQ        OREQ      'T'
     C                   Z-ADD     Z             EATRDT
     C                   ENDIF
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
     C*
     C                   MOVEL     SRDTO         EADT
     C                   WRITE     PEA02
     C*
     C* WRITE 13TH ADJUSTMENT TO NEW RECORD.
     C*
     C     A7(13)        IFNE      *ZERO
     C                   Z-ADD     A7(13)        EAAM01
     C                   Z-ADD     0             EAAM02
     C                   Z-ADD     0             EAAM03
     C                   Z-ADD     0             EAAM04
     C                   Z-ADD     0             EAAM05
     C                   Z-ADD     0             EAAM06
     C                   Z-ADD     0             EAAM07
     C                   Z-ADD     0             EAAM08
     C                   Z-ADD     0             EAAM09
     C                   Z-ADD     0             EAAM10
     C                   Z-ADD     0             EAAM11
     C                   Z-ADD     0             EAAM12
     C                   ADD       1             EATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C*
     C                   MOVE      *BLANK        EAEDIT
     C*
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DA(13)        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPO01
     C*
     C                   WRITE     PEA02
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EAARCD IFNE
     C                   ENDIF                                                  *IN80 IFEQ
     C                   ENDSR                                                  UPDTE
     C*
     C     CALLU         BEGSR
     C*
     C*  CALL PROGRAM TO UPDATE A/R
     C*
     C                   MOVE      XXCO#         QQCO#             3
     C                   MOVE      XXFND         QQFND             3
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXEMP#        QQEMP#
     C                   MOVE      XXDIV         QQDIV
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         QQDUE             8
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         Q1EFDT            8
     C                   CALL      'W8011R'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQDIV
     C                   PARM                    QQAR              2
     C                   PARM                    Q1EFDT
     C                   PARM      ' '           QQCALL            1
     C*
     C                   ENDSR
     C*================================================================
     C*   SUBROUTINE SRWASB
     C*   THIS SUBROUTINE ACCUMLATES ALL 'B' AND 'A' A/R RECORDS
     C*   WITH POSTING DATES BETWEEN XXDTE1 AND XXDTE2
     C*================================================================
     C*
     C     SRWASB        BEGSR
     C*
     C                   Z-ADD     *ZEROS        XXAMT1
     C                   Z-ADD     *ZEROS        X1AA1
     C                   Z-ADD     *ZEROS        X2AA1
     C                   Z-ADD     *ZEROS        X3AA1
     C                   Z-ADD     *ZEROS        X4AA1
     C                   Z-ADD     *ZEROS        X5AA1
     C                   Z-ADD     *ZEROS        X6AA1
     C                   Z-ADD     *ZEROS        X7AA1
     C     Y             IFEQ      1
     C     XXFUT         ANDEQ     '0'
     C                   MOVE      '1'           XXFUT
     C                   MOVE      '0010101'     XXPODT
     C                   ELSE
     C                   Z-ADD     XXDTE1        XXPODT
     C                   ENDIF                                                  Y EQ 1
     C                   MOVE      'B'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C     *IN50         DOWEQ     '0'
     C     EAPODT        ANDLT     XXDTE2
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C     TTCTYP        ANDNE     'C'
      *** beg add ***** 07/02/13 **************************************
     C     EAARCD        ANDNE     F1DVBL
      *** end add ***** 07/02/13 **************************************
     C                   ADD       EAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C                   END
     C*
     C                   MOVE      'A'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C     *IN50         DOWEQ     '0'
     C     EAPODT        ANDLT     XXDTE2
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'E'
     C     TTCTYP        ANDNE     'C'
     C                   ADD       EAAMT         XXAMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C                   END
     C*
     C*  THIS SECTION ACCUMULATES STATE TAX AND ASSESSMENT BILLINGS
     C*
     C                   Z-ADD     *ZEROS        X1AMT1
     C                   MOVE      'F'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C     *IN50         DOWEQ     '0'
     C     EAPODT        ANDLT     XXDTE2
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C     BSSEFL        ANDEQ     'E'
     C     TTCTYP        OREQ      'D'
     C     BSSEFL        ANDEQ     'S'
     C                   ADD       EAAMT         X1AMT1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   SELECT
     C     TTCTYP        WHENEQ    '1'
     C                   ADD       EAAMT         X1AA1
     C     TTCTYP        WHENEQ    '2'
     C                   ADD       EAAMT         X2AA1
     C     TTCTYP        WHENEQ    '3'
     C                   ADD       EAAMT         X3AA1
     C     TTCTYP        WHENEQ    '4'
     C                   ADD       EAAMT         X4AA1
     C     TTCTYP        WHENEQ    '5'
     C                   ADD       EAAMT         X5AA1
     C     TTCTYP        WHENEQ    '6'
     C                   ADD       EAAMT         X6AA1
     C     TTCTYP        WHENEQ    '7'
     C                   ADD       EAAMT         X7AA1
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C                   END
     C*
     C                   ENDSR
     C*
      *****************************************************************
     C*
     C     RESBM         BEGSR
     C*
     C                   CALL      'W8040C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQRTCD
     C                   PARM                    QQPCT
     C                   PARM                    QQEFDT
     C                   PARM                    QQQUE
     C                   PARM                    QQBCM1
     C                   PARM                    QQBCM2
     C                   PARM                    QQBCM3
     C                   PARM                    QQICM1
     C                   PARM                    QQICM2
     C                   PARM                    QQICM3
     C*
     C                   ENDSR
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
     C*
     C*****************************************************************
     C*** Beg   Add ***  02/08/13  ************************************
     c     DepositAdj    BegSr
     C                   Z-ADD     *ZEROS        XXADJ            11 2
     C                   Z-ADD     *ZEROS        XXADJ2           11 2
     C     F1DPS         IFEQ      'N'
     C     ELENMP        SUB       SVNMPR        XXADJ
     C                   ELSE
     C     F1DPS         IFEQ      'M'
     C     ELMAPR        SUB       SVMAPR        XXADJ
     C                   ELSE
     C     F1DPS         IFEQ      'S'
     C     ELSTPR        SUB       SVSTPR        XXADJ
     C                   ELSE
     C     ELANPR        SUB       ELPRTD        XXAM12           11 2
     C     XXAM12        SUB       S2ANPR        XXADJ
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        XXARPR           11 2
     C*
     C                   MOVE      'H'           XXARTY
     C     EAKY3         CHAIN     PEA03                              50           ALL FYRS
     C     *IN50         DOWEQ     '0'
     C     EAFYR         ANDLE     XXFYR
     C                   ADD       EAAMT         XXARPR
     C     EAKY3         READE     PEA03                                  50
     C                   ENDDO                                                  *IN50
     C*
     C     XXAB01        IFNE      *ZEROS
     C     XXARPR        DIV       XXAB01        XXARPR
     C                   ENDIF
     C*
     C     F1DPS         IFEQ      'N'
     C     SVNMPR        SUB       XXARPR        XXADJ2
     C                   ELSE
     C     F1DPS         IFEQ      'M'
     C     SVMAPR        SUB       XXARPR        XXADJ2
     C                   ELSE
     C     F1DPS         IFEQ      'S'
     C     SVSTPR        SUB       XXARPR        XXADJ2
     C                   ELSE
     C     S2ANPR        SUB       XXARPR        XXADJ2
     C                   ENDIF                                                  F1DPS = 'N'
     C                   ENDIF                                                  F1DPS = 'M'
     C                   ENDIF                                                  F1DPS = 'S'
     C                   ADD       XXADJ2        XXADJ
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD     XXADJ         XXSAVE           11 2
     C     XXADJ         IFLT      *ZERO
     C                   MULT      -1            XXADJ
     C                   ENDIF
     C                   MOVE      XXADJ         XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXADJ
     C                   ENDIF
     C                   MOVE      '00'          XXADJ
     C     XXSAVE        IFLT      *ZERO
     C     XXADJ         MULT      -1            XXADJ
     C                   ENDIF
     C*
     C     XXADJ         MULT(H)   XXAB01        XXWK90            9 0
     C     XXAB01        IFEQ      *ZERO
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  XXAB01 IFEQ
     C                   Z-ADD     XXWK90        SCTOT            11 2
     C                   MOVE      *BLANK        XXTHRS            1
     C     F1BVFL        IFNE      'Y'
     C                   Z-ADD     0             XXADJ
     C                   Z-ADD     0             SCTOT
     C                   MOVE      'Y'           XXTHRS
     C                   ELSE
     C*
     C     XXWK90        IFGE      0
     C*
     C* DO NOT POST ADJ IF LESS THAN DEBIT THRESHOLD
     C*
     C     XXWK90        IFLT      F1DADJ
     C                   Z-ADD     0             XXADJ
     C                   Z-ADD     0             SCTOT
     C                   MOVE      'Y'           XXTHRS
     C                   ENDIF                                                  XXWK90 IFLT
     C*
     C                   ELSE
     C*
     C* DO NOT POST ADJ IF GREATER THAN CREDIT THRESHOLD
     C*
     C     XXWK90        MULT      -1            XXTMP             9 0
     C*
     C     XXTMP         IFLT      F1CADJ
     C                   Z-ADD     0             XXADJ
     C                   Z-ADD     0             SCTOT
     C                   MOVE      'Y'           XXTHRS
     C                   ENDIF                                                  XXTMP IFLT
     C*
     C                   ENDIF                                                  XXWK90 IFGE
     C*
     C                   ENDIF                                                  F1BVFL IFNE
     C                   Z-ADD     SCTOT         XXDEPP           11 2
     C*
     C*  CALCULATE THE STATE TAX ADJUSTMENT
     C*
     C                   Z-ADD     0             XXTXDU           11 2
     C                   MOVE      'F'           XXARTY
     C     EAKY2         CHAIN     PEA01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      'T'
     C     BSSEFL        ANDEQ     'E'
     C     TTCTYP        OREQ      'D'
     C     BSSEFL        ANDEQ     'S'
     C                   ADD       EAAMT         XXTXDU
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY2         READE     PEA01                                  50
     C                   ENDDO                                                  *IN50
     C*
     C     RRTDFL        IFNE      'Y'
     C     XXTHRS        OREQ      'Y'
     C                   Z-ADD     0             XXTXDU
     C                   ELSE
     C*
     C     XXAB01        MULT(H)   XXADJ         XXWK90
     C     XXAB01        IFEQ      *ZERO
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  XXAB01 IFEQ
     C                   MULT(H)   RRTXPC        XXWK90
     C                   MULT(H)   .01           XXWK90
     C                   Z-ADD     XXWK90        XXTXDU
     C*
     C                   ENDIF                                                  RRTDFL IFNE
     C*
     C                   ADD       XXTXDU        SCTOT
     C                   MOVE      '0'           *IN80

     C                   Z-ADD     0             X
     C                   MOVE      *OFF          *IN89

     c                   Endsr                                                  DepositAdj

     C*** End   Add ***  02/08/13  ************************************
      *****************************************************************
      *
     OQSYSPRT4  E            OUT            1 06
     OQSYSPRT2  E            ROUTE          1 06
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
      ***** End Del ***** 08/26/14 ************************************
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
     O          E            ROUTE       2  2
     O                                           24 'REPORT SELECTIONS:'
     O*
     O          E            ROUTE          2
     O                                           20 'COMPANY:'
     O                       XXCO#         Z     42
     O          E            ROUTE          2
     O                                           18 'GROUP:'
     O                       XXFND         Z     42
     O          E            ROUTE          2
     O                                           26 'RATE SET CODE:'
     O                       XXRTCD              42
     O          E            ROUTE          2
     O                                           23 'PERCENTAGE:'
     O                       XXPCT         K     42
     O          E            ROUTE          2
     O                                           27 'EFFECTIVE DATE:'
     O                       X1PFDT              42 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'BILL COMMENTS:'
     O                       QQBCM1              36
     O                       QQBCM2              39
     O                       QQBCM3              42
     O          E            ROUTE          2
     O                                           33 'INSTALLMENT COMMENTS:'
     O                       QQICM1              36
     O                       QQICM2              39
     O                       QQICM3              42
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
     O*
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              87
     O                       DSDATE        Y    132
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
     O          E            HEAD           2
     O                                            6 'GROUP:'
     O                       XXFND         Z     10
     O                       FNNAME              71
     O          E            HEAD           2
     O                                           12 'EMPLOYER'
     O                                           19 'DIV'
     O                                           25 'FYR'
     O                                           41 'EMPLOYER NAME'
     O          E            DET            1
     O                       XXEMP#        Z     12
     O                       XXDIV         Z     19
     O                       XXFYR         Z     25
     O                       EMNAME              68
     O*
     O          E            NOREC       2  1
     O                                           60 '*** NO RECORDS MEET'
     O                                         +  1 'SELECTION CRITERIA ***'
     O*
     O*****************************************************************
     OPEA01     E            PEA01E
     O*
**  QCAEXEC TABLE
OVRDBF FILE(WDELL) TOFILE(WAELL)
OVRDBF FILE(WMEPL1) TOFILE(WAEPL1)
OVRDBF FILE(WMEPL) TOFILE(WAEPL)
DLTOVR FILE(WDELL)
DLTOVR FILE(WMEPL1)
DLTOVR FILE(WMEPL)
OVRDBF FILE(WDDSL) TOFILE(WADSL)
DLTOVR FILE(WDDSL)
OVRDBF FILE(WDELP) TOFILE(WAELP)
OVRDBF FILE(WMEPP) TOFILE(WAEPP)
DLTOVR FILE(WDELP)
DLTOVR FILE(WMEPP)
OVRDBF FILE(WMEPL6) TOFILE(WAEPL6)
DLTOVR FILE(WMEPL6)
OVRDBF FILE(WDELL1) TOFILE(WAELL1)
DLTOVR FILE(WDELL1)
OVRDBF FILE(WDILL) TOFILE(WAILL)
DLTOVR FILE(WDILL)
OVRDBF FILE(WDMPL) TOFILE(WAMPL)
DLTOVR FILE(WDMPL)
OVRDBF FILE(WDHCL3) MBR(REVISED)                           11/10/10
DLTOVR FILE(WDHCL3)                                        11/10/10
** DAYS OF WEEK FOR ARRAY                                              ***
MOTUWETHFRSASUMOTUWETHFRSA
**
312831303130313130313031
