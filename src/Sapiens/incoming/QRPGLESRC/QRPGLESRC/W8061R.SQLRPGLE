     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8061R - MASS REVISED BILLING PROGRAM
     H*****************************************************************
     H**                                                              *
     H** PROGRAM DESCRIPTION:                                         *
     H**    THIS PROGRAM WILL RUN REVISED BILLS ON EVERY NON-CX       *
     H**    POLICY WITHIN A GROUP THAT IS ACTIVE AS OF THE EFF DATE.  *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**    W8061C                                                    *
     H**                                                              *
     H** CALLS TO:                                                    *
     H** MULTITUDE                                                    *
     H**                                                              *
     H** PROGRAMMER: C:65                                             *
     H** CREATED:    02/26/04         PROJECT: SUPPORT                *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/29/25 C:65  Project: 146WKQ (Phw#118404)                   *
     H*                Fix erroneous chain to WDF1L -                 *
     H*                chain statement is missing policy year key.    *
     H*                                                               *
     H* 06/12/24 C:062 Project: 146WKQ                                *
     H*                Don't create assessments billing if not S/R or *
     H*                marked as SR assessments.                      *
     H*                                                               *
     H* 01/05/24 C:062 Support: 114720(129)                           *
     H*                Added parameter to WIINV                       *
     H*                                                               *
     H* 06/14/23 C:65  PROJECT: 150WFY (revision to 150WAR)           *
     H*                Clear LD percentage prior to running calculat- *
      *                ions in order to recalculate them properly     *
      *                in a later stage of the calculations.          *
      *                Stop calling W8010R a second time for loss     *
      *                constant within BILL subroutine.  This causes  *
      *                calculation to be called six times since the   *
      *                BILL subroutine is called three times now.     *
      *                Instead calculations will be called 3-4 times: *
      *                1) First time calculates payroll manual        *
      *                   premiums on each division and loads updated *
      *                   standard premium into WDDSP.                *
      *                2) Second call happens if loss constant or     *
      *                   % of manual is availble on the policy. This *
      *                   replicates the second call that happens     *
      *                   in SRMDIV for original bills.  The LD %     *
      *                   will be calculated on this step.            *
      *                3) Third call is able to accurately calculate  *
      *                   stock discount on each division now if      *
      *                   second call did not happen.  It also        *
      *                   calculates the final premium on each div    *
      *                   into WDMPP.                                 *
      *                4) Fourth/final call will be able to calculate *
      *                   net rates now that WDMPP has updated info.  *
     H*                                                               *
     H* 04/20/22  C:65  Project WC2869                                *
     H*              1) Add processing to run over single employer or *
     H*                 over the activity date.                       *
     H*              2) Add processing to ignore non-renewal CXs.     *
     H*              3) Run the billing calculations 3 times in order *
     H*                 for spread discount and net rate to calculate *
     H*                 properly.                                     *
     H*              4) Remove 139WJU GIC coding to clean up source.  *
     H*              5) Add maximum deposit calculations and update   *
     H*                 deposit calculation procedures.               *
     H*              6) Shift A/R summary coding to be within 'E'     *
     H*                 billing type section to match revised bill.   *
     H*              7) Add coding to write deposit and tax on dep    *
     H*                 A/R records for estimating accounts.          *
     H*              8) Populate AR parameter for estimating only,    *
     H*                 populate AR2 for S/R only, and populate both  *
     H*                 for estimating accounts with deposit.         *
     H*              9) Add WXEVP file overrides to speed up bills.   *
     H*             10) Prevent zero post dates from being written    *
     H*                 with assessment adjustments when spreading    *
     H*                 to future installments                        *
     H*             11) After completing the rounding check on        *
     H*                 estimated plans verify if there are any       *
     H*                 amounts remaining and move installments up    *
     H*                 to compensate if there are none.              *
     H*             12) Update maximum # of spool files to be 200000  *
     H*                 in order to prevent a job from splitting up   *
     H*                 in middle of a large master PEO.              *
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
     H* 02/08/13 C:190 PROJECT: 140WAU                                *
     H*                Modify program to process deposit adjustments  *
     H*                for estimating accounts also.                  *
     H*                                                               *
     H* 11/16/11 C:65  Project: 139WPL                                *
     H*                CHANGE OVERRIDE COMMAND FOR HCN OVERRIDE.      *
     H*                                                               *
     H* 11/10/10 C:33  Project: 139WPL                                *
     H*                Add overrides for HCN Program.                 *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 04/15/10 C:65  PROJECT: WC1473/SUPP#71541/SUPP#72246          *
     H*                OVERRIDE TO NEW PAYROLL LOGICAL WMEPL1E.       *
     H*                ADD OVERRIDE FOR PREMIUM PERIODS FILE WDPPP.   *
     H*                PREVENT CANCELLED POLICIES FROM PROCESSING     *
     H*                AS WELL.                                       *
     H*                                                               *
     H* 07/23/09 C:65  PROJECT: 139WJU                                *
     H*                DISABLE PROJECT BY CHANGING CLTID FOR 139 TO   *
     H*                999 AFTER CLTID IS SET.                        *
     H*                                                               *
     H* 02/12/09 C:65  PROJECT: 132WBW                                *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES.                              *
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
     H*                SV1STB - 1ST ASSESSMENT SAVE AMOUNT FOR        *
     H*                         INSTALLMENTS.                         *
     H*                SU1STB - 1ST ASSESSMENT SAVE AMOUNT FOR        *
     H*                         LUMP SUM.                             *
     H*                ST1STB - 1ST ASSESSMENT SAVE TOTAL FOR         *
     H*                         INSTALLMENTS.                         *
     H*                SL1STB - 1ST ASSESSMENT SAVE TOTAL FOR         *
     H*                         LUMP SUM.                             *
     H*                (REPEATED FOR EACH ASSESSMENT 2ND THRU 5TH)    *
     H*                                                               *
     H* 10/11/07 C:183 PROJECT: 146WBC                                *
     H*                Added code to invoke the same processing that  *
     H*                occurs on a billing schedule code change if    *
     H*                the user changes the "Monthly Billings on      *
     H*                Policy Effective Day" from BLANK to 'Y' or 'Y' *
     H*                to blank.                                      *
     H*                                                               *
     H* 07/27/07  C:190 Project WC1454                                *
     H*           Add the new parameters to the program to            *
     H*           pass along the value for producing endr-            *
     H*           osments.                                            *
     H*
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
     H* 01/26/06 C:65  PROJECT: WC1167                                *
     H*                ADD PARMS FOR FREE FORM COMMENTS TO W7615R.    *
     H*                                                               *
     H* 04/07/05 C:65  PROJECT: WC1273                                *
     H*                ALLOW DEPOSIT TO BE APPLIED TO AN ANNUAL LEVEL.*
     H*
     H*                                                               *
     H*****************************************************************
     H* INDICATORS USED:                                              *
     H*                                                               *
     H* 60 - GENERAL CHAIN                                            *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     FWMRSL     IF   E           K DISK
     FWMEAL4    UF   E           K DISK
     FWMEAL3    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
     FWAEAL     O  A E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWMEPL     IF   E           K DISK    USROPN
     FWAEPL     IF A E           K DISK
     F                                     RENAME(PEP01:PEP02)
     FWDELL     IF   E           K DISK
     FWAELL     IF A E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL03)
     FWDSCL     IF   E           K DISK
     FWTMYL     IF   E           K DISK
     F                                     RENAME(PMY01:PMY02)
     F                                     INFDS(DBFDS)
     F*** begin add ***  06/14/23  *************************************
     FWALDL2    UF   E           K DISK    USROPN
     F                                     RENAME(PLD01:PLD44)
     F*** end add *****  06/14/23  *************************************
     FWMSHL1    IF   E           K DISK
     FWMMUL1    IF A E           K DISK
     FWMENL3    IF A E           K DISK
     FWMDXL     IF   E           K DISK
     FWTMYP     IF   E             DISK
     F*** begin add ***  04/20/22  **************************************
     FWMEMPHL   if   e           k Disk    Rename(pemh01:pemh00)
     FWMCOL     IF   E           K DISK    RENAME(PCO01:PCO00) USROPN
     Fwxevp     UF   E           K DISK    RENAME(pev01:pev0p) USROPN
     F*** end add *****  04/20/22  **************************************
     FQSYSPRT4  O    F  132        PRINTER OFLIND(*INOA)
     F                                     USROPN
     F                                     INFDS(PINFDS)
     FQSYSPRT2  O    F  132        PRINTER OFLIND(*INOF)
      *****************************************************************

      *** beg add ***** 07/02/13 **************************************************
     d/copy wprDivPlan
      *** end add ***** 07/02/13 **************************************************
      *** begin add ***  04/20/22  *********************************************
     D/copy wprEmplr
      *** begin add ***  04/20/22  *********************************************
     D*** Begin Add *** 01/05/24 ****************************************
     D/Copy siWebSvcPr
     D/copy sprRunCmd
     D*** End   Add *** 01/05/24 ****************************************
     D*** Begin Add *** 06/12/24 ****************************************
     D/copy wprAR
     D*** End   Add *** 06/12/24 ****************************************

     D AB1             S              9  8 DIM(12)
     D AB2             S              9  8 DIM(12)
     D BD              S              7  0 DIM(13)
     D DA              S              8  0 DIM(13)
     D PA              S              9  2 DIM(13)
     D A1              S              9  2 DIM(13)
     D A2              S              9  2 DIM(13)
     D A3              S              9  2 DIM(13)
     D A4              S              9  2 DIM(13)
     D A5              S              9  2 DIM(13)
     D A6              S              9  2 DIM(13)
     D A7              S              9  2 DIM(13)
     D DT              S              8  0 DIM(10)
     D QCA             S             55    DIM(30) CTDATA PERRCD(1)                           EC
     D PDAT            S              7  0 DIM(4) ASCEND                        PAYROLL DATES
     D RDAT            S              7  0 DIM(4) ASCEND                        RATING DATES
     D WKD             S              7  0 DIM(53) ASCEND
     D WKA             S              9  2 DIM(53)
     D WK1             S              9  2 DIM(53)
     D WK2             S              9  2 DIM(53)
     D WK3             S              9  2 DIM(53)
     D WK4             S              9  2 DIM(53)
     D WK5             S              9  2 DIM(53)
     D WK6             S              9  2 DIM(53)
     D WK7             S              9  2 DIM(53)
     D WKT             S              9  2 DIM(53)
     D DAY             S              2    DIM(13) CTDATA PERRCD(13)              DAYS OF WEEK    %
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
      *** Beg   Add ***  02/08/13  ************************************
     d xxAB01          s              9  6
     d xxAB02          s              9  6
      *** End   Add ***  02/08/13  ************************************
     d*** Begin Add *** 01/05/24 *******************************************
     d clbBifData      s                   Sqltype(clob:64000)
     D cmdStr          s           3000    Inz varying
     D cmdLength       s             15  5 Inz

     D LodWC2663       ds                  DTAARA(LODWC2663)
     D   sweepID                     40a
     d*** End   Add *** 01/05/24 *******************************************
     D AGTAPI          C                   CONST('WA002R')
     D*
     D PINFDS        E DS                  EXTNAME(SZQ4P)
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
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
     D SVLDA           DS          1024    INZ
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
      *
     D*** begin add ***  04/20/22  ***********************************
     d strCommand      s            255
     d xxColt          s              1
     d xtDepDue        s                   like(xtanpr)
     d xtDepTaxDue     s                   like(xttxdu)
     d prevEstMaxDep   s             11s 0
     d currEstMaxDep   s             11s 0
     d prevSRMaxDep    s             11s 0
     d currSRMaxDep    s             11s 0
     d prevMaxDepPrem  s             11  0
     d currMaxDepPrem  s             11  0
     d currDepPrem     s             11  2
     d prevDepPrem     s             11  2
     d prevBSsefl      s              1
     d isChangedToSr   s               n
     d SkipDepAdj      pr              n
     D*--------------------------------------------------
     D* Procedure name: GetDepPercent
     D*--------------------------------------------------
     D GetDepPercent   PR                   like(BSab01)
     D  billPlanType                  1A    const
     D*--------------------------------------------------
     D* Procedure name: GetLesserOf
     D*--------------------------------------------------
     D GetLesserOf     PR            11  2
     D  maxDepAmt                    11  0 const
     D  calcPremAmt                  11  2 const
     D*--------------------------------------------------
     D* Procedure name: CalcMaxDeposit
     D*--------------------------------------------------
     D CalcMaxDeposit  PR
     D  estMaxDepAmt                 11S 0
     D  srMaxDepAmt                  11S 0
     D  prvMaxDepReq                   n   const options(*nopass)

     d QCmdExc         pr                  extpgm('QCMDEXC')
     d command                             const like(strCommand)
     d length                        15  5 const
     D*** end add *****  04/20/22  ***********************************
     d*** Begin Add *** 06/12/24 *******************************************
     d chrCatType      s                   Like(EAarCd)
     d isSRAs          s               n   Inz(*Off)
     d isSkipAR        s               n   Inz(*Off)
     d*** End   Add *** 06/12/24 *******************************************
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     IPSC01
     I              SCCO#                       S2CO#
     I              SCFND                       S2FND
     I              SCST                        S2ST
     I              SCEFDT                      S2EFDT
     I              SCPRBS                      S2PRBS
     I              SCSTAT                      S2STAT
     I              SCUSER                      S2USER
     I              SCPGM                       S2PGM
     I              SCTIME                      S2TIME
     I              SCDT                        S2DT
     I              SCIUSR                      S2IUSR
     I              SCIDT                       S2IDT

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
     C*** begin add ***  04/20/22  ************************************
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
     C*** end add *****  04/20/22  ************************************
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
     C     EMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X1EMP#
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
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
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
     C*  LOSS CONSTANT FILE
     C*
     C     SCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C                   KFLD                    ELPFDT
     C*
     C     SCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    RSJRST
     C*  HISTORY FILE FOR NCCI REPORTING
     C     PSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTYPE            1
      ** Employer Attached Endorsement Forms file
     C     MUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XSDIV
     C                   KFLD                    XXFYR
     C*
     C*  ENDORSEMENT REQUEST WORKFILE
     C*
     C     ENKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    LDFYR
     C                   KFLD                    Q1JOBN
     C                   KFLD                    LDEND#
     C*
     C*  ENDORS/DARTS CONTROL DEFINITION
     C*
     C     DXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*
     C*   FIELD DEFINITIONS
     C*
     C     *LIKE         DEFINE    EMCO#         XXCO#
     C     *LIKE         DEFINE    EMFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMEMP#        SVEMP#
     C     *LIKE         DEFINE    EMEMP#        X1EMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    ELPFDT        XXPFDT
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    ELBSCD        XXBSCD
     C     *LIKE         DEFINE    EADT          XXCURD
     C     *LIKE         DEFINE    EADT          XXCUR2
     C     *LIKE         DEFINE    EADT          XXDTE
     C     *LIKE         DEFINE    EADT          XXDTE1
     C     *LIKE         DEFINE    EADT          XXDTE2
     C     *LIKE         DEFINE    EADT          XXEFD1
     C     *LIKE         DEFINE    EADT          XXPODT
     C     *LIKE         DEFINE    MUEND#        XXEND#
     C     *LIKE         DEFINE    EMNAME        SVNAME
     C     *LIKE         DEFINE    EMFDNM        SVFDNM
     C     *LIKE         DEFINE    EMNAM2        SVNAM2
     C     *LIKE         DEFINE    EMFDN2        SVFDN2
      *
      *   ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQEFDT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQBCM1            2
     C                   PARM                    QQBCM2            2
     C                   PARM                    QQBCM3            2
     C                   PARM                    QQICM1            2
     C                   PARM                    QQICM2            2
     C                   PARM                    QQICM3            2
     C                   PARM                    QQDART            1
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQPEND            1
     C                   PARM                    QQDS1            41
     C                   PARM                    QQDS2            41
     C                   PARM                    QQDS3            41
     C*** begin add ***  04/20/22  **************************************
     C                   PARM                    QQSEMP            9
     C*** end add *****  04/20/22  **************************************
     C*
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
      *  Agent/Producer retrieve program parameter list
      *
     C     AGTPL         PLIST
     C                   PARM      XXCO#         ##CO#             3 0
     C                   PARM      XXFND         ##FND             3 0
     C                   PARM      XXEMP#        ##EMP#            9 0
     C                   PARM      XXDIV         ##DIV             5 0
     C                   PARM      LDFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
      *  INITIALIZE FIELDS BASED ON *ENTRY PARMS
      *
      *
     C     *DTAARA       DEFINE    *LDA          SVLDA
     C     *LOCK         IN        *DTAARA
     C                   MOVEL     QQDS1         QQDSC1           40
     C                   MOVEL     QQDS2         QQDSC2           40
     C                   MOVEL     QQDS3         QQDSC3           40
     C                   MOVE      *BLANK        XXFLAG            1
     C                   MOVE      *BLANK        XXMULT            1
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        X1EMP#
     C                   MOVE      QQEFDT        X1PFDT            8 0
     C                   MOVE      QQPEND        XXPEND            1
     C                   MOVE      'N'           XXPYCG            1
     C                   MOVE      QQDSC1        SCDSC1           40
     C                   MOVE      QQDSC2        SCDSC2           40
     C                   MOVE      QQDSC3        SCDSC3           40
     C*** begin add ***  04/20/22  ************************************
     C                   TIME                    DSDTTM
     C     *LIKE         DEFINE    EADT          SYSDT
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     DSTIME        SYSTM
     C*
     C* Set up program to run for single employer.
     C*
     C                   Move      QQSEMP        saveEmp#          9 0
     C                   If        saveEmp# <> 0
     C                   Move      saveEmp#      x1Emp#
     C                   Endif
     C*** end add *****  04/20/22  ************************************
     C                   Z-ADD     0             AB
     C                   Z-ADD     0             LDEND#
     C                   Z-ADD     0             LDEEDT
     C                   Z-ADD     0             LDFYR
     C                   Z-ADD     0             LDMYRR
     C                   MOVE      '0'           XXOPN             1
     C                   Z-ADD     0             SVEMP#
     C                   MOVE      *BLANK        XXSKIP            1
     C                   MOVE      *BLANK        MAXFLG            1
     C*** begin chg ***  04/20/22  ************************************
     C*****              Z-ADD     9000          MAXSPL            4 0
     C                   Z-ADD     200000        MAXSPL            6 0
     C*** end chg *****  04/20/22  ************************************
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
     C*** begin add ***  04/20/22  ************************************
     C                   If        saveEmp# <> 0
     C                   Move      *hival        xxpfdt
     C                   Endif
     C*** end add *****  04/20/22  ************************************
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C*
     C                   EXSR      SR1ST
     C*
     C* POSITION FILE TO NEXT EMPLOYER FOR RESUBMITS.
     C*
     C     EMKY2         SETLL     PEM01
     C*
     C* READ EMPLOYER MASTER FILE.
     C*
     C     EMKY          READE     PEM01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EMEMP#        XXEMP#
     C                   Z-ADD     EMDIV         XXDIV
     C*
     C* CHECK FOR CURRENT POLICY YEAR IF NEXT EMPLOYER.
     C*
     C     EMEMP#        IFNE      SVEMP#
     C                   Z-ADD     XXEMP#        SVEMP#
     C                   MOVE      *BLANK        XXSKIP            1
     C*
     C     EMKY1         CHAIN     PEM01                              50
     C     *IN50         DOWEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *ON
     C     ELPFDT        ORGT      XXPFDT
     C     ELFAF         OREQ      'A'
     C                   MOVE      'Y'           XXSKIP
     C                   ENDIF
       //*** Begin Add *** 06/12/24 ****************************************
       // Check to see if SR and if SR Assessments is off
       isSRAs = *Off;
       xxKey = %EditC(ELbsCd:'X');
       Chain (xxCo#:xxKey) pbs01;
       If %Found(wtabl) And BSseFl = 'S';
          If ELsrAs = ' ';
          //*** begin chg ***  01/29/25  *************************************
          //*****   Chain (xxCo#:xxFnd) pf101;
             Chain (xxCo#:xxFnd:xxFyr) pf101;
          //*** end chg *****  01/29/25  *************************************
             If %Found(wdf1l) And F1SRAs = 'Y';
                isSRAs = *On;
             EndIf;
          Else;
             isSRAs = *On;
          EndIf;
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
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
     C*** begin add ***  04/20/22  *********************************************
     C                   Move      cx(x)         intCxDate         7 0
     C                   Move      'BIL'         outMode           3
       If *in42 =  *on And OvrRenewalCx(outMode:ELCo#:ELFnd:
        ELEmp#:ELdiv:ELfyr:intCxDate) = *On;
          *in42 = *off;
       Endif;
     C*** end add *****  04/20/22  *********************************************
     C*
     C     *IN42         IFEQ      *ON
     C                   MOVE      'Y'           XXSKIP
     C                   ENDIF
     C*
     C                   LEAVE
     C*
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C     EMKY1         READE     PEM01                                  50
     C                   ENDDO
     C*
     C* POSITION EMPLOYER MASTER RECORD BACK.
     C*
     C     ELKY1         CHAIN     PEM01                              50
     C*
     C                   ENDIF
     C*
     C* PROCESS EMPLOYER
     C*
     C     XXSKIP        IFNE      'Y'
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     F1KY          CHAIN     PF101                              60
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
     C                   MOVE      *BLANK        XXLOSS            1
     C                   EXSR      CHKLC
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C                   Z-ADD     XXEMP#        SVEMP#
     C                   EXSR      SPRD
     C*** begin add ***  06/14/23  *************************************
     C                   Clear                   xxPercMan         1
     C*
     C* Clear large deductible percentage for percentage of manual calculations
     C*
       If not %open(WALDL2);
          Open WALDL2;
       Endif;

       Setll elky2 pel03;
       Reade elky2 pel03;
       Dow not %eof;
          If ELManP <> 0;
             xxPercMan = 'Y';
             Chain (ELCo#:ELFnd:ELFyr:ELEmp#:ELDiv) pld44;
             If %found;
                LDPct1 = 0;
                Update pld44;
             Endif;
          Endif;
          Reade elky2 pel03;
       Enddo;

       Close WALDL2;
     C     ELKEY         CHAIN     PEL01                              60
     C*** end add *****  06/14/23  *************************************
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
     C*** begin add ***  04/20/22  *************************************
     C                   Clear                   QQAR
     C                   Clear                   QQAR2
       xtDepDue = 0;
       xtDepTaxDue = 0;
     C*** end add *****  04/20/22  *************************************
     C                   MOVE      *OFF          *IN62
     C                   MOVE      *OFF          *IN63
     C                   MOVE      *OFF          *IN64
     C*
     C                   EXSR      LDOVRF
     C                   EXSR      BILL
     C*** begin add ***  06/14/23  ***************************************
     C* Calculate an extra time when loss constant or percentage of manual
     C                   If        xxloss = 'Y' or xxPercMan = 'Y'
     C                   Eval      calcManP = 'Y'
     C                   Exsr      BILL
     C                   Clear                   calcManP          1
     C                   Endif
     C*** end add *****  06/14/23  ***************************************
     C*** begin add ***  04/20/22  ***********************************
     C                   EXSR      BILL
     C                   EXSR      BILL
     C*** end add *****  04/20/22  ***********************************
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
     C*** begin add ***  04/20/22  *********************************************
     C                   Move      cx(x)         intCxDate         7 0
     C                   Move      'BIL'         outMode           3
       If *in42 =  *on And OvrRenewalCx(outMode:ELCo#:ELFnd:
        ELEmp#:ELdiv:ELfyr:intCxDate) = *On;
          *in42 = *off;
       Endif;
     C*** end add *****  04/20/22  *********************************************
     C*
     C     *IN42         IFEQ      *OFF
     C     ELFAF         ANDNE     'A'
     C*
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
     C     RSKY1         CHAIN     PRS01                              60
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
     C     XXPEND        IFEQ      'Y'
     C                   EXSR      ENDR
     C                   ENDIF
     C*
     C     XXDIVC        IFGT      1
     C                   EXSR      SUMM
     C                   MOVE      *BLANK        XXMULT
     C                   ENDIF
     C*
     C     EMKY1         SETGT     PEM01
     C                   ENDIF                                                  *IN69=*OFF
     C*
     C                   ENDIF                                                  XXSKIP IFNE
     C*
     C     *IN69         IFEQ      *OFF
     C     EMKY          READE     PEM01                                  60
     C                   ELSE
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  *IN69 IFEQ
     C*** begin add ***  04/20/22  *********************************************
     C                   If        saveEmp# <> 0
     C                   Move      *ON           *IN60
     C                   Endif
     C*** end add *****  04/20/22  *********************************************
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
       //*** Begin Add *** 01/05/24 ***************************************
       // Send End Sweep
       clbBifData_Data = '{"sweepID":"' + %Trim(sweepID) +
                         '","transaction":"sweep completed"}';
       clbBifData_Len  = %Len(%Trim(clbBifData_Data));
       LoadSIWEBSVCDataQ(clbBifData);
       //*** End   Add *** 01/05/24 ***************************************
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
     C                   Z-ADD     WKDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C     XXDT          IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKDATE
     C                   ENDIF                                                  XXDT IFLT
     C*
     C     DA(1)         IFNE      *ZERO
     C                   MOVE      DA(1)         QQDATE
     C                   ELSE
     C                   MOVE      WKDATE        QQDATE            8
     C                   ENDIF                                                  DA,1 IFNE
     C                   MOVE      QQDATE        QQDUE             8
     C                   MOVE      ' '           QQPRT             1
     C                   MOVE      ' '           QQRNB             1
     C*
     C                   MOVEL     'QPRINT'      QQOUTQ
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
     C                   PARM                    QQRMCD
     C                   PARM      *BLANKS       QQFCM1           30
     C                   PARM      *BLANKS       QQFCM2           30
     C                   PARM      *BLANKS       QQFCM3           30
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
     C                   Z-ADD     WKDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C     XXDT          IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         WKDATE
     C                   ENDIF                                                  XXDT IFLT
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
     C     FNKY          CHAIN     PFN01                              60
       //*** Begin Add *** 01/05/24 ***************************************
       // Create Sweep Data Area
       //cmdStr = 'CRTDTAARA DTAARA(QTEMP/LODWC2663) TYPE(*CHAR) LEN(40) ' +
       //         'VALUE('' '')';
       //cmdLength = %Len(cmdStr);
       //runCmd(cmdStr:cmdLength);

       // Populagte the sweep ID
       In *Lock LodWC2663;
       sweepID = 'SWP' + %EditC(sysDt:'X') + %EditC(sysTm:'X');
       Out LodWC2663;

       // Send End Sweep
       clbBifData_Data = '{"sweepID":"' + %Trim(sweepID) +
                         '","transaction":"sweep started"}';
       clbBifData_Len  = %Len(%Trim(clbBifData_Data));
       LoadSIWEBSVCDataQ(clbBifData);
       //*** End   Add *** 01/05/24 **************************************
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
     C*** begin add ***  04/20/22  **************************************
       prevBSsefl = BSsefl;
       CalcMaxDeposit (prevEstMaxDep:prevSRMaxDep:*On);
     C*** end add *****  04/20/22  **************************************
     C*
     C*  LOAD NEW BILLING SCHEDULE
     C*
     C     ELKEY         CHAIN     PEL02                              60
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     ELBSCD        XXKEY
     C                   MOVEL     ELBSCD        XXBSCD
     C                   MOVEL     ELBLFL        XXBLFL            1
     C     TBKY          CHAIN     PBS01                              60
     C  N60              MOVEA     AB            AB2
     C*** begin add ***  04/20/22  **************************************
       CalcMaxDeposit (currEstMaxDep:currSRMaxDep);
       If prevBSsefl = 'E' and BSsefl = 'S';
          isChangedToSR = *On;
       EndIf;
     C*** end add *****  04/20/22  **************************************
     C*
     C*** begin del ***  04/20/22  *******************************************
     C* deleted a/r summary code here
     C*** end del *****  04/20/22  *******************************************
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
      /free
       //*** begin del ***  04/20/22  *******************************************
       //*****xxAB01 = BSab01;
       //*****If BSab01 = 0;
       //*****   xxAB01 = BSab02;
       //*****Endif;
       //*** end del *****  04/20/22  *******************************************
       Exsr DepositAdj;
      /end-free
     C*** begin add ***  04/20/22  *******************************************
     C*
     C     ELARSM        IFEQ      'Y'
     C     XXWAR         ANDNE     'Y'
     C*** end add *****  04/20/22  *******************************************
     C*
     C     SCTOT         IFEQ      *ZERO
     C     F1UPAR        OREQ      'N'
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
     C*** begin del ***  04/20/22  *****************************************
     C*****              MOVE      *BLANKS       QQAR
     C*** end del *****  04/20/22  *****************************************
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
     C                   Z-ADD     SCDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
     C     XXDT          IFLT      ELBSDT
     C                   Z-ADD     ELBSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDT
     C                   ENDIF                                                  XXDT IFLT
     C*
     C     CLTID         IFNE      '139'
     C     SCDT          IFNE      *ZERO
     C                   Z-ADD     SCDT          DA(1)
     C                   ELSE
     C                   Z-ADD     SCEFDT        DA(1)
     C                   ENDIF                                                  SCDT IFNE
     C     XXADJ         MULT(H)   XXAB01        XXWK90            9 0
     C     XXAB01        IFEQ      *ZERO
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  XXAB01 IFEQ
     C                   Z-ADD     XXWK90        XXADJ
     C                   Z-ADD     XXADJ         PA(1)
     C*** begin add ***  04/20/22  ***********************************
     C     XXWAR         IFEQ      'Y'
     C                   Z-ADD     XXDEPP        PA(1)
     C                   ENDIF
     C*** end add *****  04/20/22  ***********************************
     C                   Z-ADD     XXTXDU        P1(1)
     C                   ENDIF                                                  CLTID IFNE
     C                   END
     C*** begin add ***  04/20/22  *******************************************
     C                   Endif                                                  ELARSM = 'Y'
     C*** end add *****  04/20/22  *******************************************
     C*
     C                   END                                                    BSSEFL = 'S'
     C*
     C*  COMPUTE ADJUSTMENT IF NEW BILLING SCHEDULE DESIGNATES AN
     C*  ESTIMATING ACCOUNT
     C*
     C     BSSEFL        IFEQ      'E'
      *** beg add *****  02/08/13  ************************************
      /free
       //*** begin del ***  04/20/22  *******************************************
       //*****xxAB02 = BSesdp;
       //*****If BSesdp = 0;
       //*****   xxAB02 = BSesd2;
       //*****Endif;
       //*****xxAB01 = xxAB02/100;
       //*** end del *****  04/20/22  *******************************************
       Exsr DepositAdj;
      /end-free
      *** end   add ***  02/08/13  ************************************
     C*** begin add ***  04/20/22  *******************************************
     C*
     C     ELARSM        IFEQ      'Y'
     C     XXWAR         ANDNE     'Y'
     C*
     C     ELANPR        SUB       ELPRTD        X1ANPR           11 2
       // Reduce annual premium (X1ANPR) by the amount of dividend premium
       x1anpr = x1anpr - ELdvpr;
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
     C*** end add *****  04/20/22  *******************************************
     C                   MOVE      *OFF          *IN81
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
     C                   Z-ADD     *ZEROS        YYMM
     C                   Z-ADD     *ZEROS        XXAMT3           11 2
     C                   Z-ADD     *ZEROS        X1TOT            11 2
     C                   Z-ADD     *ZEROS        X1AMT1           11 2
     C                   Z-ADD     *ZEROS        X1AMT2           11 2
     C                   Z-ADD     *ZEROS        X1AMT3           11 2
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
     C                   MOVE      '0'           XXFUT             1
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
     C*
     C*  PROCESS NON-WEEKLY BILLING SCHEDULE CODES.
     C*
     C     BSDOWK        IFEQ      *BLANKS
     C     BSFREQ        ANDNE     'T'
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
     C                   Z-ADD     12            X
     C     X             DOWGT     0
     C     AB(X)         IFEQ      *ZERO
     C                   Z-ADD     *HIVAL        BD(X)
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             X
     C                   ENDDO
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
     C     AB2(Y)        MULT(H)   ELTXDU        X1AMT2
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
     C                   ENDIF                                                  X1FLAG IFEQ
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
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         DA(X)
     C*
     C     FNKY          CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXTOT         ANDGT     *ZERO
     C     XXTOT         OREQ      *ZERO
     C     X1TOT         ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X1ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X2ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X3ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X4ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X5ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X6ATOT        ANDGT     *ZEROS
     C     XXTOT         OREQ      *ZERO
     C     X7ATOT        ANDGT     *ZEROS
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
     C*
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
     C*
     C     AB2(Z)        MULT(H)   XX1STB        A1(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A1(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A1(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C     X             IFEQ      1
     C                   ADD       XU1STB        A1(X)
     C                   ENDIF
     C                   ADD       A1(X)         X1AA3
     C*
     C     AB2(Z)        MULT(H)   XX2NDB        A2(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A2(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A2(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C     X             IFEQ      1
     C                   ADD       XU2NDB        A2(X)
     C                   ENDIF
     C                   ADD       A2(X)         X2AA3
     C*
     C     AB2(Z)        MULT(H)   XX3RDB        A3(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A3(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A3(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C     X             IFEQ      1
     C                   ADD       XU3RDB        A3(X)
     C                   ENDIF
     C                   ADD       A3(X)         X3AA3
     C*
     C     AB2(Z)        MULT(H)   XX4THB        A4(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A4(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A4(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C     X             IFEQ      1
     C                   ADD       XU4THB        A4(X)
     C                   ENDIF
     C                   ADD       A4(X)         X4AA3
     C*
     C     AB2(Z)        MULT(H)   XX5THB        A5(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A5(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A5(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C     X             IFEQ      1
     C                   ADD       XU5THB        A5(X)
     C                   ENDIF
     C                   ADD       A5(X)         X5AA3
     C*
     C     AB2(Z)        MULT(H)   XX6THB        A6(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A4(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A6(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C     X             IFEQ      1
     C                   ADD       XU6THB        A6(X)
     C                   ENDIF
     C                   ADD       A6(X)         X6AA3
     C*
     C     AB2(Z)        MULT(H)   XX7THB        A7(X)
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  A7(X)         XXWK10           11 0
     C                   Z-ADD     XXWK10        A7(X)
     C                   ENDIF                                                  EXRNDI IFEQ
     C     X             IFEQ      1
     C                   ADD       XU7THB        A7(X)
     C                   ENDIF
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
     C*
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
     C*** begin chg ***  04/20/22  *****************************************
     C*****PA(1)         ORLT      *ZERO
     C*****ELBSCD        ANDEQ     XXBSCD
     C*****XXFFL1        ANDEQ     'Y'
     C*****CLTID         ANDNE     '109'
     C     P1(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A1(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A2(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A3(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A4(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A5(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A6(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C     A7(1)         ORGT      *ZERO
     C     XXFFL1        ANDEQ     'Y'
     C*** end chg *****  04/20/22  *****************************************
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
     C*** begin chg ***  04/20/22  *****************************************
     C*****              SUB       A1(1)         X1AA3
     C*****              SUB       A2(1)         X2AA3
     C*****              SUB       A3(1)         X3AA3
     C*****              SUB       A4(1)         X4AA3
     C*****              SUB       A5(1)         X5AA3
     C*****              SUB       A6(1)         X6AA3
     C*****              SUB       A7(1)         X7AA3
     C     XU1STB        IFEQ      0
     C                   SUB       A1(1)         X1AA3
     C                   ENDIF
     C     XU2NDB        IFEQ      0
     C                   SUB       A2(1)         X2AA3
     C                   ENDIF
     C     XU3RDB        IFEQ      0
     C                   SUB       A3(1)         X3AA3
     C                   ENDIF
     C     XU4THB        IFEQ      0
     C                   SUB       A4(1)         X4AA3
     C                   ENDIF
     C     XU5THB        IFEQ      0
     C                   SUB       A5(1)         X5AA3
     C                   ENDIF
     C     XU6THB        IFEQ      0
     C                   SUB       A6(1)         X6AA3
     C                   ENDIF
     C     XU7THB        IFEQ      0
     C                   SUB       A7(1)         X7AA3
     C                   ENDIF
     C*** end chg *****  04/20/22  *****************************************
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
     C*** begin add ***  04/20/22  **************************************
     C     XU1STB        IFEQ      0
     C*** end add *****  04/20/22  **************************************
     C     A1(1)         DIV(H)    XXINST        XXAMT2
     C*
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
     C*
     C                   ADD       XXAMT2        A1(W)
     C                   ADD       XXAMT2        X1AA3
     C*** begin add ***  04/20/22  **************************************
     C                   Endif
     C     XU2ndB        IFEQ      0
     C*** end add *****  04/20/22  **************************************
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
     C*** begin add ***  04/20/22  **************************************
     C                   Endif
     C     XU3rdB        IFEQ      0
     C*** end add *****  04/20/22  **************************************
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
     C*** begin add ***  04/20/22  **************************************
     C                   Endif
     C     XU4thB        IFEQ      0
     C*** end add *****  04/20/22  **************************************
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
     C*** begin add ***  04/20/22  **************************************
     C                   Endif
     C     XU5thB        IFEQ      0
     C*** end add *****  04/20/22  **************************************
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
     C*** begin add ***  04/20/22  **************************************
     C                   Endif
     C     XU6thB        IFEQ      0
     C*** end add *****  04/20/22  **************************************
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
     C*** begin add ***  04/20/22  **************************************
     C                   Endif
     C     XU7thB        IFEQ      0
     C*** end add *****  04/20/22  **************************************
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
     C*** begin add ***  04/20/22  **************************************
     C                   Endif
     C*** end add *****  04/20/22  **************************************
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
     C*** begin chg ***  04/20/22  **************************************
     C*****              Z-ADD     A1(X)         A1(W)
     C*****              Z-ADD     0             A1(X)
     C*****              Z-ADD     A2(X)         A2(W)
     C*****              Z-ADD     0             A2(X)
     C*****              Z-ADD     A3(X)         A3(W)
     C*****              Z-ADD     0             A3(X)
     C*****              Z-ADD     A4(X)         A4(W)
     C*****              Z-ADD     0             A4(X)
     C*****              Z-ADD     A5(X)         A5(W)
     C*****              Z-ADD     0             A5(X)
     C*****              Z-ADD     A6(X)         A6(W)
     C*****              Z-ADD     0             A6(X)
     C*****              Z-ADD     A7(X)         A7(W)
     C*****              Z-ADD     0             A7(X)
     C     XU1STB        IFEQ      0
     C                   Z-ADD     A1(X)         A1(W)
     C                   Z-ADD     0             A1(X)
     C                   ENDIF
       // Add assessment adjustment to previous installment
       //  if an installment date doesn't exist for it. This
       //  can happen under a litany of conditions.
       If xu1stb <> 0 and a1(x) <> 0 and da(x) = 0;
          a1(w) = a1(w) + a1(x);
          a1(x) = 0;
       Endif;
     C     XU2NDB        IFEQ      0
     C                   Z-ADD     A2(X)         A2(W)
     C                   Z-ADD     0             A2(X)
     C                   ENDIF
       If xu2ndb <> 0 and a2(x) <> 0 and da(x) = 0;
          a2(w) = a2(w) + a2(x);
          a2(x) = 0;
       Endif;
     C     XU3RDB        IFEQ      0
     C                   Z-ADD     A3(X)         A3(W)
     C                   Z-ADD     0             A3(X)
     C                   ENDIF
       If xu3rdb <> 0 and a3(x) <> 0 and da(x) = 0;
          a3(w) = a3(w) + a3(x);
          a3(x) = 0;
       Endif;
     C     XU4THB        IFEQ      0
     C                   Z-ADD     A4(X)         A4(W)
     C                   Z-ADD     0             A4(X)
     C                   ENDIF
       If xu4thb <> 0 and a4(x) <> 0 and da(x) = 0;
          a4(w) = a4(w) + a4(x);
          a4(x) = 0;
       Endif;
     C     XU5THB        IFEQ      0
     C                   Z-ADD     A5(X)         A5(W)
     C                   Z-ADD     0             A5(X)
     C                   ENDIF
       If xu5thb <> 0 and a5(x) <> 0 and da(x) = 0;
          a5(w) = a5(w) + a5(x);
          a5(x) = 0;
       Endif;
     C     XU6THB        IFEQ      0
     C                   Z-ADD     A6(X)         A6(W)
     C                   Z-ADD     0             A6(X)
     C                   ENDIF
       If xu6thb <> 0 and a6(x) <> 0 and da(x) = 0;
          a6(w) = a6(w) + a6(x);
          a6(x) = 0;
       Endif;
     C     XU7THB        IFEQ      0
     C                   Z-ADD     A7(X)         A7(W)
     C                   Z-ADD     0             A7(X)
     C                   ENDIF
       If xu7thb <> 0 and a7(x) <> 0 and da(x) = 0;
          a7(w) = a7(w) + a7(x);
          a7(x) = 0;
       Endif;
     C*** end chg *****  04/20/22  **************************************
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
     C*** begin chg  ***  04/20/22  ******************************************
     C*****              ADD       XXAMT3        PA(1)
     C     PA(1)         IFNE      *ZERO
     C     PA(2)         OREQ      *ZERO
     C                   ADD       XXAMT3        PA(1)
     C                   ELSE
     C                   ADD       XXAMT3        PA(2)
     C                   ENDIF
     C*** end chg ******  04/20/22  ******************************************
     C                   END
     C     X1AMT3        IFLT      1.00
     C     X1AMT3        ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** begin chg  ***  04/20/22  ******************************************
     C*****              ADD       X1AMT3        P1(1)
     C     P1(1)         IFNE      *ZERO
     C     P1(2)         OREQ      *ZERO
     C                   ADD       X1AMT3        P1(1)
     C                   ELSE
     C                   ADD       X1AMT3        P1(2)
     C                   ENDIF
     C*** end chg ******  04/20/22  ******************************************
     C                   END
     C*
     C     X1AA3         IFLT      1.00
     C     X1AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** begin chg  ***  04/20/22  ******************************************
     C*****              ADD       X1AA3         A1(1)
     C     A1(1)         IFNE      *ZERO
     C     A1(2)         OREQ      *ZERO
     C                   ADD       X1AA3         A1(1)
     C                   ELSE
     C                   ADD       X1AA3         A1(2)
     C                   ENDIF
     C*** end chg ******  04/20/22  ******************************************
     C                   ENDIF
     C*
     C     X2AA3         IFLT      1.00
     C     X2AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** begin chg  ***  04/20/22  ******************************************
     C*****              ADD       X2AA3         A2(1)
     C     A2(1)         IFNE      *ZERO
     C     A2(2)         OREQ      *ZERO
     C                   ADD       X2AA3         A2(1)
     C                   ELSE
     C                   ADD       X2AA3         A2(2)
     C                   ENDIF
     C*** end chg ******  04/20/22  ******************************************
     C                   ENDIF
     C*
     C     X3AA3         IFLT      1.00
     C     X3AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** begin chg  ***  04/20/22  ******************************************
     C*****              ADD       X3AA3         A3(1)
     C     A3(1)         IFNE      *ZERO
     C     A3(2)         OREQ      *ZERO
     C                   ADD       X3AA3         A3(1)
     C                   ELSE
     C                   ADD       X3AA3         A3(2)
     C                   ENDIF
     C*** end chg ******  04/20/22  ******************************************
     C                   ENDIF
     C*
     C     X4AA3         IFLT      1.00
     C     X4AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** begin chg  ***  04/20/22  ******************************************
     C*****              ADD       X4AA3         A4(1)
     C     A4(1)         IFNE      *ZERO
     C     A4(2)         OREQ      *ZERO
     C                   ADD       X4AA3         A4(1)
     C                   ELSE
     C                   ADD       X4AA3         A4(2)
     C                   ENDIF
     C*** end chg ******  04/20/22  ******************************************
     C                   ENDIF
     C*
     C     X5AA3         IFLT      1.00
     C     X5AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** begin chg  ***  04/20/22  ******************************************
     C*****              ADD       X5AA3         A5(1)
     C     A5(1)         IFNE      *ZERO
     C     A5(2)         OREQ      *ZERO
     C                   ADD       X5AA3         A5(1)
     C                   ELSE
     C                   ADD       X5AA3         A5(2)
     C                   ENDIF
     C*** end chg ******  04/20/22  ******************************************
     C                   ENDIF
     C*
     C     X6AA3         IFLT      1.00
     C     X6AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** begin chg  ***  04/20/22  ******************************************
     C*****              ADD       X6AA3         A6(1)
     C     A6(1)         IFNE      *ZERO
     C     A6(2)         OREQ      *ZERO
     C                   ADD       X6AA3         A6(1)
     C                   ELSE
     C                   ADD       X6AA3         A6(2)
     C                   ENDIF
     C*** end chg ******  04/20/22  ******************************************
     C                   ENDIF
     C*
     C     X7AA3         IFLT      1.00
     C     X7AA3         ANDGT     -1.00
     C     EXRNDI        OREQ      'Y'
     C*** begin chg  ***  04/20/22  ******************************************
     C*****              ADD       X7AA3         A7(1)
     C     A7(1)         IFNE      *ZERO
     C     A7(2)         OREQ      *ZERO
     C                   ADD       X7AA3         A7(1)
     C                   ELSE
     C                   ADD       X7AA3         A7(2)
     C                   ENDIF
     C*** end chg ******  04/20/22  ******************************************
     C                   ENDIF
     C*** begin add ****  04/20/22  ******************************************
     C*
     C* If there are no amounts left in the 1st installment slot after the rounding
     C* is completed then move up all of the later amounts to compenstate.
     C*
     C                   If        PA(1) = 0 and P1(1) = 0 and A1(1) = 0 and
     C                             A2(1) = 0 and A3(1) = 0 and A4(1) = 0 and
     C                             A5(1) = 0 and A6(1) = 0 and A7(1) = 0
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
     C                   Endif                                                  1st array slot empty
     C*** end add ******  04/20/22  ******************************************
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
     C* STATE TAX
     C*
     C     ELTXDU        DIV(H)    T             XXAMT2
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
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
     C*
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
     C     EXRNDI        IFEQ      'Y'
     C                   Z-ADD(H)  XXAMT2        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT2
     C                   ENDIF                                                  EXRNDI IFEQ
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
     C*
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
     C*
     C                   XFOOT     A1            SCTOT2           11 2
     C                   XFOOT     A2            XXTEST
     C*
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A3            XXTEST
     C                   ADD       XXTEST        SCTOT2
     C                   XFOOT     A4            SCTOT3           11 2
     C                   XFOOT     A5            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A6            XXTEST
     C                   ADD       XXTEST        SCTOT3
     C                   XFOOT     A7            SCTOT4           11 2
     C*
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
     C*
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
     C*** begin del ***  04/20/22  *****************************************
     C*****              MOVE      *BLANKS       QQAR
     C*** end del *****  04/20/22  *****************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         SCEFDT            8 0
     C*
     C                   END                                                    IF X = 0 OR
     C*                                                    F1UPAR = 'N'
     C*** begin add ***  04/20/22  *******************************************
     C                   ENDIF                                                  ELARSM IFEQ
     C*** end add *****  04/20/22  *******************************************
     C*
     C                   END                                                    BSSEFL = 'E'
     C*
     C*** begin del ***  04/20/22  *******************************************
     C*********          ENDIF                                                  ELARSM IFEQ
     C*** end del *****  04/20/22  *******************************************
     C*
     C                   ENDSR                                                  CMD7
     C*
     C*
      *  SUBROUTINE TO SETUP THE PROPER DATA AND CALL THE ENDORSEMENT
      *  PROGRAM
     C     ENDR          BEGSR
     C                   MOVE      'P'           XXTYPE
     C     PSKY          CHAIN     PSH01                              50
     C     *IN50         IFEQ      *OFF
     C     SHSTAT        IFEQ      '3'
     C     SHSTAT        OREQ      '4'
      *
     C     *LOVAL        SETLL     PMY02
     C                   READ      PMY02                                  60
     C     *IN60         DOWEQ     '0'
     C     MYPGM1        IFEQ      'W0051R21'
      *
     C     MUKY          SETGT     PMU01
     C     MUKY          READPE    PMU01                                  60
     C     *IN60         IFEQ      *OFF
     C                   ADD       1             MUEND#
     C                   WRITE     PMU01
     C                   MOVE      MUEND#        LDEND#
     C                   Z-ADD     XXCURD        LDEEDT
     C                   MOVE      MUFYR         LDFYR
     C                   MOVE      DBRRN         LDMYRR
     C                   OUT       *DTAARA
     C                   ENDIF                                                  *IN60=*OFF
      *
     C*================================================================
     C*  WRITE AN INTERIM ADJUSTMENT OF PREMIUM MID TERM ENDORSEMENT
     C*================================================================
     C*
     C*  GET THE PREVIOUS EMPLOYER MID TERM ENDORSEMENT RECORD
      *
     C     ENKY          SETGT     PEN01
     C     ENKY          READPE    PEN01                                  60
     C   60              Z-ADD     *ZERO         ENREQ#
      *
     C*  GET ENDORS/DARTS INFORMATION
      *
     C     DXKY          CHAIN     PDX01                              60
     C   60              CLEAR                   PDX01
      *
     C*  GET AGENT/PRODUCER INFORMATION
      *
     C                   CALL      AGTAPI        AGTPL
      *
     C                   MOVE      XXCO#         ENCO#
     C                   MOVE      XXFND         ENFND
     C                   MOVE      XXEMP#        ENEMP#
     C                   MOVE      XXDIV         ENDIV
     C                   MOVE      XXFYR         ENFYR
     C                   MOVE      Q1JOBN        ENJOB
     C                   MOVE      LDEND#        ENEND#
     C                   ADD       1             ENREQ#
     C                   Z-ADD     *ZERO         ENSEQ#
      *
     C                   MOVEL     XXPFDT        ENEEDT
      *
     C                   MOVEL     SVNAME        ENNAM1
     C                   MOVEL     SVFDNM        ENFDN1
      *
     C                   MOVEL     SVNAME        ENENM1
     C                   MOVEL(P)  SVNAM2        XXNM10           10
     C                   MOVE      XXNM10        ENENM1
     C                   MOVE      SVNAM2        ENENM2
      *
     C                   MOVEL     SVFDNM        ENEFN1
     C                   MOVEL(P)  SVFDN2        XXNM10           10
     C                   MOVE      XXNM10        ENEFN1
     C                   MOVE      SVFDN2        ENEFN2
      *
     C                   MOVEL     EMCONT        ENCON1
     C                   MOVEL     EMATTN        ENATT1
     C                   MOVEL     EMCNT3        ENCN31
     C                   MOVE      EMFEIN        ENFEN1
     C                   MOVEL     EMTPOB        ENTOB1
     C                   MOVEL     EMMAD1        ENMA11
     C                   MOVEL     EMMAD2        ENMA21
     C                   MOVEL     EMMCTY        ENMCT1
     C                   MOVEL     EMMST         ENMST1
     C                   MOVEL     EMMZIP        ENMZP1
     C                   MOVEL     EMPAD1        ENPA11
     C                   MOVEL     EMPAD2        ENPA21
     C                   MOVEL     EMPCTY        ENPCT1
     C                   MOVEL     EMPST         ENPST1
     C                   MOVEL     EMPZIP        ENPZP1
      *
     C                   MOVE      *BLANKS       ENMSFL
     C                   MOVE      *BLANKS       ENADDF
     C                   MOVE      *BLANKS       ENDLDF
     C                   MOVE      *BLANKS       ENILFL
     C                   MOVE      *BLANKS       ENERFL
     C     XXPYCG        IFEQ      'Y'
     C                   MOVE      'Y'           ENCRFL
     C                   ELSE
     C                   MOVE      *BLANKS       ENCRFL
     C                   ENDIF
     C                   MOVE      *BLANKS       ENNOBF
     C                   MOVE      *BLANKS       ENRIDF
     C                   MOVE      *BLANKS       ENCIFL
     C                   MOVE      *BLANKS       ENCMFL
     C                   MOVE      'Y'           ENIAPF
     C                   MOVEL     DXPIAP        ENRMCD
      *
     C                   MOVEL     FNNAME        ENFNNM
     C                   MOVE      ##AGT#        ENAGT#
     C                   MOVEL     ##ANAM        ENAGNM
     C                   MOVEL     Q1USER        ENRUSR
      *
     C     LDMYRR        CHAIN     PMY01                              60
     C                   MOVEL     MYDSC1        ENDSC1
     C                   MOVEL     MYDSC2        ENDSC2
     C                   MOVEL     MYDSC3        ENDSC3
     C     SCDSC1        IFNE      *BLANKS
     C                   MOVEL     SCDSC1        ENDSC1
     C                   MOVEL     SCDSC2        ENDSC2
     C                   MOVEL     SCDSC3        ENDSC3
     C                   ENDIF                                                  SCDSC1 IFNE *BLANKS
     C*
     C                   MOVEL     *BLANKS       ENBLIN
     C                   MOVEL     *BLANKS       ENALIN
     C                   MOVEL     ENEND#        QQEND#
      *
     C                   TIME                    DSDTTM
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ENDT
      *
     C                   MOVEL     DSTIME        ENTIME
     C                   MOVEL     Q1USER        ENUSER
     C                   MOVEL     Q1PGM         ENPGM
     C                   MOVE      'A'           ENSTAT
     C                   MOVEL     Q1USER        ENRUSR
      *
     C                   Z-ADD     ELPFDT        ENPFD1
     C                   WRITE     PEN01
      *
     C                   ENDIF                                                  MYPGM1=W0051R21
      *
     C                   READ      PMY02                                  60
     C                   ENDDO
      *
     C                   CALL      'W7051C1'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFYR             3
     C                   PARM                    Q1JOBN
     C                   PARM                    QQEND#            5
     C                   PARM                    OROUTQ
      *
     C                   ENDIF                                                  *IN50=*OFF
     C                   ENDIF                                                  *IN50=*OFF
      *
     C                   ENDSR                                                  ENDR
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
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(21)
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(23)
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(25)
     C                   PARM                    LENGTH           15 5
     C*
     C*  Override HCD Program File (WDHCL)
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(27)
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
     C*** begin add ***  04/20/22  ********************************
       Monitor;
          Open wxevp;
          Read pev0p;
          Dow not %eof;
             Delete pev0p;
             Read pev0p;
          Enddo;
          Close wxevp;
       On-Error;
       Endmon;
     C*** end add *****  04/20/22  ********************************
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
     C*** begin add ***  06/14/23  ***************************************
     C                   If        calcManP = 'Y'
     C                   Eval      QQOvr = 'Z'
     C                   Endif
     C*** end add *****  06/14/23  ***************************************
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
     C*** begin del ***  06/14/23  ***************************************
     C*****XXLOSS        IFEQ      'Y'
     C******             CALL      'W8010R'
     C******             PARM                    QQCO#
     C******             PARM                    QQFND
     C******             PARM                    QQFYR
     C******             PARM                    QQEMP#
     C******             PARM                    QQDIV
     C******             PARM                    QQDATE
     C******             PARM                    QQESCD
     C******             PARM                    QQOVR
     C******             PARM                    QQSRCD
     C******             PARM                    QQPRT
     C******             PARM                    QQAJCD
     C******             PARM                    QQQTE
     C******             PARM                    QQRNB1
     C******             ENDIF                                                  XXLOSS IFEQ
     C*** end del *****  06/14/23  ***************************************
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
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(22)
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(24)
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(26)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(28)
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
     C*** begin chg ***  04/20/22  ********************************************
     C*****              MOVE      EAARCD        QQAR
     C                   If        BSSEFL = 'E'
     C                   Move      EAARCD        QQAR
     C                   Else
     C                   Move      EAARCD        QQAR2
     C                   Endif
     C*** end chg *****  04/20/22  ********************************************
     C*
     C     BSFREQ        IFEQ      'T'
     C                   MOVE      'T'           EAEDIT
     C                   ELSE
     C                   MOVE      *BLANK        EAEDIT
     C                   ENDIF
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
     C                   MOVEL     SRDTO         EADT
     C*
     C     EAARCD        IFNE      *BLANKS
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   ENDIF
     C                   Z-ADD     EAPO01        SVDT7             7 0
     C*** begin add ***  04/20/22  *****************************************
     C*
     C* Write deposit record for estimating accounts.
     C*
     C                   If        xxDEPP <> 0 and BSSEFL = 'E'
     C                             and EXDACD <> *blanks
     C*
     C                   MOVEL(P)  EXDACD        XXKEY
     C     TBKY          CHAIN     PTT01                              60
     C                   Clear                   EAARCD
     C                   If        *in60 = *off
     C                   MOVEL     TTKEY         EAARCD
     C*** begin add ***  04/20/22  ********************************************
     C                   Move      EAARCD        QQAR2
     C*** end add *****  04/20/22  ********************************************
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   Endif
     C*
     C                   Z-ADD     XXDEPP        EAAM01
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
     C                   MOVEL     SRDTO         EADT
     C                   MOVE      *BLANK        EAEDIT
     C     EAARCD        IFNE      *BLANKS
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   Endif
     C*
     C                   Endif                                                  If xxDEPP <> 0
     C*
     C* Write tax deposit record for estimating accounts.
     C*
     C                   If        xxTXDU <> 0 and BSSEFL = 'E'
     C                             and EXDACD <> *blanks
     C*
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EAARTY
     C                   MOVE      *BLANKS       EADESC
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'D'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         EAARCD
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTDESC        EADESC
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-ADD     XXTXDU        EAAM01
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
     C                   MOVEL     SRDTO         EADT
     C                   MOVE      *BLANK        EAEDIT
     C     EAARCD        IFNE      *BLANKS
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C                   Endif
     C*
     C                   Endif                                                  If xxTXDU <> 0
     C*** end add *****  04/20/22  *****************************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
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
       //*** Begin Add *** 06/12/24 **********************************
       isSkipAR   = *Off;
       chrCatType = GetArCat(EAarCd);
       If isSRAs And (chrCatType = '1' Or chrCatType = '2' Or
                      chrCatType = '3' Or chrCatType = '4' Or
                      chrCatType = '5' Or chrCatType = '6' Or
                      chrCatType = '7');
          isSkipAR = *On;
       EndIf;

       // Don't Write Assessments if SR Policy and Assessments
       If Not isSkipAR;
       //*** End   Add *** 06/12/24 **********************************
     C                   WRITE     PEA02
       //*** Begin Add *** 06/12/24 **********************************
       EndIf;
       //*** End   Add *** 06/12/24 **********************************
     C*
     C                   Z-ADD     SVDT7         EAPO01
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  EAARCD IFNE
     C*
     C                   ENDIF                                                  *IN80 IFEQ
     C                   ENDSR                                                  UPDTE
     C*
     C*****************************************************************
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
     C*** begin add ***  04/20/22  *****************************************
     C                   PARM                    QQAR2             2
     C*** end add *****  04/20/22  *****************************************
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
     C     ELBSCD        ANDEQ     XXBSCD
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
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
     C                   MOVE      'A'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C     *IN50         DOWEQ     '0'
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDEQ     XXBSCD
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
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
     C*  THIS SECTION ACCUMULATES STATE TAX AND ASSESSMENT BILLINGS.
     C*
     C                   Z-ADD     *ZEROS        X1AMT1
     C                   MOVE      'F'           XXARTY
     C     EAKY          SETLL     PEA01
     C     EAKY2         READE     PEA01                                  50
     C  N50              EXCEPT    PEA01E
     C     *IN50         DOWEQ     '0'
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDEQ     XXBSCD
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXCURD
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDEQ     YRMO
     C     *IN50         OREQ      *OFF
     C     EAPODT        ANDLT     XXDTE2
     C     ELBSCD        ANDNE     XXBSCD
     C     YYMM          ANDLT     YRMO
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
     C                   MOVE      XXEMP#        QQEMP#
     C*
     C                   CALL      'W8060C'
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQEMP#
     C                   PARM                    QQEFDT
     C                   PARM                    QQQUE
     C                   PARM                    QQBCM1
     C                   PARM                    QQBCM2
     C                   PARM                    QQBCM3
     C                   PARM                    QQICM1
     C                   PARM                    QQICM2
     C                   PARM                    QQICM3
     C                   PARM                    QQDART
     C                   PARM                    QQRMCD
     C                   PARM                    QQPEND
     C                   PARM                    QQDSC1
     C                   PARM                    QQDSC2
     C                   PARM                    QQDSC3
     C*** begin add ***  04/20/22  **************************************
     C                   PARM                    QQSEMP
     C*** end add *****  04/20/22  **************************************
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*
     C* CHECK IF POLICY NEEDS TO RUN CALCULATIONS TWICE.
     C*
     C*****************************************************************
     C*
     C     CHKLC         BEGSR
     C*
     C     ELKY2         CHAIN     PEL03                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     SCKY          SETGT     PSC01
     C     SCKY1         READPE    PSC01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     S2PRBS        ANDGT     *ZERO
     C*
     C                   MOVE      'Y'           XXLOSS
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     ELKY2         READE     PEL03                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDSR                                                  CHKLC SUBR
     C*
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
     C*** beg   add ***  02/08/13  ************************************
     C*---------------------------------------------------------------
     C* Process deposit adjustments.
     C*---------------------------------------------------------------
     C     DepositAdj    BegSr
     C                   Z-ADD     *ZEROS        XXADJ            11 2
     C                   Z-ADD     *ZEROS        XXADJ2           11 2
     C*** begin add ***  04/20/22  ***************************************
       If skipDepAdj = *on;
          LeaveSr;
       Endif;

       xxab01 = GetDepPercent(BSsefl);

       Select;
       When BSsefl = 'E';
          currMaxDepPrem = currEstMaxDep;
       When BSsefl = 'S';
          currMaxDepPrem = currSrMaxDep;
       Endsl;

       Select;
       When prevBSsefl = 'E';
          prevMaxDepPrem = prevEstMaxDep;
       When prevBSsefl = 'S';
          prevMaxDepPrem = prevSrMaxDep;
       Endsl;

       Select;
       When F1dps = 'M';
          currDepPrem = GetLesserOf(currMaxDepPrem:ELmapr);
          prevDepPrem = GetLesserOf(prevMaxDepPrem:svMaPr);
          xxAdj = currDepPrem - prevDepPrem;
       When F1dps = 'S';
          currDepPrem = GetLesserOf(currMaxDepPrem:ELstpr);
          prevDepPrem = GetLesserOf(prevMaxDepPrem:svStPr);
          xxAdj = currDepPrem - prevDepPrem;
       When F1dps = 'N';
          currDepPrem = GetLesserOf(currMaxDepPrem:ELenmp);
          prevDepPrem = GetLesserOf(prevMaxDepPrem:svNmPr);
          xxAdj = currDepPrem - prevDepPrem;
       Other;
          currDepPrem = GetLesserOf(currMaxDepPrem:(ELanpr-Elprtd));
          prevDepPrem = GetLesserOf(prevMaxDepPrem:s2AnPr);
          xxAdj = currDepPrem - prevDepPrem;
       Endsl;
      *
      *  TURN ON THE DEPOSIT MODE BY YEAR FLAG (XXDMFL) IF THE POLICY YEAR
      *  IS DEFINED AS SUCH IN WDF1P
      *
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      *ON           XXDPYM            1
     C                   ELSE
     C                   MOVE      *OFF          XXDPYM            1
     C                   ENDIF
     C*** end add *****  04/20/22  ***************************************
     C*
     C                   Z-ADD     *ZEROS        XXARPR           11 2
     C*
     C                   MOVE      'H'           XXARTY
     C     EAKY3         CHAIN     PEA03                              50           ALL FYRS
     C     *IN50         DOWEQ     '0'
     C     EAFYR         ANDLE     XXFYR
     C*** begin chg ***  04/20/22  ***************************************
     C*****              ADD       EAAMT         XXARPR
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C     XXDPYM        OREQ      *ON
     C     EAFYR         IFEQ      XXFYR
     C                   ADD       EAAMT         XXARPR
     C                   ENDIF
     C                   OTHER
     C     EAFYR         IFLE      XXFYR
      *
      * DO NOT INCLUDE PRIOR YEAR DEPOSIT TRANSACTIONS IF THAT YEAR IS FLAGGED
      * AS A PER YEAR DEPOSIT MODE
      *
     C     F1KEY2        SETLL     PF101
     C     F1KEY2        READE     PF101                                  60
     C     F1DPYM        IFEQ      *BLANK
     C     EAFYR         ANDLT     XXFYR
     C                   ADD       EAAMT         XXARPR
     C                   ENDIF
      *
     C     EAFYR         IFEQ      XXFYR
     C                   ADD       EAAMT         XXARPR
     C                   ENDIF
      *
     C                   ENDIF
     C                   ENDSL
     C*** end chg *****  04/20/22  ***************************************
     C     EAKY3         READE     PEA03                                  50
     C                   END                                                    *IN50
     C*
     C*** begin add ***  04/20/22  ***************************************
     C                   Z-ADD     XXARPR        XXDEPB           11 2
     C* RESET GROUP DETAIL
     C*
     C     F1KY          SETLL     PF101
     C     F1KY          READE     PF101                                  60
     C*** end add *****  04/20/22  ***************************************
     C*
     C     XXAB01        IFNE      *ZEROS
     C     XXARPR        DIV       XXAB01        XXARPR
     C                   END
     C*
     C*** begin add ***  04/20/22  ***************************************
     C*****              ADD       XXADJ2        XXADJ
       xxAdj2 = PrevDepPrem - xxArPr;
     C     ELARSM        IFNE      'Y'
     C                   ADD       XXADJ2        XXADJ
     C                   ENDIF                                                  *IN67 IFEQ
     C*** end add *****  04/20/22  ***************************************
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD     XXADJ         XXSAVE           11 2
     C     XXADJ         IFLT      *ZERO
     C                   MULT      -1            XXADJ
     C                   END
     C                   MOVE      XXADJ         XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXADJ
     C                   END
     C                   MOVE      '00'          XXADJ
     C     XXSAVE        IFLT      *ZERO
     C     XXADJ         MULT      -1            XXADJ
     C                   END
     C*
     C     XXADJ         MULT(H)   XXAB01        XXWK90            9 0
     C     XXAB01        IFEQ      *ZERO
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  XXAB01 IFEQ
     C                   Z-ADD     XXWK90        SCTOT            11 2
     C                   MOVE      *BLANK        XXTHRS            1
     C     F1BVFL        IFNE      'Y'
     C     ELBSCD        ANDEQ     XXBSCD
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
     C                   Z-ADD     0             XXDEPA           11 2
     C                   MOVE      'H'           XXARTY
     C     EAKY2         CHAIN     PEA01                              50
     C     *IN50         DOWEQ     '0'
     C                   ADD       EAAMT         XXDEPA
     C     EAKY2         READE     PEA01                                  50
     C                   ENDDO                                                  *IN50
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
     C                   END                                                    *IN50
     C*
     C     RRTDFL        IFNE      'Y'
     C     XXTHRS        OREQ      'Y'
     C*
     C     RRTDFL        IFNE      'Y'
     C                   MULT      -1            XXTXDU
     C                   ELSE
     C     XXDEPA        MULT(H)   RRTXPC        XXWK90
     C                   MULT(H)   .01           XXWK90
     C     XXWK90        SUB       XXTXDU        XXTXDU
     C                   ENDIF                                                  RRTDFL IFNE
     C*
     C                   ELSE
     C*
     C     XXAB01        MULT(H)   XXADJ         XXWK90
     C     XXAB01        IFEQ      *ZERO
     C                   Z-SUB     XXARPR        XXWK90
     C                   ENDIF                                                  XXAB01 IFEQ
     C                   ADD       XXWK90        XXDEPA
     C                   MULT(H)   RRTXPC        XXWK90
     C                   MULT(H)   .01           XXWK90
     C     XXDEPA        MULT(H)   RRTXPC        XXTEMP           11 0
     C                   MULT(H)   .01           XXTEMP
     C     XXTEMP        SUB       XXTXDU        XXTXDU
     C*
     C                   ENDIF                                                  RRTDFL IFNE
     C*
     C                   ADD       XXTXDU        SCTOT
     C                   MOVE      '0'           *IN80
     C*** begin add ***  04/20/22  ************************************
     C     ELARSM        IFEQ      'Y'
     C*
     C     XXWAR         IFNE      'Y'
      /free
       xtDepDue = xtDepDue + xxDepp;
       xtDepTaxDue = xtDepTaxDue + xxTxDu;
      /end-free
     C                   Z-ADD     0             SCTOT
     C                   ELSE
      /free
       xxDepp = xtDepDue;
       xxTxDu = xtDepTaxDue;
      /end-free
     C     XXDEPP        ADD       XXTXDU        SCTOT
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C*** end add *****  04/20/22  ************************************
     C                   Z-ADD     0             X
     C                   MOVE      *OFF          *IN89
     C                   EndSr
     C*** end   add ***  02/08/13  ************************************
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
     p*** begin add ***  04/20/22  *******************************************
     P*--------------------------------------------------
     P* Procedure name: CalcMaxDeposit
     P* Purpose:        Populate estimated and self report max
     P*                 deposit premium amounts
     P* Returns:
     P* Parameter:      prevMaxDepAmt
     P* Parameter:      currMaxDepAmt
     P* Parameter:      prvMaxDepReq
     P*--------------------------------------------------
     P CalcMaxDeposit  B
     D CalcMaxDeposit  PI
     D  estMaxDepAmt                 11S 0
     D  srMaxDepAmt                  11S 0
     D  prvMaxDepReq                   n   const options(*nopass)

     D prvDepCap       S                   Like(EMdepCap) Inz
     D depPercent      S                   like(BSab01)
      /Free
       depPercent = GetDepPercent('E');
       If depPercent <> 0;
          Eval(h) estMaxDepAmt = BSmaxDep/depPercent;
       Endif;
       depPercent = GetDepPercent('S');
       If depPercent <> 0;
          Eval(h) srMaxDepAmt  = BSmaxDep/depPercent;
       Endif;
       // If the previous cap is requested then use the previous value
       If %parms > 2 And prvMaxDepReq = *On;
          SetGt  (EMco#:EMfnd:EMemp#:EMdiv:sysDt:sysTm) pemh00;
          ReadPE (EMco#:EMfnd:EMemp#:EMdiv) pemh00;
          If %Eof(wmemphl);
             prvDepCap = 0;
          Else;
             prvDepCap = EMHdepCap;
          EndIf;
       EndIf;
       Select;
       When EMdepcap = 0 And prvDepCap = 0;
       When prvDepCap <> 0;
          estMaxDepAmt = prvDepCap;
          srMaxDepAmt  = prvDepCap;
       Other;
          estMaxDepAmt = EMdepCap;
          srMaxDepAmt  = EMdepCap;
       Endsl;
      /End-Free
     P CalcMaxDeposit  E
     P*--------------------------------------------------
     P* Procedure name: GetLesserOf
     P* Purpose:        Returns lesser of max Deposit Amt
     P*                  and calculated Prem Dep Amt
     P* Parameter:      maxDepAmt
     P* Parameter:      calcPremAmt
     P*--------------------------------------------------
     P GetLesserOf     B
     D GetLesserOf     PI            11  2
     D  maxDepAmt                    11  0 const
     D  calcPremAmt                  11  2 const
      /Free
       Select;
       When maxDepAmt <> 0 and maxDepAmt < calcPremAmt;
          Return maxDepAmt;
       Other;
          Return calcPremAmt;
       Endsl;
      /End-Free
     P GetLesserOf     E
     P*--------------------------------------------------
     P* Procedure name: GetDepPercent
     P* Purpose:        Returns deposit percentage
     P* Returns:
     P* Parameter:      billPlanType => S or E
     P*--------------------------------------------------
     P GetDepPercent   B
     D GetDepPercent   PI                  like(BSab01)
     D  billPlanType                  1A   const
      /Free
       Select;
       When billPlanType = 'E';
          Select;
          When BSesdp <> 0;
             Return BSesdp/100;
          When BSesd2 <> 0;
             Return BSesd2/100;
          Other;
             Return *zero;
          Endsl;
       When billPlanType = 'S';
          Select;
          When BSab01 <> 0;
             Return BSab01;
          When BSab02 <> 0;
             Return BSab02;
          Other;
             Return *zero;
          Endsl;
       EndSl;
      /End-Free
     P GetDepPercent   E
      *================================================================
      * Skip Deposit Adjustment if renewal year exists and is flagged
      *  to adjust deposit
      *================================================================
     p SkipDepAdj      b
     d SkipDepAdj      pi              n
     d wasOpened       s               n
     d skipDepCalc     s               n
      /free
       If not %open(WDELL);
          Open Wdell;
          wasOpened = *on;
       Endif;
       xxColt = ' ';
       Open wmcol;
       Chain(n) (XXCO#:XXFND:XXEMP#) PCO00;
       If %found;
          xxColt = 'Y';
       Endif;
       Close wmcol;
       //Look for renewal year
       Setgt  (xxCo#:xxFnd:XXemp#:xxDiv) pel01;
       Readpe (xxCo#:xxFnd:XXemp#:xxDiv) pel01;
       Chain (ELco#:%editc(ELbscd:'X')) pbs01;
       Chain (xxCo#:xxFnd:ELfyr) pf101;
       Select;
       When xxColt = 'Y';
          skipDepCalc = *on;
       When ELfyr = xxFyr;  // There is no renewal year.
          skipDepCalc = *off;
       When F1bvfl <> 'Y' and F1brfl <> 'Y'; // The renewal year doesn't adjust deposit.
          skipDepCalc = *off;
       When BSsefl = 'S'; // renewal year is self reporting
          skipDepCalc = *on;
       When BSsefl = 'E' and (BSesdp <> 0 or BSesd2 <> 0); // renewal year is est with deposit
          skipDepCalc = *on;
       Endsl;
       // Reset files to current year being processed
       Chain (xxCo#:xxFnd:XXemp#:xxDiv:xxFyr) pel02;
       Chain (ELco#:%editc(ELbscd:'X')) pbs01;
       Chain (xxCo#:xxFnd:xxFyr) pf101;
       If wasOpened = *on;
          Close Wdell;
       Endif;
       Return skipDepCalc;
      /end-free
     p SkipDepAdj      e
     p*** end add *****  04/20/22  *******************************************
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
OVRDBF FILE(WDPPL) TOFILE(WAPPL)
DLTOVR FILE(WDPPL)
OVRDBF FILE(WMEPL1E) TOFILE(WAEPL1E)
DLTOVR FILE(WMEPL1E)
OVRDBF FILE(WMEPL6E) TOFILE(WAEPL6E)
DLTOVR FILE(WMEPL6E)
OVRDBF FILE(WDHCL3) MBR(REVISED) OVRSCOPE(*JOB)            11/10/10
DLTOVR FILE(WDHCL3) LVL(*JOB)                              11/10/10
** DAYS OF WEEK FOR ARRAY                                              ***
MOTUWETHFRSASUMOTUWETHFRSA
**
312831303130313130313031
