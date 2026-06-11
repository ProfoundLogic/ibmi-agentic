     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H Bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W8020R - CALCULATE DEDUCTIBLE BILL AMOUNT
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   THIS PROGRAM CALCULATES THE DEDUCTIBLE BILL AMOUNTS THAT    *
     H*   ARE LATER USED TO PRODUCE INVOICES IN THE PRINT DEDUCTIBLE  *
     H*   INVOICE PROGRAM (W7325R).  TO DETERMINE DEDUCTIBLE BILL     *
     H*   AMOUNTS FOR EACH EMPLOYER, THE TOTAL AMOUNT PAID ON ALL     *
     H*   CLAIMS FOR EACH FUND YEAR IS COMPARED WITH THE PER OCCURRENCE
     H*   /AGGREGATE LIMITS ESTABLISHED IN EMPLOYER DETAIL (WDELP) TO *
      //   DETERMINE IF THE DEDUCIBLE HAS BEEN MET.  IF THE DEDUCTIBLE *
     H*   HAS NOT BEEN MET, A RECORD IS WRITTEN TO THE DEDUCTIBLE     *
     H*   DETAIL FILE (WDDDP) WITH THE CLAIM NUMBER, EMPLOYER NUMBER, *
     H*   INVOICE NUMBER AND DATE, AMOUNT BILLED, AND BATCH NUMBER.   *
     H*   THIS RECORD IS THEN USED BY THE PRINT DEDUCTIBLE INVOICE    *
     H*   PROGRAM (W7325R) TO PRODUCE AN INVOICE FOR ANY EMPLOYERS    *
     H*   WITH A DEBIT/CREDIT DEDUCTIBLE BALANCE.                     *
     H* CALLED FROM:                                                  *
     H*   W8007C                                                      *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H* PROGRAMMER: C:24                                              *
     H* CREATED: 02/28/95               PROJECT: WC311                *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*  30  PRIMARY READ INDICATOR FOR RECORD FORMAT PEA02           *
     H*  40  PRIMARY READ INDICATOR FOR RECORD FORMAT PDD01           *
     H*  50  PRIMARY READ INDICATOR                                   *
     H*  60  GENERAL INDICATOR USED TO CHAIN TO FILES                 *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*                                                               *
     H*  FILES:                                                       *
     H*     WMFNL (PFN01): CO#/FND                                    *
     H*     WDF1L (PF101): CO#/FND/FYR                                *
     H*     WMCML14 (PCM01): CO#/FND/EMP#/DIV/FYR/ACDT/CASE           *
     H*     WMEML (PEM01): CO#/FND/EMP#/DIV                           *
     H*     WDELL (PEL01): CO#/FND/EMP#/DIV/FYR                       *
     H*     WDELL2 (PEL02): CO#/FND/EMP#/DIV/PFDT                     *
     H*     WDDDL1 (PDD01): CO#/FND/EMP#/DIV/FYR/CASE                 *
     H*     WDDDL  (PDD00): CO#/FND/FYR/CASE/BFYR/PODT                *
     H*     WDDDL2 (PDD03): CO#/FND/PODT                              *
     H*     WTABL (PTT01): CO#/KEY                                    *
     H*     WTDDL  (PDD0T): CO#/FND/FYR/CODE                          *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/06/25 C:300 Support: 118994,  Project: 146WKU (Prescient)  *
     H*                Automated collateral to deductible xfer error. *
     H*                Prescient Prod policies 101-26 & 321-1 are not *
     H*                setup for Collateral Processing and checkbox   *
     H*                'Apply Collateral to Deductible' is Not checked*
     H*                But It still Paid their deductibles via collateral*
     H*                                                               *
     H* 09/09/24 C:300 Project: 146WKU (Prescient)                    *
     H*                Collateral Phase 2: Spread over Multi-Employers*
     H*                Pull Collateral in following order:            *
     H*                1) Other divisions                             *
     H*                2) Other policy years                          *
     H*                3) Other policy years on other divisions       *
     H*                4) Other policies                              *
     H*                5) Other policy years on other policies        *
     H*                                                               *
     H* 06/24/24 C:300 Project: 146WKR (Prescient), Support: 117807   *
     H*                1) Update Batch total amount on linked batch   *
     H*                   records                                     *
     H*                2) Calculate Collateral amount for each division*
     H*                   using EArdiv and correct Employer-Div number*
     H*                3) Collateral batches being created with wrong *
     H*                   Employer Number                             *
     H*                4) Check for AR summarized flag and get state  *
     H*                   reporting division                          *
     H*                5) Check Amount is still positive and do not   *
     H*                   match the Posted Amount which is negative   *
     H*                                                               *
     H* 06/24/24 C:300 Project: 146WKR (Prescient)                    *
     H*                Collateral Processing                          *
     H*                                                               *
     H* 08/08/22 C:079 Project: WC2859                                *
     H*                Add Death Benefits PV1 Logic                   *
     H*                                                               *
     H* 09/11/21 C:079 Project: WC2663                                *
     H*                Look for invoice number in WMEAP               *
     H*                                                               *
     H* 03/25/21 C:079 Project: WC2663                                *
     H*                Invoice Cloud changes                          *
     H*                                                               *
     H* 09/03/20 C:180 Project: WC2733                                *
     H*                Add Include Medical/Indemnity Claims           *
     H*                processing.                                    *
     H*                                                               *
     H* 11/14/17 C:136 Support: 50916                                 *
     H*                Correct issue where refunds were being         *
     H*                calculated incorrectly.                        *
     H*                                                               *
     H* 12/07/15 C:136 Project: WC2393                                *
     H*                Correct 82805 which reimplements 81389. 82805  *
     H*                was causing the calculation to write billings  *
     H*                with the wrong amount to the wrong reserve     *
     H*                type.                                          *
     H*                                                               *
     H* 08/02/13 C:180 SUPPORT WA146 82268                            *
     H*                Fix error not billing flat canceled policy.    *
     H*
     H* 07/10/13 C:062 SUPPORT: WA025 82805                           *
     H*                Correct billing if amounts are negative.       *
     H*                                                               *
     H* 01/18/13 C:136 SUPPORT: WA025 81389                           *
     H*                Reduce deductible billings if aggregate amount *
     H*                is decreased                                   *
     H*                                                               *
     H* 10/10/12 C:136 SUPPORT: WA099 80909                           *
     H*               -Expand use of some of the internal calc fields *
     H*                to accommodate large deductible field sizes.   *
     H*               -Removed calls to SREFFD. Q1DATE wasn't being   *
     H*                used.                                          *
     H*                                                               *
     H* 05/10/11 C:62  Seq#: WA093/75944                              *
     H*                Don't back out net recoveries if gross policy. *
     H*                                                               *
     H* 01/14/11 C:62  Seq#: WA025/74447                              *
     H*                Correct claim allowance process.               *
     H*                                                               *
     H* 09/14/10 C:33  Seq#: WA093/73898                              *
     H*                Only remove payments after loss date if running*
     H*                 in month-end.                                 *
     H*                                                               *
     H* 07/09/10 C:33  Project: 93WIP Priss# 22                       *
     H*                Add decimal positions to the Amount/Occurrance.*
     H*                                                               *
     H* 06/25/10 C:189 Project: WC1878                                *
     H*                 Add Single Employer Processing to the         *
     H*                 Deductible Invoice Process                    *
     H*                                                               *
     H* 06/17/10 C:33  Project: 93WIP Priss# 22                       *
     H*                Ensure multi-claims are using correct values.  *
     H*                                                               *
     H* 05/19/10 C:62 PROJECT 72714                                   *
     H*               Remove payments after loss date                 *
     H*                                                               *
     H* 04/29/10 C:33 -Ensure the entire batch amount is stored.      *
     H*                                                               *
     H* 04/26/10 C:136 PROJECT: 25WQW PRISS 21                        *
     H*                WRITE A NEW A/R BATCH FOR EACH JOB WHEN        *
     H*                IN GROUP SELECT MODE (USEGRP = *ON)            *
     H*                                                               *
     H* 04/08/10 C:33  Project: 25WRB   Priss #26                     *
     H*               -Update initial maintenance fields.             *
     H*                                                               *
     H* 03/15/10 C:33  Project: 25WRB   Priss #16                     *
     H*               -Ensure an invoice # is written to WDEAP.       *
     H*               -Include invoice # in WDDDP.                    *
     H*               -Ensure correct batch # is used for each run.   *
     H*                                                               *
     H* 03/12/10 C:65  Project: WC1838                                *
     H*               -DO NOT UPDATE LARGE DEDUCTIBLE INFO IN FILE.   *
     H*                                                               *
     H* 02/01/10 C:33  Project: 25WRB                                 *
     H*               -Check deductibles against Claims Allowance for *
     H*                 CTL025                                        *
     H*               -Change to use group select processing          *
     H*                                                               *
     H* 01/11/10 C:33  Project: WC1838                                *
     H*                Process large deductibles.                     *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -For 025, load the invoice # when writing       *
     H*                 billing records                               *
     H*                                                               *
     H* 10/13/09  C:232 Support: WA139 69427                          *
     H*                 Project 126WAO, Should be flagged on client   *
     H*                 id 126 (OIC).                                 *
     H*                                                               *
     H* 09/22/09 C:234 PROJECT: 93WIB                                 *
     H*                Update the deductible fields with the details  *
     H*                from the deductible discount table file.       *
     H*                                                               *
     H* 09/23/08 C:136 SUPPORT: WA146 65315                           *
     H*                CLEAR WDEAP FIELDS BEFORE WRITING THE DUMMY    *
     H*                RECORD                                         *
     H*                                                               *
     H* 01/28/08  C:180  PROJECT WC1648                               *
     H*                  Fix triplication error in
     H*                  deductible billings.
     H*
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H* 09/24/07  C:190 Project 135WKC                                *
     H*                 Add new parameters to entry parm list.        *
     H*                                                               *
     H* 06/04/07 C:15  Support: WC135/58381                           *
     H*                Added code to prevent a claim from being       *
     H*                processed if it was transferred by the Group   *
     H*                to Group process within the current month.     *
     H*                                                               *
     H* 10/06/06 C:15  Support: WC135/55099                           *
     H*                Changed code to use the correct eff dt for the *
     H*                2nd deduct code.                               *
     H*                                                               *
     H*  10/05/06 C:65 PROJECT SUPP#55099                             *
     H*                CORRECT GROUP FIELD FOR SREFFD CALL.           *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     H*                                                               *
     H* 04/06/06 C:15  Support: WC065/52222                           *
     H*                Added code to verify that a deduct code exists *
     H*                on the policy, when comparing the accident dt  *
     H*                against the deduct eff and exp dates, to       *
     H*                prevent claims from being billed if they are   *
     H*                outside of the deduct effective period.        *
     H*                                                               *
     H* 09/26/05 C:61  PROJECT 135WED WC1336                          *
     H*                Change to enable process group range instead   *
     H*                of single group.                               *
     H*                                                               *
     H* 04/20/05 C:53 PROJECT: WC1292 135WCH                          *
     H*               ADD PROCESSING TO USE TOTAL INCURRED TO CALC    *
     H*               BILLING                                         *
     H*                                                               *
     H* 04/07/05      PROJECT: 126WDK                                 *
     H*          C:65 PROJECT: 126WAO                                 *
     H*               CREATE A PENDING PAYMENT BATCH IF THE INVOICES  *
     H*               ARE BEING RUN FOR A PEO POLICY GROUP.           *
     H*          C:33 PROJECT: 126WCD                                 *
     H*               MULTIPLE DEDUCTIBLE DISCOUNTS PER YEAR.         *
     H*                                                               *
     H* 10/12/04 C:15  Support: WC082/44097                           *
     H*                Fixed code that retrieved the ded tax billing  *
     H*                A/R code.                                      *
     H*                                                               *
     H* 04/13/04 C:132 PROJECT:  WC1190  WA109 42171                  *
     H*              - On ADD, populate Inital Date, User & Pgm       *
     H*                                                               *
     H* 11/04/03 C:136 PROJECT: WC1161                                *
     H*                Add code for new A/R category field in WMEAP.  *
     H*                                                               *
     H* 01/29/03 C:15  Project: WC1072                                *
     H*                Added code to handle multiple claim processing *
     H*                if the employer is on a 'per occurrence' plan. *
     H*                                                               *
     H* 06/29/02 C:136 Project: WC1080 (KY Deductible Billing Tax)    *
     H*                Add code to calculate and write deductible tax *
     H*                billing.                                       *
     H*                                                               *
     H*  06/24/02  C:15  Support: WC065/27578                         *
     H*                  Added code to reconcile amounts if an emp    *
     H*                  was removed from a deduct plan and had prior *
     H*                  billings, or a rsv was removed from a ded    *
     H*                  plan and billings were made on that rsv.     *
     H*                                                               *
     H*  05/31/02  C:15  Project: WC311                               *
     H*                  When adding deduct recoveries to the claim   *
     H*                  total paid, use the pmt type to determine    *
     H*                  whether to add or subtract the amount.       *
     H*                                                               *
     H*  12/07/01  C:15  Support: WC025/30227                         *
     H*                  Corrected code which checks if an employer   *
     H*                  is cancelled.                                *
     H*                                                               *
     H*  10/12/00  C:15  Support: 24559                               *
     H*                  Added code to create an A/R batch dummy      *
     H*                  record to lockin the bat# so it doesn't get  *
     H*                  used by any other A/R batches.               *
     H*                                                               *
     H*  04/19/99  C:15  Support:                                     *
     H*                  Changed deduct recovery routine to prevent   *
     H*                  converted payments from being added to the   *
     H*                  total deduct recovered amount.               *
     H*                                                               *
     H*  01/30/99  C:15 Support: WA065/15291                          *
     H*                -Changed pgm to ensure that the full amount of *
     H*                 a negative billing is applied to the deduct   *
     H*                 pending amount.                               *
     H*                -Fixed the routine which checks for claims     *
     H*                 which were transferred from one employer to   *
     H*                 another.                                      *
     H*                                                               *
     H*  12/24/98  C:15 Support: WA095/15211                          *
     H*                 Changed the billing calculation to use the    *
     H*                 payment detail file to accumulate the deduct  *
     H*                 recoveries on claims.                         *
     H*                                                               *
     H*  07/27/98  C:65 PROJECT: WC506                                *
     H*                 RESET CLAIM FYR IN CASE IT WAS CHANGED.       *
     H*                                                               *
     H*  03/04/98  C:65 PROJECT: WC506                                *
     H*                 USE CLAIM FYR FOR A/R BATCH AND DEDUC FILES.  *
     H*                                                               *
     H*  03/04/98  C:65 PROJECT: WC810                                *
     H*                 CHANGE FUND HIERARCHY FOR DEDUC CODE SPECIFIC *
     H*                                                               *
     H*****************************************************************
     FWMFNL     IF   E           K DISK
     F* FUND DETAIL
     FWDF1L     IF   E           K DISK
     F* STATISTICAL REPORTING/RATING FILE
     FWDRRL     IF   E           K DISK
     FWMCML14   IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELP     UF   E             DISK
     F                                     RENAME(PEL01:DBRCDF)
     FWDELL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     FWDELL2    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWDEAL     IF   E           K DISK
     FWDEAL4    UF A E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWDEAL5    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
     FWDDDL1    IF   E           K DISK
     FWDDDL     UF A E           K DISK
     F                                     RENAME(PDD01:PDD00)
     FWDDDL2    IF   E           K DISK
     F                                     RENAME(PDD01:PDD02)
     FWDDDL3    IF   E           K DISK
     F                                     RENAME(PDD01:PDD03)
     FWDDDL7    UF   E           K DISK
     F                                     RENAME(PDD01:PDD07)
     FWDDAL     UF A E           K DISK
     F                                     RENAME(PDA01:PDA00)
     FWDDAL1    IF   E           K DISK
     FWDDCL     IF A E           K DISK
     FWMRSL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PPA01)
     FWTDDL2    IF   E           K DISK
     F                                     RENAME(PDD01:PDD0T)
     FWMLDL1    IF   E           K DISK
     FWTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT00)
     FWACTL2    IF   E           K DISK
     FWDPAL     IF   E           K DISK
     FWAMCL     IF   E           K DISK
     FWAMCL1    IF   E           K DISK
     F                                     RENAME(PMC01:PMC02)
     FWAETL1    IF   E           K DISK
     FWXGSL     IF   E           K DISK
     F                                     RENAME(PGS01:PGS00)
     C*** Begin Add ***  06/24/24  *********************************************
     FWMEXL     IF   E           K DISK
     C*** End   Add ***  06/24/24  *********************************************
     C*** Begin Add ***  09/09/24  *********************************************
      * CO#/CUST
     FWMEML20   IF   E           K DISK    RENAME(PEM01:PEM20)
     F                                     PREFIX(E20)
      * CO#/FND/FYR/EMP#/DIV
     FWDELL1    IF   E           K DISK    RENAME(PEL01:PELL1)
     F                                     PREFIX(L1_)
      * CO#/FND/EMP#/FYR
     FWDELL8    IF   E           K DISK    RENAME(PEL01:PELL8)
     F                                     PREFIX(L8_)
     C*** End   Add ***  09/09/24  *********************************************
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SSY01)
     D PD              S             11  2 DIM(10)
     D BD              S             11  2 DIM(10)
     D IN              S              3  0 DIM(10)
     D FYR             S              3  0 DIM(99)
     D AMT             S             11  2 DIM(99)
     D RC              S              2    DIM(10)
     D RC2             S              2    DIM(10)
     D ST              S              2    DIM(99)                              RATING STATE
     D FDT             S              7  0 DIM(99)                              POLICY EFF DATE
     D DTXB            S              2    DIM(99)                              DED TAX AR CODE
     d*** Beg Add *** 08/08/22 *********************************************
     D PV1PD           S             11  2 DIM(10)                              DED TAX AR CODE
     d*** End Add *** 08/08/22 *********************************************
      *
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     d inKey           s             20
      /Copy WprAr
      /Copy SprModule
      /Copy WprPmtSvc
     D*** Begin Add ***  06/24/24  *********************************************
      /copy WprEmplr
     D*** End   Add ***  06/24/24  *********************************************
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D                 DS
     D  DC                     1     20
     D                                     DIM(10)
     D  DDDC01                 1      2
     D  DDDC02                 3      4
     D  DDDC03                 5      6
     D  DDDC04                 7      8
     D  DDDC05                 9     10
     D  DDDC06                11     12
     D  DDDC07                13     14
     D  DDDC08                15     16
     D  DDDC09                17     18
     D  DDDC10                19     20
     D                 DS
     D  LG1                    1     20
     D                                     DIM(10)
     D  LDR101                 1      2
     D  LDR102                 3      4
     D  LDR103                 5      6
     D  LDR104                 7      8
     D  LDR105                 9     10
     D  LDR106                11     12
     D  LDR107                13     14
     D  LDR108                15     16
     D  LDR109                17     18
     D  LDR110                19     20
     D                 DS
     D  LG2                    1     20
     D                                     DIM(10)
     D  LDR201                 1      2
     D  LDR202                 3      4
     D  LDR203                 5      6
     D  LDR204                 7      8
     D  LDR205                 9     10
     D  LDR206                11     12
     D  LDR207                13     14
     D  LDR208                15     16
     D  LDR209                17     18
     D  LDR210                19     20
     d*** Beg Add *** 08/08/22 ************************************************
     D  PV1                    1     20
     D                                     DIM(10)
     D  DDPV1DB01              1      2
     D  DDPV1DB02              3      4
     D  DDPV1DB03              5      6
     D  DDPV1DB04              7      8
     D  DDPV1DB05              9     10
     D  DDPV1DB06             11     12
     D  DDPV1DB07             13     14
     D  DDPV1DB08             15     16
     D  DDPV1DB09             17     18
     D  DDPV1DB10             19     20
     d*** End Add *** 08/08/22 ************************************************
     D DBFDS           DS
     D  @@RRN                397    400B 0
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     d ConvertDate     PR                  ExtPgm('SRCVTDAT')
     d  srDtI                         8  0
     d  srFFmt                        8    Const
     d  srTFmt                        8    Const
     d  srDtO                        10
     d  srErr                         1

     C*** Begin Add ***  09/09/24  *********************************************
     d inCo#           s                   like(ELco#)
     d inFyr           s                   like(ELfyr)
     d inCust          s                   like(EMcust)
     d outFnd          s                   like(ELfnd)
     d outEmp#         s                   like(ELemp#)
     d outFyr          s                   like(ELfyr)
     d outDiv          s                   like(ELdiv)
     d outColPmt       s                   like(EAamt)

     * Return Collateral Payment from Linked Policies to customer ID
     d RtnColPmt       pr
     d inCo#                               like(ELco#)
     d inFyr                               like(ELfyr)
     d inCust                              like(EMcust)
     d outFnd                              like(ELfnd)
     d outEmp#                             like(ELemp#)
     d outFyr                              like(ELfyr)
     d outDiv                              like(ELdiv)
     d outColPmt                           like(EAamt)
     C*** End   Add ***  09/09/24  *********************************************

       DCL-S backoutDeds   ind inz;
       DCL-S claimExcluded ind inz;
       DCL-S strDedSize char(1) inz;
     d*** Beg Add *** 08/08/22 *************************************
       dcl-s isIncluded Ind Inz(*off);
     d*** End Add *** 08/08/22 *************************************
     C*
     C*   KEY LIST FOR THE EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*   KEY LIST FOR THE A/R BATCH FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSTAT
     C*
     C     EAKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C                   KFLD                    XXFND
     C                   KFLD                    XXSVEM
     C                   KFLD                    XXSVDV
     C                   KFLD                    XXBFYR
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C*
     C     EAKY12        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X2PODT
     C                   KFLD                    XXACHF
     C                   KFLD                    XXSTAT
     C*
     C*  KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCD              3
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    X2PODT
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C*  KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     CMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C     CMKEY#        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X@EMP#
     C                   KFLD                    X@DIV
     C*
     C     CMKEY@        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X@EMP#
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE CLAIM FILE
     C*
     C     DCKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*  KEY LIST FOR THE DEDUCTIBLE DETAIL FILE
     C*
     C     DDKEY         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C     DDKEY1        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C     DDKY3         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C*
     C     DDKEY2        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXBFYR
     C                   KFLD                    X2PODT
     C                   KFLD                    XXRECD
     C*
     C     DDKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    X2PODT
     C*
     C*  KEY LIST FOR THE TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  KEY LIST FOR THE FUND DETAIL FILE
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXBFYR
     C*
     C     F1KY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
      *
      *  KEY LIST FOR RATING/REPORTING FILE
      *
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ST(Z)
     C                   KFLD                    FDT(Z)
      *
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    ST(Z)
     C*
     C*  KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKYT         KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
     C*
     C     DDKYT2        KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
     C                   KFLD                    X7DCEF
     C*
     C*  KEY LIST FOR THE CASE TRANSFER FILE
     C*
     C     CTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  Key list for the Payment Detail file
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C*
     C*  Key list for the Reinsurance Multiple Claim File
     C*
     C     MCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    XACASE
     C*
     C     MCKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    XXCLM#
     C*
      *
      *================================================================
      *  *LIKE DEFN
      *================================================================
     C     *LIKE         DEFINE    XXBAT#        SVBAT1
     C     *LIKE         DEFINE    XXBAT#        SVBAT2
     C     *LIKE         DEFINE    EABATA        XXTOTC
     C     *LIKE         DEFINE    DDCO#         XXCO#
     C     *LIKE         DEFINE    DDFND         XXFND
     C     *LIKE         DEFINE    DDST          XXST
     C     *LIKE         DEFINE    DDBFYR        XXBFYR
     C     *LIKE         DEFINE    DDBFYR        SVBFYR
     C     *LIKE         DEFINE    DDPODT        X2PODT
     C     *LIKE         DEFINE    DDRECD        XXRECD
     C     *LIKE         DEFINE    CMEMP#        XXSVEM
     C     *LIKE         DEFINE    CMDIV         XXSVDV
     C     *LIKE         DEFINE    CMFYR         XXSVFY
     C     *LIKE         DEFINE    TTKEY         XXKEY
     C     *LIKE         DEFINE    DDBAT#        XXBAT#
     C     *LIKE         DEFINE    DDSTAT        XXSTAT
     C     *LIKE         DEFINE    EADESC        XXDESC
     C     *LIKE         DEFINE    EASYCD        XXSYCD
     C     *LIKE         DEFINE    EABATA        XXBATA                         CHG TO 11,2
     C     *LIKE         DEFINE    EABATA        XXTOTB
     C     *LIKE         DEFINE    XXAMPO        XXAMTB                         CHG TO 11,2
     C     *LIKE         DEFINE    XXAMPO        XXTEST
     C     *LIKE         DEFINE    MCCASE        XACASE
     C     *LIKE         DEFINE    MCCLM#        XXCLM#
     C                   Z-ADD     0             XXAMPO           11 2
     C     *LIKE         DEFINE    ELDCDE        XXDDCD
     C     *LIKE         DEFINE    ELDCEF        X7DCEF
     C     *LIKE         DEFINE    ELDEDP        XXDEDP
     C     *LIKE         DEFINE    LDAMP1        X3AMPO
     C     *LIKE         DEFINE    LDAMG1        X3AMAG
     C     *LIKE         DEFINE    LDPLN1        XX2DDP
     C     *LIKE         DEFINE    LDAMP2        XX2APO
     C     *LIKE         DEFINE    LDAMG2        XX2AAG
     C     *LIKE         DEFINE    LDAMG1        X4AMAG
     C*** Begin Add ***  06/24/24  *********************************************
     C                   Z-ADD     *ZERO         XXCOLP            9 2          Collateral Payment
     C                   Z-ADD     *ZERO         XXCOLP2           9 2          Collateral Payment
     C     *LIKE         DEFINE    xxbat#        xxlnkbat#
     C     *LIKE         DEFINE    EXocol        svocol
     C     *LIKE         DEFINE    EABATA        XXTOTD
     C                   MOVE      *Blank        XXCOLAD           1            Apply Coll to Deduc
     C                   MOVE      *Blank        XXARSM            1            AR Summarized flag
     C                   MOVE      *Blank        XXSTRD            5 0          State Reporting Div
     C*** End   Add ***  06/24/24  *********************************************
     C*** Begin Add ***  09/09/24  *********************************************
     C     *LIKE         DEFINE    EMCUST        XXCUST                         Customer ID
     C                   Z-ADD     *ZERO         XXCOLP3           9 2          Collateral Payment
     C                   Z-ADD     *ZERO         XXCOLP4           9 2          Collateral Payment
     C*** End   Add ***  09/09/24  *********************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQPODT            8
     C                   PARM                    QQPRDT            8
     C                   PARM                    QQBAT#            9
     C                   PARM                    QQLSDT            8
     C                   PARM                    QQTFND            3
     C                   PARM                    QQFRGN            3
     C                   PARM                    QQTRGN            3
     C                   PARM                    QQDDTP            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
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
     C*
     C*
     C     INSTAL        PLIST
     C                   PARM                    @@WCIF            1
     C                   PARM                    @@MCIF            1
     C                   PARM                    @@WCIN            1
     C                   PARM                    @@AR              1
     C                   PARM                    @@NTWK            1
     C                   PARM                    @@UR              1
     C                   PARM                    @@MED             1
     C                   PARM                    @@EDI             1
     C                   PARM                    @@WCDK            1
     C                   PARM                    @@RHAB            1
     C                   PARM                    @@MSS             1
     C                   PARM                    @@FSCH            1
     C                   PARM                    @@PRVB            1
     C                   PARM                    @@NPWC            1
     C                   PARM                    @@OS              1
     C                   PARM                    @@ARLS            1
     C                   PARM                    @@CCOD            1
     C                   PARM                    @@CRVW            1
     C                   PARM                    @@PROM            1
     C                   PARM                    @@MCRV            1
     C                   PARM                    @@FREQ            1
     C                   PARM                    @@CRAC            1
     C                   PARM                    @@PREO            1
     C                   PARM                    @@PARN            1
     C                   PARM                    @@MGRI            1
     C                   PARM                    @@KCIF            1
     C                   PARM                    @@CURS            1
     C                   PARM                    @@PCIB            1
     C                   PARM                    @@CLT#            3
     C                   PARM                    @@UCPF            1
     C                   PARM                    @@DCF             1
     C                   PARM                    @@AUTB            1
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFRGN        XXFRGN            3 0
     C                   MOVE      QQTRGN        XXTRGN            3 0
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
      *
     C     Q1PRMS        IFGE      7
     C                   MOVE      QQTFND        XXTFND            3 0
     C                   ELSE
     C                   Z-ADD     *ZERO         XXTFND
     C                   ENDIF
      *
      *
     C                   Z-ADD     *ZERO         X@EMP#            9 0
     C                   Z-ADD     *ZERO         X@DIV             5 0
     C*
     C     Q1PRMS        IFGT      11
     C                   MOVE      QQEMP#        X@EMP#
     C                   MOVE      QQDIV         X@DIV
      *
     C*
     C                   SELECT
     C     QQDIV         WHENNE    *BLANKS
     C                   MOVE      *ON           PRCDIV            1
     C     QQEMP#        WHENNE    *ZERO
     C                   MOVE      *ON           PRCEMP            1
     C                   ENDSL
     C                   ENDIF
     C*
     C                   MOVE      QQPODT        XXPODT            8 0
     C*
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X2PODT
     C*
     C                   MOVE      QQPRDT        XXPRDT            8 0
     C                   MOVE      QQLSDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXLSDT            7 0
     C*
     C     *LIKE         DEFINE    QQDDTP        XXDDTP
     C                   MOVE      'S'           XXDDTP
     C     Q1PRMS        IFGE      10
     C     QQDDTP        ANDNE     *BLANK
     C                   MOVE      QQDDTP        XXDDTP
     C                   ENDIF
     C*
     C                   CLEAR                   WCFL43
     C     'WC'          CHAIN     SWC01                              60
     C                   SELECT
     C     WCFL43        WHENEQ    'Y'
     C                   MOVE      *ON           USELRG            1
     C                   OTHER
     C                   MOVE      *OFF          USELRG            1
     C                   ENDSL
     C*
     C                   CALL      'RTVCLTID'                           99
     C                   PARM                    CLTID             3
     C*
     C                   CALL      'SRCHKLIB'                           99
     C                   PARM      'PDSMTH'      ##LIB            10
     C                   PARM      '*USR'        ##TYPE            4
     C                   PARM      '1'           ##APSX            1
     C     MTHEND        PARM      '0'           ##FLAG            1
     C*
     C     *LIKE         DEFINE    *IN99         MTHEND
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   MOVEL     XXDATE        XXCYM1            5 0
     C                   MOVE      *ON           FIRST1            1
     C                   MOVE      *ON           FIRST2            1
     C                   CLEAR                   SVBAT1
     C                   CLEAR                   SVBAT2
     C                   CLEAR                   XXTOTB
     C                   CLEAR                   XXTOTC
     C*** Begin Add ***  06/24/24  *********************************************
     C                   CLEAR                   xxlnkbat#
     C*** End   Add ***  06/24/24  *********************************************
      *
     C                   MOVE      *OFF          USEGRP            1
     C     XXCO#         CHAIN     PGS00                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           USEGRP
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C* Use the group select groups
     C     USEGRP        WHENEQ    *ON
     C*
     C     XXCO#         CHAIN     PGS00                              60
     C*
     C     *IN60         DOUEQ     *ON
     C*
     C                   MOVE      *OFF          VLDGRP            1
     C                   Z-ADD     GSFND         XXFND
     C     FNKEY         CHAIN     PFN01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      *ON           VLDGRP
     C                   ENDIF
     C*
     C     XXTRGN        IFNE      0
     C*
     C     CMKEY         CHAIN     PEM01                              99
     C*
     C  N99EMRGN#        IFLT      XXFRGN
     C     EMRGN#        ORGT      XXTRGN
     C                   MOVE      *OFF          VLDGRP
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     VLDGRP        IFEQ      *ON
     C     GSIEFL        ANDEQ     'I'
     C                   EXSR      SRFND
     C                   ENDIF
     C*
     C     XXCO#         READE     PGS00                                  60
     C*
     C                   ENDDO
     C*
     C                   OTHER
     C*
     C* Use the group range to select groups
     C     XXFND         DOUGT     XXTFND
     C     FNKEY         CHAIN     PFN01                              60
      *
      *
     C     *IN60         IFEQ      *OFF
     C     XXTRGN        IFNE      0
     C     CMKEY         CHAIN     PEM01                              99
     C  N99EMRGN#        IFLT      XXFRGN
     C     EMRGN#        ORGT      XXTRGN
     C     XXFND         IFEQ      999
     C                   LEAVE
     C                   ENDIF
     C                   ADD       1             XXFND
     C                   ITER
     C                   ENDIF
     C                   ENDIF
     C                   EXSR      SRFND
     C                   ENDIF
      *
     C     XXFND         IFEQ      999
     C                   LEAVE
     C                   ENDIF
      *
     C                   ADD       1             XXFND
     C                   ENDDO                                                  FNKEY
     C*
     C                   ENDSL
      *
      *  UPDATE ALL BATCH RECORDS CREATED WITH THE TOTAL BATCH AMOUNT
      *
     C                   MOVE      SVBAT1        XXBAT#
     C                   SELECT
     C     USEGRP        WHENEQ    *ON
     C                   CLEAR                   XXTOTB
     C     EAKEY2        CHAIN(N)  PEA02                              60
     C     *IN60         DOWEQ     '0'
     C                   ADD       EAAMT         XXTOTB
     C     EAKEY2        READE(N)  PEA02                                  60
     C                   ENDDO
     C                   ENDSL
     C     EAKEY2        CHAIN     PEA02                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXTOTB        EABATA
     C                   UPDATE    PEA02
     C     EAKEY2        READE     PEA02                                  60
     C                   END
      *
     C                   MOVE      XXBAT#        QQBAT#
      *
      *  Delete the A/R batch dummy record.
      *
     C                   MOVE      *HIVAL        XXFND
     C                   MOVE      *HIVAL        XXSVEM
     C                   MOVE      *HIVAL        XXSVDV
     C                   MOVE      *HIVAL        XXBFYR
     C     EAKEY1        CHAIN     PEA02                              60
     C  N60              DELETE    PEA02
      *
     C*** Begin Add ***  06/24/24  *********************************************
      *  Update all linked A/R batch records with the total batch amount
      /Free
       If xxlnkbat# <> 0;
          XXTOTD = 0;
          Exec Sql Select Sum(EAamt) Into :XXTOTD From WDEAP
           Where EAco# = :xxCo# And EAbat# = :xxlnkbat#;

          Setll (xxCo#:xxlnkbat#) pea02;
          Reade (xxCo#:xxlnkbat#) pea02;
          Dow not %eof(wdeal4);
             EAbata = XXTOTD;
             Update pea02;
             Reade (xxCo#:xxlnkbat#) pea02;
          Enddo;
       Endif;
      /End-Free
     C*** End   Add ***  06/24/24  *********************************************

     C*** Begin Add ***  06/24/24  *********************************************
      *  Delete the Collateral linked A/R batch dummy record.
      /Free
       If xxlnkbat# <> 0;
          xxbat# = xxlnkbat#;
          Chain EAKEY1 pea02;
          If %found(wdeal4);
             Delete pea02;
          Endif;
          xxbat# = %dec(qqbat#:9:0);            // Restore original Batch numner
       Endif;
      /End-Free
     C*** End   Add ***  06/24/24  *********************************************
      *
      * Update Payment A/R batch with total amount.
      *
     C     SVBAT2        IFNE      *ZERO
     C                   MOVE      SVBAT2        XXBAT#
     C     EAKEY2        CHAIN     PEA02                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XXTOTC        EABATA
     C                   UPDATE    PEA02
     C     EAKEY2        READE     PEA02                                  60
     C                   ENDDO
      *
     C                   ENDIF
     C*
     C* Update Deductible Detail with the invoice #
     C     XXCLT#        IFEQ      '025'
     C     XXBAT#        ANDNE     *ZERO
     C*
     C     DDKY7         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBAT#
     C*
     C     DDKY7         CHAIN     PDD07                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     XXPODT        SRDTI
     C                   Z-ADD     *ZERO         XDCNT             3 0
     C*
     C     DDINV#        DOUNE     *ZERO
     C     XDCNT         ORGE      12
     C*
     C                   ADD       1             XDCNT
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         #IDT
     C*
     C                   CALL      'SRINV#'
     C                   PARM      DDCO#         #ICO#
     C                   PARM      DDFND         #IFND
     C                   PARM      DDEMP#        #IEMP#
     C                   PARM      DDDIV         #IDIV
     C                   PARM      DDFYR         #IFYR
     C                   PARM                    #IDT
     C     DDINV#        PARM                    #IINV#
     C                   PARM      '0'           #INEW
     C*
     C                   SELECT
     C* Monthly
     C     BSFREQ        WHENEQ    'M'
     C     BSFREQ        OREQ      'S'
     C                   CALL      'SRPMTH8'
     C                   PARM                    SRDTI
     C                   PARM      1             SRMNTH            3 0
     C                   PARM                    SRERR
     C* Weekly
     C     BSFREQ        WHENEQ    'W'
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDTI
     C                   PARM      7             SRDAYS            5 0
     C                   PARM                    SRERR
     C* Bi-Monthly
     C     BSFREQ        WHENEQ    'T'
     C                   MOVE      #IDT          XXDD              2 0
     C     XXDD          IFGE      16
     C                   MOVE      1             #IDT
     C                   ELSE
     C                   MOVE      16            #IDT
     C                   Z-ADD     #IDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDTI
     C                   CALL      'SRPMTH8'
     C                   PARM                    SRDTI
     C                   PARM      1             SRMNTH            3 0
     C                   PARM                    SRERR
     C                   ENDIF
     C* Others
     C                   OTHER
     C                   CALL      'SRPMTH8'
     C                   PARM                    SRDTI
     C                   PARM      1             SRMNTH            3 0
     C                   PARM                    SRERR
     C                   ENDSL
     C*
     C                   ENDDO
     C*
     C                   Z-ADD     DDPODT        DDINDT
     C*
     C                   UPDATE    PDD07
     C*
     C     DDKY7         READE     PDD07                                  60
     C*
     C                   ENDDO
     C*
     C                   ENDIF
      *
     C                   MOVE      *ON           *INLR
      *
     C*====================================================================
     C* Check claims allowance
     C*====================================================================
     C     CKALLW        BEGSR
     C*
     C     *LIKE         DEFINE    TOTLDA        XXDA1
     C     *LIKE         DEFINE    TOTLDA        XXDA2
     C*
     C                   MOVE      *IN60         SVIN60
     C*
     C                   EXSR      WRITDA
     C*
     C     LDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C                   CLEAR                   LDCLAL
     C     LDKY          CHAIN     PLD01                              60
     C*
     C     LDCLAL        IFNE      *ZERO
     C* Read thru WDDAP and accumulate total amounts
     C                   EXSR      SUMDA
     C     TOTLDA        ADD       XXAMTB        XXDA1
     C     TOTLDA        SUB       LDCLAL        XXDA2
     C                   SELECT
     C     XXDA1         WHENGT    LDCLAL
     C     XXDA2         ANDLE     *ZERO
     C                   ADD       TOTLDA        XXAMTB
     C                   SUB       LDCLAL        XXAMTB
     C     TOTLDA        WHENGE    LDCLAL
     C                   OTHER
     C                   CLEAR                   XXAMTB
     C                   ENDSL
     C                   ENDIF
     C*
     C                   MOVE      SVIN60        *IN60
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Update 'DA' maintenance fields
     C*====================================================================
     C     DAMNTF        BEGSR
     C*
     C                   ADD       1             DSTIME
     C*
     C                   MOVE      *BLANKS       DASTAT
     C                   MOVE      Q1USER        DAUSER
     C                   MOVE      Q1PGM         DAPGM
     C                   Z-ADD     DSTIME        DATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DADT
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Update 'EA' initial maintenance fields
     C*====================================================================
     C     EAIMNT        BEGSR
     C*
     C                   MOVEL     EAUSER        EAIUSR
     C                   MOVEL     EAPGM         EAIPGM
     C                   Z-ADD     EADT          EAIDT
     C* * * *              Z-ADDEATIME    EAITM
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Get the invoice # for billing records being created
     C*====================================================================
     C     GETINV        BEGSR
     C*
     C     *LIKE         DEFINE    EACO#         #ICO#
     C     *LIKE         DEFINE    EAFND         #IFND
     C     *LIKE         DEFINE    EAEMP#        #IEMP#
     C     *LIKE         DEFINE    EADIV         #IDIV
     C     *LIKE         DEFINE    EAFYR         #IFYR
     C     *LIKE         DEFINE    EAPODT        #IDT
     C     *LIKE         DEFINE    EAINV#        #IINV#
     C*
     C     XXCLT#        IFEQ      '025'
     C*
     C                   CLEAR                   TTTYPE
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTTYPE        IFEQ      'A'
     C     TTTYPE        OREQ      'B'
     C     TTTYPE        OREQ      'D'
     C     TTTYPE        OREQ      'F'
     C     TTTYPE        OREQ      'H'
     C*
     C                   CALL      'SRINV#'
     C                   PARM      EACO#         #ICO#
     C                   PARM      EAFND         #IFND
     C                   PARM      EAEMP#        #IEMP#
     C                   PARM      EADIV         #IDIV
     C                   PARM      EAFYR         #IFYR
     C                   PARM      EAPODT        #IDT
     C     EAINV#        PARM                    #IINV#
     C                   PARM      '1'           #INEW             1
     C*
     C                   ENDIF
       Else;
          // Get invoice#
          If IsModEnabled('Invoice Cloud') and IcGroupOpt(EAco#:EAfnd);
             EAinv# = 0;
             Exec Sql Select EAninv Into :EAinv# From WMEAP
              Where EAco#=:EAco# And EAfnd=:EAfnd And
              EAemp#=:EAemp# And EAdiv=:EAdiv And EAfyr=:EAfyr
              And EACTyp = 'R' And
              EApoDt=:EApoDt And EAarTy in ('F');

             If EAinv# = 0;
                inKey =  %EditC(EAco#:'X') + %EditC(EAfnd:'X')  +
                 %EditC(EAemp#:'X') + %EditC(EAdiv:'X');
                EAinv# = GetNext3rdPartySeq#(inKey);
             Endif;
          Endif;
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* RMVPMT - Remove paymemnts greater than the loss date
     C*===============================================================
     C     RMVPMT        BEGSR
     C*
     C* Only process when not running for month-end
     C     MTHEND        IFEQ      '0'
     C*
     C*
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATRDT        IFGT      XXLSDT
     C     PARECD        ANDEQ     RC(X)
     C     PAIPGM        ANDNE     'CONVL825'
     C     PATRDT        ORGT      XXLSDT
     C     PARECD        ANDEQ     RC(X)
     C     PAIPGM        ANDEQ     'CONVL825'
     C     PAPGM         ANDNE     ' '
     C*
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         PD(X)
     C                   ADD       PAAMT         XXPAID
     C                   ELSE
     C                   SUB       PAAMT         PD(X)
     C                   SUB       PAAMT         XXPAID
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  60
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDSR
      *===============================================================
      * SRFND - Process Records for a single group
      *===============================================================
     C     SRFND         BEGSR
     C                   CLEAR                   ED
     C                   CLEAR                   CD
     C                   CLEAR                   RT
     C                   CLEAR                   AR
     C                   CLEAR                   RR
     C                   CLEAR                   DC
     C                   CLEAR                   PD
     C                   CLEAR                   BD
     C                   CLEAR                   IN
     C                   CLEAR                   FYR
     C                   CLEAR                   AMT
     C                   CLEAR                   RC
     C                   CLEAR                   RC2
     C                   CLEAR                   LG1
     C                   CLEAR                   LG2
     C                   CLEAR                   ST
     C                   CLEAR                   FDT
     C                   CLEAR                   DTXB
      *
     C                   CLEAR                   XXPAID
     C                   CLEAR                   XXAMPO
     C                   CLEAR                   XXBATA
     C                   CLEAR                   XXAMTB
     C                   CLEAR                   XXTLAA
      *
     C                   MOVE      *ALL'0'       *IN
      *
      *  Process all groups in group range.
      *
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C                   CALL      'SA001R'      INSTAL
     C*
     C*  FIND A VALID SYSTEM TABLE CODE
     C*
     C     XXCO#         SETLL     PSY01
     C     *IN60         DOUEQ     *ON
     C     XXCO#         READE     PSY01                                  60
     C     SYTYPE        IFEQ      'Y'
     C     SYSTAT        ANDNE     'I'
     C                   MOVEL     SYKEY         XXSYCD
     C                   MOVE      *ON           *IN60
     C                   ENDIF
     C                   ENDDO
     C*
     C*** Begin Add ***  06/24/24  *********************************************
     C* Reserve an additional batch # and create a dummy A/R Batch record in WDEAP
     C* for the linked collateral billing offset batch
      /Free
       svocol = ' ';
       if xxlnkbat# = 0;
          Chain (xxCo#:xxFnd) pex01;
          If %found(wmexl);
             If EXocol = 'Y';             // Offer Collateral Flag
                Exsr RTVBAT;
                svocol = 'Y';
                xxlnkbat# = xxbat#;
                FIRST1 = *on;
             Endif;
          Endif;
       Endif;
      /End-Free
     C*** End   Add ***  06/24/24  *********************************************
     C*
     C*  CHECK TO SEE IF BATCH RECORDS ALREADY EXIST FOR THIS DEDUCTIBLE
     C*  INVOICING JOB.  IF SO, UPDATE CURRENT BATCH RECORDS OTHERWISE
     C*  RETRIEVE A NEW BATCH NUMBER
     C*
     C     *LIKE         DEFINE    EAACHF        XXACHF
     C                   SELECT
     C     USEGRP        WHENEQ    *ON
      *
      *
     C                   EXSR      RTVBAT
     C                   OTHER
     C     DDKEY3        CHAIN     PDD02                              60
     C     *IN60         IFEQ      '0'
     C     DDBAT#        ANDNE     *ZERO
     C                   Z-ADD     DDBAT#        XXBAT#
     C                   Z-ADD     DDBAT#        SVBAT1
     C                   ELSE
     C                   EXSR      RTVBAT
     C                   END                                                    DDKEY3  *IN60
     C                   ENDSL
     C*
     C                   Z-ADD     *ZEROS        XXSVEM
     C                   Z-ADD     *ZEROS        XXSVDV
     C                   Z-ADD     *ZEROS        XXSVFY
     C                   MOVE      *OFF          I@BILL            1
     C*
     C*  READ CLAIM FILE FOR ALL EMPLOYERS
     C*
     C                   SELECT
     C     PRCDIV        WHENEQ    '1'
     C     CMKEY#        CHAIN     PCM01                              50
     C*
     C     PRCEMP        WHENEQ    '1'
     C     CMKEY@        CHAIN     PCM01                              50
     C*
     C                   OTHER
     C     CMKEY         CHAIN     PCM01                              50
     C*
     C                   ENDSL
     C     *IN50         DOWEQ     '0'
     C*
     C                   Z-ADD     CMFYR         XXBFYR
     C                   MOVE      'Y'           XXDED1            1
     C                   MOVE      'Y'           X2DED1
     C     *LIKE         DEFINE    XXDED1        X2DED1
     C                   EXSR      VLDCLM
       // Determine If discount must be generated for excluded Claims
       backoutDeds =*Off;
       If claimExcluded = *On;
          Setll (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:CMcase) PDD01;
          If %equal(WDDDL1);
             backoutDeds = *On;
          Else;
             *in73 = *On;
          Endif;
       Endif;
     C   73              GOTO      NEXT

     C*
     C     CMEMP#        IFNE      XXSVEM
     C     CMDIV         ORNE      XXSVDV
     C                   EXSR      FYRBRK
     C   71              GOTO      NEXT
     C                   EXSR      EMPBRK
     C   72              GOTO      NEXT
     C                   ELSE
     C     CMFYR         IFNE      XXSVFY
     C                   EXSR      FYRBRK
     C   71              GOTO      NEXT
     C                   ADD       1             Z
     C                   Z-ADD     CMFYR         FYR(Z)
     C                   Z-ADD     XXPFDT        FDT(Z)
     C                   MOVE      XXST          ST(Z)
     C     F1KY1         CHAIN     PF101                              60
     C                   MOVE      F1DTXB        DTXB(Z)
     C                   END
     C                   END
     C                   Z-ADD     CMFYR         XXBFYR
     C*
     C     XX2NDD        IFEQ      'N'
     C                   Z-ADD     XXAMAG        X4AMAG
     C                   MOVEL     XXTYPE        DDTYPE
     C                   MOVE      X1VERB        DDVERB
     C     *LIKE         DEFINE    DDVERB        XXVERB
     C                   MOVE      X1VERB        XXVERB
     C                   Z-ADD     X1AMPO        XXAMPO
     C                   ENDIF
     C*
     C     XX2NDD        IFEQ      'N'
     C     ELDCD2        ANDNE     *BLANKS
     C     CMACDT        ANDGE     X2EFDT
     C     X2EXDT        IFEQ      0
     C     CMACDT        ORLT      X2EXDT
     C                   Z-ADD     X2AMAG        X4AMAG
     C                   MOVEL     X2TYPE        DDTYPE
     C                   MOVEL     X2DBTY        XXDBTY
     C                   MOVE      X2VERB        DDVERB
     C                   MOVE      X2VERB        XXVERB
     C                   Z-ADD     X2AMPO        XXAMPO
     C                   MOVE      'Y'           XX2NDD
     C                   MOVE      X2TOTI        XXTOTI
     C                   ENDIF
     C                   ENDIF
       // If claim was excluded due to not being the correct claim type,
       // then backout past dedcutible entries.
       If backoutDeds = *On;
          Exsr CalcExcClaimDisc;
       Else;
     C*
     C*  BYPASS CLAIM IF ACCIDENT DATE IS OUTSIDE EFFECTIVE DATE RANGE
     C*
     C     CMACDT        IFLT      X1EFDT
     C     CMACDT        ORGE      X1EXDT
     C     X1EXDT        ANDNE     *ZERO
     C     X1EFDT        OREQ      *ZERO
     C                   MOVE      'N'           XXDED1
     C                   ENDIF
     C*
     C     CMACDT        IFLT      X2EFDT
     C     CMACDT        ORGE      X2EXDT
     C     X2EXDT        ANDNE     *ZERO
     C     X2EFDT        OREQ      *ZERO
     C                   MOVE      'N'           X2DED1
     C                   ENDIF
     C*
     C*  ACCUMULATE PAYMENTS AGAINST DEDUCTIBLE RESERVE CODES
     C*
     C                   CLEAR                   XXPAID
     C                   CLEAR                   PD
     C*
     C     1             DO        10            X                 3 0
     C*
     C     RT(X)         IFNE      *BLANK
     C                   Z-ADD     1             Y                 3 0
     C     RT(X)         LOOKUP    DC(Y)                                  60
     C     *IN60         IFEQ      *ON
     C     XXTOTI        IFEQ      'Y'
     C                   ADD       RR(X)         PD(Y)
     C                   ADD       RR(X)         XXPAID
     C                   ADD       AR(X)         PD(Y)
     C                   ADD       AR(X)         XXPAID
     C                   ELSE
     C                   ADD       AR(X)         PD(Y)
     C                   ADD       AR(X)         XXPAID           11 2
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   EXSR      RMVPMT
     C                   ENDDO
     C*
     C*  Add deductible recoveries to the payment total.
     C*
     C     XXTYPE        IFEQ      'N'
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PATRDT        IFLE      XXLSDT
     C     PABR2         ANDNE     *BLANKS
     C     PABR2         IFEQ      FNMEP1
     C     PABR2         OREQ      FNCOP1
     C     PABR2         OREQ      FNEXP1
     C     PABR2         OREQ      FNLEP1
     C                   Z-ADD     1             Y
     C     PARECD        LOOKUP    DC(Y)                                  60
     C     *IN60         IFEQ      *ON
     C     PATYPE        IFEQ      'R'
     C     PATYPE        OREQ      'S'
     C     PATYPE        OREQ      'V'
     C                   ADD       PAAMT         PD(Y)
     C                   ADD       PAAMT         XXPAID           11 2
     C                   ELSE
     C                   SUB       PAAMT         PD(Y)
     C                   SUB       PAAMT         XXPAID           11 2
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     PAKY          READE     PPA01                                  60
     C                   END
     C                   ENDIF
       //*** Beg Add *** 08/08/22 *********************************************
       // Process Payment Value 1 Death Benefits payments.  Check Payment Value 1
       // code against each Death Benefit PV1 code setup for this deductible discount
       If CMdth = 'Y' and xxToti <> 'Y';
          Setll paky ppa01;
          Reade paky ppa01;
          Dow Not %Eof(WDPAL);
             // Find the payment reserve code in the claim file then get
             // then next available spot in the array to accumulate amounts
             y = %Lookup(PAbr2:PV1);
             If y > 0;
                y = %Lookup(PAreCd:DC);
                If y = 0;
                   y = %Lookup(*blanks:DC);
                Endif;
                If y < 10;
                   DC(y)  = PAreCd;
                   RC(y)  = PAreCd;
                   PD(y)  = PD(y) + PAamt;
                   xxPaid = xxPaid + PAamt;
                Endif;
             Endif;
             Reade paky ppa01;
          Enddo;
       Endif;
       //*** End Add *** 08/08/22 *********************************************
     C*
     C*  If the claim should not be billed deductibles, clear the paid
     C*  amounts so credits can be generated.
     C*
     C     XXDED         IFEQ      'N'
     C     X2DED         ANDEQ     'N'
     C     XXDED1        OREQ      'N'
     C     X2DED1        ANDEQ     'N'
     C                   CLEAR                   XXPAID
     C                   CLEAR                   PD
     C                   ENDIF
     C*
     C*  DETERMINE DEDUCTIBLE BILLING TYPE (PER OCCURRENCE VS. AGGREGATE)
     C*
     C                   Z-ADD     *ZEROS        XXAMTB
     C                   CLEAR                   BD
     C*
     C* ACCUMULATE DEDUCTIBLE BILLED AMOUNTS FOR EACH CLAIM
     C*  HANDLE CLAIM ALLOWNACE
     C*
     C                   CLEAR                   LDCLAL
     C     LDKY          CHAIN     PLD01                              99
     C     LDCLAL        IFNE      *ZEROS
     C     *IN99         ANDEQ     *OFF
      *
     C                   Z-ADD     *ZEROS        XXPAMT
     C     DCKY          CHAIN     PDC01                              99
     C     *IN99         DOWEQ     '0'
     C                   ADD       DCAMTB        XXPAMT
     C                   Z-ADD     1             Y
     C     DCRECD        LOOKUP    RC(Y)                                  99
     C     *IN99         IFEQ      *ON
     C                   ADD       DCAMTB        BD(Y)
     C                   ELSE
     C                   Z-ADD     1             Y
     C     *BLANKS       LOOKUP    RC(Y)                                  99
     C                   MOVE      DCRECD        RC(Y)
     C                   ADD       DCAMTB        BD(Y)
     C                   ENDIF
     C     DCKY          READE     PDC01                                  99
     C                   ENDDO
      *  Handle regular deductibles
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXPAMT           11 2
     C     DDKEY         CHAIN     PDD01                              60
     C     *IN60         DOWEQ     '0'
     C                   ADD       DDAMTB        XXPAMT
     C*
     C                   Z-ADD     1             Y
     C     DDRECD        LOOKUP    RC(Y)                                  60
     C     *IN60         IFEQ      *ON
     C                   ADD       DDAMTB        BD(Y)
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     1             Y
     C     *BLANKS       LOOKUP    RC(Y)                                  60
     C                   MOVE      DDRECD        RC(Y)
     C                   ADD       DDAMTB        BD(Y)
     C                   ENDIF
     C*
     C     DDKEY         READE     PDD01                                  60
     C                   END
     C                   ENDIF
     C*
     C*  If the employer is on a 'per occurrence' plan, as opposed to a
     C*  'per accident' plan, process the multiple claim subroutine.
     C*
     C     XXVERB        IFEQ      'O'
     C     XXAMPO        ANDGT     0
     C                   EXSR      MLTCLM
     C                   END
     C*
     C*  REDUCE THE TOTAL DEDUCTIBLE BILLED BY THE PREVIOUS DED BILLED
     C*  FOR THIS CLAIM. XXTLAA is the total of deductible billed for
     c*  the policy. XXPAMT is total billed for the claim. X4AMAG is the
     c*  aggregate amount for the policy.
     C*
     C                   SUB       XXPAMT        XXTLAA
     C*
     C*  PROCESS IF OCCURRENCE TYPE OR AGGREGATE LIMIT HAS NOT YET
     C*  BEEN MET
     C*
     C     XXDBTY        IFEQ      'O'
     C     XXDBTY        OREQ      'A'
     C     XXTLAA        ANDLT     X4AMAG
     C     XXDBTY        OREQ      'B'
     C     XXTLAA        ANDLT     X4AMAG
     C*
     C*  PERFORM CALCULATIONS IF BILLING TYPE IS PER OCCURANCE
     C*
     C     XXDBTY        IFEQ      'O'
     C     XXDBTY        OREQ      'B'
     C*
     C     XXPAID        IFGT      XXAMPO
     C                   Z-ADD     XXAMPO        XXAMTB
     C                   ELSE
     C                   Z-ADD     XXPAID        XXAMTB
     C                   END                                                    XXPAID > DDAMPO
     C*
     C                   END                                                    XXDBTY = O,B
     C*
     C*  PERFORM CALCULATIONS IF BILLING TYPE IS AGGREGATE OR BOTH
     C*
     C     XXDBTY        IFEQ      'A'
     C     XXDBTY        OREQ      'B'
      *
     C     XXDBTY        IFEQ      'A'
     C     XXTLAA        ADD       XXPAID        XXWK92           11 2
     C                   ELSE
     C     XXTLAA        ADD       XXAMTB        XXWK92
     C                   END
     C     XXWK92        IFGT      X4AMAG
     C     X4AMAG        SUB       XXTLAA        XXAMTB
     C                   ELSE
     C     XXDBTY        IFEQ      'A'
     C                   Z-ADD     XXPAID        XXAMTB
     C                   END
     C                   END
     C                   ADD       XXAMTB        XXTLAA           11 2
     C*
     C                   END                                                    XXDBTY = A,B
     C                   END                                                    AGGR LMT NOT MET
     C*
     C*  REDUCE AMOUNT BILLED BY ANY PREVIOUS DEDUCTIBLE BILLED AMOUNTS
     C*  ON THIS CLAIM
     C*
     C                   SUB       XXPAMT        XXAMTB
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   EXSR      CKALLW
     C                   ENDSL
     C                   ADD       XXAMTB        XXBATA
     C*
     C*  LOAD AN INDEX ARRAY (IN) TO MOVE THE NEGATIVE BILLED
     C*  RESERVE CODES TO THE FRONT.
     C*
     C                   CLEAR                   IN
     C                   Z-ADD     1             Y
     C     1             DO        10            X
     C     PD(X)         SUB       BD(X)         XXDIFF
     C     XXDIFF        IFLT      *ZERO
     C                   Z-ADD     X             IN(Y)
     C                   ADD       1             Y
     C                   ENDIF
     C                   ENDDO
     C*
     C     1             DO        10            X
     C     X             LOOKUP    IN                                     60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     X             IN(Y)
     C                   ADD       1             Y
     C                   ENDIF
     C                   ENDDO
     C*
     C*  LOOP THROUGH THE RESERVE CODES, AND DETERMINE THE AMOUNT
     C*  AVAILABLE FOR DEDUCTIBLE BILLING.
     C*
     C     1             DO        10            Y
     C                   Z-ADD     IN(Y)         X
     C     PD(X)         SUB       BD(X)         XXDIFF           11 2
      *
      * IF THE AMOUNT AVAILABLE FOR BILLING EXCEEDS THE REMAINING
      * AMOUNT TO BE BILLED, JUST BILL THE REMAINING AMOUNT.
      * ACCOUNT FOR POSSIBILITY THAT AGGREGATE MIGHT HAVE DECREASED.
      * (XXAMTB < 0)
      *
     C                   SELECT
     C     XXDIFF        WHENLT    *ZERO
     C     BD(X)         ADD       XXDIFF        XXTEST
     C     XXTEST        IFLE      *ZERO
     C                   Z-SUB     BD(X)         XXDIFF
     C                   ENDIF
     C     XXAMTB        WHENGE    *ZERO
     C     XXDIFF        ANDGT     *ZERO
     C     XXDIFF        IFGT      XXAMTB
     C                   Z-ADD     XXAMTB        XXDIFF
     C                   ENDIF
     C     XXAMTB        WHENLT    *ZERO
     C     BD(X)         ANDGT     *ZERO
     C     BD(X)         ADD       XXAMTB        XXTEST
     C     XXTEST        IFLE      *ZERO
     C                   Z-SUB     BD(X)         XXDIFF
     C                   ELSE
     C                   Z-ADD     XXAMTB        XXDIFF
     C                   ENDIF
     C                   ENDSL
     C*  IF THERE IS NO AVAILABLE BILLING AMOUNT, INCREMENT THE LOOP
     C     XXDIFF        IFEQ      *ZERO
     C                   ITER
     C                   ENDIF
     C*  WRITE THE BILLING RECORD FOR CLAIMS ALLOWANCE POLICIES
     C                   EXSR      WRITDC
     C*
     C*  WRITE DEDUCTIBLE DETAIL RECORD TO RECORD AMOUNT BILLED, AND
     C*  SUBTRACT AMOUNT BILLED FROM AMOUNT REMAINING TO BE BILLED.
     C*
     C                   CLEAR                   PDD00
     C                   MOVE      RC(X)         XXRECD
     C     DDKEY2        CHAIN     PDD00                              40
     C     *IN40         IFEQ      '1'
     C                   Z-ADD     CMCO#         DDCO#
     C                   Z-ADD     CMFND         DDFND
     C                   Z-ADD     XXBFYR        DDBFYR
     C                   Z-ADD     CMEMP#        DDEMP#
     C                   Z-ADD     CMDIV         DDDIV
     C                   Z-ADD     CMFYR         DDFYR
     C                   MOVE      CMCASE        DDCASE
     C*
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDPODT
     C*
     C                   Z-ADD     XXBAT#        DDBAT#
     C                   END
     C*
     C                   ADD       XXDIFF        DDAMTB
     C     DDTYPE        IFEQ      'N'
     C                   ADD       XXDIFF        DDAMTP
     C                   ENDIF
     C                   SUB       XXDIFF        XXAMTB
     C*
     C                   MOVE      *BLANKS       DDSTAT
     C                   MOVE      X             DDSORT
     C                   MOVE      RC(X)         DDRECD
     C                   MOVE      Q1USER        DDUSER
     C                   MOVE      Q1PGM         DDPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        DDTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDDT
     C*
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PDD00
     C                   ELSE
     C                   MOVE      Q1USER        DDIUSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DDIDT
     C*
     C     DDAMTB        IFNE      *ZEROS
     C                   WRITE     PDD00
     C                   END
     C                   END                                                    *IN40 = '0'
     C* KEEP TRACK OF BILLED AMOUNT PER FYR.
     C                   ADD       XXDIFF        AMT(Z)
     C*
     C*  IF THE AMOUNT REMAINING TO BILL IS ZERO, LEAVE THE LOOP
     C*
     C     XXAMTB        IFEQ      *ZERO
     C                   LEAVE
     C                   ENDIF
     C*
     C                   ENDDO                                                  X = 1 TO 10
       EndIf;
     C*
     C*  UPDATE WDELP WITH 1ST & 2ND DEDUCTIBLE INFO
     C     ELDCDE        IFNE      *BLANKS
     C     ELDCD2        ORNE      *BLANKS
     C     @@RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      XXDEDP        ELDEDP
     C                   MOVE      X3AMPO        ELAMPO
     C                   MOVE      X3AMAG        ELAMAG
     C                   MOVE      XX2DDP        EL2DDP
     C                   MOVE      XX2APO        EL2APO
     C                   MOVE      XX2AAG        EL2AAG
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C                   ENDIF
     C*
     C     NEXT          TAG
     C                   SELECT
     C     PRCDIV        WHENEQ    '1'
     C     CMKEY#        READE     PCM01                                  50
     C*
     C     PRCEMP        WHENEQ    '1'
     C     CMKEY@        READE     PCM01                                  50
     C*
     C                   OTHER
     C     CMKEY         READE     PCM01                                  50
     C*
     C                   ENDSL
     C*
     C                   END                                                    *IN50
     C*
     C*  WRITE A/R BATCH RECORD FOR PREVIOUS EMPLOYER
     C*
     C                   EXSR      ARBTCH
     C*
     C* CREATE A PENDING PAYMENT BATCH IF THIS IS A PEO POLICY GROUP.
     C*
     C     XXCLT#        IFEQ      '126'
     C     FNPEOF        IFNE      *BLANKS
     C*
     C                   MOVE      *BLANKS       XXRDPY            2
     C                   Z-ADD     0             XPBAT#            9 0
     C*
     C* RETRIEVE PAYMENT BATCH# THROUGH PAYMENT CODE AND POSTING DATE.
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'R'
     C     TTTYPE        ANDEQ     'Q'
     C                   MOVEL     TTKEY         XXRDPY
     C                   MOVEL(P)  TTDESC        XPDESC           40
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     FNKEY         SETGT     PEA03
     C     FNKEY         READPE    PEA03                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     EAARCD        IFEQ      XXRDPY
     C     EAPODT        ANDEQ     X2PODT
     C                   LEAVE
     C                   ENDIF
     C     FNKEY         READPE    PEA03                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EABAT#        XPBAT#
     C                   ENDIF
     C*
     C* IF THERE IS ALREADY A PAYMENT BATCH, DELETE BEFORE REWRITING.
     C*
     C                   Z-ADD     XXBAT#        SVBAT#            9 0
     C     XPBAT#        IFNE      *ZERO
     C                   Z-ADD     XXBAT#        SVBAT#
     C                   Z-ADD     XPBAT#        XXBAT#
     C*
     C     EAKEY2        CHAIN     PEA02                              60
     C     *IN60         DOWEQ     '0'
     C                   DELETE    PEA02
     C     EAKEY2        READE     PEA02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   Z-ADD     SVBAT#        XXBAT#
     C                   ENDIF                                                  XPBAT# <> 0
      *
     C     FIRST2        IFEQ      *ON
     C                   MOVE      *OFF          FIRST2
     C*
     C*  RETRIEVE NEXT AVAILABLE BATCH NUMBER
     C*
     C                   MOVE      *BLANKS       XXSTAT
     C     EAKEY         SETGT     PEA01
     C                   READP     PEA01                                  60
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL1            9 0
     C                   END
     C*
     C                   MOVE      'T'           XXSTAT
     C     EAKEY         SETGT     PEA01
     C                   READP     PEA01                                  60
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL2            9 0
     C                   END
     C*
     C     XXVAL1        IFGT      XXVAL2
     C                   Z-ADD     XXVAL1        XPBAT#
     C                   ELSE
     C                   Z-ADD     XXVAL2        XPBAT#
     C                   END
     C                   MOVE      XPBAT#        SVBAT2
     C                   ELSE
     C                   MOVE      SVBAT2        XPBAT#
     C                   ENDIF                                                  FIRST2
      *
     C*
     C* WRITE PENDING PAYMENT BATCH RECORD FROM BILLING BATCH RECORD.
     C*
     C     I@BILL        IFEQ      *ON
      *
     C     EAKEY2        CHAIN     PEA02                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     XPBAT#        EABAT#
     C*
     C     EAARCD        IFEQ      XXRDBL
     C                   MOVEL     XXRDPY        EAARCD
     C                   MOVEL(P)  XPDESC        EADESC
     C*
     C                   ELSE
     C*
     C                   Z-ADD     EAFYR         XXBFYR
     C     F1KEY         CHAIN     PF101                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     F1DTXP        ANDNE     *BLANKS
     C                   MOVEL     F1DTXP        EAARCD
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  TTDESC        EADESC
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  XXRDBL IFEQ
     C*
     C                   ADD       EAAMT         XXTOTC
     C                   MOVE      'D'           EAACHF
      *
     C     EAAMT         IFNE      *ZERO
     C                   EXSR      GETINV
     C                   EXSR      EAIMNT
     C                   WRITE     PEA02
     C                   ENDIF
     C*
     C*
     C     EAKEY2        READE     PEA02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   ENDIF                                                  I@BILL IFEQ
     C*
     C                   ENDIF                                                  FNPEOF IFEQ
     C                   ENDIF
     C*
     C                   ENDSR                                                  SRFND
     C*****************************************************************
     C* FYRBRK SUBROUTINE              ** SUBROUTINE **   F Y R B R K *
     C*****************************************************************
     C     FYRBRK        BEGSR
     C                   MOVE      *OFF          *IN71
     C*
     C                   MOVE      'Y'           XXDED             1
     C                   MOVE      'Y'           X2DED
     C                   MOVE      'N'           XX2NDD            1
     C*
     C*  CHECK TO SEE WHAT PLAN THE EMPLOYER IS USING FOR THIS
     C*  FUND YEAR
     C*
     C                   CLEAR                   ELDCD2
     C                   CLEAR                   EL2DEF
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C  N60              MOVE      ELRTCD        XXCD
     C  N60              Z-ADD     ELPFDT        XXPFDT            7 0
     C  N60RSKY          CHAIN     PRS01                              60
     C  N60              MOVE      RSJRST        XXST
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C                   CLEAR                   BSLPGM
     C     TBKEY         CHAIN     PBS01                              61
     C*
     C     *LIKE         DEFINE    LDEFD1        X1EFDT
     C     *LIKE         DEFINE    LDEXP1        X1EXDT
     C     *LIKE         DEFINE    LDEFD2        X2EFDT
     C     *LIKE         DEFINE    LDEXP2        X2EXDT
     C     *LIKE         DEFINE    LDVRB1        X1VERB
     C     *LIKE         DEFINE    LDVRB2        X2VERB
     C*
     C                   CLEAR                   RC
     C                   CLEAR                   RC2
     C*
     C                   CLEAR                   X1EFDT
     C                   CLEAR                   X1EXDT
     C                   CLEAR                   X2EFDT
     C                   CLEAR                   X2EXDT
     C*
     C                   CLEAR                   X1VERB
     C                   CLEAR                   X2VERB
     C*
     C     USELRG        IFEQ      *ON
     C                   SELECT
     C     XXDDTP        WHENEQ    'L'
     C     BSLPGM        ANDEQ     'L'
     C     XXDDTP        OREQ      'B'
     C     BSLPGM        ANDEQ     'L'
     C                   EXSR      YRBRKL
     C     XXDDTP        WHENEQ    'S'
     C     BSLPGM        ANDNE     'L'
     C     XXDDTP        OREQ      'B'
     C     BSLPGM        ANDNE     'L'
     C                   EXSR      YRBRKS
     C                   OTHER
     C                   MOVE      *ON           *IN71
     C                   ENDSL
     C                   ELSE
     C                   EXSR      YRBRKS
     C                   ENDIF
     C*
     C* Ensure the correct reserve codes are being used
     C                   SELECT
     C     CMACDT        WHENGE    X2EFDT
     C     X2EFDT        ANDGT     *ZERO
     C     CMACDT        ANDLT     X2EXDT
     C     CMACDT        ORGE      X2EFDT
     C     X2EFDT        ANDGT     *ZERO
     C     X2EXDT        ANDEQ     *ZERO
     C                   MOVEA     RC2           RC
     C                   MOVEA     RC2           DC
     C                   ENDSL
     C*
     C*  LOAD THE TOTAL AMOUNT OF DEDUCTIBLES ALREADY BILLED FOR THIS
     C*  EMPLOYER (Policy).
     C*
     C     DDKY3         CHAIN     PDD03                              60
     C     *IN60         DOWEQ     '0'
     C                   ADD       DDAMTB        XXTLAA
     C     DDKY3         READE     PDD03                                  60
     C                   END
     C*
     C     XXDED         IFEQ      'N'
     C     XXTLAA        ANDEQ     0
     C     CMKEY1        SETGT     PCM01
     C                   MOVE      *ON           *IN71
     C                   ENDIF
     C*
     C     *IN71         IFEQ      *OFF
     C                   Z-ADD     CMFYR         XXSVFY
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Year break subroutine - Large Deductbile
     C*====================================================================
     C     YRBRKL        BEGSR
     C*
     C                   CLEAR                   LDEFD1
     C                   CLEAR                   LDEXP1
     C                   CLEAR                   LDTOT1
     C                   CLEAR                   LDTYP1
     C                   CLEAR                   LDAMP1
     C                   CLEAR                   LDAMG1
     C                   CLEAR                   LDVRB1
     C                   CLEAR                   LDOCV1
     C*
     C                   CLEAR                   LDEFD2
     C                   CLEAR                   LDEXP2
     C                   CLEAR                   LDTOT2
     C                   CLEAR                   LDTYP2
     C                   CLEAR                   LDAMP2
     C                   CLEAR                   LDAMG2
     C                   CLEAR                   LDVRB2
     C                   CLEAR                   LDOCV2
     C*
     C                   CLEAR                   RC
     C                   CLEAR                   RC2
     C*
     C     ELKEY         CHAIN(N)  PLD01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      'N'           XXDED
     C                   MOVE      'N'           X2DED
     C                   ENDIF
     C*
     C                   Z-ADD     LDEFD1        X1EFDT
     C                   Z-ADD     LDEXP1        X1EXDT
     C                   MOVE      LDVRB1        X1VERB
     C                   MOVE      LDTOT1        XXTOTI
     C                   MOVE      LDTYP1        XXDEDP
     C                   MOVE      LDAMP1        X3AMPO
     C                   MOVE      LDAMG1        X3AMAG
     C*
     C*  LOAD DEDUCTIBLE BILLING TYPE FROM EMPLOYER DETAIL
     C*
     C                   SELECT
     C     LDAMP1        WHENEQ    *ZEROS
     C     LDAMG1        ANDEQ     *ZEROS
     C                   MOVE      'N'           XXDED
     C                   MOVE      'O'           XXDBTY
     C     LDAMP1        WHENNE    *ZEROS
     C     LDAMG1        ANDNE     *ZEROS
     C                   MOVE      'B'           XXDBTY
     C     LDAMP1        WHENNE    *ZEROS
     C     LDAMG1        ANDEQ     *ZEROS
     C                   MOVE      'O'           XXDBTY
     C                   OTHER
     C                   MOVE      'A'           XXDBTY
     C                   ENDSL
     C*
     C                   Z-ADD     *ZEROS        XXTLAA
     C                   Z-ADD     LDAMP1        XXAMPO
     C*
     C                   Z-ADD     LDAMP1        X1AMPO
     C                   Z-ADD     LDAMG1        XXAMAG
     C                   MOVEL     LDTYP1        XXTYPE
     C*
     C* 2nd Deductible Info
     C                   Z-ADD     LDEFD2        X2EFDT
     C                   Z-ADD     LDEXP2        X2EXDT
     C                   MOVE      LDVRB2        X2VERB
     C                   MOVE      LDTOT2        X2TOTI
     C                   MOVE      LDTYP2        XX2DDP
     C                   MOVE      LDAMP2        XX2APO
     C                   MOVE      LDAMG2        XX2AAG
     C*
     C                   SELECT
     C     LDAMP2        WHENEQ    *ZEROS
     C     LDAMG2        ANDEQ     *ZEROS
     C                   MOVE      'N'           X2DED
     C                   MOVE      'O'           X2DBTY
     C     LDAMP2        WHENNE    *ZEROS
     C     LDAMG2        ANDNE     *ZEROS
     C                   MOVE      'B'           X2DBTY
     C     LDAMP2        WHENNE    *ZEROS
     C     LDAMG2        ANDEQ     *ZEROS
     C                   MOVE      'O'           X2DBTY
     C                   OTHER
     C                   MOVE      'A'           X2DBTY
     C                   ENDSL
     C*
     C                   Z-ADD     LDAMP2        X2AMPO
     C                   Z-ADD     LDAMG2        X2AMAG
     C                   MOVEL     LDTYP2        X2TYPE
     C*
     C                   MOVEA     LG1           DC
     C                   MOVEA     LG1           RC
     C                   MOVEA     LG2           RC2
     C*
     C* Use Occupational vs. Non-occupational
     C                   MOVE      '  LC'        XXTBID
     C                   MOVEL     CMLSCV        XXKEY
     C                   CLEAR                   GTCF01
     C     GTKY          CHAIN     PGT00                              60
     C     3             SUBST     GTCF01:2      XXCOV             2
     C     XXCOV         IFEQ      '02'
     C                   MOVE      LDOCV1        X1VERB
     C                   MOVE      LDOCV2        X2VERB
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* Year break subroutine - Small Deductbile
     C*====================================================================
     C     YRBRKS        BEGSR
      *
     C                   CLEAR                   DDAMPO
     C                   CLEAR                   DDAMAG
     C                   CLEAR                   DDTYPE
     C*
     C                   Z-ADD     ELDCEF        X1EFDT
     C                   Z-ADD     ELDCEX        X1EXDT
     C                   Z-ADD     EL2DEF        X2EFDT
     C                   Z-ADD     EL2DEX        X2EXDT
     C                   MOVEL     ELDCDE        XXDDCD
     C                   Z-ADD     ELDCEF        X7DCEF
     C*
     C                   CLEAR                   DDVERB
     C*
     C  N60DDKYT2        SETGT     PDD0T
     C  N60DDKYT         READPE    PDD0T                                  60
     C   60              MOVE      'N'           XXDED
     C     *IN60         IFEQ      *OFF
     C     DDSTAT        ANDEQ     'A'
     C                   SELECT
     C     DDVERB        WHENEQ    ' '
     C                   MOVE      'O'           DDVERB
     C                   ENDSL
     C                   MOVE      DDTOTI        XXTOTI            1
     C* POPULATE DEDUCTION INFO FIELDS
     C                   MOVE      DDTYPE        XXDEDP
     C                   MOVE      DDAMPO        X3AMPO
     C                   MOVE      DDAMAG        X3AMAG
     C                   ELSE
     C                   MOVE      *BLANKS       XXTOTI
     C                   MOVE      *BLANKS       XXDEDP
     C                   MOVE      *BLANKS       X3AMPO
     C                   MOVE      *BLANKS       X3AMAG
     C                   ENDIF
     C*
     C*  LOAD DEDUCTIBLE BILLING TYPE FROM EMPLOYER DETAIL
     C*
     C     DDAMPO        IFEQ      *ZEROS
     C     DDAMAG        ANDEQ     *ZEROS
     C                   MOVE      'N'           XXDED
     C                   MOVE      'O'           XXDBTY
     C                   ELSE
     C     DDAMPO        IFNE      *ZEROS
     C     DDAMAG        ANDNE     *ZEROS
     C                   MOVE      'B'           XXDBTY            1
     C                   ELSE
     C     DDAMPO        IFNE      *ZEROS
     C     DDAMAG        ANDEQ     *ZEROS
     C                   MOVE      'O'           XXDBTY
     C                   ELSE
     C                   MOVE      'A'           XXDBTY
     C                   END
     C                   END
     C                   END
     C                   Z-ADD     *ZEROS        XXTLAA
     C                   CLEAR                   RC
     C                   MOVEA     DC            RC
     C                   Z-ADD     DDAMPO        XXAMPO
     C                   MOVE      DDVERB        X1VERB
     C     *LIKE         DEFINE    DDTYPE        XXTYPE
     C     *LIKE         DEFINE    DDTYPE        X2TYPE
     C     *LIKE         DEFINE    LDAMP1        X1AMPO
     C     *LIKE         DEFINE    LDAMG1        XXAMAG
     C     *LIKE         DEFINE    LDAMP2        X2AMPO
     C     *LIKE         DEFINE    LDAMG2        X2AMAG
     C*
     C                   Z-ADD     DDAMPO        X1AMPO
     C                   Z-ADD     DDAMAG        XXAMAG
     C                   MOVEL     DDTYPE        XXTYPE
     C*
     C                   CLEAR                   DDAMPO
     C                   CLEAR                   DDAMAG
     C                   CLEAR                   DDTYPE
     C*
     C                   MOVEL     ELDCD2        XXDDCD
     C                   Z-ADD     EL2DEF        X7DCEF
     C*
     C                   CLEAR                   DDVERB
     C*
     C     DDKYT2        SETGT     PDD0T
     C     DDKYT         READPE    PDD0T                                  60
     C*
     C   60              MOVE      'N'           X2DED
     C     *IN60         IFEQ      *OFF
     C     DDSTAT        ANDEQ     'A'
     C                   SELECT
     C     DDVERB        WHENEQ    ' '
     C                   MOVE      'O'           DDVERB
     C                   ENDSL
     C                   MOVE      DDTOTI        X2TOTI            1
     C* POPULATE DEDUCTION INFO FIELDS
     C                   MOVE      DDTYPE        XX2DDP
     C                   MOVE      DDAMPO        XX2APO
     C                   MOVE      DDAMAG        XX2AAG
     C                   ELSE
     C                   MOVE      *BLANKS       X2TOTI
     C                   MOVE      *BLANKS       XX2DDP
     C                   MOVE      *BLANKS       XX2APO
     C                   MOVE      *BLANKS       XX2AAG
     C                   ENDIF
     C*
     C*  LOAD DEDUCTIBLE BILLING TYPE FROM EMPLOYER DETAIL
     C*
     C     *LIKE         DEFINE    XXDED         X2DED
     C     *LIKE         DEFINE    XXDBTY        X2DBTY
     C*
     C                   SELECT
     C     DDAMPO        WHENEQ    *ZEROS
     C     DDAMAG        ANDEQ     *ZEROS
     C                   MOVE      'N'           X2DED
     C                   MOVE      'O'           X2DBTY
     C     DDAMPO        WHENNE    *ZEROS
     C     DDAMAG        ANDNE     *ZEROS
     C                   MOVE      'B'           X2DBTY
     C     DDAMPO        WHENNE    *ZEROS
     C     DDAMAG        ANDEQ     *ZEROS
     C                   MOVE      'O'           X2DBTY
     C                   OTHER
     C                   MOVE      'A'           X2DBTY
     C                   ENDSL
     C*
     C                   Z-ADD     DDAMPO        X2AMPO
     C                   Z-ADD     DDAMAG        X2AMAG
     C                   MOVEL     DDTYPE        X2TYPE
     C                   MOVEA     DC            RC2
     C                   MOVE      DDVERB        X2VERB
     C*
     C***LOAD*THE TOTAL AMOUNT OF DEDUCTIBLES ALREADY BILLED FOR THIS
     C***EMPLOYER.
     C**********
     C**********
     C                   ENDSR
      *
      *
      *
     C*****************************************************************
     C* EMPBRK SUBROUTINE              ** SUBROUTINE **   E M P B R K *
     C*****************************************************************
     C     EMPBRK        BEGSR
     C                   MOVE      *OFF          *IN72
     C*
     C*  CHECK TO SEE IF THE EMPLOYER IS CANCELLED
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      '1'
     C     CMKEY1        SETGT     PCM01
     C                   MOVE      *ON           *IN72
     C                   GOTO      EMBK1
     C                   END
     C*
     C                   Z-ADD     1             X                 3 0
     C     *ZEROS        LOOKUP    ED(X)                                  60
     C  N60              Z-ADD     5             X
     C   60              SUB       1             X
     C*
     C     X             IFNE      0
     C     CD(X)         ANDNE     0
     C     CD(X)         ANDLE     X2PODT
     C     ELCXBL        ANDEQ     'N'
     C     CMKEY1        SETGT     PCM01
     C                   MOVE      *ON           *IN72
     C                   GOTO      EMBK1
     C                   END
     C*
     C                   MOVE      *OFF          *IN76
     C*
     C     X             IFNE      0
     C     CD(X)         ANDNE     0
     C     CD(X)         ANDLE     X2PODT
     C     ELPFDT        ANDEQ     ELPTDT
     C     ELCXBL        ANDEQ     'Y'
     C                   MOVE      *ON           *IN76
     C                   ENDIF
     C*
     C*  DETERMINE THE BILLING FUND YEAR FOR THE EMPLOYER
     C*
     C     ELKY1         SETGT     PEL02
     C*
     C     *IN60         DOUEQ     *ON
     C     ELKY2         READPE    PEL02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     ELPFDT        IFNE      ELPTDT
     C     *IN76         OREQ      *ON
     C                   Z-ADD     ELFYR         SVBFYR
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  ELPFDT <> ELPTDT
     C*
     C                   ELSE
     C     CMKEY1        SETGT     PCM01
     C                   MOVE      *ON           *IN72
     C                   GOTO      EMBK1
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C                   ENDDO                                                  *IN60 DOU *ON
     C*
     C*  CHAIN BACK TO EMPLOYER DETAIL USING THE CLAIM FUND YEAR
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C*  WRITE A/R BATCH RECORD FOR PREVIOUS EMPLOYER
     C*
     C                   EXSR      ARBTCH
     C*
     C                   Z-ADD     CMEMP#        XXSVEM
     C                   Z-ADD     CMDIV         XXSVDV
     C*** Begin Add ***  06/24/24  *********************************************
      /Free
       XXCOLAD = ELcolad;                                 // Apply Collateral to Deductible
       XXARSM = ELarsm;                                   // AR Summarized flag
       XXSTRD = GetStRptDv(ELco#:ELFnd:ELEmp#);           // State Reporting Div
       //*** Begin Add ***  09/09/24  *********************************************
       XXCUST = EMcust;                                   // Customer ID
       //*** End   Add ***  09/09/24  *********************************************
      /End-Free
     C*** End   Add ***  06/24/24  *********************************************
     C*
     C                   Z-ADD     0             Z                 3 0
     C                   CLEAR                   FYR
     C                   CLEAR                   AMT
     C                   CLEAR                   ST
     C                   CLEAR                   FDT
     C                   CLEAR                   DTXB
     C*
     C                   ADD       1             Z
     C                   Z-ADD     CMFYR         FYR(Z)
     C                   Z-ADD     XXPFDT        FDT(Z)
     C                   MOVE      XXST          ST(Z)
     C     F1KY1         CHAIN     PF101                              60
     C                   MOVE      F1DTXB        DTXB(Z)
     C*
     C     EMBK1         TAG
     C                   ENDSR
      *
      *
      *
     C*****************************************************************
     C* ARBTCH SUBROUTINE              ** SUBROUTINE **   A R B T C H *
     C*****************************************************************
     C     ARBTCH        BEGSR
     C*
     C*  WRITE A/R BATCH RECORD FOR THE EMPLOYER, IF THIS IS NOT THE FIRST
     C*  TIME THRU
     C*
     C     XXSVEM        IFNE      *ZEROS
     C                   Z-ADD     1             Z
     C                   Z-ADD     FYR(Z)        XXBFYR
     C     XXBFYR        DOWNE     0
     C                   Z-ADD     *ZEROS        EAAMT
     C                   Z-ADD     *ZEROS        EACKAM
     C*** Begin Add ***  06/24/24  *********************************************
      /Free
       //*** Begin Add ***  05/06/25  *********************************************
       // Initialize before IF condition
       XXCOLP = 0;
       XXCOLP2 = 0;
       //*** End   Add ***  05/06/25  *********************************************
       // This subroutine executes for previous Employer not current
       If XXCOLAD = 'Y';
          //*** Begin Del ***  05/06/25  *********************************************
          // XXCOLP = 0;
          // XXCOLP2 = 0;
          //*** End   Del ***  05/06/25  *********************************************
          // Replace CMdiv with XXSVDV , CMemp# with XXSVEM in both of these SQL
          // Run different SQL
          // for AR summarized policies - using State Reporting Division
          // for Non AR summarized policies - using Division being processed
          Chain (xxCo#:xxFnd) pex01;
          If %found(wmexl);
             If XXARSM = 'Y';
                // Collateral Payment
                Exec Sql Select Sum(EAamt) Into :XXCOLP From WMEAP
                   Where EAco# = :xxCo# And EAfnd = :xxFnd And EAemp# = :XXSVEM
                   And EAdiv = :XXSTRD and EAfyr = :XXBFYR and (EAarcd = :EXcolp
                   or EAarcd = :EXcolt);
                // Collateral Payment from A/R Batch that hasn't transferred yet
                Exec Sql Select Sum(EAamt) Into :XXCOLP2 From WDEAP
                   Where EAco# = :xxCo# And EAfnd = :xxFnd And EAemp# = :XXSVEM
                   And EAdiv = :XXSTRD and EAfyr = :XXBFYR and (EAarcd = :EXcolp
                   or EAarcd = :EXcolt) and EAstat <> 'T';
             Else;
                // Collateral Payment
                Exec Sql Select Sum(EAamt) Into :XXCOLP From WMEAP
                   Where EAco# = :xxCo# And EAfnd = :xxFnd And EAemp# = :XXSVEM
                   And EAdiv = :XXSVDV and EAfyr = :XXBFYR and (EAarcd = :EXcolp
                   or EAarcd = :EXcolt);
                // Collateral Payment from A/R Batch that hasn't transferred yet
                Exec Sql Select Sum(EAamt) Into :XXCOLP2 From WDEAP
                   Where EAco# = :xxCo# And EAfnd = :xxFnd And EAemp# = :XXSVEM
                   And EAdiv = :XXSVDV and EAfyr = :XXBFYR and (EAarcd = :EXcolp
                   or EAarcd = :EXcolt) and EAstat <> 'T';
             Endif;

             XXCOLP = XXCOLP + XXCOLP2;
          Endif;
       Endif;
      /End-Free
     C*** End   Add ***  06/24/24  *********************************************
     C*
     C*  RETRIEVE THE DEDUCTIBLE BILLING CODE THAT WILL BE USED WHEN THE
     C*  A/R BATCH RECORD IS WRITTEN
     C*
     C*
     C*  GET THE DEDUCTIBLE BILLING DESCRIPTION
     C*
     C                   MOVE      *BLANK        XXRDTP
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVE      *BLANKS       XXRDBL            2
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'R'
     C     TTTYPE        ANDEQ     'F'
     C     TTKEY         ANDNE     DTXB(Z)
     C                   MOVEL     TTKEY         XXRDBL
     C                   MOVE      TTTYPE        XXRDTP            1
     C                   MOVE      TTDESC        XXDESC
      *
      * Prevent the tax ded  AR code from being picked up if DTXB,Z = *blanks
      *
     C     LO:UP         XLATE(P)  XXDESC        U1PDSC
     C                   MOVEL(P)  'TAX'         U2PDSC
      *
     C                   EXSR      STRING
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     EAKEY1        CHAIN     PEA02                              30
     C*
     C     *IN30         DOWEQ     *OFF
     C     EAARCD        ANDNE     XXRDBL
     C     EAKEY1        READE     PEA02                                  30
     C                   ENDDO                                                  *IN30 = *OFF
     C*
     C     *IN30         IFEQ      '1'
     C     EATFFL        OREQ      'Y'
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXBAT#        EABAT#
     C                   Z-ADD     XXSVEM        EAEMP#
     C                   Z-ADD     XXSVDV        EADIV
     C*** OVERRIDE DIV IF PEO & SPECIFIC CLIENT
     C*
     C     FNPEOF        IFNE      *BLANKS
     C     @@CLT#        ANDEQ     '135'
     C                   Z-ADD     *ZEROS        EADIV
     C                   Z-ADD     XXSVDV        EARDIV
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      XXSYCD        EASYCD
     C                   MOVE      'N'           EATFAP
     C                   MOVE      'N'           EATFFL
     C                   Z-ADD     *ZEROS        EATFDT
     C                   Z-ADD     *ZEROS        EATFAM
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXBFYR        EAFYR
     C                   MOVEL     XXRDBL        EAARCD
     C                   MOVE      'R'           EACTYP
     C                   MOVEL     XXDESC        EADESC
     C                   Z-ADD     *ZEROS        EABATA
     C                   Z-ADD     0             EAAMT
     C                   Z-ADD     0             EACKAM
     C                   END
     C                   ADD       AMT(Z)        EAAMT
     C                   ADD       AMT(Z)        EACKAM
     C                   ADD       EAAMT         XXTOTB
     C                   Z-ADD     *ZEROS        XXBATA
     C*
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   MOVE      'D'           EAACHF
     C*
     C*  IF THE A/R BATCH RECORD ALREADY EXISTS AND HAS NOT BEEN TRANSFERRED
     C*  THAN UPDATE OTHERWISE WRITE A NEW RECORD
     C*
     C     *IN30         IFEQ      '0'
     C     EATFFL        ANDNE     'Y'
     C                   UPDATE    PEA02
     C                   ELSE
     C     EAAMT         IFNE      *ZEROS
     C                   EXSR      GETINV
     C                   EXSR      EAIMNT
      // Get Invoice Number
     C                   WRITE     PEA02
     C                   MOVE      *ON           I@BILL
     C                   END
     C                   END
     C*
     C*** Begin Add ***  09/09/24  *********************************************
     C                   z-add     *zeros        xxEAamtleft       9 2          Deductible amt left
     C                   z-add     *zeros        svEAbat#          9 0          Save Batch#
       xxEAamtleft = EAamt;              // Move Deductible Amount
     C*** End   Add ***  09/09/24  *********************************************
     C*** Begin Add ***  06/24/24  *********************************************
     C                   z-add     *zeros        xxcolamt          9 2          Coll Amount to use
     C                   z-add     *zeros        xxtotded          9 2          Tot Deductible bal
      /Free
       //*** Begin Chg ***  05/06/25  *********************************************
       // If xxcolp > 0 and EAamt <> 0;
       If xxcolp > 0 and EAamt <> 0 and XXCOLAD = 'Y';
       //*** End   Chg ***  05/06/25  *********************************************
          If EAamt < xxcolp;
             // then the full deductible can be paid off
             xxcolamt = EAamt;
          Else;
             // only the amount of collateral payment left over can be used
             xxcolamt = xxcolp;
          Endif;
          // Reduce the collateral balance variable by the amount being used
          xxcolp = xxcolp - xxcolamt;

          //*** Begin Add ***  09/09/24  *********************************************
          // Reduce the Deductible amount balance variable by the amount being used
          xxEAamtleft = xxEAamtleft - xxColamt;
          //*** End   Add ***  09/09/24  *********************************************

          // Write a record to WDEAP using the deductible payment EXCOLD A/R code
          xxtotded = xxtotded + xxcolamt;      // total of Deductble and Tax on Deductible
          EAamt = xxcolamt;
          EAckam = EAamt;
          EAbat2 = xxlnkbat#;                  // Linked batch#
          EAarcd = EXcold;
          XXKEY = EAarcd;
          Chain TBKEY PTT01;
          If %found();
             EAdesc = TTdesc;
             EActyp = TTctyp;
          Endif;
          write pea02;

          EAamt = 0;                           // Clear Posting Amount
          EAbat2 = 0;                          // Clear Linked batch# for Deductible Tax record
       Endif;
      /End-Free
     C*** End   Add ***  06/24/24  *********************************************

     C*** Begin Add ***  09/09/24  *********************************************
      * Check if Deductible Amount pending for payment and policy runs out of collateral,
      * Allow collateral to be pulled from other divisions of the same policy
      * only If policy is Non AR summarized
      /Free
       If xxEAamtleft > 0 and XXCOLAD = 'Y' and XXARSM <> 'Y';
          inCo#  = EAco#;
          inFyr  = EAfyr;
          outFnd = EAfnd;
          outEmp# = XXSVEM;
          outDiv = 0;                  // start from div 0

          Dow xxEAamtleft > 0;
             outColPmt = 0;
             xxColP3 = 0;
             xxColP4 = 0;

            // Validate outDiv exisits in the policy
             Chain (inCo#:outfnd:inFyr:outEmp#:outDiv) pell1;
             If %found(wdell1) and L1_ELdiv <> XXSVDV;        //skip division being processed
                // Collateral Payment
                Exec Sql Select Sum(EAamt) Into :xxColP3 From WMEAP
                 Where EAco#= :inCo# And EAfnd= :outfnd And EAemp#= :outEmp#
                 And EAdiv= :outDiv and EAfyr= :inFyr and (EAarcd = :EXcolp or
                 EAarcd = :EXcolt);
                // Collateral Payment from A/R Batch that hasn't transferred yet
                Exec Sql Select Sum(EAamt) Into :xxColP4 From WDEAP
                 Where EAco#= :inCo# And EAfnd= :outfnd And EAemp#= :outEmp#
                 And EAdiv= :outDiv and EAfyr= :inFyr and (EAarcd = :EXcolp or
                 EAarcd = :EXcolt) and EAstat <> 'T';

                outColPmt = xxColP3 + xxColP4;
             Endif;
             If not %found(wdell1);
                Leave;                          // Leave the loop
             Endif;

             If outColPmt = 0;
                outDiv = outDiv + 1;
                Iter;                           // check for next division
             Endif;


             If outColPmt > 0;
                xxColamt = 0;

                If xxEAamtleft < outColPmt;
                   // then the full deductible can be paid off
                   xxColamt = xxEAamtleft;
                Else;
                   // only the amount of collateral payment left over can be used
                   xxColamt = outColPmt;
                Endif;
                // Reduce the collateral balance variable by the amount being used
                outColPmt = outColPmt - xxColamt;

                // Reduce the Deductible amount balance by the amount being used
                xxEAamtleft = xxEAamtleft - xxColamt;

         //.1.  // Write a record to WDEAP using the deductible payment EXCOLD A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  XXSVDV;
                EAamt = xxColamt;
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = EXcold;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Collateral Rcvd from Div-' + %char(outDiv);
                Write pea02;



         //.2.  // Write the Collateral transfer record to division
                // which is providing Collateral for Paymant
                // Write a record to WDEAP using the Collateral Transfer EXCOLT A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  outDiv;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = EXcolt;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Collateral Xfer to Div-' + %char(XXSVDV);
                Write pea02;



         //.3.  // Write the collateral offset record to division
                // which is providing Collateral for Paymant
     C* Use the EXCOLO A/R code.  Populate the EABAT2 field with the batch # of the billing batch.

                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  outDiv;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbata = EAamt;                       // Batch Amount
                EAbat2 = 0;                           // Linked Batch #
                svEAbat# = EAbat#;                    // Save Batch#
                EAbat# = xxLnkbat#;
                EAarcd = EXcolo;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Collateral Offset to Div-' + %char(XXSVDV);
                Write pea02;


                EAamt = 0;                     // Clear Posting Amount
                EAbat# = svEAbat#;             // Save Batch#
             Endif;
             outDiv = outDiv + 1;
          Enddo;
       Endif;
      /End-Free
     C*** End   Add ***  09/09/24  *********************************************

     C*** Begin Add ***  09/09/24  *********************************************
      * Check if Deductible Amount pending for payment and policy runs out of collateral,
      * Allow collateral to be pulled from other years of the same policy
      * also check all divisions of other years of same policy
      /Free
       If xxEAamtleft > 0 and XXCOLAD = 'Y';
          inCo#  = EAco#;
          inFyr  = EAfyr;
          outFnd = EAfnd;
          outEmp# = XXSVEM;
          outFyr = 0;                  // start from first year of the policy
          outDiv = 0;                  // start from div 0

         // Start from 1st policy year and 1st division
          Setll (inCo#:outfnd:outEmp#) pell8;
          Reade (inCo#:outfnd:outEmp#) pell8;
          Dow not %eof(wdell8);
             If xxEAamtleft <= 0;
                Leave;                       // Leave the loop
             Endif;
             If L8_ELfyr = infyr;
                Leave;                       // Leave the loop
             Endif;
             If L8_ELarsm = 'Y';
                // Do not process other divisions for A/R Summarized policies
                If L8_ELdiv <> XXSTRD;
                   Reade (inCo#:outfnd:outEmp#) pell8;
                   Iter;                        // check for next year/division
                Endif;
             Endif;

             outColPmt = 0;
             xxColP3 = 0;
             xxColP4 = 0;
             outFyr = L8_ELfyr;
             outDiv = L8_ELdiv;

             // Collateral Payment
             Exec Sql Select Sum(EAamt) Into :xxColP3 From WMEAP
              Where EAco#= :inCo# And EAfnd= :outfnd And EAemp#= :outEmp#
              And EAdiv= :outDiv and EAfyr= :outFyr and (EAarcd = :EXcolp or
              EAarcd = :EXcolt);
             // Collateral Payment from A/R Batch that hasn't transferred yet
             Exec Sql Select Sum(EAamt) Into :xxColP4 From WDEAP
              Where EAco#= :inCo# And EAfnd= :outfnd And EAemp#= :outEmp#
              And EAdiv= :outDiv and EAfyr= :outFyr and (EAarcd = :EXcolp or
              EAarcd = :EXcolt) and EAstat <> 'T';

             outColPmt = xxColP3 + xxColP4;

             If outColPmt > 0;
                xxColamt = 0;

                If xxEAamtleft < outColPmt;
                   // then the full deductible can be paid off
                   xxColamt = xxEAamtleft;
                Else;
                   // only the amount of collateral payment left over can be used
                   xxColamt = outColPmt;
                Endif;
                // Reduce the collateral balance variable by the amount being used
                outColPmt = outColPmt - xxColamt;

                // Reduce the Deductible amount balance by the amount being used
                xxEAamtleft = xxEAamtleft - xxColamt;

         //.1.  // Write a record to WDEAP using the deductible payment EXCOLD A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  XXSVDV;
                EAamt = xxColamt;
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = EXcold;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Coll Rcvd from Fyr-' + %char(outFyr) +
                 ' Div-' + %char(outDiv);
                Write pea02;



         //.2.  // Write the Collateral transfer record to division
                // which is providing Collateral for Paymant
                // Write a record to WDEAP using the Collateral Transfer EXCOLT A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAfyr =  outFyr;
                EAdiv =  outDiv;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = EXcolt;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Coll Xfer to Fyr-' + %char(inFyr) +
                 ' Div-' + %char(XXSVDV);
                Write pea02;



         //.3.  // Write the collateral offset record to division
                // which is providing Collateral for Paymant
     C* Use the EXCOLO A/R code.  Populate the EABAT2 field with the batch # of the billing batch.

                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAfyr =  outFyr;
                EAdiv =  outDiv;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbata = EAamt;                       // Batch Amount
                EAbat2 = 0;                           // Linked Batch #
                svEAbat# = EAbat#;                    // Save Batch#
                EAbat# = xxLnkbat#;
                EAarcd = EXcolo;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Coll Offset to Fyr-' + %char(inFyr) +
                 ' Div-' + %char(XXSVDV);
                Write pea02;


                EAfyr = inFyr;                 // Restore Year
                EAamt = 0;                     // Clear Posting Amount
                EAbat# = svEAbat#;             // Save Batch#
             Endif;
             Reade (inCo#:outfnd:outEmp#) pell8;
          Enddo;
       Endif;
      /End-Free
     C*** End   Add ***  09/09/24  *********************************************

     C*** Begin Add ***  09/09/24  *********************************************
      * Check if Deductible Amount pending for payment and policy runs out of collateral,
      * Allow collateral to be pulled from linked policies with customer ID
      /Free
       If xxEAamtleft > 0 and XXCOLAD = 'Y' and XXCUST <> *Blanks;
          inCo#  = EAco#;
          inFyr  = EAfyr;
          inCust = XXCUST;

          Dow xxEAamtleft > 0;
             RtnColPmt(inCo#:inFyr:inCust:outFnd:outEmp#:outFyr:outDiv
              :outColPmt);
             If outColPmt = 0;
                Leave;
             Endif;
             If outColPmt > 0;
                xxColamt = 0;

                If xxEAamtleft < outColPmt;
                   // then the full deductible can be paid off
                   xxColamt = xxEAamtleft;
                Else;
                   // only the amount of collateral payment left over can be used
                   xxColamt = outColPmt;
                Endif;
                // Reduce the collateral balance variable by the amount being used
                outColPmt = outColPmt - xxColamt;

                // Reduce the Deductible amount balance by the amount being used
                xxEAamtleft = xxEAamtleft - xxColamt;

         //.1.  // Write a record to WDEAP using the deductible payment EXCOLD A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  XXSVDV;
                EAamt = xxColamt;
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = EXcold;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
         //     EAdesc = 'Col Rcvd frm Emp ' + %char(outfnd) +
         //               '-' + %char(outemp#) + ' Fyr-' + %char(outFyr);
                EAdesc = 'Col Rcv frm ' + %char(outfnd) +
                          '-' + %char(outEmp#) + '-' + %char(outFyr) +
                          '-' + %char(outDiv);
                Write pea02;



         //.2.  // Write the collateral transfer record using the key information
                // returned from the other employer
                // Write a record to WDEAP using the Collateral Transfer EXCOLT A/R code
                EAfnd =  outFnd;
                EAemp# = outEmp#;
                EAfyr =  outFyr;
                EAdiv =  outDiv;
                If XXARSM = 'Y';
                   EAdiv =  XXSTRD;
                Endif;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = EXcolt;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Col Xfer to Emp ' + %char(XXFND) +
                          '-' + %char(XXSVEM);
                Write pea02;



         //.3.  // Write the collateral offset record using the key information
                // returned from the other employer
     C* Use the EXCOLO A/R code.  Populate the EABAT2 field with the batch # of the billing batch.

                EAfnd =  outFnd;
                EAemp# = outEmp#;
                EAdiv =  outDiv;
                EAfyr =  outFyr;
                If XXARSM = 'Y';
                   EAdiv =  XXSTRD;
                Endif;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbata = EAamt;                       // Batch Amount
                EAbat2 = 0;                           // Linked Batch #
                svEAbat# = EAbat#;                    // Save Batch#
                EAbat# = xxLnkbat#;
                EAarcd = EXcolo;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Col Offst to Emp ' + %char(XXFND) +
                          '-' + %char(XXSVEM);
                Write pea02;


                EAfyr = inFyr;                 // Restore Year
                EAamt = 0;                     // Clear Posting Amount
                EAbat# = svEAbat#;             // Save Batch#
             Endif;
          Enddo;
       Endif;
      /End-Free
     C*** End   Add ***  09/09/24  *********************************************
      *
      * WRITE DEDUCTIBLE TAX RECORD
      *
      *
      * IF DEDUCTIBLE TAX A/R CODE EXISTS FOR YEAR
      *
     C     DTXB(Z)       IFNE      *BLANK
      *
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVE      *BLANKS       XXRDTP
     C                   MOVE      *BLANKS       XXDTXB            2
     C*
      * IF 'TAX DEDUCTIBLE BILL' IN REPORTING/RATING FILE IS 'Y'
      *
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *
     C  N60RRRDTX        IFEQ      'Y'
      *
      * LOOK FOR DEDUCTIBLE TAX BILLED RECORD IN WDEAP
      *
     C     EAKEY1        CHAIN     PEA02                              30
      *
     C     *IN30         DOWEQ     *OFF
     C     EAARCD        ANDNE     DTXB(Z)
     C     EAKEY1        READE     PEA02                                  30
     C                   ENDDO                                                  *IN30 = *OFF
      *
     C     *IN30         IFEQ      '1'
     C     EATFFL        OREQ      'Y'
      *
      * GET DESCRIPTION
      *
     C                   MOVEL(P)  DTXB(Z)       XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVEL     TTKEY         XXDTXB
     C                   MOVE      TTTYPE        XXRDTP
     C                   MOVE      TTDESC        XXDESC
     C*
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXBAT#        EABAT#
     C                   Z-ADD     XXSVEM        EAEMP#
     C                   Z-ADD     XXSVDV        EADIV
     C*** OVERRIDE DIV IF PEO & SPECIFIC CLIENT
     C*
     C     FNPEOF        IFNE      *BLANKS
     C     @@CLT#        ANDEQ     '135'
     C                   Z-ADD     *ZEROS        EADIV
     C                   Z-ADD     XXSVDV        EARDIV
     C                   ENDIF
     C*
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVE      XXSYCD        EASYCD
     C                   MOVE      'N'           EATFAP
     C                   MOVE      'N'           EATFFL
     C                   Z-ADD     *ZEROS        EATFDT
     C                   Z-ADD     *ZEROS        EATFAM
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXBFYR        EAFYR
     C                   MOVEL     XXDTXB        EAARCD
     C                   MOVE      'R'           EACTYP
     C                   MOVEL     XXDESC        EADESC
     C                   Z-ADD     *ZEROS        EABATA
     C                   Z-ADD     0             EAAMT
     C                   Z-ADD     0             EACKAM
     C*** Begin Add ***  06/24/24  *********************************************
     C* Clear Link Batch Number
     C                   Z-ADD     0             EABAT2
     C*** End   Add ***  06/24/24  *********************************************
      *
     C                   ENDIF                                                  *IN30 = '1'
      *
     C     AMT(Z)        MULT      RRTXPC        EAAMT
     C     EAAMT         MULT(H)   .01           EAAMT
     C                   Z-ADD     EAAMT         EACKAM
     C                   ADD       EAAMT         XXTOTB
     C                   Z-ADD     *ZEROS        XXBATA
     C*
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C*
     C                   MOVE      *BLANKS       EASTAT
     C                   MOVE      Q1USER        EAUSER
     C                   MOVE      Q1PGM         EAPGM
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
      *
     C                   MOVE      'D'           EAACHF
     C*
     C*  IF THE A/R BATCH RECORD ALREADY EXISTS AND HAS NOT BEEN TRANSFERRED
     C*  THAN UPDATE OTHERWISE WRITE A NEW RECORD
     C*
     C     *IN30         IFEQ      '0'
     C     EATFFL        ANDNE     'Y'
     C                   UPDATE    PEA02
     C                   ELSE
      *
     C     EAAMT         IFNE      *ZEROS
     C                   EXSR      GETINV
     C                   EXSR      EAIMNT
      // Get the next available 3rd party invoice number
     C                   WRITE     PEA02
     C                   MOVE      *ON           I@BILL
     C                   END                                                    EAAMT = 0
      *
     C                   END                                                    *IN30 = 0
      *
     C                   ENDIF                                                  RRRDTX = 'Y'
      *
     C                   ENDIF                                                  DTXB,Z = BLANK
      *
     C*** Begin Add ***  09/09/24  *********************************************
       xxEAamtleft = EAamt;              // Move Tax on Deductible Amount
     C*** End   Add ***  09/09/24  *********************************************
     C*** Begin Add ***  06/24/24  *********************************************
      /Free
       //*** Begin Chg ***  05/06/25  *********************************************
       // If xxcolp > 0 and EAamt <> 0;
       If xxcolp > 0 and EAamt <> 0 and XXCOLAD = 'Y';
       //*** End   Chg ***  05/06/25  *********************************************
          xxcolamt = 0;
          If EAamt < xxcolp;
             // then the full tax on deductible can be paid off
             xxcolamt = EAamt;
          Else;
             // only the amount of collateral payment left over can be used
             xxcolamt = xxcolp;
          Endif;
          // Reduce the collateral balance variable by the amount being used
          xxcolp = xxcolp - xxcolamt;

          //*** Begin Add ***  09/09/24  *********************************************
          // Reduce tax on Deductible amount balance by the amount being used
          xxEAamtleft = xxEAamtleft - xxColamt;
          EAfnd =  XXFND;
          EAemp# = XXSVEM;
          EAdiv =  XXSVDV;
          //*** End   Add ***  09/09/24  *********************************************

          // Write a record to WDEAP using tax on deductible payment F1DTXP A/R code
          xxtotded = xxtotded + xxcolamt;      // total of Deductble and Tax on Deductible
          EAamt = xxcolamt;
          EAckam = EAamt;
          EAbat2 = xxlnkbat#;                  // Linked batch#
          EAarcd = F1dtxp;
          XXKEY = EAarcd;
          Chain TBKEY PTT01;
          If %found();
             EAdesc = TTdesc;
          Endif;
          EActyp = TTctyp;
          write pea02;
       Endif;


     C* Write a record to the billing batch using the EXCOLT A/R code and a negative amount equal
     C* to total of the deductible and tax on deductible payment records just created above.
       If xxtotded <> 0;
          // Write a record to WDEAP using the Collateral Transfer EXCOLT A/R code
          //*** Begin Add ***  09/09/24  *********************************************
          EAfnd =  XXFND;
          EAemp# = XXSVEM;
          EAdiv =  XXSVDV;
          //*** End   Add ***  09/09/24  *********************************************
          If XXARSM = 'Y';
             EAdiv =  XXSTRD;
          Endif;
          EAamt = xxtotded * (-1);
          EAckam = EAamt;
          EAbat2 = xxlnkbat#;                  // Linked batch#
          EAarcd = EXcolt;
          XXKEY = EAarcd;
          Chain TBKEY PTT01;
          If %found();
             EAdesc = TTdesc;
          Endif;
          EActyp = TTctyp;
          write pea02;
       Endif;


     C* write another record for the collateral billing offset.
     C* This record will be written to the linked batch #.
     C* Use the EXCOLO A/R code.  Populate the EABAT2 field with the batch # of the billing batch.

       If xxtotded <> 0;
          // Write a record to WDEAP using the collateral billing offset EXCOLO A/R code
          //*** Begin Add ***  09/09/24  *********************************************
          EAfnd =  XXFND;
          EAemp# = XXSVEM;
          EAdiv =  XXSVDV;
          //*** End   Add ***  09/09/24  *********************************************
          If XXARSM = 'Y';
             EAdiv =  XXSTRD;
          Endif;
          EAamt = xxtotded * (-1);
          EAbata = EAamt;                       // Batch Amount
          EAckam = EAamt;
          EAbat2 = 0;                           // Linked Batch #
          EAbat# = xxlnkbat#;
          EAarcd = EXcolo;
          XXKEY = EAarcd;
          Chain TBKEY PTT01;
          If %found();
             EAdesc = TTdesc;
          Endif;
          EActyp = TTctyp;
          write pea02;
       Endif;
      /End-Free
     C*** End   Add ***  06/24/24  *********************************************

     C*** Begin Add ***  09/09/24  *********************************************
      * Check if Tax on Deductible Amount pending for payment and policy runs out of collateral,
      * Allow collateral to be pulled from other divisions of the same policy
      * only If policy is Non AR summarized
      /Free
       If xxEAamtleft > 0 and XXCOLAD = 'Y' and XXARSM <> 'Y';
          inCo#  = EAco#;
          inFyr  = EAfyr;
          outFnd = EAfnd;
          outEmp# = XXSVEM;
          outDiv = 0;                  // start from div 0

          Dow xxEAamtleft > 0;
             outColPmt = 0;
             xxColP3 = 0;
             xxColP4 = 0;

            // Validate outDiv exisits in the policy
             Chain (inCo#:outfnd:inFyr:outEmp#:outDiv) pell1;
             If %found(wdell1) and L1_ELdiv <> XXSVDV;        //skip division being processed
                // Collateral Payment
                Exec Sql Select Sum(EAamt) Into :xxColP3 From WMEAP
                 Where EAco#= :inCo# And EAfnd= :outfnd And EAemp#= :outEmp#
                 And EAdiv= :outDiv and EAfyr= :inFyr and (EAarcd = :EXcolp or
                 EAarcd = :EXcolt);
                // Collateral Payment from A/R Batch that hasn't transferred yet
                Exec Sql Select Sum(EAamt) Into :xxColP4 From WDEAP
                 Where EAco#= :inCo# And EAfnd= :outfnd And EAemp#= :outEmp#
                 And EAdiv= :outDiv and EAfyr= :inFyr and (EAarcd = :EXcolp or
                 EAarcd = :EXcolt) and EAstat <> 'T';

                outColPmt = xxColP3 + xxColP4;
             Endif;
             If not %found(wdell1);
                Leave;                          // Leave the loop
             Endif;

             If outColPmt = 0;
                outDiv = outDiv + 1;
                Iter;                           // check for next division
             Endif;


             If outColPmt > 0;
                xxColamt = 0;

                If xxEAamtleft < outColPmt;
                   // then the full tax on deductible can be paid off
                   xxColamt = xxEAamtleft;
                Else;
                   // only the amount of collateral payment left over can be used
                   xxColamt = outColPmt;
                Endif;
                // Reduce the collateral balance variable by the amount being used
                outColPmt = outColPmt - xxColamt;

                // Reduce the Deductible amount balance by the amount being used
                xxEAamtleft = xxEAamtleft - xxColamt;

         //.1.  // Write a record to WDEAP using the tax on deductible payment F1DTXP A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  XXSVDV;
                EAamt = xxColamt;
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = F1dtxp;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Collateral Rcvd from Div-' + %char(outDiv);
                Write pea02;



         //.2.  // Write the Collateral transfer record to division
                // which is providing Collateral for Paymant
                // Write a record to WDEAP using the Collateral Transfer EXCOLT A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  outDiv;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = EXcolt;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Collateral Xfer to Div-' + %char(XXSVDV);
                Write pea02;



         //.3.  // Write the collateral offset record to division
                // which is providing Collateral for Paymant
     C* Use the EXCOLO A/R code.  Populate the EABAT2 field with the batch # of the billing batch.

                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  outDiv;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbata = EAamt;                       // Batch Amount
                EAbat2 = 0;                           // Linked Batch #
                svEAbat# = EAbat#;                    // Save Batch#
                EAbat# = xxLnkbat#;
                EAarcd = EXcolo;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Collateral Offset to Div-' + %char(XXSVDV);
                Write pea02;


                EAamt = 0;                     // Clear Posting Amount
                EAbat# = svEAbat#;             // Save Batch#
             Endif;
             outDiv = outDiv + 1;
          Enddo;
       Endif;
      /End-Free
     C*** End   Add ***  09/09/24  *********************************************

     C*** Begin Add ***  09/09/24  *********************************************
      * Check if Tax on Deductible Amount pending for payment and policy runs out of collateral,
      * Allow collateral to be pulled from other years of the same policy
      * also check all divisions of other years of same policy
      /Free
       If xxEAamtleft > 0 and XXCOLAD = 'Y';
          inCo#  = EAco#;
          inFyr  = EAfyr;
          outFnd = EAfnd;
          outEmp# = XXSVEM;
          outFyr = 0;                  // start from first year of the policy
          outDiv = 0;                  // start from div 0

         // Start from 1st policy year and 1st division
          Setll (inCo#:outfnd:outEmp#) pell8;
          Reade (inCo#:outfnd:outEmp#) pell8;
          Dow not %eof(wdell8);
             If xxEAamtleft <= 0;
                Leave;                       // Leave the loop
             Endif;
             If L8_ELfyr = infyr;
                Leave;                       // Leave the loop
             Endif;
             If L8_ELarsm = 'Y';
                // Do not process other divisions for A/R Summarized policies
                If L8_ELdiv <> XXSTRD;
                   Reade (inCo#:outfnd:outEmp#) pell8;
                   Iter;                        // check for next year/division
                Endif;
             Endif;

             outColPmt = 0;
             xxColP3 = 0;
             xxColP4 = 0;
             outFyr = L8_ELfyr;
             outDiv = L8_ELdiv;

             // Collateral Payment
             Exec Sql Select Sum(EAamt) Into :xxColP3 From WMEAP
              Where EAco#= :inCo# And EAfnd= :outfnd And EAemp#= :outEmp#
              And EAdiv= :outDiv and EAfyr= :outFyr and (EAarcd = :EXcolp or
              EAarcd = :EXcolt);
             // Collateral Payment from A/R Batch that hasn't transferred yet
             Exec Sql Select Sum(EAamt) Into :xxColP4 From WDEAP
              Where EAco#= :inCo# And EAfnd= :outfnd And EAemp#= :outEmp#
              And EAdiv= :outDiv and EAfyr= :outFyr and (EAarcd = :EXcolp or
              EAarcd = :EXcolt) and EAstat <> 'T';

             outColPmt = xxColP3 + xxColP4;

             If outColPmt > 0;
                xxColamt = 0;

                If xxEAamtleft < outColPmt;
                   // then the full deductible can be paid off
                   xxColamt = xxEAamtleft;
                Else;
                   // only the amount of collateral payment left over can be used
                   xxColamt = outColPmt;
                Endif;
                // Reduce the collateral balance variable by the amount being used
                outColPmt = outColPmt - xxColamt;

                // Reduce the Deductible amount balance by the amount being used
                xxEAamtleft = xxEAamtleft - xxColamt;

         //.1.  // Write a record to WDEAP using the tax on deductible payment F1DTXP A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  XXSVDV;
                EAamt = xxColamt;
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = F1dtxp;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Coll Rcvd from Fyr-' + %char(outFyr) +
                 ' Div-' + %char(outDiv);
                Write pea02;



         //.2.  // Write the Collateral transfer record to division
                // which is providing Collateral for Paymant
                // Write a record to WDEAP using the Collateral Transfer EXCOLT A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAfyr =  outFyr;
                EAdiv =  outDiv;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = EXcolt;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Coll Xfer to Fyr-' + %char(inFyr) +
                 ' Div-' + %char(XXSVDV);
                Write pea02;



         //.3.  // Write the collateral offset record to division
                // which is providing Collateral for Paymant
     C* Use the EXCOLO A/R code.  Populate the EABAT2 field with the batch # of the billing batch.

                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAfyr =  outFyr;
                EAdiv =  outDiv;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbata = EAamt;                       // Batch Amount
                EAbat2 = 0;                           // Linked Batch #
                svEAbat# = EAbat#;                    // Save Batch#
                EAbat# = xxLnkbat#;
                EAarcd = EXcolo;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Coll Offset to Fyr-' + %char(inFyr) +
                 ' Div-' + %char(XXSVDV);
                Write pea02;


                EAfyr = inFyr;                 // Restore Year
                EAamt = 0;                     // Clear Posting Amount
                EAbat# = svEAbat#;             // Save Batch#
             Endif;
             Reade (inCo#:outfnd:outEmp#) pell8;
          Enddo;
       Endif;
      /End-Free
     C*** End   Add ***  09/09/24  *********************************************

     C*** Begin Add ***  09/09/24  *********************************************
      * Check if tax on Deductible Amount pending for payment and policy runs out of collateral,
      * Allow collateral to be pulled from linked policies with customer ID
      /Free
       If xxEAamtleft > 0 and XXCOLAD = 'Y' and XXCUST <> *Blanks;
          inCo#  = EAco#;
          inFyr  = EAfyr;
          inCust = XXCUST;

          Dow xxEAamtleft > 0;
             RtnColPmt(inCo#:inFyr:inCust:outFnd:outEmp#:outFyr:outDiv
              :outColPmt);
             If outColPmt = 0;
                Leave;
             Endif;
             If outColPmt > 0;
                xxColamt = 0;

                If xxEAamtleft < outColPmt;
                   // then the full tax on deductible can be paid off
                   xxColamt = xxEAamtleft;
                Else;
                   // only the amount of collateral payment left over can be used
                   xxColamt = outColPmt;
                Endif;
                // Reduce the collateral balance variable by the amount being used
                outColPmt = outColPmt - xxColamt;

                // Reduce tax on Deductible amount balance by the amount being used
                xxEAamtleft = xxEAamtleft - xxColamt;

         //.1.  // Write a record to WDEAP using tax on deductible payment F1DTXP A/R code
                EAfnd =  XXFND;
                EAemp# = XXSVEM;
                EAdiv =  XXSVDV;
                EAamt = xxColamt;
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = F1dtxp;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
         //     EAdesc = 'Col Rcvd frm Emp ' + %char(outfnd) +
         //               '-' + %char(outemp#) + ' Fyr-' + %char(outFyr);
                EAdesc = 'Col Rcv frm ' + %char(outfnd) +
                          '-' + %char(outEmp#) + '-' + %char(outFyr) +
                          '-' + %char(outDiv);
                Write pea02;



         //.2.  // Write the collateral transfer record using the key information
                // returned from the other employer
                // Write a record to WDEAP using the Collateral Transfer EXCOLT A/R code
                EAfnd =  outFnd;
                EAemp# = outEmp#;
                EAfyr =  outFyr;
                EAdiv =  outDiv;
                If XXARSM = 'Y';
                   EAdiv =  XXSTRD;
                Endif;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbat2 = xxLnkbat#;                  // Linked batch#
                EAarcd = EXcolt;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Col Xfer to Emp ' + %char(XXFND) +
                          '-' + %char(XXSVEM);
                Write pea02;



         //.3.  // Write the collateral offset record using the key information
                // returned from the other employer
     C* Use the EXCOLO A/R code.  Populate the EABAT2 field with the batch # of the billing batch.

                EAfnd =  outFnd;
                EAemp# = outEmp#;
                EAdiv =  outDiv;
                EAfyr =  outFyr;
                If XXARSM = 'Y';
                   EAdiv =  XXSTRD;
                Endif;
                EAamt = xxColamt * (-1);
                EAckam = EAamt;
                EAbata = EAamt;                       // Batch Amount
                EAbat2 = 0;                           // Linked Batch #
                svEAbat# = EAbat#;                    // Save Batch#
                EAbat# = xxLnkbat#;
                EAarcd = EXcolo;
                XXKEY = EAarcd;
                Chain TBKEY PTT01;
                If %found();
                   EActyp = TTctyp;
                Endif;
                EAdesc = 'Col Offst to Emp ' + %char(XXFND) +
                          '-' + %char(XXSVEM);
                Write pea02;


                EAfyr = inFyr;                 // Restore Year
                EAamt = 0;                     // Clear Posting Amount
                EAbat# = svEAbat#;             // Save Batch#
             Endif;
          Enddo;
       Endif;
      /End-Free
     C*** End   Add ***  09/09/24  *********************************************
      *
     C                   ADD       1             Z
     C                   Z-ADD     FYR(Z)        XXBFYR
     C                   ENDDO                                                  FYR,Z DOWNE
     C*
     C                   END                                                    XXSVEM <> 0
     C*
     C                   ENDSR
     C*===============================================================
     C* STRING - SUBROUTINE TO SEARCH STRINGS
     C*===============================================================
     C     STRING        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC           30
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC           30
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
B01  C     SCNRES        IFEQ      0
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
E01  C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*====================================================================
     C* Read thru WDDAP and accumulate total amounts
     C*====================================================================
     C     SUMDA         BEGSR
     C*
     C     *LIKE         DEFINE    *IN60         SVIN60
     C                   MOVE      *IN60         SVIN60
     C*
     C     DAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMFYR
     C*
     C     *LIKE         DEFINE    DAAMTB        TOTLDA
     C                   CLEAR                   TOTLDA
     C*
     C     DAKY1         SETLL     PDA01
     C     *IN60         DOUEQ     *ON
     C                   CLEAR                   DAAMTB
     C     DAKY1         READE     PDA01                                  60
     C                   ADD       DAAMTB        TOTLDA
     C                   ENDDO
     C*
     C                   MOVE      SVIN60        *IN60
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  VLDCLM - THIS SUBROUTINE VALIDATES THE CLAIM FOR PROCESSING. *
     C*****************************************************************
     C     VLDCLM        BEGSR
     C                   MOVE      *OFF          *IN73
       claimExcluded = *Off;
     C*
     C*  DO NOT INCLUDE THE CLAIM IF IT HAS BEEN DELETED, TRANSFERRED
     C*  TO ANOTHER CLAIM #, OR TRANSFERRED TO ANOTHER EMPLOYER.
     C*
     C     CTKY          CHAIN     PCT01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Do not include the claim only if the transfer was processed
     C*    in the current month.
     C*
     C                   MOVEL     CTDT          XXCYM2            5 0
     C     XXCYM1        IFEQ      XXCYM2
     C*
     C     CTDLTR        IFNE      *BLANK
     C     CTFYR         ORNE      CMFYR
     C     CTCASE        ORNE      CMCASE
     C     CTEMP#        ORNE      CMEMP#
     C     CTDIV         ORNE      CMDIV
     C                   MOVE      *ON           *IN73
     C                   GOTO      VLDC1
     C                   END
     C*
     C                   END
     C*
     C     CTKY          READE     PCT01                                  60
     C                   END
     C*
     C*  Do not include the claim if it was transferred using the
     C*    Group to Group transfer process in the current month.
     C*
     C     CTKY          CHAIN     PET01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL     ETDT          XXCYM2
     C     XXCYM1        IFEQ      XXCYM2
     C                   MOVE      *ON           *IN73
     C                   GOTO      VLDC1
     C                   END
     C*
     C     CTKY          READE     PET01                                  60
     C                   END
     C*
          // Initialize Fields
          Clear xxSt;
          Clear xxDdCd;
          Clear x7DcEf;
          Clear LDincMdIn;
          Clear DDincMdIn;

          // Exclude claim if inclusion type excludes it.
          strDedSize = RtvDedSize(CMco#:CMfnd:CMemp#:CMdiv:CMfyr);

          // Access Large Deductible File
          Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pld01;

          // Access Policy Detail
          Chain (CMco#:CMfnd:CMemp#:CMdiv:CMfyr) pel01;
          If %Found(wdell);
             xxDdCd = ELdCde;
             x7DcEf = ELdcEf;
          EndIf;

          // Retrieve the Policy State
          xxCd = ELrtCd;
          Chain (CMco#:xxCd) prs01;
          If %Found(wmrsl);
             xxSt = RSjrSt;
          EndIf;

          // Access Small Deductible File
          SetGT  (CMco#:CMfnd:xxSt:xxDdCd:x7DcEf) pdd0t;
          ReadPE (CMco#:CMfnd:xxSt:xxDdCd)        pdd0t;

          Select;
          When strDedSize = 'L' and LDincMdIn <> *blank;
             If (LDincMdIn = 'M' and CMltfl <> 'M') Or
                (LDincMdIn = 'I' and CMltfl <> 'I');
                claimExcluded = *On;
                LeaveSr;
             Endif;
          When strDedSize = 'S' and DDincMdIn <> *blank;
             If (DDincMdIn = 'M' and CMltfl <> 'M') Or
                (DDincMdIn = 'I' and CMltfl <> 'I');
                claimExcluded = *On;
                LeaveSr;
             Endif;
          Endsl;
     C     VLDC1         TAG
     C*
     C                   ENDSR                                                  VLDCLM
     C*****************************************************************
     C*
     C*====================================================================
     C* Write records to WDDAP
     C*====================================================================
     C     WRITDA        BEGSR
     C*
     C     DAKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    X2PODT
     C*
     C                   CLEAR                   PDA00
     C     DAKY          CHAIN     PDA00                              40
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   Z-ADD     CMCO#         DACO#
     C                   Z-ADD     CMFND         DAFND
     C                   Z-ADD     CMEMP#        DAEMP#
     C                   Z-ADD     CMDIV         DADIV
     C                   Z-ADD     CMFYR         DAFYR
     C                   MOVE      CMCASE        DACASE
     C*
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DAPODT
     C*
     C                   ENDIF
     C*
     C                   ADD       XXAMTB        DAAMTB
     C*
     C                   EXSR      DAMNTF
     C*
     C     DAAMTB        IFNE      *ZEROS
     C                   MOVEL     DAPGM         DAIPGM
     C                   MOVEL     DAUSER        DAIUSR
     C                   Z-ADD     DATIME        DAITIM
     C                   Z-ADD     DADT          DAIDT
     C                   WRITE     PDA00
     C                   ENDIF
     C*
     C                   ENDSR
     C*====================================================================
     C* Write records to WDDCP
     C*====================================================================
     C     WRITDC        BEGSR
     C*
     C                   CLEAR                   PDC01
     C*
     C                   Z-ADD     CMCO#         DCCO#
     C                   Z-ADD     CMFND         DCFND
     C                   Z-ADD     CMEMP#        DCEMP#
     C                   Z-ADD     CMDIV         DCDIV
     C                   Z-ADD     CMFYR         DCFYR
     C                   MOVE      CMCASE        DCCASE
     C                   MOVEL     RC(X)         DCRECD
     C*
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCPODT
     C*
     C                   Z-ADD     XXDIFF        DCAMTB
     C*
     C     DCAMTB        IFNE      *ZEROS
     C                   MOVEL     Q1PGM         DCPGM
     C                   MOVEL     Q1USER        DCUSER
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        DCTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DCDT
     C                   WRITE     PDC01
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*  MLTCLM - This subroutine is processed when an employer is on a
     C*    'per occurrence' plan.  On this type of plan, the occurrence
     C*    amt (DDAMPO) includes the total deduct billed for all claims
     C*    under a multiple claim.  A multiple claim is a single accident
     C*    that includes multiple claimants.  Since each claim can only
     C*    contain info for one claimant, the claims have to be grouped
     C*    together using the Multiple Claim Maintenance option on the
     C*    Reinsurance menu.  This subroutine will check the Multiple
     C*    Clm file (WAMCP) for the current clm being processed.  If the
     C*    clm exists in the file, the amt of deduct billings, for the
     C*    other clms associated with that multi clm, will be subtracted
     C*    from the per occurrence amount to prevent the per occurrence
     C*    amount from being exceeded.
     C*****************************************************************
     C     MLTCLM        BEGSR
     C*
     C***First,*load the original per occurrence amount since the table
     C***value*will be changed by this subroutine for subsequent processing.
     C**********
     C*
     C*  Determine if claim is part of a reinsurance multiple claim.
     C*
     C                   MOVE      *ZEROS        XACASE
     C                   MOVE      CMCASE        XACASE
     C     MCKY          CHAIN     PMC01                              99
     C     *IN99         IFEQ      '0'
     C*
     C*  If so, process all of the claims for that multiple claim.
     C*
     C                   MOVE      MCCLM#        XXCLM#
     C     MCKY2         CHAIN     PMC02                              99
     C     *IN99         DOWEQ     '0'
     C*****
     C*  Subtract the deduct billed for each claim, except the original
     C*  claim, from the per occurrence amount.
     C*
     C     MCCASE        IFNE      XACASE
     C*
     C                   MOVE      MCCASE        CMCASE
     C     DDKEY         CHAIN     PDD01                              60
     C     *IN60         DOWEQ     '0'
     C                   SUB       DDAMTB        XXAMPO
     C     DDKEY         READE     PDD01                                  60
     C                   ENDDO
     C*
     C                   ENDIF
     C*****
     C     MCKY2         READE     PMC02                                  99
     C                   ENDDO
     C*
     C*  Restore the value of the original case #.
     C*
     C                   MOVE      XACASE        CMCASE
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C*  FIXPND - THIS SUBROUTINE BACKS OFF EXCESS RECOVERY PENDING.  *
     C*****************************************************************
      *================================================================
      * RTVBAT - RETRIEVE NEXT A/R BATCH #
      *================================================================
     C     RTVBAT        BEGSR
      *
     C     FIRST1        IFEQ      *ON
     C                   MOVE      *OFF          FIRST1
     C                   MOVE      *BLANKS       XXSTAT
     C     EAKEY         SETGT     PEA01
     C                   READP     PEA01                                  60
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL1            9 0
     C                   END
     C*
     C                   MOVE      'T'           XXSTAT
     C     EAKEY         SETGT     PEA01
     C                   READP     PEA01                                  60
     C     *IN60         IFEQ      '0'
     C     EACO#         ANDEQ     XXCO#
     C     1             ADD       EABAT#        XXVAL2            9 0
     C                   END
     C*
     C     XXVAL1        IFGT      XXVAL2
     C                   Z-ADD     XXVAL1        XXBAT#
     C                   ELSE
     C                   Z-ADD     XXVAL2        XXBAT#
     C                   END
      *
     C     XXBAT#        IFEQ      *ZEROS
     C                   Z-ADD     1             XXBAT#
     C                   END
      *
      *  Write an A/R batch dummy record, to hold the bat#.
      *
     C                   MOVE      XXBAT#        SVBAT1
     C                   CLEAR                   PEA02
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXBAT#        EABAT#
     C                   Z-ADD     0             EABATA
     C                   MOVE      *HIVAL        EAFND
     C                   MOVE      *HIVAL        EAFYR
     C                   MOVE      *HIVAL        EAEMP#
     C                   MOVE      *HIVAL        EADIV
     C                   MOVE      *BLANKS       EAARCD
     C                   MOVE      *BLANKS       EACTYP
     C                   MOVE      *BLANKS       EADESC
     C                   Z-ADD     *ZEROS        EAPODT
     C                   Z-ADD     *ZEROS        EAINV#
     C                   Z-ADD     *ZEROS        EATRDT
     C                   MOVE      *BLANKS       EALATE
     C                   MOVE      'D'           EAACHF
     C                   EXSR      GETINV
     C                   EXSR      EAIMNT
     C                   WRITE     PEA02
     C                   ELSE
     C                   MOVE      SVBAT1        XXBAT#
     C                   ENDIF
      *
     C                   ENDSR
       //---------------------------------------------------------------------
       // Calculate Discount for Excluded Claims                                             -
       //---------------------------------------------------------------------
       Begsr CalcExcClaimDisc;

          Reade (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:CMcase) PDD01;

          //Determine offset amount
          Dow not %Eof(WDDDL1);
             If DDbat# <> xxBat#;
                xxReCd = DDreCd;
                xxDiff = DDamtB * -1;
                amt(z) += xxDiff;
                Chain ddkey2 pdd00;
                If Not %Found(wdddl);
                   DDco#  = CMco#;
                   DDfnd  = CMfnd;
                   DDbFyr = CMfYr;
                   DDemp# = CMemp#;
                   DDdiv  = CMdiv;
                   DDcase = CMcase;

                   srDtI  = xxPoDt;
                   ConvertDate(srDtI:' ':'*CYMD':srDtO:srErr);
                   If srErr <> '1';
                      DDpoDt = %Dec(%Subst(srDtO:1:7):7:0);
                   EndIf;

                   DDbat# = xxBat#;
                   DDiUsr = Q1user;

                   srDtI  = dsDate;
                   ConvertDate(srDtI:' ':'*CYMD':srDtO:srErr);
                   If srErr <> '1';
                      DDiDt  = %Dec(%Subst(srDtO:1:7):7:0);
                   EndIf;

                   DDamtB = 0;
                   DDamtP = 0;

                   ExSr MoveToWDDDP;
                   If DDamtB <> 0;
                      Write pdd00;
                   EndIf;
                Else;
                   ExSr MoveToWDDDp;
                   Update PDD00;
                EndIf;
             EndIf;
             Reade (CMco#:CMfnd:CMemp#:CMdiv:CMfyr:CMcase) PDD01;
          Enddo;

       Endsr;
       //---------------------------------------------------------------------
       // Populate WDDDP data base files.                                                    -
       //---------------------------------------------------------------------
       Begsr MoveToWDDDP;

          DDamtB += xxDiff;
          If DDtype = 'N';
             DDamtP += xxDiff;
          EndIf;
          xxAmtB -= xxDiff;
          DDstat = ' ';
          DDsort = X;
          DDreCd = xxReCd;
          DDuser = q1User;
          DStime += 1;
          DDuser = Q1user;

          srDtI  = dsDate;
          ConvertDate(srDtI:' ':'*CYMD':srDtO:srErr);
          If srErr <> '1';
             DDdt   = %Dec(%Subst(srDtO:1:7):7:0);
          EndIf;

       Endsr;
       //---------------------------------------------------------------------
       // Retrieve Deductible Size                                           -
       //---------------------------------------------------------------------

       DCL-PROC RtvDedSize;
          DCL-PI *N char(1);
             @@Co#   like(CMco#);
             @@Fnd   like(CMfnd);
             @@Emp#  like(CMemp#);
             @@Div   like(CMdiv);
             @@Fyr   like(CMfyr);
          END-PI;

          DCL-S strState like(RSjrst);
          DCL-S strKey like(BSkey);
          DCL-S strDedType char(1);

          strDedType = *blank;

          chain (@@Co#:@@Fnd:@@Emp#:@@Div:@@Fyr) Pel01;
          If %found(WDELL);
             chain (ELco#:ELrtcd) PRS01;

             If %found(WMRSL);
                strState = RSjrst;
                strKey = %Editc(ELbscd:'X');
                chain (@@Co#:strKey) PBS01;

                If %found(WTABL);
                   If BSlpgm = 'L';
                      strDedType = BSlpgm;
                   Else;
                      strDedType = 'S';
                   Endif;
                Endif;

             Endif;

          Endif;


          Return strDedType;

       END-PROC;

     C*** Begin Add ***  09/09/24  *********************************************
      //===================================================================
      // Return Collateral Payment from Linked Policies with customer ID
      // Returns *on  ----> No errors
      // Returns *off ----> Any errors
      //===================================================================
     p RtnColPmt       b
     d RtnColPmt       pi
     d inCo#                               like(ELco#)
     d inFyr                               like(ELfyr)
     d inCust                              like(EMcust)
     d outFnd                              like(ELfnd)
     d outEmp#                             like(ELemp#)
     d outFyr                              like(ELfyr)
     d outDiv                              like(ELdiv)
     d outColPmt                           like(EAamt)

     d xxColPmt        s                   like(EAamt)
     d xxColPmt2       s                   like(EAamt)
      /Free

       outFnd    = 0;
       outEmp#   = 0;
       outFyr    = 0;
       outDiv    = 0;
       outColPmt = 0;

      // Read Employer Master file to get the linked policies
       Setll (inCo#:inCust) pem20;
       Reade (inCo#:inCust) pem20;
       Dow not %eof(wmeml20);
          If (E20EMemp# = XXSVEM and E20EMdiv = XXSVDV);
             // skip the employer-division being processed
          Else;
            // Read Employer Detail file to get ELcolad (Collateral Apply To Deductible flag)
            // Start from 1st policy year and 1st division
             Setll (inCo#:E20EMfnd:E20EMEmp#) pell8;
             Reade (inCo#:E20EMfnd:E20EMEmp#) pell8;
             Dow not %eof(wdell8);
                If L8_ELcolad = 'Y';
                   xxColPmt = 0;
                   xxColPmt2 = 0;

                   // Collateral Payment
                   Exec Sql Select Sum(EAamt) Into :xxColPmt From WMEAP
                    Where EAco#=:inCo# And EAfnd=:E20EMfnd And EAemp#=:E20EMEmp#
                    And EAdiv= :L8_ELdiv and EAfyr= :L8_ELfyr and (EAarcd =
                    :EXcolp or EAarcd = :EXcolt);
                   // Collateral Payment from A/R Batch that hasn't transferred yet
                   Exec Sql Select Sum(EAamt) Into :xxColPmt2 From WDEAP
                    Where EAco#=:inCo# And EAfnd=:E20EMfnd And EAemp#=:E20EMEmp#
                    And EAdiv= :L8_ELdiv and EAfyr= :L8_ELfyr and (EAarcd =
                    :EXcolp or EAarcd = :EXcolt) and EAstat <> 'T';

                   xxColPmt = xxColPmt + xxColPmt2;
                   If xxColPmt > 0;
                      outFnd    = E20EMfnd;
                      outEmp#   = E20EMEmp#;
                      outFyr    = L8_ELfyr;
                      outDiv    = L8_ELdiv;
                      outColPmt = xxColPmt;
                      Return;
                   Endif;
                Endif;
                Reade (inCo#:E20EMfnd:E20EMEmp#) pell8;
             Enddo;
          Endif;
          Reade (inCo#:inCust) pem20;
       Enddo;

       Return;

      /End-Free
     p RtnColPmt       e
     C*** End   Add ***  09/09/24  *********************************************
