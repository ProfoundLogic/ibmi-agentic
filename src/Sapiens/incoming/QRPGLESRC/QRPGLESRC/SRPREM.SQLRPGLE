     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir ('WTBNDDIR')
     H*****************************************************************
     H/TITLE SRPREM - CALCULATE S/R PREMIUM.
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*                                                               *
     H*  THIS PROGRAM WILL CALCULATE THE S/R PREMIUM FOR A MONTH OR   *
     H*  DATE PERIOD AND MANUAL PREMIUM VALUE PASSED IN.              *
     H*                                                               *
     H*  CALLED BY:                                                   *
     H*                                                               *
     H*  CALLS TO:  NONE                                              *
     H*                                                               *
     H*  PROGRAMMER: C:65                                             *
     H*  CREATED:    09/22/06          PROJECT: WC1324                *
     H*                                                               *
     H*****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 06/23/25 C:65  Project: 146WKQ  (Phw#119264)                  *
      *                Use prorated normal in assessment factor calc  *
      *                as assessment premium is prorated itself.      *
      *                                                               *
      * 05/15/25 C:300 Project: 146WKQ, Support: 118933               *
      *                Assessments not calculating correctly.         *
      *                                                               *
      * 03/19/25 C:65  Project: 146WKQ  (Phw#118735)                  *
      *                Correct S/R assessment percentage calcs by     *
      *                defaulting payroll factor to 1 when there are  *
      *                estimated payroll premiums.  Also do not       *
      *                reduce regular percentage when no f-class      *
      *                percentage and f-classes are not flagged to    *
      *                be excluded.                                   *
      *                                                               *
      * 02/04/25 C:300 Support: 118281 (LUBA)                         *
      *                Deposits not being shown on Online S/R Worksheet
      *                1) EBF 08/22/23 changes in LUBA to show Deposit*
      *                2) Additional changes to show Prior Year Balance
      *                                                               *
      * 11/11/24 C:300 Project: WC2985                                *
      *                Apply Deductible Discount to Discounted Premium*
      *                                                               *
      * 09/12/24 C:300 Project: 146WKR, Support: 117602               *
      *                Collateral Processing                          *
      *                For Summarized A/R policies, it including all  *
      *                divisions collateral in prior collateral amount*
      *                (no changes- just moved the code to other area)*
      *                                                               *
      * 06/24/24 C:300 Project: 146WKR (Prescient)                    *
      *                Collateral Processing                          *
      *                                                               *
      * 06/12/24 C:062 Project: 146WKQ                                *
      *                Add S/R Assessment processing.                 *
      *                                                               *
      * 04/18/24 C:291 Project: WC2961                                *
      *                Apply Schedule DB/CR at Manual Premium.        *
      *                                                               *
      * 08/22/23 C:62  Project: WC2663                                *
      *                Don't clear deposit if opted in.               *
      *                                                               *
      * 05/31/23 C:65  Support: 110481 (UBIC)                         *
      *                Populate the previous balance includes ...     *
      *                text into scriptura work file and pull that    *
      *                same info from Portal workfile for same.       *
      *                                                               *
      * 04/06/22 C:65  Project: Phw#101570 (UBIC)                     *
      *                Apply deposit logic to untransferred A/R       *
      *                in the GetCurBal procedure as it does to       *
      *                regular A/R.                                   *
      *                                                               *
      * 01/19/22 C:33  Project: WC2837                                *
      *                Extend net rate conditional                    *
      *                                                               *
      * 03/25/21 C:079 Project: WC2663                                *
      *                Invoice Cloud changes                          *
      *                                                               *
      * 11/16/20 C:136 Support: 90919                                 *
      *                Disperse credit to non-summarized divisions    *
      *                previous balance calculation.                  *
      *                                                               *
      * 11/10/20 C:136 Project: SY407 (DE798)                         *
      *                Write factors to WISRPREM file for the         *
      *                previous balance notification token and        *
      *                for the S/R worksheet (W7221R)                 *
      *                                                               *
      * 07/13/20 C:136 Project: SY407 (Removed by DE798)              *
      *                Look at WDEAP_HLD for previous billed as well. *
      *                Include payments from WDEAP_HLD if flagged     *
      *                to do so.                                      *
      *                                                               *
      * 05/15/20 C:62  Project: 102WIJ                                *
      *                Don't print CR/DB Verbiage.                    *
      *                                                               *
      * 08/02/19 C:291 Project: C146WJF                               *
      *                Reduce Terrorism charges by Deviation #1       *
      *                percentage if Group master is flagged as       *
      *                'Excess Insurance Group'.                      *
      *                                                               *
      * 02/09/19 C:300 Support: PHW76720                              *
      *                Self Reporting Man Calculate Function Error    *
      *                Fix Decimal data error - When manually hitting *
      *                the calculate Bill button on SR payroll        *
      *                                                               *
      * 11/29/18 C:136 Project: 150WDI                                *
      *                Calc S/R premium to the penny                  *
      *                                                               *
      * 11/07/18 C:136 Project: 150WDK                                *
      *                Sunz wants terrorism calculated on non-large   *
      *                deductible policies only.                      *
      *                                                               *
      * 02/28/18 C:136 Support: 64554                                 *
      *                Correction for 150WAN. Use regular calculation *
      *                if manual percentage is zero                   *
      *                                                               *
      * 08/22/17 C:136 Project: 150WAN                                *
      *                Calculate premium with '% of manual premium    *
      *                factor' for Sunz only                          *
      *                                                               *
      * 04/07/17 C:062 Project 136WHF                                 *
      *                Add Territory Rating for CareWest.             *
      *                                                               *
      * 04/22/14 C:295 Project: CI130                                 *
      *                Discount information on the online version of  *
      *                S/R Payroll worksheet.                         *
      *                                                               *
      * 03/21/14 C:136 Support: WA133 85088                           *
      *          C:295 Further adj to prev balance calc to include    *
      *                billings from A/R Batch.                       *
      *                                                               *
      * 02/05/14 C:136 Support: WA133 84612                           *
      *                Adjust prev balance calculation to factor      *
      *                in batch payments correctly                    *
      *                                                               *
      * 10/07/13 C:190 Project: 25WSS                                 *
      *                From the Policy Year Detail/ Rating Tab replace*
      *                "Deviation #2" with  Stabilization Factor.     *
      *                                                               *
     H* 08/28/13 C:136 Support: WA146 83053                           *
     H*               -Return 0 previous balance to non state         *
     H*                reporting divisions that are summarized        *
     H*               -Remove RDIV from previous balance calculation  *
     H*                                                               *
     H* 08/07/13 C:136 RED3542                                        *
     H*               -Correct prev balance logic for S/R Online      *
     H*                                                               *
     H* 07/15/13 C:282 Project: WC2211                                *
     H*                To Use the terrorism rates from the new        *
     H*                employer detail fields (ELTERR and ELDTRR)     *
     H*                if populated.                                  *
     H*                                                               *
     H* 04/08/13 C:136 Project: 46WGO                                 *
     H*                Retrofit LSAT stock discount calc into         *
     H*                CompSuite                                      *
     H*                                                               *
     H* 03/22/13 C:283 SUPPORT: WA133 81700                           *
     H*                ADD UP THE WAIVER SURCHARGE TO PBAL.           *
     H*                                                               *
     H* 10/25/12 C:279 PROJECT: WC2115
     H*               -Added Entry parameter for S/R payroll info     *
     H*                                                               *
     H* 10/24/12 C:136 SUPPORT: WA099 80979                           *
     H*               -CORRECT ROUNDING ERROR FOR NET RATE PREMIUM    *
     H*               -CORRECT KEYFLD FOR CHAIN TO PBS01              *
     H*                                                               *
     H* 11/18/11 C:136 SUPPORT: WA133 78541                           *
     H*                CORRECT WAIVER CALCULATION FOR NON-MONTHLY     *
     H*                VS MONTHLY REPORTING FREQUENCY                 *
     H*                                                               *
     H* 10/07/11 C:279 PROJECT: 133WLM                                *
     H*                INCLUDE DEPOSIT IN PRIOR YEAR BALANCE FOR S/R  *
     H*                ONLINE                                         *
     H*                                                               *
     H* 08/17/11 C:191 PROJECT: WC2048                                *
     H*                ADD NEW PARAMETER TO DENOTE CALLED FROM S/R    *
     H*                ONLINE                                         *
     H*                                                               *
     H* 03/11/11 C:136 SUPPORT: WA045 76126                           *
     H*                DO NOT ACCUMULATE PAYMENTS WITH FUTURE         *
     H*                PODT'S INTO PRIOR YR BALANCE                   *
     H*                                                               *
     H* 01/24/11 C:65  PROJECT: 139WPL/PRISS 117                      *
     H*                IF CREDIT EXPIRES ON SAME DAY IT IS EFFECTIVE  *
     H*                THEN DO NOT INCLUDE IT IN CALCULATION.         *
     H*                                                               *
     H* 01/20/11 C:65  PROJECT: 139WPL                                *
     H*                ADD NEW HCN CREDIT INTO DISCOUNT FACTOR BEFORE *
     H*                MODIFIER.                                      *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                MODIFY PROGRAM TO RECOGNIZE FIELDS ADDED FOR   *
     H*                2ND PERIOD DEVIATION IN FILES WDMPP,WDELP FIL- *
     H*                ES TO CALCULATE,PRINT, OR DISPLAY PREMIUM FOR  *
     H*                SPLIT DEVIATIONS.                              *
     H*                                                               *
     H* 01/12/10 C:136 PROJECT: WC1838                                *
     H*                ADD LARGE DEDUCTIBLE PROCESSING                *
     H*                                                               *
     H* 12/17/09 C:65  PROJECT:  CI019                                *
     H*                ADD PENDING BATCH PAYMENT RECORDS TO THE       *
     H*                PREVIOUS BALANCE, SO THE A/R PAYMENT TO BE     *
     H*                POSTED CAN BE ACCURATELY DETERMINED BASED OFF  *
     H*                TOTAL BILLED MINUS TOTAL PAID.  PREVIOUS       *
     H*                BALANCE WILL INCLUDE ALL PAYMENTS FOR DIVISION.*
     H*                ADDITIONALLY, CORRECT THE A/R CHAINS FOR       *
     H*                PREVIOUS BALANCE TO WORK FOR A/R SUMMARY.      *
     H*                                                               *
     H* 10/06/08 C:65  PROJECT:  CI019                                *
     H*                FIX KEY FIELD FOR CHAINS TO A/R TABLE FILE.    *
     H*                                                               *
     H* 09/15/08 C:189 PROJECT: WC1644                                *
     H*                Add an Option to exclude terrorism from LCM    *
     H*                                                               *
     H* 06/17/08 C:65  PROJECT:  CI019                                *
     H*                ADD PREVIOUS BALANCE AMOUNT.  THIS AMOUNT WILL *
     H*                MATCH WHAT PRINTS ON THE MONTHLY INTERIM.      *
     H*                WAIVER AND REINSURANCE CHARGES ARE ALREADY     *
     H*                RETURNED SO THEY WON'T BE INCLUDED IN THIS     *
     H*                AMOUNT.                                        *
     H*                                                               *
     H* 03/28/08 C:65  PROJECT: CI019                                 *
     H*                PASS OUT OF THE PROGRAM THE PRIOR BILLED       *
     H*                PREMIUM AND TAX AMOUNTS AS WELL AS A LIST OF   *
     H*                ALL FACTORS AND SUBTOTALS TO DISPLAY THE CALC. *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
     H*                                                               *
     H* 11/09/06 C:65  PROJECT: SUPP#55717                            *
     H*                SET REPORT DATE EQUAL TO POLICY EXPIRATION     *
     H*                DATE WHEN POLICY EXPIRES MID-MONTH.  ALSO,     *
     H*                TO MATCH MONTHLY INTERIM, ONLY USER AN EXP     *
     H*                MODIFIER IF ITS DATE IS LESS THAN REPORT DT.   *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELP     IF   E             DISK
     FWDELL     IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWTLML1    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWDMPL     IF   E           K DISK
     FWMEAL     IF   E           K DISK
     FWDEAL2    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03B)
     FWMEAL24   IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWMEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
     FWMEXL     IF   E           K DISK
     FWMLDL1    IF   E           K DISK
     FWDHCL3    IF   E           K DISK
      *** beg add ***** 11/10/20 *************************************
     fwisrprem  uf a e           k Disk    Rename(pre01:pre00)
      *** end add ***** 11/10/20 *************************************
     f*** Begin add *** 06/12/24 ******************************************
     FWMPRL1    IF   E           K DISK
     f*** End   add *** 06/12/24 ******************************************
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

     d xxsron          s              1    inz(*off)
      *** beg add ***** 11/10/20 *************************************
     d indPortalCall   s               n   Inz(*off)
      *** end add ***** 11/10/20 *************************************
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
     D                 DS
     D  XXRPDT                 1      7  0
     D  XXYYMM                 1      5  0
     D  XXDAY                  6      7  0
     D  XXRPYR                 1      3  0
     D  XXRPMN                 4      5  0
     D  XXRPYM                 1      5  0
     D                 DS
     D  ELPFDT                 1      7  0
     D  XXFPYM                 1      5  0
     D  XXFPYR                 1      3  0
     D  XXFPMO                 4      5  0
     D  XXFPDY                 6      7  0
     D                 DS
     D  ELPTDT                 1      7  0
     D  XXTPYM                 1      5  0
     D  XXTPYR                 1      3  0
     D  XXTPMO                 4      5  0
     D  XXTPDY                 6      7  0
     D                 DS
     D  EX                     1    021  0
     D                                     DIM(3) DESCEND
     D  ELEMD1               001    007  0
     D  ELEMD2               008    014  0
     D  ELEMD3               015    021  0
     D                 DS
     D  EM                     1     15  4
     D                                     DIM(3)
     D  ELEXM1                 1      5  4
     D  ELEXM2                 6     10  4
     D  ELEXM3                11     15  4
     D                 DS
     D  Q1FACT                 1    143
     D  Q1LMPC                 1      5  3
     D  Q1TOT1                 6     16  0
     D  Q1DSCB                17     27  9
     D  Q1TOT2                28     38  0
     D  Q1EXPM                39     43  4
     D  Q1TOT3                44     54  0
     D  Q1DSCA                55     65  9
     D  Q1TOT4                66     76  0
     D  Q1TERR                77     87  0
     D  Q1WAIV                88     98  0
     D  Q1REIN                99    109  0
     D  Q1NETF               110    121  9
     D  Q1NETP               122    132  0
     D  Q1NWAV               133    143  0
     D                 DS
     D  Q2FACT                 1     48
     D  Q2TOT1                 1     12  2
     D  Q2TOT2                13     24  2
     D  Q2TOT3                25     36  2
     D  Q2TOT4                37     48  2
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D                 DS
     D  Q1INFO                 1     39
     D  Q1AMT3                 1     15  4
     D  Q1FAC1                16     26  9
     D  Q1PLV1                27     27
     D  Q1WFAC                28     38  9
     D  Q1WVFL                39     39  0

     d*** Begin Add *** 06/12/24 *******************************************
     d decTotalPremium...
     d                 s             11  2
     d decTotalFClassAmt...
     d                 s             11  2
     d decTotalOtherClassAmt...
     d                 s             11  2
     d decTotalAllClassAmt...
     d                 s             11  2
     d decFClassFactor...
     d                 s              9  6
     d decOtherClassFactor...
     d                 s              9  6
     d decFClassPercent...
     d                 s              9  4
     d decOtherClassPercent...
     d                 s              9  4
     D decAssessmentFactor...
     D                 s              9  6
     D decAssessmentPremium...
     D                 s             11  2
     D decAssessmentPercent...
     D                 s              9  4
     D decIncLimitFactor...
     D                 s              9  6

     d* Regular Premium
     d                 ds
     d  prmr                   1     77  2
     d                                     DIM(7)
     d  EL1APR                 1     11  2
     d  EL2APR                12     22  2
     d  EL3APR                23     33  2
     d  EL4APR                34     44  2
     d  EL5APR                45     55  2
     d  EL6APR                56     66  2
     d  EL7APR                67     77  2

     d* F-Class Premium
     d                 ds
     d  prmf                   1     77  2
     d                                     DIM(7)
     d  EL1FPR                 1     11  2
     d  EL2FPR                12     22  2
     d  EL3FPR                23     33  2
     d  EL4FPR                34     44  2
     d  EL5FPR                45     55  2
     d  EL6FPR                56     66  2
     d  EL7FPR                67     77  2

     d* Assessment Heading
     d                 ds
     d  ahdg                   1    140
     d                                     DIM(7)
     d  RR1STH                 1     20
     d  RR2NDH                21     40
     d  RR3RDH                41     60
     d  RR4THH                61     80
     d  RR5THH                81    100
     d  RR6THH               101    120
     d  RR7THH               121    140

     d* Assessment Level
     d                 ds
     d  alvl                   1      7
     d                                     DIM(7)
     d  RR1STL                 1      1
     d  RR2NDL                 2      2
     d  RR3RDL                 3      3
     d  RR4THL                 4      4
     d  RR5THL                 5      5
     d  RR6THL                 6      6
     d  RR7THL                 7      7

     d* Calculate Assessment Deductible
     d                 ds
     d  acad                   1      7                                         Calc Ass Ded
     d                                     DIM(7)
     d  RR1ATD                 1      1
     d  RR2ATD                 2      2
     d  RR3ATD                 3      3
     d  RR4ATD                 4      4
     d  RR5ATD                 5      5
     d  RR6ATD                 6      6
     d  RR7ATD                 7      7

     d* F-Class Percentage
     d                 ds
     d  pctf                   1     63  6
     d                                     DIM(7)
     d  RR1STF                 1      9  6
     d  RR2NDF                10     18  6
     d  RR3RDF                19     27  6
     d  RR4THF                28     36  6
     d  RR5THF                37     45  6
     d  RR6THF                46     54  6
     d  RR7THF                55     63  6

     d* Regular Percentage
     d                 ds
     d  pctp                   1     63  6
     d                                     DIM(7)
     d  RR1STP                 1      9  6
     d  RR2NDP                10     18  6
     d  RR3RDP                19     27  6
     d  RR4THP                28     36  6
     d  RR5THP                37     45  6
     d  RR6THP                46     54  6
     d  RR7THP                55     63  6

     d* Increased Limits Basis
     d                 ds
     d  ilb                    1      7
     d                                     DIM(7)
     d  RR1ILF                 1      1
     d  RR2ILF                 2      2
     d  RR3ILF                 3      3
     d  RR4ILF                 4      4
     d  RR5ILF                 5      5
     d  RR6ILF                 6      6
     d  RR7ILF                 7      7
     d*** begin add ***  03/19/25  ****************************************
     d                 ds
     d  xfc                    1      7
     d                                     DIM(7)
     d  RR1XFC                 1      1
     d  RR2XFC                 2      2
     d  RR3XFC                 3      3
     d  RR4XFC                 4      4
     d  RR5XFC                 5      5
     d  RR6XFC                 6      6
     d  RR7XFC                 7      7
     d*** end add *****  03/19/25  ****************************************

     d dsAssessment    ds                  qualified inz
     d  chrPrintAss                   1a
     d  chrAssessment#...
     d                                1    dim(7)
     d  decAssessmentHdg...
     d                               20    dim(7)
     d  decAssessmentPct...
     d                                6  4 dim(7)
     d  decAssessmentAmt...
     d                               11  0 dim(7)

     d dsPriorBilled   ds                  qualified inz
     d  decPriorBilledAmt...
     d                               11  0 dim(7)

     d idx             s              1  0 Inz(0)
     d outIdx          s              1  0 Inz(0)
     d arrPreviousBilled...
     d                 s             11  2 dim(7) Inz(0)
     d*** End   Add *** 06/12/24 *******************************************

     d intIncr         s              3  0 inz(1)
     d intExmd         s              5  4
     d intExmd1        s              5  4
     d intCounth       s              3  0 inz(1)
     d intCountp       s              3  0 inz(1)
      *** beg add ***** 11/16/20 *****************
     d isNonSumDiv     s               n
      *** end add ***** 11/16/20 *****************
     d strAsp1         s              5
     d strAsp2         s              5  4
     d strAsp8         s              8
     d strWk20         s             20
     d strWk25         s             25

     d strTmp          s             27
     d strNum          s              5
     d strNum8         s              8
     d isCalcToPenny   s               n    Inz
     d xxExactAmt      s             11  2
     d*** Begin Add *** 05/15/20 ****************************************
     d isNetRated      s               n   Inz(*Off)
     d*** End   Add *** 05/15/20 ****************************************
     d*** Beg add *****  04/18/24  **************************************
     d xxFac3          s             11  9
     d*** End add *****  04/18/24  **************************************

      * data structure for heading
     d                 ds
     d  hdg                    1    270
     d                                     dim(10)
     d  hdg1                   1     27
     d  hdg2                  28     54
     d  hdg3                  55     81
     d  hdg4                  82    108
     d  hdg5                 109    135
     d  hdg6                 136    162
     d  hdg7                 163    189
     d  hdg8                 190    216
     d  hdg9                 217    243
     d  hdg10                244    270

      * Prorata Percents
     d                 ds
     d pro                     1    100
     d                                     dim(10)
     d  pro1                   1     10
     d  pro2                  11     20
     d  pro3                  21     30
     d  pro4                  31     40
     d  pro5                  41     50
     d  pro6                  51     60
     d  pro7                  61     70
     d  pro8                  71     80
     d  pro9                  81     90
     d  pro10                 91    100

     d con             s             27    dim(21) ctdata perrcd(1)
     D*** begin add ***  05/31/23  ***************************************
     D MSG             S             80    DIM(1) CTDATA PERRCD(1)
     D*** end add *****  05/31/23  ***************************************
     d @2NPer          c                   const('2ND PRD')
     d UP              c                   const('ABCDEFGHIJKLMNOPQRST-
     d                                     UVWXYZ')
     d LO              c                   const('abcdefghijklmnopqrst-
     d                                     uvwxyz')
     d***  Begin Add  ***  01/19/22  *****************
      /copy wPrEmplr
     d***  End   Add  ***  01/19/22  *****************
     d*** Beg Add *** 03/25/21 ********************************************
     d/copy wPrPmtSvc
     d*** End Add *** 03/25/21 ********************************************
     d/copy wprar
     d GetCurBal       pr            +2    like(EAamt)
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d  depMode                        n   options(*nopass)
     d GetPYrBal       pr            +2    like(EAamt)
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d GetDepBal       pr            +2    like(EAamt)
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     dCalcWithPrcMan   pr

     C*
     C*  KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  KEY LIST FOR THE FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE HARBOR EXPOSURE FILE
     C*
     C     HEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C*
     C     HEKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  KEY LIST FOR THE STAT FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  KEY LIST FOR THE EMPLOYER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEFDT            7 0
     C                   KFLD                    XXCLAS            5 0
     C*
     C     EPKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     MPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  KEY LIST FOR THE PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXEFDT
     C*
     C*  KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*
     C*  KEY LIST FOR THE AGENT HISTORY FILE
     C*
     C     AHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE DISCOUNT MASTER FILE
     C*
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    ELDISC
     C*
     C*  INCR LIMIT TABLE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXILEF            7 0
     C*
     C     LMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE FUND DETAIL
     C*
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
     C*
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    KKFYR
     C*
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     **
     **  SYSTEM CONTROL FILE
     **
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*  KEY LIST FOR THE EMPLOYER A/R FILE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XSDIV
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     EAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     EAKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XSDIV
     C                   KFLD                    XXDIV
     C     EAKY8         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EMPLOYER A/R BATCH FILE
     C*
     C     EAKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EAKEY4        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XSDIV
     C*
     C*  KEY LIST FOR THE LIMITS FILE
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X1KEY
     C*
     C* HCN CREDIT KEY
     C*
     C     HCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SVFYR
      *
     C*
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    ELFYR         SVFYR
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    EMNAME        XXNAME
     C     *LIKE         DEFINE    ELRTCD        XXRTCD
     C     *LIKE         DEFINE    RSJRST        XXST
     C     *LIKE         DEFINE    ELPFDT        X2EFDT
     C     *LIKE         DEFINE    EPARAT        XXARAT
     C     *LIKE         DEFINE    RRTERR        XXTERR
     C     *LIKE         DEFINE    XXFACT        XXFAC2
     C     *LIKE         DEFINE    ELENMP        XXSEXC
     C     *LIKE         DEFINE    ELEXM1        XXMOD
     C     *LIKE         DEFINE    ELEMPM        XXEMPM
     C     *LIKE         DEFINE    EAARTY        XXARTY
     C     *LIKE         DEFINE    TTKEY         X1KEY
     C     *LIKE         DEFINE    ELHCN         XXHCN
     C     *LIKE         DEFINE    ELHCN         SVHCN
     C     *LIKE         DEFINE    @@fact        inFactors
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@RPMY            6 0
     C                   PARM                    @@TODT            8 0
     C                   PARM                    @@MAPR           11 0
     C                   PARM                    @@ADLD           11 0
     C                   PARM                    @@PAYR           11 0
     C                   PARM                    @@PREM           11 0
     C                   PARM                    @@TAX            11 0
     C                   PARM                    @@PPRM           11 0
     C                   PARM                    @@PTAX           11 0
     C                   PARM                    @@FACT          143
     C                   PARM                    @@PREV           11 2
     C                   PARM                    @@SRON            1
     C                   PARM                    @@INFO           39
     C                   PARM                    @@HDG           270
     C                   PARM                    @@PRO           100
     C                   PARM                    @@PREM2          11 2
     C                   PARM                    @@TAX2           11 2
     C                   PARM                    @@PPRM2          11 2
     C                   PARM                    @@PTAX2          11 2
     C                   PARM                    @@FACT2          48
     C*** Begin Add *** 06/12/24 *******************************************
     C                   PARM                    @@ASMNT         267
     C                   PARM                    @@PRVASMNT       77
     C*** End   Add *** 06/12/24 *******************************************
     C*** Begin Add *** 06/24/24 *******************************************
     C                   PARM                    @@COLB           11 0
     C                   PARM                    @@PRCOLB         11 0
     C                   PARM                    @@PRVCOLBAL      11 0
     C*** End   Add *** 06/24/24 *******************************************
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
     C*  Agent/Producer retrieve program parameter list
     C*
     C     AGTPL         PLIST
     C                   PARM      ELCO#         ##CO#             3 0
     C                   PARM      ELFND         ##FND             3 0
     C                   PARM      ELEMP#        ##EMP#            9 0
     C                   PARM      ELDIV         ##DIV             5 0
     C                   PARM      ELFYR         ##FYR             3 0
     C                   PARM                    ##AGT#            5 0
     C                   PARM                    ##ANAM           60
     C                   PARM                    ##PROD            5 0
     C                   PARM                    ##PNAM           60
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   MOVE      *ZEROS        Q1FACT
     C                   MOVE      *ZEROS        Q2FACT
     C**** SET S/R ONLINE FLAG
      /free
       //*** beg chg ***** 11/10/20 ************
       ////If Q1prms > 12;
       ////   If @@sron = 'S';
       ////      xxsron = *on;
       ////   Else;
       ////      xxsron = *off;
       ////   Endif;
       ////Endif;
       // If called from Portal Email processing, exclude
       //  A/R Batch records created today by WISRPAY
       //  from the previous balance calculation.
       Select;
       When Q1prms <= 12;
       When @@sron = 'P';
          xxSrOn = *On;
          indPortalCall = *On;
       When @@sron = 'S';
          indPortalCall = *Off;
          xxSrOn = *On;
       Other;
          xxSrOn = *Off;
       EndSl;
       //*** end chg ***** 11/10/20 ************
       //*** Begin Add *** 06/12/24 ****************************************
       //
       // Initialize payroll totals
       If %parms() > 21;
          Clear @@Asmnt;
          Clear @@prvAsmnt;
       EndIf;

       Clear dsAssessment;
       Clear dsPriorBilled;
       decTotalFClassAmt     = 0;
       decTotalOtherClassAmt = 0;
       decTotalAllClassAmt   = 0;
       decFClassFactor       = 0;
       decOtherClassFactor   = 0;
       decAssessmentFactor   = 0;
       decAssessmentPremium  = 0;
       decAssessmentPercent  = 0;
       //*** End   Add *** 06/12/24 ****************************************
       //*** Begin Add *** 06/24/24 ****************************************
       // Initialize payroll totals
       If %parms() > 23;
          Clear @@ColB;                  // Collateral Billed
          Clear @@PrColB;                // Prior Collateral Billed
       EndIf;
       If %parms() > 25;
          Clear @@PrvColBal;             // Previous Collateral Balance
       EndIf;
       //*** End   Add *** 06/24/24 ****************************************
      /end-free
     c                   if        Q1prms > 10
     c                   eval      inFactors = @@fact
     c                   clear                   @@fact
     c                   endif
      *
     C                   TIME                    DSDTTM
     C     @@RRN         CHAIN     WDELP                              60
     C                   Z-ADD     ELCO#         XXCO#
     C                   Z-ADD     ELFND         XXFND
     C                   Z-ADD     ELFYR         XXFYR
     C                   Z-ADD     ELEMP#        XXEMP#
     C                   Z-ADD     ELDIV         XXDIV
     C                   MOVEL(P)  ELBSCD        X1KEY
     C     TBKY          CHAIN     PBS01                              60
     C                   SELECT
     C     BSFREQ        WHENEQ    'M'
     C     BSFREQ        OREQ      *BLANK
     C                   MOVE      *OFF          *IN46
     C                   Z-ADD     @@RPMY        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYYMM
     C                   MOVE      XXYYMM        X1                2 0
     C                   MOVE      MD(X1)        XXDAY
     C     XXRPMN        IFEQ      2
     C     XXRPYR        DIV       4             XXDIVR            2 0
     C                   MVR                     XXREM             2 0
     C     XXREM         IFNE      *ZERO
     C                   MOVE      28            XXDAY
     C                   ENDIF
     C                   ENDIF
     C                   MOVEL     XXYYMM        XXRPFM
     C                   MOVE      01            XXRPFM
     C                   OTHER
     C                   MOVE      *ON           *IN46
     C                   Z-ADD     @@TODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRPDT
     C                   ENDSL
     C*
      *** Begin Add *** 06/24/24 ***************************************
      /Free
       // Collateral Billed
       If ELcolrp <> 0 and %parms() > 23;
          @@ColB = %DecH((@@MAPR * ELcolrp)/100:11:0);
       Endif;
      /end-free
      *** End   Add *** 06/24/24 ***************************************
      *
      * SET REPORT DATE EQUAL TO POLICY EXPIRATION DATE IF POLICY
      * EXPIRES MID-MONTH.
      *
     C     XXRPDT        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        XXRPDT
     C                   ENDIF
      *
     C     ELANDT        IFNE      *ZEROS
     C     ELANDT        ANDNE     ELPFDT
     C                   Z-ADD     ELANDT        XXANDT
     C                   ADD       1             XXANYR
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXANDT
     C                   ENDIF
     C*
     C     ELARSM        IFEQ      'Y'
      *
     C     EMKY1         CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV             5 0
     C                   LEAVE
     C                   ENDIF
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO
      *
     C                   ENDIF
      *
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C                   MOVE      *BLANK        LRGDFL            1
     C     WCFL43        IFEQ      'Y'
     C                   MOVEL(P)  ELBSCD        X1KEY
     C     TBKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        ANDEQ     'L'
     C     ELKEY         CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           LRGDFL            1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     FNKEY         CHAIN     PFN01                              60
     C     F1KY          CHAIN     PF101                              60
      *** Begin Add *** 06/24/24 ***************************************
     C     FNKEY         CHAIN     PEX01                              60
      *** End   Add *** 06/24/24 ***************************************
       //*** Begin Add *** 06/12/24 ****************************************
       // Set the assessment print flag
       dsAssessment.chrPrintAss = ' ';
       If F1srAs = 'Y' Or ELsrAs = 'Y';
          dsAssessment.chrPrintAss = 'Y';
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
     C     F2KY          CHAIN     PF201                              60
     C     EMKEY         CHAIN     PEM01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      ELPFDT        X2EFDT
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*** Begin Add *** 05/15/20 **************************************
     C***  Begin Chg  ***  01/19/22  ************************
     c**********         Eval      isNetRated = *Off
     C**********         If        F1netR = 'Y' And ELpfDt >= F1netD
     c**********         Eval      isNetRated = *On
     c**********         EndIf
     c                   Eval      isNetRated =
     c                              IsUseNetRate(ELco#:ELfnd:ELemp#:ELdiv:ELfyr)
     C***  End   Chg  ***  01/19/22  ************************
     C*** End   Add *** 05/15/20 **************************************
     C                   MOVE      *OFF          *IN51
     C                   MOVEL     ELPFDT        X1YR              3 0
     C                   MOVEL     ELANDT        X1AYR             3 0
     C     XXST          IFEQ      'CA'
     C     X1YR          ANDEQ     102
     C     XXST          OREQ      'CA'
     C     X1AYR         ANDEQ     102
     C                   MOVE      *ON           *IN51
     C                   ENDIF
     C*
     C     *IN46         IFEQ      *OFF
     C     XXRPFM        IFLT      ELPFDT
     C                   MOVE      ELPFDT        XXRPFM
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     @@TODT        ##DT8
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    ##DT8             8 0
     C                   PARM      6             ##PAST            5 0
     C                   PARM      *BLANK        ##ERR             1
      *
     C                   Z-ADD     ##DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRPFM
      *
     C     XXRPFM        IFLT      ELPFDT
     C                   MOVE      ELPFDT        XXRPFM
     C                   ENDIF
     C*
     C* CALCULATE DAYS IN THE TIME PERIOD.
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C                   Z-ADD     XXRPFM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     XXRPDT        SRDTI
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
     C                   ADD       1             QQDAYS
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
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
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     QQERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     QQDAYS        XXDAYS            5 0
      *
     C                   ENDIF                                                  *IN46 = *OFF
       //*** Begin Add *** 06/12/24 **********************************
       // Total Payrolls into F types and Other types so as to be able
       // to calculate payroll factors.
       Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pep01;
       If %Found(wmepl);
          DoU %Eof(wmepl);

             // Access Payroll file
             SetGT  (xxCo#:ELrtCd:EPclas:ELptDt) ppr01;
             ReadPE (xxCo#:ELrtCd:EPclas) ppr01;
             DoW Not %Eof(wmprl1);
                If PRstat = 'A';
                   Leave;
                EndIf;
                ReadPE (xxCo#:ELrtCd:EPclas) ppr01;
             EndDo;

             // Sum up amounts into F-class totals and Other totals
             Select;
             When EPspec = 'F' Or PRlhCd = 'Y';
                decTotalFClassAmt += EPempr;
             Other;
                decTotalOtherClassAmt += EPempr;
             EndSl;

             decTotalAllClassAmt += EPempr;
             ReadE (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pep01;
          EndDo;
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
      *
      *  SET UP MANUAL PREMIUM FROM PASSED IN VALUE.
      *
     C                   Z-ADD     @@MAPR        SCMAPR           11 2
     C                   Z-ADD     @@PAYR        XXPAYR           15 4
      /Free
       If cltID = '150' And BSlpgm = 'L';
          xxPayR = 0;
       Endif;
      /End-Free
     C                   Z-ADD     1             XXPROR           12 9
     C                   MOVE      *OFF          *IN63
     C***  Begin Chg  ***  01/19/22  ************************
     C*****F1NETR        IFEQ      'Y'
     C*****ELPFDT        ANDGE     F1NETD
     C                   If        isNetRated
     C***  End   Chg  ***  01/19/22  ************************
     C*
     C*  SUBTRACT EXP MOD CHG FROM NORMAL PREMIUM, GET CURRENT EXP MOD AND
     C*  MULTIPLY IT BACK INTO ELENMP SO THAT NET RATE REFLECT CURRENT EXP
     C*  MOD
     C*
     C                   MOVE      *ON           *IN63
     C     ELMAPR        IFNE      *ZERO
     C     ELEMPM        ANDNE     *ZERO
     C*
     C     MPKY          CHAIN     PMP01                              60
     C*
     C*  GET EXP MOD CHG IF NO HIT OR IF WDMPP/MPEXPA = 0
     C*
     C     *IN60         IFEQ      *ON
     C     MPEXPA        OREQ      0
     C*
     C     EPKEY2        CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'
     C                   ADD       EPADLD        XXADLD           11 2
     C                   ENDIF
     C     EPKEY2        READE     PEP01                                  60
     C                   ENDDO
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      ELPFDT        X2EFDT
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   Z-ADD     0             X1PREM           11 2
     C                   Z-ADD     0             X2PREM           11 2
     C                   ADD       ELEXP1        X1PREM
     C                   ADD       ELEXP2        X1PREM
     C                   ADD       ELEXP3        X1PREM
     C                   ADD       ELESBJ        X2PREM
     C                   SUB       ELEDCS        X2PREM
     C                   SUB       ELEDCM        X2PREM
     C     RRDV1L        IFEQ      'J'
     C                   SUB       ELEDC2        X2PREM
     C                   ENDIF
     C     RRDV2L        IFEQ      'J'
     C                   SUB       ELEDV2        X2PREM
     C                   ENDIF
     C     ELWVFL        IFEQ      'J'
     C                   ADD       ELEWAV        X2PREM
     C                   ENDIF
     C     RRASFL        IFEQ      'J'
     C                   ADD       XXADLD        X2PREM
     C                   ENDIF
     C     X1PREM        IFNE      *ZERO
     C     X1PREM        SUB       X2PREM        MPEXPA
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     0             XXPRTT
     C     *IN46         IFEQ      *OFF
     C     XXRPDT        SUB       XXRPFM        XXDAYS
     C                   ELSE
     C                   Z-ADD     XXRPFM        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##FDT8            8 0
     C                   Z-ADD     XXRPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##TDT8            8 0
     C                   CALL      'SRDDIF8'
     C                   PARM                    ##FDT8
     C                   PARM                    ##TDT8
     C     XXDAYS        PARM                    ##DAYS
     C                   PARM                    ##ERR
     C                   ENDIF
     C*
     C                   ADD       1             XXDAYS
     C                   Z-ADD     XXRPFM        XXWKDT            7 0
     C     XXWKDT        DOWLE     XXRPDT
     C*
     C     EX(3)         IFNE      *ZERO
     C     XXWKDT        ANDGE     EX(3)
     C                   Z-ADD     EM(3)         XXMOD
     C                   ELSE
     C     EX(2)         IFNE      *ZERO
     C     XXWKDT        ANDGE     EX(2)
     C                   Z-ADD     EM(2)         XXMOD
     C                   ELSE
     C     EX(1)         IFEQ      0
     C     XXWKDT        ORLT      EX(1)
     C                   Z-ADD     1             XXMOD
     C                   ELSE
     C                   Z-ADD     EM(1)         XXMOD
     C                   ENDIF
     C                   ENDIF                                                  EX,2 IFNE
     C                   ENDIF                                                  EX,3 IFNE
     C*
     C     XXMOD         DIV(H)    XXDAYS        XXPROD           11 9
     C                   ADD(H)    XXPROD        XXPRTT           11 9
     C     *IN46         IFEQ      *OFF
     C                   ADD       1             XXWKDT
     C                   ELSE
     C                   Z-ADD     XXWKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DT8             8 0
     C                   CALL      'SRFUTR8'
     C                   PARM                    ##DT8             8 0
     C                   PARM      1             ##DAYS            5 0
     C                   PARM      *BLANK        ##ERR             1
     C                   Z-ADD     ##DT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXWKDT
     C                   ENDIF
      *
     C                   ENDDO
     C*
     C*
     C                   Z-ADD(H)  XXPRTT        XXWK54            5 4
     C     XXWK54        IFEQ      *ZERO
     C                   Z-ADD     1             XXWK54
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN71
     C     ELWVPC        IFEQ      0
     C     ELEWAV        ANDNE     0
     C     ELEWAV        ORNE      0
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *ON           *IN71
      *
     C     *IN46         IFEQ      *OFF
     C     1             DIV(H)    12            XXDPRO            9 8
     C                   ELSE
     C     XXDAYS        DIV(H)    365           XXDPRO
     C                   ENDIF
      *
     C     ELWVFL        IFEQ      'M'
     C     ELWVFL        OREQ      'J'
     C     ELEMPM        SUB       ELEWAV        XXSEXC
     C                   SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR           12 9
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C     ELEWAV        MULT(H)   XXDPRO        XXEWAV           11 0
     C     ELWVFL        IFEQ      'M'
     C     XXEWAV        MULT(H)   XXPROR        XXEWAV
     C                   ELSE
     C     XXEWAV        MULT(H)   XXWK54        XXEWAV
     C                   ENDIF
     C                   ELSE
     C     ELEMPM        SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C                   ENDIF
      *
     C     ELWVFL        IFEQ      'E'
     C     ELWVFL        OREQ      'S'
     C     ELENMP        SUB       ELEWAV        ELENMP
     C     ELENMP        DIV(H)    ELEMPM        X1PROR           12 9
     C                   MULT(H)   X1PROR        XXPROR
     C     XXEWAV        MULT(H)   XXDPRO        XXEWAV
     C     ELEWAV        MULT(H)   X1PROR        XXEWAV
     C                   ELSE
     C     ELWVFL        IFEQ      'D'
     C     ELEWAV        MULT(H)   XXDPRO        XXEWAV
     C     ELENMP        SUB       ELEWAV        ELENMP
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C                   ELSE
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C     XXEWAV        MULT      X1PROR        XXEWAV
     C                   ENDIF
     C                   ENDIF
      *
     C                   ELSE
     C     ELEMPM        SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C                   ENDIF
     C*
     C                   ENDIF                                                            ELMAPR
     C*
     C                   ENDIF                                                  F1NETR IFEQ
     C*
     C* SET UP ADDITIONAL LOAD FROM PASSED IN VALUE.
     C*
     C                   Z-ADD     @@ADLD        SCADLD           11 2
     C*
     C                   MOVE      *OFF          *IN62
     C                   Z-ADD     0             Y                 3 0
     C                   Z-ADD     XXYYMM        XXRPMY            5 0
     C*
     C                   Z-ADD     1             Z                 3 0
     C                   MOVE      '0'           *IN60
     C*
     C*   INITIALIZE WORK FIELDS
     C*
     C                   Z-ADD     0             XXPRCT            5 4
     C                   Z-ADD     0             XXPRC2            5 4
     C                   Z-ADD     0             XXPCT             5 4
     C                   Z-ADD     0             XXWK92           11 2

      /free
       Clear hdg;
       Clear pro;
      /end-free

     c                   if         inFactors = 'L'
     c                   eval       *in93 = *on
     c                   else
     c                   eval       *in93 = *off
     c                   endif
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*  MANUAL PREMIUM
     C*
     C                   Select
     C                   When      cltID = '150' and ELmanp <> 0
     C                   Callp     CalcWithPrcMan
     C                   Other

     C     *IN63         IFEQ      *OFF
     C                   Z-ADD     SCMAPR        SCBLAM           11 2
     C*
     C*  SET FIRST DAY OF MONTH IF NOT WEEKLY REPORTING
     C*
     C     *IN46         IFEQ      *OFF
     C                   Z-ADD     XXRPDT        XXRPFM            7 0
     C                   MOVE      01            XXRPFM
     C*
     C     XXRPYM        IFEQ      XXFPYM
     C                   MOVE      XXFPDY        XXRPFM
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  INCREASED LIMITS
     C*
     C                   Z-ADD     *ZERO         LMPCT
     C*
     C     ELILCD        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVEL     ELILCD        XXKEY
     C*
     C     ELILEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELILEF        XXILEF
     C                   ENDIF
     C*
     C     ELILEX        IFEQ      *ZERO
     C     ELI2EF        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELI2EF        XXILEX
     C                   ENDIF
     C                   ELSE
     C                   MOVE      ELILEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELILEX        ANDNE     ELILEF
     C     ELILEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELILEX        ANDEQ     *ZERO
     C*
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LMPCT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LMPCT         MULT      .01           XXPRCT
     C     XXPRCT        MULT(H)   SCBLAM        XXRND            11 0
     C                   Z-ADD     XXRND         XXINCR           11 0
     C*
     C                   Z-ADD     0             LMPCT
     C     ELINL2        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY            10
     C                   MOVEL     ELINL2        XXKEY
     C*
     C     ELI2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELI2EF        XXILEF
     C                   ENDIF
     C*
     C     ELI2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELI2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELI2EX        ANDNE     ELI2EF
     C     ELI2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELI2EX        ANDEQ     *ZERO
     C*
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LMPCT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LMPCT         MULT      .01           XXPRC2
     C     XXPRC2        MULT(H)   SCBLAM        XXRND            11 0
     C     XXPRC2        ADD       XXPRCT        XXPCT
     C     XXPCT         MULT      100           LMPCT
     C                   ADD       XXINCR        XXRND
     C                   ADD       XXRND         SCBLAM
     C                   Z-ADD     LMPCT         Q1LMPC
     C                   Z-ADD     SCBLAM        Q1TOT1
      *** beg add ***** 11/29/18 ***************************
      /Free
       Eval(h) xxExactAmt = scMaPr + (scMaPr * xxPct);
       Q2tot1 = xxExactAmt;
      /End-Free
      *** end add ***** 11/29/18 ***************************
      //*** Beg Add ***** 04/18/24  **********************************
       xxFac3 = 1;
       If RRdcap = 'M' and ELdbcr <> *Zero;
          If ELsdef = *Zero;
             xxIlef = ELpfdt;
          Else;
             xxIlef = ELsdef;
          Endif;
          If ELsdex = *Zero;
             xxIlex = ELptdt;
          Else;
             xxIlex = ELsdex;
          Endif;
          If (xxIlef <= xxRpdt and xxIlex >= xxRpfm
           and ELsdex <> ELsdef and ELsdex <> *Zero)
           or (xxIlef <= xxRpdt and xxIlex >= xxRpfm and ELsdex = *Zero);
             If ELdbcr > 1;
                If RRdbhd = *Blank;
                   strTmp = Con(7);
                Else;
                   strWk20 = %Xlate(lo:up:RRdbhd);
                   strTmp  = strWk20;
                Endif;
             Else;
                If RRcrhd = *Blanks;
                   strTmp = Con(8);
                Else;
                   strWk20 = %Xlate(lo:up:RRcrhd);
                   strTmp  = strWk20;
                Endif;
             Endif;
             %Subst(strTmp:23:1) = '.';
             strAsp1 = %Editc(ELdbcr:'X');
             strNum = strAsp1;

             %Subst(strTmp:22:1) = %Subst(strNum:1:1);
             %Subst(strTmp:24:1) = %Subst(strNum:2:1);
             %Subst(strTmp:25:1) = %Subst(strNum:3:1);
             %Subst(strTmp:26:1) = %Subst(strNum:4:1);
             %Subst(strTmp:27:1) = %Subst(strNum:5:1);
             Hdg(intIncr) = strTmp;
             clear strTmp;

             If xxIlef > xxRpfm or xxIlex < xxRpdt;
                If xxIlef < xxRpfm;
                   xxFrDt = xxRpfm;
                Else;
                   xxFrDt = xxIlef;
                Endif;
                If xxIlex > xxRpdt;
                   xxToDt = xxRpdt;
                Else;
                   XXToDt = xxIlex;
                Endif;

                Exsr  SRPRO;
                ELdbcr = ELdbcr - 1;
                Eval(h) ELdbcr = ELdbcr * xxPro;
                ELdbcr = ELdbcr + 1;

                %Subst(strTmp:8:4) = '%)**' ;
                %Subst(strTmp:3:1) = '.' ;
                strAsp1 = %Editc(ELdbcr:'X');
                strNum = strAsp1;
                %Subst(strTmp:1:1) = '(';
                %Subst(strTmp:2:1) = %Subst(strNum:1:1);
                %Subst(strTmp:4:1) = %Subst(strNum:2:1);
                %Subst(strTmp:5:1) = %Subst(strNum:3:1);
                %Subst(strTmp:6:1) = %Subst(strNum:4:1);
                %Subst(strTmp:7:1) = %Subst(strNum:5:1);
                Pro(intIncr) = strTmp;
                Clear strTmp;
             Endif;
             xxFac3 = ELdbcr - 1;
             xxPro = LMPct * .01;
             xxPro = xxPro + 1;
             xxFac3 = xxFac3 + xxPro;
             Eval(h) xxFac3 = xxFac3/xxPro;
             intIncr = intIncr + 1;
          Endif;
       Endif;
      //*** End Add ***** 04/18/24  **********************************
     C*
     C*  CALCULATE DISCOUNT FACTOR BEFORE MODIFIER
     C*
     C                   Z-ADD     1             XXFACT           11 9
     C                   Z-ADD     1             XXFAC1           11 9
     C*
     C*  SET UP FLAG TO CALCULATE AMOUNT OF DISCOUNT TO APPLY TO FLAT CHARGE
     C*  OR SPECIFIC WAIVER. XXWFAC HOLDS THE DISCOUNT APPLIED TO THE CHARGE
     C*
     C                   Z-ADD     *HIVAL        XXWVFL
     C                   Z-ADD     1             XXWFAC           11 9
     C     ELWVPC        IFEQ      0
     C     ELEWAV        ANDNE     0
     C     ELEWAV        ORNE      0
     C     ELWVTP        ANDEQ     'S'
      *
      *  CONVERT PREMUIM LEVEL OF WAIVER TO A NUMBER USED TO DETERMINE WHAT
      *  FACTORS APPLY TO THE WAIVER CHARGE
      *
     C                   SELECT
     C     ELWVFL        WHENEQ    'M'
     C                   Z-ADD     1             XXWVFL            1 0
     C     ELWVFL        WHENEQ    'J'
     C                   Z-ADD     2             XXWVFL
     C     ELWVFL        WHENEQ    'E'
     C                   Z-ADD     3             XXWVFL
     C     ELWVFL        WHENEQ    'S'
     C                   Z-ADD     4             XXWVFL
     C     ELWVFL        WHENEQ    'D'
     C                   Z-ADD     5             XXWVFL
     C                   OTHER
     C                   Z-ADD     9             XXWVFL
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVTP        ANDEQ     'B'
     C     ELWVFL        ANDEQ     'M'
     C*
      /free
       strWk20 = %Xlate(Lo:Up:RRwvhd);
       strTmp = strWk20;
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELwvpc:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0';
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C*  TO CALCULATE EFFECT OF WAIVER CHG ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE WAIVER CHG PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     ELWVPC        MULT      .01           XXFAC1
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFAC1
     C                   DIV(H)    XXPRO         XXFAC1
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C*  DEDUCTIBLE DISCOUNT
     C*
     C     ELDDCT        IFNE      *ZERO
     C     RRRDPR        ANDEQ     'M'
     C     LRGDFL        ANDNE     'Y'
     C*
     C     ELDCEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDCEF        XXILEF
     C                   ENDIF
     C*
     C     ELDCEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELDCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDNE     ELDCEF
     C     ELDCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDEQ     *ZERO
     C*
      /free
       strTmp = Con(1);
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' or %Subst(strNum:1:1) >'0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDDCT
     C*
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     ELDDCT        MULT      -.01          XXFACT
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFACT
     C                   DIV(H)    XXPRO         XXFACT
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C*
     C     ELDD2P        IFNE      *ZERO
     C*
     C     EL2DEF        IFEQ      *ZERO
     C                   MOVE      ELDCEX        XXILEF
     C                   ELSE
     C                   MOVE      EL2DEF        XXILEF
     C                   ENDIF
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      EL2DEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDNE     EL2DEF
     C     EL2DEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDEQ     *ZERO
     C*
      /free
       strTmp = con(13);
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELdd2p:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' or %Subst(strNum:1:1) >'0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDD2P
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %editc(ELdd2p:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
          %Subst(strTmp:1:1) = '(';
          %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
          %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C*
     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     ELDD2P        MULT      -.01          XXFAC2
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFAC2
     C                   DIV(H)    XXPRO         XXFAC2
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ADD       XXFAC2        XXFACT
     C                   SUB       1             XXFACT
     C                   ENDIF
     C*
     C                   ENDIF
      * MOVED ENDIF HERE SO THAT 2ND DED APPLIED TO MANUAL WOULD NOT BE
      * APPLIED UNLESS THERE IS A FIRST DED APPLIED TO MANUAL
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT MANUAL PREMIUM LEVEL
      *
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C     LDPLV1        ANDEQ     'M'
     C*
     C     LDEFD1        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD1        XXILEF
     C                   ENDIF
     C*
     C     LDEXP1        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP1        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDNE     LDEFD1
     C     LDEXP1        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT1
     C*
     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     LDPCT1        MULT      -.01          XXFACT
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFACT
     C                   DIV(H)    XXPRO         XXFACT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C     LDPLV2        ANDEQ     'M'
     C*
     C     LDEFD2        IFEQ      *ZERO
     C                   MOVE      LDEXP1        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD2        XXILEF
     C                   ENDIF
     C*
     C     LDEXP2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDNE     LDEFD2
     C     LDEXP2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT2
     C*
     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     LDPCT2        MULT      -.01          XXFAC2
     C     LMPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFAC2
     C                   DIV(H)    XXPRO         XXFAC2
     C*
     C                   ADD       XXFAC2        XXFACT
     C                   SUB       1             XXFACT
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ADD       XXFAC1        XXFACT
     C                   SUB       1             XXFACT
      //*** Beg Add ***** 04/18/24  *********************************
       xxFact = xxFact + xxFac3;
       xxFact = xxFact - 1;
      //*** End Add ***** 04/18/24  *********************************
     C*
     C*  HCN CREDIT
     C*
     C     ELHCN         IFNE      *ZERO
     C     ELHCN         ANDNE     1
     C*
     C                   Z-ADD     ELHCN         SVHCN
     C                   Z-ADD     1             XXHCN
     C                   MOVE      *BLANK        XXHCNF            1
     C*
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     SVHCN         ELHCN
     C*
     C     HCIN          IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      HCIN          XXILEF
     C                   ENDIF
     C*
     C     HCOUT         IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      HCOUT         XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     HCOUT         ANDNE     HCIN
     C     HCOUT         ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     HCOUT         ANDEQ     *ZERO
     C*
     C                   MOVE      'Y'           XXHCNF
     C                   SUB       1             ELHCN
      /free
       strTmp = con(18);
       %Subst(strTmp:23:1) = '.';
       strasp1 = %editc(ELhcn:'X');
       strNum = strAsp1;

       %Subst(strTmp:22:1) = %Subst(strNum:1:1);
       %Subst(strTmp:24:1) = %Subst(strNum:2:1);
       %Subst(strTmp:25:1) = %Subst(strNum:3:1);
       %Subst(strTmp:26:1) = %Subst(strNum:4:1);
       %Subst(strTmp:27:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELHCN
     C*
     C                   ENDIF                                                  XXILEF IFGT
     C*
     C                   ADD       ELHCN         XXHCN
      /free
       %Subst(strTmp:8:4) = '%)**';
       %Subst(strTmp:3:1) = '.';
       strAsp1 = %EditC(xxHcn:'X');
       strnum  = strAsp1;
       %Subst(strTmp:1:1) = '(';
       %Subst(strTmp:2:1) = %Subst(strNum:1:1);
       %Subst(strTmp:4:1) = %Subst(strNum:2:1);
       %Subst(strTmp:5:1) = %Subst(strNum:3:1);
       %Subst(strTmp:6:1) = %Subst(strNum:4:1);
       %Subst(strTmp:7:1) = %Subst(strNum:5:1);

       pro(intIncr) = strTmp;
       clear strTmp;
      /end-free
     C*
     C                   ENDIF                                                  XXILEF IFLE
     C*
     C     HCKY          READE     PHC01                                  60
     C                   ENDDO
     C*
     C     XXHCNF        IFEQ      'Y'
     C*
     C                   MULT(H)   XXHCN         XXFACT
     C     XXWVFL        IFEQ      1
     C                   MULT(H)   XXHCN         XXWFAC
     C                   ENDIF
      /free
       intIncr = intIncr + 1;
      /end-free
     C*
     C                   ENDIF                                                  XXHCNF IFEQ
     C*
     C                   ENDIF                                                  ELHCN IFNE
     C*
     C*  MANAGED CARE
     C*
     C     ELASPM        IFNE      *ZERO
     C*
     C     ELMCEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELMCEF        XXILEF
     C                   ENDIF
     C*
     C     ELMCEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELMCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELMCEX        ANDNE     ELMCEF
     C     ELMCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELMCEX        ANDEQ     *ZERO
     C*
      /free
       If RR1ahd = *blanks;
         strTmp = con(2);
       Else;
          strWk20 = %Xlate(lo:up:RR1ahd);
          strTmp  = strWk20;
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELaspm:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0';
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASPM
     C*
      /free
       %Subst(strTmp:7:4) = '%)**';
       %Subst(strTmp:4:1) = '.';
       strAsp1 = %Editc(ELaspm:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C                   ENDIF
     C*
     C     ELASPM        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  SAFETY CREDIT
     C*
     C     ELASPS        IFNE      *ZERO
     C*
     C     ELSCEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELSCEF        XXILEF
     C                   ENDIF
     C*
     C     ELSCEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      ELSCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSCEX        ANDNE     ELSCEF
     C     ELSCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSCEX        ANDEQ     *ZERO
     C*
      /free
       If RR1bhd = *blanks;
         strTmp = con(3);
       Else;
         strWk20 = %Xlate(lo:up:RR1bhd);
         strTmp = strWk20;
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELasps:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASPS
     C*
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELasps:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C     ELASPS        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF

      /free
       intIncr = intIncr + 1;
      /end-free
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION - FROM SUBJECT
     C*
     C     ELASP2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'J'
     C     ELASP2        ORNE      *ZERO
     C     RRDV1L        ANDEQ     'M'
     C*
     C     ELDVEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDVEF        XXILEF
     C                   ENDIF
     C*
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
      /free
       If RRdv1h = *blanks;
          strTmp = con(4);
       Else;
         strWk20 = %Xlate(lo:up:RRdv1h);
         strTmp = strWk20;
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELasp2:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C*
      /free
       %Subst(strTmp:7:4) = '%)**';
       %Subst(strTmp:4:1) = '.';
       strAsp1 = %Editc(ELasp2:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV1L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF
     C*  DEVIATION 1 PERIOD 2- FROM SUBJECT
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'J'
     C     ELD1S2        ORNE      *ZERO
     C     RRDV1L        ANDEQ     'M'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD1D2        XXILEF
     C                   ENDIF
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD1X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C*
      /free
       strWk25 = *blanks;
       If RRdv1h = *blanks;
          strWk25 = @2nper + ' ' + con(4) ;
          strTmp = strWk25 ;
       Else;
          strWk25 = %Xlate(lo:up:RRdv1h);
          strWk25 = @2nper + ' ' + %Trim(strWk25);
          strTmp = strWk25;
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELd1s2:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C*
      /free
       %Subst(strTmp:7:4) = '%)**';
       %Subst(strTmp:4:1) = '.';
       strAsp1 = %Editc(ELd1s2:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV1L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION - FROM SUBJECT
     C*
      /free
       If (RRdv2l = 'J' or RRdv2l = 'M') and ((ELdev2 <> 0 and cltId <> '025'
          And cltId <> '136') or (ELstfc <> 0 and cltId = '025') or
          (ELdev2F <> 0 and cltId = '136'));
      /End-free
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
      /free







       Select;
       //>>>>>>>>>>>>> Stability Factor
       When cltID = '025';
          If RRdv2h = *blanks;
             strTmp =  con(17);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          %Subst(strTmp:20:1) = '.';
          strAsp8 = %Editc(ELstfc:'X');
          strNum8 = StrAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:18:1) = %Subst(strNum8:1:1);
          Endif;

          %Subst(strTmp:19:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:21:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:22:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:23:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:24:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:25:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:26:1) = %Subst(strNum8:8:1);
       //>>>>>>>>>>>>> Territory Rating
       When cltID = '136';
          If RRdv2h = *blanks;
             strTmp = con(19);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2F:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          %Subst(strTmp:22:1) = '.';
          %Subst(strTmp:23:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       // >>>>>>>>>>>> Default Deviation #2
       Other;
          If RRdv2h = *blanks;
             strTmp = con(9);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:23:1) = '.';
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       EndSl;

       hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
      /free
       Select;
       When cltId = '025';
          eval(h) ELstfc  = ELstfc * xxPro;
       When cltId = '136';
          eval(h) ELdev2F = ELdev2F * xxPro;
       Other;
          eval(h) ELdev2  = ELdev2 * xxPro;
       EndSl;
      /End-free
     C*
      /free
       Select;
       When cltId = '025';
          ELstfc = ELstfc - 1;
          Elstfc = ELstfc * xxpro;
          ELstfc = Elstfc + 1 ;
          strAsp8 = %Editc(Elstfc:'X');
          strNum8 = strAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:1:1) = '(';
             %Subst(strTmp:2:1) = %Subst(strNum8:1:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:4:1) = '.';
          %Subst(strTmp:5:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:6:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:7:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:8:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:9:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:10:1) = ')';
       When cltId = '136';
          strAsp1 = %Editc(ELdev2F:'X');
          strNum = strAsp1;

          %Subst(strTmp:1:1) = '(';
          %Subst(strTmp:2:1) = %Subst(strNum:2:1);
          %Subst(strTmp:3:1) = '.';
          %Subst(strTmp:4:1) = %Subst(strNum:3:1);
          %Subst(strTmp:5:1) = %Subst(strNum:4:1);
          %Subst(strTmp:6:1) = %Subst(strNum:5:1);
          %Subst(strTmp:7:4) = ')';
       Other;
          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:2:1) <> '0';
            %Subst(strTmp:1:1) = '(';
            %Subst(strTmp:2:1) = %Subst(strNum:2:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum:3:1);
          %Subst(strTmp:4:1) = '.';
          %Subst(strTmp:5:1) = %Subst(strNum:4:1);
          %Subst(strTmp:6:1) = %Subst(strNum:5:1);
          %Subst(strTmp:7:4) = '%)**';
       EndSl;
       pro(intIncr) = strTmp;
       clear strtmp;
      /end-free
     C                   ENDIF
     C*
      /Free
       Select;
       When cltId = '025';
          xxPro = ELstfc * -.01;
       When cltId = '136';
          xxPro = ELdev2F * -.01;
       Other;
          xxPro = ELdev2 * -.01;
       EndSl;
      /End-free
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV2L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION 2ND PERIOD- FROM SUBJECT
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     ELD2S2        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2D2        XXILEF
     C                   ENDIF
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C*
      /free
       If cltid <> '025';
          If RRdv2h = *blanks;
             strWk25 = @2nper + ' ' + con(9);
             strTmp= strWk25;
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strWk25 = @2nper + ' ' + %Trim(strWk25);
             strTmp= strWk25;
          Endif;

          %Subst(strTmp:25:1) = '%';
          %Subst(strTmp:23:1) = '.';
          strAsp1 = %Editc(ELd2s2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0';
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Else;
          If RRdv2h = *blanks;
             strTmp = con(17);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          %Subst(strTmp:20:1) = '.';
          strAsp8 = %Editc(ELstfc:'X');
          strNum8 = strAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:18:1) = %Subst(strNum8:1:1);
          Endif;

          %Subst(strTmp:19:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:21:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:22:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:23:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:24:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:25:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:26:1) = %Subst(strNum8:8:1);
       Endif;     //CLTID <> '025'

       hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD2S2

      /free
       %Subst(strTmp:7:4) = '%)**';
       %Subst(strTmp:4:1) = '.';
       strAsp1 = %Editc(ELd2s2:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       pro(intIncr) = strTmp;
       clear strTmp;
      /end-free

     C                   ENDIF
     C*
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV2L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVFL        ANDEQ     'J'
     C     ELWVTP        ANDEQ     'B'
     C*
      /free
       strWk20 = %Xlate(lo:up:RRwvhd);
       strTmp = strWk20;
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELwvpc:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
      /free
       intIncr = intIncr + 1;
      /end-free
     C*
     C                   ENDIF
     C*
      *
      * LARGE DEDUCTIBLE AT SUBJECT PREMIUM LEVEL
      *
     C                   MOVE      'J'           XXPLV1            1
     C                   MOVE      'J'           XXPLV2            1
     C                   EXSR      APLGDD
     C                   Z-ADD(H)  XXFACT        X5FACT           11 5
     C     SCBLAM        MULT(H)   X5FACT        XXRND
     C                   Z-ADD     XXRND         SCBLAM
     C                   Z-ADD     X5FACT        Q1DSCB
     C                   Z-ADD     SCBLAM        Q1TOT2
      /Free
       Eval(h) xxExactAmt = xxExactAmt * x5Fact;
       Q2tot2 = xxExactAmt;
      /End-Free
     C*
     C*  EXPERIENCE MODIFIER AND MODIFIED PREMIUM
     C*
     C     *LIKE         DEFINE    ELEXM1        XXEXMD
     C     *LIKE         DEFINE    ELARP1        XXARAP
     C                   Z-ADD     0             XXARAP
     C*
     C*  CALCULATE THE EXPERIENCE MODIFIER
     C*
     C     ELEXM1        IFNE      *ZERO
     C     ELEXM3        IFNE      0
     C     ELEMD3        ANDLT     XXRPDT
     C                   Z-ADD     ELEXM3        XXEXMD
     C                   ELSE
     C     ELEXM2        IFNE      0
     C     ELEMD2        ANDLT     XXRPDT
     C                   Z-ADD     ELEXM2        XXEXMD
     C                   ELSE
     C                   Z-ADD     ELEXM1        XXEXMD
     C                   END
     C                   END
     C*
     C*  CALCULATE THE ARAP MODIFIER
     C*
     C     ELEXM3        IFNE      0
     C     ELEMD3        ANDLE     XXRPDT
     C                   Z-ADD     ELARP3        XXARAP
     C                   ELSE
     C     ELEXM2        IFNE      0
     C     ELEMD2        ANDLE     XXRPDT
     C                   Z-ADD     ELARP2        XXARAP
     C                   ELSE
     C                   Z-ADD     ELARP1        XXARAP
     C                   END
     C                   END
     C*
     C     XXARAP        IFNE      *ZERO
     C                   SUB       1             XXARAP
     C                   ENDIF

     C     XXEXMD        MULT(H)   SCBLAM        XXRND
     C                   Z-ADD     XXRND         SCBLAM
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXEXMD        XXWFAC
     C                   ENDIF
     C                   ENDIF                                                  ELEXM1 IFNE
     C                   Z-ADD     XXEXMD        Q1EXPM
     C                   Z-ADD     SCBLAM        Q1TOT3
      /Free
       If xxExMd <> 0;
          Eval(h) xxExactAmt = xxExactAmt * xxExMd;
       Endif;
       Q2tot3 = xxExactAmt;
      /End-Free
     C*
     C*  CALCULATE DISCOUNT FACTOR AFTER MODIFIER
     C*
     C                   Z-ADD     1             XXFACT           11 9
     C*
     C*  SCHEDULE DB/CR
     C     RRDCAP        IFEQ      ' '
     C                   EXSR      CALDBR
     C                   ENDIF                                                  ELDBCR IFNE
     C*
     C*  DEVIATION - APPLIED TO EXPERIENCE
     C*
     C     ELASP2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'E'
     C*
     C     ELDVEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDVEF        XXILEF
     C                   ENDIF
     C*
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
      /free
       If RRdv1h = *blanks;
          strTmp = con(4);
       Else;
          strWk20 = %Xlate(lo:up:RRdv1h);
          strTmp = strWk20;
       Endif;

       If cltid = '045';
          strAsp2 = (ELasp2/100);
          strAsp2 = strAsp2 - 1;
          %Subst(strTmp:23:1) = '.';
          strAsp1 = %Editc(strAsp2:'X');
          strNum  = strAsp1;

          %Subst(strTmp:22:1) = %Subst(strNum:1:1);
          %Subst(strTmp:24:1) = %Subst(strNum:2:1);
          %Subst(strTmp:25:1) = %Subst(strNum:3:1);
          %Subst(strTmp:26:1) = %Subst(strNum:4:1);
          %Subst(strTmp:27:1) = %Subst(strNum:5:1);

          If %Subst(strNum:5:1) = '0' and intExMd = 0;
             intExMd1 = 0;
          Endif;
          Clear strTmp;

       Else;
          %Subst(strTmp:26:1) = '%';
          %Subst(strTmp:23:1) = '.';
          strAsp1 = %Editc(ELasp2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          hdg(intIncr) = strTmp;
          Clear strTmp;
       Endif;

      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C*
      /free
       %Subst(strTmp:7:4) = '%)**';
       %Subst(strTmp:4:1) = '.';
       strAsp1 = %Editc(ELasp2:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
          %Subst(strTmp:1:1) = '(';
          %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
          %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF
     C*  DEVIATION 1 PERIOD 2- APPLIED TO EXPERIENCE
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'E'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD1D2        XXILEF
     C                   ENDIF
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD1X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C*
      /free
       strWk25 = *blanks;
       If RRdv1h = *blanks;
          strWk25 = @2nper + ' ' + %Trim(con(4));
          strTmp = strWk25;
       Else;
          strWk25 = %Xlate(lo:up:RRdv1h);
          strWk25 = @2nper + ' ' + %Trim(strWk25);
          strTmp = strWk25;
       Endif;

       If cltid= '045';
          strAsp2 = (ELd1s2/100);
          strAsp2 = strAsp2 - 1;
          %Subst(strTmp:23:1) = '.';
          strAsp1  = %Editc(strAsp2:'X');
          strNum = strAsp1;

          %Subst(strTmp:22:1) = %Subst(strNum:1:1);
          %Subst(strTmp:24:1) = %Subst(strNum:2:1);
          %Subst(strTmp:25:1) = %Subst(strNum:3:1);
          %Subst(strTmp:26:1) = %Subst(strNum:4:1);
          %Subst(strTmp:27:1) = %Subst(strNum:5:1);
          Hdg(intIncr) = strTmp;

          If %Subst(strNum:5:1) = '0' and intExmd = 0;
             intExMd1 = 0;
          Endif;

          Clear strTmp ;
       Else;   //CLtid =045.
          %Subst(strTmp:26:1) = '%';
          %Subst(strTmp:23:1) = '.';
          strAsp1 = %Editc(ELd1s2:'X');
          strNum  = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          Hdg(intIncr) = strTmp;
          Clear strTmp;
       Endif;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C                   ENDIF
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
      /free
       %Subst(strTmp:7:4) = '%)**';
       %Subst(strTmp:4:1) = '.';
       strAsp1 = %Editc(ELd1s2:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
          %Subst(strTmp:1:1) = '(';
          %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
          %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C                   ENDIF
     C*  2ND DEVIATION - APPLIED TO EXPERIENCE
     C*

      /free
       If RRdv2l = 'E' and ((ELdev2 <> 0 and cltId <> '025'
          And cltId <> '136') or (ELstfc <> 0 and cltId = '025') or
          (ELdev2F <> 0 and cltId = '136'));
      /End-free
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
      /free
       Select;
       //>>>>>>>>>>>>> Stability Factor
       When cltID = '025';
          If RRdv2h = *blanks;
             strTmp =  con(17);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          %Subst(strTmp:20:1) = '.';
          strAsp8 = %Editc(ELstfc:'X');
          strNum8 = StrAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:18:1) = %Subst(strNum8:1:1);
          Endif;

          %Subst(strTmp:19:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:21:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:22:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:23:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:24:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:25:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:26:1) = %Subst(strNum8:8:1);
       //>>>>>>>>>>>>> Territory Rating
       When cltID = '136';
          If RRdv2h = *blanks;
             strTmp = con(19);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2F:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          %Subst(strTmp:22:1) = '.';
          %Subst(strTmp:23:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       // >>>>>>>>>>>> Default Deviation #2
       Other;
          If RRdv2h = *blanks;
             strTmp = con(9);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:23:1) = '.';
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       EndSl;

       hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
      /free
      /End-free
      /free
       Select;
       When cltId = '025';
          eval(h) ELstfc  = ELstfc * xxPro;
       When cltId = '136';
          eval(h) ELdev2F = ELdev2F * xxPro;
       Other;
          eval(h) ELdev2  = ELdev2 * xxPro;
       EndSl;
      /End-free
      /free
       Select;
       When cltId = '025';
          ELstfc = ELstfc - 1;
          Elstfc = ELstfc * xxpro;
          ELstfc = Elstfc + 1 ;
          strAsp8 = %Editc(Elstfc:'X');
          strNum8 = strAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:1:1) = '(';
             %Subst(strTmp:2:1) = %Subst(strNum8:1:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:4:1) = '.';
          %Subst(strTmp:5:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:6:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:7:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:8:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:9:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:10:1) = ')';
       When cltId = '136';
          strAsp1 = %Editc(ELdev2F:'X');
          strNum = strAsp1;

          %Subst(strTmp:1:1) = '(';
          %Subst(strTmp:2:1) = %Subst(strNum:2:1);
          %Subst(strTmp:3:1) = '.';
          %Subst(strTmp:4:1) = %Subst(strNum:3:1);
          %Subst(strTmp:5:1) = %Subst(strNum:4:1);
          %Subst(strTmp:6:1) = %Subst(strNum:5:1);
          %Subst(strTmp:7:4) = ')';
       Other;
          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:2:1) <> '0';
            %Subst(strTmp:1:1) = '(';
            %Subst(strTmp:2:1) = %Subst(strNum:2:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum:3:1);
          %Subst(strTmp:4:1) = '.';
          %Subst(strTmp:5:1) = %Subst(strNum:4:1);
          %Subst(strTmp:6:1) = %Subst(strNum:5:1);
          %Subst(strTmp:7:4) = '%)**';
       EndSl;
       pro(intIncr) = strTmp;
       Clear strtmp;
      /end-free
     C                   ENDIF

      /free
       Select;
       When cltId = '025';
          xxPro = ELstfc * -.01;
       When cltId = '136';
          xxPro = ELdev2F * -.01;
       Other;
          xxPro = ELdev2 * -.01;
       EndSl;
      /End-free
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION 2ND PERIOD- APPLIED TO EXPERIENCE
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2D2        XXILEF
     C                   ENDIF
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C*
      /free
       If cltid <> '025';
          If RRdv2h = *blanks;
             strWk25 = @2nper + ' ' + con(9);
             strTmp= strWk25;
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strWk25 = @2nper + ' ' + %Trim(strWk25);
             strTmp= strWk25;
          Endif;

          %Subst(strTmp:26:1) = '%';
          %Subst(strTmp:23:1) = '.';
          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Else;     //Clt = 025
          If RRdv2h = *blanks;
             strTmp =  con(17);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          %Subst(strTmp:20:1) = '.';
          strAsp8  = %Editc(ELstfc:'X');
          strNum8 = StrAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:18:1) = %Subst(strNum8:1:1);
          Endif;

          %Subst(strTmp:19:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:21:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:22:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:23:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:24:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:25:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:26:1) = %Subst(strNum8:8:1);
       Endif;  //CLTID <> '025'
       Select;
       //>>>>>>>>> Stability Factor
       When cltId = '025';
          If RRdv2h = *blanks;
             strTmp =  con(17);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp8  = %Editc(ELstfc:'X');
          strNum8 = StrAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:18:1) = %Subst(strNum8:1:1);
          Endif;

          %Subst(strTmp:19:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:20:1) = '.';
          %Subst(strTmp:21:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:22:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:23:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:24:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:25:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:26:1) = %Subst(strNum8:8:1);
       //>>>>>>>>> Territory Rating
       When cltId = '136';
          If RRdv2h = *blanks;
             strWk25 = @2nper + ' ' + con(19);
             strTmp= strWk25;
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strWk25 = @2nper + ' ' + %Trim(strWk25);
             strTmp= strWk25;
          Endif;

          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          %Subst(strTmp:22:1) = '.';
          %Subst(strTmp:23:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       //>>>>>>>>> Default Deviation #2
       Other;
          If RRdv2h = *blanks;
             strWk25 = @2nper + ' ' + con(9);
             strTmp= strWk25;
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strWk25 = @2nper + ' ' + %Trim(strWk25);
             strTmp= strWk25;
          Endif;

          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:23:1) = '.';
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       EndSl;

       hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD2S2
      /free
       If Cltid <> '025';
          %Subst(strTmp:7:4) = '%)**' ;
          %Subst(strTmp:4:1) = '.' ;
          strAsp1 = %Editc(ELd2s2:'X');
          strNum = strAsp1;

          If %Subst(strNum:2:1) <> '0';
             %Subst(strTmp:1:1) = '(';
             %Subst(strTmp:2:1) = %Subst(strNum:2:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum:3:1);
          %Subst(strTmp:5:1) = %Subst(strNum:4:1);
          %Subst(strTmp:6:1) = %Subst(strNum:5:1);

       Else;     //cltid= '025'
          ELstfc = ELstfc - 1 ;
          ELstfc = ELstfc * xxpro;
          ELstfc = Elstfc + 1 ;
          %Subst(strTmp:4:1) = '.';
          strAsp8 = %Editc(ELstfc:'X');
          strnum8 = strAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:1:1) = '(';
             %Subst(strTmp:2:1) = %Subst(strNum8:1:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:5:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:6:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:7:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:8:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:9:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:10:1) = ')';
       Endif;
       pro(intIncr) = strTmp;
       clear strtmp;
      /end-free
     C                   ENDIF
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C                   ENDIF
     C*
     C     RRDCAP        IFEQ      'Y'
     C                   EXSR      CALDBR
     C                   ENDIF
     C*
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVFL        ANDEQ     'E'
     C     ELWVTP        ANDEQ     'B'
     C*

      /free
       strWk20 = %Xlate(Lo:Up:RRwvhd);
       strTmp = strWk20;
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELwvpc:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT EXP MOD PREMIUM LEVEL
      *
     C                   MOVE      'E'           XXPLV1            1
     C                   MOVE      'E'           XXPLV2            1
     C                   EXSR      APLGDD
     C*
     C*  CONTRACTORS CLASS
     C*
     C     ELASP1        IFNE      *ZERO
     C*
     C     ELCREF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELCREF        XXILEF
     C                   ENDIF
     C*
     C     ELCREX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELCREX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELCREX        ANDNE     ELCREF
     C     ELCREX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELCREX        ANDEQ     *ZERO
     C*

      /free
       If RRcchd <> *blanks and cltid = '132';
          con(5) = RRcchd;
          strTmp = Con(5);
       Else;
          strTmp = Con(5);
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELasp1:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP1
     C*
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELasp1:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C     ELASP1        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C*  DEDUCTIBLE DISCOUNT
     C*
     C                   Z-ADD     *ZERO         XXDEDF           11 9
     C*
     C     ELDDCT        IFNE      *ZERO
     C     RRRDPR        ANDEQ     'S'
      *** Beg Add *****  11/11/24  *******************************************
     C     LRGDFL        ANDNE     'Y'
      *** End Add *****  11/11/24  *******************************************
     C*
     C     ELDCEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDCEF        XXILEF
     C                   ENDIF
     C*
     C     ELDCEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDNE     ELDCEF
     C     ELDCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDEQ     *ZERO
     C*
      /free
       strtmp = con(1);
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDDCT
     C*
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /End-free
     C                   ENDIF
     C*
     C     ELDDCT        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXFACT
     C*
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C     ELDD2P        IFNE      *ZERO
     C*
     C     EL2DEF        IFEQ      *ZERO
     C                   MOVE      ELDCEX        XXILEF
     C                   ELSE
     C                   MOVE      EL2DEF        XXILEF
     C                   ENDIF
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      EL2DEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDNE     EL2DEF
     C     EL2DEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDEQ     *ZERO
     C*

      /free
       strTmp = Con(13);
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' or %Subst(strNum:1:1) >'0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDD2P
     C*
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C     ELDD2P        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXFACT
     C*
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXWFAC
     C                   ENDIF
      /free
       intIncr = intIncr + 1;
      /end-free
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION - APPLIED TO STANDARD
     C*
     C     ELASP2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'S'
     C*
     C     ELDVEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDVEF        XXILEF
     C                   ENDIF
     C*
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
      /free
       If RRdv1h =  *Blanks;
          strTmp = con(4);
       Else;
         strWk20 = %Xlate (lo:up:RRdv1h);
         strTmp = strWk20;
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELasp2:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C*
     C                   MULT(H)   XXPRO         ELASP2
     C*
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELasp2:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF
     C*  DEVIATION - APPLIED TO STANDARD
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'S'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD1D2        XXILEF
     C                   ENDIF
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD1X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C*
      /free
       strWk25 = *blanks;
       If RRdv1h = *blanks;
          strWk25 = @2nper + ' ' + con(4) ;
          strTmp = strWk25 ;
       Else;
          strWk25 = %Xlate (lo:up:RRdv1h);
          strWk25 = @2nper + ' ' + %Trim(strWk25);
          strTmp = strWk25;
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELd1s2:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C                   ENDIF
     C*  2ND DEVIATION - APPLIED TO STANDARD
     C*

      /free
       If RRdv2l = 'S' and ((ELdev2 <> 0 and cltId <> '025'
          And cltId <> '136') or (ELstfc <> 0 and cltId = '025') or
          (ELdev2F <> 0 and cltId = '136'));
      /End-free
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
      /free
       Select;
       //>>>>>>>>>>>>> Stability Factor
       When cltID = '025';
          If RRdv2h = *blanks;
             strTmp =  con(17);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          %Subst(strTmp:20:1) = '.';
          strAsp8 = %Editc(ELstfc:'X');
          strNum8 = StrAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:18:1) = %Subst(strNum8:1:1);
          Endif;

          %Subst(strTmp:19:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:21:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:22:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:23:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:24:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:25:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:26:1) = %Subst(strNum8:8:1);
       //>>>>>>>>>>>>> Territory Rating
       When cltID = '136';
          If RRdv2h = *blanks;
             strTmp = con(19);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2F:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          %Subst(strTmp:22:1) = '.';
          %Subst(strTmp:23:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       // >>>>>>>>>>>> Default Deviation #2
       Other;
          If RRdv2h = *blanks;
             strTmp = con(9);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:23:1) = '.';
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       EndSl;
       hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
      /free
       If cltId = '025' ;
          eval(h) ELstfc = ELstfc * xxPro;
          ELstfc = ELstfc - 1 ;
          Elstfc = ELstfc * xxpro;
          ELstfc = Elstfc + 1 ;
          %Subst(strTmp:4:1) = '.';
          strAsp8 = %Editc(Elstfc:'X');
          strNum8 = strAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:1:1) = '(';
             %Subst(strTmp:2:1) = %Subst(strNum8:1:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:5:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:6:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:7:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:8:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:9:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:10:1) = ')';
       Else;
      /End-free
     C                   MULT(H)   XXPRO         ELDEV2
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELdev2:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Select;
       //>>>>>>>>>> Stability Factor
       When cltId = '025';
          eval(h) ELstfc = ELstfc * xxPro;
          ELstfc = ELstfc - 1 ;
          Elstfc = ELstfc * xxpro;
          ELstfc = Elstfc + 1 ;
          strAsp8 = %Editc(Elstfc:'X');
          strNum8 = strAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:1:1) = '(';
             %Subst(strTmp:2:1) = %Subst(strNum8:1:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:4:1) = '.';
          %Subst(strTmp:5:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:6:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:7:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:8:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:9:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:10:1) = ')';
       //>>>>>>>>>> Territory Rating
       When cltId = '136';
          eval(h) ELdev2F = ELdev2F * xxPro;
          strAsp1 = %Editc(ELdev2F:'X');
          strNum = strAsp1;

          %Subst(strTmp:1:1) = '(';
          %Subst(strTmp:2:1) = %Subst(strNum:2:1);
          %Subst(strTmp:3:1) = '.' ;
          %Subst(strTmp:4:1) = %Subst(strNum:3:1);
          %Subst(strTmp:5:1) = %Subst(strNum:4:1);
          %Subst(strTmp:6:1) = %Subst(strNum:5:1);
          %Subst(strTmp:7:4) = '%)**' ;
       //>>>>>>>>>> Default Deviation #2
       Other;
          eval(h) ELdev2 = ELdev2 * xxPro;
          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:2:1) <> '0';
            %Subst(strTmp:1:1) = '(';
            %Subst(strTmp:2:1) = %Subst(strNum:2:1);
          Else;
            %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum:3:1);
          %Subst(strTmp:4:1) = '.' ;
          %Subst(strTmp:5:1) = %Subst(strNum:4:1);
          %Subst(strTmp:6:1) = %Subst(strNum:5:1);
          %Subst(strTmp:7:4) = '%)**' ;
       EndSl;

       pro(intIncr) = strTmp;
       clear strTmp;
       Endif;
      /End-free
     C*
     C                   ENDIF
     C*
      /free
       Select;
       When cltId = '025';
          xxPro = ELstfc * -.01;
       When cltId = '136';
          xxPro = ELdev2F * -.01;
       Other;
          xxPro = ELdev2 * -.01;
       EndSl;
      /End-free
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*  2ND DEVIATION 2ND PERIOD- APPLIED TO STANDARD
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2D2        XXILEF
     C                   ENDIF
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C*
      /free
       If cltid <> '025';
          If RRdv2h = *blanks;
             strWk25 = @2nper + ' ' + con(9);
             strTmp= strWk25;
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strWk25 = @2nper + ' ' + %Trim(strWk25);
             strTmp= strWk25;
          Endif;

          %Subst(strTmp:25:1) = '%';
          %Subst(strTmp:23:1) = '.';
          strAsp1  = %Editc(ELd2s2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Else;   //for client ='025'
          If RRdv2h = *blanks;
             strTmp = con(17);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          %Subst(strTmp:20:1) = '.';
          strAsp8 = %Editc(ELstfc:'X');
          strNum8 = strAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:18:1) = %Subst(strNum8:1:1);
          Endif;

          %Subst(strTmp:19:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:21:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:22:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:23:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:24:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:25:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:26:1) = %Subst(strNum8:8:1);
       Endif;     //CLTID <> '025'
       hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD2S2
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C                   ENDIF
     C     SCBLAM        MULT      XXFACT        XXAMT3           15 4
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVFL        ANDEQ     'S'
     C     ELWVTP        ANDEQ     'B'
     C*
      /free
       strWk20 = %Xlate(lo:up:RRWvhd);
       strTmp = strWk20;
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELwvpc:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free

     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT STANDARD PREMIUM LEVEL
      *
     C                   MOVE      'S'           XXPLV1            1
     C                   MOVE      'S'           XXPLV2            1
     C                   EXSR      APLGDD
     C*
     C*  DISCOUNT
     C*
     C                   Z-ADD     *ZERO         XXDIS3            5 2
     C*
     C     ELASP3        IFNE      *ZERO
     C     ELDISC        ORNE      *BLANK
     C     ELEDC3        ANDNE     *ZERO
     C*
      /free
       If RRdshd = *blanks;
          strTmp = con(6);
       Else;
          strTmp = RRdshd;
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       If Elasp3 <> *zeros;
          xxdis3  = Elasp3;
          strasp1  = %editc(ELasp3:'X');
          strNum = strasp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' or %Subst(strNum:1:1) >'0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Endif;
      /end-free

     C     ELASP3        IFNE      *ZERO
     C                   Z-ADD     ELASP3        XXDIS3
     C                   ENDIF
     C*
     C     ELDISC        IFNE      *BLANK
     C*
     C     RRDSPR        IFEQ      'M'
     C*
     C     RRDV1L        IFEQ      'S'
     C     ELSTPR        SUB       ELEDC2        XXRND
     C                   ELSE
     C                   Z-ADD     ELSTPR        XXRND
     C                   ENDIF                                                  RRDV1L IFEQ
     C     RRDV2L        IFEQ      'S'
     C     XXRND         SUB       ELEDV2        XXRND
     C                   ENDIF                                                  RRDV2L IFEQ
     C     ELWVFL        IFEQ      'S'
     C     XXRND         ADD       ELEWAV        XXRND
     C                   ENDIF
     C*
     C     XXRND         SUB       ELEDC3        XXRND2           11 0
     C     XXRND         IFNE      *ZERO
     C     XXRND2        DIV(H)    XXRND         XXPCNT
     C                   ELSE
     C                   Z-ADD     1             XXPCNT
     C                   ENDIF
     C*
     C     1             SUB       XXPCNT        XXPCNT
     C     XXPCNT        MULT      100           XXDIS3
     C*
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     ELSTPR        XXRND
     C                   ELSE
     C     RRDV1L        IFEQ      'S'
     C     ELSTPR        SUB       ELEDC2        XXRND
     C                   ELSE
     C                   Z-ADD     ELSTPR        XXRND
     C                   ENDIF                                                  RRDV1L IFEQ
     C     RRDV2L        IFEQ      'S'
     C     XXRND         SUB       ELEDV2        XXRND
     C                   ENDIF                                                  RRDV2L IFEQ
     C     ELWVFL        IFEQ      'S'
     C     XXRND         ADD       ELEWAV        XXRND
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRND         IFNE      *ZERO
     C     ELEDC3        DIV(H)    XXRND         XXPCNT
     C                   ELSE
     C                   Z-ADD     *ZERO         XXPCNT            5 4
     C                   ENDIF
     C*
     C     XXPCNT        MULT      100           XXDIS3
     C*
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   ENDIF
     C*

      /free
       strasp1 = %Editc(ELdisp:'X');
       Pro(intIncr)= 'approx.';
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' or %Subst(strNum:1:1) >'0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       clear strTmp;
       intIncr =  intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C     *IN93         IFEQ      *OFF
     C     XXDIS3        IFNE      *ZERO
     C     XXDIS3        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF                                                  *IN93 = *OFF
     C*
     C*  DEVIATION - APPLIED TO DISCOUNTED
     C*
     C     ELASP2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'D'
     C*
     C     ELDVEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDVEF        XXILEF
     C                   ENDIF
     C*
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
      /free
       If RRdv1h = *blanks;
          strTmp = con(4);
       Else;
         strWk20 = %Xlate (lo:up:RRdv1h);
         strTmp = strWk20;
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELasp2:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /End-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C*
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELasp2:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF
     C*  DEVIATION 1 PERIOD 2- APPLIED TO DISCOUNTED
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'D'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD1D2        XXILEF
     C                   ENDIF
     C*
     C     ELD1X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD1X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C*

      /free
       strWk25 = *blanks;
       If RRdv1h = *blanks;
          strWk25 = @2nper + ' ' + con(4);
          strTmp = strWk25;
       Else;
          strWk25 = %Xlate(lo:up:RRdv1h);
          strWk25 = @2nper + ' ' + %Trim(strWk25);
          strTmp = strWk25;
       Endif;

       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELasp2:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELddct:'X');
       strnum = strAsp1;
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C                   ENDIF
     C*  2ND DEVIATION - APPLIED TO DISCOUNTED
     C*
      /free
       If RRdv2l = 'D' and ((ELdev2 <> 0 and cltId <> '025'
          And cltId <> '136') or (ELstfc <> 0 and cltId = '025') or
          (ELdev2F <> 0 and cltId = '136'));
      /End-free
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
      /free
       Select;
       //>>>>>>>>>>>>> Stability Factor
       When cltID = '025';
          If RRdv2h = *blanks;
             strTmp =  con(17);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          %Subst(strTmp:20:1) = '.';
          strAsp8 = %Editc(ELstfc:'X');
          strNum8 = StrAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:18:1) = %Subst(strNum8:1:1);
          Endif;

          %Subst(strTmp:19:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:21:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:22:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:23:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:24:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:25:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:26:1) = %Subst(strNum8:8:1);
       //>>>>>>>>>>>>> Territory Rating
       When cltID = '136';
          If RRdv2h = *blanks;
             strTmp = con(19);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2F:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          %Subst(strTmp:22:1) = '.';
          %Subst(strTmp:23:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       // >>>>>>>>>>>> Default Deviation #2
       Other;
          If RRdv2h = *blanks;
             strTmp = con(9);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:23:1) = '.';
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       EndSl;
       hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free

     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C*
      /free
       Select;
       When cltId = '025';
          eval(h) ELstfc  = ELstfc * xxPro;
       When cltId = '136';
          eval(h) ELdev2F = ELdev2F * xxPro;
       Other;
          eval(h) ELdev2  = ELdev2 * xxPro;
       EndSl;
       Select;
       When cltId = '025';
          ELstfc = ELstfc - 1;
          Elstfc = ELstfc * xxpro;
          ELstfc = Elstfc + 1 ;
          strAsp8 = %Editc(Elstfc:'X');
          strNum8 = strAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:1:1) = '(';
             %Subst(strTmp:2:1) = %Subst(strNum8:1:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:4:1) = '.';
          %Subst(strTmp:5:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:6:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:7:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:8:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:9:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:10:1) = ')';
       When cltId = '136';
          strAsp1 = %Editc(ELdev2F:'X');
          strNum = strAsp1;

          %Subst(strTmp:1:1) = '(';
          %Subst(strTmp:2:1) = %Subst(strNum:2:1);
          %Subst(strTmp:3:1) = '.';
          %Subst(strTmp:4:1) = %Subst(strNum:3:1);
          %Subst(strTmp:5:1) = %Subst(strNum:4:1);
          %Subst(strTmp:6:1) = %Subst(strNum:5:1);
          %Subst(strTmp:7:4) = ')';
       Other;
          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:2:1) <> '0';
            %Subst(strTmp:1:1) = '(';
            %Subst(strTmp:2:1) = %Subst(strNum:2:1);
          Else;
             %Subst(strTmp:2:1) = '(';
          Endif;

          %Subst(strTmp:3:1) = %Subst(strNum:3:1);
          %Subst(strTmp:4:1) = '.';
          %Subst(strTmp:5:1) = %Subst(strNum:4:1);
          %Subst(strTmp:6:1) = %Subst(strNum:5:1);
          %Subst(strTmp:7:4) = '%)**';
       EndSl;
       pro(intIncr) = strTmp;
       clear strtmp;
      /End-free
     C                   ENDIF
     C*
      /free
       Select;
       When cltId = '025';
          xxPro = ELstfc * -.01;
       When cltId = '136';
          xxPro = ELdev2F * -.01;
       Other;
          xxPro = ELdev2 * -.01;
       EndSl;
      /End-free
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION 2ND PERIOD- APPLIED TO DISCOUNTED
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2D2        XXILEF
     C                   ENDIF
     C*
     C     ELD2X2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2X2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
      /free
       Select;
       When cltId = '025';
          If RRdv2h = *blanks;
             strTmp =  con(17);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          %Subst(strTmp:20:1) = '.';
          strAsp8 = %Editc(ELstfc:'X');
          strNum8 = StrAsp8;

          If %Subst(strNum8:1:1) <> '0';
             %Subst(strTmp:18:1) = %Subst(strNum8:1:1);
          Endif;

          %Subst(strTmp:19:1) = %Subst(strNum8:2:1);
          %Subst(strTmp:21:1) = %Subst(strNum8:3:1);
          %Subst(strTmp:22:1) = %Subst(strNum8:4:1);
          %Subst(strTmp:23:1) = %Subst(strNum8:5:1);
          %Subst(strTmp:24:1) = %Subst(strNum8:6:1);
          %Subst(strTmp:25:1) = %Subst(strNum8:7:1);
          %Subst(strTmp:26:1) = %Subst(strNum8:8:1);
       When cltId = '136';
          If RRdv2h = *blanks;
             strTmp = con(19);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2F:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          %Subst(strTmp:22:1) = '.';
          %Subst(strTmp:23:1) = %Subst(strNum:3:1);
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Other;
          If RRdv2h = *blanks;
             strTmp = con(9);
          Else;
             strWk20 = %Xlate(lo:up:RRdv2h);
             strTmp = strWk20;
          Endif;

          strAsp1 = %Editc(ELdev2:'X');
          strNum = strAsp1;

          If %Subst(strNum:1:1) <> '0';
             %Subst(strTmp:20:1) = %Subst(strNum:1:1);
          Endif;

          If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
             %Subst(strTmp:21:1) = %Subst(strNum:2:1);
          Endif;

          %Subst(strTmp:22:1) = %Subst(strNum:3:1);
          %Subst(strTmp:23:1) = '.';
          %Subst(strTmp:24:1) = %Subst(strNum:4:1);
          %Subst(strTmp:25:1) = %Subst(strNum:5:1);
          %Subst(strTmp:26:1) = '%';
       EndSl;

       hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD2S2
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C                   ENDIF
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVFL        ANDEQ     'D'
     C     ELWVTP        ANDEQ     'B'
     C*
      /free
       //****** Beg Add ***** 04/22/14 ***********************************
       strWk20 = %Xlate(Lo:Up:RRwvhd);
       strTmp = strWk20;
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELwvpc:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
       //***** End Add ***** 04/22/14 ***********************************
      /end-free
     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT NORMAL PREMIUM LEVEL
      *
     C                   MOVE      'D'           XXPLV1            1
     C                   MOVE      'D'           XXPLV2            1
     C                   EXSR      APLGDD
     C*
      *** Beg Add *****  11/11/24  *******************************************
     C*
     C*  DEDUCTIBLE DISCOUNT - APPLIED TO DISCOUNTED PREMIUM
     C*
     C                   Z-ADD     *ZERO         XXDEDF           11 9
     C*
     C     ELDDCT        IFNE      *ZERO
     C     RRRDPR        ANDEQ     'D'
     C     LRGDFL        ANDNE     'Y'
     C*
     C     ELDCEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELDCEF        XXILEF
     C                   ENDIF
     C*
     C     ELDCEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDNE     ELDCEF
     C     ELDCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDEQ     *ZERO
     C*
      /free
       strtmp = con(1);
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' Or %Subst(strNum:1:1) > '0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDDCT
     C*
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /End-free
     C                   ENDIF
     C*
     C     ELDDCT        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXFACT
     C*
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXWFAC
     C                   ENDIF
     C*
      /free
       intIncr = intIncr + 1;
      /end-free
     C                   ENDIF
     C     ELDD2P        IFNE      *ZERO
     C*
     C     EL2DEF        IFEQ      *ZERO
     C                   MOVE      ELDCEX        XXILEF
     C                   ELSE
     C                   MOVE      EL2DEF        XXILEF
     C                   ENDIF
     C*
     C     EL2DEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      EL2DEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDNE     EL2DEF
     C     EL2DEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDEQ     *ZERO
     C*

      /free
       strTmp = Con(13);
       %Subst(strTmp:26:1) = '%';
       %Subst(strTmp:23:1) = '.';
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:1:1) <> '0';
          %Subst(strTmp:20:1) = %Subst(strNum:1:1);
       Endif;

       If %Subst(strNum:2:1) <> '0' or %Subst(strNum:1:1) >'0' ;
          %Subst(strTmp:21:1) = %Subst(strNum:2:1);
       Endif;

       %Subst(strTmp:22:1) = %Subst(strNum:3:1);
       %Subst(strTmp:24:1) = %Subst(strNum:4:1);
       %Subst(strTmp:25:1) = %Subst(strNum:5:1);
       Hdg(intIncr) = strTmp;
       clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDD2P
     C*
      /free
       %Subst(strTmp:7:4) = '%)**' ;
       %Subst(strTmp:4:1) = '.' ;
       strAsp1 = %Editc(ELddct:'X');
       strNum = strAsp1;

       If %Subst(strNum:2:1) <> '0';
         %Subst(strTmp:1:1) = '(';
         %Subst(strTmp:2:1) = %Subst(strNum:2:1);
       Else;
         %Subst(strTmp:2:1) = '(';
       Endif;

       %Subst(strTmp:3:1) = %Subst(strNum:3:1);
       %Subst(strTmp:5:1) = %Subst(strNum:4:1);
       %Subst(strTmp:6:1) = %Subst(strNum:5:1);
       Pro(intIncr) = strTmp;
       Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C     ELDD2P        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXFACT
     C*
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         XXDEDF
     C                   SUB       XXDEDF        XXWFAC
     C                   ENDIF
      /free
       intIncr = intIncr + 1;
      /end-free
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      *** End Add *****  11/11/24  *******************************************
     C*
     C* AIRCRAFT REINSURANCE (ESTIMATED)
     C*
      *
     C     ELARCP        IFNE      *ZEROS
     C     ELARCP        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      6
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELARCP IFNE
     C*
     C                   ADD       XXARAP        XXFACT
     C*
     C                   Z-ADD(H)  XXFACT        X5FACT
     C     SCBLAM        MULT(H)   X5FACT        XXRND
     C                   Z-ADD     X5FACT        Q1DSCA
     C                   Z-ADD     XXRND         Q1TOT4
     C*** Begin Add *** 06/12/24 *******************************************
       // Save off premium
       decTotalPremium = xxRnd;
     C*** Begin Add *** 06/12/24 *******************************************
      /Free
       Eval(h) xxExactAmt = xxExactAmt * x5Fact;
       Q2tot4 = xxExactAmt;
      /End-Free
     C*
     C* IF A WAIVER SURCHARGE IS ON THE POLICY AND IT IS NOT A
     C* PERCENTAGE, THEN WE NEED TO ADD IN 1/12 OF THE CHARGE TO
     C* THE PREVIOUS BALANCE TO REFLECT IT AND PRINT A MESSAGE.
     C*
     C                   MOVE      *OFF          *IN71
     C*** begin add ***  05/31/23  *************************************
     C                   MOVE      *BLANKS       XXPBAL          100
     C*** end add *****  05/31/23  *************************************
     C     ELEWAV        IFNE      *ZERO
     C     ELWVPC        ANDEQ     *ZERO
     C     ELEWAV        ORNE      *ZERO
     C     ELWVTP        ANDEQ     'S'
     C*
     C     *IN46         IFEQ      *OFF
     C     ELEWAV        DIV(H)    12            XXPREM           11 0
     C                   ELSE
     C     XXDAYS        DIV       365           XXPRO
     C     ELEWAV        MULT(H)   XXPRO         XXPREM
     C                   ENDIF                                                  *IN46
     C*
     C     XXPREM        MULT(H)   XXWFAC        XXPREM
     C                   ADD       XXPREM        XXRND
     C                   MOVE      *ON           *IN71
     C                   Z-ADD     XXPREM        Q1WAIV
     C                   ADD       XXPREM        PBAL
     C*** begin add ***  05/31/23  *************************************
     C     LO:UP         XLATE(P)  RRWVHD        XXWVHD           20
     C     XXPBAL        IFEQ      *BLANKS
     C                   MOVEL     MSG(1)        XXPBAL
     C     XXPBAL        CAT       XXWVHD:1      XXPBAL
     C                   ELSE
     C     XXPBAL        CAT       ',':0         XXPBAL
     C     XXPBAL        CAT       XXWVHD:1      XXPBAL
     C                   ENDIF                                                  XXPBAL IFEQ
     C*** end add *****  05/31/23  *************************************
     C*
     C                   ENDIF                                                  ELEWAV IFNE
     C*
     C* IF A REINSURANCE CHARGE IS ON THE POLICY AND IT IS NOT A
     C* PERCENTAGE, THEN WE NEED TO ADD IN 1/12 OF THE CHARGE TO
     C* THE PREVIOUS BALANCE TO REFLECT IT AND PRINT A MESSAGE.
     C*
     C                   MOVE      *OFF          *IN73
     C     ELEARC        IFNE      *ZERO
     C     ELARCP        ANDEQ     *ZERO
     C*
     C     *IN46         IFEQ      *OFF
     C     ELEARC        DIV(H)    12            XXPREM           11 0
     C                   ELSE
     C     XXDAYS        DIV       365           XXPRO
     C     ELEARC        MULT(H)   XXPRO         XXPREM
     C                   ENDIF                                                  *IN46
     C*
     C                   Z-ADD     XXPREM        SCARAM           11 2
     C                   ADD       XXPREM        XXRND
     C                   MOVE      *ON           *IN73
     C                   Z-ADD     XXPREM        Q1REIN
     C*** begin add ***  05/31/23  *************************************
     C     XXPBAL        IFEQ      *BLANKS
     C                   MOVEL     MSG(1)        XXPBAL
     C     XXPBAL        CAT       CON(15):1     XXPBAL
     C                   ELSE
     C     XXPBAL        CAT       ',':0         XXPBAL
     C     XXPBAL        CAT       CON(15):1     XXPBAL
     C                   ENDIF                                                  XXPBAL IFEQ
     C*** end add *****  05/31/23  *************************************
     C*
     C                   ENDIF                                                  ELEARC IFNE
     C*
     C                   ELSE
     C     SCMAPR        MULT(H)   XXPROR        XXWK10           11 0
     C                   Z-ADD     XXWK10        SCNETP           11 2
     C                   ADD       SCADLD        SCNETP
     C                   Z-ADD     SCNETP        SCBLAM
     C                   Z-ADD     SCNETP        XXRND
     C                   ADD       XXEWAV        XXRND
     C                   Z-ADD     XXPROR        Q1NETF
     C                   Z-ADD     SCNETP        Q1NETP
     C                   Z-ADD     XXEWAV        Q1NWAV
     C*** Begin Add *** 06/12/24 *******************************************
       // Save off premium
       decTotalPremium = xxWk10;
     C*** Begin Add *** 06/12/24 *******************************************
     C*
     C                   ENDIF                                                  *IN63 IFEQ
     C*
     C* CALCULATE TERRORISM RISK CHARGE AND ADD TO TOTAL PREMIUM AMT.
     C*
     C     XXEFDT        IFNE      *ZERO
     C     XXEFDT        ANDNE     X2EFDT
     C                   MOVE      XXEFDT        X2EFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        X2EFDT
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN72
     C                   Z-ADD     0             XXPREM
     C*
     C     ELTERR        IFNE      *ZERO
     C     ELDTRR        ORNE      *ZERO
     C     XXPAYR        DIV       100           XXPREM
     C                   Z-ADD(H)  ELTERR        XXTERR
     C                   ADD(H)    ELDTRR        XXTERR
     C                   MULT(H)   XXTERR        XXPREM
     C                   ELSE
     C     RRTERR        IFNE      *ZERO
     C     RRDTRR        ORNE      *ZERO
     C     XXPAYR        DIV       100           XXPREM
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   ELRTCD        QQRTCD
     C                   MOVE(P)   RRTERC        QQCLAS
     C                   MOVE(P)   X2EFDT        QQEFDT
     C                   CALL      'SRLCM'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRTCD            3
     C                   PARM                    QQCLAS            5
     C                   PARM                    QQEFDT            7
     C                   PARM      *BLANKS       QQLCM             5
      *
     C     QQLCM         IFNE      *BLANKS
     C                   MOVE      QQLCM         XXLCM             5 4
     C                   ENDIF
      *
     C     RRTERL        IFNE      'Y'
     C     RRTERR        MULT(H)   XXLCM         RRTERR
     C     RRDTRR        MULT(H)   XXLCM         RRDTRR
     C                   ENDIF                                                  RRTERL IFNE
     C                   Z-ADD(H)  RRTERR        XXTERR
     C                   ADD(H)    RRDTRR        XXTERR
     C                   MULT(H)   XXTERR        XXPREM
      /Free
        //Apply Excess premium reduction % (Deviation #1) against Terrirism chrages
        If FNExIns = 'Y' and ELASP2 > 0;
           Eval(h) xxPrem = xxPrem - (xxPrem * ELAsp2/100);
        Endif;
      /End-Free
     C                   ENDIF                                                  RRTERR & RRDTRR
     C*** BEG ADD ***  07/15/13  ********************************************
     C                   ENDIF                                                  ELTERR & ELDTRR
     C*** END ADD ***  07/15/13  ********************************************
     C*
     C     XXPREM        IFNE      *ZERO
     C*
     C                   Z-ADD     XXPREM        SCTERR           11 2
     C                   ADD       XXPREM        XXRND
     C                   MOVE      *ON           *IN72
     C*
     C                   ENDIF                                                  XXPREM IFNE
     C*
     C                   MOVE      ELPFDT        X2EFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   Z-ADD     XXRND         SCBLAM
     C                   Z-ADD     SCTERR        Q1TERR

     C                   Endsl
      /Free
       Eval(h) xxExactAmt += xxPrem;
      /End-Free
     C*
     C*  PREMIUM DUE
     C*
     C     CLTID         IFEQ      '046'
     C*
     C     *IN93         IFEQ      *OFF
     C     XXDIS3        ANDNE     *ZERO
     C     F1DSCD        ANDNE     *BLANKS
     C     XXDIS3        MULT      XXAMT3        XXAMT3
     C     XXAMT3        DIV(H)    100           XXAMT3
     C                   Z-ADD(H)  XXAMT3        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXAMT3
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXAMT3
     C                   ENDIF                                                  *IN93 IFEQ
     C*
     C                   ENDIF                                                  CLTID = '046'
      *
     C     RRTXPC        IFNE      *ZERO
     C     RRTXPC        MULT(H)   SCBLAM        XXAMT            15 4
     C     XXAMT         MULT(H)   .01           XXTAX            11 0
     C                   Z-ADD     XXTAX         SCTXBA           11 2
     C                   ELSE
     C                   Z-ADD     0             SCTXBA
     C                   ENDIF                                                  RRTXPC IFNE
     C*
     C                   Z-ADD     SCBLAM        @@PREM
     C                   Z-ADD     SCTXBA        @@TAX

     C     Q1PRMS        IFGT      8
     C*
     C* GATHER PRIOR BILLED PREMIUM/TAX AMOUNTS FOR REPORTING DATE.
     C*
     C                   Z-ADD     *ZERO         X3AMT            15 5
     C                   MOVE      'B'           XXARTY
     C*
     C                   MOVE      *OFF          *IN61
     C     ELARSM        IFEQ      'Y'
     C                   MOVE      *ON           *IN61
     C                   ENDIF
     C*
     C  N61EAKY          CHAIN     PEA01                              60
     C   61EAKY1         CHAIN     PEA02                              60
      *
     C     *IN60         DOWEQ     *OFF
     C                   MOVEL(P)  EAARCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDNE     'C'
     C     TTCTYP        ANDNE     'E'
     C     EAARTY        ANDEQ     XXARTY
     C     EAPODT        ANDEQ     XXRPDT
     C     EAARCD        ANDEQ     F1SRBB
     C                   ADD       EAAMT         X3AMT
     C                   ENDIF                                                  *IN60 IFEQ
      *
       //*** Begin Add *** 06/24/24 ****************************************
       // Accumulate Prior Collateral Billed
       If *in60 = *Off and EAarcd = Excolb and EApodt = xxRpDt
        and %parms() > 23;
          @@PrColB += EAamt;
       EndIf;
       // Accumulate Previous Collateral Balance               // for CiCenter return
       If *in60 = *Off and (EAarcd = Excolb or EAarcd = Excolo)
        and %parms() > 25;
          @@PrvColBal += EAamt;
       EndIf;
       If *in60 = *Off and (EAarcd = Excolp or EAarcd = Excolt)
        and %parms() > 25;
          @@PrvColBal -= EAamt;
       EndIf;
       //*** End   Add *** 06/24/24 ****************************************
      *
     C  N61EAKY          READE     PEA01                                  60
     C   61EAKY1         READE     PEA02                                  60
     C                   ENDDO                                                  *IN60
     C*
     C* ADD IN UNTRANSFERRED BATCH RECORDS TO PRIOR BILLED AS WELL.
     C*
     C  N61EAKEY3        CHAIN     PEA03B                             60
     C   61EAKEY4        CHAIN     PEA03B                             60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EATFFL        IFNE      'Y'
     C*
     C     *IN61         IFEQ      *OFF
     C     *IN61         OREQ      *ON
     C     EARDIV        ANDEQ     XXDIV
     C*
     C                   MOVEL(P)  EAARCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDNE     'C'
     C     TTCTYP        ANDNE     'E'
     C     TTTYPE        ANDEQ     XXARTY
     C     EAPODT        ANDEQ     XXRPDT
     C     EAARCD        ANDEQ     F1SRBB
     C                   ADD       EAAMT         X3AMT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
       //*** Begin Add *** 06/24/24 ****************************************
       // Accumulate Prior Collateral Billed
       If *in60 = *Off and EAarcd = Excolb and EApodt = xxRpDt
        and %parms() > 23;
          @@PrColB += EAamt;
       EndIf;
       // Accumulate Previous Collateral Balance               // for CiCenter return
       If *in60 = *Off and (EAarcd = Excolb or EAarcd = Excolo)
        and %parms() > 25;
          @@PrvColBal += EAamt;
       EndIf;
       If *in60 = *Off and (EAarcd = Excolp or EAarcd = Excolt)
        and %parms() > 25;
          @@PrvColBal -= EAamt;
       EndIf;
       //*** End   Add *** 06/24/24 ****************************************
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C  N61EAKEY3        READE     PEA03B                                 60
     C   61EAKEY4        READE     PEA03B                                 60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* COLLECT UP TAX BILLED AMOUNTS.
     C*
     C                   Z-ADD     *ZERO         X3TAX            15 5
     C                   MOVE      'F'           XXARTY
     C*
     C  N61EAKY          CHAIN     PEA01                              60
     C   61EAKY1         CHAIN     PEA02                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     *IN60         IFEQ      *OFF
     C*** Begin Chg *** 06/12/24 *******************************************
     C*****TTCTYP        ANDEQ     'T'
     C     EACTYP        ANDEQ     'T'
     C*** End   Chg *** 06/12/24 *******************************************
     C     EAARTY        ANDEQ     XXARTY
     C     EAPODT        ANDEQ     XXRPDT
     C                   ADD       EAAMT         X3TAX
     C                   ENDIF                                                  *IN60 IFEQ
       //*** Begin Add *** 06/12/24 ****************************************
       // Accumulate Previous Billed Amounts for each Assessment
       If (*in60 = *Off) And
          (EActyp = '1' Or EActyp = '2' Or EActyp = '3' Or
           EActyp = '4' Or EActyp = '5' Or EActyp = '6' Or
           EActyp = '7') And
          (EAarTy = xxArTy) And
          (EApodt = xxRpDt);
          // Load Values for each assessments
          Select;
          When EAcTyp = '1';
             arrPreviousBilled(1) += EAamt;
          When EAcTyp = '2';
             arrPreviousBilled(2) += EAamt;
          When EAcTyp = '3';
             arrPreviousBilled(3) += EAamt;
          When EAcTyp = '4';
             arrPreviousBilled(4) += EAamt;
          When EAcTyp = '5';
             arrPreviousBilled(5) += EAamt;
          When EAcTyp = '6';
             arrPreviousBilled(6) += EAamt;
          When EAcTyp = '7';
             arrPreviousBilled(7) += EAamt;
          EndSl;
       EndIf;
       //*** End   Add *** 06/12/24 ****************************************
      *
     C  N61EAKY          READE     PEA01                                  60
     C   61EAKY1         READE     PEA02                                  60
     C                   ENDDO                                                  *IN60
     C*
     C* ADD IN UNTRANSFERRED BATCH RECORDS TO PRIOR BILLED AS WELL.
     C*
     C  N61EAKEY3        CHAIN     PEA03B                             60
     C   61EAKEY4        CHAIN     PEA03B                             60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     EATFFL        IFNE      'Y'
     C*
     C     *IN61         IFEQ      *OFF
     C     *IN61         OREQ      *ON
     C     EARDIV        ANDEQ     XXDIV
     C*
     C                   MOVEL(P)  EAARCD        X1KEY
     C     TBKY          CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'T'
     C     TTTYPE        ANDEQ     XXARTY
     C     EAPODT        ANDEQ     XXRPDT
     C                   ADD       EAAMT         X3TAX
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C  N61EAKEY3        READE     PEA03B                                 60
     C   61EAKEY4        READE     PEA03B                                 60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   Z-ADD     0             PBAL             11 2
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT             7 0
     C*
     C     FNKEY         CHAIN     PEX01                              60
      /free
       Select;
       //*** beg chg ***** 11/16/20 **********
       // Disperse credit for current year from state reporting
       //  division to non-summarized division. If credit doesn't
       //  exist, return prv balance of zero to Online Reporting.
       //*** end chg ***** 11/16/20 **********
       When xxdiv <> xsdiv and ELarsm = 'Y';
        //*** beg chg ***** 11/16/20 **********
        //*** pBal = 0;
          isNonSumDiv = *On;
          pBal = GetCurBal(xxco#:xxfnd:xxemp#:xsdiv:xxfyr);
          Select;
          When pBal >= 0;
            // No Credit Exists
             pBal = 0;
          When (@@Prem + @@Tax - x3Amt - x3tax) <= 0;
            // Nothing to bill
             pBal = 0;
          When (@@Prem + @@Tax - x3Amt - x3tax + pBal) < 0;
            // Billing takes part of the credit
             pBal = (@@Prem + @@Tax - x3Amt - x3tax) * -1;
          Other;
            //Billing takes all the credit
          EndSl;
          If pBal > 0;
             pBal = 0;
          Endif;
        //*** end chg ***** 11/16/20 **********
       Other;
         pBal = GetCurBal(xxco#:xxfnd:xxemp#:xxdiv:xxfyr);
         pBal = pBal + GetDepBal(xxco#:xxfnd:xxemp#:xxdiv:xxfyr);
         pBal = pBal + GetPYrBal(xxco#:xxfnd:xxemp#:xxdiv:xxfyr);
       Endsl;
      /end-free
       //*** Begin Add *** 06/12/24 ***************************************
       // Check and see if SR Assessments being done
       Chain (xxCo#:xxFnd:xxFyr) pf101;
       If (F1srAs = 'Y' Or ELsrAs = 'Y') And %Parms() > 21;
          dsAssessment.chrPrintAss = 'Y';
       EndIf;

       // Process Assesments
       outIdx = 0;
       If dsAssessment.chrPrintAss = 'Y' And %Parms() > 21;

          // Populate arrays to return
          For idx = 1 to 7;
             If aLvl(idx) <> ' ';
                outIdx += 1;
                dsAssessment.chrAssessment#(outIdx) = %Editc(idx:'X');
                // Assign headers
                If aHdg(idx) = ' ';
                   Select;
                   When idx = 1;
                      dsAssessment.decAssessmentHdg(outIdx) =
                         '1st Assessment';
                   When idx = 2;
                      dsAssessment.decAssessmentHdg(outIdx) =
                         '2nd Assessment';
                   When idx = 3;
                      dsAssessment.decAssessmentHdg(outIdx) =
                         '3rd Assessment';
                   When idx = 4;
                      dsAssessment.decAssessmentHdg(outIdx) =
                         '4th Assessment';
                   When idx = 5;
                      dsAssessment.decAssessmentHdg(outIdx) =
                         '5th Assessment';
                   When idx = 6;
                      dsAssessment.decAssessmentHdg(outIdx) =
                         '6th Assessment';
                   When idx = 7;
                      dsAssessment.decAssessmentHdg(outIdx) =
                         '7th Assessment';
                   EndSL;
                Else;
                   dsAssessment.decAssessmentHdg(outIdx) = aHdg(idx);
                EndIf;

                // Calculate the Payroll factors
                If decTotalAllClassAmt <> 0;
                   decFClassFactor     =
                      decTotalFClassAmt/decTotalAllClassAmt;
                   decOtherClassFactor =
                      decTotalOtherClassAmt/decTotalAllClassAmt;
                Else;
                   decFClassFactor     = 0;
             //*** begin chg ***  03/19/25  ******************************
             //****decOtherClassFactor = 0;
                   decOtherClassFactor = 1;
             //*** end chg *****  03/19/25  ******************************
                EndIf;

                // Calculate the Assessment Factors
                If acad(idx) = 'Y' And ELedCt = 0;
                   dsAssessment.decAssessmentPct(outIdx) = 0;
                Else;
                   // Calculate the
                   decAssessmentPremium = prmr(idx) + prmf(idx);

                //*** Begin Chg ***  05/15/25  ******************************
                // Select;
                // When aLvl(idx) = 'J';
                //    If acad(idx) = 'Y';
                //       ELeSbj += (ELeTer + ELeDTr);
                //    EndIf;
                //    Monitor;
                //       decAssessmentFactor = decAssessmentPremium/ELeSbj;
                //    On-Error;
                //       decAssessmentFactor = 0;
                //    EndMon;
                // When aLvl(idx) = 'E';
                //    If acad(idx) = 'Y';
                //       ELeMpm += (ELeTer + ELeDTr);
                //    EndIf;
                //    Monitor;
                //       decAssessmentFactor = decAssessmentPremium/ELeMpm;
                //    On-Error;
                //       decAssessmentFactor = 0;
                //    EndMon;
                // When aLvl(idx) = 'S';
                //    If acad(idx) = 'Y';
                //       ELstPr += (ELeTer + ELeDTr);
                //    EndIf;
                //    Monitor;
                //       decAssessmentFactor = decAssessmentPremium/ELstPr;
                //    On-Error;
                //       decAssessmentFactor = 0;
                //    EndMon;
                // When aLvl(idx) = 'D';
                //    If acad(idx) = 'Y';
                //       ELeNmP += (ELeTer + ELeDTr);
                //    EndIf;
                //    Monitor;
                //       decAssessmentFactor = decAssessmentPremium/ELeNmP;
                //    On-Error;
                //       decAssessmentFactor = 0;
                //    EndMon;
                // When aLvl(idx) = 'X';
                //    If acad(idx) <> 'Y';
                //       ELanPr -= (ELeTer + ELeDTr);
                //    EndIf;
                //    Monitor;
                //       decAssessmentFactor = decAssessmentPremium/ELanPr;
                //    On-Error;
                //       decAssessmentFactor = 0;
                //    EndMon;
                // EndSl;

                   If ELeNmP <> 0;           // Regular Premium (Estimated Normal Premium)
                      Monitor;
             //*** begin chg *** 06/23/25  *************************************
             //******    decAssessmentFactor = decAssessmentPremium/ELeNmP;
                         decAssessmentFactor = decAssessmentPremium /
                                                (ELeNmP * ELPror);
             //*** end chg ***** 06/23/25  *************************************
                      On-Error;
                         decAssessmentFactor = 1;
                      EndMon;
                   Endif;
                   If ELeNmP = 0 or decAssessmentFactor = 0;
                      decAssessmentFactor = 1;
                   Endif;
                //*** End   Chg ***  05/15/25  ******************************

                   // Calculate Assessment Percents
                   decFClassPercent     = decFClassFactor * pctf(idx);
                   decOtherClassPercent = decOtherClassFactor * pctp(idx);
             //*** begin add ***  03/19/25  ******************************
                   If pctf(idx) = 0 and xfc(idx) = ' ';
                      decOtherClassPercent = pctp(idx);
                   Endif;
             //*** end add *****  03/19/25  ******************************
                   decAssessmentPercent = decAssessmentFactor *
                      (decFClassPercent + decOtherClassPercent);

                   // Include Increate Limit pecentage
                   If ilb(idx) <> ' ' And ELanPr <> 0;

                      Select;
                      When ilb(idx) = 'I';
                         Monitor;
                            decIncLimitFactor = (ELein1 + ELein2)/ELanPr;
                         On-Error;
                            decIncLimitFactor = 0;
                         EndMon;
                      When ilb(idx) = 'W';
                         Monitor;
                            decIncLimitFactor = 1 - ((ELein1 + ELein2)/ELanPr);
                         On-Error;
                            decIncLimitFactor = 1;
                         EndMon;
                      EndSl;

                      decAssessmentPercent =
                         decAssessmentPercent * decIncLimitFactor;
                   EndIf;

                   // Load Assessement Percent into Scriptura workfile
                   dsAssessment.decAssessmentPct(outIdx) =
                      decAssessmentPercent;

                EndIf;

                // Calculate the Assessment Amount
                dsAssessment.decAssessmentAmt(outIdx) =
                   %DecH((decTotalPremium * decAssessmentPercent)/100:11:0);

                // Load the Prior Assessment Amount
                dsPriorBilled.decPriorBilledAmt(outIdx) =
                   %DecH(arrPreviousBilled(idx):11:0);

             EndIf;
          EndFor;

          // Load parameters
          @@ASMNT    = dsAssessment;
          @@PRVASMNT = dsPriorBilled;
       EndIf;
       //*** End   Add *** 06/12/24 ***************************************
      *
     C                   Z-ADD     PBAL          @@PREV
     C                   Z-ADD     X3AMT         @@PPRM
     C                   Z-ADD     X3TAX         @@PTAX
     C                   MOVE      Q1FACT        @@FACT
      /Free
       If %Parms > 16;
          @@Prem2 = xxExactAmt;
          Eval(h) @@Tax2  = xxExactAmt * RRtxpc;
          Eval(h) @@pprm2 = x3Amt;
          Eval(h) @@ptax2 = x3Tax;
          @@Fact2 = Q2Fact;
       Endif;
      /End-Free
     C                   ENDIF
     C                   CLEAR                   Q1INFO
     C                   MOVE      XXAMT3        Q1AMT3
     C                   MOVE      XXFACT        Q1FAC1
     C                   MOVE      XXPLV1        Q1PLV1
     C                   MOVE      XXWFAC        Q1WFAC
     C                   MOVE      XXWVFL        Q1WVFL
     C     Q1PRMS        IFGT      13
     C                   MOVE      Q1INFO        @@INFO
     C                   ENDIF
      /free
          Exsr HdgPro;
      /end-free
     C*
     C*  END THE PROGRAM
     C*
       //*** Beg Add 11/10/20 *****************************
       // Write to WISRPREM if called from WISRPAY.ProcessSrPayments
       //  for EFT or 3rd Party Request
       If indPortalCall = *On;
          Chain (xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr) pre00;
          REco#  = xxCo#;
          REfnd  = xxFnd;
          REemp# = xxEmp#;
          REdiv  = xxDiv;
          REfyr  = xxFyr;
          REmapr = @@Mapr;
          REadld = @@AdLd;
          REpayr = @@Payr;
          REprem = @@Prem;
          REtax  = @@Tax;
          REPPrm = @@PPrm;
          REPTax = @@PTax;
          REfact = @@Fact;
          REtax  = @@Tax;
          REprev = @@Prev;
       //*** begin add ***  05/31/23  **************************************
          RePBalTxt = xxPBal;
       //*** end add *****  05/31/23  **************************************
          If %Found(WISRPREM);
             Update pre00;
          Else;
             Write pre00;
          Endif;
       Endif;
       //*** End Add 11/10/20 *****************************

     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
     C***************************************************************
     C*
     C*  SUBROUTINE TO PRORATE PREMIUM FACTORS
     C*
     C***************************************************************
     C     SRPRO         BEGSR
     C*
     C                   MOVE      *ON           *IN74
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     XXTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C*
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
     C                   ADD       1             QQDAYS
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQFROM
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XQTO
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
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     QQERR         IFNE      '1'
     C                   SUB       1             QQDAYS
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* PRORATE AGAINST THE NUMBER OF DAYS IN THE MONTH IF RPT FREQ NOT WEEKLY
     C*
     C     *IN46         IFEQ      *OFF
     C                   Z-ADD     XXRPMN        X                 3 0
     C                   Z-ADD     MD(X)         XXDAYS            5 0
     C     XXRPMN        IFEQ      2
     C     XXRPYR        DIV       4             XXDIVR            2 0
     C                   MVR                     XXREM             2 0
     C     XXREM         IFNE      *ZERO
     C                   Z-ADD     28            XXDAYS
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXRPMN        IFEQ      XXFPMO
     C     XXRPYR        ANDEQ     XXFPYR
     C                   SUB       XXFPDY        XXDAYS
     C                   ADD       1             XXDAYS
     C*
     C     XXDAYS        IFEQ      *ZERO
     C                   ADD       1             XXDAYS
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXRPMN = XXFPMO
     C*                                                AND XXRPYR = XXFPYR
     C     XXRPMN        IFEQ      XXTPMO
     C     XXRPYR        ANDEQ     XXTPYR
     C                   Z-ADD     XXTPDY        XXDAYS
     C                   ENDIF                                                  XXRPMN = XXTPMO
     C*                                                AND XXRPYR = XXTPYR
     C                   ENDIF
     C     QQDAYS        DIV       XXDAYS        XXPRO            11 9
     C*
     C                   ENDSR
     C*
     C     CALDBR        BEGSR
     C*  SCHEDULE DB/CR
     C*
     C     ELDBCR        IFNE      *ZERO
     C*
     C     ELSDEF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELSDEF        XXILEF
     C                   ENDIF
     C*
     C     ELSDEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELSDEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSDEX        ANDNE     ELSDEF
     C     ELSDEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     ELSDEX        ANDEQ     *ZERO
     C*
      /free
        If ELdbcr > 1;
           If RRdbhd = *Blanks;
              strTmp = Con(7);
           Else;
              strWk20 = %Xlate(lo:up:RRdbhd);
              strTmp  = strWk20;
           Endif;
         Else;
           If RRcrhd = *Blanks;
              strTmp = Con(8);
           Else;
              strWk20 = %Xlate(lo:up:RRcrhd);
              strTmp  = strWk20;
           Endif;
        Endif;  // ELDBCR IFGT

         If cltid = '136';
            strTmp = Con(14);
         Endif;
         %Subst(strTmp:23:1) = '.';
         Select ;
         When cltid = '136';
            ELdbcr = ELcmul * ELdbcr;
            strAsp1 = %Editc(ELdbcr:'X');
         Other;
            strAsp1 = %Editc(ELdbcr:'X');
         EndSl;
         strNum = strAsp1;

         %Subst(strTmp:22:1) = %Subst(strNum:1:1);
         %Subst(strTmp:24:1) = %Subst(strNum:2:1);
         %Subst(strTmp:25:1) = %Subst(strNum:3:1);
         %Subst(strTmp:26:1) = %Subst(strNum:4:1);
         %Subst(strTmp:27:1) = %Subst(strNum:5:1);
         Hdg(intIncr) = strTmp;
         clear strTmp;
      /end-free
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   SUB       1             ELDBCR
     C                   MULT(H)   XXPRO         ELDBCR
     C                   ADD       1             ELDBCR
     C*
      /free
            %Subst(strTmp:8:4) = '%)**' ;
            %Subst(strTmp:3:1) = '.' ;
            strAsp1 = %Editc(ELdbcr:'X');
            strNum = strAsp1;
            %Subst(strTmp:1:1) = '(';
            %Subst(strTmp:2:1) = %Subst(strNum:1:1);
            %Subst(strTmp:4:1) = %Subst(strNum:2:1);
            %Subst(strTmp:5:1) = %Subst(strNum:3:1);
            %Subst(strTmp:6:1) = %Subst(strNum:4:1);
            %Subst(strTmp:7:1) = %Subst(strNum:5:1);
            Pro(intIncr) = strTmp;
            Clear strTmp;
      /end-free
     C                   ENDIF
     C*
     C                   MULT(H)   ELDBCR        XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   ELDBCR        XXWFAC
     C                   ENDIF
     C*
      /free
       If cltID = '102' And isNetRated;
          Hdg(intIncr) = ' ';
          Pro(intIncr) = ' ';
       Else;
          intIncr = intIncr + 1;
       EndIf;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     CLTID         IFEQ      '136'
     C     ELCMUL        ANDNE     *ZERO
     C                   MULT(H)   ELCMUL        XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   ELCMUL        XXWFAC
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     XXFACT        SVFACT           11 9
     C                   ENDSR
      *============================================================
      * APPLY LARGE DEDUCTIBLE DISCOUNT
      *============================================================
     C     APLGDD        BEGSR
      *
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C     LDPLV1        ANDEQ     XXPLV1
     C*
     C     LDEFD1        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD1        XXILEF
     C                   ENDIF
     C*
     C     LDEXP1        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP1        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDNE     LDEFD1
     C     LDEXP1        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP1        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT1
     C*
     C                   ENDIF
     C*
     C     LDPCT1        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     LRGDFL        IFEQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C     LDPLV2        ANDEQ     XXPLV2
     C*
     C     LDEFD2        IFEQ      *ZERO
     C                   MOVE      LDEXP1        XXILEF
     C                   ELSE
     C                   MOVE      LDEFD2        XXILEF
     C                   ENDIF
     C*
     C     LDEXP2        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX            7 0
     C                   ELSE
     C                   MOVE      LDEXP2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDNE     LDEFD2
     C     LDEXP2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
     C     LDEXP2        ANDEQ     *ZERO
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPDT
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT            7 0
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPDT
     C                   MOVE      XXRPDT        XXTODT            7 0
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LDPCT2
     C*
     C                   ENDIF
     C*
     C     LDPCT2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C                   ENDIF
     C*
     C                   ENDIF
      *
     C                   ENDSR
      /free
       //============================================================
       // HdgPro - Return Heading and Prorated Premium Factor
       //============================================================
       Begsr HdgPro;

       If Q1prms > 14;
          @@Hdg  = ' ';

          @@Hdg  = hdg(1) + hdg(2) + hdg(3) + hdg(4) + hdg(5) + hdg(6)+
          hdg(7) + hdg(8) + hdg(9) + hdg(10);
       Endif;

       If Q1prms > 15;
          @@Pro  = ' ';
          @@Pro  = pro(1) + pro(2) + pro(3) + pro(4) + pro(5) + pro(6) +
          pro(7) + pro(8) + pro(9) + pro(10);
       Endif;

       EndSr;
      /end-free
      *============================================================
      *------------------------------------------------------------
      * For Sunz only, calc prem with % of man prem factor
      *------------------------------------------------------------
     pCalcWithPrcMan   b
     dCalcWithPrcMan   pr

      /Free
       scBlAm = %IntH(scMaPr * ELmanp / 100);
       xxExactAmt = %DecH(scMaPr*ELmanp/100:11:2);
      /End-Free

     pCalcWithPrcMan   e
      *============================================================
      * GetCurBal - returns balance on current year
      *============================================================
     p GetCurBal       b
     d GetCurBal       pi            +2    like(EAamt)
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d  depMode                        n   options(*nopass)
     d balAmount       s             +2    like(EAamt)
     d calcDeposit     s               n   inz(*off)
     d xxArty          s                   like(EAarty)
      /free
       If %parms > 5;
          calcDeposit = depMode;
       Endif;
       Setll (inCo#:inFnd:inEmp#:inDiv:inFyr) pea01;
       Reade (inCo#:inFnd:inEmp#:inDiv:inFyr) pea01;
       Dow not(%eof);
          If EActyp = *blanks;
             Chain (EAco#:EAarcd) ptt01;
             If %found;
                EActyp = TTctyp;
             Else;
                Reade (inCo#:inFnd:inEmp#:inDiv:inFyr) pea01;
                Iter;
             Endif;
          Endif;
          Select;
          When EApodt > xxDat;
         //*** beg add ***** 11/16/20 ****************
          When isNonSumDiv = *On And EActyp <> 'P' And EActyp <> 'T';
         //*** end add ***** 11/16/20 ****************
         //*** Begin Add *** 06/24/24 ****************************************
         // Exclude all Collateral amounts
          When (EAarcd = Excolb or EAarcd = Excolo or EAarcd = Excolp
           or EAarcd = Excolt);
         //*** End   Add ***** 06/24/24 ****************
          When calcDeposit = *on and EActyp = 'D';
            balAmount = balAmount + Arvalue(EAamt:EAarty);
          When calcDeposit = *off and EActyp <> 'D';
            balAmount = balAmount + Arvalue(EAamt:EAarty);
          Endsl;
       Reade (inCo#:inFnd:inEmp#:inDiv:inFyr) pea01;
       Enddo;
       // Add balance from untransferred batch payments
       Setll (inCo#:inFnd:inFyr:inEmp#:inDiv) pea03b;
       Reade (inCo#:inFnd:inFyr:inEmp#:inDiv) pea03b;
       Dow not(%eof);
          Chain (EAco#:EAarcd) ptt01;
          If %found;
             xxArty = TTtype;
          Else;
             Reade (inCo#:inFnd:inFyr:inEmp#:inDiv) pea03b;
             Iter;
          Endif;
          Select;
          When EAstat <> *blank;
         //*** Begin Add *** 06/24/24 ****************************************
         // Exclude all Collateral amounts
          When (EAarcd = Excolb or EAarcd = Excolo or EAarcd = Excolp
           or EAarcd = Excolt);
         //*** End   Add ***** 06/24/24 ****************
         //*** beg add ***** 11/16/20 ****************
          When isNonSumDiv = *On And EActyp <> 'P' And EActyp <> 'T';
         //*** end add ***** 11/16/20 ****************
         //*** begin chg ***  04/06/22  ***********************************
         //***** Other;
         //*****    balAmount = balAmount + ArValue(EAamt:xxArty);
          When calcDeposit = *on and EActyp = 'D';
            balAmount = balAmount + Arvalue(EAamt:xxArty);
          When calcDeposit = *off and EActyp <> 'D';
            balAmount = balAmount + Arvalue(EAamt:xxArty);
         //*** end chg *****  04/06/22  ***********************************
          Endsl;
       Reade (inCo#:inFnd:inFyr:inEmp#:inDiv) pea03b;
       Enddo;
       Return balAmount;
      /end-free
     p GetCurBal       e
      *============================================================
      * GetPYrBal - returns balance on prior years
      *============================================================
     p GetPYrBal       b
     d GetPYrBal       pi            +2    like(EAamt)
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d balAmount       s             +2    like(EAamt)
      /free
       // EXayfl - include all years in Monthly Interim Prv Bal
       // EXpybs - include all years in S/R Online
       If EXayfl <> 'Y' and xxSron = *off
        or EXpybs <> 'Y' and xxSron = *on;
          Return 0;
       Endif;
       Setll (inCo#:inFnd:inEmp#:inDiv) pel02;
       Reade (inCo#:inFnd:inEmp#:inDiv) pel02;
       Dow not(%eof) and ELfyr < inFyr;
          //Skip contested audits
          If ELfaf = 'A' and ELctst = 'Y';
             Reade (inCo#:inFnd:inEmp#:inDiv) pel02;
             Iter;
          Endif;
          balAmount = balAmount +
           getCurBal (inCo#:inFnd:inEmp#:inDiv:ELfyr);
       Reade (inCo#:inFnd:inEmp#:inDiv) pel02;
       Enddo;
       //*** Beg Add *** 03/25/21 *********************************
       //*** Begin Del *** 02/04/25 *******************************
       //*** If ICpolicyOpt(inCo#:inFnd:inEmp#) = 'Y';
       //***    Return 0;
       //*** Else;
       //*** End   Del *** 02/04/25 *******************************
       //*** End Add *** 03/25/21 *********************************
       Return balAmount;
       //*** Beg Add *** 03/25/21 *********************************
       //*** Begin Del *** 02/04/25 *******************************
       //*** Endif;
       //*** End   Del *** 02/04/25 *******************************
       //*** End Add *** 03/25/21 *********************************
      /end-free
     p GetPYrBal       e
      *============================================================
      * GetDepBal - returns deposit balance
      *============================================================
     p GetDepBal       b
     d GetDepBal       pi            +2    like(EAamt)
     d  inCo#                              like(ELco#)
     d  inFnd                              like(ELfnd)
     d  inEmp#                             like(ELemp#)
     d  inDiv                              like(ELdiv)
     d  inFyr                              like(ELfyr)
     d balAmount       s             +2    like(EAamt)
     d depMode         s               n   inz(*on)
      /free
       If EXindp <> 'Y';
          Return 0;
       Endif;
       //*** Beg Add *** 03/25/21 *********************************
       //*** Begin Del *** 08/22/23 *******************************
       //*** If ICpolicyOpt(inCo#:inFnd:inEmp#) = 'Y';
       //***   Return 0;
       //*** Endif;
       //*** End   Del *** 08/22/23 *******************************
       //*** End Add *** 03/25/21 *********************************
       Setgt (inCo#:inFnd:inEmp#:inDiv:infyr) pel02;
       Readpe (inCo#:inFnd:inEmp#:inDiv) pel02;
       Dow not(%eof);
          Chain (ELco#:ELfnd:ELfyr) pf101;
          Select;
          // If current year is flagged for single year deposit,
          //  get balance for current year and leave.
          When ELfyr = inFyr and F1dpym = 'Y';
             balAmount = balAmount +
              getCurBal (inCo#:inFnd:inEmp#:inDiv:ELfyr:depMode);
             Leave;
          // Skip if prior year is flagged for single year deposit
          When ELfyr < inFyr and F1dpym = 'Y';
          Other;
             balAmount = balAmount +
              getCurBal (inCo#:inFnd:inEmp#:inDiv:ELfyr:depMode);
          Endsl;
       Readpe (inCo#:inFnd:inEmp#:inDiv) pel02;
       Enddo;
       Return balAmount;
      /end-free
     p GetDepBal       e
**
312931303130313130313031
**
DEDUCTIBLE CREDIT
MANAGED CARE CREDIT
SAFETY CREDIT
DEVIATION
CONTRACTORS CLASS
STOCK DISCOUNT
SCHEDULE DEBIT
SCHEDULE CREDIT
2ND DEVIATION
State Tax
ARAP CHARGE
EXPERIENCE MODIFIER
2nd DEDUCTIBLE CREDIT                                      01/08/04
COMPANY FACTOR
FACULTATIVE REINSURANCE                                    07/10/06
Foreign Terrorism
STBL FACTOR
HCN CREDIT                                                 11/10/10
TERRITORY RATING
CON,20  future use
CON,21  future use
** MESSAGES
 ** PREVIOUS BALANCE INCLUDES
