     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7291R1 - AGING REPORT
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*    This program reads the employer workfile (WXEBP),          *
     H*    evaluates the employers in it and produces an aging report *
     H*    from this evaluation.                                      *
     H*                                                               *
     H*    Aging report was established as a separate report, taken   *
     H*    from Employer Late Notice report.                          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W7291C1                                                    *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 02/04/98  PROGRAMMER- C:87  PROJECT: CM033          *
     H*****************************************2***********************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 01/06/25 C:333 Project: WC2997                                *
     H*                Aging Rpt Output and File Output Control       *
     H*                                                               *
     H* 04/06/21 C:180 Project: WC1986                                *
     H*                Add GetEftStatus to replace old methods        *
     H*                                                               *
     H* 08/01/16 C:307 Support: 42617                                 *
     H*                Code chanegs to print effective date range     *
     H*                in header.                                     *
     H*                                                               *
     H* 06/15/16 C:136 Project: SY371                                 *
     H*                Add Phase 2 Scriptura processing               *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 05/02/14 C:283 Support: WA146 85109                           *
     H*                Check the Audited status using the logical     *
     H*                with AR Type 'D' and use the Transaction       *
     H*                date of Audit Transaction for processing.      *
     H*                                                               *
     H* 10/11/11 C:189 Support: WA146 SEQ78213                        *
     H*                Routing page is generating one extra page due  *
     H*                to a erroneous SKIP operation                  *
     H*                                                               *
     H* 10/22/10 C:255 Support: SEQ73572                              *
     H*                Checked object WA073R in library list instead  *
     H*                Call Stack.                                    *
     H*                                                               *
     H* 10/06/10 C:136 SUPPORT: SY190                                 *
     H*                ADD NEWER ASSESSMENT CATEGORIES TO AGING       *
     H*                ALGORITHM                                      *
     H*                                                               *
     H* 07/27/10 C:257 SUPPORT: SEQ73572
     H*                IF API WA073R EXIST IN CALL STACK THEN USE
     H*                FILE WMEFL6 INSTEAD OF WMDBL3
     H*                                                               *
     H* 05/21/10 C:180 SUPPORT 72755
     H*                RESEARCH BY CG CONFIRMED CHANGE ON 12/29/09
     H*                WAS WRONG AND FIX SHOULD BE APPLICABLE TO
     H*                ALL CLIENTS, NOT JUST GIC.
     H*
     H* 05/20/10 C:180 SUPPORT 72755
     H*                GIC ONLY (AT THIS TIME) POPULATE 90DAY BUCKET
     H*                ON THE QUEST REPORT.
     H*
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 12/29/09 C:220 Seq#: 139WKG/71155                              *
     H*                Output file field need to populate.            *
     H*                                                               *
     H* 04/23/09 C:33  Seq#: WA132/64681                              *
     H*                Date calculations need to be moved one day.    *
     H*                                                               *
     H* 04/07/09 C:220 SUPPORT: 66684                                 *
     H*                MAKE CPYF CMD COMP WITH FILE WMEAP             *
     H*                -COMPILED ONLY.                                *
     H*                                                               *
     H* 01/15/09 C:191 SEQ#: WA136/66113                              *
     H*                FIX: SOME TRANSACTION WERE ERRORNEOUSLY SKIPPED*
     H*                                                               *
     H* 12/18/08 C:197 PROJECT 139WKG                                 *
     H*                CHANGE W7290R1 TO EXPAND AGING DATA CATEGORIES *
     H*                                                               *
     H* 07/18/07 C:183 PROJECT WC1491                                 *
     H*                ADD PROCESSING FOR DEPOSIT REFUDN AND VOID     *
     H*                TYPE TRANSACTIONS.                             *
     H*                                                               *
     H* 05/29/07  C:33   Project: WC1443   WA139/58533                *
     H*                  Account for groups that don't exist.         *
     H*                                                               *
     H* 08/22/06 C:33  Seq#: WA045/54091                              *
     H*               -Activity status not being loaded properly.     *
     H*                                                               *
     H* 08/16/06  C:33   Project: WC1443                              *
     H*                  Aging Report Across Groups                   *
     H*                                                               *
     H* 08/09/06 C:136 PROJECT: 136WBG                                *
     H*                ADD DEPOSIT BY POLICY YEAR MODE                *
     H*                                                               *
     H* 01/10/05  C:33 Project: WC1050   WA139/50774                  *
     H*                Don't produce a routing page for groups that   *
     H*                 do not exist.                                 *
     H*                                                               *
     H* 10/19/05 C:156 PROJECT: 132WEF                                *
     H*                Calculate number of days between the report    *
     H*                Balance Date and the last installment in A/R.  *
     H*                                                               *
     H* 07/07/05 C: 58 PROJECT: WC1205
     H*              - Access activity status code from employer
     H*                detail WDELP rather than master WMEMP.
     H*                                                               *
     H* 01/07/05 C:136 Support: WA121 44786                           *
     H*                Add additional processing to exlude A/R codes  *
     H*                when categories are selected when called from  *
     H*                the preprocessor. An edit prevents this        *
     H*                from happening when called from the prompt.    *
     H*                                                               *
     H* 12/17/04 C:136 PROJECT: WC1248
     H*                Age all records by posting date.               *
     H*                                                               *
     H* 12/07/04 C:136 PROJECT: 129WHU                                *
     H*                Add FCC Mods to Base.                          *
     H*                Add a new selection for ACH direct debit,      *
     H*                vendor payroll, or Direct Bill only            *
     H*                transactions.                                  *
     H*                                                               *
     H* 10/29/04 C:136 Support: WA109 44248                           *
     H*                Zero out all the buckets if current anount is  *
     H*                zero.                                          *
     H*                                                               *
     H* 09/07/04  C:53 Project wc1050                                 *
     H*                Add aging rpt processing from A/R detail.      *
     H*                                                               *
     H* 05/13/04 C:136 Support: WA121 41675                           *
     H*                Add billings subsequent to the balance date    *
     H*                from WMEAP and WMXAP to the deferred amount    *
     H*                (AGDEFR) for the Clarendon Preprocessor.       *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 08/19/03 C:136 SUPPORT: WA091 38663                           *
     H*                Amounts less than the minimum credit or debit  *
     H*                parameter are printing. Also removed           *
     H*                unneccessary IF statement.                     *
     H*                                                               *
     H* 09/17/02 C:09  PROJECT SUPPORT (SEQ #34195)                   *
     H*                Correct coding from 11/13/01 (below) which     *
     H*                addressed issues on Sequence # 29658.          *
     H*                We need to include (+/-) 'H' transactions      *
     H*                based on Posting Date even though -'H' trans   *
     H*                are being aged as though they are deposit      *
     H*                payments ('I' types).                          *
     H*                                                               *
     H* 05/15/02 C:136 SUPPORT: WA109 SEQ:32468                       *
     H*                Aging report not producing correct totals.     *
     H*                                                               *
     H* 05/15/02  C:65   PROJECT: WC1058                              *
     H*                  ADD CODE FOR INCLUDE/EXCLUDE CATEGORIES.     *
     H*                  THIS CODE DOES NOT RUN CONCURRENT WITH       *
     H*                  A/R CODE SELECTION.                          *
     H*                                                               *
     H* 05/03/02  C:35   PROJECT: WC970                               *
     H*                  IF AMOUNT DUE IS 0 BUT THERE IS A DEFERRED   *
     H*                  AMOUNT, WRITE TO THE OUTPUT FILE.            *
     H*                                                               *
     H* 05/02/02  C:35   PROJECT: WC970                               *
     H*                  DO NOT INCLUDE BILLINGS IN DEFERRED DUE IF   *
     H*                  POLICY EFFECTIVE DATE IS AFTER BALANCE DATE. *
     H*                                                               *
     H* 11/30/01 C:61  SUPPORT WA095 29888                            *
     H*                Code currently processes taxes (fees with      *
     H*                category 'T') the same as billings.  Expanded  *
     H*                code to treat fees of categories S, 1, 2, 3,   *
     H*                F, and D the same way.                         *
     H*                                                               *
     H* 11/29/01 C:61  SUPPORT WA109 29955 CM033                      *
     H*                Correct totals by agent.                       *
     H*                                                               *
     H* 11/13/01 C:09  PROJECT SUPPORT (WA109/SEQ #29658)             *
     H*                Treat Deposit billings ('H') transactions that *
     H*                are reversed (i.e. negative 'H' trans) as      *
     H*                payments.  This will ensure the aging looks    *
     H*                right when deposits are billed at the beg of   *
     H*                each year and then transferred to prem at the  *
     H*                end of the year.                               *
     H*                                                               *
     H* 10/31/01 C:65  PROJECT WC970                                  *
     H*                ADD PROCESSING FOR CCGI DATA FIELDS ADDED INTO *
     H*                WFAGP.  THESE REPLACE THE FORMER WFRAP FILE.   *
     H*                                                               *
     H* 10/31/01 C:65  PROJECT SUPPORT                                *
     H*                CORRECT THE MINIMUM DEBIT/CREDIT CODE.  IT     *
     H*                WAS NOT WORKING AT ALL IF THE DEBIT OR CREDIT  *
     H*                OPTION WAS CHOSEN.                            *
     H*                                                               *
     H* 08/23/01 C:61  PROJECT WC1050                                 *
     H*                When accumulating payments, select if          *
     H*                transaction date is less than or equal to the  *
     H*                balance date.  If a credit balance remains,    *
     H*                use the transaction dates of the most recent   *
     H*                payment transactions and apply to the aging    *
     H*                column.                                        *
     H*                                                               *
     H* 11/16/00  C:35   PROJECT: WC577                               *
     H*                  ADD CODE FOR INCLUDE/EXCLUDE ACTIVITY STATUS *
     H*                  CODES.                                       *
     H*                                                               *
     H* 10/18/00  C:35   PROJECT: SUPPORT WA109 #24688                *
     H*                  WHEN APPLYING NEGATIVE PAYMENT BALANCE TO    *
     H*                  BUCKETS, NEED TO READ BACK THROUGH ALL YEARS *
     H*                  FOR EAARTY = 'I' TRANSACTIONS.               *
     H*                                                               *
     H* 08/24/00  C:35   PROJECT: SUPPORT WA109 #24088                *
     H*                  DETAIL ITEMS WERE BEING ADDED TO TOTALS      *
     H*                  WITHOUT THE SAME CONDITIONS THAT WERE        *
     H*                  CONTROLLING THE PRINT, SO TOTALS DID NOT     *
     H*                  MATCH DETAIL.                                *
     H*                                                               *
     H* 03/29/00 C:90  PROJECT: SEQ#WA033/22323                       *
     H*                CONDITIONED THE PRINTING OF DET, DET1, DET2    *
     H*                SO THAT MULTIPLE EMPLOYERS DO NOT PRINT ON     *
     H*                THE SAME LINE.                                 *
     H*                                                               *
     H* 01/18/00 C:90  PROJECT: WC831                                 *
     H*                IF ENTERED, ONLY PRINT BALANCE AMOUNT IF IT    *
     H*                MEETS THE ENTERED MINIMUM CREDIT/DEBIT BALANCE *
     H*                AMOUNT. WRITE REPORT FIELDS TO REPORT WORKFILE *
     H*                WFAGP.                                         *
     H*                                                               *
     H* 12/17/99 C:33  PROJECT: WC899    WA096/20958                  *
     H*                BY-PASS ENTIRE CALC IF ACCOUNT CURRENT POLICY. *
     H*                                                               *
     H* 08/23/99 C:87  PROJECT: SUPP WA093 #19191                     *
     H*                Change pgm to base audit status on ELFAF.      *
     H*                                                               *
     H* 08/05/99 C:33  SEQ#: WA093/19050                              *
     H*                PRINT THE PROPER CANCEL DATE.                  *
     H*                                                               *
     H* 07/02/99 C:87  PROJECT: SUPP WA072 #18705                     *
     H*                Modify so that 'Tax billed' is calculated the  *
     H*                same as other billing transactions (a/r types  *
     H*                'B' and 'A')                                   *
     H*                                                               *
     H* 06/18/99 C:118 SUPPORT:  WA100/18515                          *
     H*                Modify report output so that HEAD2 prints at   *
     H*                the top of each page when EA or EN is chosen.  *
     H*                                                               *
     H* 04/12/99 C:90 PROJECT: 96WAD (IN BASE: 07/19/99 C:33 WC899)   *
     H*               DO NOT PROCESS ACCOUNT CURRENT POLICIES.        *
     H*                                                               *
     H* 01/12/99 C:87 PROJECT: WC831                                  *
     H*               Add pol from date to report. Display canx       *
     H*               date even if canx is not yet effective (canx is *
     H*               pending). Add employer count. Only count        *
     H*               employer as audited if ELFAF = 'A' to prevent   *
     H*               pgm from picking up revised audits as audited.  *
     H*                                                               *
     H* 01/05/99 C:87 PROJECT: SUPP WA091 15120                       *
     H*               Make report process sort by agent name the same *
     H*               as sort by agent #.                             *
     H*                                                               *
     H* 12/10/98 C:61 SUPPORT 14595                                   *
     H*               Only process active (WMA3P/A3STAT = 'A')        *
     H*               Employer 3rd Address records.                   *
     H*                                                               *
     H* 11/16/98 C:87 PROJECT: SUPP WA090 15057                       *
     H*               Modified code to correct report to page break   *
     H*               on new agent when processed by agt number (AN). *
     H*                                                               *
     H* 08/20/98 C:87 PROJECT: CM033                                  *
     H*               WHEN SORTING BY AGENT NUMBER('AN'), START A     *
     H*               NEW PAGE FOR EACH AGENT# AND SUM NUMBER OF      *
     H*               EMPLOYERS FOR EACH PAGE.  ALSO ADD HEADER LINES *
     H*               TO PRINT AGENT NAME AND ADDRESS.                *
     H*                                                               *
     H* 07/14/98 C:09 PROJECT: WC831                                  *
     H*               DO NOT PRINT AN EMPLOYER DETAIL LINE IF ALL     *
     H*               AMOUNTS ARE ZERO.                               *
     H*                                                               *
     H*****************************************************************
     H*                                                               *
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*         08  -  BATCH MODE                                     *
     H*         09  -  DEMAND MODE                                    *
     H*         20-49  USED TO LIST SELECTED A/R CODES FROM PROMPT    *
     H*                SCREEN ON ROUTING PAGE.                        *
     H*         50  -  EOF INDICATOR FOR MAIN READ LOOP               *
     H*         51  -  *ON : A/R CODES 'INCLUDED', *OFF : 'EXCLUDED'  *
     H*         60  -  GENERAL CHAINING                               *
     H*         61  -  GENERAL CHAINING                               *
     H*         71  -  ACCT AUDIT STATUS (ON=AUDITED)                 *
     H*         72  -  ACCT ACTIVE & NOT CX PEND AS OF RUN DATE       *
     H*         73  -  ACCT IS CX PEND AS OF RUN DATE                 *
     H*         74  -  ACCT IX CANCELLED AS OF RUN DATE               *
     H*         75  -  *ON - SELF REPORTING, *OFF - ESTIMATING        *
     H*         77  -  EMFDNM FEDERAL NAME EXISTS                     *
     H*         80  -  CHAIN EMPLYR MSTR OK                           *
     H*         81  -  CHAIN EMPLYR DTL OK                            *
     H*         82  -  CHAIN FND DTL OK                               *
     H*         83  -  VALIDATE SLECTED ACCT STATUS                   *
     H*         91  -  Used to specify whether to print all accounts  *
     H*                that have a credit only, a debit only, or      *
     H*                both.                                          *
     H*         97  -  INDICATES AGENT BUCKETS W/ NON-ZERO VALUES     *
     H*                                                               *
     H*****************************************************************
     F*
     FSMORL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMA3L     IF   E           K DISK
     FWMEAL8    IF   E           K DISK
     FWMEAL3    IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWMEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03)
      *
     FWMEAL     IF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
      *
     FWDELL1    IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWXGSL     IF   E           K DISK
     F                                     RENAME(PGS01:PGS00)
     FWXEBL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F*
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWFAGP     O    E             DISK
     F                                     RENAME(PAG01:PAG02)
     FWFAGP2    O    E             DISK
     F                                     RENAME(PAG01:PAG03)
     FWTAVL     IF   E           K DISK
     FWTCTL     IF   E           K DISK
     FWMCPL     IF   E           K DISK
     FWMREL     IF   E           K DISK
     FWMXAL     IF   E           K DISK
     FWMDBL3    IF   E           K DISK
     FWDVHL1    IF   E           K DISK
     FWMEFL6    IF   E           K DISK
     FQSYSPRT2  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FWMEAL10   IF   E           K DISK
     F                                     RENAME(PEA01:PEA10)

     D*
     D GP              S              3  0 DIM(999)                             Groups NOT used
     D GPN             S             40    DIM(999)                             Group Names
      *** beg chg ***** 06/17/16 **************************
     D*** COD             S              2    DIM(30) ASCEND                       A/R CODES
     D*** DES             S             30    DIM(30)                              A/R CODE DESC
     D*** CDE             S              2    DIM(30)                              AR CDE FOR OUTPUT
     D*** AS              S              2    DIM(5) ASCEND                        ACTIVITY STATUS
     D*** ASDS            S             30    DIM(30)                              ACT STS DESC
     D*** CT              S              1    DIM(10) ASCEND                       CATEGORIES    S
     D*** CTS             S             30    DIM(10)                              CATEGORY DESC

     d cde             s              2    dim(30)
     d cod             s              2    dim(30) ascend                       A/R Codes
     d des             s             30    dim(30)                              A/R Code Desc
     d ct              s              1    dim(10) ascend                       A/R Ctgy
     d cts             s             30    dim(10)                              A/R Ctgy Desc
     d as              s              2    dim(5) ascend                        Act Status
     d asds            s             30    dim(5)                               Act Status Desc
     d
     d                 ds
     d xar                     1     60    dim(30)
     d  AGarcde1               1      2
     d  AGarcde2               3      4
     d  AGarcde3               5      6
     d  AGarcde4               7      8
     d  AGarcde5               9     10
     d  AGarcde6              11     12
     d  AGarcde7              13     14
     d  AGarcde8              15     16
     d  AGarcde9              17     18
     d  AGarcde10             19     20
     d  AGarcde11             21     22
     d  AGarcde12             23     24
     d  AGarcde13             25     26
     d  AGarcde14             27     28
     d  AGarcde15             29     30
     d  AGarcde16             31     32
     d  AGarcde17             33     34
     d  AGarcde18             35     36
     d  AGarcde19             37     38
     d  AGarcde20             39     40
     d  AGarcde21             41     42
     d  AGarcde22             43     44
     d  AGarcde23             45     46
     d  AGarcde24             47     48
     d  AGarcde25             49     50
     d  AGarcde26             51     52
     d  AGarcde27             53     54
     d  AGarcde28             55     56
     d  AGarcde29             57     58
     d  AGarcde30             59     60

     d                 ds
     d xard                    1   1500    dim(30)
     d  AGardsc1               1     50
     d  AGardsc2              51    100
     d  AGardsc3             101    150
     d  AGarcsc4             151    200
     d  AGardsc5             201    250
     d  AGardsc6             251    300
     d  AGardsc7             301    350
     d  AGardsc8             351    400
     d  AGardsc9             401    450
     d  AGardsc10            451    500
     d  AGardsc11            501    550
     d  AGardsc12            551    600
     d  AGardsc13            601    650
     d  AGardsc14            651    700
     d  AGardsc15            701    750
     d  ARardsc16            751    800
     d  AGardsc17            801    850
     d  AGardsc18            851    900
     d  AGardsc19            901    950
     d  AGardsc20            951   1000
     d  AGardsc21           1001   1050
     d  AGardsc22           1051   1100
     d  AGardsc23           1101   1150
     d  AGardsc24           1151   1200
     d  AGardsc25           1201   1250
     d  AGardsc26           1251   1300
     d  AGardsc27           1301   1350
     d  AGardsc28           1351   1400
     d  AGardsc29           1401   1450
     d  AGardsc30           1451   1500

     d                 ds
     d xct                     1     10    dim(10)
     d  AGctcde1               1      1
     d  AGctcde2               2      2
     d  AGctcde3               3      3
     d  AGctcde4               4      4
     d  AGctcde5               5      5
     d  AGctcde6               6      6
     d  AGctcde7               7      7
     d  AGctcde8               8      8
     d  AGctcde9               9      9
     d  AGctcde10             10     10

     d                 ds
     d xctd                    1    500    dim(10)
     d  AGctdsc1               1     50
     d  AGctdsc2              51    100
     d  AGctdsc3             101    150
     d  AGctcsc4             151    200
     d  AGctdsc5             201    250
     d  AGctdsc6             251    300
     d  AGctdsc7             301    350
     d  AGctdsc8             351    400
     d  AGctdsc9             401    450
     d  AGctdsc10            451    500

     d                 ds
     d xas                     1     10    dim(5)
     d  AGascde1               1      2
     d  AGascde2               3      4
     d  AGascde3               5      6
     d  AGascde4               7      8
     d  AGascde5               9     10

     d                 ds
     d xasd                    1    250    dim(5)
     d  AGasdsc1               1     50
     d  AGasdsc2              51    100
     d  AGasdsc3             101    150
     d  AGascsc4             151    200
     d  AGasdsc5             201    250
      *** end chg ***** 06/17/16 **************************
     D*
     D ARTITL          C                   CONST('A/R Detail Report A-
     D                                     ging')
     D TITLE1          C                   CONST('Aging Report')
     D*
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     11  0
     D  QCDIV                 12     14  0
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D                 DS
     D  EF                     1     35  0
     D                                     DIM(5)                               Effective dates
     D  EMEFD1                 1      7  0
     D  EMEFD2                 8     14  0
     D  EMEFD3                15     21  0
     D  EMEFD4                22     28  0
     D  EMEFD5                29     35  0
     D*
     D                 DS
     D  CX                     1     35  0
     D                                     DIM(5)                               Canx dates
     D  EMCAD1                 1      7  0
     D  EMCAD2                 8     14  0
     D  EMCAD3                15     21  0
     D  EMCAD4                22     28  0
     D  EMCAD5                29     35  0
     D                 DS
     D  XXDBST                 1      7  0
     D  XXDBYR                 1      3  0
     D  XXDBMN                 4      5  0
     D  XXDBDY                 6      7  0
      *
     D                 DS
     D  DSEMDV                 1     15
     D  DSEMP#                 2     10
     D  DSDIV                 11     15
      *
     D DRECT           C                   CONST('DIRECT BILL')
     D VPAYR           C                   CONST('VENDOR PAYROLL')
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D xxBLDT          S              7  0
     d*** BEG ADD *** 04/06/21 ****************************************
     d strCo#          S              3a
     d strEftType      S              1a
     d strFnd          S              3a
     d strEftKey       S             30a
     d strDate         S              7a
     d strPre          S              1a    inz
     d strRrn          S              9a    inz
     d strEobTyp       S              3     inz
     d strCkTp         S             10     inz
     d strFyr          S              3     inz
     d strEnv          S              1     inz
     d*** END ADD *** 04/06/21 ****************************************

      *** beg add ***** 06/15/16 ******************************************
     d isUseScriptura  s              1n   inz
     d isUseSpoolOut   s              1n   inz
     d isUseXML        s              1n   inz
     d isUse3rdParty   s              1n   inz
     d outCo#          s              3  0 inz
     d outFnd          s              3  0 inz
     d outFldrPrefix   s              2    inz
     d outPreString    s             10    inz
     d outReturn       s            250    inz
     d outXMLFile      s             61    inz
     d processType     s            100    inz
     d xmlFile         s             60    inz
     d/copy sPrModule
     d/copy sPrXML
     d WrtRptCriteria  pr
      *** end add ***** 06/15/16 ******************************************

     d*** BEG ADD *** 04/06/21 ****************************************
     d GetEFTStatus    Pr             1n
     d inCo#                          3
     d inType                         1
     d inFnd                          3
     d inKey                         30
     d inDate                         7
     d inPre                          1
     d inRrn                          9    options(*nopass)
     d inEobType                      3    options(*nopass)
     d inCkTp                        10    options(*nopass)
     d inFyr                          3    options(*nopass)
     d inEnv                          1    options(*nopass)
     d*** END ADD *** 04/06/21 ****************************************

     C     *LIKE         DEFINE    EAPODT        XXFUTR
     C     *LIKE         DEFINE    EAPODT        XX30DT
     C     *LIKE         DEFINE    EAPODT        XX60DT
     C     *LIKE         DEFINE    EAPODT        XX90DT
     C     *LIKE         DEFINE    EAPODT        XP90DT
     C     *LIKE         DEFINE    EAPODT        X120DT
     C     *LIKE         DEFINE    EAPODT        X150DT
     C     *LIKE         DEFINE    EAPODT        X180DT
     C     *LIKE         DEFINE    EAPODT        XXABDT
     C     *LIKE         DEFINE    EAPODT        XXAUDT
     C     *LIKE         DEFINE    EBEMP#        XXEMP#
     C     *LIKE         DEFINE    AGCI#D        XX#DAY
     C     *LIKE         DEFINE    EBEMP#        RSTOEN
     C     *LIKE         DEFINE    EBEMP#        RSFREN
     C     *LIKE         DEFINE    EBDIV         XXDIV
     C     *LIKE         DEFINE    EBFND         XXFND
     C     *LIKE         DEFINE    EBFYR         XXFYR
     C     *LIKE         DEFINE    FNNAME        XXFNAM
     C     *LIKE         DEFINE    FNMAD1        XFMAD1
     C     *LIKE         DEFINE    FNMAD1        XFMAD2
     C     *LIKE         DEFINE    FNMAD1        XFMAD3
     C     *LIKE         DEFINE    EBAGT#        HLDAGT
     C     *LIKE         DEFINE    EBAGT#        XXAGT#
     C     *LIKE         DEFINE    AAFEIN        XXFEIN
     C     *LIKE         DEFINE    AAMOD         XXMOD
     C     *LIKE         DEFINE    EAPODT        XXDAT1
     C     *LIKE         DEFINE    EAPODT        XXDAT2
     C     *LIKE         DEFINE    EAPODT        XXLBDT
     **
     **   KEY LIST FOR THE USER PARAMETER FILE
     **
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     **
     **   KEY LIST FOR THE COMPANY FILE
     **
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     **
     **   KEY LIST FOR THE FUND
     **
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     **
     **   KEY LIST FOR THE EMPLOYER MASTER FILE
     **
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   DIRECT DEBIT HISTORY
     C*
     C     DBKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    SYSDT
     C*
     C*   VENDOR HISTORY
     C*
     C     VHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     VHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    SYSDT
     **
     **   KEY LIST FOR THE EMPLOYER DETAIL FILE
     **
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     **
     **   KEY LIST FOR THE 3RD ADDRESS FILE
     **
     C     A3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     **
     **   KEY LISTS FOR THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     **
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     **
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXARTY
      *
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXARTY            1
      *
     C     EAKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY
      *
     C     EAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     EAKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     **
     **   KEY LIST FOR THE FUND DETAIL FILE
     **
     C     F2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     **
     **   KEY LIST FOR THE SELECTED EMPLOYER WORK FILE
     **
     C     EBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     **
     **  KEY LIST FOR THE OVERRIDE FILE
     **
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     **
     **  KEY LIST FOR THE TABLE FILE
     **
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     **
     **  KEY LIST FOR FUND DETAIL
     **
     C     F1KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     **
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
     C*
     **  BILLING COMMENT FILE
     **
     C     BCKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBCOM           10
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C*
     C     CPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AGCHAP
     C*
     C     REKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    AGRGN#
     C     CTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCTYP            1
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQBLDT            8
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQSRT1            2
     C                   PARM                    QQSRT2            2
     C                   PARM                    QQFREA            2
     C                   PARM                    QQTOEA            2
     C                   PARM                    QQFREN            9
     C                   PARM                    QQTOEN            9
     C                   PARM                    QQFRAA            2
     C                   PARM                    QQTOAA            2
     C                   PARM                    QQFRAN            5
     C                   PARM                    QQTOAN            5
     C                   PARM                    QQACST            1
     C                   PARM                    QQESSR            1
     C                   PARM                    QQCRDB            1
     C                   PARM                    QQAUST            1
     C                   PARM                    QQCODE           60
     C                   PARM                    QQARIE            1
     C                   PARM                    QQMINC            9
     C                   PARM                    QQMIND            9
     C                   PARM                    QQIEFL            1
     C                   PARM                    QQASTC           10
     C                   PARM                    QQCTIE            1
     C                   PARM                    QQCTYP           10
     C                   PARM                    QQBTYP            1
     C                   PARM                    QQSLAR            1
      *** beg add ***** 06/15/16 **************************************
     C                   PARM                    QQXMLKEY         61
      *** beg add ***** 06/15/16 **************************************

     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     **
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     **
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     **
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     **
     ******************************************************************
     **    SET UP AND INITALIZE
     ******************************************************************
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT             7 0
     C                   MOVE      *ALL'_'       LINE            198
      *
     C     Q1PRMS        IFGE      30
     C                   MOVE      QQBTYP        X1BTYP            1
     C                   ELSE
     C                   MOVE      *BLANK        X1BTYP
     C                   ENDIF
      *** END ADD ***** 12/07/04 **************************************
      *
     C     Q1PRMS        IFGE      31
     C                   MOVE      QQSLAR        XXSLAR            1
     C                   ELSE
     C                   MOVE      *BLANK        XXSLAR
     C                   ENDIF
      *
     C                   SELECT
     C     X1BTYP        WHENEQ    *BLANK
     C                   MOVEL     '*ALL'        XXBTYP           20
     C     X1BTYP        WHENEQ    'A'
     C                   MOVEL     'ACH'         XXBTYP
     C     X1BTYP        WHENEQ    'D'
     C                   MOVEL     DRECT         XXBTYP
     C     X1BTYP        WHENEQ    'P'
     C                   MOVEL     VPAYR         XXBTYP
     C                   ENDSL
      *
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQBLDT        XXRNDT            8 0
     C                   MOVEA     QQCODE        COD
     C                   MOVE      QQMINC        XXMINC            9 2
     C                   MOVE      QQMIND        XXMIND            9 2
     C*
     C     XXMINC        MULT      -1            XXMINC
     C                   MOVEA     QQASTC        AS
     C                   MOVEA     QQCTYP        CT
     C                   MOVEL(P)  'WA073R'      XXPGM
    C*
     C                   MOVEL     '*PGM'        XXTYP             7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM
     C                   PARM                    XXTYP
     C                   PARM                    XXNACH            1

     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBLDT
      *
     C                   MOVE(P)   QQFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT            7 0
      *
     C                   MOVE(P)   QQTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXTODT            7 0
      *
      *  IF COD,30 IN ARRAY CONTAINS '@@' THEN CHG TO BLANKS. (THIS
      *    IS TO PREVENT PSBL ERR IN PASSING PARM, SEE W7290R)
      *
     C     COD(30)       IFEQ      '@@'
     C                   MOVE      *BLANKS       COD(30)
     C                   ENDIF
      *
      *  BATCH OR DEMAND  MODE
      *
     C     QQMODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09                          DEMAND MODE
     C                   ELSE
     C                   MOVE      *ON           *IN08                          BATCH MODE
     C                   ENDIF
     **
     C                   MOVE      *ALL'_'       SEPA80           75
     C                   Z-ADD     *ZERO         GRCUTL           11 2
     C                   Z-ADD     *ZERO         GR30TL           11 2
     C                   Z-ADD     *ZERO         GR60TL           11 2
     C                   Z-ADD     *ZERO         GR90TL           11 2
     C                   Z-ADD     *ZERO         GRFTOT           11 2
     C                   Z-ADD     *ZERO         GROV90           11 2
     C                   Z-ADD     *ZERO         GRTDEF           11 2
     C                   Z-ADD     *ZERO         GRTLDU           11 2
     C                   Z-ADD     *ZERO         A1CUTL           11 2
     C                   Z-ADD     *ZERO         A130TL           11 2
     C                   Z-ADD     *ZERO         A160TL           11 2
     C                   Z-ADD     *ZERO         A190TL           11 2
     C                   Z-ADD     *ZERO         A1FTOT           11 2
     C                   Z-ADD     *ZERO         A1OV90           11 2
     C                   Z-ADD     *ZERO         GRTEMP            5 0          TOTAL # EMPLOYERS
     C                   MOVE      *OFF          AGTSRT
     **
     **   CHAIN TO THE USER PARAMETER FILE
     **
     C     UPKEY         CHAIN     PUP01                              60
     **
     **  CHAIN TO THE COMPANY FILE
     **
     C     COKEY         CHAIN     PCO01                              60
     **
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        CONAME
     C                   ENDIF

     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     **
     **   CHAIN TO THE FUND MASTER FILE
     **
     C                   MOVE      *OFF          GRPFND            1
     C     FNKEY         CHAIN     PFN01                              60
     **
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           GRPFND            1
     C                   ENDIF
     C*
     C*** CHECK IF CALLED FROM A/R DETAIL RUN
     C*
     C                   CALL      'SRCHKPGM'                           99
     C                   PARM      'W7381C'      XXPGM            10
     C                   PARM      *BLANKS       SRERR
     C*
     C     SRERR         IFEQ      *ON
     C                   MOVE      *ON           ARCFLG            1
     C                   ELSE
     C                   MOVE      *OFF          ARCFLG
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   MOVE      ARCFLG        *IN83
     C*** beg add ***** 06/15/16 ******************************************
      /Free
       If Q1prms >= 32;
          processType = 'Aging Report';
          outReturn = processType;
          outFldrPrefix = *blanks;
          isUseSpoolOut  = UseSpoolOut(processType);
          isUseScriptura = UseScriptura(processType);
          isUse3rdParty  = Use3rdXml(processType);
          isUseXML = isUseScriptura or isUse3rdParty;
          If isUseXML;
             outPreString = 'AgingRpt';
             xmlFile = GetXMLFileName(' ':*zeros:outCo#:outFnd:
              outFldrPrefix:outPreString:outReturn);
             QQxmlkey = xmlfile + '@';
          Endif;
       Endif;
      /End-free
     C*** end add ***** 06/15/16 ******************************************
     **
     *****************************************************************
     ** MOVE ENTRY PARMS TO ROUTING PAGE
     *****************************************************************
      *
     C                   MOVE      QQCO#         RSCO#             3 0
     C                   MOVE      QQFND         RSFND             3 0
     C                   MOVE      QQFRDT        RSFRDT            8 0
     C                   MOVE      QQTODT        RSTODT            8 0
     C                   MOVE      QQTODT        RSRNDT            8 0
     C                   MOVE      QQBLDT        RSBLDT            8 0
     **
     ** INITIALIZE SORT PARAMETERS
     **
     C                   MOVE      *BLANK        RSFREA            2
     C                   MOVE      *BLANK        RSTOEA            2
     C                   MOVE      *ZEROS        RSFREN
     C                   MOVE      *ZEROS        RSTOEN
     C                   MOVE      *BLANK        RSFRAA            2
     C                   MOVE      *BLANK        RSTOAA            2
     C                   MOVE      *ZEROS        RSFRAN            5 0
     C                   MOVE      *ZEROS        RSTOAN            5 0
     **
     ** DISPLAY CORRECT SORT CODES
     **
      * * * * * * *
      *   SORT 1
      * * * * * * *
     C                   SELECT
      *
     C     QQSRT1        WHENEQ    'AN'
     C                   MOVEL     'AGENT #'     RSSRT1           13
     C***  BEGIN ADD  ***  08/20/98  **********************************
     C                   MOVE      *ON           AGTSRT            1
     C***  END   ADD  ***  08/20/98  **********************************
      *
     C     QQSRT1        WHENEQ    'AA'
     C     'AGENT'       CAT       'NAME':1      RSSRT1
     C***  BEGIN ADD  ***  01/05/99  **********************************
     C                   MOVE      *ON           AGTSRT            1
     C***  END   ADD  ***  01/05/99  **********************************
      *
     C     QQSRT1        WHENEQ    'EN'
     C     'EMPLOYER'    CAT       '/DIV':0      RSSRT1
      *
     C     QQSRT1        WHENEQ    'EA'
     C     'EMPLOYER'    CAT       'NAME':1      RSSRT1
      *
     C                   ENDSL
      * * * * * * *
      *   SORT 2
      * * * * * * *
     C                   SELECT
      *
     C     QQSRT2        WHENEQ    'EN'
     C     'EMPLOYER'    CAT       '/DIV':0      RSSRT2           13
      *
     C     QQSRT2        WHENEQ    'EA'
     C     'EMPLOYER'    CAT       'NAME':1      RSSRT2
      *
     C     QQSRT2        WHENEQ    *BLANK
     C                   MOVEL     'NONE'        RSSRT2
      *
     C                   ENDSL
     **
     C                   MOVE      QQFRAN        RSFRAN
     C                   MOVE      QQTOAN        RSTOAN
     C                   MOVE      QQFRAA        RSFRAA
     C                   MOVE      QQTOAA        RSTOAA
     C                   MOVE      QQFREN        RSFREN
     C                   MOVE      QQTOEN        RSTOEN
     C                   MOVE      QQFREA        RSFREA
     C                   MOVE      QQTOEA        RSTOEA
     **
     C                   SELECT
     C     QQACST        WHENEQ    'A'
     C                   MOVEL     'ACTIVE'      RSACT            25            ACTIVE
     C     QQACST        WHENEQ    'C'
     C     'CANCELLE'    CAT       'D':0         RSACT                          CANCELLED
     C     QQACST        WHENEQ    'B'
     C     'BOTH ACT'    CAT       'IVE AN':0    RSACT                          BOTH
     C                   CAT       'D CANC':0    RSACT
     C                   CAT       'ELLED':0     RSACT
     C                   ENDSL
     **
     C                   SELECT
     C     QQAUST        WHENEQ    'A'
     C                   MOVEL     'AUDITED'     RSAUST           26            AUDITED
     C     QQAUST        WHENEQ    'N'
     C     'NON-AUDI'    CAT       'TED':0       RSAUST                         NOT AUTITED
     C     QQAUST        WHENEQ    'B'
     C     'BOTH AUD'    CAT       'ITED &':0    RSAUST                         BOTH
     C                   CAT       ' NON-A':0    RSAUST
     C                   CAT       'UDITED':0    RSAUST
     C                   ENDSL
      *
     C                   SELECT
     C     QQESSR        WHENEQ    'E'
     C     'ESTIMATI'    CAT       'NG':0        RSESSR           33
     C     QQESSR        WHENEQ    'S'
     C     'SELF-REP'    CAT       'ORTING':0    RSESSR
     C                   OTHER
     C     'BOTH EST'    CAT       'IMATED':0    RSESSR
     C                   CAT       'AND SE':1    RSESSR
     C                   CAT       'LF-REP':0    RSESSR
     C                   CAT       'ORTING':0    RSESSR
     C                   ENDSL
      *
     C                   SELECT
     C     QQCRDB        WHENEQ    'C'
     C                   MOVEL(P)  'CREDIT'      RSCRDB           33
     C     QQCRDB        WHENEQ    'D'
     C                   MOVEL(P)  'DEBIT'       RSCRDB
     C                   OTHER
     C     'BOTH CRE'    CAT       'DIT':0       RSCRDB
     C                   CAT       'AND':1       RSCRDB
     C                   CAT       'DEBIT':1     RSCRDB
     C                   ENDSL
     **
     ** Sort the Code Array
     **
     C                   SORTA     COD
     C                   MOVE      *BLANK        CDE
     C                   MOVE      *BLANK        DES
      *
     C     QQARIE        IFNE      *BLANK
     **
     ** Fill the Code Decription Array which Corresponds to the Code
     **
     C                   Z-ADD     1             Z                 2 0
     C     1             DO        30            Y                 2 0
     **
     ** Indicators 20 to 49 are used for Output Purposes
     **
     C     COD(Y)        IFNE      *BLANK
     C     Z             ADD       19            Z1                2 0
     C                   MOVE      *ON           *IN(Z1)
     C                   MOVEL(P)  COD(Y)        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     COD(Y)        CDE(Z)
     C                   MOVEL     TTDESC        DES(Z)
     C                   ADD       1             Z
     C                   ENDIF
     C                   ENDIF
     C                   ENDDO                                                  1 DO 30  Y
      *
      *   SET INDICATOR FOR EXCLUDE VS INCLUDE VERBIAGE
      *
     C     QQARIE        IFEQ      'E'
     C                   MOVE      *OFF          *IN51
     C                   ELSE
     C                   MOVE      *ON           *IN51
     C                   ENDIF                                                  QQARIE = 'E'
      *
     C                   ELSE
      *
      *   PRINT ALL CODES ON ROUTING PAGE
      *
     C                   MOVE      *ON           *IN51
     C                   MOVE      '*ALL'        RSCODE            4
     **
     C                   ENDIF                                                  QQARIE IFNE *BLANK
      **** BEGIN ADD **** 05/15/02 *********************************************
      *
      * SORT THE CATEGORIES ARRAY
      *
     C                   MOVE      *BLANK        CTS
      *
     C     QQCTIE        IFNE      *BLANK
      *
      * Fill the Code Decription Array which Corresponds to the Code
      *
     C     1             DO        10            Y
      *
      * Indicators 52 to 56 AND 62 TO 66 are used for Output Purposes
      *
     C     CT(Y)         IFNE      *BLANK
      *
     C     Y             IFLT      6
     C     Y             ADD       51            Z1                2 0
     C                   ELSE
     C     Y             ADD       56            Z1                2 0
     C                   ENDIF
     C                   MOVE      *ON           *IN(Z1)
     C                   MOVE      CT(Y)         XXCTYP
      *
     C     CTKY          CHAIN     PCT01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CTDESC        CTS(Y)
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  CT,Y IFNE *BLANK
      *
     C                   ENDDO                                                  1 DO 10  Y
      *
      *   SET INDICATOR FOR EXCLUDE VS INCLUDE VERBIAGE
      *
     C     QQCTIE        IFEQ      'E'
     C                   MOVE      *OFF          *IN59
     C                   ELSE
     C                   MOVE      *ON           *IN59
     C                   ENDIF                                                  QQCTIE = 'E'
      *
     C                   ELSE
      *
      *   PRINT ALL CODES ON ROUTING PAGE
      *
     C                   MOVE      *ON           *IN59
     C                   MOVE      '*ALL'        RSCTC             4
     **
     C                   ENDIF                                                  QQCTIE IFNE *BLANK
      **** END ADD ****** 05/15/02 *********************************************
      **** BEGIN ADD **** 11/16/00 *********************************************
      *
      * SORT THE ACTIVITY STATUS ARRAY
      *
      *** beg del ***** 06/15/16 *****************
     C***********        SORTA     AS
     C***********        MOVE      *BLANK        DES
      *** end del ***** 06/15/16 *****************

     C     QQIEFL        IFNE      *BLANK
      *
      * Fill the Code Decription Array which Corresponds to the Code
      *
     C     1             DO        5             Y
      *
      * Indicators 15 to 19 are used for Output Purposes
      *
     C     AS(Y)         IFNE      *BLANK
      *
     C     Y             ADD       14            Z1                2 0
     C                   MOVE      *ON           *IN(Z1)
     C                   MOVEL(P)  AS(Y)         XXKEY
      *
     C     TBKEY         CHAIN     PAV01                              60
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AVDESC        ASDS(Y)
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  AS,Y IFNE *BLANK
      *
     C                   ENDDO                                                  1 DO 30  Y
      *
      *   SET INDICATOR FOR EXCLUDE VS INCLUDE VERBIAGE
      *
     C     QQIEFL        IFEQ      'E'
     C                   MOVE      *OFF          *IN14
     C                   ELSE
     C                   MOVE      *ON           *IN14
     C                   ENDIF                                                  QQIEFL = 'E'
      *
     C                   ELSE
      *
      *   PRINT ALL CODES ON ROUTING PAGE
      *
     C                   MOVE      *ON           *IN14
     C                   MOVE      '*ALL'        RSASTC            4
     **
     C                   ENDIF                                                  QQIEFL IFNE *BLANK
     C*
      **** Beg Chg ***** 01/06/25 *****************************************
     C***  Begin Add  ***  01/10/06  **************************************
     C***  GRPFND        IFEQ      *ON
     C***  Begin Add  ***  08/16/06  **************************************
     C***  ARCFLG        OREQ      *OFF
     C***  End   Add  ***  08/16/06  **************************************
           If (GRPFND = *ON) OR (ARCFLG = *OFF)
           OR (GRPFND = *OFF AND ARCFLG = *ON);
      **** End Chg ***** 01/06/25 *****************************************
     C*
     C                   OPEN      QSYSPRT2
     C*
     C                   MOVEL(P)  TITLE1        ORTITL
     C***  End   Add  ***  01/10/06  **************************************
      **** END   ADD **** 11/16/00 *********************************************
     **
     **   GET THE DISTRIBUTION INFORMATION & PRINT
     **
     C                   MOVE      '1'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*** BEG ADD ****** 09/07/04  **********************************
     C     ARCFLG        IFEQ      *ON
     C                   MOVEL(P)  ARTITL        ORTITL
     C                   ENDIF
     C*** END ADD ****** 09/07/04  **********************************
      *
      *  LEFT JUSTIFY REPORT TITLE
      *
     C     ' '           CHECK     ORTITL        X                 2 0
      *
     C     X             IFNE      0
     C                   SUBST(P)  ORTITL:X      XXTITL           60
     C                   ENDIF                                                  X IFNE 0
      *
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   ENDIF                                                  *IN60IF=*OFF
      ***** BEG ADD ******* 11/29/01 **********************************
     C*
     C***  Begin Add  ***  01/10/06  **************************************
     C                   ENDIF                                                  Group Exists
     C***  End   Add  ***  01/10/06  **************************************
      *
      *  Center Title in Field
      *
     C                   MOVEL(P)  ORTITL        ##DATA
     C                   CALL      'SRCENTER'                           99
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        ORTITL
      *
      ***** END ADD ******* 11/29/01 **********************************
     **
     C                   MOVE      *ON           *INOA
     C                   MOVE      'N'           XXFLAG            1
     C                   Z-ADD     *ZERO         Z
     **
     **   CALCULATE 30, 60, & 90 DAYS IN THE PAST ACCORDING TO DATE
     **   ENTERED
      *         *** 30 DAYS ***
     C***  Begin Chg  ***  04/23/09  **************************************
     C**********           Z-ADD30        XXDAYS  50
     C                   Z-ADD     31            XXDAYS            5 0
     C***  End   Chg  ***  04/23/09  **************************************
     C                   EXSR      SRPAST
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX30DT
     **
      *         *** 60 DAYS ***
     C***  Begin Chg  ***  04/23/09  **************************************
     C**********           Z-ADD60        XXDAYS  50
     C                   Z-ADD     61            XXDAYS            5 0
     C***  End   Chg  ***  04/23/09  **************************************
     C                   EXSR      SRPAST
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX60DT
     **
      *         *** 90 DAYS ***
     C***  Begin Chg  ***  04/23/09  **************************************
     C**********           Z-ADD90        XXDAYS  50
     C                   Z-ADD     91            XXDAYS            5 0
     C***  End   Chg  ***  04/23/09  **************************************
     C                   EXSR      SRPAST
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XX90DT
      *** BEG ADD ***** 12/18/08 *****************************************
      *         *** 120 DAYS ***
     C***  Begin Chg  ***  04/23/09  **************************************
     C**********           Z-ADD120       XXDAYS  50
     C                   Z-ADD     121           XXDAYS            5 0
     C***  End   Chg  ***  04/23/09  **************************************
     C                   EXSR      SRPAST
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X120DT
     **
      *         *** 150 DAYS ***
     C***  Begin Chg  ***  04/23/09  **************************************
     C**********           Z-ADD150       XXDAYS  50
     C                   Z-ADD     151           XXDAYS            5 0
     C***  End   Chg  ***  04/23/09  **************************************
     C                   EXSR      SRPAST
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X150DT
     **
      *         *** 180 DAYS ***
     C***  Begin Chg  ***  04/23/09  **************************************
     C**********           Z-ADD180       XXDAYS  50
     C                   Z-ADD     180           XXDAYS            5 0
     C***  End   Chg  ***  04/23/09  **************************************
     C                   EXSR      SRPAST
     C                   MOVE      SRDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X180DT
      *** END ADD ***** 12/18/08 *****************************************
      *** BEG ADD ***** 08/19/03 *****************************************
      *
      * Prevent amounts less than minimum (QQMIND or QQMINC) from printing.
      *
     C                   SELECT
     C     QQCRDB        WHENEQ    'D'
     C     XXMINC        ANDEQ     *ZERO
     C                   ADD       1             XXMINC
     C     QQCRDB        WHENEQ    'C'
     C     XXMIND        ANDEQ     *ZERO
     C                   ADD       1             XXMIND
     C                   ENDSL
      *** END ADD ***** 08/19/03 *****************************************
      *
     C***  Begin Add  ***  08/16/06  **************************************
     C***  Beg Chg ******  01/06/25  **************************************
     C***                SELECT
     C***  ARCFLG        WHENEQ    *ON
     C***                MOVE      *OFF          USEGSL
     C***                EXSR      GRPINF
     C***                OTHER
     C***                MOVE      *ON           USEGSL            1
     C***                EXSR      NXTGRP
     C***                ENDSL
     C                   MOVE      *ON           USEGSL            1
     C                   EXSR      NXTGRP
     C***  End Chg ******  01/06/25  **************************************
     C***  End   Add  ***  08/16/06  **************************************
     **
     **  MAIN LOOP BEGINS HERE
     **
     **   START READING THE LATE NOTICE WORK FILE
     **
     C     EBKEY         SETLL     PEB01
     C     EBKEY         READE     PEB01                                  50
     C*
     C***  Begin Add  ***  08/16/06  **************************************
     C     *IN50         IFEQ      *ON
     C                   EXSR      LDNOGP
     C                   EXSR      NEWGRP
     C                   ENDIF
     C***  End   Add  ***  08/16/06  **************************************
     **
     C     *IN50         DOWEQ     *OFF
     C*
     C***  BEGIN ADD  ***  12/17/99  *********************************
     C     EBACUR        IFNE      'Y'
     C***  END   ADD  ***  12/17/99  *********************************
      ***** BEG ADD ******* 11/29/01 **********************************
     C*  SET VALUE OF HOLD AGENT# TO CURRENT AGENT#
     C                   Z-ADD     EBAGT#        HLDAGT
      ***** END ADD ******* 11/29/01 **********************************
     **
     C                   MOVE      EBEMP#        XXEMP#
     C                   MOVE      EBDIV         XXDIV
     C                   MOVE      EBFND         XXFND
     C                   MOVE      EBFYR         XXFYR
      *
     **   LOAD POLICY NUMBER
     **
     C                   CALL      'WA022R'
     C                   PARM      *BLANKS       XXPLCY           15
     C                   PARM                    XXEMP#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     **
     **  VALIDATE RECORD TO EMPLOYER NAME & AGENT RANGE SELECTIONS
     **
     C                   MOVEL(P)  EBENAM        XXNM              2
     C                   MOVEL(P)  EBENAM        XXENM2           38
     **
      *   CHECK FOR PREVIOUS FUND YEAR
      *
     C                   SUB       1             XXFYR
     C     ELKEY         CHAIN     PEL01                              92
      *
     C                   ADD       1             XXFYR
     C**** Begin Del **** 08/22/06 *********************************************
     C*****BEGIN ADD **** 07/07/05 *********************************************
     C********** ELKEY     CHAINPEL01                81
     C**********           MOVELELASCD    EMASTS    P
     C*****END*ADD ****** 07/07/05 *********************************************
     C**** End   Del **** 08/22/06 *********************************************
      *
     **   CHAIN TO EMPLOYER MASTER FILE
     **
     C     EMKEY         CHAIN     PEM01                              80
     C**** BEGIN ADD **** 08/22/06 *********************************************
     C     ELKEY         CHAIN     PEL01                              81
     C                   MOVEL(P)  ELASCD        EMASTS
     C**** END ADD ****** 08/22/06 *********************************************
      **** BEGIN ADD **** 11/16/00 *********************************************
     C     *IN80         IFEQ      *OFF
     C     QQIEFL        ANDNE     *BLANK
      *
     C                   Z-ADD     1             Y
     C     EMASTS        LOOKUP    AS(Y)                                  60
      *
      *   CHECK IF CODES ARE 'EXCLUDE'
      *
     C     QQIEFL        IFEQ      'E'
      *
     C     EMASTS        IFEQ      *BLANK
     C                   MOVE      *ON           *IN60
     C                   ELSE
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  EMASTS IFEQ *BLANK
      *
     C                   ELSE
      *
     C     EMASTS        IFEQ      *BLANK
     C                   MOVE      *OFF          *IN60
     C                   ENDIF                                                  EMASTS IFEQ *BLANK
      *
     C                   ENDIF                                                  QQIEFL = 'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN80
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
     C                   ENDIF                                                  *IN80 IFEQ *OFF
      **** BEGIN ADD **** 11/16/00 *********************************************
     **
     **   CHAIN TO EMPLOYER DETAIL FILE
     **
     C**** BEGIN DEL **** 07/07/05 *********************************************
     C***********ELKEY**** CHAINPEL01                81
     C**** END DEL ****** 07/07/05 *********************************************
     **
     **   CHAIN TO THE FUND DETAIL FILE
     **
     C     F1KY          CHAIN     PF101                              82
      *
      *   MAKE SURE FILES GOT A HIT
      *
     C     *IN80         IFEQ      *OFF
     C     *IN81         ANDEQ     *OFF
     C     *IN82         ANDEQ     *OFF
      *
     C*** BEG ADD ***** 12/07/04 **************************************
     C                   MOVE      *OFF          ACHFLG            1
     C     X1BTYP        IFEQ      'A'
     C     X1BTYP        OREQ      'D'
     C*** BEG CHG ***** 07/27/10 ************************************
     C*
     C* CHECK IF NEW ACH MODULE IS INSTALLED ('WC1806')
     C*
      **** BEG   CHG **** 10/22/10 *********************************************
     C******     XXNACH    IFEQ *ON
      //*** Beg Change *** 04/06/21 *************************************
     C**     XXNACH        IFEQ      *OFF
      **** END   CHG **** 10/22/10 *********************************************
     C**     EMKEY         CHAIN     PEF01                              62
     C**     *IN62         DOWEQ     *OFF
     C**     SYSDT         IFLT      EFEXDT
     C**     SYSDT         ANDGE     EFEFDT
     C**     EFEXDT        OREQ      *ZERO
     C**     SYSDT         ANDGE     EFEFDT
     C**                   MOVE      *ON           ACHFLG                         EMPLOYER IS ACH
     C**                   LEAVE
     C**                   ENDIF
     C**     EMKEY         READE     PEF01                                  62
     C**                   ENDDO

     C**                   ELSE
     C*** END CHG ***** 07/27/10 ************************************
      *
     C**     DBKY1         SETGT     PDB01
     C**     EMKEY         READPE    PDB01                                  60
     C**     *IN60         IFEQ      *OFF
     C**     DBEXDT        ANDEQ     *ZERO
     C**     *IN60         OREQ      *OFF
     C**     SYSDT         ANDLT     DBEXDT
     C**                   MOVE      *ON           ACHFLG                         EMPLOYER IS ACH
     C**                   ENDIF                                                  *IN60 = *OFF
     C*** BEG CHG ***** 07/27/10 ************************************
     C**                   ENDIF
     C*** END CHG ***** 07/27/10 ************************************
       strCo# = %Editc(xxCo#:'X');
       strEftType = 'E';
       strFnd = %Editc(xxFnd:'X');
       strEftKey = %Editc(xxEmp#:'X') + %Editc(xxDiv:'X');
       strDate =%Editc(SysDt:'X');
       strFyr = %Editc(xxFyr:'X');
       ACHFLG = GetEftStatus(strCo#:strEftType:strfnd:strEftKey:
                             strDate:strPre:strRrn:strEobTyp:
                             strCkTp:strFyr);
      //*** End Change *** 04/06/21 *************************************
      *
     C                   ENDIF                                                  X1BTYP = 'A'
      *
     C     VHKY1         SETGT     PVH01
     C     VHKY          READPE    PVH01                                  60
     C     *IN60         IFEQ      *OFF
     C     VHEXDT        ANDEQ     *ZERO
     C     *IN60         OREQ      *OFF
     C     SYSDT         ANDLT     VHEXDT
     C                   MOVE      *ON           VDRFLG            1            EMPLOYER IS VDR
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   DO
      *
     C     X1BTYP        IFEQ      'P'
     C     VDRFLG        ANDEQ     *OFF
     C     X1BTYP        OREQ      'D'
     C     VDRFLG        ANDEQ     *ON
     C                   LEAVE                                                  DON'T PROCESS EMPLOY
     C                   ENDIF                                                  X1BTYP = 'P'
     C*** END ADD ***** 12/07/04 **************************************
      *
     **  IF FEDERAL NAME IS NOT BLANK THEN PRINT IT
     **
     C                   MOVEL(P)  EMFDNM        XXFDNM           38
     **
     C     XXFDNM        IFNE      *BLANKS
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  01/12/99  **********************************
     C* If EBCNDT = 0, check if there is a cancel date in the future
     C                   Z-ADD     *ZERO         CXDATE            7 0
     C     EBCNDT        IFEQ      *ZERO
     C***  BEGIN ADD  ***  08/05/99  *********************************
     C     EBCNDT        ORGT      XXBLDT
     C***  END   ADD  ***  08/05/99  *********************************
     C*
     C*  Find policy effective date
     C                   Z-ADD     1             XI                1 0
     C     *ZERO         LOOKUP    EF(XI)                                 70
     C*
     C     EF(XI)        IFNE      *ZERO
     C                   Z-ADD     5             XI
     C                   ELSE
     C                   SUB       1             XI
     C                   ENDIF
     C*
     C     XI            DOWGT     *ZERO
     C     EF(XI)        IFGT      EBPFDT
     C                   SUB       1             XI
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C*  Determine if cancellation date is in the future
     C     XI            IFNE      *ZERO
     C     CX(XI)        ANDNE     *ZERO
     C                   Z-ADD     CX(XI)        CXDATE
     C                   ENDIF                                                  XI<>*ZERO
     C*
     C                   ENDIF                                                  EBCNDT=*ZERO
     C***  END   ADD  ***  01/12/99  **********************************
     **
     **  SETUP THE ACCOUNT STATUS INDICATORS: 72 = ACTIVE
     **                                       73 = CANCEL PEND
     **                                       74 = CANCELLED
     C                   MOVE      *OFF          *IN72
     C                   MOVE      *OFF          *IN73
     C                   MOVE      *OFF          *IN74
     C***  BEGIN ADD  ***  08/05/99  *********************************
     C                   CLEAR                   XXCXDT
     C***  END   ADD  ***  08/05/99  *********************************
     **
     C     EBCNDT        IFEQ      *ZERO                                        ACTIVE
     C***  BEGIN ADD  ***  01/12/99  **********************************
     C     CXDATE        ANDEQ     *ZERO
     C***  END   ADD  ***  01/12/99  **********************************
     C                   MOVE      *ON           *IN72
     C                   ENDIF
     **
     C***  BEGIN CHG  ***  01/12/99  **********************************
     C********** EBCNDT    IFGT XXBLDT                     CX PENDING
     C     CXDATE        IFNE      *ZERO                                        CX PENDING
     C***  END   CHG  ***  01/12/99  **********************************
     **
     C                   MOVE      *ON           *IN73
     **
     C***  BEGIN CHG  ***  01/12/99  **********************************
     C*******************  Z-ADDEBCNDT    SRDTI
     C                   Z-ADD     CXDATE        SRDTI
     C***  END   CHG  ***  01/12/99  **********************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT            8 0
     **
     C                   ENDIF                                                  CXDATE<>*ZERO
     **
     C     EBCNDT        IFNE      *ZERO                                        CANCELLED
     C     EBCNDT        ANDLE     XXBLDT
     **
     C                   Z-ADD     EBCNDT        SRDTI
     **
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT
     C                   MOVE      *ON           *IN74
     **
     C                   ENDIF                                                  EBCNDT<>*ZERO
     C*
     C***  BEGIN ADD  ***  01/12/99  **********************************
     C*  CONVERT POL EFF DT FOR OUTPUT
     C                   Z-ADD     EBPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C***  END   ADD  ***  01/12/99  **********************************
     **
     C                   Z-ADD     *ZERO         XXAMTD            9 2
     C                   Z-ADD     *ZERO         XXAUDT
     C                   Z-ADD     *ZERO         XXPAY             9 2
     C***  BEGIN ADD  ***  10/31/01  **********************************
     C                   Z-ADD     0             XXDEFR            9 2          deferred due
     C***  END   ADD  ***  10/31/01  **********************************
     **
     C                   MOVEL     EBENAM        XXENAM           28
     C                   Z-ADD     *ZERO         XXCURR            9 2
     C                   Z-ADD     *ZERO         XX30DY            9 2
     C                   Z-ADD     *ZERO         XX60DY            9 2
     C                   Z-ADD     *ZERO         XX90DY            9 2
     C                   Z-ADD     *ZERO         XP90DY            9 2
      *** BEG ADD ***** 12/18/08 *****************************************
     C                   Z-ADD     *ZERO         X120DY            9 2
     C                   Z-ADD     *ZERO         X150DY            9 2
     C                   Z-ADD     *ZERO         X180DY            9 2
      *** END ADD ***** 12/18/08 *****************************************
     C                   Z-ADD     *ZERO         XXABAL            9 2
     **
     **  FIND THE LAST AUDIT DATE FOR THE FUND YEAR
     **
     C***  BEGIN ADD  ***  01/12/99  **********************************
     C*    Only get audit date if ELFAF = 'A'
     C     ELFAF         IFEQ      'A'
     C***  END   ADD  ***  01/12/99  **********************************
     C*
     C                   MOVEL     'D'           XXARTY
     **
      *** BEG CHG ***** 05/02/14 **************************************
     C**********     EAKY2         SETGT     PEA03
     C**********     EAKY3         READPE    PEA03                                  60
     **
     C**********     *IN60         IFEQ      *OFF
     C**********                   Z-ADD     EAPODT        XXAUDT
     C**********                   ENDIF
      /Free
       Setll EAKY2 WMEAL10;
       Reade EAKY2 WMEAL10;
       If Not %Eof and
           EAtrdt <= xxBLDT;
          Eval xxAUDT = EAtrdt;
       Endif;
      /End-Free
      *** END CHG ***** 05/02/14 **************************************
     C*
     C***  BEGIN ADD  ***  01/12/99  **********************************
     C                   ENDIF                                                  ELFAF='A'
     C***  END   ADD  ***  01/12/99  **********************************
     **
     **  IF AUDIT DATE IS GREATER THAN BALANCE DATE, DON'T TREAT
     **     THIS AS AN AUDITED ACCOUNT
     **
     C     XXAUDT        IFGT      XXBLDT
     C***  BEGIN ADD  ***  08/23/99  **********************************
     C     ELFAF         ORNE      'A'
     C***  END   ADD  ***  08/23/99  **********************************
     C                   Z-ADD     *ZERO         XXAUDT
     C                   ENDIF
     **
     **  SET AUDIT FLAG
     **
     C     XXAUDT        IFGT      *ZERO
     C                   MOVE      'A'           ADFLG             1
     C                   MOVE      *ON           *IN71
     C                   ELSE
     C                   MOVE      'N'           ADFLG
     C                   MOVE      *OFF          *IN71
     C                   ENDIF
      *
     **
     **   VALIDATE TO SELECTED AUDIT STATUS
     **
     C     QQAUST        IFEQ      'B'
     C     QQAUST        OREQ      ADFLG
     **
     **  USE CANCEL DATE IF IT LESS THAN THE BALANCE DATE AND THE
     **  EMPLOYER HAS NOT BEEN AUDITTED
     **
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXABDT
     **
     C                   MOVE      *OFF          *IN95
     C*
     C     ADFLG         IFNE      'A'
     C     EBCNDT        ANDNE     *ZERO
     C     EBCNDT        ANDLT     XXABDT
     **
     C*** BEG ADD ***** 09/07/04 **************************************
     C     ARCFLG        IFEQ      *OFF
     C*** END ADD ***** 09/07/04 **************************************
     C                   Z-ADD     EBCNDT        XXABDT
     C*** BEG ADD ***** 09/07/04 **************************************
     C                   ENDIF
     C*** END ADD ***** 09/07/04 **************************************
     **
     C                   MOVE      *ON           *IN95                          CANCELLED
     C                   ENDIF
     **
     **   START READING THE EMPLOYER ACCOUNTS RECEIVABLE FILE
     **
      ***  BEGIN ADD  ***  10/19/05  **********************************
     C                   Z-ADD     *ZEROS        XXLBDT
     C     EAKEY         SETLL     PEA01
     C     EAKEY         READE     PEA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
     C     EAARTY        OREQ      'D'
     C     EAAMT         IFGT      *ZEROS
     C     EAPODT        IFGT      XXLBDT
     C     EAPODT        ANDLE     XXBLDT
     C                   Z-ADD     EAPODT        XXLBDT
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      ***  END ADD ******  10/19/05  **********************************
      *
      *** BEG ADD ***** 12/17/04 ***********************************************
     C     XXSLAR        IFEQ      'Y'
     C                   Z-ADD     EAPODT        XXSLDT            7 0          SELECT DATE
     C                   ELSE
     C                   Z-ADD     EATRDT        XXSLDT
     C                   ENDIF
      *** END ADD ***** 12/17/04 ***********************************************
      *** BEG ADD ***** 12/07/04 ***********************************************
      *
      *   IF BILLING TYPE = 'A'CH, SKIP THE TRANSACTION IF EMPLOYER ISN'T ACH.
      *   IF BILLING TYPE = 'D'IRECT BILL, SKIP TRANSACTION IF EMPLOYER IS ACH.
      *
     C                   DO
      *
     C     X1BTYP        IFEQ      'A'
     C     ACHFLG        ANDEQ     *OFF
     C     X1BTYP        OREQ      'D'
     C     ACHFLG        ANDEQ     *ON
     C                   LEAVE                                                  SKIP TRANSACTION
     C                   ENDIF
      *** END ADD ***** 12/07/04 ***********************************************
     **
     **   INIT VARIABLES
     **
     C                   Z-ADD     0             XXBILL            9 2
     C                   Z-ADD     0             XXCRED            9 2
     C*** BEG ADD ****** 01/15/09 **********************************************
     C* RETRIEVE A/R TYPE CODE IN BEGINNING AS IT IS USED IN VARIOUS CONDTIONS
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*** END ADD ****** 01/15/09 **********************************************
     **
     **   A/R CODE MUST BE ONE OF THE CODES ENTERED UNLESS
     **    CODES WERE LEFT BLANK THEN ALL CODES WILL BE USED
     **
     C     QQARIE        IFNE      *BLANK
     C                   Z-ADD     1             Y
     C     EAARCD        LOOKUP    COD(Y)                                 60
      *
      *   CHECK IF CODES ARE 'EXCLUDE'
      *
     C     QQARIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
      *** BEG ADD ***** 01/07/05 **********************************
     C                   MOVE      *ON           *IN61
      *** END ADD ***** 01/07/05 **********************************
     C                   ELSE
     C                   MOVE      *OFF          *IN60
      *** BEG ADD ***** 01/07/05 **********************************
     C                   MOVE      *OFF          *IN61
      *** END ADD ***** 01/07/05 **********************************
     C                   ENDIF
      *
     C                   ENDIF                                                  QQARIE = 'E'
      *
     C                   ENDIF                                                  QQARIE NE *BLANK
      **** BEGIN ADD **** 05/15/02 *********************************************
     **
     **   A/R CODE MUST BE ONE OF THE CATEGORIES ENTERED UNLESS
     **    CATEGORIES WERE LEFT BLANK THEN ALL CATEGORIES WILL BE USED
     **
     C     QQCTIE        IFNE      *BLANK
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             Y
     C     TTCTYP        LOOKUP    CT(Y)                                  60
      *
      *   CHECK IF CATEGORIES ARE 'EXCLUDE'
      *
     C     QQCTIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDIF                                                  QQCTIE = 'E'
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  QQCTIE NE *BLANK
      **** END ADD ****** 05/15/02 *********************************************
      *** BEG ADD ***** 01/07/05 **********************************
      *
      * OVERRIDE THE CATEGORY FLAG IF THE A/R CODE WAS PREVIOUSLY EXCLUDED.
      *
     C     *IN60         IFEQ      *ON
     C     *IN61         ANDEQ     *OFF
     C     QQARIE        ANDEQ     'E'
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *** END ADD ***** 01/07/05 **********************************
     **
     C     *IN60         IFEQ      *ON
     C     EAARCD        ANDNE     *BLANK
     C     QQARIE        OREQ      *BLANK
      **** BEGIN ADD **** 05/15/02 *********************************************
     C     QQCTIE        ANDEQ     *BLANK
      **** END ADD ****** 05/15/02 *********************************************
     **
     **   INCLUDE 'A' & 'B' AR TYPES UP TO CANX DATE OR BILLING THROUGH
     **   DATE, WHICHEVER COMES FIRST. INCLUDE ALL OTHERS UP TO BILLING
     **   THRU DATE EXCEPT INCLUDE PAYMENT TRANS THRU PAYMENT THRU DATE
      *
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'A'
      *
     C     EAPODT        IFLE      XXABDT
     C     *IN95         ANDEQ     *OFF                                         NOT CANCELLE
     C     EAPODT        ORLT      XXABDT
     C     *IN95         ANDEQ     *ON                                          CANCELLED
     C                   Z-ADD     EAAMT         XXBILL
     C                   ADD       EAAMT         XXAMTD
     C***  BEGIN ADD  ***  10/31/01  **********************************
     C                   ELSE
     C     *IN95         IFEQ      *OFF
      **** BEGIN ADD **** 05/02/02 *********************************************
     C     EBPFDT        ANDLE     XXBLDT
      **** END   ADD **** 05/02/02 *********************************************
      *** BEG ADD ***** 09/07/04 ***********************************************
     C     ARCFLG        OREQ      *ON
      *** END ADD ***** 09/07/04 ***********************************************
     C                   ADD       EAAMT         XXDEFR
     C                   ENDIF
     C***  END   ADD  ***  10/31/01  **********************************
     C                   ENDIF                                                  EAPODT LE XXABDT
      *
     C                   ELSE                                                   EAARTY = 'B'
     C*
     C***  BEGIN ADD  ***  07/02/99  **********************************
     C*   Calculate tax billed the same as 'A' and 'B' a/r types and calc
     C*   the other fees like the others
     C     EAARTY        IFEQ      'F'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     TTCTYP        IFEQ      'T'                                          tax billed type
      ***** BEG ADD ******* 11/30/01 **********************************
     C     TTCTYP        OREQ      'S'
     C     TTCTYP        OREQ      '1'
     C     TTCTYP        OREQ      '2'
     C     TTCTYP        OREQ      '3'
      *** BEG ADD ***** 10/06/10 **************************************
     C     TTCTYP        OREQ      '4'
     C     TTCTYP        OREQ      '5'
     C     TTCTYP        OREQ      '6'
     C     TTCTYP        OREQ      '7'
      *** END ADD ***** 10/06/10 **************************************
     C     TTCTYP        OREQ      'F'
     C     TTCTYP        OREQ      'D'
      ***** END ADD ******* 11/30/01 **********************************
     C*
     C     EAPODT        IFLE      XXABDT
     C     *IN95         ANDEQ     *OFF
     C     EAPODT        ORLT      XXABDT
     C     *IN95         ANDEQ     *ON
     C                   Z-ADD     EAAMT         XXBILL
     C                   ADD       EAAMT         XXAMTD
     C***  BEGIN ADD  ***  10/31/01  **********************************
     C                   ELSE
     C     *IN95         IFEQ      *OFF
      **** BEGIN ADD **** 05/02/02 *********************************************
     C     EBPFDT        ANDLE     XXBLDT
      **** END   ADD **** 05/02/02 *********************************************
      *** BEG ADD ***** 09/07/04 ***********************************************
     C     ARCFLG        OREQ      *ON
      *** END ADD ***** 09/07/04 ***********************************************
     C                   ADD       EAAMT         XXDEFR
     C                   ENDIF
     C***  END   ADD  ***  10/31/01  **********************************
     C                   ENDIF                                                  EAPODT<=XXABDT
     C*
     C                   ELSE                                                   TTCTYP = 'T'
     C*
     C     EAPODT        IFLE      XXBLDT
     C                   Z-ADD     EAAMT         XXBILL
     C                   ADD       EAAMT         XXAMTD
     C***  BEGIN ADD  ***  10/31/01  **********************************
     C                   ELSE
      **** BEGIN ADD **** 05/02/02 *********************************************
     C     EBPFDT        IFLE      XXBLDT
      **** END   ADD **** 05/02/02 *********************************************
      *** BEG ADD ***** 09/07/04 ***********************************************
     C     ARCFLG        OREQ      *ON
      *** END ADD ***** 09/07/04 ***********************************************
     C                   ADD       EAAMT         XXDEFR
      **** BEGIN ADD **** 05/02/02 *********************************************
     C                   ENDIF                                                  EBPFDT IFLE XXBLDT
      **** END   ADD **** 05/02/02 *********************************************
     C***  END   ADD  ***  10/31/01  **********************************
     C                   ENDIF                                                  EAPODT<=XXBLDT
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C***  END   ADD  ***  07/02/99  **********************************
      *
     C     EAARTY        IFEQ      'D'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'R'
     C***  BEGIN DEL  ***  07/02/99  **********************************
     C********** EAARTY    OREQ 'F'
     C***  END   DEL  ***  07/02/99  **********************************
     C***  BEG ADD ******  07/18/07  **********************************
     C     TTCTYP        ANDNE     'D'
     C***  END ADD ******  07/18/07  **********************************
      *
     C     EAPODT        IFLE      XXBLDT
      *** BEG ADD ***** 09/07/04 ***********************************************
     C     ARCFLG        OREQ      *ON
     C     EAARTY        ANDNE     'D'
      *** END ADD ***** 09/07/04 ***********************************************
     C                   Z-ADD     EAAMT         XXBILL
     C                   ADD       EAAMT         XXAMTD
     C***  BEGIN ADD  ***  10/31/01  **********************************
     C                   ELSE
      *
     C     EAARTY        IFEQ      'D'
      **** BEGIN ADD **** 05/02/02 *********************************************
     C     EBPFDT        IFLE      XXBLDT
      **** END   ADD **** 05/02/02 *********************************************
      *** BEG ADD ***** 09/07/04 ***********************************************
     C     ARCFLG        OREQ      *ON
      *** END ADD ***** 09/07/04 ***********************************************
     C                   ADD       EAAMT         XXDEFR
      **** BEGIN ADD **** 05/02/02 *********************************************
     C                   ENDIF                                                  EBPFDT IFLE XXBLDT
      **** END   ADD **** 05/02/02 *********************************************
     C                   ENDIF
     C***  END   ADD  ***  10/31/01  **********************************
      *
     C                   ENDIF                                                  EAPODT IFLE XXBLDT
     **
     C                   ELSE                                                   EAARTY = 'D'
     **
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'Q'
     C***  BEG ADD ******  02/19/04  **********************************
     C     EAARTY        OREQ      'T'
     C***  END ADD ******  02/19/04  **********************************
     C***  BEG ADD ******  07/18/07  **********************************
     C     TTCTYP        ANDNE     'D'
     C***  END ADD ******  07/18/07  **********************************
     **
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    IFLE XXBLDT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    IFLE XXBLDT ********************************
     C     XXSLDT        IFLE      XXBLDT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
      *** BEG ADD ***** 09/07/04 ***********************************************
     C     ARCFLG        OREQ      *ON
      *** END ADD ***** 09/07/04 ***********************************************
     C                   Z-ADD     EAAMT         XXCRED
     C                   SUB       EAAMT         XXAMTD
     C                   ENDIF                                                  EAPODT IFLE XXBLDT
     **
     C                   ENDIF                                                  EAARTY = 'P'
     **
     C                   ENDIF                                                  EAARTY = 'D'
     C*
     C***  BEGIN ADD  ***  07/02/99  **********************************
     C                   ENDIF                                                  EAARTY = 'F'
     C***  END   ADD  ***  07/02/99  **********************************
     **
     C                   ENDIF                                                  EAARTY = 'B'
     **
     **  ADD DEDUCTIBLE BILLINGS AFTER CANCEL DATE AND BEFORE RUN DATE
     **
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
      *** BEG ADD ***** 09/07/04 ***********************************************
     C     ARCFLG        ANDEQ     *OFF
      *** END ADD ***** 09/07/04 ***********************************************
      *
     C     EAARTY        IFEQ      'B'                                          BILLING TYPE
     C     TTCTYP        ANDEQ     'R'                                          DED BILLING
     C     EAPODT        ANDGT     XXABDT                                       POST DT < RUNDT
     C     EAPODT        ANDLE     XXBLDT                                       POST DT < RUNDT
     **
     C                   Z-ADD     EAAMT         XXBILL
     C                   ADD       EAAMT         XXAMTD
     **
     C                   ENDIF                                                  EAARTY = 'B'
     **
     C***  BEGIN ADD  ***  10/31/01  **********************************
     C*    Include deductible billings after billing through date if
     C*    not already included (if account is cancelled) for deferred amt
     C     *IN95         IFEQ      *ON
     C     EAARTY        ANDEQ     'B'                                          BILLING TYPE
     C     TTCTYP        ANDEQ     'R'                                          DED BILLING
     C     EAPODT        ANDGT     XXBLDT
      **** BEGIN ADD **** 05/02/02 *********************************************
     C     EBPFDT        ANDLE     XXBLDT
      **** END   ADD **** 05/02/02 *********************************************
     C                   ADD       EAAMT         XXDEFR
     C                   ENDIF                                                  EAARTY = 'B'
     C***  END   ADD  ***  10/31/01  **********************************
     C                   ENDIF                                                  *IN60 IFEQ *OFF
     **
     C                   ENDIF                                                  *IN60 EQ *ON
      *
     C     EAPODT        IFLE      XXAUDT
     C                   ADD       XXBILL        XXABAL
     C                   SUB       XXCRED        XXABAL
      *
     C                   ELSE
      *
     C                   SELECT
      *** BEG ADD ***** 12/18/08 *****************************************
     C     EAPODT        WHENLE    X180DT
     C                   ADD       XXBILL        X180DY
     C     EAPODT        WHENLE    X150DT
     C                   ADD       XXBILL        X150DY
     C     EAPODT        WHENLE    X120DT
     C                   ADD       XXBILL        X120DY
      *** END ADD ***** 12/18/08 *****************************************
     C     EAPODT        WHENLE    XX90DT
     C                   ADD       XXBILL        XX90DY
     C     EAPODT        WHENLE    XX60DT
     C                   ADD       XXBILL        XX60DY
     C     EAPODT        WHENLE    XX30DT
     C                   ADD       XXBILL        XX30DY
     C                   OTHER
     C                   ADD       XXBILL        XXCURR
     C                   ENDSL
      *
     C                   ADD       XXCRED        XXPAY
      *
     C                   ENDIF                                                  EAPODT >= XXAUDT
      *
      *** BEG ADD ***** 12/07/04 ***********************************************
     C                   ENDDO                                                  DO BEFORE BTYP=A
      *** END ADD ***** 12/07/04 ***********************************************
      *
     C     EAKEY         READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 EQ *OFF
     C*** BEG ADD *****  08/09/06  ************************************
      *
      *  TURN ON THE DEPOSIT MODE BY YEAR FLAG (XXDPYM) IF THE POLICY YEAR
      *  IS DEFINED AS SUCH IN WDF1P
      *
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      'Y'           XXDPYM            1            CUR YR DEP MODE
     C                   ELSE
     C                   MOVE      'N'           XXDPYM            1
     C                   ENDIF
     C*** END ADD *****  08/09/06  ************************************
     **
     **  ACCUMULATE H & I TRANSACTIONS
      *    CHECK ALL FUND YEARS
     **
     C                   MOVE      'H'           XXARTY
      *
      *** BEG ADD ***** 09/07/04 ***********************************************
     C     ARCFLG        IFEQ      *OFF
      *** END ADD ***** 09/07/04 ***********************************************
     C     EAKY1         SETLL     PEA02
     C     EAKY5         READE     PEA02                                  60
      *** BEG ADD ***** 09/07/04 ***********************************************
     C                   ELSE
     C     EAKY3         SETLL     PEA03
     C     EAKY4         READE     PEA03                                  60
     C                   ENDIF
      *** END ADD ***** 09/07/04 ***********************************************
      *
     C     *IN60         DOWEQ     *OFF
      *** BEG CHG ***** 07/18/07 ******************************************
     C********** EAARTY    ANDLE'I'
     C     EAARTY        ANDLE     'T'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'D'
      *** END CHG ***** 07/18/07 ******************************************
      *
      *** BEG ADD ***** 12/17/04 ***********************************************
     C     XXSLAR        IFEQ      'Y'
     C                   Z-ADD     EAPODT        XXSLDT            7 0          SELECT DATE
     C                   ELSE
     C                   Z-ADD     EATRDT        XXSLDT
     C                   ENDIF
      *** END ADD ***** 12/17/04 ***********************************************
      *** BEG ADD ***** 12/07/04 ***********************************************
      *
      *   IF BILLING TYPE = 'A'CH, SKIP THE TRANSACTION IF EMPLOYER ISN'T ACH.
      *   IF BILLING TYPE = 'D'IRECT BILL, SKIP TRANSACTION IF EMPLOYER IS ACH.
      *
     C                   DO
      *
     C     X1BTYP        IFEQ      'A'
     C     ACHFLG        ANDEQ     *OFF
     C     X1BTYP        OREQ      'D'
     C     ACHFLG        ANDEQ     *ON
     C                   LEAVE                                                  SKIP TRANSACTION
     C                   ENDIF
      *** END ADD ***** 12/07/04 ***********************************************
     **
     **   INIT VARIABLES
     **
     C                   Z-ADD     0             XXBILL
     C                   Z-ADD     0             XXCRED
     **
     **   A/R CODE MUST BE ONE OF THE CODES ENTERED UNLESS
     **    CODES WERE LEFT BLANK THEN ALL CODES WILL BE USED
     **
     C     QQARIE        IFNE      *BLANK
     C                   Z-ADD     1             Y
     C     EAARCD        LOOKUP    COD(Y)                                 60
      *
      *   CHECK IF CODES ARE 'EXCLUDE'
      *
     C     QQARIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
      *** BEG ADD ***** 01/07/05 **********************************
     C                   MOVE      *ON           *IN61
      *** END ADD ***** 01/07/05 **********************************
     C                   ELSE
     C                   MOVE      *OFF          *IN60
      *** BEG ADD ***** 01/07/05 **********************************
     C                   MOVE      *OFF          *IN61
      *** END ADD ***** 01/07/05 **********************************
     C                   ENDIF
      *
     C                   ENDIF                                                  QQARIE = 'E'
      *
     C                   ENDIF                                                  QQARIE IFNE *BLANK
      **** BEGIN ADD **** 05/15/02 *********************************************
     **
     **   A/R CODE MUST BE ONE OF THE CATEGORIES ENTERED UNLESS
     **    CATEGORIES WERE LEFT BLANK THEN ALL CATEGORIES WILL BE USED
     **
     C     QQCTIE        IFNE      *BLANK
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             Y
     C     TTCTYP        LOOKUP    CT(Y)                                  60
      *
      *   CHECK IF CATEGORIES ARE 'EXCLUDE'
      *
     C     QQCTIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDIF                                                  QQCTIE = 'E'
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  QQCTIE NE *BLANK
      **** END ADD ****** 05/15/02 *********************************************
      *** BEG ADD ***** 01/07/05 **********************************
      *
      * OVERRIDE THE CATEGORY FLAG IF THE A/R CODE WAS PREVIOUSLY EXCLUDED.
      *
     C     *IN60         IFEQ      *ON
     C     *IN61         ANDEQ     *OFF
     C     QQARIE        ANDEQ     'E'
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *** END ADD ***** 01/07/05 **********************************
     **
     C     *IN60         IFEQ      *ON
     C     EAARCD        ANDNE     *BLANK
     C     QQARIE        OREQ      *BLANK
      **** BEGIN ADD **** 05/15/02 *********************************************
     C     QQCTIE        ANDEQ     *BLANK
      **** END ADD ****** 05/15/02 *********************************************
     **
      *** BEG DEL ***** 08/09/06 **********************************
     C********** EAFYR     IFLE EBFYR
      *** END DEL ***** 08/09/06 **********************************
     **
      *** BEG ADD ***** 08/09/06 **********************************
     C     EAFYR         IFLT      EBFYR
     C     XXDPYM        ANDEQ     'N'
     C     EAFYR         OREQ      EBFYR
      *
     C                   MOVE      'N'           XXPYDM            1
     C     EAFYR         IFLT      EBFYR
     C     F1KEY2        SETLL     PF101
     C     F1KEY2        READE     PF101                                  60
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      'Y'           XXPYDM            1            PRIOR YEAR DEP MODE
     C                   ENDIF
     C                   ENDIF                                                                 R
      *
     C     XXPYDM        IFEQ      'N'
      *** END ADD ***** 08/09/06 **********************************
      *
      ***** BEGIN ADD ***** 09/17/02 **********************************
     C     EAARTY        IFEQ      'H'
     C                   Z-ADD     EAPODT        XXDAT1
     C                   Z-ADD     XXABDT        XXDAT2
     C                   ELSE
      *** BEG CHG ***** 12/17/04 **************************************
     C*****************    Z-ADDEATRDT    XXDAT1
     C                   Z-ADD     XXSLDT        XXDAT1
      *** END CHG ***** 12/17/04 **************************************
     C                   Z-ADD     XXBLDT        XXDAT2
     C                   ENDIF
      ***** END ADD ******* 09/17/02 **********************************
     **
     **   INCLUDE 'H' TRANS THRU CANX DATE OR BILLING THROUGH DATE,
     **   WHICHEVER COMES FIRST. INCLUDE 'I' TRANS THRU PAYMENT THRU DT
     **
     C     EAARTY        IFEQ      'H'
      ***** BEGIN ADD ***** 11/13/01 **********************************
     C     EAAMT         ANDGT     *ZERO
      ***** END ADD ******* 11/13/01 **********************************
      *** BEG ADD ***** 07/18/07 **********************************
     C     EAARTY        OREQ      'R'
      *** END ADD ***** 07/18/07 **********************************
     **
      ***** BEGIN CHG ***** 09/17/02 **********************************
     C***********EAPODT****IFLE XXABDT
     C     XXDAT1        IFLE      XXDAT2
     C     *IN95         ANDEQ     *OFF                                         NOT CANCELLE
     C***********EAPODT****ORLT XXABDT
     C     XXDAT1        ORLT      XXDAT2
      ***** END CHG ******* 09/17/02 **********************************
     C     *IN95         ANDEQ     *ON                                          CANCELLED
     C                   Z-ADD     EAAMT         XXBILL
     C                   ADD       EAAMT         XXAMTD
     C***  BEGIN ADD  ***  10/31/01  **********************************
     C                   ELSE
      **** BEGIN ADD **** 05/02/02 *********************************************
     C     EBPFDT        IFLE      XXBLDT
      **** END   ADD **** 05/02/02 *********************************************
     C                   ADD       EAAMT         XXDEFR
      **** BEGIN ADD **** 05/02/02 *********************************************
     C                   ENDIF                                                  EBPFDT IFLE XXBLDT
      **** END   ADD **** 05/02/02 *********************************************
     C***  END   ADD  ***  10/31/01  **********************************
     C                   ENDIF                                                  EAPODT IFLE XXABDT
      *
     C                   ENDIF                                                  EAARTY = 'H'
     **
     C     EAARTY        IFEQ      'I'
      ***** BEGIN ADD ***** 11/13/01 **********************************
     C     EAARTY        OREQ      'H'
     C     EAAMT         ANDLT     *ZERO
      ***** END ADD ******* 11/13/01 **********************************
      *** BEG ADD ***** 07/18/07 **********************************
     C     EAARTY        OREQ      'T'
      *** END ADD ***** 07/18/07 **********************************
      *
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    IFLE XXBLDT
      ***** BEGIN CHG ***** 09/17/02 **********************************
     C***********EATRDT****IFLE XXBLDT
     C     XXDAT1        IFLE      XXDAT2
      ***** END CHG ******* 09/17/02 **********************************
      ***** END CHG ******* 08/23/01 **********************************
      ***** BEGIN ADD ***** 11/13/01 **********************************
     C     EAAMT         IFLT      *ZERO
     C     EAARTY        ANDEQ     'H'
     C                   MULT      -1            EAAMT
     C                   END
      ***** END ADD ******* 11/13/01 **********************************
     C                   Z-ADD     EAAMT         XXCRED
     C                   SUB       EAAMT         XXAMTD
     C                   ENDIF                                                  EATRDT IFLE XXBLDT
      *
     C                   ENDIF                                                  EAARTY = 'I'
     **
     C*** BEG ADD ***** 08/09/06 ******************************************
     C                   ENDIF                                                  XXPYDM = 'N'
     C*** END ADD ***** 08/09/06 ******************************************
     **
     C                   ENDIF                                                  EAFYR IFLE EBFYR
     **
     C                   ENDIF                                                  *IN60 = *ON
      *
     C     EAPODT        IFLE      XXAUDT
     C                   ADD       XXBILL        XXABAL
     C                   SUB       XXCRED        XXABAL
      *
     C                   ELSE
      *
     C                   SELECT
      *** BEG ADD ***** 12/18/08 *****************************************
     C     EAPODT        WHENLE    X180DT
     C                   ADD       XXBILL        X180DY
     C     EAPODT        WHENLE    X150DT
     C                   ADD       XXBILL        X150DY
     C     EAPODT        WHENLE    X120DT
     C                   ADD       XXBILL        X120DY
      *** END ADD ***** 12/18/08 *****************************************
     C     EAPODT        WHENLE    XX90DT
     C                   ADD       XXBILL        XX90DY
     C     EAPODT        WHENLE    XX60DT
     C                   ADD       XXBILL        XX60DY
     C     EAPODT        WHENLE    XX30DT
     C                   ADD       XXBILL        XX30DY
     C                   OTHER
     C                   ADD       XXBILL        XXCURR
     C                   ENDSL
      *
     C                   ADD       XXCRED        XXPAY
      *
     C                   ENDIF                                                  EAPODT >= XXAUDT
      *
      *** BEG ADD ***** 12/07/04 ***********************************************
     C                   ENDDO                                                  DO BEFORE BTYP=A
      *** END ADD ***** 12/07/04 ***********************************************
     C*** BEG ADD ***** 07/18/07 ******************************************
     C                   ENDIF                                                  TTCTYP = 'D'
     C*** END ADD ***** 07/18/07 ******************************************
      *
      *** BEG ADD ***** 09/07/04 ************************************
     C     ARCFLG        IFEQ      *OFF
      *** END ADD ***** 09/07/04 ************************************
     C     EAKY5         READE     PEA02                                  60
      *** BEG ADD ***** 09/07/04 ************************************
     C                   ELSE
     C     EAKY4         READE     PEA03                                  60
     C                   ENDIF
      *** END ADD ***** 09/07/04 ************************************
      *
     C                   ENDDO                                                  *IN60 = *OFF
     C*** BEG ADD *****  08/09/06  ************************************
     C*
     C* RESET GROUP DETAIL
     C*
     C     F1KY          SETLL     PF101
     C     F1KY          READE     PF101                                  60
     C*** END ADD *****  08/09/06  ************************************
      *** BEG ADD ***** 05/13/04 ************************************
      *
      *  Add amounts from WMXAP to the deferred amount for the
      *  Clarendon Preprocessor for non-audited accounts with
      *  canx date in future. If the Aging is called from the A/R Detail,
      *  an empty WMXAP/L is copied to QTEMP to prevent this code from affecting
      *  the totals.
      *
     C     *IN73         IFEQ      *ON
     C     ADFLG         ANDEQ     'N'
     C     EAKEY         SETLL     PXA01
     C     EAKEY         READE     PXA01                                  60
     C     *IN60         DOWEQ     *OFF
      *
     C     XAPODT        IFGT      XXBLDT
     C                   ADD       XAAMT         XXDEFR
     C                   ENDIF
      *
     C     EAKEY         READE     PXA01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  *IN73
      *** END ADD ***** 05/13/04 ************************************
      *
     C     XXABAL        IFNE      *ZERO
      *
     C                   SELECT
      *** BEG ADD ***** 12/18/08 ************************************
     C     XXAUDT        WHENLE    X180DT
     C                   ADD       XXABAL        X180DY
     C     XXAUDT        WHENLE    X150DT
     C                   ADD       XXABAL        X150DY
     C     XXAUDT        WHENLE    X120DT
     C                   ADD       XXABAL        X120DY
      *** END ADD ***** 12/18/08 ************************************
     C     XXAUDT        WHENLE    XX90DT
     C                   ADD       XXABAL        XX90DY
     C     XXAUDT        WHENLE    XX60DT
     C                   ADD       XXABAL        XX60DY
     C     XXAUDT        WHENLE    XX30DT
     C                   ADD       XXABAL        XX30DY
     C                   OTHER
     C                   ADD       XXABAL        XXCURR
     C                   ENDSL
      *
     C                   ENDIF                                                  XXABAL IFNE *ZERO
      *
     **
     **   THIS IS THE CREDIT/DEBIT/BOTH FLAG FOR AGING REPORT
     **
     C     QQCRDB        IFEQ      'B'
     C     QQCRDB        OREQ      'C'
     C     XXAMTD        ANDLT     *ZERO
     C     QQCRDB        OREQ      'D'
     C     XXAMTD        ANDGT     *ZERO
     **
     C                   MOVE      *ON           *IN91
     **
     C                   ELSE
     C                   MOVE      *OFF          *IN91
     **
     C                   ENDIF
     C*
     C***  BEGIN ADD  ***  08/20/98  **********************************
     C*   IF SORT IS BY AGENT  AND CURRENT AGENT IS DIFFERENT FROM LAST
     C*   AGENT PROCESSED START A NEW PAGE
     C     AGTSRT        IFEQ      *ON
     C     EBAGT#        ANDNE     HLDAGT
     C     *IN91         ANDEQ     *ON
     C                   MOVE      *ON           *INOA
     C                   ENDIF
     C***  END   ADD  ***  08/20/98  **********************************
     **
     C***  BEGIN DEL  ***  08/20/98  **********************************
     ***  PRINT THE LATE NOTICE REPORT
     ***
     C**         *INOA     IFEQ *ON
     ***
     C**                   EXCPTHEAD
     C**                   MOVE *OFF      *INOA
     ***
     C**                   ENDIF
     ***
     C**                   MOVE 'Y'       XXFLAG
     C***  END   DEL  ***  08/20/98  **********************************
     **
     C*** BEGIN DEL ***  07/14/98  ************************************
     C************IN91     IFEQ *ON
     ************
     C***********          SELEC
     ************
     C***********EBPFDT    WHGT XXBLDT
     C***********          EXCPTDET
     C***********          EXCPTDET1
     C***********EBCNDT    WHNE *ZERO
     C***********          EXCPTDET
     C***********          EXCPTDET2
     C***********          OTHER
     C***********          EXCPTDET
     ************
     C***********          ENDSL
     ************
     C***********          ENDIF                           *IN91IF=*ON
     C*** END DEL *****  07/14/98  ************************************
     **
     **  THIS ROUTINE CALCULATES AGING FOR A DEBIT OR A CREDIT BALANCE
     **     (APPLIES POSITIVE PAYMENTS FROM 90 DAY UNTIL DISSOLVED)
     **
      *** BEG ADD ***** 12/18/08 *****************************************
     C     XXPAY         IFGT      *ZERO
     C     X180DY        ANDGT     *ZERO
     **
     C     XXPAY         IFLE      X180DY
     **
     C     X180DY        SUB       XXPAY         X180DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       X180DY        XXPAY
     C                   Z-ADD     *ZERO         X180DY
     **
     C                   ENDIF                                                  XXPAY    < X180DY
     **
     C                   ENDIF                                                  XXPAY    > *ZERO
     **
     C     XXPAY         IFGT      *ZERO
     C     X150DY        ANDGT     *ZERO
     **
     C     XXPAY         IFLE      X150DY
     **
     C     X150DY        SUB       XXPAY         X150DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       X150DY        XXPAY
     C                   Z-ADD     *ZERO         X150DY
     **
     C                   ENDIF                                                  XXPAY    < X150DY
     **
     C                   ENDIF                                                  XXPAY    > *ZERO
     **
     C     XXPAY         IFGT      *ZERO
     C     X120DY        ANDGT     *ZERO
     **
     C     XXPAY         IFLE      X120DY
     **
     C     X120DY        SUB       XXPAY         X120DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       X120DY        XXPAY
     C                   Z-ADD     *ZERO         X120DY
     **
     C                   ENDIF                                                  XXPAY    < X120DY
     **
     C                   ENDIF                                                  XXPAY    > *ZERO
      *** END ADD ***** 12/18/08 *****************************************
     C     XXPAY         IFGT      *ZERO
     C     XX90DY        ANDGT     *ZERO
     C     XXPAY         IFLE      XX90DY
     **
     C     XX90DY        SUB       XXPAY         XX90DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       XX90DY        XXPAY
     C                   Z-ADD     *ZERO         XX90DY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPAY         IFGT      *ZERO
     C     XX60DY        ANDGT     *ZERO
     **
     C     XXPAY         IFLE      XX60DY
     **
     C     XX60DY        SUB       XXPAY         XX60DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       XX60DY        XXPAY
     C                   Z-ADD     *ZERO         XX60DY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPAY         IFGT      *ZERO
     C     XX30DY        ANDGT     *ZERO
     **
     C     XXPAY         IFLE      XX30DY
     **
     C     XX30DY        SUB       XXPAY         XX30DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       XX30DY        XXPAY
     C                   Z-ADD     *ZERO         XX30DY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPAY         IFGT      *ZERO
     C     XXCURR        ANDGT     *ZERO
     **
     C     XXPAY         IFLE      XXCURR
     **
     C     XXCURR        SUB       XXPAY         XXCURR
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       XXCURR        XXPAY
     C                   Z-ADD     *ZERO         XXCURR
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     **  THIS ROUTINE CALCULATES AGING FOR DEBIT OR A CREDIT BALANCE
     **     (APPLIES NEGATIVE PAYMENTS FROM 90 DAY UNTIL DISSOLVED)
     **
      *** BEG ADD ***** 12/18/08 *****************************************
     C     XXPAY         IFLT      *ZERO
     C     X180DY        ANDLT     *ZERO
     **
     C     XXPAY         IFGE      X180DY
     **
     C     X180DY        SUB       XXPAY         X180DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       X180DY        XXPAY
     C                   Z-ADD     *ZERO         X180DY
     **
     C                   ENDIF                                                  XXPAY   > X180DY
     **                                                    XXPAY   < *ZERO
     C                   ENDIF
     **
     C     XXPAY         IFLT      *ZERO
     C     X150DY        ANDLT     *ZERO
     **
     C     XXPAY         IFGE      X150DY
     **
     C     X150DY        SUB       XXPAY         X150DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       X150DY        XXPAY
     C                   Z-ADD     *ZERO         X150DY
     **
     C                   ENDIF                                                  XXPAY   > X150DY
     **
     C                   ENDIF                                                  XXPAY   < *ZERO
     **
     C     XXPAY         IFLT      *ZERO
     C     X120DY        ANDLT     *ZERO
     **
     C     XXPAY         IFGE      X120DY
     **
     C     X120DY        SUB       XXPAY         X120DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       X120DY        XXPAY
     C                   Z-ADD     *ZERO         X120DY
     **
     C                   ENDIF                                                  XXPAY   > X120DY
     **
     C                   ENDIF                                                  XXPAY   < *ZERO
      *** END ADD ***** 12/18/08 *****************************************
     C     XXPAY         IFLT      *ZERO
     C     XX90DY        ANDLT     *ZERO
     **
     C     XXPAY         IFGE      XX90DY
     **
     C     XX90DY        SUB       XXPAY         XX90DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       XX90DY        XXPAY
     C                   Z-ADD     *ZERO         XX90DY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPAY         IFLT      *ZERO
     C     XX60DY        ANDLT     *ZERO
     **
     C     XXPAY         IFGE      XX60DY
     **
     C     XX60DY        SUB       XXPAY         XX60DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       XX60DY        XXPAY
     C                   Z-ADD     *ZERO         XX60DY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPAY         IFLT      *ZERO
     C     XX30DY        ANDLT     *ZERO
     **
     C     XXPAY         IFGE      XX30DY
     **
     C     XX30DY        SUB       XXPAY         XX30DY
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       XX30DY        XXPAY
     C                   Z-ADD     *ZERO         XX30DY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPAY         IFLT      *ZERO
     C     XXCURR        ANDLT     *ZERO
     **
     C     XXPAY         IFGE      XXCURR
     **
     C     XXCURR        SUB       XXPAY         XXCURR
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ELSE
     **
     C     XXPAY         SUB       XXCURR        XXPAY
     C                   Z-ADD     *ZERO         XXCURR
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXAMTD        IFLT      *ZERO
     **
     **  THIS ROUTINE CALCULATES AGING FOR A CREDIT BALANCE.
     **
     C     EAKY4         SETGT     PEA04
1    C     XXPAY         DOWGT     *ZERO
     **
      *** BEG CHG **** 05/15/02 ************************************************
     C********** EAKY4     REDPEPEA04                    61
     C     EAKY5         READPE    PEA04                                  61
      *** END CHG **** 05/15/02 ************************************************
      *
      *** BEG ADD ***** 12/17/04 ***********************************************
     C     XXSLAR        IFEQ      'Y'
     C                   Z-ADD     EAPODT        XXSLDT            7 0          SELECT DATE
     C                   ELSE
     C                   Z-ADD     EATRDT        XXSLDT
     C                   ENDIF
      *** END ADD ***** 12/17/04 ***********************************************
      *** BEG ADD ***** 12/07/04 ***********************************************
      *
      *   IF BILLING TYPE = 'A'CH, SKIP THE TRANSACTION IF EMPLOYER ISN'T ACH.
      *   IF BILLING TYPE = 'D'IRECT BILL, SKIP TRANSACTION IF EMPLOYER IS ACH.
      *
     C                   DO
      *
     C     X1BTYP        IFEQ      'A'
     C     ACHFLG        ANDEQ     *OFF
     C     X1BTYP        OREQ      'D'
     C     ACHFLG        ANDEQ     *ON
     C                   LEAVE                                                  SKIP TRANSACTION
     C                   ENDIF
      *** END ADD ***** 12/07/04 ***********************************************
     **
2    C     *IN61         IFEQ      *ON
     C                   GOTO      SKIP
     **
     C                   ENDIF
     **
     **   A/R CODE MUST BE ONE OF THE CODES ENTERED UNLESS
     **    CODES WERE LEFT BLANK THEN ALL CODES WILL BE USED
     **
     C     QQARIE        IFNE      *BLANK
     C                   Z-ADD     1             Y
     C     EAARCD        LOOKUP    COD(Y)                                 60
      *
      *   CHECK IF CODES ARE 'EXCLUDE'
      *
     C     QQARIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
      *** BEG ADD ***** 01/07/05 **********************************
     C                   MOVE      *ON           *IN61
      *** END ADD ***** 01/07/05 **********************************
     C                   ELSE
     C                   MOVE      *OFF          *IN60
      *** BEG ADD ***** 01/07/05 **********************************
     C                   MOVE      *OFF          *IN61
      *** END ADD ***** 01/07/05 **********************************
     C                   ENDIF
      *
     C                   ENDIF                                                  QQARIE = 'E'
      *
     C                   ENDIF                                                  QQARIE NE *BLANK
      **** BEGIN ADD **** 05/15/02 *********************************************
     **
     **   A/R CODE MUST BE ONE OF THE CATEGORIES ENTERED UNLESS
     **    CATEGORIES WERE LEFT BLANK THEN ALL CATEGORIES WILL BE USED
     **
     C     QQCTIE        IFNE      *BLANK
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             Y
     C     TTCTYP        LOOKUP    CT(Y)                                  60
      *
      *   CHECK IF CATEGORIES ARE 'EXCLUDE'
      *
     C     QQCTIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDIF                                                  QQCTIE = 'E'
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  QQCTIE NE *BLANK
      **** END ADD ****** 05/15/02 *********************************************
      *** BEG ADD ***** 01/07/05 **********************************
      *
      * OVERRIDE THE CATEGORY FLAG IF THE A/R CODE WAS PREVIOUSLY EXCLUDED.
      *
     C     *IN60         IFEQ      *ON
     C     *IN61         ANDEQ     *OFF
     C     QQARIE        ANDEQ     'E'
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *** END ADD ***** 01/07/05 **********************************
     **
     C     *IN60         IFEQ      *ON
     C     EAARCD        ANDNE     *BLANK
     C     QQARIE        OREQ      *BLANK
      **** BEGIN ADD **** 05/15/02 *********************************************
     C     QQCTIE        ANDEQ     *BLANK
      **** END ADD ****** 05/15/02 *********************************************
     **
     C     EAAMT         IFGT      *ZERO
      *** BEG ADD **** 05/15/02 ******************************************
     C     EAARTY        OREQ      'H'
     C     EAAMT         ANDLT     *ZERO
     C*
     C                   MOVE      'N'           XXNDEP            1
     C     EAARTY        IFEQ      'H'
     C     EAAMT         ANDLT     *ZERO
     C                   MULT      -1            EAAMT
     C                   MOVE      'Y'           XXNDEP
     C                   ENDIF
      *** END ADD **** 05/15/02 ***************************************
     **
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    IFLE XXBLDT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    IFLE XXBLDT
     C     XXSLDT        IFLE      XXBLDT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C     EAFYR         ANDEQ     EBFYR
     **
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'Q'
     **
     C     XXPAY         IFGT      EAAMT
     **
     C                   SELECT
     **
      *** BEG ADD ***** 12/18/08 *****************************************
     C     XXSLDT        WHENLE    X180DT
     C                   SUB       EAAMT         X180DY
     C     XXSLDT        WHENLE    X150DT
     C                   SUB       EAAMT         X150DY
     C     XXSLDT        WHENLE    X120DT
     C                   SUB       EAAMT         X120DY
      *** END ADD ***** 12/18/08 *****************************************
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX90DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX90DT
     C     XXSLDT        WHENLE    XX90DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX90DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX60DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX60DT
     C     XXSLDT        WHENLE    XX60DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX60DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX30DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX30DT
     C     XXSLDT        WHENLE    XX30DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX30DY
     C                   OTHER
     C                   SUB       EAAMT         XXCURR
     **
     C                   ENDSL
     **
     C                   SUB       EAAMT         XXPAY
     **
     C                   ELSE                                                   XXPAY<=EAAMT
     **
     C                   SELECT
     **
      *** BEG ADD ***** 12/18/08 *****************************************
     C     XXSLDT        WHENLE    X180DT
     C                   SUB       XXPAY         X180DY
     C     XXSLDT        WHENLE    X150DT
     C                   SUB       XXPAY         X150DY
     C     XXSLDT        WHENLE    X120DT
     C                   SUB       XXPAY         X120DY
      *** END ADD ***** 12/18/08 *****************************************
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX90DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX90DT
     C     XXSLDT        WHENLE    XX90DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX90DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX60DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX60DT
     C     XXSLDT        WHENLE    XX60DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX60DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX30DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX30DT
     C     XXSLDT        WHENLE    XX30DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX30DY
     C                   OTHER
     C                   SUB       XXPAY         XXCURR
     **
     C                   ENDSL
     **
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     **
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    IFLE XXBLDT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    IFLE XXBLDT
     C     XXSLDT        IFLE      XXBLDT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C     EAARTY        ANDEQ     'I'
      **** BEG ADD **** 05/15/02 **************************************
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    ORLE XXBLDT
     C     XXSLDT        ORLE      XXBLDT
      *** END CHG ***** 12/17/04 **************************************
     C     EAARTY        ANDEQ     'H'
     C     XXNDEP        ANDEQ     'Y'
      **** END ADD **** 05/15/02 **************************************
     **
     C     XXPAY         IFGT      EAAMT
     **
     C                   SELECT
     **
      **** BEG ADD **** 12/18/08 **************************************
     C     XXSLDT        WHENLE    X180DT
     C                   SUB       EAAMT         X180DY
     C     XXSLDT        WHENLE    X150DT
     C                   SUB       EAAMT         X150DY
     C     XXSLDT        WHENLE    X120DT
     C                   SUB       EAAMT         X120DY
      **** END ADD **** 12/18/08 **************************************
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX90DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX90DT
     C     XXSLDT        WHENLE    XX90DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX90DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX60DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX60DT
     C     XXSLDT        WHENLE    XX60DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX60DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX30DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX30DT
     C     XXSLDT        WHENLE    XX30DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX30DY
     C                   OTHER
     C                   SUB       EAAMT         XXCURR
     **
     C                   ENDSL
     **
     C                   SUB       EAAMT         XXPAY
     **
     C                   ELSE                                                   XXPAY<=EAAMT
     **
     C                   SELECT
      **** BEG ADD **** 12/18/08 **************************************
     C     XXSLDT        WHENLE    X180DT
     C                   SUB       XXPAY         X180DY
     C     XXSLDT        WHENLE    X150DT
     C                   SUB       XXPAY         X150DY
     C     XXSLDT        WHENLE    X120DT
     C                   SUB       XXPAY         X120DY
      **** END ADD **** 12/18/08 **************************************
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX90DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX90DT
     C     XXSLDT        WHENLE    XX90DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX90DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX60DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX60DT
     C     XXSLDT        WHENLE    XX60DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX60DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX30DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX30DT
     C     XXSLDT        WHENLE    XX30DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX30DY
     C                   OTHER
     C                   SUB       XXPAY         XXCURR
     **
     C                   ENDSL
     **
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
      *** BEG ADD **** 05/15/02 ******************************************
     C     EAARTY        IFEQ      'H'
     C     XXNDEP        ANDEQ     'Y'
     C                   MULT      -1            EAAMT
     C                   ENDIF
      *** END ADD **** 05/15/02 ******************************************
     **
     C                   ENDIF                                                  EAAMT > 0
     C                   ENDIF                                                  ARCODE CHK
     **
      *** BEG ADD ***** 12/07/04 ***********************************************
     C                   ENDDO                                                  DO BEFORE BTYP=A
      *** END ADD ***** 12/07/04 ***********************************************
     **
     C                   ENDDO                                                  XXPAY>0
     C     SKIP          TAG
     **
     C                   ENDIF                                                  XXAMTD<0
     **
     **  IF THE TOTAL REMAINING PAYMENT AMOUNT IS LESS THAN ZERO,
     **     LOAD THE REMAINDER ACCORDING TO POSTING DATE
     **
     C     EAKY4         SETGT     PEA04
     C     XXPAY         DOWLT     *ZERO
     **
      **** BEGIN CHG **** 10/18/00 *********************************************
     C********** EAKY4     REDPEPEA04                    61
     C     EAKY5         READPE    PEA04                                  61
      **** END   CHG **** 10/18/00 *********************************************
      *
      *** BEG ADD ***** 12/17/04 ***********************************************
     C     XXSLAR        IFEQ      'Y'
     C                   Z-ADD     EAPODT        XXSLDT            7 0          SELECT DATE
     C                   ELSE
     C                   Z-ADD     EATRDT        XXSLDT
     C                   ENDIF
      *** END ADD ***** 12/17/04 ***********************************************
      *** BEG ADD ***** 12/07/04 ***********************************************
      *
      *   IF BILLING TYPE = 'A'CH, SKIP THE TRANSACTION IF EMPLOYER ISN'T ACH.
      *   IF BILLING TYPE = 'D'IRECT BILL, SKIP TRANSACTION IF EMPLOYER IS ACH.
      *
     C                   DO
      *
     C     X1BTYP        IFEQ      'A'
     C     ACHFLG        ANDEQ     *OFF
     C     X1BTYP        OREQ      'D'
     C     ACHFLG        ANDEQ     *ON
     C                   LEAVE                                                  SKIP TRANSACTION
     C                   ENDIF
      *** END ADD ***** 12/07/04 ***********************************************
     **
     C     *IN61         IFEQ      *ON
     C                   GOTO      SKIP2
     **
     C                   ENDIF
     **
     **   A/R CODE MUST BE ONE OF THE CODES ENTERED UNLESS
     **    CODES WERE LEFT BLANK THEN ALL CODES WILL BE USED
     **
     C     QQARIE        IFNE      *BLANK
     C                   Z-ADD     1             Y
     C     EAARCD        LOOKUP    COD(Y)                                 60
      *
      *   CHECK IF CODES ARE 'EXCLUDE'
      *
     C     QQARIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
      *** BEG ADD ***** 01/07/05 **********************************
     C                   MOVE      *ON           *IN61
      *** END ADD ***** 01/07/05 **********************************
     C                   ELSE
     C                   MOVE      *OFF          *IN60
      *** BEG ADD ***** 01/07/05 **********************************
     C                   MOVE      *OFF          *IN61
      *** END ADD ***** 01/07/05 **********************************
     C                   ENDIF
      *
     C                   ENDIF                                                  QQARIE = 'E'
      *
     C                   ENDIF                                                  QQARIE NE *BLANK
      **** BEGIN ADD **** 05/15/02 *********************************************
     **
     **   A/R CODE MUST BE ONE OF THE CATEGORIES ENTERED UNLESS
     **    CATEGORIES WERE LEFT BLANK THEN ALL CATEGORIES WILL BE USED
     **
     C     QQCTIE        IFNE      *BLANK
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     1             Y
     C     TTCTYP        LOOKUP    CT(Y)                                  60
      *
      *   CHECK IF CATEGORIES ARE 'EXCLUDE'
      *
     C     QQCTIE        IFEQ      'E'
      *
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *
     C                   ENDIF                                                  QQCTIE = 'E'
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  QQCTIE NE *BLANK
      **** END ADD ****** 05/15/02 *********************************************
      *** BEG ADD ***** 01/07/05 **********************************
      *
      * OVERRIDE THE CATEGORY FLAG IF THE A/R CODE WAS PREVIOUSLY EXCLUDED.
      *
     C     *IN60         IFEQ      *ON
     C     *IN61         ANDEQ     *OFF
     C     QQARIE        ANDEQ     'E'
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
      *** END ADD ***** 01/07/05 **********************************
     **
     C     *IN60         IFEQ      *ON
     C     EAARCD        ANDNE     *BLANK
     C     QQARIE        OREQ      *BLANK
      **** BEGIN ADD **** 05/15/02 *********************************************
     C     QQCTIE        ANDEQ     *BLANK
      **** END ADD ****** 05/15/02 *********************************************
     **
     C     EAAMT         IFLT      *ZERO
     **
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    IFLE XXBLDT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    IFLE XXBLDT ********************************
     C     XXSLDT        IFLE      XXBLDT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C     EAFYR         ANDEQ     EBFYR
     **
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'Q'
     **
     C     XXPAY         IFLT      EAAMT
     **
     C                   SELECT
     **
      ***** BEG ADD ******* 12/18/08 **********************************
     C     XXSLDT        WHENLE    X180DT
     C                   SUB       EAAMT         X180DY
     C     XXSLDT        WHENLE    X150DT
     C                   SUB       EAAMT         X150DY
     C     XXSLDT        WHENLE    X120DT
     C                   SUB       EAAMT         X120DY
      ***** END ADD ******* 12/18/08 **********************************
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX90DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX90DT
     C     XXSLDT        WHENLE    XX90DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX90DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX60DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX60DT
     C     XXSLDT        WHENLE    XX60DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX60DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX30DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX30DT
     C     XXSLDT        WHENLE    XX30DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX30DY
     C                   OTHER
     C                   SUB       EAAMT         XXCURR
     **
     C                   ENDSL
     **
     C                   SUB       EAAMT         XXPAY
     **
     C                   ELSE                                                   XXPAY>=EAAMT
     **
     C                   SELECT
     **
      ***** BEG ADD ******* 12/18/08 **********************************
     C     XXSLDT        WHENLE    X180DT
     C                   SUB       XXPAY         X180DY
     C     XXSLDT        WHENLE    X150DT
     C                   SUB       XXPAY         X150DY
     C     XXSLDT        WHENLE    X120DT
     C                   SUB       XXPAY         X120DY
      ***** END ADD ******* 12/18/08 **********************************
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX90DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX90DT
     C     XXSLDT        WHENLE    XX90DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX90DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX60DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX60DT
     C     XXSLDT        WHENLE    XX60DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX60DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX30DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX30DT
     C     XXSLDT        WHENLE    XX30DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX30DY
     C                   OTHER
     C                   SUB       XXPAY         XXCURR
     **
1    C                   ENDSL
     **
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ENDIF                                                  XXPAY IFLT EAAMT
     **
     C                   ENDIF                                                  EAARTY IFEQ 'P'
     **
     C                   ENDIF                                                  EATRDT IFLE XXBLDT
     **
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    IFLE XXBLDT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    IFLE XXBLDT
     C     XXSLDT        IFLE      XXBLDT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C     EAARTY        ANDEQ     'I'
     **
     C     XXPAY         IFLT      EAAMT
     **
     C                   SELECT
     **
      ***** BEG ADD ******* 12/18/08 **********************************
     C     XXSLDT        WHENLE    X180DT
     C                   SUB       EAAMT         X180DY
     C     XXSLDT        WHENLE    X150DT
     C                   SUB       EAAMT         X150DY
     C     XXSLDT        WHENLE    X120DT
     C                   SUB       EAAMT         X120DY
      ***** END ADD ******* 12/18/08 **********************************
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX90DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX90DT
     C     XXSLDT        WHENLE    XX90DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX90DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX60DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX60DT
     C     XXSLDT        WHENLE    XX60DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX60DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX30DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX30DT
     C     XXSLDT        WHENLE    XX30DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       EAAMT         XX30DY
     C                   OTHER
     C                   SUB       EAAMT         XXCURR
     **
     C                   ENDSL
     **
     C                   SUB       EAAMT         XXPAY
     **
     C                   ELSE                                                   XXPAY>=EAAMT
     **
     C                   SELECT
      ***** BEG ADD ******* 12/18/08 **********************************
     C     XXSLDT        WHENLE    X180DT
     C                   SUB       XXPAY         X180DY
     C     XXSLDT        WHENLE    X150DT
     C                   SUB       XXPAY         X150DY
     C     XXSLDT        WHENLE    X120DT
     C                   SUB       XXPAY         X120DY
      ***** END ADD ******* 12/18/08 **********************************
     **
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX90DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX90DT
     C     XXSLDT        WHENLE    XX90DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX90DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX60DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX60DT
     C     XXSLDT        WHENLE    XX60DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX60DY
      ***** BEG CHG ******* 08/23/01 **********************************
     C*********  EAPODT    WHLE XX30DT
      *** BEG CHG ***** 12/17/04 **************************************
     C*********  EATRDT    WHLE XX30DT
     C     XXSLDT        WHENLE    XX30DT
      *** END CHG ***** 12/17/04 **************************************
      ***** END CHG ******* 08/23/01 **********************************
     C                   SUB       XXPAY         XX30DY
     C                   OTHER
     C                   SUB       XXPAY         XXCURR
     **
     C                   ENDSL
     **
     C                   Z-ADD     *ZERO         XXPAY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C                   ENDIF                                                  EAAMT<0
     **
     C                   ENDIF                                                  ARCODE CHK
     **
      *** BEG ADD ***** 12/07/04 ***********************************************
     C                   ENDDO                                                  DO BEFORE BTYP=A
      *** END ADD ***** 12/07/04 ***********************************************
     **
     C                   ENDDO                                                  XXPAY<0
     C     SKIP2         TAG
     **
     ** IF DEBIT BALANCE, ACCUMULATE ALL NEGATIVE COLUMNS, THEN
     **     ZERO OUT THOSE COLUMNS AND APPLY TOTAL NEGATIVE AMOUNT
     **     STARTING FROM THE 90 DAY.
     **
     C     XXAMTD        IFGT      *ZERO
     C                   EXSR      SRNEGA
     C                   ENDIF
     **
     ** IF CREDIT BALANCE, ACCUMULATE ALL POSITIVE COLUMNS, THEN
     **     ZERO OUT THOSE COLUMNS AND APPLY TOTAL POSITIVE AMOUNT
     **     STARTING FROM THE 90 DAY.
     **
     C     XXAMTD        IFLT      *ZERO
     C                   EXSR      SRPOSA
     C                   ENDIF
     **
     C     XXAMTD        IFEQ      *ZERO
     C                   EXSR      SRZERA
     C                   ENDIF
      *
     C                   Z-ADD     *ZERO         XXFUTR
      *
     C     XXLBDT        IFNE      *ZEROS
     C                   Z-ADD     *ZERO         XX#DAY
     C                   Z-ADD     *ZERO         AGCI#D
     C     EAPODT        IFNE      *ZERO
     C     EAPODT        ANDLT     XXBLDT
     C     XXAMTD        ANDGT     *ZERO
     C                   Z-ADD     XXLBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         QQFROM            8 0
     C                   Z-ADD     XXBLDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C*
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR

     C     QQDAYS        IFGT      *ZERO
     C     QQERR         ANDEQ     *OFF
     C                   Z-ADD     QQDAYS        AGCI#D
     C                   Z-ADD     QQDAYS        XX#DAY
     C                   ENDIF

     C                   ENDIF

     C                   ELSE
     C                   Z-ADD     *ZEROS        AGCI#D
     C                   Z-ADD     *ZEROS        XX#DAY
     C                   ENDIF
      *
     **
     **  TOTAL THE AMOUNTS DUE FOR ALL ACCOUNTS THAT HAVE A BALANCE
     **     DUE IN THE 90 DAY BUCKET.
     **
     C     *IN91         IFEQ      *ON
     C*
     C     XXCURR        IFNE      *ZEROS
     C     XX30DY        ORNE      *ZEROS
     C     XX60DY        ORNE      *ZEROS
     C     XX90DY        ORNE      *ZEROS
     C     X120DY        ORNE      *ZEROS
     C     X150DY        ORNE      *ZEROS
     C     X180DY        ORNE      *ZEROS
     C     XXAMTD        ORNE      *ZEROS
     C                   MOVE      'Y'           XXFLAG
     **
     C     EBACUR        IFNE      'Y'
     **
     C                   SELECT
      *** beg add ***** 06/15/16 *****************
     C                   When      isUseXMl and not(isUseSpoolOut)
     c                   Eval      xxLbDt = 0
      *** end add ***** 06/15/16 *****************
     C     QQCRDB        WHENEQ    'C'
     C     XXAMTD        ANDLT     XXMINC
     C     XXMINC        ANDNE     *ZERO
     C     XXAMTD        ANDLT     *ZERO
     C     QQCRDB        OREQ      'B'
     C     XXAMTD        ANDLT     XXMINC
     C     XXMINC        ANDNE     *ZERO
     C     XXAMTD        ANDLT     *ZERO
     C                   SELECT
     **
     C     EBPFDT        WHENGT    XXBLDT
     C                   EXSR      SRHEAD
     C                   EXCEPT    DET
     C                   Z-ADD     *ZEROS        XXLBDT
     C                   EXCEPT    DET1
     C     EBCNDT        WHENNE    *ZERO
     C     CXDATE        ORNE      *ZERO
     C                   EXSR      SRHEAD
     C                   EXCEPT    DET
     C                   Z-ADD     *ZEROS        XXLBDT
     C                   EXCEPT    DET2
     C                   OTHER
     C                   EXSR      SRHEAD
     C                   EXCEPT    DET
     C                   Z-ADD     *ZEROS        XXLBDT
     **
     C                   ENDSL
     C*
     C     QQCRDB        WHENEQ    'D'
     C     XXAMTD        ANDGT     XXMIND
     C     XXMIND        ANDNE     *ZERO
     C     QQCRDB        OREQ      'B'
     C     XXAMTD        ANDGT     XXMIND
     C     XXMIND        ANDNE     *ZERO
     C                   SELECT
     **
     C     EBPFDT        WHENGT    XXBLDT
     C                   EXSR      SRHEAD
     C                   EXCEPT    DET
     C                   Z-ADD     *ZEROS        XXLBDT
     C                   EXCEPT    DET1
     C     EBCNDT        WHENNE    *ZERO
     C     CXDATE        ORNE      *ZERO
     C                   EXSR      SRHEAD
     C                   EXCEPT    DET
     C                   Z-ADD     *ZEROS        XXLBDT
     C                   EXCEPT    DET2
     C                   OTHER
     C                   EXSR      SRHEAD
     C                   EXCEPT    DET
     C                   Z-ADD     *ZEROS        XXLBDT
     **
     C                   ENDSL
     C*
     C     XXMINC        WHENEQ    *ZERO
     C     XXMIND        OREQ      *ZERO
     C                   SELECT
     **
     C     EBPFDT        WHENGT    XXBLDT
     C                   EXSR      SRHEAD
     C                   EXCEPT    DET
     C                   Z-ADD     *ZEROS        XXLBDT
     C                   EXCEPT    DET1
     C     EBCNDT        WHENNE    *ZERO
     C     CXDATE        ORNE      *ZERO
     C                   EXSR      SRHEAD
     C                   EXCEPT    DET
     C                   Z-ADD     *ZEROS        XXLBDT
     C                   EXCEPT    DET2
     C                   OTHER
     C                   EXSR      SRHEAD
     C                   EXCEPT    DET
     C                   Z-ADD     *ZEROS        XXLBDT
     **
     C                   ENDSL
     C*
     C                   ENDSL
     C                   SELECT
      *** beg add ***** 06/16/16 ****************
     C                   When      isUseXMl and not(isUseSpoolOut)
     c                   Exsr      file
      *** end add ***** 06/16/16 ****************
     C     QQCRDB        WHENEQ    'C'
     C     XXAMTD        ANDLT     XXMINC
     C     XXMINC        ANDNE     *ZERO
     C     XXAMTD        ANDLT     *ZERO
     C     QQCRDB        OREQ      'B'
     C     XXAMTD        ANDLT     XXMINC
     C     XXMINC        ANDNE     *ZERO
     C     XXAMTD        ANDLT     *ZERO
     C                   EXSR      TOTAL
     C                   EXCEPT    DET3
     C                   EXSR      FILE
     C                   MOVE      *ON           *IN97
     C                   ADD       1             AGTCNT
     C                   ADD       1             GRTEMP
     C*
     C     QQCRDB        WHENEQ    'D'
     C     XXAMTD        ANDGT     XXMIND
     C     XXMIND        ANDNE     *ZERO
     C     QQCRDB        OREQ      'B'
     C     XXAMTD        ANDGT     XXMIND
     C     XXMIND        ANDNE     *ZERO
     C                   EXSR      TOTAL
     C                   EXCEPT    DET3
     C                   EXSR      FILE
     C                   MOVE      *ON           *IN97
     C                   ADD       1             AGTCNT
     C                   ADD       1             GRTEMP
     C*
     C     XXMINC        WHENEQ    *ZERO
     C     XXMIND        OREQ      *ZERO
     C                   EXSR      TOTAL
     C                   EXCEPT    DET3
     C                   EXSR      FILE
     C                   MOVE      *ON           *IN97
     C                   ADD       1             AGTCNT
     C                   ADD       1             GRTEMP
     C*
     C                   ENDSL
     C                   ENDIF                                                  EBACUR NE Y
     C                   ELSE
     C     XXDEFR        IFNE      0
     C                   EXSR      TOTAL
     C                   EXSR      FILE
     C                   ENDIF                                                  XXDEFR IFNE 0
     C                   ENDIF                                                  NONZERO BAL
     **
     C                   ENDIF                                                  *IN91IF=*ON
     **
     C                   EXSR      SRLOAD
      *
     C                   ENDIF                                                  QQAUST IFNE 'B'
      *
     C                   ENDDO                                                  X1BTYP = 'P'
      *
     C                   ENDIF                                                  *IN80 = *OFF
     C*
     C*  SET VALUE OF HOLD AGENT# TO CURRENT AGENT#
     C                   Z-ADD     EBAGT#        HLDAGT
     C*
     C                   ENDIF                                                  EBACUR<>Y
      *
     C     EBKEY         READE     PEB01                                  50
     C*
     C*   IF SORT IS BY AGENT  AND CURRENT AGENT IS DIFFERENT FROM LAST
     C*   AGENT PROCESSED: PRINT AGENT TOTALS FOR PREVIOUS AGENT AND
     C*   RESET AGENT TOTAL BUCKETS
     C     AGTSRT        IFEQ      *ON
     C     EBAGT#        IFNE      HLDAGT
     C     *IN50         OREQ      *ON
     C*
     C*     IF ALL PREV AGT BUCKETS = 0, DO NOT PRINT TOTALS
     C*
     C     *IN97         IFEQ      *ON
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    GRPHD
     C                   EXCEPT    AGTHD
     C                   EXCEPT    HEAD2
     C                   MOVE      *OFF          *INOA
     C                   EXCEPT    AGTOTL
     C                   ELSE
     C                   EXCEPT    AGTOTL
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN97
     C                   Z-ADD     0             AGTCNT            3 0
     C                   MOVE      *ON           *INOA
     C*
     C                   ELSE
     C*
     C                   Z-ADD     EBAGT#        HLDAGT
     C*
     C                   ENDIF                                                  *IN97=*ON
     C*
     C                   ENDIF                                                  EBAGT#<>HLDAGGT
     C                   ENDIF                                                  AGSTRT=*ON
     C*
     C     *IN50         IFEQ      *ON
     C                   EXSR      NEWGRP
     C                   ENDIF
      *
     C                   ENDDO                                                  *IN50=*OFF
     **
      *** beg add ***** 06/15/16 *****************
     c                   If        isUseSpoolOut
      *** end add ***** 06/15/16 *****************

     C     XXFLAG        IFEQ      'Y'
     C     ARCFLG        OREQ      *OFF
     C*
     C     ARCFLG        IFEQ      *OFF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        XXCONM           40
     C                   ENDIF
     C*    IF SORT BY AGENT THEN START NEW PAGE FOR GRAND TOTALS
     C     AGTSRT        IFEQ      *ON
     C     ARCFLG        OREQ      *OFF
     C                   EXCEPT    HEAD
     C     ARCFLG        IFEQ      *ON
     C                   EXCEPT    GRPHD
     C                   ENDIF
     C                   EXCEPT    HEAD2
     C                   ENDIF
     C                   SELECT
     C     ARCFLG        WHENEQ    *ON
     C                   EXSR      GRPTTL
     C                   ENDSL
     C                   EXCEPT    LRTOTL
     C                   ENDIF
     C*
     C     USEGSL        IFEQ      *ON
     C                   MOVE      *ON           *INOA
     C                   EXSR      PRNOGP
     C                   ENDIF
     **
      /Free
       //***** Beg Add ***** 08/26/14 *************************************
       If not %Open(Qsysprt2) and (GrpFnd = *on or ArcFlg = *off);
          Open Qsysprt2;
       Endif;
       xxPrt = '1';
       Chain orKey POR01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 *************************************
      /End-free

      *** beg add ***** 06/15/16 *****************
     c                   Endif                                                  isUseSpoolOut
      *** end add ***** 06/15/16 *****************

     C                   MOVE      *ON           *INLR
     *******************************************************************
     **   SRLOAD - SUBROUTINE TO LOAD WORK FIELDS WITH NAMES AND ADDRESS
     *******************************************************************
     C     SRLOAD        BEGSR
     **
     **   LOAD EMPLOYER ADDRESS, CITY, STATE, & ZIP
     **
     C     EMMPFL        IFEQ      'M'
     **
     C                   MOVEL(P)  EMMAD1        XXADR1           34
     C                   MOVEL(P)  EMMAD2        XXADR2           34
     C                   MOVEL(P)  EMMCTY        XXCITY
     C                   MOVEL     EMMST         XXSTAT
     C                   MOVEL     EMMZIP        XXZIP1
     C                   MOVE      EMMZIP        XXZIP2
     **
     C                   ELSE
     **
     C                   MOVEL(P)  EMPAD1        XXADR1
     C                   MOVEL(P)  EMPAD2        XXADR2
     C                   MOVEL(P)  EMPCTY        XXCITY
     C                   MOVEL     EMPST         XXSTAT
     C                   MOVEL     EMPZIP        XXZIP1
     C                   MOVE      EMPZIP        XXZIP2
     **
     C                   ENDIF
     **
     ** LOAD THE BILLING ADDRESS IF ITS NOT BLANK
     **
     C     A3KY          CHAIN     PA301                              60
     **
     C     *IN60         IFEQ      *ZERO
      ***** BEG ADD ******* 12/10/98 **********************************
     C     A3STAT        ANDEQ     'A'
      ***** END ADD ******* 12/10/98 **********************************
     C     A3ADR1        IFNE      *BLANK
     **
     C                   MOVEL(P)  A3ADR1        XXADR1
     C                   MOVEL(P)  A3ADR2        XXADR2
     C                   MOVEL(P)  A3CTY         XXCITY
     C                   MOVEL     A3ST          XXSTAT
     C                   MOVEL     A3ZIP1        XXZIP1
     C                   MOVEL     A3ZIP2        XXZIP2
     **
     C                   ENDIF                                                  *IN60  IFEQ
     **
     C                   ENDIF                                                  A3ADR1 IFNE
     **
     C                   EXSR      SRCSTZ
     C                   MOVEL(P)  XXCSTZ        XXADR3           34
     **
     **  REMOVE BLANK LINE FROM EMPLOYER ADDRESS IF NECESSARY
     **
     C     XXADR2        IFEQ      *BLANK
     C                   MOVEL(P)  XXADR3        XXADR2
     C                   MOVEL     *BLANK        XXADR3
     C                   ENDIF
     **
     C                   ENDSR
     ************************************************************
     **  SRCSTZ - SUBROUTINE TO BUILD THE CITY/ ST/ ZIP LINE    *
     ************************************************************
     C     SRCSTZ        BEGSR
     **
     C     XXCITY        CAT(P)    ',':0         XXCSTZ           40
     C                   CAT       XXSTAT:1      XXCSTZ
     C                   CAT       XXZIP1:1      XXCSTZ
     **
     C     XXZIP2        IFNE      *ZERO
     C     XXZIP2        ANDNE     *BLANK
     C                   CAT       '-':0         XXCSTZ
     C                   CAT       XXZIP2:0      XXCSTZ
     C                   ENDIF
     **
     C                   ENDSR
      *
     *******************************************************************
     **   SRNEGA - SUBROUTINE TO REMOVE NEGATIVE AMOUNTS FROM AGING
     **            BUCKETS FOR DEBIT ACCOUNTS.
     *******************************************************************
     C     SRNEGA        BEGSR
     **
     C                   Z-ADD     *ZERO         XXNEGA            9 2
     **
     ** ACCUMULATE ALL NEGATIVE AMOUNTS
     **
      *** BEG ADD ***** 12/18/08 *****************************************
     C     X180DY        IFLT      *ZERO
     C                   SUB       X180DY        XXNEGA
     C                   Z-ADD     *ZERO         X180DY
     C                   ENDIF
     **
     C     X150DY        IFLT      *ZERO
     C                   SUB       X150DY        XXNEGA
     C                   Z-ADD     *ZERO         X150DY
     C                   ENDIF
     **
     C     X120DY        IFLT      *ZERO
     C                   SUB       X120DY        XXNEGA
     C                   Z-ADD     *ZERO         X120DY
     C                   ENDIF
     **
      *** END ADD ***** 12/18/08 *****************************************
     C     XX90DY        IFLT      *ZERO
     C                   SUB       XX90DY        XXNEGA
     C                   Z-ADD     *ZERO         XX90DY
     C                   ENDIF
     **
     C     XX60DY        IFLT      *ZERO
     C                   SUB       XX60DY        XXNEGA
     C                   Z-ADD     *ZERO         XX60DY
     C                   ENDIF
     **
     C     XX30DY        IFLT      *ZERO
     C                   SUB       XX30DY        XXNEGA
     C                   Z-ADD     *ZERO         XX30DY
     C                   ENDIF
     **
     C     XXCURR        IFLT      *ZERO
     C                   SUB       XXCURR        XXNEGA
     C                   Z-ADD     *ZERO         XXCURR
     C                   ENDIF
     **
      *** BEG ADD ***** 12/18/08 *****************************************
     C     XXNEGA        IFGT      *ZERO
     C     X180DY        ANDGT     *ZERO
     **
     C     XXNEGA        IFLE      X180DY
     **
     C     X180DY        SUB       XXNEGA        X180DY
     C                   Z-ADD     *ZERO         XXNEGA
     **
     C                   ELSE
     **
     C     XXNEGA        SUB       X180DY        XXNEGA
     C                   Z-ADD     *ZERO         X180DY
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXNEGA        IFGT      *ZERO
     C     X150DY        ANDGT     *ZERO
     **
     C     XXNEGA        IFLE      X150DY
     **
     C     X150DY        SUB       XXNEGA        X150DY
     C                   Z-ADD     *ZERO         XXNEGA
     **
     C                   ELSE
     **
     C     XXNEGA        SUB       X150DY        XXNEGA
     C                   Z-ADD     *ZERO         X150DY
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXNEGA        IFGT      *ZERO
     C     X120DY        ANDGT     *ZERO
     **
     C     XXNEGA        IFLE      X120DY
     **
     C     X120DY        SUB       XXNEGA        X120DY
     C                   Z-ADD     *ZERO         XXNEGA
     **
     C                   ELSE
     **
     C     XXNEGA        SUB       X120DY        XXNEGA
     C                   Z-ADD     *ZERO         X120DY
     C                   ENDIF
     **
     C                   ENDIF
      *** END ADD ***** 12/18/08 *****************************************
     **
     C     XXNEGA        IFGT      *ZERO
     C     XX90DY        ANDGT     *ZERO
     **
     C     XXNEGA        IFLE      XX90DY
     **
     C     XX90DY        SUB       XXNEGA        XX90DY
     C                   Z-ADD     *ZERO         XXNEGA
     **
     C                   ELSE
     **
     C     XXNEGA        SUB       XX90DY        XXNEGA
     C                   Z-ADD     *ZERO         XX90DY
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXNEGA        IFGT      *ZERO
     C     XX60DY        ANDGT     *ZERO
     **
     C     XXNEGA        IFLE      XX60DY
     **
     C     XX60DY        SUB       XXNEGA        XX60DY
     C                   Z-ADD     *ZERO         XXNEGA
     **
     C                   ELSE
     **
     C     XXNEGA        SUB       XX60DY        XXNEGA
     C                   Z-ADD     *ZERO         XX60DY
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXNEGA        IFGT      *ZERO
     C     XX30DY        ANDGT     *ZERO
     **
     C     XXNEGA        IFLE      XX30DY
     **
     C     XX30DY        SUB       XXNEGA        XX30DY
     C                   Z-ADD     *ZERO         XXNEGA
     **
     C                   ELSE
     **
     C     XXNEGA        SUB       XX30DY        XXNEGA
     C                   Z-ADD     *ZERO         XX30DY
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXNEGA        IFGT      *ZERO
     C     XXCURR        ANDGT     *ZERO
     **
     C     XXNEGA        IFLE      XXCURR
     **
     C     XXCURR        SUB       XXNEGA        XXCURR
     C                   Z-ADD     *ZERO         XXNEGA
     **
     C                   ELSE
     **
     C     XXNEGA        SUB       XXCURR        XXNEGA
     C                   Z-ADD     *ZERO         XXCURR
     C                   ENDIF
     **
     C                   ENDIF
     **
     C                   ENDSR
     **
     *******************************************************************
     **   SRPOSA - SUBROUTINE TO REMOVE POSITIVE AMOUNTS FROM AGING
     **            BUCKETS FOR CREDIT ACCOUNTS.
     *******************************************************************
     C     SRPOSA        BEGSR
     **
     C                   Z-ADD     *ZERO         XXPOSA            9 2
     **
     ** ACCUMULATE ALL NEGATIVE AMOUNTS
     **
      *** BEG ADD ***** 12/18/08 *****************************************
     C     X180DY        IFGT      *ZERO
     C                   SUB       X180DY        XXPOSA
     C                   Z-ADD     *ZERO         X180DY
     C                   ENDIF
     **
     C     X150DY        IFGT      *ZERO
     C                   SUB       X150DY        XXPOSA
     C                   Z-ADD     *ZERO         X150DY
     C                   ENDIF
     **
     C     X120DY        IFGT      *ZERO
     C                   SUB       X120DY        XXPOSA
     C                   Z-ADD     *ZERO         X120DY
     C                   ENDIF
     **
      *** END ADD ***** 12/18/08 *****************************************
     C     XX90DY        IFGT      *ZERO
     C                   SUB       XX90DY        XXPOSA
     C                   Z-ADD     *ZERO         XX90DY
     C                   ENDIF
     **
     C     XX60DY        IFGT      *ZERO
     C                   SUB       XX60DY        XXPOSA
     C                   Z-ADD     *ZERO         XX60DY
     C                   ENDIF
     **
     C     XX30DY        IFGT      *ZERO
     C                   SUB       XX30DY        XXPOSA
     C                   Z-ADD     *ZERO         XX30DY
     C                   ENDIF
     **
     C     XXCURR        IFGT      *ZERO
     C                   SUB       XXCURR        XXPOSA
     C                   Z-ADD     *ZERO         XXCURR
     C                   ENDIF
     **
      *** BEG ADD ***** 12/18/08 *****************************************
     C     XXPOSA        IFLT      *ZERO
     C     X180DY        ANDLT     *ZERO
     **
     C     XXPOSA        IFGE      X180DY
     **
     C     X180DY        SUB       XXPOSA        X180DY
     C                   Z-ADD     *ZERO         XXPOSA
     **
     C                   ELSE
     **
     C     XXPOSA        SUB       X180DY        XXPOSA
     C                   Z-ADD     *ZERO         X180DY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPOSA        IFLT      *ZERO
     C     X150DY        ANDLT     *ZERO
     **
     C     XXPOSA        IFGE      X150DY
     **
     C     X150DY        SUB       XXPOSA        X150DY
     C                   Z-ADD     *ZERO         XXPOSA
     **
     C                   ELSE
     **
     C     XXPOSA        SUB       X150DY        XXPOSA
     C                   Z-ADD     *ZERO         X150DY
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPOSA        IFLT      *ZERO
     C     X120DY        ANDLT     *ZERO
     **
     C     XXPOSA        IFGE      X120DY
     **
     C     X120DY        SUB       XXPOSA        X120DY
     C                   Z-ADD     *ZERO         XXPOSA
     **
     C                   ELSE
     **
     C     XXPOSA        SUB       X120DY        XXPOSA
     C                   Z-ADD     *ZERO         X120DY
     C                   ENDIF
     **
     C                   ENDIF
     **
      *** END ADD ***** 12/18/08 *****************************************
     C     XXPOSA        IFLT      *ZERO
     C     XX90DY        ANDLT     *ZERO
     **
     C     XXPOSA        IFGE      XX90DY
     **
     C     XX90DY        SUB       XXPOSA        XX90DY
     C                   Z-ADD     *ZERO         XXPOSA
     **
     C                   ELSE
     **
     C     XXPOSA        SUB       XX90DY        XXPOSA
     C                   Z-ADD     *ZERO         XX90DY
     **
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPOSA        IFLT      *ZERO
     C     XX60DY        ANDLT     *ZERO
     **
     C     XXPOSA        IFGE      XX60DY
     **
     C     XX60DY        SUB       XXPOSA        XX60DY
     C                   Z-ADD     *ZERO         XXPOSA
     **
     C                   ELSE
     **
     C     XXPOSA        SUB       XX60DY        XXPOSA
     C                   Z-ADD     *ZERO         XX60DY
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPOSA        IFLT      *ZERO
     C     XX30DY        ANDLT     *ZERO
     **
     C     XXPOSA        IFGE      XX30DY
     **
     C     XX30DY        SUB       XXPOSA        XX30DY
     C                   Z-ADD     *ZERO         XXPOSA
     **
     C                   ELSE
     **
     C     XXPOSA        SUB       XX30DY        XXPOSA
     C                   Z-ADD     *ZERO         XX30DY
     C                   ENDIF
     **
     C                   ENDIF
     **
     C     XXPOSA        IFLT      *ZERO
     C     XXCURR        ANDLT     *ZERO
     **
     C     XXPOSA        IFGE      XXCURR
     **
     C     XXCURR        SUB       XXPOSA        XXCURR
     C                   Z-ADD     *ZERO         XXPOSA
     **
     C                   ELSE
     **
     C     XXPOSA        SUB       XXCURR        XXPOSA
     C                   Z-ADD     *ZERO         XXCURR
     C                   ENDIF
     **
     C                   ENDIF
     **
     C                   ENDSR
     *** BEG ADD ***** 10/29/04 **********************************
      *===========================================================
      *   SRZERA -  ZERO OUT BUCKETS IF AMOUNT DUE = ZERO
      *===========================================================
     C     SRZERA        BEGSR
     C                   CLEAR                   XXCURR
     C                   CLEAR                   XX30DY
     C                   CLEAR                   XX60DY
     C                   CLEAR                   XX90DY
      *** BEG ADD ***** 12/18/08 *****************************************
     C                   CLEAR                   X120DY
     C                   CLEAR                   X150DY
     C                   CLEAR                   X180DY
      *** END ADD ***** 12/18/08 *****************************************
     C                   ENDSR
     *** END ADD ***** 10/29/04 **********************************
     **
     *************************************************************
     **   SRPAST - SUBROUTINE TO CALCULATE A DATE IN THE PAST    *
     *************************************************************
     C     SRPAST        BEGSR
     **
     C                   Z-ADD     XXDAYS        SRPST             5 0
      *
     C                   MOVE      XXRNDT        SRDATE            8 0
      *
     C                   CALL      'SRPAST8'
     C                   PARM                    SRDATE
     C                   PARM                    SRPST
     C                   PARM      *BLANK        SRERR             1
     C                   PARM      *BLANK        SRTERM            1
      *
     C                   ENDSR
     **
      ***  BEGIN ADD  ***  08/20/98  **********************************
     *************************************************************
     **   AGADDR - SUBROUTINE TO LOAD AGENT ADDRESS              *
     *************************************************************
     C     AGADDR        BEGSR
     C*
     C                   MOVE      *BLANKS       X2ADD1           40
     C                   MOVE      *BLANKS       X2CSTZ           40
      ***** BEG CHG ******* 11/29/01 **********************************
     C*********            Z-ADDEBAGT#    XXAGT#
     C                   Z-ADD     HLDAGT        XXAGT#
      ***** END CHG ******* 11/29/01 **********************************
     C*
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     AAFEIN        XXFEIN
     C                   Z-ADD     AAMOD         XXMOD
     C*
     C     AGKY          CHAIN     PAG01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C     AGMAD1        IFNE      *BLANK
     C                   MOVEL     AGMAD1        X2ADD1
     C                   MOVEL     AGMZIP        X2ZIP1            5
     C                   MOVE      AGMZIP        X2ZIP2            4
     C     AGMCTY        CAT       ',':0         X2CSTZ
     C                   CAT       AGMST:1       X2CSTZ
     C                   CAT       X2ZIP1:1      X2CSTZ
     C                   CAT       '-':0         X2CSTZ
     C                   CAT       X2ZIP2:0      X2CSTZ
     C*
     C                   ELSE
     C                   MOVEL     AGPAD1        X2ADD1
     C                   MOVEL     AGPZIP        X2ZIP1
     C                   MOVE      AGPZIP        X2ZIP2
     C     AGPCTY        CAT       ',':0         X2CSTZ
     C                   CAT       AGPST:1       X2CSTZ
     C                   CAT       X2ZIP1:1      X2CSTZ
     C                   CAT       '-':0         X2CSTZ
     C                   CAT       X2ZIP2:0      X2CSTZ
     C*
     C                   ENDIF                                                  AGMAD1<>*BLANK
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  AGADDR
      ***  END   ADD  ***  08/20/98  **********************************
      **** BEGIN ADD **** 01/18/00 ************************************
     C*================================================================
     C* FILE - SUBROUTINE TO WRITE TO FILE WFAGP
     C*================================================================
     CSR   FILE          BEGSR
     C*
     C                   CLEAR                   PAG02
     C     ARCFLG        IFEQ      *ON
     C                   CLEAR                   PAG03
     C                   ENDIF
     C*
     C                   Z-ADD     RSCO#         AGCO#
     C                   Z-ADD     XXFND         AGFND
     C                   Z-ADD     XXEMP#        AGEMP#
     C                   Z-ADD     EBDIV         AGDIV
     C                   Z-ADD     EBFYR         AGFYR
     C                   MOVEL     XXENAM        AGENAM
     C                   Z-ADD     EBAGT#        AGAGT#
     C                   Z-ADD     XXEFDT        AGEFDT
     C                   Z-ADD     XXCXDT        AGCXDT
     C                   Z-ADD     XXCURR        AGCURR
     C                   Z-ADD     XX30DY        AG30DY
     C                   Z-ADD     XX60DY        AG60DY
     C                   Z-ADD     XX90DY        AG90DY
     C                   Z-ADD     X120DY        AG120D
     C                   Z-ADD     X150DY        AG150D
     C                   Z-ADD     X180DY        AG180D
     C                   Z-ADD     XXAMTD        AGAMTD
     C   71              MOVE      'A'           AGAUDF
     C                   MOVEL     Q1PGM         AGPGM
     C                   MOVEL     Q1USER        AGUSER
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AGDT
     C                   Z-ADD     DSTIME        AGTIME
     C                   MOVEL     CONAME        AGCNAM
     C                   MOVEL     FNNAME        AGGNAM
     C                   MOVEL     XXEFDT        AGEFD2
     C                   MOVEL     XXCXDT        AGCXD2
     C                   MOVE      QQESSR        AGESSR
     C                   MOVE      QQACST        AGACST
     C                   MOVE      QQCRDB        AGCRDB
     C                   Z-ADD     XXDEFR        AGDEFR
     C     XXAMTD        ADD       XXDEFR        AGTLDU
     C                   MOVE      EBJRST        AGRST
     C                   MOVE      EBCHAP        AGCHAP
     C     CPKY          CHAIN     PCP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CPDESC        AGCDES
     C                   ENDIF
     C                   Z-ADD     EBRGN#        AGRGN#
     C     REKY          CHAIN     PRE01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     REDESC        AGRDES
     C                   ENDIF
     C                   MOVE      RSFRDT        AGPEFF
     C                   MOVE      RSTODT        AGPEXP
     C                   MOVE      RSBLDT        AGBALD
     C                   MOVE      DSDATE        AGLDT
     C                   Z-ADD     XX#DAY        AGCI#D
      *** beg add 06/15/16 **********************************
      /free
       WrtRptCriteria();
      /end-free
      *** end add 06/15/16 **********************************
     C                   WRITE     PAG02
     C*
     C     ARCFLG        IFEQ      *ON
     C                   WRITE     PAG03
     C                   ENDIF
     C*
     C                   ENDSR                                                  FILE
     C*====================================================================
     C* GRPINF - Process Group info
     C*====================================================================
     CSR   GRPINF        BEGSR
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      *ON           GRPFND            1
     C*
     C* USE ALTERNATE FUND NAME IF FOUND IN FUND MASTER
     C*
     C                   MOVEL     FNNAME        XXFNAM
     C                   SELECT
     C     FNANAM        WHENNE    *BLANKS
     C                   MOVEL(P)  FNANAM        XXCONM
     C                   OTHER
     C                   MOVEL(P)  CONAME        XXCONM
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*   CENTER COMPANY NAME IN FIELD
     C*
     C                   MOVEL(P)  XXCONM        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        XXCONM           40
     C*
     C*   GET THE FUND AUXILIARY FILE
     C*
     C     F2KEY         CHAIN     PF201                              60
     C*
     C*   LOAD FUND ADDRESS, CITY, STATE, & ZIP
     C*
     C                   MOVEL(P)  FNMAD1        XFMAD1
     C                   MOVEL(P)  FNMAD2        XFMAD2
     C                   MOVEL(P)  FNMCTY        XXCITY           25
     C                   MOVEL     FNMST         XXSTAT            2
     C                   MOVEL     FNMZIP        XXZIP1            5
     C                   MOVE      FNMZIP        XXZIP2            4
     C*
     C                   EXSR      SRCSTZ
     C                   MOVEL     XXCSTZ        XFMAD3
     C*
     C     FNMAD2        IFEQ      *BLANK
     C                   MOVEL     XFMAD3        XFMAD2
     C                   MOVEL     *BLANK        XFMAD3
     C                   ENDIF
     C*
     C*  MOVE CORRECT FUND PHONE NUMBER TO WORK FIELDS
     C*
     C     FNPH1         IFEQ      *ZERO
     C                   MOVE      FNAREA        XXAREA            3
     C                   MOVEL     FNPH2         XXPHN1            3
     C                   MOVE      FNPH2         XXPHN2            4
     C                   ELSE
     C     FNPH1A        IFNE      *ZERO
     C                   MOVE      FNPH1A        XXAREA
     C                   ELSE
     C                   MOVE      800           XXAREA
     C                   ENDIF
     C                   MOVEL     FNPH1         XXPHN1
     C                   MOVE      FNPH1         XXPHN2
     C                   ENDIF
     C*
     C*  BUILD UP CONTACT USER NAME & PHONE NUMBER
     C*
     C                   MOVEL(P)  F2UNM1        XXNAME           49
     C                   CAT       ', (':0       XXNAME
     C                   CAT       XXAREA:0      XXNAME
     C                   CAT       ')':0         XXNAME
     C                   CAT       XXPHN1:1      XXNAME
     C                   CAT       '-':0         XXNAME
     C                   CAT       XXPHN2:0      XXNAME
     C                   CAT       '.':0         XXNAME
     C*
     C                   MOVEL(P)  F2UNM2        XXNAM2           49
     C                   CAT       ', (':0       XXNAM2
     C                   CAT       XXAREA:0      XXNAM2
     C                   CAT       ')':0         XXNAM2
     C                   CAT       XXPHN1:1      XXNAM2
     C                   CAT       '-':0         XXNAM2
     C                   CAT       XXPHN2:0      XXNAM2
     C                   CAT       '.':0         XXNAM2
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* GRPTTL - Totals for Groups
     C*====================================================================
     CSR   GRPTTL        BEGSR

      *** beg add ***** 06/15/16 *****************
      /free
       If isUseXml and not(isUseSpoolOut);
          LeaveSr;
       Endif;
      /end-free
      *** end add ***** 06/15/16 *****************

     C     XXFLAG        IFEQ      'Y'
     C     ARCFLG        ANDEQ     *OFF
     C*    IF SORT BY AGENT THEN START NEW PAGE FOR GRAND TOTALS
     C     AGTSRT        IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    GRPHD
     C                   EXCEPT    HEAD2
     C                   ENDIF
     C                   EXCEPT    GRTOTL
     C                   ENDIF
     C*
     C                   ADD       GRTEMP        TTLEMP            5 0
     C                   ADD       GRCUTL        XXCUTL           11 2
     C                   ADD       GR60TL        XX60TL           11 2
     C                   ADD       GRFTOT        XXFTOT           11 2
     C                   ADD       GR30TL        XX30TL           11 2
     C                   ADD       GR90TL        XX90TL           11 2
     C                   ADD       GROV90        OVER90           11 2
     C                   ADD       GRTDEF        TOTDEF           11 2
     C                   ADD       GRTLDU        TOTLDU           11 2
     C*
     C                   CLEAR                   GRTEMP
     C                   CLEAR                   GRCUTL
     C                   CLEAR                   GR60TL
     C                   CLEAR                   GRFTOT
     C                   CLEAR                   GR30TL
     C                   CLEAR                   GR90TL
     C                   CLEAR                   GROV90
     C                   CLEAR                   GRTDEF
     C                   CLEAR                   GRTLDU
     C*
     C                   MOVE      'N'           XXFLAG
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* LDNOGP - Load Groups for final page(s)
     C*====================================================================
     CSR   LDNOGP        BEGSR
     C*
     C                   Z-ADD     FNFND         G                 4 0
     C     G             IFNE      *ZERO
     C                   Z-ADD     FNFND         GP(G)
     C                   MOVEL     FNNAME        GPN(G)
     C                   ENDIF

      /free
       Clear pag02;
       AGco#   = FNco#;
       AGfnd   = FNfnd;
       AGcnam  = COname;
       AGgnam  = FNname;
       AGxmlFile = xmlFile;
       AGNoGrp = 'Y';
       Write pag02;
      /end-free

     C                   ENDSR
     C*
     C*====================================================================
     C* PRNOGP - Print Groups selected but not used
     C*====================================================================
     CSR   PRNOGP        BEGSR

      *** beg add ***** 06/15/16 *****************
      /free
       If isUseXml and not(isUseSpoolOut);
          LeaveSr;
       Endif;
      /end-free
      *** end add ***** 06/15/16 *****************

     C     1             DO        999           G
     C     GP(G)         IFNE      *ZERO
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C                   EXCEPT    NOGRP1
     C                   ENDIF
     C                   EXCEPT    NOGRP2
     C                   ENDIF
     C                   ENDDO
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* NEWGRP - Processing for a new group
     C*====================================================================
     CSR   NEWGRP        BEGSR
     C*
     C     USEGSL        IFEQ      *ON
     C*
     C     *IN50         DOUEQ     *OFF
     C     LSTGRP        OREQ      *ON
     C                   EXSR      NXTGRP
     C     EBKEY         SETLL     PEB01
     C     EBKEY         READE     PEB01                                  50
     C     *IN50         IFEQ      *ON
     C                   EXSR      LDNOGP
     C                   ENDIF
     C                   ENDDO
     C*
     C     LSTGRP        IFEQ      *ON
     C                   MOVE      *ON           *IN50
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*====================================================================
     C* NXTGRP - Process the next group
     C*====================================================================
     CSR   NXTGRP        BEGSR
     C*
     C     XXFND         IFNE      *ZERO
     C                   EXSR      GRPTTL
     C                   MOVE      *ON           *INOA
     C                   ENDIF
     C*
     C     FNKEY         SETGT     PGS00
     C     XXCO#         READE     PGS00                                  50
     C*
     C     *IN50         DOWEQ     *OFF
     C     GSIEFL        ANDNE     'I'
     C     XXCO#         READE     PGS00                                  50
     C                   ENDDO
     C*
     C                   SELECT
     C     *IN50         WHENEQ    *OFF
     C                   MOVE      *OFF          LSTGRP
     C                   Z-ADD     GSFND         XXFND
     C                   EXSR      GRPINF
     C     *IN50         WHENEQ    *ON
     C                   MOVE      *ON           LSTGRP            1
     C                   ENDSL
     C*
     C                   ENDSR
     C***  End   Add  ***  08/16/06  **************************************
     C*
      **** BEGIN ADD **** 08/24/00 *********************************************
     C     TOTAL         BEGSR

      *** beg add ***** 06/15/16 *****************
      /free
       If isUseXml and not(isUseSpoolOut);
          LeaveSr;
       Endif;
      /end-free
      *** end add ***** 06/15/16 *****************

     C     XX90DY        ADD       X120DY        XP90DY
     C                   ADD       X150DY        XP90DY
     C                   ADD       X180DY        XP90DY
     C                   ADD       XXCURR        GRCUTL
     C                   ADD       XX30DY        GR30TL
     C                   ADD       XX60DY        GR60TL
     C                   ADD       XP90DY        GR90TL
     C                   ADD       XXAMTD        GRFTOT
     C                   ADD       XXDEFR        GRTDEF
     C     GRFTOT        ADD       GRTDEF        GRTLDU
      *
     C     XP90DY        IFNE      *ZERO
     C                   ADD       XXAMTD        GROV90
     C                   ENDIF                                                  XP90DY
      *
      *  ACCUMULATE AGENT TOTALS BUCKETS
      *
     C                   ADD       XXCURR        A1CUTL
     C                   ADD       XX30DY        A130TL
     C                   ADD       XX60DY        A160TL
     C                   ADD       XP90DY        A190TL
     C                   ADD       XXAMTD        A1FTOT
      *
     C     XP90DY        IFNE      *ZERO
     C                   ADD       XXAMTD        A1OV90
     C                   ENDIF                                                  XP90DY
      *
      *
     C                   ENDSR                                                  TOTAL
      *==============================================================
      * SRHEAD - Print header at top of new page
      *==============================================================
     CSR   SRHEAD        BEGSR
      * zzz
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEAD
     C***  Begin Add  ***  08/16/06  **************************************
     C                   EXCEPT    GRPHD
     C***  End   Add  ***  08/16/06  **************************************
      *
     C     AGTSRT        IFEQ      *ON
     C                   EXSR      AGADDR                                       GET AGT ADDR
     C                   EXCEPT    AGTHD                                        AGT HEADER
     C                   ENDIF                                                  AGTSRT
      *
     C                   EXCEPT    HEAD2
     C                   MOVE      *OFF          *INOA
     C                   ENDIF                                                  *INOA = *ON
      *
     C                   ENDSR                                                  SRHEAD
      *
     C***  Begin Add  ***  08/16/06  **************************************
     C*====================================================================
     C* WRITEB - Write records to WXEBP
     C*====================================================================
     CSR   WRITEB        BEGSR
     C*
     C                   SELECT
     C     QQTOEA        WHENNE    *BLANKS
     C                   MOVE      'EA'          QQSEL1
     C                   MOVE      QQFREA        QQS1FR
     C                   MOVE      QQTOEA        QQS1TO
     C     QQTOEN        WHENNE    *ZEROS
     C                   MOVE      'EN'          QQSEL1
     C                   MOVE      QQFREN        DSEMP#
     C                   MOVE      '00000'       DSDIV
     C                   MOVE      DSEMDV        QQS1FR
     C                   MOVE      QQTOEN        DSEMP#
     C                   MOVE      '99999'       DSDIV
     C                   MOVE      DSEMDV        QQS1TO
     C     QQTOAN        WHENNE    *ZEROS
     C                   MOVE      'AN'          QQSEL1
     C                   MOVE      QQFRAN        QQS1FR
     C                   MOVE      QQTOAN        QQS1TO
     C     QQTOAA        WHENNE    *BLANKS
     C                   MOVE      'AA'          QQSEL1
     C                   MOVEL     QQFRAA        QQS1FR
     C                   MOVEL     QQTOAA        QQS1TO
     C                   OTHER
     C                   MOVE      *BLANKS       QQSEL1
     C                   MOVE      *BLANKS       QQS1FR
     C                   MOVE      *BLANKS       QQS1TO
     C                   ENDSL
     C*
     C                   SELECT
     C     QQTOAN        WHENNE    *ZEROS
     C     QQSEL1        ANDNE     *BLANKS
     C                   MOVE      'AN'          QQSEL2
     C                   MOVE      QQFRAN        QQS2FR
     C                   MOVE      QQTOAN        QQS2TO
     C     QQTOAA        WHENNE    *BLANKS
     C     QQSEL1        ANDNE     *BLANKS
     C                   MOVE      'AA'          QQSEL2
     C                   MOVEL     QQFRAA        QQS2FR
     C                   MOVEL     QQTOAA        QQS2TO
     C                   OTHER
     C                   MOVE      *BLANKS       QQSEL2
     C                   MOVE      *BLANKS       QQS2FR
     C                   MOVE      *BLANKS       QQS2TO
     C                   ENDSL
     C*
     C                   CALL      'SRRPTR'
     C                   PARM                    QQCO#
     C                   PARM                    QQFRDT
     C                   PARM                    QQTODT
     C                   PARM                    QQBLDT
     C                   PARM                    QQACST
     C                   PARM                    QQESSR
     C                   PARM      'E'           QQSTAT            1
     C                   PARM                    QQSRT1
     C                   PARM                    QQSRT2
     C                   PARM      'M'           QQEMPF            1
     C                   PARM      'M'           QQAMPF            1
     C                   PARM      '  '          QQPRTY            2
     C                   PARM      *ZEROS        QQEMP#            9
     C                   PARM      *ZEROS        QQDIV             5
     C                   PARM      *ZEROS        QQFYR             3
     C                   PARM                    QQSEL1            2
     C                   PARM                    QQS1FR           15
     C                   PARM                    QQS1TO           15
     C                   PARM                    QQSEL2            2
     C                   PARM                    QQS2FR           15
     C                   PARM                    QQS2TO           15
     C*
     C                   ENDSR                                                  SRHEAD
     C***  End   Add  ***  08/16/06  **************************************
      *
      ***** END ADD ******* 11/29/01 **********************************
     **                                                                *
     O*****************************************************************
     O*** BEG CHG ***** 09/07/04 **************************************
     O*********** QSYSPRT1E  102           ROUTE
     OQSYSPRT2  E            ROUTE          1 02
     O*** END CHG ***** 09/07/04 **************************************
      ***** Beg Del ***** 08/26/14 ************************************
     O********                                    8 'XXXXXXXX'
     O********                                   20 'XXXXXX'
     O********                                   34 'XX      XX'
     O********                                   45 'XXXXXXXXXX'
     O********                                   56 'XXXXXXXXXX'
     O********                                   67 'XXX     XX'
     O********                                   77 'XXXXXX'
     O********  E            ROUTE          1
     O********                                    9 'XXXXXXXXX'
     O********                                   21 'XXXXXXXX'
     O********                                   34 'XX      XX'
     O********                                   45 'XXXXXXXXXX'
     O********                                   56 'XXXXXXXXXX'
     O********                                   67 'XXXX    XX'
     O********                                   78 'XXXXXXXX'
     O********  E            ROUTE          1
     O********                                   10 'XX      XX'
     O********                                   22 'XX      XX'
     O********                                   34 'XX      XX'
     O********                                   41 'XX'
     O********                                   52 'XX'
     O********                                   67 'XXXX    XX'
     O********                                   79 'XX      XX'
     O********  E            ROUTE          1
     O********                                   10 'XX      XX'
     O********                                   22 'XX      XX'
     O********                                   34 'XX      XX'
     O********                                   41 'XX'
     O********                                   52 'XX'
     O********                                   67 'XX XX   XX'
     O********                                   71 'XX'
     O********  E            ROUTE          1
     O********                                    9 'XXXXXXXXX'
     O********                                   22 'XX      XX'
     O********                                   34 'XX      XX'
     O********                                   41 'XX'
     O********                                   52 'XX'
     O********                                   67 'XX XX   XX'
     O********                                   71 'XX'
     O********  E            ROUTE          1
     O********                                    8 'XXXXXXXX'
     O********                                   22 'XX      XX'
     O********                                   34 'XX      XX'
     O********                                   41 'XX'
     O********                                   52 'XX'
     O********                                   67 'XX  XX  XX'
     O********                                   71 'XX'
     O********  E            ROUTE          1
     O********                                    4 'XXXX'
     O********                                   22 'XX      XX'
     O********                                   34 'XX      XX'
     O********                                   41 'XX'
     O********                                   52 'XX'
     O********                                   67 'XX  XX  XX'
     O********                                   79 'XX    XXXX'
     O********  E            ROUTE          1
     O********                                    5 'XXXXX'
     O********                                   22 'XX      XX'
     O********                                   34 'XX      XX'
     O********                                   41 'XX'
     O********                                   52 'XX'
     O********                                   67 'XX   XX XX'
     O********                                   79 'XX    XXXX'
     O********  E            ROUTE          1
     O********                                    6 'XX XXX'
     O********                                   22 'XX      XX'
     O********                                   34 'XX      XX'
     O********                                   41 'XX'
     O********                                   52 'XX'
     O********                                   67 'XX   XX XX'
     O********                                   79 'XX      XX'
     O********  E            ROUTE          1
     O********                                    7 'XX  XXX'
     O********                                   22 'XX      XX'
     O********                                   34 'XX      XX'
     O********                                   41 'XX'
     O********                                   52 'XX'
     O********                                   67 'XX    XXXX'
     O********                                   79 'XX      XX'
     O********  E            ROUTE          1
     O********                                    8 'XX   XXX'
     O********                                   21 'XXXXXXXX'
     O********                                   33 'XXXXXXXX'
     O********                                   41 'XX'
     O********                                   56 'XXXXXXXXXX'
     O********                                   67 'XX    XXXX'
     O********                                   78 'XXXXXXXX'
     O********  E            ROUTE          2
     O********                                    9 'XX    XXX'
     O********                                   20 'XXXXXX'
     O********                                   32 'XXXXXX'
     O********                                   41 'XX'
     O********                                   56 'XXXXXXXXXX'
     O********                                   67 'XX     XXX'
     O********                                   77 'XXXXXX'
     O********  E            ROUTE          1
     O********                                   15 'ROUTE TO:'
     O********       08      ORDIS1              60
     O********          09   UPUSNM              67
     O********  E            ROUTE          1
     O********       08      ORDIS2              60
     O********  E            ROUTE          1
     O********       08      ORDIS3              60
     O********  E            ROUTE          2
     O********       08      ORDIS4              60
      ***** End Del ***** 08/26/14 *******************************************
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
     O          E            ROUTE          1
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
      ***** Beg Del ***** 08/26/14 *******************************************
     O********  E            ROUTE          1
     O********                                   19 'PROGRAM NAME:'
     O********               ORPNAM              30
     O********                                   36 'FILE:'
     O********               ORFIL#              38
     O********                                   46 'COPIES:'
     O********               ORCOPY        Z     50
     O********                                   56 'HOLD:'
     O********               ORHOLD              61
     O********  E            ROUTE          2
     O********                                   19 'OUTPUT QUEUE:'
     O********       08      OROUTQ              30
     O********          09   QQQUE               30
     O********                                   37 'FORMS:'
     O********               ORFORM              48
     O********                                   56 'SAVE:'
     O********               ORSAVE              61
      ***** End Del ***** 08/26/14 *******************************************
     O**   SELECTION CRITERIA
     O          E            ROUTE          1
     O                                           20 'COMPANY      :'
     O                       RSCO#         Z     24
     O***  Begin Chg  ***  08/16/06  **************************************
     O********E* 1             ROUTE
     O          E    83      ROUTE          1
     O***  End   Chg  ***  08/16/06  **************************************
     O                                           20 'GROUP        :'
     O                       RSFND         Z     24
     O***  Beg Chg  ***  01/14/16  **************************************
     O***       E            ROUTE          1
     O***  Beg Chg  ***  08/01/16  **************************************
     O***       E    83      ROUTE          1
     O          E   N83      ROUTE          1
     O***  End Chg  ***  08/01/16  **************************************
     O***  End Chg  ***  01/14/16  **************************************
     O                                           27 'POLICY EFF DT RANGE :'
     O                       RSFRDT              38 '  /  /    '
     O                                         +001 '-'
     O                       RSTODT            +001 '  /  /    '
     O          E            ROUTE          1
     O                                           27 'BALANCE AS OF       :'
     O                       RSBLDT              38 '  /  /    '
     O          E            ROUTE          1
     O                                           27 'PRIMARY SORT        :'
     O                       RSSRT1              41
     O          E            ROUTE          1
     O                                           27 'SECONDARY SORT      :'
     O                       RSSRT2              41
     O          E            ROUTE          1
     O                                           27 'AGT # RANGE         :'
     O                       RSFRAN        Z     38
     O                                           40 '-'
     O                       RSTOAN        Z     51
     O          E            ROUTE          1
     O                                           27 'AGENT NAME RANGE    :'
     O                       RSFRAA              38
     O                                           40 '-'
     O                       RSTOAA              51
     O          E            ROUTE          1
     O                                           27 'EMPLOYER NAME RANGE :'
     O                       RSFREA              38
     O                                           40 '-'
     O                       RSTOEA              51
     O          E            ROUTE          1
     O                                           27 'EMPLOYER # RANGE    :'
     O                       RSFREN        Z     38
     O                                           40 '-'
     O                       RSTOEN        Z     51
     O          E            ROUTE          1
     O                                           27 'ACCOUNT STATUS      :'
     O                       RSACT               53
     O          E            ROUTE          1
     O                                           27 'AUDIT STATUS        :'
     O                       RSAUST              54
     O          E            ROUTE          1
     O                                           27 'ACCOUNT TYPE        :'
     O                       RSESSR              61
     O          E            ROUTE          1
     O                                           27 'REPORT TYPE         :'
     O                       RSCRDB              61
      **** BEGIN ADD **** 01/18/00 ************************************
     O          E            ROUTE          1
     O                                           27 'CREDIT MINIMUM      :'
     O                       XXMINC        J   +001
     O          E            ROUTE          1
     O                                           27 'DEBIT MINIMUM       :'
     O                       XXMIND        J   +001
      **** END   ADD **** 01/18/00 ************************************
      **** BEG ADD ****** 12/07/04 ************************************
     O          E            ROUTE          1
     O                                           27 'BILLING TYPE PLAN   :'
     O                       XXBTYP            +001
      **** END ADD ****** 12/07/04 ************************************
     O          E            ROUTE          1
     O               51                          30 'A/R CODES INCLUDED IN'
     O               51                          42 'CALCULATION'
     O              N51                          28 'A/R CODES EXCLUDED '
     O              N51                          44 'FROM CALCULATION'
     O          E      N20   ROUTE          1
     O                       RSCODE              26
     O          E            ROUTE          1
     O                  20                       11 '1.'
     O                       CDE(1)              14
     O                       DES(1)              45
     O                  30                       50 '11.'
     O                       CDE(11)             53
     O                       DES(11)             84
     O                  40                       89 '21.'
     O                       CDE(21)             92
     O                       DES(21)            123
     O          E            ROUTE          1
     O                  21                       11 '2.'
     O                       CDE(2)              14
     O                       DES(2)              45
     O                  31                       50 '12.'
     O                       CDE(12)             53
     O                       DES(12)             84
     O                  41                       89 '22.'
     O                       CDE(22)             92
     O                       DES(22)            123
     O          E            ROUTE          1
     O                  22                       11 '3.'
     O                       CDE(3)              14
     O                       DES(3)              45
     O                  32                       50 '13.'
     O                       CDE(13)             53
     O                       DES(13)             84
     O                  42                       89 '23.'
     O                       CDE(23)             92
     O                       DES(23)            123
     O          E            ROUTE          1
     O                  23                       11 '4.'
     O                       CDE(4)              14
     O                       DES(4)              45
     O                  33                       50 '14.'
     O                       CDE(14)             53
     O                       DES(14)             84
     O                  43                       89 '24.'
     O                       CDE(24)             92
     O                       DES(24)            123
     O          E            ROUTE          1
     O                  24                       11 '5.'
     O                       CDE(5)              14
     O                       DES(5)              45
     O                  34                       50 '15.'
     O                       CDE(15)             53
     O                       DES(15)             84
     O                  44                       89 '25.'
     O                       CDE(25)             92
     O                       DES(25)            123
     O          E            ROUTE          1
     O                  25                       11 '6.'
     O                       CDE(6)              14
     O                       DES(6)              45
     O                  35                       50 '16.'
     O                       CDE(16)             53
     O                       DES(16)             84
     O                  45                       89 '26.'
     O                       CDE(26)             92
     O                       DES(26)            123
     O          E            ROUTE          1
     O                  26                       11 '7.'
     O                       CDE(7)              14
     O                       DES(7)              45
     O                  36                       50 '17.'
     O                       CDE(17)             53
     O                       DES(17)             84
     O                  46                       89 '27.'
     O                       CDE(27)             92
     O                       DES(27)            123
     O          E            ROUTE          1
     O                  27                       11 '8.'
     O                       CDE(8)              14
     O                       DES(8)              45
     O                  37                       50 '18.'
     O                       CDE(18)             53
     O                       DES(18)             84
     O                  47                       89 '28.'
     O                       CDE(28)             92
     O                       DES(28)            123
     O          E            ROUTE          1
     O                  28                       11 '9.'
     O                       CDE(9)              14
     O                       DES(9)              45
     O                  38                       50 '19.'
     O                       CDE(19)             53
     O                       DES(19)             84
     O                  48                       89 '29.'
     O                       CDE(29)             92
     O                       DES(29)            123
     O          E            ROUTE          1
     O                  29                       11 '10.'
     O                       CDE(10)             14
     O                       DES(10)             45
     O                  39                       50 '20.'
     O                       CDE(20)             53
     O                       DES(20)             84
     O                  49                       89 '30.'
     O                       CDE(30)             92
     O                       DES(30)            123
      **** BEGIN ADD **** 05/15/02 *********************************************
     O          E            ROUTE          1
     O               59                          32 'CATEGORIES INCLUDED IN '
     O               59                          43 'CALCULATION'
     O              N59                          33 'CATEGORIES EXCLUDED FRO'
     O              N59                          46 'M CALCULATION'
     O          E       59   ROUTE          1
     O                       RSCTC               26
     O          E            ROUTE          1
     O                  52                        6 '1.'
     O                       CT(1)                9
     O                       CTS(1)              40
     O                  62                       45 '6.'
     O                       CT(6)               48
     O                       CTS(6)              79
     O          E            ROUTE          1
     O                  53                        6 '2.'
     O                       CT(2)                9
     O                       CTS(2)              40
     O                  63                       45 '7.'
     O                       CT(7)               48
     O                       CTS(7)              79
     O          E            ROUTE          1
     O                  54                        6 '3.'
     O                       CT(3)                9
     O                       CTS(3)              40
     O                  64                       45 '8.'
     O                       CT(8)               48
     O                       CTS(8)              79
     O          E            ROUTE          1
     O                  55                        6 '4.'
     O                       CT(4)                9
     O                       CTS(4)              40
     O                  65                       45 '9.'
     O                       CT(9)               48
     O                       CTS(9)              79
     O          E            ROUTE          1
     O                  56                        6 '5.'
     O                       CT(5)                9
     O                       CTS(5)              40
     O                  66                       45 '10.'
     O                       CT(10)              48
     O                       CTS(10)             79
      **** END ADD ****** 05/15/02 *********************************************
      **** BEGIN ADD **** 11/16/00 *********************************************
     O          E            ROUTE          1
     O               14                          32 'ACTIVITY CODES INCLUDED'
     O               14                          47 'IN CALCULATION'
     O              N14                          33 'ACTIVITY CODES EXCLUDED'
     O              N14                          50 'FROM CALCULATION'
     O          E       14   ROUTE          1
     O                       RSASTC              26
     O          E            ROUTE          1
     O                  15                        6 '1.'
     O                       AS(1)                9
     O                       ASDS(1)             40
     O                  18                       45 '4.'
     O                       AS(4)               48
     O                       ASDS(4)             79
     O          E            ROUTE          1
     O                  16                        6 '2.'
     O                       AS(2)                9
     O                       ASDS(2)             40
     O                  19                       45 '5.'
     O                       AS(5)               48
     O                       ASDS(5)             79
     O          E            ROUTE          1
     O                  17                        6 '3.'
     O                       AS(3)                9
     O                       ASDS(3)             40
      **** END   ADD **** 11/16/00 *********************************************
      ***** BEG CHG ******* 08/23/01 **********************************
     O******  E  160           ROUTE
     O          E            ROUTE       1
      ***** END CHG ******* 08/23/01 **********************************
      ****** BEG DLT ***** 01/28/10 **********************************
     O***                                23 '(C) Copyright Fiserv'
     O***                              +000 ', Inc.,'
     O***                      *YEAR   +001
      ****** END DLT ***** 01/28/10 **********************************
      ****** BEG ADD ***** 01/28/10 **********************************
      ****** BEG CHG ***** 10/11/11 **********************************
     O***************        E  159           ROUTE
     O          E            ROUTE          1
      ****** END CHG ***** 10/11/11 **********************************
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END ADD ***** 01/28/10 **********************************
     O****************************************************************
     O* REPORT HEADING
     O****************************************************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
      ***** BEG CHG ******* 11/29/01 **********************************
     O***************          XXCONM   118
     O                       XXCONM             120
      ***** END CHG ******* 11/29/01 **********************************
     O                       DSDATE        Y    196
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL             130
     O                                          191 'PAGE:'
     O                       PAGE          Z    196
     O          E            HEAD           1
      ***** Beg Chg * 01/14/16
     O*                                          70 'POLICY EFF DATE RANGE:'
     O*                      RSFRDT            +001 '  /  /    '
     O*                                        +001 '-'
     O*                      RSTODT            +001 '  /  /    '
     O***  Beg Chg  ***  08/01/16  **************************************
     O****           83                          70 'POLICY EFF DATE RANGE:'
     O****           83      RSFRDT            +001 '  /  /    '
     O****           83                        +001 '-'
     O****           83      RSTODT            +001 '  /  /    '
     O              N83                          70 'POLICY EFF DATE RANGE:'
     O              N83      RSFRDT            +001 '  /  /    '
     O              N83                        +001 '-'
     O              N83      RSTODT            +001 '  /  /    '
     O***  End Chg  ***  08/01/16  **************************************
      ***** End Chg * 01/14/16
     O                                          113 'BALANCE DATE:'
     O                       RSBLDT            +001 '  /  /    '
     ** SHOW REPORT SELECTION CRITERIA AND IF A/R CODES SELECTED
     O          E            HEAD           1
     O                                           63 'ACCOUNT STATUS:'
     O                       RSACT             +001
     O                                          113 'AUDIT STATUS:'
     O                       RSAUST            +001
     O          E            HEAD           1
     O                                           61 'ACCOUNT TYPE:'
     O                       RSESSR            +001
     O                                          112 'REPORT TYPE:'
     O                       RSCRDB            +001
      **** BEGIN ADD **** 01/18/00 ************************************
     O          E            HEAD           1
     O                                           63 'MINIMUM CREDIT:'
     O                       XXMINC        J   +001
     O                                          114 'MINIMUM DEBIT:'
     O                       XXMIND        J   +001
      **** END   ADD **** 01/18/00 ************************************
     O          E            HEAD           1
     O               99                          80 'NOTE: SPECIFIC A/R CODES'
     O               99                             ' WERE SELECTED'
     O***  Begin Chg  ***  08/16/06  **************************************
     O********E* 2             HEAD
     O          E            GRPHD          2
     O***  End   Chg  ***  08/16/06  **************************************
     O                                           06 'GROUP:'
     O                       FNFND         Z     10
     O                       FNNAME              72
     O*
     O***  BEGIN ADD  ***  08/20/98  **********************************
     O          E            AGTHD       1  1
     O                                           06 'AGENT:'
     O                       EBAGT#        Z     12
     O                       AGNAME              53
     O          E            AGTHD          1
     O                       X2ADD1              53
     O          E            AGTHD          2
     O                       X2CSTZ              53
     O***  END   ADD  ***  08/20/98  **********************************
     O*
     O***  BEGIN CHG  ***  08/20/98  **********************************
     O******* E  1             HEAD
     O          E            HEAD2          1
     O***  END   CHG  ***  08/20/98  **********************************
     O                                           20 'POL'
      *** BEG ADD ***** 10/19/05 **************************************
     O                                           96 'CURR INST'
      *** END ADD ***** 10/19/05 **************************************
     O                                          110 '0 - 30 DAYS'
     O                                          127 '31 - 60 DAYS'
     O                                          144 '61 - 90 DAYS'
     O                                          161 'OVER 90 DAYS'
      *** BEG ADD ***** 09/07/04 **************************************
     O                                          176 'CURRENT  '
      *** END ADD ***** 09/07/04 **************************************
      ***** BEG ADD ******* 11/29/01 **********************************
      *
     O          E            HEAD2       0  0
     O                       LINE              +000
      ***** END ADD ******* 11/29/01 **********************************
      *
     O***  BEGIN CHG  ***  08/20/98  **********************************
     O******* E  1             HEAD
     O          E            HEAD2          1
     O***  END   CHG  ***  08/20/98  **********************************
     O                                           09 'EMPLOYER'
     O                                           15 'DIV'
     O                                           20 ' YR'
     O                                           26 'NAME'
     O                                           57 'AGENT'
     O***  BEGIN CHG  ***  01/12/99  **********************************
     O********************************** 67 'STATUS'
     O                                           69 'POL EFF DT'
     O                                           79 'STATUS'
      *** BEG ADD ***** 10/19/05 **************************************
     O                                           96 '# OF DAYS'
      *** END ADD ***** 10/19/05 **************************************
     O***  END   CHG  ***  01/12/99  **********************************
     O                                          127 'PAST DUE'
     O                                          144 'PAST DUE'
     O                                          161 'PAST DUE'
     O                                          176 'TOTAL DUE'
     O                                          190 'AUDITED'
      *
     O          E            DET            0
     O                       XXEMP#        Z     09
     O                       EBDIV         Z     15
     O                       EBFYR         Z     20
     O                       XXENAM              50
     O                       EBAGT#        Z     57
     O***  BEGIN CHG  ***  01/12/99  **********************************
     O******* E  0 *********** DET1
     O************************           61 'NE'
     O******* E  0 *********** DET2
     O************************           61 'CX'
     O************************ XXCXDTY   72
     O*
     O                       XXEFDT        Y     69
     O          E            DET1           0
     O                                           73 'NE'
     O          E            DET2           0
     O                                           73 'CX'
     O                       XXCXDT        Y     84
     O***  END   CHG  ***  01/12/99  **********************************
     O          E            DET3           1
     O***  BEG   ADD  ***  10/19/05  **********************************
     O                       XX#DAY        4     94
     O***  END   ADD  ***  10/19/05  **********************************
     O                       XXCURR        J    111
     O                       XX30DY        J    128
     O                       XX60DY        J    145
      ***** BEG CHG ******* 12/18/08 **********************************
     O*************            XX90DYJ  162
     O                       XP90DY        J    162
      ***** BEG CHG ******* 12/18/08 **********************************
     O                       XXAMTD        J    178
     O               71                         187 'A'
     O              N71                         187 ' '
     O***  BEGIN ADD  ***  08/20/98  **********************************
     O          E            AGTOTL      1  1
     O                                           67 'AGENT TOTALS:'
      ***** BEG DEL ******* 11/29/01 **********************************
     O*********                AGCUTLJ  111
     O*********                AG30TLJ  128
     O*********                AG60TLJ  145
     O*********                AG90TLJ  162
     O*********                AGFTOTJ  178
      ***** END DEL ******* 11/29/01 **********************************
      ***** BEG ADD ******* 11/29/01 **********************************
     O                       A1CUTL        JB   111
     O                       A130TL        JB   128
     O                       A160TL        JB   145
     O                       A190TL        JB   162
     O                       A1FTOT        JB   178
      ***** END ADD ******* 11/29/01 **********************************
     O          E            AGTOTL         1
     O                                           45 'AGENT'
     O                       HLDAGT        Z     51
     O                                           67 'EMPLOYER COUNT:'
     O                       AGTCNT        Z     71
     O                                          147 'TOTAL DUE OF ACCTS'
     O                                          161 'OVER 90 DAYS:'
      ***** BEG DEL ******* 11/29/01 **********************************
     O*********                AGOV90J  178
      ***** END DEL ******* 11/29/01 **********************************
      ***** BEG ADD ******* 11/29/01 **********************************
     O                       A1OV90        JB   178
      ***** END ADD ******* 11/29/01 **********************************
      *
     O***  Begin Add  ***  08/16/06  **************************************
     O          E            GRTOTL      1  0
     O                                           67 'GROUP EMPLOYERS:'
     O                       GRTEMP        J     75
     O          E            GRTOTL      1  1
     O                                           60 'GROUP'
     O                                           67 'TOTAL:'
     O                       GRCUTL        J    111
     O                       GR60TL        J    145
     O                       GRFTOT        J    178
     O          E            GRTOTL         2
     O                       GR30TL        J    128
     O                       GR90TL        J    162
     O          E            GRTOTL         1
     O                                          147 'TOTAL DUE OF ACCTS'
     O                                          161 'OVER 90 DAYS:'
     O                       GROV90        J    178
     O          E            GRTOTL         1
     O                                          147 'TOTAL DEFERRED DUE'
     O                                          161 '  . . . . . :'
     O                       GRTDEF        J    178
     O          E            GRTOTL         1
     O                                          147 'TOTAL AMOUNT DUE  '
     O                                          161 '. . . . . . :'
     O                       GRTLDU        J    178
     O***  End   Add  ***  08/16/06  **************************************
     O*
     O          E            LRTOTL      1  0
     O***  BEGIN CHG  ***  01/12/99  **********************************
     O********************************** 66 'GRAND'
     O                                           67 'TOTAL EMPLOYERS:'
     O                       TTLEMP        J     75
     O***  END   CHG  ***  01/12/99  **********************************
     O***  END   ADD  ***  08/20/98  **********************************
     O          E            LRTOTL      1  1
     O***  BEGIN ADD  ***  01/12/99  **********************************
     O                                           60 'GRAND'
     O***  END   ADD  ***  01/12/99  **********************************
     O                                           67 'TOTAL:'
     O                       XXCUTL        J    111
     O                       XX60TL        J    145
     O                       XXFTOT        J    178
     O          E            LRTOTL         2
     O                       XX30TL        J    128
     O                       XX90TL        J    162
     O          E            LRTOTL         1
     O                                          147 'TOTAL DUE OF ACCTS'
     O                                          161 'OVER 90 DAYS:'
     O                       OVER90        J    178
     O*** BEG ADD ***** 09/07/04 **************************************
     O          E            LRTOTL         1
     O                                          147 'TOTAL DEFERRED DUE'
     O                                          161 '  . . . . . :'
     O                       TOTDEF        J    178
     O          E            LRTOTL         1
     O                                          147 'TOTAL AMOUNT DUE  '
     O                                          161 '. . . . . . :'
     O                       TOTLDU        J    178
     O*** END ADD ***** 09/07/04 **************************************
     O***  Begin Add  ***  08/16/06  **************************************
     O          E            NOGRP1         1
     O                                         +  2 'The following groups'
     O                                         +  1 'were selected for this'
     O                                         +  1 'report but no data was'
     O                                         +  1 'generated:'
     O          E            NOGRP2         1
     O                       GP(G)         Z   +  2
     O                       GPN(G)            +  2
     O***  End   Add  ***  08/16/06  **************************************
      *** beg add ***** 06/15/16 **************************************
      *==============================================================
     p* Procedure name: WrtRptSelections
     p* Purpose:        Writes report parameters to workfile
     p* Returns:
     p WrtRptCriteria  b
     d WrtRptCriteria  pi
      /free
       AGxmlfile = xmlFile;
       AGrsfrdt = RSfrdt;
       AGrstodt = RStodt;
       AGrssrt1 = RSsrt1;
       AGrssrt2 = RSsrt2;
       AGrsfran = RSfran;
       AGrstoan = RStoan;
       AGrsfrea = RSfrea;
       AGrstoea = RStoea;
       AGrsfren = RSfren;
       AGrstoen = RStoen;
       AGrsact  = RSact;
       AGrsessr = RSessr;
       AGxxminc = xxMinc;
       AGxxmind = xxMind;
       If *in99;
          AGarcdex = 'NOTE: SPECIFIC A/R CODES WERE SELECTED';
       Endif;
       AGrsaust = RSaust;
       AGrscrdb = RScrdb;
       AGxxbtyp = xxBTyp;
       Select;
       When cde(1) = *blank;
         AGarcdmsg = '*ALL';
       When *in51;
         AGarcdmsg = 'A/R CODES INCLUDED IN THE REPORT';
       Other;
         AGarcdmsg = 'A/R CODES EXCLUDED FROM THE REPORT';
       Endsl;
       xar  = cde;
       xard = des;
       Select;
       When ct(1) = *blank;
         AGctgymsg = '*ALL';
       When *in59;
         AGctgymsg = 'CATEGORIES INCLUDED IN THE REPORT';
       Other;
         AGctgymsg = 'CATEGORIES EXCLUDED FROM THE REPORT';
       Endsl;
       xct  = ct;
       xctd = cts;
       Select;
       When as(5) = *blank;
         AGascdmsg = '*ALL';
       When *in14;
         AGascdmsg = 'ACTIVITY CODES INCLUDED IN THE REPORT';
       Other;
         AGascdmsg = 'ACTIVITY CODES EXCLUDED FROM THE REPORT';
       Endsl;
       xas  = as;
       xasd = asds;
      /end-free
     p WrtRptCriteria  e
      *** end add ***** 06/15/16 **************************************


