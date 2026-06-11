     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7321R - PREMIUM NOTICE
     H*****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 05/06/25 C:333 Support: 118328 (CoveRisk)                     *
      *                PDF of Invoice not available on IVC.           *
      *                - For Paid invoices, premium notice is showing *
      *                  only the premium amount and not the other line
      *                  items (fee, tax etc.)                        *
      *                - Currently Zero due Premium notice is         *
      *                  refraining for just CLT 129 (for both mass & *
      *                  indiviual). Changes done to refrain Zero due *
      *                  premium notice for mass requests only.       *
      *                                                               *
      * 04/30/25 C:291 Support: 118949 (CoveRisk)                     *
      *                Fix Division number on Remittance section. It  *
      *                was printing Division 0 on all division Premium*
      *                notices.                                       *
      *                                                               *
      * 02/24/25 C:291 Support: 118243 (Sunz)                         *
      *                Don't request Premium Notice template for      *
      *                Combined Bill when Single division Bill is     *
      *                requested regardless of Merge Prem notice flag.*
      *                Single Division Bill request will not generate *
      *                Combined Bill Document.                        *
      *                                                               *
      * 02/12/25 C:062 Support: 118495(099)                           *
      *                Don't include items that fall outside the date *
      *                range.
      *                                                               *
      * 02/04/25 C:291 Support: 118418                                *
      *                Each division Prm Notice included in Combined  *
      *                Bill document, print that division's name/     *
      *                address instead of State Rpt Division.         *
      *                To accomplish This: Create SXDOP for each div  *
      *                Premium Notice in Combined Bill document.      *
      *                If XTData = 'Y', SXDOP is created by WXMLDATA. *
      *                                                               *
      * 12/04/24 C:62  Ticket: 118101(133)(WC2663)                    *
      *                Add division to the printed inv#.              *
      *                                                               *
      * 10/30/24 C:291 Support: 117069                                *
      *                Fix Remittance section to not print City,      *
      *                State and Zip twice in Scriptura.              *
      *                                                               *
      * 07/17/24 C:65  Project: Phw#115835 (Interal)                  *
      *                Use correct indicator for mass run for         *
      *                paperless check.                               *
      *                                                               *
      * 06/24/24 C:300 Project: 146WKR (Prescient)                    *
      *                Collateral Processing                          *
      *                                                               *
      * 05/22/24 C:291 Project: WC2670                                *
      *                Print Lockbox address under Remit section when *
      *                Lockbox Address exist.                         *
      *                                                               *
      * 04/24/24 C:291 Project: WC2670                                *
      *                Don't email Premium Notice separately when     *
      *                called from Bills. Premium notice will either  *
      *                be mailed as part of combo doc or emailed as an*
      *                attachment with the Bill.                      *
      *                                                               *
      * 04/04/24 C:291 Project: WC2670                                *
      *                Add Premium Notice Title, Premium/Cont word    *
      *                and OCR.                                       *
      *                Updated XPIDesc to be used for iDarts          *
      *                description: Premium Notice or Contribution    *
      *                Notice.                                        *
      *                                                               *
      * 03/20/24 C:291 Project: WC2670                                *
      *                Generate separate iDarts for Premium notice    *
      *                when called for Quote Bill.                    *
      *                                                               *
      * 12/19/23 C:300 Support: 114684 (LUBA)                         *
      *                Premium notices are printing when quote Bill is*
      *                issued. The Premium notice is not saving to darts
      *                but is printing out with the quote Bill.       *
      *                                                               *
      * 09/26/23 C:062 Project: WC2670                                *
      *                Incorporate scriptura.                         *
      *                                                               *
      * 08/29/23 C:062 Project: 110502                                *
      *                Use new invoice# field to prevent it from being*
      *                cleared, we want the invoice# passed in used   *
      *                in the print.
      *                                                               *
      * 08/28/23 C:291 Projecr: WC2920                                *
      *                Use Procedure ICPaperlessOpt instead of        *
      *                ICPolicyOpt to suppress Premium Notice.        *
      *                                                               *
      * 08/23/23 C:62  Support: 110502 (140)                          *
      *                Fix Wrong Invoice # issue on Premium Notice    *
      *                                                               *
      * 06/14/23 C340  Support: 110502 (UBIC)                         *
      *                Fix Wrong Invoice # issue on Premium Notice    *
      *                                                               *
      * 01/28/22 C:65  Project: Phw#98830  (CoveRisk)                 *
      *                Call Scriptura regardless of 'z' index, the    *
      *                call is at end of program and it will only be  *
      *                called once ever.  Also, produce iDarts for    *
      *                spoolfile is requested.  If there is no LDA    *
      *                values present, use the iDarts flag/code       *
      *                passed into program for Scriptura (when called *
      *                from bills for instance).                      *
      *                                                               *
      * 08/27/21 C:65  Project: WC2663                                *
      *                Ensure if called from WIAR and there is a      *
      *                credit balance for the invoice #, then allow   *
      *                previous balance for the year to be pulled in. *
      *                                                               *
      * 06/03/21 C:62  Project: WC2663                                *
      *                Add Invoice #                                  *
      *                                                               *
      * 05/25/21 C:79  Project: WC2663                                *
      *                Invoice Cloud changes 3rd Party Verbiage       *
      *                                                               *
      * 05/05/21 C:65  Project: WC2663                                *
      *                Populate field in LDA for the location/name of *
      *                the output document for image retrieval.       *
      *                                                               *
      * 05/04/21 C:136 Project: WC1986                                *
      *                Adjust EFT logic.                              *
      *                                                               *
      * 03/31/21 C:062 Project: WC2663                                *
      *                Add invoice# processing.                       *
      *                                                               *
      * 02/03/20 C:136 Project: WC2187                                *
      *                Populate maint date to Scriptura workfile      *
      *                                                               *
      * 05/31/19 C:65  Support: 79861 (FMIC) (Replicate 64432)        *
      *                Save Output from OutputWindow in SPILETTER     *
      *                uses position 190 of LDA.  If set, it          *
      *                causes Scriptura to fail when processing a mass*
      *                run. Adjusted ldMassFile in LDA to use         *
      *                different positions.                           *
     H*                                                               *
     H* 06/11/18 C:65  Project: 150WAQ                                *
     H*                Check collateral setup and change the deposit  *
     H*                headings if collateral is set up.              *
     H*                                                               *
     H* 06/08/18 C:291 Support: 70924                                 *
     H*                Move the cancel verbiage above the Agent       *
     H*                information box for Luba.                      *
     H*                                                               *
     H* 05/31/18 C:062 Project: 133WQN                                *
     H*                Check to see if this premium notice should     *
     H*                be printed for mass billing                    *
     H*                                                               *
     H* 05/16/18 C:UXN Support 64563                                  *
     H*                Incorrect policy number listed in MICR         *
     H*                line of premium notice billing                 *
     H*                                                               *
     H* 01/10/18 C:33  Support 63341                                  *
     H*                Clear agt# before WA002R call                  *
     H*                                                               *
     H* 08/21/17 C:136 Project: 150WAN                                *
     H*                Do not call Scriptura CL (S4401C) when         *
     H*                called from payroll import                    *
     H*                                                               *
     H* 08/08/17 C:79  Project: SY395                                 *
     H*                Change logic to call S4401C only once          *
     H*                                                               *
     H* 08/10/16 C:65  Project: SY371                                 *
     H*                Link the Scriptura workfile data to the        *
     H*                template xml file name.                        *
     H*                                                               *
     H* 06/16/16 C:65  Project: SY371                                 *
     H*                Request XMLPrep record for Scriptura if needed *
     H*                and update into work file.  Populate new work  *
     H*                file fields as well.                           *
     H*                                                               *
     H* 10/16/15 C:180 Support 37559                                  *
     H*                Fix key to billing plan.                       *
     H*                                                               *
     H* 09/14/15 C:136 Project: 133WPH                                *
     H*                Reformat invoice for LUBA                      *
     H*                 Add QLINE1133, QAGT133, QINFO133              *
     H*                                                               *
     H* 08/07/15 C:136 Project: 133WPH                                *
     H*                Add system flag for lockbox processing         *
     H*                                                               *
     H* 07/02/15 C:65  Support: Phw#34034 (UBIC)                      *
     H*                Do not process the check added under 80974     *
     H*                when running the premium notice for mass runs. *
     H*                The check caused notices requested during an   *
     H*                an audit to not generate due to future offset  *
     H*                transactions.                                  *
     H*                                                               *
     H* 06/10/15 C:65  Project: 102WGI Priss:21                       *
     H*                Let the minimum balance check prevent notices  *
     H*                from printing for single employer "generate    *
     H*                all" requests when called from bills.  Ensure  *
     H*                assessments print on the month they belong.    *
     H*                Change routing page to be called after main    *
     H*                loop and before program ends.                  *
     H*                                                               *
      * 05/07/15 C:306 Support: 34477                                 *
      *                 To fix the Inquiries spelling and spacing     *
      *                 between contact numbers on premuim notice.    *
      *
      * 05/05/15 C:306 Support: 34299                                 *
      *                To Fix the routing sheet issue coming on       *
      *                Premium notice for resolve to darts            *
      *
     H* 02/23/15 C:136 Project: 140WBH                                *
     H*              - Don't accumulate prior year deposit for UBIC   *
     H*              - Add deposit state tax.                         *
     H*                                                               *
     H* 10/06/14 C:279 Project: 102WGI (Back to Base:102CB)           *
     H*                Add param for generating all premium notices   *
     H*                at the same time according to billing schedule.*
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 08/25/14 C:302 Project  129WTH                                *
     H*                         Copy address to fit the new envelop   *
     H*                         design.  Also move the amount due     *
     H*                         and amount enclosed to the right      *
     H*                         outside the envelop window.           *
     H*                                                               *
     H* 06/23/14 C:180 SUPPORT  RED 4378 Prevent invalid numeric data *
     H*                         error when building OCR line          *
     H*                         for all qualified clients             *
     H*
     H* 06/21/14 C:180 SUPPORT  129IMPLEMENTATION V14R3               *
     H*                         Bypass invoice printing when amtdue   *
     H*                         is negative for clt 129               *
     H*                                                               *
     H* 05/22/14 C:136 Project: 129WSB (CoveRisk Retrofit)            *
     H*          C:33  Blank first position of OCR line for client    *
     H*                129. Also moved scanline 1 space left so       *
     H*                that rightmost character would be caught by    *
     H*                the scanner                                    *
     H*                                                               *
     H* 04/10/14 C:65  Project: WC2277                                *
     H*                Call the generate notice procedure for         *
     H*                creating records for a notification email.     *
     H*                                                               *
     H* 01/27/14 C:279 Project: 129WSB  Priss #21                     *
     H*                - Corrected process for new "Audit" option.    *
     H*                                                               *
     H* 11/22/13 C:190 Project: 129WSB                                *
     H*                -Modify program to process new "Audit" option. *
     H*                -Receive new flag as entry parameter for       *
     H*                  "Suppress prior year balance".               *
     H*                                                               *
     H* 10/07/13 C:190 Project: 25WSS                                 *
     H*                Change the program to determine if             *
     H*                Premium or Contribution should be printed      *
     H*                based on Group setup.                          *
     H*                                                               *
     H* 09/12/13 C:290 SUPPORT WA025 81143                            *
     H*                INITIALIZE ##CONL BEFORE CALLING SRADDR SO     *
     H*                THAT EVERYTIME THE ADDRESS GETS POPULATED      *
     H*                PROPERLY.                                      *
     H*                                                               *
     H* 08/09/13 C:180 SUPPORT WA093 82734                            *
     H*                POPULATE KEY VALUES PRIOR TO CHAINING TO       *
     H*                CHECK FOR AUDIT INTERIM DAYS.                  *
     H*
     H* 07/02/13 C:136 Project: 147WAI                                *
     H*                Exclude dividend plan transactions (F1DVBL,    *
     H*                F1DVPD, F1DVXR) from selection and balance     *
     H*                calculations                                   *
     H*                                                               *
     H* 05/08/13 C:180 SUPPORT  WA093 81867                           *
     H*                INCLUDE FINAL A/R BALANCE WHEN AUDITED         *
     H*                FOR CLIENT 093.                                *
     H*
     H* 02/28/13 C:136 Support: Red2461                               *
     H*                Add Retro category to balance calc             *
     H*                                                               *
     H* 12/13/12 C:283 SUPPORT: WA071 81254                           *
     H*                "SERVICE BY:" SHOULD BE PRINTED FOR ALL        *
     H*                CLIENTS OTHER THAN CLTID = 139.                *
     H*                                                               *
     H* 11/05/12 C:136 SUPPORT: WA093 80974                           *
     H*                DO NOT CHECK AUDIT INTERIM DAYS IF POLICY      *
     H*                YEAR IS NOT AUDITED                            *
     H*                                                               *
     H* 10/19/12 C:136 SUPPORT: WA136 80977                           *
     H*                PRIOR YEAR BALANCES FOR 4TH AND 6TH ASST       *
     H*                CORRECTED                                      *
     H*                                                               *
     H* 07/19/12 C:283 SUPPORT: WA071 80293                           *
     H*                ADDRESS LINE 2 NOT PRINTING.                   *
     H*                                                               *
     H* 07/12/12 C:136 SUPPORT: WA093 78423                           *
     H*                ADJUST LOGIC FOR AUDIT INTERIM DAYS TO         *
     H*                WORK WITH RENEWAL/NEW BUSINESS/ALL TYPE        *
     H*                RUNS (QQGRMA = R/M/A)                          *
     H*                                                               *
     H* 06/01/12 C:280 SUPPORT: 79873                                 *
     H*                NOT PRINTING INVOICE FOR AUDITS                *
     H*                                                               *
     H* 05/07/12 C:136 SUPPORT: WA025 79862                           *
     H*                PREVENT CONTACT NAME FROM PRINTING TWICE IN    *
     H*                TASB OUTPUT                                    *
     H*                                                               *
     H* 05/01/12 C:136 SUPPORT: WA140 79836                           *
     H*                CORRECT DEPOSIT CALC                           *
     H*                                                               *
     H* 12/09/11 C:282 SUPPORT: WC2027                                *
     H*                ADD DUE UPON RECEIPT TO PREMIUM NOTICE IF NO   *
     H*                VALID DATE                                     *
     H*                                                               *
     H* 10/26/11 C:136 SUPPORT: WA114 78313                           *
     H*                REMOVE 114WTY. THE BILLING CONTACT NAME        *
     H*                IS PROVIDED BY SRADDR                          *
     H*                                                               *
     H* 10/10/11 C:062 SUPPORT 78217                                  *
     H*                CHANGE Q1DATE PARM                             *
     H*                                                               *
     H* 04/12/11 C:062 SUPPORT 76501                                  *
     H*                Compile Only                                   *
     H*                                                               *
     H* 03/24/11 C:136 SUPPORT: WA093 73580                           *
     H*               -ADJUST PREMIUM NOTICE CALC OF DUE DATE SO      *
     H*                THAT DUE DATE FALLS IN INSTALLMENT DATE RNG    *
     H*               -ADJUST CURRENT YEAR BALANCE CALC SO THAT       *
     H*                CURRENT BALANCE USES THE INSTALLMENT DATE RNG  *
     H*                INSTEAD OF THE ENTIRE MONTH
     H*               -EXCLUDE DEDUCTIBLE BILLING FROM CALC OF        *
     H*                DUE DATE IF EXCLUDED FROM BAL                  *
     H*                                                               *
     H* 03/16/11 C:255 SUPPORT 76245                                  *
     H*               Address spacing on premium notice was off.      *
     H*               Recompile as changed printer file.              *
     H*                                                               *
     H* 02/07/11 C:136 SUPPORT: WA045 75462                           *
     H*               -ALLOW EFT POLICY TO GET NOTICE IF CALLED FROM  *
     H*                BILLING, OR IF RUNNING FOR SINGLE POLICY       *
     H*                                                               *
     H* 11/29/10 C:136 SUPPORT: WA071 74976                           *
     H*               -REIMPLEMENT SCAN LINE FOR ECM                  *
     H*                LOST FROM 70639 (REWORKED)                     *
     H*               -REIMPLEMENT DETACH VERBIAGE FOR ECM            *
     H*                LOST FROM 70839                                *
     H*               -BOTH ITEMS ARE MISSING FROM V10 AND ARE NOT    *
     H*                NEEDED AT THIS POINT                           *
     H*                                                               *
     H* 10/22/10 C:255 Support: SEQ73572                              *
     H*                Checked object WA073R in library list instead  *
     H*                Call Stack.                                    *
     H*                                                               *
     H* 10/13/10 C:136 PROJECT: 71WKU                                 *
     H*                ALLOW RETRO PAYMENTS TO CALCULATE INTO BALANCE *
     H*                FOR POLICIES STARTING IN FYR 111               *
     H*                                                               *
     H* 10/05/10 C:136 SUPPORT: WA114 74248 XREF WC1568               *
     H*                APPLY ASSESSMENT BALANCE TO THE ASSESSMENT     *
     H*                CURRENT BALANCE BUCKET IF NOT BILLED IN        *
     H*                INSTALLMENTS. ALSO, PUT PRIOR MONTH            *
     H*                INSTALLMENT FEES IN PREVIOUS BALANCE.          *
     H*                                                               *
     H* 07/27/10 C:257 SUPPORT: SEQ73572                              *
     H*                IF API WA073R EXIST IN CALL STACK THEN USE     *
     H*                FILE WMEFL6 INSTEAD OF WMDBL3                  *
     H*                                                               *
     H* 05/10/10 C:65  PROJECT: 25WQW/SUPP#72564                      *
     H*                BACK UP EXPIRATION DATE ONE DAY IF USED FOR    *
     H*                THE CONTRIBUTION ENDING DATE.                  *
     H*                                                               *
     H* 05/06/10 C:65  PROJECT: 25WQW/SUPP#72564                      *
     H*                PREVENT PRIOR YEAR BALANCE FROM BEING INCLUDED *
     H*                WITHIN THE AMOUNT DUE FIELD.                   *
     H*                                                               *
     H* 04/26/10 C:136 PROJECT: 25WQW PRISS 21                        *
     H*                ADJUST TO DATES TO SYNC UP WITH THE NEXT       *
     H*                INSTALLMENT DATE, POLICY EXP DATE, OR          *
     H*                MONTH END DATE                                 *
     H*                                                               *
     H* 04/07/10 C:136 PROJECT: 25WRC PRISS 31                        *
     H*                VARIOUS CORRECTIONS AND ADDTIONS               *
     H*                                                               *
     H* 04/05/10 C:220 PROJECT: 25WRQ                                 *
     H*                MOVE TITLE AND LOGO BOX FOR CLIENT - TASB      *
     H*                                                               *
     H* 03/16/10 C:136 PROJECT: 25WQI PRISS                           *
     H*                CORRECT INVOICE NUMBER                         *
     H*                                                               *
     H* 03/11/10 C:65  PROJECT: 25WRC PRISS #25                       *
     H*                CHANGE BILLING LOGO TO NOT DEFAULT NAME.       *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/14/10 C:136 PROJECT: 25WRC                                 *
     H*                ADD CLIENT SPECIFIC CHANGES FOR TASB           *
     H*                CONTRIBUTION FORM                              *
     H*                                                               *
     H* 01/12/10 C:198 PROJECT: 114WVB PRISS 10                       *
     H*                Moved the SCAN line at the bottom of QBOT114   *
     H*                a line above.                                  *
     H*                                                               *
     H* 12/23/09 C:198 PROJECT: 114WVB                                *
     H*                Added client specific changes for QINFO and    *
     H*                QBOT.                                          *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Print 'Statement #' along with the invoice #   *
     H*                                                               *
     H* 11/13/09 C:180 CONTRIBUTION HEADING CHANGES - UNACCOUNTED.
     H*                BUT ACCEPTED SINCE CLIENT ACCEPTED POST-DATED
     H*                CHANGES.
     H*
     H* 10/30/09 C:234 SUPPORT: SY184 PRISS# 22                       *
     H*                Routing Page should include information as to  *
     H*                what selection the user made.                  *
     H*                Do not print the routing page when output type *
     H*                is file                                        *
     H*                                                               *
     H* 10/09/09 C:191 SUPPORT: 70237 WA114                           *
     H*                Not implemented in V10 because fix already     *
     H*                present as part of 09/21/09 changes.           *
     H*                                                               *
     H* 09/25/09 C:234 SUPPORT: 69852, 69693, 69510                   *
     H*                Separate working of program for Retro Setlement*
     H*                Call and Mandatory Premium Notice print call   *
     H*                Fix errors on premium notice.                  *
     H*                                                               *
     H* 09/21/09 C:33  Project: 139WMG                                *
     H*                No longer print 'Serviced by:' on the notice.  *
     H*                                                               *
     H* 09/15/09 C:191 SUPPORT: 69652 WA114                           *
     H*                WHEN RUN WITH REVISED BILL/AUDIT BILL ETC. DART*
     H*                DO NOT GENERATE                                *
     H*                                                               *
     H* 09/07/09 C:234 SUPPORT: SYS184 PRISS# 22                      *
     H*                Changes for PROJECT 132WLB (prm notice portion)*
     H*                sent back to base                              *
     H*                                                               *
     H* 08/11/09 C:180 SUPPORT 68972
     H*                EXCLUDE AUDITS BASED ON INTERIM DAYS WHEN
     H*                RUNNING RENEWALS.
     H*
     H* 07/10/09 C:191 SUPPORT: 69090 WA114                           *
     H*                PREVIOUS YEAR BALANCE NOT ACCOUNTED            *
     H*                                                               *
     H* 06/04/09 C:062 PROJECT: 114WTY                                *
     H*                ADD CONTACT NAME                               *
     H*                                                               *
     H* 06/01/09 C:227 SUPPORT: WA114/68281                           *
     H*               - CORRECT SCANLINE IN PREMIUM NOTICE            *
     H*                                                               *
     H* 05/21/09 C:227 SUPPORT: WA099/68551                           *
     H*               - CORRECT ASSESSMENT HEADING                    *
     H*                                                               *
     H* 05/19/09 C:191 SUPPORT: WA139/68187                           *
     H*               - CORRECT ASSESSMENT DESC.                      *
     H*                                                               *
     H* 04/28/09 C:191 SUPPORT: WA109/67116 WA139/68187               *
     H*               - ADD INSURED NAME AND PERFORATIONS             *
     H*                                                               *
     H* 03/10/09 C:136 PROJECT: 114WRK PRISS 13                       *
     H*               -RE-ARRANGE PRINTER FILE                        *
     H*               -RE-ARRANGE SCAN LINE                           *
     H*                                                               *
     H* 02/18/09 C:33  Project: 132WGT  WA132/67224                   *
     H*               -Add statement to new format.                   *
     H*               -If resolving to DARTS:                         *
     H*                1 - Create a single spoolfile output.          *
     H*                2 - Create multiple pages for resovling.       *
     H*                                                               *
     H* 01/08/09 C:65  Project; WC1769                                *
     H*                ENSURE NEW ASSESSMENTS ARE NOT ADDED INTO      *
     H*                OTHER FEES LINE ITEM.                          *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 11/18/08 C:61  PROJECT: 132WLB(PRISS#8)                       *
     H*                Recompile to pick up changes WFPNP/PNDUDT.     *
     H*                                                               *
     H* 11/07/08 C:190 PROJECT: 132WLB(PRISS#9)                       *
     H*                Q1PRMS PROCESSING HANDELED                     *
     H*                                                               *
     H* 10/07/08 C:65  PROJECT: 114WRK                                *
     H*                ADD NEW FORMAT LAYOUT FOR PREMIUM NOTICE.      *
     H*                                                               *
     H* 10/01/08 C:190 PROJECT: 132WLB                                *
     H*                IF RUN FOR CLT132 AND QQPFB <> 'P' OR 'B'      *
     H*                THEN DO NOT WRITE INVOIC.                      *
     H*                                                               *
     H* 09/04/08 C:062 PROJECT: WC1611                                *
     H*                USE ELNWRW INSTEAD OF EHNWRW                   *
     H*                                                               *
     H* 08/20/08 C:136 PROJECT: 114WQB                                *
     H*                WACHOVIA LOCKBOX UPLOAD, ADD SCANLINE          *
     H*                                                               *
     H* 02/15/08 C:65  PROJECT WC1568                                 *
     H*                CORRECT HEADING FOR 3RD ASSESSMENT. ALSO,      *
     H*                REMOVED PRINTING OF SEPERATE LINE ITEMS FOR    *
     H*                EACH ASSESSMENT BASED ON BILL IN INSTALLMENT   *
     H*                FLAG.  FOR MULTI-STATE THE FLAG VALUE WAS NOT  *
     H*                RELIABLE TO PRINT FOR A/R SUMMARY ACCOUNTS.    *
     H*                ADD TWO ARRAYS TO STORE THE ASSESSMENT         *
     H*                HEADINGS TO PRINT FOR A/R SUMMARY. ONLY PRINT  *
     H*                THE OPTIONAL HEADING IF THE ASSESSMENT EXISTS  *
     H*                FOR ONLY ONE STATE ON THE POLICY.              *
     H*                                                               *
     H* 02/12/08 C:65  PROJECT WC1568                                 *
     H*                ADD A SPACEB TO PREVENT ASSESSMENT HEADING     *
     H*                FROM OVERLAPPING THE PRIOR LINE ITEM IN THE    *
     H*                PRINTER FILE W7321O.                           *
     H*                                                               *
     H* 11/02/07 C:61  SUPPORT WA071 60929 71WIG                      *
     H*                Correct text.                                  *
     H*                                                               *
     H* 10/26/07 C:33  Project: 114WPF  Priss# 4                      *
     H*                For Client 114 only, restore changes removed   *
     H*                 by seq#53052.                                 *
     H*                                                               *
     H* 09/12/07 C:136 SUPPORT: WA099 60127                           *
     H*               -ADJUSTED LOGIC SO THAT PAYMENTS WOULD BE       *
     H*                PUT INTO THE CORRECT BUCKET PER GW.            *
     H*                                                               *
     H* 09/04/07 C:136 SUPPORT: WA045 59965                           *
     H*               -DUE DATE NOT DISPLAYING CORRECTLY FOR DEPOSIT  *
     H*                CORRECT PREVIOUS YEAR DEPOSIT BALANCE CALC     *
     H*               -DISPLAY DEPOSIT IN RESOLVE TO DARTS COPY       *
     H*                                                               *
     H* 08/01/07 C:65  PROJECT: SUPP#59211/CM042                      *
     H*                DO NOT CHANGE THE POSTING DATE FIELD FOR       *
     H*                CHECKING FOR A VALID A/R TRANSACTION WHEN      *
     H*                RUNNING AN 'R' RENEWAL SINGLE EMPLOYER NOTICE. *
     H*                                                               *
     H* 07/24/07 C:65  PROJECT: 114WHO/114WMF                         *
     H*                ADD PROGRAMMING TO RECOGNIZE THE A/R BATCH     *
     H*                RECORD WRITTEN BY THE SETTLEMENT PROCESS.      *
     H*                                                               *
     H* 07/18/07 C:183 PROJECT: WC1491                                *
     H*               -ADD PROCESSING FOR DEPOSIT REFUND AND VOID/    *
     H*                STOP PAYS.                                     *
     H*                                                               *
     H* 07/19/07 C:136 PRISS: 114WMF 00046                            *
     H*                REMOVE 114WCW FROM BASE.                       *
     H*                                                               *
     H* 05/15/07 C:184 PROJECT WC1568                                 *
     H*                Charge assessments on the same basis as premium*
     H*                installments                                   *
     H*                                                               *
     H* 05/08/07 C:189 Seq#: WA109/52423                              *
     H*                 Premium Notice Being Generated Even if no     *
     H*                 Balance is due. Added new Min. Bal Field      *
     H*                 to Routing Page.                              *
     H*                                                               *
     H* 04/18/07 C:189 Seq#: WA132/57236                              *
     H*                Premium notices should exclude the Agency      *
     H*                Bill.                                          *
     H*                                                               *
     H* 04/12/07 C:33  Seq#: WA139/53052                              *
     H*                Do not print single employers if there is no   *
     H*                 current premium due.                          *
     H*                                                               *
     H* 04/04/07 C:61  PROJECT 71WIG                                  *
     H*                Print "(copy not acceptable)" text for clt071  *
     H*                only.                                          *
     H*                                                               *
     H* 03/23/07 C:33  Seq#: WA071/57707                              *
     H*                Clear the heading information before loading.  *
     H*                                                               *
     H* 03/15/07 C:136 SUPPORT: WA132 57628                           *
     H*                PROJECT 132WGT INTRODUCED A CONFLICT WITH      *
     H*                THE USE OF *IN98, WHICH WAS USED TO            *
     H*                CONDITION THE DISPLAY OF PREVIOUS UNPAID       *
     H*                BALANCE FOR 71WHE. CHANGED PROJECT 71WHE TO    *
     H*                USE *IN49                                      *
     H*                                                               *
     H* 01/20/07 C:183 PROJECT: C132WGT - FURTHER CHANGES IN PREMIUM  *
     H*                NOTICE MODIFICATION.                           *
     H*                                                               *
     H* 12/26/06 C:183 PROJECT: C132WGT - PREMIUM NOTICE              *
     H*                MODIFICATION.                                  *
     H*                                                               *
     H* 12/15/06 C:136 SUPPORT: WA071 56019                           *
     H*                CORRECT VERBIAGE FOR CURRENT DUE. 'SETTLEMENT  *
     H*                AMOUNT' WAS PRINTING.  'CONTRIBUTION NOTICE'   *
     H*                NEEDS TO PRINT.                                *
     H*                                                               *
     H* 11/20/06 C:180 PROJECT: C114WMF - BACK TO BASE
     H*               -REF. 114WCW - CHANGE TEXT "FEES DUES" TO
     H*                "STATE FEES DUES" ON PREMIUM NOTICES FOR
     H*                CLIENT 114. Search for date 11/20/06.
     H*               -REF. 114WEM - CHECK EMPLOYER'S CROSS-REFERENCE
     H*                WHEN CALCULATING PREVIOUS UNPAID BALANCE.
     H*                Search for date 02/13/03 along with 11/20/06
     H*                too because there were other non-related chgs
     H*                on that same date of 02/13/03.
     H*                                                               *
     H* 10/11/06 C:33  Seq#: WA132/50844                              *
     H*                Do not use parms if not passed.                *
     H*                                                               *
     H* 09/20/06 C:136 PROJECT: 71WHE (BACK TO BASE)                  *
     H*               -CLIENT SPECIFIC - ADD CODE FOR PRINTING        *
     H*                                  REPORT TITLE.                *
     H*               -CLIENT SPECIFIC - CHANGE 'PREMIUM' VERBIAGE    *
     H*                                  TO 'CONTRIBUTION'            *
     H*               -CLIENT SPECIFIC - SPECIAL FORMAT FOR POLICY #  *
     H*               -CLIENT SPECIFIC - CHG 'POLICY#' TO 'MEMBER #'  *
     H*               -CLIENT SPECIFIC - ADD CODE FOR OCR STRING      *
     H*               -CLIENT SPECIFIC - ADD CODE FOR RETRO DEDUCTIBLE*
     H*               -CLIENT SPECIFIC - SUPPRESS FUTURE CX VERBIAGE  *
     H*                                                               *
     H* 09/05/06 C:33  Project: WC1347  WA045/54451                   *
     H*                Only resolve the document when flag is set.    *
     H*                                                               *
     H* 08/30/06 C:33  Project: WC1410  WA045/54451                   *
     H*                This project should not have taken away any    *
     H*                 functionality.                                *
     H*                                                               *
     H* 08/09/06 C:136 PROJECT: 136WBG                                *
     H*                ADD DEPOSIT BY POLICY YEAR MODE                *
     H*                                                               *
     H* 07/18/06 C:132 PROJECT: WA132 - 50844                         *
     H*              - Change logic on Renewal to use Detail History  *
     H*                file (WDEHP) to determine if is a renewal.     *
     H*              - If no Detail History, check prior year on      *
     H*                employer detail, if exists, is a renewal.      *
     H*                                                               *
     H* 05/24/06 C:53  PROJECT: WC1410                                *
     H*                BASE PREMIUM CALC ON FLAG, ADD DEPOSIT CALC    *
     H*                BASED ON FLAG. CONVERT OUTPUT TO PRTF          *
     H*                                                               *
     H* 01/24/06 C:65  PROJECT: 114WHO                                *
     H*                REMOVE PROGRAMMING FOR RETRO OFFSET RECORD.    *
     H*                                                               *
     H* 01/12/06 C:168 PROJECT: 132WDT WC1347                         *
     H*                Resolve output to DARTS for each employer.     *
     H*                                                               *
     H* 09/26/05 C:65  PROJECT: 114WHO                                *
     H*                EXCLUD RETRO PREMIUM RECORDS FROM A/R TOTAL.   *
     H*                CHANGE WORDING OF PREMIUM LINE ITEM TO         *
     H*                SETTLEMENT AMOUNT AS WELL.                     *
     H*                                                               *
     H* 03/08/05 C:65  PROJECT: WC1202                                *
     H*                PRINT EXTENDED EMPLOYER NAME FIELDS.           *
     H*                                                               *
     H* 03/07/05 C:136 PROJECT: WC1199 (REVAMP DUNNING II)            *
     H*              - ADD PROCESSING FOR MINIMUM BALANCE DUE FLAG    *
     H*              - ADD VERBIAGE TO NOTICE IF POLICY THAT IS       *
     H*                PENDING CX IS PICKED UP BY RUN                 *
     H*              - ADD INSTALLMENT RANGE TO DARTS REMARK          *
     H*                                                               *
     H* 12/22/04 C:61  SUPPORT WA129 43809                            *
     H*              - Do not consider "H" type A/R when determining  *
     H*                if there are "Qualifying Transactions".        *
     H*              - Seek posting date on first "B", "A", "D", "F"  *
     H*                type A/R record in date range to determine     *
     H*                "Due Date".                                    *
     H*                                                               *
     H* 10/11/04  C:65 PROJECT: WC1064                                *
     H*                RECOGNIZE TRANSFER CODES FOR INSTALLMENT FEES. *
     H*                                                               *
     H* 09/24/04 C:53  PROJECT 132WBD                                 *
     H*                ADD AGENT HEADER VARIABLE FIELD                *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H*  01/13/04 C:136 Project: WC1116                               *
     H*                 Update conditioning of direct debit status to *
     H*                 recognize expiration date.                    *
     H*                                                               *
     H*  09/23/03 C:136 Support: WA129/39121  (WC1116)                *
     H*                 Do not let premium notice run for any ACH     *
     H*                 employer except at audit.                     *
     H*                                                               *
     H*  06/11/03 C:136 Support: 37533                                *
     H*                 Add condition for direct debit status.        *
     H*                                                               *
     H*  06/02/03 C:132 Project: WC1117                               *
     H*                 Changed to use GROUP NAME instead of Office   *
     H*                 Accounting Name if FNCGFL <> "Y" for          *
     H*                 Self-Insured Groups                           *
     H*                                                               *
     H*  05/08/03 C:132 Project: WC1117                               *
     H*                 Changed to use LOCK BOX address (if applies)  *
     H*                 only on the remittance stub only.             *
     H*                                                               *
     H*  04/11/03 C:136 Project: WC1116                               *
     H*                 Bypass ACH employers during mass run unless   *
     H*                 requested specifically.                       *
     H*                                                               *
     H*  02/13/03 C:132 Project: WC1117                               *
     H*                 Added LOCK BOX address check in group         *
     H*                 auxiliary file. If present in WMF2P, use the  *
     H*                 Lock box address instead of accounting office *
     H*                 address.                                      *
     H*                                                               *
     H* 07/10/02 C:61  SUPPORT WA071 33349                            *
     H*                When "Exclude Deductible Fees" = 'Y', do not   *
     H*                include deductible fees in previous policy     *
     H*                year unpaid fee balance.                       *
     H*                                                               *
     H*  03/04/02  C:65  PROJECT: WC1064                              *
     H*                  PRINT A LINE ITEM FOR INSTALLMENT FEES.      *
     H*                                                               *
     H* 11/16/01 C:61  SUPPORT WA114 29337                            *
     H*                Add line item "current month adjustments".     *
     H*                                                               *
     H* 07/09/01 C:61  SUPPORT WA096 28044                            *
     H*              - By-pass previous years if on agency bill.      *
     H*                (Account current, Plan='A')                    *
     H*                                                               *
     H*  07/02/01  C:90  PROJECT: WC1028                              *
     H*                  Print "File #" instead of "Policy #" if a    *
     H*                  carrier policy number has been entered.      *
     H*                                                               *
     H*  03/08/01  C:90  SEQ# WA112/26416                             *
     H*                  WHEN A NOTICE IS RUN FOR AN AUDIT TYPE=A,    *
     H*                  CALCULATE THE DUE DATE BY ADDING THE # OF    *
     H*                  ADDT'L DAYS TO THE INSTALLMENT FROM DATE.    *
     H*                                                               *
     H*  08/08/00  C:33  SEQ#: WA101/23867                            *
     H*                 -USE 'SA020R' INSTEAD OF 'WA003R'.            *
     H*                 -PAD 'XXNUM' WHEN USING 'CHGNUM'              *
     H*                                                               *
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 10/12/99 C:87  PROJECT: WC705                                 *
     H*                Add carrier policy # to header info.           *
     H*                                                               *
     H* 09/07/99 C:87  PROJECT: SUPP WA095 19554                      *
     H*                Padded field with blanks to avoid bad results. *
     H*                                                               *
     H* 08/23/99 C:87  PROJECT: SUPP WA091 #19265                     *
     H*                When calculating for 'Monthly', include adjust-*
     H*                ments to determine if policy qualifies.        *
     H*                                                               *
     H* 08/10/99 C:33  SEQ#: WA095/19195                              *
     H*                ACCOUNT FOR NEGATIVE BALANCES.                 *
     H*                                                               *
     H* 04/05/99 C:33  PROJECT: WC899                                 *
     H*                EXCLUDE ACCOUNT CURRENT POLICIES.              *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 05/13/99 C:118 SUPPORT: WA100/17997                           *
     H*                Modified accumulated current billed premium to *
     H*                include audit transactions. Modified routing   *
     H*                page to include routing destination and left   *
     H*                justify the report title.                      *
     H*                                                               *
     H* 04/26/99 C:87  PROJECT: CM042                                 *
     H*                Added parms for addtl days due (QQADDU) and for*
     H*                generate renewal, monthly or all flag QQGRMA). *
     H*                Removed policy effective from and to dates.    *
     H*                Installment date now comes in a date range -   *
     H*                appropriate processing added.  Add subroutine  *
     H*                to calc due date using QQADDU value.  Add sub- *
     H*                routines (SRRMA SRREMO) to determine if a pol  *
     H*                qualifies as renewal or monthly.               *
     H*                                                               *
     H* 01/25/99 C:87  PROJECT: 96WBN                                 *
     H*                Found that blank line was printed in line      *
     H*                items in certain cases. Corrected.             *
     H*                                                               *
     H* 12/22/98 C:87  PROJECT: CM041                                 *
     H*                Line items 'Previous policy yr unpaid - Tax    *
     H*                Balance' and '- Fees Balance'. Do not print    *
     H*                prev pol yr balances if sum is less than or    *
     H*                equal to 0. Adjust output to accomodate new    *
     H*                line items.                                    *
     H*                Add Interim days processing.                   *
     H*                                                               *
     H* 12/10/98 C:61  SUPPORT 14595                                  *
     H*                Only process active (WMA3P/A3STAT = 'A')       *
     H*                Employer 3rd Address records.                  *
     H*                                                               *
     H* 12/09/98  C:61 SUPPORT WA048 15374                            *
     H*                Correct print of Employer Name/address etc.    *
     H*                                                               *
     H* 11/30/98  C:35   PROJECT: CM041                               *
     H*                  ADDED OPTIONAL POLICY EFFECTIVE DATE RANGE.  *
     H*                                                               *
     H*  10/12/98 C:87 PROJECT: SUPPORT WA072 #13930                  *
     H*                Revised check for any qualifying transactions- *
     H*                wasn't picking up all appropriate policies.    *
     H*                Add line items: 'Previous Policy Yr Unpaid     *
     H*                State Tax' and 'State Tax Due'.  Adjusted      *
     H*                lower part of form to accommodate addtnl lines *
     H*                                                               *
     H*  09/10/98 C:87 PROJECT: SUPPORT                               *
     H*                ADJUSTED EMP NAME, ADD ON BOTTOM OF NOTICE     *
     H*                DOWN ONE LINE. REVERSED AGENT AREA CODES FOR   *
     H*                OUTPUT TO NOTICE.                              *
     H*                                                               *
     H*  08/31/98 C:87 PROJECT: 48WLF                                 *
     H*                FOUND THAT IF ALL LINE ITEMS PRINTED, THEN     *
     H*                TOTAL WOULD OVERLAP EXPENSE CONSTANT. CHANGED  *
     H*                LINE THAT TOTAL PRINTS ON.                     *
     H*                MOVED LEFT MARGIN TO PRINT 1 CHAR TO RIGHT     *
     H*                                                               *
     H*  07/28/98 C:65 PROJECT: CM039                                 *
     H*                USE THE ACCTING OFFICE FIELD FOR DIRECT INQRY. *
     H*                                                               *
     H*  06/01/98 C:65 PROJECT: CM039                                 *
     H*                USE THE ACCOUNTING OFFICE INFO FOR ADDRESSES   *
     H*                AND NAMES. ALSO CHANGE DETACH MESSAGE.         *
     H*                                                               *
     H*  02/23/98 C:87 PROJECT: SUPPORT                               *
     H*                MOVED *BLANKS INTO DARTS COMMENT(DACOMM) TO    *
     H*                CORRECT WRONG AMOUNTS BEING WRITTEN TO FILE.   *
     H*                ALSO INCREASED LENGTH OF DACOMM TO 120.        *
     H*                                                               *
     H*  02/18/98 C:35 PROJECT: CM031                                 *
     H*                CHANGED TO ONLY PRINT PRIOR YR UNPAID BALANCE  *
     H*                IF PRIOR YEAR HAS AN UNCONTESTED AUDIT AND     *
     H*                DOES NOT HAVE A RECORD IN THE WORKFILE.  ALSO, *
     H*                FOR PRIOR YEAR BALANCE, ONLY INCLUDE POSTINGS  *
     H*                UP TO AND INCLUDING THE INSTALLMENT DATE       *
     H*                ENTERED ON THE PROMPT SCREEN.                  *
     H*                                                               *
     H*  12/01/97 C:87 PROJECTS CM031,CM032                           *
     H*                MODIFY CODE TO ACCOUNT FOR CHANGES TO SRRPTR   *
     H*                 WHICH NOW BUILDS WORKFILE TAKING IN TO CONS-  *
     H*                 IDERATION THE ACCOUNT STATUS AND ACCOUNT RE-  *
     H*                 CEIVABLE TYPE.                                *
     H*                MODIFY CODE SO THAT 'FEES DUE', 'DEDUCTIBLE    *
     H*                 FEES DUE', 'MEMBERSHIP FEES DUE', & 'EXPENSE  *
     H*                 CONSTANT DUE' DO NOT PRINT IF EQUAL TO ZERO.  *
     H*                REMOVE "EFF DT RANGE" FROM ROUTING PAGE.       *
     H*                                                               *
     H*  10/29/97 C:79 PROJECT WC712                                  *
     H*                PREMIUM NOTICE, ADD CODE TO WRITE A DARTS      *
     H*                REMARK.                                        *
     H*                                                               *
     H*  07/03/97 C:79 PROJECT WC712                                  *
     H*                PREMIUM NOTICE, ADDL V3 CHANGES                *
     H*                                                               *
     H*  06/13/97 C:62 PROJECT WC681                                  *
     H*                CHANGE HARD-CODED 800 AREA CODE TO FNPH1A      *
     H*                                                               *
     H*  02/09/97 C:57 PROJECT CM017                                  *
     H*                CHG TO REQUIRE 'B','A',OR 'D' TRANS IN WMEAP   *
     H*                ON QQDATE TO PRODUCE INVOICE.                  *
     H*                                                               *
     H*****************************************************************
     F*
     FWMFNL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     infds(dbfds)
     FSDCMLA    IF   E           K DISK    rename(pcm01:pcm0a)
     FWMEML     UF   E           K DISK
     FWMEAL6    IF   E           K DISK
     FWMEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWFPNP     O    E             DISK
     F                                     RENAME(PFN01:PFN02)
     FWXPNP1    O    E             DISK    USROPN
     F                                     RENAME(PFN01:PFN03)
     FXMLPREL1  UF   E           K DISK
     FWMDBL     IF   E           K DISK
     F                                     RENAME(PDB01:PDB02)
     FWMEAL3    IF   E           K DISK
     F                                     RENAME(PEA01:PEA0L3)
     FWDEAL2    IF   E           K DISK
     F                                     RENAME(PEA01:PEA03B)
     FWDF2L     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PNA01)
      *
      * Application System Control
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SFB01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FWMAHL     IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWXEBL     IF   E           K DISK
     FWXEBL1    IF   E           K DISK
     F                                     RENAME(PEB01:PEB02)
     FWMA3L     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMIFL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWDCXL1    IF   E           K DISK
     FWTCRL     IF   E           K DISK
     F*** Begin Add *** 09/26/23 *******************************************
     FWSBLL     IF   E           K DISK    Rename(pbl01:pbl00)
     fxmlteml1  uf   e           k disk    rename(xt100:xt101)
     F*** End   Add *** 09/26/23 *******************************************
     FW7321O    O    E             PRINTER OFLIND(*IN85)
     F                                     USROPN
      *
     FWZEXL     IF   E           K DISK
     F                                     RENAME(PEX01:ZEX01)
     FWMCOL     IF   E           K DISK    RENAME(PCO01:PCO00) USROPN

     D*
     D IS              S              7  0 DIM(14) ASCEND                       INSTALL DATES
     D MN              S              9    DIM(12) CTDATA PERRCD(1)             MONTH NAME
     D CXNOTE          S             55    DIM(1) CTDATA PERRCD(1)              CX VERBIAGE
     D OCL             S              1    DIM(46)                              SCANLINE          %
     D WTL             S              1  0 DIM(46) CTDATA PERRCD(46)            WEIGHT            %
     D ASS             S              2    DIM(7)                               ASSESSMENT STATE
     D ASH             S             20    DIM(7)                               ASSESSMENT HEADING
     D OC2             S              1    DIM(39)                              SCANLINE          %
     D WT2             S              1    DIM(39) CTDATA PERRCD(39)            WEIGHT            %
     d MD              s              2  0 dim(12) ctData perRcd(12)
     DisContri         s              1
     D/copy wPrGroup
     D/copy sprModule
     D/copy wPrPmtSvc
     D*** Begin Add *** 09/26/23 *******************************************
     D/copy sPrJobInfo
     D*** End   Add *** 09/26/23 *******************************************
     DisLockBox        s               n
     DisAudited        s               n
     D##Co##           s              3  0
     D##Fnd##          s              3  0
     DisAddType        s              1n
     DisUseOCR         s              1n
     D##type           s              1    inz
     DqqInv#           s             10
     dinvoice#         s                   Like(EANINV)
     ddspInvoice#      s                   Like(EANINV)
     d*** Begin Add *** 02/12/25 ****************************************
     dholdInvoice#     s                   Like(EANINV)
     d*** End   Add *** 02/12/25 ****************************************
     DisAudit          s               n
     DsrDates          s              7  0 DIM(99)
     DsrBilledPrem     s             11  2
     DsrBilledTax      s             11  2
     DsrBilledFee      s             11  2
     DpremPaid         s             11  2
     DtaxPaid          s             11  2
     DinstPaid         s             11  2
     D/copy wPrBank
       Dcl-S chrChkType Char(10);
       Dcl-S chrEftType Char(1) Inz('E');
       Dcl-S chrEftKey  Char(30);
       Dcl-S chrEfDt Char(7);
       Dcl-S chrEobType Char(3);
       Dcl-S chrFyr Char(3);
       Dcl-S chrPre Char(1);
       Dcl-S chrRRN Char(9);
       Dcl-S isSelected Ind;
      //*** Beg Add ***** 03/20/24 **************************************
       Dcl-S isQteMode Ind;
      //*** End Add ***** 03/20/24 **************************************
      //*** Beg Add ***** 10/30/24 **************************************
       Dcl-S xxAAD1 Char(40);
       Dcl-S xxAAD2 Char(40);
      //*** End Add ***** 10/30/24 **************************************

     DAddCheckDigit    pr                  like(xxOCR)
     D inOCR                               like(xxOCR) const
     d
      ***** Beg Add ***** 12/19/23 ***************************
     d ChkStack        pr                  extpgm('SRCHKPGM')
     d  outPgmName                   10    const
     d  isFound                       1
     d Callpgm         s              1n
      ***** End Add ***** 12/19/23 ***************************
     dObjExists        pr             1n
     d object                        10    const
     d objType                        7    const
     d
     d GenScriptOut    pr                  ExtPgm('S4401C')
     d inXml_                        61
     d inXml           s             61
     d isMassReq       s              1n   inz
     d massFlag        s              1    inz(*blanks)
     d xmlFileBlank    s                   like(XPXML)
     d xmlLink         s                   like(XPLink)
     d xmlFlowName     s                   like(XPXML)
     d xmlFile         s                   like(XPXML)
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D isUseScript     S              1n   inz
     d xxeCity         s             40
     d xxeSt           s              2
     d xxeZip          s              9s 0
     d xxaCity         s             40
     d xxaSt           s              2
     d xxaZip          s              9s 0
     d xxoCity         s             40
     d xxoSt           s              2
     d xxoZip          s              9s 0
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('PN')
     d processType     c                   const('Premium Notice')
     d*** Begin Add *** 09/26/23 *******************************************
     d doNotRunScriptura...
     d                 s               n
     d calledFromBills...
     d                 s               n
     d templateFile    s             60a   Inz
     d*** End   Add *** 09/26/23 *******************************************

     d StLetter        pr                  ExtPgm('STLETTER')
     d   letMode_                    10    const
     d   xmlFile_                    60
     d   message_                   250    options(*nopass)
     d   mode_                        1    const options(*nopass)
     d   rrn_                         9    const options(*nopass)
     d   prefix_                      2    const options(*nopass)
     d   string_                     10    const options(*nopass)
     d   co#_                         3    const options(*nopass)
     d   fnd_                         3    const options(*nopass)
     d   mass_                        1    const options(*nopass)
     d   link_                       60    const options(*nopass)
     d*
     D/copy sPrXML
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXPODT                 1      7  0
     D  XXPCYM                 1      5  0
     D  XXPODD                 6      7  0
     D                 DS
     D  FNPH1                  1      7  0
     D  XXPH11                 1      3  0
     D  XXPH12                 4      7  0
     D                 DS
     D  FNPH2                  1      7  0
     D  XXPH21                 1      3  0
     D  XXPH22                 4      7  0
     D                 DS
     D  AGPHN1                 1      7  0
     D  XXAG11                 1      3  0
     D  XXAG12                 4      7  0
     D                 DS
     D  AGPHN2                 1      7  0
     D  XXAG21                 1      3  0
     D  XXAG22                 4      7  0
     D                 DS
     D  EAPODT                 1      7  0
     D  ZZCYM                  1      5  0
     D  ZZDD                   6      7  0
     D                 DS
     D  #DATE                  1      8  0
     D  #MM                    1      2  0
     D  #DD                    3      4  0
     D  #YY                    5      8  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECT. DATE
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATE
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D dsOCR           DS
     D  #2FND                  1      3  0
     D  #2EMP#                 4     12  0
     D  #2DIV                 13     17  0
     D  #2FYR                 18     20  0
     D  XXAMT1                21     31  2
     D  #2FYRP                32     34  0
     D  XXAMT2                35     45  2
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND                       COMPANY NAME
     D  CONAME                 1     40
     D                 DS
     D  ACNM                   1     40
     D                                     DIM(40) ASCEND
     D  OFACNM                 1     40
      *
     D                 DS
     D  X1DATE                 1      8  0
     D  X1MM                   1      2
     D  X1DD                   3      4
     D  X1CC                   5      6
     D  X1YY                   7      8
      *
     d                 ds
     d  intPfDt                1      7  0
     d  yy1                    1      3  0
     d  mm1                    4      5  0
     d  dd1                    6      7  0
     d  yymm                   1      5  0

     d                 ds
     d  dsBill                 1    108  8
     d                                     dim(12)
     d  BSab01                 1      9  8
     d  BSab02                10     18  8
     d  BSab03                19     27  8
     d  BSab04                28     36  8
     d  BSab05                37     45  8
     d  BSab06                46     54  8
     d  BSab07                55     63  8
     d  BSab08                64     72  8
     d  BSab09                73     81  8
     d  BSab10                82     90  8
     d  BSab11                91     99  8
     d  BSab12               100    108  8

     d  isProcAll      s              1
     d  intCount       s              2  0
     d  xxDuDt         s              8  0
     d  xxDate         s              8  0
     d  xxIfDt         s              7  0
     d  xxItDt         s              7  0
      *
     D                 DS
     D  ZZKEY                  1     40
     D  ZZCO#                  1      3  0
     D  ZZFND                  4      6  0
     D  ZZEMP#                 7     15  0
     D  ZZDIV                 16     20  0
     D                 DS
     D  XQDT                   1      8  0
     D  XXMTH                  1      2  0
     D  XXDAY                  3      4
     D  XXYR                   5      8
     D                 DS
     D  ID                     1     98
     D                                     DIM(14) ASCEND                       INSTALL DATES
     D  DT1                    1      7  0
     D  DT2                    8     14  0
     D  DT3                   15     21  0
     D  DT4                   22     28  0
     D  DT5                   29     35  0
     D  DT6                   36     42  0
     D  DT7                   43     49  0
     D  DT8                   50     56  0
     D  DT9                   57     63  0
     D  DT10                  64     70  0
     D  DT11                  71     77  0
     D  DT12                  78     84  0
     D  DT13                  85     91  0
     D  DT14                  92     98  0
     D                 DS
     D  AM                     1    154
     D                                     DIM(14) ASCEND                       INSTALL AMOUNTS
     D  AM1                    1    011  0
     D  AM2                   12    022  0
     D  AM3                   23    033  0
     D  AM4                   34    044  0
     D  AM5                   45    055  0
     D  AM6                   56    066  0
     D  AM7                   67    077  0
     D  AM8                   78    088  0
     D  AM9                   89    099  0
     D  AM10                 100    110  0
     D  AM11                 111    121  0
     D  AM12                 122    132  0
     D  AM13                 133    143  0
     D  AM14                 144    154  0
     D                 DS
     D  EMN                    1    350
     D                                     DIM(7)
     D  XXEMN1               001    050
     D  XXEMN2               051    100
     D  XXEMN3               101    150
     D  XXEMN4               151    200
     D  XXEMN5               201    250
     D  XXEMN6               251    300
     D  XXEMN7               301    350
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x#RRN           s              9  0 inz(0)
     d x#seq#          s              5  0 inz(0)
     d x#Dcid          s             12    inz(' ')
     d x#Fold          s             80    inz(' ')
     d x#Uncn          s            255    inz(' ')
     d x#Key           c                   const('Premium Notice')
     D x#Type          c                   const('Policy Set')
     D x#Mode          c                   const('E')
     d*
     D/copy sPrNotice
     D CHGNUM          C                   CONST('SA020R')
     D DA              C                   CONST('PREMIUM NOTICE RUN -
     D                                     ON:')
     D D71             C                   CONST('CONTRIBUTION NOTICE-
     D                                      RUN ON:')
     D AUDTIT          C                   CONST('Audit')
     D CONHDG          C                   CONST('CONTRIBUTION  NOTICE')
     D AUDHDG          C                   CONST('F I N A L   -
     D                                     A U D I T   -
     D                                     N O T I C E')
     D REVHDG          C                   CONST('R E V I S E D')
     D REGHDG          C                   CONST('WORKERS'' -
     D                                     COMPENSATION -
     D                                     INVOICE')
     D DA2             C                   CONST('FOR INSTALLMENT RANG-
     D                                     E:')
     D HDRBAS          C                   CONST('P R E M I U M   -
     D                                     N O T I C E')
     D HDR129          C                   CONST('WORKERS''  -
     D                                     COMPENSATION  -
     D                                     INVOICE')
     D FILE            C                   CONST('File')
     D PLCY            C                   CONST('Policy')
     D CERT            C                   CONST('Certificate')
     D BAL             C                   CONST('Balance')
     D Audit1          c                   const('AUDIT   ')
     D suppPrYear      s              1
     D @PRCINV         S               N   INZ(*ON)
      *
      * TASB INVOICE CONSTANTS
      *
     D HDR25           C                   CONST('WORKERS COMPENSATION-
     D                                      CONTRIBUTION-
     D                                      INVOICE')
     D URL25           C                   CONST('tasbrmf.org')
     D DST25           C                   CONST('County-District #:')
     D PRT25           C                   CONST('Participant #:')
     D PRD25           C                   CONST('Participation -
     D                                     Period:')
     D ATT25           C                   CONST('Attn:')
     D PDT25           C                   CONST('Print Date:')
     D STM25           C                   CONST('Statement #:')
     D DDT25           C                   CONST('Due Date:')
     D AMT25           C                   CONST('Amount Due:')
     D CHG25           C                   CONST('Charges')
     D PAY25           C                   CONST('Payments')
     D BAL25           C                   CONST('Balance')
     D PRI25           C                   CONST('Prior To:')
     D CNT25           C                   CONST('Contributions:')
     D BLD25           C                   CONST('Balance Due')
     D CUR25           C                   CONST('Current Due')
     D PSD25           C                   CONST('Past Due')
     D MCP25           C                   CONST('Make Check Payable -
     D                                     To:')
     D DIT25           C                   CONST('Direct Inquiries To:')
     D INS25           C                   CONST('MEMBER:')
     D RTO25           C                   CONST('REMIT TO:')
     D WCF25           C                   CONST('WC Contributions -
     D                                     Fund Year')
     D ST#25           C                   CONST('Statement Number:')
     D CD#25           C                   CONST('Country District #:')
     D AMR25           C                   CONST('Amount Remitted: ')
     D PRW25           C                   CONST('(Please Return With-
     D                                      Remittance)')
     D                 DS
     D  XQCFDT                 1      8  0
     D  CFMTH                  1      2
     D  CFDAY                  3      4
     D  CFYR                   5      8
     D                 DS
     D  XQCTDT                 1      8  0
     D  CTMTH                  1      2
     D  CTDAY                  3      4
     D  CTYR                   5      8
     D  ELBSDT                 1      7  0
     D  XXCYM                  1      5  0
     D  XXDD                   6      7  0
     D MEMB            C                   CONST('Member')
     D DEPHD           C                   CONST('INCL DEPOSITS:')
     D EXPHD           C                   CONST('INCL EXP CONS:')
     D X132            C                   CONST('* * * *     -
     D                                     PLEASE RETURN -
     D                                     ENTIRE COPY OF -
     D                                     NOTICE WITH -
     D                                     REMITTANCE     -
     D                                     * * * *')
     D*
     D RENEWL          C                   CONST('RENEWALS')
     D MNTHLY          C                   CONST('MONTHLY ')
     D ALL             C                   CONST('ALL     ')
     D CARPOL          C                   CONST('Carrier Policy #: ')
     D AH1             C                   CONST('Prev Pyr Unpaid')
     D AH2             C                   CONST('Prev Cyr Unpaid')
     D AH4             C                   CONST('Previous Policy Year Unpaid')
     D XCPERF          C                   CONST('- - - - - - - - - -
     D                                     - - - - - - - - - -
     D                                     - - - - - - - - - -
     D                                     - - - - - - - - - -
     D                                     - - - - ')
     D XCDTXT          C                   CONST('Please detach and -
     D                                     return with your -
     D                                     remittance. (Copy -
     D                                     not acceptable.)')
     D AH3             C                   CONST('ASSESSMENT')
     D XXTOP           C                   CONST('OUTPUT TYPE  :')
     D XCDURP          C                   CONST('Due upon receipt')
     d str3rdVrb                           Like(OF3rdVrb)
     d str3rdVrb2                          Like(OF3rdVrb)
     D                 DS
     D  XXSCAN                 1     41
     D  DSFND                  1      3
     D  DSEMP#                 4     12
     D  DSFYR                 13     15
     D  DSDIV                 16     20
     D  DSDUDT                21     28
     D  DSTOTL                29     39
     D  DSCKDG                40     40  0
     D  DSNEGT                41     41
     D                 DS
     D  EMSEMP                 1     15
     D  XXCNDX                 2      7
     D*

     dsvLDA            ds          1024    dtaara(*lda)
     d ldSrPayImpFlg                  1    overlay(svLDA:29)
     d*** Begin Add *** 09/26/23 *******************************************
     d keyEmail                      40    Overlay(svLDA:56)
     d*** End   Add *** 09/26/23 *******************************************
     D ldMassFile                    60    Overlay(svlda:129)
     d*** Begin Add *** 09/26/23 *******************************************
     d ldOutputFile                  60    Overlay(svLDA:191)
     d*** End   Add *** 09/26/23 *******************************************
     D xxDart                         1    overlay(svlda:251)
     d xxCopy                         1    overlay(svlda:257)
     d*** Begin Add *** 09/26/23 *******************************************
     d xxEmail                        1    overlay(svlda:322)
     d*** End   Add *** 09/26/23 *******************************************
     D xxPath                       255    Overlay(svlda:673)
     D*
     d DocRoot         pr                  ExtPgm('RTVDOCROOT')
     d   $#Root_                     10

     D PSDS          ESDS                  EXTNAME(SZQ1P)

      *
      * NEED TO RENAME FIELDS IN WXEBP SO THAT WE DON'T STEP ON THE
      * DATA IN THE CURRENT RECORD WHEN CHECKING FOR PRIOR FYR
      *
     IPEB02
     I              EBCO#                       E2CO#
     I              EBFND                       E2FND
     I              EBFYR                       E2FYR
     I              EBEMP#                      E2EMP#
     I              EBDIV                       E2DIV
     I              EBSRT1                      E2SRT1
     I              EBSRT2                      E2SRT2
     I              EBENAM                      E2ENAM
     I              EBAGT#                      E2AGT#
     I              EBANAM                      E2ANAM
     I              EBRGN#                      E2RGN#
     I              EBCHAP                      E2CHAP
     I              EBCNTY                      E2CNTY
     I              EBEZIP                      E2EZIP
     I              EBAZIP                      E2AZIP
     I              EBSEFL                      E2SEFL
     I              EBUSER                      E2USER
     I              EBPGM                       E2PGM
     I              EBTIME                      E2TIME
     I              EBDT                        E2DT
     I              EBINDT                      E2INDT
     I              EBPFDT                      E2PFDT
     I              EBCNDT                      E2CNDT
     I              EBJRST                      E2JRST
     I              EBPTDT                      E2PTDT
     IZEX01
     I              EXCO#                       ZXCO#
     I              EXFND                       ZXFND
     I              EXEMP#                      ZXEMP#
     I              EXDIV                       ZXDIV
     I              EXOFND                      ZXOFND
     I              EXOEMP                      ZXOEMP
     I              EXODIV                      ZXODIV
     I              EXUSER                      ZXUSER
     I              EXPGM                       ZXPGM
     I              EXTIME                      ZXTIME
     I              EXDT                        ZXDT
     I              EXIUSR                      ZXIUSR
     I              EXIDT                       ZXIDT
     I              EXCAR                       ZXCAR
      *
      *===============================================================
      *  KLIST
      *===============================================================
      *
      *  Application System Control
      *

     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
      *
      *
      *  KLIST FOR STATISTICAL FILE
      *
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      *
      *
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      *
      *
      *   KEY LIST FOR THE USER PARAMETER FILE
      *
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
      *
      *  KEY LIST FOR THE OVERRIDE FILE
      *
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
      *
      * KEY LIST FOR THE COMMENT FILE
      *
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
      *   KEY LIST FOR THE COMPANY FILE
      *
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *   KEY LIST FOR THE FUND
      *
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *   KEY LIST FOR THE OFFICE
      *
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMOFF#
      *
      *  FUND DETAIL
      *
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
      *
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
     C*
      *   KEY LIST FOR THE EMPLOYER DETAIL FILE
      *
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     ELKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
      * Employer Detail key for Prior Year
      *
      *
      * Employer Detail History key for current year
      *
      *   KEY LIST FOR THE EMPLOYER MASTER FILE
      *
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      *   KEY LIST FOR THE EMPLOYER ACCOUNTS RECEIVABLE FILE
      *
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXITDT
     C*
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
     C                   KFLD                    XXARTY
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXITDT
     C*
     C     EAKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
      *
      *   KEY LIST FOR THE EMPLR ACCTS RCVABLE FILE - PRIOR YEAR
      *
     C     EAKYP         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYRP            3 0
     C*
     C     EAKYP1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYRP            3 0
     C                   KFLD                    XXARTY            1
      *
      *   KEY LIST FOR THE FUND DETAIL FILE
      *
     C     F2KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      *  AGENT HISTORY FILE
      *
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      *  AGENT ASSIGNMENT FILE
      *
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#
      *
      *  AGENT MASTER FILE
      *
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *  EMPLOYER THIRD ADDRESS FILE
      *
     C     A3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
      *
      * COMMERCIAL REPORT WORKFILE
      *
     C     EBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C*
     C*  INSTALLMENT FEE FILE
     C*
     C     IFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST              2
     C                   KFLD                    XXEFDT            7 0
     C*
     C     IFKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
      *
      * VENDOR HISTORY FILE
      *
     C     DBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXPODT
      *
      *  CX Code
      *
     C     CXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    KKCXDT
      *
      *  CX Reason
      *
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    KKCXDT
      *
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY
      *
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* Darts File
     C*
     C     keySDCMLA     KLIST
     C                   KFLD                    q#Key
     C                   KFLD                    x#Seq#
      *
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
      *
     C     *LIKE         DEFINE    AHAGT#        XXAGT#
     C     *LIKE         DEFINE    AAFEIN        XXFEIN
     C     *LIKE         DEFINE    AAMOD         XXMOD
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELEMP#        XXFREN
     C     *LIKE         DEFINE    ELEMP#        XXTOEN
     C     *LIKE         DEFINE    ELCO#         XXCO#
     C     *LIKE         DEFINE    ELFND         XXFND
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    EAPODT        XXPO01
     C     *LIKE         DEFINE    EAPODT        XXPO31
     C     *LIKE         DEFINE    ORTITL        XXTITL
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C*
     C                   MOVE      *BLANK        XXPAUD            1
      *
      *  ENTRY PARAMETERS
      *
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQDATE            8
     C                   PARM                    QQDAT2            8
     C                   PARM                    QQADDU            3
     C                   PARM                    QQEXDF            1
     C                   PARM                    QQCOM1            2
     C                   PARM                    QQCOM2            2
     C                   PARM                    QQCOM3            2
     C                   PARM                    QQPYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQFRDT            8
     C                   PARM                    QQTODT            8
     C                   PARM                    QQSRT1            2
     C                   PARM                    QQSRT2            2
     C                   PARM                    QQFREA            2
     C                   PARM                    QQTOEA            2
     C                   PARM                    QQFRAA            2
     C                   PARM                    QQTOAA            2
     C                   PARM                    QQFREN            9
     C                   PARM                    QQTOEN            9
     C                   PARM                    QQFRAN            5
     C                   PARM                    QQTOAN            5
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQDRMK            5
     C                   PARM                    QQIDYS            3
     C                   PARM                    QQGRMA            1
     C                   PARM                    QQMINB            7
     C                   PARM                    QQDEP             1
     C                   PARM                    QQEXP             1
     C                   PARM                    QQDRES            1
     C                   PARM                    QQPFB             1
     C                   PARM                    QQMASS            1
     C                   PARM                    QQSUPBALP         1
     c                   Parm                    qqAll             1
     c                   Parm                    qqInv#
     C*
     C     *LIKE         DEFINE    QQDRES        XXDRES
     C     *LIKE         DEFINE    QQDEP         XXDEP
     C     *LIKE         DEFINE    QQEXP         XXEXP
     C*
      /Free
       isUseSpoolOut = UseSpoolOut(processType);
       isUseScript = UseScriptura(processType);
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);
       If isUseScript;
          In svLda;
          //*** Begin Add *** 09/26/23 *********************************
          Out svLda;
          //*** End   Add *** 09/26/23 *********************************
       Endif;
      /end-free
      /Free
       //*** Beg Add ***** 03/20/24 ****************************************
       isQteMode = *Off;
       If QQMODE = 'Q';
          isQteMode = *On;
          QQMODE = 'D';
       Endif;
       //*** End Add ***** 03/20/24 ****************************************
       ##Co## = %dec(qqCo#:3:0);
       ##Fnd## = %dec(qqFnd:3:0);
       If Q1prms > 37;
          invoice# = %Dec(qqInv#:10:0);
          dspInvoice# = %Dec(qqInv#:10:0);
       EndIf;
       isContri = GetPrmContflg(##Co##:##Fnd##);
       //*** Begin Add *** 09/26/23 ***************************************
       // Determine if called from billing or audit processes
       doNotRunScriptura = *Off;
       calledFromBills   = *Off;
       If PgmInStack('W7615R') = *On Or PgmInStack('W7700R') = *On;
          doNotRunScriptura = *On;
          calledFromBills   = *On;
       EndIf;

       // Access the billing setup file
       Chain ##Co## pbl00;

       // Inmitialize Template File Field
       templateFile = ' ';
       //*** End   Add *** 09/26/23 ***************************************
      /End-free
     C                   CALL      'SRCHKPGM'
     C                   PARM      'WIAR'        XXPGNM           10
     C                   PARM      *BLANKS       XXWIARPGM         1
     C     Q1PRMS        IFGE      31
     C                   MOVEL     QQDEP         XXDEP
     C                   MOVE      *ON           *IN21
     C                   ENDIF
     C*
     C     Q1PRMS        IFGE      32
     C                   MOVEL     QQEXP         XXEXP
     C                   MOVE      *ON           *IN22
     C                   ENDIF
     C*
     C     Q1PRMS        IFGE      33
     C     QQDRES        ANDEQ     'Y'
     C                   MOVEL     'Y'           X2DRES            1
     C                   ENDIF
     C                   CLEAR                   XXDRES
     C                   Clear                   xxResolve         1
     C     Q1PRMS        IFGE      33
     C                   Move      QQDres        xxResolve
     C                   Endif
     C*
     C     Q1PRMS        IFGE      34
     C                   MOVEL     QQPFB         XXPFB             1
     C                   SETON                                        23
     C     XXTOP         CAT(P)    XXPFB:2       RSTOP
     C                   ENDIF
     C     XXPFB         IFEQ      *BLANK
     C                   MOVEL     'P'           XXPFB
     C                   SETOFF                                       23
     C                   ENDIF
      /free
       If Q1PRMS > 35;
          suppPrYear = QQSUPBALP;
       Endif;
      /end-free
      /free
       isProcAll = ' ';
       If q1Prms > 36;
          isProcAll = qqAll;
       Endif;
      /end-free
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      *
      *
     C                   EXSR      SRINIT
     C*
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN97
     C                   ENDIF
     C*
     C                   If        CltId = '133'
     C                   Move      *On           *In96
     C                   Endif
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         X2DATE            8 0
     C*
     C                   CLEAR                   XXTRA1
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '129'
     C                   MOVEL(P)  HDR129        ##DATA
     C     CLTID         WHENEQ    '132'
     C                   MOVE      *ON           *IN98
      /Free
       If isContri = '1';
          ##data = conHdg;
       Else;
          ##data = hdrBas;
       Endif;
      /End-free
     C                   MOVEL(P)  X132          XXTRA1
     C                   Z-ADD     10.25         XTRA1V
     C                   Z-ADD     1.6           XTRA1H
     C     CLTID         WHENEQ    '093'
      /Free
       If isContri = '1';
          ##data = conHdg;
       Else;
          ##data = hdrBas;
       Endif;
      /End-free
     C     QQMODE        IFEQ      'A'
     C     QQMODE        OREQ      'R'
     C                   MOVE      *ON           AUD93             1
     C                   MOVE      'D'           QQMODE
     C                   ELSE
     C                   MOVE      *OFF          AUD93
     C                   ENDIF
     C     CLTID         WHENEQ    '071'
      *
     C                   MOVE      QQMODE        XXMODE            1
     C                   SELECT
     C     QQMODE        WHENEQ    'A'
     C                   MOVE      *ON           AUDIT             1
     C                   MOVE      'D'           QQMODE
     C                   MOVEL(P)  AUDHDG        ##DATA
     C     QQMODE        WHENEQ    'R'
     C                   MOVE      *ON           AUDIT             1
     C                   MOVE      'D'           QQMODE
     C     REVHDG        CAT(P)    AUDHDG:3      ##DATA
     C                   OTHER
     C                   MOVE      *OFF          AUDIT
     C                   MOVEL(P)  CONHDG        ##DATA
     C                   ENDSL
     C                   MOVE      AUDIT         *IN49
     C                   OTHER
      /Free
       If isContri = '1';
          ##data = conHdg;
       Else;
          ##data = hdrBas;
       Endif;
      /End-free
     C                   ENDSL
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXHDR
     C*
     C                   CALL      'SRCENTER'
     C                   PARM      XXTRA1        ##DATA           80
     C                   PARM      80            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        XXTRA1
      *
     C     XXDRES        IFEQ      'Y'
     C                   ENDIF
      *
      *  Get Office Install Flag
      *
     C                   MOVEL     'SY'          XXSYK
     C     SYKY          CHAIN     SSY01                              60
      *
      *
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXPODT
     C                   MOVE      QQMINB        XXMINB            7 2
     C     DEPHD         CAT(P)    XXDEP:2       RSDEP
     C     EXPHD         CAT(P)    XXEXP:2       RSEXP
      ***********
      ***********
      **BUILD*FIRST DAY OF INSTALLMENT MONTH
      **BUILD*LAST DAY OF INSTALLMENT MONTH
      ***********
      *
     C                   MOVE      QQADDU        XXADDU            3 0
     C                   MOVE      QQPYR         XXPYR             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQFRDT        XXFRDT            8 0
     C                   MOVE      QQTODT        XXTODT            8 0
     C                   MOVE      QQFREN        XXFREN
     C                   MOVE      QQTOEN        XXTOEN
     C                   MOVE      QQFRAN        XXFRAN            5 0
     C                   MOVE      QQTOAN        XXTOAN            5 0
     C*
     C                   MOVE      QQIDYS        XXIDYS            3 0
     C*
     C                   MOVE      QQDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXIFDT            7 0
     C*
     C                   MOVE      QQDAT2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXITDT            7 0
      *
      *  TASB HAS ITS OWN BALANCE LOGIC WHICH WILL NOT BE
      *  IMPACTED BY THIS CHANGE
      *
     C                   SELECT
     C     CLTID         WHENEQ    '025'
     C                   Z-ADD     *ZERO         XXPO01
     C                   MOVEL     XXPCYM        XXPO01
     C                   ADD       01            XXPO01
     C     XXPO01        ADD       30            XXPO31
     C                   OTHER
     C                   Z-ADD     XXIFDT        XXPO01
     C                   Z-ADD     XXITDT        XXPO31
     C                   ENDSL
     C                   MOVE      *OFF          *IN67
      * FLAG TO INDICATE RETRO SETTLEMENT
     C                   MOVE      *OFF          XXRTSM
     C                   MOVE      *OFF          XXMPNT
     C                   MOVE      QQGRMA        XXGRMA            1
     C     QQGRMA        IFEQ      'Y'
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W7226R'      XXPGNM           10
     C                   PARM      *BLANKS       XXRTSM            1
      * IF W7226R IS THE CALLING PROGRAM THEN WORKING WILL BE FLAGGED A
      * S RETRO SETTLEMENT CALL OTHER WISE IT WILL BE TREATED AS MANDAT
      * ORY PREMIUM NOTICE PRINT.
     C     XXRTSM        IFEQ      *ON
      * *IN67 WILL BE SETON ONLY FOR RETRO SETTLEMENT
     C                   MOVE      *ON           *IN67
     C                   ELSE
     C                   MOVE      *ON           XXMPNT            1            MN PRMN PRT FLG
     C                   ENDIF
     C                   MOVE      'B'           QQGRMA
     C                   ENDIF
     C                   Move      *Off          *in68
     C                   Move      ' '           xxBill            1
     C                   If        qqGrma = 'Y' or qqGrma = 'Z'
     C                   Move      *On           *in68
     C                   Move      'Y'           xxBill
     C                   If        qqGrma = 'Y'
     C                   Move      'B'           qqGrma
     C                   Else
     C                   Move      'A'           qqGrma
     C                   Endif
     C                   Endif
      *
      *  BATCH OR DEMAND  MODE
      *
     C     QQMODE        IFEQ      'D'
     C                   MOVE      *ON           *IN09                          DEMAND MODE
     C                   ELSE
     C                   MOVE      *ON           *IN08                          BATCH MODE
     C                   ENDIF
      *
     C                   TIME                    DSDTTM
     **
     **   CONVERT SYSTEM DATE AND TIME TO 8 DIGIT BILLING DATE
     **
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         RPDATE            8 0
     **
      *
      *   CHAIN TO THE COMPANY FILE
      *
     C     COKEY         CHAIN     PCO01                              60
      *
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      *
      *   REMOVE LEADING BLANKS FROM COMPANY NAME
      *
     C                   Z-ADD     1             X                 2 0
     C     *BLANK        LOOKUP    CONM(X)                            60
     C                   MOVEA     CONM(X)       XXCONM           40
      *
      *   CHAIN TO THE FUND FILE
      *
     C     FNKEY         CHAIN     PFN01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     **
     ** USE ALTERNATE FUND NAME IF IT IS NOT BLANK
     **
     C                   MOVE      *OFF          *IN99
      *
     C                   MOVE      *OFF          *IN66
     C     FNCGFL        IFEQ      'Y'
     C                   MOVE      *ON           *IN66
     C                   ENDIF                                                  FNCGFL IFEQ
      *
      *   LOAD CITY, STATE, & ZIP FOR FUND ADDRESS
      *
     C                   MOVEL     FNMST         XXSTAT            2
     C                   MOVEL     FNMZIP        XXZIP1            5
     C                   MOVE      FNMZIP        XXZIP2            4
     C                   MOVEL     FNMCTY        XXCITY           25
     C                   EXSR      SR1ST
     C                   MOVEL(P)  XXCSTZ        XXFCSZ           40
      *
      *   MOVE CORRECT PHONE NUMBER TO OUTPUT
      *
      *
      *   GET THE FUND DETAIL FILE
     **
      *  Check for Lock box address in Group Auxiliary file
      *  If exists use lock box address instead of Accounting Ofc addr
     C     F2KEY         CHAIN     PF201                              60
     C                   MOVE      ' '           XXLKBX            1
     C  N60F2LAD1        IFGT      *BLANKS
     C                   MOVE      'Y'           XXLKBX
      * Use indicator 85 to print on remittance stub.
     C                   MOVE      *ON           *IN85
     C                   ELSE
     C                   MOVE      *OFF          *IN85
     C                   ENDIF
     **
      *
      *
      *
     C                   ENDIF                                                  *IN60 = OFF
     C*
     C*
     C     FNKEY         CHAIN     PEX01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  'Agent'       EXAHDR
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  'Agent'       EXAHDR
     C                   ENDIF
     C*

      /free
       Clear xmlLink;
       Select;
       //*** Begin Add *** 09/26/23 ****************************************
       When calledFromBills = *On;
          isMassReq = *On;
          massFlag = 'Y';
       //*** End   Add *** 09/26/23 ****************************************
       When ldSrPayImpFlg = 'Y';
          isMassReq = *off;
          massFlag = 'Y';
       When xxEmp# = 0;
          isMassReq = *on;
          massFlag = 'Y';
       Endsl;
      /end-free
     C*
     C*  CHAIN TO USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C                   MOVE      Q1USER        EMUSER
     C                   MOVE      Q1PGM         EMPGM
      *
      *   GET COMMENT DESCRIPTIONS
      *
     C                   MOVEL(P)  QQCOM1        XXKEY            10
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCMT1           30
     C                   ENDIF
      *
     C                   MOVEL(P)  QQCOM2        XXKEY            10
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCMT2           30
     C                   ENDIF
      *
     C                   MOVEL(P)  QQCOM3        XXKEY            10
     C     TBKEY         CHAIN     PBC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     BCDESC        XXCMT3           30
     C                   ENDIF
      *
      *  LOAD ROUTING PAGE SELECTION DATA
      *
     C                   MOVE      QQCO#         RSCO#             3 0
     C                   MOVE      QQFND         RSFND             3 0
     C                   MOVE      QQDATE        RSDATE            8 0
     C                   MOVE      QQDAT2        RSDAT2            8 0
     C                   MOVE      QQADDU        RSADDU            3 0
     C                   MOVE      QQMINB        RSMINB            7 2
     C                   MOVEL     QQEXDF        RSEXDF            1
     C                   MOVEL     QQCOM1        RSCOM1            2
     C                   MOVEL     XXCMT1        RSCMT1           30
     C                   MOVEL     QQCOM2        RSCOM2            2
     C                   MOVEL     XXCMT2        RSCMT2           30
     C                   MOVEL     QQCOM3        RSCOM3            2
     C                   MOVEL     XXCMT3        RSCMT3           30
     C                   MOVE      QQDRMK        RSDRMK            5
     C*
     C*  Load description for renewal, monthly or all
     C                   SELECT
     C     QQGRMA        WHENEQ    'R'
     C                   MOVE      RENEWL        XXDRMA            8
     C     QQGRMA        WHENEQ    'M'
     C                   MOVE      MNTHLY        XXDRMA
     C     QQGRMA        WHENEQ    'A'
     C                   Move      Audit1        xxDrma
     C     qqGrma        WhenEq    'B'
     C                   MOVE      ALL           XXDRMA
     C                   OTHER
     C                   MOVE      *BLANK        XXDRMA
     C                   ENDSL
      *
     C     QQEMP#        IFNE      *ZERO
     C     QQEMP#        ANDNE     *BLANK
     C                   MOVE      *ON           *IN24
     C                   MOVE      QQPYR         RSPYR             3 0
     C                   MOVE      QQEMP#        RSEMP#
     C                   MOVE      QQDIV         RSDIV#
     C                   ELSE
     C                   MOVE      QQFRDT        RSFRDT            8 0
     C                   MOVE      QQTODT        RSTODT            8 0
     ******************************************************************
     **  SELECT SORT ORDER
     ******************************************************************
     C                   SELECT
     **
     C     QQSRT1        WHENEQ    'AN'
     C     'AGENT NU'    CAT       'MBER':0      RSSRT1           40
     **
     C     QQSRT1        WHENEQ    'AA'
     C     'AGENT AL'    CAT       'PHA':0       RSSRT1
     **
     C     QQSRT1        WHENEQ    'EN'
     C     'EMPLR NU'    CAT       'MBER':0      RSSRT1
     **
     C     QQSRT1        WHENEQ    'EA'
     C     'EMPLR AL'    CAT       'PHA':0       RSSRT1
     **
     C                   ENDSL
     ******************************************************************
      *
     C     QQSRT2        IFEQ      'EA'
     C     'EMPLR AL'    CAT       'PHA':0       RSSRT2           40
     C                   ENDIF
     C     QQSRT2        IFEQ      'EN'
     C     'EMPLR NU'    CAT       'MBER':0      RSSRT2
     C                   ENDIF
     C                   ENDIF                                                  QQEMP = 0
      *
     C     QQSRT2        IFNE      'EA'
     C     QQSRT2        ANDNE     'EN'
     C     '*NO'         CAT       'NE'          RSSRT2
     C                   ENDIF
      *
     ******************************************************************
     **  SELECT RANGES
     ******************************************************************
     **
     C     XXTOAN        IFNE      *ZEROS
     C                   MOVE      *ON           *IN25
     C                   MOVE      QQFRAN        RSFRAN
     C                   MOVE      QQTOAN        RSTOAN
     C                   ENDIF
     **
     C     QQTOAA        IFNE      *BLANKS
     C                   MOVE      *ON           *IN26
     C                   MOVEL     QQFRAA        RSFRAA            2
     C                   MOVEL     QQTOAA        RSTOAA            2
     C                   ENDIF
     **
     C     XXTOEN        IFNE      *ZEROS
     C                   MOVE      *ON           *IN27
     C                   MOVE      QQFREN        RSFREN
     C                   MOVE      QQTOEN        RSTOEN
     C                   ENDIF
     **
     C     QQTOEA        IFNE      *BLANKS
     C                   MOVE      *ON           *IN28
     C                   MOVEL     QQFREA        RSFREA            2
     C                   MOVEL     QQTOEA        RSTOEA            2
     C                   ENDIF
     ******************************************************************
      *
      *   GET THE DISTRIBUTION INFORMATION & PRINT
      *
     C                   MOVE      '0'           XXPRT
     C     ORKEY         CHAIN     POR01                              60
     C*
     C* PULL THE HOLD STATUS FOR NEW FORMAT BILLS FROM REPORT #0.
     C*
     C                   MOVE      '*YES'        X1HOLD            4
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ORHOLD        X1HOLD
     C                   ENDIF                                                  *IN60 IFEQ
      ***** Beg Add ***** 12/19/23 ***************************
      * Change the HOLD status to Yes, if called from W8010R (Quote Bill)
      /free
       callpgm = *off;
       ChkStack('W8010R':CallPgm);
       If callpgm = *on;
          X1HOLD = '*YES';
       Endif;
      /end-free
      ***** End Add ***** 12/19/23 ***************************
     C     *IN60         IFEQ      *OFF
     C*   DO NOT PRINT THE ROUTING PAGE WHEN OUTPUT TYPE IS FILE
     C     XXPFB         ANDNE     'F'
     C*   LEFT JUSTIFIY TITLE ON ROUTING PAGE
     C*
     C                   MOVE      *BLANKS       XXTITL
     C     ORTITL        IFNE      *BLANKS
     C                   MOVE(P)   ORTITL        XXDATA           60
     C     ' '           CHECK     XXDATA        N                 3 0
     C*
     C     N             IFLE      60
     C                   SUBST(P)  XXDATA:N      ORTITL
     C                   ENDIF
     C                   ENDIF
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   OPEN      W7321O
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C                   MOVE      *ON           XXOPN             1
     C     XXDRES        IFEQ      'Y'
      /free
      /end-free
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   CLOSE     W7321O
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C                   MOVE      *OFF          XXOPN
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *ON           *INOA
     C*
     C* We will process 2x's if resolving to DARTS
     C     1             DO        2             Z                 1 0
      *
      *   START READING THE WORK FILE
      *
     C                   READ      PEB01                                  61
     C     *IN61         DOWEQ     *OFF
     C     *in68         OrEq      *On
     C     *IN67         OREQ      *ON
     C     AUDIT         OREQ      *ON
     C     XXMPNT        OREQ      *ON
      *
     C     *IN67         IFEQ      *ON
     C     AUDIT         OREQ      *ON
     C     XXMPNT        OREQ      *ON
     C     *in68         OrEq      *On
     C                   Move      *Off          *in68
     C                   Z-ADD     XXEMP#        EBEMP#
     C                   Z-ADD     XXDIV         EBDIV
     C                   Z-ADD     XXPYR         EBFYR
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         XXDUDT            8 0
     C     AUDIT         IFEQ      *ON
     C                   MOVE      QQDATE        XXDUDT
     C                   ENDIF
     **
     C                   MOVE      EBEMP#        XXEMP#
     C                   MOVE      EBDIV         XXDIV
      /free
       // If Mass Run, skip if not opted in
       //*** begin chg ***  07/17/24  ************************************
       //*****If isMassReq;
       If *in24 = *off;
       //*** begin chg ***  07/17/24  ************************************
          If ICPaperlessOpt(xxCo#:xxFnd:xxEmp#) <> 0;
             Read peb01;
             If %Eof(wxebl);
                *in61 = *On;
             EndIf;
             Iter;
          EndIf;
       EndIf;
       xxFyr = EBfyr;
       clear dsBill;
       intPfDt =0;

       If isProcAll = 'Y';
          Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pel01;
          If %found;
             *in60 = *off;
             intPfDt = ELbsDt;

             xxKey = %editc(ELbsCd:'X');

             Chain (xxCo#:xxKey) pbs01;
             If %found;
                *in60 = *off;
             Else;
                *in60 = *on;
             Endif;
          Else;
             *in60 = *on;
          Endif;
       Endif;

       For intCount = 1 to 12;
          If dsBill(intCount) <> *Zero or isProcAll <> 'Y';
             If isProcAll = 'Y';

                xxDuDt = 0;
                srDtI = intPfDt;
                Exsr cvtDate;
                qqDate = srDtO;

                srDtI = intPfDt;
                srDti = (%Div(srDtI:100) * 100) + MD(MM1);

                Exsr cvtDate;
                qqDat2 = srDtO;


                If qqDate <> ' ';
                   xxDate = %Int(%Trim(qqDate));
                   srDtI = xxDate;
                   Exsr cvtDtCYMD;
                   If srDtO <> ' ';
                      xxPoDt = %Int(%trim(srDtO));
                   Endif;
                Endif;

                // Build First day of installment month

                xxPO01 = 0;
                xxPO01 = (xxPCYM * 100) + 1;

                // Build Last day of installment month
                xxPO31 = xxPO01 + 30;

                If qqDate <> ' ';
                   srDtI = %Int(%Trim(qqDate));
                   Exsr cvtDtCYMD;
                   If srDtO <> ' ';
                      xxIfDt = %Int(%trim(srDtO));
                   Endif;
                Endif;

                If qqDat2 <> ' ';
                   srDtI = %Int(%Trim(qqDat2));
                   Exsr cvtDtCYMD;
                   If srDtO <> ' ';
                      xxItDt = %Int(%trim(srDtO));
                   Endif;
                Endif;
             Endif;
      /end-free
     C     CLTID         IFEQ      '093'
     C                   MOVE      XXFYR         SVFYR             3 0
     C                   MOVE      EBFYR         XXFYR
     C     AUD93         IFEQ      *ON
     C     EAKY          SETGT     PEA01                              6060
     C   60*HIVAL        SETGT     PEA01
     C     EAKY          READPE    PEA01                                  60
     C  N60              Z-ADD     EAPODT        XXPO01
     C  N60              Z-ADD     EAPODT        XXPO31
     C                   ELSE
     C                   Z-ADD     XXIFDT        XXPO01
     C                   Z-ADD     XXITDT        XXPO31
     C                   ENDIF
     C                   MOVE      SVFYR         XXFYR
     C                   ENDIF
     C                   Z-ADD     *ZEROS        SVFND
     C                   Z-ADD     *ZEROS        SVEMP#
     C                   Z-ADD     *ZEROS        SVDIV
      *
      *  SKIP ACH EMPLOYER UNLESS SPECIFICALLY PROMPTED FOR
      *
     C*
     C* CHECK IF NEW ACH MODULE IS INSTALLED ('WC1806')
     C*
      *
      * SKIP THE EFT CHECK IF RUN IN SINGLE MODE OR IF CALLED FROM BILLING
       isSelected = *On;
       chrEftKey = %Editc(xxEmp#:'X') + %Editc(xxDiv:'X');
       chrEfDt = %Char(%Date():*Cymd0);
       chrFyr = %Editc(xxFyr:'X');
       Select;
       When *In24 = *On; // single emp# run
       When GetEftStatus(qqCo#:chrEftType:qqFnd:chrEftKey:
        chrEfDt:chrPre:chrRRN:chrEobType:chrChkType:chrFyr) = *On;
          isSelected = *Off; // skip in mass if employer is on EFT
       Endsl;
      *
     C                   MOVE      QQCO#         @@CO#
     C                   MOVE      QQFND         @@FND
     C                   Z-ADD     EBEMP#        @@EMP#
     C                   Z-ADD     EBDIV         @@DIV
     C                   Z-ADD     EBFYR         @@FYR
     C                   MOVE      QQDATE        XXRNDT            8 0
     C                   Z-ADD     XXRNDT        @@DATE
     C                   MOVEL     '*'           @@PLAN
      *
     C                   CALL      'WA002R'
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@EMP#            9 0
     C                   PARM                    @@DIV             5 0
     C                   PARM                    @@FYR             3 0
     C                   PARM      *zeros        @@AGT#            5 0
     C                   PARM      *blanks       @@ANAM           60
     C                   PARM                    @@PROD            5 0
     C                   PARM                    @@PNAM           60
     C                   PARM                    @@AREA            3 0
     C                   PARM                    @@PHON            7 0
     C                   PARM                    @@PLAN            1
     C                   PARM                    @@DATE            8 0
      *
     C     @@PLAN        IFEQ      'A'
                         isSelected = *Off;
     C                   ENDIF
     C** EXCLUDE AUDITS BASED ON INTERMIM DAYS WHEN RUNNING RENEWALS
     C                   MOVE      *ZEROS        XXAUDT
     C                   Z-ADD     EBFYR         XXFYRP
     C                   EXSR      SRINDT
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXIDT             7 0
     C*

     C                   MOVE      EBFYR         XXFYR
     c     elkey         chain     pel01
     c                   if        xxaudt > xxidt and
     c                             ELFAF = 'A'
     c                             and qqemp# = *zero
     C                             Or (*in24 = *Off And
     C                             ELprmNtMas = 'Y')
                         isSelected = *Off;
     c                   endif
     C                   eval      x#RRN = dbRRN
      * READ THE PRIOR FUND YEAR TO DETERMINE IF THE EMPLOYER HAS AN
      * UNCONTESTED AUDIT
      *
     C     EBFYR         SUB       1             XXFYRP                         GT PRIOR FYR
     C                   MOVE      XXFYRP        XXFYR
     C                   MOVE      'N'           XXPAUD
      *
     C     ELKEY         CHAIN     PEL01                              60
      *
      *
      * If no prior year, check employer's cross reference record.
      *
     C     *IN60         IFEQ      *ON
      *
     C     EMKEY         CHAIN     ZEX01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     XXFND         SVFND
     C                   Z-ADD     XXEMP#        SVEMP#
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     ZXOFND        XXFND
     C                   Z-ADD     ZXOEMP        XXEMP#
     C                   Z-ADD     ZXODIV        XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDIF                                                  *IN60 = *ON
      *
     C     *IN60         IFEQ      *OFF
      *
     C                   CALL      'WA002R'
     C                   PARM      ELCO#         #1CO#             3 0
     C                   PARM      ELFND         #1FND             3 0
     C                   PARM      ELEMP#        #1EMP#            9 0
     C                   PARM      ELDIV         #1DIV             5 0
     C                   PARM      ELFYR         #1FYR             3 0
     C                   PARM      *zeros        #1AGT#            5 0
     C                   PARM      *blanks       #1ANAM           60
     C                   PARM                    #1PROD            5 0
     C                   PARM                    #1PNAM           60
     C                   PARM                    #1AREA            3 0
     C                   PARM                    #1PHON            7 0
     C                   PARM      '*'           #1PLAN            1
     C                   PARM      *ZERO         #1DATE            8 0
      *
      *
     C     ELFAF         IFEQ      'A'
     C     ELCTST        ANDNE     'Y'
     C     #1PLAN        ANDNE     'A'
      *
      * INCLUDE PRIOR YEAR AUDIT BALANCE ONLY IF THERE IS NOT A RECORD FOR THE
      * PRIOR YEAR ALREADY IN THE WORKFILE
      *
     C     EBKY          CHAIN     PEB02                              60
      *
     C     *IN60         IFEQ      *ON
     C                   MOVE      'Y'           XXPAUD
     C                   ENDIF                                                  *IN60 IFEQ *ON
     C*
     C*  Don't print the prior year unpaid balances if the prior year audit
     C*  date + interim days is older than the installment date.
     C                   EXSR      SRINDT
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXIDT             7 0
     C*
     C     XXAUDT        IFGT      XXIDT
     C                   MOVE      'N'           XXPAUD
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELFAF IFEQ 'A'
      *
     C                   ENDIF                                                  *IN60 IFEQ *OFF
      *
      * Restore key values if employer's cross reference was used.
      *
     C     SVFND         IFNE      *ZERO
     C     SVEMP#        ANDNE     *ZERO
     C                   Z-ADD     SVFND         XXFND
     C                   Z-ADD     SVEMP#        XXEMP#
     C                   Z-ADD     SVDIV         XXDIV
     C                   ENDIF
      *
     C                   MOVE      EBFYR         XXFYR
      *
      *   START READING THE EMPLOYER DETAIL FILE
      *
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     **
     **   LOAD POLICY NUMBER
     **
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   EXSR      SRP71#
     C                   OTHER
     C                   CALL      'WA022R'
     C                   PARM      *BLANKS       XXPLCY           15
     C                   PARM                    XXEMP#
     C                   PARM                    XXFND
     C                   PARM                    XXFYR
     C                   ENDSL
     C*
     C                   MOVE      *OFF          *IN72
     C*
     C*   If a carrier pol# exists then format it for display on report
     C*
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C*
     C                   MOVE      *ON           *IN72
     C                   MOVEL(P)  RPCAP#        QQSTRG
     C*
     C*     Call api to right justify string
     C                   CALL      'SA004R'
     C                   PARM                    QQSTRG           80
     C*
     C                   MOVE(P)   QQSTRG        XXCAP#
     C                   MOVEL     CARPOL        XXCAP#
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      *OFF          *IN43
     C*   Call subroutine to process for monthly, renewal or all
     C                   EXSR      SRRMA
     C     *IN43         IFEQ      *OFF
                         isSelected = *Off;
     C                   ENDIF
      *
      *
      *  SEE IF POLICY HAS A QUALIFYING A/R TRANSACTION IN THE DATE RANGE
      *
     C                   MOVE      *OFF          QTRNFL            1            QUAL TRANS FLAG
     C                   Z-ADD     *ZERO         SVFPDT            7 0          FIRST POST DATE
     C                   Z-ADD     *ZERO         SVLPDT            7 0          LAST POST DATE
     C     EAKY2         SETGT     PEA01
     C     EAKY          READPE    PEA01                                  60
     C     *IN60         DOWEQ     *OFF
       // Skip A/R records if invoice passed in and A/R type set to skip
       If SkipICAR(xxCo#:invoice#:EAnInv:EAarCd);
          ReadPE EAky pea01;
          If %Eof(wmeal6);
            *in60 = *On;
          EndIf;
          Iter;
       EndIf;
      *
     C     EAPODT        IFLT      XXIFDT
     C                   LEAVE
     C                   ENDIF
      *
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   SELECT
     C     QQEXDF        WHENEQ    'Y'
     C     TTCTYP        ANDEQ     'R'
     C     EAARTY        WHENEQ    'A'                                          ADJUSTMENTS
     C     EAARCD        ANDNE     F1RDCD
     C     EAARTY        OREQ      'B'                                          BILLINGS
     C     EAARCD        ANDNE     F1DVBL
     C     EAARTY        OREQ      'D'                                          AUDITS
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'H'
     C     XXDEP         ANDEQ     'Y'
     C                   MOVE      *ON           QTRNFL            1
     C     SVLPDT        IFEQ      *ZERO
     C                   Z-ADD     EAPODT        SVLPDT
     C                   ENDIF
     C     SVFPDT        IFEQ      *ZERO
     C     EAPODT        ORLT      SVFPDT
     C                   Z-ADD     EAPODT        SVFPDT
     C                   ENDIF
     C                   ENDSL
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C     EAKY          READPE    PEA01                                  60
     C                   ENDDO
      *
      * CHECK UNTRANSFERRED BATCH RECORDS FOR RETRO SETTLEMENTS AS WELL.
      *
     C     *IN67         IFEQ      *ON
     C     QTRNFL        ANDEQ     *OFF
     C     EAKEY2        SETGT     PEA03B
     C     EAKEY3        READPE    PEA03B                                 60
     C     *IN60         DOWEQ     *OFF
       // Skip A/R records if invoice passed in and A/R type set to skip
       If SkipICAR(xxCo#:invoice#:EAinv#:EAarCd);
          ReadPE EAkey3 pea03b;
          If %Eof(wdeal2);
             *in60 = *On;
          EndIf;
          Iter;
       EndIf;
      *
     C     EAPODT        IFLT      XXIFDT
     C                   LEAVE
     C                   ENDIF
      *
     C     EATFFL        IFNE      'Y'
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'C'
     C     TTTYPE        ANDEQ     'B'
     C                   MOVE      *ON           QTRNFL            1
     C     SVLPDT        IFEQ      *ZERO
     C                   Z-ADD     EAPODT        SVLPDT
     C                   ENDIF
     C     SVFPDT        IFEQ      *ZERO
     C     EAPODT        ORLT      SVFPDT
     C                   Z-ADD     EAPODT        SVFPDT
     C                   ENDIF
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  EATFFL IFNE
      *
     C     EAKEY3        READPE    PEA03B                                 60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   ENDIF                                                  *IN67 IFEQ
     C*
      *
      *  PRINT INVOICE, IF AUDIT (ECM SPECIFIC), SINGLE EMP RUN,
      *  OR CALLED FROM BILLING,
      *
     C     *IN24         IFEQ      *ON
     C     AUDIT         OREQ      *ON
     C     XXMPNT        OREQ      *ON
     C                   MOVE      *ON           QTRNFL
     C                   ENDIF
      *
     C     QTRNFL        IFEQ      *OFF
                         isSelected = *Off;
     C                   ELSE
     C                   EXSR      SRDUDT
     C                   ENDIF

       If isSelected;
      *
      *   CHAIN TO THE FUND DETAIL TO GET DEDUCTIBLE BILLED/PAID CODES
      *
     C     F1KEY         CHAIN     PF101                              60
      *
      * GET BEGINNING YEAR AND END YEAR VALUES FOR TASB
      *
     C     F1STPO        ADD       19000000      WRKDAT            8 0
     C                   MOVEL     WRKDAT        BEGYR             4
     C     F1ENPO        ADD       19000000      WRKDAT
     C                   MOVEL     WRKDAT        WRKYR             4
     C                   MOVE      WRKYR         EXPYR             2
      *
      *   CHECK THE EFFCTIVE AND CANCELLATION DATES
      *
     C     EMKEY         CHAIN     PEM01                              60
     C     *IN60         IFEQ      *OFF
     **
     **  IF FEDERAL NAME IS NOT BLANK THEN PRINT IT
     **
     C                   MOVEL(P)  EMFDNM        XXFDNM           38
     C                   MOVE      *BLANKS       XXUNM2
     C                   MOVE      *BLANKS       XXAREA
     C                   MOVE      *BLANKS       XXPHN1
     C                   MOVE      *BLANKS       XXPHN2
      *
      * GET THE OFFICE INFORMATION
      *
     C     OFKY          CHAIN     POF01                              60
     C     *IN60         IFEQ      *OFF
       // Get the 3rd Party Verbiage if policy is opted in for Invoice Cloud
       // or if called from WIAR
       *in76 = *off;
       If (ICPaperlessOpt(xxCo#:xxFnd:xxEmp#) <> 0 or xxWiArPgm = *on)
        and OF3rdVrb <> *blanks;
          *in76 = *on;
          str3rdVrb = OF3rdVrb;
          str3rdVrb2 = OF3rdVrb;
       Endif;
      *
     C                   Z-ADD     1             X                 2 0
     C     *BLANK        LOOKUP    ACNM(X)                            60
     C                   MOVEA     ACNM(X)       XXCONM           40
      *
      * If Self-INsured Group, use Group Name intead of Office Acct Name
     C   66              MOVEL(P)  OFACNM        XXACNM           40
     C  N66              MOVEL     FNNAME        XXACNM
     C     CLTID         IFEQ      '025'
     C                   MOVEL(P)  OFCKNM        XXACNM           40
     C                   ENDIF
     C                   MOVEL(P)  OFCKNM        XXCKNM           40
     C     OFAAD2        IFEQ      *BLANK
     C                   MOVE      *ON           *IN80
     C                   ELSE
     C                   MOVE      *OFF          *IN80
     C                   ENDIF
     C                   MOVEL     OFAST         XXSTAT            2
     C                   MOVEL     OFAZIP        XXZIP1            5
     C                   MOVE      OFAZIP        XXZIP2            4
     C                   MOVEL     OFACTY        XXCITY           25
     C                   Movel     OFACTY        XXoCity
     C                   Movel     OFAST         XXoSt
     C                   Z-Add     OFAZIP        XXoZip
     C                   EXSR      SR1ST
     C                   MOVEL(P)  XXCSTZ        XOACSZ           40
     C                   MOVEL(P)  XXACNM        XXOFL1
     C                   MOVEL(P)  OFAAD1        XXOFL2
     C                   MOVEL(P)  OFAAD2        XXOFL3
     C                   MOVEL(P)  XOACSZ        XXOFL4
      *** Beg Add ***** 10/30/24 *************************************
       xxAAD1 = OFAAD1;
       xxAAD2 = OFAAD2;
      *** End Add ***** 10/30/24 *************************************
      *  If EXISTS use LOCK BOX address instead of Accounting Ofc addr
      *  by Moving LOCK BOX address fields into accunting Ofc addr flds
     C     XXLKBX        IFEQ      'Y'
     C     F2LBNM        IFNE      *BLANKS
     C                   MOVEL(P)  F2LBNM        XXACNM
     C                   ELSE
     C                   MOVEL(P)  XXACNM        XXACNM
     C                   ENDIF
      *** Beg Add ***** 10/30/24 *************************************
       xxAAD1 = F2LAD1;
       xxAAD2 = F2LAD2;
      *** End Add ***** 10/30/24 *************************************
      *** Beg Add ***** 05/22/24 *************************************
       XXoCity = F2LCTY;
       XXoSt = F2LST;
       XXoZip = F2LZIP;
      *** End Add ***** 05/22/24 *************************************
     C                   MOVEL(P)  F2LAD2        XXLBA2
     C                   MOVEL     F2LST         XXSTAT            2
     C                   MOVEL     F2LZIP        XXZIP1            5
     C                   MOVE      F2LZIP        XXZIP2            4
     C                   MOVEL     F2LCTY        XXCITY           25
     C                   EXSR      SR1ST
     C     F2LAD2        IFNE      *BLANKS
     C                   MOVEL(P)  XXCSTZ        XXLBA3           40
     C                   ELSE
     C                   MOVEL(P)  XXCSTZ        XXLBA2           40
     C                   ENDIF                                                  F2LBA2 IFNE
     C*
     C     F2LBNM        IFNE      *BLANKS
     C                   MOVEL(P)  F2LBNM        LBACNM           40
     C                   ELSE
     C                   MOVEL(P)  XXACNM        LBACNM
     C                   ENDIF
     C                   MOVEL(P)  F2LAD1        LBAAD1           40
     C                   MOVEL(P)  F2LAD2        LBAAD2           40
     C                   MOVEL(P)  XXCSTZ        LBACSZ           40
     C                   MOVEL(P)  XXACNM        XXOFL1
     C                   MOVEL(P)  LBAAD1        XXOFL2
     C                   MOVEL(P)  LBAAD2        XXOFL3
     C                   MOVEL(P)  LBACSZ        XXOFL4
     C                   ENDIF
     C     XXOFL3        IFEQ      *BLANKS
     C                   MOVEL     XXOFL4        XXOFL3
     C                   CLEAR                   XXOFL4
     C                   ENDIF
     C     OFAPHN        IFNE      *ZERO
     C                   MOVEL     OFAPHN        XXPHN1            3 0
     C                   MOVE      OFAPHN        XXPHN2            4 0
     C                   Z-ADD     OFAARA        XXAREA            3 0
     C                   ENDIF
     C                   MOVEL(P)  OFPRNM        XXUNM2           36
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     **
     C     XXFDNM        IFNE      *BLANKS
     C                   MOVE      *ON           *IN77
     C                   ENDIF
     C                   EXCEPT    PEM01E
     C                   ENDIF
      *
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   ENDIF
      *
     C     Y             DOWNE     *ZERO
     C     ELPTDT        IFGE      ED(Y)
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             Y
     C                   ENDDO
      *
      *
      *  GET PENDING CX VERBIAGE FOR NOTICE
      *
     C                   MOVE      *OFF          *IN73
     C                   CLEAR                   XXCXDT
     C                   CLEAR                   XXRESN
      *
     C     CD(Y)         IFGE      XXPODT                                       FUTURE CANCEL
     C     CLTID         ANDNE     '071'
      *
     C                   MOVE      *ON           *IN73
     C                   Z-ADD     CD(Y)         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXCXDT            8 0
      *
      *  GET RATING STATE
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C                   ELSE
     C                   MOVE      *BLANKS       XXST
     C                   ENDIF
      *
      *  GET CANCELLATION CODE
      *
     C                   Z-ADD     CD(Y)         KKCXDT            7 0
     C     CXKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  CXRSN         XXKEY            10
     C                   ELSE
     C                   MOVE      *BLANKS       XXKEY
     C                   ENDIF
      *
      *  GET CANCELLATION REASON
      *
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CRDESC        XXRESN           60
     C                   ELSE
     C                   MOVE      *BLANKS       XXRESN
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     CD(Y)         IFGE      XXPODT                                       FUTURE CANCEL
     C     CD(Y)         OREQ      *ZERO                                        NOT CANCEL
     C     *IN24         OREQ      *ON                                          SINGLE EMPLR
     C     *IN67         OREQ      *ON                                          SETTLEMENT
     C     AUDIT         OREQ      *ON
     C     XXMPNT        OREQ      *ON                                          MAND PREM NOTIC
     C     qqGrma        OREQ      'A'
     C*
     C*
     C* IF POLICY IS A/R SUMMARY AND PRINTING STATE REPORTING DIVISION
     C* THEN BUILD AN ARRAY OF HEADINGS TO USE FOR THE ASSESSMENTS.
     C*
     C                   CLEAR                   ASS
     C                   CLEAR                   ASH
     C*
     C     ELARSM        IFEQ      'Y'
     C     EMSTRP        ANDEQ     'Y'
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C                   ELSE
     C                   MOVE      *BLANKS       XXST
     C                   ENDIF
     C*
     C                   MOVE      ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C* 1ST ASSESSMENT HEADING.
     C*
     C     RR1STH        IFNE      *BLANKS
     C     ASS(1)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(1)
     C                   MOVEL     RR1STH        ASH(1)
     C                   ELSE
     C     XXST          IFNE      ASS(1)
     C                   MOVE      *BLANKS       ASH(1)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR1STH IFNE
     C*
     C* 2ND ASSESSMENT HEADING.
     C*
     C     RR2NDH        IFNE      *BLANKS
     C     ASS(2)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(2)
     C                   MOVEL     RR2NDH        ASH(2)
     C                   ELSE
     C     XXST          IFNE      ASS(2)
     C                   MOVE      *BLANKS       ASH(2)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR2NDH IFNE
     C*
     C* 3RD ASSESSMENT HEADING.
     C*
     C     RR3RDH        IFNE      *BLANKS
     C     ASS(3)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(3)
     C                   MOVEL     RR3RDH        ASH(3)
     C                   ELSE
     C     XXST          IFNE      ASS(3)
     C                   MOVE      *BLANKS       ASH(3)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR3RDH IFNE
     C*
     C* 4TH ASSESSMENT HEADING.
     C*
     C     RR4THH        IFNE      *BLANKS
     C     ASS(4)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(4)
     C                   MOVEL     RR4THH        ASH(4)
     C                   ELSE
     C     XXST          IFNE      ASS(4)
     C                   MOVE      *BLANKS       ASH(4)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR4THH IFNE
     C*
     C* 5TH ASSESSMENT HEADING.
     C*
     C     RR5THH        IFNE      *BLANKS
     C     ASS(5)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(5)
     C                   MOVEL     RR5THH        ASH(5)
     C                   ELSE
     C     XXST          IFNE      ASS(5)
     C                   MOVE      *BLANKS       ASH(5)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR5THH IFNE
     C*
     C* 6TH ASSESSMENT HEADING.
     C*
     C     RR6THH        IFNE      *BLANKS
     C     ASS(6)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(6)
     C                   MOVEL     RR6THH        ASH(6)
     C                   ELSE
     C     XXST          IFNE      ASS(6)
     C                   MOVE      *BLANKS       ASH(6)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR6THH IFNE
     C*
     C* 7TH ASSESSMENT HEADING.
     C*
     C     RR7THH        IFNE      *BLANKS
     C     ASS(7)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(7)
     C                   MOVEL     RR7THH        ASH(7)
     C                   ELSE
     C     XXST          IFNE      ASS(7)
     C                   MOVE      *BLANKS       ASH(7)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR7THH IFNE
     C*
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* SET ARRAYS EQUAL TO HEADINGS FOR THE STATE.
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C                   ELSE
     C                   MOVE      *BLANKS       XXST
     C                   ENDIF
     C*
     C                   MOVE      ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C     RR1STH        IFNE      *BLANKS
     C     ASS(1)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(1)
     C                   MOVEL     RR1STH        ASH(1)
     C                   ELSE
     C     XXST          IFNE      ASS(1)
     C                   MOVE      *BLANKS       ASH(1)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR1STH IFNE
     C*
     C     RR2NDH        IFNE      *BLANKS
     C     ASS(2)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(2)
     C                   MOVEL     RR2NDH        ASH(2)
     C                   ELSE
     C     XXST          IFNE      ASS(2)
     C                   MOVE      *BLANKS       ASH(2)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR2NDH IFNE
     C*
     C     RR3RDH        IFNE      *BLANKS
     C     ASS(3)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(3)
     C                   MOVEL     RR3RDH        ASH(3)
     C                   ELSE
     C     XXST          IFNE      ASS(3)
     C                   MOVE      *BLANKS       ASH(3)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR3RDH IFNE
     C*
     C     RR4THH        IFNE      *BLANKS
     C     ASS(4)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(4)
     C                   MOVEL     RR4THH        ASH(4)
     C                   ELSE
     C     XXST          IFNE      ASS(4)
     C                   MOVE      *BLANKS       ASH(4)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR4THH IFNE
     C*
     C     RR5THH        IFNE      *BLANKS
     C     ASS(5)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(5)
     C                   MOVEL     RR5THH        ASH(5)
     C                   ELSE
     C     XXST          IFNE      ASS(5)
     C                   MOVE      *BLANKS       ASH(5)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR5THH IFNE
     C*
     C     RR6THH        IFNE      *BLANKS
     C     ASS(6)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(6)
     C                   MOVEL     RR6THH        ASH(6)
     C                   ELSE
     C     XXST          IFNE      ASS(6)
     C                   MOVE      *BLANKS       ASH(6)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR6THH IFNE
     C*
     C     RR7THH        IFNE      *BLANKS
     C     ASS(7)        IFEQ      *BLANKS
     C                   MOVE      XXST          ASS(7)
     C                   MOVEL     RR7THH        ASH(7)
     C                   ELSE
     C     XXST          IFNE      ASS(7)
     C                   MOVE      *BLANKS       ASH(7)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  RR7THH IFNE
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C*
     C* RESET EMPLOYER DETAIL CHAIN.
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      RSJRST        XXST
     C                   ELSE
     C                   MOVE      *BLANKS       XXST
     C                   ENDIF
     C*
     C                   MOVE      ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *
     C     F1KEY         CHAIN     PF101                              60
     C                   MOVEL(P)  ELBSCD        XXKEY
       //* Reset fields for S/R billings array.
       Clear srDates;
       Clear srBilledPrem;
       Clear srBilledTax;
       Clear srBilledFee;
       Clear premPaid;
       Clear taxPaid;
       Clear instPaid;
       x = 1;

       //* Determine if invoice # is a credit, then retrieve current balance
       //* below for premium notice.
       If xxWIARPGM = *on;
          xxAmtD = 0;
          isAudit = *off;
          Setll eaky pea01;
          Reade(n) eaky pea01;
          Dow not %eof;

             If not SkipICAR(xxCo#:invoice#:EAnInv:EAarCd);
                If EAARTy = 'A' or EAARTy = 'B' or EAARTy = 'D' or
                 EAARTy = 'F' or EAARTy = 'H';
                   xxAmtD += EAAmt;
                EndIf;
                If EAARTy = 'D';
                   isAudit = *on;
                EndIf;
             EndIf;

             Reade(n) eaky pea01;
          Enddo;

          //* Do not restrict a/r reads below on invoice # if invoice is
          //* a credit, since those will send to IC as current balance due.
          If xxAmtD < 0 or isAudit;
             invoice# = 0;
          Endif;

          Chain tbkey pbs01;
          //* Ignore S/R billings for S/R account using current balance.
          If %found and BSSefl = 'S' and not isAudit and invoice# = 0;
             Setll eaky pea01;
             Reade(n) eaky pea01;
             Dow not %eof;
                If EAArcd = F1SRBB;
                   If %lookup(EAPodt:srDates) = 0 and x < 99;
                      srDates(x) = EAPodt;
                      x += 1;
                   Endif;
                Endif;
                Reade(n) eaky pea01;
             Enddo;
          Endif;

       Endif;
     C                   Z-ADD     *ZERO         XXAMTD            9 2          PREV UNPAID BAL
     C                   Z-ADD     *ZERO         XXFEED            9 2          OTH FEES DUE
     C                   Z-ADD     *ZERO         XXDEDT            9 2          DED FEES DUE
     C                   Z-ADD     *ZERO         XXMBFT            9 2          MBR FEES DUE
     C                   Z-ADD     *ZERO         XXEXCN            9 2          EXP CONST DUE
     C                   Z-ADD     *ZERO         XXAMTP            9 2          PRIOR YR BAL
     C                   Z-ADD     *ZERO         XXAMT             9 2          CURR AMT DUE
     C                   Z-ADD     *ZERO         XXCMAJ            9 2          CURR MTH ADJ
     C                   Z-ADD     *ZERO         XPPRMC            9 2          TASB PRV PRM CHG
     C                   Z-ADD     *ZERO         XPPRMP            9 2          TASB PRV PRM PAY
     C                   Z-ADD     *ZERO         XCOTHC            9 2          TASB CUR OTH CHG
     C                   Z-ADD     *ZERO         XPOTHC            9 2          TASB PRV OTH CHG
     C                   Z-ADD     *ZERO         XPOTHP            9 2          TASB PRV OTH PAY
     C                   Z-ADD     *ZERO         XPTAXC            9 2          TASB PRV TAX CHG
     C                   Z-ADD     *ZERO         XPTAXP            9 2          TASB PRV TAX PAY
     C                   Z-ADD     *ZERO         XP1STC            9 2          TASB PRV 1ST CHG
     C                   Z-ADD     *ZERO         XP1STP            9 2          TASB PRV 1ST PAY
     C                   Z-ADD     *ZERO         XP2NDC            9 2          TASB PRV 2ND CHG
     C                   Z-ADD     *ZERO         XP2NDP            9 2          TASB PRV 2ND PAY
     C                   Z-ADD     *ZERO         XP3RDC            9 2          TASB PRV 3RD CHG
     C                   Z-ADD     *ZERO         XP3RDP            9 2          TASB PRV 3RD PAY
     C                   Z-ADD     *ZERO         XP4THC            9 2          TASB PRV 4TH CHG
     C                   Z-ADD     *ZERO         XP4THP            9 2          TASB PRV 4TH PAY
     C                   Z-ADD     *ZERO         XP5THC            9 2          TASB PRV 5TH CHG
     C                   Z-ADD     *ZERO         XP5THP            9 2          TASB PRV 5TH PAY
     C                   Z-ADD     *ZERO         XP6THC            9 2          TASB PRV 6TH CHG
     C                   Z-ADD     *ZERO         XP6THP            9 2          TASB PRV 6TH PAY
     C                   Z-ADD     *ZERO         XP7THC            9 2          TASB PRV 7TH CHG
     C                   Z-ADD     *ZERO         XP7THP            9 2          TASB PRV 7TH PAY
     C                   Z-ADD     *ZERO         XCDEDC            9 2          TASB CUR DED CHG
     C                   Z-ADD     *ZERO         XPDEDC            9 2          TASB PRV DED CHG
     C                   Z-ADD     *ZERO         XPDEDP            9 2          TASB PRV DED PAY
     C                   Z-ADD     *ZERO         XCMBFC            9 2          TASB CUR MBF CHG
     C                   Z-ADD     *ZERO         XPMBFC            9 2          TASB PRV MBF CHG
     C                   Z-ADD     *ZERO         XPMBFP            9 2          TASB PRV MBF PAY
     C                   Z-ADD     *ZERO         XCEXPC            9 2          TASB CUR EXP CHG
     C                   Z-ADD     *ZERO         XPEXPC            9 2          TASB PRV EXP CHG
     C                   Z-ADD     *ZERO         XPEXPP            9 2          TASB PRV EXP PAY
     C                   Z-ADD     *ZERO         XPDEPC            9 2          TASB PRV DEP CHG
     C                   Z-ADD     *ZERO         XPDEPP            9 2          TASB PRV DEP PAY
     C                   Z-ADD     *ZERO         XCINFC            9 2          TASB CUR INF CHG
     C                   Z-ADD     *ZERO         XPINFC            9 2          TASB PRV INF CHG
     C                   Z-ADD     *ZERO         XPINFP            9 2          TASB PRV INF PAY
     C                   Z-ADD     *ZERO         XXTAX             9 2          CURR TAX DUE
     C                   Z-ADD     *ZERO         XXTAXD            9 2          CURR TAX DUE
     C                   Z-ADD     *ZERO         XXTAXP            9 2          PREV UNPD TAX
     C*** Begin Add ***  06/24/24  *********************************************
     C                   Z-ADD     *ZERO         XXCOLD            9 2          Collateral Due
     C*** End   Add ***  06/24/24  *********************************************
     C*
     C                   Z-ADD     *ZERO         XX1ST             9 2                      BAL
     C                   Z-ADD     *ZERO         XX1STP            9 2                      BAL
     C                   Z-ADD     *ZERO         XX2ND             9 2                      BAL
     C                   Z-ADD     *ZERO         XX2NDP            9 2                      BAL
     C                   Z-ADD     *ZERO         XX3RD             9 2                      BAL
     C                   Z-ADD     *ZERO         XX3RDP            9 2                      BAL
     C                   Z-ADD     *ZERO         XX4TH             9 2                      BAL
     C                   Z-ADD     *ZERO         XX4THP            9 2                      BAL
     C                   Z-ADD     *ZERO         XX5TH             9 2                      BAL
     C                   Z-ADD     *ZERO         XX5THP            9 2                      BAL
     C                   Z-ADD     *ZERO         XX6TH             9 2                      BAL
     C                   Z-ADD     *ZERO         XX6THP            9 2                      BAL
     C                   Z-ADD     *ZERO         XX7TH             9 2                      BAL
     C                   Z-ADD     *ZERO         XX7THP            9 2                      BAL
     C*
     C                   Z-ADD     *ZERO         XXFEEP            9 2          PREV UNPD FEES
     C                   Z-ADD     *ZERO         XDEPPV            9 2          DEPOSIT UNPAID BALAN
     C                   Z-ADD     *ZERO         XDEPCU            9 2          DEPOSIT CURR BILLED
     C                   MOVE      *OFF          *IN30                          PRT DEP UNPAID BAL
     C                   MOVE      *OFF          *IN31                          PRT DEP CURR BILLED
     C*
     C*
     C                   MOVE      *OFF          *IN88                          PRT OTH FEES DUE
     C                   MOVE      *OFF          *IN82                          PRT DED FEES
     C                   MOVE      *OFF          *IN83                          PRT EXP CONST
     C                   MOVE      *OFF          *IN86                          PRT PREV FYR AMT
     C                   MOVE      *OFF          *IN87                          PRT MBR FEES
     C                   MOVE      *OFF          *IN89                          PRT PR YR TAX
     C                   MOVE      *OFF          *IN90                          PRT CURR TAX
     C                   MOVE      *OFF          *IN32
     C                   MOVE      *OFF          *IN33
     C                   MOVE      *OFF          *IN34
     C                   MOVE      *OFF          *IN35
     C                   MOVE      *OFF          *IN36
     C                   MOVE      *OFF          *IN37
     C                   MOVE      *OFF          *IN38
     C                   MOVE      *OFF          *IN39
     C                   MOVE      *OFF          *IN41
     C                   MOVE      *OFF          *IN42
     C                   MOVE      *OFF          *IN43
     C                   MOVE      *OFF          *IN44
     C                   MOVE      *OFF          *IN45
     C                   MOVE      *OFF          *IN46
     C*** Begin Add ***  06/24/24  *********************************************
     C                   MOVE      *OFF          *IN48
     C*** End   Add ***  06/24/24  *********************************************
     C*
     C                   MOVE      *OFF          *IN91                          PRT PR YR FEES
     C                   MOVE      *OFF          *IN92                          CUR MTH ADJUST
     C                   Clear                   xxColt            1
      /free
       Open wmcol;
       Chain(n) (XXCO#:XXFND:XXEMP#) PCO00;
       If %found;
          xxColt = 'Y';
       Endif;
       Close wmcol;
      /end-free
     C                   Z-ADD     *ZERO         XXIFAM            9 2          INSTALL FEES
     C                   MOVE      *OFF          *IN84                          INSTALL FEEST
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C     IFKY          SETGT     PIF01
     C     IFKY1         READPE    PIF01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IFIFBL
     C                   MOVE      *BLANKS       IFIFPD
     C                   MOVE      *BLANKS       IFIFTR
     C                   MOVE      *BLANKS       IFIFTC
     C                   Z-ADD     0             IFIFEE
     C                   ENDIF
      *
      *   GET THE NAME OF THE MONTH
      *
     C                   MOVEL     XXDATE        MM                2 0
     C                   MOVEL(P)  MN(MM)        XXDESC
     C                   SELECT
     C     CLTID         WHENEQ    '071'
      /Free
       If Audit = *on or qqGrma = 'A';
      /End-free
     C                   MOVEL(P)  AUDTIT        XXDESC
     C                   ENDIF
     C                   CAT       'Billed':1    XXDESC
     C                   CAT       'Contri':1    XXDESC
     C                   CAT       'bution':0    XXDESC
     C                   OTHER
      /Free
       If isContri = '1';
          xxDesc = xxDesc+' '+'Billed'+' '+'Contribution';
       Else;
      /End-free
     C                   CAT       'Billed':1    XXDESC
     C                   CAT       'Premiu':1    XXDESC
     C                   CAT       'm':0         XXDESC
      /Free
       Endif;
      /End-free
     C                   ENDSL
     C     *IN67         IFEQ      *ON
     C     CLTID         ANDNE     '071'
     C                   MOVEL(P)  'Settle'      XXDESC
     C                   CAT       'ment':0      XXDESC
     C                   CAT       'Amount':1    XXDESC
     C                   ENDIF
     C                   MOVEL(P)  MN(MM)        X1DESC
     C                   If        xxColt = 'Y'
     C                   CAT       'Billed':1    X1DESC
     C                   CAT       'Collat':1    X1DESC
     C                   CAT       'eral':0      X1DESC
     C                   Else
     C                   CAT       'Billed':1    X1DESC
     C                   CAT       'Deposi':1    X1DESC
     C                   CAT       't':0         X1DESC
     C                   Endif
      *
      *  TURN ON THE DEPOSIT MODE BY YEAR FLAG (XXDPYM) IF THE POLICY YEAR
      *  IS DEFINED AS SUCH IN WDF1P
      *
     C     F1DPYM        IFEQ      'Y'
     C     CLTID         OREQ      '140'
     C                   MOVE      'Y'           XXDPYM            1
     C                   ELSE
     C                   MOVE      'N'           XXDPYM            1
     C                   ENDIF
      *
      *
      *   CALCULATE THE REPORT AMOUNTS
      *
     C     *LIKE         DEFINE    EANINV        SVNINV
     C                   Z-ADD     0             SVNINV
     C     EAKY          SETLL     PEA01
     C     EAKY          READE     PEA01                                  60
     C     *IN60         DOWEQ     *OFF
     C     EAPODT        ANDLE     XXPO31
       // Skip A/R records if invoice passed in and A/R type set to skip
       If SkipICAR(xxCo#:invoice#:EAnInv:EAarCd:EAPodt:EAAmt);
          ReadE EAky pea01;
          If %Eof(wmeal6);
             *in60 = *On;
          EndIf;
          Iter;
       EndIf;
      *
      * GET A/R CATEGORY
      *
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *ON
     C     EAKY          READE     PEA01                                  60
     C                   ITER
     C                   ENDIF
      *
     C                   SELECT
     C     EAARCD        WHENEQ    F1DVBL
     C     EAARCD        OREQ      F1DVPD
     C     EAARCD        OREQ      F1DVXR
     C     TTCTYP        WHENEQ    'P'
     C     TTCTYP        OREQ      'C'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'A'
     C     EAARCD        ANDNE     F1RDCD
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
      *
     C     EANINV        IFNE      *ZERO
     C                   Z-ADD     EANINV        SVNINV
     C                   ENDIF
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XXAMT
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XPPRMC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'N'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'R'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XXCMAJ
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XPPRMC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'T'
     C     EAARTY        OREQ      'P'
      *
     C                   SELECT
     C     EAARCD        WHENEQ    F1RCCD
     C     EAFYR         ANDLE     110
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   SUB       EAAMT         XXCMAJ
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XPPRMP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    'T'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XXTAX
     C     EAPODT        WHENLT    XXPO01
     C                   ADD       EAAMT         XXTAXD
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XPTAXC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XXTAX
     C                   SUB       EAAMT         XXCMAJ
      *** End Chg ***** 05/06/25 ***************************
     C     EAPODT        WHENLT    XXPO01
     C                   SUB       EAAMT         XXTAXD
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XPTAXP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C*** Begin Add ***  06/24/24  *********************************************
      *   ACCUMULATE COLLATERAL DUE
     C     EAARCD        WHENEQ    EXCOLB
     C     EAARCD        OREQ      EXCOLO
      *
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
     C                   ADD       EAAMT         XXCOLD
      /free
       Endif;
      /end-free
      *
     C     EAARCD        WHENEQ    EXCOLP
     C     EAARCD        OREQ      EXCOLT
      *
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XXCOLD
     C                   SUB       EAAMT         XXCMAJ
      *** End Chg ***** 05/06/25 ***************************
      /free
       Endif;
      /end-free
     C*** End   Add ***  06/24/24  *********************************************
      *
     C     TTCTYP        WHENEQ    '1'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR1STB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
     C                   ADD       EAAMT         XX1ST
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP1STC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR1STB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XX1ST
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XX1ST
       Endif;
      *** End Chg ***** 05/06/25 ***************************
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP1STP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    '2'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR2NDB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
     C                   ADD       EAAMT         XX2ND
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP2NDC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR2NDB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XX2ND
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XX2ND
       Endif;
      *** End Chg ***** 05/06/25 ***************************
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP2NDP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    '3'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR3RDB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
     C                   ADD       EAAMT         XX3RD
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP3RDC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR3RDB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XX3RD
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XX3RD
       Endif;
      *** End Chg ***** 05/06/25 ***************************
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP3RDP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    '4'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR4THB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
     C                   ADD       EAAMT         XX4TH
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP4THC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR4THB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XX4TH
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XX4TH
       Endif;
      *** End Chg ***** 05/06/25 ***************************
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP4THP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    '5'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR5THB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
     C                   ADD       EAAMT         XX5TH
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP5THC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR5THB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XX5TH
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XX5TH
       Endif;
      *** End Chg ***** 05/06/25 ***************************
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP5THP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    '6'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR6THB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
     C                   ADD       EAAMT         XX6TH
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP6THC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR6THB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XX6TH
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XX6TH
       Endif;
      *** End Chg ***** 05/06/25 ***************************
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP6THP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    '7'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR7THB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
     C                   ADD       EAAMT         XX7TH
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP7THC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C     RR7THB        ORNE      'Y'
     C     isProcAll     ANDNE     'Y'
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XX7TH
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XX7TH
       Endif;
      *** End Chg ***** 05/06/25 ***************************
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XP7THP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    'R'
     C     QQEXDF        ANDNE     'Y'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
     C                   ADD       EAAMT         XXDEDT
      /free
       Endif;
      /end-free
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XCDEDC
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPDEDC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XXDEDT
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XXDEDT
       Endif;
      *** End Chg ***** 05/06/25 ***************************
      /free
       Endif;
      /end-free
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   SUB       EAAMT         XCDEDC
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPDEDP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    'M'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
     C                   ADD       EAAMT         XXMBFT
      /free
       Endif;
      /end-free
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XCMBFC
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPMBFC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XXMBFT
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XXMBFT
       Endif;
      *** End Chg ***** 05/06/25 ***************************
      /free
       Endif;
      /end-free
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   SUB       EAAMT         XCMBFC
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPMBFP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    'E'
     C     XXEXP         ANDEQ     'Y'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'B'
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
     C                   ADD       EAAMT         XXEXCN
      /free
       Endif;
      /end-free
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XCEXPC
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPEXPC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'P'
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XXEXCN
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XXEXCN
       Endif;
      *** End Chg ***** 05/06/25 ***************************
      /free
       Endif;
      /end-free
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   SUB       EAAMT         XCEXPC
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPEXPC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENEQ    'D'
     C     XXDEP         ANDEQ     'Y'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'H'
     C     EAARTY        OREQ      'R'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XDEPCU
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XDEPPV
     C                   ADD       EAAMT         XPDEPC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'I'
     C     EAARTY        OREQ      'T'
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   SUB       EAAMT         XDEPCU
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XDEPPV
     C                   ADD       EAAMT         XPDEPP
      /free
       Endif;
      /end-free
     C                   ENDSL

     C                   ENDSL

     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
     C                   ADD       EAAMT         XXFEED
      /free
       Endif;
      /end-free
     C                   SELECT
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPOTHC
      /free
       Endif;
      /end-free
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XCOTHC
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XXFEED
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XXFEED
       Endif;
      *** End Chg ***** 05/06/25 ***************************
      /free
       Endif;
      /end-free
     C                   SELECT
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPOTHP
      /free
       Endif;
      /end-free
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   SUB       EAAMT         XCOTHC
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     EAARCD        WHENEQ    IFIFBL
     C     EAARCD        OREQ      IFIFPD
     C     EAARCD        OREQ      IFIFTR
     C     EAARCD        OREQ      IFIFTC
      *
     C                   SELECT
     C     EAARCD        WHENEQ    IFIFBL
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XXIFAM
     C                   ADD       EAAMT         XCINFC
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
     C                   ADD       EAAMT         XPINFC
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C     EAARCD        WHENEQ    IFIFPD
     C     EAARCD        OREQ      IFIFTR
     C     EAARCD        OREQ      IFIFTC
      *
     C                   SELECT
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   SUB       EAAMT         XXIFAM
     C                   SUB       EAAMT         XCINFC
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   SUB       EAAMT         XXAMTD
     C                   ADD       EAAMT         XPINFP
      /free
       Endif;
      /end-free
     C                   ENDSL
      *
     C                   ENDSL
      *
     C     TTCTYP        WHENNE    'R'
     C     TTCTYP        ANDNE     'M'
     C     TTCTYP        ANDNE     '1'
     C     TTCTYP        ANDNE     '2'
     C     TTCTYP        ANDNE     '3'
     C     TTCTYP        ANDNE     '4'
     C     TTCTYP        ANDNE     '5'
     C     TTCTYP        ANDNE     '6'
     C     TTCTYP        ANDNE     '7'
     C     TTCTYP        ANDNE     'T'
     C     EAARCD        ANDNE     IFIFBL
     C     EAARCD        ANDNE     IFIFPD
     C     EAARCD        ANDNE     IFIFTR
     C     EAARCD        ANDNE     IFIFTC
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'F'
      *
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
     C                   ADD       EAAMT         XXFEED
      /free
       Endif;
      /end-free
     C                   SELECT
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPOTHC
      /free
       Endif;
      /end-free
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XCOTHC
     C                   ENDSL
      *
     C     EAARTY        WHENEQ    'Q'
      *
      /free
       If isProcAll <> 'Y' or (EApoDt >= xxPO01 and EApoDt <= xxPO31);
      /end-free
      *** Beg Chg ***** 05/06/25 ***************************
     C***                SUB       EAAMT         XXFEED
       If EAPODT >= XXPO01 and EAPODT <= XXPO31;
     C                   SUB       EAAMT         XXCMAJ
       Else;
     C                   SUB       EAAMT         XXFEED
       Endif;
      *** End Chg ***** 05/06/25 ***************************
      /free
       Endif;
      /end-free
     C                   SELECT
     C     EAPODT        WHENLT    XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XPOTHP
      /free
       Endif;
      /end-free
     C     EAPODT        WHENGE    XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   SUB       EAAMT         XCOTHC
     C                   ENDSL
      *
     C                   ENDSL
      *
     C                   ENDSL
      *
       //*** Begin Add *** 02/12/25 ****************************************
       holdInvoice# = EAninv;
       //*** End   Add *** 02/12/25 ****************************************
     C     EAKY          READE     PEA01                                  60
     C*** Begin Add *** 02/12/25 *******************************************
     C*
       // If last record read falls outside the date range then restore the
       // invoice# of the last invoice read.
       If EApodt > xxPO31;
          EAninv = holdInvoice#;
       EndIf;
     C*** End   Add *** 02/12/25 *******************************************
     C                   ENDDO                                                  *IN60 DOWEQ*OFF
     C*
     C* CHECK UNTRANSFERRED BATCH RECORDS FOR RETRO SETTLEMENTS AS WELL.
     C*
     C     *IN67         IFEQ      *ON
      *
     C     EAKEY3        SETLL     PEA03B
     C     EAKEY3        READE     PEA03B                                 60
     C*
     C     *IN60         DOWEQ     *OFF
     C     EAPODT        ANDLE     XXPO31
       // Skip A/R records if invoice passed in and A/R type set to skip
       If SkipICAR(xxCo#:invoice#:EAnInv:EAarCd:EAPodt:EAAmt);
          ReadE EAkey3 pea03b;
          If %Eof(wdeal2);
             *in60 = *On;
          EndIf;
          Iter;
       EndIf;
     C*
     C     EATFFL        IFNE      'Y'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'C'
     C     TTTYPE        ANDEQ     'B'
     C*
      *
      *   ACCUMULATE CURRENT BILLED PREMIUM
      *
     C     EAPODT        IFGE      XXPO01
     C     EAPODT        ANDLE     XXPO31
     C                   ADD       EAAMT         XXAMT
     C                   ENDIF
      *
      *   ACCUMULATE PREVIOUS UNPAID BALANCE
      *
     C     EAPODT        IFLT      XXPO01
      /free
       If isProcAll <> 'Y';
      /end-free
     C                   ADD       EAAMT         XXAMTD
      /free
       Endif;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  EATFFL IFNE
     C*
     C     EAKEY3        READE     PEA03B                                 60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  *IN67 IFEQ
      *
      * ADD PRIOR YEAR DEPOSIT BALANCE IF FLAGGED TO DO SO
      *
     C     XXDEP         IFEQ      'Y'
     C     XXDPYM        ANDEQ     'N'
     C                   MOVE      'H'           XXARTY
     C     EAKY4         CHAIN     PEA0L3                             60
     C     *IN60         DOWEQ     '0'
     C     EAARTY        ANDLE     'T'
       // Skip A/R records if invoice passed in and A/R type set to skip
       If SkipICAR(xxCo#:invoice#:EAnInv:EAarCd:EAPodt:EAAmt);
          ReadE EAky4 pea0l3;
          If %Eof(wmeal3);
             *in60 = *On;
          EndIf;
          Iter;
       EndIf;
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'D'
      *
     C     EAFYR         IFLT      EBFYR
     C     F1KEY2        SETLL     PF101
     C     F1KEY2        READE     PF101                                  60
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      'Y'           XXPYDM            1
     C                   ELSE
     C                   MOVE      'N'           XXPYDM            1
     C                   ENDIF
      *
     C     EAARTY        IFEQ      'H'
     C     XXPYDM        ANDEQ     'N'
     C     EAARTY        OREQ      'R'
     C     XXPYDM        ANDEQ     'N'
     C                   ADD       EAAMT         XDEPPV
     C     CLTID         IFEQ      '025'
     C                   ADD       EAAMT         XPDEPC
     C                   ENDIF
     C                   ENDIF
      *
     C     EAARTY        IFEQ      'I'
     C     XXPYDM        ANDEQ     'N'
     C     EAARTY        OREQ      'T'
     C     XXPYDM        ANDEQ     'N'
     C                   SUB       EAAMT         XDEPPV
     C     CLTID         IFEQ      '025'
     C                   ADD       EAAMT         XPDEPP
     C                   ENDIF
     C                   ENDIF
      *
     C                   ENDIF                                                  EAFYR IFLT EBFYR
     C                   ENDIF                                                  TTCTYP = 'D'
     C     EMKEY         READE     PEA0L3                                 60
     C                   ENDDO
      *
     C     F1KEY         SETLL     PF101
     C     F1KEY         READE     PF101                                  60
     C                   ENDIF                                                  XXDPYM='N'
      *
      *   CALCULATE THE UNPAID BALANCE FOR PRIOR YEAR
      *
     C     XXPAUD        IFEQ      'Y'
     C     AUDIT         ANDNE     *ON
     C     suppPrYear    andne     'Y'
      *
     C     EAKYP         SETLL     PEA01
     C     EAKYP         READE     PEA01                                  60
      *
      * If no prior year, check for employer's cross reference record and check
      * for prior year balance.
      *
     C     *IN60         IFEQ      *ON
      *
     C     EMKEY         CHAIN     ZEX01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   Z-ADD     XXFND         SVFND
     C                   Z-ADD     XXEMP#        SVEMP#
     C                   Z-ADD     XXDIV         SVDIV
     C                   Z-ADD     ZXOFND        XXFND
     C                   Z-ADD     ZXOEMP        XXEMP#
     C                   Z-ADD     ZXODIV        XXDIV
      *
     C     EAKYP         SETLL     PEA01
     C     EAKYP         READE     PEA01                                  60
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C                   ENDIF                                                  *IN60 = *ON
      *
     C     *IN60         DOWEQ     *OFF
     C     EAPODT        ANDLE     XXPODT
     C     CLTID         ANDNE     '025'
       // Skip A/R records if invoice passed in and A/R type set to skip
       If SkipICAR(xxCo#:invoice#:EAnInv:EAarCd:EAPodt:EAAmt);
          ReadE EAkyP pea01;
          If %Eof(wmeal6);
             *in60 = *On;
          EndIf;
          Iter;
       EndIf;
      *
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
      *
     C     EAARTY        IFEQ      'B'
     C     EAARCD        ANDNE     F1DVBL
     C     EAARTY        OREQ      'A'
     C     EAARCD        ANDNE     F1RDCD
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'R'
     C     TTCTYP        ANDNE     'D'
     C                   ADD       EAAMT         XXAMTP
     C                   ENDIF
      *
     C     EAARTY        IFEQ      'P'
     C     EAARCD        ANDNE     F1DVPD
     C     EAARCD        ANDNE     F1DVXR
     C     EAARTY        OREQ      'T'
     C     TTCTYP        ANDNE     'D'
     C                   SELECT
     C     EAARCD        WHENEQ    F1RCCD
     C     EAFYR         ANDLE     110
     C                   OTHER
     C                   SUB       EAAMT         XXAMTP
     C                   ENDSL
     C                   ENDIF
     C*
     C*  CALCULATE STATE TAX DUE
      *
     C     EAARTY        IFEQ      'Q'
     C     EAARTY        OREQ      'F'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
      *
     C*
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDEQ     'T'
     C                   SUB       EAAMT         XXTAXP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     'T'
     C                   ADD       EAAMT         XXTAXP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDEQ     '1'
     C*
     C                   SUB       EAAMT         XX1STP
     C                   ENDIF
     C*
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '1'
     C*
     C                   ADD       EAAMT         XX1STP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDEQ     '2'
     C*
     C                   SUB       EAAMT         XX2NDP
     C                   ENDIF
     C*
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '2'
     C*
     C                   ADD       EAAMT         XX2NDP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDEQ     '3'
     C*
     C                   SUB       EAAMT         XX3RDP
     C                   ENDIF
     C*
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '3'
     C*
     C                   ADD       EAAMT         XX3RDP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDEQ     '4'
     C*
     C                   SUB       EAAMT         XX4THP
     C                   ENDIF
     C*
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '4'
     C*
     C                   ADD       EAAMT         XX4THP
     C                   ENDIF
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDEQ     '5'
     C                   SUB       EAAMT         XX5THP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '5'
     C*
     C                   ADD       EAAMT         XX5THP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDEQ     '6'
     C                   SUB       EAAMT         XX6THP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '6'
     C                   ADD       EAAMT         XX6THP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDEQ     '7'
     C                   SUB       EAAMT         XX7THP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDEQ     '7'
     C                   ADD       EAAMT         XX7THP
     C                   ENDIF
      *
      *  If "Exclude deductible fees" = Y, then do not include
      *  deductible fees in prior year unpaid fee balance.
      *
     C     QQEXDF        IFNE      'Y'
     C     TTCTYP        ORNE      'R'
      *
     C*  Calculate prior yr fees due
     C     EAARTY        IFEQ      'Q'
     C     TTCTYP        ANDNE     'T'
     C     TTCTYP        ANDNE     '1'
     C     TTCTYP        ANDNE     '2'
     C     TTCTYP        ANDNE     '3'
     C     TTCTYP        ANDNE     '4'
     C     TTCTYP        ANDNE     '5'
     C     TTCTYP        ANDNE     '6'
     C     TTCTYP        ANDNE     '7'
     C                   SUB       EAAMT         XXFEEP
     C                   ENDIF
     C*
     C     EAARTY        IFEQ      'F'
     C     TTCTYP        ANDNE     'T'
     C     TTCTYP        ANDNE     '1'
     C     TTCTYP        ANDNE     '2'
     C     TTCTYP        ANDNE     '3'
     C     TTCTYP        ANDNE     '4'
     C     TTCTYP        ANDNE     '5'
     C     TTCTYP        ANDNE     '6'
     C     TTCTYP        ANDNE     '7'
     C                   ADD       EAAMT         XXFEEP
     C                   ENDIF
      *
     C                   ENDIF                                                  qqexdf <> y
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDIF                                                  EAARTY='Q' OR 'F'
      *
     C                   ENDIF                                                  *IN60 = *OFF
      *
     C     EAKYP         READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ*OFF
      *
      * Restore key values if employer's cross reference was used in prior
      * year calculations.
      *
     C     SVFND         IFNE      *ZERO
     C     SVEMP#        ANDNE     *ZERO
     C                   Z-ADD     SVFND         XXFND
     C                   Z-ADD     SVEMP#        XXEMP#
     C                   Z-ADD     SVDIV         XXDIV
     C                   ENDIF
      *
     C                   ENDIF                                                  XXPAUD IFEQ 'Y'
     C     QQEXDF        IFEQ      'Y'
     C                   Z-ADD     *ZERO         XXDEDT
     C                   ENDIF
     C*** Begin Add ***  06/24/24  *********************************************
     C     XXCOLD        IFNE      *ZERO
     C                   MOVE      *ON           *IN48
     C                   ENDIF
     C*** End   Add ***  06/24/24  *********************************************
      *
     C     XXFEED        IFNE      *ZERO
     C                   MOVE      *ON           *IN88
     C                   ENDIF
     C     XXIFAM        IFNE      *ZERO
     C                   MOVE      *ON           *IN84
     C                   ENDIF
      *
     C     XXDEDT        IFNE      *ZERO
     C                   MOVE      *ON           *IN82
     C                   ENDIF
      *
     C     XXEXCN        IFNE      *ZERO
     C                   MOVE      *ON           *IN83
     C                   ENDIF
      *
     C     XXAMTP        IFNE      *ZERO
     C                   MOVE      *ON           *IN86
     C                   ENDIF
      *
     C     XXTAXP        IFNE      *ZERO
     C                   MOVE      *ON           *IN89
     C                   ENDIF
     C*
     C     XXTAX         IFNE      *ZERO
     C                   MOVE      *ON           *IN90
     C                   ENDIF
     C     XX1STP        IFNE      *ZERO
     C                   MOVE      *ON           *IN32
     C                   ENDIF
     C*
     C     XX1ST         IFNE      *ZERO
     C                   MOVE      *ON           *IN33
     C                   ENDIF
     C*
     C     XX2NDP        IFNE      *ZERO
     C                   MOVE      *ON           *IN34
     C                   ENDIF
     C*
     C     XX2ND         IFNE      *ZERO
     C                   MOVE      *ON           *IN35
     C                   ENDIF
     C*
     C     XX3RDP        IFNE      *ZERO
     C                   MOVE      *ON           *IN36
     C                   ENDIF
     C*
     C     XX3RD         IFNE      *ZERO
     C                   MOVE      *ON           *IN37
     C                   ENDIF
     C*
     C     XX4THP        IFNE      *ZERO
     C                   MOVE      *ON           *IN38
     C                   ENDIF
     C*
     C     XX4TH         IFNE      *ZERO
     C                   MOVE      *ON           *IN39
     C                   ENDIF
     C*
     C     XX5THP        IFNE      *ZERO
     C                   MOVE      *ON           *IN41
     C                   ENDIF
     C*
     C     XX5TH         IFNE      *ZERO
     C                   MOVE      *ON           *IN42
     C                   ENDIF
     C*
     C     XX6THP        IFNE      *ZERO
     C                   MOVE      *ON           *IN43
     C                   ENDIF
     C*
     C     XX6TH         IFNE      *ZERO
     C                   MOVE      *ON           *IN44
     C                   ENDIF
     C*
     C     XX7THP        IFNE      *ZERO
     C                   MOVE      *ON           *IN45
     C                   ENDIF
     C*
     C     XX7TH         IFNE      *ZERO
     C                   MOVE      *ON           *IN46
     C                   ENDIF
     C*
     C     XXFEEP        IFNE      *ZERO
     C                   MOVE      *ON           *IN91
     C                   ENDIF
      *
     C     XXCMAJ        IFNE      *ZERO
     C                   MOVE      *ON           *IN92
     C                   ENDIF
     C     XDEPPV        IFNE      *ZERO
     C                   MOVE      *ON           *IN30
     C                   ENDIF
     C     XDEPCU        IFNE      *ZERO
     C                   MOVE      *ON           *IN31
     C                   ENDIF
     C*
     C*  The sum of ALL previous pol yr balances must be greater
     C*  than zero for ANY of them to appear on the notice
     C     XXAMTP        ADD       XXTAXP        XXALLP           11 2
     C                   ADD       XXFEEP        XXALLP
     C*
     C                   ADD       XX1STP        XXALLP
     C                   ADD       XX2NDP        XXALLP
     C                   ADD       XX3RDP        XXALLP
     C                   ADD       XX4THP        XXALLP
     C                   ADD       XX5THP        XXALLP
     C                   ADD       XX6THP        XXALLP
     C                   ADD       XX7THP        XXALLP
     C     XXALLP        IFEQ      *ZERO
     C                   SETOFF                                       868991
     C                   SETOFF                                       323436
     C                   SETOFF                                       384143
     C                   SETOFF                                       45
     C                   ENDIF
      *
     C     XXMBFT        IFNE      *ZERO
     C                   MOVE      *ON           *IN87
     C                   ENDIF
     C* Reduce payments by s/r billings up to amount of total paid when
     c* running current balance on S/R account and not audited for IC.
     C                   If        BSSefl = 'S' and not isAudit and invoice# = 0
     C                             and xxWIARPgm = *on
     C
     C* S/R Premium
     C                   If        srBilledPrem > premPaid
     C                   Add       premPaid      xxAmtD
     C                   Else
     C                   Add       srBilledPrem  xxAmtD
     C                   Endif
     C
     C* S/R Tax
     C                   If        srBilledTax > taxPaid
     C                   Add       taxPaid       xxAmtD
     C                   Else
     C                   Add       srBilledTax   xxAmtD
     C                   Endif
     C
     C* S/R Installment Fees
     C                   If        srBilledFee > instPaid
     C                   Add       instPaid      xxAmtD
     C                   Else
     C                   Add       srBilledFee   xxAmtD
     C                   Endif
     C
     C                   Endif
      *
     C     XXAMT         ADD       XXAMTD        XXTOTL           10 2
     C   92              ADD       XXCMAJ        XXTOTL
     C                   ADD       XXFEED        XXTOTL
      *
     C   82              ADD       XXDEDT        XXTOTL
     C   83              ADD       XXEXCN        XXTOTL
     C   84              ADD       XXIFAM        XXTOTL
     C   86              ADD       XXAMTP        XXTOTL
     C   87              ADD       XXMBFT        XXTOTL
     C   90              ADD       XXTAX         XXTOTL
     C   89              ADD       XXTAXP        XXTOTL
     C   91              ADD       XXFEEP        XXTOTL
     C   30              ADD       XDEPPV        XXTOTL
     C   31              ADD       XDEPCU        XXTOTL
     C   33              ADD       XX1ST         XXTOTL
     C   32              ADD       XX1STP        XXTOTL
     C   35              ADD       XX2ND         XXTOTL
     C   34              ADD       XX2NDP        XXTOTL
     C   37              ADD       XX3RD         XXTOTL
     C   36              ADD       XX3RDP        XXTOTL
     C   39              ADD       XX4TH         XXTOTL
     C   38              ADD       XX4THP        XXTOTL
     C   42              ADD       XX5TH         XXTOTL
     C   41              ADD       XX5THP        XXTOTL
     C   44              ADD       XX6TH         XXTOTL
     C   43              ADD       XX6THP        XXTOTL
     C   46              ADD       XX7TH         XXTOTL
     C   45              ADD       XX7THP        XXTOTL
     C*** Begin Add ***  06/24/24  *********************************************
     C   48              ADD       XXCOLD        XXTOTL
     C*** End   Add ***  06/24/24  *********************************************
     **
     ** LOAD THIRD ADDRESS IF NOT BLANK
     **
     C     A3KY          CHAIN     PA301                              60
     **
     C     *IN60         IFEQ      *ZERO
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     **
     C                   MOVEL     A3ST          XXSTAT            2
     C                   MOVEL     A3ZIP1        XXZIP1            5
     C                   MOVE      A3ZIP2        XXZIP2            4
     C                   MOVEL     A3CTY         XXCITY           25
     C                   Movel     A3Cty         XXeCity
     C                   Movel     A3St          XXeSt
     C                   Movel     A3Zip1        XXeZip
     C                   Move      A3Zip2        XXeZip
     C                   EXSR      SR1ST
     C                   MOVEL(P)  XXCSTZ        XXECSZ           40
     C                   MOVEL(P)  A3ADR1        XXADR1           40
     C                   MOVEL(P)  A3ADR2        XXADR2           40
     **
      *
      *  PRINT 'OUT OF COUNTRY ADDRESS' IF ENTERED
      *
     C     A3OOC         IFNE      *BLANKS
     C                   MOVEL(P)  A3OOC         XXECSZ           40
     C                   ENDIF                                                  A3OOC
      *
     **
     C                   ELSE
      *
     C     EMMPFL        IFEQ      'M'
      *
      *   LOAD CITY, STATE, & ZIP FOR THE MAILING ADDRESS
      *
     C                   MOVEL     EMMST         XXSTAT            2
     C                   MOVEL     EMMZIP        XXZIP1            5
     C                   MOVE      EMMZIP        XXZIP2            4
     C                   MOVEL     EMMCTY        XXCITY           25
     C                   Movel     EMMCty        XXeCity
     C                   Movel     EMMSt         XXeSt
     C                   Z-add     EMMZip        XXeZip
     C                   EXSR      SR1ST
     C                   MOVEL(P)  XXCSTZ        XXECSZ           40
     C                   MOVEL(P)  EMMAD1        XXADR1           40
     C                   MOVEL(P)  EMMAD2        XXADR2           40
      *
     C                   ELSE
      *
      *   LOAD CITY, STATE, & ZIP FOR THE PHYSICAL ADDRESS
      *
     C                   MOVEL     EMPST         XXSTAT            2
     C                   MOVEL     EMPZIP        XXZIP1            5
     C                   MOVE      EMPZIP        XXZIP2            4
     C                   MOVEL     EMPCTY        XXCITY           25
     C                   Movel     EMPCty        XXeCity
     C                   Movel     EMPSt         XXeSt
     C                   Z-add     EMPZip        XXeZip
     C                   EXSR      SR1ST
     C                   MOVEL(P)  XXCSTZ        XXECSZ
     C                   MOVEL(P)  EMPAD1        XXADR1
     C                   MOVEL(P)  EMPAD2        XXADR2
     C                   ENDIF                                                  EMMPFL IFEQ
     **
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     XXADR2        IFEQ      *BLANK
     C                   MOVE      *ON           *IN81
     C                   ELSE
     C                   MOVE      *OFF          *IN81
     C                   ENDIF
     C*
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   MOVE      XXEMP#        ##EMP#            9
     C                   MOVE      XXDIV         ##DIV             5
     C                   MOVE      'W'           ##ODR             1
     C     CLTID         IFEQ      '025'
     C                   MOVE      *BLANK        ##ODR
     C                   ENDIF
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
     C                   MOVE      *ZEROS        ##CONL
     C                   CALL      'SRADDR'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##ODR
     C                   PARM                    ##NODR
     C                   PARM                    ##AODR
     C                   PARM                    ##EMN1
     C                   PARM                    ##EMN2
     C                   PARM                    ##EMN3
     C                   PARM                    ##EMN4
     C                   PARM                    ##EMN5
     C                   PARM                    ##EMN6
     C                   PARM                    ##EMN7
     C                   PARM                    ##ADRL            3 0
     C                   PARM                    ##NAML            3 0
     C                   PARM                    ##FEDL            3 0
     C                   PARM                    ##PEO1           57
     C                   PARM                    ##PEO2           57
     C                   PARM                    ##PGFL            1
     C                   PARM                    ##CONL            3 0
     C*
     C                   MOVE      ##EMN1        XXEMN1           50
     C                   MOVE      ##EMN2        XXEMN2           50
     C                   MOVE      ##EMN3        XXEMN3           50
     C                   MOVE      ##EMN4        XXEMN4           50
     C                   MOVEL     ##EMN5        XXEMN5           50
     C                   MOVEL     ##EMN6        XXEMN6
     C                   MOVEL     ##EMN7        XXEMN7
      *
      *   GET AGENT# VIA AGENT HISTORY AND AGENT MASTER FILES
      *                                                    |
     C     AHKEY         SETGT     PAH01                                        |
     C                   READP     PAH01                                  50    |
     C     *IN50         DOWEQ     *OFF                                         |
     C     AHSTAT        ANDEQ     'I'                                          |
     C                   READP     PAH01                                  50    |
     C                   ENDDO                                                  |
      *                                                    |
     C     *IN50         IFEQ      *ON                                          |
     C     XXCO#         ORNE      AHCO#                                        |
     C     XXFND         ORNE      AHFND                                        |
     C     ELEMP#        ORNE      AHEMP#                                       |
     C     ELDIV         ORNE      AHDIV                                        |
     C     EBFYR         ORNE      AHFYR                                        |
     C                   Z-ADD     *ZERO         AHAGT#                         |
     C                   ENDIF                                                  |
      *
      *  GET THE AGENT NAME
      *
     C                   Z-ADD     0             XXAGN1
     C                   Z-ADD     0             XXAGN2
     C                   Z-ADD     0             XAAREA
     C                   Z-ADD     AHAGT#        XXAGT#
     C     AAKY          CHAIN     PAA01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     AAFEIN        XXFEIN
     C                   Z-ADD     AAMOD         XXMOD
     C     AGKY          CHAIN     PAG01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        AGNAME
     C                   MOVE      *BLANK        AGMAD1
     C                   MOVE      *BLANK        XXACSZ           40
     C                   Clear                   XXaCity
     C                   Clear                   XXaSt
     C                   Clear                   XXaZip
     C                   ELSE
     C                   MOVEL     AGMST         XXSTAT
     C                   MOVEL     AGMZIP        XXZIP1
     C                   MOVE      AGMZIP        XXZIP2
     C                   MOVEL     AGMCTY        XXCITY
     C                   Movel     AGMCty        XXaCity
     C                   Movel     AGMSt         XXaSt
     C                   Z-add     AGMZip        XXaZip
     C*
     C     AGPHN1        IFEQ      *ZERO
     C                   Z-ADD     XXAG21        XXAGN1            3 0
     C                   Z-ADD     XXAG22        XXAGN2            4 0
     C                   Z-ADD     AGARE2        XAAREA            3 0
     C                   ELSE
     C                   Z-ADD     XXAG11        XXAGN1            3 0
     C                   Z-ADD     XXAG12        XXAGN2            4 0
     C                   Z-ADD     AGARE1        XAAREA
     C                   ENDIF
     C                   EXSR      SR1ST
     C                   MOVEL(P)  XXCSTZ        XXACSZ
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *BLANK        AGNAME
     C                   MOVE      *BLANK        AGMAD1
     C                   MOVE      *BLANK        XXACSZ
     C                   Clear                   XXaCity
     C                   Clear                   XXaSt
     C                   Clear                   XXaZip
     C                   ENDIF
     C*
     C*
     C                   MOVEL(P)  AHAGT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XPAGT#            5
     C*
     C     EXAHDR        CAT(P)    ':':0         XPAHDR
     C                   CAT       XPAGT#:1      XPAHDR
     C                   CAT       AGNAME:1      XPAHDR
     C*
      *
      *  PRINT THE INVOICE
      *
     C     XXTOTL        IFGE      XXMINB                                       BILLABLE > 0
     C     *IN24         ANDEQ     *OFF
     C     *IN24         OREQ      *ON                                          SINGLE EMPLR
     C     XXTOTL        ANDGE     XXMINB                                       BILLABLE > 0
     C     *IN24         OREQ      *ON                                          SINGLE EMPLR
     C     CLTID         ANDEQ     '114'
     C     *IN67         OREQ      *ON                                          SETTLEMENT
     C     XXMPNT        OREQ      *ON                                          MAND PRM NOTIC
     C     isProcAll     ANDNE     'Y'
     C*
      /Free
        //*** Beg Chg ***** 05/06/25 *************************
        //***If CLTID = '129' and XXTOTL <= *ZERO;
        If massFlag = 'Y' and XXTOTL <= *ZERO;
        //*** End Chg ***** 05/06/25 *************************
           @PRCINV = *OFF;
        Else;
           @PRCINV = *ON;
        Endif;

        If @PRCINV;
      /End-free
     C     *IN67         IFEQ      *ON
     C     XXDUDT        ANDEQ     *ZERO
     C     XXMPNT        OREQ      *ON                                          MAND PRM NOTIC
     C     XXDUDT        ANDEQ     *ZERO
     C                   Z-ADD     XXPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDUDT
     C                   ENDIF
     C                   MOVE      XXDUDT        X1DUDT            8 0
     C                   MOVE      XXDUDT        X2DUDT            8 0
     C                   MOVE      XXDUDT        X4DUDT            8 0
     C                   MOVE      XXTOTL        X1TOTL           10 2
     C                   MOVE      XXTOTL        X2TOTL           10 2
     C                   MOVE      XXECSZ        X1ECSZ           40
     C                   MOVE      XXECSZ        X2ECSZ           40
     C                   MOVE      XXECSZ        X3ECSZ           40
     C                   MOVE      XXCKNM        @XCKNM           40
     C                   MOVE      EMNAME        @MNAME           40
     C                   MOVE      EMNAME        @MNAM2           40
     C                   MOVE      XXPLCY        @XPLCY           15
     C                   MOVE      XXADR1        @XADR1           40
     C                   MOVE      XXADR2        @XADR2           40
     C                   MOVE      RPDATE        @PDATE            8 0
     C                   MOVE      EMDIV         @MDIV             5 0
     C                   MOVE      XXFYR         @XFYR             3 0
     C                   MOVE      RPDATE        @PDATE            8 0
     C                   MOVE      XOACSZ        @OACSZ           40
     C                   EXSR      LDINFO
     C                   MOVEL     XXDUTX        X3DUDT           28
      *
     C     CLTID         IFEQ      '071'
     C                   SETON                                        78
     C                   ELSE
     C                   SETOFF                                       78
     C                   ENDIF
     C*
     C*  DO NOT PRINT ACCOUNT CURRENT POLICIES
     C*
     C     EBACUR        IFNE      'Y'

     C*

      /free
       isUseOCR = cltID = '071' or cltID = '129' or isLockbox;
       If isUseOCR;
         If cltID <> '129';
           If isAudited;
              ##type = '2';
           Else;
              ##type = '1';
           Endif;
         Endif;
         #2fnd  = xxfnd;
         #2fyr  = xxfyr;
         #2fyrp = xxfyrp;
         #2emp# = xxemp#;
         #2div  = xxdiv;
         xxamt2 = xxamtp;
         xxamt1 = xxcmaj + xxamt + xxamtd + xxexcn;
            If xxamt1 < 0;
               xxamt1 = 0;
            Endif;
            If xxamt2 < 0;
               xxamt2 = 0;
            Endif;
            xxOCR = AddCheckDigit((%trim(##type) + %trim(dsOCR)));
            If audit = *on;
               xxamt += xxamtd;
            Endif;
       Endif;
      /end-free

     C     CLTID         IFEQ      '114'
     C                   CLEAR                   XXSCAN
     C                   MOVE      XXFND         DSFND
     C                   MOVE      XXEMP#        DSEMP#
     C                   MOVE      XXDIV         DSDIV
     C                   MOVE      XXFYR         DSFYR
     C                   MOVE      XXDUDT        DSDUDT
     C                   MOVEL     '0'           DSTOTL
     C                   MOVE      XXTOTL        DSTOTL
     C                   MOVE      ' '           DSNEGT
     C     XXTOTL        IFLT      0
     C     XXTOTL        MULT      -1            @@TOTL           10 2
     C                   MOVE      @@TOTL        DSTOTL
     C                   MOVE      '-'           DSNEGT
     C                   ENDIF
     C                   MOVEL     XXSCAN        XXOC39           39
     C                   MOVEA     XXOC39        OC2
     C                   Z-ADD     *ZEROS        XXSUM
     C     1             DO        39            A                 2 0
     C                   MOVE      OC2(A)        XXLN              1 0
     C                   MOVE      WT2(A)        XXRN              1 0
     C     XXLN          MULT      XXRN          XX2N              2 0
     C                   MOVEL     XX2N          XXLN
     C                   MOVE      XX2N          XXRN
     C     XXLN          ADD       XXRN          XXTTL             2 0
     C                   ADD       XXTTL         XXSUM             5 0
     C                   ENDDO
     C     XXSUM         DIV       10            XXWRK             2 0
     C                   MVR                     XXREM             1 0
     C     10            SUB       XXREM         DSCKDG
     C                   ENDIF
     C*
     C     XXPFB         IFEQ      'B'
     C     XXPFB         OREQ      'P'
     C     XXDRES        OREQ      'Y'
     C*
     C* OPEN PRINTER FILE FOR NOTICE.
     C*
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C     XXOPN         IFEQ      *OFF
     C                   OPEN      W7321O
     C                   MOVE      *ON           XXOPN
     C                   ENDIF
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C*
     C* PRINT NEW FORMAT PREMIUM NOTICE.
     C*
     C     EXBTYP        IFEQ      'Y'
     C     CLTID         OREQ      '025'
     C*
     C* POLICY INFO SECTION
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XQDT
     C                   Z-ADD     XQDT          XQPR25
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MN(X)         XQPRDT
     C                   CAT       XXDAY:1       XQPRDT
     C                   CAT       ',':0         XQPRDT
     C                   CAT       XXYR:1        XQPRDT
     C*
     C                   MOVEL(P)  'Division'    XQDIVH           20
     C                   CAT       ':':0         XQDIVH
     C*
     C                   MOVEL(P)  'Rating S'    XQRSTH           20
     C                   CAT       'tate:':0     XQRSTH
     C*
     C                   MOVEL(P)  'Policy'      XQPROF           20
     C                   CAT       'For:':1      XQPROF
     C*
     C                   MOVEL(P)  'Print'       XQPROD           20
     C                   CAT       'Date:':1     XQPROD
     C*
     C     CLTID         IFEQ      '114'
     C                   MOVEL(P)  'Due'         XQDUOD           20
     C                   CAT       'Date:':1     XQDUOD
     C                   ENDIF
     C     XXDUDT        IFEQ      *ZERO
     C                   MOVEL     XCDURP        XQDUOD
     C                   ENDIF                                                  IFEQ *ZERO
     C*
     C                   MOVEL(P)  'For Peri'    XQPERD           20
     C                   CAT       'od:':0       XQPERD
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C                   Z-ADD     XQDT          XQFRDT            8 0
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MN(X)         XQPFDT           20
     C                   CAT       XXDAY:1       XQPFDT
     C                   CAT       ',':0         XQPFDT
     C                   CAT       XXYR:1        XQPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C                   Z-ADD     XQDT          XQTODT            8 0
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MN(X)         XQPTDT           20
     C                   CAT       XXDAY:1       XQPTDT
     C                   CAT       ',':0         XQPTDT
     C                   CAT       XXYR:1        XQPTDT
     C*
     C                   MOVEL(P)  XQPFDT        XQPOLP           45
     C                   CAT       ' to':1       XQPOLP
     C                   CAT       XQPTDT:1      XQPOLP
     C*
     C                   MOVEL(P)  CARPOL        XQCAPH
     C                   MOVEL(P)  RPCAP#        XQCAP#
     C*
     C                   MOVE      @@PLCY        @1PLCY
     C*
     C* AGENT INFO SECTION
     C*
     C                   MOVE      *BLANKS       XXAGHD
     C                   MOVE      XXAGT#        X1AGT#            5
     C     EXAHDR        CAT       ':':0         XXAGHD
     C                   CAT       X1AGT#:1      XXAGHD
     C*
     C                   MOVE      *BLANKS       XXAGPH
     C*
     C     XXAGN1        IFNE      *ZERO
     C                   MOVE      XAAREA        X1AREA            3
     C                   MOVE      XXAGN1        X1AGN1            3
     C                   MOVE      XXAGN2        X1AGN2            4
     C*
     C                   MOVEL     '('           XXAGPH
     C                   CAT       X1AREA:0      XXAGPH
     C                   CAT       ')':0         XXAGPH
     C                   CAT       X1AGN1:1      XXAGPH
     C                   CAT       '-':1         XXAGPH
     C                   CAT       X1AGN2:1      XXAGPH
     C                   ENDIF                                                  XXAGN1 IFNE
     C*
     C* INFO BOX SECTION
     C*
     C                   MOVE      *BLANKS       XXCHKH
     C                   MOVE      *BLANKS       XXREMH
     C                   MOVE      *BLANKS       XXSERH
     C                   MOVE      *BLANKS       XXDIRH
     C                   MOVE      *BLANKS       XXOFAD          120
     C                   MOVE      *BLANKS       XXSERN
     C                   MOVE      *BLANKS       XXDIRN
     C*
     C                   MOVEL     'Please'      XXCHKH
     C                   CAT       'make':1      XXCHKH
     C                   CAT       'checks':1    XXCHKH
     C                   CAT       'payabl':1    XXCHKH
     C                   CAT       'e to:':0     XXCHKH
     C*
     C                   MOVEL     'Remit'       XXREMH
     C                   CAT       'to:':1       XXREMH
     C*
     C                   MOVEL     'Serviced'    XXSERH
     C                   CAT       'by:':1       XXSERH
     C     CLTID         IFEQ      '139'
     C                   CLEAR                   XXSERH
     C                   ENDIF
     C*
      /Free
          XXDIRH = 'Direct Inquiries to: ';
      /End-Free
     C*
     C                   MOVEL     OFAAD1        XXOFAD
     C                   CAT       OFAAD2:1      XXOFAD
     C                   CAT       ',':0         XXOFAD
     C                   CAT       XOACSZ:1      XXOFAD
     C     CLTID         IFNE      '114'
     C                   MOVEL(P)  'Insured:'    XQNAMH           20
     C                   MOVEL(P)  EMNAME        XQNAME
     C                   ENDIF
     C                   MOVE(P)   XCPERF        XXPERF
     C                   MOVEL(P)  XCDTXT        XXDTXT
     C*
     C  N99              MOVEL     XXCONM        XXSERN
     C   99              MOVEL     FNANAM        XXSERN
     C*
     C                   MOVEL     XXUNM2        XXDIRN
     C*
     C     XXPHN1        IFNE      *ZERO
     C                   MOVE      XXAREA        X1AREA
     C                   MOVE      XXPHN1        X1AGN1
     C                   MOVE      XXPHN2        X1AGN2
     C*
     C                   CAT       '(':1         XXDIRN
     C                   CAT       X1AREA:0      XXDIRN
     C                   CAT       ')':0         XXDIRN
     C                   CAT       X1AGN1:1      XXDIRN
     C                   CAT       '-':1         XXDIRN
     C                   CAT       X1AGN2:1      XXDIRN
     C                   ENDIF                                                  XXPHN1 IFNE
     C*
     C                   CLEAR                   @1NOTE
     C                   CLEAR                   X1RESN
     C                   MOVEL     @XNOTE        @1NOTE
     C                   MOVE      XXCXDT        XQDT
     C                   MOVE      XXMTH         XXM               2
     C                   CAT       XXM:1         @1NOTE
     C                   CAT       '/':0         @1NOTE
     C                   CAT       XXDAY:0       @1NOTE
     C                   CAT       '/':0         @1NOTE
     C                   CAT       XXYR:0        @1NOTE
     C                   CAT       '.':0         @1NOTE
     C*
     C                   MOVEL     'Reason:'     X1RESN
     C                   CAT       XXRESN:2      X1RESN
     C*** Begin Add *** 02/12/25 *******************************************
      /free
       //
       // Let's restore the invoice#
       If holdInvoice# <> 0;
          EAninv = holdInvoice#;
       EndIf;
      /end-free
     C*** End   Add *** 02/12/25 *******************************************
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '046'
     C     F2BLOG        ANDEQ     'TIMBER'
     C                   Z-ADD     0.5           LOGHRZ
     C                   Z-ADD     6.3           LOGVRT
     C     CLTID         WHENEQ    '025'
     C     F2BLOG        ANDEQ     'LOGO025'
     C                   Z-ADD     0.650         LOGHRZ
     C                   Z-ADD     .55           LOGVRT
     C                   OTHER
     C                   Z-ADD     0.6           LOGHRZ
     C                   Z-ADD     5.2           LOGVRT
     C                   ENDSL
     C*
     C     XXCLT#        IFEQ      '025'
     C                   Z-ADD     5.36          STMHV
     C     'State'       CAT(P)    'ment #':0    XQSTMH
     C                   MOVEL     SVNINV        XQSTMD
     C                   ENDIF
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '025'
      *
      * PRINTER FIRST HEADER
      *
     C                   MOVEL     HDR25         XXHD1T
     C                   MOVEL(P)  'FUND'        XXHD2T
     C     XXHD2T        CAT       'YEAR':1      XXHD2T
     C     XXHD2T        CAT       BEGYR:1       XXHD2T
     C     XXHD2T        CAT       '-':0         XXHD2T
     C     XXHD2T        CAT       EXPYR:0       XXHD2T
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   WRITE     QHEAD
     C                   WRITE     QHEADT
     C                   WRITE     QHEAD1T
      *
      * PRINT LINES
      *
     C                   WRITE     QLINE1T
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
      *
      * PRINTER SECOND HEADER
      *
     C                   CLEAR                   XQSEMP
     C                   CLEAR                   CNDESC
     C                   SELECT
     C     EMPST         WHENNE    *BLANKS
     C     EMPST         CAT(P)    EMCNTY:0      XXKEY
     C                   OTHER
     C     EMMST         CAT(P)    EMCNTY:0      XXKEY
     C                   ENDSL
     C     TBKEY         CHAIN     PCN01                              60
     C     CNDESC        IFNE      *BLANKS
     C     CNDESC        CAT(P)    '-':0         XQSEMP
     C                   ENDIF
     C     XQSEMP        CAT(P)    XXCNDX:0      XQSEMP
     C                   MOVEL(P)  DST25         XQDIST
     C                   MOVEL(P)  PRT25         XQPART
     C                   CALL      'WA005R'
     C                   PARM      EMEMP#        @@EMP#            9 0
     C                   PARM      EMDIV         @@DIV             5 0
     C                   PARM                    @@NAME           40
     C                   PARM                    @@EDN            80
      *
     C                   MOVEL(P)  @@EDN         XQEDN            20
     C                   MOVEL(P)  PRD25         XQPRDH                         EMPLOYER POLICY PRD
     C                   MOVEL(P)  ATT25         XQATTH                         ATTN:
     C                   MOVEL(P)  PDT25         XQPRD3                         PRINT DATE
     C                   MOVEL(P)  STM25         XQSTMH                         STATEMENT#
     C                   MOVEL(P)  DDT25         XQDUEH                         DUE DATE
     C     XXDUDT        IFEQ      *ZERO
     C                   MOVEL     XCDURP        XQDUEH
     C                   ENDIF                                                  IFEQ *ZERO
     C                   MOVEL(P)  AMT25         XQAMTH                         AMOUNT DUE
     C                   CLEAR                   XXDATA
     C                   Z-ADD     SVNINV        XQNINV
     C                   MOVEL     SVNINV        XXDATA
     C     '0'           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      XQINVA                         FIELD
     C                   ENDIF
     C                   CLEAR                   XQCONT
     C     A3KY          CHAIN     PA301                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     A3CONT        XQCONT
     C                   ENDIF
     C                   MOVE      '-'           XQDASH
      *
      * IF SRADDR PASSES BACK A CONTACT NAME (##CONL <> 0) REMOVE IT
      * FROM THE ADDRESS ARRY (EMN) AND PUSH THE SUBSEQUENT ENTRIES UP
      * ONE NOTCH.
      *
     C     ##CONL        IFNE      0
     C                   Z-ADD     ##CONL        YY                3 0
     C                   MOVE      *BLANKS       EMN(YY)
     C     YY            ADD       1             ZZ                3 0
     C     YY            DO        7
     C                   CLEAR                   EMN(YY)
     C                   MOVE      EMN(ZZ)       EMN(YY)
     C                   CLEAR                   EMN(ZZ)
     C                   ADD       1             YY
     C                   ADD       1             ZZ
     C     ZZ            IFEQ      8
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C                   ENDIF
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   WRITE     QHEAD2T
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
      *
      * PRINT THIRD HEADER  (CHARGES, PAYMENTS, BALANCE)
      *
     C                   MOVEL     CHG25         XQCHDG
     C                   MOVEL     PAY25         XQPHDG
     C                   MOVEL     BAL25         XQBHDG
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   WRITE     QHEAD3T
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
      *
      * PRINT DETAIL  (PRIOR, CURRENT CONTRIBTION, TOTALS)
      *
     C                   MOVEL     PRI25         XQPRTV
     C                   Z-ADD     XXPO01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQPDTO
     C                   MOVEL     CNT25         XQCONV
     C                   Z-ADD     XXPO01        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQCFDT
      *
      *  PULL INSTALLMENT DATES FROM SRINST
      *
     C                   MOVE      ELCO#         Q1CO#
     C                   MOVE      ELFND         Q1FND
     C                   MOVE(P)   ELFYR         Q1FYR
     C                   MOVEL(P)  ELBSCD        Q1BSCD
     C                   MOVE      ELBSDT        Q1BSDT
     C                   MOVE      ELPFDT        Q1PFDT
     C                   MOVE      ELPTDT        Q1PTDT
     C                   MOVE      ELPFDT        Q#DATE
     C                   MOVE      10000000      Q1PREM                         ANY AMOUNT WORKS
     C                   MOVE      *BLANKS       Q1TYPE
     C                   MOVE      *ZEROS        Q1DA1
     C                   MOVE      *ZEROS        Q1DA2
     C                   MOVE      *ZEROS        Q1DA3
     C                   MOVE      *ZEROS        Q1AM1
     C                   MOVE      *ZEROS        Q1AM2
     C                   MOVE      *ZEROS        Q1AM3
     C                   CALL      'SRINST'
     C                   PARM                    Q1CO#             3
     C                   PARM                    Q1FND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1BSCD            3
     C                   PARM                    Q1BSDT            7
     C                   PARM                    Q1PFDT            7
     C                   PARM                    Q1PTDT            7
     C                   PARM                    Q#DATE            7
     C                   PARM                    Q1PREM           11
     C                   PARM                    Q1TYPE            1
     C                   PARM                    Q1DA1           126
     C                   PARM                    Q1DA2           126
     C                   PARM                    Q1DA3           126
     C                   PARM                    Q1AM1           198
     C                   PARM                    Q1AM2           198
     C                   PARM                    Q1AM3           198
      *
      * LOAD INSTALLMENT DATES TO THE IS ARRAY.  THE INSTALLMENT DATES
      * CORERSPOND TO NON-ZERO AMOUNTS IN THE AM ARRAY
      *
     C                   MOVEA     Q1DA1         ID
     C                   MOVEA     Q1AM1         AM
     C                   CLEAR                   IS
     C                   Z-ADD     1             CT2               2 0
     C     1             DO        14            CT1               2 0
     C     AM(CT1)       IFNE      *ZERO
     C                   MOVE      ID(CT1)       IS(CT2)
     C                   ADD       1             CT2
     C                   ENDIF
     C                   ENDDO
      *
      * LOAD HI-VALUE TO IS INDEXES THAT EQUAL ZERO IN PREP FOR LOOKUP OPERATION
      *
     C     1             DO        14            CT1               2 0
     C     IS(CT1)       IFEQ      *ZERO
     C                   MOVE      *HIVAL        IS(CT1)
     C                   ENDIF
     C                   ENDDO
      *
      * GET THE NEXT INSTALLMENT DATE AFTER XXPO01 (THE FIRST DATE OF MONTH)
      *
     C                   Z-ADD     1             CT1
     C     XXPO01        LOOKUP    IS(CT1)                            60
     C                   Z-ADD     IS(CT1)       XXNXTD            7 0          NEXT INSTALLMENT DAT
      *
      * IF XXNXTD IS HIGH VALUE, PUT THE POLICY EXPIRATION DATE IN IF FIRST
      * DAY OF THE MONTH IS LESS THAN THE POLICY EXPIRATION DATE; ELSE,
      * PUT THE LAST DAY OF THE MONTH. IF XXNXTD IS NOT HIGH VALUE, SUBTRACT
      * ONE DAY AND POPULATE TO DATE WITH IT
      *
     C     XXNXTD        IFEQ      *HIVAL
     C                   SELECT
     C     XXPO01        WHENLT    ELPTDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQNXTD
     C                   CALL      'SRPAST8'
     C                   PARM                    QQNXTD            8 0
     C                   PARM      1             QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   Z-ADD     QQNXTD        XQCTDT
     C     XXPO01        WHENGE    ELPTDT
     C                   Z-ADD     0             CT1
     C                   DO        3             CT1
     C                   Z-ADD     XXPO31        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQCTDT
     C     XQCTDT        IFNE      *ZERO
     C                   LEAVE
     C                   ELSE
     C                   SUB       1             XXPO31
     C                   ENDIF
     C                   ENDDO
     C                   ENDSL
     C                   ELSE
     C                   Z-ADD     XXNXTD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQNXTD
     C                   CALL      'SRPAST8'
     C                   PARM                    QQNXTD            8 0
     C                   PARM      1             QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   Z-ADD     QQNXTD        XQCTDT
     C                   ENDIF
     C                   CLEAR                   XQCNTD
     C     CFMTH         CAT       '/':0         XQCNTD
     C     XQCNTD        CAT       CFDAY:0       XQCNTD
     C     XQCNTD        CAT       '/':0         XQCNTD
     C     XQCNTD        CAT       CFYR:0        XQCNTD
     C     XQCNTD        CAT       '-':1         XQCNTD
     C     XQCNTD        CAT       CTMTH:1       XQCNTD
     C     XQCNTD        CAT       '/':0         XQCNTD
     C     XQCNTD        CAT       CTDAY:0       XQCNTD
     C     XQCNTD        CAT       '/':0         XQCNTD
     C     XQCNTD        CAT       CTYR:0        XQCNTD
      *
     C                   MOVEL     BAL25         XQBALV
     C                   MOVEL     CUR25         XQCURV
     C                   MOVEL     PSD25         XQPSDV
      * PRIOR CHARGE XQPCHG
     C                   Z-ADD     XPPRMC        XQPCHG
     C                   ADD       XPOTHC        XQPCHG
     C                   ADD       XPTAXC        XQPCHG
     C                   ADD       XP1STC        XQPCHG
     C                   ADD       XP2NDC        XQPCHG
     C                   ADD       XP3RDC        XQPCHG
     C                   ADD       XP4THC        XQPCHG
     C                   ADD       XP5THC        XQPCHG
     C                   ADD       XP6THC        XQPCHG
     C                   ADD       XP7THC        XQPCHG
     C                   ADD       XPDEDC        XQPCHG
     C                   ADD       XPMBFC        XQPCHG
     C                   ADD       XPEXPC        XQPCHG
     C                   ADD       XPDEPC        XQPCHG
     C                   ADD       XPINFC        XQPCHG
      * PRIOR PAYMENTS XQPPYM
     C                   Z-ADD     XPPRMP        XQPPYM
     C                   ADD       XPOTHP        XQPPYM
     C                   ADD       XPTAXP        XQPPYM
     C                   ADD       XP1STP        XQPPYM
     C                   ADD       XP2NDP        XQPPYM
     C                   ADD       XP3RDP        XQPPYM
     C                   ADD       XP4THP        XQPPYM
     C                   ADD       XP5THP        XQPPYM
     C                   ADD       XP6THP        XQPPYM
     C                   ADD       XP7THP        XQPPYM
     C                   ADD       XPDEDP        XQPPYM
     C                   ADD       XPMBFP        XQPPYM
     C                   ADD       XPEXPP        XQPPYM
     C                   ADD       XPDEPP        XQPPYM
     C                   ADD       XPINFP        XQPPYM
      * PRIOR BALANCE XQPBAL
     C     XQPCHG        SUB       XQPPYM        XQPBAL
      * CURRENT CHARGES XQCCHG
     C     XXAMT         ADD       XXCMAJ        XQCCHG
     C                   ADD       XCOTHC        XQCCHG
     C                   ADD       XXTAX         XQCCHG
     C                   ADD       XX1ST         XQCCHG
     C                   ADD       XX2ND         XQCCHG
     C                   ADD       XX3RD         XQCCHG
     C                   ADD       XX4TH         XQCCHG
     C                   ADD       XX5TH         XQCCHG
     C                   ADD       XX6TH         XQCCHG
     C                   ADD       XX7TH         XQCCHG
     C                   ADD       XCDEDC        XQCCHG
     C                   ADD       XCMBFC        XQCCHG
     C                   ADD       XCEXPC        XQCCHG
     C                   ADD       XDEPCU        XQCCHG
     C                   ADD       XCINFC        XQCCHG
      * TOTAL CHARGES XQTCHG
     C     XQPCHG        ADD       XQCCHG        XQTCHG
      * TOTAL PAYMENTS XQTPYM
     C                   Z-ADD     XQPPYM        XQTPYM
      * TOTAL BALANCE XQTBAL
     C     XQTCHG        SUB       XQTPYM        XQTBAL
      * CURRENT DUE BOX
     C                   Z-ADD     XQTBAL        CURBAL
      * PAST DUE BOX
     C                   Z-ADD     XQPBAL        PSTBAL
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   WRITE     QDETT
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C                   MOVEL     MCP25         XQMCPV
     C                   MOVEL     DIT25         XQDITV
     C                   MOVEL     INS25         XQINSV
     C                   MOVEL     RTO25         XQRTOV
     C                   MOVEL     XXCKNM        XQCKNM
     C                   MOVEL     OFAARA        XQDITP
     C                   MOVEL     OFAPHN        XAPHN1            3
     C                   MOVE      OFAPHN        XAPHN2            4
     C                   CAT       '-':0         XQDITP
     C                   CAT       XAPHN1:0      XQDITP
     C                   CAT       '-':0         XQDITP
     C                   CAT       XAPHN2:0      XQDITP
     C                   CLEAR                   XQWCFY
     C                   MOVEL     WCF25         XQWCFY
     C                   CAT       BEGYR:1       XQWCFY
     C                   MOVEL     PRT25         XQPRTH
     C                   MOVEL     X1TOTL        XXNUM
     C                   CALL      'WA003R'
     C                   PARM                    XXNUM            80
     C                   PARM      'X,XXX.XX'    EDTMSK           10
     C                   MOVEL(P)  XXNUM         XXDATA
     C     ' '           CHECK     XXDATA        N                 3 0
     C     N             IFLE      60                                           LENGTH
     C                   SUBST(P)  XXDATA:N      XXNUM                          FIELD
     C                   ENDIF
     C     '$'           CAT       XXNUM:0       XQTOTL
     C                   MOVE      *ALL'_'       XQAMRH
     C                   MOVEL     AMR25         XQAMRH
     C                   MOVEL     PRW25         XQPRWH
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   WRITE     QBOTT
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C                   OTHER
       // Populate invoice field
       Select;
       When EAinv# <> 0 or EAnInv <> 0 Or dspInvoice# <> 0;
          //*** Begin Chg *** 12/04/24 *************************************
          //*** Select;
          //*** When dspInvoice# <> 0;
          //***    invoiceInf = 'Invoice #: ' + %EditC(ELco#:'X') +
          //***         %EditC(ELfnd:'X') + %EditC(ELemp#:'X') + '-' +
          //***         %EditC(dspInvoice#:'X');
          //*** When EAninv <> 0;
          //***    invoiceInf = 'Invoice #: ' + %EditC(ELco#:'X') +
          //***         %EditC(ELfnd:'X') + %EditC(ELemp#:'X') + '-' +
          //***         %EditC(EAninv:'X');
          //*** When EAinv# <> 0;
          //***    invoiceInf = 'Invoice #: ' + %EditC(ELco#:'X') +
          //***         %EditC(ELfnd:'X') + %EditC(ELemp#:'X') + '-' +
          //***         %EditC(EAinv#:'X');
          //***
          //*** EndSl;
          Select;
          When dspInvoice# <> 0;
             invoiceInf = 'Invoice #: ' + %EditC(ELco#:'X') +
                  %EditC(ELfnd:'X') + %EditC(ELemp#:'X') +
                  %EditC(ELdiv:'X') + '-' +
                  %EditC(dspInvoice#:'X');
          When EAninv <> 0;
             invoiceInf = 'Invoice #: ' + %EditC(ELco#:'X') +
                  %EditC(ELfnd:'X') + %EditC(ELemp#:'X') +
                  %EditC(ELdiv:'X') + '-' +
                  %EditC(EAninv:'X');
          When EAinv# <> 0;
             invoiceInf = 'Invoice #: ' + %EditC(ELco#:'X') +
                  %EditC(ELfnd:'X') + %EditC(ELemp#:'X') +
                  %EditC(ELdiv:'X') + '-' +
                  %EditC(EAinv#:'X');
          EndSl;
          //*** End   Chg *** 12/04/24 *************************************
       Other;
          invoiceInf = ' ';
       EndSl;

     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   SELECT
     C     CLTID         WHENEQ    '114'
     C                   WRITE     QHEAD
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   WRITE     QHEAD2
     C                   WRITE     QHEAD3
     C                   WRITE     QDET
     C                   WRITE     QAGT
     C                   WRITE     QINFO114
     C                   WRITE     QBOT114
     C     CLTID         WHENEQ    '129'
     C                   WRITE     QHEAD
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   WRITE     QHEAD2
     C                   WRITE     QHEAD3
     C                   WRITE     QDET
     C                   WRITE     QAGT
     C                   WRITE     QINFO129
     C                   WRITE     QBOT129
     C     CLTID         WHENEQ    '133'
     C                   WRITE     QHEAD
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1133
     C                   WRITE     QHEAD2
     C                   WRITE     QHEAD3
     C                   WRITE     QDET
     C                   WRITE     QAGT133
     C                   WRITE     QINFO133
     C                   WRITE     QBOT
     C                   OTHER
     C                   WRITE     QHEAD
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   WRITE     QHEAD2
     C                   WRITE     QHEAD3
     C                   WRITE     QDET
     C                   WRITE     QAGT
     C                   WRITE     QINFO
     C                   WRITE     QBOT
     C                   ENDSL
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************

     C                   ENDSL
     C*
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   SELECT
     C     CLTID         WHENEQ    '132'
     C                   WRITE     QXTRA1
     C                   ENDSL
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C*
     C                   ELSE
     C*
     C* PRINT OLD FORMAT PREMIUM NOTICE.
     C*
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   WRITE     INVOIC
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C*
     C                   ENDIF                                                  EXBTYP IFEQ
     C*
     C* CLOSE PRINTER FILE FOR NOTICE.
     C*
     C                   ENDIF                                                  XXPFB = 'P', 'B'
     C     XXPFB         IFEQ      'B'
     C     XXPFB         OREQ      'F'
     C     isUseXML      OREQ      *on
     C                   EXSR      SROUTF
     C                   ENDIF                                                  XXPFB = 'P', 'B'
      *
     C     XXDRES        IFEQ      'Y'
     C                   ENDIF
      *
     C                   ENDIF                                                  EBACUR = Y
      *
     C     EMKEY         CHAIN     PEM01                              40
      *
      * WRITE A DARTS REMARK
      *
     C                   CALL      'SRCHKPGM'
     C                   PARM      'WIAR'        XXPGNM           10
     C                   PARM      *BLANKS       XXRTSM            1
     C                   If        xxrtsm = *off
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W8010R'      XXPGNM
     C                   PARM      *BLANKS       XXRTSM
     C                   Endif
     C*
     C* Generate document if called from WIAR and Scriptura not being used.
     C*
     C                   If        xxrtsm = *on and isUseXML = *off
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'PN'          ##PRE             2
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM      QQFND         ##FND             3
     C                   PARM      'P'           ##RTCD            1
     C*
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C     XXOPN         IFEQ      *ON
     C                   CLOSE     W7321O
     C                   MOVE      *OFF          XXOPN
     C                   ENDIF
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C                   MOVE      XXEMP#        ##EMP#
     C                   MOVE      '*YES'        X2HOLD            4
      *
     C                   CALL      'W7321C0'
     C                   PARM                    ##FLR            80
     C                   PARM                    ##DCID           12
     C                   PARM                    ##EMP#            9
     C                   PARM                    X2HOLD
      *
     C                   MOVEL(P)  ##FLR         Q#FOLD           80
     C                   MOVEL(P)  ##DCID        Q#DCID           12
     C                   Clear                   xxRoot           10
      /free
       DocRoot(xxRoot);

       In svLDA;
       xxPath = '/' + %trim(xxRoot) + '/' + %trim(Q#Fold) + '/' + %trim(Q#Dcid);
       Out svLDA;

      /end-free
     C*
     C                   Endif
      /free
       If qqDRmk <> ' ' and x2DRes <> 'Y' and (cltId <> '102' or (isProcAll <>
        'Y' or intCount = 12));
      /end-free
     C                   EXSR      DARTS
     C                   ENDIF
     **
     C                   TIME                    DSDTTM
     **
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EMDT
     **
     C                   Z-ADD     DSTIME        EMTIME
     C                   Z-ADD     XXPODT        EMLIDT
     C     *IN40         IFEQ      *OFF
     C                   UPDATE    PEM01
     C                   ENDIF
      *
     C                   ENDIF                                                  IF XXTOTL > 0
      /Free
          Endif;
      /End-free
     C                   ENDIF                                                  CD,Y >=XXPODT
     C                   ENDIF                                                  IN60 = *OFF
       Endif; // If isSelected = *On
     C     *IN67         IFEQ      *ON
     C                   MOVE      *OFF          *IN67
     C                   ENDIF
     C     AUDIT         IFEQ      *ON
     C                   MOVE      *OFF          AUDIT
     C                   ENDIF
     C     XXMPNT        IFEQ      *ON                                          MAND PREM NOTIC
     C                   MOVE      *OFF          XXMPNT
     C                   ENDIF
      /free
          Endif;

          MM1 = MM1 + 1;
          DD1 = 1;

          If MM1 > 12;
             MM1 = 1;
             YY1 = YY1 + 1;
          Endif;

          If isProcAll <> 'Y';
             Leave;
          Endif;

       Endfor;
      /end-free
     C                   READ      PEB01                                  61
     C                   ENDDO                                                  IN61DOWEQ*OFF
      *
     C     X2DRES        IFEQ      'Y'
     C                   MOVEL     'Y'           XXDRES
     C                   ENDIF
     C*
     C                   SELECT
     C     XXDRES        WHENEQ    'Y'
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C     XXOPN         IFEQ      *ON
     C                   CLOSE     W7321O
     C                   ENDIF
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C                   CLEAR                   X2DRES
     C     *LOVAL        SETLL     PEB01
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 *******************************************
     C                   OPEN      W7321O
     C*** Begin Add *** 09/26/23 *******************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C                   MOVE      *ON           XXOPN
     C                   MOVE      *OFF          *IN67
      **** IN67 WILL BE SETON ONLY FOR RETRO SETTLEMENT CALL
     C     XXRTSM        IFEQ      *ON
     C                   MOVE      *ON           *IN67
     C                   ENDIF
     C*
     C     XXGRMA        IFEQ      'Y'
     C     XXRTSM        ANDEQ     *OFF
      **** IN67 WILL BE SETOFF FOR MANDATORY PREMIUM NOTICE PRINT CALL
     C                   MOVE      *ON           XXMPNT
     C                   ENDIF
     C*
     C                   MOVE      *OFF          AUDIT
     C     XXMODE        IFEQ      *ON
     C                   MOVE      *ON           AUDIT
     C                   ENDIF
     C                   MOVE      AUDIT         *IN49
     C                   OTHER
     C                   LEAVE
     C                   ENDSL
     C*
     C                   ENDDO
      /free
       Exsr wrtRoute;
      /end-free
      *
      /Free

      /End-free
      /Free
          //*** Begin Add *** 09/26/23 *************************************
          //*** If isUseXml;
          If isUseXml And Not doNotRunScriptura;
          //*** End   Add *** 09/26/23 *************************************
             Select;
             When ldSRPayImpFlg = 'Y';
             When xmlFlowName = *blanks;
             When objExists('PDSDEV':'*LIB');
                inXml = xmlFlowName + '$';
        //* Call process to generate Scriptura output/Darts.
                Monitor;
                Callp     GenScriptOut(inXml);
                On-error;
                EndMon;
             Other;
                inXml = xmlFlowName + '$';
                Callp     GenScriptOut(inXml);
             EndSl;
          Endif;
      /end-free
     C                   MOVE      *ON           *INLR
      *
      *--------------------------------------------------------------------
      * wrtRoute
      *--------------------------------------------------------------------

      /free
       Begsr wrtRoute;
       If *in67  = *off and *in68 = *off and xxPfb <> 'F' and isBilling = *off;
          //*** Begin Add *** 09/26/23 ****************************
          If isUseSpoolOut;
          //*** End   Add *** 09/26/23 ****************************
             If xxOpn = *on;
                Close w7321o;
             Endif;
             Open w7321o;
             Write Route;
             Close w7321o;
          //*** Begin Add *** 09/26/23 ****************************
          EndIf;
          //*** End   Add *** 09/26/23 ****************************
       Endif;
       Endsr;
      /end-free
      ******************************************************
      *    -SUBROUTINE TO LOAD THE CITY, STATE, AND ZIP    *
      ******************************************************
     C     SR1ST         BEGSR
      *
     C     XXCITY        CAT(P)    ',':0         XXCSTZ           40
     C                   CAT       XXSTAT:1      XXCSTZ
     C                   CAT       XXZIP1:1      XXCSTZ
     C     XXZIP2        IFNE      *ZERO
     C     XXZIP2        ANDNE     *BLANK
     C                   CAT       '-':0         XXCSTZ
     C                   CAT       XXZIP2:0      XXCSTZ
     C                   ENDIF
      *
     C                   ENDSR
      ********************************************************************
      *  DARTS SUBROUTINE  -  CREATE DARTS RECORD
      ********************************************************************
     C     DARTS         BEGSR
     C                   If        not isUseSpoolOut
     C                   Leavesr
     C                   Endif
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
      *
      *  If office is installed, resolve output for DARTS
      *
     C     SYOFIF        IFEQ      'Y'
     C     XXDRES        ANDEQ     'Y'
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       ##FLR            80
     C                   PARM      *BLANKS       ##DCID           12
     C                   PARM      'PN'          ##PRE             2
     C                   PARM      QQCO#         ##CO#             3
     C                   PARM      QQFND         ##FND             3
     C                   PARM      'P'           ##RTCD            1
      *
     C     XXOPN         IFEQ      *ON
     C                   CLOSE     W7321O
     C                   MOVE      *OFF          XXOPN
     C                   ENDIF
      *
     C                   MOVE      XXEMP#        ##EMP#
      *
     C                   CALL      'W7321C0'
     C                   PARM                    ##FLR            80
     C                   PARM                    ##DCID           12
     C                   PARM                    ##EMP#            9
     C                   PARM                    X1HOLD
     C                   MOVEL(P)  ##FLR         Q#FOLD           80
     C                   MOVEL(P)  ##DCID        Q#DCID           12
     C                   ENDIF
      *
      *   ESTABLISH KEY
      *
     C                   MOVE      *BLANKS       ZZKEY
     C                   MOVE      XXCO#         ZZCO#
     C                   MOVE      XXFND         ZZFND
     C                   MOVE      XXEMP#        ZZEMP#
     C                   MOVE      XXDIV         ZZDIV
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  ZZKEY         Q#KEY            40
      /Free
       If cltID = '071' or isContri = '1';
       /End-free
     C                   MOVEL     D71           DACOMM          255
     C                   ELSE
     C                   MOVEL     DA            DACOMM          255
     C                   ENDIF
     C                   MOVEL(P)  RPDATE        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       DACOMM
     C                   CAT       DA2:1         DACOMM
     C                   MOVEL     RSDATE        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       DACOMM
     C                   CAT       '-':1         DACOMM
     C                   MOVEL     RSDAT2        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       DACOMM
     C                   CAT       '.':0         DACOMM
     C                   CAT       'BY:':1       DACOMM
     C                   CAT       UPUSNM:1      DACOMM
     C                   CAT       '.':0         DACOMM
     C                   CAT       'AMOUNT':1    DACOMM
     C                   CAT       'DUE:':1      DACOMM
     C                   MOVEL(P)  XXTOTL        XXNUM
     C                   MOVEL     'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       DACOMM
     C                   CAT       '.':0         DACOMM
     C                   CAT       'DUE BY':1    DACOMM
     C                   CAT       ':':0         DACOMM
     C                   MOVEL(P)  XXDUDT        XXNUM
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   CALL      CHGNUM        CHGPL
     C                   CAT       XXNUM:1       DACOMM
     C                   CAT       '.':0         DACOMM
     C                   MOVEL(P)  DACOMM        Q#COMM          256
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  'N'           Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVEL(P)  QQDRMK        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  'O'           Q#IMGF            1
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   PARM                    Q#CODE
     C                   MOVE      *BLANKS       DACOMM
     C                   Move      q#Seq#        x#Seq#
     C
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W7615R'      XXPGNM
     C                   PARM      *BLANKS       XXRTSM
      /free

       //* Only generate records if called from estimated bill for Policy Set.
       If xxRTSM = *on;

          Chain keySDCMLA pcm0a;

          If %found;
             x#Dcid = CMDcid;
             x#Fold = CMFold;
             x#Uncn = CMUncn;

             GenerateNotice(x#RRN:x#Mode:x#Type:x#Key:x#Dcid:x#Fold:x#Seq#:
              x#Uncn);
          Endif;

       Endif;

      /end-free
      *
     C     EDARTS        ENDSR
     C*====================================================================
     C* Load information for printer file
     C*====================================================================
     C     LDINFO        BEGSR
     C*
     C                   MOVEA     CXNOTE        @XNOTE
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   MOVEL(P)  MEMB          ##DATA
     C     CLTID         WHENEQ    '129'
     C     *IN72         ANDEQ     *OFF
     C     *IN66         ANDEQ     *OFF
     C                   MOVEL(P)  CERT          ##DATA
     C     CLTID         WHENEQ    '129'
     C     *IN72         ANDEQ     *OFF
     C     *IN66         ANDEQ     *ON
     C                   MOVEL(P)  PLCY          ##DATA
     C     *IN72         WHENEQ    *ON
     C                   MOVEL(P)  FILE          ##DATA
     C     *IN72         WHENEQ    *OFF
     C                   MOVEL(P)  PLCY          ##DATA
     C                   OTHER
     C                   MOVEL(P)  PLCY          ##DATA
     C                   ENDSL
     C*
     C     ##DATA        CAT(P)    '#:':1        ##DATA
     C*
     C                   CALL      'SA004R'
     C                   PARM                    ##DATA
     C*
     C                   MOVE      ##DATA        @@PLCY
     C                   MOVE      ##DATA        @2PLCY
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '129'
     C     *IN66         ANDEQ     *OFF
     C                   MOVEL(P)  CERT          ##DATA
     C                   OTHER
     C                   MOVEL(P)  PLCY          ##DATA
     C                   ENDSL
     C*
     C     ##DATA        CAT(P)    'Year:':1     ##DATA
     C*
     C                   CALL      'SA004R'
     C                   PARM                    ##DATA
     C*
     C                   MOVE      ##DATA        @@YRHD
     C                   MOVE      ##DATA        @2YRHD
     C*
     C                   CLEAR                   XXPRHD
     C                   SELECT
     C     CLTID         WHENEQ    '129'
     C     *IN66         ANDEQ     *OFF
     C     'Previous'    CAT       CERT:1        XXPRHD
     C                   OTHER
     C     'Previous'    CAT       PLCY:1        XXPRHD
     C                   ENDSL
     C                   CAT       'Yr':1        XXPRHD           45
     C                   CAT       'Unpaid':1    XXPRHD
     C*
     C     XXPRHD        CAT(P)    'Tax':1       XXTXBL
     C                   CAT       BAL:2         XXTXBL
     C*
     C                   CLEAR                   XX1ABL
     C                   CLEAR                   XX1AHD
     C*
     C     CLTID         IFEQ      '129'
     C     *IN66         ANDEQ     *OFF
     C                   MOVEL     AH2           XX1ABL
     C                   ELSE
     C                   MOVEL     AH1           XX1ABL
     C                   ENDIF
     C*
     C     ASH(1)        IFNE      *BLANKS
     C                   CAT       ASH(1):1      XX1ABL
     C                   ELSE
     C                   CAT       '1ST':1       XX1ABL
     C                   CAT       AH3:1         XX1ABL
     C                   ENDIF
     C*
     C                   CAT       BAL:1         XX1ABL
     C*
     C     ASH(1)        IFNE      *BLANKS
     C                   MOVEL     ASH(1)        XX1AHD
     C                   ELSE
     C                   MOVEL     '1ST ASSE'    XX1AHD
     C                   CAT       'SSMENT':0    XX1AHD
     C                   ENDIF
     C                   CAT       'Due':1       XX1AHD
     C*
     C                   CLEAR                   XX2ABL
     C                   CLEAR                   XX2AHD
     C*
     C     CLTID         IFEQ      '129'
     C     *IN66         ANDEQ     *OFF
     C                   MOVEL     AH2           XX2ABL
     C                   ELSE
     C                   MOVEL     AH1           XX2ABL
     C                   ENDIF
     C*
     C     ASH(2)        IFNE      *BLANKS
     C                   CAT       ASH(2):1      XX2ABL
     C                   ELSE
     C                   CAT       '2ND':1       XX2ABL
     C                   CAT       AH3:1         XX2ABL
     C                   ENDIF
     C*
     C                   CAT       BAL:1         XX2ABL
     C*
     C     ASH(2)        IFNE      *BLANKS
     C                   MOVEL     ASH(2)        XX2AHD
     C                   ELSE
     C                   MOVEL     '2ND ASSE'    XX2AHD
     C                   CAT       'SSMENT':0    XX2AHD
     C                   ENDIF
     C                   CAT       'Due':1       XX2AHD
     C*
     C                   CLEAR                   XX3ABL
     C                   CLEAR                   XX3AHD
     C*
     C     CLTID         IFEQ      '129'
     C     *IN66         ANDEQ     *OFF
     C                   MOVEL     AH2           XX3ABL
     C                   ELSE
     C                   MOVEL     AH1           XX3ABL
     C                   ENDIF
     C*
     C     ASH(3)        IFNE      *BLANKS
     C                   CAT       ASH(3):1      XX3ABL
     C                   ELSE
     C                   CAT       '3RD':1       XX3ABL
     C                   CAT       AH3:1         XX3ABL
     C                   ENDIF
     C*
     C                   CAT       BAL:1         XX3ABL
     C*
     C     ASH(3)        IFNE      *BLANKS
     C                   MOVEL     ASH(3)        XX3AHD
     C                   ELSE
     C                   MOVEL     '3RD ASSE'    XX3AHD
     C                   CAT       'SSMENT':0    XX3AHD
     C                   ENDIF
     C                   CAT       'Due':1       XX3AHD
     C*
     C                   CLEAR                   XX4ABL
     C                   CLEAR                   XX4AHD
     C*
     C     CLTID         IFEQ      '129'
     C     *IN66         ANDEQ     *OFF
     C                   MOVEL     AH2           XX4ABL
     C                   ELSE
     C                   MOVEL     AH1           XX4ABL
     C                   ENDIF
     C*
     C     ASH(4)        IFNE      *BLANKS
     C                   CAT       ASH(4):1      XX4ABL
     C                   ELSE
     C                   CAT       '4TH':1       XX4ABL
     C                   CAT       AH3:1         XX4ABL
     C                   ENDIF
     C*
     C                   CAT       BAL:1         XX4ABL
     C*
     C     ASH(4)        IFNE      *BLANKS
     C                   MOVEL     ASH(4)        XX4AHD
     C                   ELSE
     C                   MOVEL     '4TH ASSE'    XX4AHD
     C                   CAT       'SSMENT':0    XX4AHD
     C                   ENDIF
     C                   CAT       'Due':1       XX4AHD
     C*
     C                   CLEAR                   XX5ABL
     C                   CLEAR                   XX5AHD
     C*
     C     CLTID         IFEQ      '129'
     C     *IN66         ANDEQ     *OFF
     C                   MOVEL     AH2           XX5ABL
     C                   ELSE
     C                   MOVEL     AH1           XX5ABL
     C                   ENDIF
     C*
     C     ASH(5)        IFNE      *BLANKS
     C                   CAT       ASH(5):1      XX5ABL
     C                   ELSE
     C                   CAT       '5TH':1       XX5ABL
     C                   CAT       AH3:1         XX5ABL
     C                   ENDIF
     C*
     C                   CAT       BAL:1         XX5ABL
     C*
     C     ASH(5)        IFNE      *BLANKS
     C                   MOVEL     ASH(5)        XX5AHD
     C                   ELSE
     C                   MOVEL     '5TH ASSE'    XX5AHD
     C                   CAT       'SSMENT':0    XX5AHD
     C                   ENDIF
     C                   CAT       'Due':1       XX5AHD
     C*
     C                   CLEAR                   XX6ABL
     C                   CLEAR                   XX6AHD
     C*
     C     CLTID         IFEQ      '129'
     C     *IN66         ANDEQ     *OFF
     C                   MOVEL     AH2           XX6ABL
     C                   ELSE
     C                   MOVEL     AH1           XX6ABL
     C                   ENDIF
     C*
     C     ASH(6)        IFNE      *BLANKS
     C                   CAT       ASH(6):1      XX6ABL
     C                   ELSE
     C                   CAT       '6TH':1       XX6ABL
     C                   CAT       AH3:1         XX6ABL
     C                   ENDIF
     C*
     C                   CAT       BAL:1         XX6ABL
     C*
     C     ASH(6)        IFNE      *BLANKS
     C                   MOVEL     ASH(6)        XX6AHD
     C                   ELSE
     C                   MOVEL     '6TH ASSE'    XX6AHD
     C                   CAT       'SSMENT':0    XX6AHD
     C                   ENDIF
     C                   CAT       'Due':1       XX6AHD
     C*
     C                   CLEAR                   XX7ABL
     C                   CLEAR                   XX7AHD
     C*
     C     CLTID         IFEQ      '129'
     C     *IN66         ANDEQ     *OFF
     C                   MOVEL     AH2           XX7ABL
     C                   ELSE
     C                   MOVEL     AH1           XX7ABL
     C                   ENDIF
     C*
     C     ASH(7)        IFNE      *BLANKS
     C                   CAT       ASH(7):1      XX7ABL
     C                   ELSE
     C                   CAT       '7TH':1       XX7ABL
     C                   CAT       AH3:1         XX7ABL
     C                   ENDIF
     C*
     C                   CAT       BAL:1         XX7ABL
     C*
     C     ASH(7)        IFNE      *BLANKS
     C                   MOVEL     ASH(7)        XX7AHD
     C                   ELSE
     C                   MOVEL     '7TH ASSE'    XX7AHD
     C                   CAT       'SSMENT':0    XX7AHD
     C                   ENDIF
     C                   CAT       'Due':1       XX7AHD
     C*
      /Free
       If isContri = '1';
          xxPrbl = %trim(xxPrhd) + ' Contri';
       Else;
          xxPrbl = %trim(xxPrhd) + ' Prem';
       Endif;
       /End-free
     C                   CAT       BAL:1         XXPRBL
     C     XXPRHD        CAT(P)    'Fees':1      XXFEBL
     C                   CAT       BAL:1         XXFEBL
     C                   MOVE      *BLANKS       ##7BLK            7
     C                   MOVE      *BLANKS       XXDUTX
      *
     C     XXDUDT        IFEQ      *ZERO
     C                   MOVEL     XCDURP        XXDUTX
     C                   MOVE      '1'           *IN65
     C                   ENDIF                                                  IFEQ *ZERO
      *
     C     XXDUDT        IFNE      *ZERO
     C                   MOVE      '0'           *IN65
     C                   MOVE      XXDUDT        #DATE
     C                   MOVE      #MM           #MMC              2
     C                   MOVE      #DD           #DDC              2
     C                   MOVE      #YY           #YYC              4
      *
     C                   MOVEL     DDT25         XXDUTX
     C     XXDUTX        CAT       ##7BLK:0      XXDUTX
     C     XXDUTX        CAT       #MMC:7        XXDUTX
     C     XXDUTX        CAT       '/':0         XXDUTX
     C     XXDUTX        CAT       #DDC:0        XXDUTX
     C     XXDUTX        CAT       '/':0         XXDUTX
     C     XXDUTX        CAT       #YYC:0        XXDUTX
      *
     C                   ENDIF                                                  IFNE *ZERO
     C*
     C                   ENDSR
     C*
      **********************************************************
      * CvtDate - Convert Date and Check Date validity
      **********************************************************
     c     cvtDate       Begsr
     c                   Call      'SRCVTDAT'    dtMDYY
     c                   Endsr
      **********************************************************
      * CvtDtCYMD - Convert Date in CYMD format
      **********************************************************
     c     cvtDtCYMD     Begsr
     c                   Call      'SRCVTDAT'    dtCYMD
     c                   Endsr
     C*****************************************************************
     C* SRINDT - Calc last audit date of prior yr plus interim days
     C*****************************************************************
     C     SRINDT        BEGSR
     C*
     C                   MOVE      'D'           XXARTY
     C*
     C     EAKYP1        SETGT     PEA02
     C     EAKYP1        READPE    PEA02                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     XXIDYS        ZZIDYS
     C                   Z-ADD     EAPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZAUDT
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    ZZAUDT            8 0
     C                   PARM                    ZZIDYS            5 0
     C                   PARM                    ZZERR             1
     C*
     C     ZZERR         IFNE      '1'
     C                   Z-ADD     ZZAUDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXAUDT            7 0
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60=*ON
     C*
     C                   ENDSR                                                  SRINDT
     C*****************************************************************
     C* SRDUDT - Calc the due date based on the a/r posting date
     C*****************************************************************
     C     SRDUDT        BEGSR
     C*
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDAT            7 0
     C                   MOVEL     XXIFDT        X5DAT1
     C                   MOVEL     SYSDAT        X5DAT2
     C                   SELECT
     C     X5DAT1        WHENGT    X5DAT2
     C                   Z-ADD     SVFPDT        WORKDT            7 0
     C     X5DAT1        WHENLE    X5DAT2
     C                   Z-ADD     SVLPDT        WORKDT
     C                   ENDSL
     C                   Z-ADD     WORKDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ZZDATE
     C*
     C                   Z-ADD     XXADDU        ZZDAYS
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    ZZDATE            8 0
     C                   PARM                    ZZDAYS            5 0
     C                   PARM                    ZZERR             1
     C*
     C     ZZERR         IFNE      '1'
     C                   Z-ADD     ZZDATE        XXDUDT
     C                   ENDIF                                                  ZZERR<>'1'
     C*
     C*
     C                   ENDSR                                                  SRDUDT
     C*****************************************************************
     C* SRRMA - Process for renewals, monthly or all
     C*****************************************************************
     C     SRRMA         BEGSR
      *
     C                   MOVE      *BLANK        XXRM              1
     C                   SELECT

     C     QQGRMA        WHENEQ    'B'
     C     QQGRMA        OREQ      'A'
     C     ELFAF         ANDEQ     'A'
     C                   MOVE      'A'           XXRM
     C     QQGRMA        WHENEQ    'R'
     C     QQGRMA        OREQ      'M'
     C                   EXSR      SRREMO
     C                   OTHER
     C                   ENDSL
      *
      *  Check if policy qualifies for processing
      *
     C     XXRM          IFEQ      'A'
     C     XXRM          OREQ      QQGRMA
     C                   SETON                                        43         qualifies
     C                   ENDIF                                                  XXRM='A'
     C*
     C                   ENDSR                                                  SRRMA
     C*****************************************************************
     C* SRREMO - Is this policy a renewal or monthly
     C*****************************************************************
     C     SRREMO        BEGSR
     C*
      *
     C     EMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
      *
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    EMDIV
      *
      *  GET BILLING START DATE OF STATE RPT DIVISION
      *
     C     EMKY1         SETLL     PEM01
     C     EMKY1         READE     PEM01                                  60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        IFEQ      'Y'
     C                   LEAVE
     C                   ENDIF
     C     EMKY1         READE     PEM01                                  60
     C                   ENDDO
      *
     C                   MOVEL     XXIFDT        X5DAT1            5 0
     C                   MOVEL     ELBSDT        X5DAT2            5 0
     C     EMDIV         IFNE      XXDIV
     C     ELKY1         CHAIN     PEL01                              60
     C                   MOVEL     ELBSDT        X5DAT2            5 0
     C     ELKEY         CHAIN     PEL01                              60
     C                   ENDIF
      *
      *  IF PROMPT FROM DATE IS IN FIRST MONTH OF STATE RPT DIV, THIS
      *  IS A RENEWAL MONTH
      *
     C                   SELECT
     C     ELNWRW        WHENEQ    'R'
     C     X5DAT1        ANDEQ     X5DAT2
     C                   MOVE      'R'           XXRM
     C                   OTHER
     C                   MOVE      'M'           XXRM
     C                   ENDSL
      *
     C                   ENDSR                                                  SRREMO
      *================================================================
      *   SRP71# - CONCAT GRP-EMP-DIV WITHOUT LEADING ZEROS
      *================================================================
     C     SRP71#        BEGSR
      *
     C                   CLEAR                   XXPLCY
     C                   CLEAR                   X1PLCY
     C                   MOVE(P)   ELFND         Z@FND             3
     C                   MOVE(P)   ELEMP#        Z@EMP             5
     C                   MOVE(P)   ELFYR         Z@FYR             3
     C     '0'           CHECK     Z@FND         X
     C                   SUBST     Z@FND:X       XXTMP1            3
     C     XXTMP1        CAT       '-':0         X1PLCY           13
     C     X1PLCY        CAT       Z@EMP:0       X1PLCY
     C     X1PLCY        CAT       '-':0         X1PLCY
     C     X1PLCY        CAT       Z@FYR:0       X1PLCY
     C                   MOVEL(P)  X1PLCY        QQSTRG
     C                   CALL      'SA004R'
     C                   PARM                    QQSTRG
     C                   MOVE(P)   QQSTRG        XXPLCY
      *
     C                   ENDSR
     C*----------------------------------------------------------------
     C* SROUTF - WRITE RECORDS IN WFPNP FOR EACH PREMIUM NOTICE PROCESSED.
     C*----------------------------------------------------------------
     C     SROUTF        BEGSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         saveDate          7 0
     C*
     C                   If        isUseXml and z = 1
     C                   Open      wxpnp1
     C                   Exsr      ScriptXML
     C                   Endif
     C                   CLEAR                   PFN02
     C*** Begin Add *** 02/12/25 *******************************************
       //
       // Let's restore the invoice#
       If holdInvoice# <> 0;
          EAninv = holdInvoice#;
       EndIf;
     C*** End   Add *** 02/12/25 *******************************************
     C*
     C                   MOVE      RSCO#         PNCO#                          Company
     C                   MOVE      RSFND         PNFND                          Group
     C                   MOVEL     XXACNM        PNFNAM                         Group Name
     C                   MOVE      XXEMP#        PNEMP#                         Employer#
      *** Beg Chg *** 04/30/25 **************************************************
     C***                MOVE      QQDIV         PNDIV                          Division
     C                   MOVE      EMDIV         PNDIV                          Division
      *** End Chg *** 04/30/25 **************************************************
     C                   MOVE      XXFYR         PNFYR                          Policy Year
     C                   MOVEL     EMNAME        PNENAM                         Employer Name
     C                   MOVEL     XXFDNM        PNEFDN                         Emp Federal ID
     C                   MOVEL     XXADR1        PNMAD1                         Emp Mail Addr 1
     C                   MOVEL     XXADR2        PNMAD2                         Emp Mail Addr 2
     C                   MOVEL     XXECSZ        PNMCSZ                         Emp Cty,St,Zip
     C                   Z-ADD     XXAGT#        PNAGT#                         Agent#
     C                   MOVEL     AGNAME        PNANNM                         Agent Name
     C                   MOVEL     AGMAD1        PNAAD1                         Agent Addr 1
     C                   MOVEL     XXACSZ        PNACSZ                         Agent Cty,St,Zp
     C                   MOVEL     OFACNM        PNACNM                         Acc dept name
      *** Beg Chg ***** 05/22/24 **************************************
     C***                MOVEL     OFAAD1        PNACA1                         Account Add1
     C***                MOVEL     OFAAD2        PNACA2                         Account Add2
       //*** Beg Chg ***** 10/30/24 **************************************
       //***PNACA1 = xxOFL2;
       //***PNACA2 = xxOFL3;
       PNACA1 = xxAAD1;
       PNACA2 = xxAAD2;
       //*** End Chg ***** 10/30/24 **************************************
      *** Beg Chg ***** 05/22/24 **************************************
     C                   MOVEL     XOACSZ        PNACCZ                         Acc cty, st zip
     C                   MOVE      XXIFDT        PNFIDT                         Inst from date
     C                   MOVE      XXITDT        PNFTDT                         Inst to date
     C                   MOVEL     QQGRMA        PNNWFL                         New/Ren or Both
     C                   MOVE      RSADDU        PNADYS                         Add days inst due
     C                   MOVEL     RSEXDF        PNEDFL                         Excld deduc fee(Y/N)
     C                   Z-ADD     XXIDYS        PNIDPA                         Int days pr audit
     C                   MOVEL     RSCOM1        PNCOM1                         Comment code 1
     C                   MOVEL     RSCMT1        PNCMD1                         Comt code des 1
     C                   MOVEL     RSCOM2        PNCOM2                         Comment code 2
     C                   MOVEL     RSCMT2        PNCMD2                         Comt code des 2
     C                   MOVEL     RSCOM3        PNCOM3                         Comment code 3
     C                   MOVEL     RSCMT3        PNCMD3                         Comt code des 3
     C                   MOVE      RSDRMK        PNDART                         Dart code
     C                   MOVEL     XXDRES        PNDRRS                         Resolve to dart
     C                   MOVE      XXMINB        PNMNBD                         Min Bal due
     C                   MOVE      XXEXP         PNECFL                         Incl exp con flag
     C                   MOVE      XXDEP         PNDPFL                         Incl deposit flag
     C                   MOVE      ELBSCD        PNBSCD                         Billing sch code
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKY          CHAIN     PBS01                              60
     C  N60              MOVEL     BSDESC        PNBSDS                         Billing sch des
     C******* ACH INFO******************************************************
     C     ACKY          CHAIN     PDB02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           PNACHE                         ACH Emp (Y/N)
     C                   MOVE      DBCSFL        PNCSFL                         ACH chk/sav flag
     C                   MOVE      DBDFII        PNDFII                         DFI identification
     C                   MOVE      DBTRCD        PNTRCD                         Transit/Routing
     C                   MOVEL     DBDFIA        PNDFIA                         DFI Account no.
     C                   MOVE      DBPRED        PNPRED                         Pre-Noti date
     C                   MOVE      DB1STD        PN1STD                         Date of 1st transfer
     C                   MOVE      DBLSTD        PNLSTD                         Date of lst transfer
     C                   ENDIF
     C*************************************************************************
     C                   MOVE      PNACHE        PNACHE                         Date of bill
     C                   MOVE      XXPLCY        PNPOL#                         Policy number
     C                   MOVE      XXAMTP        PNUPBL                         Pr unpd premium blnc
     C                   MOVE      XXTAXP        PNUTBL                         Pr unpd TAX blnce
     C                   MOVE      XXFEEP        PNUFBL                         Prv unpaid fee
     C                   MOVE      XXAMT         PNCURP                         Cur mth bil pre
     C                   MOVE      XXFEED        PNFEED                         Fee due
     C                   MOVE      XXTAX         PNSTXD                         State tax due
     C                   MOVE      XXDEDT        PNDFDU                         Ded fee due
     C                   MOVE      XX1STP        PN1ASS                         1st assmt due
     C                   MOVE      XX2NDP        PN2ASS                         2nd assmt due
     C                   MOVE      XX3RDP        PN3ASS                         3rd assmt due
     C                   MOVE      XX4THP        PN4ASS                         4th assmt due
     C                   MOVE      XX5THP        PN5ASS                         5th assmt due
     C                   MOVE      XX6THP        PN6ASS                         6th assmt due
     C                   MOVE      XX7THP        PN7ASS                         7th assmt due
     C                   MOVE      XXMBFT        PNMFDU                         Mbrship fee due
     C                   MOVE      XXEXCN        PNEXDU                         Exp const due
     C                   MOVE      XXIFAM        PNINDU                         Inst fee due
     C                   MOVE      X1TOTL        PNTOTD                         Tot amt due
     C                   MOVE      XXDUDT        PNDUDT                         Due date
     C                   MOVEL     @XCKNM        PNCHKP                         Chk payable to
     C                   MOVEL     XXCONM        PNSRVB                         Serviced by
     C***************DIRECT INQUIRIES TO*********************************
     C                   MOVE      XXAREA        X@AREA            3
     C                   MOVE      XXPHN1        X@PHN1            3
     C                   MOVE      XXPHN2        X@PHN2            4
     C     XXUNM2        CAT(P)    X@AREA:2      PNDIRT
     C                   CAT(P)    X@PHN1:2      PNDIRT
     C                   CAT(P)    X@PHN2:2      PNDIRT
     C*******************************************************************
      *
     C                   Movel     xxphn1        PNOffPhn
     C                   Move      xxphn2        PNOffPhn
     C                   Eval      PNOffArea = xxarea
     C                   Eval      PNDirInq = xxunm2
     C*** Begin Chg *** 09/26/23 *******************************************
     C**********         Eval      PNXML = XPXMLT
     C                   Eval      PNXML = xmlFile
     C*** End   Chg *** 09/26/23 *******************************************
     C                   Eval      PNTxbl = xxTxbl
      /free

       If ash(1) <> ' ';
          PN1abl = %trim(ah4) + ' ' + %trim(ash(1)) + ' Balance';
       Else;
          PN1abl = %trim(ah4) + ' 1st Assessment Balance';
       Endif;

       If ash(2) <> ' ';
          PN2abl = %trim(ah4) + ' ' + %trim(ash(2)) + ' Balance';
       Else;
          PN2abl = %trim(ah4) + ' 2nd Assessment Balance';
       Endif;

       If ash(3) <> ' ';
          PN3abl = %trim(ah4) + ' ' + %trim(ash(3)) + ' Balance';
       Else;
          PN3abl = %trim(ah4) + ' 3rd Assessment Balance';
       Endif;

       If ash(4) <> ' ';
          PN4abl = %trim(ah4) + ' ' + %trim(ash(4)) + ' Balance';
       Else;
          PN4abl = %trim(ah4) + ' 4th Assessment Balance';
       Endif;

       If ash(5) <> ' ';
          PN5abl = %trim(ah4) + ' ' + %trim(ash(5)) + ' Balance';
       Else;
          PN5abl = %trim(ah4) + ' 5th Assessment Balance';
       Endif;

       If ash(6) <> ' ';
          PN6abl = %trim(ah4) + ' ' + %trim(ash(6)) + ' Balance';
       Else;
          PN6abl = %trim(ah4) + ' 6th Assessment Balance';
       Endif;

       If ash(7) <> ' ';
          PN7abl = %trim(ah4) + ' ' + %trim(ash(7)) + ' Balance';
       Else;
          PN7abl = %trim(ah4) + ' 7th Assessment Balance';
       Endif;

      /end-free
     C                   Eval      PNPrbl = xxPrbl
     C                   Eval      PNFebl = xxFebl
     C                   Eval      PNPvbl = 'Previous Balance'
     C                   Eval      PNCurh = xxDesc
     C                   Eval      PNPdbl = 'Previous Unpaid Deposit'
     C                   If        xxColt = 'Y'
     C                   Eval      PNPdbl = 'Previous Unpaid Collateral'
     C                   Endif
     C                   Eval      PNCdhd = x1Desc
     C                   Eval      PNCahd = 'Current Month Adjustments'
     C                   Eval      PNTaxh = 'State Tax Due'
     C                   Eval      PN1sth = xx1ahd
     C                   Eval      PN2ndh = xx2ahd
     C                   Eval      PN3rdh = xx3ahd
     C                   Eval      PN4thh = xx4ahd
     C                   Eval      PN5thh = xx5ahd
     C                   Eval      PN6thh = xx6ahd
     C                   Eval      PN7thh = xx7ahd
     C                   Eval      PNFeeh = 'Fees Due'
     C                   If        xxClt# = '129'
     C                   Eval      PNFeeh = 'State Fees Due'
     C                   Endif
     C                   Eval      PNDedh = 'Deductible Fees Due'
     C                   Eval      PNMbrh = 'Membership Fees Due'
     C                   Eval      PNExph = 'Expense Constant Due'
     C                   Eval      PNInsh = 'Installment Fees Due'
     C                   Eval      PNToth = 'Total Amount Due'
     C*
     C                   Eval      PN1st  = xx1st
     C                   Eval      PN2nd  = xx2nd
     C                   Eval      PN3rd  = xx3rd
     C                   Eval      PN4th  = xx4th
     C                   Eval      PN5th  = xx5th
     C                   Eval      PN6th  = xx6th
     C                   Eval      PN7th  = xx7th
     C                   Eval      PNAmtd = xxAmtd
     C                   Eval      PNPdep = xdeppv
     C                   Eval      PNCdep = xdepcu
     C                   Eval      PNCadj = xxcmaj
     C*
     C                   Eval      PNPfdt = xqfrdt
     C                   Eval      PNPtdt = xqtodt
     C                   Eval      PNRefHdg = %trim(@1plcy)
     C                   Eval      PNPolicy# = xxplcy
     C*
     C                   If        *in72 = *on
     C                   Eval      PNRefHdg = %trim(@@plcy)
     C                   Eval      PNCarHdg = %trim(xqcaph)
     C                   Eval      PNCarpol = xqcap#
     C                   Endif
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         PNPrtDate
     C*
     C                   Eval      PNRateSt = xxst
     C                   Eval      PNAgtHdg = xxaghd
     C                   If        xxagn1 <> 0
     C                   Movel     xxagn1        PnAgtPhn
     C                   Move      xxagn2        PnAgtPhn
     C                   Eval      PNAgtArea = xaarea
     C                   Endif
     C*
     C                   Eval      PNMCity = xxeCity
     C                   Eval      PNMSt   = xxeSt
     C                   Eval      PNMZip  = xxeZip
     C*
     C                   Eval      PNACity = xxaCity
     C                   Eval      PNASt   = xxaSt
     C                   Eval      PNAZip  = xxaZip
     C*
     C                   Eval      PNOCity = xxoCity
     C                   Eval      PNOSt   = xxoSt
     C                   Eval      PNOZip  = xxoZip
     c                   Eval      PNdt = saveDate
       PN3rdVrb = str3rdVrb;
       Select;
       //*** Begin Add *** 12/04/24 ****************************************
       When dspInvoice# <> 0;
          PNinv# = dspInvoice#;
       //*** End   Add *** 12/04/24 ****************************************
       When EAnInv <> 0;
          PNinv# = EAnInv;
       When EAinv# <> 0;
          PNinv# = EAinv#;
       Other;
          PNinv# = 0;
       EndSl;
       //*** Beg Add ***** 04/04/24 ****************************************
       PNocr = XXocr;
       If isContri = '1';
          PNTitle = 'Contribution Notice';
          PNPrmWord = 'Contribution';
       Else;
          PNTitle = 'Premium Notice';
          PNPrmWord = 'Premium';
       Endif;
       //*** End Add ***** 04/04/24 ****************************************
       //*** Beg Add ***** 06/24/24 ****************************************
       If xxcold <> 0;
          PNcold  = xxcold;
          PNcoldH = 'Collateral Due';
       Endif;
       //*** End Add ***** 06/24/24 ****************************************
     C*
     C     XXPFB         IFEQ      'B'
     C     XXPFB         OREQ      'F'
     C                   WRITE     PFN02
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        templateFile <> ' '
     C                   Eval      PNXML = templateFile
     C                   WRITE     PFN02
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C                   Endif
     C
     C                   If        isUseXml and z = 1
     C
     C                   WRITE     PFN03
     C*** Begin Add *** 09/26/23 *******************************************
     C                   If        templateFile <> ' '
     C                   Eval      PNXML = templateFile
     C                   WRITE     PFN03
     C                   EndIf
     C*** End   Add *** 09/26/23 *******************************************
     C                   Close     wxpnp1
     C
     C                   Endif
     C                   ENDSR                                                  SROUTF
     C*----------------------------------------------------------------
     C* INIT - INITIALIZATION
     C*----------------------------------------------------------------
     C     SRINIT        BEGSR
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C                   MOVEL(P)  'WA073R'      XXPGM
    C*
     C                   MOVEL     '*PGM'        XXTYP             7
     C                   CALL      'SRCHKOBJ'
     C                   PARM                    XXPGM            10
     C                   PARM                    XXTYP
     C                   PARM                    XXNACH            1
     c                   Call      'SRCHKPGM'
     c                   Parm      'W8002C'      xxPgNm
     c                   Parm      *Blanks       isBilling         1
     c
     c                   If        isBilling = *off
     c
     c                   Call      'SRCHKPGM'
     c                   Parm      'W7700C'      xxPgNm
     c                   Parm      *Blanks       isBilling         1
     c
     c                   Endif
      *
     C                   MOVE      *OFF          *IN75
     C     XXCLT#        IFEQ      '071'
     C                   MOVE      *ON           *IN75
     C                   ENDIF
      /free
       isLockBox = IsModEnabled('LOCKBOX');
       If isLockbox and xxclt# <> '114' and xxclt# <> '129';
          *in75 = *on;
       Endif;
      /end-free
      *
      /free
       If xxCLT# = '129';
          *in64 = *On;
       Else;
          *in64 = *Off;
       Endif;
      /end-free
      *
     C                   ENDSR
      /free

       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

       xmlFile = ' ';
       message = %trim(processType);
       string = ' ';
       If isMassReq;

          //*** Begin Chg *** 09/26/23 *************************************
          //*** StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
          //***  'PremNotice':'000':'000':massFlag:xmlLink);

          //*** isMassReq = *off;
          //*** xmlLink = xmlFile;
          //*** xmlFlowName = xmlFile;
          //*** Chain (xmlFile) xp100;
          //*** XPCat = 'PS';
          //*** Update xp100;
          In *Lock *dtaara;
          Out *dtaara;

          If ldMassFile <> ' ';
             xmlFile = ldMassFile;
          Else;
             StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
              'PremNotice':'000':'000':massFlag:xmlLink);

             In *Lock *dtaara;
             ldMassFile = %trim(xmlFile);
             Out *dtaara;

             Chain (xmlFile) xp100;
             XPCat = 'PS';
             Update xp100;
          EndIf;

          isMassReq   = *off;
          xmlLink     = xmlFile;
          xmlFlowName = xmlFile;
          //*** End   Chg *** 09/26/23 *************************************

       Endif;

       StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):
        xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

       If xmlFlowName = ' ';
          xmlFlowName = xmlFile;
       Endif;

       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       XPData = 'Y';
       //* Default iDarts fields if LDA has not been set up.
       If xxDart = ' ' and xxCopy = ' ';
          XPDart = xxResolve;
          XPRmcd = QQDRmk;
       Endif;
       //*** Beg Add ***** 03/20/24 ************************************
       If isQteMode;
          XPQte = 'Q';
       Else;
          XPQte = *Blank;
       ENDIF;
       //*** End Add ***** 03/20/24 ************************************
       //*** Begin Add *** 09/26/23 *************************************
       // Populate Darts Remark
       If calledFromBills And XPdart = 'Y';
          XPRmcd = QQDRmk;
       EndIf;

       // Only set email if called from Bill processes
       If calledFromBills;
          // Populate Email Address
          If xxDart = 'Y' And keyEmail <> ' ' And
             (BLmrgPrn = 'N' Or  BLmrgPrn = ' ');
             XPEMailD = keyEmail;
          Else;
             XPEMailD = ' ';
          EndIf;

          If BLmrgPrn <> 'N' And BLmrgPrn <> ' ';
             XPskip = 'Y';
          EndIf;

          // Clear Other Email fields
          //*** Beg Chg ***** 04/24/24 **********************************
          //***If keyEmail = ' ' And BLmrgPrn <> 'N' And BLmrgPrn <> ' ';
          //***   XPmail    = ' ';
          //***   XPmailAdd = ' ';
          //***EndIf;
          //Premium Notice will never separately emailed when called from Bills
          // It will be emailed either part of the combo doc or an attachement with the Bill
          XPmail    = ' ';
          XPmailAdd = ' ';
       EndIf;

       // Process when Premium Notice is merged into Combo Document
       //*** Beg Chg ***** 02/24/25 ************************************
       //***If BLmrgPrn <> 'N' And BLmrgPrn <> ' ' And calledFromBills;
       If BLmrgPrn <> 'N' and BLmrgPrn <> ' ' and calledFromBills
        and ldOutputFile <> *Blank;
       //*** End Chg ***** 02/24/25 ************************************

          // Call STLETTER in TEMPLRQST
          templateFile = ldOutputFile;
          message      = processType;

          StLetter('TEMPLRQST':templateFile:message:'E':%editc(x#RRN:'X'):
           xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:
           xmlLink);

          //*** Beg Add ***** 02/04/25 **********************************
          Chain (templateFile) xt101;
          If %Found(xmlteml1);
             XTData = 'Y';
             XTCo#  = xxCo#;
             XTFnd  = xxFnd;
             XTEmp# = xxEmp#;
             XTDiv  = xxDiv;
             XTFyr  = ELFyr;
             Update xt101;
          Endif;
          //*** End Add ***** 02/04/25 **********************************
          If BLmrgPrn = 'O';
             xmlFile = templateFile;
             templateFile = ' ';
             Delete xp100;
             LeaveSR;
          EndIf;
       EndIf;
       //*** End   Add *** 09/26/23 *************************************

       //*** Beg Add ***** 04/04/24 **************************************
       If isContri = '1';
          XPIDesc = 'Contribution Notice';
       Else;
          XPIDesc = 'Premium Notice';
       Endif;
       XPIDesc = %Trim(XPIDesc) + ' was requested by ' + %Trim(UPUSNM) +
        ' on ' + %Char(%Date():*USA) + ' for policy year ' +
        %Char(xxFyr) + '.';
       //*** End Add ***** 04/04/24 **************************************

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DsTime;
       XPDt   = saveDate;

       Update xp100;
       In svLDA;
       xxPath = %trim(XPDir) + %trim(XPFile);
       Out svLDA;

       Endsr;

      /end-free
     O*
     OPEM01     E            PEM01E
     O*

     P*=======================================================================
     P* Add the check digit to the OCR line
     P*=======================================================================
     PAddCheckDigit    b
     DAddCheckDigit    pi                  like(xxOCR)
     D inOCR                               like(xxOCR) const

     D dsOCL           ds
     D  ocl                           1    DIM(46)                              SCANLINE          %
     D xxNum           s              1  0
     D xxLn            s              1  0
     D xxRn            s              1  0
     D xx2n            s              2
     D xxSum           s              5  0 inz
     D xxCheckDigit    s              2  0
     D i               s              2  0
     D outOCR          s                   like(xxOCR)

      /Free
       outOCR = inOCR;
       dsOCL = inOCR;
       For i = 1 to %len(inOCR)-1;
          If ocl(i) = *blank;
             Leave;
          Endif;
          xxNum = %int(ocl(i));
          xx2n = %editc(xxNum * wtl(i):'X');
          xxLn = %int(%subst(xx2n:1:1));
          xxRn = %int(%subst(xx2n:2:1));
          xxSum = xxSum + (%int(%subst(xx2n:1:1)) + %int(%subst(xx2n:2:1)));
       Endfor;
       xxCheckDigit = 10 - %rem(xxSum:10);
       %subst(outOCR:i:1) = %subst(%editc(xxCheckDigit:'X'):2:1);

       Return outOCR;
      /End-free
     PAddCheckDigit    e
     P*====================================================================
     P* Determine if the object exists
     P*====================================================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e
     P*====================================================================
     P* Determine if A/R is to be skipped due to Invoice Cloud
     P*
     P* inCompany#   - Company #
     P* inInvoice#   - Invoice# passed in to program
     P* inARInvoice# - A/R Invoice#
     P* inARCode     - A/R Code
     P*
     P*====================================================================
     PSkipICAR         b
     DSkipICAR         pi             1n
     D inCompany#                     3  0 const                                Company#
     D inInvoice#                    10  0 const
     D inARInvoice#                  10s 0 const
     D inARCode                       2    const
     D inPostDate                     7  0 const options(*nopass)
     D inAmount                       9  2 const options(*nopass)

     D arCode          s             10

      /free

       // Don't skip if invoice# is zero
       If inInvoice# = 0;
          // Skip if A/R Code is marked as excluded from Invoice Cloud
          arCode = inARCode;
          Chain (inCompany#:arCode) ptt01;

          If %Found(wtabl) And TTexIC = 'Y' and xxWIARPgm = *on;
             Return *On;
          Endif;

          // Skip if A/R Code is S/R billing related and policy is non-audited S/R.
          If %parms >= 6 and xxWIARPgm = *on;

             //* Total up paid amounts for premium, tax, and installment fees.
             If TTCTyp = 'P' or TTCTyp = 'T' or arCode = IFIFBL or
              arCode = IFIFPD or arCode = IFIFTR or arCode = IFIFTC;

                If TTType = 'P' or TTType = 'Q';
                   Select;
                   When TTCtyp = 'P';
                      premPaid += inAmount;
                   When TTCtyp = 'T';
                      taxPaid  += inAmount;
                   Other;
                      instPaid += inAmount;
                   Endsl;

                Endif;

             Endif;

             //* Exclude s/r billing amounts.
             If %lookup(inPostDate:srDates) <> 0;

                If TTCTyp = 'P' or TTCTyp = 'T' or arCode = IFIFBL or
                 arCode = IFIFPD or arCode = IFIFTR or arCode = IFIFTC;

                   If TTType = 'B' or TTType = 'F';
                      Select;
                      When TTCtyp = 'P';
                         srBilledPrem += inAmount;
                      When TTCtyp = 'T';
                         srBilledTax  += inAmount;
                      Other;
                         srBilledFee  += inAmount;
                      Endsl;

                      Return *On;
                   Endif;

                Endif;

             Endif;

          Endif;
          Return *Off;
       Else;
          // Don't skip if Invoice number matches what was sent in.
          If inInvoice# = inArInvoice#;
             // Skip if A/R Code is marked as excluded from Invoice Cloud
             arCode = inARCode;
             Chain (inCompany#:arCode) ptt01;
             If %Found(wtabl) And TTexIC = 'Y';
                Return *On;
             Else;
                Return *Off;
             EndIf;
          Else;
             Return *On;
          EndIf;
       EndIf;

      /end-free

     PSkipICAR         e

** MN
January
February
March
April
May
June
July
August
September
October
November
December
** NOT
Your policy is currently pending cancellation effective
** WTL {Weight}                                                        09/20/06
3713713713713713713713713713713713713713713713                         09/20/06
** WT2 {Weight - WACHOVIA UPLOAD}                                      08/20/08
753275327532753275327532753275327532753                                08/20/08
** MD - NUMBER OF DAYS PER MONTH                                       08/20/08
312831303130313130313031                                               08/20/08
