     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt) bnddir('WTBNDDIR')
     H dftactgrp(*no) actgrp(*caller)
     h bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7221R - MONTHLY INTERIM PAYROLL REPORT
     H*****************************************************************
     H* REVISIONS:                                                    *
     h*
     h* 12/10/25 C:349 Support: 120177 (UBIC)
     H*                Compute the rate value based on 2 or 3 decimals*
     H*                based on the adoption effective date of 3 digits*
     H*                rates (RS3DATE) field value potentially being  *
     H*                greater than the policy term date.             *
     H*                                                               *
     H* 06/23/25 C:65  Project: 146WKQ  (Phw#119264)                  *
     H*                Use prorated normal in assessment factor calc  *
     H*                as assessment premium is prorated itself.      *
     H*                                                               *
     H* 05/15/25 C:300 Project: 146WKQ, Support: 118933               *
     H*                Assessments not calculating correctly.         *
     H*                                                               *
     H* 03/19/25 C:65  Project: 146WKQ  (Phw#118735)                  *
     H*                Correct S/R assessment percentage calcs by     *
     H*                defaulting payroll factor to 1 when there are  *
     H*                estimated payroll premiums.  Also do not       *
     H*                reduce regular percentage when no f-class      *
     H*                percentage and f-classes are not flagged to    *
     H*                be excluded.                                   *
     H*                                                               *
     H* 11/11/24 C:300 Project: WC2985                                *
     H*                Apply Deductible Discount to Discounted Premium*
     H*                                                               *
     H* 08/27/24 C:65  Support: 115371 (FMIC)                         *
     H*                Change EFT Portal worksheet to pull data from  *
     H*                WASRP due to Phw#115228 change.  This change   *
     H*                moved the printing of completed worksheet to   *
     H*                be last after WASRP has been moved.            *
     H*                                                               *
     H* 06/24/24 C:300 Project: 146WKR (Prescient)                    *
     H*                Collateral Processing                          *
     H*                                                               *
     H* 06/12/24 C:062 Project: 146WKQ                                *
     H*                Add assessment processing to print.            *
     H*                                                               *
     H* 04/18/24 C:291 Project: WC2961                                *
     H*                Adjust the "Factor before Modifier" by Schedule*
     H*                DB/CR factor when applied against Manual       *
     H*                Premium.                                       *
     H*                                                               *
     H* 02/06/24 C:65  Support: 114504 (FMIC)                         *
     H*                Add more coding to exp mod averaging to not    *
     H*                include the policy expiration date's day.      *
     H*                                                               *
     H* 11/16/23 C:65  Support: 114504 (FMIC)                         *
     H*                Correct exp mod averaging for reporting period *
     H*                by updating the ending date of the reporting   *
     H*                period to reflect the policy expiration date.  *
     H*                                                               *
     H* 07/30/23 C:62  Project: WC2819                                *
     H*                Check to see if exp mod 2 and 3 are not rated. *
     H*                                                               *
     H* 07/17/23 C:65  Project: 146WKN                                *
     H*                Skip policies on weekly S/R plan in mass runs. *
     H*                                                               *
     H* 05/31/23 C:65  Support: 110481 (UBIC)                         *
     H*                Populate the previous balance includes ...     *
     H*                text into scriptura work file and pull that    *
     H*                same info from Portal workfile for same.       *
     H*                                                               *
     H* 02/03/23 C:65  Support: 105592 (FMIC)                         *
     H*                Generate all 12 monthly reports into one       *
     H*                document instead of one document each.         *
     H*                                                               *
     H* 12/07/22 C:079 Support: 102/103265                            *
     H*                Condition spoolfile print on isUseSpoolOut     *
     H*                                                               *
     H* 11/22/22 C:65  Support: 104319 (FMIC)                         *
     H*                If printing payroll premium for completed form *
     H*                then do not divide by 100 for non per capita.  *
     H*                                                               *
     H* 10/20/22 C:300 Support: 103199 (UBIC)                         *
     H*                For (Net Rate and Non-net Rate) Monthly Self-  *
     H*                Reporting Worksheet run from Connect Portal -- *
     H*               -Remove Foreign Terrorism charge from Total     *
     H*                Premium Due and Add into Total Due             *
     H*                                                               *
     H* 03/28/22 C:65  Support: phw#101537 (96463) (FMIC)             *
     H*               -Do not calc prev billed if no reporting date.  *
     H*                                                               *
     H* 03/07/22 C:65  Project: Phw#101344 (UBIC)                     *
     H*                Do not print installment fee if running for    *
     H*                Portal/Connect.                                *
     H*                                                               *
     H* 02/07/22 C:33  Support: 100785                                *
     H*               -When running a single request and the period   *
     H*                 overlaps two years, turn on mass run          *
     H*               -Check for running over single year.            *
     H*                                                               *
     H* 01/19/22 C:33  Project: WC2837                                *
     H*                Extend net rate conditional                    *
     H*                                                               *
     H* 01/17/22 C:65  Project: Phw#94569 (FMIC)/Phw#95975 (MHCA)     *
     H*                Mass request for Scriptura is true for jobs    *
     H*                where all 12 forms are being requested.        *
     H*                                                               *
     H* 12/22/21 C:33  Support: 98878                                 *
     H*               -Adjust Scriptura increased limit amount        *
     H*                                                               *
     H* 06/29/21 C:136 Support: 96463                                 *
     H*               -Calculate previous billed amount and correct   *
     H*                logic error for 0 exp mod when printing        *
     H*                completed form from CompSuite.                 *
     H*                                                               *
     H* 05/03/21 C:65  Project: SY407/Phw#95772 (Inhouse)             *
     H*                Scriptura iDarts is improperly wiping out user *
     H*                selections when process is run.  Defaulting    *
     H*                for portal processes should happen within      *
     H*                programs calling worksheet and may need to be  *
     H*                removed as well if those programs are fixed.   *
     H*                                                               *
     H* 02/12/21 C:062 Project: 94251(136)                            *
     H*                Various changes.                               *
     H*                                                               *
     H* 11/10/20 C:136 Project: SY407 DE798                           *
     H*                Use WISRPREM workfile for balance calculation  *
     H*                when called from Portal instead of calling     *
     H*                SRPREM. Associated code added on 07/13/20      *
     H*                removed without comment.                       *
     H*                                                               *
     H* 09/25/20 C:136 Project: SY407 DE787                           *
     H*                Populate net rate premium figure to payroll    *
     H*                premium and payroll total premium figures when *
     H*                the group is flagged for net rate.             *
     H*                                                               *
     H* 07/13/20 C:136 Project: SY407                                 *
     H*               -Condition remittance section based on EFT,     *
     H*                3rd Party, etc. (Scriptura output only)        *
     H*               -Use SRPREM for balance calculation when        *
     H*                called from Portal.                            *
     H*               -isMassReq should be *On only if processing     *
     H*                in mass (QQemp#  = 0)                          *
     H*                                                               *
     H* 06/08/20 C:33  Project: WC2498                                *
     H*               -Add cancel date to workfile.                   *
     H*               -Exclude cancelled classes (already existed).   *
     H*                                                               *
     H* 05/15/20 C:62  Project: 102WIJ                                *
     H*                Don't print CR/DB Verbiage.                    *
     H*                                                               *
     H* 11/12/19 C:136 Project: SY407 (DE545)                         *
     H*                Populate worksheet from hold file WASRP_HLD    *
     H*                for 3rd Party processing                       *
     H*                                                               *
     H* 08/22/19 C:65  Project: SY407                                 *
     H*                Restructure to write main Scriptura workfile   *
     H*                after payroll records are written so there is  *
     H*                a manual premium total that can be used for    *
     H*                populated forms - and populate the rest of the *
     H*                premium amounts as well for balance section.   *
     H*                                                               *
     H* 08/21/19 C:65  Project: SY407                                 *
     H*                Correct payroll amount field for Scriptura.    *
     H*                                                               *
     H* 07/16/19 C:062 Project: SY407                                 *
     H*                Add completion flag.                           *
     H*                                                               *
     H* 04/30/19 C:062 Project: 152WAB                                *
     H*                Apply Optacomp Mods.                           *
     H*                                                               *
     H* 04/29/19 C:33  Project: SY407                                 *
     H*               -Store XML path for communication to 'Portal'   *
     H*               -Print Reported values                          *
     H*                                                               *
     H* 03/29/18 C:65  PROJECT: Phw#69912  (CareWest)                 *
     H*                Swap CareWest resolved output payroll lines    *
     H*                to use proper description field - update in    *
     H*                W7221O1.                                       *
     H*                                                               *
     H* 07/11/17 C:65  PROJECT: SY371                                 *
     H*                Remove the xml prep record if there are no     *
     H*                payrolls for the worksheet to prevent it       *
     H*                from printing in Scriptura.                    *
     H*                                                               *
     H* 06/26/17 C:136 Project: CI143                                 *
     H*               -Change process type for Scriptura to           *
     H*                'Self-Reporting Worksheet'                     *
     H*               -Populate payroll effective date                *
     H*               -Always run mass process                        *
     H*                                                               *
     H* 04/07/17 C:062 Project 136WHF                                 *
     H*                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 02/28/17 C:65  PROJECT: SY371                                 *
     H*                Update category on master record for mass      *
     H*                run to allow proper work file data program     *
     H*                to be called.                                  *
     H*                                                               *
     H* 01/03/17 C:136 Support: 44210                                 *
     H*                Suppress DARTS Remarks if called from billing  *
     H*                for FMIC only                                  *
     H*                                                               *
     H* 12/16/16 C:33  Project: SY354  44748                          *
     H*                Only print routing for QSYSPRT2 if records     *
     H*                 are listed.                                   *
     H*                                                               *
     H* 06/21/16 C:65  Project: SY371                                 *
     H*                Request XMLPrep record for Scriptura if needed *
     H*                and update into new work file.                 *
     H*                                                               *
     A* 11/23/15 C:300 SUPPORT: 33450                                 *
     A*                Top 2 lines are out of alignment in Monthly    *
     A*                Interm Payroll report (Self-Reporting Worksheet)
     A*                --Recompile only--                             *
     H*                                                               *
     F* 08/05/15 C:307 Project: SY354                                 *
     F*                -Adding 'Route To:'                            *
     F*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/14 C:290 REDMINE: 3754                                  *
     H*                Corrected the logic to Populate Stabilization  *
     H*                factor only once in the monthly interim report *
     H*                                                               *
     H* 10/07/13 C:190 Project: 25WSS                                 *
     H*                Change the program to determine if             *
     H*                Premium or Contribution should be printed      *
     H*                based on Group setup.                          *
     H*                                                               *
     H* 07/15/13 C:282 Project: WC2211                                *
     H*                To print the terrorism rates from the new      *
     H*                employer detail fields (ELTERR and ELDTRR)     *
     H*                if populated.                                  *
     H*                                                               *
     H* 07/03/13 C:190 PROJECT: 147WBR                                *
     H*                Include a page segment statement to print the  *
     H*                logo. Pull the logo from Group Auxiliary       *
     H*                to print on the worksheet.                     *
     H*                                                               *
     H* 04/05/11 C:189 PROJECT: WC1838/SEQ75792                       *
     H*                ADD LARGE DEDUCTIBLE PROCESSING                *
     H*                                                               *
     H* 03/16/11 C:190 PROJECT: 132WPI                                *
     H*                Print the new optional heading field for       *
     H*                the contractor's credit line item if it is     *
     H*                not blank(client 132 only).                   *
     H*                                                               *
     H* 01/24/11 C:65  PROJECT: 139WPL/PRISS 117                      *
     H*                IF CREDIT EXPIRES ON SAME DAY IT IS EFFECTIVE  *
     H*                THEN DO NOT INCLUDE IT IN CALCULATION.         *
     H*                                                               *
     H* 01/07/11 C:65  PROJECT: 114WVY/SUPP#75403                     *
     H*                CORRECT EXPIRATION DATE FOR 1ST PERIOD DEVS.   *
     H*                                                               *
     H* 12/23/10 C:246 SUPPORT: 75058
     H*                Fix the issue with Worksheet is not printing for
     H*                single employer.                               *
     H*                                                               *
     H* 11/10/10 C:65  PROJECT: 139WPL                                *
     H*                ADD NEW HCN CREDIT INTO DISCOUNT FACTOR BEFORE *
     H*                MODIFIER AND PRINT A LINE ITEM IN THE DISCOUNT *
     H*                BOX.                                           *
     H*                                                               *
     H* 10/28/10 C:183 Support: 74583                                 *
     H*                Changes for:                                   *
     H*                1) Do not display underline in payroll column  *
     H*                   if the class code is population rated,      *
     H*                   PRPCF = 'O'.                                *
     H*                2) Do not print/include the payroll records    *
     H*                   if EPCLAS =  RRDCCD (Aircraft surcharge     *
     H*                   code)                                       *
     H* ** Conflict resolution complete till this point. Projects/    *
     H*    Supports retrofitted are:                                  *
     H*    1) PRISS 29 133WKA.                                        *
     H*                                                               *
     H* 10/01/10 C:180 PRISS 29 133WKA                                *
     H*                PGM COMPILED ONLY - CHANGES IN PRTF W7221O1:   *
     H*                FIX PRINTING - CLASS CODE DETAILS OVERLAYING   *
     H*                PAGE HEADINGS.                                 *
     H*                                                               *
     H* 06/28/10 C:257 PROJECT: WC1912                                *
     H*                INSTALLMENT FEES ON S/R ACCOUNTS ->           **
     H*                CHECK THAT IF THE EXEMPT FLAG IS SET TO 'BLANK'*
     H*                AT THE EMPLOYER DETAIL LEVEL SET-UP            *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                ADD CHANGES IN PROGRAM FOR 2ND PERIOD DEVIATION*
     H*                WHEN DEVIATIONS ARE SPLIT. 2ND PERIOD DEVIATIO-*
     H*                NS CHANGES WILL APPLY FOR (M,J,E,S,D) PREMIUM  *
     H*                LEVELS AS IT IS FOR 1ST PERIOD DEVIATIONS.     *
     H*                                                               *
     H* 02/25/10 C:65  PROJECT: 25WRC                                 *
     H*                CORRECT PRINTING OF THE STABIZATION FACTOR     *
     H*                AND ALTER PRINTING OF DISCOUNT FACTORS TO      *
     H*                HAVE 6 DECIMAL PLACES.                         *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 01/14/10 C:190 PROJECT: 25WRC                                 *
     H*                BILLING ALOGRITHM RETROFIT                     *
     H*                - For CLT025,  use stabilization factor to cal *
     H*                  the amount of change for dev#2               *
     H*                                                               *
     H* 02/16/10 C:232 SUPPORT: WA146 71363                           *
     H*                Fixed the issue of class code description.     *
     H*                (Retrofitted the changes of support 69655 from *
     H*                V0R1QA).                                       *
     H*                                                               *
     H* 09/02/09 C:220 SUPPORT: 68960                                 *
     H*                EXCLUDE CANCELLED POLICES FROM S/R REPORT      *
     H*                                                               *
     H* 08/28/09 C:220 SUPPORT: 69655                                 *
     H*                -FIX NEEDED FOR CLASS CODE DESC                *
     H*                -FIX NEEDED FOR NET RATE CALCULATION           *
     H*                -FIX NEEDED FOR EXPERIENCE MODIFIER            *
     H*                                                               *
     H* 08/19/09 C:136 SUPPORT: WA139 69537                           *
     H*                FIX NEEDED FOR *ENTRY PARMS.                   *
     H*                USE Q1PRMS TO REFERENCE QQALL FOR BACKWARDS    *
     H*                COMPATIBILITY REASONS.                         *
     H*                                                               *
     H* 07/22/09 C:136 PROJECT: 136WED                                *
     H*               -CLIENT SPECIFIC CHANGES FOR AN EXPANDED        *
     H*                DESCRIPTION AND EXTENDED EXPANDED DESCRIPTION  *
     H*               -CLIENT SPECIFIC CHANGES FOR THE INSTRUCTIONS   *
     H*                                                               *
     H* 06/26/09 C:198 PROJECT: WC1694                                *
     H*               -Included policies that have a vendor history   *
     H*                record and vpmthi = 'Y'.                       *
     H*               -Correction so that mass run works when         *
     H*                QQEMP# = *zeros                                *
     H*                                                               *
     H* 06/01/09 C:136 PROJECT: 139WLP                                *
     H*                DISPLAY POLICY PERIOD IN HEADER                *
     H*                                                               *
     H* 05/05/09 C:180 SUPPORT 67225 - Create PDFs not just for Mass
     H*                run, but also for individual employer run.
     H*
     H* 04/20/09 C:190 PROJECT: WC1473                                *
     H*                Use the new logical(s) to get the description  *
     H*                by extension.                                  *
     H*                                                               *
     H* 04/10/09 C:61  SUPPORT 67899                                  *
     H*                Print division on form.                        *
     H*                                                               *
     H* 03/03/09 C:220 SUPPORT: 67408                                 *
     H*                LCTA DOSN'T WANT DEDUCTIBLE CREDIT ON S/R      *
     H*                REPORT                                         *
     H*                                                               *
     H* 03/02/09 C:65  PROJECT: 81WEI                                 *
     H*                Use optional headings for terrorism charges.   *
     H*                                                               *
     H* 02/25/09 C:61  Project  133WGE                                *
     H*                Format so address positioned in window envelop *
     H*                for clt133.                                    *
     H*                                                               *
     H* 02/09/09 C:33  Project: 136WDQ                                *
     H*                Add agent phone #.                             *
     H*                                                               *
     H* 01/20/09 C:61  SUPPORT WA093 66720 WC1619
     H*                USE REPORTING DATE AS INPUT TO SREFFD CALLS.   *
     H*                                                               *
     H* 01/19/09 C:220 SUPPORT: 66704                                 *
     H*                CAREWEST DOES NOT WANT TO SEE BOX 10 DISPLAY   *
     H*                ON THE S/R.                                    *
     H*                                                               *
     H* 01/16/09 C:136 SUPPORT: WA146 66464                           *
     H*                NOT needed after V8R1                          *
     H*                                                               *
     H* 12/24/08 C:65  PROJECT: SUPPORT/WC1619                        *
     H*                CORRECT CHAIN TO WMEPP FOR SREFFD TO BE A SETGT*
     H*                AND REDPE IN ORDER TO GRAB CORRECT PAYROLL CLAS*
     H*                                                               *
     H* 12/19/08 C:33  PROJECT: 102WEP  WA102/66497                   *
     H*                DON'T PRINT 'PREVIOUS BALANCE' AMOUNT FOR      *
     H*                 CLT102 ON ANY WORKSEET.                       *
     H*                                                               *
     H* 12/17/08 C:136 SUPPORT: WC1627                                *
     H*                USE EPENTR TO POPULATE THE NET RATE FACTOR FOR *
     H*                CAREWEST ONLY PER GW                           *
     H*                                                               *
     H* 12/12/08 C:33  Project: 102WEP  Priss# 18                     *
     H*               -Process lrg deductible correctly. (93WGW/93WEP)*
     H*               -Don't print 'Previous Balance'.                *
     H*                                                               *
     H* 11/25/08 C:65  PROJECT: 93WGW/SUPP#66184                      *
     H*                APPLY FIX NOTED BELOW FOR A RATED POLICIES TO  *
     H*                FIRST SECTION FOR TERRORISM RATES.             *
     H*                                                               *
     H* 11/05/08 C:136 PROJECT: 146WBM                                *
     H*                ADD S/R INSTALLMENT FEE                        *
     H*                                                               *
     H* 10/31/08 C:189 PROJECT 139WJG                                 *
     H*                Add Payroll company to MSR Worksheet           *
     H*                                                               *
     H* 10/29/08 C:61  PROJECT 93WGW (ADDITIONAL CHANGES) PRISS 34    *
     H*              - Use correct effective date when retrieving     *
     H*                reporting rating record for 'A' rated policy.  *
     H*                Necessary to print Terror rates.               *
     H*              - Use correct effective date when retrieving
     H*                payroll class description.                     *
     H*                                                               *
     H* 10/17/08 C:65  PROJECT: WC1644/SUPPORT                        *
     H*                APPLY LCM AGAINST DOMESTIC TERRORISM RATE.     *
     H*                                                               *
     H* 09/23/08 C:136 SUPPORT: WA102 65290                           *
     H*                MAKE WC1627(NET RATE) BACWARDS COMPATIBLE FOR  *
     H*                CLIENTS THAT HAVE MODIFIED CODE AND HAVE NOT   *
     H*                IMPLEMENTED WC1627                             *
     H*                                                               *
     H* 09/15/08 C:189 PROJECT: WC1644                                *
     H*                Add an Option to exclude terrorism from LCM    *
     H*                                                               *
     H* 08/22/08 C:191 PROJECT: 102WEP                                *
     H*                BACK TO BASE : BASE OPTION                     *
     H*                 - MO INT RPT-ALLOW GEN OF ALL 12 FORMS        *
     H*                                                               *
     H* 08/21/08 C:136 SUPPORT: WA133 64891                           *
     H*                ANOTHER CORRECTION TO 45WFO SO THAT A          *
     H*                SECOND PAGE DOESN'T PRINT INCORRECTLY.         *
     H*                                                               *
     F* 08/04/08 C:136 SUPPORT: WA045 64622
     F*               -CORRECTIONS TO 45WFO. AN EXTRA PAGE WAS BEING PRINTED
     F*                INCORRECTLY IF THERE WERE MORE THAN 9 PAYROLLS ON THE
     F*                POLICY. MOVED 'CALCULATE TOTAL . . .' VERBIAGE DOWN
     F*                SEVERAL LINES.
     F*               -ALSO, CORRECTION TO WRITELINES1 CONDITIONING SO
     F*                THAT LINES WILL WRITE ON ADDITIONAL PAGES OF THE M/I
     F*
     H* 06/06/08 C:65  PROJECT: CI019                                 *
     H*                ADD CHAIN TO BILLING SCHD CODE FILE FOR ONLINE.*
     H*                                                               *
     H* 06/06/08 C:180 CI019 Create PDF in test folder when running
     H*                      in test env.
     H*                                                               *
     H* 04/22/08 C:136 SUPPORT: WA045 63047                           *
     H*                CORRECTION TO PRIOR DEPOSIT ROUTINE TO ACCOUNT *
     H*                FOR ALL YEAR BALANCE FLAG IN WMEXP AND TO FIX  *
     H*                ROUNDING ISSUE                                 *
     H*                                                               *
     H* 04/06/08 C:61  PROJECT 93WGW (ADDITIONAL CHANGES)             *
     H*              - Add second print of Terror Rate. clt093 only.  *
     H*              - Add Policy Service Rep name and extension to   *
     H*                "billing inquiries" output line.  clt093 only. *
     H*              - Fix print of "Increased Limits".               *
     H*              - Include "State Tax" in EZ Premium Factor.      *
     H*                                                               *
     H* 04/05/08 C:61  SUPPORT: WA136 62061                           *
     H*                Changed so clt093 can print payrolls.          *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 03/28/08 C:65  PROJECT: CI019                                 *
     H*                DO NOT PRINT A MONTHLY INTERIM IN MASS IF      *
     H*                ONLINE PROCESSING AND NO VALID ACH INFO.       *
     H*                                                               *
     H* 02/20/08 C:33  Seq#: WA099/62348                              *
     H*                Only include depoists posted after the balance *
     H*                 date entered on the prompt.                   *
     H*                                                               *
     H* 01/30/08 C:136 SUPPORT: WA136 62068                           *
     H*                CORRECTED ROUTINE THAT CALCULATES # OF DAYS    *
     H*                IN NET RATE ALGORITHM.                         *
     H*                                                               *
     H* 01/28/08 C:136 SUPPORT: WA136 62061                           *
     H*                CORRECTED ROUTINE THAT PULLS IN RATES.  THE    *
     H*                STATE RPT DIVISION'S RATING DATES WERE NOT     *
     H*                BEING USED TO PULL IN RATES FOR THE NON-       *
     H*                STATE REPORTING DIVISIONS.                     *
     H*                                                               *
     H* 01/25/08 C:136 SUPPORT: WA136 61789                           *
     H*                FIX EXP MOD CALC IN NET RATE WHEN MOD DATE     *
     H*                NE ZERO AND MOD = 0                            *
     H*                                                               *
     H* 01/22/08  C:65  PROJECT: WC1627                               *
     H*                 PULL THE NET RATE FOR EACH PAYROLL CLASS      *
     F*                 FROM THE NEW NET FIELDS.                      *
     H*                                                               *
     H* 01/10/08  C:180 PROJECT: CI019                                *
     H*                 Write PDF documents to IFS folder for
     H*                 client 133 during mass run only.
     H*
     H* 11/30/07 C:136 SUPPORT: WA136 61055                           *
     H*                SHOW PREVIOUS BALANCE WHEN RUN FOR THE FIRST   *
     H*                MONTH OF THE POLICY IF RUN WITHIN THE POLICY   *
     H*                DATE RANGE. THIS REVERSES 91WBT.               *
     H*                                                               *
     H* 11/29/07  C:180 PROJECT WC1491
     H*               -ADD PROCESSING FOR DEPOSIT REFUND AND VOID/    *
     H*                STOP PAYS.                                     *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
     H*                                                               *
     H* 12/04/07  C:65  PROJECT: 93WGW                                *
     H*                 CORRECT CALCULATION OF EZ PREMIUM FACTOR.     *
     H*                                                               *
     H* 11/05/07  C:193 PROJECT: 93WGW                                *
     H*                 SELF REPORTING FORM CLIENT 093.               *
     H*                 PROJECT: 93WEP                                *
     H*                 ADD PROCESSING FOR LARGE DEDUCTIBLE FLAG.     *
     H*                                                               *
     H* 10/30/07 C:136 SUPPORT: WA045 60774                           *
     H*                MOVE ADDRESS OVER 3/4 INCH FOR LCTA            *
     H*                                                               *
     H* 10/02/07  C:65  Project SUPP#60111                            *
     H*                 PRINT WAIVER MESSAGE UNDER ONE FIELD TO       *
     H*                 PREVENT FONT OVERLAP ISSUES.                  *
     H*                                                               *
     H* 09/13/07  C:65  Project SUPP#60111                            *
     H*                 CORRECT WAIVER MESSAGE FIELDS COPIED FROM V6. *
     H*                                                               *
     H* 07/30/07  C:65  PROJECT 71WGH (REMOVE SUPP#43064)             *
     H*                 REMOVE SUPPORT CHANGE FOR CLIENT 71 TO APPLY  *
     H*                 DEVIATION #1 AND #2 TO SAME PREMIUM FIGURE    *
     H*                 WHEN APPLIED TO MODIFIED PREMIUM.             *
     H*                                                               *
     H* 07/16/07 C:183 PROJECT: 45WFO                                 *
     H*                MOVE VALUE IN XXDBCR, SCHEDULED DB/CR FACTOR   *
     H*                FOR CLIENT 45.                                 *
     H*                                                               *
     H* 05/09/07 C:136 SUPPORT: WA081 55717                           *
     H*                CORRECTION TO 55717 ON 11/09/06. NEED TO RESET *
     H*                XXRPDT AT BEGINNING OF WORK LOOP ON PEB01      *
     H*                IN CASE IT WAS CHANGED TO EQUAL ELPTDT         *
     H*                                                               *
     H* 04/25/07  C:33  Seq#: WA099/57651                             *
     H*                 Add entry lines when the Premium is not       *
     H*                  printed.                                     *
     H*                                                               *
     H* 04/19/07 C:190 Project 136WCM
     H*                 ADD NEW FIELD 'Exclude terrorism charge on -
     H*                 monthly S/R worksheet.
     H*                                                               *
     H* 04/03/07 C: 58 PROJECT: 132WHR Add SCINDR - Include deposit   *
     H*                parameter for S/R worksheets; if = Y include   *
     H*                                                               *
     H* 11/21/06 C:65  PROJECT SUPP#55923                             *
     H*                MOVE PRINT DATE TO TOP OF FORM AND PREVIOUS    *
     H*                BALANCE INCLUDES WAIVER TO PRINT DATE'S PRIOR  *
     H*                LOCATION, TO MAKE ROOM FOR SIGNATURE LINE.     *
     H*                                                               *
     H* 11/09/06 C:65  PROJECT: SUPP#55717                            *
     H*                SET REPORT DATE EQUAL TO POLICY EXPIRATION     *
     H*                DATE WHEN POLICY EXPIRES MID-MONTH.            *
     H*                                                               *
     H* 08/18/06 C:33  Seq#: WA109/54079                              *
     H*               -Alignment issues.                              *
     H*                                                               *
     H* 07/26/06 C:136 PROJECT WC1324                                 *
     H*               -IMPROVE ACCURACY OF NET RATE CALCULATION BY    *
     H*                TWEAKING THE EXP MOD WEIGHTED AVERAGE CALC AND *
     H*                BY PROPERLY ACCOUNTING FOR A FLAT OR SPECIFIC  *
     H*                WAIVER CHARGE                                  *
     H*               -IMPROVE ACCURACY OF STANDARD CALCULATION BY    *
     H*                PROPERLY ACCOUNTING FOR A FLAT OR SPECIFIC     *
     H*                WAIVER CHARGE                                  *
     H*                                                               *
     H* 07/20/06 C:33  Project: 109WDZ   WA109/50263                  *
     H*                Condition calls to WA114R.                     *
     H*                                                               *
     H* 07/10/06 C:65  PROJECT 132WEW
     H*                CALCULATE THE FACULTATIVE REINSURANCE CHARGE IN
     H*                THE DISCOUNT AFTER MODIFIER CALCULATION        *
     H*                IF THE CHARGE IS A FLAT AMOUNT, ADD IN 1/12TH  *
     H*                OF THE CHARGE IN THE SAME MANNER AS WAIVER.    *
     H*                CHANGED THE PREVIOUS BALANCE MESSAGE TO        *
     H*                ONE FIELD FOR CONCATENATIONS.                  *
     H*                                                               *
     H* 06/06/06 C:136 PROJECT: WC1394 (RESOLVE TO DARTS)             *
     H*                - ADD RESOLVE TO DARTS PARM                    *
     H*                - CHG OUTPUT TO PRINTER FILES                  *
     H*                - ADD CODE TO RESOLVE DOC TO DARTS             *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 01/27/06 C:136 SUPPORT: WA136 51046                           *
     H*                CORRECT NET RATE FACTOR FOR SPLIT EXP MODS     *
     H*                                                               *
     H* 01/20/06 C:33  Project: 109WDZ   WA109/50263                  *
     H*                Chain to billing schedule file.                *
     H*                                                               *
     H* 01/16/06 C:33  Project: 109WDZ   WA109/50263                  *
     H*                Position employer address for envelope window. *
     H*                                                               *
     H* 12/18/05 C:136 PROJECT: 136WBH                                *
     H*                CLIENT SPECIFIC MOD FOR DEBIT/CREDIT FACTOR    *
     H*                                                               *
     H* 12/12/05 C:33  Seq#: WA109/50263                              *
     H*                Load agent at proper point.                    *
     H*                                                               *
     H* 08/10/05 C:136 PROJECT: 129WKQ                                *
     H*                ADD LOSS COST MULTIPLIER                       *
     H*                                                               *
     H* 06/24/05 C:33  Project: 109WDZ                                *
     H*                {109AM}                                        *
     H*               -Use API to determine if monthly interim        *
     H*                 produced for S/R employer.                    *
     H*               -Use API to determine if deposit transfer to    *
     H*                 premium for S/R employer.                     *
     H*                {109AX}                                        *
     H*               -Use unique transfer codes for S/R deposit. Add *
     H*                 correction to only transfer deposit activity  *
     H*                 in the current/prior years.                   *
     H*                {109BC}                                        *
     H*               -Call API WA114R to determine if month should   *
     H*                 be included based on S/R frequency - monthly, *
     H*                 quarterly or semi-annually.  Write a DARTS    *
     H*                 remark if the 'Monthly Interim' remark code in*
     H*                 ADMIN/DARTS file is not blank.                *
     H*                {109CH}                                        *
     H*               -Write S/R records for any new/renewed accounts *
     H*                 that transfer/renew late.                     *
     H*                {109CR}                                        *
     H*               -Generate additional offsets for Deposit Premium*
     H*                 and Tax on Deposit Premium.                   *
     H*                {109CX}                                        *
     H*            ? - Print one less payroll per page.               *
     H*              - Add signature line to last page.               *
     H*                {109CX}                                        *
     H*               -CA flat rate increase. 0.2% for all 2002       *
     H*                 policies (anniv or eff dt) and a 4.9% increase*
     H*                 for the unexpired portion as of 1/1/2003.     *
     H*                                                               *
     H* 04/07/05 C:156 PROJECT: 126WDK                                *
     H*                PROJECT: 126WCD                                *
     H*                Multiple Deductible discounts per Year.        *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                PRINT EXTENDED EMPLOYER NAME FIELDS.           *
     H*                                                               *
     H* 03/07/05 C:136 PROJECT: WC1199 (REVAMP DUNNING II)            *
     H*                ADD CODE TO SKIP POLICIES THAT HAVEN'T HAD     *
     H*                A CALCULATE, UPDATE, AND PRINT BILL RUN BY     *
     H*                CHECKING PREMIUM HISTORY. PRINT THE INFO       *
     H*                FOR THESE SKIPPED POLICIES IN AN EXCEPTION     *
     H*                REPORT.                                        *
     H*                                                               *
     H* 02/15/05 C:65  PROJECT: WC1167                                *
     H*                OVERRIDE THE PRINTER FILE TO OPEN AND CLOSE    *
     H*                FOR THE NET RATE.  THIS IS NECESSARY SINCE     *
     H*                EACH EMPLOYER COULD HAVE ONE OF TWO OVERLAYS.  *
     H*                QSYSPRT WILL CONTAIN THE EMPLOYERS WITHOUT     *
     H*                NET RATE WHILE QSYSPRT1 WILL CONTAIN THE NET   *
     H*                RATE EMPLOYERS.                                *
     H*                                                               *
     H* 01/07/05 C:136 PROJECT: WC1250                                *
     H*                ADD DOMESTIC TERRORISM RATE TO TERRORISM RATE  *
     H*                                                               *
     H* 11/23/04 C:65 PROJECT: SUPPORT#43064                          *
     H*               APPLY DEVIATION #2 AFTER DEV #1 ON MODIFIED     *
     H*               PREMIUM INSTEAD OF TO THE SAME FIGURE.          *
     H*                                                               *
     H* 09/24/04 C:53  PROJECT 132WBD                                 *
     H*                ADD AGENT HEADER VARIABLE FIELD                *
     H*                                                               *
     H* 02/19/04 C:136 PROJECT: WC1033                                *
     H*                Add code for new A/R type of 'T' (True Void)   *
     H*                                                               *
     H* 02/06/04 C:65 PROJECT: SUPPORT#39886/WC1120                   *
     H*               DO NOT USE THE ARD WHEN PULLING TERRORISM RATE  *
     H*               FOR NJ.  USE THE POLICY EFFECTIVE DATE ONLY.    *
     H*                                                               *
     H* 01/09/04 C:136 PROJECT: WC1115                                *
     H*                Skip employers that have an active vendor      *
     H*                history record in mass run.                    *
     H*                                                               *
     H*  12/18/03 C:132 Project: WA0129 SEQ: 40427                    *
     H*                 Change to back out load of LOCK BOX address   *
     H*                 Commented out the Flag used to indicate that  *
     H*                 LOCK BOX address to be used, can reinstall by *
     H*                 uncommenting this code                        *
     H*                                                               *
     H* 10/22/03 C:61  SUPPORT WA046 39626                            *
     H*                Only process active status employer payroll    *
     H*                records.                                       *
     H*                                                               *
     H* 10/10/03 C:65  PROJECT: SUPP/WC1123/WC1149                    *
     H*                ENSURE RATING DATE ARRAYS ARE NOT FILLED WITH  *
     H*                RATE SPLIT DATES WHEN THE SPLIT IS NOT VALID.  *
     H*                                                               *
     H* 10/07/03 C:65  PROJECT: WC1149                                *
     H*                IF AN FLORIDA POLICY HAS A PERIOD RATED BEFORE *
     H*                10/1/3 AND EXPIRES AFTER 10/1/3 THEN PULL      *
     H*                RATES AS OF 10/1/3 FOR OCTOBER AND FORWARD.    *
     H*                                                               *
     H* 09/18/03 C:136 Support: 39055 (WC1115)                        *
     H*                Skip employer if vendor number is assigned in  *
     H*                mass run only.                                 *
     H*                                                               *
     H* 04/30/03 C:65  PROJECT: WC1123                                *
     H*                IF AN FLORIDA POLICY HAS A PERIOD RATED 1/1/3  *
     H*                TO 3/31/3, THEN PULL 4/1/3 RATES FOR ANY       *
     H*                MONTHS AFTER MARCH.                            *
     H*                                                               *
     H* 02/18/03 C:136 Project: WC1115                                *
     H*                Skip employer if vendor number is assigned in  *
     H*                WDELP.                                         *
     H*                                                               *
     H*  02/13/03 C:132 Project: WC1117                               *
     H*                 Added LOCK BOX address check in group         *
     H*                 auxiliary file. If present in WMF2P, use the  *
     H*                 Lock box address instead of accounting office *
     H*                 address.                                      *
     H*                                                               *
     H* 01/29/03 C:65  PROJECT: WC744                                 *
     H*                ADD THE ARAP CHARGE TO THE DISCOUNT AFTER      *
     H*                MODIFIER FIGURE.  THE ARAP CHARGE IS CALCULATED*
     H*                WITHOUT ANY OF THE OTHER DISCOUNTS INCLUDED.   *
     H*                                                               *
     H* 01/17/03 C:65  PROJECT: WC1120                                *
     H*                ADD A NEW BOX TO THE INTERIM FOR TERRORISM     *
     H*                RISK ACT BETWEEN TOTAL PREMIUM DUE AND THE     *
     H*                STATE TAX BOX.  ADJUSTED OUTPUT SPECS FOR IT   *
     H*                AND ADDED MORE INSTRUCTIONS TO OVERLAY.        *
     H*                REDUCE PRINTED PAYROLL LIMIT BY 1 FOR ROOM.    *
     H*                                                               *
     H* 01/13/03 C:65  PROJECT: WC1104                                *
     H*                ALLOW THE DEVIATIONS TO BE APPLIED TO MANUAL.  *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT WC1099                                  *
     H*               IF A PAYROLL CLASS CODE HAS AN EXTENSION WITH   *
     H*               A STAT CODE AND AN ADDITIONAL LOAD AMOUNT THEN  *
     H*               PRINT THE SUBHEADING FOR TERRITORY DIFF.        *
     H*                                                               *
     H* 04/25/02  C:65  PROJECT CM045                                 *
     H*                 IMPLEMENT CHANGES TO INCLUDE WAIVER SURCHARGE *
     H*                 ON THE MONTHLY INTERIM.  IF THE WAIVER IS A   *
     H*                 BLANKET PERCENTAGE THEN IT CAN BE ADDED INTO  *
     H*                 THE BEFORE OR AFTER MODIFIER.  IF IT IS A     *
     H*                 FLAT AMOUNT OR SPECIFIC TO EACH WAIVER THEN   *
     H*                 1/12 OF THE ANNUAL AMOUNT WILL BE ADDED INTO  *
     H*                 THE PREVIOUS BALANCE AMOUNT WITH A FOOTNOTE   *
     H*                 ADDED AT THE BOTTOM TO INDICATE SO.           *
     H*                                                               *
     H* 10/11/01  C:65  SUPPORT# 28939                                *
     H*                 ANNIVERSARY RATING DATE SHOULD BE USED AS     *
     H*                 LONG AS IT FALLS BEFORE THE POLICY EFF DATE,  *
     H*                 AND THE RATING DATE IN EFFECT DOES NOT. ALSO  *
     H*                 IF THE RATING DATE IN EFFECT IS EQUAL TO THE  *
     H*                 POLICY EXPIRATION DATE, DO NOT USE THE IT.    *
     H*                                                               *
     H* 09/24/01  C:65  PROJECT WC1041                                *
     H*                 EXPAND INCR LIMITS PERCENT FROM 3.2 TO 5.3    *
     H*                                                               *
     H* 09/24/01  C:65  PROJECT WC1035                                *
     H*                 EXPAND PAYROLL RATE FIELD BY 2 DIGITS.        *
     H*                                                               *
     H* 04/21/01 C:65  PROJECT: SUPPORT#27203                         *
     H*                DEVIATIONS #1 AND #2 WERE NOT BEING APPLIED TO *
     H*                THE SAME PREMIUM WHEN CALCULATING THE FACTOR.  *
     H*                                                               *
     H* 03/14/01 C:65  PROJECT WC1027                                 *
     H*                IF A CLASS CODE IS POPULATION RATED, SET ITS   *
     H*                PREMIUM TO 1/12 OF THE PREMIUM ON THE CLASS.   *
     H*                                                               *
     H* 03/02/01 C:09  PROJECT: WC577 / WA112 / SEQ# 26563            *
     H*                Only use Anniversary Rating Date when          *
     H*                appropriate.                                   *
     H*                                                               *
     H* 02/21/01 C:65  PROJECT: WC1026                                *
     H*                PRINT MERIT RATING FOR OUTPUT IF APPLICABLE.   *
     H*                                                               *
     H* 10/04/00 C:90  PROJECT: WC577                                 *
     H*                Write a DARTS remark if the 'Monthly Interim'  *
     H*                remark code in ADMIN/DARTS file is not blank.  *
     H*                                                               *
     H* 02/09/00 C:65  PROJECT: SUPPORT#21783                         *
     H*                CHANGE THE SIZE OF DATE FIELDS TO ALLOW PROPER *
     H*                COMPARISIONS.                                  *
     H*                                                               *
     H* 10/12/99 C:87  PROJECT: WC705                                 *
     H*                Add carrier policy # to header info.           *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 09/17/99  C:118  SUPPORT: WA090/19678                         *
     H*                  MAKE CAPITALIZATION AND PLURAL CORRECTION.   *
     H*                                                               *
     H* 09/03/99  C:35   PROJECT: SUPPORT WA088 #19531                *
     H*                  REVERSED PREVIOUS CHANGE TO DATE CONVERSION  *
     H*                  CALL.                                        *
     H*                                                               *
     H* 08/12/99  C:65  PROJECT WC935                                 *
     H*                 ALLOW FOR 1ST EXP MOD TO BE ZERO, IN ORDER    *
     H*                 TO SKIP IT.                                   *
     H*                                                               *
     H* 06/30/99  C:35   PROJECT: SUPPORT WA102 #18672                *
     H*                  USE MONTHEND DATE TO DO THE SETGT ON THE     *
     H*                  EMPLOYER PAYROLL FILE, SO THAT PGM WILL PICK *
     H*                  UP PAYROLLS ADDED MID-TERM.                  *
     H*                                                               *
     H* 03/12/99 C:65 PROJECT WC705                                   *
     H*               CHANGE THE PRINTING OF TAX FOR THE NEW FIELD.   *
     H*                                                               *
     H* 01/29/99 C:65 PROJECT CLT096/16323                            *
     H*               WHEN STOCK DISCOUNT IS APPLIED TO MOD MANUAL,   *
     H*               USE THE PERCENTAGE USED IN THE FACTOR CALC      *
     H*               INSTEAD OF THE TRUE PERCENTAGE.                 *
     H*                                                               *
     H* 01/28/99 C:65 PROJECT SUPPORT                                 *
     H*               CHANGE CONVERSION OF RPMY FOR DATE REASONS.     *
     H*                                                               *
     H* 12/22/98 C:65 PROJECT SUPPORT                                 *
     H*               USE CALCULATED DISCOUNT TO DISPLAY.             *
     H*                                                               *
     H* 12/10/98 C:61 SUPPORT 14595                                   *
     H*               Only process active (WMA3P/A3STAT = 'A')        *
     H*               Employer 3rd Address records.                   *
     H*                                                               *
     H* 12/09/98  C:65   PROJECT: SUPPORT                             *
     H*                  CORRECTION TO 11/13/98 SUPPORT CHANGE.       *
     H*                                                               *
     H* 11/19/98  C:35   PROJECT: SUPPORT                             *
     H*                  ADD FLAG TO CONTROL PRINTING OF MISSING      *
     H*                  MONTHS.                                      *
     H*                                                               *
     H* 11/13/98  C:65   PROJECT: SUPPORT                             *
     H*                  DO NOT RUN A REPORT IF THE CANCEL DATE       *
     H*                  EQUALS THE POLICY EFFECTIVE DATE AND         *
     H*                  USE THE POLICY EXP DATE IF IT IS LESS THAN   *
     H*                  THE CANCEL DATE.                             *
     H*                                                               *
     H* 10/27/98  C:35   PROJECT: SUPPORT WA081 #14666                *
     H*                  DO NOT USE ALTERNATE BILLING ADDRESS IF IT   *
     H*                  IS BLANK.                                    *
     H*                                                               *
     H*  08/11/98  C:65  PROJECT: WC843                               *
     H*                  DISPLAY EXTENSION DESCRIPTION IF NOT BLANK.  *
     H*                                                               *
     H*  07/28/98 C:65 PROJECT: CM039                                 *
     H*                USE THE ACCTING OFFICE FIELD FOR DIRECT INQRY. *
     H*                                                               *
     H* 07/27/98  C:65  PROJECT: SUPPORT                              *
     H*                 CHANGED CHECK FOR IF DISCOUNT IS EFFECTIVE.   *
     H*                                                               *
     H* 07/16/98 C:108  PROJECT: WC577                                *
     H*                 PAYMENT DUE DATE LABEL AND DATE VALUE SHOULD  *
     H*                 NOT PRINT IF DATE VALUE IS ZERO.              *
     H*                                                               *
     H* 07/13/98  C:65  PROJECT: WC846                                *
     H*                 CHECK IF DISCOUNT IS APPLIED TO MOD MANUAL.   *
     H*                                                               *
     H* 06/16/98  C:90  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H* 06/10/98  C:90  PROJECT: 91WBT                                *
     H*                 SUPPRESS PRINT OF 'PREVIOUS BALANCE' IF       *
     H*                 INTERIM IS BEING GENERATED FOR THE 1ST MONTH  *
     H*                 OF POLICY YEAR.                               *
     H*                                                               *
     H* 06/01/98  C:65  PROJECT: CM039                                *
     H*                 USE ACCOUNTING INFO FOR MAKE CHECK PAYABLE.   *
     H*                                                               *
     H* 06/01/98  C:65  PROJECT: WC705                                *
     H*                 USE OPTIONAL HEADING FOR STATE TAX.           *
     H*                                                               *
     H* 03/19/98  C:65  PROJECT: WC705                                *
     H*                 SHOW STATE TAX BOX ONLY IF THERE IS A TAX.    *
     H*                                                               *
     H* 02/09/98  C:65  PROJECT: WC705                                *
     H*                 ADD OPTIONAL HEADING FOR STOCK DISCOUNT.      *
     H*                                                               *
     H* 01/28/97  C:65  PROJECT: WC709                                *
     H*                 ADD RATING STATE TO THE TOP OF THE INTERIM.   *
     H*                                                               *
     H* 12/22/97  C:65  PROJECT: WC709                                *
     H*                 MOVE PAYMENT DATE OVER TO THE LEFT SOME.      *
     H*                                                               *
     H* 12/15/97  C:65  PROJECT: WC709                                *
     H*                 ADDED EMPLOYER RANGE PARMS.                   *
     H*                                                               *
     H* 10/27/97  C:65  PROJECT: WC709                                *
     H*                 REMOVED DIFFERENT PRINT FOR POLICY NUMBER.    *
     H*                                                               *
     H*****************************************************************
     H*  INDICATOR USAGE:              (ADDED 07/16/98)               *
     H*                                                               *
     H*  14 - PAYMENT DUE DATE VALUE IS NON-ZERO.                     *
     H*  28 -                                                         *
     H*  58 -                                                         *
     H*  60 -                                                         *
     H*  63 -                                                         *
     H*                                                               *
     H*****************************************************************
     F*
     FSMORL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWDELL     IF   E           K DISK
     F                                     infds(dbfds)
     FWMUPL     IF   E           K DISK
      *
      * System User
     FSMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP02)
     FWTLML1    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEPL7E   IF   E           K DISK
     FWMPXL1    IF   E           K DISK
     FWMPXPE    IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWMPRPE    IF   E           K DISK
     FWXEBL     IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWMEXL     IF   E           K DISK
      * PREMIUM HISTORY
     FWDEHL     IF   E           K DISK
      * PREMIUM HISTORY
     FWDMPL     IF   E           K DISK
     FWDF2L     IF   E           K DISK    RENAME(PF201:PF201)
     FW7221O    O    E             PRINTER OFLIND(*IN01)
     FW7221O1   O    E             PRINTER OFLIND(*IN02)
     F                                     USROPN
     FQSYSPRT2  O    F  105        PRINTER OFLIND(*INOB)
     F                                     usropn
     F*
     F* Employer Payroll File                        Added LJQ 8-18-88
     FWASRL2    IF A E           K DISK
     FWASRL4E   IF   E           K DISK    rename(psr01:psr04e)
     F*
     F* Employer A/R File                            Added LJQ 8-18-88
     FWMEAL8    IF   E           K DISK
     FWMEAL4    IF   E           K DISK
     F                                     RENAME(PEA01:PEA04)
     FWMEAP     O    E             DISK
     F                                     RENAME(PEA01:PEA0P)
     FWAQTL7    IF   E           K DISK
     F                                     RENAME(PQT01:PQT07)
     FWMEPL     IF   E           K DISK
     F                                     RENAME(PEP01:PEP00)
     F*
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWDHEL     IF   E           K DISK
     FWMA3L     IF   E           K DISK
     FWDVHL1    IF   E           K DISK
     FWTDDL2    IF   E           K DISK
     FWMVPL     IF   E           K DISK
     FWMIFL     IF   E           K DISK
     FWDHCL3    IF   E           K DISK
     FWMLDL1    IF   E           K DISK
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
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FWXMIP1    O    E             DISK    USROPN
     FWXMIP2    O    E             DISK    USROPN
     FXMLPREL1  UF   E           K DISK
     fWMP3L     if   e           k Disk    Rename(pp301:pp30l)
     fSMDPP     uf a e           k Disk    Rename(pdp01:pdp0p)
     fWINOTIL   if   e           k disk    Rename(pno01:pno00)
     fwisrprem  uf a e           k Disk    Rename(pre01:pre00)

     D CL              S             11    DIM(999) ASCEND
     D EXT             S              5    DIM(999) ASCEND
     D FYR             S              3  0 DIM(99)
     D TY              S              1    DIM(99)
     D AMT2            S             15  4 DIM(99)
     Dindex            s              3  0
     DisContri         s              1
     D/copy wPrGroup
     D*
     D* Array for month indicator and table for month names LJQ 8-18-88
     D MN              S              3    DIM(12) CTDATA PERRCD(1)              Month name
     D*
     D* Arrays for class and amount                       LJQ 8-19-88
     D CLS             S              5  0 DIM(999)
     D AMT             S             11  2 DIM(999)
     D EMP             S             11  2 DIM(999)
     D NRT             S              9  4 DIM(999)
     D CON             S             27    DIM(21) CTDATA PERRCD(1)
     D TMP             S              1    DIM(27)
     D NUM             S              1    DIM(5)
     D NUM8            S              1    DIM(8)
     D MD              S              2  0 DIM(12) CTDATA PERRCD(12)            # DAYS IN MONTHS
     D XXD             S              1    DIM(10)                              PRINT DISCOUNTS
     D XX1             S              1    DIM(10)                              PRINT DISCOUNTS
     D TAX             S              1    DIM(8)
     D TTX             S              1    DIM(8)
     D RMK             S             80    DIM(1) CTDATA PERRCD(1)              DARTS DIARY REMARK
     D HDV             S             80    DIM(3) CTDATA PERRCD(1)              DARTS DIARY REMARK
     D MSG             S             80    DIM(1) CTDATA PERRCD(1)
     D CARPOL          C                   CONST('Carrier Policy #: ')
     D PYMTDU          C                   CONST('Payment Due:')
     D XXAVCX          C                   CONST('To Avoid Cancellatio-
     D                                     n')
     D XXCEXM          C                   CONST('Experience Mod')
     D XXINCL          C                   CONST('Increased Limits')
     D WKWC            C                   CONST('WORKERS COMPENSATION')
     D XXLINE          C                   CONST('_______________')
     D X2LINE          C                   CONST('_______________')
     d dbcrHdg         s             30
     d isNetRated      s               n   Inz(*Off)
     d strPortalRunType...
     d                 s             20a
     d*** Begin Add *** 06/12/24 *******************************************
     d decTotalAssessmentAmt...
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
     D idx             s              1  0
     d isSRAs          s               n   Inz(*Off)
     d outIdx          s              1  0

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

     d* Assessment Heading
     d                 ds
     d  hass                   1    140
     d                                     DIM(7)
     d  M11STH                 1     20
     d  M12NDH                21     40
     d  M13RDH                41     60
     d  M14THH                61     80
     d  M15THH                81    100
     d  M16THH               101    120
     d  M17THH               121    140

     d* Assessment Percentage
     d                 ds
     d  pass                   1     42  4
     d                                     DIM(7)
     d  M11STP                 1      6  4
     d  M12NDP                 7     12  4
     d  M13RDP                13     18  4
     d  M14THP                19     24  4
     d  M15THP                25     30  4
     d  M16THP                31     36  4
     d  M17THP                37     42  4

     d* Assessment Amount
     d                 ds
     d  aass                   1     77  2
     d                                     DIM(7)
     d  M11STA                 1     11  2
     d  M12NDA                12     22  2
     d  M13RDA                23     33  2
     d  M14THA                34     44  2
     d  M15THA                45     55  2
     d  M16THA                56     66  2
     d  M17THA                67     77  2

     d* Assessment Sequence #
     d                 ds
     d  sass                   1     14  0
     d                                     DIM(7)
     d  M11STS                 1      2  0
     d  M12NDS                 3      4  0
     d  M13RDS                 5      6  0
     d  M14THS                 7      8  0
     d  M15THS                 9     10  0
     d  M16THS                11     12  0
     d  M17THS                13     14  0
     d*** End   Add *** 06/12/24 *******************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  EMN                    1    350
     D                                     DIM(7)
     D  XXEMN1                 1     50
     D  XXEMN2                51    100
     D  XXEMN3               101    150
     D  XXEMN4               151    200
     D  XXEMN5               201    250
     D  XXEMN6               251    300
     D  XXEMN7               301    350
     D                 DS
     D  HDG                    1    270
     D                                     DIM(10)
     D  HDG1                   1     27
     D  HDG2                  28     54
     D  HDG3                  55     81
     D  HDG4                  82    108
     D  HDG5                 109    135
     D  HDG6                 136    162
     D  HDG7                 163    189
     D  HDG8                 190    216
     D  HDG9                 217    243
     D  HDG10                244    270
     D                 DS
     D  PRO                    1    100
     D                                     DIM(10)                              PRORATA PERCENTS
     D  PRO1                   1     10
     D  PRO2                  11     20
     D  PRO3                  21     30
     D  PRO4                  31     40
     D  PRO5                  41     50
     D  PRO6                  51     60
     D  PRO7                  61     70
     D  PRO8                  71     80
     D  PRO9                  81     90
     D  PRO10                 91    100
     D                 DS
     D  FNA                    1    148
     D                                     DIM(4)
     D  FNA1                   1     37
     D  FNA2                  38     74
     D  FNA3                  75    111
     D  FNA4                 112    148
     D                 DS
     D  DE                     1    035  0
     D                                     DIM(5) DESCEND
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  DC                     1    035  0
     D                                     DIM(5)
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  ED                     1    021  0
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
     D  ARP                    1     15  4
     D                                     DIM(3)
     D  ELARP1                 1      5  4
     D  ELARP2                 6     10  4
     D  ELARP3                11     15  4
     D                 DS
     D  XXCLS                  1     11
     D  EPCLAS                 1      5  0
     D  EPSPEC                 6      6
     D  EPEXT                  7     11
     D                 DS
     D  XXPFDT                 1      7  0
     D  XXYR                   1      3  0
     D  XXMTH                  4      5  0
     D  XXDAY                  6      7  0
     D                 DS
     D  XXPRNT                 1      4  2
     D  XXPRCT                 1      2  0
     D  XXRMD                  3      4  0
     D                 DS
     D  XXPFD                  1      8  0
     D  XXPFM                  1      2  0
     D  XXPFY                  5      8  0
     D                 DS
     D  XXPTD                  1      8  0
     D  XXPTM                  3      4  0
     D  XXPTY                  5      8  0
     D                 DS
     D  XXYM                   1      5  0
     D  XXYY                   1      3  0
     D  XXMM                   4      5  0
     D*
     D* DUE DATE FOR INTERIM
     D                 DS
     D  XXDUED                 1      8  0
     D  XXDUMN                 1      2  0
     D  XXDUDA                 3      4  0
     D  XXDUYR                 5      8  0
     D  xadued                 1      8
     D*
     D* Reporting month & year
     D                 DS
     D  XXRPMY                 1      6  0
     D  XXRPMN                 1      2  0
     D  XXRPYR                 3      6  0
     D                 DS
     D  XXREP                  1      8  0
     D  XXREPM                 1      2  0
     D  XXREPD                 3      4  0
     D  XXREPY                 5      8  0
     D*
     D* Start date for serch of payroll file
     D                 DS
     D  XXSTDT                 1      5  0
     D  XXSTYR                 1      3  0
     D  XXSTMN                 4      5  0
     D*
     D* Comparison date for stopping search of payroll
     D                 DS
     D  XXCPYM                 1      5  0
     D  XXCPYR                 1      3  0
     D  XXCPMN                 4      5  0
     D*
     D* Name of the Months for output                  LJQ 8-18-88
     D                 DS
     D  XMN                    1     72
     D                                     DIM(24)
     D  X1MN                   1      3
     D  X2MN                   4      6
     D  X3MN                   7      9
     D  X4MN                  10     12
     D  X5MN                  13     15
     D  X6MN                  16     18
     D  X7MN                  19     21
     D  X8MN                  22     24
     D  X9MN                  25     27
     D  X10MN                 28     30
     D  X11MN                 31     33
     D  X12MN                 34     36
     D  X13MN                 37     39
     D  X14MN                 40     42
     D  X15MN                 43     45
     D  X16MN                 46     48
     D  X17MN                 49     51
     D  X18MN                 52     54
     D  X19MN                 55     57
     D  X20MN                 58     60
     D  X21MN                 61     63
     D  X22MN                 64     66
     D  X23MN                 67     69
     D  X24MN                 70     72
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
     D                 DS
     D  XXRPDT                 1      7  0
     D  XXRPYM                 1      5  0
     D  XXRPDY                 6      7  0
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
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D                 DS
     D  DSDT2                  1      8  0
     D  DSMM2                  1      2
     D  DSDD2                  3      4
     D  DSYY2                  5      8
     D                 DS
     D  IFCO#                  1      3
     D  IFFND                  4      6
     D  IFEMP#                 7     15
     D  IFDIV                 16     20
     D  IFPERY                21     24
     D  IFPERM                25     26
     D  IFDOT                 27     27
     D  IFEXT                 28     30
     D  QQDC30                 1     30
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')
     D CHGNUM          C                   CONST('SA020R')
     D VBPSTD          C                   CONST('- Past Due')
     D @2NPER          C                   CONST('2ND PRD')
     D svIn78          s              1n
     D svIn83          s              1n
     D svWk90          s              9  0
     D x5RpCYM         s              5  0
     D isMassReq       s              1n   inz
     d massFlag        s              1
     d xmlLink         s                   like(XPLink)
     d xmlFileName     s                   like(XPXML)
     d xmlFileBlank    s                   like(XPXML)
     d xmlFile         s                   like(XPXML)
     D isUseSpoolOut   s              1n   inz
     D isUseXML        S              1n   inz
     D isUseScript     S              1n   inz
     D isNotRated      S              1n   inz(*Off)
     d xxCont          s            120
     d xxeCity         s             40
     d xxeSt           s              2
     d xxeZip          s              9s 0
     d strLang         s             60
     d strLangCode     s            250
     D string          s             10    inz(*blanks)
     D message         s            250    inz(*blanks)
     D xxPre           s              2    inz('MI')
     d processType     c                   const('Self-Reporting Worksheet')
     d intPayroll      s             11s 0

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
     D*
     D dbfds           ds
     D  dbrrn                397    400b 0
     D*
     d x#RRN           s              9  0 inz(0)
     d*** begin add ***  02/03/23  **************************************
     d svRRN           s              9  0 inz(0)
     d templateFile    s                   like(XPFILE)
     d*** end add *****  02/03/23  **************************************
     d*** Beg add *****  04/18/24  **************************************
     d xxFac3          s             11  9
     d strTmp          s             27
     d strNum          s              5
     d strAsp1         s              5
     d*** End add *****  04/18/24  **************************************
        //*** Beg Add *** 12/10/25 ******************************************
     d Rate8_2         s              8s 2 Inz
     d RateDec         s              1  0 Inz
        //*** End Add *** 12/10/25 ******************************************
     d*
     d b               s              3s 0
     d y               s              3s 0
     d*
     D sHdg            S             40    DIM(20)
     D sFact           S              5S 4 DIM(20)
     D sPerc           S              5S 2 DIM(20)
     D sProF           S              5S 4 DIM(20)
     D sProP           S              5S 2 DIM(20)
     D sAppr           S              1    DIM(20)
     d*
     DstrCmd           s            500a   varying inz
     DstrOutputPath    s            500a   varying inz('No output generated.')
     D/copy sPrRunCmd
     D/copy sPrXML
     d GetUsaDate      pr             8  0
     d  inDate7                       7  0
     d* Returns '1' in parm 2 if pgm in stack
     d ChkPgmInStack   pr                  ExtPgm('SRCHKPGM')
     d                               10    const
     d                                1

     d pgmInStack      s               n
     d suppressDarts   s               n

     Ddt5CYMD          PR             5  0
     D numDate6                       6  0 Const
     d decHoldManPrem...
     d                 s                   Like(SRManPrm)
     d decHoldPayrAmt...
     d                 s                   Like(SRAmtP)
     d intCurDt7       s              7  0
     d is3rdParty      s               n

      /Free
       Dcl-S kkDocKey Like(DPkey);
       Dcl-S kkDocType Like(DPtype) Inz('SRWORKSHEET');
       Dcl-S workDate Date;
      /End-Free
     d                 ds
     d  dsFactors              1    143
     d  dsLmPc                 1      5  3
     d  dsTot1                 6     16  0
     d  dsDscB                17     27  9
     d  dsTot2                28     38  0
     d  dsExpM                39     43  4
     d  dsTot3                44     54  0
     d  dsDscA                55     65  9
     d  dsTot4                66     76  0
     d  dsTerr                77     87  0
     d  dsWaiv                88     98  0
     d  dsRein                99    109  0
     d  dsNetF               110    121  9
     d  dsNetP               122    132  0
     d  dsNWav               133    143  0
       Dcl-S intRptDate Zoned(7:0);
       Dcl-S decPrvBilled Zoned(11:2);
       Dcl-S dtWork Date;
       //***  Begin Add  ***  02/07/22  **************
       dcl-s isProcess ind inz(*on);
       dcl-ds dsW7221R dtaara('QTEMP/W7221R') qualified inz;
        W7221R char(250);
         path char(250) overlay(W7221R:1);
         fyr char(3) overlay(W7221R:1);
       end-ds;
     DCvtToDate        PR              d
     D numDate                        8  0 Const
     DGetFirstDay      PR              d
     D dateIn                          d   const
     DGetLastDay       PR              d
     D dateIn                          d   const
       //***  End   Add  ***  02/07/22  **************

     D***  Begin Add  ***  01/19/22  **********************
      /copy wPrEmplr
     D***  End   Add  ***  01/19/22  **********************

     IPIF01
     I              IFCO#                       @IFCO#
     I              IFFND                       @IFFND

     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C     CHGPL1        PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C                   PARM                    XXDEC             3 0
     C*
     C*   KEY LIST FOR THE COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*   KEY LIST FOR THE FUND
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND             3 0
     C*
     C*   KEY LIST FOR THE OFFICE
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMOFF#
     C*
     C*   KEY LIST FOR THE RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRATE
     C*
     C*   KEY LIST FOR THE HARBOR EXPOSURE FILE
     C*
     C     HEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXPDT
     C*
     C     HEKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*   KEY LIST FOR THE STAT RPT/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXPDT
     C*
     C     RRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*   KEY LIST FOR EMPLOYER MASTER FILE
     C*
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C*   KEY LIST FOR THE QUOTE TRACKING FILE
     C*
     C     QTKY7         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXSTAT            1
     C*
     C*   KEY LIST FOR THE EMPLOYER HISTORY FILE
     C*
     C     EHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXSTAT            1
     C*
     C* Key List for the Employer Payroll File
     C*
     C     SRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXYM
     C*
     C*   KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C*
     C     EPKYL         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    EPCLAS
     C*
     C     EPKYL1        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPSPEC
     C                   KFLD                    EPEXT
     C                   KFLD                    XXRPDT
     C*
     C*   KEY LIST FOR THE OVERRIDE FILE
     C*
     C     ORKEY         KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT             1
     C*
     C*   KEY LIST FOR THE USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*   KEY LIST FOR THE BILLING SCHEDULE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*   KEY LIST FOR THE EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     EPKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXCLAS
     C                   KFLD                    XXSPEC
     C                   KFLD                    XXEXT
     C                   KFLD                    XXRPDT
     C*
     C*   KEY LIST FOR THE PAYROLL CLASSIFICATION FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    KKEFDT            7 0
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    EPCLAS
     C                   KFLD                    KKEFDT
     C*
     C     PRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C                   KFLD                    EPCLAS
     C*
     C* Key List for the Employer Payroll File
     C*
     C     SRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXSTDT
     C*
     C* Key List for the Employer A/R File
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C     EAKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C*
     C     EAKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  AGENT ASSIGNMENT FILE
     C*
     C     AAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*  INCR LIMIT TABLE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXILEF
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
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    KKFYR
     C*
     C*
     C*
     C*   KEY LIST FOR COMMERCIAL WORKFILE BUILD
     C*
     C     EBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR 3RD EMPLOYER ADDRESS FILE
     C*
     C     A3KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C                   KFLD                    K1EFDT            7 0
     C     PXKY1         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*  Darts file
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  Remark type table file
     C     CTKY          KLIST
     C                   KFLD                    Q#RMCD
     C*
     C     VHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXRPDT
     C     VHKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    #XXTDT
     C     EHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
      *
      *  DEDUCTIBLE DISCOUNT TABLE
      *
     C     DDKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    ELDCDE
     C                   KFLD                    ELPFDT
     C*
     C     DDKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    XXST
     C                   KFLD                    ELDCDE
     C     VPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    VHVDR#
     C     PRKYE         KLIST
     C                   KFLD                    PRCO#
     C                   KFLD                    PRRTCD
     C                   KFLD                    PRCLAS
     C     PRKYE1        KLIST
     C                   KFLD                    PRCO#
     C                   KFLD                    PRRTCD
     C                   KFLD                    PRCLAS
     C                   KFLD                    PREFDT
     C*
     C* HCN CREDIT KEY
     C*
     C     HCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    X1KEY
     C*
     C     *LIKE         DEFINE    EPCLAS        XXCLAS
     C     *LIKE         DEFINE    HEEFDT        XXPDT
     C     *LIKE         DEFINE    ELRTCD        XXRATE
     C     *LIKE         DEFINE    EPSPEC        XXSPEC
     C     *LIKE         DEFINE    EPEXT         XXEXT
     C     *LIKE         DEFINE    PRRTCD        XXRTCD
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    EBFYR         XXFYR
     C     *LIKE         DEFINE    EMDT          XXDAT
     C     *LIKE         DEFINE    EMDT          XXFRDT
     C     *LIKE         DEFINE    EMDT          XXTODT
     C     *LIKE         DEFINE    EMDT          XXRPTO
     C     *LIKE         DEFINE    EMDT          XXRPFM
     C     *LIKE         DEFINE    EMDT          X1RPDT
     C     *LIKE         DEFINE    EMDT          XXILEF
     C     *LIKE         DEFINE    EMDT          XXILEX
     C     *LIKE         DEFINE    EMDT          X1TO
     C     *LIKE         DEFINE    EMDT          X1FROM
     C     *LIKE         DEFINE    EAAMT         SVAMT
     C     *LIKE         DEFINE    EAARTY        SVARTY
     C     *LIKE         DEFINE    EAFYR         SVFYR
     C     *LIKE         DEFINE    ELENMP        XXSEXC
     C     *LIKE         DEFINE    ELEXM1        XXEMOD
     C     *LIKE         DEFINE    EAFYR         KKFYR
     C     *LIKE         DEFINE    ELHCN         XXHCN
     C     *LIKE         DEFINE    ELHCN         SVHCN
     C     *LIKE         DEFINE    XXFACT        XXFAC2
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQRPMY            6
     C                   PARM                    QQOUTQ           10
     C                   PARM                    QQMODE            1
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQRNDT            8
     C                   PARM                    QQDUE             9
     C                   PARM                    QQSRT             1
     C                   PARM                    QQFAGT            5
     C                   PARM                    QQTAGT            5
     C                   PARM                    QQBGDT            8
     C                   PARM                    QQENDT            8
     C                   PARM                    QQFEMP            9
     C                   PARM                    QQTEMP            9
     C                   PARM                    QQPRMM            1
     C                   PARM                    QQDART            1
     C                   PARM                    QQINDP            1
     C                   PARM                    QQALL             1
     C                   PARM                    QQCOMPLETED       1
     C                   PARM                    QQUSER           10
     C*
      /Free
       isUseSpoolOut = UseSpoolOut(processType);
       isUseScript = UseScriptura(processType);
       isUseXML = UseScriptura(processType)
        or Use3rdXml(processType);
      /end-free

      /Free
       // Account for Connect requests.
       // User is passed in.
       // 'E' = EFT Payment Confirmation
       // 'S' = 3rd Party Payment Success Confirmation
       // 'P' = Reports pending submission (not paid).
       If Q1prms >= 22;
          Select;
          When qqCompleted = 'E';
             qqCompleted = 'Y';
             strPortalRunType = 'EFT Payment';
             Q1user = QQuser;  // Connect passes in user.
          When qqCompleted = 'P';
             qqCompleted = 'Y';
             strPortalRunType = 'Pending Submission';
             Q1user = QQuser;  // Connect passes in user.
          When qqCompleted = 'S';
             qqCompleted = 'Y';
             strPortalRunType = '3rd Party Success';
             Q1user = QQuser;  // Connect passes in user.
          EndSl;
       EndIf;
      /End-Free

     C*
     C     Q1PRMS        IFGE      20
     C                   MOVE      QQALL         XXALL             1
     C                   ENDIF
      *
     C     Q1PRMS        IFGE      18
     C     QQDART        ANDEQ     'Y'
     C                   MOVE      'Y'           XXRESV            1
     C                   ENDIF
      *When intrem report was submitted for single employer the
      * employer number is moved to QQFEMP and QQTEMP in W7220R.
      * If the both  QQFEMP and QQTEMP are the same then consider the
      * as for single employer
     C                   MOVE      *OFF          *IN47             1
     C     QQFEMP        IFEQ      QQTEMP
     C     QQFEMP        IFNE      *BLANKS
     C     QQFEMP        IFNE      *ZEROS
     C                   MOVE      *ON           *IN47
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      QQFAGT        XXFAGT            5 0
     C                   MOVE      QQTAGT        XXTAGT            5 0
     C                   MOVE      QQFEMP        XXFEMP            9 0
     C                   MOVE      QQTEMP        XXTEMP            9 0
     C                   MOVE      QQBGDT        XXBGD             8 0
     C                   MOVE      QQENDT        XXEND             8 0
     C                   Z-ADD     XXBGD         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT            7 0
     C                   Z-ADD     XXEND         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT            7 0
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXCURD            8 0
       //***  Begin Add  ***  02/07/22  **************
      /Free
       Reset dsW7221R;
       In(e) dsW7221R;
      /End-free
       //***  End   Add  ***  02/07/22  **************
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
     C     DTCYM1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YMD'        SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCTO8        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTSUB1        PLIST
     C                   PARM                    @@DT1             8 0
     C                   PARM                    @@DAYS            5 0
     C                   PARM                    SRERR
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
     C     DTMDY5        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C     CLTID         IFEQ      '045'
     C                   MOVE      '1'           *IN45
     C                   ENDIF
     C*
     C                   MOVE      '0'           *IN25
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C                   Z-ADD     .52           EMNHRZ
     C     CLTID         WHENEQ    '045'
     C                   Z-ADD     1.02          EMNHRZ
     C     CLTID         WHENEQ    '133'
     C                   Z-ADD     1.0           EMNHRZ
     C     CLTID         WHENEQ    '025'
     C                   Z-ADD     .27           EMNHRZ
     C                   MOVE      '1'           *IN25
     C                   OTHER
     C                   Z-ADD     .27           EMNHRZ
     C                   ENDSL
      /Free
       If isContri = '1';
          *in25 = '1';
       Else;
          *in25 = '0';
       Endif;
      /End-free
     C*
     C                   MOVE      QQRPMY        XXRPMY
     C                   Eval      x5RpCYM = Dt5CYMD(xxRpMY)
     C                   MOVE      *BLANKS       QQDC30
     C                   MOVE      *BLANKS       QQIFLR
      *
     C     CLTID         IFEQ      '133'                                                    ID ='136
     C                   MOVE      '.'           IFDOT
     C                   MOVE      'PDF'         IFEXT
     C                   MOVE      XXRPMN        IFPERM
     C                   MOVE      XXRPYR        IFPERY
     C     XXENV         IFEQ      ' '
     C                   MOVEL(P)  'SRPDF'       QQIFLR           80
     C                   ELSE
     C                   MOVEL(P)  'TSRPDF'      QQIFLR           80
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     XXRPMN        XXREPM
     C                   Z-ADD     XXRPYR        XXREPY
     C                   Z-ADD     1             XXREPD
     C                   Z-ADD     XXREP         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRPYM
     C                   MOVEL     SRDTO         ##YYMM            5
     C                   MOVE      31            XXRPDY
     C                   Z-ADD     XXRPDT        SVRPDT            7 0
     C                   MOVE      QQCO#         XXCO#             3 0
     C                   MOVE      QQFND         XXFND             3 0
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQRNDT        XXRNDT            8 0
     C                   MOVE      QQDUE         XXDUE             9 2
     C     XXDUE         MULT      -1            XXNDUE            9 2
      /free

       Clear xmlLink;
       Clear xmlFileName;
       //*** begin chg ***  01/17/22  *************************************
       //*****If xxEmp# = 0;
       //*** begin chg ***  02/03/23  **************************************
       //*****If xxEmp# = 0 or xxAll = 'Y';
       //* If generating all for one employer, will be single document.
       If xxEmp# = 0;
       //*** end chg *****  02/03/23  **************************************
       //*** end chg *****  01/17/22  *************************************
          isMassReq = *on;
          massFlag = 'Y';
       Endif;

      /end-free
     C*
     C*   CHAIN TO THE USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C*   CHAIN TO THE COMPANY FILE
     C*
     C     COKEY         CHAIN     PCO01                              60
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
      *
      *   Center company name in field
      *
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        XXCONM           40
      *
     C                   MOVE      *ALL'_'       LINE            105
     C*
     C*   CHAIN TO THE FUND FILE
     C*
     C     FNKEY         CHAIN     PFN01                              60
      ***  Get the Fund Aaxiliary
      /free
       Clear F2blog;
          *in84 = *Off;
       Chain fnkey PF201;
       If F2blog <> ' ';
          *in84 = *On;
       Endif;
       Select;
       When (cltid = '152' or cltid = '114') and F2blog = 'OCTABILL';
            LOGHRZ =  0.2;
            LOGVRT = 0.3;
       Other;
            LOGHRZ = 0.2;
            LOGVRT = 0.3;
       Endsl;
      /end-free
     C*
     C*
     C     EXKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  'Agent'       EXAHDR
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  'Agent'       EXAHDR
     C                   ENDIF
     C*
     C*
     C*
     C*  LOAD FUND NAME INTO CENTERED PRINT FIELD.
     C*
     C     CLTID         IFEQ      '093'
     C                   CAT       WKWC:24       XXFNNM
     C                   ELSE
     C     ' '           CHECKR    FNNAME        N                 3 0
     C     60            SUB       N             N
     C     N             DIV       2             N
     C                   CAT       FNNAME:N      XXFNNM           60
     C                   MOVEL(P)  XXFNNM        X1FNNM
     C                   CALL      'SRCENTER'
     C                   PARM                    X1FNNM           80
     C                   PARM      60            X1LEN             3 0
     C                   MOVEL(P)  X1FNNM        XXFNNM
     C                   ENDIF
     C*
     C*  BUILD THE DUE DATE FROM FUND MASTER AND REPORTING MONTH,
     C*  IF THE DUE DATE IS GIVEN.
     C*
     C     FNMIDY        IFNE      *ZERO
     C                   MOVE      *ON           *IN14
     C                   Z-ADD     0             XXDUED            8 0
     C                   Z-ADD     XXRPMN        XXDUMN
     C                   Z-ADD     XXRPYR        XXDUYR
     C                   ADD       1             XXDUMN
     C     XXDUMN        IFEQ      13
     C                   Z-ADD     1             XXDUMN
     C                   ADD       1             XXDUYR
     C                   ENDIF                                                  XXDUMN IFEQ
     C                   Z-ADD     FNMIDY        XXDUDA
     C                   MOVEL     XXDUED        XXNUM
     C                   MOVEL     'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXDUDT           10
     C     PYMTDU        CAT       XXDUDT:1      XXPYDU
     C                   ENDIF                                                  FNMIDY IFEQ
     C*
     C*  LOAD FUND NAME AND ADDRESS INTO PRINT ARRAY
     C*
     C*
     C*
     C*
     C*  GET THE FUND AUXILIARY RECORD
     C*
     C*
     C*
     C*   GET THE DISTRIBUTION INFORMATION
     C*
     C                   MOVE      '3'           XXPRT             1
      *
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   ENDIF                                                  QQMODE IFEQ
      *
     C     ORKEY         CHAIN     POR01                              60
     C*
     C*   START READING THE WORK FILE                      JJJ 2-28-89
     C*
     C     EBKY          CHAIN     PEB01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C* FINDOUT THE FIRST DAY OF THE REPORTING MONTH
     C*
     C                   MOVE      QQRPMY        X5RPMY            6
     C     2             SUBST     X5RPMY:1      X5M               2
     C     4             SUBST     X5RPMY:3      X5YY              4
     C     X5M           CAT       '01'          X5MD              4
     C     X5MD          CAT       X5YY          X5RPDT            8
     C                   MOVEL     X5RPDT        X5RPDN            8 0
     C                   Z-ADD     X5RPDN        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFD              7 0
     C                   Z-ADD     SVRPDT        XXRPDT
     C*
       //***  Begin Add  ***  02/07/22  **********************
      /Free
       // If the running for the first month and there is overlapping years, use mass
       If massFlag <> 'Y';
          If GetFirstDay(CvtToDate(EBptDt)) = CvtToDate(x5rpDn)
           and GetLastDay(CvtToDate(EBptDt)) <> CvtToDate(EBptDt);
             isMassReq = *on;
             massFlag = 'Y';
          Endif;
       Endif;
      /End-free
       //***  End   Add  ***  02/07/22  **********************
     C*
       //***  Begin Chg  ***  02/07/22  **********************
     C*****EBEMP#        IFGE      XXFEMP
     C*****EBEMP#        ANDLE     XXTEMP
     C**********
     C*****EBAGT#        IFGE      XXFAGT
     C*****EBAGT#        ANDLE     XXTAGT
      /Free
       isProcess = *on;
       Select;
       When EBemp# < xxFEmp or EBemp# > xxTEmp;
          isProcess = *off;
       When EBagt# < xxFAgt or EBagt# > xxTAgt;
          isProcess = *off;
       When dsW7221R.fyr <> *blanks and dsW7221R.fyr <> %char(EBfyr);
          isProcess = *off;
       Endsl;

       If isProcess;
      /End-free
       //***  End   Chg  ***  02/07/22  **********************
     C*
     C                   MOVE      EBEMP#        XXEMP#
     C                   MOVE      EBDIV         XXDIV
     C                   MOVE      EBFYR         XXFYR
     C*
     C                   MOVE      *OFF          *IN77
     C*
     C*   Skip policies that haven't had a final bill run by checking
     C*   existence of premium history record.
     C*
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*   If a carrier pol# exists then format it for display on report
     C*
     C                   CLEAR                   XXCAP#
     C     RPKY          CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C*
     C                   MOVE      *ON           *IN77
     C     CARPOL        CAT(P)    RPCAP#        XXCAP#           36
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   MOVE      *BLANKS       F1SHTR
     C                   MOVE      *BLANKS       F1SITR
     C                   MOVE      *BLANKS       F1SPTR
     C                   MOVE      *BLANKS       F1SFTR
     C                   MOVE      *BLANKS       F1SQTR
     C                   MOVE      *BLANKS       F1SRTR
     C                   MOVE      *BLANKS       F1SPOF
     C                   MOVE      *BLANKS       F1SRPO
     C*
     C*  GET THE FUND DETAIL RECORD
     C*
     C     F1KY          CHAIN     PF101                              60
     C*
     C     EMKEY1        CHAIN     PEM01                              60        JJJ 2-28-89
     C                   MOVE      *BLANKS       FNA
     C                   MOVE      *BLANKS       XXDIRI           30
     C*
     C     OFKY          CHAIN     POF01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     OFMINM        XXDIRI
     C                   Z-ADD     1             N
     C                   MOVEL(P)  OFCKNM        FNA(N)
     C                   ADD       1             N
     C                   MOVEL(P)  OFAAD1        FNA(N)
     C                   ADD       1             N
     C     OFAAD2        IFNE      *BLANKS
     C                   MOVEL(P)  OFAAD2        FNA(N)
     C                   ADD       1             N
     C                   ENDIF
     C                   MOVEL(P)  OFACTY        FNA(N)
     C                   CAT       ',':0         FNA(N)
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVEL     OFAST         XXSTZP
     C                   MOVE      OFAZIP        XXSTZP
     C                   MOVE      XXSTZP        XXZIP4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5
     C                   MOVEL     OFAZIP        XXST10
     C                   MOVE      XX5           XXST10
     C                   MOVE      XXST10        XXSTZP
     C                   END
     C*
     C                   CAT       XXSTZP:1      FNA(N)
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  MOVE THE EMPLOYER ADDRESSES, CITY, STATE AND ZIP TO OUTPUT
     C*
     C                   MOVE      *BLANKS       EMN
     C                   MOVE      *BLANKS       XXMAD1           40
     C                   MOVE      *BLANKS       XXMAD2           40
     C                   Clear                   XXeCity
     C                   Clear                   XXeSt
     C                   Clear                   XXeZip
     C                   Clear                   XXCont
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL     EMNAME        EMN(1)
     C                   MOVEL     EMFDNM        EMN(2)
     C                   ELSE
     C                   MOVEL     EMNAME        EMN(2)
     C                   ENDIF
     C*
     C* USE 3RD EMPLOYER ADDRESS IF IT EXISTS
     C*
     C     A3KY          CHAIN     PA301                              60
     C     *IN60         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C                   MOVEL     A3ADR1        XXMAD1
     C                   MOVEL     A3ADR2        XXMAD2
     C                   Movel     A3Cont        xxCont
     C                   Movel     A3Cty         XXeCity
     C                   Movel     A3St          XXeSt
     C                   Movel     A3Zip1        XXeZip
     C                   Move      A3Zip2        XXeZip
     C                   MOVE      *BLANKS       EMCS
     C     A3ADR2        COMP      *BLANKS                                63
     C     A3OOC         IFNE      *BLANKS
     C                   MOVEL     A3OOC         EMCS
     C                   ELSE
     C     A3CTY         CAT       ',':0         EMCS
     C     EMCS          CAT       A3ST:1        EMCS
     C     EMCS          CAT       A3ZIP1:1      EMCS
     C     A3ZIP2        IFNE      *BLANKS
     C     EMCS          CAT       '-':0         EMCS
     C     EMCS          CAT       A3ZIP2:0      EMCS
     C                   ENDIF                                                  A3ZIP2 IFNE
     C                   ENDIF                                                  A3OOC IFNE
     C*
     C                   ELSE                                                   *IN60 IFEQ
     C*
     C*  BASED ON MAILING/PHYSICAL ADDRESS FLAG MOVE CORRECT ADDRESSES,
     C*  CITY, STATE AND ZIP TO OUTPUT
     C*
     C     EMMPFL        IFEQ      'M'
     C     EMMAD2        COMP      *BLANKS                                63
     C                   MOVE      EMMAD1        XXMAD1
     C                   MOVE      EMMAD2        XXMAD2
     C                   Movel     EMMCty        XXeCity
     C                   Movel     EMMSt         XXeSt
     C                   Z-add     EMMZip        XXeZip
     C                   ELSE
     C     EMPAD2        COMP      *BLANKS                                63
     C                   MOVE      EMPAD1        XXMAD1
     C                   MOVE      EMPAD2        XXMAD2
     C                   Movel     EMPCty        XXeCity
     C                   Movel     EMPSt         XXeSt
     C                   Z-add     EMPZip        XXeZip
     C                   END
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMCTY        XXLAST           56
     C                   MOVEL     EMMST         XXSTZP           13
     C                   MOVE      EMMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     EMPCTY        XXLAST           56
     C                   MOVEL     EMPST         XXSTZP           13
     C                   MOVE      EMPZIP        XXSTZP
     C                   END
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     EMPZIP        XXST10           10
     C                   END
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END
      /Free
       isContri = GetPrmContflg(xxCo#:xxFnd);
      /End-free
     C*
     C*  CONCATENATE CITY, STATE AND ZIP
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        EMCS             40
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   MOVE      XXEMP#        ##EMP#            9
     C                   MOVE      XXDIV         ##DIV             5
     C                   MOVE      ' '           ##ODR             1
     C                   MOVE      ' '           ##NODR            1
     C                   MOVE      ' '           ##AODR            1
     C                   MOVE      *BLANKS       ##EMN1           50
     C                   MOVE      *BLANKS       ##EMN2           50
     C                   MOVE      *BLANKS       ##EMN3           50
     C                   MOVE      *BLANKS       ##EMN4           50
     C                   MOVE      *BLANKS       ##EMN5           50
     C                   MOVE      *BLANKS       ##EMN6           50
     C                   MOVE      *BLANKS       ##EMN7           50
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
     C*
     C                   MOVEL     ##EMN1        XXEMN1
     C                   MOVEL     ##EMN2        XXEMN2
     C                   MOVEL     ##EMN3        XXEMN3
     C                   MOVEL     ##EMN4        XXEMN4
     C                   MOVEL     ##EMN5        XXEMN5
     C                   MOVEL     ##EMN6        XXEMN6
     C                   MOVEL     ##EMN7        XXEMN7
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C                   DO        7             X
     C                   CLEAR                   EMN50
     C                   CAT       EMN(X):5      EMN50            50
     C                   MOVEL     EMN50         EMN(X)
     C                   ENDDO
     C                   ENDSL
     C*
     C     XXEMN6        DOWEQ     *BLANKS
     C                   MOVEL(P)  XXEMN5        XXEMN6
     C                   MOVEL(P)  XXEMN4        XXEMN5
     C                   MOVEL(P)  XXEMN3        XXEMN4
     C                   MOVEL(P)  XXEMN2        XXEMN3
     C                   MOVEL(P)  XXEMN1        XXEMN2
     C                   MOVEL(P)  *BLANKS       XXEMN1
     C                   ENDDO
     C*
     C*  GET THE EMPLOYER DETAIL RECORD
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C                   eval      x#RRN = dbRRN
     C***  Begin Chg  ***  01/19/22  **********************
     c**********         Eval      isNetRated = *Off
     C**********         If        F1netR = 'Y' And ELpfDt >= F1netD
     c**********         Eval      isNetRated = *On
     c**********         EndIf
     C                   Eval      isNetRated =
     C                              IsUseNetRate(ELco#:ELfnd:ELemp#:ELdiv:ELfyr)
     C***  End   Chg  ***  01/19/22  **********************
       //*** Begin Add *** 06/12/24 **************************************
       // Check and see if SR Assessments being done
       isSRAs = *Off;
       Chain (ELco#:ELfnd:ELfYr) pf101;
       If F1srAs = 'Y' Or ELsrAs = 'Y';
          isSRAs = *On;
       EndIf;
       //*** End   Add *** 06/12/24 **************************************
     C*** End   Add *** 05/15/20 **************************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C                   MOVE      *BLANK        LRGDFL            1
     C     WCFL43        IFEQ      'Y'
     C                   MOVEL(P)  ELBSCD        X1KEY            10
     C     TBKY          CHAIN     PBS01                              60
     C     *IN60         IFEQ      *OFF
     C     BSLPGM        ANDEQ     'L'
     C     ELKEY         CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           LRGDFL            1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           DTLFLG            1
     C                   ELSE
     C                   MOVE      'N'           DTLFLG
     C                   ENDIF
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ELRTCD        XXRATE
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXPDT
     C     RRKY          SETGT     PIF01
     C     RRKY2         READPE    PIF01                                  60
     C     *IN60         IFEQ      *OFF
     C*** begin chg ***  03/07/22  ******************************************
     C**** ELIFFL        IFEQ      *BLANK
     C                   If        ELIFFL = ' ' and strPortalRunType = ' '
     C*** end chg *****  03/07/22  ******************************************
     C                   Z-ADD     IFISFE        SRFEE
     C                   ELSE
     C                   Z-ADD     *ZERO         SRFEE
     C                   ENDIF
     C                   Z-ADD     *ZERO         SRFEE2            7 2
     C                   ELSE
     C                   Z-ADD     *ZERO         SRFEE
     C                   Z-ADD     *ZERO         SRFEE2
     C                   ENDIF
     C                   MOVE      *OFF          *IN60
     C                   ENDIF
     C     VHKY1         SETGT     PVH01
     C     ELKEY         READPE    PVH01                                  62
     C     *IN62         IFEQ      *OFF
      *
     C                   SELECT
     C     VHEXDT        WHENEQ    *ZERO
     C                   MOVE      *ON           VDRFLG            1
     C     VHEXDT        WHENNE    *ZERO
     C     VHEXDT        ANDGT     XXRPDT
     C                   MOVE      *ON           VDRFLG
     C                   OTHER
     C                   MOVE      *OFF          VDRFLG
     C                   ENDSL
      *
     C     VPKY          CHAIN     PVP01                              60
     C     *IN60         IFEQ      *OFF
     C     VPMTHI        ANDEQ     'Y'
     C                   MOVE      *OFF          VDRFLG
     C                   ENDIF
     C                   ELSE
     C                   MOVE      *OFF          VDRFLG
     C                   ENDIF
     C     DTLFLG        IFEQ      'Y'
     C     VDRFLG        ANDEQ     *OFF
     C     DTLFLG        OREQ      'Y'
     C     QQEMP#        ANDNE     *BLANK
     C     QQEMP#        ANDNE     *ZEROS
     C                   Z-ADD     1             XXPROR           12 9
     C***  Begin Chg  ***  01/19/22  **********************
     C*****F1NETR        IFEQ      'Y'
     C*****ELPFDT        ANDGE     F1NETD
     C                   If        isNetRated
     C***  End   Chg  ***  01/19/22  **********************
     C*
     C*
     C*  SUBTRACT EXP MOD CHG FROM MODIFIED PREMIUM, GET CURRENT EXP MOD AND
     C*  MULTIPLY IT BACK INTO ELENMP SO THAT NET RATE REFLECT CURRENT EXP
     C*  MOD
     C*
     C                   MOVE      *ON           *IN62
     C     ELMAPR        IFNE      *ZERO
     C     ELEMPM        ANDNE     *ZERO
     C     EPKEY         CHAIN     PMP01                              60
     C*
     C*  GET EXP MOD CHG IF NO HIT OR IF WDMPP/MPEXPA = 0
     C*
     C     *IN60         IFEQ      *ON
     C     MPEXPA        OREQ      0
     C*
     C     EPKY          CHAIN     PEP00                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'
     C                   ADD       EPADLD        XXADLD           11 2
     C                   ENDIF
     C     EPKY          READE     PEP00                                  60
     C                   ENDDO
     C*
     C                   MOVE      ELRTCD        XXRATE
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      ELPFDT        XXPDT
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
      *
     C*
     C     *LIKE         DEFINE    XXBGDT        X1BGDT
     C     ELPFDT        IFGT      XXBGDT
     C                   Z-ADD     ELPFDT        X1BGDT
     C                   ELSE
     C                   Z-ADD     XXBGDT        X1BGDT
     C                   ENDIF
     C*** begin add ***  11/16/23  **************************************
     C                   If        ELPtdt < xxendt
     C*** begin chg ***  02/06/24  **************************************
     C*****              Z-ADD     ELPtdt        xxendt
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQNXTD
     C                   CALL      'SRPAST8'
     C                   PARM                    QQNXTD            8 0
     C                   PARM      1             QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   Z-ADD     QQNxtd        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxendt
     C*** end chg *****  02/06/24  **************************************
     C                   Endif
     C*** end add *****  11/16/23  **************************************
     C*
     C                   Z-ADD     0             XXPRTT
     C     XXENDT        SUB       X1BGDT        XXDAYS
     C                   ADD       1             XXDAYS
     C                   Z-ADD     X1BGDT        XXWKDT            7 0
     C     XXWKDT        DOWLE     XXENDT
     C*
     C     ED(3)         IFNE      *ZERO
     C     XXWKDT        ANDGE     ED(3)
     C                   Z-ADD     EM(3)         XXEMOD
     C                   ELSE
     C     ED(2)         IFNE      *ZERO
     C     XXWKDT        ANDGE     ED(2)
     C                   Z-ADD     EM(2)         XXEMOD
     C                   ELSE
     C     ED(1)         IFEQ      0
     C     XXWKDT        ORLT      ED(1)
     C                   Z-ADD     1             XXEMOD
     C                   ELSE
     C     EM(1)         IFNE      *ZERO
     C                   Z-ADD     EM(1)         XXEMOD
     C                   ELSE
     C                   Z-ADD     1             XXEMOD
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  ED,2 IFNE
     C                   ENDIF                                                  ED,3 IFNE
     C*
     C     XXEMOD        DIV(H)    XXDAYS        XXPROD           11 9
     C                   ADD(H)    XXPROD        XXPRTT           11 9
     C                   ADD       1             XXWKDT
     C                   ENDDO
     C*
     C                   Z-ADD(H)  XXPRTT        XXWK54            5 4
     C     XXWK54        IFEQ      *ZERO
     C                   Z-ADD     1             XXWK54
     C                   ENDIF
     C                   MOVE      *OFF          *IN71
     C     ELWVPC        IFEQ      0
     C     ELEWAV        ANDNE     0
     C     ELEWAV        ORNE      0
     C     ELWVTP        ANDEQ     'S'
     C                   MOVE      *ON           *IN71
     C     1             DIV(H)    12            XXDPRO            9 8
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
     C                   Z-ADD     XXEWAV        XXPREM
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
     C                   ELSE
     C*
     C                   MOVE      *OFF          *IN62
     C*
     C                   ENDIF                                                  F1NETR IFEQ
     C                   MOVE      *OFF          *IN99
     C                   MOVE      *BLANK        XX1STM            1
     C*
     C     XXALL         IFEQ      'Y'
     C                   MOVE      *ON           *IN99
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFD
     C                   MOVE      XXPFM         XXRPMN
     C                   MOVE      XXPFY         XXRPYR
     C*
     C                   Z-ADD     XXRPMY        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRPYM
     C                   MOVE      31            XXRPDY
     C                   Z-ADD     XXRPDT        XXDT
     C                   MOVE      01            XXDT
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     *OFF
     C     XXRPYM        ORLE      XXTPYM
     C     XXDT          ANDNE     ELPTDT
     C     XXALL         ANDEQ     'Y'
     C*
     C     XXALL         IFEQ      'Y'
     C*
     C                   MOVE      XXRPMY        SVRPMY            6 0
     C*
     C                   Z-ADD     XXRPDT        XXDT
     C                   MOVE      01            XXDT
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQBGDT
     C*
     C                   Z-ADD     XXRPMN        X
     C                   MOVE      MD(X)         XXDT
     C                   Z-ADD     XXDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQENDT
     C*
     C                   MOVE      QQBGDT        XXBGD             8 0
     C                   MOVE      QQENDT        XXEND             8 0
     C                   Z-ADD     XXBGD         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT            7 0
     C                   Z-ADD     XXEND         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT            7 0
     C*
     C     ELMAPR        IFNE      *ZERO
     C     ELEMPM        ANDNE     *ZERO
     C***  Begin Chg  ***  01/19/22  **********************
     C*****F1NETR        ANDEQ     'Y'
     C*****F1NETD        ANDLE     ELPFDT
     C     isNetRated    andeq     *on
     C***  End   Chg  ***  01/19/22  **********************
      *
     C     ELEMPM        SUB       MPEXPA        XXSEXC
     C     XXSEXC        DIV(H)    ELMAPR        XXPROR
     C*
     C*** begin add ***  11/16/23  **************************************
     C                   If        ELPfdt > xxbgdt
     C                   Z-add     ELPFDT        xxbgdt
     C                   Endif
     C                   If        ELPtdt < xxendt
     C*** begin chg ***  02/06/24  **************************************
     C*****              Z-ADD     ELPtdt        xxendt
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQNXTD
     C                   CALL      'SRPAST8'
     C                   PARM                    QQNXTD            8 0
     C                   PARM      1             QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   Z-ADD     QQNxtd        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxendt
     C*** end chg *****  02/06/24  **************************************
     C                   Endif
     C*** end add *****  11/16/23  **************************************
     C                   Z-ADD     0             XXPRTT
     C     XXENDT        SUB       XXBGDT        XXDAYS
     C                   ADD       1             XXDAYS
     C                   Z-ADD     XXBGDT        XXWKDT            7 0
     C     XXWKDT        DOWLE     XXENDT
     C*
     C     ED(3)         IFNE      *ZERO
     C     XXWKDT        ANDGE     ED(3)
     C                   Z-ADD     3             X1
     C                   ELSE
     C     ED(2)         IFNE      *ZERO
     C     XXWKDT        ANDGE     ED(2)
     C                   Z-ADD     2             X1
     C                   ELSE
     C                   Z-ADD     1             X1
     C                   ENDIF                                                  ED,2 IFNE
     C                   ENDIF                                                  ED,3 IFNE
     C*
     C     EM(X1)        DIV(H)    XXDAYS        XXPROD
     C                   ADD(H)    XXPROD        XXPRTT
     C                   ADD       1             XXWKDT
     C                   ENDDO
     C*
     C                   Z-ADD(H)  XXPRTT        XXWK54
     C     XXWK54        IFEQ      *ZERO
     C                   Z-ADD     1             XXWK54
     C                   ENDIF
     C     XXPROR        MULT(H)   XXWK54        XXPROR
     C     ELENMP        DIV(H)    ELEMPM        X1PROR
     C                   MULT(H)   X1PROR        XXPROR
     C*
     C                   ENDIF                                                            ELMAPR
     C*
     C     FNMIDY        IFNE      *ZERO
     C                   MOVE      *ON           *IN14
     C                   Z-ADD     0             XXDUED            8 0
     C                   Z-ADD     XXRPMN        XXDUMN
     C                   Z-ADD     XXRPYR        XXDUYR
     C                   ADD       1             XXDUMN
     C     XXDUMN        IFEQ      13
     C                   Z-ADD     1             XXDUMN
     C                   ADD       1             XXDUYR
     C                   ENDIF                                                  XXDUMN IFEQ
     C                   Z-ADD     FNMIDY        XXDUDA
     C                   MOVEL     XXDUED        XXNUM
     C                   MOVEL     'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL     XXNUM         XXDUDT           10
     C     PYMTDU        CAT       XXDUDT:1      XXPYDU
     C                   ENDIF                                                  FNMIDY IFEQ
     C*
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN96
     C     ELPROM        IFEQ      'M'
     C                   MOVE      *ON           *IN96
     C                   ENDIF
     C                   MOVE      ELRTCD        XXRATE
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      ELPFDT        XXPDT
     C                   MOVE      RSJRST        XXST
     C                   SELECT
     C     CLTID         WHENEQ    '093'
      *
      *  GET LARGE DEDUCTIBLE FLAG'S VALUE
      *
     C     DDKY1         SETGT     PDD01
     C     DDKY          READPE    PDD01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      DDLRGF        XXLRGF            1
     C                   ELSE
     C                   MOVE      *BLANK        XXLRGF
     C                   ENDIF
     C*
     C                   OTHER
     C                   CLEAR                   XXLRGF
     C                   ENDSL
      *
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPFD
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXPTD
      *
      * SET REPORT DATE EQUAL TO POLICY EXPIRATION DATE IF POLICY
      * EXPIRES MID-MONTH.
      *
     C     XXRPDT        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        XXRPDT
     C                   ENDIF
     C*
     C*   NOW MAKE SURE THAT THE EMPLOYER IS ACTIVE
     C*
     C                   Z-ADD     1             X1                3 0
     C     *ZERO         LOOKUP    DE(X1)                                 60
     C  N60              Z-ADD     5             X1
     C   60X1            SUB       1             X1
     C     X1            IFNE      0
     C*
     C*
     C*   THE CANCELLATION DATE MUST BE GREATER
     C*
     C                   MOVEL     XXRPYM        XXYYMM            5 0
     C*
     C     DC(X1)        IFNE      0
     C*
     C                   MOVEL     DC(X1)        XXCNDT            5 0
     C     XXYYMM        CABGT     XXCNDT        NXT
     C*
     C     XXYYMM        IFEQ      XXCNDT
     C                   MOVE      DC(X1)        XXDD              2 0
     C     XXDD          IFEQ      1
     C                   GOTO      NXT
     C                   ENDIF                                                  XXDD IFEQ
     C                   ENDIF                                                  XXYYMM IFEQ
     C*
     C     DC(X1)        IFLE      ELPFDT
     C                   GOTO      NXT
     C                   ENDIF                                                  XXCNDT IFLT
     C*
     C                   END
     C*
     C     DE(X1)        IFNE      0
     C*
     C                   MOVEL     DE(X1)        XXEFDT            5 0
     C     XXYYMM        CABLT     XXEFDT        NXT
     C*
     C                   END
     C*
     C                   MOVE      QQBGDT        XXFDT             8 0
     C                   MOVE      QQENDT        XXTDT             8 0
     C*** begin add ***  11/16/23  **************************************
     C* Reset beginning/ending dates for coding below.
     C                   Z-ADD     XXFDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXBGDT
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXENDT
     C*** end add *****  11/16/23  **************************************
     C                   MOVE      '-'           XXDASH            1
     C                   MOVE      '-'           X1DASH            1
     C                   Z-ADD     *ZERO         X1PFDT            8 0
     C                   Z-ADD     *ZERO         X1PTDT            8 0
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1PFDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1PTDT
     C*
     C     ELPFDT        IFGT      XXBGDT
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDT
     C                   ENDIF                                                  ELPFDT IFGT
     C*
     C     ELPTDT        IFLT      XXENDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTDT
     C                   ENDIF                                                  ELPTDT IFLT
     C*
     C     DC(X1)        IFLT      XXENDT
     C     DC(X1)        ANDNE     0
     C                   Z-ADD     DC(X1)        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTDT
     C                   ENDIF                                                  DC,X1 IFLT
     C     ELPTDT        IFLT      DC(X1)
     C     ELPTDT        ANDLT     XXENDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTDT
     C                   ENDIF                                                  ELPTDT IFLT
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C*
     C* WRITE S/R RECORDS IF THE POLICY TRANSFERED/RENEWED LATE.
     C*
     C                   EXSR      SRLATE
     C*
     C                   CLEAR                   BSSEFL
     C                   CLEAR                   BSSRFQ
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              63
     C*
     C     BSSRFQ        IFEQ      'Q'
     C     BSSRFQ        OREQ      'S'
     C*
     C                   Z-ADD     XXTDT         ##DATE
     C*
     C                   SELECT
     C     BSSRFQ        WHENEQ    'Q'
     C                   Z-SUB     4             ##MTHS
     C     BSSRFQ        WHENEQ    'S'
     C                   Z-SUB     7             ##MTHS
     C                   ENDSL
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    ##DATE
     C                   PARM      1             ##DAYS            5 0
     C                   PARM                    ##ERR
     C*
     C                   CALL      'SRFMTH8'
     C                   PARM                    ##DATE            8 0
     C                   PARM                    ##MTHS            3 0
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFNE      '1'
     C                   Z-ADD     ##DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ##DT              7 0
     C     ##DT          IFLT      ELPFDT
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFDT
     C                   ELSE
     C                   Z-ADD     1             ##MTHS
     C                   CALL      'SRFMTH8'
     C                   PARM                    ##DATE
     C                   PARM                    ##MTHS
     C                   PARM                    ##ERR
     C*
     C                   Z-ADD     ##DATE        XXFDT
     C*
     C                   ENDIF                                                  ##DT<ELPFDT
     C*
     C                   ENDIF                                                  ##ERR<>'1'
     C*
     C                   ENDIF                                                  BSSRFQ='Q'OR'S'
     C*
     C                   ENDSL
     C*
     C                   MOVE      ELFYR         XXPYR
     C                   MOVEL     EBANAM        XXANAM           30
      *
     C                   CALL      'WA002R'
     C                   PARM      EBCO#         #2CO#             3 0
     C                   PARM      EBFND         #2FND             3 0
     C                   PARM      *ZEROS        #2EMP#            9 0
     C                   PARM      *ZEROS        #2DIV             5 0
     C                   PARM      *ZEROS        #2FYR             3 0
     C                   PARM      EBAGT#        #2AGT#            5 0
     C                   PARM      *BLANKS       #2ANAM           60
     C                   PARM      *ZEROS        #2PROD            5 0
     C                   PARM      *BLANKS       #2PNAM           60
     C                   PARM      *ZEROS        #2AREA            3 0
     C                   PARM      *ZEROS        #2PHON            7 0
     C*
     C                   CLEAR                   XXAPH#
     C*
     C     #2AREA        IFNE      *ZEROS
     C                   MOVE      #2AREA        XXAREA            3
     C     '('           CAT       XXAREA:0      XXAPH#
     C                   CAT       ')':0         XXAPH#
     C                   ENDIF
     C*
     C     #2PHON        IFNE      *ZEROS
     C                   MOVEL     #2PHON        XXPHN3            3
     C                   MOVE      #2PHON        XXPHN4            4
     C                   CAT       XXPHN3:0      XXAPH#
     C                   CAT       '-':0         XXAPH#
     C                   CAT       XXPHN4:0      XXAPH#
     C                   ENDIF
      *
     C     CLTID         IFEQ      '139'
     C                   EXSR      SRVEND
     C                   ELSE
     C                   MOVE      *BLANKS       XXVEND
     C                   ENDIF
     C                   CLEAR                   XXPOL#
     C                   MOVEL     EMFND         XXPOL#           15
     C                   MOVE      EMEMP#        X1EMP#            9
     C                   MOVE      XXPYR         X1PYR             3
     C                   CAT       X1EMP#:0      XXPOL#
     C                   CAT       X1PYR:0       XXPOL#
     C                   MOVE      *OFF          *IN13
      *
     C     CLTID         IFEQ      '133'
     C                   MOVE      *ON           *IN13
     C                   ENDIF
     C*
     C                   MOVEL(P)  EBAGT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XPAGT#            5
     C*
     C     EXAHDR        CAT(P)    ':':0         XPAHDR           22
     C                   CAT       XPAGT#:1      XPAHDR
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '109'
      *
      *  FOR S/R EMPLOYERS
      *    - Exclude from Monthly Interim Report if indicated
      *
     C     EBSEFL        IFEQ      'S'
      *
     C                   MOVE      EBCO#         ##CO#
     C                   MOVE      ELPFDT        ##PFDT
     C                   MOVE      ELBSCD        ##BSCD
      *
     C                   CALL      'WA114R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##PFDT            7
     C                   PARM                    ##BSCD            3
     C                   PARM                    ##YYMM            5
     C                   PARM      *BLANK        ##EXFL            1
     C                   PARM      *BLANK        ##TDFL            1
      *
     C     ##EXFL        IFEQ      'Y'
      *
     C     ##TDFL        IFEQ      'Y'
     C                   EXSR      SRTRDP                                       XFER DEPOSIT
     C                   ENDIF
      *
     C                   GOTO      NXT
      *
     C                   ENDIF
      *
     C                   ENDIF                                                  EBSEFL = S
     C*
     C                   ENDSL
     C* DO NOT ALLOW A MONTHLY INTERIM TO PRINT FOR AN EMPLOYER
     C* HAVING CX DATE GREATER THAN FIRST DAY OF THE REPORTING MONTH.
     C     QQEMP#        IFEQ      *ZEROS
     C     EBCNDT        ANDNE     *ZEROS
     C     EBCNDT        ANDLE     XXFD
     C* Check if request is for single employer
     C     *IN47         IFEQ      *OFF
     C*
     C                   GOTO      NXT
     C*
     C                   ENDIF
     C                   ENDIF                                                  QQEMP# IFEQ
     C*
     C* DO NOT ALLOW A MONTHLY INTERIM TO PRINT FOR AN ONLINE EMPLOYER
     C* IF PROCESSING IN MASS.
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C*
     C     QQEMP#        IFEQ      *ZEROS
     C     BSONLI        ANDEQ     'Y'
     C*** begin add ***  07/17/23  *******************************************
     C     QQEMP#        OREQ      *ZEROS
     C     BSFREQ        ANDEQ     'W'
     C*** end add *****  07/17/23  *******************************************
     C*
     C* Check if request is for single employer
     C     *IN47         IFEQ      *OFF
     C                   GOTO      NXT
     C*
     C                   ENDIF
     C                   ENDIF                                                  QQEMP# IFEQ
     C*
     C*  FORMAT TAX TO PRINT
     C*
     C                   MOVE      *OFF          *IN75
     C     RRTXPC        IFGT      0
     C                   MOVE      *ON           *IN75
     C                   ENDIF
     C     RRTXHD        IFNE      *BLANKS
     C                   MOVEL(P)  RRTXHD        XXTHD            13
     C                   ELSE
     C                   MOVEL(P)  CON(10)       XXTHD
     C                   ENDIF
     C                   CLEAR                   TAX
     C                   CLEAR                   TTX
     C                   MOVE      '%'           TAX(8)
     C                   MOVE      '.'           TAX(3)
     C                   MOVE      RRTXPC        XXTAX             6
     C                   MOVEA     XXTAX         TTX
     C                   MOVE      TTX(1)        TAX(1)
     C                   MOVE      TTX(2)        TAX(2)
     C                   MOVE      TTX(3)        TAX(4)
     C                   MOVE      TTX(4)        TAX(5)
     C                   MOVE      TTX(5)        TAX(6)
     C                   MOVE      TTX(6)        TAX(7)
     C*
     C     TAX(1)        IFEQ      '0'
     C                   MOVE      *BLANK        TAX(1)
     C                   ENDIF                                                  TAX,1 EQ 0
     C*
     C     TAX(2)        IFEQ      '0'
     C     TAX(1)        ANDEQ     '0'
     C                   MOVE      *BLANK        TAX(2)
     C                   ENDIF                                                  TAX,2 EQ 0
     C                   MOVEA     TAX           X1TAX             8
     C*
     C*  IDENTIFY INDIVIDUAL CREDITS AND DISCOUNTS
     C*
     C                   Z-ADD     1             A                 3 0
     C                   MOVE      *OFF          *IN28
     C                   CLEAR                   HDG
     C                   Clear                   sHdg
     C                   Clear                   sFact
     C                   Clear                   sPerc
     C                   Clear                   sProF
     C                   Clear                   sProP
     C                   Clear                   sAppr
     C                   Z-add     1             b
     C                   Z-ADD     1             XXFACT           11 9
     C                   MOVE      *OFF          *IN74
     C                   CLEAR                   PRO
     C                   CLEAR                   XX1
     C                   CLEAR                   XXD
     C                   Z-ADD     XXRPDT        X1RPDT
     C                   Z-ADD     X1RPDT        XXRPFM
     C                   MOVE      01            XXRPFM
     C     XXRPYM        IFEQ      XXFPYM
     C                   MOVE      XXFPDY        XXRPFM
     C                   ENDIF
     C                   Z-ADD     X1RPDT        XXRPTO
     C                   Z-ADD     XXRPMN        X
     C                   MOVE      MD(X)         XXRPTO
     C     XXRPYM        IFEQ      XXTPYM
     C                   MOVE      XXTPDY        XXRPTO
     C                   ENDIF
     C*
     C*  INCREASED LIMITS
     C*
     C     *LIKE         DEFINE    LMPCT         XLPCT
     C*
     C                   Z-ADD     *ZERO         LMPCT
     C                   Z-ADD     *ZERO         XLPCT
     C                   MOVE      *OFF          *IN76
     C*
     C     ELILCD        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   Z-ADD     ELI2EF        XXILEX
     C                   ENDIF
     C                   ELSE
     C                   MOVE      ELILEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELILEX        ANDNE     ELILEF
     C     ELILEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELILEX        ANDEQ     *ZERO
     C*
     C                   MOVE      ELRTCD        XXRATE
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C                   MOVE      *ON           *IN76
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C     LMPCT         MULT(H)   XXPRO         XLPCT
     C*
     C                   ELSE
     C                   ADD       LMPCT         XLPCT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  INCREASED LIMITS
     C*
     C                   Z-ADD     *ZERO         LMPCT
     C*
     C     ELINL2        IFNE      *BLANK
     C*
     C                   MOVE      *BLANK        XXKEY
     C                   MOVEL     ELINL2        XXKEY
     C*
     C                   Z-ADD     ELI2EF        XXILEF
     C*
     C     ELI2EX        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        XXILEX
     C                   ELSE
     C                   Z-ADD     ELI2EX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELI2EX        ANDNE     ELI2EF
     C     ELI2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELI2EX        ANDEQ     *ZERO
     C*
     C                   MOVE      ELRTCD        XXRATE
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C                   MOVE      *ON           *IN76
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         LMPCT
     C                   ADD       LMPCT         XLPCT
     C*
     C                   ELSE
     C                   ADD       LMPCT         XLPCT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
      *
      *  FORMAT INCREASED LIMITS PERCENTAGE NUMBER
      *
     C                   MOVEL(P)  XLPCT         XXNUM
     C                   MOVEL     '3'           EDTMSK
     C                   Z-ADD     3             XXDEC
     C                   CALL      CHGNUM        CHGPL1
     C                   MOVEL(P)  XXNUM         XXILPC            7
     C                   CAT       '%':0         XXILPC
     C*
      //*** Beg Add ***** 04/18/24  **********************************
       xxFac3 = 1;
       If RRDCAP = 'M' and ELDBCR <> *Zero;
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
          If (xxIlef <= xxRpto and xxIlex >= xxRpfm
           and ELsdex <> ELsdef and ELsdex <> *Zero)
           or (xxIlef <= xxRpto and xxIlex >= xxRpfm and ELsdex = *Zero);
             *In28 = *On;
             If ELdbcr > 1;
                If RRdbhd = *Blank;
                   strTmp = Con(7);
                   xxWk20 = Con(7);
                Else;
                   xxWk20 = RRdbhd;
                   strTmp = xxWk20;
                Endif;
             Else;
                If RRcrhd = *Blanks;
                   strTmp = Con(8);
                   xxWk20 = Con(8);
                Else;
                   xxWk20 = RRcrhd;
                   strTmp  = xxWk20;
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
             Hdg(A) = strTmp;
             sHdg(b) = xxWk20;
             sFact(b) = ELdbcr;
             b += 1;
             clear strTmp;

             If xxIlef > xxRpfm or xxIlex < xxRpto;
                If xxIlef < xxRpfm;
                   xxFrDt = xxRpfm;
                Else;
                   xxFrDt = xxIlef;
                Endif;
                If xxIlex > xxRpto;
                   xxToDt = xxRpto;
                Else;
                   xxToDt = xxIlex;
                Endif;

                Exsr  SRPRO;
                ELdbcr = ELdbcr - 1;
                Eval(h) ELdbcr = ELdbcr * xxPro;
                ELdbcr = ELdbcr + 1;
                sProF(b) = ELdbcr;
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
                Pro(A) = strTmp;
                Clear strTmp;
             Endif;
             xxFac3 = ELdbcr - 1;
             xxPro = XLPct *.01;
             xxPro = xxPro + 1;
             xxFac3 = xxFac3 + xxPro;
             Eval(h) xxFac3 = xxFac3/xxPro;
             A = A + 1;
          Endif;
       Endif;
      //*** End Add ***** 04/18/24  **********************************

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
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVTP        ANDEQ     'B'
     C     ELWVFL        ANDEQ     'M'
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEL(P)  RRWVHD        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELWVPC        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELWVPC        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C*  TO CALCULATE EFFECT OF WAIVER CHG ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE WAIVER CHG PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     ELWVPC        MULT      .01           XXFAC1
     C     XLPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFAC1
     C                   DIV(H)    XXPRO         XXFAC1
     C                   ADD       1             A
     C                   ENDIF
     C*
     C*
     C*  DEDUCTIBLE DISCOUNT
     C*
     C     ELDDCT        IFNE      *ZERO
     C     RRRDPR        ANDEQ     'M'
     C     LRGDFL        ANDNE     'Y'
     C     XXLRGF        ANDEQ     *BLANK
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDNE     ELDCEF
     C     ELDCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEA     CON(1)        TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(1)        sHdg(b)
     C                   Z-add     ELDDCT        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDDCT
     C                   Z-add     ELDDCT        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     ELDDCT        MULT      -.01          XXFACT
     C     XLPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFACT
     C                   DIV(H)    XXPRO         XXFACT
     C                   ADD       1             A
     C                   ENDIF
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      EL2DEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEA     CON(13)       TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(13)       sHdg(b)
     C                   Z-add     ELDD2P        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDD2P
     C                   Z-add     ELDD2P        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C*  TO CALCULATE EFFECT OF DEDUCTIBLE ON PREMIUM AFTER INCREASED
     C*  LIMITS, WE WILL NEED TO FACTOR THE LIMITS PREMIUM IN BECAUSE
     C*  THE DEDUCTIBLE PERCENTAGE ACTUALLY APPLIES TO MANUAL
     C*
     C     ELDD2P        MULT      -.01          XXFACT
     C     XLPCT         MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   ADD       XXPRO         XXFACT
     C                   DIV(H)    XXPRO         XXFACT
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      LDEXP1        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      LDEXP2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
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
      //*** End Add ***** 04/18/24  *********************************
       xxFact = xxFact + xxFac3;
       xxFact = xxFact - 1;
      //*** End Add ***** 04/18/24  ********************************
     C*
     C*  HCN CREDIT
     C*
     C     ELHCN         IFNE      *ZERO
     C     ELHCN         ANDNE     1
     C*
     C                   Z-ADD     ELHCN         SVHCN
     C                   Z-ADD     1             XXHCN
     C                   MOVE      *BLANK        XXHCNF            1
     C                   Z-ADD     XXFACT        SVFACT           11 9
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     HCOUT         ANDNE     HCIN
     C     HCOUT         ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     HCOUT         ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVE      'Y'           XXHCNF
     C                   MOVEA     CON(18)       TMP
      *
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELHCN         XXASP1            5
     C                   Movel(p)  con(18)       sHdg(b)
     C                   Z-add     ELHCN         sFact(b)
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(22)
     C                   MOVE      NUM(2)        TMP(24)
     C                   MOVE      NUM(3)        TMP(25)
     C                   MOVE      NUM(4)        TMP(26)
     C                   MOVE      NUM(5)        TMP(27)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(18)       sHdg(b)
     C                   Z-add     ELHCN         sFact(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   SUB       1             ELHCN
     C                   MULT(H)   XXPRO         ELHCN
     C                   ADD       ELHCN         XXHCN
     C                   Z-add     xxHCN         sProF(b)
     C*
     C                   MOVEA     ')**'         TMP(8)
     C                   MOVE      '.'           TMP(3)
     C                   MOVE      XXHCN         XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(1)        TMP(2)
     C*
     C                   MOVE      NUM(2)        TMP(4)
     C                   MOVE      NUM(3)        TMP(5)
     C                   MOVE      NUM(4)        TMP(6)
     C                   MOVE      NUM(5)        TMP(7)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C*
     C                   ELSE
     C*
     C                   SUB       1             ELHCN
     C                   ADD       ELHCN         XXHCN
     C*
     C                   ENDIF                                                  XXILEF IFGT
     C*
     C                   ENDIF                                                  XXILEF IFLE
     C*
     C     HCKY          READE     PHC01                                  60
     C                   ENDDO
     C*
     C     XXHCNF        IFEQ      'Y'
     C*
     C     XXHCN         MULT(H)   XXFACT        XXFACT
     C     XXWVFL        IFEQ      1
     C                   MULT(H)   XXHCN         XXWFAC
     C                   ENDIF
     C*
     C                   ADD       1             A
     C                   ENDIF                                                  XXHCNF IFEQ
     C*
     C                   Z-ADD     SVHCN         ELHCN
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELMCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELMCEX        ANDNE     ELMCEF
     C     ELMCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELMCEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C*
     C     RR1AHD        IFEQ      *BLANKS
     C                   MOVEA     CON(2)        TMP
     C                   MOVEL     CON(2)        XXWK20
     C                   ELSE
     C                   MOVEL(P)  RR1AHD        XXWK20           20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C*
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELASPM        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELASPM        sPerc(b)
     c                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASPM
     C                   Z-add     ELASPM        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELASPM        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELASPM        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C     XXWVFL        IFEQ      1
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ADD       1             A
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELSCEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELSCEX        ANDNE     ELSCEF
     C     ELSCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELSCEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C*
     C     RR1BHD        IFEQ      *BLANKS
     C                   MOVEA     CON(3)        TMP
     C                   MoveL     CON(3)        xxWk20
     C                   ELSE
     C                   MOVEL(P)  RR1BHD        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELASPS        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELASPS        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASPS
     C                   Z-add     ELASPS        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELASPS        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELASPS        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C     XXWVFL        IFEQ      1
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION
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
     C*
     C     ELDVEX        IFEQ      *ZERO
     C     ELD1S2        ANDNE     *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD1D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD            8 0
     C*CALL API TO WITH DAYS= -1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-SUB     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
     C                   ELSE
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C*
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(4)        TMP
     C                   Movel     Con(4)        xxWk20
     C                   ELSE
     C                   MOVEL(P)  RRDV1H        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C*
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELASP2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C                   Z-add     ELASP2        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C     XXWVFL        IFEQ      1
     C     RRDV1L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'J'
     C     ELD1S2        ORNE      *ZERO
     C     RRDV1L        ANDEQ     'M'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELDVEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD            8 0
     C*CALL API TO WITH DAYS= 1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-ADD     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEF
     C*
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C*
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(4):1      XXWK25           25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV1H        XXWK25
     C     @2NPER        CAT       XXWK25:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELD1S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk25        sHdg(b)
     C                   Z-add     ELD1S2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C                   Z-add     ELD1S2        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELD1S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV1L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION
     C*
     C     ELDEV2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C**
     C     ELDEV2        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDEQ     '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDEQ     '025'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDEQ     '136'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDEQ     '136'
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C*
     C     ELD2EX        IFEQ      *ZERO
     C     ELD2S2        ANDNE     *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD2D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD            8 0
     C*CALL API TO WITH DAYS= -1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-SUB     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
     C                   ELSE
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C*
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(17)       tmp
     C                   Else
     C     lo:up         Xlate     RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   ENDIF
     C*
     C                   Move      ELstFc        xxAsp8            8
     C                   MoveA     xxAsp8        num8
     C     num8(1)       IfNE      '0'
     C                   Move      num8(1)       tmp(18)
     C                   EndIf
     C                   Move      num8(2)       tmp(19)
     C                   Move      '.'           tmp(20)
     C                   Move      num8(3)       tmp(21)
     C                   Move      num8(4)       tmp(22)
     C                   Move      num8(5)       tmp(23)
     C                   Move      num8(6)       tmp(24)
     C                   Move      num8(7)       tmp(25)
     C                   Move      num8(8)       tmp(26)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(19)       tmp
     C                   MoveA     con(19)       xxWk20
     C                   Else
     C                   Movel(P)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   EndIf                                                  RRDBHD IFEQ
      *
     C                   Move      ELdev2F       xxAsp1            5
     C                   MoveA     xxAsp1        num
     C                   Move      num(1)        tmp(22)
     C                   Move      '.'           tmp(23)
     C                   Move      num(2)        tmp(24)
     C                   Move      num(3)        tmp(25)
     C                   Move      num(4)        tmp(26)
     C                   Move      num(5)        tmp(27)
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(9)        tmp
     C                   MoveL     con(9)        xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   EndIf
     C*
     C                   MOVE      ELdev2        xxAsp1            5
     C*
     C                   MoveA     xxAsp1        NUM
     C     num(1)        IfNE      '0'
     C                   Move      num(1)        tmp(20)
     C                   EndIf
     C*
     C     num(2)        IfNE      '0'
     C     num(1)        OrGT      '0'
     C                   Move      num(2)        tmp(21)
     C                   EndIf
     C*
     C                   Move      num(3)        tmp(22)
     C                   Move      '.'           tmp(23)
     C                   Move      num(4)        tmp(24)
     C                   Move      num(5)        tmp(25)
     C                   Move      '%'           tmp(26)
     C                   Endsl
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELdev2        sPerc(b)
     C                   Z-add     ELdev2F       sFact(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   Sub       1             ELstFc
     C                   Mult(H)   xxPro         ELstFc
     C                   Add       1             ELstFc
     C*
     C                   Move      ELstFc        xxAsp8            8
     C                   MoveA     xxAsp8        num8
     C     num8(1)       IFNE      '0'
     C                   Move      '('           tmp(1)
     C                   Move      num8(1)       tmp(2)
     C                   ELSE
     C                   Move      '('           tmp(2)
     C                   END
     C                   Move      num8(2)       tmp(3)
     C                   Move      '.'           tmp(4)
     C                   Move      num8(3)       tmp(5)
     C                   Move      num8(4)       tmp(6)
     C                   Move      num8(5)       tmp(7)
     C                   Move      num8(6)       tmp(8)
     C                   Move      num8(7)       tmp(9)
     C                   Move      ')'           tmp(10)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   Sub       1             ELdev2F
     C                   Mult(H)   xxPro         ELdev2F
     C                   Add       1             ELdev2F
     C*
     C                   Move      ELdev2F       xxAsp1
     C                   MoveA     xxAsp1        num
     C                   Move      '('           tmp(1)
     C                   Move      num(2)        tmp(2)
     C                   Move      '.'           tmp(3)
     C                   Move      num(3)        tmp(4)
     C                   Move      num(4)        tmp(5)
     C                   Move      num(5)        tmp(6)
     C                   MoveA     ')'           tmp(7)
     C*>>>>>>>>>>>>>>>>> Default Devaition #2
     C                   Other
     C                   Mult(H)   xxPro         ELdev2
     C                   Z-Add     ELdev2        sProP(b)
     C*
     C                   Move      ELdev2        xxAsp1            5
     C                   MoveA     xxAsp1        num
     C     num(2)        IfNE      '0'
     C                   Move      '('           tmp(1)
     C                   Move      num(2)        tmp(2)
     C                   Else
     C                   Move      '('           tmp(2)
     C                   End
     C                   Move      num(3)        tmp(3)
     C                   Move      '.'           tmp(4)
     C                   Move      num(4)        tmp(5)
     C                   Move      num(5)        tmp(6)
     C                   MoveA     '%)**'        tmp(7)
     C                   Endsl
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C                   Select
     C                   When      cltID = '025'
     C                   Mult(H)   ELstFc        xxFact
     C                   Z-Add     ELstFc        xxPro
     C                   When      cltID = '136'
     C                   Mult(H)   ELdev2F       xxFact
     C                   Z-Add     ELdev2F       xxPro
     C                   Other
     C     ELdev2        Mult      -.01          xxPro
     C                   Add       1             xxpro
     C                   Mult(H)   xxPro         xxFact
     C                   Endsl
     C     XXWVFL        IFEQ      1
     C     RRDV2L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION PERIOD 2
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDNE     '025'
     C     ELD2S2        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'J'
     C     CLTID         ANDEQ     '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'M'
     C     CLTID         ANDEQ     '025'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD2EX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD
     C*CALL API TO WITH DAYS= 1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-ADD     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN28
     C*
     C     CLTID         IFNE      '025'
     C*
     C     RRDV2H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(9):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C                   Movel(p)  RRDV2H        XXWK25
     C     @2NPER        CAT       XXWK25:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELD2S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(17)       TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV2H        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C*
     C                   MOVE      '.'           TMP(20)
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(18)
     C                   END
     C                   MOVE      NUM8(2)       TMP(19)
     C                   MOVE      NUM8(3)       TMP(21)
     C                   MOVE      NUM8(4)       TMP(22)
     C                   MOVE      NUM8(5)       TMP(23)
     C                   MOVE      NUM8(6)       TMP(24)
     C                   MOVE      NUM8(7)       TMP(25)
     C                   MOVE      NUM8(8)       TMP(26)
     C*
     C                   ENDIF                                                  CLTID <> '025'
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk25        sHdg(b)
     C                   Z-add     ELD2S2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C*
     C     CLTID         IFNE      '025'
     C                   MULT(H)   XXPRO         ELD2S2
     C                   Z-add     ELD2S2        sProP(b)
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELD2S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C*FOR CLIENT 025
     C                   ELSE
     C                   SUB       1             ELSTFC
     C                   MULT(H)   XXPRO         ELSTFC
     C                   ADD       1             ELSTFC
     C*
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM8(1)       TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM8(2)       TMP(3)
     C                   MOVE      NUM8(3)       TMP(5)
     C                   MOVE      NUM8(4)       TMP(6)
     C                   MOVE      NUM8(5)       TMP(7)
     C                   MOVE      NUM8(6)       TMP(8)
     C                   MOVE      NUM8(7)       TMP(9)
     C                   MOVE      ')'           TMP(10)
     C*
     C                   ENDIF                                                  CLTID <> '025'
     C*
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     CLTID         IFNE      '025'
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*FOR CLIENT 025
     C                   ELSE
     C                   MULT(H)   ELSTFC        XXFACT
     C                   Z-ADD     ELSTFC        XXPRO
     C                   ENDIF                                                  CLTID <> '025'
     C*
     C     XXWVFL        IFEQ      1
     C     RRDV2L        ANDEQ     'J'
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVTP        ANDEQ     'B'
     C     ELWVFL        ANDEQ     'J'
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEL(P)  RRWVHD        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELWVPC        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELWVPC        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C                   ADD       1             A
     C                   ENDIF
     C*
      *
      * LARGE DEDUCTIBLE AT SUBJECT PREMIUM LEVEL
      *
     C                   MOVE      'J'           XXPLV1            1
     C                   MOVE      'J'           XXPLV2            1
     C                   EXSR      APLGDD
     C*
     C                   Z-ADD(H)  XXFACT        XXDIS1            9 6
     C                   Z-ADD     1             XXFACT           11 9
     C                   MOVE      XXDIS1        XXWK9A            9
     C                   MOVEA     XXWK9A        TMP
     C                   MOVE      '.'           XX1(4)
     C     TMP(1)        IFNE      *ZERO
     C                   MOVE      TMP(1)        XX1(1)
     C                   MOVE      TMP(2)        XX1(2)
     C                   MOVE      TMP(3)        XX1(3)
     C                   ELSE
     C     TMP(2)        IFNE      *ZERO
     C                   MOVE      TMP(2)        XX1(2)
     C                   MOVE      TMP(3)        XX1(3)
     C                   ELSE
     C     TMP(3)        IFNE      *ZERO
     C                   MOVE      TMP(3)        XX1(3)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      TMP(4)        XX1(5)
     C                   MOVE      TMP(5)        XX1(6)
     C                   MOVE      TMP(6)        XX1(7)
     C     TMP(9)        IFNE      *ZERO
     C                   MOVE      TMP(7)        XX1(8)
     C                   MOVE      TMP(8)        XX1(9)
     C                   MOVE      TMP(9)        XX1(10)
     C                   ELSE
     C     TMP(8)        IFNE      *ZERO
     C                   MOVE      TMP(7)        XX1(8)
     C                   MOVE      TMP(8)        XX1(9)
     C                   ELSE
     C     TMP(7)        IFNE      *ZERO
     C                   MOVE      TMP(7)        XX1(8)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   MOVEA     XX1           XXDFBM           10
     C*
     C*  SCHEDULE DB/CR
     C*
     C*
     C*
     C*
      *
      *
     C*
     C*
     C*
     C*
     C     RRDCAP        IFEQ      ' '
     C                   EXSR      CALDBR
     C                   ENDIF                                                  ELDBCR IFNE
     C*
     C*  DEVIATION - APPLIED TO MODIFIED
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
     C*
     C     ELDVEX        IFEQ      *ZERO
     C     ELD1S2        ANDNE     *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD1D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD
     C*CALL API TO WITH DAYS= -1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-SUB     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
     C                   ELSE
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C                   ENDIF
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C     RRDV1H        IFEQ      *BLANKS
     C                   MOVEA     CON(4)        TMP
     C                   Movel     CON(4)        xxWk20
     C                   ELSE
     C                   MOVEL(P)  RRDV1H        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C     *IN45         IFEQ      '1'
     C     ELASP2        DIV       100           XXASP2            5 4
     C     1             SUB       XXASP2        XXASP2
     C*
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      XXASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(22)
     C                   MOVE      NUM(2)        TMP(24)
     C                   MOVE      NUM(3)        TMP(25)
     C                   MOVE      NUM(4)        TMP(26)
     C                   MOVE      NUM(5)        TMP(27)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELASP2        sPerc(b)
     C                   Eval      b = b + 1
     C     NUM(5)        IFEQ      '0'
     C     XXEXMD        ANDEQ     0
     C                   MOVE      *ZEROS        XEXMD1            5 4
     C                   MOVE      *ON           *IN59
     C                   ENDIF
     C                   CLEAR                   TMP
     C                   ELSE
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELASP2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C                   Z-add     ELASP2        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION1 2ND PERIOD- APPLIED TO MODIFIED
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'E'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELDVEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD            8 0
     C*CALL API TO WITH DAYS= 1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-ADD     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEF
     C*
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN28
     C                   MOVE      *BLANKS       XXWK25
     C*
     C     RRDV1H        IFEQ      *BLANKS
     C     @2NPER        CAT       CON(4):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV1H        XXWK25
     C     @2NPER        CAT       XXWK25:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C     *IN45         IFEQ      '1'
     C     ELD1S2        DIV       100           XXASP2            5 4
     C     1             SUB       XXASP2        XXASP2
     C*
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      XXASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(22)
     C                   MOVE      NUM(2)        TMP(24)
     C                   MOVE      NUM(3)        TMP(25)
     C                   MOVE      NUM(4)        TMP(26)
     C                   MOVE      NUM(5)        TMP(27)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk25        sHdg(b)
     C                   Z-add     ELD1S2        sPerc(b)
     C                   Eval      b = b + 1
     C*
     C     NUM(5)        IFEQ      '0'
     C     XXEXMD        ANDEQ     0
     C                   MOVE      *ZEROS        XEXMD1            5 4
     C                   MOVE      *ON           *IN59
     C                   ENDIF
     C*
     C                   CLEAR                   TMP
     C                   ELSE
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELD1S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk25        sHdg(b)
     C                   Z-add     ELD1S2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C                   ENDIF
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C                   Z-add     ELD1S2        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELD1S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION - APPLIED TO MODIFIED
     C*
     C     ELDEV2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C     CLTID         ANDEQ     '025'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C     CLTID         ANDEQ     '136'
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C*
     C     ELD2EX        IFEQ      *ZERO
     C     ELD2S2        ANDNE     *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD2D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD            8 0
     C*CALL API TO WITH DAYS= -1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-SUB     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
     C                   ELSE
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(17)       tmp
     c                   MoveL     con(17)       xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        XXwK20
     C                   MoveA     xxWk20        tmp
     C                   ENDIF
     C*
     C                   Move      ELstFc        xxAsp8            8
     C                   MoveA     xxAsp8        num8
     C     num8(1)       IfNE      '0'
     C                   Move      num8(1)       tmp(18)
     C                   EndIf
     C                   Move      num8(2)       tmp(19)
     C                   Move      '.'           tmp(20)
     C                   Move      num8(3)       tmp(21)
     C                   Move      num8(4)       tmp(22)
     C                   Move      num8(5)       tmp(23)
     C                   Move      num8(6)       tmp(24)
     C                   Move      num8(7)       tmp(25)
     C                   Move      num8(8)       tmp(26)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(19)       tmp
     C                   Movel     con(19)       xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   EndIf                                                  RRDBHD IFEQ
      *
     C                   Move      ELdev2F       xxAsp1            5
     C                   MoveA     xxAsp1        num
     C                   Move      num(1)        tmp(22)
     C                   Move      '.'           tmp(23)
     C                   Move      num(2)        tmp(24)
     C                   Move      num(3)        tmp(25)
     C                   Move      num(4)        tmp(26)
     C                   Move      num(5)        tmp(27)
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(9)        tmp
     C                   Movel     con(9)        xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   EndIf
     C*
     C                   MOVE      ELdev2        xxAsp1            5
     C                   MoveA     xxAsp1        NUM
     C     num(1)        IfNE      '0'
     C                   Move      num(1)        tmp(20)
     C                   EndIf
     C*
     C     num(2)        IfNE      '0'
     C     num(1)        OrGT      '0'
     C                   Move      num(2)        tmp(21)
     C                   EndIf
     C*
     C                   Move      num(3)        tmp(22)
     C                   Move      '.'           tmp(23)
     C                   Move      num(4)        tmp(24)
     C                   Move      num(5)        tmp(25)
     C                   Move      '%'           tmp(26)
     C                   Endsl
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELdev2        sPerc(b)
     C                   Z-add     ELdev2F       sFact(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   Sub       1             ELstFc
     C                   Mult(H)   xxPro         ELstFc
     C                   Add       1             ELstFc
     C*
     C                   Move      ELstFc        xxAsp8            8
     C                   MoveA     xxAsp8        num8
     C     num8(1)       IFNE      '0'
     C                   Move      '('           tmp(1)
     C                   Move      num8(1)       tmp(2)
     C                   ELSE
     C                   Move      '('           tmp(2)
     C                   END
     C                   Move      num8(2)       tmp(3)
     C                   Move      '.'           tmp(4)
     C                   Move      num8(3)       tmp(5)
     C                   Move      num8(4)       tmp(6)
     C                   Move      num8(5)       tmp(7)
     C                   Move      num8(6)       tmp(8)
     C                   Move      num8(7)       tmp(9)
     C                   Move      ')'           tmp(10)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   Sub       1             ELdev2F
     C                   Mult(H)   xxPro         ELdev2F
     C                   Add       1             ELdev2F
     C*
     C                   Move      ELdev2F       xxAsp1
     C                   MoveA     xxAsp1        num
     C                   Move      '('           tmp(1)
     C                   Move      num(2)        tmp(2)
     C                   Move      '.'           tmp(3)
     C                   Move      num(3)        tmp(4)
     C                   Move      num(4)        tmp(5)
     C                   Move      num(5)        tmp(6)
     C                   MoveA     ')'           tmp(7)
     C*>>>>>>>>>>>>>>>>> Default Devaition #2
     C                   Other
     C                   Mult(H)   xxPro         ELdev2
     C                   Z-Add     ELdev2        sProP(b)
     C*
     C                   Move      ELdev2        xxAsp1            5
     C                   MoveA     xxAsp1        num
     C     num(2)        IfNE      '0'
     C                   Move      '('           tmp(1)
     C                   Move      num(2)        tmp(2)
     C                   Else
     C                   Move      '('           tmp(2)
     C                   End
     C                   Move      num(3)        tmp(3)
     C                   Move      '.'           tmp(4)
     C                   Move      num(4)        tmp(5)
     C                   Move      num(5)        tmp(6)
     C                   MoveA     '%)**'        tmp(7)
     C                   Endsl
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C                   Select
     C                   When      cltID = '025'
     C                   Mult(H)   ELstFc        xxFact
     C                   Z-Add     ELstFc        xxPro
     C                   When      cltID = '136'
     C                   Mult(H)   ELdev2F       xxFact
     C                   Z-Add     ELdev2F       xxPro
     C                   Other
     C     ELdev2        Mult      -.01          xxPro
     C                   Add       1             xxpro
     C                   Mult(H)   xxPro         xxFact
     C                   Endsl
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION 2N PERIOD- APPLIED TO MODIFIED
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C*
     C     CLTID         ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'E'
     C     CLTID         ANDEQ     '025'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD2EX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD            8 0
     C*CALL API TO WITH DAYS= 1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-ADD     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN28
     C     CLTID         IFNE      '025'
     C*
     C     RRDV2H        IFEQ      *BLANK
     C     @2NPER        CAT       CON(9):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV2H        XXWK25
     C     @2NPER        CAT       XXWK25:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELD2S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(17)       TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV2H        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C*
     C                   MOVE      '.'           TMP(20)
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(18)
     C                   END
     C*
     C                   MOVE      NUM8(2)       TMP(19)
     C                   MOVE      NUM8(3)       TMP(21)
     C                   MOVE      NUM8(4)       TMP(22)
     C                   MOVE      NUM8(5)       TMP(23)
     C                   MOVE      NUM8(6)       TMP(24)
     C                   MOVE      NUM8(7)       TMP(25)
     C                   MOVE      NUM8(8)       TMP(26)
     C*
     C                   ENDIF                                                  CLTID <> '025'
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk25        sHdg(b)
     C                   Z-add     ELd2s2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C*
     C     CLTID         IFNE      '025'
     C*
     C                   MULT(H)   XXPRO         ELD2S2
     C                   Z-add     ELD2S2        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELD2S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C*FOR CLIENT 025
     C                   ELSE
     C                   SUB       1             ELSTFC
     C                   MULT(H)   XXPRO         ELSTFC
     C                   ADD       1             ELSTFC
     C*
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM8(1)       TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C*
     C                   MOVE      NUM8(2)       TMP(3)
     C                   MOVE      NUM8(3)       TMP(5)
     C                   MOVE      NUM8(4)       TMP(6)
     C                   MOVE      NUM8(5)       TMP(7)
     C                   MOVE      NUM8(6)       TMP(8)
     C                   MOVE      NUM8(7)       TMP(9)
     C                   MOVE      ')'           TMP(10)
     C*
     C                   ENDIF                                                  CLTID <> '025'
     C*
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     CLTID         IFNE      '025'
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C     CLTID         IFEQ      '071'
     C     SVFACT        MULT(H)   XXPRO         S2FACT           11 9
     C     SVFACT        SUB       S2FACT        SVFACT
     C     XXFACT        SUB       SVFACT        XXFACT
     C                   ELSE
     C*
     C                   MULT(H)   XXPRO         XXFACT
     C                   ENDIF                                                  CLTID IFEQ
     C*FOR CLIENT 025
     C                   ELSE                                                   CLTID <> '025'
     C                   MULT(H)   ELSTFC        XXFACT
     C                   Z-ADD     ELSTFC        XXPRO
     C                   ENDIF                                                  CLTID <> '025'
     C*
     C     XXWVFL        IFLT      3
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C     RRDCAP        IFEQ      'Y'
     C                   EXSR      CALDBR
     C                   ENDIF
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVTP        ANDEQ     'B'
     C     ELWVFL        ANDEQ     'E'
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEL(P)  RRWVHD        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELWVPC        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELWVPC        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C                   ADD       1             A
     C                   ENDIF
     C*
      *
      * LARGE DEDUCTIBLE AT SUBJECT PREMIUM LEVEL
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELCREX        ANDNE     ELCREF
     C     ELCREX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELCREX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C     RRCCHD        IFNE      *BLANKS
     C     CLTID         ANDEQ     '132'
     C                   MOVEL     RRCCHD        CON(5)
     C                   MOVEA     CON(5)        TMP
     C                   ELSE
     C                   MOVEA     CON(5)        TMP
     C                   ENDIF
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELASP1        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(5)        sHdg(b)
     C                   Z-add     ELASP1        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP1
     C                   Z-add     ELASP1        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELASP1        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELASP1        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C                   ENDIF
     C*
     C*  DEDUCTIBLE DISCOUNT
     C*
     C                   Z-ADD     *ZERO         XXDEDF           11 9
     C*
     C     ELDDCT        IFNE      *ZERO
     C     RRRDPR        ANDEQ     'S'
     C     XXLRGF        ANDEQ     *BLANK
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDNE     ELDCEF
     C     ELDCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEA     CON(1)        TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(1)        sHdg(b)
     C                   Z-add     ELDDCT        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDDCT
     C                   Z-add     ELDDCT        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELDDCT        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXDEDF
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         X1DEDF           11 9
     C                   SUB       X1DEDF        XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         XXD2DF           11 9
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      EL2DEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDNE     EL2DEF
     C     EL2DEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEA     CON(13)       TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(13)       sHdg(b)
     C                   Z-add     ELDD2P        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDD2P
     C                   Z-add     ELDD2P        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELDD2P        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXD2DF
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         X1DEDF
     C                   SUB       X1DEDF        XXWFAC
     C                   ENDIF
     C                   ADD       1             A
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
     C*
     C     ELDVEX        IFEQ      *ZERO
     C     ELD1S2        ANDNE     *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD1D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD            8 0
     C*CALL API TO WITH DAYS= -1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-SUB     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
     C                   ELSE
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C                   ENDIF
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C     RRDV1H        IFEQ      *BLANK
     C                   MOVEA     CON(4)        TMP
     C                   Movel     CON(4)        xxWk20
     C                   ELSE
     C                   MOVEL     RRDV1H        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELASP2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C                   Z-add     ELASP2        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION1 PERIOD2  - APPLIED TO STANDARD
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'S'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELDVEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD
     C*CALL API TO WITH DAYS= 1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-ADD     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEF
     C*
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN28
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANK
     C     @2NPER        CAT       CON(4):1      XXWK25           25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV1H        XXWK25
     C     @2NPER        CAT       XXWK25:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELD1S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk25        sHdg(b)
     C                   Z-add     ELD1S2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C                   Z-add     ELD1S2        sProP(b)
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELD1S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION - APPLIED TO STANDARD
     C*
     C     ELDEV2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C     CLTID         ANDEQ     '025'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C     CLTID         ANDEQ     '136'
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C*
     C     ELD2EX        IFEQ      *ZERO
     C     ELD2S2        ANDNE     *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD2D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD
     C*CALL API TO WITH DAYS= -1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-SUB     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
     C                   ELSE
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(17)       tmp
     c                   MoveL     con(17)       xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   ENDIF
     C*
     C                   Move      ELstFc        xxAsp8            8
     C                   MoveA     xxAsp8        num8
     C     num8(1)       IfNE      '0'
     C                   Move      num8(1)       tmp(18)
     C                   EndIf
     C                   Move      num8(2)       tmp(19)
     C                   Move      '.'           tmp(20)
     C                   Move      num8(3)       tmp(21)
     C                   Move      num8(4)       tmp(22)
     C                   Move      num8(5)       tmp(23)
     C                   Move      num8(6)       tmp(24)
     C                   Move      num8(7)       tmp(25)
     C                   Move      num8(8)       tmp(26)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(19)       tmp
     c                   MoveL     con(19)       xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   EndIf                                                  RRDBHD IFEQ
      *
     C                   Move      ELdev2F       xxAsp1            5
     C                   MoveA     xxAsp1        num
     C                   Move      num(1)        tmp(22)
     C                   Move      '.'           tmp(23)
     C                   Move      num(2)        tmp(24)
     C                   Move      num(3)        tmp(25)
     C                   Move      num(4)        tmp(26)
     C                   Move      num(5)        tmp(27)
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(9)        tmp
     C                   Movel     con(9)        xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   EndIf
     C*
     C                   MOVE      ELdev2        xxAsp1            5
     C                   MoveA     xxAsp1        NUM
     C     num(1)        IfNE      '0'
     C                   Move      num(1)        tmp(20)
     C                   EndIf
     C*
     C     num(2)        IfNE      '0'
     C     num(1)        OrGT      '0'
     C                   Move      num(2)        tmp(21)
     C                   EndIf
     C*
     C                   Move      num(3)        tmp(22)
     C                   Move      '.'           tmp(23)
     C                   Move      num(4)        tmp(24)
     C                   Move      num(5)        tmp(25)
     C                   Move      '%'           tmp(26)
     C                   Endsl
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELdev2        sPerc(b)
     C                   Z-add     ELdev2F       sFact(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   Sub       1             ELstFc
     C                   Mult(H)   xxPro         ELstFc
     C                   Add       1             ELstFc
     C*
     C                   Move      ELstFc        xxAsp8            8
     C                   MoveA     xxAsp8        num8
     C     num8(1)       IFNE      '0'
     C                   Move      '('           tmp(1)
     C                   Move      num8(1)       tmp(2)
     C                   ELSE
     C                   Move      '('           tmp(2)
     C                   END
     C                   Move      num8(2)       tmp(3)
     C                   Move      '.'           tmp(4)
     C                   Move      num8(3)       tmp(5)
     C                   Move      num8(4)       tmp(6)
     C                   Move      num8(5)       tmp(7)
     C                   Move      num8(6)       tmp(8)
     C                   Move      num8(7)       tmp(9)
     C                   Move      ')'           tmp(10)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   Sub       1             ELdev2F
     C                   Mult(H)   xxPro         ELdev2F
     C                   Add       1             ELdev2F
     C*
     C                   Move      ELdev2F       xxAsp1
     C                   MoveA     xxAsp1        num
     C                   Move      '('           tmp(1)
     C                   Move      num(2)        tmp(2)
     C                   Move      '.'           tmp(3)
     C                   Move      num(3)        tmp(4)
     C                   Move      num(4)        tmp(5)
     C                   Move      num(5)        tmp(6)
     C                   MoveA     ')'           tmp(7)
     C*>>>>>>>>>>>>>>>>> Default Devaition #2
     C                   Other
     C                   Mult(H)   xxPro         ELdev2
     C                   Z-Add     ELdev2        sProP(b)
     C*
     C                   Move      ELdev2        xxAsp1            5
     C                   MoveA     xxAsp1        num
     C     num(2)        IfNE      '0'
     C                   Move      '('           tmp(1)
     C                   Move      num(2)        tmp(2)
     C                   Else
     C                   Move      '('           tmp(2)
     C                   End
     C                   Move      num(3)        tmp(3)
     C                   Move      '.'           tmp(4)
     C                   Move      num(4)        tmp(5)
     C                   Move      num(5)        tmp(6)
     C                   MoveA     '%)**'        tmp(7)
     C                   Endsl
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION 2ND PERIOD- APPLIED TO STANDARD
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C*
     C     CLTID         ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'S'
     C     CLTID         ANDEQ     '025'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD2EX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD
     C*CALL API TO WITH DAYS= 1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-ADD     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C*
     C     CLTID         IFNE      '025'
     C*
     C     RRDV2H        IFEQ      *BLANK
     C     @2NPER        CAT       CON(9):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV2H        XXWK25
     C     @2NPER        CAT       XXWK25:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELD2S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(17)       TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV2H        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C*
     C                   MOVE      '.'           TMP(20)
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(18)
     C                   END
     C*
     C                   MOVE      NUM8(2)       TMP(19)
     C                   MOVE      NUM8(3)       TMP(21)
     C                   MOVE      NUM8(4)       TMP(22)
     C                   MOVE      NUM8(5)       TMP(23)
     C                   MOVE      NUM8(6)       TMP(24)
     C                   MOVE      NUM8(7)       TMP(25)
     C                   MOVE      NUM8(8)       TMP(26)
     C*
     C                   ENDIF                                                  CLTID <> '025'
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk25        sHdg(b)
     C                   Z-add     ELD2S2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C*
     C     CLTID         IFNE      '025'
     C                   MULT(H)   XXPRO         ELD2S2
     C                   Z-add     ELD2S2        sProP(b)
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELD2S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C*FOR CLIENT 025
     C                   ELSE
     C                   SUB       1             ELSTFC
     C                   MULT(H)   XXPRO         ELSTFC
     C                   ADD       1             ELSTFC
     C*
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM8(1)       TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C*
     C                   MOVE      NUM8(2)       TMP(3)
     C                   MOVE      NUM8(3)       TMP(5)
     C                   MOVE      NUM8(4)       TMP(6)
     C                   MOVE      NUM8(5)       TMP(7)
     C                   MOVE      NUM8(6)       TMP(8)
     C                   MOVE      NUM8(7)       TMP(9)
     C                   MOVE      ')'           TMP(10)
     C                   ENDIF                                                  CLTID <> '025'
     C*
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVTP        ANDEQ     'B'
     C     ELWVFL        ANDEQ     'S'
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEL(P)  RRWVHD        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELWVPC        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELWVPC        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C*
     C                   ENDIF
      *
      * LARGE DEDUCTIBLE AT SUBJECT PREMIUM LEVEL
      *
     C                   MOVE      'S'           XXPLV1            1
     C                   MOVE      'S'           XXPLV2            1
     C                   EXSR      APLGDD
     C     CLTID         IFEQ      '093'
     C*
     C*  LARGE DEDUCTIBLE DISCOUNT
     C*
     C                   Z-ADD     *ZERO         XXDEDF           11 9
     C*
     C     ELDDCT        IFNE      *ZERO
     C     XXLRGF        ANDEQ     'Y'
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDNE     ELDCEF
     C     ELDCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEA     CON(1)        TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(1)        sHdg(b)
     C                   Z-add     ELDDCT        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDDCT
     C                   Z-add     ELDDCT        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELDDCT        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELDDCT <> 0
     C*
     C                   ENDIF                                                  CLTID = 093
     C*
     C*  DISCOUNT
     C*
     C                   Z-ADD     *ZERO         XXDIS3            5 2
     C     ELASP3        IFNE      *ZERO
     C     ELDISC        ORNE      *BLANK
     C     ELEDC3        ANDNE     *ZERO
     C                   MOVE      *ON           *IN28
     C     RRDSHD        IFEQ      *BLANKS
     C                   MOVEA     CON(6)        TMP
     C                   MOVEL     CON(6)        XXWK20
     C                   ELSE
     C                   MOVEA     RRDSHD        TMP
     C                   MOVEL     RRDSHD        XXWK20
     C                   ENDIF                                                  RRDSHD IFEQ
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C     ELASP3        IFNE      *ZERO
     C                   Z-ADD     ELASP3        XXDIS3
     C                   MOVE      ELASP3        XXASP1            5
     C                   Z-add     ELASP3        sPerc(b)
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   ENDIF
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
     C                   MOVE      XXPCNT        XXASP1            5
     C*
     C                   ELSE
     C     RRDSPR        IFEQ      'S'
     C                   Z-ADD     ELSTPR        XXRND            11 0
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
     C                   MOVE      XXPCNT        XXASP1            5
     C     XXPCNT        MULT      100           XXDIS3
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   MOVE      ELDISP        XXASP1
     C     RRDSPR        IFEQ      'M'
     C                   MOVE      XXPCNT        XXASP1
     C                   ENDIF                                                  RRDSPR IFEQ
     C                   MOVEL     'approx.'     PRO(A)
     C                   Z-add     ELDISP        sPerc(b)
     C                   If        rrdspr = 'M'
      /Free
       Eval(h) sPerc(b) = xxDis3;
      /End-Free
     C                   Endif
     C                   Move      'Y'           sAppr(b)
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   ENDIF
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxWk20        sHdg(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   ENDIF
     C*
     C     RRRDPR        IFEQ      'S'
     C                   SUB       XXDEDF        XXFACT
     C                   SUB       XXD2DF        XXFACT
     C                   ENDIF
     C*
     C     RRDV1L        IFEQ      'S'
     C     ELASP2        ANDNE     *ZERO
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C*
     C     RRDV2L        IFEQ      'S'
     C     ELDEV2        ANDNE     *ZERO
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C     RRDV2L        OREQ      'S'
     C     ELSTFC        ANDNE     *ZERO
     C     CLTID         ANDEQ     '025'
     C     RRDV2L        OREQ      'S'
     C     ELDEV2F       ANDNE     *ZERO
     C     CLTID         ANDEQ     '136'
      *
     C                   Select
     C                   When      cltID = '025'
     C                   Mult(H)   ELstFc        xxFact
     C                   Z-Add     ELstFc        xxPro
     C                   When      cltID = '136'
     C                   Mult(H)   ELdev2F       xxFact
     C                   Z-Add     ELdev2F       xxPro
     C                   Other
     C     ELdev2        Mult      -.01          xxPro
     C                   Add       1             xxpro
     C                   Mult(H)   xxPro         xxFact
     C                   Endsl
     C     XXWVFL        IFLT      4
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
     C     ELWVFL        IFEQ      'S'
     C     ELWVPC        ANDNE     *ZERO
     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C                   ENDIF
     C*
     C     XXDIS3        IFNE      *ZERO
     C     XXDIS3        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ENDIF
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
     C*
     C     ELDVEX        IFEQ      *ZERO
     C     ELD1S2        ANDNE     *ZERO
     C*
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD1D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD
     C*CALL API TO WITH DAYS= -1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-SUB     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
     C                   ELSE
     C     ELDVEX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELDVEX        XXILEX
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDNE     ELDVEF
     C     ELDVEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDVEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C     RRDV1H        IFEQ      *BLANK
     C                   MOVEA     CON(4)        TMP
     C                   Movel     Con(4)        xxWk20
     C                   ELSE
     C                   MOVEL(P)  RRDV1H        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELASP2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELASP2
     C                   Z-add     ELASP2        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELASP2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELASP2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  DEVIATION 1 PERIOD 2 - APPLIED TO DISCOUNTED
     C     ELD1S2        IFNE      *ZERO
     C     RRDV1L        ANDEQ     'D'
     C*
     C     ELD1D2        IFEQ      *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELDVEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD
     C*CALL API TO WITH DAYS= 1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-ADD     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEF
     C*
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDNE     ELD1D2
     C     ELD1X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD1X2        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN28
     C                   MOVE      *BLANKS       XXWK25
     C     RRDV1H        IFEQ      *BLANK
     C     @2NPER        CAT       CON(4):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV1H        XXWK25
     C     @2NPER        CAT       XXWK25:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELD1S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk25        sHdg(b)
     C                   Z-add     ELD1S2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELD1S2
     C                   Z-add     ELD1S2        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELD1S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELD1S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION - APPLIED TO DISCOUNTED
     C*
     C     ELDEV2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C     CLTID         ANDNE     '025'
     C     CLTID         ANDNE     '136'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C     CLTID         ANDEQ     '025'
     C* Use Territory Rating for CLTID = '136'
     C     ELdev2F       ORNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C     CLTID         ANDEQ     '136'
     C*
     C     ELD2EF        IFEQ      *ZERO
     C                   MOVE      ELPFDT        XXILEF
     C                   ELSE
     C                   MOVE      ELD2EF        XXILEF
     C                   ENDIF
     C*
     C*
     C     ELD2EX        IFEQ      *ZERO
     C     ELD2S2        ANDNE     *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD2D2        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD
     C*CALL API TO WITH DAYS= -1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-SUB     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
     C                   ELSE
     C     ELD2EX        IFEQ      *ZERO
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      ELD2EX        XXILEX
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDNE     ELD2EF
     C     ELD2EX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2EX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(17)       tmp
     C                   Movel     con(17)       xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   ENDIF
     C*
     C                   Move      ELstFc        xxAsp8            8
     C                   MoveA     xxAsp8        num8
     C     num8(1)       IfNE      '0'
     C                   Move      num8(1)       tmp(18)
     C                   EndIf
     C                   Move      num8(2)       tmp(19)
     C                   Move      '.'           tmp(20)
     C                   Move      num8(3)       tmp(21)
     C                   Move      num8(4)       tmp(22)
     C                   Move      num8(5)       tmp(23)
     C                   Move      num8(6)       tmp(24)
     C                   Move      num8(7)       tmp(25)
     C                   Move      num8(8)       tmp(26)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(19)       tmp
     C                   MoveL     con(19)       xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   EndIf                                                  RRDBHD IFEQ
      *
     C                   Move      ELdev2F       xxAsp1            5
     C                   MoveA     xxAsp1        num
     C                   Move      num(1)        tmp(22)
     C                   Move      '.'           tmp(23)
     C                   Move      num(2)        tmp(24)
     C                   Move      num(3)        tmp(25)
     C                   Move      num(4)        tmp(26)
     C                   Move      num(5)        tmp(27)
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     RRdv2H        IfEQ      *Blanks
     C                   MoveA     con(9)        tmp
     C                   Movel     con(9)        xxWk20
     C                   Else
     C                   Movel(p)  RRdv2H        xxWk20
     C                   MoveA     xxWk20        tmp
     C                   EndIf
     C*
     C                   MOVE      ELdev2        xxAsp1            5
     C*
     C                   MoveA     xxAsp1        NUM
     C     num(1)        IfNE      '0'
     C                   Move      num(1)        tmp(20)
     C                   EndIf
     C*
     C     num(2)        IfNE      '0'
     C     num(1)        OrGT      '0'
     C                   Move      num(2)        tmp(21)
     C                   EndIf
     C*
     C                   Move      num(3)        tmp(22)
     C                   Move      '.'           tmp(23)
     C                   Move      num(4)        tmp(24)
     C                   Move      num(5)        tmp(25)
     C                   Move      '%'           tmp(26)
     C                   Endsl
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELdev2        sPerc(b)
     C                   Z-add     ELdev2F       sFact(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stability Factor
     C                   When      cltID = '025'
     C                   Sub       1             ELstFc
     C                   Mult(H)   xxPro         ELstFc
     C                   Add       1             ELstFc
     C*
     C                   Move      ELstFc        xxAsp8            8
     C                   MoveA     xxAsp8        num8
     C     num8(1)       IFNE      '0'
     C                   Move      '('           tmp(1)
     C                   Move      num8(1)       tmp(2)
     C                   ELSE
     C                   Move      '('           tmp(2)
     C                   END
     C                   Move      num8(2)       tmp(3)
     C                   Move      '.'           tmp(4)
     C                   Move      num8(3)       tmp(5)
     C                   Move      num8(4)       tmp(6)
     C                   Move      num8(5)       tmp(7)
     C                   Move      num8(6)       tmp(8)
     C                   Move      num8(7)       tmp(9)
     C                   Move      ')'           tmp(10)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   Sub       1             ELdev2F
     C                   Mult(H)   xxPro         ELdev2F
     C                   Add       1             ELdev2F
     C*
     C                   Move      ELdev2F       xxAsp1
     C                   MoveA     xxAsp1        num
     C                   Move      '('           tmp(1)
     C                   Move      num(2)        tmp(2)
     C                   Move      '.'           tmp(3)
     C                   Move      num(3)        tmp(4)
     C                   Move      num(4)        tmp(5)
     C                   Move      num(5)        tmp(6)
     C                   MoveA     ')'           tmp(7)
     C*>>>>>>>>>>>>>>>>> Default Devaition #2
     C                   Other
     C                   Mult(H)   xxPro         ELdev2
     C                   Z-Add     ELdev2        sProP(b)
     C*
     C                   Move      ELdev2        xxAsp1            5
     C                   MoveA     xxAsp1        num
     C     num(2)        IfNE      '0'
     C                   Move      '('           tmp(1)
     C                   Move      num(2)        tmp(2)
     C                   Else
     C                   Move      '('           tmp(2)
     C                   End
     C                   Move      num(3)        tmp(3)
     C                   Move      '.'           tmp(4)
     C                   Move      num(4)        tmp(5)
     C                   Move      num(5)        tmp(6)
     C                   MoveA     '%)**'        tmp(7)
     C                   Endsl
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C                   Select
     C                   When      cltID = '025'
     C                   Mult(H)   ELstFc        xxFact
     C                   Z-Add     ELstFc        xxPro
     C                   When      cltID = '136'
     C                   Mult(H)   ELdev2F       xxFact
     C                   Z-Add     ELdev2F       xxPro
     C                   Other
     C     ELdev2        Mult      -.01          xxPro
     C                   Add       1             xxpro
     C                   Mult(H)   xxPro         xxFact
     C                   Endsl
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF
     C*  2ND DEVIATION 2ND PERIOD- APPLIED TO DISCOUNTED
     C     ELD2S2        IFNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C*
     C     CLTID         ANDNE     '025'
     C* Use Stabilization factor for CLTID = '025'
     C     ELSTFC        ORNE      *ZERO
     C     RRDV2L        ANDEQ     'D'
     C     CLTID         ANDEQ     '025'
     C*
     C     ELD2D2        IFEQ      *ZERO
     C*CONVERT DATE TO 8 NUMERIC LENGTH
     C                   Z-ADD     ELD2EX        SRDTI
     C                   CALL      'SRCVTDAT'    DTCTO8
     C                   MOVE      *ZEROS        XXCURD
     C                   MOVEL     SRDTO         XXCURD
     C*CALL API TO WITH DAYS= 1 TO CALCULATE PREVIOUS DATE
     C                   Z-ADD     XXCURD        @@DT1
     C                   Z-ADD     1             @@DAYS
     C                   CALL      'SRFUTR8'     DTSUB1
     C                   Z-ADD     @@DT1         XXCURD
     C*CONVERT PREVIOUS DATE TO CYMD FORMAT
     C                   Z-ADD     XXCURD        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXILEX
     C*
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDNE     ELD2D2
     C     ELD2X2        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELD2X2        ANDEQ     *ZERO
     C                   MOVE      *ON           *IN28
     C*
     C     CLTID         IFNE      '025'
     C*
     C     RRDV2H        IFEQ      *BLANK
     C     @2NPER        CAT       CON(9):1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ELSE
     C                   MOVEL(P)  RRDV2H        XXWK25
     C     @2NPER        CAT       XXWK25:1      XXWK25
     C                   MOVEA     XXWK25        TMP
     C                   ENDIF
     C*
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELD2S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C*FOR CLIENT 025
     C                   ELSE
     C     RRDV2H        IFEQ      *BLANKS
     C                   MOVEA     CON(17)       TMP
     C                   ELSE
     C     LO:UP         XLATE     RRDV2H        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF
     C*
     C                   MOVE      '.'           TMP(20)
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      NUM8(1)       TMP(18)
     C                   END
     C*
     C                   MOVE      NUM8(2)       TMP(19)
     C                   MOVE      NUM8(3)       TMP(21)
     C                   MOVE      NUM8(4)       TMP(22)
     C                   MOVE      NUM8(5)       TMP(23)
     C                   MOVE      NUM8(6)       TMP(24)
     C                   MOVE      NUM8(7)       TMP(25)
     C                   MOVE      NUM8(8)       TMP(26)
     C*
     C                   ENDIF                                                  CLTID <> '025'
     C*
      /free
        index = A-1;
        If %scan((%subst(xxWK20:1:17)):Hdg(index)) = 0 AND RRdv2h <> ' ';
      /end-free
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk25        sHdg(b)
     C                   Z-add     ELD2S2        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C*
     C     CLTID         IFNE      '025'
     C                   MULT(H)   XXPRO         ELD2S2
     C                   Z-add     ELD2S2        sProP(b)
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELD2S2        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C*
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C*FOR CLIENT 025
     C                   ELSE
     C                   SUB       1             ELSTFC
     C                   MULT(H)   XXPRO         ELSTFC
     C                   ADD       1             ELSTFC
     C*
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
     C*
     C     NUM8(1)       IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM8(1)       TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C*
     C                   MOVE      NUM8(2)       TMP(3)
     C                   MOVE      NUM8(3)       TMP(5)
     C                   MOVE      NUM8(4)       TMP(6)
     C                   MOVE      NUM8(5)       TMP(7)
     C                   MOVE      NUM8(6)       TMP(8)
     C                   MOVE      NUM8(7)       TMP(9)
     C                   MOVE      ')'           TMP(10)
     C*
     C                   ENDIF                                                  CLTID <> '025'
     C*
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
      /free
       Endif;
      /end-free
     C*
     C     CLTID         IFNE      '025'
     C     ELD2S2        MULT      -.01          XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C*FOR CLIENT 025
     C                   ELSE                                                   CLTID <> '025'
     C                   MULT(H)   ELSTFC        XXFACT
     C                   Z-ADD     ELSTFC        XXPRO
     C                   ENDIF                                                  CLTID <> '025'
     C*
     C     XXWVFL        IFLT      5
     C                   MULT(H)   XXPRO         XXWFAC
     C                   ENDIF
     C*
      /free
       If %scan((%subst(xxWK20:1:17)):Hdg(index)) = 0 AND RRdv2h <> ' ';
      /end-free
     C                   ADD       1             A
      /free
       endif;
      /end-free
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  WAIVER SURCHARGE
     C*
     C     ELWVPC        IFNE      *ZERO
     C     ELWVTP        ANDEQ     'B'
     C     ELWVFL        ANDEQ     'D'
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEL(P)  RRWVHD        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELWVPC        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  xxwk20        sHdg(b)
     C                   Z-add     ELWVPC        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     ELWVPC        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C                   ADD       1             A
     C*** Begin Del *** 02/12/21 ******************************************
     C**********         ADD       1             B
     C*** End   Del *** 02/12/21 ******************************************
     C                   ENDIF
     C*
      *
      * LARGE DEDUCTIBLE AT SUBJECT PREMIUM LEVEL
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
     C     XXLRGF        ANDEQ     *BLANK
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDNE     ELDCEF
     C     ELDCEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELDCEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEA     CON(1)        TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(1)        sHdg(b)
     C                   Z-add     ELDDCT        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDDCT
     C                   Z-add     ELDDCT        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELDDCT        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXDEDF
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         X1DEDF           11 9
     C                   SUB       X1DEDF        XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     *ZERO         XXD2DF           11 9
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      EL2DEX        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDNE     EL2DEF
     C     EL2DEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     EL2DEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C                   MOVEA     CON(13)       TMP
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(13)       sHdg(b)
     C                   Z-add     ELDD2P        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   MULT(H)   XXPRO         ELDD2P
     C                   Z-add     ELDD2P        sProP(b)
     C*
     C                   MOVEA     '%)**'        TMP(7)
     C                   MOVE      '.'           TMP(4)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(2)        IFNE      '0'
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(2)        TMP(2)
     C                   ELSE
     C                   MOVE      '('           TMP(2)
     C                   END
     C                   MOVE      NUM(3)        TMP(3)
     C                   MOVE      NUM(4)        TMP(5)
     C                   MOVE      NUM(5)        TMP(6)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C                   ENDIF
     C*
     C     ELDD2P        MULT      .01           XXPRO
     C     XXFACT        MULT      XXPRO         XXD2DF
     C     XXWVFL        IFLT      4
     C     XXWFAC        MULT(H)   XXPRO         X1DEDF
     C                   SUB       X1DEDF        XXWFAC
     C                   ENDIF
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      *** End Add *****  11/11/24  *******************************************
     C*
     C* CALCULATE ARAP CHARGE (AGAINST STANDARD PREMIUM)
     C*
     C     ED(3)         IFNE      *ZERO
     C     X1RPDT        ANDGT     ED(3)
     C                   Z-ADD     3             X1
     C                   ELSE
     C     ED(2)         IFNE      *ZERO
     C     X1RPDT        ANDGT     ED(2)
     C                   Z-ADD     2             X1
     C                   ELSE
     C                   Z-ADD     1             X1
     C                   ENDIF                                                  ED,2 IFNE
     C                   ENDIF                                                  ED,3 IFNE
     C*
     C     ARP(X1)       IFNE      *ZERO
     C     ARP(X1)       SUB       1             XXPCT             5 4
     C                   ADD       XXPCT         XXFACT
     C                   MOVE      *ON           *IN28
     C                   MOVEA     CON(11)       TMP
     C                   MOVE      '.'           TMP(23)
     C                   MOVE      ARP(X1)       XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(22)
     C                   MOVE      NUM(2)        TMP(24)
     C                   MOVE      NUM(3)        TMP(25)
     C                   MOVE      NUM(4)        TMP(26)
     C                   MOVE      NUM(5)        TMP(27)
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(11)       sHdg(b)
     C                   Z-add     ARP(X1)       sFact(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C                   ENDIF                                                  ARP,X1 IFNE
     C*
      *
      * FACULTATIVE REINSURANCE (ESTIMATED)
      *
     C     ELARCP        IFNE      *ZERO
     C*
     C                   MOVEA     CON(15)       TMP
      *
     C     ELARCP        MULT      .01           XXPRO
     C                   ADD       1             XXPRO
     C                   MULT(H)   XXPRO         XXFACT
     C                   MOVE      '%'           TMP(26)
     C                   MOVE      '.'           TMP(23)
     C*
     C*  contract the heading to be printed
     C*
     C                   MOVE      ELARCP        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        TMP(20)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        TMP(21)
     C                   END
     C                   MOVE      NUM(3)        TMP(22)
     C                   MOVE      NUM(4)        TMP(24)
     C                   MOVE      NUM(5)        TMP(25)
     C*
     C                   MOVEA     TMP           HDG(A)
     C                   Movel(p)  con(15)       sHdg(b)
     C                   Z-add     ELARCP        sPerc(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C*
     C                   ENDIF                                                  ELARCP IFNE
     C****  PRINT MODIFIER, INCR LIMIT IN HDR,PRO WHEN CLIENT = 093
     C                   MOVE      '0'           WKPRHD            1
     C                   MOVE      '0'           WKPRPO            1
     C                   MOVE      '0'           WKTXPC            1
     C*
     C                   Z-ADD(H)  XXFACT        XXDISC            9 6
     C                   MOVE      XXDISC        XXWK9A
     C                   MOVEA     XXWK9A        TMP
     C                   MOVE      '.'           XXD(4)
     C     TMP(1)        IFNE      *ZERO
     C                   MOVE      TMP(1)        XXD(1)
     C                   MOVE      TMP(2)        XXD(2)
     C                   MOVE      TMP(3)        XXD(3)
     C                   ELSE
     C     TMP(2)        IFNE      *ZERO
     C                   MOVE      TMP(2)        XXD(2)
     C                   MOVE      TMP(3)        XXD(3)
     C                   ELSE
     C     TMP(3)        IFNE      *ZERO
     C                   MOVE      TMP(3)        XXD(3)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      TMP(4)        XXD(5)
     C                   MOVE      TMP(5)        XXD(6)
     C                   MOVE      TMP(6)        XXD(7)
     C     TMP(9)        IFNE      *ZERO
     C                   MOVE      TMP(7)        XXD(8)
     C                   MOVE      TMP(8)        XXD(9)
     C                   MOVE      TMP(9)        XXD(10)
     C                   ELSE
     C     TMP(8)        IFNE      *ZERO
     C                   MOVE      TMP(7)        XXD(8)
     C                   MOVE      TMP(8)        XXD(9)
     C                   ELSE
     C     TMP(7)        IFNE      *ZERO
     C                   MOVE      TMP(7)        XXD(8)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   MOVEA     XXD           XXDFAM           10
     C*
     C                   MOVE      *BLANKS       CL
     C                   MOVE      *BLANKS       EXT
     C                   Z-ADD     0             EMP
     C*
     C*  GOTO THE EMPLOYER PAYROLL
     C*  FILE AND FIND THE PAYROLL RECORDS
     C*
     C     EPKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     '0'
     C     EPSTAT        IFNE      'I'
     C* Check if the Class EPCLAS = RRDCCD (Aircraft Surcharge Code) then
     C* do not print/include this line item.
     C     EPCLAS        ANDNE     RRDCCD
     C*
     C                   Z-ADD     0             XXWK50            5 0
     C                   MOVEL     EPCADT        XXWK50
     C     EPCADT        IFEQ      0
     C     EPCADT        ORNE      0
     C     XXWK50        ANDGE     XXRPYM
     C*
     C                   Z-ADD     1             X1
     C     XXCLS         LOOKUP    CL(X1)                                 60
     C     *IN60         IFEQ      '0'
     C     *BLANK        LOOKUP    CL(X1)                                 60
     C     *IN60         IFEQ      '1'
     C*
     C*   GET THE CLASS CODE RECORD,
     C*
     C                   MOVE      XXCLS         CL(X1)
     C                   MOVE      EPEXT         EXT(X1)
     C                   Z-ADD     EPEMPR        EMP(X1)
     C                   Z-ADD     EPENTR        NRT(X1)
     C                   END
     C*
     C                   END
     C*
     C                   ENDIF                                                  EPCADT IFEQ
     C*
     C                   END
     C*
     C     EPKEY         READE     PEP01                                  60
     C                   END
     C*
     C*   NOW PRINT THE PAYROLL REPORT FOR EVERY MONTH REQUESTED
     C*
     C                   Z-ADD     ELPFDT        XXPFDT
     C*
     C*
     C*   GET THE NAME OF THE MONTH
     C*
     C                   MOVE      *BLANKS       XXRMO             2 0
     C                   MOVE      *BLANKS       XXNMMO           09
     C                   MOVE      *BLANKS       XXERR            01
     C                   Z-ADD     XXRPYR        XXDATE            4 0
     C                   Z-ADD     XXRPMN        XXRMO
     C                   CALL      'SRMTHD'
     C                   PARM                    XXRMO
     C                   PARM                    XXNMMO
     C                   PARM                    XXERR
     C*
     C*   LOAD THE NAME OF MONTH AND YEAR
     C*
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     XXNMMO        XXLAST
     C                   MOVEL     XXDATE        XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        XXNMDT           15
     C*
     C*  Added code to calculate previous credit or debit balance
     C*  and code to determine months for which reports haven't been
     C*  received.                                         LJQ 8-18-88
     C*                                                          "
     C     QQPRMM        IFEQ      'Y'
     C     XXALL         ANDNE     'Y'
     C                   EXSR      SRMN                                               "
     C                   ELSE
      *
     C                   MOVE      *BLANKS       XMN
     C                   MOVE      '0'           *IN54
      *
     C                   ENDIF                                                  QQPRMM IFEQ 'Y'

     C                   EXSR      SRBAL

     C     CLTID         IFNE      '093'
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C                   WRITE     HDCNST
       Write HDLOGO;
     C  N62              WRITE     LINES
     C   62              WRITE     LINES1
     C                   Z-ADD     ELDIV         X1DIV
     C  N13              WRITE     HEAD
     C   13              WRITE     HEAD13
     C                   MOVE      *OFF          *IN01
     C     XXRESV        IFEQ      'Y'
     C                   OPEN      W7221O1
     C                   WRITE     H1CNST
      /free
       Write HDLOGO1;
      /end-free
     C  N62              WRITE     L1NES
     C   62              WRITE     L1NES1
     C                   Z-ADD     ELDIV         X1DIV
     C  N13              WRITE     H1AD
     C   13              WRITE     H1AD13
     C                   MOVE      *OFF          *IN02
     C                   ENDIF
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C                   EXSR      SRPRT
     C                   ELSE                                                   CLTID = 93
     C*
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C                   WRITE     KRMSLOGO
     C                   WRITE     HDCNS093
     C                   WRITE     LINES093
     C                   WRITE     HEAD093
     C                   MOVE      *OFF          *IN01
     C     XXRESV        IFEQ      'Y'
     C                   OPEN      W7221O1
     C                   WRITE     H1CNS093
     C                   WRITE     K1MSLOGO
     C                   WRITE     L1NES093
     C                   WRITE     H1AD093
     C                   MOVE      *OFF          *IN02
     C                   ENDIF
     C*
     C                   EXSR      SRP093
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C                   ENDIF                                                  CLTID = 93
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C     XXRESV        IFEQ      'Y'
     C                   CLOSE     W7221O1
     C                   ENDIF
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C*
     C*  Call subroutine to write a DARTS remark
     C                   EXSR      SRDART
     C                   END
     C                   SELECT
     C     CLTID         WHENEQ    '109'
      *
      *  FOR S/R EMPLOYERS
      *    - Transfer Deposit to Premium if indicated
      *
     C     EBSEFL        IFEQ      'S'
      *
     C                   MOVE      EBCO#         ##CO#
     C                   MOVE      ELPFDT        ##PFDT
     C                   MOVE      ELBSCD        ##BSCD
      *
     C                   CALL      'WA114R'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##PFDT            7
     C                   PARM                    ##BSCD            3
     C                   PARM                    ##YYMM            5
     C                   PARM      *BLANK        ##EXFL            1
     C                   PARM      *BLANK        ##TDFL            1
      *
     C     ##TDFL        IFEQ      'Y'
     C                   EXSR      SRTRDP                                       XFER DEPOSIT
     C                   ENDIF
      *
     C                   ENDIF                                                  EBSEFL = S
     C*
     C                   ENDSL
     C*
     C     XXALL         IFEQ      'Y'
     C                   MOVE      SVRPMY        XXRPMY
     C*
     C                   ADD       1             XXRPMN
     C     XXRPMN        IFEQ      13
     C                   ADD       1             XXRPYR
     C                   Z-ADD     1             XXRPMN
     C                   ENDIF
     C*
     C                   Z-ADD     XXRPMY        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXRPYM
     C                   MOVE      31            XXRPDY
     C                   Z-ADD     XXRPDT        XXDT
     C                   MOVE      01            XXDT
     C                   MOVE      'N'           XX1STM
     C                   ENDIF                                                  XXALL IFEQ
     C*
     C                   MOVE      *ON           *IN99
     C                   ENDDO
     C     NXT           TAG
     C                   END
     C*
     C                   ELSE
      *
      * Print exception report for policies that are skipped becuase they do not
      *   have a final bill run.
      *
     C     FIRST         IFEQ      ' '
     C                   MOVE      '1'           FIRST             1
     C                   MOVE      *ON           *INOB
     C                   ENDIF

      /Free
       If not %open(qsysprt2);
          Open qsysprt2;
       Endif;
      /End-free
      *
     C     *INOB         IFEQ      *ON
     C                   MOVE      *OFF          *INOB
     C                   EXCEPT    HEAD2
     C                   ENDIF
      *
     C                   EXCEPT    DET2
      *
     C                   ENDIF
     C*
       //***  Begin Chg  ***  02/07/22  **********************
     C**********         ENDIF                                                  EBAGT# IFGE
     C**********
     C**********         ENDIF                                                  EBEMP# IFGE
     C                   Endif
       //***  End   Chg  ***  02/07/22  **********************
     C*
     C     EBKY          READE     PEB01                                  60
     C                   END
     C*
      /Free
       If %open(qsysprt2);
          Chain (q1Pgm:'2') smorl;
          *in08 = *on;
          If not %found;
             ORtitl = 'Monthly Interim - Exception Report';
             *in08 = *off;
          Endif;
          *in09 = not *in08;
          *inob = *on;
          Except route2;
          Close qsysprt2;
       Endif;
      /End-free
     C* Call process to generate Scriptura output/Darts if requested.
     C                   If        xmlFileName <> ' '
     C                   Eval      QQXml = xmlFileName + '$'
     C                   Call      'S4401C'
     C                   Parm                    QQXml            61
      /Free
       strOutputPath =  %trim(GetXMLOutputName(xmlFileName:*on));
      /End-free
     C                   Endif
      /Free
       // Save output path
       Monitor;
          strCmd = 'CRTDTAARA DTAARA(QTEMP/W7221R) TYPE(*CHAR) LEN(250)';
          RunCmd(strCmd:%len(strCmd));
       On-error;
       Endmon;
       strCmd = 'CHGDTAARA DTAARA(QTEMP/W7221R *ALL)' +
        ' VALUE(''' + strOutputPath + ''')';
       RunCmd(strCmd:%len(strCmd));
       If strPortalRunType = 'EFT Payment' Or
          strPortalRunType = '3rd Party Success';
          workDate = %Date(xxRep:*Usa);
          workDate = workDate + %Months(1) - %Days(1);
          kkDocKey = %Editc(xxCo#:'X') + %Editc(xxFnd:'X') +
           %Editc(xxEmp#:'X') + %Editc(xxDiv:'X') + %Editc(xxFyr:'X') +
           %Editc(%Dec(%Char(workDate:*Cymd0):7:0):'X');
          Chain (kkDocType:kkDocKey) pdp0p;
          DPtype = kkDocType;
          DPkey = kkDocKey;
          DPPath = strOutputPath;
          DPuser = Q1user;
          DPpgm = Q1pgm;
          DPdt = %Dec(%Date():*Cymd);
          DPtime = %Dec(%Time():*Hms);
          If %Found;
             Update pdp0p;
          Else;
             Write pdp0p;
          EndIf;
       Endif;
      /End-free
     C                   MOVE      '1'           *INLR
     C*
      *====================================================================
      *  SRTRDP - TRANSFER DEPOSIT TO PREMIUM
      *====================================================================
     C     SRTRDP        BEGSR
      *
     C     F1SHTR        IFNE      *BLANKS
     C     F1SITR        ANDNE     *BLANKS
     C     F1SPTR        ANDNE     *BLANKS
     C     F1SPOF        ANDNE     *BLANKS
     C*
     C                   CLEAR                   BSSEFL
     C*
     C                   MOVEL     ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
      *
     C     BSSEFL        IFEQ      'S'                                          SELF REPORT
      *
     C                   Z-ADD     0             X                 3 0
     C                   MOVE      'Y'           XX1ST             1
     C                   MOVE      *ZEROS        SVAMT
     C                   MOVE      *ZEROS        SVARTY
     C                   MOVE      *ZEROS        SVFYR
      *
      *  ACCUMULATE I & H TRANSACTIONS TO BUCKETS BY TYPE & FYR
      *
     C     EAKY4         CHAIN     PEA04                              60
      *
     C     *IN60         DOWEQ     '0'
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
      *
     C     TTCTYP        IFEQ      'D'
     C     EAFYR         ANDLE     XXFYR
      *
     C     EAARTY        IFEQ      'F'
     C     F1SFTR        ANDNE     *BLANKS
     C     F1SQTR        ANDNE     *BLANKS
     C     F1SRTR        ANDNE     *BLANKS
     C     F1SRPO        ANDNE     *BLANKS
     C     EAARTY        OREQ      'Q'
     C     F1SFTR        ANDNE     *BLANKS
     C     F1SQTR        ANDNE     *BLANKS
     C     F1SRTR        ANDNE     *BLANKS
     C     F1SRPO        ANDNE     *BLANKS
     C     EAARTY        OREQ      'I'
     C     EAARTY        OREQ      'H'
     C     EAARTY        OREQ      'R'
     C     EAARTY        OREQ      'T'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'R'
     C                   MULT      -1            EAAMT
     C                   MOVE      'I'           EAARTY
     C     EAARTY        WHENEQ    'T'
     C                   MOVE      'I'           EAARTY
     C                   ENDSL
      *
     C     EAARTY        IFNE      SVARTY
     C     EAFYR         ORNE      SVFYR
     C     XX1ST         ANDEQ     'N'
      *
     C     SVAMT         IFNE      *ZEROS
      *
     C                   ADD       1             X
     C                   Z-ADD     SVFYR         FYR(X)
     C                   MOVEL     SVARTY        TY(X)
     C                   Z-ADD     SVAMT         AMT2(X)
     C                   ENDIF                                                  SVAMT <> 0
      *
     C                   MOVE      EAARTY        SVARTY
     C                   MOVE      EAFYR         SVFYR
     C                   MOVE      *ZEROS        SVAMT
     C                   ENDIF                                                  LEVEL BRK
      *
     C                   ADD       EAAMT         SVAMT
      *
     C     XX1ST         IFEQ      'Y'
     C                   Z-ADD     EAFYR         SVFYR
     C                   MOVE      EAARTY        SVARTY
     C                   MOVE      'N'           XX1ST
     C                   ENDIF                                                  XX1ST = 'Y'
      *
     C                   ENDIF                                                  EAARTY IFEQ
      *
     C                   ENDIF                                                  EAARTY =
      *                                                    'H' OR 'I'
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO                                                  *IN60
      *
      *  FORCE LAST TO ARRAYS
      *
     C     SVAMT         IFNE      *ZEROS
     C                   ADD       1             X
     C                   Z-ADD     SVFYR         FYR(X)
     C                   MOVEL     SVARTY        TY(X)
     C                   Z-ADD     SVAMT         AMT2(X)
     C                   ENDIF                                                  SVAMT
      *
      *  INITIALIZE OUTPUT FIELDS
      *
     C                   Z-ADD     *ZEROS        XXAMT            15 4
     C                   Z-ADD     *ZEROS        XTAMT            11 2
     C                   Z-ADD     *ZEROS        XXHAMT           15 4
     C                   Z-ADD     *ZEROS        XXFAMT           15 4
     C                   Z-ADD     XXCO#         EACO#
     C                   Z-ADD     XXFND         EAFND
     C                   Z-ADD     XXEMP#        EAEMP#
     C                   Z-ADD     XXDIV         EADIV
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   MOVE      *BLANKS       EAEDIT
     C                   MOVE      *BLANKS       EASTAT
     C                   Z-ADD     *ZEROS        EABUL1
     C                   Z-ADD     *ZEROS        EACHK#
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
      *
      *  WRITE REVERSING ENTRIES FOR EACH FYR/TYPE
      *
     C                   DO        X             Z                 3 0
      *
     C     AMT2(Z)       IFNE      *ZERO
     C                   Z-ADD     FYR(Z)        EAFYR
      *
      *  DEPOSIT PAYMENT NEGATION
      *
     C                   SELECT
      *
     C     TY(Z)         WHENEQ    'I'
     C                   MOVE      F1SITR        EAARCD
     C                   MOVEL(P)  F1SITR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVEL(P)  TTDESC        EADESC
      *
     C     TY(Z)         WHENEQ    'H'
      *
      *  DEPOSIT BILLING NEGATION
      *
     C                   MOVE      F1SHTR        EAARCD
     C                   MOVEL(P)  F1SHTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVEL(P)  TTDESC        EADESC
      *
     C     TY(Z)         WHENEQ    'F'
     C                   MOVE      F1SFTR        EAARCD
     C                   MOVEL(P)  F1SFTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVEL(P)  TTDESC        EADESC
      *
     C     TY(Z)         WHENEQ    'Q'
     C                   MOVE      F1SQTR        EAARCD
     C                   MOVEL(P)  F1SQTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVEL(P)  TTDESC        EADESC
      *
     C                   ENDSL
      *
      *  NEGATE AMOUNT
     C     AMT2(Z)       MULT      -1            EAAMT
     C                   MOVE      'A'           EASTAT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVE      *BLANK        EALATE
     C                   WRITE     PEA0P
      *
      *  ACCUMULATE PAYMENT AMOUNTS FOR LATER TRANSFER TO PREMIUM
      *
     C     TY(Z)         IFEQ      'I'
     C                   ADD       AMT2(Z)       XXAMT
     C                   ENDIF
      *
     C     TY(Z)         IFEQ      'Q'
     C                   ADD       AMT2(Z)       XTAMT
     C                   ENDIF
      *
      *  Accumulate Deposit Billed Amounts for later transfer to premium
      *
     C     TY(Z)         IFEQ      'H'
     C                   ADD       AMT2(Z)       XXHAMT
     C                   ENDIF
      *
     C     TY(Z)         IFEQ      'F'
     C                   ADD       AMT2(Z)       XXFAMT
     C                   ENDIF
      *
     C                   ENDIF                                                  AMT2,Z NE *ZERO
      *
     C                   ENDDO                                                  DO  X  Z
      *
      *  IF THERE IS AN ACCRUED DEPOSIT PAID AMOUNT, TRANSFER IT
      *
     C     XXAMT         IFNE      *ZEROS
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   MOVE      XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   MOVE      F1SPTR        EAARCD
     C                   MOVEL(P)  F1SPTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVEL(P)  TTDESC        EADESC
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   Z-ADD     XXAMT         EAAMT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVE      *ZEROS        EABUL1
     C                   MOVE      *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C                   WRITE     PEA0P
     C                   ENDIF                                                  XXAMT <> 0
      *
      *  If there is an accrued Deposit Billed Amount,
      *  transfer it to premium
      *
     C     XXHAMT        IFNE      *ZEROS
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   MOVE      XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   MOVE      F1SPOF        EAARCD
     C                   MOVEL(P)  F1SPOF        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVEL(P)  TTDESC        EADESC
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   Z-ADD     XXHAMT        EAAMT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVE      *ZEROS        EABUL1
     C                   MOVE      *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C                   WRITE     PEA0P
     C                   ENDIF                                                  XXAMT <> 0
      *
      *  IF THERE IS AN ACCRUED TAX ON DEP PAID AMOUNT, TRANSFER IT.
      *
     C     XTAMT         IFNE      *ZEROS
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   MOVE      XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   MOVE      F1SRTR        EAARCD
     C                   MOVEL(P)  F1SRTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVEL(P)  TTDESC        EADESC
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   Z-ADD     XTAMT         EAAMT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVE      *ZEROS        EABUL1
     C                   MOVE      *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C                   WRITE     PEA0P
     C                   ENDIF                                                  XXAMT <> 0
      *
      *  If there is an accrued tax on deposit billed amount,
      *  transfer it.
      *
     C     XXFAMT        IFNE      *ZEROS
     C                   MOVE      XXCO#         EACO#
     C                   MOVE      XXFND         EAFND
     C                   MOVE      XXFYR         EAFYR
     C                   MOVE      XXEMP#        EAEMP#
     C                   MOVE      XXDIV         EADIV
     C                   MOVE      F1SRPO        EAARCD
     C                   MOVEL(P)  F1SRPO        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVEL(P)  TTDESC        EADESC
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EATRDT
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   Z-ADD     XXFAMT        EAAMT
     C                   MOVE      'A'           EASTAT
     C                   MOVEL     Q1USER        EAUSER
     C                   MOVEL     Q1PGM         EAPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        EATIME
     C                   MOVE      *ZEROS        EABUL1
     C                   MOVE      *ZEROS        EACHK#
     C                   MOVE      *BLANK        EALATE
     C                   WRITE     PEA0P
     C                   ENDIF                                                  XXAMT <> 0
      *
     C                   ENDIF                                                  BSSEFL = S
      *
     C                   ENDIF
      *
     C                   ENDSR                                                  SRTRDP
     C*
     C*****************************************************************
     C*  PRINT SUBROUTINE                                             *
     C*****************************************************************
     C     SRPRT         BEGSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         saveDate          7 0
     C*
     C                   If        isUseXml
     C                   Open      wxmip1
     C                   Open      wxmip2
     C                   Exsr      ScriptXML
     C                   Endif
     C*
     C*   INITIALIZE PAGE NUMBER
     C*
       //*** Begin Add *** 06/12/24 ****************************************
       //
       // Initialize payroll totals
       decTotalFClassAmt     = 0;
       decTotalOtherClassAmt = 0;
       decTotalAllClassAmt   = 0;
       decFClassFactor       = 0;
       decOtherClassFactor   = 0;
       decAssessmentFactor   = 0;
       decAssessmentPremium  = 0;
       decAssessmentPercent  = 0;
       decTotalAssessmentAmt = 0;
       //*** End   Add *** 06/12/24 ****************************************
     C                   Z-ADD     1             XXPAGE            3 0
     C                   MOVE      *OFF          *IN72
     C*
     C     RRPTFL        IFEQ      'Y'
     C                   MOVE      '1'           *IN80
     C                   MOVE      '1'           *IN81
     C                   ELSE
     C                   MOVE      '0'           *IN80
     C                   MOVE      '0'           *IN81
     C                   ENDIF
     C*
     C*   TAKE THE REPORTING DATE AND FIND THE APPROPRIATE EXPERIENCE
     C*   MOD
     C*
     C     ELEXM1        IFNE      *ZERO
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      *OFF          *IN69
     C                   ENDIF                                                  ELEXM1 IFNE
     C     ED(3)         IFNE      *ZERO
     C     X1RPDT        ANDGT     ED(3)
     C                   Z-ADD     3             X1
     C                   ELSE
     C     ED(2)         IFNE      *ZERO
     C     X1RPDT        ANDGT     ED(2)
     C                   Z-ADD     2             X1
     C                   ELSE
     C                   Z-ADD     1             X1
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *OFF          *IN79
     C                   MOVE      *BLANKS       XXEMHD           19
     C                   Eval      isNotRated = *Off
     C                   If        (x1 = 1  And ELnoExM1 = 'Y') Or
     C                             (ELexM1 = 0)
     C                   Eval      isNotRated = *On
     C                   EndIf
     C*** Begin Add *** 07/30/23 *******************************************
     C                   If        x1 = 2  And ELnoExM2 = 'Y'
     C                   Eval      isNotRated = *On
     C                   EndIf
     C                   If        x1 = 3  And ELnoExM3 = 'Y'
     C                   Eval      isNotRated = *On
     C                   EndIf
     C*** End   Add *** 07/30/23 *******************************************
     C     EM(X1)        IFNE      *ZERO
     C                   MOVE      *ON           *IN79
     C                   MOVEL     CON(12)       XXEMHD
     C                   ENDIF
     C                   Z-ADD     EM(X1)        XXEXMD            5 4
     C***  Begin Chg  ***  01/19/22  **********************
     C*****F1NETR        IFEQ      'Y'
     C*****ELPFDT        ANDGE     F1NETD
     C                   If        isNetRated
     C***  End   Chg  ***  01/19/22  **********************
     C                   Z-ADD(H)  XXPRTT        XXEXMD
     C     XXEXMD        IFNE      *ZERO
     C                   MOVE      *ON           *IN79
     C                   MOVEL     CON(12)       XXEMHD
     C                   ENDIF
     C                   ENDIF
     C*                                                    JJJ 11-3-89
     C*                                                    JJJ 11-3-89
     C*   CHECK IF ANNIVERSARY RATING DATE IS IN EFFECT    |
     C*                                                    |
     C*
     C*  ANNIVERSARY RATING SHOULD BE BASED ON THE LAST DAY OF
     C*  THE REPORTING MONTH/YEAR NOT THE CURRENT BALANCE DATE
     C*  ENTERED FROM THE PROMPT
     C*
     C*
     C*  We should only use the ARD if it is applicable for the policy
     C*  period for which the interim is being produced
     C*
     C*
     C* USE 4/1/3 FL RATES IF APPLICABLE.
     C*
     C*
     C* USE 10/1/3 FL RATES IF APPLICABLE.
     C*
      *
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   XXRPDT        ##DAT1            7
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DAT1
      *
     C                   MOVE      ##DAT1        XXDT
     C*
     C*
     C* SET TERRORISM CHARGE FOR PROPER RATING PERIOD.
     C*
     C                   Z-ADD     0             XXTERR            5 3
     C*
     C                   MOVE      XXDT          XXPDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        XXPDT
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   MOVEL(P)  CON(16)       XXTERH
     C*
     C                   MOVEL(P)  CON(16)       XXTERI
     C*
     C     *IN60         IFEQ      *OFF
     C     RRTERH        ANDNE     *BLANKS
     C                   MOVEL(P)  RRTERH        XXTERH
     C                   MOVEL(P)  RRTERH        XXTERI
     C                   ENDIF
     C*
     C                   CAT       'rate':1      XXTERI
     C  N62              CAT       'and':1       XXTERI
     C*
     C                   MOVEL(P)  XXTERH        X1TERH
     C                   MOVEL(P)  XXTERI        X1TERI
     C     ELterr        IFNE      *ZERO
     C                   Z-ADD     ELTERR        XXTERR
     C                   ELSE
     C     *IN60         IFEQ      *OFF
     C     RRTERR        ANDNE     *ZERO
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   ELRTCD        QQRTCD
     C                   MOVE(P)   RRTERC        QQCLAS
     C                   MOVE(P)   XXPDT         QQEFDT
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
     C                   ENDIF                                                  RRTERL IFNE
     C                   Z-ADD     RRTERR        XXTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C     ELDTRR        IFNE      *ZERO
     C     XXTERR        IFEQ      *ZERO
     C                   Z-ADD     ELDTRR        XXTERR
     C                   ELSE
     C                   ADD       ELDTRR        XXTERR
     C                   ENDIF
     C                   ELSE
     C     RRDTRR        IFNE      *ZERO
      *
     C     RRTERL        IFNE      'Y'
     C     RRDTRR        MULT(H)   XXLCM         RRDTRR
     C                   ENDIF                                                  RRTERL IFNE
     C     XXTERR        IFEQ      *ZERO
     C                   Z-ADD     RRDTRR        XXTERR
     C                   ELSE
     C                   ADD       RRDTRR        XXTERR
     C                   ENDIF
      *
     C                   ENDIF                                                  RRDTRR
     C                   ENDIF                                                  RRDTRR
     C*
     C                   MOVE      ELPFDT        XXPDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C                   MOVE      *OFF          *IN31
     C                   MOVEL     ELPFDT        X1YR              3 0
     C                   MOVEL     ELANDT        X1AYR             3 0
     C     XXST          IFEQ      'CA'
     C     X1YR          ANDEQ     102
     C     XXST          OREQ      'CA'
     C     X1AYR         ANDEQ     102
     C                   MOVE      *ON           *IN31
     C                   ENDIF
     C*
     C                   z-add     0             xxTotMan         11 2
     C                   z-add     0             xxTotPay         11 2
     C                   Move      'Y'           noPayroll         1
      /Free
       is3rdParty = CheckFor3rdParty();
      /End-Free
     C*
     C                   Z-ADD     1             X2                3 0
     C     CL(X2)        DOWNE     *BLANK
     C                   MOVE      CL(X2)        XXCLS
     C*
     C                   MOVE      EXT(X2)       EPEXT
     C                   Z-ADD     NRT(X2)       EPENTR
      *
      *  GET RATING DATE FOR PULLING INFO FROM PAYROLL TABLE
      *  BASED ON POLICY INFO, PAYROLL CLASS, AND REPORT DATE
      *
     C                   Z-ADD     0             KKEFDT
     C                   Z-ADD     0             K1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   XXRPDT        ##DAT1            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DAT1
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DAT1        KKEFDT
     C                   MOVE      ##DAT1        K1EFDT
      *
      *  GET PAYROLL CLASS DESCRIPTION AND 'A' RATING FLAG
      *
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      *BLANKS       XXDESC           30
     C                   MOVE      *BLANKS       XXARFL            1
     C* Refresh the Underline indicator for Payroll column
     C                   MOVE      '0'           *IN83
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         DOWEQ     *OFF
     C     PRSTAT        IFEQ      'A'
     C                   MOVEL     PRDESC        XXDESC
     C                   MOVEL     PRARFL        XXARFL
     C* If the class code is population rated, then do not display underline
     C* in payroll column.
     C     PRPCF         IFEQ      'O'
     C                   MOVE      '1'           *IN83
     C                   ENDIF                                                  PRPCF IFEQ 'O'
     C                   LEAVE
     C                   ENDIF
     C     PRKY2         READPE    PPR01                                  60
     C                   ENDDO
      *
      * GET EXPANDED PAYROLL DESCRIPTION FOR CAREWEST
      *
     C     CLTID         IFEQ      '136'
     C                   MOVE      *OFF          *IN82
     C                   MOVEL     *BLANKS       XXDES1
     C                   MOVE      *BLANKS       XXDES2
     C     PRKYE1        SETGT     PPR01E                             60
     C     PRKYE         READPE    PPR01E                                 60
     C     *IN60         DOWEQ     *OFF
     C     PRSTAT        IFEQ      'A'
     C     PREXDS        ANDNE     *BLANK
     C                   MOVE      *ON           *IN82
     C                   MOVEL     PREXDS        XXW100          100
     C                   MOVEL(P)  XXW100        XXDES1
     C                   MOVE(P)   XXW100        XXDES2
     C                   LEAVE
     C                   ENDIF
     C     PRKYE         READPE    PPR01E                                 60
     C                   ENDDO
     C                   ENDIF
     C*
     C*
     C*  CHECK TO SEE IF EMPLOYER IS USING "A" RATING
     C*
     C     ELSTAT        IFEQ      'Y'
     C     XXARFL        ANDEQ     'Y'
     C*
     C*  RETRIEVE "A" RATE FOR PAYROLL CLASS CODE
     C*
     C                   Z-ADD     EPCLAS        XXCLAS
     C                   MOVE      EPSPEC        XXSPEC
     C                   MOVEL     EPEXT         XXEXT
     C     EPKY1         SETGT     PEP01
     C                   READP     PEP01                                  60
     C     *IN60         IFEQ      '0'
     C     XXCO#         ANDEQ     EPCO#
     C     XXFND         ANDEQ     EPFND
     C     XXFYR         ANDEQ     EPFYR
     C     EMEMP#        ANDEQ     EPEMP#
     C     EMDIV         ANDEQ     EPDIV
     C     XXCLAS        ANDEQ     EPCLAS
     C     XXSPEC        ANDEQ     EPSPEC
     C     XXEXT         ANDEQ     EPEXT
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   EXSR      GETDSC
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C   01              WRITE     HDCNST
      /free
       Write HDLOGO;
      /end-free
     C   01
     CANN62              WRITE     LINES
     C   01
     CAN 62              WRITE     LINES1
     C                   Z-ADD     ELDIV         X1DIV
     C  N13              WRITE     HEAD
     C   13              WRITE     HEAD13
     C     XXRESV        IFEQ      'Y'
     C   02              WRITE     H1CNST
      /free
       Write HDLOGO1;
      /end-free
     C   02
     CANN62              WRITE     L1NES
     C   02
     CAN 62              WRITE     L1NES1
     C                   Z-ADD     ELDIV         X1DIV
     C  N13              WRITE     H1AD
     C   13              WRITE     H1AD13
     C                   ENDIF
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C                   MOVE      '0'           *IN01
     C                   MOVE      '0'           *IN02
     C*
     C                   Z-ADD     EPARAT        XXARAT
     C                   Z-ADD     XXARAT        XNARAT
     C     *IN62         IFEQ      *ON
     C     EPENTR        IFNE      *ZERO
     C     CLTID         ANDEQ     '136'
     C                   Z-ADD     EPENTR        XNARAT            7 4
     C                   ELSE
     C                   MULT(H)   XXPROR        XNARAT            7 4
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *OFF          *IN78
     C     PRPCF         IFEQ      'O'
     C     EMP(X2)       DIV(H)    12            XXWK90            9 0
     C                   Z-ADD     0             XXARAT
     C                   MOVE      *ON           *IN78
     C                   ENDIF
     C*
     C                   If        qqCompleted = 'Y'
     C                   Exsr      WASRPset
     C                   EndIf
     C                   Exsr      ScriptPay
     C*
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C                   SELECT
     C     CLTID         WHENEQ    '136'
     C                   WRITE     DET136
     C                   WRITE     TDET
     C     XXRESV        IFEQ      'Y'
     C                   WRITE     D1T136
     C                   WRITE     T1ET
     C                   ENDIF
     C                   OTHER
     C  N62              WRITE     DET
     C   62              WRITE     DET1
     C                   WRITE     TDET
     C     XXRESV        IFEQ      'Y'
     C  N62              WRITE     D1T
     C   62              WRITE     D1T1
     C                   WRITE     T1ET
     C                   ENDIF
     C                   ENDSL
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C                   ADD       1             XXCNT
     C                   Exsr      WASRPreset
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C*
     C     *LIKE         DEFINE    ELPFDT        XXDT
     C     EPKYL1        SETGT     PEP01
     C     EPKYL         READPE    PEP01                                  64
     C                   EXSR      GETDSC
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C   01              WRITE     HDCNST
      /free
       Write HDLOGO;
      /end-free
     C   01
     CANN62              WRITE     LINES
     C   01
     CAN 62              WRITE     LINES1
     C                   Z-ADD     ELDIV         X1DIV
     C  N13              WRITE     HEAD
     C   13              WRITE     HEAD13
     C     XXRESV        IFEQ      'Y'
     C   02              WRITE     H1CNST
      /free
       Write HDLOGO1;
      /end-free
     C   02
     CANN62              WRITE     L1NES
     C   02
     CAN 62              WRITE     L1NES1
     C                   Z-ADD     ELDIV         X1DIV
     C  N13              WRITE     H1AD
     C   13              WRITE     H1AD13
     C                   ENDIF
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C                   MOVE      '0'           *IN01
     C                   MOVE      '0'           *IN02
     C*
     C*
     C*  ADJUST RATE IF NECESSARY
     C*
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   PRRTCD        QQRTCD
     C                   MOVE(P)   PRCLAS        QQCLAS
     C                   MOVE(P)   XXDT          QQEFDT
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
        //*** Beg Chg *** 12/10/25 ******************************************
     C***  PRRT          MULT(H)   XXLCM         PRRT
     C                   EVAL      RateDec = ChkRatSetCd(ELCO#:PRRTCD:ELPFDT)
     C                   IF        RateDec = 3
     C     PRRT          MULT(H)   XXLCM         PRRT
     C                   ELSE
     C     PRRT          MULT(H)   XXLCM         Rate8_2
     C                   Z-ADD     Rate8_2       PRRT
     C                   ENDIF
        //*** End Chg *** 12/10/25 ******************************************
     C                   Z-ADD     PRRT          XXARAT
     C     EPSPEC        IFEQ      'F'
     C     PRLHCD        IFEQ      'N'
     C                   MOVE      ELRTCD        XXRATE
     C                   Z-ADD     ELPFDT        XXPDT
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     HEKY          SETGT     PHE01
     C     HEKY2         READPE    PHE01                                  60
     C     *IN60         IFEQ      *OFF
     C     XXARAT        MULT      HELHNF        XXARAT
     C                   ENDIF                                                  *IN60 IFEQ
     C                   END
     C                   END
     C*
     C     *IN31         IFEQ      *ON
     C                   MULT(H)   1.002         XXARAT
     C     XXDT          IFLT      1030101
     C     X1RPDT        ANDGT     1030101
     C                   MULT(H)   1.049         XXARAT
     C                   ENDIF                                                  XXDT IFLT
     C                   ENDIF                                                  *IN31 IFEQ
     C*
     C                   Z-ADD     XXARAT        XNARAT
     C     *IN62         IFEQ      *ON
     C     EPENTR        IFNE      *ZERO
     C     CLTID         ANDEQ     '136'
     C                   Z-ADD     EPENTR        XNARAT            7 4
     C                   ELSE
     C                   MULT(H)   XXPROR        XNARAT            7 4
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *OFF          *IN78
     C     PRPCF         IFEQ      'O'
     C     EMP(X2)       DIV(H)    12            XXWK90            9 0
     C                   Z-ADD     0             XXARAT
     C                   MOVE      *ON           *IN78
     C                   ENDIF
     C*
     C                   If        qqCompleted = 'Y'
     C                   Exsr      WASRPset
     C                   EndIf
     C                   Exsr      ScriptPay
     C*
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C                   SELECT
     C     CLTID         WHENEQ    '136'
     C                   WRITE     DET136
     C                   WRITE     TDET
     C     XXRESV        IFEQ      'Y'
     C                   WRITE     D1T136
     C                   WRITE     T1ET
     C                   ENDIF
     C                   OTHER
     C  N62              WRITE     DET
     C   62              WRITE     DET1
     C                   WRITE     TDET
     C     XXRESV        IFEQ      'Y'
     C  N62              WRITE     D1T
     C   62              WRITE     D1T1
     C                   WRITE     T1ET
     C                   ENDIF
     C                   ENDSL
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C                   ADD       1             XXCNT
     C                   Exsr      WASRPreset
     C                   END                                                    ELSTAT = 'Y'
     C*
     C     X2            ADD       1             X2AD              3 0
     C*
      *
      *
      *  SET LIMIT TO THE NUMBER OF PAYROLLS THAT CAN PRINT IN THE DETAIL
      *  SECTION
      *
     C                   SELECT
     C     CLTID         WHENEQ    '045'
     C                   Z-ADD     9             LIMIT             2 0
     C     *IN62         WHENEQ    *OFF                                         IS NOT NET RATE
     C                   Z-ADD     9             LIMIT
     C     *IN62         WHENEQ    *ON                                          IS NOT NET RATE
     C                   Z-ADD     10            LIMIT
     C                   ENDSL
     C     XXCNT         IFGT      LIMIT
     C     CL(X2AD)      ANDNE     *BLANK
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN73
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C  N62              WRITE     LINES
     C   62              WRITE     LINES1
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C                   MOVE      *OFF          *IN50
     C     CLTID         IFEQ      '136'
     C                   MOVE      *ON           *IN50
     C                   ENDIF
      *
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C                   SELECT
     C     CLTID         WHENEQ    '136'
     C                   WRITE     TTC136
     C                   WRITE     TOT
     C                   WRITE     TOTNET
     C     XXRESV        IFEQ      'Y'
     C                   WRITE     T1C136
     C                   WRITE     T1T
     C                   WRITE     T1TNET
     C                   ENDIF
     C                   OTHER
     C  N62              WRITE     TTCNST
     C   62              WRITE     TTCNS1
     C                   WRITE     TOT
     C  N62              WRITE     TOTNOT
     C   62              WRITE     TOTNET
     C     XXRESV        IFEQ      'Y'
     C  N62              WRITE     T1CNST
     C   62              WRITE     T1CNS1
     C                   WRITE     T1T
     C  N62              WRITE     T1TNOT
     C   62              WRITE     T1TNET
     C                   ENDIF
     C                   ENDSL
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C
     C                   MOVE      *OFF          *IN73
     C                   ADD       1             XXPAGE
     C                   MOVE      *ZERO         XXCNT             3 0
     C                   MOVE      '1'           *IN01
     C                   MOVE      '1'           *IN02
     C                   END
     C*
     C                   ADD       1             X2
     C*
     C                   END
     C*
     C                   MOVE      *OFF          *IN50
     C     CLTID         IFEQ      '136'
     C                   MOVE      *ON           *IN50
     C                   ENDIF
      *
       //*** Beg Add 12/07/22 *************************************************
       If  isUseSpoolout;
       //*** End Add 12/07/22 *************************************************
     C                   SELECT
     C     CLTID         WHENEQ    '136'
     C                   WRITE     TTC136
     C                   WRITE     TOT
     C                   WRITE     TOTNET
     C     XXRESV        IFEQ      'Y'
     C                   WRITE     T1C136
     C                   WRITE     T1T
     C                   WRITE     T1TNET
     C                   ENDIF
     C                   OTHER
     C  N62              WRITE     TTCNST
     C   62              WRITE     TTCNS1
     C                   WRITE     TOT
     C  N62              WRITE     TOTNOT
     C   62              WRITE     TOTNET
     C     XXRESV        IFEQ      'Y'
     C  N62              WRITE     T1CNST
     C   62              WRITE     T1CNS1
     C                   WRITE     T1T
     C  N62              WRITE     T1TNOT
     C   62              WRITE     T1TNET
     C                   ENDIF
     C                   ENDSL
       //*** Beg Add 12/07/22 *************************************************
       Endif;
       //*** End Add 12/07/22 *************************************************
     C                   MOVE      '1'           *IN01
     C                   MOVE      '1'           *IN02
     C                   Z-ADD     0             XXCNT
     C                   Exsr      ScriptMain
     C*
     C                   If        isUseXml
     C                   Close     wxmip1
     C                   Close     wxmip2
     C                   If        noPayroll = 'Y'
     C                   Exsr      ScriptRemove
     C                   Endif
     C                   Endif
     C                   ENDSR
     C*
     C*****************************************************************
     C*  PRINT SUBROUTINE FOR CLIENT 093                              *
     C*****************************************************************
     C     SRP093        BEGSR
     C*
     C*
     C*   INITIALIZE PAGE NUMBER
     C*
     C                   Z-ADD     1             XXPAGE            3 0
     C                   MOVE      *OFF          *IN72
     C*
     C     RRPTFL        IFEQ      'Y'
     C                   MOVE      '1'           *IN80
     C                   MOVE      '1'           *IN81
     C                   ELSE
     C                   MOVE      '0'           *IN80
     C                   MOVE      '0'           *IN81
     C                   ENDIF
     C*
     C*   TAKE THE REPORTING DATE AND FIND THE APPROPRIATE EXPERIENCE
     C*   MOD
     C*
     C     ELEXM1        IFNE      *ZERO
     C                   MOVE      *ON           *IN69
     C                   ELSE
     C                   MOVE      *OFF          *IN69
     C                   ENDIF                                                  ELEXM1 IFNE
      *
     C     ED(3)         IFNE      *ZERO
     C     X1RPDT        ANDGT     ED(3)
     C                   Z-ADD     3             X1
      *
     C                   ELSE
      *
     C     ED(2)         IFNE      *ZERO
     C     X1RPDT        ANDGT     ED(2)
     C                   Z-ADD     2             X1
     C                   ELSE
     C                   Z-ADD     1             X1
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE      *OFF          *IN79
     C                   MOVE      *BLANKS       XXEMHD           19
     C                   Eval      isNotRated = *Off
     C                   If        (x1 = 1  And ELnoExM1 = 'Y') Or
     C                             (ELexM1 = 0)
     C                   Eval      isNotRated = *On
     C                   EndIf
     C*** Begin Add *** 07/30/23 *******************************************
     C                   If        x1 = 2  And ELnoExM2 = 'Y'
     C                   Eval      isNotRated = *On
     C                   EndIf
     C                   If        x1 = 3  And ELnoExM3 = 'Y'
     C                   Eval      isNotRated = *On
     C                   EndIf
     C*** End   Add *** 07/30/23 *******************************************
     C     EM(X1)        IFNE      *ZERO
     C                   MOVE      *ON           *IN79
     C                   MOVEL     CON(12)       XXEMHD
     C                   ENDIF
     C                   Z-ADD     EM(X1)        XXEXMD            5 4
     C                   If        isNetRated
     C                   Z-ADD(H)  XXPRTT        XXEXMD
     C     XXEXMD        IFNE      *ZERO
     C                   MOVE      *ON           *IN79
     C                   MOVEL     CON(12)       XXEMHD
     C                   ENDIF
     C                   ENDIF
      *
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   XXRPDT        ##DAT1            7
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DAT1
      *
     C                   MOVE      ##DAT1        XXDT
     C*
     C*
     C* SET TERRORISM CHARGE FOR PROPER RATING PERIOD.
     C*
     C                   Z-ADD     0             XXTERR            5 3
     C*
     C                   MOVE      XXDT          XXPDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        XXPDT
     C                   ENDIF
     C     ELTERR        IFNE      *ZERO
     C                   Z-ADD     ELTERR        XXTERR
     C                   ELSE
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C     *IN60         IFEQ      *OFF
     C     RRTERR        ANDNE     *ZERO
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   ELRTCD        QQRTCD
     C                   MOVE(P)   RRTERC        QQCLAS
     C                   MOVE(P)   XXPDT         QQEFDT
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
     C                   ENDIF                                                  RRTERL IFNE
     C                   Z-ADD     RRTERR        XXTERR
     C                   ENDIF
     C                   ENDIF
     C*
     C     ELDTRR        IFNE      *ZERO
     C     XXTERR        IFEQ      *ZERO
     C                   Z-ADD     ELDTRR        XXTERR
     C                   ELSE
     C                   ADD       ELDTRR        XXTERR
     C                   ENDIF
     C                   ELSE
     C     RRDTRR        IFNE      *ZERO
      *
     C     RRTERL        IFNE      'Y'
     C     RRDTRR        MULT(H)   XXLCM         RRDTRR
     C                   ENDIF                                                  RRTERL IFNE
     C     XXTERR        IFEQ      *ZERO
     C                   Z-ADD     RRDTRR        XXTERR
     C                   ELSE
     C                   ADD       RRDTRR        XXTERR
     C                   ENDIF
      *
     C                   ENDIF                                                  RRDTRR
     C                   ENDIF                                                  RRDTRR
     C*
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C                   MOVE      *OFF          *IN31
     C                   MOVEL     ELPFDT        X1YR              3 0
     C                   MOVEL     ELANDT        X1AYR             3 0
     C     XXST          IFEQ      'CA'
     C     X1YR          ANDEQ     102
     C     XXST          OREQ      'CA'
     C     X1AYR         ANDEQ     102
     C                   MOVE      *ON           *IN31
     C                   ENDIF
     C*
     C                   Z-ADD     1             X2                3 0
     C     CL(X2)        DOWNE     *BLANK
     C                   MOVE      CL(X2)        XXCLS
     C*
     C                   MOVE      EXT(X2)       EPEXT
     C*
     C*  CHECK TO SEE IF EMPLOYER IS USING "A" RATING
     C*
     C     ELSTAT        IFEQ      'Y'
     C*
     C*  RETRIEVE "A" RATE FOR PAYROLL CLASS CODE
     C*
     C                   Z-ADD     EPCLAS        XXCLAS
     C                   MOVE      EPSPEC        XXSPEC
     C                   MOVE      EPEXT         XXEXT
     C     EPKY1         SETGT     PEP01
     C                   READP     PEP01                                  60
     C     *IN60         IFEQ      '0'
     C     XXCO#         ANDEQ     EPCO#
     C     XXFND         ANDEQ     EPFND
     C     XXFYR         ANDEQ     EPFYR
     C     EMEMP#        ANDEQ     EPEMP#
     C     EMDIV         ANDEQ     EPDIV
     C     XXCLAS        ANDEQ     EPCLAS
     C     XXSPEC        ANDEQ     EPSPEC
     C     XXEXT         ANDEQ     EPEXT
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   Z-ADD     0             KKEFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   XXRPDT        ##DAT1            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DAT1
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DAT1        KKEFDT
     C*
     C*  GET PAYROLL CLASS CODE DESCRIPTION
     C*
      *
     C     PRKY1         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C   60              MOVE      *BLANKS       PRDESC
      *
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C                   Z-ADD     0             K1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   XXRPDT        ##DAT1            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DAT1
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DAT1        K1EFDT
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        PRDESC
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C                   Z-ADD     0             K1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   XXRPDT        ##DAT1            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DAT1
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DAT1        K1EFDT
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        PRDESC
     C                   ENDIF                                                  EPEXT IFNE
     C     EPEXT         IFNE      *BLANKS
     C     PXCODE        ANDNE     *ZERO
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *OFF          *IN32
     C                   ENDIF                                                  EPEXT IFNE
     C*
     C   01              WRITE     HDCNS093
     C                   WRITE     KRMSLOGO
     C                   WRITE     LINES093
     C                   WRITE     HEAD093
     C     XXRESV        IFEQ      'Y'
     C   02              WRITE     H1CNS093
     C                   WRITE     K1MSLOGO
     C                   WRITE     L1NES093
     C                   WRITE     H1AD093
     C                   ENDIF
     C                   MOVE      '0'           *IN01
     C                   MOVE      '0'           *IN02
     C*
     C                   Z-ADD     EPARAT        XXARAT
     C                   Z-ADD     XXARAT        XNARAT
     C     *IN62         IFEQ      *ON
     C     EPENTR        IFNE      *ZERO
     C     CLTID         ANDEQ     '136'
     C                   Z-ADD     EPENTR        XNARAT            7 4
     C                   ELSE
     C                   MULT(H)   XXPROR        XNARAT            7 4
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *OFF          *IN78
     C     PRPCF         IFEQ      'O'
     C     EMP(X2)       DIV(H)    12            XXWK90            9 0
     C                   Z-ADD     0             XXARAT
     C                   MOVE      *ON           *IN78
     C                   ENDIF
     C  N62              WRITE     DET093
     C                   WRITE     TDET093
     C     XXRESV        IFEQ      'Y'
     C  N62              WRITE     D1T093
     C                   WRITE     T1ET093
     C                   ENDIF
     C                   ADD       1             XXCNT
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   Z-ADD     0             KKEFDT
     C                   MOVE(P)   ELCO#         Q1CO#             3
     C                   MOVE(P)   ELFND         Q1FND             3
     C                   MOVE(P)   ELFYR         Q1FYR             3
     C                   MOVE(P)   ELEMP#        Q1EMP#            9
     C                   MOVE(P)   ELDIV         Q1DIV             5
     C                   MOVE(P)   XXRPDT        Q1RPDT            7
     C                   MOVE(P)   'P'           Q1TYPE            1
     C                   MOVE(P)   EPCLAS        Q1CLAS            5
     C                   CALL      'SREFFD'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1DIV
     C                   PARM                    Q1RPDT
     C                   PARM                    Q1TYPE
     C                   PARM                    Q1CLAS
      *
     C                   MOVE      Q1RPDT        KKEFDT
     C*
     C*   GET THE PAYROLL CLASS RECORD, AND RATE
     C*
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C     XXCO#         IFEQ      PRCO#
     C     XXRTCD        ANDEQ     PRRTCD
     C     EPCLAS        ANDEQ     PRCLAS
     C     KKEFDT        ANDGE     PREFDT
     C*
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C                   Z-ADD     0             K1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   XXRPDT        ##DAT1            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DAT1
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DAT1        K1EFDT
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        PRDESC
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C                   Z-ADD     0             K1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   XXRPDT        ##DAT1            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
      *
     C                   CALL      'SREFFD'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##FYR
     C                   PARM                    ##EMP#
     C                   PARM                    ##DIV
     C                   PARM                    ##DAT1
     C                   PARM                    ##TYPE
     C                   PARM                    ##CLAS
      *
     C                   MOVE      ##DAT1        K1EFDT
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        PRDESC
     C                   ENDIF                                                  EPEXT IFNE
     C     EPEXT         IFNE      *BLANKS
     C     PXCODE        ANDNE     *ZERO
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *OFF          *IN32
     C                   ENDIF                                                  EPEXT IFNE
     C   01              WRITE     HDCNS093
     C   01
     CANN62              WRITE     LINES093
     C                   WRITE     KRMSLOGO
     C                   WRITE     HEAD093
     C     XXRESV        IFEQ      'Y'
     C   02              WRITE     H1CNS093
     C                   WRITE     K1MSLOGO
     C   02
     CANN62              WRITE     L1NES093
     C                   WRITE     H1AD093
     C                   ENDIF
     C                   MOVE      '0'           *IN01
     C                   MOVE      '0'           *IN02
     C*
     C*
     C*  ADJUST RATE IF NECESSARY
     C*
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   PRRTCD        QQRTCD
     C                   MOVE(P)   PRCLAS        QQCLAS
     C                   MOVE(P)   XXDT          QQEFDT
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
        //*** Beg Chg *** 12/10/25 ******************************************
     C***  PRRT          MULT(H)   XXLCM         PRRT
     C                   EVAL      RateDec = ChkRatSetCd(ELCO#:PRRTCD:ELPFDT)
     C                   IF        RateDec = 3
     C     PRRT          MULT(H)   XXLCM         PRRT
     C                   ELSE
     C     PRRT          MULT(H)   XXLCM         Rate8_2
     C                   Z-ADD     Rate8_2       PRRT
     C                   ENDIF
        //*** End Chg *** 12/10/25 ******************************************
     C                   Z-ADD     PRRT          XXARAT
     C     EPSPEC        IFEQ      'F'
     C     PRLHCD        IFEQ      'N'
     C                   MOVE      ELRTCD        XXRATE
     C                   Z-ADD     ELPFDT        XXPDT
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     HEKY          SETGT     PHE01
     C     HEKY2         READPE    PHE01                                  60
     C     *IN60         IFEQ      *OFF
     C     XXARAT        MULT      HELHNF        XXARAT
     C                   ENDIF                                                  *IN60 IFEQ
     C                   END
     C                   END
     C*
     C     *IN31         IFEQ      *ON
     C                   MULT(H)   1.002         XXARAT
     C     XXDT          IFLT      1030101
     C     X1RPDT        ANDGT     1030101
     C                   MULT(H)   1.049         XXARAT
     C                   ENDIF                                                  XXDT IFLT
     C                   ENDIF                                                  *IN31 IFEQ
     C*
     C                   Z-ADD     XXARAT        XNARAT
     C     *IN62         IFEQ      *ON
     C     EPENTR        IFNE      *ZERO
     C     CLTID         ANDEQ     '136'
     C                   Z-ADD     EPENTR        XNARAT            7 4
     C                   ELSE
     C                   MULT(H)   XXPROR        XNARAT            7 4
     C                   ENDIF
     C                   ENDIF
     C                   MOVE      *OFF          *IN78
     C     PRPCF         IFEQ      'O'
     C     EMP(X2)       DIV(H)    12            XXWK90            9 0
     C                   Z-ADD     0             XXARAT
     C                   MOVE      *ON           *IN78
     C                   ENDIF
     C                   WRITE     DET093
     C                   WRITE     TDET093
     C     XXRESV        IFEQ      'Y'
     C                   WRITE     D1T093
     C                   WRITE     T1ET093
     C                   ENDIF
     C                   ADD       1             XXCNT
     C*
     C                   END
     C                   END                                                    ELSTAT = 'Y'
     C*
     C                   CLEAR                   XNDFBM
     C                   CLEAR                   XNDFAM
     C                   CLEAR                   XXEZPF
     C*
     C     X2            ADD       1             X2AD              3 0
 NEW C     XLPCT         DIV       100           XXDPCT            5 3
     C*
     C     3             SUBST     XXDFBM:1      WKFLD1            3
     C     6             SUBST     XXDFBM:5      WKFLD2            6
     C                   MOVEL     WKFLD2        WKFLD3            6 6
     C*
     C                   MOVEL     WKFLD1        XNDFBM            9 6
     C     XNDFBM        ADD       WKFLD3        XNDFBM
     C*
     C                   CLEAR                   WKFLD1
     C                   CLEAR                   WKFLD2
     C                   CLEAR                   WKFLD3
     C*
     C     3             SUBST     XXDFAM:1      WKFLD1
     C     6             SUBST     XXDFAM:5      WKFLD2
     C                   MOVEL     WKFLD2        WKFLD3
     C*
     C                   MOVEL     WKFLD1        XNDFAM            9 6
     C     XNDFAM        ADD       WKFLD3        XNDFAM
     C*
     C     XNDFBM        MULT      XNDFAM        XXEZPF
     C     XXDPCT        IFGT      0
     C                   ADD       1             XXDPCT
     C     XXEZPF        MULT      XXDPCT        XXEZPF
     C                   ENDIF
     C*
     C     XXEXMD        IFGT      0
     C     XXEZPF        MULT      XXEXMD        XXEZPF
     C                   ENDIF
      *
      *  State Tax
      *
     C     RRTXPC        IFGT      0
     C     RRTXPC        DIV       100           XXTMP2           15 7
     C                   ADD       1             XXTMP2
     C                   MULT(H)   XXTMP2        XXEZPF
     C                   ENDIF
      *
     C*
     C*** LOAD EXPERIENCE MOD AND INCREASED LIMITS FOR CLIENT 093
     C*
     C                   CLEAR                   WKTMP
     C                   CLEAR                   WKTMP1
     C                   CLEAR                   WKHDG9
     C*
     C     XXEXMD        IFGT      *ZEROS
     C     WKPRHD        ANDEQ     '0'
     C                   Z-ADD     1             I                 2 0
     C     *BLANKS       LOOKUP    HDG(I)                                 60
     C                   MOVEL     XXCEXM        HDG(I)
     C*
     C                   MOVEL     XXEXMD        WKTMP             1
     C                   MOVE      XXEXMD        WKTMP1            4
     C                   MOVEL     WKTMP         WKHDG9            6
     C     WKHDG9        CAT       '.':0         WKHDG9
     C     WKHDG9        CAT       WKTMP1:0      WKHDG9
     C*
     C     HDG(I)        CAT       WKHDG9:4      HDG(I)
     C                   MOVE      '1'           WKPRHD
     C                   ENDIF
     C*
     C     XLPCT         IFGT      *ZEROS
     C     WKPRPO        ANDEQ     '0'
     C                   Z-ADD     1             I
     C     *BLANKS       LOOKUP    HDG(I)                                 60
     C                   MOVEL     XXINCL        HDG(I)
     C                   MOVEL     XXILPC        PRO(I)
     C                   MOVE      '1'           WKPRPO
     C                   ENDIF
     C*
      *
      *  State Tax
      *
     C     RRTXPC        IFGT      *ZEROS
     C     WKTXPC        ANDEQ     '0'
     C                   Z-ADD     1             I
     C     *BLANKS       LOOKUP    HDG(I)                                 60
     C                   MOVEL(P)  XXTHD         HDG(I)
     C                   MOVEL     X1TAX         PRO(I)
     C                   MOVE      '1'           WKTXPC
     C                   ENDIF
      *
      *
     C     XXCNT         IFGT      10
     C     CL(X2AD)      ANDNE     *BLANK
     C                   MOVE      *ON           *IN72
     C                   MOVE      *ON           *IN73
     C                   Z-ADD     XXTERR        XXTER1            5 3
     C                   EXSR      GETPSR
     C                   WRITE     TTCNS093
     C                   WRITE     TOT093
     C                   WRITE     TOTNO093
     C                   WRITE     HDG9093
     C                   WRITE     PRO9093
     C     XXRESV        IFEQ      'Y'
     C                   WRITE     T1CNS093
     C                   WRITE     T1T
     C                   WRITE     T1TNO093
     C                   WRITE     H1G9093
     C                   WRITE     P1O9093
     C                   ENDIF
     C                   MOVE      *OFF          *IN73
     C                   ADD       1             XXPAGE
     C                   MOVE      *ZERO         XXCNT             3 0
     C                   MOVE      '1'           *IN01
     C                   MOVE      '1'           *IN02
     C                   END
     C*
     C                   ADD       1             X2
     C*
     C                   END
     C*
     C                   Z-ADD     XXTERR        XXTER1            5 3
     C                   EXSR      GETPSR
     C                   WRITE     TTCNS093
     C                   WRITE     TOT093
     C                   WRITE     TOTNO093
     C                   WRITE     HDG9093
     C                   WRITE     PRO9093
     C     XXRESV        IFEQ      'Y'
     C                   WRITE     T1CNS093
     C                   WRITE     T1T093
     C                   WRITE     T1TNO093
     C                   ENDIF
     C                   MOVE      '1'           *IN01
     C                   MOVE      '1'           *IN02
     C                   Z-ADD     0             XXCNT
     C                   ENDSR
     C*
     C*****************************************************************
     C*             MISSING MONTHS' REPORTS SUBROUTINE                *
     C*****************************************************************
     C*
     C     SRMN          BEGSR                                                  LJQ 8-18-88
     C*
     C* Check for special cases that are invalid and valid:
     C* 1. Case when a payroll class is incorrectly keyed but then amt.
     C*    is reversed.  If there are no other payroll classes for that
     C*    employer then there is a missing month.
     C* 2. Case when there is payroll class but the reported amount = 0
     C*    This is a valid entry and should not have a missing month.
     C* 3. Case when there are no reported payrolls.  This case is a
     C*    missing month.
     C* 4. Case where there are reported payrolls that have not been
     C*    reversed.  There is no missing month.
     C*
     C                   Z-ADD     0             XXCPYM                         Compare Date
     C                   MOVE      *BLANKS       XMN
     C                   MOVE      '0'           *IN54
     C                   Z-ADD     1             X2                3 0
     C*
     C* The range of dates begins with employer's policy from date to
     C* the month prior to the previous requested month. Take requested
     C* month and subtract 1 to get the prior month.
     C*                                                            ¦
     C     XXRPMN        SUB       1             XXCPMN                                 ¦
     C                   Z-ADD     XXRPMN        XXREPM
     C                   Z-ADD     XXRPYR        XXREPY
     C                   Z-ADD     1             XXREPD
     C                   Z-ADD     XXREP         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCPYR
     C                   Z-ADD     XXYR          XXSTYR
     C                   Z-ADD     XXMTH         XXSTMN
     C     XXRPMN        IFEQ      1
     C                   Z-ADD     12            XXCPMN                         |
     C                   Z-ADD     XXRPMN        XXREPM
     C                   Z-ADD     XXRPYR        XXREPY
     C                   Z-ADD     1             XXREPD
     C                   Z-ADD     XXREP         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCPYR
     C                   SUB       1             XXCPYR
     C                   END
     C*
     C* Loop through all months from start date to comparison date.
     C*
     C     XXSTDT        IFLE      XXCPYM
     C     XXSTDT        DOWLE     XXCPYM
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '109'
     C*
     C                   MOVE      ELCO#         #@CO#
     C                   MOVE      ELPFDT        #@PFDT
     C                   MOVE      ELBSCD        #@BSCD
     C                   MOVE      XXSTDT        #@STDT
     C*
     C                   CALL      'WA114R'
     C                   PARM                    #@CO#             3
     C                   PARM                    #@PFDT            7
     C                   PARM                    #@BSCD            3
     C                   PARM                    #@STDT            5
     C                   PARM      *BLANK        #@EXFL            1
     C                   PARM      *BLANK        #@TDFL            1
     C*
     C     #@EXFL        IFEQ      'Y'
     C                   ADD       1             XXSTMN
     C                   GOTO      NXT1
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   MOVE      *ZEROS        CLS
     C                   MOVE      *ZEROS        AMT
     C                   MOVE      '0'           *IN57
     C                   Z-ADD     0             CNT               5 0
     C*
     C* Begin reading employer payroll file
     C*
     C     SRKEY         CHAIN     PSR01                              60
     C     *IN60         DOWEQ     '0'
     C                   Z-ADD     1             Y1                5 0
     C*
     C* CHECK FOR ANY AMOUNTS THAT ARE NOT = 0
     C*
     C     SRAMTP        IFNE      0
     C                   MOVE      '1'           *IN57
     C                   END
     C     Y1            DOWLE     999
     C     CLS(Y1)       IFNE      0
     C     CLS(Y1)       ANDNE     SRCLAS
     C                   ADD       1             Y1
     C                   ELSE
     C     SRCLAS        IFEQ      CLS(Y1)
     C                   ADD       SRAMTP        AMT(Y1)
     C                   Z-ADD     1000          Y1
     C                   ELSE
     C     CLS(Y1)       IFEQ      0
     C                   Z-ADD     SRCLAS        CLS(Y1)
     C                   Z-ADD     SRAMTP        AMT(Y1)
     C                   ADD       1             CNT
     C                   Z-ADD     1000          Y1
     C                   END
     C                   END
     C                   END
     C                   END
     C     SRKEY         READE     PSR01                                  60
     C                   END
     C*
     C* Check for Case 1.
     C*
     C* Loop through and total amount array.  If amounts = 0 and there
     C* were non-zero records added (*in57 = 1) then we have case 1.
     C*
     C     *IN57         IFEQ      '1'
     C                   Z-ADD     1             Y1
     C                   Z-ADD     0             XAMT             11 2
     C     Y1            DOWLE     999
     C                   ADD       AMT(Y1)       XAMT
     C                   ADD       1             Y1
     C                   END
     C*
     C     XAMT          IFEQ      0
     C                   Z-ADD     XXSTMN        X1                3 0
     C                   ADD       1             XXSTMN                         get nxt mnth
     C*
     C* Load the necessary months' name into the output fields
     C*
     C     X2            IFGT      24
     C                   Z-ADD     1             X2
     C                   END
     C                   MOVE      MN(X1)        XMN(X2)
     C                   ADD       1             X2
     C                   MOVE      '1'           *IN54
     C                   GOTO      NXT1
     C                   END
     C                   END
     C*
     C* Check for case 3.
     C*
     C     CNT           IFEQ      0
     C                   Z-ADD     XXSTMN        X1                3 0
     C                   ADD       1             XXSTMN                         get nxt mnth
     C*
     C* Load the necessary months' name into the output fields
     C*
     C     X2            IFGT      24
     C                   Z-ADD     1             X2
     C                   END
     C                   MOVE      MN(X1)        XMN(X2)
     C                   ADD       1             X2
     C                   MOVE      '1'           *IN54
     C                   GOTO      NXT1
     C                   END
     C*
     C* Cases 2 & 4 continue for the next month
     C*
     C                   ADD       1             XXSTMN                         get nxt mnth
     C*
     C     NXT1          TAG
     C*
     C     XXSTMN        IFGT      12
     C                   Z-ADD     1             XXSTMN
     C                   ADD       1             XXSTYR
     C                   END
     C*
     C                   END
     C*
     C                   ELSE
     C*
     C                   MOVE      '0'           *IN54
     C*
     C                   END
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C*    CALCULATE CREDIT OR DEBIT BALANCES                         *
     C*****************************************************************
     C*
     C     SRBAL         BEGSR
     C*
       If x5RPCYM <> 0;
       // Get last day of month
       intRptDate = x5RpCYM * 100 + 1;
       dtWork = %Date(intRptDate:*Cymd);
       dtWork = dtWork + %Months(1);
       dtWork = dtWork - %Days(1);
       intRptDate = %Dec(dtWork:*Cymd);
       // Sum self reported premium and taxes for the report date
       decPrvBilled = 0;
       Exec Sql
        With PrvBilled (arAmount)
        As (
        Select EAamt
        From WMEAP
        Where EAco# = :xxCo#
         And EAfnd = :xxFnd
         And EAfyr = :xxFyr
         And EAemp# = :EMemp#
         And EArdiv = :EMdiv
         And EApodt = :intRptDate
         And (EAarcd = :F1srbb Or
          //*** Begin Add *** 06/12/24 *************************************
          //*** (EActyp = 'T' And EAarty = 'F'))
          (EActyp = 'T' And EAarty = 'F') Or
          (EActyp in ('1','2','3','4','5','6','7') And EAarty = 'F' And
           :isSRAs = '1'))
          //*** End   Add *** 06/12/24 *************************************
        Union All
        Select EAamt
        From WDEAP
        Join WTTTP
         On EAco# = TTco# And EAarcd = Substr(TTkey,1,2)
        Where EAco# = :xxCo#
         And EAfnd = :xxFnd
         And EAfyr = :xxFyr
         And EAemp# = :EMemp#
         And EArdiv = :EMdiv
         And EAstat = ' '
         And EApodt = :intRptDate
         And (EAarcd = :F1srbb Or
          //*** Begin Add *** 06/12/24 *************************************
          //*** (EActyp = 'T' And TTtype = 'F'))
          (EActyp = 'T' And TTtype = 'F') Or
          (EActyp in ('1','2','3','4','5','6','7') And TTtype = 'F' And
           :isSRAs = '1'))
          //*** End   Add *** 06/12/24 *************************************
          )
        Select Sum(arAmount)
        Into :decPrvBilled
        From PrvBilled;
       Else;
          decPrvBilled = 0;
       Endif;
     C                   TIME                    DSDTTM
     C                   MOVE      '0'           *IN55
     C                   MOVE      '0'           *IN56
     C                   Z-ADD     0             PBAL             11 2
     C                   Z-ADD     XXRNDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT
     C*
     C*   SET THE START OF THE FILE FOR THE PASSED PARMS.
     C*
     C     EAKEY         CHAIN     PEA01                              58
     C     *IN58         DOWEQ     '0'
      *
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
      *
     C     EAPODT        IFLE      XXDAT
     C     XX1STM        ANDEQ     *BLANK
     C*
     C*
     C                   SELECT
     C     EAARTY        WHENEQ    'P'
     C     EAARTY        OREQ      'Q'
     C                   SUB       EAAMT         PBAL
     C     EAARTY        WHENEQ    'T'
     C     TTCTYP        ANDEQ     'P'
     C                   SUB       EAAMT         PBAL
     C     EAARTY        WHENEQ    'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'N'
     C                   ADD       EAAMT         PBAL
     C     EAARTY        WHENEQ    'R'
     C     TTCTYP        ANDEQ     'P'
     C                   ADD       EAAMT         PBAL
     C                   ENDSL
      *
     C*
     C                   END
     C*
     C     EAKEY         READE     PEA01                                  58
     C                   END
      *********
     C********* CALCULATE PRIOR YEAR BALANCE
      *********
     C     EXAYFL        IFEQ      'Y'
      *
     C     EAKY2         CHAIN     PEA04                              58
     C     *IN58         DOWEQ     '0'
     C     EAFYR         ANDLT     XXFYR
      *
     C     EAPODT        IFLE      XXDAT
      *
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
      *
     C                   SELECT
      *
     C     EAARTY        WHENEQ    'P'
     C     EAARTY        OREQ      'Q'
     C                   SUB       EAAMT         PBAL
      *
     C     EAARTY        WHENEQ    'T'
     C     TTCTYP        ANDEQ     'P'
     C                   SUB       EAAMT         PBAL
      *
     C     EAARTY        WHENEQ    'A'
     C     EAARTY        OREQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'F'
     C     EAARTY        OREQ      'N'
     C                   ADD       EAAMT         PBAL
      *
     C     EAARTY        WHENEQ    'R'
     C     TTCTYP        ANDEQ     'P'
     C                   ADD       EAAMT         PBAL
     C                   ENDSL
      *
     C                   ENDIF                                                  EAPODT<XXDAT
      *
     C     EAKY2         READE     PEA04                                  58
     C                   ENDDO                                                  *IN58 DOWEQ
     C                   ENDIF                                                  EXAYFL NE 'Y'
     C*
     C* IF A WAIVER SURCHARGE IS ON THE POLICY AND IT IS NOT A
     C* PERCENTAGE, THEN WE NEED TO ADD IN 1/12 OF THE CHARGE TO
     C* THE PREVIOUS BALANCE TO REFLECT IT AND PRINT A MESSAGE.
     C*
     C                   MOVE      *OFF          *IN71
     C                   MOVE      *BLANKS       XXPBAL           80
     C     ELEWAV        IFNE      *ZERO
     C     ELWVPC        ANDEQ     *ZERO
     C     ELEWAV        ORNE      *ZERO
     C     ELWVTP        ANDEQ     'S'
     C*
     C     LO:UP         XLATE(P)  RRWVHD        XXWVHD           20
     C     XXPBAL        IFEQ      *BLANKS
     C                   MOVEL     MSG(1)        XXPBAL
     C     XXPBAL        CAT       XXWVHD:1      XXPBAL
     C                   ELSE
     C     XXPBAL        CAT       ',':0         XXPBAL
     C     XXPBAL        CAT       XXWVHD:1      XXPBAL
     C                   ENDIF                                                  XXPBAL IFEQ
     C     ELEWAV        DIV(H)    12            XXPREM           11 0
     C     XXPREM        MULT(H)   XXWFAC        XXPREM
     C                   ADD       XXPREM        PBAL
     C                   MOVE      *ON           *IN71
     C                   MOVE      '1'           *IN56
     C*
     C                   ENDIF                                                  ELEWAV IFNE
     C*
     C*
     C* IF A REINSURANCE CHARGE IS ON THE POLICY AND IT IS NOT A
     C* PERCENTAGE, THEN WE NEED TO ADD IN 1/12 OF THE CHARGE TO
     C* THE PREVIOUS BALANCE TO REFLECT IT AND PRINT A MESSAGE.
     C*
     C     ELEARC        IFNE      *ZERO
     C     ELARCP        ANDEQ     *ZERO
     C*
     C     XXPBAL        IFEQ      *BLANKS
     C                   MOVEL     MSG(1)        XXPBAL
     C     XXPBAL        CAT       CON(15):1     XXPBAL
     C                   ELSE
     C     XXPBAL        CAT       ',':0         XXPBAL
     C     XXPBAL        CAT       CON(15):1     XXPBAL
     C                   ENDIF                                                  XXPBAL IFEQ
     C*
     C     ELEARC        DIV(H)    12            XXPREM           11 0
     C                   ADD       XXPREM        PBAL
     C                   MOVE      *ON           *IN71
     C                   MOVE      '1'           *IN56
     C*
     C                   ENDIF                                                  ELEARC IFNE
     C*
     C*
     C*   CALCULATE PRIOR DEPOSIT BILLED
     C     QQINDP        IFEQ      'Y'
     C                   Z-ADD     *LOVAL        KKFYR
     C                   Z-ADD     0             XXPRDP           13 2
     C     *LIKE         DEFINE    EAFYR         S1FYR
     C                   Z-ADD     0             S1FYR
     C*
     C*  TURN ON THE DEPOSIT MODE BY YEAR FLAG (XXDPYM) IF THE POLICY YEAR
     C*  IS DEFINED AS SUCH IN WDF1P UNLESS ALL YEAR FLAG IS SET TO 'Y' IN
     C*  WMEXP
     C*
     C     F1KY          CHAIN     PF101                              60
     C     F1DPYM        IFEQ      'Y'
     C     EXAYFL        ANDNE     'Y'
     C                   MOVE      'Y'           XXDPYM            1
     C                   ELSE
     C                   MOVE      'N'           XXDPYM            1
     C                   ENDIF
     C*
     C     EAKY1         SETLL     PEA04
     C     EAKY4         READE     PEA04                                  60
     C     *IN60         DOWEQ     '0'
     C     EAFYR         ANDLE     XXFYR
     C*
     C     EAPODT        IFLE      XXDAT
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C                   SELECT
     C     XXDPYM        WHENEQ    'Y'
     C     EAFYR         IFEQ      XXFYR
     C     EAARTY        IFEQ      'H'
     C     EAARTY        OREQ      'R'
     C     TTCTYP        ANDEQ     'D'
     C                   ADD       EAAMT         XXPRDP
     C                   ENDIF
     C     EAARTY        IFEQ      'I'
     C     EAARTY        OREQ      'T'
     C     TTCTYP        ANDEQ     'D'
     C                   SUB       EAAMT         XXPRDP
     C                   ENDIF
     C                   ENDIF
     C*
     C                   OTHER
     C     EAFYR         IFLE      XXFYR
     C*
     C* DO NOT INCLUDE PRIOR YEAR DEPOSIT TRANSACTIONS IF THAT YEAR IS FLAGGED
     C* AS A PER YEAR DEPOSIT MODE UNLESS ALL YEAR BALANCE FLAG IS SET IN WMEXP
     C*
     C     EAFYR         IFLT      XXFYR
     C     EAFYR         ANDNE     S1FYR
     C                   Z-ADD     EAFYR         S1FYR
     C     F1KEY2        SETLL     PF101
     C     F1KEY2        READE     PF101                                  60
     C                   ENDIF
     C     F1DPYM        IFEQ      *BLANK
     C     EXAYFL        OREQ      'Y'
     C     EAFYR         OREQ      XXFYR
     C     EAARTY        IFEQ      'H'
     C     EAARTY        OREQ      'R'
     C     TTCTYP        ANDEQ     'D'
     C                   ADD       EAAMT         XXPRDP
     C                   ENDIF
     C     EAARTY        IFEQ      'I'
     C     EAARTY        OREQ      'T'
     C     TTCTYP        ANDEQ     'D'
     C                   SUB       EAAMT         XXPRDP
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF
     C*
     C     EAKY4         READE     PEA04                                  60
     C                   ENDDO
     C*
     C                   ADD       XXPRDP        PBAL
     C*
     C* RESET GROUP DETAIL
     C*
     C     F1KY          CHAIN     PF101                              60
     C*
     C                   ENDIF                                                  QQINDP=Y
     C*
     C*
     C***  SET INDICATORS FOR PRINTING DEBIT OR CREDIT BALANCE.
     C*
     C* DEBIT BALANCE
     C*
     C     PBAL          IFGE      XXDUE
     C                   MOVE      '1'           *IN56
     C                   END
     C*
     C* CREDIT BALANCE
     C*
     C     PBAL          IFLT      0
     C     PBAL          IFLE      XXNDUE
     C                   MOVE      '1'           *IN56
     C                   END
     C                   END
     C*
     C                   MOVE      *BLANKS       XXPSTD
     C     CLTID         IFEQ      '136'
     C     PBAL          ANDGT     *ZERO
     C                   MOVEL     VBPSTD        XXPSTD
     C                   ENDIF
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '102'
     C                   MOVE      *OFF          *IN56
     C                   ENDSL
     C                   ENDSR
     C*
     C*  SUBROUTINE TO PRORATE PREMIUM FACTORS
     C*
     C     SRPRO         BEGSR
     C*
     C                   MOVE      *ON           *IN74
     C*
     C                   Z-ADD     0             XXLPDT            7 0
     C                   Z-ADD     XXFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     XXTODT        SRDTI
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
     C                   MOVEL     SRDTO         X1FROM
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TO
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
     C                   Z-ADD     XXLPDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   ENDIF
     C*
     C* PRORATE AGAINST THE NUMBER OF DAYS IN THE MONTH
     C*
     C                   Z-ADD     XXRPMN        X                 3 0
     C                   Z-ADD     MD(X)         XXDAYS            5 0
     C*
     C     XXRPMN        IFEQ      XXFPMO
     C     XXRPYR        ANDEQ     XXPFY
     C     MD(X)         SUB       XXFPDY        XXDAYS
     C                   ADD       1             XXDAYS
     C*
     C     XXDAYS        IFEQ      *ZERO
     C                   ADD       1             XXDAYS
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXRPMN = XXFPMO
     C*                                                AND XXRPYR = XXFPYR
     C     XXRPMN        IFEQ      XXTPMO
     C     XXRPYR        ANDEQ     XXPTY
     C                   Z-ADD     XXTPDY        XXDAYS
     C                   ENDIF                                                  XXRPMN = XXTPMO
     C*                                                AND XXRPYR = XXTPYR
     C     QQDAYS        DIV       XXDAYS        XXPRO            11 9
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SRDART
     C*================================================================
     C     SRDART        BEGSR
     C*
     C*    If DARTS remark exists in ADMIN/DARTS file then write remark
     C*
     C     EXMICD        IFNE      *BLANK
     C     isUseScript   ANDNE     *on
     C     suppressDarts AndNe     *on
     C*
     C*    GET NEXT DOC ID
     C*
     C     XXRESV        IFEQ      'Y'
     C     CLTID         IFEQ      '133'                                                    ID ='136
     C                   MOVE      EBCO#         IFCO#
     C                   MOVE      EBFND         IFFND
     C                   MOVE      EBEMP#        IFEMP#
     C                   MOVE      EBDIV         IFDIV
     C                   ENDIF
      *
     C                   CALL      'SNXTDCID'
     C                   PARM      *BLANKS       #1FOLD           80
     C                   PARM      *BLANKS       #1DCID           12
     C                   PARM      'MI'          #1PRE             2
     C                   PARM      QQCO#         #1CO#             3
     C                   PARM      QQFND         #1FND             3
     C                   PARM      *BLANKS       #1RTCD            1
      *
      *  CALL CL TO RESOLVE W7221O1 TO DARTS
      *
     C                   CALL      'W7221C1'
     C                   PARM                    #1FOLD           80
     C                   PARM                    #1DCID           12
     C                   PARM                    QQIFLR
     C                   PARM                    QQDC30
      *
     C                   MOVEL(P)  #1FOLD        Q#FOLD           80
     C                   MOVEL(P)  #1DCID        Q#DCID           12
     C                   MOVE(P)   'O'           Q#IMGF            1
     C                   MOVEL(P)  'N'           Q#DIFL            1
      *
     C                   ELSE
      *
     C                   MOVEL     *BLANKS       Q#FOLD
     C                   MOVEL     *BLANKS       Q#DCID
     C                   MOVEL     *BLANK        Q#IMGF
     C                   MOVEL     *BLANK        Q#DIFL
      *
     C                   ENDIF
      /Free
       strOutputPath = GetPath(%trim(#1fold):%trim(#1dcId));
      /End-free
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVE      *BLANK        QCKEY
     C                   MOVE      EBCO#         QCCO#
     C                   MOVE      EBFND         QCFND
     C                   MOVE      EBEMP#        QCEMP#
     C                   MOVE      EBDIV         QCDIV
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVEL(P)  EXMICD        Q#RMCD            5
     C                   MOVE      EBFYR         XAFYR             2
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   EXSR      DRTCOM
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
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
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C                   ENDSR                                                  SRDART
     C*
     C*================================================================
     C* DRTCOM - Fill in comment for DARTS
     C*================================================================
     C     DRTCOM        BEGSR
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         ##DATE            8 0
     C*
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   MOVEL(P)  RMK(1)        Q#COMM          256
     C     Q#COMM        CAT       UPUSNM:1      Q#COMM
     C     Q#COMM        CAT       ', on':0      Q#COMM
     C                   Z-ADD     ##DATE        DSDT2
     C     Q#COMM        CAT       DSMM2:1       Q#COMM
     C     Q#COMM        CAT       '/':0         Q#COMM
     C     Q#COMM        CAT       DSDD2:0       Q#COMM
     C     Q#COMM        CAT       '/':0         Q#COMM
     C     Q#COMM        CAT       DSYY2:0       Q#COMM
     C     Q#COMM        CAT       'for':1       Q#COMM
     C     Q#COMM        CAT       'policy':1    Q#COMM
     C     Q#COMM        CAT       'year':1      Q#COMM
     C     XXFYR         IFGT      99
     C                   MOVE      XXFYR         XBFYR             3
     C     Q#COMM        CAT       XBFYR:1       Q#COMM
     C                   ELSE                                                   FYR > 99
     C                   MOVE      XXFYR         XAFYR
     C     Q#COMM        CAT       XAFYR:1       Q#COMM
     C                   ENDIF                                                  FYR > 99
     C     Q#COMM        CAT       '.':0         Q#COMM
     C*
     C                   ENDSR                                                  DRTCOM
     C*
     C*====================================================================
     C* SUBROUTINE TO CHECK IF S/R RECORDS NEED TO BE WRITTEN.
     C*====================================================================
     C*
     C     SRLATE        BEGSR
     C*
     C* FOR NEW ACCOUNTS CHECK QUOTE TRANSFER DATE.
     C*
     C     ELPFDT        IFEQ      EMEFD1
     C*
     C                   MOVE      'W'           XXSTAT
     C*
     C* CHECK QUOTE TRACKING TO FIND THE TRANSFER DATE.
     C*
     C     QTKY7         CHAIN     PQT07                              60
     C     *IN60         DOWEQ     *OFF
     C     QTTYPE        ANDNE     'W'
     C     QTKY7         READE     PQT07                                  60
     C                   ENDDO
     C*
     C* PROCESS ACCOUNT IF TRANSFERED AFTER POLICY EFFECTIVE DATE.
     C*
     C     *IN60         IFEQ      *OFF
     C     QTWKDT        ANDGT     EMEFD1
     C*
     C                   MOVEL     EMEFD1        XXYM              5 0
     C                   MOVEL     QTWKDT        XQYM              5 0
     C*
     C* WRITE S/R RECORDS FOR EACH MONTH PRIOR TO TRANSFER DATE.
     C*
     C                   EXSR      SRPMTH
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE
     C*
     C* FOR RENEWAL ACCOUNTS CHECK DATE ON ORIGINAL BILL.
     C*
     C                   MOVE      'E'           XXSTAT
     C     EHKY2         CHAIN     PEH01                              60
     C*
     C* PROCESS ACCOUNT IF BILLED AFTER POLICY EFFECTIVE DATE.
     C*
     C     *IN60         IFEQ      *OFF
     C     EHTRDT        ANDGT     ELPFDT
     C*
     C                   MOVEL     ELPFDT        XXYM              5 0
     C                   MOVEL     EHTRDT        XQYM              5 0
     C*
     C* WRITE S/R RECORDS FOR EACH MONTH PRIOR TO BILLED DATE.
     C*
     C                   EXSR      SRPMTH
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELPFDT IFEQ
     C*
     C                   ENDSR                                                  SRLATE
     C*
     C*====================================================================
     C* SUBROUTINE TO WRITE S/R RECORDS FOR EACH PRIOR MONTH.
     C*====================================================================
     C*
     C     SRPMTH        BEGSR
     C*
     C     XXYM          DOWLT     XQYM
     C     XXYM          ANDLT     XXYYMM
     C     XXYM          ANDLE     XXTPYM
     C*
     C* IF NO S/R PAYROLLS FOUND FOR PRIOR MONTH, WRITE THEM.
     C*
     C     SRKY          CHAIN     PSR01                              60
     C     *IN60         IFEQ      *ON
     C*
     C                   MOVEL     XXYM          XXEPDT            7 0
     C                   MOVE      MD(XXMM)      XXEPDT
     C*
     C     EPKY          CHAIN     PEP00                              60
     C     *IN60         DOWEQ     '0'
     C     EPEFDT        ANDLE     XXEPDT
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C                   Z-ADD     0             XXWK50            5 0
     C                   MOVEL     EPCADT        XXWK50
     C     EPCADT        IFEQ      0
     C     EPCADT        ORNE      0
     C     XXWK50        ANDGE     XXYM
     C*
     C* WRITE A S/R RECORD FOR THIS CLASS CODE.
     C*
     C                   EXSR      SRWRIT
     C*
     C                   ENDIF                                                  EPCADT IFEQ
     C*
     C                   ENDIF                                                  EPSTAT IFNE
     C*
     C     EPKY          READE     PEP00                                  60
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ADD       1             XXMM
     C     XXMM          IFGT      12
     C                   ADD       1             XXYY
     C                   Z-ADD     1             XXMM
     C                   ENDIF                                                  XXMM IFGT
     C*
     C                   ENDDO                                                  XQYM DOWGT
     C*
     C                   ENDSR                                                  SRPMTH
     C*
     C*====================================================================
     C* SUBROUTINE TO A S/R RECORD FOR A CLASS CODE.
     C*====================================================================
     C*
     C     SRWRIT        BEGSR
     C*
     C                   CLEAR                   PSR01
     C*
     C                   Z-ADD     EPCO#         SRCO#
     C                   Z-ADD     EPFND         SRFND
     C                   Z-ADD     EPEMP#        SREMP#
     C                   Z-ADD     EPDIV         SRDIV
     C                   Z-ADD     EPFYR         SRFYR
     C                   Z-ADD     EPCLAS        SRCLAS
     C                   MOVEL     EPSPEC        SRSPEC
     C                   MOVEL     EPEXT         SREXT
     C                   Z-ADD     EPEFDT        SREFDT
     C***  Begin Add  ***  06/08/20  ******************************
     C                   Z-ADD     EPcaDt        SRcaDt
     C***  End   Add  ***  06/08/20  ******************************
     C                   Z-ADD     XXYM          SRRPMY
     C                   Z-ADD     0             SRAMTP
     C                   MOVEL     'S'           SRTYFL
     C                   MOVEL     Q1USER        SRIUSR
     C                   MOVEL     Q1USER        SRUSER
     C                   MOVEL     Q1PGM         SRIPGM
     C                   MOVEL     Q1PGM         SRPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SRIDT
     C                   MOVEL     SRDTO         SRDT
     C                   MOVEL     DSTIME        SRITM
     C                   MOVEL     DSTIME        SRTIME
     C*
     C                   WRITE     PSR01
     C*
     C                   ENDSR                                                  SRWRIT
     C*================================================================
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
     C     XXILEF        IFLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELSDEX        ANDNE     ELSDEF
     C     ELSDEX        ANDNE     *ZERO
     C     XXILEF        ORLE      XXRPTO
     C     XXILEX        ANDGE     XXRPFM
     C     ELSDEX        ANDEQ     *ZERO
     C*
     C                   MOVE      *ON           *IN28
     C     ELDBCR        IFGT      1
     C     RRDBHD        IFEQ      *BLANKS
     C                   MOVEA     CON(7)        TMP
     C                   Movel     Con(7)        xxWk20
     C                   ELSE
     C                   MOVEL(P)  RRDBHD        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF                                                  RRDBHD IFEQ
     C                   ELSE
     C     RRCRHD        IFEQ      *BLANKS
     C                   MOVEA     CON(8)        TMP
     C                   Movel     Con(8)        xxWk20
     C                   ELSE
     C                   MOVEL(P)  RRCRHD        XXWK20
     C                   MOVEA     XXWK20        TMP
     C                   ENDIF                                                  RRCRHD IFEQ
     C                   ENDIF                                                  ELDBCR IFGT
      *
     C     CLTID         IFEQ      '136'
     C                   MOVEA     CON(14)       TMP
     C                   Movel     Con(14)       xxWk20
     C                   ENDIF
      *
     C                   MOVE      '.'           TMP(23)
     C                   SELECT
     C     CLTID         WHENEQ    '136'
     C     ELDBCR        MULT(H)   ELCMUL        ELDBCR            5 4
     C                   MOVE      ELDBCR        XXASP1            5
     C                   OTHER
     C                   MOVE      ELDBCR        XXASP1            5
     C                   ENDSL
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      NUM(1)        TMP(22)
     C                   MOVE      NUM(2)        TMP(24)
     C                   MOVE      NUM(3)        TMP(25)
     C                   MOVE      NUM(4)        TMP(26)
     C                   MOVE      NUM(5)        TMP(27)
     C                   MOVEA     TMP           HDG(A)
     C                   MOVEL     xxWk20        sHdg(b)
     C                   Eval      dbcrHdg = sHdg(b)
     C                   Z-add     ELDBCR        sFact(b)
     C                   Eval      b = b + 1
     C                   CLEAR                   TMP
     C     XXILEF        IFGT      XXRPFM
     C     XXILEX        ORLT      XXRPTO
     C*
     C     XXILEF        IFLT      XXRPFM
     C                   MOVE      XXRPFM        XXFRDT
     C                   ELSE
     C                   MOVE      XXILEF        XXFRDT
     C                   ENDIF
     C*
     C     XXILEX        IFGT      XXRPTO
     C                   MOVE      XXRPTO        XXTODT
     C                   ELSE
     C                   MOVE      XXILEX        XXTODT
     C                   ENDIF
     C*
     C                   EXSR      SRPRO
     C                   SUB       1             ELDBCR
     C                   MULT(H)   XXPRO         ELDBCR
     C                   ADD       1             ELDBCR
     C                   Z-add     ELDBCR        sProF(b)
     C*
     C*
     C                   MOVEA     ')**'         TMP(8)
     C                   MOVE      '.'           TMP(3)
     C                   MOVE      ELDBCR        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C                   MOVE      '('           TMP(1)
     C                   MOVE      NUM(1)        TMP(2)
     C*
     C                   MOVE      NUM(2)        TMP(4)
     C                   MOVE      NUM(3)        TMP(5)
     C                   MOVE      NUM(4)        TMP(6)
     C                   MOVE      NUM(5)        TMP(7)
     C                   MOVEA     TMP           PRO(A)
     C                   CLEAR                   TMP
     C*
     C                   ENDIF
     C*
     C     ELDBCR        MULT(H)   XXFACT        XXFACT
     C     XXWVFL        IFLT      3
     C                   MULT(H)   ELDBCR        XXWFAC
     C                   ENDIF
     c                   If        cltID = '102' And isNetRated
     c                   Eval      hdg(a) = ' '
     c                   Eval      pro(a) = ' '
     c                   Else
     C                   ADD       1             A
     C                   EndIf
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     XXFACT        SVFACT           11 9
     C*
     C                   ENDSR                                                  CALDBR
     C     *INZSR        BEGSR
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3

      /Free
       ChkPgmInStack('W7615R':pgmInStack);
       If pgmInStack = *On And cltID = '102';
          suppressDarts = *On;
       Endif;
      /End-free

     C     CLTID         IFEQ      '133'
     C     QQEMP#        IFEQ      *BLANKS
     C     QQEMP#        OREQ      *ZEROS
     C     QQFEMP        IFGT      *ZERO
     C     QQFEMP        ANDNE     QQTEMP
     C                   CALL      'CLRDIR'
     C                   ENDIF
     C                   ENDIF
     C                   CALL      'SA007C'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   MOVEL(P)  ##RTCD        XXENV             1
     C                   ENDIF
     C                   MOVE      *OFF          *IN59

      /Free
       intCurDt7 = %Dec(%Char(%Date():*Cymd0):7:0);
      /End-Free
     C                   ENDSR
      *
      *
      *====================================================================
      *  GETPSR - GET Policy Services Representative, if entered
      *====================================================================
     C     GETPSR        BEGSR
      *
     C                   MOVEL(P)  XXDIRI        XXBINQ
      *
     C     EMPSRV        IFNE      *BLANKS
     C     EMPSRV        CHAIN     PUP02                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     XXBINQ        IFNE      *BLANKS
     C                   CAT(P)    ',':0         XXBINQ
     C                   ENDIF
      *
     C                   CAT(P)    UPUSNM:1      XXBINQ
      *
     C     UPEXT#        IFNE      *ZERO
     C                   MOVEL(P)  UPEXT#        XXNUM
     C                   MOVEL     *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         XXTMP1            4
     C                   CAT(P)    ', ext.':0    XXBINQ
      *
      * Remove leading blanks
      *
     C     ' '           CHECK     XXNUM         X1                3 0
     C                   SUBST(P)  XXNUM:X1      XXTMP1            4
      *
      * Append extension
      *
     C                   CAT(P)    XXTMP1:1      XXBINQ
     C                   ENDIF                                                  UPEXT#
      *
     C                   ENDIF                                                  *IN60
      *
     C                   ENDIF                                                  EMPSRV
      *
     C                   ENDSR                                                  GETPSR
      *====================================================================
     C     SRVEND        BEGSR
     C                   MOVE      *BLANKS       XXVEND
     C                   Z-ADD     XXTDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY5
     C                   MOVEL     SRDTO         #XXTDT            7 0
      *
     C     VHKY2         SETGT     PVH01
      *
     C     ELKEY         READPE    PVH01                                  60
     C     *IN60         IFEQ      *OFF
      *
     C     VPKY          CHAIN     PVP01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     'Vendor#:'    XXVEND
     C                   MOVEL     VHVDR#        VHVDRC            5
     C                   CAT       VHVDRC:1      XXVEND
     C                   CAT       VPNAME:1      XXVEND
     C                   ENDIF
     C                   ENDIF
     C                   ENDSR                                                  SRVEND
      *================================================================
      * GETDSC  - GET EXTENDED DESCRIPTION IF APPLICABLE
      *================================================================
     C     GETDSC        BEGSR
      *
     C     EPEXT         IFNE      *BLANKS
      *
      * LOOK FOR GLOBAL EXTENSION DESCRIPTION
      *
     C                   CLEAR                   PPX01
     C                   MOVE      *BLANKS       XXST
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C     *IN60         DOWEQ     *OFF
     C     PXSTAT        IFEQ      'A'
     C                   MOVE      *OFF          *IN82
     C                   MOVEL(P)  PXDESC        XXDESC
     C                   LEAVE
     C                   ENDIF
     C     PXKY1         READPE    PPX01                                  60
     C                   ENDDO
      *
      * LOOK FOR STATE SPECIFIC DESC IF GLOBAL DESCRIPTION DOESN'T EXIST
      *
     C     *IN60         IFEQ      *ON
     C     RSKY          CHAIN     PRS01                              61
     C                   MOVE      RSJRST        XXST
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C     *IN60         DOWEQ     *OFF
     C     PXSTAT        IFEQ      'A'
     C                   MOVE      *OFF          *IN82
     C                   MOVEL(P)  PXDESC        XXDESC
     C                   LEAVE
     C                   ENDIF
     C     PXKY1         READPE    PPX01                                  60
     C                   ENDDO
     C                   ENDIF                                                  *IN60 = *ON
      *
      *  DISPLAY TERRITORY DIFFERENTIAL VERBIAGE, IF APPLICABLE
      *
     C     EPEXT         IFNE      *BLANKS
     C     PXCODE        ANDNE     *ZERO
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *OFF          *IN32
     C                   ENDIF                                                  EPEXT IFNE
      *
      * GET EXTENSION EXPANDED  DESCRIPTION FOR CAREWEST
      *
     C     CLTID         IFEQ      '136'
     C     PXKY          SETGT     PPX01E
     C     PXKY1         READPE    PPX01E                                 60
     C     *IN60         DOWEQ     *OFF
     C     PXSTAT        IFEQ      'A'
     C     PXEXDS        ANDNE     *BLANK
     C                   MOVE      *ON           *IN82
     C                   MOVEL     PXEXDS        XXW100          100
     C                   MOVEL(P)  XXW100        XXDES1
     C                   MOVE(P)   XXW100        XXDES2
     C                   LEAVE
     C                   ENDIF
     C     PXKY1         READPE    PPX01E                                 60
     C                   ENDDO
     C                   ENDIF
      *
     C                   ENDIF                                                  EPEXT <> *BLANK
      *
     C                   ENDSR                                                  GETDSC
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      LDEXP1        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
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
     C                   MOVE      ELPTDT        XXILEX
     C                   ELSE
     C                   MOVE      LDEXP2        XXILEX
     C                   ENDIF
     C*
     C     XXILEF        IFLE      XXRPDT
     C     XXILEX        ANDGE     XXRPFM
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
      *============================================================
      /free

       // *=========================================================================
       // *  ScriptXML - Reserve/Update XML record for Scriptura output.
       // *=========================================================================
       Begsr ScriptXML;

       xmlFile = ' ';
       message = %trim(processType);
       string = ' ';

       If isMassReq;

          StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
           'Mnthly_Int':'000':'000':massFlag:xmlLink);

          isMassReq = *off;
          xmlFileName = xmlFile;
          xmlLink = xmlFile;
          Chain (xmlFile) xp100;
          XPCat = 'PS';
          Update xp100;

       Endif;

       //*** begin chg ***  02/03/23  **************************************
       //***StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):xxPre:
       //*** string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

       //* When generating all reports on a policy, the RRN will be the same.
       If svRRN = x#RRN;
          templateFile = XPFile;
          StLetter('TEMPLRQST':templateFile:message:'E':%editc(x#RRN:'X'):xxPre:
           string:%editc(xxCo#:'X'):%editc(xxFnd:'X'));
          xmlFile = templateFile;
       Else;
          StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):xxPre:
           string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);
       //*** end add *****  02/03/23  **************************************

       If xmlFileName = ' ';
          xmlFileName = xmlFile;
       Endif;

       If xmlLink = ' ';
          Chain (xmlFile) xp100;
          xmlFile = XPXmlT;
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       XPData = 'Y';
       Select;
       When strPortalRunType = 'EFT Payment';
          Chain (xxCo#:'N001':'EFTPAYCONF') pno00;
          If NOdart <> *Blanks;
             XPRmCd = NOdart;
             XPDart = 'Y';
          Endif;
       When strPortalRunType = '3rd Party Success';
          Chain (xxCo#:'N001':'3RDPAYCONF') pno00;
          If NOdart <> *Blanks;
             XPRmCd = NOdart;
             XPDart = 'Y';
          Endif;
       Endsl;

       XPPgm  = Q1PGM;
       XPUser = Q1USER;
       XPTime = DsTime;
       XPDt   = saveDate;

       Update xp100;
       //*** begin add ***  02/03/23  **************************************
       Endif;
       svRRN = x#RRN;
       //*** end add *****  02/03/23  **************************************

       Endsr;
       // *=========================================================================
       // *  ScriptRemove - Remove the record for this interim as it has no payroll.
       // *=========================================================================
       Begsr ScriptRemove;

       If xmlLink = ' ';
          Chain (xmlFileName) xp100;
          //* Cancel the Scriptura requeset since no form to generate.
          xmlFileName = ' ';
       Else;
          Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
       Endif;

       If %found;
          Delete xp100;
       Endif;

       Endsr;
       // *=========================================================================
       // *  ScriptMain - Write the main workfile record for Scriptura output.
       // *=========================================================================
       Begsr ScriptMain;

       If not isUseXML;
          Leavesr;
       Endif;

       //* Write the main work file record.
       Clear pmi01;

       M1XML = xmlFile;

       M1Co#  = xxCo#;
       M1Fnd  = xxFnd;
       M1Emp# = xxEmp#;
       M1Div  = xxDiv;
       M1Fyr  = xxFyr;

       If *in25 = *on;
          M1PrmWord = 'Contribution';
       Else;
          M1PrmWord = 'Premium';
       Endif;

       If *in62 = *on;
          M1NetRate = 'Y';
          M1NetVerb = 'Net';
       Endif;

       If *in71 = *on;
          M1WaivBal = 'Y';
       //*** begin add ***  05/31/23  ***********************************
          M1PBalTxt = xxPbal;
       //*** end add *****  05/31/23  ***********************************
       Endif;

       M1Logo = F2BLOG;
       M1FndNam = %trim(xxFnnm);
       M1PrtDate = xxCurd;
       If xadued <> ' ';
          M1PayDate = xxDued;
       Endif;
       M1PfDate  = x1pfdt;
       M1PtDate  = x1ptdt;
       M1RfDate  = xxfdt;
       M1RtDate  = xxtdt;

       If EXAHDR <> ' ';
          M1AgtHdg = %trim(EXAHDR) + ':';
       Else;
          M1AgtHdg = 'Agent:';
       Endif;

       M1Agt# = EBAgt#;
       M1AgtNam = xxAnam;
       M1AgtArea = #2Area;
       M1AgtPhn  = #2Phon;

       Chain(n) (xxCo#:xxFnd:xxAgt#) paa01;
       If %found;
          Chain(n) (xxCo#:AAFein:AAMod) pag01;
          If %found;
             M1AgtAdr  = AGMad1;
             M1AgtCity = AGMCty;
             M1AgtSt   = AGMSt;
             M1AgtZip  = AGMZip;
          Endif;
       Endif;

       M1PolFdnm = EMFdnm + EMFdn2;
       M1PolName = EMName + EMFdn2;
       M1PolCont = xxCont;
       M1PolAdr1 = xxMAD1;
       M1PolAdr2 = xxMAD2;
       M1PolCity = xxeCity;
       M1PolSt   = xxeSt;
       M1PolZip  = xxeZip;

       M1RefHdg  = 'Policy No.:';
       M1Policy# = xxPol#;
       M1RateSt  = xxSt;

       If xxCap# <> ' ';
          M1CarHdg = 'Carrier Policy#:';
          M1CarPol = RPCap#;
       Endif;


       If *in74 = *on;
          M1ProPerc = 'Y';
       Endif;

       M1TerHdg = xxTerH;
       M1TaxHdg = xxThd;

       If *in96 = *on;
          M1ExpHdg = 'Merit Rating';
          M1Merit = 'Y';
       Else;
          M1ExpHdg = 'Experience Modifier';
       Endif;

       M1IncLmt = xlPct;
       M1BefMod = xxDis1;
       M1ExpMod = xxExmd;
       M1AftMod = xxDisc;
       M1TerRte = xxTerr;
       M1TaxRte = RRTxpc;
       M1PrvBal = pBal;
       M1SRFee  = srFee;

       // Clear Experience Mod Fields if Not Rated
       If isNotRated;
          Clear M1ExpHdg;
          Clear M1expMod;
       EndIf;

      //* Populate premium figures for populated forms.

       Select;
       When strPortalRunType <> *Blanks And isNetRated;
          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pre00;
          dsFactors = REfact;
          M1TotMan = REprem;
          M1TotPrm = REprem;
          M1TerAmt = dsterr;
          M1TotPrm = M1TotPrm - M1TerAmt;
          M1TaxAmt = REtax;
          M1PrvBal = REprev;
          M1PrvBilld = REpPrm + REpTax;
          M1TotDue = M1TotPrm - M1PrvBilld +
           M1TaxAmt + M1PrvBal;
        //***  Begin Add  ***  10/20/22  ********
          M1TotDue = M1TotDue + M1TerAmt;
        //***  End   Add  ***  10/20/22  ********
       //*** begin add ***  05/31/23  ***********************************
          If dsWaiv <> 0 or dsRein <> 0;
             M1PrvBal += dsWaiv;
             M1PrvBal += dsRein;
             M1TotPrm -= dsWaiv;
             M1TotPrm -= dsRein;
             M1PBalTxt = REPBalTxt;
          Else;
             Clear M1PBalTxt;
          Endif;
       //*** end add *****  05/31/23  ***********************************
       When strPortalRunType <> *Blanks;
          Chain (ELco#:ELfnd:ELemp#:ELdiv:ELfyr) pre00;
          dsFactors = REfact;
          M1TotMan = REmapr;
          M1IncLmt = dsLmPc;
          M1IncAmt = %Dech(REmapr * dsLmPc / 100:11:0);
          M1SubTot1 = dsTot1;
          M1BefMod = dsDscB;
          M1SubTot2 = dsTot2;
          M1ExpMod = dsExpM;
          M1SubTot3 = dsTot3;
          M1AftMod = dsDscA;
          M1TotPrm = REprem;
          M1TerAmt = dsTerr;
        //***  Begin Add  ***  10/20/22  ********
          M1TotPrm = M1TotPrm - M1TerAmt;
        //***  End   Add  ***  10/20/22  ********
          M1TaxAmt = REtax;
          M1PrvBal = REprev;
          M1PrvBilld = REpPrm + REpTax;
          M1TotDue = M1TotPrm - M1PrvBilld +
           M1TaxAmt + M1PrvBal;
        //***  Begin Add  ***  10/20/22  ********
          M1TotDue = M1TotDue + M1TerAmt;
        //***  End   Add  ***  10/20/22  ********
       //*** begin add ***  05/31/23  ***********************************
          If dsWaiv <> 0 or dsRein <> 0;
             M1PrvBal += dsWaiv;
             M1PrvBal += dsRein;
             M1TotPrm -= dsWaiv;
             M1TotPrm -= dsRein;
             M1PBalTxt = REPBalTxt;
          Else;
             Clear M1PBalTxt;
          Endif;
       //*** end add *****  05/31/23  ***********************************
       When qqCompleted = 'Y' And isNetRated;
          M1TotMan = xxTotMan;
          M1TotPrm = xxTotMan;
          Eval(h) xxwk90 = xxTotPay * M1TerRte / 100;
          M1TerAmt = xxwk90;
          Eval(h) xxwk90 = (M1TotPrm + M1TerAmt) * (M1TaxRte / 100);
          M1TaxAmt = xxwk90;
          If M1PrvBilld <> 0;
             M1SRfee = 0;
          Endif;
          M1PrvBilld = decPrvBilled;
          M1TotDue = M1TotPrm + M1TerAmt + M1TaxAmt +
          M1SRFee + M1PrvBal - M1PrvBilld;
       When qqCompleted = 'Y';
          M1TotMan = xxTotMan;
          Eval(h) xxwk90 = xxTotMan * M1IncLmt / 100;
          M1IncAmt = xxwk90;
          M1SubTot1 = M1TotMan + M1IncAmt;
          Eval(h) xxwk90 = M1SubTot1 * M1BefMod;
          M1SubTot2 = xxwk90;
          If M1ExpMod <> 0;
             Eval(h) xxwk90 = M1SubTot2 * M1ExpMod;
          Endif;
          M1SubTot3 = xxwk90;
          Eval(h) xxwk90 = M1SubTot3 * M1AftMod;
          M1TotPrm = xxwk90;
          Eval(h) xxwk90 = xxTotPay * M1TerRte / 100;
          M1TerAmt = xxwk90;
          Eval(h) xxwk90 = (M1TotPrm + M1TerAmt) * (M1TaxRte / 100);
          M1TaxAmt = xxwk90;
         // Subtract previous billed in total calculation.
         // Zero out installment fee because it has already been billed
         //  and is included in previous balance.
          If M1PrvBilld <> 0;
             M1SRfee = 0;
          Endif;
          M1PrvBilld = decPrvBilled;
          M1TotDue = M1TotPrm + M1TerAmt + M1TaxAmt +
          M1SRFee + M1PrvBal - M1PrvBilld;
       Endsl;

       // Due Date in upper R/H corner
       M1PayDate = *Zeros;

       // Direct Inquiries To:
       M1DirInq  = xxDiri;
       M1OffPhn  = OFAPhn;
       M1OffArea = OFAAra;

       // Remittance section
       Select;
       When strPortalRunType = 'EFT Payment'
        Or strPortalRunType = '3rd Party Success';
          If M1TotDue > 0;
             M1RemitHdr = 'Online Payment Details:';
             M1PayDat2  = %Dec(%Date():*Usa);
             M1PayAmt   = M1TotDue;
             M1RemitFtr = 'Premium paid using online services.';
          Else;
             M1RemitHdr = 'No Payment Due At This Time';
          Endif;
       When strPortalRunType = 'Pending Submission';
          M1RemitHdr = 'Report Pending Submission';
       Other;
          M1RemitHdr = 'Make Check Payable To:';
          M1OffNam  = OFCknm;
          M1OffAd1  = OFAad1;
          M1OffAd2  = OFAad2;
          M1OffCity = OFActy;
          M1OffSt   = OFAst;
          M1OffZip  = OFAzip;
          If xadued <> ' ';
             M1PayDate = xxDued;
          Endif;
       EndSl;

       M1Month01 = x1mn;
       M1Month02 = x2mn;
       M1Month03 = x3mn;
       M1Month04 = x4mn;
       M1Month05 = x5mn;
       M1Month06 = x6mn;
       M1Month07 = x7mn;
       M1Month08 = x8mn;
       M1Month09 = x9mn;
       M1Month10 = x10mn;
       M1Month11 = x11mn;
       M1Month12 = x12mn;
       M1Month13 = x13mn;
       M1Month14 = x14mn;
       M1Month15 = x15mn;
       M1Month16 = x16mn;
       M1Month17 = x17mn;
       M1Month18 = x18mn;
       M1Month19 = x19mn;
       M1Month20 = x20mn;
       M1Month21 = x21mn;
       M1Month22 = x22mn;
       M1Month23 = x23mn;
       M1Month24 = x24mn;

       M1FORMCMP = qqCompleted;

       M1Pgm  = Q1PGM;
       M1User = Q1USER;
       M1Time = DsTime;
       M1Dt   = saveDate;

       //*** Begin Add *** 06/12/24 ***************************************
       // Check and see if SR Assessments being done
       Chain (xxCo#:xxFnd:xxFyr) pf101;
       If F1srAs = 'Y' Or ELsrAs = 'Y';
          M1srAs = 'Y';
       EndIf;

       // Process Assesments
       outIdx = 0;
       If M1srAs = 'Y';
          For idx = 1 to 7;
             If aLvl(idx) <> ' ';
                outIdx += 1;
                sass(outIdx) = outIdx;
                // Assign headers
                If aHdg(idx) = ' ';
                   Select;
                   When idx = 1;
                      hass(outIdx) = '1st Assessment';
                   When idx = 2;
                      hass(outIdx) = '2nd Assessment';
                   When idx = 3;
                      hass(outIdx) = '3rd Assessment';
                   When idx = 4;
                      hass(outIdx) = '4th Assessment';
                   When idx = 5;
                      hass(outIdx) = '5th Assessment';
                   When idx = 6;
                      hass(outIdx) = '6th Assessment';
                   When idx = 7;
                      hass(outIdx) = '7th Assessment';
                   EndSL;
                Else;
                   hass(outIdx) = aHdg(idx);
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
                   pass(outIdx) = 0;
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
                   pass(outIdx) = decAssessmentPercent;

                EndIf;

                // Calculate the Assessment Amount
                If qqCompleted = 'Y';
                   aass(outIdx) = %DecH((M1totPrm * pass(outIdx))/100:11:0);
                   decTotalAssessmentAmt += aass(outIdx);
                EndIf;

             EndIf;
          EndFor;
       EndIf;

       // Populate the total seq#
       If outIdx > 0;
          outIdx += 1;
          M1totSeq# = outIdx;
          M1totDue += decTotalAssessmentAmt;
       EndIf;
       //*** End   Add *** 06/12/24 ***************************************

       //*** Begin Add *** 06/24/24 ***************************************
       // Collateral Reported Percentage
       If ELcolrp <> 0;
          M1colrp = ELcolrp;

          // Calculate Collateral Amount
          If qqCompleted = 'Y';
             M1colra = %DecH((M1TotMan * ELcolrp)/100:11:0);
             M1TotDue = M1TotDue + M1colra;
          EndIf;
       Endif;
       //*** End   Add *** 06/24/24 ***************************************

       Write pmi01;

       //* Write the discounts to file.
       y = 1;
       Dow y < b;

          Clear pmi02;

          M2XML = xmlFile;
          M2Type = 'D';
          If (cltID <> '102') Or
             (cltID = '102' And sHdg(y) <> dbcrHdg) Or
             (cltID = '102' And sHdg(y) = dbcrHdg And Not isNetRated);
             M2DiscHdg = sHdg(y);
             M2DiscFac = sFact(y);
             M2DiscPer = sPerc(y);
          EndIf;
          M2ProrFac = sProF(y);
          M2ProrPer = sProP(y);
          M2Approx  = sAppr(y);

          M2Pgm  = Q1PGM;
          M2User = Q1USER;
          M2Time = DsTime;
          M2Dt   = saveDate;

          if M2DiscHdg <> ' ';
             Write pmi02;
          Endif;

          y = y + 1;
       Enddo;

       Endsr;
       // *=========================================================================
       // *  ScriptPay - Write the payroll workfile record for Scriptura output.
       // *=========================================================================
       Begsr ScriptPay;

       If not isUseXML;
          Leavesr;
       Endif;

       Clear pmi02;

       M2XML = xmlFile;
       M2Type = 'P';

       M2EPClas  = EPClas;
       M2EPSpec  = EPSpec;
       M2EPExt   = EPExt;
       M2EPefdt  = getUsaDate(EPefdt);
       M2EPcaDt  = getUsaDate(EPcaDt);
       M2Desc    = xxDesc;
       M2Rate    = xxArat;
       M2NetRt   = xnArat;
       M2payroll = intPayroll;
       // *In78 set *On in WasrpSet
       // xxWk90 is set to SRManPrm in WasrpSet
       // Recalculating M2Premium for net rate
       //  works best for base and Clt 136.
       Select;
       When *In78 = *Off;
       When isNetRated = *On;
          Eval(h) M2Premium = M2Payroll * M2NetRt / 100;
       //*** begin add ***  11/22/22  ********************************
          If PRPCF = 'C';
             Eval(h) M2Premium = M2Payroll * M2NetRt;
          Endif;
       //*** end add *****  11/22/22  ********************************
          xxTotMan += M2Premium;
          xxTotPay += M2Payroll;
       Other;
          M2Premium = xxwk90;
          xxTotMan = xxTotMan + xxwk90;
          xxTotPay = xxTotPay + M2payroll;
       EndSl;
       If *in32 = *on;
          M2TerDiff = 'Territory Differential';
       Endif;

       M2Pgm  = Q1PGM;
       M2User = Q1USER;
       M2Time = DsTime;
       M2Dt   = saveDate;

       //*** Begin Add *** 06/12/24 ****************************************
       // Total Payrolls into F types and Other types so as to be able to
       // calculate payroll factors.
       Select;
       When EPspec = 'F' Or PRlhCd = 'Y';
          decTotalFClassAmt += EPempr;
       Other;
          decTotalOtherClassAmt += EPempr;
       EndSl;

       decTotalAllClassAmt += EPempr;
       //*** End   Add *** 06/12/24 ****************************************

       Write pmi02;
       noPayroll = ' ';

       Endsr;
       //=========================================================================

       //=========================================================================
       //  Set values from WASRP
       //=========================================================================
       Begsr WASRPset;

        Clear xxPayroll;
        intPayroll = 0;
        Clear *In78;
        Clear *In83;
        Select;
        //*** begin chg ***  08/27/24  ********************************************
        //*****When strPortalRunType = *Blanks;
        When strPortalRunType = *Blanks or
         strPortalRunType = 'EFT Payment';
        //*** end chg *****  08/27/24  ********************************************
           Chain (EPco#:EPfnd:EPfyr:EPemp#:EPdiv:
            EPclas:EPspec:EPext:x5RpCYM) psr04e;
           If %Found;
              *In78 = *On;
              *In83 = *On;
              xxPayroll = %editc(%int(SRamtP):'1');
              intPayroll = SRAmtp;
              //*** Beg Chg ***** 04/18/24  *************************
              //***xxWk90 = SRManPrm;
              Eval(h) xxWk90 = SRManPrm;
              //*** Beg Chg ***** 04/18/24  *************************
           Endif;
        Other;
           Exec Sql
             Select SRamtp, SRManPrm
             Into :decHoldPayrAmt, :decHoldManPrem
             From WASRP_HLD
             Where SRco# = :EPco#
              And SRfnd = :EPfnd
              And SRemp# = :EPemp#
              And SRdiv = :EPdiv
              And SRfyr = :EPfyr
              And SRclas = :EPclas
              And SRspec = :EPspec
              And SRext = :EPext
              And SRrpmy = :x5RpCYM;

           If sqlStt = '00000';
              Eval(h) intPayroll = decHoldPayrAmt;
              xxPayroll = %EditC(intPayroll:'1');
              Eval(h) xxWk90 = decHoldManPrem;
              *In78 = *On;
              *In83 = *On;
           Endif;
        EndSl;
       Endsr;
       //=========================================================================
       //  Reet values before WASRP
       //=========================================================================
       Begsr WASRPreset;
        xxWk90 = svWk90;
       Endsr;

      /end-free
      *
     OQSYSPRT2  E            ROUTE2         1 06
     O          E            ROUTE2         1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE2         1
     O               08      ORDIS2              60
     O          E            ROUTE2         1
     O               08      ORDIS3              60
     O          E            ROUTE2         2
     O               08      ORDIS4              60
     O          E            ROUTE2         2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
     O          E            ROUTE2         1 59
     O                       #BRND1
     O          E            ROUTE2         1
     O                       #BRND2
     O          E            HEAD2          1 06
     O                       DSTIME              08 '  :  :  '
     O                       XXCONM              72
     O                       DSDATE        Y    103
     O          E            HEAD2          2
     O                       Q1PGM               10
     O                       HDV(1)              92
     O                                           97 'PAGE:'
     O                       PAGE          Z    103
     O          E            HEAD2          1
     O                       HDV(2)              92
     O          E            HEAD2          2
     O                       HDV(3)              92
     O          E            HEAD2          2
     O                       XXFND         Z     05
     O                       FNNAME              66
     O          E            HEAD2          0
     O                       LINE
     O          E            HEAD2          2
     O                                           12 'EMPLOYER#'
     O                                           22 'DIVISION'
     O                                           33 'POLICY YR'
     O                                           48 'EMPLOYER NAME'
     O          E            DET2           1
     O                       EBEMP#        Z     12
     O                       EBDIV         3     22
     O                       EBFYR         Z     33
     O                       EBENAM              75
      /Free
       Dcl-Proc CheckFor3rdParty;
       Dcl-Pi *N Ind;
       End-Pi;
        Setgt (xxCo#:xxFnd:xxEmp#:xxDiv:
         'S/R Online Connect':intCurDt7) pp30l;
        Dou Not(%Eof(WMP3L));
           Readpe (xxCo#:xxFnd:xxEmp#:xxDiv) pp30l;
           If %Eof;
              Return *Off;
           Endif;
           Select;
           When P3enbld <> 'Y';
           When P3exdt <> 0 and P3exdt <= intCurDt7;
           When P3Efdt > intCurDt7;
           Other;
              Return *On;
           EndSl;
        Enddo;
        Return *Off;
       End-Proc;
      /End-Free

      //=========================================================
      // Get the full path of the document resolved to Darts
      //=========================================================
     pGetPath          b
     dGetPath          pi           250    varying
     d inDir                         80    varying const
     d inDoc                         12    varying const

     dRtvRoot          pr                  extpgm('RTVDOCROOT')
     d outRoot                       10

     dstrRoot          s             10
      /Free
       Select;
       When inDir = *blanks;
          Return *blanks;
       When inDoc = *blanks;
          Return *blanks;
       Other;
          RtvRoot(strRoot);
          Return '/' + %trim(strRoot) + '/' + inDir + '/' + inDoc;
       Endsl;
      /End-free
     pGetPath          e

     p GetUsaDate      b
     d GetUsaDate      pi             8  0
     d  inDate7                       7  0
     d  intEfDt        s              8  0
     c                   Z-add     inDate7       srdti
     c                   Call      'SRCVTDAT'    dtmdyy
     c                   Movel     srdto         intEfDt
     c                   Return    intEfDt
     p GetUsaDate      e

       //*** Beg Add *** 12/10/25 ******************************************
     p ChkRatSetCd     b
     d ChkRatSetCd     pi             1  0
     d  Comp#                         3  0
     d  RateCd                        3a
     d  inDate7                       7s 0
      *
     c     RSKY01        Klist
     c                   Kfld                    Comp#
     c                   Kfld                    RateCd
      *
     c     RSKY01        Chain(e)  PRS01
     c                   If        %Found
     c                   If        RS3DATE = 0
     c                             or inDate7 < RS3DATE
     c                   Return    2
     c                   Else
     c                   Return    3
     c                   Endif
     c                   Else
     c                   Return    2
     c                   Endif
     p ChkRatSetCd     e
       //*** End Add *** 12/10/25 ******************************************
**
JAN
FEB
MAR
APR
MAY
JUN
JUL
AUG
SEP
OCT
NOV
DEC
**
Deductible Credit
Managed Care Credit
Safety Credit
Deviation
Contractors Class
Stock Discount
Schedule Debit
Schedule Credit
2nd Deviation
State Tax
ARAP Charge
Experience Modifier
2nd Deductible Credit                                      01/08/04
Company Factor
Facultative Reinsurance                                    07/10/06
Foreign Terrorism
STBL Factor
HCN Credit                                                 11/10/10
Territory Rating
CON,20  future use
CON,21  future use
** MD - NUMBER OF DAYS PER MONTH
312831303130313130313031
** RMK - DARTS REMARK
A monthly interim was generated by
** HEADER VERBIAGE FOR EXCEPTION REPORT
       MONTHLY INTERIMS NOT GENERATED AS A RESULT OF FINAL BILLING STATUS
    A Monthly S/R Worksheet was not produced for the following accounts since
      a final bill (Calculate, Update A/R and Print) has not yet been run:
** MESSAGES
 ** PREVIOUS BALANCE INCLUDES
