     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     Hdftactgrp(*no) option(*nodebugio:*srcstmt)
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H/TITLE W7700R - PRINT BILLS - FINAL AUDITED
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 11/05/25 C:65  Support: 119977 (Prescient)                    *
     H*                Push the standard limits to scriptura workfile *
     H*                when there is no increased limits code.        *
     H*                                                               *
     H* 10/25/25 C:65  Support: 119977 (Prescient)                    *
     H*                Allow bill to be produced when no payroll are  *
     H*                present; write payroll record for "No Payroll".*
     h*                                                               *
     h* 08/28/25 C:349 Support: 119522 (CoveRisk)                     *
     h*                Allow Audit Bill diary requests to be written  *
     h*                to iDarts when Spool File field value in the   *
     h*                application configuration file is turned off.  *
     h*                If scriptura is active, the Audit Bill attach- *
     h*                ment generated in call to STLETTER is used to  *
     h*                update the iDarts audit diary attachments in   *
     h*                SDCMP.                                         *
     h*                                                               *
     h* 08/01/25 C:65  Support: 119398   (Sunz)                       *
     h*                Change the logic which determines which divs   *
     H*                will run. If a summarized div is running, or   *
     H*                if the expiration date includes the state rpt  *
     H*                div, then all summarized divisions will run.   *
     H*                If a non-summarized division with a different  *
     H*                expiration date than state rpt div runs then   *
     H*                do not process any summarized divisions. The   *
     H*                summary and combo bill will only generate if   *
     H*                the run is going to include the state rpt div. *
     H*                07/29/25 change entirely replaced.             *
     H*                                                               *
     H* 07/29/25 C:65  Support: 119398 (Sunz)                         *
     H*                Do not generate combo bill if running for a    *
     H*                single division, nor the A/R summary bill.     *
     H*                                                               *
     H* 07/25/25 C:65  Support: 119094 (CareWest)                     *
     H*                Do not clear the audit non-compliance amount   *
     H*                on premium line item for net rated policies.   *
     H*                                                               *
     H* 07/17/25 C:65  Support: 119398 (Sunz)                         *
     H*                When generating combination bill document if   *
     H*                the state reporting division does not produce  *
     H*                a bill, generate a blank document instead.     *
     H*                                                               *
     H* 06/18/25 C:291 Support: 119165, 119230                        *
     H*                Don't request XML for Summary Bill if none of  *
     H*                the Summarized divisions are processing through*
     H*                Audit Bill.                                    *
     H*                Delete Combined Bill dataset (xmlprep, xmltemp *
     H*                wxblp) if State Reporting division is not      *
     H*                processing through Audit Bill.                 *
     H*                                                               *
     H* 06/06/25 C:342 Support: 118514 (Cove Risk)                    *
     H*                1.If Policy is increased limit then use  WTLMP *
     H*                  file fields.                                 *
     H*                2.If Policy is not increased limit then use    *
     H*                  WDRRP file fields.                           *
     H*                3.RREMLB was missing to populate in wxblp file *
     H*                  for B0EMLB field.                            *
     H*                                                               *
     H* 05/12/25 C:300 Project: WC3008                                *
     H*                Display all Premium before Proration           *
     H*                Change heading CON(10) from 'Annual Premium'   *
     H*                to 'Audited Premium'                           *
     H*                Change heading CON(62) from 'Annual Contribution'
     H*                to 'Audited Contribution'                      *
     H*                                                               *
     H* 04/04/25 C:333 Support: 118762 (FCC)                          *
     H*                Audit Bill not Producing on CX Division.       *
     H*                                                               *
     H* 02/28/25 C:300 Support: 118624                                *
     H*                Waiver not printing on output correctly - PDSQA testing
     H*                When waiver is applied to multiple divisions   *
     H*                                                               *
     H* 02/27/25 C:291 Support: 118418                                *
     H*                Fixed Template order in Combined Bill document.*
     H*                                                               *
     H* 02/04/25 C:291 Support: 118418                                *
     H*                For each division included in Combined Bill    *
     H*                document, print that division's name/address   *
     H*                instead of State Rpt Division. To accomplish   *
     H*                This: Create SXDOP for each division included  *
     H*                in Combined Bill document.                     *
     H*                                                               *
     H* 11/11/24 C:300 Project: WC2985                                *
     H*                Apply Deductible Discount to Discounted Premium*
     H*                                                               *
     H* 11/08/24 C:65  Support: 117363 (Amwins)                       *
     H*                Change the total line item to include policy   *
     H*                fee amount from wdelp.                         *
     H*                                                               *
     H* 09/12/24 C:062 Ticket: 117449(146)                            *
     H*                Don't print due date or due amount if there    *
     H*                is a overall credit.                           *
     H*                                                               *
     H* 08/14/24 C:291 Project: WC2974                                *
     H*                Print Actual Increased Limits amount and       *
     H*                Balance to Increased Limits amount.            *
     H*                                                               *
     H* 08/05/24 C:062 Ticket: 117074(129)                            *
     H*                Correct print for audits.                      *
     H*                                                               *
     H* 05/20/24 C:291 Project: WC2961                                *
     H*                When Schedule DB/CR applied against Manual     *
     H*                Premium, Print the Schedule DB/CR line item    *
     H*                just before Waiver surchange it applied to     *
     H*                Manual Premium.                                *
     H*                                                               *
     H* 05/07/24 C:062 Project: WC2670                                *
     H*                Make sure summary bill attaches to state       *
     H*                reporting division email.and resolves to       *
     H*                state reporting division.                      *
     H*                                                               *
     H* 04/24/24 C:291 Project: WC2670                                *
     H*                When Merge Premium notice into combo document  *
     H*                is set to 'No', attach premium notice to the   *
     H*                Bill.                                          *
     H*                                                               *
     H* 04/23/24 C:062 Project: WC2670(115433)                        *
     H*                Exclude second step documents.                 *
     H*                                                               *
     H* 04/09/24 C:291 Project: WC2670                                *
     H*                Changed XXADLD field length to 9,2 same length *
     H*                as EPADLD.                                     *
     H*                                                               *
     H* 04/04/24 C:291 Project: WC2670                                *
     H*                Add Additional load and Expiration date on     *
     H*                summary bill.                                  *
     H*                                                               *
     H* 03/20/24 C:62  Project: WC2663(115064)(129)                   *
     H*                Print due amount if the audit is a credit.     *
     H*                                                               *
     H* 03/19/24 C:291 Project: WC2670                                *
     H*                Add iDarts Description.                        *
     H*                                                               *
     H* 03/12/24 C:62  Project: WC2670                                *
     H*                Don't print divisions if no payroll.           *
     H*                                                               *
     H* 12/05/23 C:300 Project: WC2810/Phw#114498 (Sunz)              *
     H*                (Implement 11/13/23 C:65 changes from W7615R)  *
     H*                Cleanup coding by replacing prior coding with  *
     H*                new re-written coding for the bottom premium   *
     H*                line items.  Add a few more line items to      *
     H*                Summary bill.                                  *
     H*                The balance to minimum figure will print       *
     H*                before and add into standard premium.          *
     H*                                                               *
     H* 09/26/23 C:62  Project: WC2670                                *
     H*                Add scriptura processing.                      *
     H*                                                               *
     H* 07/30/23 C:62  Project: WC2918                                *
     H*                Add Not rated logic to exp mod 2 and 3.        *
     H*                                                               *
     H* 07/07/23 C:300 Project: WC2810                                *
     H*                Print pre-minimum premium figures when a policy*
     H*                invokes minimum premium                        *
     H*                Print a balance to minimum line item to show   *
     H*                adjustment up to minimum                       *
     H*                Check XXMINF (Minimum Premium Flag) instead of *
     H*                ELAMNP to print WDMPP figures                  *
     H*                                                               *
     H* 06/14/23 C:65  Project: 150WFY                                *
     H*                Sunz request to print 3 decimal places for     *
     H*                large deductible percentage for MI (modified). *
     H*                                                               *
     H* 10/19/22 C:79  Project: 148WDS                                *
     H*                Audit Bill Denote"Not A Bill"Starstone Accts   *
     H*                                                               *
     H* 06/16/22 C:65  Project: Phw#102594 (CoveRisk)                 *
     H*                Ensure when running a mass audit to skip any   *
     H*                division that is not yet expired, either by    *
     H*                policy expiration date or by cancellation date *
     H*                if it exists.  Additionally, prevent print of  *
     H*                policies that have not been audited yet.       *
     H*                                                               *
     H* 01/28/22 C:65  Project: Phw#98830  (CoveRisk)                 *
     H*                Do not call premium noticed for A/R summarized *
     H*                divisions, only non-summarized.  Add a call to *
     H*                premium notice program within PRTSUM subroutine*
     H*                to generate a notice for the summarized.       *
     H*                                                               *
     H* 03/31/21 C:62  Project: WC2663                                *
     H*                -Use 3rd Party Verbiage if opted in.           *
     H*                                                               *
     H* 02/05/21 C:65  Project: Phw#94364 (THE)                       *
     H*                Reduce manual premium on summary page by incr  *
     H*                limits and waiver if minimum premium.          *
     H*                                                               *
     H* 01/29/21 C:65  Project: WC2734/Phw#87703 (CareWest)           *
     H*                Ensure Split Modifier shows if second modifier *
     H*                is populated, regardless of premium assigned   *
     H*                to it from split payrolls.                     *
     H*                                                               *
     H* 01/11/21 C:65  Project: 133WRA/Phw#93920 (LUBA)               *
     H*                The renaming of single division bills should   *
     H*                release the spool file if needed.              *
     H*                                                               *
     H* 01/08/21 C:65  Project: 133WRA/Phw#93734 (LUBA)               *
     H*                Do not delete the spool files for regular bill *
     H*                and summary bill prior to allowing them to     *
     H*                resolve to iDarts for LUBA as well.            *
     H*                                                               *
     H* 12/23/20 C:65  Project: Phw#93417 (SCS) - Related 133WRA      *
     H*                Do not delete the spool files for regular bill *
     H*                and summary bill when not client 133 to allow  *
     H*                them to resolve to iDarts.                     *
     H*                                                               *
     H* 11/23/20 C:65  Project: 148WCA                                *
     H*                Display Stop Gap Premium if client 148 and     *
     H*                the state is flagged as monopolistic and there *
     H*                is an additional load amount.                  *
     H*                                                               *.
     H* 06/10/20 C:136 Project: WC2734                                *
     H*                Show first mod as blank if not rated.          *
     H*                                                               *
     H* 05/12/20 C:62  Project: 102WIJ                                *
     H*                Don't print CR/DB Verbiage.                    *
     H*                                                               *
     H* 05/08/20 C:180 Support 84893                                  *
     H*                Reactivate 133WRA                              *
     H*                                                               *
     H* 01/30/20 C:180 Support 86820                                  *
     H*                Deactivate 133WRA                              *
     H*                                                               *
     H* 12/17/19 C:180 Project: 150WEH                                *
     H*                Add SR payroll Audit                           *
     H*                                                               *
     H* 12/10/19 C:65  Project: Phw#84671 (Sunz)                      *
     H*                Use XSPFDT for pulling assessments and terror- *
     H*                ism for NJ instead of ELPFDT - the state rpt   *
     H*                division's policy effective date.              *
     H*                                                               *
     H* 06/26/19 C:65  Project: Phw#76880                             *
     H*                Update from/to date for non-ARD rated policies *
     H*                for when pdat array indicates a split.         *
     H*                                                               *
     H* 05/03/19 C:180 Project: 133WRA                                *
     H*                Special spool file renaming for LUBA.          *
     H*                                                               *
     H* 02/11/19 C:291 Project: WC2206                                *
     H*                Only print first modifier if 1st Modifier is   *
     H*                Rated.                                         *
     H*                                                               *
     H* 12/21/18 C:291 Project: PHW75074                              *
     H*                Add Sign to NY Flat rate Adjustment amount.    *
     H*                Do not display Flat rate decrease amount when  *
     H*                minimum premium is invoked.                    *
     H*                                                               *
     H* 10/15/18 C:65  Project: 150WAR/Phw#50676                      *
     H*                Sunz request to print 3 decimal places for     *
     H*                large deductible percentage.                   *
     H*                                                               *
     H* 08/08/18 C:062 Project: 150WCS                                *
     H*                Add All Division Parameter                     *
     H*                                                               *
     H* 02/01/18 C:136 Project: 148WAH                                *
     H*                Add Policy Fee premium line item               *
     H*                                                               *
     H* 01/30/18 C:291 Project: WC2560                                *
     H*                NY Premium Adjustment eff 10/17                *
     H*                                                               *
     H* 10/13/17 C:65  Project: 150WBU                                *
     H*                Run audit bills on all divisions regardless of *
     H*                expiration date for client 150.                *
     H*                                                               *
     H* 04/07/17 C:062 Project 136WHF                                 *
     H*                Add Territory Rating for CareWest.             *
     H*                                                               *
     H* 03/23/17 C:65  Project: Phw#46291 (AMWINS)                    *
     H*                Clear rating state/heading for summary bill.   *
     H*                                                               *
     H* 11/16/16 C:136 Support: 44303                                 *
     H*                Correct 40481.  Clear verbiage for mass run.   *
     H*                                                               *
     H* 09/14/16 C:136 Project: WC2070                                *
     H*                Add Audit Noncompliance Charge                 *
     H*                                                               *
     H* 04/28/16 C:136 Support: 40481                                 *
     H*                Display exp mod/arap as split only if          *
     H*                the exp mod/arap factor is active within the   *
     H*                policy date range.                             *
     H*                                                               *
     H* 12/18/15 C:65  Project: Phw#39005 (LUBA)                      *
     H*                Release the audit bill spool file after        *
     H*                combination/resolve to Darts is complete.      *
     H*                The release statement was removed from the     *
     H*                resolve CL as part of Phw#38400.               *
     H*                                                               *
     H* 10/29/15 C:295 Project: WC2384                                *
     H*                Printing ARAP factors on bills                 *
     H*                                                               *
      * 10/20/15 C:302 PROJECT: WC2364                                *
      *                Add non-ratable class code flag                *
      *                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 07/07/15 C:306 Support: PHW31734                              *
     H*                Correction made to print either Large deductib-*
     H*                le or Deductible discount in the Premium break *
     H*                down (Audit Bill report)                       *
     H*                                                               *
     H* 06/17/15 C:65  Project: Redmine #4659 / Phw#35355             *
     H*                Check output routing AHBILLA Report #0 to      *
     H*                trigger the combination bill for CSI.          *
     H*                                                               *
     h* 08/26/14 C:307 Project: SY354                                 *
     h*                -Implement Routing Page Changes.               *
     h*                                                               *
     H* 03/11/15 C:65  Project: Redmine #4659 (Client 89 Request)     *
     H*                Check inactive status on the new report to     *
     H*                determine if combination processing should no  *
     H*                longer occur.                                  *
     H*                                                               *
     H* 02/06/15 C:298 Support: PHW33104                              *
     H*                Correction made to print Audited # of          *
     H*                Employees when Audit Bill  is run for          *
     H*                Cltid = 111                                    *
     H*                                                               *
     H* 10/24/14 C:65  Project: Redmine #4659 (Client 89 Request)     *
     H*                Add call to W7615C3 to combine individual div  *
     H*                bills into one spool file.  When called with   *
     H*                a blank DART flag it will combine the spool    *
     H*                files together.  When the DART flag is filled  *
     H*                then it will resolve the spool file and        *
     H*                delete/save/release it as indicated.  The      *
     H*                output routing settings from report #6 will    *
     H*                be used for the save/hold status.              *
     H*                                                               *
     H* 07/16/14 C:136 Project: WC2295 (OK Rate Split)                *
     H*                Add OK Rate Split date to the pDat array so    *
     H*                that split payroll display correctly           *
     H*                                                               *
     H* 07/11/14 C:279 Project: 111WBL                                *
     H*                Modified to subtract the dividend from the     *
     H*                premium prior to determining billing amount.   *
     H*                                                               *
     H* 06/16/14 C:136 Project: WC2283 (TN Rate Split)                *
     H*                Add TN Rate Split date to the pDat array so    *
     H*                that split payroll display correctly           *
     H*                                                               *
     H* 04/04/14 C:136 Project: 129WSB Priss #36                      *
     H*                Pass default value of 'Suppress Prior Year     *
     H*                Balance' flag from WMEXP to Premium Notice     *
     H*                call.                                          *
     H*                                                               *
     H* 02/07/14 C:65  Project: 129WSB (Added under SY336 Priss 63)   *
     H*                Add the new paramter to W7321C call.           *
     H*                                                               *
     H* 10/07/13 C:190 Project: 25WSS                                 *
     H*                Change the program to determine if             *
     H*                Premium or Contribution should be printed      *
     H*                based on Group setup.                          *
     H*                                                               *
     H* 06/25/13 C:65  Project: 147WAI                                *
     H*                Print a line item for dividend amount returned *
     H*                from the dividend API in the balance section   *
     H*                to reduce the total amount due on the policy.  *
     H*                Do not include dividend A/R when combining     *
     H*                the payments for total paid.                   *
     H*                Add binding directory WTBNDDIR as well.        *
     H*                                                               *
     H* 04/01/13 C:180 SUPPORT: WA093 81868                           *
     H*                Include final A/R balance when audited         *
     H*                for client 093.                                *
     H*
     H* 10/19/12 C:283 Support: WA089 80873                           *
     H*                Correct the Condition to Apply Flat Rate       *
     H*                Adjustment.                                    *
     H*                                                               *
     H* 09/06/12 C:277 Support: WA089 80649                           *
     H*                Make correction on -ve sign                    *
     H*                                                               *
     H* 12/01/11 C:189 Support: WA114 78487                           *
     H*                Summary page should show the rating state      *
     H*                of main state reporting division and not of    *
     H*                any other division                             *
     H*
     H* 10/13/11 C:191 Project: WC2079                                *
     H*                IL Premium Adjustment eff 9-1-11               *
     H*                                                               *
     H* 03/27/11 C:235 SUPPORT: 71062                                 *
     H*                MASS BILLING ISSUES                            *
     H*                Delete Bill Transaction  File record if        *
     H*                Audit Flag='Y' for division being processed in *
     H*                single employer mode for Calculate/Update/Print*
     H*                bills                                          *
     H*                                                               *
     H* 07/13/11 C:061 PROJECT: WC2037
     H*                Print "Unearn Prem Surcharge" in
     H*                "Premium Breakdown" section.
     H*                                                               *
     H* 06/06/11 C:58  PROJECT: WC1995                                *
     H*                Compile Only due to change in W7615O           *
     H*                ALSO change variable name Q1DATE, per change   *
     H*                of that variable in IBM DS SZQ1P               *
     H*                                                               *
     H* 04/21/11 C:235 PROJECT: 114WYM                                *
     H*                OCR for Cancellation/ Audit Bills              *
     H*                                                               *
     H* 03/16/11 C:190 PROJECT: 132WPI                                *
     H*                Print the new optional heading field for the   *
     H*                contractor's credit line item if it is not     *
     H*                blank(client 132 only).                        *
     H*                                                               *
     H* 01/03/11 C:189 Support: WA071 75315                           *
     H*                Print 'Final Audit Contribution' instead of    *
     H*                'Audited Contibution' for ECM                  *
     H*                                                               *
     H* 12/31/10 C:255 PROJECT: WC1918                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP FOR POLICY PAGE, ITEM 3C.       *
     H*                                                               *
     H* 12/16/10 C:065 SUPPORT:  WA025/75112                          *
     H*                Change the cancellation check logic to loop    *
     H*                backwards through the cancellation date array. *
     H*                                                               *
     H* 11/10/10 C:65  PROJECT: 139WPL                                *
     H*                Add new line item for HCN credit for GIC.      *
     H*                This credit will be applied just before subject*
     H*                premium.                                       *
     H*                                                               *
     H* 11/01/10 C:136 Support: WA139 72450                           *
     H*                Re-enable 71WIA to allow DARTS for all types   *
     H*                of bills.                                      *
     H*                                                               *
     H* 10/18/10 C:251 Project: WC1939                                *
     H*                09/01/2010 OK Rate split                       *
     H*                If a policy with OK coverage is unexpired as   *
     H*                of 09/01/10 then the policy will need to be    *
     H*                split for the rate decrease                    *
     H*                                                               *
     H* 10/05/10  C:189 PROJECT WC1776                                *
     H*                 Modify the code to show decimal places for    *
     H*                 payroll amounts in the policy page            *
     H*                                                               *
     H* 10/01/10 C:227 Support: 74394                                 *
     H*                Fix the issue of Assessment headings in Audit  *
     H*                bill. Changed 'Terrorism Risk Act' to          *
     H*                'Foreign Terrorism Chg' in CON array.          *
     H*                                                               *
     H* 09/09/10 C:246 Support: 72450                                 *
     H*                Fix the issue with skipping writing the darts  *
     H*                remarks when it encounters invalid remark code *
     H*                                                               *
     H* 09/07/10 C:255 PROJECT: WC1929                                *
     H*                Add changes in program to check flag 'Disable  *
     H*                DE Rate flag' based on this, changes for proje-*
     H*                ct WC1768 are disabled.                        *
     H*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                ADD CHANGES IN PROGRAM FOR 2ND PERIOD DEVIATION*
     H*                WHEN DEVIATIONS ARE SPLIT. 2ND PERIOD DEVIATIO-*
     H*                NS CHANGES WILL APPLY FOR (M,J,E,S,D) PREMIUM  *
     H*                LEVELS AS IT IS FOR 1ST PERIOD DEVIATIONS.     *
     H*                                                               *
     H* 05/13/10 C:65  PROJECT: Supp#72690                            *
     H*                Print Net Rate per 100 for TASB instead of     *
     H*                Net Rate, conditioned on indicator 27.         *
     H*                                                               *
     H* 05/07/10 C:136 Porject: WC1861 Priss 5                        *
     H*                Requirements changed for project               *
     H*                                                               *
     H* 04/07/10 C:136 Project: 25WRB PRISS 31                        *
     H*                Various changes needed.                        *
     H*                                                               *
     H* 04/05/10 C:220 PROJECT: 25WRQ                                 *
     H*                MOVE TITLE AND LOGO BOX FOR CLIENT - TASB      *
     H*                                                               *
     H* 03/26/10-A C:189 SUPPORT: WA114 71556                         *
     H*                Invoke minimum premium message priniting       *
     H*                even if policy does not invoke miniumum        *
     H*                                                               *
     H* 03/26/10 C:136 Project: 25WRC PRISS 26                        *
     H*                Addition changes to output                     *
     H*                                                               *
     H* 03/11/10 C:65  Project: 25WRC  Priss #22 - 25                 *
     H*                Stabilization factor debit is greater than 1,  *
     H*                not less than zero, as its a factor not a      *
     H*                percentage.  Changed premium line items to     *
     H*                contribution on summary bill.  Add positioning *
     H*                info for the TASB logo. Remove deductible, exp *
     H*                mod, and increased limits info from summary.   *
     H*                                                               *
     H* 03/03/10    C:235 PROJECT: 114WWG                             *
     H*                Heart and Lung Group 101                       *
     H*                                                               *
     H* 03/03/10 C:189 Support: 71820                                 *
     H*                                                               *
     H* 02/25/10 C:65  PROJECT: 25WRC                                 *
     H*                Fix audited contribution heading in balance.   *
     H*                                                               *
     H* 02/22/10 C:136 PROJECT: 139WMG                                *
     H*                CHG 'SERVICED BY' VERBIAGE TO 'REMIT PAYMENT   *
     H*                TO' VERBIAGE                                   *
     H*                                                               *
     H* 01/20/10 C:180 Support: 71406                                 *
     H*          C:136 Add mass billing parm for W7321C               *
     H*                                                               *
     H* 01/14/10 C:191 PROJECT: 25WRC                                 *
     H*                Billing Algorithm Retrofit                     *
     H*                                                               *
     H* 01/13/10 C:136 PROJECT: WC1838                                *
     H*                ADD EXCEPTION REPORT FOR MASS AUDIT TO LIST    *
     H*                LARGE DEDUCTIBLE POLICIES SKIPPED DURING THE   *
     H*                MASS RUN BECAUSE OF THE USER LACKING THE       *
     H*                NECESSARY AUTHORITY LEVEL.                     *
     H*                                                               *
     H* 01/12/10 C:65  PROJECT: WC1838                                *
     H*                LARGE DEDUCTIBLES - USE NEW FILE FOR LARGE     *
     H*                DEDUCTIBLE POLICIES, BASED ON BILLING SCHED CD.*
     H*                                                               *
     H* 12/29/09 C:136 PROJECT: WC1861                                *
     H*                ADD AZ RATE SPLIT - 01/01/2010                 *
     H*                                                               *
     H* 12/14/09 C:234 SUPPORT: WA114 70895                           *
     H*                Compile Only due to change in w7615o           *
     H*
     H* 12/08/09 C:234 SUPPORT: WA133 70887                           *
     H*                Print 'PER CLAIM' or 'PER OCCURRENCE' based on *
     H*                DDVERB flag not working correctly.             *
     H*                                                               *
     H* 11/23/09 C:136 PROJECT: 139WNR                                *
     H*                FL RATE SPLIT FOR 07/01/09 - CLT SPECIFIC      *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Print 'Statement #' along with the invoice #   *
     H*                                                               *
     H* 10/21/09 C:180 SUPPORT 70382 INCORRECT PRINT FORMAT FOR
     H*                CLIENT 046.
     H*
     H* 10/16/09 C:235 PROJECT: 139WMD                                *
     H*                NAME/ADDRESS ON TOP OF QUOTES & BILLINGS       *
     H*                                                               *
     H* 06/19/09 C:227 SUPPORT: WA114 68666                           *
     H*                ADD MINIMUM PREMIUM MESSAGE                    *
     H*                                                               *
     H* 05/15/09 C:33  Project: WC1769   Priss#32                     *
     H*                Do not print 6th and 7th assessments, if not   *
     H*                 applicable.                                   *
     H*                                                               *
     H* 05/15/09 C:189 PROJECT : 139WLJ                               *
     H*                WORK WITH MULTIPLE DARTS DIARY RECIPIENTS      *
     H*                                                               *
     H* 04/20/09 C:190 PROJECT WC1473                                 *
     H*                Use Payroll Extension Description              *
     H*                                                               *
     H* 03/26/09 C:65  PROJECT: 102WEV                                *
     H*                ADD A FEES PAID LINE ITEM TO CLIENT 102 BILL   *
     H*                AND CHANGE PREVIOUSLY PAID TO PREMIUM PAID.    *
     H*                                                               *
     H* 03/19/09 C:136 Support: WA114 67724                           *
     H*                Add a compatibility parm to the Prem Notice    *
     H*                call of W7321C as a result of 132WLB.          *
     H*                                                               *
     H* 03/11/09 C:61  Project: WC1630                                *
     H*                Add programming to include the deductible      *
     H*                billed category in the "Fees billed" line item *
     H*                and the deductible paid category in the        *
     H*                "Previously Paid" line item.                   *
     H*                                                               *
     H* 03/02/09 C:65  PROJECT: 81WEI                                 *
     H*                Use optional headings for terrorism charges.   *
     H*                                                               *
     H* 02/24/09 C:33  Project: 45WHC                                 *
     H*                New logo adjustments.                          *
     H*                                                               *
     H* 02/23/09 C:65  PROJECT: SUPP#67116                            *
     H*                ADD AGENT # TO THE RIGHT OF THE AGENT HEADER.  *
     H*                                                               *
     H* 12/16/08 C:136 PROJECT: WC1768                                *
     H*                DE RATE SPLIT FOR 10/01/08                     *
     H*                                                               *
     H* 12/11/08 C:65  Project: WC1769                                *
     H*                ADD CHANGES TO PRINT TWO NEW ASSESSMENTS.      *
     H*                                                               *
     H* 10/30/08 C:65  PROJECT:  WC1753                               *
     H*                Added processing to show a line item in the    *
     H*                payroll section of the bill for the amount     *
     H*                of decrease to the manual premium figure if    *
     H*                the policy jurisdiction state is NY and the    *
     H*                policy is unexpired as of 10/01/2007.          *
     H*                                                               *
     H* 09/23/08 C:136 SUPPORT: WA102 65290                           *
     H*                MAKE WC1627(NET RATE) BACWARDS COMPATIBLE FOR  *
     H*                CLIENTS THAT HAVE MODIFIED CODE AND HAVE NOT   *
     H*                IMPLEMENTED WC1627                             *
     H*                                                               *
     H* 09/02/08 C:197 PROJECT :071WJK                                *
     H*                BILLS (ORG, RVSD, AUDIT): REMOVE WORDING:      *
     H*                "NOT AN INVOICE-FOR INFORMATION ONLY"          *
     H*                                                               *
     H* 07/15/08 C:136 SUPPORT: WA071 64248                           *
     H*                THE FINAL AUDIT READY DARTS REMARK WAS BEING   *
     H*                SENT TO THE WRONG PROFILE                      *
     H*                                                               *
     H* 06/05/08 C:65  PROJECT: 46WFM                                 *
     H*                ADD OTHER FEES LINE ITEM TO LSAT OUTPUT.       *
     H*                                                               *
     H* 04/28/08 C:136 SUPPORT: WA140 63016                           *
     H*                ENSURE THAT PREMIUM NOTICE PRINTS IF FLAGGED   *
     H*                TO DO SO                                       *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 03/04/08 C:65  PROJECT: SUPP#62208                            *
     H*                ENABLE DIVISION HEADING TO PRINT FOR ALL.      *
     H*                                                               *
     H* 02/19/08 C:65  PROJECT SUPPORT                                *
     H*                CHANGE SNXTDCID COMMAND TO PASS A 'P' VALUE    *
     H*                OVER FOR CREATING PDF DOCUMENT NAMES.          *
     H*                                                               *
     H* 01/22/08  C:65  PROJECT: WC1627                               *
     H*                 PULL THE NET RATE AND NET PREMIUM FOR EACH    *
     F*                 PAYROLL CLASS FROM THE NEW NET FIELDS.        *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
     H*                                                               *
     H* 12/19/07 C:65  SUPPORT: #61566/114WMF                         *
     H*                SWAP POSITIONS OF 2ND DEVELOPMENT FACTOR AND   *
     H*                F-CLASS EXCESS LOSS FACTOR.                    *
     H*                                                               *
     H* 10/15/07 C:65  PROJECT: 114WOQ                                *
     H*                DO NOT PRINT EXPENSE CONSTANT LINE ITEMS FOR   *
     H*                GROUP 2 PA BUSINESS FOR CLIENT 114.            *
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 54504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H* 08/22/07 C:193 PROJECT: 102WEL                                *
     H*                NSF FEE ADDED TO AUDIT                         *
     H*                                                               *
     H* 08/17/07 C:65  SUPPORT: 59452 (WA090 58000)                   *
     H*                REMOVE DEDUCTIBLE FEES FROM GENERAL FEES LINE  *
     H*                ITEM.                                          *
     H*                                                               *
     H* 08/07/07 C:65  SUPPORT: 59452 (WA090 58000)                   *
     H*                MOVE THE GENERAL FEES HEADING/AMOUNT OVER FOR  *
     H*                OTHER CLIENTS IN DOBAL AND A/R SUMMARY SECTIONS*
     H*                                                               *
     H* 07/18/07 C:136 PROJECT: WC1491                                *
     H*                IMPLEMENT 'R' AND 'T' TYPES FOR DEPOSIT        *
     H*                                                               *
     H* 07/16/07 C:183 Project: 45WFO                                 *
     H*               -Remove all processing related to project 45WFN *
     H*                                                               *
     H* 07/12/07 C:193 SUPPORT: 55502 45WFN                           *
     H*                CHECK TO PROCESS NON-AUDITED POLICIES FOR A    *
     H*                NON-REVISED MASS AUDIT AND ONLY PROCESS        *
     H*                AUDITED POLICIES FOR A REVISED MASS AUDIT.     *
     H*                                                              *
     H* 06/29/07 C:136 SUPPORT: WA109 58921                           *
     H*                CORRECT HEADING FOR CA FLAT RATE INCREASE      *
     H*                                                               *
     H* 06/08/07 C:65  SUPPORT: 55650                                 *
     H*                CONDITION THE PAGE SEGMENT FOR THE BILLING     *
     H*                LOGO TO PREVENT SYSTEM MESSAGES WHEN PRINTING  *
     H*                WHEN THE VALUE OF THE LOGO NAME IS BLANK.      *
     H*                *IN85 IS SWAPPED TO THIS CHANGE INSTEAD OF     *
     H*                SHORT RATE (O-SPECS NO LONGER USE IT).         *
     H*                                                               *
     H* 05/15/07 C:136 SUPPORT: WA090 58000                           *
     H*                ADD FEES INTO BALANCE SECTION FOR ALL CLIENTS  *
     H*                EXCEPT 133, 109, 102, AND 046 WHO HAVE         *
     H*                PREVIOUSLY PLACED CLIENT SPECIFIC LOGIC        *
     H*                FOR CALCULATING VARIOUS FEE BALANCES.          *
     H*                                                               *
     H* 05/11/07  C:65  PROJECT: SUPPORT                              *
     H*                 CLEAR OUT THE DIVISION HEADING/NUMBER FIELDS  *
     H*                 FOR THE NEW FORMAT SUMMARY BILL.              *
     H*                                                               *
     H* 05/03/07  C:180 PROJECT: 114WMF
     H*                 COPIED CHANGES DATED 05/18/05, 01/11/05 &
     H*                 11/05/03 FROM C114DVP LIBRARY.
     H*
     H*                 ADDED CHANGES DATED 05/18/05 & 11/05/03 TO
     H*                 4TH & 5TH ASSMNTS CALC.
     H*
     H* 04/06/07  C:65  PROJECT: WC1514                               *
     H*                 APPLY SHORT RATE PENALTY TO DIFFERENT PREMIUM *
     H*                 LEVELS AND STORE AMOUNT IN A NEW FIELD.       *
     H*                                                               *
     H* 04/02/07 C:132 PROJECT: 45WFN                                 *
     H*                Back to Base on Audits                         *
     H*                - ADD Code from 45WBT for Prompt Pay Discount  *
     H*                  to print correctly.                          *
     H*                                                               *
     H* 03/22/07 C:132 PROJECT: 45WFN                                 *
     H*                Back to Base on Audits                         *
     H*                - Remove 45WDI verbiage                        *
     H*                - Implement 45WCE, 45WCB, 45WDB, 45WHB Mods    *
     H*                                                               *
     H* 03/14/07 C:136 SUPPORT: WA132 57034                           *
     H*                ADD PARM TO CALL TO W7615C TO PREVENT          *
     H*                PARM MISMATCH. THE NEW PARM ALWAYS             *
     H*                GETS A 'Y' SINCE AUDIT BILL ALWAYS PRINTS      *
     H*                HARDCOPY.                                      *
     H*                                                               *
     H* 02/07/07 C:136 PROJECT: 114WMF                                *
     H*                ADD EXCEPTIONS AND FIELDS USED IN ESTIMATED    *
     H*                BILL THAT AREN'T USED IN THE AUDIT BILL SO     *
     H*                THAT THIS PROGRAM WILL COMPILE.                *
     H*                                                               *
     H* 01/22/07 C:136 SUPPORT: WA139 56766                           *
     H*                PREVENT DUPLICATE PAYROLL NUMBERS ON           *
     H*                SUMMARIZED PAGE OF AGENT BILL.                 *
     H*                                                               *
     H* 01/18/07  C:65  PROJECT: WC1167/SUPP#55637                    *
     H*                 PASS HOLD STATUS ALONG TO W7615C FOR NEW      *
     H*                 BILLING STYLE.                                *
     H*                                                               *
     H* 12/19/06 C:61  SUPPORT: WA099 56307 46WFP                     *
     H*                Use variable positioning for Logo.             *
     H*                                                               *
     H* 11/27/06 C:136 PROJECT: 71WHL                                 *
     H*                WRITE 'FINAL AUDIT READY' DIARY REMARK         *
     H*                IF QQFADC AND QQFASC ARE NOT BLANK.            *
     H*                                                               *
     H* 11/15/06 C:132 PROJECT: 71WHM                                 *
     H*                Mass Audit: Same Options as single Audit.      *
     H*                                                               *
     H* 09/20/06 C:136 PROJECT: 71WHE (BACK TO BASE)                  *
     H*                WRITE A PREMIUM NOTICE IF FLAGGED TO DO SO     *
     H*                IN ADMIN/DARTS CONTROL.                        *
     H*                                                               *
     H* 08/07/06 C:136 SUPPORT: WA136 54053                           *
     H*                CORRECT STATEMENT THAT POPULATES THE COMPANY   *
     H*                FACTOR FOR PROJECT 136WBH TO NOT FILL THE      *
     H*                HDG ARRAY FROM POSITION 70 FORWARDS            *
     H*                                                               *
     H*  08/03/06 C:136 SUPPORT: WA102 53919                          *
     H*                 MAKE CODE ENTERED FOR 51626 BACKWARDS         *
     H*                 COMPATIBLE FOR CLIENTS THAT DON'T POPULATE    *
     H*                 THE PREMIUM PERIODS FILE.                     *
     H*                                                               *
     H* 07/06/06 C:168 PROJECT: 132WEW                                *
     H*                ADD A LINE ITEM FOR FACULTATIVE REINSURANCE    *
     H*                CHARGE.                                        *
     H*                                                               *
     H* 06/27/06 C:65  PROJECT: 109WDQ                                *
     H*                ADD A LINE ITEM FOR STOP GAP FEES TO BALANCE.  *
     H*                THIS LINE ITEM WILL USE THE GENERIC FEE        *
     H*                LINE THAT WAS ADDED UNDER PROJECT 133WCI.      *
     H*                THE GENERIC FEE LINE WAS ADDED TO THE          *
     H*                SUMMARY AUDIT BILL AS WELL TO HANDLE THIS.     *
     H*                                                               *
     H* 06/19/06 C:61  PROJECT: 133WCV                                *
     H*          C:136 Add line for "Harbor Exposure Fee" after       *
     H*                "Normal Premium" and before "Expense Constant".*
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H* 05/03/06 C:132 PROJECT: 46WFL                                 *
     H*                Add back to base the MOD code for client 46.   *
     H*                                                               *
     H* 04/26/06 C:65  PROJECT: SUPP# 52463/71WGH                     *
     H*                REMOVE MODIFIED CODE FOR THE BALANCE BOX.      *
     H*                                                               *
     H*  04/21/06 C:65  PROJECT:71WGH(REIMPLEMENT 71FDN,71WFT)        *
     H*                 MOVE THE ENTRIES FROM WDF1PX TO WDF1P.        *
     H*                                                               *
     H* 04/17/06 C:168  PROJECT: 46WFM (COPY OF CLT046-46WFD)         *
     H*                 Change the total section to show 'Billed'     *
     H*                 'Paid' 'Balance Due' for each line item.      *
     H*                 Add NSF/Legal Fees.                           *
     H*                                                               *
     H*  04/07/06 C:168 PROJECT: 140WAO                               *
     H*                 CHANGED THE DISPLAY DISCOUNT TO PRINT THE     *
     H*                 ACTUAL ONE USED DURING CALCULATION            *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     H*                                                               *
     H*  03/03/06 C:136 SUPPORT: WA136 51706                          *
     H*                 DIVISION BY ZERO CHECK NEED FOR 51626         *
     H*                                                               *
     H*  02/27/06 C:136 SUPPORT:WA136 51626                           *
     H*                 ADD CODE TO POPULATE PREMIUM PERIODS FILE SO  *
     H*                 THAT NET RATE CALCULATION WILL DISPLAY        *
     H*                 PROPERLY FOR EACH PERIOD.                     *
     H*                                                               *
     H*  02/03/06 C:65 PROJECT: SUPP#51072/WC899                      *
     H*                ADD IN 3RD, 4TH, AND 5TH ASSESSMENTS TO THE    *
     H*                PRIOR BILLED TOTAL AS WELL.                    *
     H*                                                               *
     H*  01/30/06 C:65 PROJECT: WC1327/71WGH                          *
     H*                CHANGE PARM # FOR CONTRIBUTION NOTICES.        *
     H*                                                               *
     H*  06/20/05 C:65 PROJECT: WC1327/71WGH                          *
     H*                ALLOW PROGRAM TO BE CALLED IN SINGLE DIVISION  *
     H*                MODE.  THIS MODE WILL ALLOW A BILL TO BE RUN   *
     H*                ON A SINGLE DIVISION ON A POLICY.              *
     H*                FOR CLIENT 71 ONLY.                            *
     H*                                                               *
     H*  01/12/06 C:65 Project: 71WGH                                 *
     H*                Generate Contribution Notices with audit bills.*
     H*                FOR CLIENT 71 ONLY.                            *
     H*                                                               *
     H* 01/10/06 C:65  PROJECT: WC1167/SUPP#50479                     *
     H*                CHANGE NET RATE CALC TO USE AUDIT FIELDS.      *
     H*                                                               *
     H* 01/04/06 C:65  PROJECT: SUPPORT#50363                         *
     H*                MOVE THE ENTIRE NAME OVER FOR MAKE CHECKS PAY. *
     H*                                                               *
     H* 12/18/05 C:136 PROJECT: 136WBH                                *
     H*                CLIENT SPECIFIC CHANGES TO CREDIT/DEBIT FACTOR *
     H*                                                               *
     H* 12/13/05 C:136 PROJECT: 71WGH                                 *
     H*                CLIENT SPECIFIC MODS TO NEW FORMAT BILL.       *
     H*                                                               *
     H* 11/23/05 C:65  PROJECT WC1167                                 *
     H*                PRINT AMOUNTS FOR TERRORISM AND DOMESTIC       *
     H*                TERRORISM WITHIN PREMIUM SECTION FOR NEW BILLS.*
     H*                                                               *
     H* 11/22/05  C:65  PROJECT 102CG  ORIGINAL C:65 ON 09/23/02      *
     H*                 CHANGE BALANCE SECTION OF AUDIT BILL TO       *
     H*                 INCLUDE 4 LINES ONLY:  AUDIT TOTAL, LATE FEES *
     H*                 DUE, PREVIOUSLY PAID, AND TOTAL DUE.          *
     H*                 THIS IS DONE FOR CLIENT 102 ONLY.             *
     H*                                                               *
     H* 11/21/05 C:65  PROJECT SUPP#49545                             *
     H*                REMOVE IN98 FOR OVERFLOW ON PRINTER FILE AS    *
     H*                IT IS NOT ACTUALLY USED, BUT IS FOR 5TH ASSMNT.*
     H*                                                               *
     H* 08/04/05 C:65  PROJECT SUPP#47527                             *
     H*                ADJUST TO PROPERLY PRINT THE AGENT PHONE #.    *
     H*                                                               *
     H* 08/01/05 C:136 PROJECT: 133WCI (ADD FEES)                     *
     H*                ADD CLIENT SPECIFIC CODE FOR FEES; HOWEVER,    *
     H*                THE FIELD IN THE OUPUT IS GENERIC AND CAN BE   *
     H*                USED FOR OTHER THINGS.                         *
     H*                                                               *
     H* 07/21/05 C:65  PROJECT: SUPP#48192                            *
     H*                FLAG PDF DOCUMENTS AS AN 'O' IMAGE TYPE.       *
     H*                                                               *
     H*  06/24/05 C:33 Project: 109WDZ                                *
     H*                {109BD}                                        *
     H*                Pull the membership fees from employer detail. *
     H*                {109DB}                                        *
     H*                CA flat rate increase. 0.2% for all 2002       *
     H*                 policies(anniv or eff dt) and a 4.9% increase *
     H*                 for the unexpired portion as of 1/1/2003.     *
     H*                 Print a payroll line item for this increase.  *
     H*                                                               *
     H* 05/18/05 C:65 PROJECT: 114WGH                                 *
     H*               PULL ASSESSMENTS AS OF THE POL EFF DATE FOR NJ. *
     H*          C:180 COPIED FROM C114DVP AS PART OF 114WMF - 05/03/07
     H*
     H* 04/07/05 C:156 PROJECT: 126WDK                                *
     H*                PROJECT: 126WCD                                *
     H*                MULTIPLE DEDUCTIBLE DISCOUNTS PER YEAR.        *
     H*                                                               *
     H* 04/13/05 C:136 SUPPORT: WA093 45977                           *
     H*                ADD CODE TO PROCESS 'TRUE VOIDS'               *
     H*                                                               *
     H* 03/07/05 C:65  PROJECT: WC1202                                *
     H*                PRINT EXTENDED EMPLR NAMES ON THE NEW FORMAT   *
     H*                BILLING OUTPUT.                                *
     H*                                                               *
     H* 02/22/05 C:65  PROJECT: WC1167                                *
     H*                ALLOW FLAT CANCELLATIONS TO PRINT A BILL TO    *
     H*                CREATE A PAPER TRAIL FOR THE AUDIT.            *
     H*                                                               *
     H* 02/15/05 C:65  PROJECT: WC1167                                *
     H*                IF FLAGGED THEN PROCESS A NEW FORMAT BILL WITH *
     H*                PRINTER FILE OUTPUT.  THE NEW BILL FORMAT WILL *
     H*                RESOLVE TO DARTS USING A PDF CONVERSION PGM.   *
     H*                THREE NEW PARMS ARE ADDED AS WELL FOR THE FREE *
     H*                FORM COMMENTS.                                 *
     H*                                                               *
     H* 01/11/05 C:65 PROJECT: 114WGH                                 *
     H*               ADD A 6TH SLOT TO EMPLR NAME/ADDRESS ARRAY FOR  *
     H*               BILLING CONTACT NAME, ON THE 1ST PAGE ONLY.     *
     H*          C:180 COPIED FROM C114DVP AS PART OF 114WMF - 05/03/07
     H*                                                               *
     H* 01/07/05 C:136 PROJECT: WC1250                                *
     H*                ADD DOMESTIC TERRORISM CHARGE AS A PREMIUM     *
     H*                LINE ITEM.                                     *
     H*                                                               *
     H* 10/11/04  C:65 PROJECT: WC1064                                *
     H*                RECOGNIZE TRANSFER CODES FOR INSTALLMENT FEES. *
     H*                                                               *
     H* 09/24/04 C:53  PROJECT 132WBD                                 *
     H*                ADD AGENT HEADER VARIABLE FIELD                *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                ADD VOLUNTARY LABOR CHARGE.                    *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227                                *
     H*                PRINT 4TH AND 5TH ASSESSMENTS WHEN APPLICABLE. *
     H*                                                               *
     H* 05/24/04 C:65  PROJECT: SY100                                 *
     H*                SET THE SHELL DOCUMENT FOLDER APPROPRIATELY IF *
     H*                THE BILL IS RESOLVED ON A DATACENTER BOX.      *
     H*                                                               *
     H* 04/26/04 C:136 PROJECT: WC1169                                *
     H*                PROCESS OCCUPATIONAL DISEASE PAYROLL CLASS     *
     H*                                                               *
     H* 01/09/04 C:136 PROJECT: WC1115                                *
     H*                DISPLAY DIRECT BILL INFORMATION IF ACTIVE      *
     H*                VENDOR HISTORY EXISTS AND IF ACCOUNT IS        *
     H*                AGENCY BILLED                                  *
     H*                                                               *
     H* 11/19/03 C:65  PROJECT: SUPPORT                               *
     H*                ENSURE THAT ALL THE ADDRESS CHECKS ON 3RD      *
     H*                ADDRESS FILE VERIFY AN ADDRESS EXISTS.         *
     H*                                                               *
     H* 11/05/03 C:65 PROJECT 114WDN/SUPP#35531                       *
     H*               IF THE ASSESSMENT IS TO BE SPLIT, THEN PRINT    *
     H*               THE LINE ITEM EVEN IF THE FIRST PERIOD DOES NOT *
     H*               HAVE A PERCENTAGE.                              *
     H*          C:180 COPIED FROM C114DVP AS PART OF 114WMF - 05/03/07
     H*
     H* 10/21/03 C:65  PROJECT: WC1017                                *
     H*                ADD TABLE ENTRY FOR RENEWAL QUOTE PROCESSING.  *
     H*                                                               *
     H* 10/10/03 C:65  PROJECT: SUPP/WC1123/WC1149                    *
     H*                ENSURE RATING DATE ARRAYS ARE NOT FILLED WITH  *
     H*                RATE SPLIT DATES WHEN THE SPLIT IS NOT VALID.  *
     H*                                                               *
     H* 10/07/03 C:65  PROJECT: WC1149                                *
     H*                PRINT PAYROLL SPLIT DATES FOR THE FL 10/1/3    *
     H*                RATE CHANGE.                                   *
     H*                                                               *
     H* 09/15/03  C:65  PROJECT: SUPP#38837                           *
     H*                 PREVENT STANDARD LIMITS FROM PRINTING WHEN A  *
     H*                 HAS MULTIPLE PAYROLL PAGES AND INCR LIMITS.   *
     H*                 COPIED IN BY C:136                            *
     H*                                                               *
     H* 05/21/03  C:65  PROJECT: SUPP#37208                           *
     H*                 CORRECT 3RD BILLING ADDRESS ZIP CODE PRINTING *
     H*                                                               *
     H*  05/08/03 C:132 Project: WC1117                               *
     H*                 REMOVED LOCK BOX address check in group       *
     H*                 auxiliary file, as requested by client 129    *
     H*                                                               *
     H* 04/30/03 C:65  PROJECT: WC1123                                *
     H*                PRINT PAYROLL SPLIT DATES FOR THE FL 4/1/3     *
     H*                RATE CHANGE.                                   *
     H*                                                               *
     H* 04/21/03 C:65  PROJECT: WC1115                                *
     H*                IF VENDOR PROCESSING AND AGENCY BILLED,        *
     H*                DISPLAY DIRECT BILLED INFORMATION.             *
     H*                                                               *
     H*  02/13/03 C:132 Project: WC1117                               *
     H*                 Added LOCK BOX address check in group         *
     H*                 auxiliary file. If present in WMF2P, use the  *
     H*                 Lock box address instead of accounting office *
     H*                 address.                                      *
     H*                                                               *
     H* 02/06/03  C:65  PROJECT: SUPP#35726                           *
     H*                 PRINT THE STOCK DISCOUNT PERCENTAGE ON BILLS. *
     H*                 COPIED IN BY C:136                            *
     H*                                                               *
     H* 01/30/03  C:65  PROJECT: SUPPORT#35479                        *
     H*                 ADD 3RD ASSMNT INDICATOR FOR TOTAL DISLAY.    *
     H*                                                               *
     H* 01/29/03 C:65  PROJECT: WC744                                 *
     H*                ADD ARAP PREMIUM CHARGE TO THE PREMIUM CALC.   *
     H*                                                               *
     H* 01/17/03 C:65  PROJECT: WC1120                                *
     H*                ADD TERRORISM RISK ACT TO ANNUAL PREMIUM.      *
     H*                THE AMOUNT IS DETERMINED OFF PAYROLL AND       *
     H*                IS ADDED TO ANNUAL PREMIUM, WHETHER OR NOT     *
     H*                MINIMUM PREMIUM IS INVOKED.                    *
     H*                                                               *
     H* 01/13/03 C:65  PROJECT: WC1104                                *
     H*                ALLOW THE DEVIATIONS TO BE APPLIED TO MANUAL.  *
     H*                                                               *
     H* 11/19/02 C:65 PROJECT WC1099                                  *
     H*               INCREASE SIZE OF THE ADDITIONAL LOAD FIELD.     *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT WC1099                                  *
     H*               IF A PAYROLL CLASS CODE HAS AN EXTENSION WITH   *
     H*               A STAT CODE AND AN ADDITIONAL LOAD AMOUNT THEN  *
     H*               PRINT THE SUBHEADING FOR TERRITORY DIFF.        *
     H*                                                               *
     H* 11/07/02 C:65 PROJECT WC1055                                  *
     H*               REPLACE THE EXPENSE CONSTANT CHECK ON STATE     *
     H*               REPORTING DIVISION WITH A CHECK ON A VALUE.     *
     H*                                                               *
     H* 09/20/02 C:65 PROJECT: CM045/#33798                           *
     H*               BILL WAIVER SURCHARGE ON TOP OF MINIMUM PREM.   *
     H*               COPIED IN BY C:136                              *
     H*                                                               *
     H*  03/04/02  C:65  PROJECT: WC1064                              *
     H*                  PRINT A LINE ITEM FOR INSTALLMENT FEES.      *
     H*                                                               *
     H* 02/07/02 C:65 PROJECT WC1067                                  *
     H*               BREAK OUT EXPENSE CONSTANT ON A MIN PREM POLICY *
     H*               IN A/R IF INDICATED.                            *
     H*                                                               *
     H* 01/16/02  C:65  PROJECT: SUPPORT#29562                        *
     H*                 WHEN ACCUMULATING PREVIOUSLY BILLED FOR       *
     H*                 ACCOUNT CURRENT, INCLUDE AUDIT TRANSACTIONS.  *
     H*                                                               *
     H* 11/21/01  C:90  PROJECT: SEQ#29878                            *
     H*                 Reduce the size of the classification desc.   *
     H*                 to accomodate the expansion of other fields.  *
     H*                                                               *
     H* 11/20/01  C:65  PROJECT WC1058                                *
     H*                 PULL OPTIONAL HEADING FOR SAFETY FEE.         *
     H*                                                               *
     H* 11/05/01  C:90  PROJECT: SEQ#:29629                           *
     H*                 Correct changes made under WC1052 to display  *
     H*                 premium as a rounded number.                  *
     H*                                                               *
     H* 10/09/01  C:65  PROJECT: SUPPORT#29190                        *
     H*                 USE EMPLR DETAIL FEES, THE FIELD SHOULD BE    *
     H*                 ACCURATE FOR ALL MODES OF BILLS NOW.          *
     H*                                                               *
     H* 10/04/01  C:65  PROJECT WC1043                                *
     H*                 APPLY CONTRACTORS CREDIT IMMEDIATELY AFTER    *
     H*                 EXPERIENCE MODIFIER, INSTEAD OF JUST BEFORE   *
     H*                 STANDARD PREMIUM.                             *
     H*                                                               *
     H* 10/01/01  C:65  PROJECT WC1045                                *
     H*                 PRINT THE LOSS CONSTANT IF NEEDED.            *
     H*                                                               *
     H* 09/24/01  C:65  PROJECT WC1041                                *
     H*                 EXPAND INCR LIMITS PERCENT FROM 3.2 TO 5.3    *
     H*                                                               *
     H* 09/24/01  C:65  PROJECT WC1035                                *
     H*                 EXPAND PAYROLL RATE FIELD BY 2 DIGITS.        *
     H*                                                               *
     H* 08/30/01 C:90  PROJECT: WC1052                                *
     H*                Expand premium fields from 9.2 to 11.2         *
     H*                                                               *
     H* 08/21/01 C:65 PROJECT: SUPP#28643                             *
     H*               PULL ASSESSMENTS PER THE ANNIV DATE IF APPLIC.  *
     H*                                                               *
     H* 04/27/01 C:61  SUPPORT WA112 27242                            *
     H*                Update value of Audit received date AURCVD     *
     H*                if AURCVD has not been entered when Audit      *
     H*                Print date AUPRDT is updated.                  *
     H*                                                               *
     H* 04/26/01  C:65  SUPPORT: #27207                               *
     H*                 PRINT DEDUC PLAN INFO IF THERE IS A CODE.     *
     H*                                                               *
     H* 02/21/01 C:65 PROJECT: WC1026                                 *
     H*               PRINT MERIT RATING FOR OUTPUT IF APPLICABLE.    *
     H*                                                               *
     H* 01/02/01 C:65 PROJECT: WC1024                                 *
     H*               PRINT 3RD ASSESSMENTS WHEN APPLICABLE.          *
     H*                                                               *
     H* 11/01/00  C:65  PROJECT: SUPPORT                              *
     H*                 RESET THE STATE AFTER THE EXTENSION DESC.     *
     H*                                                               *
     H* 10/13/00  C:65   PROJECT: WC899                               *
     H*                 CHANGE PREVIOUSLY PAID INTO PREVIOUSLY BILLED *
     H*                 FOR AGENCY BILLED EMPLOYERS.                 *
     H*                  CHANGE THE DESCRIPTION ON BILL TO READ DIRECT*
     H*                  OR AGENCY BILLING DEPENDING ON PLAN TYPE.    *
     H*                                                               *
     H*  09/29/00 C:65 PROJECT: SUPPORT#24334                         *
     H*                CLEAR THE DARTS COMMENT KEY BEFORE EACH EMPLR  *
     H*                                                               *
     H*  08/25/00 C:65 PROJECT: SUPPORT#24113                         *
     H*                MOVE THE ASSESSMENT PAID CODE OUTSIDE THE      *
     H*                ASSESSMENT PERCENTAGE CHECK.                   *
     H*
     H*  04/26/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H*  04/25/00 C:65 PROJECT: SUPPORT#22676                         *
     H*                DO NOT PRINT SHORT RATE LINE IF POLICY INVOKES *
     H*                MINIMUM PREMIUM.                               *
     H*                                                               *
     H*  04/19/00 C:65 PROJECT: SUPPORT                               *
     H*                DO NOT SUBRACT 2/29 AS A LEAP DAY IF IT IS THE *
     H*                POLICY START DATE.                             *
     H*                                                               *
     H* 04/05/00  C:65  PROJECT: WC936                                *
     H*                 PRINT THE PAGE TOTAL FOR THE SUMMARY SHEET.   *
     H*                 CORRECT THE HEADINGS FOR SUMMARY ASSESSMENTS. *
     H*                                                               *
     H* 03/24/00  C:65  PROJECT: WC936                                *
     H*                 ZERO OUT THE PAGE COUNT FOR EACH DIVISION.    *
     H*                                                               *
     H* 03/14/00  C:65  PROJECT: SUPPORT#22196                        *
     H*                 DO NOT USE THE EMPLR DETAIL FIELDS FOR FEES   *
     H*                 IN CASE THE PROGRAM IS DOING A CALC & PRT.    *
     H*                                                               *
     H* 02/29/00  C:118 PROJECT: WC577                                *
     H*                 LOAD Q1PGM INTO CMIPGM                        *
     H*                                                               *
     H* 01/11/00  C:65  PROJECT: SUPPORT #21333                       *
     H*                 CONDITION PERCENTAGE SIGNS ON ASSESSMENTS.    *
     H*                                                               *
     H* 12/03/99  C:65  PROJECT: SUPPORT                              *
     H*                 CHANGE DARTS COMMENT TO READ WITH PROPER DATE *
     H*                 AND FYR.                                      *
     H*                                                               *
     H*  11/15/99 C:65 PROJECT: WC936                                 *
     H*                PROCESS ALL DIVISIONS ON A SINGLE POLICY RUN.  *
     H*                IF A POLICY IS SUMMARY A/R, THEN DO NO DISPLAY *
     H*                BALANCE INFORMATION OTHER THAN THE BILLED      *
     H*                AMOUNTS ON THE DIVISION.  A SUMMARY INSTALLMENT*
     H*                SHEET WILL BE PRODUCED TO LIST THE BALANCE AND *
     H*                INSTALLMENT INFORMATION FOR THE SUMMARIZED     *
     H*                DIVISIONS.                                     *
     H*                                                               *
     H* 10/25/99  C:65  PROJECT: WC908                                *
     H*                 PULL STANDARD INCR LIMITS FROM STATISTICAL    *
     H*                                                               *
     H* 10/20/99 C:65 PROJECT: WC953                                  *
     H*               SHOW BILLED AMOUNTS FOR THE FEES AND COLLECT    *
     H*               THE PAYMENTS INTO PREVIOUSLY PAID.              *
     H*                                                               *
     H* 10/19/99  C:65  PROJECT WC951                                 *
     H*                 MOVE BALANCE INFO OVER FOR ASSESSMENT PERCNT. *
     H*                                                               *
     H* 10/07/99 C:65 PROJECT: WC937                                  *
     H*                DISPLAY THE COMBINED PERCENTAGES FOR BOTH 'F'  *
     H*                AND NON-'F' CLASSES FOR ASSESSMENTS.           *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 09/29/99  C:65  PROJECT CM045                                 *
     H*                 CHANGE WAIVER PROCESS TO CHECK PREM FLAG AT   *
     H*                 THE EMPLOYER DETAIL LEVEL.                    *
     H*                                                               *
     H* 09/16/99 C:65  PROJECT: WC851                                 *
     H*                PULL THE SHORT-RATE/PRORATE FLAG FROM WDELP.   *
     H*                                                               *
     H* 08/24/99  C:90  PROJECT: WC902                                *
     H*                 PRINT 'PER CLAIM' OR 'PER OCCURRENCE' BASED   *
     H*                 ON DDVERB FLAG.                               *
     H*                                                               *
     H* 08/12/99  C:65  PROJECT WC935                                 *
     H*                 ALLOW FOR 1ST EXP MOD TO BE ZERO, IN ORDER    *
     H*                 TO SKIP IT.                                   *
     H*                                                               *
     H* 08/10/99  C:65  PROJECT: SUPPORT #19169                       *
     H*                 CHANGE SHORT RATE CALC TO USE FDAYS.          *
     H*                                                               *
     H* 07/22/99  C:65 PROJECT:  WC577                                *
     H*                DISPLAY AMOUNT OF SCHEDULE DB/CR RESULT ON     *
     H*                THE LINE ITEM.                                 *
     H*                                                               *
     H* 07/16/99  C:65  SUPPORT: #18864                               *
     H*                 USE A LARGER FIELD FOR PRINTING A3 ZIP CODE.  *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*                                                               *
     H* 06/25/99  C:65  SUPPORT: 18465                                *
     H*                CHANGE THE LOGICAL AND KEY FOR INCR LIMITS.    *
     H*                                                               *
     H* 06/01/99  C:65  SUPPORT: CM045                                *
     H*                ADD WAIVER SURCHARGE TO PREMIUM INFORMATION    *
     H*                AND ALLOW ADDL LOAD TO APPLY AT J,E,S,D.       *
     H*                                                               *
     H* 04/13/99  C:65  SUPPORT: 17670                                *
     H*                 ONLY CALC IN CALC + PRINT MODE.               *
     H*                                                               *
     H* 03/19/99  C:65  SUPPORT: 17218                                *
     H*                 ROUND THE FEES WHEN CALCULATING THEM.         *
     H*                                                               *
     H* 03/19/99  C:65  SUPPORT: 17109                                *
     H*                 USE THE OUT OF COUNTRY ADDRESS IF THERE.      *
     H*                                                               *
     H* 03/11/99  C:65  SUPPORT: WC705                                *
     H*                 CHANGE THE PRINTING OF TAX % TO RELECT FIELD. *
     H*                                                               *
     H*  03/11/99  C:65  PROJECT: SUPPORT #17040                      *
     H*                  CHECK THE FLAGS FOR MEMBERSHIP FEE AND       *
     H*                  SAFETY FEE BEFORE ADJUSTING.                 *
     H*                                                               *
     H*  03/04/99  C:87  PROJECT: WC885                               *
     H*                  Check admin/darts file (WMEXP) and set *on   *
     H*                  indicators 83 and 84 based on EXCMSG and     *
     H*                  EXDMSG respectively.                         *
     H*                                                               *
     H*  03/02/99  C:65  PROJECT: SUPPORT                             *
     H*                  REMOVE PROJECT MOD WC844.                    *
     H*                                                               *
     H* 02/09/99 C:65 SUPPORT 16383                                   *
     H*               ADD INDICATOR CHECK AFTER ALL BALANCE ITEMS FOR *
     H*               CREDIT MESSAGE.                                 *
     H*                                                               *
     H*  01/06/99  C:65  PROJECT: WC705                               *
     H*                  ALLOW SAFTEY FEE MINIMUM WITH NO PERCENTAGE. *
     H*                                                               *
     H* 12/10/98 C:61 SUPPORT 14595                                   *
     H*               Only process active (WMA3P/A3STAT = 'A')        *
     H*               Employer 3rd Address records.                   *
     H*                                                               *
     H* 11/18/98  C:65  PROJECT: WC705                                *
     H*                 DEFAULT POLICY EXP DATE TO ONE YEAR IN FUTURE *
     H*                 IF THERE IS NO EMPLR HISTORY RECORD.          *
     H*                                                               *
     H* 11/12/98 C:65 PROJECT: SUPPORT                                *
     H*               DISPLAY STANDARD LIABILITY LIMITS ON 1ST PAGE.  *
     H*                                                               *
     H* 11/02/98 C:65 PROJECT: SUPPORT                                *
     H*               LENGTHEN THE INCR LIMITS DESCRIPTION.           *
     H*                                                               *
     H* 10/28/98 C:65 PROJECT: SUPPORT                                *
     H*               ROUND MEMBERSHIP FEES WHEN PRORATED.            *
     H*                                                               *
     H* 10/15/98 C:65 PROJECT: SUPPORT                                *
     H*               PRINT STANDARD INCR LIMIT DESCRIPTION IF THEY   *
     H*               HAVE NO CODE.                                   *
     H*                                                               *
     H* 10/09/98 C:65 PROJECT: SUPPORT                                *
     H*               CHECK IF EMPLOYER A3 ADDRESS IS BLANK.          *
     H*               ALSO CHANGED FIELD FOR LEAP YEAR CHECK.         *
     H*                                                               *
     H* 10/02/98  C:65  PROJECT: SUPPORT WA101/14126                  *
     H*                 USE AUDITED ANNUAL PREMIUM FOR DARTS COMMENT. *
     H*                                                               *
     H* 09/11/98  C:65  PROJECT: WC705                                *
     H*                 MOVE WINDOW BOX UP TWO LINES (CUT EMN BY 3C)  *
     H*                                                               *
     H* 09/08/98  C:65  PROJECT: WC705                                *
     H*                 ADD CARRIER POLICY NUMBER TO THE BILL.        *
     H*                                                               *
     H*  08/25/98  C:65  PROJECT: WC705                               *
     H*                  CHECK ON CATEGORY WHEN ACCUMULATING TAX      *
     H*                  PAYMENTS.                                    *
     H*                                                               *
     H*  08/11/98  C:65  PROJECT: WC843                               *
     H*                  DISPLAY EXTENSION DESCRIPTION IF NOT BLANK.  *
     H*                                                               *
     H* 07/31/98  C:65  PROJECT 88WAJ                                 *
     H*                 ADD THE BILL EXP CNST TO ALL DIVS FLAG.       *
     H*                                                               *
     H* 07/28/98  C:65  PROJECT CM039                                 *
     H*                 USE THE ACCOUNTING PHONE # FOR SERVICED BY.   *
     H*                                                               *
     H* 07/20/98  C:65  PROJECT: WC844                                *
     H*                 CHECK FOR INDIANA ACCOUNTS EFFECTIVE DURING   *
     H*                 7/1/97 OR LATER FOR RATE ADJUSTMENT.          *
     H*                 PRINT MANUAL AND THE ADJUSTMENT AMT ON THE    *
     H*                 FIRST PAGE OF THE BILL AND PRINT ADJ MANUAL   *
     H*                 ON THE SECOND PAGE.                           *
     H*                                                               *
     H* 07/17/98  C:65  PROJECT: WC847                                *
     H*                 MOVE THE AMOUNT COLUMN FOR THE BALANCE INFO   *
     H*                 TO THE RIGHT SO WE CAN FIT IN THE ASSESSMENT  *
     H*                 PERCENTAGES TO THE RIGHT OF THEIR HEADING.    *
     H*                                                               *
     H* 06/15/98  C:65  PROJECT CM039                                 *
     H*                 CHANGE THE SERVICED BY ADDRESS TO ACCOUNTING. *
     H*                                                               *
     H* 06/15/98  C:65  PROJECT WC705                                 *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H* 06/01/98  C:65  PROJECT CM039                                 *
     H*                 CHANGE NAME FOR MAKE CHECKS PAYABLE TO.       *
     H*                                                               *
     H* 05/29/98  C:65  PROJECT WC705                                 *
     H*                 ALWAYS USE FLEXIBLE HEADING FOR DEVIATION.    *
     H*                 USE FLEXIBLE HEADING FOR STATE TAX.           *
     H*                                                               *
     H* 05/27/98  C:65  PROJECT WC705                                 *
     H*                 FIX EFFECTIVE DATE FOR PAYROLLS FOR SPLITS.   *
     H*                                                               *
     H* 05/18/98  C:65  PROJECT WC705                                 *
     H*                 USE THE NEW PREM FIELDS FOR ASSESSMENT CALCS. *
     H*                                                               *
     H*  05/05/98  C:65  PROJECT: WC705                               *
     H*                  CHANGE EMDBA TO EMFDNM.                      *
     H*                                                               *
     H*  05/04/98  C:87  PROJECT: WC630                               *
     H*                  REAUDIT FLAG IN AUDIT TRACKING(AURVFL) IS    *
     H*                  SET TO 'Y' IF MORE THAN ONE AUDIT RECORD IS  *
     H*                  FOUND IN THE A/R FILE.                       *
     H*                                                               *
     H*  04/27/98  C:65  PROJECT: WC705                               *
     H*                  CHANGE REVISED PARM FOR CALC,PRT.            *
     H*                                                               *
     H* 03/17/98  C:65  PROJECT: WC705                                *
     H*                 ALWAYS PRINT INCR LIMITS IF THEY HAVE A CODE. *
     H*                                                               *
     H* 03/04/98  C:65  PROJECT: WC705                                *
     H*                 CHANGE DATE OF BILL TO CURRENT DATE AND ADD   *
     H*                 DUE DATE DOWN BY THE BALANCE.                 *
     H*                                                               *
     H*  02/23/98 C:85 Project: WC577                                 *
     H*                CHANGED THE FIELD MOVED TO CMSMCD              *
     H*                FROM *BLANKS TO CTSMCD.                        *
     H*                                                               *
     H* 02/09/98  C:65  PROJECT: WC705                                *
     H*                 ADD OPTIONAL HEADING FOR STOCK DISCOUNT.      *
     H*                                                               *
     H* 02/04/98  C:65  PROJECT: WC705                                *
     H*                 REMOVE PAYMENT PLAN FOR AUDIT BILL.           *
     H*                                                               *
     H* 01/06/98  C:65  PROJECT: WC808                                *
     H*                 ADDED AUTO-FOLDER SWITCHING FOR MAX DOCS.     *
     H*                                                               *
     H* 12/23/97  C:65  PROJECT WC776                                 *
     H*                 PRINT FIRST AND SECOND ASSESSMENT.            *
     H*                                                               *
     H* 11/20/97  C:65  PROJECT V3                                    *
     H*                 ADDED INDICATORS FOR PRINTING OF SAFETY AND   *
     H*                 MEMBERSHIP FEES DUE WHEN ZERO.                *
     H*                 ALSO ADDED CHANGE TO PRINT CORRECT RATES FOR  *
     H*                 ANNIVERSARY RATED ACCOUNTS.                   *
     H* 11/19/97  C:65  PROJECT: V3                                   *
     H*                 CHANGED FYR TO POLICY YEAR IN THE DARTS REMARK*
     H*                                                               *
     H* 11/14/97  C:65  PROJECT: V3                                   *
     H*                 ADDED OVERFLOW TO CORRECT MASS PRINTINGS.     *
     H*                                                               *
     H* 10/27/97  C:65  PROJECT: V3                                   *
     H*                 REMOVED DIFFERENT FORMAT FOR POLICY NUMBER.   *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE                                               *
     H*                                                               *
     H* 1-27 USED FOR INDICATING SUB-TOTALS ON THE PREMIUM LINES.     *
     H* 38  AUDITS ARE ALWAYS NON-QUOTE
     H* 39  PRINT DEPOSIT INFO FOR UOTE
     H* 40  GENERAL CHAIN INDICATOR                                   *
     H* 41  CALCULATED DEPOSIT DUE IS GREATER THAN ZERO               *
     H* 42  CALCULATED DEPOSIT DUE IS LESS THAN ZERO                  *
     H* 43  CALCULATED DEPOSIT DUE IS LESS THAN ZERO AND SELF-        *
     H*     REPORTING TYPE, DEDUCT AS CREDIT                          *
     H* 44  POLICY INVOKED ESTIMATED MINIMUM PREMIUM                  *
     H* 45  STATE TAX IS ZERO.                                        *
     H* 48  ESTIMATED INCREMENT LIMIT EXISTS                          *
     H* 49  2ND ESTIMATED INCREMENT LIMIT EXISTS                      *
     H* 50  GENERAL CHAIN INDICATOR                                   *
     H* 51  PRINT BOTTOM RIGHT HAND CORNER OF BILL ON LAST PAGE       *
     H* 52  DO NOT PRINT MEMBERSHIP FEES DUE IF ZERO.                 *
     H* 53  DO NOT PRINT SAFETY FEES DUE IF ZERO.                     *
     H* 54  MORE THAN ONE PAGE OF PAYROLL RECORDS EXIST FOR EMPLOYER  *
     H* 55  DO NOT PRINT FIRST ASSESSMENT IF ZERO.                    *
     H* 56  DO NOT PRINT SECOND ASSESSMENT IF ZERO.                   *
     H* 58  ESTIMATED DISCOUNT #1 HAS A VALUE                         *
     H* 59  PRORATA PREMIUM IS TO BE USED                             *
     H* 60  GENERAL CHAIN INDICATOR                                   *
     H* 61  USED TO DETERMINE IF FIRST TIME THRU TO EXECUTE SR1ST     *
     H* 62  ADDITIONAL LOAD VALUE IS USED                             *
     H* 63  REVISED FINAL AUDIT BILLING                               *
     H* 65  FUND ADDRESS EQUAL TO BLANKS                              *
     H* 85  BILLING LOGO PRESENT FOR NEW FORMAT BILL                  *
     H* 87  ANNIVERSARY RATING DATES ARE TO BE USED                   *
     H* 88  SET IF ANNIVERSARY RATING DATE IS ZERO                    *
     H* 89  USE SPLIT EXPERIENCE MODIFIER AND DESCRIPTION INSTEAD     *
     H*     OF SINGLE EXPERIENCE MODIFIER                             *
     H* 90  SHOW PAYROLL START AND END DATES                          *
     H* 91  FUND PHONE NUMBER IS NOT EQUAL TO ZERO SO PRINT           *
     H* 94  15.7% DISCOUNT WAS APPLIED                                *
     H* XXPRT  AT LEAST ONE LINE OF DETAIL HAS BEEN PRINTED SO GO     *
     H*        AHEAD AND PRINT TOTALS                                 *
     H*                                                               *
     H*****************************************************************
     FWABLL     UF   E           K DISK
     FWABLL1    IF   E           K DISK
     F                                     RENAME(PBL01:PBL0E)
     FWABLL2    IF   E           K DISK
     F                                     RENAME(PBL01:PBL0A)
     FWDELL     IF   E           K DISK
     F*** Begin Add *** 09/26/23 *******************************************
     F                                     infds(dbfds)
     F*** End   Add *** 09/26/23 *******************************************
     FWMUPL     IF   E           K DISK
     FWMUPL1    IF   E           K DISK
     F                                     RENAME(PUP01:PUP0L1)
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMOFL     IF   E           K DISK
     FWDF2L     IF   E           K DISK
     FWMEPLE    IF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWMEAL4    IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     f*** Begin Add *** 09/26/23 *******************************************
     F                                     IGNORE(PB201)
     f*** End   Add *** 09/26/23 *******************************************
     FWMRSL     IF   E           K DISK
     FWDRRL     IF   E           K DISK
     FWTLML1    IF   E           K DISK
     FWTDDL2    IF   E           K DISK
     FWMLDL1    IF   E           K DISK
     FWMAHL     IF   E           K DISK
     FWMAAL1    IF   E           K DISK
     FWMAGL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FWMSTL1    IF   E           K DISK
     FWDSTL     IF   E           K DISK
     F                                     RENAME(PST01:PST02)
     FWMAUL     UF   E           K DISK
     FWDEHL     IF   E           K DISK
     FWMA3L     IF   E           K DISK
     F*
     FWMPXL1    IF   E           K DISK
     FWMRPL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     FWXEPL     IF A E           K DISK
     F                                     RENAME(PEP01:XEP01)
     FWMIFL     IF   E           K DISK
     FWDVHL1    IF   E           K DISK
     FWDELL1    IF   E           K DISK
     F                                     RENAME(PEL01:PEL02)
     FWMAPL     IF   E           K DISK
     FWDPPL1    IF   E           K DISK
     FWMRTL     IF   E           K DISK    rename(prt01:prt0x)
     F                                     prefix(rx:2)
     FSMORL     IF   E           K DISK
     fwmdvl     if   e           k disk    rename(pdv01:pdv00)
      * Employer Premium Details File
     Fwdmpl     if   e           k disk
     f*** Begin Add *** 09/26/23 *******************************************
     fwxblp     if a e           k disk    rename(pbl01:pbl0p) usropn
     f                                     prefix(B0:2)
     fwxblp1    if a e           k disk    rename(pbl011:pbl0p1) usropn
     f                                     prefix(B1:2)
     fwxblp2    if a e           k disk    rename(pbl012:pbl0p2) usropn
     f                                     prefix(B2:2)
     fwsbll     if   e           k disk    rename(pbl01:sbl00) prefix(BD:2)
     fwsbl1l    if   e           k disk    rename(pbl01:pbl100)
     fxmlprel1  uf   e           k disk
     fxmlteml1  uf   e           k disk    Rename(xt100:xt101)
     fxmlteml   if   e           k disk    prefix(xi:2)
     f*** End   Add *** 09/26/23 *******************************************
      //*** Beg Add ***** 06/18/25 ****************************************
      //*** begin del ***  08/01/25  ****************************************
     f*****wxbll     uf   e           k disk    rename(pbl01:pbl00) usropn
     f*****                                     prefix(Bx:2)
      //*** end del *****  08/01/25  ****************************************
      //*** End Add ***** 06/18/25 ****************************************
      *** Beg Add ***** 04/24/24 *******************************************
     fxmlprel5  uf   e           k disk    rename(xp100:xp105)
      *** End Add ***** 04/24/24 *******************************************
     f*** Begin Add *** 03/12/24 *******************************************
     FWMEPL7    IF   E           K DISK    Rename(pep01:pep07) Prefix(e7:2)
     f*** End   Add *** 03/12/24 *******************************************
     f*** Begin Add *** 03/20/24 *******************************************
     FWMEAL     IF   E           K DISK    Rename(pea01:pea00) Prefix(e0:2)
     f*** End   Add *** 03/20/24 *******************************************
      *** Beg Add *****  05/12/25  *******************************************
      * Expense Constant Definition by State file
     Fwdecl     if   e           k disk
      *** End Add *****  05/12/25  *******************************************
     FW7615O    O    E             PRINTER OFLIND(*IN99)
     F                                     USROPN
     F*
     FQSYSPRT   O    F   85        PRINTER OFLIND(*INOF)
     F                                     USROPN
     FQSYSPRT1  O    F   85        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FQSYSPRT3  O    F  132        PRINTER OFLIND(*INOB)
     F                                     USROPN
      *** Beg Add *****  08/28/25  *******************************************
      * Remark Detail File - KEY/RMCD/CDAT
     FSDCMLD    uf   e           k disk                                         rcdfmt: PCM01
      *** End Add *****  08/28/25  *******************************************
     F*
      *** Beg Chg ***** 08/14/24 *******************************************
     D*** CON             S             30    DIM(91) CTDATA PERRCD(1)
     D intDim          C                   CONST(95)
     D CON             S             30    DIM(intDim) CTDATA PERRCD(1)
      *** End Chg ***** 08/14/24 *******************************************
     D HDG             S             30    DIM(27)
     D TAM             S             11  2 DIM(27)
     D PRC             S              8    DIM(27)
     D NUM6            S              1    DIM(6)
     D DSH             S              1    DIM(27)
     D SGN             S              1    DIM(27)
     D EFD             S              8  0 DIM(27)
     D EXD             S              8  0 DIM(27)
     D RATE            S              1    DIM(7)
     D EMN             S             37    DIM(5)
     D EMN2            S             37    DIM(6)
     D COM             S             50    DIM(3)
     D TMP             S              1    DIM(30)
     D XTX             S              1    DIM(3)
     D NUM             S              1    DIM(5)
     D NUM7            S              1    DIM(7)
     D NUM8            S              1    DIM(8)
     D PCT9            S              1    DIM(9)
     D PCT             S              1    DIM(8)
     D PDAT            S              7  0 DIM(8) ASCEND                        ANNIV DATES
     D RDAT            S              7  0 DIM(8) ASCEND                        RATING DATES
     D MTH             S              3  1 DIM(12) CTDATA PERRCD(1) ASCEND      MONTHS/HARBOR EXP %
     D HP              S              4  3 DIM(12) ALT(MTH)
     D DYS             S              3  0 DIM(8)                               DAYS IN EACH PERIOD
     D DY3             S              3  0 DIM(8)                               15.7 CREDIT DAYS
     D RMK             S             80    DIM(8) CTDATA PERRCD(1)              DARTS DIARY REMARK
     D TIT             S             35    DIM(4) CTDATA PERRCD(1)              TITLE TABLE
     D DUE             S              1    DIM(9)                               ARRAY FOR MONTH
     D QCA             S             55    DIM(1) CTDATA PERRCD(1)              CMD EXEC/QCAEXEC
     D TIT2            S             35    DIM(2) CTDATA PERRCD(1)              TITLE TABLE
     D MNTH            S              9    DIM(12) CTDATA PERRCD(1)
     D GHDG            S             20    DIM(10) CTDATA PERRCD(1)
     D T71             S             40    DIM(1) CTDATA PERRCD(1)              TITLE TABLE 071
      *** Beg Chg *****  05/12/25  *******************************************
     d***** MSG             S             80    DIM(2) CTDATA PERRCD(1)
     d MSG             S             80    DIM(3) CTDATA PERRCD(1)
      *** End Chg *****  05/12/25  *******************************************
     d strMod          s              6
     d strARAP         s              6
     d strEffdt        s             10
     d volH            s             20    dim(12) ctData perRcd(1)
     d vol             s             20    dim(5)
     d vol#            s              7  0 dim(5)
     d volA            s              2  0 dim(5)
     d xtDvnd          s             11  2 inz
     d xxDvnd          s             11  2 inz
     d v               s              2  0 inz(0)
     d intCount        s              2  0 inz(0)
     D*
     D                 DS
     D  XXENPO                 1      7  0
     D  XXENYR                 1      3  0
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D  DSMM                   7      8
     D  DSDD                   9     10
     D  DSYY                  11     12
     D                 DS
     D  DSDT2                  1      8  0
     D  DSMM2                  1      2
     D  DSDD2                  3      4
     D  DSYY2                  5      8
     D                 DS
     D  CONM                   1     40
     D                                     DIM(40) ASCEND
     D  CONAME                 1     40
     D                 DS
     D  OFAZIP                 1      9  0
     D  OFMZ1                  1      5
     D  OFMZ2                  6      9
      ********* LOCK BOX ZIP CODE FROM GROUP AUX FILE
     D                 DS
     D  COPHN#                 1      7  0
     D  COPHN1                 1      3
     D  COPHN2                 4      7
     D                 DS
     D  OFAPHN                 1      7  0
     D  OFAPH1                 1      3
     D  OFAPH2                 4      7
     D                 DS
     D  COZIP                  1      9  0
     D  COMZ1                  1      5
     D  COMZ2                  6      9
     D                 DS
     D  FNAZIP                 1      9  0
     D  FNMZ1                  1      5
     D  FNMZ2                  6      9
     D                 DS
     D  QQPOL#                 1     15
     D  QPFND                  1      3
     D  QPEMP#                 4     12
     D  QPFYR                 13     15
     D                 DS
     D  XXTXPD                 1     11
     D  XXTXMD                 1      3
     D  XXTXTD                 4      6
     D  XXTXHD                 7      9
     D  XXTXCD                10     11
     D                 DS
     D  XXTXPR                 1     14
     D  XXTXM                  1      3
     D  XXCM1                  4      4
     D  XXTXT                  5      7
     D  XXCM2                  8      8
     D  XXTXH                  9     11
     D  XXPER                 12     12
     D  XXTXC                 13     14
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
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D                 DS
     D  XQDT                   1      8  0
     D  XXMTH                  1      2  0
     D  XXDAY                  3      4
     D  XXYR                   5      8
     D                 DS
     D  QQDCID                 1     12
     D  QQ1ST                  1      2
     D  QQ2ND                  6      8
     D  QQ3RD                 10     12
     D                 DS
     D  QHDG                   1    900
     D                                     DIM(30)
     D  QHDG1                  1     30
     D  QHDG2                 31     60
     D  QHDG3                 61     90
     D  QHDG4                 91    120
     D  QHDG5                121    150
     D  QHDG6                151    180
     D  QHDG7                181    210
     D  QHDG8                211    240
     D  QHDG9                241    270
     D  QHDG10               271    300
     D  QHDG11               301    330
     D  QHDG12               331    360
     D  QHDG13               361    390
     D  QHDG14               391    420
     D  QHDG15               421    450
     D  QHDG16               451    480
     D  QHDG17               481    510
     D  QHDG18               511    540
     D  QHDG19               541    570
     D  QHDG20               571    600
     D  QHDG21               601    630
     D  QHDG22               631    660
     D  QHDG23               661    690
     D  QHDG24               691    720
     D  QHDG25               721    750
     D  QHDG26               751    780
     D  QHDG27               781    810
     D  QHDG28               811    840
     D  QHDG29               841    970
     D  QHDG30               871    900
     D                 DS
     D  QPRC                   1    240
     D                                     DIM(30)
     D  QPRC1                  1      8
     D  QPRC2                  9     16
     D  QPRC3                 17     24
     D  QPRC4                 25     32
     D  QPRC5                 33     40
     D  QPRC6                 41     48
     D  QPRC7                 49     56
     D  QPRC8                 57     64
     D  QPRC9                 65     72
     D  QPRC10                73     80
     D  QPRC11                81     88
     D  QPRC12                89     96
     D  QPRC13                97    104
     D  QPRC14               105    112
     D  QPRC15               113    120
     D  QPRC16               121    128
     D  QPRC17               129    136
     D  QPRC18               137    144
     D  QPRC19               145    152
     D  QPRC20               153    160
     D  QPRC21               161    168
     D  QPRC22               169    176
     D  QPRC23               177    184
     D  QPRC24               185    192
     D  QPRC25               193    200
     D  QPRC26               201    208
     D  QPRC27               209    216
     D  QPRC28               217    224
     D  QPRC29               225    232
     D  QPRC30               233    240
     D                 DS
     D  QTAM                   1    330  2
     D                                     DIM(30)
     D  QTAM1                  1     11  2
     D  QTAM2                 12     22  2
     D  QTAM3                 23     33  2
     D  QTAM4                 34     44  2
     D  QTAM5                 45     55  2
     D  QTAM6                 56     66  2
     D  QTAM7                 67     77  2
     D  QTAM8                 78     88  2
     D  QTAM9                 89     99  2
     D  QTAM10               100    110  2
     D  QTAM11               111    121  2
     D  QTAM12               122    132  2
     D  QTAM13               133    143  2
     D  QTAM14               144    154  2
     D  QTAM15               155    165  2
     D  QTAM16               166    176  2
     D  QTAM17               177    187  2
     D  QTAM18               188    198  2
     D  QTAM19               199    209  2
     D  QTAM20               210    220  2
     D  QTAM21               221    231  2
     D  QTAM22               232    242  2
     D  QTAM23               243    253  2
     D  QTAM24               254    264  2
     D  QTAM25               265    275  2
     D  QTAM26               276    286  2
     D  QTAM27               287    297  2
     D  QTAM28               298    308  2
     D  QTAM29               309    319  2
     D  QTAM30               320    330  2
     D                 DS
     D  QSGN                   1     30
     D                                     DIM(30)
     D  QSGN1                  1      1
     D  QSGN2                  2      2
     D  QSGN3                  3      3
     D  QSGN4                  4      4
     D  QSGN5                  5      5
     D  QSGN6                  6      6
     D  QSGN7                  7      7
     D  QSGN8                  8      8
     D  QSGN9                  9      9
     D  QSGN10                10     10
     D  QSGN11                11     11
     D  QSGN12                12     12
     D  QSGN13                13     13
     D  QSGN14                14     14
     D  QSGN15                15     15
     D  QSGN16                16     16
     D  QSGN17                17     17
     D  QSGN18                18     18
     D  QSGN19                19     19
     D  QSGN20                20     20
     D  QSGN21                21     21
     D  QSGN22                22     22
     D  QSGN23                23     23
     D  QSGN24                24     24
     D  QSGN25                25     25
     D  QSGN26                26     26
     D  QSGN27                27     27
     D  QSGN28                28     28
     D  QSGN29                29     29
     D  QSGN30                30     30
     D                 DS
     D  AHD                    1    300
     D                                     DIM(15)
     D  XAHD01                 1     20
     D  XAHD02                21     40
     D  XAHD03                41     60
     D  XAHD04                61     80
     D  XAHD05                81    100
     D  XAHD06               101    120
     D  XAHD07               121    140
     D  XAHD08               141    160
     D  XAHD09               161    180
     D  XAHD10               181    200
     D  XAHD11               201    220
     D  XAHD12               221    240
     D  XAHD13               241    260
     D  XAHD14               261    280
     D  XAHD15               281    300
     D                 DS
     D  APC                    1    135  6
     D                                     DIM(15)
     D  XAPC01                 1      9  6
     D  XAPC02                10     18  6
     D  XAPC03                19     27  6
     D  XAPC04                28     36  6
     D  XAPC05                37     45  6
     D  XAPC06                46     54  6
     D  XAPC07                55     63  6
     D  XAPC08                64     72  6
     D  XAPC09                73     81  6
     D  XAPC10                82     90  6
     D  XAPC11                91     99  6
     D  XAPC12               100    108  6
     D  XAPC13               109    117  6
     D  XAPC14               118    126  6
     D  XAPC15               127    135  6
     D                 DS
     D  APS                    1     15
     D                                     DIM(15)
     D  XAPS01                 1      1
     D  XAPS02                 2      2
     D  XAPS03                 3      3
     D  XAPS04                 4      4
     D  XAPS05                 5      5
     D  XAPS06                 6      6
     D  XAPS07                 7      7
     D  XAPS08                 8      8
     D  XAPS09                 9      9
     D  XAPS10                10     10
     D  XAPS11                11     11
     D  XAPS12                12     12
     D  XAPS13                13     13
     D  XAPS14                14     14
     D  XAPS15                15     15
     D                 DS
     D  AAM                    1    165  2
     D                                     DIM(15)
     D  XAAM01                 1     11  2
     D  XAAM02                12     22  2
     D  XAAM03                23     33  2
     D  XAAM04                34     44  2
     D  XAAM05                45     55  2
     D  XAAM06                56     66  2
     D  XAAM07                67     77  2
     D  XAAM08                78     88  2
     D  XAAM09                89     99  2
     D  XAAM10               100    110  2
     D  XAAM11               111    121  2
     D  XAAM12               122    132  2
     D  XAAM13               133    143  2
     D  XAAM14               144    154  2
     D  XAAM15               155    165  2
     D                 DS
     D  AUL                    1    165
     D                                     DIM(15)
     D  XAUL01                 1     11
     D  XAUL02                12     22
     D  XAUL03                23     33
     D  XAUL04                34     44
     D  XAUL05                45     55
     D  XAUL06                56     66
     D  XAUL07                67     77
     D  XAUL08                78     88
     D  XAUL09                89     99
     D  XAUL10               100    110
     D  XAUL11               111    121
     D  XAUL12               122    132
     D  XAUL13               133    143
     D  XAUL14               144    154
     D  XAUL15               155    165
     D                 DS
     D  DUW                    1     36
     D                                     DIM(12)                               'DUE'  WORDS
     D  DUW1                   1      3
     D  DUW2                   4      6
     D  DUW3                   7      9
     D  DUW4                  10     12
     D  DUW5                  13     15
     D  DUW6                  16     18
     D  DUW7                  19     21
     D  DUW8                  22     24
     D  DUW9                  25     27
     D  DUW10                 28     30
     D  DUW11                 31     33
     D  DUW12                 34     36
     D                 DS
     D  DDU                    1    216
     D                                     DIM(12)                              DUE DATE-WORDS
     D  DDU0                   1     18
     D  DDU1                   1     18
     D  DDU2                  19     36
     D  DDU3                  37     54
     D  DDU4                  55     72
     D  DDU5                  73     90
     D  DDU6                  91    108
     D  DDU7                 109    126
     D  DDU8                 127    144
     D  DDU9                 145    162
     D  DDU10                163    180
     D  DDU11                181    198
     D  DDU12                199    216
     D                 DS
     D  AMT                    1    108  2
     D                                     DIM(12)                              PMT AMOUNT
     D  AMT1                   1      9  2
     D  AMT2                  10     18  2
     D  AMT3                  19     27  2
     D  AMT4                  28     36  2
     D  AMT5                  37     45  2
     D  AMT6                  46     54  2
     D  AMT7                  55     63  2
     D  AMT8                  64     72  2
     D  AMT9                  73     81  2
     D  AMT10                 82     90  2
     D  AMT11                 91     99  2
     D  AMT12                100    108  2
     D                 DS
     D  XXPCTA                 1     12  9
     D  XXP1                   2      3
     D  XXP2                   4     12
     D                 DS
     D  XXPCT150               1      6  3
     D  XXP1150                2      3
     D  XXP2150                4      6
     D  cmdString      s            255
     d*** Begin Add *** 09/26/23 *******************************************
     d* ProtoTypes for SPIXML
     d/copy sPrXML

     d* Scriptura ProtoTypes
     d GenScriptOut    pr                  ExtPgm('S4401C')
     d   inXml_                      61

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

     d* LDA Data Structure
     d svLDA           ds          1024    dtaara(*lda)
     d  ldEnd#                        5  0 overlay(svlda:1)
     d  ldEndA                        5    overlay(svlda:1)
     d  keyEmail                     40    overlay(svLDA:56)
     d  keySeq#                       5    overlay(svLDA:96)
     D  ldMassFile                   60    Overlay(svlda:129)
     D  ldOutputFile                 60    Overlay(svlda:191)
     d  xxDart                        1    overlay(svlda:251)
     d  xxRMCD                        5    overlay(svlda:252)
     d  x1Copy                        1    overlay(svlda:257)
     d  xxPrint                       1    overlay(svlda:258)
     d  xxPrinter                    10    overlay(svlda:259)
     d  xxTray                       50    overlay(svlda:269)
     d  xx#CopiesA                    3    overlay(svlda:319)
     d  xxEmail                       1    overlay(svlda:322)
     d  xxToEmail                   200    overlay(svlda:323)
     d  xxSubject                   150    overlay(svlda:523)
     d  xxBody                      350    overlay(svlda:673)

     d* Work Fields
     d hasBeenRead     s               n
     d hasBillingTemplate...
     d                 s               n

     d isMassReq       s               n
     d*** Begin Add *** 05/07/24 **************************************
     d isSummary       s               n
     d*** End   Add *** 05/07/24 **************************************
     d isUseSpoolOut   s               n
     d isUseXml        s               n
     d isUseScript     s               n

     d xmlBillName     s             60
     d xmlCombName     s             60
     d xmlFile         s                   Like(XPxml)
     d xmlFileBlank    s                   Like(XPxml) Inz(' ')
     d xmlFileName     s                   Like(XPxml)
     d xmlLink         s                   Like(XPlink)
     d xmlSumCombName  s             60
     d xmlSumName      s             60
      *** Beg Add ***** 04/24/24 ****************************************
     d strXPFile       s             60
     d strXPDir        s             60
     d strXPFileComb   s             60
     d strXPDirComb    s             60
     d*** Begin Add *** 05/07/24 **************************************
     d strXPFileSRD    s             60
     d strXPDirSRD     s             60
     d*** End   Add *** 05/07/24 **************************************
     d strEmailD       s           1000
      *** End Add ***** 04/24/24 ****************************************
      *** Beg Add ***** 02/04/25 **************************************
     dcomboBillXMLKey  s             60
      *** End Add ***** 02/04/25 **************************************

     d xxArSum         s              1
     d xxBillType      s             50
     d xxBillTypeChar  s             50    Inz('A')
     d xxCombine       s              1
     d xxPercent       s              8
     d xxPre           s              2    Inz('BL')

     d intWXBLP1seq    s              5  0 Inz(0)
     d intWXBLP2seq    s              5  0 Inz(0)

     d billTitle       s             35
     d idx             s              3  0
     D index           s              5  0
     d intPos          s              3  0
     d inXml           s             61
     d massFlag        s              1
     d message         s            250
     d outFnd          s              3  0
     d outputFileComb  s             60
     d outSt           s              2
     d processType     S             60    Inz('Audit Bill')
     d string          s             10
     d templateFile    s             60
     d x#RRN           s              9  0 Inz(0)
     d*** Begin Add *** 05/07/24 *******************************************
     d xsRRN           s              9  0 Inz(0)
     d*** End   Add *** 05/07/24 *******************************************
     d massReqGen      s              1    Inz('N')

     d dbfds           ds
     d  dbrrn                397    400b 0
     d*** End   Add *** 09/26/23 *******************************************
     d*** Begin Add *** 03/12/24 *******************************************
     d hasPayrolls     s              1n   Inz(*Off)
     d*** End   Add *** 03/12/24 *******************************************
     d*** Beg Add ***** 04/04/24 *******************************************
     d xxEfdt1         s              7  0
      // Effective Date
     d SrEffdt         pr                  extpgm('SREFFD')
     d ##CO#                          3
     d ##FND                          3
     d ##FYR                          3
     d ##EMP#                         9
     d ##DIV                          5
     d ##Date                         7
     d ##Type                         1
     d ##Clas                         5
     d*** End Add ***** 04/04/24 *******************************************

     d QCMDEXC         pr                  ExtPgm('QCMDEXC')
     d   cmdStr_                    255    const
     d   length_                     15  5 const
     D*                                                                *****
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRST-
     D                                     UVWXYZ')
     D LO              C                   CONST('abcdefghijklmnopqrst-
     D                                     uvwxyz')
     D OCC             C                   CONST('Per Occurrence')
     D CLM             C                   CONST('Per Claim')
     D POL#71          C                   CONST('        Member #:')
     D FAR1            C                   CONST('Policy year ')
     D FAR2            C                   CONST('ready for final -
     D                                     audit. Final audited-
     D                                      premium calculated -
     D                                     to')
     D AUDVB1          C                   CONST('Audit occurred ')
     D AUDVB2          C                   CONST('months after the eff-
     D                                     ective date. WCSTAT-
     D                                      review needed')
     D ERTITL          C                   CONST('Mass Audit Billing -
     D                                     Security Level Exc-
     D                                     eption Report')
     D DIST#           C                   CONST('District#:')
     D SERVBY          C                   CONST('     Serviced by:')
     D REMITO          C                   CONST('Remit payment to:')
     D*                                                                *****
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D @2nper          C                   '2nd period'
     D ApplyRateSplit  pr              n
     D inState                        2a
     D inDate                         7  0
     D outstcd                        5  0 options(*nopass)
     d xxFlRtAdj       s                   like(epampr)
     d xtpred          s                   like(epampr)
     d xxstcd          s              5  0
     d isContri        s              1
     D tasmin          s              9  2 inz
     D XXMINF          s              1    inz
     D*** Beg add ***  12/05/23  *********************************************
     D i               s              2  0 inz
     D xxwk11          s             11  0 inz
     D*** End add ***  12/05/23  *********************************************
      /copy wPrEmplr
     d/copy wPrGroup
     d/copy wPrPmtSvc
     d xxemsn          s              1    inz
     D xcILRate        c                   const('IL Flat rate adjustment')
     d xxFlRtAdjNY     s                   like(epampr)
     d xtpredNY        s                   like(epampr)
     D xcNYRate        c                   const('NY Flat rate Decrease')
     d isTN14          s               n
     d isOK14          s               n
     d dbcrHdg         s             30
     d outEnv          s              1
     d/copy wprDivPlan
     d isActive        pr              n
     d  effDt                              like(Elemd1)
     d isSplitMod      pr              n
     d  inFact1                            like(ELexm1)
     d  inFact2                            like(ELexm1)
     d  inPrem1                            like(ELexp1)
     d  inPrem2                            like(ELexp1)
     d  inDate1                            like(ELemd1)
     d  inDate2                            like(ELemd1)
     D  allDiv         s               n   Inz(*Off)
      *** Beg Add *****  08/28/25  *******************************************
     D SDCMLDds      e ds                  ExtName(SDCMP)
     D SDCMLDsv      e ds                  ExtName(SDCMP) Prefix(s)
     D wkFolder        s                   Inz Like(QQFOLD)
     D SlashPos        s              5  0 Inz
      *** End Add *****  08/28/25  *******************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     EMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C     EMKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR             3 0
     C*
     C     ELKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C*  EMPLOYER HISTORY FILE
     C*
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C*  SHORT RATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C*
     C     STKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C     STKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    X2EFDT
     C                   KFLD                    EDAYS
     C*
     C*  DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXCODE
     C                   KFLD                    X7DDEF
     C*
     C     DDKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXCODE
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
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
     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    ELRTCD
     C                   KFLD                    XXCLAS            5 0
     C                   KFLD                    X1EFDT            7 0
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AUDIT TRACKING FILE
     C*
     C     AUKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  OFFICE MASTER
     C*
     C     OFKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    EMOFF#
     C*
     C*  FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C*  EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTYPE            1
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXTYPE            1
     C                   KFLD                    XXYMD
     C*
     C*  BILLING WORK FILE
     C*
     C*** Begin Chg *** 09/26/23 *******************************************
     C*****BLKEY         KLIST
     C     BLKY          KLIST
     C*** End   Chg *** 09/26/23 *******************************************
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     BLKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  BILLING SCHEDULE CODE FILE
     C*
     C     BSKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  BILLING COMMENT FILE
     C*
     C     BCKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXBCOM           10
     C*
     C*  AGENT HISTORY FILE
     C*
     C     AHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  AGENT         FILE
     C*
     C     AAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXAGT#            5 0
     C*
     C*  AGENT MASTER FILE
     C*
     C     AGKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN            9 0
     C                   KFLD                    XXMOD             3 0
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C     UPKY1         KLIST
     C                   KFLD                    XXFASC
     C*
     C*  COMPANY FILE
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  INCR LIMIT TABLE FILE
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  INCR LIMIT TABLE FILE
     C*
     C     LMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    ELPFDT
     C*
     C     LMKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C                   KFLD                    X1EFDT
     C     PXKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C*
     C*  ADMIN/DARTS FILE
     C     EXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C     VHKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXYMD
     C*
     C* Key List for the Agent Producer Master File
     C*
     C     APKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
     C                   KFLD                    AHPROD
     C*
     C     PPKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPFND
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    EPFYR
     C*
     C     PPKY1         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPFND
     C                   KFLD                    EPEMP#
     C                   KFLD                    EPDIV
     C                   KFLD                    EPFYR
     C                   KFLD                    EPEFDT
     C*
     C* OUTPUT ROUTING FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    XXPGM            10
     C                   KFLD                    XXRPT#            1
     C*
     C     CTKEY         KLIST
     C                   KFLD                    EXAACD
     C     *LIKE         DEFINE    DDCODE        XXCODE
     C     *LIKE         DEFINE    EPAMPR        XXAMPR
     C     *LIKE         DEFINE    ELASP2        XXASPN
     C     *LIKE         DEFINE    EPEFDT        XXYMD
     C     *LIKE         DEFINE    EPEMP#        SVEMP#
     C     *LIKE         DEFINE    EPEMP#        XXEMP#
     C     *LIKE         DEFINE    EPDIV         XXDIV
     C     *LIKE         DEFINE    ELDT          XXEFDT
     C     *LIKE         DEFINE    ELDT          X2EFDT
     C     *LIKE         DEFINE    ELDT          XXLPDT
     C     *LIKE         DEFINE    ELDT          SVANDT
     C     *LIKE         DEFINE    ELDT          X1FROM
     C     *LIKE         DEFINE    ELDT          X1TO
     C     *LIKE         DEFINE    FNPH1A        XXPH1A
     C     *LIKE         DEFINE    EL2DEF        X7DDEF
     C     *LIKE         DEFINE    UPUSER        XXUSER
     C*
     C     *LIKE         DEFINE    XXDUED        XXDUDT
     C     *LIKE         DEFINE    Q1NETP        XXAMT
     C*
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQEMP#            9
     C                   PARM                    QQDIV             5
     C                   PARM                    QQDATE            8
     C                   PARM                    QQCOM1            2
     C                   PARM                    QQCOM2            2
     C                   PARM                    QQCOM3            2
     C                   PARM                    QQREV             1
     C                   PARM                    QQRMCD            5
     C                   PARM                    QQFCM1           30
     C                   PARM                    QQFCM2           30
     C                   PARM                    QQFCM3           30
     C                   PARM                    QQCONT            1
     C                   PARM                    QQFADC            5            FIN ADT RMRK CD
     C                   PARM                    QQFASC            3            FIN ADT STAF CD
     C                   PARM                    QQIDBD            1            DEDUCT INCLUDED
     C                   PARM                    QQALLDIV          1
       //*** Begin Add *** 09/26/23 ***************************************
       // Set Scriptura Indicators
       isUseSpoolOut = UseSpoolOut(processType);
       isUseScript   = UseScriptura(processType);
       isUseXML      = UseScriptura(processType) or Use3rdXml(processType);

       // Open Scriptura  Work files
       If isUseXML = *On;
          Open wxblp;
          Open wxblp1;
          Open wxblp2;
          //*** Beg Add ***** 06/18/25 **************************************
          //*** begin del ***  08/01/25  ****************************************
          //*****Open wxbll;
          //*** end del *****  08/01/25  ****************************************
          //*** End Add ***** 06/18/25 **************************************
       EndIf;

       // Populate svLDA data structure for Scriptura
       In svLda;
       Out svLda;
       //*** End   Add *** 09/26/23 ***************************************
     C*** Begin Add *** 08/28/25 ***************************************
     C                   Clear                   @@MDR
     C*** End   Add *** 08/28/25 ***************************************
     C*
     C* Set All division indicator
     C                   If        %Parms() >= 19
     C                   If        qqAllDiv = 'Y'
     C                   Eval      allDiv = *On
     C                   EndIf
     C                   EndIf
      *
      *  Deductible Included
      *
     C     Q1PRMS        IFGE      18
     C                   MOVE      QQIDBD        XXIDBD            1
     C                   ELSE
     C                   MOVE      *BLANK        XXIDBD
     C                   ENDIF
      *
     C*
     C     Q1PRMS        IFGE      16
     C                   MOVE      QQFADC        XXFADC            5
     C                   MOVE      QQFASC        XXFASC            3
     C     UPKY1         CHAIN     PUP0L1                             60
     C                   MOVE      UPUSR         XXUSER
     C                   ELSE
     C                   MOVE      *BLANKS       XXFADC
     C                   MOVE      *BLANKS       XXFASC
     C                   ENDIF
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
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      'A'           BILL              1
     C                   MOVE      'E'           TURN              1
     C                   MOVEL     CON(37)       SVTERH           30
     C                   MOVEL     CON(43)       SVDTRH           30
     C*
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C                   MOVE      CLTID         XXCLT#            3
     C                   MOVE      '0'           *IN25
      /Free
       *in26 = *off;
       If xxClt# = '025';
          *in25 = *on;
          *in26 = *on;
       Endif;
      /End-free
     C*** Begin Add *** 09/26/23 ***************************************
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         sysDt             7 0
     C*** End   Add *** 09/26/23 ***************************************
     C     XXCLT#        IFEQ      '139'
     C                   MOVEL     REMITO        XQSERH
     C                   ELSE
     C                   MOVEL     SERVBY        XQSERH
     C                   ENDIF
     C     QQREV         IFGE      'A'
     C     QQREV         ANDLE     'F'
     C*
     C                   SELECT
     C     QQREV         WHENEQ    'A'
     C                   MOVE      '1'           QQREV
     C     QQREV         WHENEQ    'B'
     C                   MOVE      '2'           QQREV
     C     QQREV         WHENEQ    'C'
     C                   MOVE      '3'           QQREV
     C     QQREV         WHENEQ    'D'
     C                   MOVE      '4'           QQREV
     C     QQREV         WHENEQ    'E'
     C                   MOVE      '5'           QQREV
     C     QQREV         WHENEQ    'F'
     C                   MOVE      '6'           QQREV
     C                   ENDSL
     C                   MOVE      *ON           *IN59
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN59
     C                   ENDIF                                                  QQREV IFGE
     C     QQREV         IFEQ      '2'
     C     QQREV         OREQ      '5'
     C                   MOVE      'Y'           XXCALC            1
     C                   ELSE
     C                   MOVE      ' '           XXCALC
     C                   ENDIF
     C                   MOVE      ' '           XXUPAR            1
     C*
     C     QQREV         IFEQ      '3'
     C     QQREV         OREQ      '6'
     C                   MOVE      'Y'           XXUPAR
     C                   ENDIF
     C*
     C     QQREV         IFEQ      '1'
     C     QQREV         OREQ      '2'
     C     QQREV         OREQ      '3'
     C                   MOVE      ' '           QQREV
     C                   ENDIF
     C*
     C     QQREV         IFEQ      '4'
     C     QQREV         OREQ      '5'
     C     QQREV         OREQ      '6'
     C                   MOVE      'Y'           QQREV
     C                   ENDIF
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQDATE        XXDATE            8 0
     C                   MOVE      QQCOM1        XXCOM1            2
     C                   MOVE      QQCOM2        XXCOM2            2
     C                   MOVE      QQCOM3        XXCOM3            2
     C                   MOVE      QQREV         XXREV             1
     C*
     C                   MOVEL(P)  QQRMCD        XXRM              5
     C*
     C*
     C* Combine division bills together if report #6 has been set up.
     C*
     C                   Move      *off          divCombine        1
     C                   Move      '*NO '        X6Hold            4
     C                   Move      '*NO '        X6Save            4
     C                   Clear                   X6Cpys            2 0
     C*
     C                   Move      '6'           xxRpt#
     C                   Move      Q1Pgm         xxPgm
     C                   If        cltid = '114'
     C                   Movel(p)  'AHBILLA'     xxPgm
     C                   Move      '0'           xxRpt#
     C                   Endif
     C     ORKY          Chain     Por01                              60
     C     *IN60         Ifeq      *OFF
     C     ORSTAT        Andne     'I'
     C*
     C                   Move      *On           divCombine
     C                   Move      ORHold        X6Hold
     C                   Move      ORSave        X6Save
     C                   Move      ORCopy        X6Cpys
     C*
     C                   Endif
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C                   Z-ADD     XXDATE        XXDUED
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVE      QQEMP#        SVEMP#
     C                   if        svemp# = *zero
     C                   move      'Y'           ##mass            1
     C                   else
     C                   move      'N'           ##mass
     C                   endif
     C                   Z-ADD     0             XSDIV             5 0
     C                   MOVE      ' '           XXSUM             1
     C*
     C                   Z-ADD     0             XTPRO            11 2
     C                   Z-ADD     0             XTMFEE           11 2
     C                   Z-ADD     0             XTINST           11 2
     C                   Z-ADD     0             XTSFEE           11 2
     C                   Z-ADD     0             XT1STB           11 2
     C                   Z-ADD     0             XT2NDB           11 2
     C                   Z-ADD     0             XTFEEB           11 2
     C                   Z-ADD     0             XT3RDB           11 2
     C                   Z-ADD     0             XT4THB           11 2
     C                   Z-ADD     0             XT5THB           11 2
     C                   Z-ADD     0             XT6THB           11 2
     C                   Z-ADD     0             XT7THB           11 2
     C                   Z-ADD     0             XTEXCN           11 2
     C                   Z-ADD     0             XTSTOP           11 2
     C                   Z-ADD     0             XTTXDU           11 2
     C                   Z-ADD     0             XTDPRL           11 2
     C                   Z-ADD     0             XTDPDU           11 2
     C                   Z-ADD     0             XTANCA           11 2
     C                   Z-ADD     0             XTPRPD           11 2          PREM PAID
     C                   Z-ADD     0             XTTAXP           11 2          TAX PAID
     C                   Z-ADD     0             XT1SPD           11 2          1ST PAID
     C                   Z-ADD     0             XT2NPD           11 2          2ND PAID
     C                   Z-ADD     0             XTEXPD           11 2          EXP CON PAID
     C                   Z-ADD     0             XTMFPD           11 2          MEMBER DUES PAID
     C                   Z-ADD     0             XTSFPD           11 2          SAFETY FEE PAID
     C                   Z-ADD     0             XTGFEE           11 2          GENERAL FEE BILL
     C                   Z-ADD     0             XTGFPD           11 2          GENERAL FEE PAID
     C                   Z-ADD     0             XTGPD            11 2          GENERAL FEE PAID
     C                   Z-ADD     0             XTDIVA           11 2          Dividend Amount
     c                   eval      xtpred = 0
     c                   eval      xxFlrtAdj = 0
     c                   eval      xtpredNY = 0
     c                   eval      xxFlrtAdjNY = 0
     C                   Clear                   divPrints         1
     C                   Z-ADD     0             xcAAPR           11 2
     C                   EVAL      tasmin = 0
     C                   EVAL      XXMINF = ' '
     C*** begin add ***  08/01/25  ********************************************
     C                   Move      *off          includeSum        1
     C                   Move      *off          excludeSum        1
     C                   Move      ' '           svARSM            1
     C                   Z-add     0             srdEXDT           7 0
     C*** end add *****  08/01/25  ********************************************
     C*
     C     SVEMP#        IFNE      *ZERO
     C*** begin add ***  08/01/25  ********************************************
     C* Get state rpt info to determine which divisions to process.
     C     EMKEY1        CHAIN(N)  PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        ANDNE     'Y'
     C     EMKEY1        READE(N)  PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   Z-add     EMDiv         xxDiv
     C*
     C     ELKEY         CHAIN(N)  PEL01                              60
     C                   Move      ELARSM        svARSM
     C                   Z-add     ELPtdt        srdEXDT
     C*
     C                   Move      QQDiv         xxDiv
     C*** end add *****  08/01/25  ********************************************
     C     ELKEY         CHAIN     PEL01                              60
     C                   Z-ADD     ELPTDT        SVEXDT            7 0
      *** Beg Del *****  05/12/25  *******************************************
      * Move this code to another place where WDELP is read in loop
     C*****              IF        ELAMNP = 'Y'
     C*****              EVAL      XXMINF = 'Y'
     C*****              ENDIF
      *** End Del *****  05/12/25  *******************************************
     C*** begin add ***  08/01/25  ********************************************
     C* Process all summarized divisions together, or exclude them if not processing
     C* a summarized state rpt div.
     C                   Select
     C                   When      ELARSM = 'Y' or
     C                             (svARSM = 'Y' and svEXDT = srdEXDT)
     C                   Move      *on           includeSum
     C                   When      ELARSM = ' ' and svARSM = 'Y' and
     C                             svEXDT <> srdEXDT
     C                   Move      *on           excludeSum
     C                   Endsl
     C*** end add *****  08/01/25  ********************************************
     C                   ENDIF
     C  N61              EXSR      SR1ST
     C*
     C                   SETOFF                                       8384
     C*  Chain to ADMIN/DARTS CONTROL file
     C     EXKY          CHAIN     PEX01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  'Agent #'     EXAHDR
     C                   ENDIF
     C*
     C*    Suppress msg if flag = 'Y'
     C     EXCMSG        IFEQ      'Y'
     C                   MOVE      *ON           *IN83
     C                   ENDIF
     C*
     C     EXDMSG        IFEQ      'Y'
     C                   MOVE      *ON           *IN84
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  'Agent #'     EXAHDR
     C     EXAHDR        CAT       ':':0         EXAHDR
     C*
     C                   ENDIF                                                  *IN60=*OFF
     C*
     C***  ACCESS USER PARAMETER FILE
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C* PULL THE HOLD STATUS FOR NEW FORMAT BILLS FROM REPORT #1 (OLD FORMAT).
     C*
     C                   Move      Q1Pgm         xxPgm
     C                   MOVE      '1'           XXRPT#
     C                   MOVE      '*YES'        X1HOLD            4
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      ORHOLD        X1HOLD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  GET THE FUND MASTER
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         CABEQ     '1'           END
      /Free
       isContri = GetPrmContFlg(xxCo#:xxFnd);
       If isContri = '1';
          *in26 = *On;
       Endif;
      /end-Free
     C                   MOVEL(P)  FNNAME        XXFNNM           40
     C     FNMAD2        COMP      *BLANKS                                65
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C                   MOVEL     FNMCTY        XXLAST           56
     C                   MOVEL     FNMST         XXSTZP           13
     C                   MOVE      FNMZIP        XXSTZP
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C                   MOVEL     FNMZIP        XXST10           10
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        FNCS             30
     C*
     C* BUILD FUND INFO FOR NEW FORMAT BILL.
     C*
     C                   MOVE      *BLANK        XXNEWF            1
     C                   MOVE      *OFF          *IN28
     C                   MOVE      *OFF          *IN29
     C                   MOVE      *OFF          *IN39
     C*
     C     FNKEY         CHAIN     PEX01                              60
     C*
     C                   EXSR      SRAGHD
     C*
     C     *IN60         IFEQ      *OFF
     C     EXBTYP        ANDEQ     'Y'
     C     CLTID         ANDNE     '046'
     C*
     C                   MOVE      'Y'           XXNEWF
     C*
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
     C                   MOVEL(P)  'Contri'      XQPRMW           30
     C                   CAT       'bution':0    XQPRMW
     C                   CAT       'Break':1     XQPRMW
     C                   CAT       'down':0      XQPRMW
     C                   OTHER
     C                   MOVEL(P)  'Premium'     XQPRMW           30
     C                   CAT       'Breakd':1    XQPRMW
     C                   CAT       'own':0       XQPRMW
     C                   ENDSL
     C*
     C                   MOVEL(P)  FNNAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  FNNAME        XQFNNM           60
     C*
     C                   MOVEL(P)  FNMAD1        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  FNMAD1        XQFAD1           40
     C     EXSGRA        IFEQ      'Y'
     C                   MOVE      *BLANKS       XQFAD1
     C                   ENDIF
     C*
     C     FNMAD2        IFEQ      *BLANKS
     C*
     C                   MOVEL(P)  XXLAST        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  XXLAST        XQFAD2           40
     C     EXSGRA        IFEQ      'Y'
     C                   MOVE      *BLANKS       XQFAD2
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XQFCTY           40
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  FNMAD2        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  FNMAD2        XQFAD2           40
     C     EXSGRA        IFEQ      'Y'
     C                   MOVE      *BLANKS       XQFAD2
     C                   ENDIF
     C*
     C                   MOVEL(P)  XXLAST        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL(P)  XXLAST        XQFCTY           40
     C     EXSGRA        IFEQ      'Y'
     C                   MOVE      *BLANKS       XQFCTY
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XQFPHN           22
     C     FNPH1         IFNE      *ZERO
     C                   MOVE      FNPH1A        X1AREA            3
     C                   MOVEL     FNPH1         X1PHN             3
     C                   MOVE      FNPH1         X2PHN             4
     C                   MOVEL     'Voice:'      XQFPHN
     C                   CAT       '(':2         XQFPHN
     C                   CAT       X1AREA:0      XQFPHN
     C                   CAT       ')':0         XQFPHN
     C                   CAT       X1PHN:1       XQFPHN
     C                   CAT       '-':0         XQFPHN
     C                   CAT       X2PHN:0       XQFPHN
     C                   ENDIF                                                  FNPHN1 IFNE
     C*
     C                   MOVE      *BLANKS       XQFPHF           20
     C     FNFAX         IFNE      *ZERO
     C                   MOVE      FNFAXA        X1AREA            3
     C                   MOVEL     FNFAX         X1PHN             3
     C                   MOVE      FNFAX         X2PHN             4
     C                   MOVEL     'Fax:'        XQFPHF
     C                   CAT       '(':2         XQFPHF
     C                   CAT       X1AREA:0      XQFPHF
     C                   CAT       ')':0         XQFPHF
     C                   CAT       X1PHN:1       XQFPHF
     C                   CAT       '-':0         XQFPHF
     C                   CAT       X2PHN:0       XQFPHF
     C                   ENDIF                                                  FNFAX IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   SETOFF                                       91
     C*
     C     FNPH1         IFNE      *ZERO
     C                   SETON                                        91
     C                   ELSE
     C     FNAREA        IFNE      *ZERO
     C                   MOVEL     '('           AREA1             1
     C                   MOVE      ')'           AREA4             4
     C                   MOVEL     FNAREA        AREA4
     C                   MOVE      AREA4         AREA5             5
     C                   MOVEL     AREA1         AREA5
     C                   END
     C                   END
     C     FNPH1A        IFNE      *ZERO
     C                   Z-ADD     FNPH1A        XXPH1A
     C                   ELSE
     C                   Z-ADD     800           XXPH1A
     C                   ENDIF
     C*
     C*  FORMAT ALTERNATE COMPANY ADDRESSES FOR PRINTING
     C*
     C*
     C*  GET THE FUND AUXILIARY
     C*
     C                   CLEAR                   F2BLOG
     C                   MOVE      *OFF          *IN85
     C     FNKEY         CHAIN     PF201                              60
     C     F2BLOG        IFNE      *BLANKS
     C                   MOVE      *ON           *IN85
     C                   ENDIF                                                  F2BLOG IFNE
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '046'
     C     F2BLOG        ANDEQ     'TIMBER'
     C                   Z-ADD     0.5           LOGHRZ
     C                   Z-ADD     6.3           LOGVRT
     C     CLTID         WHENEQ    '045'
     C     F2BLOG        ANDEQ     'LCTALOG2'
     C                   Z-ADD     0.5           LOGHRZ
     C                   Z-ADD     5.2           LOGVRT
     C                   when      CltID = '025'
     C                             and F2BLOG = 'LOGO025'
     C                   eval      logHrz =  .25
     C                   eval      logVrt =  .65
     C                   OTHER
     C                   Z-ADD     0.6           LOGHRZ
     C                   Z-ADD     5.2           LOGVRT
     C                   ENDSL
     C*
     C     CLTID         IFEQ      '025'
     C                   Z-ADD     5.36          STMHV
     C     'State'       CAT(P)    'ment #:':0   XQSTMH
     C                   MOVEL     EANINV        XQSTMD
     C                   ENDIF
     **
      *********  Check for Lock box address in Group Auxiliary file
      *********  If exists use lock box address instead of Accounting Ofc addr
     C*
     C*
     C*  GET THE FUND DETAIL
     C*
     C     XXFYR         IFNE      *ZERO
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         CABEQ     '1'           END
     C                   ENDIF                                                  XXFYR IFNE
     C*
     C     TURNT         TAG
     C*
     C* OPEN PRINT FILE FOR HARDCOPY OUTPUT
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   OPEN      QSYSPRT1
     C                   MOVE      *ON           *INOA
     C                   ENDIF
     C                   Z-ADD     0             XXPAG4            3 0
     C                   MOVE      '1'           OPENF             1
     C     SVEMP#        IFNE      *ZERO
     C     *IN59         ANDEQ     *ON
     C*
     C     EMKEY1        CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV
     C                   LEAVE
     C                   ENDIF
     C*
     C     EMKEY1        READE     PEM01                                  60
     C                   ENDDO
     C*
     C                   ENDIF                                                  SVEMP# IFNE
     C*
     C*  READ THE EMPLOYER RECORDS
     C*
        //*** Begin Add *** 09/26/23 ***************************************
        // Initialize Scriptura fields and work fields
        xxSt           = ' ';
        xmlBillName    = ' ';
        xmlSumName     = ' ';
        xmlCombName    = ' ';
        xmlSumCombName = ' ';
        //*** Beg Add ***** 04/24/24 ****************************************
        strXPFile      = ' ';
        strXPDir       = ' ';
        strXPFileComb  = ' ';
        strXPDirComb   = ' ';
        //*** Begin Add *** 05/07/24 ***************************************
        strXPFileSRD   = ' ';
        strXPDirSRD    = ' ';
        //*** End   Add *** 05/07/24 ***************************************
        //*** End Add ***** 04/24/24 ****************************************
        //*** Beg Add ***** 06/18/25 ***************************************
        comboBillXMLKey = ' ';
        //*** End Add ***** 06/18/25 ***************************************
        // Update LDA with output file name
        In *Lock *dtaara;
        ldOutputFile = ' ';
        Out *dtaara;
        //*** End   Add *** 09/26/23 ***************************************
     C     SVEMP#        CABNE     *ZERO         READEM
     C*
     C     TURN          IFEQ      'E'
     C*** Begin Chg *** 09/26/23 *******************************************
     C*****BLKEY         SETLL     PBL0E
     C     BLKY          SETLL     PBL0E
     C*** End   Chg *** 09/26/23 *******************************************
     C                   ELSE
     C*** Begin Chg *** 09/26/23 *******************************************
     C*****BLKEY         SETLL     PBL0A
     C     BLKY          SETLL     PBL0A
     C*** End   Chg *** 09/26/23 *******************************************
     C                   ENDIF                                                  TURN IFEQ
     C*
     C     READE         TAG
     C*
     C     TURN          IFEQ      'E'
     C*** Begin Chg *** 09/26/23 *******************************************
     C*****BLKEY         READE     PBL0E                                  60
     C     BLKY          READE     PBL0E                                  60
     C*** End   Chg *** 09/26/23 *******************************************
     C                   ELSE
     C*** Begin Chg *** 09/26/23 *******************************************
     C*****BLKEY         READE     PBL0A                                  60
     C     BLKY          READE     PBL0A                                  60
     C*** End   Chg *** 09/26/23 *******************************************
     C                   ENDIF                                                  TURN IFEQ
     C*
     C     *IN60         CABEQ     '1'           END

     C     XXSUM         IFEQ      'Y'
     C     BLEMP#        ANDNE     XXEMP#
     C                   MOVE      *BLANK        XXSUM
     C                   EXSR      PRTSUM
     C*
     C                   Z-ADD     0             XTPRO
     C                   Z-ADD     0             XTMFEE
     C                   Z-ADD     0             XTINST
     C                   Z-ADD     0             XTSFEE
     C                   Z-ADD     0             XTFEEB
     C                   Z-ADD     0             XT1STB
     C                   Z-ADD     0             XT2NDB
     C                   Z-ADD     0             XT3RDB
     C                   Z-ADD     0             XT4THB
     C                   Z-ADD     0             XT5THB
     C                   Z-ADD     0             XT6THB
     C                   Z-ADD     0             XT7THB
     C                   Z-ADD     0             XTEXCN
     C                   Z-ADD     0             XTSTOP
     C                   Z-ADD     0             XTTXDU
     C                   Z-ADD     0             XTDPRL
     C                   Z-ADD     0             XTDPDU
     C                   Z-ADD     0             XTANCA           11 2
     C                   Z-ADD     0             XTPRPD
     C                   Z-ADD     0             XTTAXP
     C                   Z-ADD     0             XT1SPD
     C                   Z-ADD     0             XT2NPD
     C                   Z-ADD     0             XTEXPD
     C                   Z-ADD     0             XTMFPD
     C                   Z-ADD     0             XTSFPD
     C                   Z-ADD     0             XTGFEE
     C                   Z-ADD     0             XTDIVA
     C                   Z-ADD     0             XTGFPD
     C                   Z-ADD     0             XTMAPR           11 2
     C                   Z-ADD     0             XTESBJ           11 2
     C                   Z-ADD     0             XTSTPR           11 2
     C                   Z-ADD     0             XTENMP           11 2
     C                   Z-ADD     0             XTEXCO           11 2
     C                   Z-ADD     0             XTESTP           11 2
     C*** Beg add ***  12/05/23  *********************************************
     C                   Z-ADD     0             XTMBAL           11 2
     C*** End add ***  12/05/23  *********************************************
     C                   Z-ADD     0             XTANPR           11 2
     C                   Z-ADD     0             XTEMPM           11 2
     C                   Z-ADD     0             XTETER           11 2
     C                   Z-ADD     0             XTEDTR           11 2
     C                   MOVE      *BLANK        XXNY              1
     C                   MOVE      *BLANK        XXMA              1
     C                   MOVE      *BLANK        XXPRF             1
     C*
     C                   ENDIF

      //*** Beg Add ***** 06/18/25 ****************************************
       If BLemp# <> XXemp# and strXPFileSRD = *Blank;
          Exsr RmvCombBill;
       Endif;
      //*** End Add ***** 06/18/25 ****************************************
     C*
     C* Resolve the combined bill together if report #6 has been set up.
     C*
     C                   If        divCombine = *on and xxNewF = 'Y'
     C                             and turn = 'E' and divPrints = *on
     C                             and BLEmp# <> xxEmp#
     C*
     C                   Exsr      RslCombo
     C*
     C                   Endif                                                  If divCombine = *on
     C*
     C* Clear print flag for next employer.
     C*
     C                   If        BLEmp# <> xxEmp#
     C                   Clear                   divPrints
     C                   Z-ADD     0             xcAAPR
        //*** Begin Add **# 09/26/23 ***************************************
           // Initialize Scriptura fields and work fields
           xxSt           = ' ';
           xmlBillName    = ' ';
           xmlSumName     = ' ';
           xmlCombName    = ' ';
           xmlSumCombName = ' ';
           //*** Beg Add ***** 04/24/24 *************************************
           strXPFile      = ' ';
           strXPDir       = ' ';
           strXPFileComb  = ' ';
           strXPDirComb   = ' ';
           //*** Begin Add *** 05/07/24 ************************************
           strXPFileSRD   = ' ';
           strXPDirSRD    = ' ';
           //*** End   Add *** 05/07/24 ************************************
           //*** End Add ***** 04/24/24 *************************************
           //*** Beg Add ***** 06/18/25 ***************************************
           comboBillXMLKey = ' ';
           //*** End Add ***** 06/18/25 ***************************************
           // Update LDA with output file name
           In *Lock *dtaara;
           ldOutputFile = ' ';
           Out *dtaara;
        //*** End   Add *** 09/26/23 ***************************************
     C                   Endif
     C*
     C     BLFLG2        IFNE      'Y'
     C                   GOTO      READE
     C                   ENDIF                                                  BLFLG2 NE Y
     C*
     C     BLFYR         IFNE      XXFYR
     C                   Z-ADD     BLFYR         XXFYR
     C*
     C*  GET THE FUND DETAIL
     C*
     C     F1KEY         CHAIN     PF101                              60
     C     *IN60         CABEQ     '1'           READE
     C*
     C                   ENDIF                                                  BLFYR IFNE
     C*
     C                   Z-ADD     BLEMP#        XXEMP#
     C                   Z-ADD     BLDIV         XXDIV
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C     XXREV         IFEQ      'Y'
     C     ELRAFL        IFNE      'R'
     C                   GOTO      READE
     C                   ENDIF
     C                   ELSE
     C     ELRAFL        IFEQ      'R'
     C                   GOTO      READE
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         sysDt             7 0
     C* If the employer/division is not expired yet and running mass
     C* audit then skip to next employer/division.
     C*
     C                   If        ELPtdt > sysDt and svEmp# = 0
     C     BLKEY2        Setgt     PBL01
     C                   Goto      READE
     C                   Endif                                                  If ELPTDT > sysdt
     C*
     C* If the employer/division is not audited and running mass
     C* audit then skip to next employer/division.
     C*
     C                   If        ELfaf <> 'A' and svEmp# = 0
     C     BLKEY2        Setgt     PBL01
     C                   Goto      READE
     C                   Endif                                                  If ELPTDT > sysdt
     C*
     C*  READ THE EMPLOYER RECORDS
     C*
     C     READEM        TAG
     C                   Z-ADD     0             xspfdt            7 0
      *** Beg Add ***** 04/04/25 *******************************
     C                   Z-ADD     0             xsptdt            7 0
      *** Beg end ***** 04/04/25 *******************************
      /Free
        //* Find policy effective date of state rpt division.

        //*** Begin Add *** 05/07/24 *******************************   *****
        xsRRN = 0;
        //*** End   Add *** 05/07/24 *******************************   *****
        Setll emkey1 pem01;
        Reade emkey1 pem01;
        Dow not %eof;
           If EMStrp = 'Y';
              Chain(n) (XXCO#:XXFND:XXEMP#:EMDIV:XXFYR) pel01;
              If %found;
                 //*** Begin Add *** 05/07/24 **********************   *****
                 xsRRN  = dbrrn;
                 //*** End   Add *** 05/07/24 **********************   *****
                 xspfdt = ELPfdt;
                 //*** Beg Add ***** 04/04/25 ******************************
                 xsptdt = ELPtdt;
                 //*** End Add ***** 04/04/25 ******************************
                 //*** Begin Add *** 09/26/23 ******************************
                 // Find the state reporting division state
                 xxSt = GetRatingState(ELco#:ELfnd:ELemp#:ELdiv:ELfyr);
                 //*** End   Add *** 09/26/23 ******************************
                 //*** Beg Add ***** 02/04/25 ******************************
                 xsDiv = EMDiv;
                 //*** End Add ***** 02/04/25 ******************************
                 //*** Beg Add ***** 05/12/25 ******************************
                 IF ELAMNP = 'Y';
                    XXMINF = 'Y';
                 ENDIF;
                 //*** End Add ***** 05/12/25 ******************************
              Endif;
              Leave;
           Endif;
           Reade emkey1 pem01;
        Enddo;

        //*** Begin Add *** 09/26/23 ***************************************
        // Set scriptura work fields
        xxBillType = processType;
        xxDivC     = 0;
        xxArSum    = ' ';
        xxCombine  = ' ';

        Chain (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
        If %Found(wdell1);
           DoU %Eof(wdell1);
              If ELarSm = 'Y';
                 xxArSum = 'Y';
              EndIf;
              xxDivC += 1;
              If xxArSum = 'Y' And xxDivC > 1;
                 Leave;
              EndIf;
              ReadE (xxCo#:xxFnd:xxFyr:xxEmp#) pel02;
           EndDo;
        EndIf;

        // Find bill template
        If isUseXml = *On;
           For idx = 1 to 4;
              Select;
              When idx = 1;
                 outFnd = xxFnd;
                 outSt = xxSt;
              When idx = 2;
                 outFnd = xxFnd;
                 outSt = *Blanks;
              When idx = 3;
                 outFnd = 0;
                 outSt = xxSt;
              When idx = 4;
                 outFnd = 0;
                 outSt = *Blanks;
              Endsl;

              // Did we find the template?
              hasBillingTemplate =
                GetBillingTemplate(xxCo#:xxBillTypeChar:outFnd:outSt);

              // Billing Template has been found so leave loop
              If hasBillingTemplate = *On;
                 Leave;
              Endif;

           EndFor;
        EndIf;

        // Check if flagged to combine bill output
        // Multi-Div, Summarized A/R, Merge Prm Notice

        // Process type was found
        //*** begin chg ***  07/29/25  ************************************
        //*****If hasBillingTemplate;
        //*** begin chg ***  08/01/25  **************************************
        //*****If hasBillingTemplate and *in59 = *off;
        If hasBillingTemplate and (svemp# = 0 or includeSum = *on or
         allDiv = *on or svexdt = srdexdt);
        //*** end chg *****  08/01/25  **************************************
        //*** end chg *****  07/29/25  ************************************
          If (BDCombA = 'Y' And (xxDivC > 1 Or xxArSum = 'Y' Or
             (BDPrmNA = 'Y' And BDMrgPrN <> *Blank)));
             xxCombine = 'Y';
          EndIf;
        EndIf;

        //Mass Request Flag
        If massReqGen = 'N';
           If svEmp# <> 0 and xxDivC = 1 and ELarsm <> 'Y' And
              BDPrmNA <> 'Y';
           Else;
              isMassReq = *On;
              massFlag = 'Y';
           Endif;
        Endif;
        //*** End   Add *** 09/26/23 ***************************************

      /End-Free
     C*
     C*
     C     SVEMP#        IFEQ      *ZERO
     C     EMKEY         CHAIN     PEM01                              60
     C                   ELSE
     C  N59EMKEY1        CHAIN     PEM01                              60
     C   59EMKEY         CHAIN     PEM01                              60
     C                   ENDIF
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EMDIV         XXDIV
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV
     C                   ENDIF
     C*
     C                   MOVE      *OFF          *IN57
     C                   MOVE      *BLANKS       XXCKNM           30
     C                   MOVE      *BLANKS       XXCONM
     C                   MOVE      *BLANKS       X2CKNM           40
     C                   Z-ADD     1             A
     C                   MOVE      *BLANKS       COM
     C*
     C     OFKY          CHAIN     POF01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     OFCKNM        XXCKNM
     C                   MOVEL     OFCKNM        X2CKNM
     C                   MOVEL(P)  OFACNM        XXCONM
     C     OFAPHN        IFNE      *ZERO
     C     XXCONM        CAT       'Phn#':5      XXCONM
     C     XXCONM        CAT       '(':1         XXCONM
     C                   MOVE      OFAARA        XXAREA            3
     C     XXCONM        CAT       XXAREA:0      XXCONM
     C     XXCONM        CAT       ')':0         XXCONM
     C     XXCONM        CAT       OFAPH1:1      XXCONM
     C     XXCONM        CAT       '-':0         XXCONM
     C     XXCONM        CAT       OFAPH2:0      XXCONM
     C                   ENDIF                                                  OFAPHN IFNE
     C*
      *
     C                   MOVEL(P)  OFAAD1        COM(A)
     C                   ADD       1             A
     C     OFAAD2        IFNE      *BLANKS
     C                   MOVEL(P)  OFAAD2        COM(A)
     C                   ADD       1             A
     C                   ENDIF
     C                   MOVEL(P)  OFACTY        XXMADR           50
     C     XXMADR        CAT       ',':0         XXMADR
     C     XXMADR        CAT       OFAST:1       XXMADR
     C     XXMADR        CAT       OFMZ1:1       XXMADR
     C     OFMZ2         IFNE      '0000'
     C     XXMADR        CAT       '-':0         XXMADR
     C     XXMADR        CAT       OFMZ2:0       XXMADR
     C                   ENDIF
     C                   MOVEL(P)  XXMADR        COM(A)
     C                   ADD       1             A
     C     XXNEWF        IFEQ      'Y'
     C*
     C                   MOVE      *BLANKS       XQCONM           40
     C                   MOVE      *BLANKS       XQCOM1           50
     C                   MOVE      *BLANKS       XQCOM2           50
     C                   MOVE      *BLANKS       XQCOM3           50
     C                   MOVE      *BLANKS       XQCOM4           50
     C*
     C                   MOVEL     OFACNM        XQCONM
     C                   MOVEL     COM(1)        XQCOM1
     C                   MOVEL     COM(2)        XQCOM2
     C*
     C     COM(3)        IFNE      *BLANKS
     C                   MOVEL     COM(3)        XQCOM3
     C*
     C     OFAPHN        IFNE      *ZERO
     C                   MOVEL     '('           XQCOM4
     C                   MOVE      OFAARA        XXAREA            3
     C     XQCOM4        CAT       XXAREA:0      XQCOM4
     C     XQCOM4        CAT       ')':0         XQCOM4
     C     XQCOM4        CAT       OFAPH1:1      XQCOM4
     C     XQCOM4        CAT       '-':0         XQCOM4
     C     XQCOM4        CAT       OFAPH2:0      XQCOM4
     C                   ENDIF                                                  OFAPHN IFNE
     C*
     C                   ELSE
     C*
     C     OFAPHN        IFNE      *ZERO
     C                   MOVEL     '('           XQCOM3
     C                   MOVE      OFAARA        XXAREA            3
     C     XQCOM3        CAT       XXAREA:0      XQCOM3
     C     XQCOM3        CAT       ')':0         XQCOM3
     C     XQCOM3        CAT       OFAPH1:1      XQCOM3
     C     XQCOM3        CAT       '-':0         XQCOM3
     C     XQCOM3        CAT       OFAPH2:0      XQCOM3
     C                   ENDIF                                                  OFAPHN IFNE
     C*
     C                   ENDIF                                                  COM,3 IFNE
     C*
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     **
     C*
     C*  RETRIEVE CURRENT EMPLOYER DETAIL RECORD TO DETERMINE
     C*  IF SELF-REPORTING TYPE IN **CURRENT** YEAR.
     C*
     C                   MOVE      'N'           XXSELF
     C     ELKEY2        SETGT     PEL01
     C                   READP     PEL01                                  60
     C     *IN60         IFEQ      '0'
     C     XXCO#         ANDEQ     ELCO#
     C     XXFND         ANDEQ     ELFND
     C     XXEMP#        ANDEQ     ELEMP#
     C     XXDIV         ANDEQ     ELDIV
     C*
     C*  RETRIEVE BILLING SCHEDULE CODE TO DETERMINE IF
     C*  SELF-REPORTING
     C*
     C                   MOVE      *BLANKS       XXKEY            10
     C                   MOVEL     ELBSCD        XXKEY
     C     BSKEY1        CHAIN     PBS01                              60
     C     *IN60         IFEQ      '0'
     C     BSSEFL        ANDEQ     'S'
     C                   MOVE      'Y'           XXSELF            1
     C                   END
     C     *IN60         IFEQ      '0'
     C                   MOVEL(P)  BSDESC        XBDESC           25
     C                   ELSE
     C                   MOVE      *BLANKS       XBDESC
     C                   ENDIF                                                  *IN60 IFEQ
     C                   END
     C*
     C*  RETRIEVE EMPLOYER DETAIL FOR YEAR BEING AUDITED
     C**
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANKS       XQMINM           45
     C     ELAMNP        IFEQ      'Y'
     C                   MOVEL     MSG(1)        XQMINM
     C                   ENDIF
      /Free
       If ELamnp = 'Y' and isContri = '1';
          XQMINM = MSG(2);
       Endif;
      /End-free
      /free
       MinPrmMsg1 = *Blanks;
       MinPrmMsg2 = *Blanks;
       // Add a note at the bottom of the balance box to indicate:
       // * Minimum has been invoked and all premium except terrorism charges are
       // billed to state reporting division.
       IF XXMINF = 'Y';
          MinPrmMsg1 = '* Minimum has been invoked and all premium except +
           terrorism';
          MinPrmMsg2 = 'charges are billed to state reporting division.';
       Endif;
      /end-free
     C*
     C*** begin chg ***  08/01/25  ********************************************
     C*****ELPTDT        IFEQ      SVEXDT
     C*****SVEMP#        OREQ      *ZERO
     C*****allDiv        orEQ      *On
     C                   Clear                   processDiv        1
     C* Turn on indicator to run division if not running mass.
     C                   Select
     C                   When      svemp# = 0
     C                   When      allDiv = *on
     C                   Move      *on           processDiv
     C                   When      ELARSM = 'Y' and includeSum = *on
     C                   Move      *on           processDiv
     C                   When      ELARSM = ' ' and excludeSum = *on and
     C                             ELPtdt = svExdt
     C                   Move      *on           processDiv
     C                   When      ELPtdt = svExdt and excludeSum = *off
     C                   Move      *on           processDiv
     C                   Endsl
     C*
     C                   If        svemp# = 0 or processDiv = *on
     C*** end chg *****  08/01/25  ********************************************
     C*
     C**
     C*
     C*  CHAIN FOR STAT RPT/RATING INFO
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C     RRKY          SETGT     PIF01
     C     RRKY2         READPE    PIF01                                  60
     C                   Move      *off          *in49
     C*
     C* Set title for additional load for monopolistic states
     C*
     C                   If        RRMono = 'Y'
     C*
     C                   Select
     C                   When      cltid = '148'
     C                   Move      *on           *in49
     C                   Endsl
     C*
     C                   Endif
     C*
     C* Set the terrorism heading fields to optional or default heading.
     C*
     C     RRTERH        IFNE      *BLANKS
     C                   MOVEL(P)  RRTERH        CON(37)
     C                   ELSE
     C                   MOVEL(P)  SVTERH        CON(37)
     C                   ENDIF
     C*
     C     RRDTRH        IFNE      *BLANKS
     C                   MOVEL(P)  RRDTRH        CON(43)
     C                   ELSE
     C                   MOVEL(P)  SVDTRH        CON(43)
     C                   ENDIF
     C**
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFROM            8 0
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTO              8 0
     C*
     C                   Z-ADD     ELEMD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEMD1            8 0
     C*
     C                   Z-ADD     ELEMD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEMD2            8 0
     C*
     C                   Z-ADD     ELEMD3        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEMD3            8 0
      /free
       strMod = 'Mod '  ;
       strEffdt = 'Eff Dates';
       If ELARP1 <> 0 or ELARP2 <> 0 or ELARP3 <> 0;
          strARAP  = 'ARAP';
       Endif;
      /End-free
     C                   Z-ADD     ELARP1        XXARP1            5 4
     C                   Z-ADD     ELARP2        XXARP2            5 4
     C                   Z-ADD     ELARP3        XXARP3            5 4
     C                   Z-ADD     ELEXM1        XXEXM1            5 4
     C                   Z-ADD     ELEXM2        XXEXM2            5 4
     C                   Z-ADD     ELEXM3        XXEXM3            5 4
      /free
       Clear strMod1;
       Clear strMod2;
       Clear strMod3;
      /end-free
      /free
       //Only print first modifier if 1st Modifier is Rated (Elnoexm1 = ' ').
       //If IsActive(ELemd1);
       If IsActive(ELemd1) and Elnoexm1 = ' ';
          strMod1 = %editc(xxExm1:'2') + ' ' +
           %editc(xxArp1:'2') +
           '  ' + %char(%date(xxEmd1:*usa):*usa);
       Endif;
       //*** Begin Chg *** 07/30/23 ****************************************
       //********** If IsActive(ELemd2);
       //Only print first modifier if 2nd Modifier is Rated (Elnoexm2 = ' ').
       If IsActive(ELemd2) and Elnoexm2 = ' ';
       //*** End   Chg *** 07/30/23 ****************************************
          strMod2 = %editc(xxExm2:'2') + ' ' +
           %editc(xxArp2:'2') +
           '  ' + %char(%date(xxEmd2:*usa):*usa);
       Endif;
       //*** Begin Chg *** 07/30/23 ****************************************
       //********** If IsActive(ELemd3);
       //Only print first modifier if 3rd Modifier is Rated (Elnoexm3 = ' ').
       If IsActive(ELemd3) and Elnoexm3 = ' ';
       //*** End   Chg *** 07/30/23 ****************************************
          strMod3 = %editc(xxExm3:'2') + ' ' +
           %editc(xxArp3:'2') +
           '  ' + %char(%date(xxEmd3:*usa):*usa);
       Endif;
      /end-free
     C*
     C                   MOVE      XXFND         QPFND
     C                   MOVE      XXEMP#        QPEMP#
     C                   MOVE      XXFYR         QPFYR
     C*
     C*  CHECK IF EMPLOYER IS CANCELLED BEFORE POLICY FROM DATE
     C*
     C                   Z-ADD     1             Y                 1 0
     C     *ZERO         LOOKUP    ED(Y)                                  60
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     5             Y
     C                   ELSE
     C                   SUB       1             Y
     C                   END
     C     Y             DOWNE     0
     C     ELPTDT        COMP      ED(Y)                              60  60
     C  N60              SUB       1             Y
     C  N60              END
     C*
     C                   MOVE      *BLANK        SKIPE             1
     C*
     C     Y             IFEQ      0
     C                   MOVE      'Y'           SKIPE
     C                   END
     C*
     C     CD(Y)         IFNE      0
     C     CD(Y)         ANDLT     ELPFDT
     C                   END
     C     SKIPE         IFEQ      *BLANK
     C*
     c*
     c* Print Deductible, Exp Modifier, and Limits info on all bills except summary bill.
     c*
     C                   MOVE      *OFF          *IN58
     C     ELARSM        IFEQ      'Y'
     C                   MOVE      *ON           *IN57
     C                   MOVE      'Y'           XXSUM
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   OPEN      W7615O
     C                   MOVE      *ON           XX1ST             1
     C                   ENDIF                                                  XXNEWF IFEQ
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   OPEN      QSYSPRT
     C                   Z-ADD     *ZEROS        XXPAGE            3 0
     C                   MOVE      *ON           *INOF
     C                   ENDIF                                                  TURN IFEQ
     C                   Z-ADD     0             XXPAG4            3 0
     C*
     C                   MOVE      *ON           *INOA
     C*
     C                   SETOFF                                       63
     C*    DETERMINE IF REVISED BILLING
     C     XXREV         IFEQ      'Y'
     C                   SETON                                        63
     C                   END
     C*
     C*    DETERMINE IF SHORT RATED NORMAL PREM
     C*
     C                   MOVE      *BLANK        XXSRPR            1
     C     ELSRPR        IFEQ      'S'
     C     ELAMNP        ANDNE     'Y'
     C                   MOVE      'Y'           XXSRPR
     C                   END
     C     EPKEY         CHAIN     PRP01                              60
     C     *IN60         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANKS
     C                   MOVEL(P)  RPCAP#        XXCAP#           25
     C                   MOVE      *ON           *IN66
     C                   ELSE
     C                   MOVE      *BLANKS       XXCAP#
     C                   MOVE      *OFF          *IN66
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     ELMFEE        F1MFEE
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
     C     @@PLAN        IFNE      'A'
     C                   MOVE      *OFF          *IN37
     C                   ELSE
     C                   MOVE      *ON           *IN37
     C                   ENDIF
     C*
     C     VHKY1         SETGT     PVH01
     C     ELKEY         READPE    PVH01                                  62
     C     *IN62         IFEQ      *OFF
      *
     C                   SELECT
     C     VHEXDT        WHENEQ    *ZERO
     C                   MOVE      *ON           VDRFLG            1
     C     VHEXDT        WHENNE    *ZERO
     C     VHEXDT        ANDGT     XXYMD
     C                   MOVE      *ON           VDRFLG
     C                   OTHER
     C                   MOVE      *OFF          VDRFLG
     C                   ENDSL
      *
     C                   ELSE
     C                   MOVE      *OFF          VDRFLG
     C                   ENDIF
      *
     C     @@PLAN        IFNE      'A'
     C     VDRFLG        OREQ      *ON
     C                   MOVE      *OFF          *IN92
     C                   ELSE
     C                   MOVE      *ON           *IN92
     C                   ENDIF
     C**
     C**   BUILD ADDRESS
     C**
     C     EMKEY         CHAIN     PA301                              60
     C*
     C                   MOVE      *BLANKS       EMN
     C                   MOVE      *BLANKS       EMN2
     C                   Z-ADD     1             B
     C                   Z-ADD     1             A
     C*
     C                   MOVEL(P)  EMNAME        EMN(A)
     C                   MOVEL(P)  EMNAME        EMN2(B)
     C                   ADD       1             B
     C                   ADD       1             A
     C*
     C     EMFDNM        IFNE      *BLANKS
     C                   MOVEL(P)  EMFDNM        EMN(A)
     C                   ADD       1             A
     C                   MOVEL(P)  EMFDNM        EMN2(B)
     C                   ADD       1             B
     C                   ENDIF                                                  EMFDNM IFNE
     C     *IN60         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3CONT        ANDNE     *BLANKS
     C                   MOVEL     A3CONT        EMN2(B)
     C                   ADD       1             B
     C                   ENDIF
     C*
     C     *IN60         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C                   MOVEL     A3ADR1        EMN(A)
     C                   ADD       1             A
     C                   MOVEL     A3ADR1        EMN2(B)
     C                   ADD       1             B
     C     A3ADR2        IFNE      *BLANKS
     C                   MOVEL     A3ADR2        EMN(A)
     C                   ADD       1             A
     C                   MOVEL     A3ADR2        EMN2(B)
     C                   ADD       1             B
     C                   ENDIF
     C                   ELSE                                                   *IN60 IFEQ
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL(P)  EMMAD1        EMN(A)
     C                   ADD       1             A
     C                   MOVEL     EMMAD1        EMN2(B)
     C                   ADD       1             B
     C     EMMAD2        IFNE      *BLANKS
     C                   MOVEL(P)  EMMAD2        EMN(A)
     C                   ADD       1             A
     C                   MOVEL     EMMAD2        EMN2(B)
     C                   ADD       1             B
     C                   ENDIF                                                  EMMAD2 IFNE
     C                   ELSE
     C                   MOVEL(P)  EMPAD1        EMN(A)
     C                   ADD       1             A
     C                   MOVEL     EMPAD1        EMN2(B)
     C                   ADD       1             B
     C     EMPAD2        IFNE      *BLANKS
     C                   MOVEL(P)  EMPAD2        EMN(A)
     C                   ADD       1             A
     C                   MOVEL     EMPAD2        EMN2(B)
     C                   ADD       1             B
     C                   ENDIF                                                  EMPAD2 IFNE
     C                   ENDIF                                                  EMMPFL IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C     *IN60         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C                   MOVEL     A3CTY         XXLAST
     C                   MOVEL     A3ST          XXSTZP           13
     C                   MOVEL     A3ZIP1        XXZZ              9
     C                   MOVE      A3ZIP2        XXZZ              9
     C                   MOVE      XXZZ          XXSTZP
     C                   ELSE
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMCTY        XXLAST           56
     C                   MOVEL     EMMST         XXSTZP           13
     C                   MOVE      EMMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     EMPCTY        XXLAST           56
     C                   MOVEL     EMPST         XXSTZP           13
     C                   MOVE      EMPZIP        XXSTZP
     C                   ENDIF
     C                   ENDIF                                                  *IN60 IFEQ
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     *IN60         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C                   MOVEL     A3ZIP1        XXST10           10
     C                   ELSE
     C     EMMPFL        IFEQ      'M'
     C                   MOVEL     EMMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     EMPZIP        XXST10           10
     C                   END
     C                   ENDIF                                                  *IN60 IFEQ
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   END
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        EMN(A)
     C                   MOVEL     XXLAST        EMN2(B)
     C     *IN60         IFEQ      *OFF
     C     A3STAT        ANDEQ     'A'
     C     A3ADR1        ANDNE     *BLANKS
     C     A3OOC         ANDNE     *BLANKS
     C                   MOVEL(P)  A3OOC         EMN(A)
     C                   MOVEL     A3OOC         EMN2(B)
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ADD       1             A
     C                   ADD       1             B
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C                   MOVE      *BLANKS       XXPROD           45
     C                   MOVEL     XXYMD         XXEFDT
     C     AHKEY         SETGT     PAH01
     C                   READP     PAH01                                  50
     C     *IN50         DOWEQ     '0'
     C     AHSTAT        ANDEQ     'I'
     C                   READP     PAH01                                  50
     C                   END
     C*
     C     *IN50         IFEQ      '0'
     C     XXCO#         ANDEQ     AHCO#
     C     XXFND         ANDEQ     AHFND
     C     XXEMP#        ANDEQ     AHEMP#
     C     XXDIV         ANDEQ     AHDIV
     C     XXFYR         ANDEQ     AHFYR
     C                   MOVEL     AHAGT#        XXAGT#
     C*
     C     AAKEY         CHAIN     PAA01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AAFEIN        XXFEIN
     C                   MOVEL     AAMOD         XXMOD
     C                   ELSE
     C                   MOVE      *ZEROS        XXFEIN
     C                   MOVE      *ZEROS        XXMOD
     C                   END
     C*
     C     APKY          CHAIN     PAP01                              50
     C     *IN50         IFEQ      *OFF
     c     xxclt#        andne     '025'
     C*
     C     APFSN         IFNE      *BLANKS
     C                   MOVEL     APFSN         XXPROD
     C                   CAT       APLSN:1       XXPROD
     C                   ELSE
     C                   MOVEL     APLSN         XXPROD
     C                   ENDIF                                                  APFSN IFNE
     C*
     C                   ENDIF                                                  *IN50 IFEQ
     C     AGKEY         CHAIN     PAG01                              50
     C     *IN50         IFEQ      '0'
     C                   MOVEL     AGNAME        ANAME            34
     C                   MOVE      *BLANKS       XXLAST
     C                   MOVE      *BLANKS       XXFRST
     C                   MOVE      *BLANKS       XXSTZP
     C                   MOVE      *BLANKS       XXZIP4
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMAD1        AADD1            34
     C                   MOVEL     AGMCTY        XXSHRT           19
     C                   MOVEL     XXSHRT        XXLAST           56
     C                   MOVEL     AGMST         XXSTZP           13
     C                   MOVE      AGMZIP        XXSTZP
     C                   ELSE
     C                   MOVEL     AGPAD1        AADD1
     C                   MOVEL     AGPCTY        XXSHRT
     C                   MOVEL     XXSHRT        XXLAST
     C                   MOVEL     AGPST         XXSTZP
     C                   MOVE      AGPZIP        XXSTZP
     C                   ENDIF
     C                   MOVE      XXSTZP        XXZIP4            4
     C*
     C     XXZIP4        IFEQ      *ZERO
     C                   MOVE      '    '        XXSTZP
     C                   ELSE
     C                   MOVEL     '-'           XX5
     C                   MOVE      XXZIP4        XX5               5
     C     AGMAD1        IFNE      *BLANKS
     C                   MOVEL     AGMZIP        XXST10           10
     C                   ELSE
     C                   MOVEL     AGPZIP        XXST10           10
     C                   ENDIF
     C                   MOVE      XX5           XXST10           10
     C                   MOVE      XXST10        XXSTZP
     C                   ENDIF
     C*
     C                   MOVEL     XXSTZP        XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        ACSZ             34
     C                   ELSE
     C                   MOVE      *BLANKS       ANAME
     C                   MOVE      *BLANKS       AADD1
     C                   MOVE      *BLANKS       ACSZ
     C                   MOVE      *ZEROS        AGARE1
     C                   MOVE      *ZEROS        AGPHN1
     C                   END
     C*
     C                   ELSE
     C                   MOVE      *BLANK        AHAGT#
     C                   MOVE      *BLANK        ANAME
     C                   MOVE      *BLANKS       AADD1
     C                   MOVE      *BLANKS       ACSZ
     C                   MOVE      *ZEROS        AGARE1
     C                   MOVE      *ZEROS        AGPHN1
     C                   END
     C*
     C     AGPHN1        IFEQ      *ZEROS
     C                   MOVE      *OFF          *IN69
     C                   ELSE
     C                   MOVE      *ON           *IN69
     C                   ENDIF
     C                   MOVE      *BLANK        XXLRGD            1
     C*
     C*   VERIFY IF POLICY IS LARGE DEDUCTIBLE BASED ON BILLING SCHEDULE CODE
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     BSKEY1        CHAIN     PBS01                              60
     C*
     C     *IN60         IFEQ      *ZERO
     C     BSLPGM        ANDEQ     'L'
     C*
     C     ELKEY         CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           XXLRGD
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     XXLRGD        IFEQ      'Y'
     C     SVEMP#        ANDEQ     *ZERO
     C                   MOVE      UPLVL         ##ULVL
     C                   MOVE      UPTYPE        ##UTYP
     C                   MOVE      EXLDAL        ##CLVL
     C                   MOVE      EXLDAT        ##CTYP
     C*
     C                   CALL      'SRCCSC'
     C                   PARM                    ##UTYP            1
     C                   PARM                    ##ULVL            3
     C                   PARM                    ##CTYP            1
     C                   PARM                    ##CLVL            3
     C                   PARM      '1'           ##EXEC            1
     C                   PARM      ' '           ##RES             1
      *
     C     ##RES         IFEQ      *ON
     C                   EXSR      PRTLDE
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   CLOSE     W7615O
     C                   ENDIF
     C                   GOTO      READE
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* BUILD STRINGS FOR NEW FORMAT BILL.
     C*
     C                   MOVE      *OFF          *IN90
     C                   Z-ADD     1             XQPROR            9 6
     C     XXNEWF        IFEQ      'Y'
     C*
     C* COUNT # OF DIVISIONS ON POLICY.
     C*
     C                   Z-ADD     0             XXDIVC            5 0
     C     ELKY2         CHAIN     PEL02                              60
     C     *IN60         DOWEQ     *OFF
      *
     C     ELPFDT        IFNE      ELPTDT
     C                   ADD       1             XXDIVC
     C                   ENDIF
      *
     C     ELKY2         READE     PEL02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     ELKEY         CHAIN     PEL01                              60
      /Free
       Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pmp01;
      /end-free
      *** Beg Add *****  05/12/25  *******************************************
     C                   Move      *blank        PrintFlg          1
      *** End Add *****  05/12/25  *******************************************
     C*
     C* COMPUTE NET RATE FACTOR.
     C*
     C     F1NETR        IFEQ      'Y'
     C     ELPFDT        ANDGE     F1NETD
     C                   MOVE      *ON           *IN90
     C*
     C     ELAMPR        IFNE      *ZERO
     C     ELANMP        DIV(H)    ELAMPR        XZPROR            9 6
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVEL(P)  'MAKE CHE'    XXCHCK           65
     C                   CAT       'CKS PA':0    XXCHCK
     C                   CAT       'YABLE':0     XXCHCK
     C                   CAT       'TO':1        XXCHCK
     C     LO:UP         XLATE     X2CKNM        X1CKNM           40
     C                   CAT       X1CKNM:1      XXCHCK
       xx3rdPV = ' ';
       If ICPolicyOpt(xxCo#:xxFnd:xxEmp#) = 'Y';
          If OF3rdVrb <> ' ';
             xx3rdPV = OF3rdVrb;
             xxChck  = ' ';
          EndIf;
       EndIf;
     C*
     C                   MOVEL     XXPROD        X1PRNM
     C     XXCLT#        IFEQ      '025'
     C                   MOVEL     *BLANKS       X1PRNM
     C                   ENDIF
     C                   Z-ADD     XXAGT#        XQAGT#
     C*
     C     ANAME         IFNE      *BLANKS
     C                   MOVEL     AGNAME        XQAGNM           40
     C                   MOVEL     AGNAME        X2AGNM
     C                   ELSE
     C                   MOVE      *BLANKS       XQAGNM
     C                   MOVE      *BLANKS       X2AGNM
     C                   ENDIF
     C*
     c                   select
     c                   when      cltid  = '025'
     c                   eval      xqqref = '      Participant #:'
     c                   when      cltid  = '071'
     c                   eval      xqqref = pol#71
     c                   other
     c                   eval      xqqref = 'Policy Reference:'
     c                   endsl
     C*
     C* TITLE ON BILL
     C*
     C                   MOVE      *ON           *IN31
     C                   MOVEL(P)  TIT2(1)       X5TITL           20
     C                   MOVEL(P)  TIT2(2)       X6TITL           20
       If cltId = '148' and F2CAR# = 38073;
          x5Titl = '  AUDIT - NOT A BILL';
          x6Titl = '  AUDIT - NOT A BILL';
       Endif;
     C                   MOVEL(P)  TIT2(1)       X5TITL2          20
     C                   MOVEL(P)  TIT2(2)       X6TITL2          20
     C*
     c                   select
     c                   when      cltid = '025'
     c                   call      'WA005R'
     c                   parm                    xxemp#
     c                   parm                    xxdiv
     c                   parm                    xxname
     c                   parm                    xxedn            80
     c                   eval      xqpol# = xxedn
     c                   other
     c                   move      qqpol#        xqpol#           15
     c                   endsl
     C     CLTID         IFEQ      '071'
     C                   MOVEL(P)  T71(1)        X71INF           40
     C                   MOVE      *BLANKS       X71INF
     C                   EXSR      SRP71#
     C                   ENDIF
     C*
     C     XXDIVC        IFGT      1
     C     CLTID         OREQ      '071'
     C     CLTID         ORNE      '071'
     C                   MOVEL(P)  'Division'    XQDIVH           20
     C                   CAT       ':':0         XQDIVH
     C                   MOVE      XXDIV         XQDIV             5
     C                   ELSE
     C                   MOVE      *BLANKS       XQDIVH
     C                   MOVE      *BLANKS       XQDIV
     C                   ENDIF                                                  XXDIVC IFGT
     C*
     C                   MOVEL(P)  'Rating S'    XQRSTH           20
     C                   CAT       'tate:':0     XQRSTH
     C*
     C     *IN66         IFEQ      *ON
     C                   MOVEL(P)  'Carrier'     XQCAPH           20
     C                   CAT       'Pol#:':1     XQCAPH
     C                   ENDIF                                                  *IN66 IFEQ
     C*
     C                   MOVEL(P)  'Policy'      XQPROF           20
     C                   CAT       'For:':1      XQPROF
     C*
     C                   MOVEL(P)  'Print'       XQPROD           20
     C                   CAT       'Date:':1     XQPROD
     C*
     c                   select
     c                   when      cltid = '025'
     c                   eval      xqdsth = 'County District #:'
     c                   clear                   cndesc
     c                   select
     c                   when      EMPST <> *blanks
     c                   eval      xxkey = empst + emcnty
     c                   other
     c                   eval      xxkey = emmst + emcnty
     c                   endsl
     c     tbkey         chain     pcn01
     c                   if        CNDESC <> *blanks
     c                   eval      xqdstd = %trim(CNDESC)
     c                                      + '-' + %subst(EMSEMP:2:6)
     c                   else
     c                   eval      xqdstd = %subst(EMSEMP:2:6)
     c                   endif
     c                   eval      xqperd = 'Participation Period:'
     c                   eval      pdthz = 1.85
     c                   other
     c                   eval      xqperd = 'For Period:'
     c                   eval      pdthz = 1.4                                  policy dates X coord
     c                   endsl
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
     C                   MOVEL(P)  'Contri'      XQPRMW           30
     C                   CAT       'bution':0    XQPRMW
     C                   CAT       'Break':1     XQPRMW
     C                   CAT       'down':0      XQPRMW
     C                   OTHER
     C                   MOVEL(P)  'Premium'     XQPRMW           30
     C                   CAT       'Breakd':1    XQPRMW
     C                   CAT       'own':0       XQPRMW
     C                   ENDSL
     C*
     C                   MOVEL(P)  'Balance'     XQBALW           30
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQPRDT           20
     C                   CAT       XXDAY:1       XQPRDT
     C                   CAT       ',':0         XQPRDT
     C                   CAT       XXYR:1        XQPRDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQPFDT           20
     C                   CAT       XXDAY:1       XQPFDT
     C                   CAT       ',':0         XQPFDT
     C                   CAT       XXYR:1        XQPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQPTDT           20
     C                   CAT       XXDAY:1       XQPTDT
     C                   CAT       ',':0         XQPTDT
     C                   CAT       XXYR:1        XQPTDT
     C*
     C                   MOVEL(P)  XQPFDT        XQPOLP           45
     C                   CAT       ' to':1       XQPOLP
     C                   CAT       XQPTDT:1      XQPOLP
     C*
     C                   MOVE      *BLANKS       X1DUED
     C     XXDUED        IFNE      *ZERO
     C     XXDUED        ANDGT     99
     C                   MOVEL     XXDUED        XQDT
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       X1DUED
     C                   CAT       XXDAY:1       X1DUED
     C                   CAT       ',':0         X1DUED
     C                   CAT       XXYR:1        X1DUED
     C                   ENDIF                                                  XXDUED IFNE
           //*** Begin Del *** 09/12/24 ************************************
           //*** Begin Add *** 03/20/24 ************************************
           //*** If AuditCreditExists(ELco#:ELfnd:ELfyr:ELemp#:ELdiv:sysDt);
           //***    x1DueD = 'Past Due';
           //*** EndIf;
           //*** End   Add *** 03/20/24 ************************************
           //*** End   Del *** 09/12/24 ************************************
     C                   MOVE      X1DUED        X2DUED
     C*
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   MOVE      XXEMP#        ##EMP#            9
     C                   MOVE      XXDIV         ##DIV             5
     C                   MOVE      'W'           ##ODR             1
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
     C                   MOVE      ##EMN1        XXEMN1           50
     C                   MOVE      ##EMN2        XXEMN2           50
     C                   MOVE      ##EMN3        XXEMN3           50
     C                   MOVE      ##EMN4        XXEMN4           50
     C                   MOVE      ##EMN5        XXEMN5           50
     C                   MOVE      ##EMN6        XXEMN6           50
     C                   MOVE      ##EMN7        XXEMN7           50
     C*
     C                   MOVE      'TEST1'       QDTXT1           80
     C                   MOVE      'TEST2'       QDTXT2           80
     C                   MOVE      *BLANKS       QQTXT1           80
     C                   MOVE      *BLANKS       QQTXT2           80
     C                   MOVE      *BLANKS       QQTXT3           80
     C                   Z-ADD     0             Q#DAYS            3 0
     C*
     C     FNKEY         CHAIN     PEX01                              60
     C                   EXSR      SRAGHD
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     EXBTXT        QQTXT1
     C                   MOVE      EXBTXT        XQTEMP          160
     C                   MOVEL     XQTEMP        QQTXT2
     C                   MOVE      EXBTXT        QQTXT3
     C                   Z-ADD     EX#DYS        Q#DAYS
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C                   MOVE      *BLANK        XXPRT             1
     C                   MOVE      *BLANK        X2PRT             1
     C                   MOVE      *BLANK        X3PRT             1
     C                   MOVE      *BLANK        HDG
     C                   MOVE      *BLANK        DSH
     C                   MOVE      *BLANK        SGN
     C                   MOVE      *BLANK        TAM
     C                   MOVE      *BLANK        PRC
     C                   Z-ADD     0             X1TXDU
     C                   Z-ADD     0             QNETP
     C                   Z-ADD     0             XXMFDU
     C                   Z-ADD     0             XXSFDU
     C                   Z-ADD     0             XXECDU
     C                   Z-ADD     0             XXDEPA
     C                   Z-ADD     0             XXDPRP
     C                   Z-ADD     0             XXBAL
     C                   Z-ADD     0             XXTLDU
     C                   Z-ADD     0             AMT
     C                   Z-ADD     0             ZZ
     C                   Z-ADD     0             X1WKDT
     C*
     C                   Z-ADD     0             X1PRO
     C                   Z-ADD     0             X1DPRL
     C                   Z-ADD     0             X1DPDU
     C                   Z-ADD     0             X1MFEE
     C                   Z-ADD     0             X1SFEE
     C                   Z-ADD     0             X1INST
     C                   Z-ADD     0             X1EXCN
     C                   Z-ADD     0             X2TXDU
     C*
     C                   Z-ADD     0             X11STB
     C                   Z-ADD     0             X1196
     C                   Z-ADD     0             X12NDB
     C                   Z-ADD     0             X1296
     C                   Z-ADD     0             X13RDB
     C                   Z-ADD     0             X1396
     C                   Z-ADD     0             X14THB
     C                   Z-ADD     0             X1496
     C                   Z-ADD     0             X15THB
     C                   Z-ADD     0             X1596
     C                   Z-ADD     0             X16THB
     C                   Z-ADD     0             X1696
     C                   Z-ADD     0             X17THB
     C                   Z-ADD     0             X1796
     C                   MOVE      *BLANKS       X2SFHD
     C                   MOVE      *BLANKS       X11STH
     C                   MOVE      *BLANKS       X12NDH
     C                   MOVE      *BLANKS       X13RDH
     C                   MOVE      *BLANKS       X14THH
     C                   MOVE      *BLANKS       X15THH
     C                   MOVE      *BLANKS       X16THH
     C                   MOVE      *BLANKS       X17THH
     C                   MOVE      *BLANKS       X2THD
     C                   Z-ADD     0             EFD
     C                   Z-ADD     0             EXD
     C                   MOVEA     '0000000'     *IN(01)
     C                   MOVEA     '0000000'     *IN(08)
     C                   MOVEA     '0000000'     *IN(15)
     C                   MOVEA     '000'         *IN(22)
      /Free
       *in27 = *off;
      /End-free
     c*
     c* Print "Net Rate per 100"
     c*
     c                   if        xxnewf = 'Y'
     c
     c                   select
     c                   when      cltid = '025'
     c                   move      *on           *in27
     c                   endsl
     c
     c                   endif
     C*
     C*  JJJ 8-1-89: FLAG IF ANNIVERSARY RATING DATE IS IN EFFECT &
     C*              LOAD ANNIV OUTPUT DATES
     C*
     C                   Z-ADD     0             XXANDT
     C                   Z-ADD     0             XXFRAV
     C                   Z-ADD     0             XXTOAV
     C     ELANDT        COMP      0                                      88
     C  N88ELANDT        COMP      ELPFDT                                 88
     C  N88              Z-ADD     ELANDT        XXANDT
     C  N88              ADD       1             XXANYR
     C*
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXANNV            8 0
     C*
     C                   MOVE      '0'           *IN87
     C  N88              MOVE      '1'           *IN87
     C  N88XXANDT        IFGE      ELPTDT
     C     XXANDT        ORLE      ELPFDT
     C                   MOVE      '1'           *IN88
     C                   END
     C*
     C                   MOVE      '1'           *IN89
     C     *IN88         IFEQ      '0'
     C     ELEMD1        ANDEQ     ELANDT
     C     ELEMD2        ANDEQ     XXANDT
     C     ELEXM1        ANDNE     ELEXM2
     C                   MOVE      '0'           *IN89
     C                   END
     C  N88              Z-ADD     XXANDT        SVANDT
     C*
     C*  GET NUMBER OF FUND POLICY DAYS
     C*
     C                   Z-ADD     0             XXLPDT
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C*
     C                   Z-ADD     F1ENPO        SRDTI
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
     C                   EXSR      SRLEAP
     C*
     C                   Z-ADD     QQDAYS        FDAYS             5 0
     C*
     C*  GET NUMBER OF EMPLOYER POLICY DAYS
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   EXSR      SRLEAP
     C*
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C*
     C*  CLEAR/LOAD EFFECTIVE DATE/RATE ARRAYS
     C*
     C                   Z-ADD     9999999       PDAT
     C                   Z-ADD     9999999       RDAT
     C                   Z-ADD     *ZERO         DYS
     C                   Z-ADD     *ZERO         DY3
     C*
     C                   Z-ADD     ELPFDT        PDAT(1)
     C  N87              Z-ADD     ELPFDT        RDAT(1)
     C                   If        xspfdt <> 0
     C                   Z-add     xspfdt        rdat(1)
     C                   Endif
     C   87              Z-ADD     ELANDT        RDAT(1)
     C                   Z-ADD     2             X                 3 0
     C     *IN88         IFEQ      '0'
     C*
     C                   Z-ADD     XXANDT        PDAT(X)
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ADD       1             X
     C*
     C                   END                                                      *IN88='0'
     C                   MOVE      *OFF          *IN96
     C                   MOVE      *BLANK        XXOCT             1
     C*
     C* CHECK IF ANY POLICY PERIODS NEED TO BE SPLIT FOR 4/1/3 RATES.
     C*
     C     XXST          IFEQ      'FL'
     C*
     C                   DO        8             W                 2 0
     C     W             ADD       1             Z
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       IFGE      1030101
     C     RDAT(W)       ANDLE     1030331
     C     PDAT(W)       ANDLT     1030401
     C     PDAT(Z)       ANDGT     1030401
     C*
     C                   Z-ADD     1030401       PDAT(X)
     C                   Z-ADD     1030401       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           *IN96
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C     RDAT(W)       IFLT      1031001
     C     PDAT(W)       ANDLT     1031001
     C     PDAT(Z)       ANDGT     1031001
     C                   Z-ADD     1031001       PDAT(X)
     C                   Z-ADD     1031001       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      'Y'           XXOCT
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 4 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* DE RATE SPLIT FOR 10/01/08
     C*
     C                   MOVE      *OFF          XXDEFL            1
     C     XXST          IFEQ      'DE'
     C*DE rate split flag
     C     EXDE08        ANDNE     'Y'
     C*
     C                   DO        8             W                 2 0
     C     W             ADD       1             Z
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C     RDAT(W)       IFLT      1081001
     C     PDAT(W)       ANDLT     1081001
     C     PDAT(Z)       ANDGT     1081001
     C                   Z-ADD     1081001       PDAT(X)
     C                   Z-ADD     1081001       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XXDEFL
     C                   ENDIF                                                  RDAT,W IFGE
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 8 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* FL RATE SPLIT FOR 07/01/09
     C*
     C                   MOVE      *OFF          XX0709            1
     C     XXST          IFEQ      'FL'
     C     CLTID         ANDEQ     '139'
     C*
     C                   DO        8             W                 2 0
     C     W             ADD       1             Z
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C     RDAT(W)       IFGE      1090401
     C     RDAT(W)       ANDLT     1090701
     C     PDAT(W)       ANDLT     1090701
     C     PDAT(Z)       ANDGT     1090701
     C                   Z-ADD     1090701       PDAT(X)
     C                   Z-ADD     1090701       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XX0709
     C                   ENDIF                                                  RDAT,W IFGE
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 8 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* AZ RATE SPLIT FOR 01/01/2010
     C*
     C                   MOVE      *OFF          XXAZ10            1
     C     XXST          IFEQ      'AZ'
     C*
     C                   DO        8             W                 2 0
     C     W             ADD       1             Z
     C*
     C     RDAT(W)       IFNE      *HIVAL
     c                   if        rdat(w) >= 1091001 and
     c                             rdat(w) <= 1091231 and
     c                             pdat(w) <  1100101 and
     c                             pdat(z) >  1100101
     C                   Z-ADD     1100101       PDAT(X)
     C                   Z-ADD     1100101       RDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XXAZ10
     C                   ENDIF                                                  RDAT,W IFGE
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 8 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C* OK RATE SPLIT FOR 09/01/2010
     C*
     C                   move      *off          xxOK10            1
     C                   if        xxst = 'OK'
     C* Check opt out flag for OK 1100901 rate change
     C                   z-add     1100901       xxSpltDt          7 0
     C                   if        ApplyRateSplit(xxst:xxSpltDt)
     C                   do        8             W                 2 0
     C                   eval      z = w + 1
     C*
     C                   if        rdat(w) <> *hival  and
     C                             rdat(w) < xxSpltDt and
     C                             pdat(w) < xxSpltDt and
     C                             pdat(z) > xxSpltDt
     C                   Z-ADD     xxSpltDt      pdat(x)
     C                   Z-ADD     xxSpltDt      rdat(x)
     C                   ADD       1             X
     C                   eval      xxOK10 = *on
     C                   endif                                                  RDAT,W IFGE
     C*
     C                   enddo                                                  DO 8 W
     C                   endif                                                  ApplyRateSplit
     C*
     C                   endif                                                  xxst
      * TN Rate Split FOR 07/01/2014
     C                   move      *off          isTN14
     C                   if        xxst = 'TN'
     C                   z-add     1140701       xxSpltDt          7 0          mld
     C                   if        ApplyRateSplit(xxst:xxSpltDt)
     C                   do        8             w                 2 0
     C                   eval      z = w + 1
     C                   if        pdat(w) < xxSpltDt and
     C                             pdat(z) > xxSpltDt
     C                   eval      isTN14 = *on
     C                   eval      pdat(x) = xxSpltDt
     C                   eval      rdat(x) = pdat(x)
     C                   eval      x = x + 1
     C                   leave
     C                   endif                                                  pdat(W)
     C                   enddo                                                  DO 8 X
     C                   endif                                                  ApplyRateSplit
     C                   endif                                                  xxst
      * OK Rate Split FOR 02/01/2014
     C                   move      *off          isOK14
     C                   if        xxst = 'OK'
     C                   z-add     1140201       xxSpltDt          7 0
     C                   if        ApplyRateSplit(xxst:xxSpltDt)
     C                   do        8             w                 2 0
     C                   eval      z = w + 1
     C                   if        pdat(w) < 1140101  and
     C                             pdat(z) > xxSpltDt
     C                   eval      isOK14 = *on
     C                   eval      pdat(x) = xxSpltDt
     C                   eval      rdat(x) = pdat(x)
     C                   eval      x = x + 1
     C                   leave
     C                   endif                                                  pdat(W)
     C                   enddo                                                  DO 8 X
     C                   endif                                                  ApplyRateSplit
     C                   endif                                                  xxst

     C                   Z-ADD     0             XXRDT1
     C                   Z-ADD     0             XXRDT2
     C     XXOCT         IFEQ      'Y'
     C                   Z-ADD     1031001       XXRDT2            7 0
     C                   ENDIF
     C     *IN96         IFEQ      *ON
     C                   Z-ADD     1030401       XXRDT1            7 0
     C                   ENDIF
     C     XXDEFL        IFEQ      *ON
     C                   Z-ADD     1081001       XXRDT1            7 0
     C                   ENDIF
     C     XX0709        IFEQ      *ON
     C                   Z-ADD     1090701       XXRDT1            7 0
     C                   ENDIF
     C     XXAZ10        IFEQ      *ON
     C                   Z-ADD     1100101       XXRDT1            7 0
     C                   ENDIF
     C*   OK RATE SPLIT FOR 09/01/2010
     C                   if        xxOK10 = *on
     C                   Z-ADD     xxSpltDt      XXRDT1            7 0          SPLIT DATE
     C                   endif                                                  XXOK10 = *ON
      /free
       If isTN14 or isOK14;
          xxRdt1 = xxSpltDt;
       Endif;
      /end-free
     C*
     C*  PLACE THE EXPERIENCE MOD DATES INTO PDAT IF F1SPLA = 'Y'.
     C*  IF ANNIVERSARY RATED, LOAD THE PROPER RATING DATE.
     C*
     C     F1SPLA        IFEQ      'Y'
     C*
     C     ELEMD2        IFNE      *ZERO
     C     ELEMD2        ANDGT     ELPFDT
     C     ELEMD2        ANDNE     XXANDT
     C     ELEMD2        ANDLT     ELPTDT
     C     ELEMD2        ANDNE     XXRDT1
     C     ELEMD2        ANDNE     XXRDT2
     C                   Z-ADD     ELEMD2        PDAT(X)
     C*
     C     *IN87         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        RDAT(X)
     C                   ELSE
     C     ELEMD2        IFLT      XXANDT
     C                   Z-ADD     ELANDT        RDAT(X)
     C                   ELSE
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ENDIF
     C                   ENDIF
     C     *IN96         IFEQ      *ON
     C     ELEMD2        ANDGE     1030401
     C     RDAT(X)       ANDLT     1030401
     C                   Z-ADD     1030401       RDAT(X)
     C                   ENDIF
     C*
     C     XXOCT         IFEQ      'Y'
     C     ELEMD2        ANDGE     1031001
     C     RDAT(X)       ANDLT     1031001
     C                   Z-ADD     1031001       RDAT(X)
     C                   ENDIF
     C     XXDEFL        IFEQ      *ON
     C     ELEMD2        ANDGE     1081001
     C     RDAT(X)       ANDLT     1081001
     C                   Z-ADD     1081001       RDAT(X)
     C                   ENDIF
     C     XX0709        IFEQ      *ON
     C     ELEMD2        ANDGE     1090701
     C     RDAT(X)       ANDLT     1090701
     C                   Z-ADD     1090701       RDAT(X)
     C                   ENDIF
     C     XXAZ10        IFEQ      *ON
     C     ELEMD2        ANDGE     1100101
     C     RDAT(X)       ANDLT     1100101
     C                   Z-ADD     1100101       RDAT(X)
     C                   ENDIF
     C                   IF        XXOK10 = *ON  AND
     C                             ELEMD2 >= xxSpltDt AND
     C                             RDAT(X) < xxSpltDt
     C                   Z-ADD     xxSpltDt      RDAT(X)
     C                   ENDIF
      /free
       If ELemd2 >= xxSpltDt and rDat(x) < xxSpltDt and (isOK14 or isTN14);
          rDat(x) = xxSpltDt;
       Endif;
      /end-free
     C*
     C                   ADD       1             X
     C                   ENDIF
     C*
     C     ELEMD3        IFNE      *ZERO
     C     ELEMD3        ANDGT     ELPFDT
     C     ELEMD3        ANDNE     XXANDT
     C     ELEMD3        ANDLT     ELPTDT
     C     ELEMD3        ANDNE     XXRDT1
     C     ELEMD3        ANDNE     XXRDT2
     C                   Z-ADD     ELEMD3        PDAT(X)
     C*
     C     *IN87         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        RDAT(X)
     C                   ELSE
     C     ELEMD3        IFLT      XXANDT
     C                   Z-ADD     ELANDT        RDAT(X)
     C                   ELSE
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ENDIF
     C                   ENDIF
     C     *IN96         IFEQ      *ON
     C     ELEMD3        ANDGE     1030401
     C     RDAT(X)       ANDLT     1030401
     C                   Z-ADD     1030401       RDAT(X)
     C                   ENDIF
     C*
     C     XXOCT         IFEQ      'Y'
     C     ELEMD3        ANDGE     1031001
     C     RDAT(X)       ANDLT     1031001
     C                   Z-ADD     1031001       RDAT(X)
     C                   ENDIF
     C     XXDEFL        IFEQ      *ON
     C     ELEMD3        ANDGE     1081001
     C     RDAT(X)       ANDLT     1081001
     C                   Z-ADD     1081001       RDAT(X)
     C                   ENDIF
     C     XX0709        IFEQ      *ON
     C     ELEMD3        ANDGE     1090701
     C     RDAT(X)       ANDLT     1090701
     C                   Z-ADD     1090701       RDAT(X)
     C                   ENDIF
     C     XXAZ10        IFEQ      *ON
     C     ELEMD3        ANDGE     1100101
     C     RDAT(X)       ANDLT     1100101
     C                   Z-ADD     1100101       RDAT(X)
     C                   ENDIF
     C                   IF        XXOK10 = *ON  AND
     C                             ELEMD3 >= xxSpltDt AND
     C                             RDAT(X) < xxSpltDt
     C                   Z-ADD     xxSpltDt      RDAT(X)
     C                   ENDIF
      /free
       If ELemd3 >= xxSpltDt and rDat(x) < xxSpltDt and (isTN14 or isOK14);
          rDat(x) = xxSpltDt;
       Endif;
      /end-free
     C*
     C                   ADD       1             X
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     ELPTDT        PDAT(X)
     C*
     C                   SORTA     PDAT
     C                   SORTA     RDAT
     C*
     C                   DO        8             X
     C*
     C     PDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         PDAT(X)
     C                   ENDIF
     C     RDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         RDAT(X)
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  GET ACTUAL NUMBER OF DAYS IN EACH ANNIV RATING PERIOD
     C*
     C                   Z-ADD     1             X
     C*
     C     RDAT(X)       DOWNE     0
     C     X             ADD       1             Z
     C*
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   EXSR      SRLEAP
     C*
     C                   Z-ADD     QQDAYS        DYS(X)
     C*
     C                   ADD       1             X
     C                   END
     C*
     C                   Z-ADD     0             PDAYS3            5 0
     C*
     C*  CALCULATE NUMBER OF DAYS 1993 RATES WERE USED.  THE NUMBER
     C*  OF DAYS WILL BE USED IF THE 15.7% 93 RATE CREDIT IS TO BE
     C*  APPLIED.  ONLY DO THIS FOR FLORIDA FUNDS.
     C*
     C                   MOVE      'N'           XX157             1
     C*
     C     FNMST         IFEQ      'FL'
     C*
     C     ELPTDT        IFGT      0940101
     C                   Z-ADD     1             X
     C*
     C     RDAT(X)       DOWNE     *ZERO
     C     X             ADD       1             Z
     C*
     C     RDAT(X)       IFGE      0930101
     C     RDAT(X)       ANDLE     0931231
     C     PDAT(Z)       ANDGT     0931231
     C                   MOVE      'Y'           XX157
     C                   ADD       DYS(X)        PDAYS3
     C                   Z-ADD     DYS(X)        DY3(X)
     C*
     C*  IF PART OF THE PERIOD WAS IN 1993, BACK THOSE DAYS OUT OF PDAYS3
     C*
     C     PDAT(X)       IFLT      0940101
     C*
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     01011994      XQTO
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   SUB       QQDAYS        PDAYS3
     C                   SUB       QQDAYS        DY3(X)
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  FNMST = FL
     C*
     C     XXDIV         IFEQ      XSDIV
     C*
     C                   MOVEL     QQPOL#        SXPOL#           15
     C                   MOVEL     XXFROM        SXFROM            8
     C                   MOVEL     XXTO          SXTO              8
     C                   MOVEL     XXANNV        SXANNV            8
     C                   MOVE      *IN87         SXIN87            1
     C                   MOVEL     EMN(1)        SXEMN1           37
     C                   MOVEL     EMN(2)        SXEMN2           37
     C                   MOVEL     EMN(3)        SXEMN3           37
     C                   MOVEL     EMN(4)        SXEMN4           37
     C                   MOVEL     EMN(5)        SXEMN5           37
     C                   MOVEL     AHAGT#        SXAGT#            5
     C                   MOVEL     ANAME         SXNAME           34
     C                   MOVEL     AADD1         SXADD1           34
     C                   MOVEL     ACSZ          SXACSZ           34
     C                   MOVE      *IN69         SXIN69            1
     C                   MOVEL     AGARE1        SXARE1            3
     C                   MOVEL     AGPHN1        SXPHN1            7
     C*
     C                   ENDIF                                                  XXDIV IFEQ
     C                   CLEAR                   XTADLD           11 2
     C                   CLEAR                   XTOCCD           11 2
     C                   CLEAR                   XTNRAT           11 2
     C                   MOVE      *BLANKS       XXOCCD           30
     C                   MOVE      *ZERO         TPREM            11 2
     C                   MOVE      *ZERO         MPREM            11 2
     C                   MOVE      *BLANK        XXRATE            1
     C                   MOVE      *BLANK        X1RATE            1
     C                   MOVE      '0'           *IN48
      /free
       vol  = *blanks;
       vol# = 0;
       volA = 0;
       v = 0;
      /end-free
     C*
     C     RREMLB        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN48
     C                   ENDIF
     C*
     C     ELILCD        IFNE      *BLANKS
     C                   MOVE      '1'           *IN48
     C                   ENDIF                                                  ELILCD IFNE
     C                   MOVEL(P)  RREMLB        XXEMLB           30
     C                   MOVE      *BLANKS       XQANNH
     C                   MOVE      *BLANKS       XQANND
     C*
     C     *IN87         IFEQ      *ON
     C     XXNEWF        ANDEQ     'Y'
     C                   MOVEL(P)  'Annivers'    XQANNH           20
     C                   CAT       'ary Da':0    XQANNH
     C                   CAT       'te:':0       XQANNH
     C                   Z-ADD     ELANDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQANND           20
     C                   CAT       XXDAY:1       XQANND
     C                   CAT       ',':0         XQANND
     C                   CAT       XXYR:1        XQANND
     C                   ENDIF                                                  *IN87 IFEQ
     C*** begin add ***  10/25/25  *****************************************
     C                   Clear                   noPayrollBill     1
       Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pep07;
       If not %Found(wmepl7);
          noPayrollBill = 'Y';
       Endif;
     C*** end add *****  10/25/25  *****************************************
       //*** Begin Add *** 09/26/23 ****************************************
       //*** Begin Add *** 03/12/24 ****************************************
       // Check to see if division has payrolls
       hasPayrolls = HasDivPayrolls(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr);
       //*** End   Add *** 03/12/24 ****************************************
       x#RRN = dbRRN;
       //*** Begin Chg *** 03/12/24 ****************************************
       //*** If isUseXml;
       If isUseXml and hasPayrolls;
       //*** End   Chg *** 03/12/24 ****************************************
          Exsr ScriptXML;
       Endif;
       //*** End   Add *** 09/26/23 ****************************************
     C*
     C                   Z-ADD     *ZERO         FILL2             3 0
     C     EPKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C     EPSTAT        IFNE      'I'
     C                   ADD       1             FILL2
      /free
       If CltId = '111';
          Exsr GetVolCnt;
       Endif;
      /end-free
     C                   ENDIF
     C     EPKEY         READE     PEP01                                  60
     C                   ENDDO
     C*
      /free
       // Add Volunteered Count
       If v > 0 and CltId = '111';
          Fill2 = Fill2 + %div(v:2) + 2;
       Endif;
      /end-free

     C* CALCULATE TOTAL # OF PAGES FOR BILL.
     C*
     C                   SUB       4             FILL2
     C     FILL2         IFLE      0
     C                   Z-ADD     1             FILL2
     C                   ELSE
     C*
     C     FILL2         DIV       21            FILLR             6 3
     C                   MOVE      FILLR         FILLRC            3 3
     C                   Z-ADD     FILLR         FILL2
     C                   ADD       1             FILL2
     C     FILLRC        IFNE      0
     C                   ADD       1             FILL2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     FILL2         XXPAGC            3 0
     C*
     C*  READ EMPLOYER PAYROLL RECORDS
     C*
     C     EPKEY         SETLL     PEP01
     C     READPR        TAG
     C                   SETOFF                                       5054
     C                   SETON                                        51
     C                   Z-ADD     *ZERO         FILL
     C     *IN50         DOUEQ     '1'
     C     EPKEY         READE     PEP01                                  50
     c                   if        *in50 = *on and xtpred <> 0
     c                   z-add     1110901       xxSpltDt
     c                   eval      xxstcd = 0
     c                   if        xxst = 'IL' and
     c                             elpfdt < xxSpltDt and
     c                             elptdt > xxSpltDt and
     c                             ApplyRateSplit(xxst:xxSpltDt:xxstcd)
     c                   exsr      srFlatRateAdj
     c                   endif
     c                   endif                                                  *in50 = *on
     c                   if        *in50 = *on and xtpredNY <> 0
     c                   z-add     1170410       xxSpltDt
     c                   eval      xxstcd = 0
     c                   if        xxst = 'NY' and
     c                             elpfdt >= 1161001 and
     c                             elpfdt <= 1170930 and
     c                             ApplyRateSplit(xxst:xxSpltDt:xxstcd)
     c                   exsr      FlatRateAdjNY
     c                   endif
     c                   endif                                                  *in50 = *on
     C     *IN50         IFEQ      '0'
     C     EPSTAT        ANDNE     'I'
     C     EPSAFL        ANDEQ     'A'
     C     *IN50         OREQ      '1'
     C     xxst          ANDEQ     'IL'
     C     xxFlrtAdj     ANDNE     0
     C     *IN50         OREQ      '1'
     C     xxst          ANDEQ     'NY'
     C     xxFlrtAdjNY   ANDNE     0
     C     ELAMNP        ANDNE     'Y'
     C*
     C                   ADD       2             FILL              2 0
     C     XXNEWF        IFEQ      'Y'
     C     FILL          CABGT     42            FULLT                54
     C                   ELSE
     C*
     C     XXPAG4        IFEQ      *ZERO
     C     FILL          CABGT     32            FULLT                54
     C                   ELSE
     C     FILL          CABGT     44            FULLT                54
     C                   ENDIF
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C     XXNEWF        IFEQ      'Y'
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1FRAV
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1TOAV
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C*  DISPLAY PAYROLL PERIOD START AND END DATES FOR ANNIVERSARY
     C*  RATED OR SPLIT MOD ACCOUNTS.
     C*
     C                   MOVE      *OFF          *IN33
     C*
     C     PDAT(3)       IFNE      *ZERO
     C                   MOVE      *ON           *IN33
     C                   Z-ADD     1             Z
     C     EPEFDT        LOOKUP    PDAT(Z)                            60
     C*
     C     *IN60         IFEQ      '1'
     C     Z             ANDGT     1
     C                   SUB       1             Z
     C                   ELSE
     C                   Z-ADD     1             Z
     C                   END
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRAV            8 0
     C*
     C                   ADD       1             Z
     C*
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTOAV            8 0
     C                   Z-add     xxfrav        x1frav
     C                   Z-add     xxtoav        x1toav
     C*
     C                   ENDIF
     C*
     C                   ADD       EPAMPR        TPREM
     C*
     C                   Z-ADD     0             XXADLD
     C                   Z-ADD     EPAAPR        XXEAPR           11 2
     C                   Z-ADD     EPAMPR        XXEMPR            9 0
     C*  GET PAYROLL CLASS FILE
     C*
     C                   Z-ADD     EPCLAS        XXCLAS
     C     PDAT(3)       IFNE      *ZERO
     C     EPEFDT        ANDEQ     ELPFDT
     C     ELANDT        ANDNE     *ZERO
     C                   Z-ADD     ELANDT        XXEFDT
     C                   ELSE
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   ENDIF
     C                   MOVE      *OFF          *IN97
     C     *IN96         IFEQ      *ON
     C     XXOCT         OREQ      'Y'
     C     XXDEFL        OREQ      *ON
     C     XX0709        OREQ      *ON
     C     XXAZ10        OREQ      *ON
     C     XXOK10        OREQ      *ON
     C     isTN14        OREQ      *ON
     C     isOK14        OREQ      *ON
     C*
     C     EPEFDT        IFEQ      1031001
     C     XXOCT         ANDEQ     'Y'
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   ENDIF
     C                   MOVE      *ON           *IN97
     C     EPEFDT        IFEQ      1030401
     C     *IN96         ANDEQ     *ON
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   ENDIF
     C     EPEFDT        IFEQ      1081001
     C     XXDEFL        ANDEQ     *ON
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   ENDIF
     C     EPEFDT        IFEQ      1090701
     C     XX0709        ANDEQ     *ON
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   ENDIF
     C     EPEFDT        IFEQ      1100101
     C     XXAZ10        ANDEQ     *ON
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   ENDIF
     C     EPEFDT        IFEQ      xxSpltDt
     C     XXOK10        ANDEQ     *ON
     C                   Z-ADD     EPEFDT        XXEFDT
     C                   ENDIF
      /free
       If  EPefdt = xxSpltDt and (isTN14 = *on or isOK14 = *on);
          xxEfdt = EPefdt;
       Endif;
      /end-free
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRDT            8 0
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    PDAT(X)                            60
     C*
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTODT            8 0
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN96 IFEQ
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DAT1            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   XXCLAS        ##CLAS            5
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
     C                   MOVE      ##DAT1        X1EFDT
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCO#         COMP      XXCO#                              6060
     C  N60PRCLAS        COMP      XXCLAS                             6060
     C                   END
     C   60              MOVE      *BLANKS       PRDESC
     C     EPEXT         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXST
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DAT1            7
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
     C                   MOVE      ##DAT1        X1EFDT
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        PRDESC
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DAT1            7
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
     C                   MOVE      ##DAT1        X1EFDT
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        PRDESC
     C                   ENDIF                                                  EPEXT IFNE
     C                   MOVE      '0'           *IN62
     C                   MOVE      '0'           *IN70
     C     *IN60         IFEQ      '0'
     C     EPADLD        ANDNE     0
      *** Beg Chg ***** 04/09/24 ***********************************
     C***                Z-ADD     EPADLD        XXADLD            7 2
     C                   Z-ADD     EPADLD        XXADLD            9 2
      *** End Chg ***** 04/09/24 ***********************************
     C*     PROCCD        IFEQ      'Y'
     C*                   ADD       EPADLD        XTOCCD           11 2
     C*                   Z-ADD     PRRT          EPARAT
     C*                   MOVEL(P)  PRDESC        XXOCCD
     C*                   MOVE      '1'           *IN70
     C*                   ELSE
     C*     PXCODE        IFNE      *ZERO
     C*     EPEXT         ANDNE     *BLANKS
     C*                   MOVE      '1'           *IN32
     C*                   ADD       EPADLD        TPREM
     C*                   ELSE
     C*                   MOVE      '0'           *IN32
     C*                   ADD       EPADLD        XTADLD           11 2
     C*                   ENDIF                                                  PXCODE IFNE
     C*                   ENDIF                                                  EPCLAS IFEQ
      /Free
       Select;
          When PRoccd = 'Y';
             XToccd = Xtoccd + EPadld;
             EParat = PRrt;
             xxOccd = PRdesc;
             *in70 = *on;
          When PRnrat = 'Y';
             XTnrat = Xtnrat + EPadld;
             EParat = PRrt;
             xxOccd = PRdesc;
             *in70 = *on;
          When PXcode <> *Zero and EPext <> *Blanks;
             tPrem = tPrem + EPadld;
             *In32 = *on;
          Other;
             *In32 = *off;
             XTadld = XTadld + EPadld;
       Endsl;
      /End-Free
     C                   MOVE      '1'           *IN62
     C                   END
     C     EPCLAS        IFEQ      RRDCCD
     C                   Z-ADD     PRRT          EPARAT
     C                   ENDIF                                                  EPCLAS EQ
     C                   MOVE      RSJRST        XXST
     C                   if         *in50 = *on
     C                   MOVEL(P)  EPDESC        PRDESC
     C                   EVAL      EPARAT = 0
     C                   EVAL      XXEAPR = 0
     C                   endif
     C*
     C*  OUTPUT DETAIL LINE ITEM
     C*
     C                   MOVE      '1'           XXPRT             1
     C                   MOVE      '1'           X2PRT             1
     C                   MOVE      '1'           X3PRT             1
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C   OF              EXCEPT    HEAD
     C   OF              EXCEPT    HEAD3
     C                   SETOFF                                       OF
     C                   ENDIF
     C*
     C     *INOA         IFEQ      *ON
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    HEADT
     C                   EXCEPT    HEAD3T
     C                   ENDIF
     C                   SETOFF                                       OA
     C                   ENDIF
     C     XXNEWF        IFEQ      'Y'
     C     XX1ST         ANDEQ     *ON
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   WRITE     QHEAD
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QHD171
     C                   WRITE     QLN171
     C                   WRITE     QHD271
     C                   WRITE     QHD371
     C                   OTHER
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   WRITE     QHEAD2
     C                   WRITE     QHEAD3
     C                   ENDSL
     C                   WRITE     QBOX1
     C                   MOVE      *OFF          XX1ST
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C*  APPLY 15.7% CREDIT TO PORTION OF MANUAL USING 1993 RATES;
     C*  GET CORRECT DATE FROM RDAT ARRAY TO DETERMINE RATING DATE USED
     C*  TO PULL RATE FROM FILE
     C*
     C                   Z-ADD     *ZEROS        XXDISC
     C                   Z-ADD     EPAMPR        XXAMPR
     C                   MOVE      '0'           *IN94
     C*
     C*  15.7% DISCOUNT IS APPLIED TO PORTION OF 94 POLICY USING
     C*  93 RATES
     C*
     C     XX157         IFEQ      'Y'
     C                   Z-ADD     1             Z
     C     EPEFDT        LOOKUP    PDAT(Z)                            60
     C*
     C     *IN60         IFEQ      '1'
     C     Z             ANDGT     1
     C                   SUB       1             Z
     C                   ELSE
     C                   Z-ADD     1             Z
     C                   END
     C*
     C     DY3(Z)        IFGT      *ZERO
     C                   ADD       1             FILL
     C                   MOVE      *ON           *IN94
     C*
     C*  APPLY 15.7 % CREDIT TO 1994 PORTION OF THE PAYROLL PERIOD
     C*
     C     DYS(Z)        IFNE      *ZEROS
     C     DY3(Z)        DIV(H)    DYS(Z)        X2WK54            5 4
     C                   ELSE
     C                   Z-ADD     *ZEROS        X2WK54
     C                   END
     C*
     C*  HARBOR EXPOSURE CLASSES WILL ONLY RECEIVE UP TO 3.2 %
     C*  CREDIT ON UNEXPIRED PORTION OF POLICY
     C*
     C     EPSPEC        IFEQ      'F'
     C*
     C*  FIND THE AVERAGE # OF MONTHS 93 RATES WERE APPLIED TO
     C*  DETERMINE WHAT PERCENTAGE WILL BE APPLIED
     C*
     C     PDAYS3        DIV(H)    30            XXMTHS            3 1
     C                   Z-ADD     1             T                 3 0
     C     XXMTHS        LOOKUP    MTH(T)                             60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     HP(T)         XXWO54            5 4
     C                   ELSE
     C                   Z-ADD     *ZEROS        XXWO54
     C                   END                                                    *IN60
     C                   ELSE
     C     X2WK54        MULT(H)   0.157         XXWO54
     C                   END                                                    HARBOR EXP
     C*
     C     PRPCF         IFEQ      'P'
     C     EPARAT        DIV       100           XXWH54            5 4
     C     EPAAPR        MULT      XXWH54        XXAMPR
     C                   ELSE
     C     EPAAPR        MULT      EPARAT        XXAMPR
     C                   END
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD     XXAMPR        XXSAVE           11 2
     C     XXAMPR        IFLT      *ZERO
     C                   MULT      -1            XXAMPR
     C                   END
     C                   MOVE      XXAMPR        XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXAMPR
     C                   END
     C                   MOVE      '00'          XXAMPR
     C     XXSAVE        IFLT      *ZERO
     C     XXAMPR        MULT      -1            XXAMPR
     C                   END
     C*
     C     XXAMPR        MULT(H)   XXWO54        XXDISC           11 2
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD     XXAMPR        XXSAVE           11 2
     C     XXAMPR        IFLT      *ZERO
     C                   MULT      -1            XXAMPR
     C                   END
     C                   MOVE      XXAMPR        XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXAMPR
     C                   END
     C                   MOVE      '00'          XXAMPR
     C     XXSAVE        IFLT      *ZERO
     C     XXAMPR        MULT      -1            XXAMPR
     C                   END
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD     XXDISC        XXSAVE           11 2
     C     XXDISC        IFLT      *ZERO
     C                   MULT      -1            XXDISC
     C                   END
     C                   MOVE      XXDISC        XX                2 0
     C     XX            IFGE      50
     C                   ADD       1             XXDISC
     C                   END
     C                   MOVE      '00'          XXDISC
     C     XXSAVE        IFLT      *ZERO
     C     XXDISC        MULT      -1            XXDISC
     C                   END
     C                   END                                                    DY3,X <> 0
     C*
     C                   END                                                    XX157 = 'Y'
     C*
     C*  DON'T PRINT RATE CREDIT IF IT IS ZERO
     C*
     C     XXDISC        IFEQ      *ZEROS
     C                   MOVE      '0'           *IN94
     C                   END
     C     *IN90         IFEQ      *ON
     C     PPKY1         SETGT     PPP01
     C     PPKY          READPE    PPP01                                  60
     C     *IN60         IFEQ      *OFF
     C     PPAMPR        ANDGT     *ZERO
     C     PPANMP        DIV       PPAMPR        XQPROR
     C                   ELSE
     C                   Z-ADD     XZPROR        XQPROR
     C                   ENDIF
     C     EPANTR        IFNE      *ZERO
     C                   Z-ADD     EPANTR        XQARAT            9 4
     C                   Z-ADD     EPANTP        XXEMPR
     C                   ELSE
     C     EPARAT        MULT(H)   XQPROR        XQARAT            9 4
     C                   MULT(H)   XQPROR        XXEMPR
     C                   ENDIF
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   IF        XXEMPR < 0
     C                   MOVE      '-'           XXEMSN
     C                   ELSE
     C                   MOVE      ' '           XXEMSN
     C                   ENDIF
      /free
       If  *in50 = *on and xxFlrtAdjNY <> 0;
          If xxFlrtAdjNY > 0;
             xxEmsn = '-';
          Else;
             xxEmsn = ' ';
          Endif;
          If ELAMNP = 'Y';
             xxEmpr = 0;
             xxEmsn = ' ';
          Endif;
       Endif;
      /end-free
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C     XXNEWF        IFEQ      *BLANK
     C                   EXCEPT    DETT
     C                   ELSE
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QDET71
     C                   OTHER
     C                   WRITE     QDET
     C                   ENDSL
     C                   ENDIF                                                  XXNEWF IFEQ
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XPEFDT
     C*
       // Write to Payroll work file (WXBLP1)
       If isUseXml;
          Exsr ScriptPayrolls;
       EndIf;
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C     *IN33         IFEQ      *OFF
     C     *IN62         ANDEQ     *OFF
     C     *IN94         ANDEQ     *ON
     C     *IN97         ANDEQ     '0'
     C                   SUB       1             FILL
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C                   EXCEPT    DET2
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C                   EXCEPT    DET2T
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C                   ELSE
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C                   EXCEPT    DET1
     C                   EXCEPT    DET2
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C                   EXCEPT    DET1T
     C                   EXCEPT    DET2T
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C                   END
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C*  WRITE RECORDS TO THE SUMMARY PAYROLL FILE
     C*
     C     ELARSM        IFEQ      'Y'
     C     TURN          ANDEQ     'E'
     C*
     C                   Z-ADD     EPDIV         EPDISC
     C                   MOVE      XXST          EPPST
     C                   Z-ADD     XXEAPR        EPEAPR
     C                   Z-ADD     XXEMPR        EPEMPR
     C                   Z-ADD     XXADLD        EPADLD
     C                   MOVEL(P)  PRDESC        EPDESC
     C                   WRITE     XEP01
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     c                   if        xxst = 'IL' and *in50 = *off
     c                   eval      xtpred = xtpred + epampr
     c                   endif
     c                   if        xxst = 'NY' and *in50 = *off
     c                   eval      xtpredNY = xtpredNY + epampr
     c                   endif
     C*
     C*
     C                   ENDIF                                                  *IN50 IFEQ
     C*
     C                   ENDDO                                                  *IN50 DOUEQ
      /free
       If CltId = '111';
          Exsr PrtVolList;
       Endif;
      /end-free
     C*
     C                   Z-ADD     TPREM         SPREM            11 2
     C                   MOVE      '00000'       XXPROR
     C                   Z-ADD     0             FACTY
     C*
     C* CALCULATE SHORT RATED PREMIUM
     C*
     C     XXSRPR        IFEQ      'Y'
     C                   Z-ADD     0             XXENDT            7 0
     C*
     C     FNCGFL        IFNE      'Y'
     C*
     C* USE FYR END DATE FOR NON-COMMERCIAL
     C*
     C                   Z-ADD     F1ENPO        XXENDT
     C*
     C                   ELSE
     C*
     C*  CHECK EMPLOYER HISTORY TO FIND THE PRE-CANCEL ELPTDT FOR COMM.
     C*
     C     EHKY          SETGT     PEH01
     C     EHKY          READPE    PEH01                                  60
     C     *IN60         DOWEQ     *OFF
     C     EHFAF         ANDNE     'R'
     C     EHFAF         ANDNE     'E'
     C     EHKY          READPE    PEH01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     EHPTDT        XXENDT
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  FNCGFL IFNE
     C*
     C                   Z-ADD     0             WDAYS             5 0
     C*
     C* IF NO ENDING DATE, DEFAULT TO ONE YEAR FROM EFF DATE.
     C*
     C     XXENDT        IFEQ      *ZERO
     C*
     C                   Z-ADD     ELPFDT        XXENPO
     C                   ADD       1             XXENYR
     C                   Z-ADD     XXENPO        XXENDT
     C*
     C                   ENDIF                                                  XXENDT IFEQ
     C*
     C     XXENDT        IFNE      *ZERO
     C*
     C*  GET NUMBER OF DAYS POLICY WAS ORIGINALLY WRITTEN FOR IN
     C*  ORDER TO CALCULATE EXTENDED DAYS FOR SHORT RATING
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     XXENDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   EXSR      SRLEAP
     C*
     C                   Z-ADD     QQDAYS        WDAYS             5 0
     C*
     C                   ENDIF                                                  XXENDT IFNE
     C*
     C*  GET NUMBER OF EMPLOYER POLICY DAYS
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*
     C                   EXSR      SRLEAP
     C*
     C                   Z-ADD     QQDAYS        PDAYS             5 0
     C*
     C     WDAYS         IFNE      0
     C     PDAYS         DIV       WDAYS         FACT             11 8
     C     FDAYS         MULT(H)   FACT          XDAYS             3 0
     C                   ELSE
     C                   Z-ADD     0             XDAYS
     C                   ENDIF
     C*
     C     FDAYS         IFNE      0
     C     365           DIV       FDAYS         FACT
     C     XDAYS         MULT(H)   FACT          EDAYS             3 0
     C                   ELSE
     C                   Z-ADD     0             EDAYS
     C                   ENDIF
     C*
     C                   Z-ADD     WDAYS         TDAYS             3 0
     C*
     C                   Z-ADD     ELPFDT        X2EFDT
     C     STKY          SETGT     PST01
     C     STKY1         READPE    PST01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     STEFDT        X2EFDT
     C     STKY2         SETGT     PST02
     C     STKY          READPE    PST02                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      STPERC        XXPROR
     C                   ELSE
     C                   MOVE      '00000'       XXPROR
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ELSE
     C                   MOVE      '00000'       XXPROR
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  ANNUALIZE THE PREMIUM FACTOR
     C*
     C     PDAYS         IFNE      0
     C     WDAYS         DIV       PDAYS         FACT             11 8
     C                   ELSE
     C                   Z-ADD     0             FACT
     C                   END
     C*
     C     FDAYS         IFNE      0
     C     365           DIV       FDAYS         FACTX            11 8
     C                   ELSE
     C                   Z-ADD     0             FACTX
     C                   END
     C     FACT          MULT(H)   FACTX         FACT
     C*
     C                   Z-ADD(H)  FACT          FACTY             7 4
     C*
     C                   ENDIF                                                  *IN85 IFEQ
     C*
     C* CA FLAT RATE INCREASE FOR 2002 POLICIES (ANNIVERSARY OR EFF DT)
     C*
     C     XXST          IFEQ      'CA'
     C     *IN90         ANDEQ     *OFF
     C*
     C                   MOVEL     ELPFDT        XXPFYR            3 0
     C                   MOVEL     ELANDT        XXAYR             3 0
     C*
     C     XXPFYR        IFEQ      102
     C     XXAYR         OREQ      102
     C*
     C                   MOVE      'Y'           XXRATE
     C                   ADD       4             FILL              2 0
     C*
     C*
     C     XXNEWF        IFEQ      'Y'
     C*
     C     FILL          IFGT      42
     C                   Z-ADD     4             FILL
     C                   SETON                                        54
     C                   GOTO      FULLT
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     XXPAG4        IFEQ      *ZERO
     C*
     C     FILL          IFGT      32
     C                   Z-ADD     4             FILL
     C                   SETON                                        54
     C                   GOTO      FULLT
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     FILL          IFGT      44
     C                   Z-ADD     4             FILL
     C                   SETON                                        54
     C                   GOTO      FULLT
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXPAG4 IFEQ
     C*
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C                   ENDIF                                                  XXPFYR
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C     RTAG          TAG
     C*
     C     XXRATE        IFEQ      'Y'
     C*
     C                   MOVE      *BLANK        XXRATE
     C                   SETOFF                                       54
     C                   SETON                                        51
     C                   MOVE      '1'           XXPRT             1
     C                   MOVE      '1'           X2PRT             1
     C*
     C                   Z-ADD     0             EPCLAS
     C                   MOVE      *BLANK        EPSPEC
     c                   If        isContri = '1'
     C                   MoveL(P)  CON(68)       PRdesc
     c                   Else
     c                   MoveL(P)  CON(30)       PRdesc
     c                   Endif
     C                   Z-ADD     0             XXEAPR
     C                   Z-ADD     0             EPARAT
     C                   Z-ADD     TPREM         XXEMPR
     C*
     C*
     C                   Z-ADD     0             XQARAT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRAV
     C                   MOVEL     SRDTO         X1FRAV
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTOAV
     C                   MOVEL     SRDTO         X1TOAV
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C     XXNEWF        IFEQ      *BLANK
     C                   EXCEPT    DETT
     C                   ELSE
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QDET71
     C                   OTHER
     C                   WRITE     QDET
     C                   ENDSL
     C                   ENDIF                                                  XXNEWF IFEQ
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C*
     C                   MOVEL(P)  CON(73)       PRDESC
     C                   Z-ADD     0             XXEAPR
     C                   Z-ADD     0             EPARAT
     C     XXSRPR        IFEQ      'Y'
     C*
     C                   MOVEL     XXPROR        XXPCT             7 4
     C                   DIV       100           XXPCT
     C     FACTY         IFNE      *ZERO
     C     ELAMPR        DIV       FACTY         SPREM
     C                   ENDIF
     C     XXPCT         IFNE      *ZERO
     C     SPREM         DIV(H)    XXPCT         XXWK10
     C                   ELSE
     C                   Z-ADD(H)  SPREM         XXWK10
     C                   ENDIF
     C                   Z-ADD     XXWK10        SPREM
     C     SPREM         SUB       TPREM         XXEMPR
     C*
     C                   ELSE
     C     ELAMPR        SUB       TPREM         XXEMPR
     C                   ENDIF
     C*
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C     XXNEWF        IFEQ      *BLANK
     C                   EXCEPT    DETT
     C                   ELSE
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QDET71
     C                   OTHER
     C                   WRITE     QDET
     C                   ENDSL
     C                   ENDIF                                                  XXNEWF IFEQ
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C*
     C                   ENDIF                                                  XXRATE IFEQ
     C*
     C     XXST          IFEQ      'NY'
     C     ELPTDT        ANDGT     1071001
     C     RDAT(1)       ANDLT     1071001
     C     RDAT(1)       ANDNE     1061001
     C     *IN90         ANDEQ     *OFF
     C*
     C                   MOVE      'Y'           X1RATE
     C                   ADD       4             FILL
     C*
     C     XXNEWF        IFEQ      'Y'
     C*
     C     FILL          IFGT      42
     C                   Z-ADD     4             FILL
     C                   SETON                                        54
     C                   GOTO      FULLT
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     XXPAG4        IFEQ      *ZERO
     C*
     C     FILL          IFGT      32
     C                   Z-ADD     4             FILL
     C                   SETON                                        54
     C                   GOTO      FULLT
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C     FILL          IFGT      44
     C                   Z-ADD     4             FILL
     C                   SETON                                        54
     C                   GOTO      FULLT
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXPAG4 IFEQ
     C*
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C                   ENDIF
     C*
     C     RTAG1         TAG
     C*
     C     X1RATE        IFEQ      'Y'
     C                   MOVE      *BLANK        X1RATE
     C                   SETOFF                                       54
     C                   SETON                                        51
     C                   MOVE      '1'           XXPRT
     C                   MOVE      '1'           X2PRT
     C                   Z-ADD     0             EPCLAS
     C                   Z-ADD     0             XXEAPR
     C                   Z-ADD     0             EPARAT
     C                   Z-ADD     0             XQARAT
     C                   MOVE      ' '           EPSPEC
     c                   If        isContri = '1'
     C                   MoveL(P)  CON(68)       PRdesc
     c                   Else
     c                   MoveL(P)  CON(30)       PRdesc
     c                   Endif
     C                   Z-ADD     TPREM         XXEMPR
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFRAV
     C                   MOVEL     SRDTO         X1FRAV
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXTOAV
     C                   MOVEL     SRDTO         X1TOAV
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C     XXNEWF        IFEQ      *BLANK
     C                   EXCEPT    DETT
     C                   ELSE
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QDET71
     C                   OTHER
     C                   WRITE     QDET
     C                   ENDSL
     C                   ENDIF                                                  XXNEWF IFEQ
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C                   MOVEL(P)  CON(74)       PRDESC
     C*
     C     *IN85         IFEQ      '1'
     C                   MOVEL     XXPROR        XXPCT
     C     XXPCT         DIV(H)    100           XXPCT
     C*
     C     FACTY         IFNE      0
     C     ELAMPR        DIV(H)    FACTY         SPREM
     C                   ENDIF
     C*
     C     XXPCT         IFNE      0
     C     SPREM         DIV(H)    XXPCT         XXWK10
     C                   ELSE
     C                   Z-ADD(H)  SPREM         XXWK10
     C                   ENDIF
     C*
     C                   Z-ADD     XXWK10        SPREM
     C     SPREM         SUB       TPREM         XXEMPR
     C*
     C                   ELSE
     C     ELAMPR        SUB       TPREM         XXEMPR
     C                   ENDIF
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    DET
     C                   ENDIF
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C     XXNEWF        IFEQ      *BLANK
     C                   EXCEPT    DETT
     C                   ELSE
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QDET71
     C                   OTHER
     C                   WRITE     QDET
     C                   ENDSL
     C                   ENDIF                                                  XXNEWF IFEQ
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C                   ENDIF
     C*
     C*
     C*  ACCUMULATE AMOUNTS FOR PAYMENTS TO DATE
     C*
     C                   Z-ADD     0             XXPRPD           11 2
     C                   Z-ADD     0             XXEXPD           11 2
     C                   Z-ADD     0             XXGPD            11 2
     C                   Z-ADD     0             XXDPRL            9 2
     C                   Z-ADD     0             XXRCTL
     C                   MOVE      'P'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C     EAARCD        IFNE      F1RCCD
     C                   ADD       EAAMT         XXDPRL            9 2
     C                   ELSE
     C                   ADD       EAAMT         XXRCTL            9 2
     C                   ENDIF
      *
     C     CLTID         WHENEQ    '046'                                        CLTID=46
     C                   ADD       EAAMT         XXDPRL
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'E'
     C                   ADD       EAAMT         XXEXPD
     C                   ELSE
     C                   ADD       EAAMT         XXPRPD           11 2
     C                   ENDIF                                                  CLTID=46
      *
     C                   OTHER
     C                   If        EAARCD <> F1DVPD
     C                             and EAARCD <> F1DVXR
     C                   ADD       EAAMT         XXDPRL            9 2
     C                   Endif
     C                   ENDSL
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C                   MOVE      'T'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDNE     'D'
     C                   ADD       EAAMT         XXDPRL
     C                   ENDIF
     C     CLTID         IFEQ      '046'                                        CLTID=46
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C*
     C                   MOVE      'N'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C                   SUB       EAAMT         XXDPRL
     C     CLTID         IFEQ      '046'                                        CLTID=46
      *
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'E'
     C                   SUB       EAAMT         XXEXPD
     C                   ELSE
     C                   SUB       EAAMT         XXPRPD           11 2
     C                   ENDIF
      *
     C                   ENDIF                                                   CLTID=46
      *
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C*
     C                   MOVE      'V'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C                   SUB       EAAMT         XXDPRL
      *
     C     CLTID         IFEQ      '046'                                        CLTID=46
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'E'
     C                   SUB       EAAMT         XXEXPD
     C                   ELSE
     C                   SUB       EAAMT         XXPRPD           11 2
     C                   ENDIF
     C                   ENDIF                                                  CLTID=46
     C*
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C*
     C                   MOVE      'R'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDNE     'D'
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
      *
     C     CLTID         IFEQ      '046'                                        CLTID=46
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'E'
     C                   SUB       EAAMT         XXEXPD
     C                   ELSE
     C                   SUB       EAAMT         XXPRPD           11 2
     C                   ENDIF
     C                   ENDIF                                                  CLTID=46
     C*
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C*
     C*  INCLUDE STATE TAX PAYMENTS IN PAYMENT TOTAL
     C*
     C     CLTID         IFEQ      '046'
     C                   Z-ADD     0             XXTAXP           11 2
     C                   ENDIF
      *
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'T'
     C                   ADD       EAAMT         XXDPRL
      *
     C     CLTID         IFEQ      '046'
     C                   ADD       EAAMT         XXTAXP           11 2
     C                   ENDIF
      *
     C     CLTID         IFEQ      '102'
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
     C                   END
      *
     C     *IN60         IFEQ      *OFF
     C     CLTID         ANDNE     '109'
     C     CLTID         ANDNE     '102'
     C     CLTID         ANDNE     '046'
      *
      *
     C     TTCTYP        IFNE      'M'
     C     TTCTYP        ANDNE     'S'
     C     TTCTYP        ANDNE     'T'
     C     TTCTYP        ANDNE     '1'
     C     TTCTYP        ANDNE     '2'
     C     TTCTYP        ANDNE     '3'
     C     TTCTYP        ANDNE     '4'
     C     TTCTYP        ANDNE     '5'
     C     TTCTYP        ANDNE     '6'
     C     TTCTYP        ANDNE     '7'
     C     EAARCD        ANDNE     IFIFPD
     C     EAARCD        ANDNE     IFIFTR
     C     EAARCD        ANDNE     IFIFTC
     C     TTCTYP        ANDNE     'R'
     C     XXIDBD        ANDEQ     *BLANK
     C     TTCTYP        OREQ      'R'
     C     XXIDBD        ANDEQ     'Y'
     C                   ADD       EAAMT         XXDPRL
     C                   ENDIF                                                  TTCTYP <> 'M'
      *
     C                   ENDIF                                                  *60 = *OFF
     C     CLTID         IFEQ      '046'
     C     TTCTYP        ANDNE     'M'
     C     TTCTYP        ANDNE     'S'
     C     TTCTYP        ANDNE     'T'
     C     TTCTYP        ANDNE     'R'
     C     TTCTYP        ANDNE     '1'
     C     TTCTYP        ANDNE     '2'
     C     TTCTYP        ANDNE     '3'
     C     TTCTYP        ANDNE     '4'
     C     TTCTYP        ANDNE     '5'
     C     TTCTYP        ANDNE     '6'
     C     TTCTYP        ANDNE     '7'
     C     EAARCD        ANDNE     IFIFPD
     C     EAARCD        ANDNE     IFIFTR
     C     EAARCD        ANDNE     IFIFTC
     C     EAARCD        ANDNE     'NP'
     C     EAARCD        ANDNE     'LP'
     C     EAARCD        ANDNE     'LR'
     C                   ADD       EAAMT         XXDPRL
     C                   ADD       EAAMT         XXGPD
     C                   ENDIF                                                  CLTID IFEQ
      *
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C*
     C*  CALCULATE DEPOSIT DUE
     C*
     C                   Z-ADD     ELAAPR        XXDPDU            9 2
     C*
     C*  INCLUDE STATE TAX BILLED IN BALANCE, CHECK FOR ZERO
     C*
     C                   Z-ADD     ELTXDU        XXTXDU           11 2
     C                   ADD       ELTXDU        XXDPDU               4142
     C     CLTID         IFEQ      '071'
     C                   SUB       XXRCTL        XXDPDU               4142
     C                   ENDIF
     C     CLTID         IFEQ      '102'
     C                   SUB       ELTXDU        XXDPDU               4142
     C                   ENDIF
     C     ELTXDU        IFEQ      *ZERO
     C                   MOVE      *ON           *IN45
     C                   ELSE
     C                   MOVE      *OFF          *IN45
     C                   END
     C*
     C*  Print dividend amount in balance section and reduce
     C*  the amount due.
     C*
     C                   Z-ADD     0             xxDivA           11 2
      /free
       xxDivA = PrintDividend(ELCo#:ELFnd:ELEmp#:ELDiv:ELFyr:outEnv);
       xxDPDU = xxDPDU - xxDivA;
      /end-free
     C*
     C*  FIND IF THE EMPLOYER IS CANCELLED AND WHEN.
     C*
     C                   Z-ADD     5             XXX               1 0
     C                   MOVE      *BLANK        SVCNCL            1
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      'Y'           SVCNCL
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             XXX
     C                   ENDDO
     C*
     C*  IF SELF-REPORTING TYPE AND CREDIT AMOUNT, PRINT PLEASE DEDUCT
     C*
     C                   MOVE      '0'           *IN43
     C     *IN42         IFEQ      '1'
     C     XXSELF        ANDEQ     'Y'
     C     SVCNCL        ANDNE     'Y'
     C     *IN57         ANDEQ     *OFF
     C                   MOVE      '1'           *IN43
     C                   END
     C*
     C*  CALCULATE TOTAL NORMAL
     C*
     C     ELASPR        SUB       ELADC2        XXTNOR            9 2
     C                   SUB       ELADC3        XXTNOR
     C*
     C*  CALCULATE PRORATA PREMIUM
     C*
     C     ELPROR        IFEQ      1
     C                   END
     C*
     C     ELAAPR        SUB       ELPRTD        XXPRO            11 2
     C     CLTID         IFEQ      '102'
     C                   Z-ADD     ELAAPR        XXPRO            11 2
     C                   ENDIF
      /free
       If CltId = '111';
          xxDvnd = 0;

          Chain (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) pdv00;
          If %found and DVamt <> *zero;
             xxDvnd = DVamt;
             xxDPDU = xxDPDU - DVamt;
          Endif;
       Endif;
      /end-free
     C*
     C*  EXPENSE CONSTANT COMES FROM ELPRTD
     C*
     C     ELPRTD        IFGT      *ZERO
     C                   Z-ADD     ELPRTD        XXEXCN            9 2
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPPRTD        XXEXCN
     C                   ENDIF
     C                   ELSE
     C                   Z-ADD     0             XXEXCN
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C*  CHECK AUDITED MIN. PREMIUM FLAG
     C*
     C     FNKEY         CHAIN     PEX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANK        EXBEXP
     C*
     C                   MOVEL(P)  'Agent #'     EXAHDR
     C*
     C                   ELSE
     C*
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  'Agent #'     EXAHDR
     C                   ENDIF
     C*
     C     EXAHDR        CAT       ':':0         EXAHDR
     C*
     C                   ENDIF
     C*
     C                   MOVE      '0'           *IN34
     C     ELAMNP        IFEQ      'Y'
     C                   MOVE      '1'           *IN34
     C                   ENDIF
     C                   SETOFF                                       44
     C     ELAMNP        IFEQ      'Y'
     C                   SETON                                        44
     C                   Z-ADD     0             XXEXCN            9 2
     C     EXBEXP        IFEQ      'Y'
     C                   Z-ADD     ELPRTD        XXEXCN
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPPRTD        XXEXCN
     C                   ENDIF
     C                   MOVE      '0'           *IN44
     C                   ENDIF                                                  EXBEXP IFEQ
     C                   END
     C*
     C     XXEXCN        COMP      *ZERO                                  35
     C     ELFND         IFEQ      2
     C     CLTID         ANDEQ     '114'
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  ELFND IFNE
     C     ELFND         IFEQ      101
     C     CLTID         ANDEQ     '114'
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  ELFND IFNE
     C*
     C*  LOAD TOTAL ARRAYS
     C*
     C                   Z-ADD     1             A                 3 0          MANUAL PREM
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
     C                   MOVEL     CON(59)       HDG(A)
     C                   OTHER
     C                   MOVEL     CON(1)        HDG(A)
     C                   ENDSL
     C     XXSRPR        IFEQ      'Y'
     C     ELSRAM        ANDEQ     *ZERO
     C                   Z-ADD     SPREM         TAM(A)
     C                   ELSE
     C                   Z-ADD     ELAMPR        TAM(A)
     C                   ENDIF
     C*
     C*  IF MIN PREM, DON'T INCLUDE THE INCR LIMIT MIN IN MANUAL PREM
     C*
     C     XXMINF        IFEQ      'Y'
     C                   SUB       ELAIN1        TAM(A)
     C                   SUB       ELAIN2        TAM(A)
     C                   SUB       ELAWAV        TAM(A)
     C                   Z-ADD     MPAMPR        TAM(A)
     C*  Increased limits amounts do not have to be subtracted from MPAMPR
     C*  they are not included in this field.
     C                   END
     C*
     C     XXSRPR        IFEQ      'Y'
     C     RRSRAF        ANDEQ     *BLANK
     C     XXSRPR        OREQ      'Y'
     C     RRSRAF        ANDEQ     'M'
     C                   ADD       1             A
     C                   ELSE
     C                   ADD       2             A
     C                   ENDIF
     C     XXSRPR        IFEQ      'Y'
     C     RRSRAF        ANDEQ     *BLANK
     C     XXSRPR        OREQ      'Y'
     C     RRSRAF        ANDEQ     'M'
     C**********
     C**CALCULATE SHORT RATED PREMIUM
     C**********
     C**USE*FYR*END DATE FOR NON-COMMERCIAL
     C**********
     C***CHECK*EMPLOYER HISTORY TO FIND THE PRE-CANCEL ELPTDT FOR COMM.
     C**********
     C**IF*NO*ENDING DATE, DEFAULT TO ONE YEAR FROM EFF DATE.
     C**********
     C**********
     C***GET*NUMBER OF DAYS POLICY WAS ORIGINALLY WRITTEN FOR IN
     C***ORDER*TO CALCULATE EXTENDED DAYS FOR SHORT RATING
     C**********
     C***GET*NUMBER OF EMPLOYER POLICY DAYS
     C**********
     C*
     C     ELSRAM        IFNE      *ZERO
     C                   Z-ADD     ELSRAM        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C     ELAMPR        SUB       SPREM         TAM(A)
     C                   ENDIF
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   MOVEA     CON(63)       TMP
     C                   OTHER
     C                   MOVEA     CON(26)       TMP
     C                   ENDSL
     C                   MOVEA     XXPROR        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVE      '%'           PCT(7)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C**********
     C***ANNUALIZE THE PREMIUM FACTOR
     C**********
     C                   MOVE      FACTY         XXASP7            7
     C                   MOVEA     XXASP7        NUM7
     C                   MOVE      'X'           TMP(30)
     C                   MOVE      '.'           TMP(24)
     C     NUM7(1)       IFNE      *ZERO
     C                   MOVE      NUM7(1)       TMP(21)
     C                   ENDIF
     C     NUM7(1)       IFNE      *ZERO
     C     NUM7(2)       ORNE      *ZERO
     C                   MOVE      NUM7(2)       TMP(22)
     C                   ENDIF
     C                   MOVE      NUM7(3)       TMP(23)
     C                   MOVE      NUM7(4)       TMP(25)
     C                   MOVE      NUM7(5)       TMP(26)
     C                   MOVE      NUM7(6)       TMP(27)
     C                   MOVE      NUM7(7)       TMP(28)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C*
     C                   ADD       2             A
     C*
     C                   ENDIF                                                  *IN85 IFEQ
     C     ELAWAV        IFNE      *ZERO
     C     ELWVFL        ANDEQ     *BLANK
     C                   MOVE      'M'           ELWVFL
     C                   ENDIF
     C*
     C*
     C* DEVIATION #1 APPLIED TO MANUAL
     C*
     C     RRDV1L        IFEQ      'M'
     C*
     C     ELADC2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELASP2        ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADC2        TAM(A)
     C     ELASP2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADC2        TAM(A)
     C                   ENDIF
     C*
     C     RRDV1H        IFEQ      *BLANK
     C                   MOVEL     CON(6)        HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV1H        HDG(A)
     C                   ENDIF
     C*
     C     ELDVEF        IFNE      *ZERO
     C                   Z-ADD     ELDVEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELDVEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELDVEX        IFNE      *ZERO
     C                   Z-ADD     ELDVEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C     ELASP2        IFLT      0
      *
     C     ELASP2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELASP2        XXASP2            5
     C                   ENDIF                                                  ELASP2
     C                   MOVEA     XXASP2        NUM
      *
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDC2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C* DEVIATION #1 2ND PERIOD APPLIED TO MANUAL
     C     RRDV1L        IFEQ      'M'
     C*
     C     ELD1A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD1S2        ANDNE     *ZERO
     C                   Z-ADD     ELD1A2        TAM(A)
     C     ELD1S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD1A2        TAM(A)
     C                   ENDIF
     C*
     C     RRDV1H        IFEQ      *BLANK
     C     con(6)        CAT       @2nper:1      HDG(A)
     C                   ELSE
     C     rrdv1h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD1D2        IFNE      *ZERO
     C                   Z-ADD     ELD1D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD1X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD1X2        IFNE      *ZERO
     C                   Z-ADD     ELD1X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C     ELD1S2        IFLT      0
     C     ELD1S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP2            5
     C                   ENDIF                                                  ELD1S2
     C                   MOVEA     XXASP2        NUM
      *
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELD1A2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*  LOAD 2ND DEVIATION IF APPLIED TO MANUAL PREMIUM
     C*
     C     RRDV2L        IFEQ      'M'
     C*
     C     ELADV2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELDEV2        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDEV2F       ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADV2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADV2        TAM(A)
     C                   ENDIF
     C                   Select
     C                   When      ELdev2  <> 0
     C                   If        ELdev2 < *Zeros
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELstFc  <> 0
     C                   If        ELstFc > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELdev2F <> 0
     C                   If        ELdev2F > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   EndSl
     C*
     C     RRDV2H        IFEQ      *BLANK
     C  N25              MOVEL     CON(19)       HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV2H        HDG(A)
     C                   ENDIF
     C*
     C     ELD2EF        IFNE      *ZERO
     C                   Z-ADD     ELD2EF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2EX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2EX        IFNE      *ZERO
     C                   Z-ADD     ELD2EX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
      *
      *
      *
      *
      *
      *
      *
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stabilization Factor
     C                   When      cltID = '025'
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
      *
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      '.'           PCT(2)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   MOVE      ELdev2F       XXASP3
     C                   MOVEA     XXASP3        NUM

     C                   MOVE      NUM(1)        PCT(2)
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      NUM(2)        PCT(4)
     C                   MOVE      NUM(3)        PCT(5)
     C                   MOVE      NUM(4)        PCT(6)
     C                   MOVE      NUM(5)        PCT(7)
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     ELDEV2        IFLT      0
      *
     C     ELDEV2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP2            5
     C                   ENDIF                                                  ELEDV2
     C                   MOVEA     XXASP2        NUM
      *
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVE      '%'           PCT(7)
     C                   EndSl
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDV2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO MANUAL PREMIUM
     C     RRDV2L        IFEQ      'M'
     C*
     C     ELD2A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     XXCLT#        ANDNE     '025'
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXCLT#        ANDEQ     '025'
     C                   Z-ADD     ELD2A2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD2A2        TAM(A)
     C                   ENDIF
     C     XXCLT#        IFNE      '025'
     C     ELD2S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   ELSE
     C     ELSTFC        IFGT      1
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   ENDIF
     C*
     C     RRDV2H        IFEQ      *BLANK
     C  N25con(19)       CAT       @2nper:1      HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C     rrdv2h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD2D2        IFNE      *ZERO
     C                   Z-ADD     ELD2D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2X2        IFNE      *ZERO
     C                   Z-ADD     ELD2X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C     XXCLT#        IFNE      '025'
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP2            5
     C                   ENDIF                                                  ELD2S2
     C*
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C*FOR CLIENT 025
     C                   ELSE
     C                   MOVE      '.'           PCT(2)
     C                   MOVE      ELSTFC        XXAS8             8
     C                   MOVEA     XXAS8         NUM8
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C                   ENDIF                                                  CLTID<>025
     C*
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELD2A2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ

      //*** Beg Add ***** 05/20/24 *********************************
      // Schedule DB/CR applied at Manual Premium
       If RRdcap = 'M';
          Exsr CALDBR;
       Endif;
      //*** End Add ***** 05/20/24 *********************************
     C*
     C* WAIVER SURCHARGE
     C*
     C     ELWVFL        IFEQ      'M'
     C*
      *** Beg Chg *****  02/28/25  *******************************************
     C*****ELAWAV        IFNE      *ZERO
     C                   If        (ELAWAV <> 0 and XXMINF <> 'Y') OR
     C                             (MPAWAV <> 0 and XXMINF = 'Y')
      *** End Add *****  02/28/25  *******************************************
     C*
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(32)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
     C*
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   ENDIF
      *** Beg Add *****  05/12/25  *******************************************
      /Free
       // Print an '*' appended to heading on the line item
       If ELPror > 0 and ELPror < 1;
          If xxminf = 'Y';
             HDG(A) = %trim(HDG(A)) + '*';
             PrintFlg = 'Y';
          Endif;
       Endif;
      /End-Free
      *** End Add *****  05/12/25  *******************************************
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  RRAWAV IFNE
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* INCREASED LIMITS
     C*
     C                   MOVE      '0'           *IN48
     C*
     C     RREMLB        IFEQ      *BLANKS
     C                   MOVE      *ON           *IN48
     C                   ENDIF
     C*
     C     ELILCD        IFNE      *BLANKS                                      INCR LIMITS
     C                   MOVE      '1'           *IN48
     C                   MOVEL     CON(3)        HDG(A)
      //*** Beg Chg ***** 08/14/24 *********************************
     C***                Z-ADD     ELAIN1        TAM(A)
     C***  ELAIN1        IFNE      *ZERO                                        INCR LIMITS
     C***                MOVE      '+'           SGN(A)
     C***                ENDIF
       Tam(A) = MPain1;
       If MPain1 <> 0;
          SGN(A) = '+';
       Endif;
      //*** End Chg ***** 08/14/24 *********************************
     C     ELILEF        IFNE      *ZERO
     C                   Z-ADD     ELILEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELILEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELILEX        IFNE      *ZERO
     C                   Z-ADD     ELILEX        SRDTI
     C                   ELSE
     C     ELI2EF        IFNE      *ZERO
     C                   Z-ADD     ELI2EF        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF                                                  ELI2EF IFNE
     C                   ENDIF                                                  ELILEX IFNE
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C*  GET DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVE      *BLANKS       XLDSC1
     C                   MOVEL     ELILCD        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *BLANKS       XLMP              5
     C                   MOVE      LMPCT         XLMP
     C                   MOVEA     XLMP          NUM
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      '%'           PCT(7)
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C                   MOVE      NUM(2)        PCT(2)
     C                   MOVE      NUM(3)        PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   MOVEL(P)  LMDESC        XLDSC1           18
     C                   MOVE      XLDSC1        HDG(A)
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ADD       1             A
      //*** Beg Add ***** 08/14/24 *********************************
      //Print Balance to Increased Limit Minimum
       If MPabil <> 0;
          HDG(A) = Con(92);
          TAM(A) = MPabil;
          SGN(A) = '+';

          //*** Beg Add *****  05/12/25  *******************************************
          // Print an '*' appended to heading on the line item
          If ELPror > 0 and ELPror < 1;
             If (xxminf = 'Y' and xxst = 'NJ')
              OR (xxminf = 'Y' and ELcofl <> 'Y');
                HDG(A) = %trim(HDG(A)) + '*';
                PrintFlg = 'Y';
             Endif;
          Endif;
          //*** End Add *****  05/12/25  *******************************************

          A += 1;
       Endif;
      //*** End Add ***** 08/14/24 *********************************
     C                   ENDIF
     C*
     C* 2ND INCREASED LIMITS
     C*
     C     ELINL2        IFNE      *BLANKS                                      INCR LIMITS
     C                   MOVEL     CON(23)       HDG(A)
      //*** Beg Chg ***** 08/14/24 *********************************
     C***                Z-ADD     ELAIN2        TAM(A)
     C***  ELAIN2        IFNE      *ZERO
     C***                MOVE      '+'           SGN(A)
     C***                ENDIF
       Tam(A) = MPain2;
       If MPain2 <> 0;
          SGN(A) = '+';
       Endif;
      //*** Beg Chg ***** 08/14/24 *********************************
     C*
     C     ELI2EF        IFNE      ELPFDT
     C     ELI2EX        ORNE      0
     C*
     C                   Z-ADD     ELI2EF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELI2EX        IFNE      *ZERO
     C                   Z-ADD     ELI2EX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELI2EF IFNE
     C*
     C*  GET DESCRIPTION
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVE      *BLANKS       XLDSC2
     C                   MOVEL     ELINL2        XXKEY
     C     LMKY          SETGT     PLM01
     C     LMKY2         READPE    PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C     LMKY2         READPE    PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C     *IN60         IFEQ      *OFF
     C                   MOVEL(P)  LMDESC        XLDSC2           18
     C                   MOVE      XLDSC2        HDG(A)
     C                   MOVE      *BLANKS       XLMP              5
     C                   MOVE      LMPCT         XLMP
     C                   MOVEA     XLMP          NUM
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      '%'           PCT(7)
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C                   MOVE      NUM(2)        PCT(2)
     C                   MOVE      NUM(3)        PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ADD       1             A
      //*** Beg Add ***** 08/14/24 *********************************
      //Print Balance to 2nd Increased Limit Minimum
       If MPabil2 <> 0;
          HDG(A) = Con(93);
          TAM(A) = MPabil2;
          SGN(A) = '+';

          //*** Beg Add *****  05/12/25  *******************************************
          // Print an '*' appended to heading on the line item
          If ELPror > 0 and ELPror < 1;
             If (xxminf = 'Y' and xxst = 'NJ')
              OR (xxminf = 'Y' and ELcofl <> 'Y');
                HDG(A) = %trim(HDG(A)) + '*';
                PrintFlg = 'Y';
             Endif;
          Endif;
          //*** End Add *****  05/12/25  *******************************************

          A += 1;
       Endif;
      //*** End Add ***** 08/14/24 *********************************
     C                   ENDIF
      *
      *  VOLUNTARY LABOR CHARGE
      *
     C     ELVLCA        IFNE      *ZERO
     C*
     C     RRVLCH        IFEQ      *BLANKS
     C                   MOVEL     CON(42)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRVLCH        HDG(A)
     C                   ENDIF                                                  RRVLCH IFEQ
     C*
     C                   Z-ADD     ELVLCA        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPAVLC        TAM(A)
     C                   ENDIF
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  ELVLCA IFNE
     C*
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO MANUAL PREMIUMS
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     ELADCT        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDDCT        ANDNE     *ZERO
     C*
     C     ELDCEF        IFNE      *ZERO
     C                   Z-ADD     ELDCEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELDCEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELDCEX        IFNE      *ZERO
     C                   Z-ADD     ELDCEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   Z-ADD     ELADCT        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCT        TAM(A)
     C                   ENDIF
     C     ELADCT        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   MOVEL     CON(14)       HDG(A)
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   END
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   END
     C*
     C     ELADD2        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDD2P        ANDNE     *ZERO
     C*
     C     EL2DEF        IFNE      *ZERO
     C                   Z-ADD     EL2DEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELDCEX        SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C*
     C                   Z-ADD     EL2DEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C*
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   Z-ADD     ELADD2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADD2        TAM(A)
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   MOVEL     CON(47)       HDG(A)
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   CLEAR                   TMP
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*  GET DESCRIPTION
     C*
     C                   MOVE      *OFF          *IN47
     C                   MOVE      *ON           *IN46
     C     ELDCDE        IFNE      *BLANK
     C                   CLEAR                   XXDDVB
     C                   MOVE      ELDCDE        XXCODE
     C                   Z-ADD     ELPFDT        X7DDEF
     C                   MOVE      ELDCEF        Q1DATX
     C                   MOVE      ELFYR         Q1FYR
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1DATX            7
     C*
     C                   MOVE      Q1DATX        X7DDEF
     C     DDKY          SETGT     PDD01
     C     DDKY2         READPE    PDD01                                  46
     C     *IN46         IFEQ      *OFF
     C     DDSTAT        ANDEQ     'A'
     C*
     C                   SELECT
     C     DDVERB        WHENEQ    'C'
     C                   MOVEL(P)  CLM           XXOCCL           15
     C                   MOVEL(P)  XXOCCL        XXDDVB
     C                   OTHER
     C                   MOVEL(P)  OCC           XXOCCL
     C                   MOVEL(P)  XXOCCL        XXDDVB
     C                   ENDSL
     C*
     C                   MOVEL(P)  DDDESC        XXDDD1           27
     C     DDTYPE        IFEQ      'G'
     C                   MOVEL     'Gross'       XXDEDT            5
     C                   ELSE
     C                   MOVEL     'Net  '       XXDEDT
     C                   ENDIF                                                  DDTYPE IFEQ
     C*
     C*
     C                   Z-ADD     DDAMPO        XXAMPO
     C                   Z-ADD     DDAMAG        XXAMAG
     C*
     C                   ENDIF                                                  *IN46 IFEQ
     C*
     C                   ENDIF                                                  ELDDCT IFNE
     C*
     C                   MOVE      *ON           *IN47
     C*
     C     ELDCD2        IFNE      *BLANK
     C*
     C                   CLEAR                   XXDDVB
     C                   MOVE      ELDCD2        XXCODE
     C                   Z-ADD     EL2DEF        X7DDEF
     C                   MOVE      EL2DEF        Q1DATX
     C                   MOVE      ELFYR         Q1FYR
     C                   MOVE      ELEMP#        Q1EMP#
     C                   MOVE      ELDIV         Q1DIV
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1DATX            7
     C*
     C                   MOVE      Q1DATX        X7DDEF
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY2         READPE    PDD01                                  47
     C*
     C     *IN47         IFEQ      *OFF
     C     DDSTAT        ANDEQ     'A'
     C*
     C                   SELECT
     C     DDVERB        WHENEQ    'C'
     C                   MOVEL(P)  CLM           X2OCCL           15
     C                   MOVEL(P)  X2OCCL        XXDDVB
     C                   OTHER
     C                   MOVEL(P)  OCC           X2OCCL
     C                   MOVEL(P)  X2OCCL        XXDDVB
     C                   ENDSL
     C*
     C                   MOVEL(P)  DDDESC        X2DDD1           27
     C*
     C     DDTYPE        IFEQ      'G'
     C                   MOVEL     'Gross'       X2DEDT            5
     C                   ELSE
     C                   MOVEL     'Net  '       X2DEDT
     C                   ENDIF
     C*
     C     *LIKE         DEFINE    DDAMPO        X2AMPO
     C     *LIKE         DEFINE    DDAMAG        X2AMAG
     C*
     C                   Z-ADD     DDAMPO        X2AMPO
     C                   Z-ADD     DDAMAG        X2AMAG
     C*
     C                   Z-ADD     EL2DEF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X82DEF            8 0
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C     XXLRGD        IFEQ      'Y'
     C*
     C*  1st Large Deductible applied to Manual
     C*
     C     LDPLV1        IFEQ      'M'
     C*
     C     ELADCT        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C     LDEFD1        IFNE      *ZERO
     C                   Z-ADD     LDEFD1        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     LDEXP1        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     LDEXP1        IFNE      *ZERO
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   Z-ADD     ELADCT        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCT        TAM(A)
     C                   ENDIF
     C     ELADCT        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   MOVEL     CON(79)       HDG(A)
     C*
     C                   MOVE      LDPCT1        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV1 IFEQ 'M'
     c*
     c*  2nd Large Deductible applied to Manual
     c*
     C     LDPLV2        IFEQ      'M'
     C*
     C     ELADD2        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C     LDEFD2        IFNE      *ZERO
     C                   Z-ADD     LDEFD2        SRDTI
     C                   ELSE
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C*
     C                   Z-ADD     LDEXP2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C*
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   Z-ADD     ELADD2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADD2        TAM(A)
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   MOVEL     CON(80)       HDG(A)
     C*
     C                   MOVE      LDPCT2        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV2 IFEQ 'M'
     c*
     c*  1st Large Deductible Info
     c*
     C     LDPCT1        IFNE      0
     C*
     C                   MOVE      *OFF          *IN46
     C                   CLEAR                   XXDDVB
     C*
     C                   SELECT
     C     LDVRB1        WHENEQ    'C'
     C                   MOVEL(P)  CLM           XXOCCL           15
     C                   MOVEL(P)  XXOCCL        XXDDVB
     C                   OTHER
     C                   MOVEL(P)  OCC           XXOCCL
     C                   MOVEL(P)  XXOCCL        XXDDVB
     C                   ENDSL
     C*
     C                   MOVEL(P)  LDDES1        XXDDD1           27
     C     LDTYP1        IFEQ      'G'
     C                   MOVEL     'Gross'       XXDEDT            5
     C                   ELSE
     C                   MOVEL     'Net  '       XXDEDT
     C                   ENDIF                                                  DDTYPE IFEQ
     C*
     C                   Z-ADD     LDAMP1        XXAMPO
     C                   Z-ADD     LDAMG1        XXAMAG
     C*
     C                   ENDIF                                                  LDPCT1 IFNE 0
     c*
     c*  2nd Large Deductible Info
     c*
     C     LDPCT2        IFNE      0
     C*
     C                   MOVE      *OFF          *IN47
     C                   CLEAR                   XXDDVB
     C*
     C                   SELECT
     C     LDVRB2        WHENEQ    'C'
     C                   MOVEL(P)  CLM           X2OCCL           15
     C                   MOVEL(P)  X2OCCL        XXDDVB
     C                   OTHER
     C                   MOVEL(P)  OCC           X2OCCL
     C                   MOVEL(P)  X2OCCL        XXDDVB
     C                   ENDSL
     C*
     C                   MOVEL(P)  LDDES2        X2DDD1           27
     C*
     C     LDTYP2        IFEQ      'G'
     C                   MOVEL     'Gross'       X2DEDT            5
     C                   ELSE
     C                   MOVEL     'Net  '       X2DEDT
     C                   ENDIF
     C*
     C                   Z-ADD     LDAMP2        X2AMPO
     C                   Z-ADD     LDAMG2        X2AMAG
     C*
     C                   Z-ADD     LDEFD2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X82DEF
     C*
     C                   ENDIF                                                  LDPCT2 IFNE 0
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*
     C*  HCN CREDIT
     C*
     C     ELAHCN        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELHCN         ANDNE     *ZERO
     C*
     C                   Z-ADD     ELAHCN        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPAHCN        TAM(A)
     C                   ENDIF
     C*
     C     ELAHCN        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   MOVEL     CON(85)       HDG(A)
     C*
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      ELHCN         XXASP3            5
     C                   MOVEA     XXASP3        NUM
     C                   MOVE      NUM(1)        PCT(2)
     C                   MOVE      NUM(2)        PCT(4)
     C                   MOVE      NUM(3)        PCT(5)
     C                   MOVE      NUM(4)        PCT(6)
     C                   MOVE      NUM(5)        PCT(7)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  ELHCN IFNE *ZERO
     C*
     C*  SUBJECT PREMIUM
     C*
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
     C                   MOVEL     CON(64)       HDG(A)
     C                   OTHER
     C                   MOVEL     CON(15)       HDG(A)
     C                   ENDSL
     C                   Z-ADD     ELASBJ        TAM(A)
     C     XXMINF        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C                   SUB       ELAWAV        TAM(A)
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPASBJ        TAM(A)
     C                   ENDIF
     C* Waiver does not need to be subtracted from the MPASBJ
     C     A             SUB       1             B                 2 0
     C                   MOVE      *ON           *IN(B)
     C                   ADD       2             A
     C*
     C*  MANAGED CARE CREDIT
     C*
     C     ELADCM        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELASPM        ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADCM        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCM        TAM(A)
     C                   ENDIF
     C     ELADCM        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C     RR1AHD        IFEQ      *BLANK
     C                   MOVEL     CON(16)       HDG(A)
     C                   ELSE
     C                   MOVEL     RR1AHD        HDG(A)
     C                   ENDIF                                                  RR1AHD IFEQ
     C     ELMCEF        IFNE      *ZERO
     C                   Z-ADD     ELMCEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELMCEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELMCEX        IFNE      *ZERO
     C                   Z-ADD     ELMCEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      ELASPM        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   END
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   CLEAR                   TMP
     C                   ADD       1             A
     C                   END
     C*
     C*  SAFETY CREDIT
     C*
     C     ELADCS        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELASPS        ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADCS        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCS        TAM(A)
     C                   ENDIF
     C     ELADCS        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C     RR1BHD        IFEQ      *BLANK
     C                   MOVEL     CON(17)       HDG(A)
     C                   ELSE
     C                   MOVEL     RR1BHD        HDG(A)
     C                   ENDIF                                                  RR1BHD IFEQ
     C     ELSCEF        IFNE      *ZERO
     C                   Z-ADD     ELSCEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELSCEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELSCEX        IFNE      *ZERO
     C                   Z-ADD     ELSCEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      ELASPS        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   END
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   END
     C*
     C*  LOAD DEVIATION IF APPLIED TO SUBJECT PREMIUM
     C*
     C     RRDV1L        IFEQ      'J'
     C*
     C     ELADC2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELASP2        ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADC2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADC2        TAM(A)
     C                   ENDIF
     C     ELASP2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C     RRDV1H        IFEQ      *BLANK
     C                   MOVEL     CON(6)        HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV1H        HDG(A)
     C                   ENDIF
     C     ELDVEF        IFNE      *ZERO
     C                   Z-ADD     ELDVEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELDVEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELDVEX        IFNE      *ZERO
     C                   Z-ADD     ELDVEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C     ELASP2        IFLT      0
      *
     C     ELASP2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELASP2        XXASP2            5
     C                   ENDIF                                                  ELASP2
     C                   MOVEA     XXASP2        NUM
      *
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDC2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C*  LOAD DEVIATION 1 2ND PERIOD  IF APPLIED TO SUBJECT PREMIUM
     C     RRDV1L        IFEQ      'J'
     C*
     C     ELD1A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD1S2        ANDNE     *ZERO
     C                   Z-ADD     ELD1A2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD1A2        TAM(A)
     C                   ENDIF
     C     ELD1S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C     RRDV1H        IFEQ      *BLANK
     C     con(6)        CAT       @2nper:1      HDG(A)
     C                   ELSE
     C     rrdv1h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD1D2        IFNE      *ZERO
     C                   Z-ADD     ELD1D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD1X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD1X2        IFNE      *ZERO
     C                   Z-ADD     ELD1X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C     ELD1S2        IFLT      0
     C     ELD1S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP2            5
     C                   ENDIF                                                  ELD1S2
     C                   MOVEA     XXASP2        NUM
      *
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELD1A2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*  LOAD 2ND DEVIATION IF APPLIED TO SUBJECT PREMIUM
     C*
     C     RRDV2L        IFEQ      'J'
     C*
     C     ELADV2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELDEV2        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDEV2F       ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADV2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADV2        TAM(A)
     C                   ENDIF
     C                   Select
     C                   When      ELdev2  <> 0
     C                   If        ELdev2 < *Zeros
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELstFc  <> 0
     C                   If        ELstFc > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELdev2F <> 0
     C                   If        ELdev2F > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   EndSl
     C*
     C     RRDV2H        IFEQ      *BLANK
     C  N25              MOVEL     CON(19)       HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV2H        HDG(A)
     C                   ENDIF
     C     ELD2EF        IFNE      *ZERO
     C                   Z-ADD     ELD2EF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2EX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2EX        IFNE      *ZERO
     C                   Z-ADD     ELD2EX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
      *
      *
      *
      *
      *
      *
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stabilization Factor
     C                   When      cltID = '025'
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
      *
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      '.'           PCT(2)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   MOVE      ELdev2F       XXASP3
     C                   MOVEA     XXASP3        NUM

     C                   MOVE      NUM(1)        PCT(2)
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      NUM(2)        PCT(4)
     C                   MOVE      NUM(3)        PCT(5)
     C                   MOVE      NUM(4)        PCT(6)
     C                   MOVE      NUM(5)        PCT(7)
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     ELDEV2        IFLT      0
      *
     C     ELDEV2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP2            5
     C                   ENDIF                                                  ELEDV2
     C                   MOVEA     XXASP2        NUM
      *
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVE      '%'           PCT(7)
     C                   EndSl
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDV2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO SUBJECT PREMIUM
     C     RRDV2L        IFEQ      'J'
     C*
     C     ELD2A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     XXCLT#        ANDNE     '025'
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXCLT#        ANDEQ     '025'
     C                   Z-ADD     ELD2A2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD2A2        TAM(A)
     C                   ENDIF
     C     XXCLT#        IFNE      '025'
     C     ELD2S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     ELSTFC        IFGT      1
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   ENDIF
     C*
     C     RRDV2H        IFEQ      *BLANK
     C  N25con(19)       CAT       @2nper:1      HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C     rrdv2h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD2D2        IFNE      *ZERO
     C                   Z-ADD     ELD2D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2X2        IFNE      *ZERO
     C                   Z-ADD     ELD2X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C     XXCLT#        IFNE      '025'
     C                   MOVE      '%'           PCT(7)
     C                   MOVE      '.'           PCT(4)
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP2            5
     C                   ENDIF                                                  ELD2S2
     C                   MOVEA     XXASP2        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C*FOR CLIENT 025
     C                   ELSE
     C                   MOVE      '.'           PCT(2)
     C                   MOVEA     XXAS8         NUM8
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C                   ENDIF                                                  CLTID<>025
     C*
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELD2A2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*
     C* WAIVER SURCHARGE
     C*
     C     ELWVFL        IFEQ      'J'
     C*
      *** Beg Chg *****  02/28/25  *******************************************
     C*****ELAWAV        IFNE      *ZERO
     C                   If        (ELAWAV <> 0 and XXMINF <> 'Y') OR
     C                             (MPAWAV <> 0 and XXMINF = 'Y')
      *** End Add *****  02/28/25  *******************************************
     C*
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(32)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
     C*
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   ENDIF
      *** Beg Add *****  05/12/25  *******************************************
      /Free
       // Print an '*' appended to heading on the line item
       If ELPror > 0 and ELPror < 1;
          If xxminf = 'Y';
             HDG(A) = %trim(HDG(A)) + '*';
             PrintFlg = 'Y';
          Endif;
       Endif;
      /End-Free
      *** End Add *****  05/12/25  *******************************************
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  RRAWAV IFNE
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADDITIONAL LOAD
     C*
     C     RRASFL        IFEQ      'J'
     C*
     C     XTADLD        IFNE      *ZERO
     C                   MOVEL     CON(18)       HDG(A)
     C   49              Eval      HDG(A) = 'Stop Gap Premium'
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRASFL IFEQ
     C*
     C* APPLY SHORT RATE PENALTY TO SUBJECT PREMIUM.
     C*
     C     XXSRPR        IFEQ      'Y'
     C     RRSRAF        ANDEQ     'J'
     C*
     C                   Z-ADD     ELSRAM        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   MOVEA     CON(63)       TMP
     C                   OTHER
     C                   MOVEA     CON(26)       TMP
     C                   ENDSL
     C*
     C                   MOVEA     XXPROR        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVE      '%'           PCT(7)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C*
     C                   MOVE      FACTY         XXASP7            7
     C                   MOVEA     XXASP7        NUM7
     C                   MOVE      'X'           TMP(30)
     C                   MOVE      '.'           TMP(24)
     C     NUM7(1)       IFNE      *ZERO
     C                   MOVE      NUM7(1)       TMP(21)
     C                   ENDIF
     C     NUM7(1)       IFNE      *ZERO
     C     NUM7(2)       ORNE      *ZERO
     C                   MOVE      NUM7(2)       TMP(22)
     C                   ENDIF
     C                   MOVE      NUM7(3)       TMP(23)
     C                   MOVE      NUM7(4)       TMP(25)
     C                   MOVE      NUM7(5)       TMP(26)
     C                   MOVE      NUM7(6)       TMP(27)
     C                   MOVE      NUM7(7)       TMP(28)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  *IN85 IFEQ
     C     XXLRGD        IFEQ      'Y'
     C*
     C*  1st Large Deductible applied to SUBJECT
     C*
     C     LDPLV1        IFEQ      'J'
     C*
     C     ELADCT        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C     LDEFD1        IFNE      *ZERO
     C                   Z-ADD     LDEFD1        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     LDEXP1        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     LDEXP1        IFNE      *ZERO
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   Z-ADD     ELADCT        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCT        TAM(A)
     C                   ENDIF
     C     ELADCT        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   MOVEL     CON(79)       HDG(A)
     C*
     C                   MOVE      LDPCT1        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV1 IFEQ 'J'
     c*
     c*  2nd Large Deductible applied to SUBJECT
     c*
     C     LDPLV2        IFEQ      'J'
     C*
     C     ELADD2        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C     LDEFD2        IFNE      *ZERO
     C                   Z-ADD     LDEFD2        SRDTI
     C                   ELSE
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C*
     C                   Z-ADD     LDEXP2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C*
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   Z-ADD     ELADD2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADD2        TAM(A)
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   MOVEL     CON(80)       HDG(A)
     C*
     C                   MOVE      LDPCT2        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV2 IFEQ 'J'
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C                   MOVE      *OFF          *IN95
     C     ELPROM        IFEQ      'M'
     C                   MOVE      *ON           *IN95
     C                   ENDIF
     C*
     C*  EXPERIENCE MODIFIER AND MODIFIED PREMIUM
     C*
     C     ELEXM1        IFNE      *ZERO
     C*
     C     *IN89         IFEQ      *ON

      /free
       If IsSplitMod(ELexm1:ELexm2:ELaxp1:ELaxp2:ELemd1:ELemd2) or
        IsSplitMod(ELexm1:ELexm3:ELaxp1:ELaxp3:ELemd1:ELemd3)   or
        IsSplitMod(ELexm2:ELexm3:ELaxp2:ELaxp3:ELemd2:ELemd3);
          *in89 = *off;
       Endif;
       If ELNoExm1 = 'Y' and ELExm2 <> 0;
          *in89 = *off;
       Endif;
      /end-free
     C                   ENDIF
     C     *IN89         IFEQ      *ON
     C                   MOVEL     CON(4)        HDG(A)
     C                   CLEAR                   XXASP1
     C                   IF        XXMINF = 'Y'
     C     MPAXP1        IFNE      *ZERO
     C                   MOVE      ELEXM1        XXASP1
     C                   ELSE
     C     MPAXP2        IFNE      *ZERO
     C                   MOVE      ELEXM2        XXASP1
     C                   ELSE
     C                   MOVE      ELEXM3        XXASP1
     C                   ENDIF
     C                   ENDIF
     C                   ELSE
     C     ELAXP1        IFNE      *ZERO
     C                   MOVE      ELEXM1        XXASP1
     C                   ELSE
     C     ELAXP2        IFNE      *ZERO
     C                   MOVE      ELEXM2        XXASP1
     C                   ELSE
     C                   MOVE      ELEXM3        XXASP1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXASP1        IFEQ      '00000'
     C                   MOVE      ELEXM1        XXASP1
     C                   ENDIF                                                  XXASP1 IFEQ
     C*
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      NUM(1)        PCT(2)
     C                   MOVE      NUM(2)        PCT(4)
     C                   MOVE      NUM(3)        PCT(5)
     C                   MOVE      NUM(4)        PCT(6)
     C                   MOVE      NUM(5)        PCT(7)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPAMPM        TAM(A)
     C                   ELSE
     C                   Z-ADD     ELAMPM        TAM(A)
     C                   ENDIF
     C     ELAMNP        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C     ELWVFL        ANDNE     'J'
     C                   IF        XXMINF <> 'Y'
     C                   SUB       ELAWAV        TAM(A)
     C                   ENDIF
     C                   ENDIF
     C  N89              MOVEL     CON(13)       HDG(A)
     C     ELPROM        IFEQ      'M'
     C                   MOVEL     CON(34)       HDG(A)
     C                   ENDIF
     C     A             SUB       1             B                 2 0
     C                   MOVE      *ON           *IN(B)
     C                   ADD       2             A
     C*
     C                   ENDIF
     C*
     C*  CONTRACTORS CREDIT
     C*
     C     ELADC1        IFNE      *ZERO                                        CONTR CLASS
     C     XXMINF        OREQ      'Y'
     C     ELASP1        ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADC1        TAM(A)
     C     ELADC1        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADC1        TAM(A)
     C     MPADC1        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   ENDIF
     C     ELCREF        IFNE      *ZERO
     C                   Z-ADD     ELCREF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELCREX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELCREX        IFNE      *ZERO
     C                   Z-ADD     ELCREX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     c                   if        rrcchd <> ' '
     c                   movel     rrcchd        hdg(a)
     c                   else
     c                   movel     con(2)        hdg(a)
     c                   endif
     C                   MOVE      ELASP1        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   END
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ELSE
     C                   END
     C*
     C*  SCHEDULE DB/CR
     C*
     C*
     C*
     C* MOVE DEFAULT HEADING AT FIRST
     C* CHECK DEBIT HEADING
     C* CHECK CREDIT HEADING
     C     RRDCAP        IFEQ      ' '
     C                   EXSR      CALDBR
     C                   ENDIF                                                  ELDBCR IFNE
     C*
     C*  LOAD DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
     C*
     C     RRDV1L        IFEQ      'E'
     C*
     C     ELADC2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELASP2        ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADC2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADC2        TAM(A)
     C                   ENDIF
     C     ELASP2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C     RRDV1H        IFEQ      *BLANK
     C                   MOVEL     CON(6)        HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV1H        HDG(A)
     C                   ENDIF
     C     ELDVEF        IFNE      *ZERO
     C                   Z-ADD     ELDVEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELDVEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELDVEX        IFNE      *ZERO
     C                   Z-ADD     ELDVEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELASP2        IFLT      0
      *
     C     ELASP2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELASP2        XXASP2            5
     C                   ENDIF                                                  ELASP2
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDC2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     C     RRDV1L        IFEQ      'E'
     C*
     C     ELD1A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD1S2        ANDNE     *ZERO
     C                   Z-ADD     ELD1A2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD1A2        TAM(A)
     C                   ENDIF
     C     ELD1S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C     RRDV1H        IFEQ      *BLANK
     C     con(6)        CAT       @2nper:1      HDG(A)
     C                   ELSE
     C     rrdv1h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD1D2        IFNE      *ZERO
     C                   Z-ADD     ELD1D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD1X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD1X2        IFNE      *ZERO
     C                   Z-ADD     ELD1X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELD1S2        IFLT      0
     C     ELD1S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP2            5
     C                   ENDIF                                                  ELD1S2
     C*
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELD1A2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C*  LOAD 2ND DEVIATION IF APPLIED TO EXPERIENCE PREMIUM
     C*
     C     RRDV2L        IFEQ      'E'
     C*
     C*
     C     ELADV2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELDEV2        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDEV2F       ANDNE     *ZERO
     C                   Z-ADD     ELADV2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADV2        TAM(A)
     C                   ENDIF
     C                   Select
     C                   When      ELdev2  <> 0
     C                   If        ELdev2 < *Zeros
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELstFc  <> 0
     C                   If        ELstFc > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELdev2F <> 0
     C                   If        ELdev2F > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   EndSl
     C*
     C     RRDV2H        IFEQ      *BLANK
     C  N25              MOVEL     CON(19)       HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV2H        HDG(A)
     C                   ENDIF
     C     ELD2EF        IFNE      *ZERO
     C                   Z-ADD     ELD2EF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2EX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2EX        IFNE      *ZERO
     C                   Z-ADD     ELD2EX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
      *
      *
      *
      *
      *
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stabilization Factor
     C                   When      cltID = '025'
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
      *
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      '.'           PCT(2)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   MOVE      ELdev2F       XXASP3
     C                   MOVEA     XXASP3        NUM

     C                   MOVE      NUM(1)        PCT(2)
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      NUM(2)        PCT(4)
     C                   MOVE      NUM(3)        PCT(5)
     C                   MOVE      NUM(4)        PCT(6)
     C                   MOVE      NUM(5)        PCT(7)
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     ELDEV2        IFLT      0
      *
     C     ELDEV2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP2            5
     C                   ENDIF                                                  ELEDV2
     C                   MOVEA     XXASP2        NUM
      *
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVE      '%'           PCT(7)
     C                   EndSl
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELDEV2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO EXPERIENCE PREMIUM
     C     RRDV2L        IFEQ      'E'
     C*
     C     ELD2A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     XXCLT#        ANDNE     '025'
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXCLT#        ANDEQ     '025'
     C                   Z-ADD     ELD2A2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD2A2        TAM(A)
     C                   ENDIF
     C     XXCLT#        IFNE      '025'
     C     ELD2S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     ELSTFC        IFGT      1
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  CLTID <> 025
     C*
     C     RRDV2H        IFEQ      *BLANK
     C  N25con(19)       CAT       @2nper:1      HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C     rrdv2h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD2D2        IFNE      *ZERO
     C                   Z-ADD     ELD2D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2X2        IFNE      *ZERO
     C                   Z-ADD     ELD2X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C     XXCLT#        IFNE      '025'
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP2            5
     C                   ENDIF                                                  ELD2S2
     C*
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C*FOR CLIENT 025
     C                   ELSE
     C                   MOVE      '.'           PCT(2)
     C                   MOVE      ELSTFC        XXAS8             8
     C                   MOVEA     XXAS8         NUM8
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C                   ENDIF                                                  CLTID <> 025
     C*
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELD2A2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C     RRDCAP        IFEQ      'Y'
     C                   EXSR      CALDBR
     C                   ENDIF
     C*
     C* WAIVER SURCHARGE
     C*
     C     ELWVFL        IFEQ      'E'
     C*
      *** Beg Chg *****  02/28/25  *******************************************
     C*****ELAWAV        IFNE      *ZERO
     C                   If        (ELAWAV <> 0 and XXMINF <> 'Y') OR
     C                             (MPAWAV <> 0 and XXMINF = 'Y')
      *** End Add *****  02/28/25  *******************************************
     C*
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(32)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
     C*
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   ENDIF
      *** Beg Add *****  05/12/25  *******************************************
      /Free
       // Print an '*' appended to heading on the line item
       If ELPror > 0 and ELPror < 1;
          If xxminf = 'Y';
             HDG(A) = %trim(HDG(A)) + '*';
             PrintFlg = 'Y';
          Endif;
       Endif;
      /End-Free
      *** End Add *****  05/12/25  *******************************************
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  RRAWAV IFNE
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C*  ADDITIONAL LOAD
     C*
     C     RRASFL        IFEQ      'E'
     C     XTADLD        IFNE      *ZERO
     C                   MOVEL     CON(18)       HDG(A)
     C   49              Eval      HDG(A) = 'Stop Gap Premium'
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
     C                   ENDIF                                                  RRASFL IFEQ
     C     XXLRGD        IFEQ      'Y'
     C*
     C*  1st Large Deductible applied to MODIFIED
     C*
     C     LDPLV1        IFEQ      'E'
     C*
     C     ELADCT        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C     LDEFD1        IFNE      *ZERO
     C                   Z-ADD     LDEFD1        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     LDEXP1        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     LDEXP1        IFNE      *ZERO
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   Z-ADD     ELADCT        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCT        TAM(A)
     C                   ENDIF
     C     ELADCT        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   MOVEL     CON(79)       HDG(A)
     C*
     C                   MOVE      LDPCT1        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C                   If        cltid = '150' and ELManP <> 0
     c                   Z-add     ldpct1        xxpct150
     c                   move      *blanks       xxpct1
     c                   movel     xxp1150       xxpct1
     c                   cat       '.':0         xxpct1
     c                   cat       xxp2150:0     xxpct1
     c                   cat       '%':0         xxpct1
     C                   Endif
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV1 IFEQ 'E'
     c*
     c*  2nd Large Deductible applied to MODIFIED
     c*
     C     LDPLV2        IFEQ      'E'
     C*
     C     ELADD2        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C     LDEFD2        IFNE      *ZERO
     C                   Z-ADD     LDEFD2        SRDTI
     C                   ELSE
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C*
     C                   Z-ADD     LDEXP2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C*
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   Z-ADD     ELADD2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADD2        TAM(A)
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   MOVEL     CON(80)       HDG(A)
     C*
     C                   MOVE      LDPCT2        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV2 IFEQ 'E'
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*
     C*  CONTRACTORS CREDIT
     C*
     C*
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO STANDARD PREMIUMS
     C*
     C     RRRDPR        IFEQ      'S'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     ELADCT        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDDCT        ANDNE     *ZERO
     C*
     C     ELDCEF        IFNE      *ZERO
     C                   Z-ADD     ELDCEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELDCEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELDCEX        IFNE      *ZERO
     C                   Z-ADD     ELDCEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C                   Z-ADD     ELADCT        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCT        TAM(A)
     C                   ENDIF
     C     ELADCT        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   MOVEL     CON(14)       HDG(A)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   END
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDD2P        ANDNE     *ZERO
     C*
     C     EL2DEF        IFNE      *ZERO
     C                   Z-ADD     EL2DEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELDCEX        SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C*
     C                   Z-ADD     ELDCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C*
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C                   Z-ADD     ELADD2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADD2        TAM(A)
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   MOVEL     CON(47)       HDG(A)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C     ELLOSS        IFNE      *ZERO
     C                   MOVEL     CON(35)       HDG(A)
     C                   Z-ADD     ELLOSS        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPALOS        TAM(A)
     C                   ENDIF
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
      *
     C     XTOCCD        IFNE      *ZERO
     C                   MOVEL(P)  CON(39)       HDG(A)
     C                   Z-ADD     XTOCCD        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
      /Free
       If XTnrat <> *Zero;
          HDG(A) = Con(91);
          TAM(A) = XTnrat;
          Sgn(A) = '+';
          A = A + 1;
       Endif;
      /End-Free
     C*
     C* APPLY SHORT RATE PENALTY TO STANDARD PREMIUM.
     C*
     C     XXSRPR        IFEQ      'Y'
     C     RRSRAF        ANDEQ     'S'
     C*
     C                   Z-ADD     ELSRAM        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   MOVEA     CON(63)       TMP
     C                   OTHER
     C                   MOVEA     CON(26)       TMP
     C                   ENDSL
     C*
     C                   MOVEA     XXPROR        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVE      '%'           PCT(7)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C*
     C                   MOVE      FACTY         XXASP7            7
     C                   MOVEA     XXASP7        NUM7
     C                   MOVE      'X'           TMP(30)
     C                   MOVE      '.'           TMP(24)
     C     NUM7(1)       IFNE      *ZERO
     C                   MOVE      NUM7(1)       TMP(21)
     C                   ENDIF
     C     NUM7(1)       IFNE      *ZERO
     C     NUM7(2)       ORNE      *ZERO
     C                   MOVE      NUM7(2)       TMP(22)
     C                   ENDIF
     C                   MOVE      NUM7(3)       TMP(23)
     C                   MOVE      NUM7(4)       TMP(25)
     C                   MOVE      NUM7(5)       TMP(26)
     C                   MOVE      NUM7(6)       TMP(27)
     C                   MOVE      NUM7(7)       TMP(28)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF                                                  *IN85 IFEQ
      *** Beg Chg ***  12/05/23  *******************************************
     C*
     C* BALANCE TO MINIMUM PREMIUM - prints before standard and adds in to increase it.
     C*
      /Free
       tasmin = 0;
       If ELAMNP = 'Y' and EMSTRP = 'Y';
          tasMin = MPABal;
          HDG(A) = 'Balance to Minimum Premium';
          TAM(A) = tasmin;
          //*** Beg Add *****  05/12/25  *******************************************
          // Print an '*' appended to heading on the line item
          If ELPror > 0 and ELPror < 1;
             If (ELcofl <> 'Y' and xxst <> 'NJ');
                HDG(A) = %trim(HDG(A)) + '*';
                PrintFlg = 'Y';
             Endif;
          Endif;
          //*** End Add *****  05/12/25  *******************************************
          A = A + 1;
       Endif;
      /End-free
      *** End Chg ***  12/05/23  *******************************************
     C*
     C*  STANDARD PREMIUM
     C*
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
     C                   MOVEL     CON(60)       HDG(A)
     C                   OTHER
     C                   MOVEL     CON(5)        HDG(A)                         STD PREM
     C                   ENDSL
     C                   Z-ADD     ELASPR        TAM(A)
     C     XXMINF        IFEQ      'Y'
     C     ELWVFL        ANDNE     'M'
     C     ELWVFL        ANDNE     'J'
     C     ELWVFL        ANDNE     'E'
     C                   SUB       ELAWAV        TAM(A)
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPASPR        TAM(A)
      *** Beg Add ***  12/05/23  *******************************************
     C                   Add       tasmin        TAM(A)
      *** End Add ***  12/05/23  *******************************************
     C                   ENDIF
     C     A             SUB       1             B                 2 0
     C                   MOVE      *ON           *IN(B)
     C                   ADD       2             A
     C*
     C*  LOAD DEVIATION IF APPLIED TO STANDARD PREMIUM
     C*
     C     RRDV1L        IFEQ      'S'
     C*
     C     ELADC2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELASP2        ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADC2        TAM(A)
     C     ELASP2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADC2        TAM(A)
     C                   ENDIF
     C*
     C     RRDV1H        IFEQ      *BLANK
     C                   MOVEL     CON(6)        HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV1H        HDG(A)
     C                   ENDIF
     C     ELDVEF        IFNE      *ZERO
     C                   Z-ADD     ELDVEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELDVEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELDVEX        IFNE      *ZERO
     C                   Z-ADD     ELDVEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELASP2        IFLT      0
      *
     C     ELASP2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELASP2        XXASP2            5
     C                   ENDIF                                                  ELASP2
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDC2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     C     RRDV1L        IFEQ      'S'
     C*
     C     ELD1A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD1S2        ANDNE     *ZERO
     C                   Z-ADD     ELD1A2        TAM(A)
     C     ELD1S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD1A2        TAM(A)
     C                   ENDIF
     C*
     C     RRDV1H        IFEQ      *BLANK
     C     con(6)        CAT       @2nper:1      HDG(A)
     C                   ELSE
     C     rrdv1h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD1D2        IFNE      *ZERO
     C                   Z-ADD     ELD1D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD1X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD1X2        IFNE      *ZERO
     C                   Z-ADD     ELD1X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELD1S2        IFLT      0
     C     ELD1S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP2            5
     C                   ENDIF                                                  ELD1S2
     C*
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELD1A2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*  LOAD 2ND DEVIATION IF APPLIED TO STANDARD PREMIUM
     C*
     C     RRDV2L        IFEQ      'S'
     C*
     C     ELADV2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELDEV2        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDEV2F       ANDNE     *ZERO
     C                   Z-ADD     ELADV2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADV2        TAM(A)
     C                   ENDIF
     C                   Select
     C                   When      ELdev2  <> 0
     C                   If        ELdev2 < *Zeros
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELstFc  <> 0
     C                   If        ELstFc > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELdev2F <> 0
     C                   If        ELdev2F > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   EndSl
     C*
     C     RRDV2H        IFEQ      *BLANK
     C  N25              MOVEL     CON(19)       HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV2H        HDG(A)
     C                   ENDIF
     C     ELD2EF        IFNE      *ZERO
     C                   Z-ADD     ELD2EF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2EX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2EX        IFNE      *ZERO
     C                   Z-ADD     ELD2EX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
      *
      *
      *
      *
      *
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stabilization Factor
     C                   When      cltID = '025'
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
      *
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      '.'           PCT(2)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   MOVE      ELdev2F       XXASP3
     C                   MOVEA     XXASP3        NUM

     C                   MOVE      NUM(1)        PCT(2)
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      NUM(2)        PCT(4)
     C                   MOVE      NUM(3)        PCT(5)
     C                   MOVE      NUM(4)        PCT(6)
     C                   MOVE      NUM(5)        PCT(7)
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     ELDEV2        IFLT      0
      *
     C     ELDEV2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP2            5
     C                   ENDIF                                                  ELEDV2
     C                   MOVEA     XXASP2        NUM
      *
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVE      '%'           PCT(7)
     C                   EndSl
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDV2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO STANDARD PREMIUM
     C     RRDV2L        IFEQ      'S'
     C*
     C     ELD2A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     XXCLT#        ANDNE     '025'
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXCLT#        ANDEQ     '025'
     C                   Z-ADD     ELD2A2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD2A2        TAM(A)
     C                   ENDIF
     C     XXCLT#        IFNE      '025'
     C     ELD2S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     ELSTFC        IFGT      1
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   ENDIF
     C*
     C     RRDV2H        IFEQ      *BLANK
     C  N25con(19)       CAT       @2nper:1      HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C     rrdv2h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD2D2        IFNE      *ZERO
     C                   Z-ADD     ELD2D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2X2        IFNE      *ZERO
     C                   Z-ADD     ELD2X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C     XXCLT#        IFNE      '025'
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP2            5
     C                   ENDIF                                                  ELD2S2
     C*
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C*FOR CLIENT 025
     C                   ELSE
     C                   MOVE      '.'           PCT(2)
     C                   MOVE      ELSTFC        XXAS8             8
     C                   MOVEA     XXAS8         NUM8
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C                   ENDIF                                                  CLTID <>025
     C*
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDV2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*
     C* WAIVER SURCHARGE
     C*
     C     ELWVFL        IFEQ      'S'
     C*
      *** Beg Chg *****  02/28/25  *******************************************
     C*****ELAWAV        IFNE      *ZERO
     C                   If        (ELAWAV <> 0 and XXMINF <> 'Y') OR
     C                             (MPAWAV <> 0 and XXMINF = 'Y')
      *** End Add *****  02/28/25  *******************************************
     C*
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(32)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
     C*
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   ENDIF
      *** Beg Add *****  05/12/25  *******************************************
      /Free
       // Print an '*' appended to heading on the line item
       If ELPror > 0 and ELPror < 1;
          If xxminf = 'Y';
             HDG(A) = %trim(HDG(A)) + '*';
             PrintFlg = 'Y';
          Endif;
       Endif;
      /End-Free
      *** End Add *****  05/12/25  *******************************************
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  RRAWAV IFNE
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADDITIONAL LOAD
     C*
     C     RRASFL        IFEQ      'S'
     C*
     C     XTADLD        IFNE      *ZERO
     C                   MOVEL     CON(18)       HDG(A)
     C   49              Eval      HDG(A) = 'Stop Gap Premium'
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRASFL IFEQ
     C     XXLRGD        IFEQ      'Y'
     C*
     C*  1st Large Deductible applied to STANDARD
     C*
     C     LDPLV1        IFEQ      'S'
     C*
     C     ELADCT        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C     LDEFD1        IFNE      *ZERO
     C                   Z-ADD     LDEFD1        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     LDEXP1        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     LDEXP1        IFNE      *ZERO
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   Z-ADD     ELADCT        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCT        TAM(A)
     C                   ENDIF
     C     ELADCT        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   MOVEL     CON(79)       HDG(A)
     C*
     C                   MOVE      LDPCT1        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV1 IFEQ 'S'
     c*
     c*  2nd Large Deductible applied to STANDARD
     c*
     C     LDPLV2        IFEQ      'S'
     C*
     C     ELADD2        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C     LDEFD2        IFNE      *ZERO
     C                   Z-ADD     LDEFD2        SRDTI
     C                   ELSE
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C*
     C                   Z-ADD     LDEXP2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C*
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   Z-ADD     ELADD2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADD2        TAM(A)
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   MOVEL     CON(80)       HDG(A)
     C*
     C                   MOVE      LDPCT2        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV2 IFEQ 'S'
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
     C*
     C*   CALCULATE STOP GAP FEES.
     C*
     C                   Z-ADD     *ZERO         XXSTOP           11 2
     C                   Z-ADD     *ZERO         XPSTOP           11 2
     C*
     C     CLTID         IFEQ      '109'
     C*
     C                   Z-ADD     *ZERO         XXGAMT
     C                   MOVE      *BLANKS       XXGHDG
     C*
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'G'
     C                   ADD       EAAMT         XXSTOP
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'G'
     C                   ADD       EAAMT         XPSTOP
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C* PRINT LINE ITEM IF THERE ARE STOP GAP FEES BILLED.
     C*
     C     XXSTOP        IFNE      *ZERO
     C                   MOVEL(P)  GHDG(2)       XXGHDG
     C                   Z-ADD     XXSTOP        XXGAMT
     C                   ENDIF                                                  XXSTOP IFNE
     C*
     C                   ADD       XXSTOP        XXDPDU               4142
     C                   ADD       XPSTOP        XXDPRL
     C*
     C                   ENDIF                                                  CLTID = 109
     C*
     C*   CALCULATE MEMBERSHIP DUES PAID
     C*
     C                   Z-ADD     *ZERO         XXMFEE
     C     CLTID         IFEQ      '046'
     C                   Z-ADD     *ZERO         XXMFPD            9 2
     C                   ENDIF
     C                   MOVE      *OFF          *IN52
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'M'                                          MEMBERSHIP FEES
     C                   ADD       EAAMT         XXMFEE            9 2
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C*   CALCULATE MEMBERSHIP DUES. PRORATE IT IF INDICATED.
     C*
     C                   Z-ADD     *ZERO         XXMDUE            7 2
     C     SVCNCL        IFEQ      'Y'
     C     F1CXAD        ANDEQ     'Y'
     C     SVCNCL        OREQ      ' '
     C     F1YRAD        ANDEQ     'Y'
     C     F1PMFL        IFEQ      'Y'                                          PRORATE MEMBER FEE
     C     PDAYS         ANDNE     FDAYS
     C                   Z-ADD     *ZERO         XXMPRO            5 4
     C     FDAYS         IFNE      *ZERO
     C     PDAYS         DIV(H)    FDAYS         XXMPRO
     C                   ENDIF                                                  IF FDAYS <> 0
     C     F1MFEE        MULT(H)   XXMPRO        XXMDUE
     C                   ELSE
     C                   Z-ADD     F1MFEE        XXMDUE                         BILL FULL MEMBER FEE
     C                   ENDIF                                                  IF F1PMFL = Y
     C                   Z-ADD(H)  XXMDUE        XXWK10           11 0
     C                   Z-ADD     XXWK10        XXMDUE
     C                   ELSE
     C*
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'M'
     C                   ADD       EAAMT         XXMDUE
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C                   ENDIF                                                  SVCNCL IFEQ
     C                   Z-ADD     ELMFAM        XXMDUE
     C                   Z-ADD     0             XXINST           11 2
     C                   Z-ADD     0             XXINPD           11 2
     C                   MOVE      *OFF          *IN67
     C     RRKY          SETGT     PIF01
     C     RRKY2         READPE    PIF01                                  60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C     EAARCD        IFEQ      IFIFPD                                                   EES
     C     EAARCD        OREQ      IFIFTR                                                   EES
     C     EAARCD        OREQ      IFIFTC                                                   EES
     C                   ADD       EAAMT         XXINPD
     C                   ENDIF
     C*
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C     EAARCD        IFEQ      IFIFBL                                                   EES
     C                   ADD       EAAMT         XXINST
     C                   ENDIF
     C*
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C                   ELSE
     C                   MOVE      *BLANK        IFIFBL
     C                   MOVE      *BLANK        IFIFPD
     C                   MOVE      *BLANK        IFIFTR
     C                   MOVE      *BLANK        IFIFTC
     C                   Z-ADD     0             IFIFEE
     C                   ENDIF
     C*
     C* CALCULATE FEES BILLED.
     C*
     C                   Z-ADD     *ZERO         XXFEEB           11 2
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFNE      'M'
     C     TTCTYP        ANDNE     'S'
     C     TTCTYP        ANDNE     '1'
     C     TTCTYP        ANDNE     '2'
     C     TTCTYP        ANDNE     '3'
     C     TTCTYP        ANDNE     '4'
     C     TTCTYP        ANDNE     '5'
     C     TTCTYP        ANDNE     '6'
     C     TTCTYP        ANDNE     '7'
     C     TTCTYP        ANDNE     'T'
     C     EAARCD        ANDNE     IFIFBL
     C     TTCTYP        ANDNE     'R'
     C     XXIDBD        ANDEQ     *BLANK
     C     TTCTYP        OREQ      'R'
     C     XXIDBD        ANDEQ     'Y'
     C     CLTID         IFEQ      '046'
     C     EAARCD        ANDNE     'NB'
     C     EAARCD        ANDNE     'LF'
     C     EAARCD        ANDNE     'LW'
     C     CLTID         ORNE      '046'
     C                   ADD       EAAMT         XXFEEB
     C                   ENDIF                                                  CLTID IFEQ
     C                   ENDIF                                                  TTCTYP IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     CLTID         IFNE      '109'
     C     CLTID         ANDNE     '102'
     C     CLTID         ANDNE     '046'
     C                   MOVE      *BLANKS       XXGHDG           20            GENERIC HEADING
     C                   MOVE      *ZEROS        XXGAMT           11 2
     C                   ENDIF                                                  CLTID = 133
     C     CLTID         IFNE      '109'
     C     CLTID         ANDNE     '102'
     C     XXFEEB        ANDNE     *ZERO
     C     CLTID         OREQ      '046'
     C                   MOVEL     GHDG(1)       XXGHDG           20
     C                   Z-ADD     XXFEEB        XXGAMT
     C                   ADD       XXFEEB        XXDPDU               4142
     C                   ENDIF
     C*
     C*
     C*   CALCULATE SAFETY FEES
     C*
     C                   Z-ADD     *ZERO         XXSDUE           11 2
     C                   Z-ADD     *ZERO         XXSFEE           11 2
     C     CLTID         IFEQ      '046'
     C                   Z-ADD     *ZERO         XXSFPD           11 2
     C                   ENDIF
     C                   MOVE      *OFF          *IN53
     C*
     C     ELSFPC        IFNE      *ZERO
     C     ELSFMN        ORNE      *ZEROS
     C     RRSFHD        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(36)       XXSFHD           20
     C                   ELSE
     C                   MOVEL(P)  RRSFHD        XXSFHD
     C                   ENDIF
     C*
     C                   MOVE      *BLANK        XSCALC            1
     C     XXCALC        IFEQ      'Y'
     C     SVCNCL        IFEQ      'Y'
     C     RRCXAD        ANDEQ     'Y'
     C     SVCNCL        OREQ      ' '
     C     RRYRAD        ANDEQ     'Y'
     C                   MOVE      'Y'           XSCALC
     C                   ENDIF
     C                   ENDIF
     C     XSCALC        IFEQ      'Y'
     C*
     C                   Z-ADD     0             XXPREM           15 4
     C                   SELECT
     C     RRSFAP        WHENEQ    'J'
     C                   Z-ADD     ELASBJ        XXPREM
     C     RRSFAP        WHENEQ    'E'
     C                   Z-ADD     ELAFRS        XXPREM
     C     RRSFAP        WHENEQ    'S'
     C                   Z-ADD     ELASPR        XXPREM
     C     RRSFAP        WHENEQ    'D'
     C                   Z-ADD     ELANMP        XXPREM
     C     RRDV1L        IFEQ      'D'
     C                   ADD       ELADC2        XXPREM
     C                   ENDIF
     C     RRDV2L        IFEQ      'D'
     C                   ADD       ELADV2        XXPREM
     C                   ENDIF
     C                   ENDSL                                                  RRSFAP SELC
     C*
     C     ELSFPC        MULT      XXPREM        XXSFAM           15 4
     C     XXSFAM        DIV(H)    100           XXSDUE
     C*
     C     RRSPFL        IFEQ      'Y'
     C                   MULT(H)   ELPROR        XXSDUE
     C                   ENDIF                                                  RRSPFL IFEQ
     C*
     C     XXSDUE        IFLT      ELSFMN
     C                   Z-ADD     ELSFMN        XXSDUE
     C                   ENDIF                                                  XXSFEE IFLT
     C*
     C                   Z-ADD(H)  XXSDUE        XXWK10
     C                   Z-ADD     XXWK10        XXSDUE
     C*
     C                   ELSE
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'S'                                          SAFETY FEES
     C                   ADD       EAAMT         XXSDUE
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C                   ENDIF                                                  XXCALC IFEQ
     C*
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'S'                                          SAFETY FEES
     C                   ADD       EAAMT         XXSFEE
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     XXSDUE        SUB       XXSFEE        XXSFDU           11 2
     C*
     C                   ENDIF                                                  ELSFPC IFNE
     C                   Z-ADD     ELSFAM        XXSDUE
     C*
     C*   CALCULATE 1ST ASSESSMENT
     C*
     C                   Z-ADD     *ZERO         XX1STB           11 2
     C                   MOVE      *OFF          *IN55
     C                   MOVE      *BLANKS       XX1STP            9
     C                   Z-ADD     RDAT(1)       XXEFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   If        xspfdt <> 0
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   Endif
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C     RR1STP        IFNE      *ZERO
     C     EL1AAM        ORNE      *ZERO
     C*
     C     RR1STP        IFEQ      *ZERO
     C     RR1STF        ANDEQ     *ZERO
     C     RDAT(2)       ANDNE     *ZERO
     C*
     C     XXST          IFEQ      'PA'
     C                   Z-ADD     RDAT(2)       XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  RR1STP IFEQ
     C*
     C     RR1STH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(27)       XX1STH           20
     C                   ELSE
     C                   MOVEL(P)  RR1STH        XX1STH
     C                   ENDIF
     C                   MOVE      *BLANKS       X1ST              7
     C     RR1STF        IFNE      *ZERO
     C     EL1FPR        ANDNE     *ZERO
     C     EL1APR        ADD       EL1FPR        XXPREM
     C     EL1APR        DIV       XXPREM        XX1P              9 6
     C     EL1FPR        DIV       XXPREM        XX1F              9 6
     C                   MULT(H)   RR1STP        XX1P
     C                   MULT(H)   RR1STF        XX1F
     C                   ADD(H)    XX1P          XX1F
     C                   Z-ADD(H)  XX1F          X196              9 6
     C                   ELSE
     C                   MOVE      RR1STP        X196
     C                   ENDIF                                                  RR1STF IFNE
     C                   MOVEA     X1ST          NUM7
     C                   MOVE      '.'           PCT9(4)
     C                   MOVE      '%'           PCT9(9)
     C     NUM7(1)       IFNE      '0'
     C                   MOVE      NUM7(1)       PCT9(1)
     C                   ENDIF
     C     NUM7(2)       IFNE      '0'
     C     NUM7(1)       ORGT      '0'
     C                   MOVE      NUM7(2)       PCT9(2)
     C                   ENDIF
     C                   MOVE      NUM7(3)       PCT9(3)
     C                   MOVE      NUM7(4)       PCT9(5)
     C                   MOVE      NUM7(5)       PCT9(6)
     C                   MOVE      NUM7(6)       PCT9(7)
     C                   MOVE      NUM7(7)       PCT9(8)
     C                   MOVEA     PCT9          XX1STP
     C                   CLEAR                   PCT9
     C*
     C     XXCALC        IFEQ      'Y'
     C*
     C                   Z-ADD     EL1APR        XXPREM           15 4
     C     RR1STF        MULT      EL1FPR        XXAMT3           15 4
     C     RR1STP        MULT      EL1APR        XXPREM
     C                   ADD       XXPREM        XXAMT3
     C     XXAMT3        DIV(H)    100           XX1STD           11 0
     C                   Z-ADD     XX1STD        XX1STB
     C*
     C                   ELSE
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '1'
     C                   ADD       EAAMT         XX1STB
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C                   ENDIF                                                  XXCALC IFEQ
     C*
     C*
     C*
     C                   Z-ADD     EL1AAM        XX1STB
     C                   ENDIF                                                  RR1STP IFNE
     C     CLTID         IFEQ      '046'
     C                   Z-ADD     0             XX1SPD           11 2
     C                   ENDIF
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '1'
     C                   ADD       EAAMT         XXDPRL
     C     CLTID         IFEQ      '102'
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
     C     CLTID         IFEQ      '046'
     C                   ADD       EAAMT         XX1SPD           11 2
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     XX1STB        IFNE      *ZERO
     C                   MOVE      *ON           *IN55
     C                   ENDIF
     C*
     C*   CALCULATE 2ND ASSESSMENT
     C*
     C                   Z-ADD     *ZERO         XX2NDB           11 2
     C                   MOVE      *OFF          *IN56
     C                   MOVE      *BLANKS       XX2NDP            9
     C                   Z-ADD     RDAT(1)       XXEFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   If        xspfdt <> 0
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   Endif
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C     RR2NDP        IFNE      *ZERO
     C     EL2AAM        ORNE      *ZERO
     C*
     C     RR2NDP        IFEQ      *ZERO
     C     RR2NDF        ANDEQ     *ZERO
     C     RDAT(2)       ANDNE     *ZERO
     C*
     C     XXST          IFEQ      'PA'
     C                   Z-ADD     RDAT(2)       XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  RR2NDP IFEQ
     C*
     C     RR2NDH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(28)       XX2NDH           20
     C                   ELSE
     C                   MOVEL(P)  RR2NDH        XX2NDH
     C                   ENDIF
     C                   MOVE      *BLANKS       X2ND              7
     C     RR2NDF        IFNE      *ZERO
     C     EL2FPR        ANDNE     *ZERO
     C     EL2APR        ADD       EL2FPR        XXPREM
     C     EL2APR        DIV       XXPREM        XX2P              9 6
     C     EL2FPR        DIV       XXPREM        XX2F              9 6
     C                   MULT(H)   RR2NDP        XX2P
     C                   MULT(H)   RR2NDF        XX2F
     C                   ADD(H)    XX2P          XX2F
     C                   Z-ADD(H)  XX2F          X296              9 6
     C                   ELSE
     C                   MOVE      RR2NDP        X296
     C                   ENDIF                                                  RR2NDF IFNE
     C                   MOVEA     X2ND          NUM7
     C                   MOVE      '.'           PCT9(4)
     C                   MOVE      '%'           PCT9(9)
     C     NUM7(1)       IFNE      '0'
     C                   MOVE      NUM7(1)       PCT9(1)
     C                   ENDIF
     C     NUM7(2)       IFNE      '0'
     C     NUM7(1)       ORGT      '0'
     C                   MOVE      NUM7(2)       PCT9(2)
     C                   ENDIF
     C                   MOVE      NUM7(3)       PCT9(3)
     C                   MOVE      NUM7(4)       PCT9(5)
     C                   MOVE      NUM7(5)       PCT9(6)
     C                   MOVE      NUM7(6)       PCT9(7)
     C                   MOVE      NUM7(7)       PCT9(8)
     C                   MOVEA     PCT9          XX2NDP
     C                   CLEAR                   PCT9
     C*
     C     XXCALC        IFEQ      'Y'
     C*
     C                   Z-ADD     EL2APR        XXPREM           15 4
     C     RR2NDF        MULT      EL2FPR        XXAMT3           15 4
     C     RR2NDP        MULT      EL2APR        XXPREM
     C                   ADD       XXPREM        XXAMT3
     C     XXAMT3        DIV(H)    100           XX2NDD           11 0
     C                   Z-ADD     XX2NDD        XX2NDB
     C*
     C                   ELSE
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '2'
     C                   ADD       EAAMT         XX2NDB
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C                   ENDIF                                                  XXCALC IFEQ
     C*
     C                   Z-ADD     EL2AAM        XX2NDB
     C*
     C                   ENDIF                                                  RR2NDP IFNE
     C     CLTID         IFEQ      '046'
     C                   Z-ADD     0             XX2NPD           11 2
     C                   ENDIF
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '2'
     C                   ADD       EAAMT         XXDPRL
     C     CLTID         IFEQ      '102'
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
     C     CLTID         IFEQ      '046'
     C                   ADD       EAAMT         XX2NPD           11 2
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     XX2NDB        IFNE      *ZERO
     C                   MOVE      *ON           *IN56
     C                   ENDIF
     C*
     C*   CALCULATE 3RD ASSESSMENT
     C*
     C                   Z-ADD     *ZERO         XX3RDB           11 2
     C                   MOVE      *OFF          *IN64
     C                   MOVE      *BLANKS       XX3RDP            9
     C                   Z-ADD     RDAT(1)       XXEFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   If        xspfdt <> 0
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   Endif
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C     RR3RDP        IFNE      *ZERO
     C     EL3AAM        ORNE      *ZERO
     C*
     C     RR3RDP        IFEQ      *ZERO
     C     RR3RDF        ANDEQ     *ZERO
     C     RDAT(2)       ANDNE     *ZERO
     C*
     C     XXST          IFEQ      'PA'
     C                   Z-ADD     RDAT(2)       XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  RR3RDP IFEQ
     C*
     C     RR3RDH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(33)       XX3RDH           20
     C                   ELSE
     C                   MOVEL(P)  RR3RDH        XX3RDH
     C                   ENDIF
     C                   MOVE      *BLANKS       X3RD              7
     C     RR3RDF        IFNE      *ZERO
     C     EL3FPR        ANDNE     *ZERO
     C     EL3APR        ADD       EL3FPR        XXPREM
     C     EL3APR        DIV       XXPREM        XX3P              9 6
     C     EL3FPR        DIV       XXPREM        XX3F              9 6
     C                   MULT(H)   RR3RDP        XX3P
     C                   MULT(H)   RR3RDF        XX3F
     C                   ADD(H)    XX3P          XX3F
     C                   Z-ADD(H)  XX3F          X396              9 6
     C                   ELSE
     C                   MOVE      RR3RDP        X396
     C                   ENDIF                                                  RR3RDF IFNE
     C                   MOVEA     X3RD          NUM7
     C                   MOVE      '.'           PCT9(4)
     C                   MOVE      '%'           PCT9(9)
     C     NUM7(1)       IFNE      '0'
     C                   MOVE      NUM7(1)       PCT9(1)
     C                   ENDIF
     C     NUM7(2)       IFNE      '0'
     C     NUM7(1)       ORGT      '0'
     C                   MOVE      NUM7(2)       PCT9(2)
     C                   ENDIF
     C                   MOVE      NUM7(3)       PCT9(3)
     C                   MOVE      NUM7(4)       PCT9(5)
     C                   MOVE      NUM7(5)       PCT9(6)
     C                   MOVE      NUM7(6)       PCT9(7)
     C                   MOVE      NUM7(7)       PCT9(8)
     C                   MOVEA     PCT9          XX3RDP
     C                   CLEAR                   PCT9
     C*
     C     XXCALC        IFEQ      'Y'
     C*
     C                   Z-ADD     EL3APR        XXPREM           15 4
     C     RR3RDF        MULT      EL3FPR        XXAMT3           15 4
     C     RR3RDP        MULT      EL3APR        XXPREM
     C                   ADD       XXPREM        XXAMT3
     C     XXAMT3        DIV(H)    100           XX3RDD           11 0
     C                   Z-ADD     XX3RDD        XX3RDB
     C*
     C                   ELSE
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '3'
     C                   ADD       EAAMT         XX3RDB
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C                   ENDIF                                                  XXCALC IFEQ
     C*
     C                   Z-ADD     EL3AAM        XX3RDB
     C*
     C                   ENDIF                                                  RR3RDP IFNE
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '3'
     C                   ADD       EAAMT         XXDPRL
     C     CLTID         IFEQ      '102'
     C     CLTID         OREQ      '046'
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     XX3RDB        IFNE      *ZERO
     C                   MOVE      *ON           *IN64
     C                   ENDIF
     C*
     C*
     C*   CALCULATE 4TH ASSESSMENT
     C*
     C                   Z-ADD     *ZERO         XX4THB           11 2
     C                   MOVE      *OFF          *IN91
     C                   MOVE      *BLANKS       XX4THP            9
     C*
     C                   Z-ADD     RDAT(1)       XXEFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   If        xspfdt <> 0
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   Endif
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
      *
     C     RR4THP        IFNE      *ZERO
     C     EL4AAM        ORNE      *ZERO
     C*
     C     RR4THP        IFEQ      *ZERO
     C     RR4THF        ANDEQ     *ZERO
     C     RDAT(2)       ANDNE     *ZERO
     C*
     C     XXST          IFEQ      'PA'
     C                   Z-ADD     RDAT(2)       XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  RR4THP IFEQ
     C*
     C     RR4THH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(40)       XX4THH           20
     C                   ELSE
     C                   MOVEL(P)  RR4THH        XX4THH
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       X4TH              7
     C*
     C     RR4THF        IFNE      *ZERO
     C     EL4FPR        ANDNE     *ZERO
     C     EL4APR        ADD       EL4FPR        XXPREM
     C     EL4APR        DIV       XXPREM        XX4P              9 6
     C     EL4FPR        DIV       XXPREM        XX4F              9 6
     C                   MULT(H)   RR4THP        XX4P
     C                   MULT(H)   RR4THF        XX4F
     C                   ADD(H)    XX4P          XX4F
     C                   Z-ADD(H)  XX4F          X496              9 6
     C                   ELSE
     C                   MOVE      RR4THP        X496
     C                   ENDIF                                                  RR4THF IFNE
     C*
     C                   MOVEA     X4TH          NUM7
     C                   MOVE      '.'           PCT9(4)
     C                   MOVE      '%'           PCT9(9)
     C*
     C     NUM7(1)       IFNE      '0'
     C                   MOVE      NUM7(1)       PCT9(1)
     C                   ENDIF
     C*
     C     NUM7(2)       IFNE      '0'
     C     NUM7(1)       ORGT      '0'
     C                   MOVE      NUM7(2)       PCT9(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM7(3)       PCT9(3)
     C                   MOVE      NUM7(4)       PCT9(5)
     C                   MOVE      NUM7(5)       PCT9(6)
     C                   MOVE      NUM7(6)       PCT9(7)
     C                   MOVE      NUM7(7)       PCT9(8)
     C                   MOVEA     PCT9          XX4THP
     C                   CLEAR                   PCT9
     C*
     C     XXCALC        IFEQ      'Y'
     C                   Z-ADD     EL4APR        XXPREM           15 4
     C     RR4THF        MULT      EL4FPR        XXAMT3           15 4
     C     RR4THP        MULT      EL4APR        XXPREM
     C                   ADD       XXPREM        XXAMT3
     C     XXAMT3        DIV(H)    100           XX4THD           11 0
     C                   Z-ADD     XX4THD        XX4THB
     C*
     C                   ELSE
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '4'
     C                   ADD       EAAMT         XX4THB
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C                   ENDIF                                                  XXCALC IFEQ
     C*
     C                   Z-ADD     EL4AAM        XX4THB
     C*
     C                   ENDIF                                                  RR4THP IFNE
     C*
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '4'
     C                   ADD       EAAMT         XXDPRL
     C     CLTID         IFEQ      '102'
     C     CLTID         OREQ      '046'
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     XX4THB        IFNE      *ZERO
     C                   MOVE      *ON           *IN91
     C                   ENDIF
     C*
     C*   CALCULATE 5TH ASSESSMENT
     C*
     C                   Z-ADD     *ZERO         XX5THB           11 2
     C                   MOVE      *OFF          *IN98
     C                   MOVE      *BLANKS       XX5THP            9
     C                   Z-ADD     RDAT(1)       XXEFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   If        xspfdt <> 0
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   Endif
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C     RR5THP        IFNE      *ZERO
     C     EL5AAM        ORNE      *ZERO
     C*
     C     RR5THP        IFEQ      *ZERO
     C     RR5THF        ANDEQ     *ZERO
     C     RDAT(2)       ANDNE     *ZERO
     C*
     C     XXST          IFEQ      'PA'
     C                   Z-ADD     RDAT(2)       XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  RR5THP IFEQ
     C*
     C     RR5THH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(41)       XX5THH           20
     C                   ELSE
     C                   MOVEL(P)  RR5THH        XX5THH
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       X5TH              7
     C*
     C     RR5THF        IFNE      *ZERO
     C     EL5FPR        ANDNE     *ZERO
     C     EL5APR        ADD       EL5FPR        XXPREM
     C     EL5APR        DIV       XXPREM        XX5P              9 6
     C     EL5FPR        DIV       XXPREM        XX5F              9 6
     C                   MULT(H)   RR5THP        XX5P
     C                   MULT(H)   RR5THF        XX5F
     C                   ADD(H)    XX5P          XX5F
     C                   Z-ADD(H)  XX5F          X596              9 6
     C                   ELSE
     C                   MOVE      RR5THP        X596
     C                   ENDIF                                                  RR4THF IFNE
     C*
     C                   MOVEA     X5TH          NUM7
     C                   MOVE      '.'           PCT9(4)
     C                   MOVE      '%'           PCT9(9)
     C*
     C     NUM7(1)       IFNE      '0'
     C                   MOVE      NUM7(1)       PCT9(1)
     C                   ENDIF
     C*
     C     NUM7(2)       IFNE      '0'
     C     NUM7(1)       ORGT      '0'
     C                   MOVE      NUM7(2)       PCT9(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM7(3)       PCT9(3)
     C                   MOVE      NUM7(4)       PCT9(5)
     C                   MOVE      NUM7(5)       PCT9(6)
     C                   MOVE      NUM7(6)       PCT9(7)
     C                   MOVE      NUM7(7)       PCT9(8)
     C                   MOVEA     PCT9          XX5THP
     C                   CLEAR                   PCT9
     C*
     C     XXCALC        IFEQ      'Y'
     C*
     C                   Z-ADD     EL5APR        XXPREM           15 4
     C     RR5THF        MULT      EL5FPR        XXAMT3           15 4
     C     RR5THP        MULT      EL5APR        XXPREM
     C                   ADD       XXPREM        XXAMT3
     C     XXAMT3        DIV(H)    100           XX5THD           11 0
     C                   Z-ADD     XX5THD        XX5THB
     C*
     C                   ELSE
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '5'
     C                   ADD       EAAMT         XX5THB
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C                   ENDIF                                                  XXCALC IFEQ
     C*
     C                   Z-ADD     EL5AAM        XX5THB
     C*
     C                   ENDIF                                                  RR5THP IFNE
     C*
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '5'
     C                   ADD       EAAMT         XXDPRL
     C     CLTID         IFEQ      '102'
     C     CLTID         OREQ      '046'
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     XX5THB        IFNE      *ZERO
     C                   MOVE      *ON           *IN98
     C                   ENDIF
     C*
     C*
     C*   CALCULATE 6TH ASSESSMENT
     C*
     C                   MOVE      *OFF          PRT6TH            1
     C                   Z-ADD     *ZERO         XX6THB           11 2
     C                   MOVE      *BLANKS       XX6THP            9
     C                   Z-ADD     RDAT(1)       XXEFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   If        xspfdt <> 0
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   Endif
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C     RR6THP        IFNE      *ZERO
     C     EL6AAM        ORNE      *ZERO
     C*
     C     RR6THP        IFEQ      *ZERO
     C     RR6THF        ANDEQ     *ZERO
     C     RDAT(2)       ANDNE     *ZERO
     C*
     C     XXST          IFEQ      'PA'
     C                   Z-ADD     RDAT(2)       XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  RR6THP IFEQ
     C*
     C     RR6THH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(75)       XX6THH           20
     C                   ELSE
     C                   MOVEL(P)  RR6THH        XX6THH
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       X6TH              7
     C*
     C     RR6THF        IFNE      *ZERO
     C     EL6FPR        ANDNE     *ZERO
     C     EL6APR        ADD       EL6FPR        XXPREM
     C     EL6APR        DIV       XXPREM        XX6P              9 6
     C     EL6FPR        DIV       XXPREM        XX6F              9 6
     C                   MULT(H)   RR6THP        XX6P
     C                   MULT(H)   RR6THF        XX6F
     C                   ADD(H)    XX6P          XX6F
     C                   Z-ADD(H)  XX6F          X696              9 6
     C                   ELSE
     C                   MOVE      RR6THP        X696
     C                   ENDIF                                                  RR4THF IFNE
     C*
     C                   MOVEA     X6TH          NUM7
     C                   MOVE      '.'           PCT9(4)
     C                   MOVE      '%'           PCT9(9)
     C*
     C     NUM7(1)       IFNE      '0'
     C                   MOVE      NUM7(1)       PCT9(1)
     C                   ENDIF
     C*
     C     NUM7(2)       IFNE      '0'
     C     NUM7(1)       ORGT      '0'
     C                   MOVE      NUM7(2)       PCT9(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM7(3)       PCT9(3)
     C                   MOVE      NUM7(4)       PCT9(5)
     C                   MOVE      NUM7(5)       PCT9(6)
     C                   MOVE      NUM7(6)       PCT9(7)
     C                   MOVE      NUM7(7)       PCT9(8)
     C                   MOVEA     PCT9          XX6THP
     C                   CLEAR                   PCT9
     C*
     C     XXCALC        IFEQ      'Y'
     C*
     C                   Z-ADD     EL6APR        XXPREM           15 4
     C     RR6THF        MULT      EL6FPR        XXAMT3           15 4
     C     RR6THP        MULT      EL6APR        XXPREM
     C                   ADD       XXPREM        XXAMT3
     C     XXAMT3        DIV(H)    100           XX6THD           11 0
     C                   Z-ADD     XX6THD        XX6THB
     C*
     C                   ELSE
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '6'
     C                   ADD       EAAMT         XX6THB
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C                   ENDIF                                                  XXCALC IFEQ
     C*
     C                   Z-ADD     EL6AAM        XX6THB
     C*
     C                   ENDIF                                                  RR6THP IFNE
     C*
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '6'
     C                   ADD       EAAMT         XXDPRL
     C     CLTID         IFEQ      '102'
     C     CLTID         OREQ      '046'
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     XX6THB        IFNE      *ZERO
     C                   MOVE      *ON           PRT6TH
     C                   ENDIF
     C*
     C*   CALCULATE 7TH ASSESSMENT
     C*
     C                   MOVE      *OFF          PRT7TH            1
     C                   Z-ADD     *ZERO         XX7THB           11 2
     C                   MOVE      *BLANKS       XX7THP            9
     C                   Z-ADD     RDAT(1)       XXEFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   If        xspfdt <> 0
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   Endif
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C     RR7THP        IFNE      *ZERO
     C     EL7AAM        ORNE      *ZERO
     C*
     C     RR7THP        IFEQ      *ZERO
     C     RR7THF        ANDEQ     *ZERO
     C     RDAT(2)       ANDNE     *ZERO
     C*
     C     XXST          IFEQ      'PA'
     C                   Z-ADD     RDAT(2)       XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  RR7THP IFEQ
     C*
     C     RR7THH        IFEQ      *BLANKS
     C                   MOVEL(P)  CON(76)       XX7THH           20
     C                   ELSE
     C                   MOVEL(P)  RR7THH        XX7THH
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       X7TH              7
     C*
     C     RR7THF        IFNE      *ZERO
     C     EL7FPR        ANDNE     *ZERO
     C     EL7APR        ADD       EL7FPR        XXPREM
     C     EL7APR        DIV       XXPREM        XX7P              9 6
     C     EL7FPR        DIV       XXPREM        XX7F              9 6
     C                   MULT(H)   RR7THP        XX7P
     C                   MULT(H)   RR7THF        XX7F
     C                   ADD(H)    XX7P          XX7F
     C                   Z-ADD(H)  XX7F          X796              9 6
     C                   ELSE
     C                   MOVE      RR7THP        X796
     C                   ENDIF                                                  RR4THF IFNE
     C*
     C                   MOVEA     X7TH          NUM7
     C                   MOVE      '.'           PCT9(4)
     C                   MOVE      '%'           PCT9(9)
     C*
     C     NUM7(1)       IFNE      '0'
     C                   MOVE      NUM7(1)       PCT9(1)
     C                   ENDIF
     C*
     C     NUM7(2)       IFNE      '0'
     C     NUM7(1)       ORGT      '0'
     C                   MOVE      NUM7(2)       PCT9(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM7(3)       PCT9(3)
     C                   MOVE      NUM7(4)       PCT9(5)
     C                   MOVE      NUM7(5)       PCT9(6)
     C                   MOVE      NUM7(6)       PCT9(7)
     C                   MOVE      NUM7(7)       PCT9(8)
     C                   MOVEA     PCT9          XX7THP
     C                   CLEAR                   PCT9
     C*
     C     XXCALC        IFEQ      'Y'
     C*
     C                   Z-ADD     EL7APR        XXPREM           15 4
     C     RR7THF        MULT      EL7FPR        XXAMT3           15 4
     C     RR7THP        MULT      EL7APR        XXPREM
     C                   ADD       XXPREM        XXAMT3
     C     XXAMT3        DIV(H)    100           XX7THD           11 0
     C                   Z-ADD     XX7THD        XX7THB
     C*
     C                   ELSE
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '7'
     C                   ADD       EAAMT         XX7THB
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C                   ENDIF                                                  XXCALC IFEQ
     C*
     C                   Z-ADD     EL7AAM        XX7THB
     C*
     C                   ENDIF                                                  RR7THP IFNE
     C*
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '7'
     C                   ADD       EAAMT         XXDPRL
     C     CLTID         IFEQ      '102'
     C     CLTID         OREQ      '046'
     C                   SUB       EAAMT         XXDPRL
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     XX7THB        IFNE      *ZERO
     C                   MOVE      *ON           PRT7TH
     C                   ENDIF
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C                   ADD       XX1STB        XXDPDU
     C                   ADD       XX2NDB        XXDPDU
     C     CLTID         IFNE      '046'
     C                   ADD       XX3RDB        XXDPDU
     C                   ADD       XX4THB        XXDPDU
     C                   ADD       XX5THB        XXDPDU
     C                   ADD       XX6THB        XXDPDU
     C                   ADD       XX7THB        XXDPDU
     C                   ENDIF
     C     CLTID         IFEQ      '102'
     C                   SUB       XX1STB        XXDPDU
     C                   SUB       XX2NDB        XXDPDU
     C                   SUB       XX3RDB        XXDPDU
     C                   SUB       XX4THB        XXDPDU
     C                   SUB       XX5THB        XXDPDU
     C                   SUB       XX6THB        XXDPDU
     C                   SUB       XX7THB        XXDPDU
     C                   ENDIF
     C*
     C*   LOAD MEMBERSHIP DUES
     C*
     C                   ADD       XXMFEE        XXDPRL
     C     CLTID         IFEQ      '046'
     C                   ADD       XXMFEE        XXMFPD
     C                   ENDIF
     C                   Z-ADD     XXMDUE        XXMFEE
     C     XXMFEE        IFNE      *ZERO
     C                   MOVE      *ON           *IN52
     C                   ELSE
     C                   MOVE      *OFF          *IN52
     C                   ENDIF
     C                   ADD       XXMFEE        XXDPDU               4142
     C     CLTID         IFEQ      '102'
     C                   SUB       XXMFEE        XXDPRL
     C                   SUB       XXMFEE        XXDPDU               4142
     C                   ENDIF
     C                   ADD       XXINPD        XXDPRL
     C     XXINST        IFNE      *ZERO
     C                   MOVE      *ON           *IN67
     C                   ELSE
     C                   MOVE      *OFF          *IN67
     C                   ENDIF
     C                   ADD       XXINST        XXDPDU               4142
     C     CLTID         IFEQ      '102'
     C                   SUB       XXINPD        XXDPRL
     C                   SUB       XXINST        XXDPDU               4142
     C                   ENDIF
     C*
     C*   LOAD SAFETY FEES
     C*
     C                   ADD       XXSFEE        XXDPRL
     C     CLTID         IFEQ      '046'
     C                   ADD       XXSFEE        XXSFPD
     C                   ENDIF
     C                   Z-ADD     XXSDUE        XXSFEE
     C     XXSFEE        IFNE      *ZERO
     C                   MOVE      *ON           *IN53
     C                   ELSE
     C                   MOVE      *OFF          *IN53
     C                   ENDIF
     C                   ADD       XXSFEE        XXDPDU               4142
     C     CLTID         IFEQ      '102'
     C*
     C                   SUB       XXSFEE        XXDPRL
     C                   SUB       XXSFEE        XXDPDU               4142
     C*
     C* CALCULATE LATE FEES DUE
     C*
     C                   Z-ADD     0             XXLFEE           11 2
     C                   Z-ADD     0             XXLFPD           11 2
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'L'
     C     TTCTYP        OREQ      'F'
     C     TTCTYP        OREQ      'S'
     C     TTCTYP        OREQ      'T'
     C     TTCTYP        OREQ      'M'
     C     TTCTYP        OREQ      'X'
     C     TTCTYP        OREQ      '1'
     C     TTCTYP        OREQ      '2'
     C     TTCTYP        OREQ      '3'
     C     TTCTYP        OREQ      '4'
     C     TTCTYP        OREQ      '5'
     C     TTCTYP        OREQ      '6'
     C     TTCTYP        OREQ      '7'
     C                   ADD       EAAMT         XXLFEE
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'L'
     C     TTCTYP        OREQ      'F'
     C     TTCTYP        OREQ      'S'
     C     TTCTYP        OREQ      'T'
     C     TTCTYP        OREQ      'M'
     C     TTCTYP        OREQ      'X'
     C     TTCTYP        OREQ      '1'
     C     TTCTYP        OREQ      '2'
     C     TTCTYP        OREQ      '3'
     C     TTCTYP        OREQ      '4'
     C     TTCTYP        OREQ      '5'
     C     TTCTYP        OREQ      '6'
     C     TTCTYP        OREQ      '7'
     C                   ADD       EAAMT         XXLFPD
     C                   ENDIF
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
     C*
     C     XXLFEE        IFNE      *ZERO
     C                   MOVE      *ON           *IN68
     C                   ELSE
     C                   MOVE      *OFF          *IN68
     C                   ENDIF
     C*
     C                   ADD       XXLFEE        XXDPDU               4142
     C                   SUB       XXLFPD        XXDPDU               4142
     C*
     C                   ENDIF                                                  CLTID IFEQ
     C*
     C*  GET LEGAL/NON-SUFFICIENT FEES AND PAYMENTS. CALCULATE BALANCE
     C*
     C     CLTID         IFEQ      '046'                                        CLTID=46
     C                   Z-ADD     *ZERO         XXGFEE           11 2
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C     EAARCD        IFEQ      'NB'
     C     EAARCD        OREQ      'LF'
     C     EAARCD        OREQ      'LW'
     C                   ADD       EAAMT         XXGFEE
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
      *
     C     XXGFEE        IFNE      *ZERO
     C                   MOVE      *OFF          *IN28
     C                   ADD       XXGFEE        XXDPDU
     C                   ELSE
     C                   MOVE      *ON           *IN28
     C                   ENDIF
      *
     C                   Z-ADD     *ZERO         XXGFPD           11 2
     C                   MOVE      'Q'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C*
     C     EAARCD        IFEQ      'NP'
     C     EAARCD        OREQ      'LP'
     C     EAARCD        OREQ      'LR'
     C                   ADD       EAAMT         XXGFPD
     C                   ENDIF
     C     EAKEY         READE     PEA01                                  40
     C                   ENDDO
      *
     C     XXGFPD        IFNE      *ZERO
     C                   ADD       XXGFPD        XXDPRL
     C***                  SUB  XXGFPD    XXDPDU
     C                   ENDIF
      *
     C                   ENDIF                                                  CLTID=46
     C*
     C* ACCUMULATE PREVIOUSLY BILLED FOR AGENCY BILLING
     C*
     C     *IN92         IFEQ      *ON
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1DATE            7 0
     C                   Z-ADD     0             XXDPRL
     C                   MOVE      'B'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C     EATRDT        IFLT      X1DATE
     C     EAARCD        ANDNE     F1DVBL
     C                   ADD       EAAMT         XXDPRL
     C                   ENDIF                                                  EATRDT IFLT
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C*
     C                   MOVE      'A'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C     EATRDT        IFLT      X1DATE
     C                   ADD       EAAMT         XXDPRL
     C                   ENDIF                                                  EATRDT IFLT
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C*
     C                   MOVE      'D'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C     EATRDT        IFLT      X1DATE
     C                   ADD       EAAMT         XXDPRL
     C                   ENDIF                                                  EATRDT IFLT
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C*
     C                   MOVE      'F'           XXTYPE
     C     EAKEY         CHAIN     PEA01                              40
     C     *IN40         DOWEQ     '0'
     C     EATRDT        IFLT      X1DATE
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'T'
     C     TTCTYP        OREQ      'M'
     C     TTCTYP        OREQ      'S'
     C     TTCTYP        OREQ      '1'
     C     TTCTYP        OREQ      '2'
     C     TTCTYP        OREQ      '3'
     C     TTCTYP        OREQ      '4'
     C     TTCTYP        OREQ      '5'
     C     TTCTYP        OREQ      '6'
     C     TTCTYP        OREQ      '7'
     C                   ADD       EAAMT         XXDPRL
     C                   ENDIF                                                  TTCTYP IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  EATRDT IFLT
     C     EAKEY         READE     PEA01                                  40
     C                   END
     C*
     C                   ENDIF                                                  *IN92 IFEQ
     C                   SUB       XXDPRL        XXDPDU               4142
     C                   Add       xxPro         xcAAPR
     C                   Add       xxEXCN        xcAAPR
     C     ELARSM        IFEQ      'Y'
     C                   MOVEA     '00'          *IN(41)
     C                   ADD       XXPRO         XTPRO
     C                   ADD       XXMFEE        XTMFEE
     C                   ADD       XXINST        XTINST
     C                   ADD       XXSFEE        XTSFEE
     C                   ADD       XXFEEB        XTFEEB
     C                   ADD       XX1STB        XT1STB
     C                   ADD       XX2NDB        XT2NDB
     C                   ADD       XX3RDB        XT3RDB
     C                   ADD       XX4THB        XT4THB
     C                   ADD       XX5THB        XT5THB
     C                   ADD       XX6THB        XT6THB
     C                   ADD       XX7THB        XT7THB
     C                   ADD       XXEXCN        XTEXCN
     C                   ADD       XXSTOP        XTSTOP
     C                   ADD       ELTXDU        XTTXDU
     C                   ADD       XXDPRL        XTDPRL
     C                   ADD       ELANCA        XTANCA
     C                   ADD       XXGPD         XTGPD
     C                   ADD       XXDIVA        XTDIVA
     C                   ADD       XXDPDU        XTDPDU
      *** Beg Chg ***  12/05/23  *******************************************
     C*****              ADD       ELAMPR        XTMAPR
     C*****XXMINF        IFEQ      'Y'
     C*****              SUB       ELAIN1        XTMAPR
     C*****              SUB       ELAIN2        XTMAPR
     C*****              SUB       ELAWAV        XTMAPR
     C*
     C*****              Select
     C*****              When      ELWVFL = 'D' or ELWVFL = 'S'
     C*****              SUB       ELAWAV        XTESBJ
     C*****              SUB       ELAWAV        XTEMPM
     C*****              SUB       ELAWAV        XTSTPR
     C*****              When      ELWVFL = 'E'
     C*****              SUB       ELAWAV        XTESBJ
     C*****              SUB       ELAWAV        XTEMPM
     C*****              When      ELWVFL = 'J'
     C*****              SUB       ELAWAV        XTESBJ
     C*****              Endsl
     C*
     C*****              Endif
     C*****              ADD       ELASBJ        XTESBJ
     C*****              ADD       ELASPR        XTSTPR
     C*****              ADD       ELAMPM        XTEMPM
     C*****              ADD       ELATER        XTETER
     C*****              ADD       ELANMP        XTENMP
     C*****              ADD       ELPRTF        XTEXCO
     C*****              ADD       ELANMP        XTESTP
     C*****              ADD       ELPRTF        XTESTP
     C*****              ADD       ELATER        XTESTP
     C*****              ADD       ELAAPR        XTANPR
     C*****              ADD       ELADTR        XTEDTR
     C*****              ADD       ELADTR        XTESTP
     C*
     C* Print more line items on summary and include balance to min changes.
     C*
     C                   Eval      xxwk11 = MPABal

     C* Premium levels.
     C                   IF        XXMINF = 'Y'
     C                   ADD       MPAMPR        XTMAPR
     C                   ADD       MPASBJ        XTESBJ
     C                   ADD       MPAMPM        XTEMPM
     C                   ADD       MPASPR        XTSTPR
     C                   Add       xxwk11        XTSTPR
     C                   ADD       MPANMP        XTENMP
     C                   Add       xxwk11        XTENMP
     C                   ADD       MPANMP        XTESTP
     C                   Add       xxwk11        XTESTP
     C                   Add       xxwk11        XTMBAL
     C                   ELSE
     C                   ADD       ELAMPR        XTMAPR
     C                   ADD       ELASBJ        XTESBJ
     C                   ADD       ELAMPM        XTEMPM
     C                   ADD       ELASPR        XTSTPR
     C                   ADD       ELANMP        XTENMP
     C                   ADD       ELANMP        XTESTP
     C                   ENDIF

     C* Expense constant
     C* Estimated premium
     C                   IF        XXMINF = 'Y'
     C                   ADD       MPPRTD        XTEXCO
     C                   ADD       MPPRTD        XTESTP
     C                   ELSE
     C                   ADD       ELPRTD        XTEXCO
     C                   ADD       ELPRTD        XTESTP
     C                   ENDIF

     C* Terrorism charges
     C                   ADD       ELATER        XTETER
     C                   ADD       ELADTR        XTEDTR

     C* Total Premium
     C                   ADD       ELAAPR        XTANPR
     C*** End Chg *****  12/05/23  *******************************************
     C     CLTID         IFEQ      '071'
     C                   ADD       XXRCTL        XTRCTL           11 2
     C                   ENDIF
     C     CLTID         IFEQ      '046'
     C                   ADD       XXPRPD        XTPRPD
     C                   ADD       XXTAXP        XTTAXP
     C                   ADD       XX1SPD        XT1SPD
     C                   ADD       XX2NPD        XT2NPD
     C                   ADD       XXEXPD        XTEXPD
     C                   ADD       XXMFPD        XTMFPD
     C                   ADD       XXSFPD        XTSFPD
     C                   ADD       XXGFEE        XTGFEE
     C                   ADD       XXGFPD        XTGFPD
     C                   ENDIF
      /free
       If CltId = '111';
          xtDvnd = xtDvnd + xxDvnd;
       Endif;
      /end-free
     C                   ENDIF
     C*
     C*  IF SELF-REPORTING TYPE AND CREDIT AMOUNT, PRINT PLEASE DEDUCT
     C*
     C                   MOVE      '0'           *IN43
     C     *IN42         IFEQ      '1'
     C     XXSELF        ANDEQ     'Y'
     C     SVCNCL        ANDNE     'Y'
     C     *IN57         ANDEQ     *OFF
     C                   MOVE      '1'           *IN43
     C                   END
     C*
     C*  LOAD DISCOUNT AMT INTO ARRAY IF NOT EQUAL TO 0
     C*
     C     ELADC3        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELASP3        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDISC        ANDNE     *BLANK
     C*
     C                   Z-ADD     ELADC3        TAM(A)                         DISCOUNT
     C     ELADC3        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADC3        TAM(A)
     C                   ENDIF
     C*
     C     RRDSHD        IFEQ      *BLANKS
     C                   MOVEL     CON(7)        HDG(A)
     C                   ELSE
     C                   MOVEL     RRDSHD        HDG(A)
     C                   ENDIF                                                  RRDSHD IFEQ
     C*
      * If client <> 046, IGNORE Self-reporting accounts.
     C     CLTID         IFNE      '046'
     C     ELADC3        IFEQ      0
     C                   ADD       1             A
     C                   ELSE
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELDISP        IFNE      *ZERO
     C                   MOVE      ELDISP        XXASP3            5
     C                   ELSE
     C     ELASP3        IFNE      *ZERO
     C                   MOVE      ELASP3        XXASP3            5
     C                   ENDIF
     C                   ENDIF
     C                   MOVEA     XXASP3        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   END
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   END
     C                   ENDIF                                                  CLTID <> 046
      * If client 046, skip discount for Self-reporting accounts.
      *  Copied from W7700RSV at C046.
     C     CLTID         IFEQ      '046'
     C     ELADC3        IFEQ      0
     C     XXSELF        OREQ      'Y'
     C                   ADD       1             A
     C                   ELSE
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELASP3        IFNE      *ZERO
     C                   MOVE      ELASP3        XXASP3            5
     C                   ELSE
     C                   MOVE      ELDISP        XXASP3            5
     C                   ENDIF
     C*
     C                   MOVEA     XXASP3        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   END
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   END
     C                   ENDIF                                                  CLTID = 046
      *
     C                   ENDIF
     C*
     C*  LOAD DEVIATION IF APPLIED TO DISCOUNTED PREMIUM
     C*
     C     RRDV1L        IFEQ      'D'
     C*
     C     ELADC2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELASP2        ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADC2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADC2        TAM(A)
     C                   ENDIF
     C     ELASP2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C     RRDV1H        IFEQ      *BLANK
     C                   MOVEL     CON(6)        HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV1H        HDG(A)
     C                   ENDIF
     C     ELDVEF        IFNE      *ZERO
     C                   Z-ADD     ELDVEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELDVEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELDVEX        IFNE      *ZERO
     C                   Z-ADD     ELDVEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELASP2        IFLT      0
      *
     C     ELASP2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELASP2        XXASP2            5
     C                   ENDIF                                                  ELASP2
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDC2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*
     C*  LOAD DEVIATION 1 2ND PERIOD IF APPLIED TO DISCOUNTED PREMIUM
     C     RRDV1L        IFEQ      'D'
     C*
     C     ELD1A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD1S2        ANDNE     *ZERO
     C                   Z-ADD     ELD1A2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD1A2        TAM(A)
     C                   ENDIF
     C     ELD1S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C     RRDV1H        IFEQ      *BLANK
     C     con(6)        CAT       @2nper:1      HDG(A)
     C                   ELSE
     C     rrdv1h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD1D2        IFNE      *ZERO
     C                   Z-ADD     ELD1D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD1X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD1X2        IFNE      *ZERO
     C                   Z-ADD     ELD1X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELD1S2        IFLT      0
     C     ELD1S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD1S2        XXASP2            5
     C                   ENDIF                                                  ELD1S2
     C*
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF                                                  NUM,1 IFNE
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF                                                  NUM,2 IFNE
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELD1A2 IFNE
     C*
     C                   ENDIF                                                  RRDV1L IFEQ
     C*  LOAD 2ND DEVIATION IF APPLIED TO DISCOUNTED PREMIUM
     C*
     C     RRDV2L        IFEQ      'D'
     C*
     C     ELADV2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELDEV2        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDEV2F       ANDNE     *ZERO
     C*
     C                   Z-ADD     ELADV2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADV2        TAM(A)
     C                   ENDIF
     C*
     C                   Select
     C                   When      ELdev2  <> 0
     C                   If        ELdev2 < *Zeros
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELstFc  <> 0
     C                   If        ELstFc > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   When      ELdev2F <> 0
     C                   If        ELdev2F > 1
     C                   Eval      sgn(a) = '+'
     C                   Else
     C                   Eval      sgn(a) = '-'
     C                   EndIf
     C                   EndSl
     C     RRDV2H        IFEQ      *BLANK
     C  N25              MOVEL     CON(19)       HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRDV2H        HDG(A)
     C                   ENDIF
     C     ELD2EF        IFNE      *ZERO
     C                   Z-ADD     ELD2EF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2EX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2EX        IFNE      *ZERO
     C                   Z-ADD     ELD2EX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
      *
      *
      *
      *
     C                   Select
     C*>>>>>>>>>>>>>>>>> Stabilization Factor
     C                   When      cltID = '025'
     C                   MOVE      ELSTFC        XXASP8            8
     C                   MOVEA     XXASP8        NUM8
      *
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      '.'           PCT(2)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C*>>>>>>>>>>>>>>>>> Territory Rating
     C                   When      cltID = '136'
     C                   MOVE      ELdev2F       XXASP3
     C                   MOVEA     XXASP3        NUM

     C                   MOVE      NUM(1)        PCT(2)
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      NUM(2)        PCT(4)
     C                   MOVE      NUM(3)        PCT(5)
     C                   MOVE      NUM(4)        PCT(6)
     C                   MOVE      NUM(5)        PCT(7)
     C*>>>>>>>>>>>>>>>>> Default Deviation #2
     C                   Other
     C     ELDEV2        IFLT      0
      *
     C     ELDEV2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
      *
     C                   ELSE
     C                   MOVE      ELDEV2        XXASP2            5
     C                   ENDIF                                                  ELEDV2
     C                   MOVEA     XXASP2        NUM
      *
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVE      '%'           PCT(7)
     C                   EndSl
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELEDV2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*  LOAD 2ND DEVIATION 2ND PERIOD IF APPLIED TO DISCOUNTED PREMIUM
     C     RRDV2L        IFEQ      'D'
     C*
     C     ELD2A2        IFNE      *ZERO                                        DEVIATION
     C     XXMINF        OREQ      'Y'
     C     ELD2S2        ANDNE     *ZERO
     C*
     C     XXCLT#        ANDNE     '025'
     C     XXMINF        OREQ      'Y'
     C     ELSTFC        ANDNE     *ZERO
     C     XXCLT#        ANDEQ     '025'
     C                   Z-ADD     ELD2A2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPD2A2        TAM(A)
     C                   ENDIF
     C     XXCLT#        IFNE      '025'
     C     ELD2S2        IFLT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*FOR CLIENT 025
     C                   ELSE
     C     ELSTFC        IFGT      1
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  CLTID <> 025
     C*
     C     RRDV2H        IFEQ      *BLANK
     C  N25con(19)       CAT       @2nper:1      HDG(A)
     C   25              MOVEL     CON(72)       HDG(A)
     C                   ELSE
     C     rrdv2h        CAT       @2nper:1      HDG(A)
     C                   ENDIF
     C*
     C     ELD2D2        IFNE      *ZERO
     C                   Z-ADD     ELD2D2        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELD2X2        ORNE      0
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELD2X2        IFNE      *ZERO
     C                   Z-ADD     ELD2X2        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C     XXCLT#        IFNE      '025'
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C     ELD2S2        IFLT      0
     C     ELD2S2        MULT      -1            XXASPN
     C                   MOVE      XXASPN        XXASP2
     C                   ELSE
     C                   MOVE      ELD2S2        XXASP2            5
     C                   ENDIF                                                  ELD2S2
     C*
     C                   MOVEA     XXASP2        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C*FOR CLIETN 025
     C                   ELSE
     C                   MOVE      '.'           PCT(2)
     C                   MOVE      ELSTFC        XXAS8             8
     C                   MOVEA     XXAS8         NUM8
     C                   MOVE      NUM8(2)       PCT(1)
     C                   MOVE      NUM8(3)       PCT(3)
     C                   MOVE      NUM8(4)       PCT(4)
     C                   MOVE      NUM8(5)       PCT(5)
     C                   MOVE      NUM8(6)       PCT(6)
     C                   MOVE      NUM8(7)       PCT(7)
     C                   MOVE      NUM8(8)       PCT(8)
     C                   ENDIF                                                  CLTID<>025
     C*
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELD2A2 IFNE
     C*
     C                   ENDIF                                                  RRDV2L IFEQ
     C*
     C* WAIVER SURCHARGE
     C*
     C     ELWVFL        IFEQ      'D'
     C*
      *** Beg Chg *****  02/28/25  *******************************************
     C*****ELAWAV        IFNE      *ZERO
     C                   If        (ELAWAV <> 0 and XXMINF <> 'Y') OR
     C                             (MPAWAV <> 0 and XXMINF = 'Y')
      *** End Add *****  02/28/25  *******************************************
     C*
     C     RRWVHD        IFEQ      *BLANKS
     C                   MOVEL     CON(32)       HDG(A)
     C                   ELSE
     C                   MOVEL     RRWVHD        HDG(A)
     C                   ENDIF                                                  RRWVHD IFEQ
     C*
     C                   Z-ADD     ELAWAV        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPAWAV        TAM(A)
     C                   ENDIF
      *** Beg Add *****  05/12/25  *******************************************
      /Free
       // Print an '*' appended to heading on the line item
       If ELPror > 0 and ELPror < 1;
          If xxminf = 'Y';
             HDG(A) = %trim(HDG(A)) + '*';
             PrintFlg = 'Y';
          Endif;
       Endif;
      /End-Free
      *** End Add *****  05/12/25  *******************************************
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF                                                  RRAWAV IFNE
     C*
     C                   ENDIF                                                  RRWVFL IFEQ
     C*
     C* ADDITIONAL LOAD
     C*
     C     RRASFL        IFEQ      'D'
     C*
     C     XTADLD        IFNE      *ZERO
     C                   MOVEL     CON(18)       HDG(A)
     C   49              Eval      HDG(A) = 'Stop Gap Premium'
     C                   Z-ADD     XTADLD        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
     C*
     C                   ENDIF                                                  RRASFL IFEQ
     C     XXLRGD        IFEQ      'Y'
     C*
     C*  1st Large Deductible applied to DISCOUNTED
     C*
     C     LDPLV1        IFEQ      'D'
     C*
     C     ELADCT        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT1        ANDNE     *ZERO
     C*
     C     LDEFD1        IFNE      *ZERO
     C                   Z-ADD     LDEFD1        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     LDEXP1        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     LDEXP1        IFNE      *ZERO
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   Z-ADD     ELADCT        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCT        TAM(A)
     C                   ENDIF
     C     ELADCT        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   MOVEL     CON(79)       HDG(A)
     C*
     C                   MOVE      LDPCT1        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C                   If        cltid = '150' and ELManP <> 0
     c                   Z-add     ldpct1        xxpct150
     c                   move      *blanks       xxpct1
     c                   movel     xxp1150       xxpct1
     c                   cat       '.':0         xxpct1
     c                   cat       xxp2150:0     xxpct1
     c                   cat       '%':0         xxpct1
     C                   Endif
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV1 IFEQ 'D'
     c*
     c*  2nd Large Deductible applied to DISCOUNTED
     c*
     C     LDPLV2        IFEQ      'D'
     C*
     C     ELADD2        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     LDPCT2        ANDNE     *ZERO
     C*
     C     LDEFD2        IFNE      *ZERO
     C                   Z-ADD     LDEFD2        SRDTI
     C                   ELSE
     C                   Z-ADD     LDEXP1        SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C*
     C                   Z-ADD     LDEXP2        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C*
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   Z-ADD     ELADD2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADD2        TAM(A)
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   MOVEL     CON(80)       HDG(A)
     C*
     C                   MOVE      LDPCT2        XXPCTA
     C*
     C                   MOVE      *BLANKS       XXPCT1           13
     C                   MOVEL     XXP1          XXPCT1
     C                   CAT       '.':0         XXPCT1
     C                   CAT       XXP2:0        XXPCT1
     C                   CAT       '%':0         XXPCT1
     C*
     C                   MOVE      XXPCT1        HDG(A)
     C*
     C                   ADD       1             A
     C*
     C                   END
     C*
     C                   ENDIF                                                  LDPLV2 IFEQ 'D'
     C*
     C                   ENDIF                                                  XXLRGD IFEQ
      *** Beg Add *****  11/11/24  *******************************************
     C*
     C*  DEDUCTIBLE DISCOUNT - IF APPLIED TO DISCOUNTED PREMIUMS
     C*
     C     RRRDPR        IFEQ      'D'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     ELADCT        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDDCT        ANDNE     *ZERO
     C*
     C     ELDCEF        IFNE      *ZERO
     C                   Z-ADD     ELDCEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELDCEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELDCEX        IFNE      *ZERO
     C                   Z-ADD     ELDCEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C                   Z-ADD     ELADCT        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADCT        TAM(A)
     C                   ENDIF
     C     ELADCT        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   MOVEL     CON(14)       HDG(A)
     C                   MOVE      ELDDCT        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   END
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C     XXMINF        OREQ      'Y'
     C     ELDD2P        ANDNE     *ZERO
     C*
     C     EL2DEF        IFNE      *ZERO
     C                   Z-ADD     EL2DEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELDCEX        SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C*
     C                   Z-ADD     ELDCEX        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C*
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   MOVE      '.'           PCT(4)
     C                   MOVE      '%'           PCT(7)
     C                   Z-ADD     ELADD2        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADD2        TAM(A)
     C                   ENDIF
     C*
     C     ELADD2        IFNE      *ZERO
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C*
     C                   MOVEL     CON(47)       HDG(A)
     C                   MOVE      ELDD2P        XXASP1            5
     C                   MOVEA     XXASP1        NUM
     C*
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   ENDIF
     C*
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   ENDIF
     C*
     C                   MOVE      NUM(3)        PCT(3)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C*
     C                   ADD       1             A
     C*
     C                   ENDIF
     C*
     C                   ENDIF
      *** End Add *****  11/11/24  *******************************************
     C*
     C* ARAP CHARGE
     C*
      *** Beg Del *****  05/12/25  *******************************************
     C*****XXMINF        IFNE      'Y'
      *** End Del *****  05/12/25  *******************************************
     C*
     C     ELRPA1        IFNE      *ZERO
     C     ELRPA2        ORNE      *ZERO
     C     ELRPA3        ORNE      *ZERO
      *** Beg Add *****  05/12/25  *******************************************
     C     MPRPA1        ORNE      *ZERO
     C     MPRPA2        ORNE      *ZERO
     C     MPRPA3        ORNE      *ZERO
      *** End Add *****  05/12/25  *******************************************
     C                   MOVEL     CON(38)       HDG(A)
     C                   IF        XXMINF = 'Y'
     C     MPRPA1        ADD       MPRPA2        TAM(A)
     C                   ADD       MPRPA3        TAM(A)
     C                   ELSE
     C     ELRPA1        ADD       ELRPA2        TAM(A)
     C                   ADD       ELRPA3        TAM(A)
     C                   ENDIF
     C     TAM(A)        IFGT      *ZERO
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF

      /free
       If IsSplitMod(ELarp1:ELarp2:ELrpa1:ELrpa2:ELemd1:ELemd2) or
        IsSplitMod(ELarp1:ELarp3:ELrpa1:ELrpa3:ELemd1:ELemd3)   or
        IsSplitMod(ELarp2:ELarp2:ELrpa2:ELrpa2:ELemd2:ELemd3);
      /end-free
     C                   MOVEL     CON(38)       HDG(A)
     C                   ELSE
     C*
     C     ELRPA1        IFNE      *ZERO
     C                   MOVE      ELARP1        XXASP1
     C                   ELSE
     C     ELRPA2        IFNE      *ZERO
     C                   MOVE      ELARP2        XXASP1
     C                   ELSE
     C                   MOVE      ELARP3        XXASP1
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXASP1        IFEQ      '00000'
     C                   MOVE      ELARP1        XXASP1
     C                   ENDIF                                                  XXASP1 IFEQ
     C*
     C                   MOVEA     XXASP1        NUM
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      NUM(1)        PCT(2)
     C                   MOVE      NUM(2)        PCT(4)
     C                   MOVE      NUM(3)        PCT(5)
     C                   MOVE      NUM(4)        PCT(6)
     C                   MOVE      NUM(5)        PCT(7)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ENDIF                                                  ELARP1 IFNE
     C                   ADD       1             A
      *** Beg Del *****  05/12/25  *******************************************
     C*****              ENDIF                                                  ELRPA1 IFNE
      *** End Del *****  05/12/25  *******************************************
     C*
     C                   ENDIF                                                  ELAMNP IFNE
      *** Beg chg ***  12/05/23  *******************************************
      *
      * Re-arrange bottom section of premium line items for new ordering plus
      * the balance to minimum changes.  Starting with normal premium all the
      * way thru bottom line total premium.
      *
      * Re-arrange coding in this ordering:
      *   Normal Premium
      *   Minimum Premium
      *   Expense Constant
      *   Harbor Exposure Fees
      *   Policy Fees
      *   Estimated Premium (Annual Premium)
      *   Terrorism Charge
      *   Domestic Terrorism Charge
      *   Fees and Taxes
      *   Total Premium
      * ******
      *** End chg ***  12/05/23  *******************************************
     C*
     C* NORMAL PREMIUM
     C*
     C     XXMINF        IFEQ      'Y'
      *** Beg Del ***  12/05/23  *******************************************
      * Delete this duplicate Code
      /Free
       // Policy Fee
       // If ELapf <> 0 and EXbexp <> 'Y';
       //    If RRpfhd = *blanks;
       //       hdg(a) = xxSt + ' ' + con(90);
       //    Else;
       //       hdg(a) = RRpfhd;
       //    Endif;
       //    tam(a) = ELapf;
       //    sgn(a) = '+';
       //    a += 1;
       // Endif;
      /End-Free
      *** End Del ***  12/05/23  *******************************************

     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
     C                   MOVEL     CON(63)       HDG(A)
     C                   OTHER
      *** Beg chg ***  12/05/23  *******************************************
      * Do not show 'Total Minimum Premium' heading
     C*****              MOVEL     CON(11)       HDG(A)
     C                   MOVEL     CON(8)        HDG(A)
      *** End chg ***  12/05/23  *******************************************
     C                   ENDSL
     C                   ELSE
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
     C                   MOVEL     CON(61)       HDG(A)
     C                   OTHER
     C                   MOVEL     CON(8)        HDG(A)
     C                   ENDSL
     C                   END
     C                   Z-ADD     ELANMP        TAM(A)
     C     XXMINF        IFEQ      'Y'
      *** Beg Del *****  05/12/25  *******************************************
     C*****EXBEXP        ANDNE     'Y'
      *** End Del *****  05/12/25  *******************************************
     C                   ADD       ELATER        TAM(A)
     C                   ADD       ELADTR        TAM(A)
     C                   ADD       ELAARC        TAM(A)
     C                   ADD       ELAPF         TAM(A)
     C                   ENDIF
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPANMP        TAM(A)
     C                   ENDIF
     C                   IF        ELAMNP = 'Y'
     C* Include Balnace to Minimum Premium
     C                   Add       tasmin        tam(a)
     C                   ENDIF
     C     A             SUB       1             B                 2 0
     C                   MOVE      *ON           *IN(B)
     C                   ADD       2             A
     C*
     C*  EXPENSE CONSTANT
     C*
     C     EXBEXP        IFEQ      'Y'
     C                   MOVEL     CON(9)        HDG(A)                         EXP CONSTANT
     C*
     C     XXEXCN        IFGT      *ZERO
     C                   Z-ADD     XXEXCN        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C                   Z-ADD     0             TAM(A)
     C                   END
      *** Beg Add *****  05/12/25  *******************************************
      * Print an '*' appended to heading on the line item
       If ELPror > 0 and ELPror < 1;
          Setgt rrky pec01;
          Readpe rrky2 pec01;
          If not %eof(wdecl);
             If (ECsful = 'Y' and ELcofl <> 'Y');
                HDG(A) = %trim(HDG(A)) + '*';
                PrintFlg = 'Y';
             Endif;
          Endif;
       Endif;
      *** End Add *****  05/12/25  *******************************************

     C     ELFND         IFNE      2
     C     ELFND         ANDNE     101
     C     CLTID         ORNE      '114'
     C                   ADD       1             A
     C                   ENDIF                                                  ELFND IFNE
      *** Bed Add *****  05/12/25  *******************************************
      * Following END to Expense Constant condition (EXBEXP IFEQ 'Y') was missing here
      * During re-arrange, It was placed after Estimated Premium/Annual Premium section
      * moving it here.
     C                   END
      *** End Add *****  05/12/25  *******************************************
     C*
      *** Beg Add *****  05/12/25  *******************************************
     C* Move both Terroism Charge to print just after Expense Constant line
     C* Earlier it was printing just after Prorata Factor line
     C*
     C* TERRORISM RISK CHARGE
     C*
     C     ELATER        IFNE      *ZERO
     C                   MOVEL     CON(37)       HDG(A)
     C                   Z-ADD     ELATER        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
     C*
     C* DOMESTIC TERRORISM RISK CHARGE
     C*
     C     ELADTR        IFNE      *ZERO
     C                   MOVEL     CON(43)       HDG(A)
     C                   Z-ADD     ELADTR        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
      *** End Add *****  05/12/25  *******************************************
     C*
     C*
     C* FACULTATIVE REINSURANCE CHARGE
     C*
     C     ELAARC        IFNE      *ZERO
      *** Beg Del *****  05/12/25  *******************************************
     C*****EXBEXP        ANDNE     'Y'
      *** End Del *****  05/12/25  *******************************************
     C                   MOVEL     CON(72)       HDG(A)
     C                   Z-ADD     ELAARC        TAM(A)
      *** Beg Del ***  12/05/23  *******************************************
     C*****              IF        XXMINF = 'Y'
     C*****              Z-ADD     MPAARC        TAM(A)
     C*****              ENDIF
      *** End Del ***  12/05/23  *******************************************
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
      *
      *  HARBOR EXPOSURE FEE
      *
     C     ELHXMN        IFNE      *ZERO
     C                   MOVEL     CON(71)       HDG(A)
     C                   Z-ADD     ELHXMN        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
     C*
      /Free
       // Policy Fee
       If ELapf <> 0;
          If RRpfhd = *blanks;
             hdg(a) = xxSt + ' ' + con(90);
          Else;
             hdg(a) = RRpfhd;
          Endif;
          tam(a) = ELapf;
          sgn(a) = '+';
          a += 1;
       Endif;
      /End-Free
      *
      * UNEARN PREM SURCHARGE
      *
     C     ELUPSA        IFNE      *ZERO
     C                   MOVEL(p)  CON(86)       HDG(A)
     C                   Z-ADD     elupsa        TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPAUSA        TAM(A)
     C                   ENDIF
     C                   MOVE      '+'           SGN(A)
     C                   ADD       1             A
     C                   ENDIF
      /Free
       If ELanca <> 0;
          tam(a) = ELanca;
       //*** begin chg ***  07/25/25  **********************************
       //*****   hdg(a) = 'Audit Noncompliance Charge';
          hdg(a) = Con(94);
       //*** end chg *****  07/25/25  **********************************
          sgn(a) = '+';
          a += 1;
       Endif;
      /End-free
      *
      *

     C*
     C* ESTIMATED PREMIUM / Annual Premium
     C*
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
      *** Beg Chg *****  12/05/23  *******************************************
      * Show Annual Premium instead of Total Premium
     C*****              MOVEL     CON(62)       HDG(A)
     C*****              OTHER
     C*****              MOVEL     CON(10)       HDG(A)                         EST PREM
      *** Beg Chg *****  05/12/25  *******************************************
     C*****              EVAL      HDG(A) = 'Annual Premium Contribution'
     C*****              OTHER
     C*****              EVAL      HDG(A) = 'Annual Premium'
     C                   EVAL      HDG(A) = 'Audited Contribution'
     C                   OTHER
     C                   EVAL      HDG(A) = 'Audited Premium'
      *** End Chg *****  05/12/25  *******************************************
      *** End Chg *****  12/05/23  *******************************************
     C                   ENDSL
     C     XXEXCN        ADD       ELANMP        TAM(A)
      *** Beg Del *****  12/05/23  *******************************************
      * Do not include terrorism charges in Estimated annual premium
     C*****              ADD       ELATER        TAM(A)
     C*****              ADD       ELADTR        TAM(A)
      *** End Del *****  12/05/23  *******************************************
     C                   ADD       ELHXMN        TAM(A)
     C                   ADD       ELAARC        TAM(A)
     C                   ADD       ELUPSA        TAM(A)
     C                   ADD       ELANCA        TAM(A)
     C                   ADD       ELAPF         TAM(A)
     C                   IF        XXMINF = 'Y'
     C* Include Normal Premium from WDMPP instead of WDELP
     C                   SUB       ELANMP        TAM(A)
     C                   ADD       MPANMP        TAM(A)
     C                   ENDIF
     C                   IF        ELAMNP = 'Y'
     C* Include Balnace to Minimum Premium
     C                   ADD       TASMIN        TAM(A)
     C                   ENDIF
      *** Beg Add *****  05/12/25  *******************************************
     C* Add Full Domestic Terrorism charges and Terrorism charges
     C                   Add       ELater        TAM(A)
     C                   Add       ELadtr        TAM(A)
      *** End Add *****  05/12/25  *******************************************
     C     A             SUB       1             B                 2 0
     C                   MOVE      *ON           *IN(B)
     C                   ADD       2             A
      *** Bed Del *****  05/12/25  *******************************************
      * Following END belongs to Expense Constant condition (EXBEXP IFEQ 'Y')
      * moving it to correct place
     C*****              END
      *** End Del *****  05/12/25  *******************************************
     C*
     C*   LOAD PREMIUM INTO ARRAY IF NOT EQUAL 1
     C*
     C     XXSRPR        IFNE      'Y'
     C*                                          PRORATED
     C     ELPROR        IFNE      1
     C                   MOVEL     CON(12)       HDG(A)
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      ELPROR        XXPROR            5
     C                   MOVEA     XXPROR        NUM
     C     NUM(1)        IFNE      '0'
     C                   MOVE      NUM(1)        PCT(1)
     C                   END
     C     NUM(2)        IFNE      '0'
     C     NUM(1)        ORGT      '0'
     C                   MOVE      NUM(2)        PCT(2)
     C                   END
     C                   MOVE      NUM(3)        PCT(4)
     C                   MOVE      NUM(4)        PCT(5)
     C                   MOVE      NUM(5)        PCT(6)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ELSE
     C                   END
     C*
     C                   ENDIF                                                  *IN85 IFNE
     C*
      *** Beg Del *****  05/12/25  *******************************************
     C* Move both Terroism Charge to print just after Expense Constant line
     C*
     C* TERRORISM RISK CHARGE
     C*
     C*****ELATER        IFNE      *ZERO
     C*****              MOVEL     CON(37)       HDG(A)
     C*****              Z-ADD     ELATER        TAM(A)
     C*****              MOVE      '+'           SGN(A)
     C*****              ADD       1             A
     C*****              ENDIF
     C*
     C*
     C* DOMESTIC TERRORISM RISK CHARGE
     C*
     C*****ELADTR        IFNE      *ZERO
     C*****              MOVEL     CON(43)       HDG(A)
     C*****              Z-ADD     ELADTR        TAM(A)
     C*****              MOVE      '+'           SGN(A)
     C*****              ADD       1             A
     C*****              ENDIF
      *** End Del *****  05/12/25  *******************************************
     C*
     C*   LOAD THE STATE TAX INTO ARRAY
     C*
     C     RRTXHD        IFNE      *BLANKS
     C                   MOVEL(P)  RRTXHD        XXTHD            20
     C                   ELSE
     C                   MOVEL(P)  CON(24)       XXTHD            20
     C                   ENDIF
     C     ELTXDU        IFNE      0
     C*
     C                   Z-ADD     ELTXDU        TAM(A)
     C                   MOVE      '+'           SGN(A)
     C                   CLEAR                   TMP
     C                   CLEAR                   XTX
     C                   MOVE      *BLANKS       XXTXPD
     C                   MOVE      *BLANKS       XXTXPR
     C     RRTXHD        IFNE      *BLANKS
     C                   MOVEA     RRTXHD        TMP
     C                   ELSE
     C                   MOVEA     CON(24)       TMP
     C                   ENDIF                                                  RRTXHD IFNE
     C*
     C                   MOVE      ELTXPR        XXTXPD
     C* CENTS
     C                   MOVE      '.'           XXPER
     C                   MOVE      XXTXCD        XXTXC
     C* HUNDREDS
     C     ELTXPR        IFGE      1000
     C                   MOVE      ','           XXCM2
     C                   MOVE      XXTXHD        XXTXH
     C                   ENDIF
     C* THOUSANDS
     C     ELTXPR        IFGE      1000000
     C                   MOVE      ','           XXCM1
     C                   MOVE      XXTXTD        XXTXT
     C                   ENDIF
     C* HANDLE RANGES TO ELIMINATE LEADING ZEROES.
     C     ELTXPR        IFGE      0
     C     ELTXPR        ANDLT     1000
     C                   MOVEA     XXTXHD        XTX
     C     XTX(1)        IFEQ      '0'
     C                   MOVE      *BLANK        XTX(1)
     C     XTX(2)        IFEQ      '0'
     C                   MOVE      *BLANK        XTX(2)
     C                   ENDIF                                                  XTX,2 IFEQ
     C                   ENDIF                                                  XTX,1 IFEQ
     C                   MOVEA     XTX           XXTXH
     C                   ENDIF                                                  ELTXPR IFGE
     C*
     C     ELTXPR        IFGE      1000
     C     ELTXPR        ANDLT     1000000
     C                   MOVEA     XXTXTD        XTX
     C     XTX(1)        IFEQ      '0'
     C                   MOVE      *BLANK        XTX(1)
     C     XTX(2)        IFEQ      '0'
     C                   MOVE      *BLANK        XTX(2)
     C                   ENDIF                                                  XTX,2 IFEQ
     C                   ENDIF                                                  XTX,1 IFEQ
     C                   MOVEA     XTX           XXTXT
     C                   ENDIF                                                  ELTXPR IFGE
     C*
     C     ELTXPR        IFGE      1000000
     C                   MOVEA     XXTXMD        XTX
     C     XTX(1)        IFEQ      '0'
     C                   MOVE      *BLANK        XTX(1)
     C     XTX(2)        IFEQ      '0'
     C                   MOVE      *BLANK        XTX(2)
     C                   ENDIF                                                  XTX,2 IFEQ
     C                   ENDIF                                                  XTX,1 IFEQ
     C                   MOVEA     XTX           XXTXM
     C                   ENDIF                                                  ELTXPR IFGE
     C*
     C*
     C                   MOVEA     XXTXPR        TMP(13)
     C                   MOVE      '*'           TMP(28)
     C                   MOVEA     TMP           HDG(A)
     C                   CLEAR                   TMP
     C*
     C                   MOVE      '.'           PCT(3)
     C                   MOVE      RRTXPC        X6PROR            6
     C                   MOVEA     X6PROR        NUM6
     C     NUM6(1)       IFNE      '0'
     C                   MOVE      NUM6(1)       PCT(1)
     C                   END
     C     NUM6(2)       IFNE      '0'
     C     NUM6(1)       ORGT      '0'
     C                   MOVE      NUM6(2)       PCT(2)
     C                   END
     C                   MOVE      NUM6(3)       PCT(4)
     C                   MOVE      NUM6(4)       PCT(5)
     C                   MOVE      NUM6(5)       PCT(6)
     C                   MOVE      NUM6(6)       PCT(7)
     C                   MOVE      '%'           PCT(8)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
      *** Beg Add *****  12/05/23  *******************************************
     C                   ENDIF
     C                   IF        ELTXDU <> 0
      *** Beg Del *****  05/12/25  *******************************************
     C*****                        or ELATER <> 0 or ELADTR <> 0
      *** End Del *****  05/12/25  *******************************************
      *** End Chg *****  12/05/23  *******************************************
     C*
     C*   LOAD THE TOTAL PREMIUM INTO ARRAY
     C*
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
      *** Beg Chg *****  05/12/25  *******************************************
     C*****              MOVEL     CON(62)       HDG(A)
     C                   MOVEL     CON(65)       HDG(A)
      *** End Chg *****  05/12/25  *******************************************
     C                   OTHER
     C                   MOVEL     CON(25)       HDG(A)                         TOT ST TAX
     C                   ENDSL
     C                   IF        XXMINF = 'Y'
     C     ELTXDU        ADD       MPAAPR        TAM(A)
      *** Beg Chg *****  12/05/23  *******************************************
      * Add Domestic Terrorism charges and Terrorism charges
     C                   EVAL      TAM(A) = TAM(A) + ELATER + ELADTR
     C                   EVAL      TAM(A) = TAM(A) + MPABaL
      *** End Chg *****  12/05/23  *******************************************
     C*** begin add ***  11/08/24  *******************************************
     C                   Add       ELAPF         TAM(A)
     C*** end add *****  11/08/24  *******************************************
      *** Beg Add *****  05/12/25  *******************************************
     C                   ADD       ELAARC        TAM(A)
      *** End Add *****  05/12/25  *******************************************
     C                   ELSE
     C     ELTXDU        ADD       ELAAPR        TAM(A)
     C                   ENDIF
     C     ELAMNP        IFEQ      'Y'
     C                   MOVE      '*'           SGN(A)
     C                   ENDIF
     C     A             SUB       1             B                 2 0
     C                   MOVE      *ON           *IN(B)
     C                   ADD       1             A
     C*
     C                   END
      *** end chg *****  11/13/23  *******************************************
      //*** Beg Add *****  05/12/25  *******************************************
      // Print "* Indicated line items do not get prorated"
       If PrintFlg = 'Y';
          TAM(A) = 0;
          HDG(A) = MSG(3);
          A += 1;
       Endif;
      //*** End Add *****  05/12/25  *******************************************
      *** Beg chg ***  12/05/23  *******************************************
      * End of Re-arrange
      *** End Chg *****  12/05/23  *******************************************
     C*
     C*  GET BILLING COMMENT DESCRIPTIONS
     C*
     C     XXCOM1        IFNE      *BLANK
     C                   MOVEL     XXCOM1        XXBCOM
     C     BCKEY1        CHAIN     PBC01                              50
     C  N50              MOVEL     BCDESC        BCCOM1           30
     C                   END
     C*
     C     XXCOM2        IFNE      *BLANK
     C                   MOVEL     XXCOM2        XXBCOM
     C     BCKEY1        CHAIN     PBC01                              50
     C  N50              MOVEL     BCDESC        BCCOM2           30
     C                   END
     C*
     C     XXCOM3        IFNE      *BLANK
     C                   MOVEL     XXCOM3        XXBCOM
     C     BCKEY1        CHAIN     PBC01                              50
     C  N50              MOVEL     BCDESC        BCCOM3           30
     C                   END
     C     QQFCM1        IFNE      *BLANKS
     C     QQFCM2        ORNE      *BLANKS
     C     QQFCM3        ORNE      *BLANKS
     C                   MOVEL(P)  QQFCM1        BCCOM1
     C                   MOVEL(P)  QQFCM2        BCCOM2
     C                   MOVEL(P)  QQFCM3        BCCOM3
     C                   ENDIF                                                  QQFCM1 IFNE
     C*
     C*  GET AGENT NAME VIA AGENT HISTORY AND AGENT MASTER FILES
     C*
     C     FULLT         TAG
     C*
     C*  OUTPUT TOTALS
     C*  IF MORE PAYROLL RECORDS TO READ, DON'T PRINT AMTS ON BILL
     C*
     C     *IN54         IFEQ      '1'
     C                   MOVE      '0'           *IN51
     C                   END
     C*** begin add ***  10/25/25  **********************************************
     c* Ensure output gets generated when we don't have any payroll records.
     C                   If        noPayrollBill = 'Y'
     C*
     C                   MOVE      '1'           XXPRT             1
     C                   MOVE      '1'           X2PRT             1
     C                   Move      *off          *in54
     C                   Move      *on           *in51
     C                   Eval      prdesc = '<No Payrolls Exist>'
     C                   Clear                   epclas
     C                   Clear                   epspec
     C                   Clear                   epext
     C                   Clear                   eparat
     C                   Clear                   xqarat
     C                   Clear                   xxeapr
     C                   Clear                   xxempr
     C                   Clear                   xxemsn
     C*
     C                   If        isUseSpoolOut
     C                   Write     qhead
     C                   Write     qhead1
     C                   Write     qline1
     C                   Write     qhead2
     C                   Write     qhead3
     C                   Write     qbox1
     C                   Write     qdet
     C                   Endif
     C*
     C                   Endif
     C*** end add *****  10/25/25  **********************************************
     C*
     C     XXPRT         IFEQ      '1'
     C*
     C     *IN54         IFEQ      *OFF
     C     CLTID         IFEQ      '046'
      *
     C     XXPRO         SUB       XXPRPD        XXPBAL           11 2
     C     XXTXDU        SUB       XXTAXP        XXTBAL           11 2
     C     XXMFEE        SUB       XXMFPD        XXMBAL           11 2
     C     XX1STB        SUB       XX1SPD        XX1BAL           11 2
     C     XX2NDB        SUB       XX2NPD        XX2BAL           11 2
     C     XXEXCN        SUB       XXEXPD        XXEBAL           11 2
     C     XXSFEE        SUB       XXSFPD        XXSBAL           11 2
     C     XXDPDU        ADD       XXDPRL        XXTBIL           11 2
     C     XXGFEE        SUB       XXGFPD        XXGBAL           11 2
     C     XXGAMT        SUB       XXGPD         XXFBAL           11 2
      *
     C                   ENDIF
     C*** Begin Chg *** 09/26/23 ***************************************
     C*****TURN*         IFEQ      'E'
     C*****EXABFL        ANDEQ     'Y'
     C*****ELARSM        ANDNE     'Y'
     c                   If        (turn = 'E') And
     c                             ((EXabFl = 'Y' And isUseSpoolOut) Or
     c                              (BDPRMNA = 'Y' And isUseXML)) And
     c                             (ELarSm <> 'Y')
     C*** End   Chg *** 09/26/23 ***************************************
      *
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C     XXFND         ANDEQ     5
     C     XXDPDU        ANDGT     0
     C                   EXSR      W7321C
     C                   OTHER
     C     XXUPAR        IFEQ      'Y'
     C     XXDPDU        ANDGT     0
     C                   EXSR      W7321C
     C                   ENDIF
     C                   ENDSL
     C*
     C**WRITE*A*RECORD TO THE INVOICE WORKFILE.
     C*
     C                   ENDIF                                                  TURN IFEQ
     C*
     C*  THIS IS THE LAST PAGE OF THE BILL, PRINT TOTALS AND SUCH.
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C     XXPAG4        IFEQ      *ZERO
     C     XXPAGE        OREQ      *ZERO
     C     TURN          ANDEQ     'E'
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C                   ADD       1             XXPAGE
     C                   EXCEPT    BOT
     C                   EXCEPT    HEAD2
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C                   ADD       1             XXPAG4
     C                   EXCEPT    BOTT
     C                   EXCEPT    HEAD2T
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   MOVE      *OFF          *INOA
     C*
     C                   ELSE
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C                   ADD       1             XXPAGE
     C                   EXCEPT    PAG1
     C                   EXCEPT    HEAD2
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C                   ADD       1             XXPAG4
     C                   EXCEPT    PAG1T
     C                   EXCEPT    HEAD2T
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   MOVE      *OFF          *INOA
     C*
     C                   ENDIF                                                  XXPAG4 IFEQ
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAGE
     C                   EXCEPT    TOTH
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAG4
     C                   EXCEPT    TOTHT
     C                   ENDIF                                                  XXNEWF IFEQ
     C     CLTID         IFEQ      '102'
     C                   MOVEA     '00'          *IN(55)
     C                   MOVE      *OFF          *IN64
     C                   MOVE      *OFF          *IN91
     C                   MOVE      *OFF          *IN98
     C                   MOVE      *OFF          PRT6TH            1
     C                   MOVE      *OFF          PRT7TH            1
     C                   ENDIF
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C     *IN55         IFEQ      *ON
     C     *IN56         OREQ      *ON
     C     *IN64         OREQ      *ON
     C     *IN91         OREQ      *ON
     C     *IN98         OREQ      *ON
      *
     C     CLTID         IFEQ      '046'
     C                   EXCEPT    TOTA1#
     C                   ELSE
     C                   EXCEPT    TOTA1
     C                   ENDIF
      *
     C                   ELSE
      *
     C     CLTID         IFEQ      '046'
     C                   EXCEPT    TOTA#
     C                   ELSE
     C                   EXCEPT    TOTA
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   EXCEPT    TOTF
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C     *IN55         IFEQ      *ON
     C     *IN56         OREQ      *ON
     C     *IN64         OREQ      *ON
     C     *IN91         OREQ      *ON
     C     *IN98         OREQ      *ON
      *
     C     CLTID         IFEQ      '046'
     C                   EXCEPT    TOTA#1
     C                   ELSE
     C                   EXCEPT    TOTAT1
     C                   ENDIF
      *
     C                   ELSE
     C     CLTID         IFEQ      '046'
     C                   EXCEPT    TOTAT#
     C                   ELSE
     C                   EXCEPT    TOTAT
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C                   EXCEPT    TOTFT
     C                   MOVE      *ON           *INOA
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C*** Begin Del *** 08/05/24 ***************************************
     C*** Begin Add *** 09/26/23 ***************************************
     C*****isUseSpoolOut IfEQ      *On
     C*** End   Add *** 09/26/23 ***************************************
     C*** End   Del *** 08/05/24 ***************************************
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 08/05/24 ***************************************
     C     isUseSpoolOut IfEQ      *On
     C*** End   Add *** 08/05/24 ***************************************
     C*
     C     FILL          IFGT      8
     C                   ADD       1             XXPAG4
     C                   MOVE      *OFF          *IN51
     C*
     C                   EXSR      DOPAGE
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QFOOT71
     C                   OTHER
     C                   WRITE     QFOOT
     C                   ENDSL
     C                   WRITE     QHEAD
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QHD171
     C                   WRITE     QLN171
     C                   WRITE     QHD271
     C                   WRITE     QHD371
     C                   OTHER
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   WRITE     QHEAD2
     C                   WRITE     QHEAD3
     C                   ENDSL
     C                   WRITE     QBOX1
     C                   ENDIF                                                  FILL IFGT
     C*
     C                   ADD       1             XXPAG4
     C                   MOVE      *ON           *IN51
     C                   WRITE     QBOX2
     C                   WRITE     QLINE2
     C                   WRITE     QBOX3
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C                   Z-ADD     1             E                 2 0
     C                   MOVE      *BLANKS       QHDG
     C                   MOVE      *BLANKS       QPRC
     C                   Z-ADD     0             QTAM
     C                   MOVE      *BLANKS       QSGN
     C*
     C     1             DO        27            D                 2 0
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
     C*
     C     HDG(D)        IFNE      *BLANKS
     C     E             ANDLE     24
     C     *IN90         ANDEQ     *OFF
     C     HDG(D)        ANDNE     CON(9)
     C     HDG(D)        ORNE      *BLANKS
     C     E             ANDLE     24
     C     HDG(D)        ANDNE     CON(59)
     C     HDG(D)        ANDNE     CON(60)
     C     HDG(D)        ANDNE     CON(64)
     C     HDG(D)        ANDNE     CON(9)
     C*
     C                   MOVEL(P)  HDG(D)        QHDG(E)          30
     C                   MOVE(P)   PRC(D)        QPRC(E)           8
     C                   MOVE(P)   HDG(D)        XXTEST            3
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(61)
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(62)
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(63)
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(12)
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(40)
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(37)
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(43)
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(25)
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(33)
     C     *IN90         OREQ      *ON
     C     XXTEST        ANDEQ     '*  '
     C*** begin add ***  07/25/25  ******************************************
     C     *IN90         OREQ      *ON
     C     HDG(D)        ANDEQ     CON(94)
     C*** end add *****  07/25/25  ******************************************
     C                   Z-ADD     TAM(D)        QTAM(E)          11 2
     C                   MOVE      SGN(D)        QSGN(E)           1
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ADD       1             E
     C                   ENDIF                                                  HDG,A IFNE
     C*
     C                   OTHER
      /Free
       If (Hdg(D)  <> *blanks and E <= 24 and *in90 = *off) or (isContri = '0'
        and Hdg(D) <> *blanks and E <= 24 and Hdg(D) <> Con(1)
        and Hdg(D) <> Con(5)  and Hdg(d) <> Con(15)) or (isContri = '1'
        and Hdg(D) <> *blanks and E <= 23 and Hdg(D) <> Con(59)
        and Hdg(D) <> Con(60) and Hdg(d) <> Con(64));
      /End-free
     C*
     C                   If        cltID = '102' And hdg(d) = dbcrHdg And
     C                             *in90 = *On
     C                   Iter
     C                   Endif
     C                   MOVEL(P)  HDG(D)        QHDG(E)          30
     C                   MOVE(P)   PRC(D)        QPRC(E)           8
     C                   MOVE(P)   HDG(D)        XXTEST            3
     C*
      /Free
       If *in90 = *off or (*in90 = *on and Hdg(D) = Con(8) and isContri ='0')
        or (*in90 = *on and Hdg(D) = Con(9)) or (*in90 = *on and Hdg(D) =
        Con(10) and isContri = '0') or (*in90 = *on and Hdg(D) = Con(11) and
        isContri = '0') or (*in90 = *on and Hdg(D) = Con(12)) or (*in90 = *on
        and Hdg(D) = Con(40)) or (*in90 = *on and Hdg(D) = Con(25)) or
        (*in90 = *on and Hdg(D) = Con(37)) or (*in90 = *on and Hdg(D) = Con(43))
        //*** begin add ***  07/25/25  ******************************************
        or (*in90 = *on and Hdg(D) = Con(94))
        //*** end add *****  07/25/25  ******************************************
        or (*in90 = *on and Hdg(D) = Con(53) and isContri = '0')
        or (*in90 = *on and Hdg(D) = Con(54) and isContri = '1')
        or (*in90 = *on and Hdg(D) = Con(55) and isContri = '1')
        or (*in90 = *on and Hdg(D) = Con(57) and isContri = '1')
        or (*in90 = *on and xxTest = '*');
      /End-free
     C                   Z-ADD     TAM(D)        QTAM(E)          11 2
     C                   MOVE      SGN(D)        QSGN(E)           1
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   ADD       1             E
     C                   ENDIF                                                  HDG,A IFNE
     C*
     C                   ENDSL
     C*
     C                   ENDDO                                                  1 DO 27 D
     C*
     C                   Z-ADD     XXPRO         X1PRO            11 2
     C                   Z-ADD     XXINST        X1INST           11 2
     C                   Z-ADD     XXMFEE        X1MFEE            7 2
     C                   Z-ADD     XXEXCN        X1EXCN           11 2
     C                   Z-ADD     XXSFEE        X1SFEE           11 2
     C                   Z-ADD     ELTXDU        X2TXDU           11 2
     C                   Z-ADD     XXDPRL        X1DPRL            9 2
     C                   Z-ADD     XXDPDU        X1DPDU           11 2
     C                   Z-ADD     XXDPDU        Q1NETP           11 2
     C                   MOVE      XXSFHD        X2SFHD           20
     C                   MOVE      XXTHD         X2THD            20
     C                   MOVE      XX1STH        X11STH           20
     C                   MOVE      X196          X1196             9 6
     C                   Z-ADD     XX1STB        X11STB           11 2
     C                   MOVE      XX2NDH        X12NDH           20
     C                   MOVE      X296          X1296             9 6
     C                   Z-ADD     XX2NDB        X12NDB           11 2
     C                   MOVE      XX3RDH        X13RDH           20
     C                   MOVE      X396          X1396             9 6
     C                   Z-ADD     XX3RDB        X13RDB           11 2
     C                   MOVE      XX4THH        X14THH           20
     C                   MOVE      X496          X1496             9 6
     C                   Z-ADD     XX4THB        X14THB           11 2
     C                   MOVE      XX5THH        X15THH           20
     C                   MOVE      X596          X1596             9 6
     C                   Z-ADD     XX5THB        X15THB           11 2
     C                   MOVE      XX6THH        X16THH           20
     C                   MOVE      X696          X1696             9 6
     C                   Z-ADD     XX6THB        X16THB           11 2
     C                   MOVE      XX7THH        X17THH           20
     C                   MOVE      X796          X1796             9 6
     C                   Z-ADD     XX7THB        X17THB           11 2
     C*
     C* COPY ALL BALANCE FIELDS TO GENERIC VARIABLES.
     C*
     C                   EXSR      DOBAL
     C*
     C                   EXSR      DOPAGE
     C*
      *
     C                   MOVE      *OFF          *IN36
     C     CLTID         IFEQ      '114'
     C                   MOVE      *ON           *IN36
      *
     C                   CALL      'WA082R'
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    XXEMP#
     C                   PARM                    XXFYR
     C                   PARM                    XXDIV
     C                   PARM      XXDUED        XXDUDT
     C                   PARM      Q1NETP        XXAMT
     C                   PARM                    XXSCAN           41
     C                   ENDIF
       //*** Begin Chg *** 09/12/24 ************************************
       If Q1netP > 0 And
          AuditCreditExists(ELco#:ELfnd:ELfyr:ELemp#:ELdiv:sysDt);
          x1DueD = 'Past Due';
       EndIf;
       //*** End   Chg *** 09/12/24 ************************************
       //*** Begin Add *** 09/26/23 ****************************************
       //Print Premiums
       //*** Begin chg *** 09/26/23 ****************************************
       //*** If isUseXml;
       hasPayrolls = HasDivPayrolls(EMco#:EMfnd:EMemp#:EMdiv:ELfyr);
       If isUseXml And hasPayrolls;
       //*** End   Chg *** 09/26/23 ****************************************
          ScriptHeader(xmlBillName);
          ScriptPremium(xmlBillName);
          ScriptBalance(xmlBillName);
       Endif;
       //*** End   Add *** 09/26/23 ****************************************
      *
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QPRM171
     C                   WRITE     QPREM2
     C                   WRITE     QFOOT71
     C                   OTHER
     C                   WRITE     QPREM1
     C                   WRITE     QPREM2
     C                   WRITE     QFOOT
     C                   ENDSL
       //*** Begin Add *** 09/26/23 ****************************************
       EndIf;
       //*** End   Add *** 09/26/23 ****************************************
     C*
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C                   ELSE                                                   *IN54 = *OFF
     C*
     C     XXPAG4        IFEQ      *ZERO
     C     XXPAGE        OREQ      *ZERO
     C     TURN          ANDEQ     'E'
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAGE
     C                   EXCEPT    BOT
     C                   EXCEPT    HEAD2
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOF
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAG4
     C                   EXCEPT    BOTT
     C                   EXCEPT    HEAD2T
     C                   EXCEPT    HEAD3T
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   MOVE      '0'           *INOA
     C*
     C                   ELSE
     C*
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAGE
     C                   EXCEPT    PAG1
     C                   EXCEPT    HEAD2
     C                   EXCEPT    HEAD3
     C                   MOVE      '0'           *INOF
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAG4
     C                   EXCEPT    PAG1T
     C                   EXCEPT    HEAD2T
     C                   EXCEPT    HEAD3T
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   MOVE      '0'           *INOA
     C*
     C                   ENDIF                                                  XXPAG4 IFEQ
     C*
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAG4
     C                   MOVE      *OFF          *IN51
     C*
     C                   EXSR      DOPAGE
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEQ      *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QFOOT71
     C                   OTHER
     C                   WRITE     QFOOT
     C                   ENDSL
     C                   WRITE     QHEAD
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QHD171
     C                   WRITE     QLN171
     C                   WRITE     QHD271
     C                   WRITE     QHD371
     C                   OTHER
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   WRITE     QHEAD2
     C                   WRITE     QHEAD3
     C                   ENDSL
     C                   WRITE     QBOX1
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   MOVE      *OFF          XX1ST
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C                   ENDIF                                                  *IN54 IFEQ
     C*
     C* DO NOT EXCEPT THESE TOTALS FOR AUDITED BILLS.
     C*
     C     BILL          IFEQ      'E'
     C                   Z-ADD     0             XXBAL            11 2
     C                   Z-ADD     0             XXTLDU            9 2
     C                   Z-ADD     0             XXDEPA            9 2
     C                   Z-ADD     0             XXDPRP            9 2
     C                   Z-ADD     0             XXDUED            8 0
     C                   Z-ADD     0             XXECDU           11 2
     C                   Z-ADD     0             XXSFDU           11 2
     C                   Z-ADD     0             XXMFDU            7 2
     C                   Z-ADD     0             X1WKDT            8 0
     C                   Z-ADD     0             ZZ                3 0
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEQ      *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    TOTS
     C                   EXCEPT    TOTST
     C                   EXCEPT    TOTE
     C                   EXCEPT    TOTET
     C                   EXCEPT    TOTE1
     C                   EXCEPT    TOTS1
     C                   EXCEPT    TOTET1
     C                   EXCEPT    TOTST1
     C                   EXCEPT    SUME
     C                   EXCEPT    SUME1
     C                   EXCEPT    SUMET
     C                   EXCEPT    SUMET1
     C                   EXCEPT    SUMS
     C                   EXCEPT    SUMS1
     C                   EXCEPT    SUMST
     C                   EXCEPT    SUMST1
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   MOVE      *BLANKS       XXAD1            40
     C                   MOVE      *BLANKS       XXAD2            40
     C                   MOVE      *BLANKS       XXAD3            40
     C                   MOVE      *BLANKS       XXNAME           40
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEQ      *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    HDG114
     C                   EXCEPT    HDT114
     C                   EXCEPT    NOTE
     C                   EXCEPT    NOTET
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   ENDIF                                                  BILL IFEQ E
     C*
     C                   MOVE      '0'           XXPRT
     C                   END
     C*
     C*  IF MORE PAYROLL RECORDS TO READ, RETURN TO PAYROLL FILE
     C*
     C     XXRATE        IFEQ      'Y'
     C                   GOTO      RTAG
     C                   ENDIF
     C     X1RATE        IFEQ      'Y'
     C                   GOTO      RTAG1
     C                   ENDIF
     C     *IN54         IFEQ      '1'
     C                   READP     PEP01                                  50
     C     *IN50         IFEQ      '0'
     C     EPCO#         ANDEQ     XXCO#
     C     EPFND         ANDEQ     XXFND
     C     EPEMP#        ANDEQ     XXEMP#
     C     EPDIV         ANDEQ     XXDIV
     C                   GOTO      READPR
     C                   END
     C                   END
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   CLOSE     W7615O
     C                   If         divCombine = *off and CLTID = '133' And
     C                              X2prt = '1'
      *
     C                              Or (divCombine = *on and CLTID = '133' and
     C                                  xxDivC = 1 and  ELarsm = *blanks)
      *
     C                   Select
     C                   When       Turn = 'E'
     C                   Movel(p)  'P'           x3Splf
     C                   Move      xxEmp#        x3Splf
     C                   When       Turn = 'A'
     C                   Move      *Zeros        x3Splf
     C                   Movel     'A'           x3Splf
     C                   Move      XXagt#        x3Splf
     C                   Endsl
      *
     C                   Move      *blanks       x3Hard            1
     C                   Move      *blanks       x3Save            4
     C                   Move      *blanks       x3Hold            4
     C                   Move      *blanks       x3Cpys            2
     C                   Move      'X'           x3Dart            1
     C                   Move      *blanks       x3Fold           80
     C                   Move      *blanks       x3Dcid           12
     C
     C*** Begin Add *** 09/26/23 ***************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 ***************************************
     C                   Call      'W7615C3'
     C                   Parm                    x3Splf
     C                   Parm                    x3Hard
     C                   Parm                    x3Save
     C                   Parm                    x3Hold
     C                   Parm                    x3Cpys
     C                   Parm                    x3Dart
     C                   Parm                    x3Fold
     C                   Parm                    x3Dcid
      /Free
        If QQRMCD = *blanks;
          Monitor;
             cmdString = 'DLTSPLF FILE(W7615O) SPLNBR(*LAST)';
             length = 255;
             QCMDEXC(cmdString:length);
          On-Error;
          EndMon;
        Endif;
          If X1Hold <> '*YES';
             //* Release spool file for renamed bill.
             Monitor;
                cmdString = 'RLSSPLF FILE(' + x3splf + ') SPLNBR(*LAST)';
                length = 255;
                QCMDEXC(cmdString:length);
             On-Error;
             EndMon;
          Endif;
      /End-Free
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   Endif
     C*
     C* Combine division bills together if report #6 has been set up.
     C*
     C                   If        divCombine = *on and turn = 'E'
     C                             and x2Prt = '1'
     C                             and (xxDivC > 1 or ELARSM = 'Y')
     C*
     C                   Move      *on           divPrints
     C*
     C                   Select
     C                   When       CltId = '133'
     C                   Movel(p)  'P'           x3Splf
     C                   Move      xxEmp#        x3Splf
      *
     C                   Other
     C                   Movel(p)  'B'           x3Splf           10
     C                   Move      xxEmp#        x3Splf
     C                   Endsl
      *
     C                   Move      *blanks       x3Hard            1
     C                   Move      *blanks       x3Save            4
     C                   Move      *blanks       x3Hold            4
     C                   Move      *blanks       x3Cpys            2
     C                   Move      *blanks       x3Dart            1
     C                   Move      *blanks       x3Fold           80
     C                   Move      *blanks       x3Dcid           12
     C
     C*** Begin Add *** 09/26/23 ***************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 ***************************************
     C                   Call      'W7615C3'
     C                   Parm                    x3Splf
     C                   Parm                    x3Hard
     C                   Parm                    x3Save
     C                   Parm                    x3Hold
     C                   Parm                    x3Cpys
     C                   Parm                    x3Dart
     C                   Parm                    x3Fold
     C                   Parm                    x3Dcid
      *
       //* Delete W7615O spool file
       If cltid = '133' and QQRMCD = *blanks;
          Monitor;
             cmdString = 'DLTSPLF FILE(W7615O) SPLNBR(*LAST)';
             length = 255;
             QCMDEXC(cmdString:length);
          On-Error;
          EndMon;
       Endif;
      /End-Free
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   Endif                                                  If divCombine = *on
     C                   If        divCombine = *on and turn = 'A'
     C                             and x2Prt = '1'
     C                             and (xxDivC > 1 or ELARSM = 'Y')
     C                             and CltId = '133'
     C*
     C                   Move      *on           divPrints
     C*
     C                   Move      *Zeros        x3Splf
     C                   Movel     'A'           x3Splf
     C                   Move      XXagt#        x3Splf
      *
     C                   Move      *blanks       x3Hard            1
     C                   Move      *blanks       x3Save            4
     C                   Move      *blanks       x3Hold            4
     C                   Move      *blanks       x3Cpys            2
     C                   Move      *blanks       x3Dart            1
     C                   Move      *blanks       x3Fold           80
     C                   Move      *blanks       x3Dcid           12
     C
     C*** Begin Add *** 09/26/23 ***************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 ***************************************
     C                   Call      'W7615C3'
     C                   Parm                    x3Splf
     C                   Parm                    x3Hard
     C                   Parm                    x3Save
     C                   Parm                    x3Hold
     C                   Parm                    x3Cpys
     C                   Parm                    x3Dart
     C                   Parm                    x3Fold
     C                   Parm                    x3Dcid
     C
       //* Delete W7615O spool file
          Monitor;
             cmdString = 'DLTSPLF FILE(W7615O) SPLNBR(*LAST)';
             length = 255;
             QCMDEXC(cmdString:length);
          On-Error;
          EndMon;
      /End-Free
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   Endif                                                  If divCombine = *on
     C                   ENDIF                                                  XXNEWF IFEQ
     C     TURN          IFEQ      'E'
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   CLOSE     QSYSPRT
     C                   ENDIF                                                  XXNEWF IFEQ 9
     C                   ADD       1             XXSPOL            4 0
     C*
     C     X2PRT         IFEQ      '1'
      *
     C     XXCALC        IFEQ      'Y'
     C     XXFADC        ANDNE     *BLANK
     C     XXFASC        ANDNE     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C*** Begin Del *** 08/28/25 ***************************************
     C***  isUseSpoolOut AndEq     *On
     C*** End   Del *** 08/28/25 ***************************************
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXSR      DARTS1
     C                   ENDIF
      *
     C     QQRMCD        IFEQ      *BLANKS
     C*** Begin Add *** 09/26/23 ***************************************
     C*** Begin Del *** 08/28/25 ***************************************
     C***  isUseSpoolOut AndEq     *On
     C*** End   Del *** 08/28/25 ***************************************
     C*** End   Add *** 09/26/23 ***************************************
     C                   MOVE      'Y'           @@MDR             1
     C                   EXSR      DARTS
     C                   ENDIF
     C     QQRMCD        IFNE      *BLANKS
     C                   MOVE      XXSPOL        QQSPL#
     C                   MOVE      XXFND         QPFND
     C                   MOVE      XXEMP#        QPEMP#
     C                   MOVE      XXFYR         QPFYR
     C                   MOVE      XXDIV         QPDIV             5
     C                   MOVEL     F2FOLD        QQSFLD
     C                   MOVE      *BLANKS       QQFOLD           80
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVE      'BL'          QQPRE             2
     C*
     C* IF RESOLVING NEW FORMAT BILL THEN CHANGE DOC NAME TO .PDF
     C*
     C     XXNEWF        IFEQ      'Y'
     C                   MOVE      'P'           QQRTCD
     C                   ELSE
     C                   MOVE      *BLANK        QQRTCD
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   CALL      'SNXTDCID'
     C                   PARM                    QQFOLD
     C                   PARM                    QQDCID
     C                   PARM                    QQPRE
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQRTCD            1
     C                   PARM      '00000'       QQSUB#            5
     C                   PARM      *BLANKS       QQPFLR           80
     C*
     C     QQPFLR        IFNE      *BLANKS
     C*
     C     QQPFLR        CAT       '/':0         QQPFLR
     C*
     C     F2FOLD        IFEQ      *BLANKS
     C     QQPFLR        CAT(P)    'WCPOLS':0    QQSFLD
     C                   ELSE
     C     QQPFLR        CAT(P)    F2FOLD:0      QQSFLD
     C                   ENDIF                                                  F2FOLD IFEQ
     C*
     C                   ENDIF                                                  QQPFLR IFNE
     C*
     C* IF RESOLVING NEW FORMAT BILL THEN CHANGE DOC NAME TO .PDF
     C*
     C*
     C*
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 ***************************************
     C                   CALL      'W7615C'
     C                   PARM                    QQSPL#            4
     C                   PARM                    QQDCID           12
     C                   PARM                    QQPOL#
     C                   PARM                    QPDIV
     C                   PARM                    QQSFLD           40
     C                   PARM                    QQFOLD
     C                   PARM                    X1HOLD
     C                   PARM      'Y'           QQHARD            1
      /Free
       If cltid = '133';
       //* Delete W7615O spool file
          Monitor;
             cmdString = 'DLTSPLF FILE(W7615O) SPLNBR(*LAST)';
             length = 255;
             QCMDEXC(cmdString:length);
          On-Error;
          EndMon;
       Endif;
      /End-Free
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C*** Begin Del *** 08/28/25 ***************************************
     C***  isUseSpoolOut IfEq      *On
     C*** End   Del *** 08/28/25 ***************************************
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXSR      DARTS
     C*** Begin Add *** 09/26/23 ***************************************
     C*** Begin Del *** 08/28/25 ***************************************
     C***                EndIf
     C*** End   Del *** 08/28/25 ***************************************
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C                   ELSE
     C*
     C     XXNEWF        IFEQ      *BLANK
     C                   Z-ADD     55            LENGTH
     C                   CALL      'QCAEXEC'
     C                   PARM                    QCA(1)
     C                   PARM                    LENGTH           15 5
     C                   ENDIF
     C*
     C                   ENDIF                                                  QQRMCD IFNE
      /Free

       //* If new running new format bill, release the spool file if needed.

                         If X1Hold <> '*YES' and XXNewF = 'Y' and x2Prt = '1';

                            Monitor;
                               cmdString = 'RLSSPLF FILE(W7615O) SPLNBR(*LAST)';
                               length = 255;
                               QCMDEXC(cmdString:length);
                            On-Error;
                            EndMon;

                         Endif;
      /End-free
     C*
     C*  UPDATE THE AUDIT TRACKING LOG FILE
     C*
     C*
     C* Update dates based on the type of processing: Print; print &
     C* Calculate; print, calculate and update
     C     AUKY          CHAIN     PAU01                              50
     C     *IN50         IFEQ      '0'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C* Always Update Audit Print Date
     C                   MOVEL     SRDTO         AUAPDT                         Audit Print Dt
     C* Update Audit Calculate Date
     C     XXCALC        IFEQ      'Y'
     C                   MOVEL     SRDTO         AUACDT                         Calculate Date
     C                   ENDIF                                                  XXCALC = "Y"
      *
     C* Update Audit Calculate Date and Final Audit Date
     C     XXUPAR        IFEQ      'Y'
     C                   MOVEL     SRDTO         AUACDT                         Calculate Date
     C                   MOVEL     SRDTO         AUPRDT                         Final Aud PrtDt
      *
     C     AURCVD        IFEQ      *ZERO
     C                   MOVEL     SRDTO         AURCVD
     C                   ENDIF
      *
     C*  SET REAUDIT FLAG(AURVFL) TO 'Y' IF MORE THAN ONE AUDIT RECORD
     C                   Z-ADD     *ZERO         CNT               1 0
     C                   MOVE      'D'           XXTYPE
     C     EAKY1         SETGT     PEA01
     C     EAKEY         READPE    PEA01                                  60
     C     *IN60         DOWEQ     *OFF
     C     CNT           ANDLT     2
     C                   ADD       1             CNT
     C     EAKEY         READPE    PEA01                                  60
     C                   ENDDO
     C     CNT           IFEQ      2
     C                   MOVE      'Y'           AURVFL
     C                   ELSE
     C                   MOVE      'N'           AURVFL
     C                   ENDIF
     C*
     C     XXREV         IFEQ      'Y'
     C                   MOVE      'Y'           AUREFL
     C                   MOVE      'N'           AURSGN
     C                   MOVE      *ZEROS        AUMAIL
     C                   MOVE      *ZEROS        AUCNTD
     C                   MOVE      *BLANKS       AUSTAF
     C                   MOVE      *BLANKS       AUCOM1
     C                   MOVE      *BLANKS       AUCOM2
     C                   MOVE      *BLANKS       AUCOM3
     C                   MOVE      *BLANKS       AUCOM4
     C                   ENDIF                                                  XXREV IFEQ
     C                   ENDIF                                                  XXUPAR IFEQ
     C                   MOVEL     Q1USER        AUUSER
     C                   MOVEL     Q1PGM         AUPGM
     C                   Z-ADD     DSTIME        AUTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         AUDT
     C                   UPDATE    PAU01
     C                   ENDIF                                                  *IN50 IFEQ
      *
     C*
     C                   MOVE      *BLANK        X2PRT
     C                   ENDIF                                                  X2PRT IFEQ
     C*
     C                   ENDIF                                                  TURN IFEQ
     C                   ENDIF                                                  SKIPE IFEQ
     C*
     C                   ENDIF                                                  ELPTDT IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     SVEMP#        IFNE      *ZERO
     C     XXUPAR        ANDEQ     'Y'
     c     SVEMP#        OREQ      *ZERO
     C     TURN          ANDEQ     'A'
     C     XXUPAR        ANDEQ     'Y'
     C*
     C     BLKEY2        SETLL     PBL01
     C     BLKEY2        READE     PBL01                                  60
     C     *IN60         DOWEQ     *OFF
     C     BLFLG2        IFEQ      'Y'
     C                   DELETE    PBL01
     C                   ENDIF
     C     BLKEY2        READE     PBL01                                  60
     C                   ENDDO
     C                   ENDIF
     C*
     C*
     C     SVEMP#        IFEQ      *ZERO
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C  N59EMKEY1        READE     PEM01                                  60
     C   59EMKEY         READE     PEM01                                  60
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*
     C     ENDA          TAG
     C*
     C                   Z-ADD     *ZEROS        XXPAGE
     C                   Z-ADD     *ZEROS        XXPAG4
     C     SVEMP#        CABEQ     *ZERO         READE
     C*
     C     END           TAG
     C     XXSUM         IFEQ      'Y'
     C     *IN59         ANDEQ     *OFF
     C                   EXSR      PRTSUM
     C                   ENDIF

      //*** Beg Add ***** 06/18/25 ****************************************
       If strXPFileSRD = *Blank;
          Exsr RmvCombBill;
       Endif;
      //*** End Add ***** 06/18/25 ****************************************
     C*
     C* Resolve the combined bill together if report #6 has been set up.
     C*
     C                   If        divCombine = *on and xxNewF = 'Y'
     C                             and turn = 'E' and divPrints = *on
     C*
     C                   Exsr      RslCombo
     C*
     C                   Endif                                                  If divCombine = *on
     C*
     C                   Clear                   divPrints
     C*
     C     OPENF         IFEQ      '1'
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   CLOSE     QSYSPRT1
     C                   MOVE      *BLANK        OPENF
     C                   ENDIF
     C*
     C* CLOSE LARGE DED ERROR REPORT IF OPEN
     C*
     C     FRSTE         IFEQ      *ON
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   Except    ERoute
     C                   CLOSE     QSYSPRT3
     C                   MOVE      *BLANK        FRSTE
     C                   ENDIF
     C*
     C     TURN          IFEQ      'E'
     C                   MOVE      'A'           TURN
     C     SVEMP#        CABEQ     *ZERO         TURNT
     C                   ENDIF                                                  TURN IFEQ
       //*** Begin Add *** 09/26/23 ****************************************
       // Close Work Files
       If isUseXml;
          Close WXBLP;
          Close WXBLP1;
          Close WXBLP2;
          //*** Beg Add ***** 06/18/25 **************************************
          //*** begin del ***  08/01/25  ****************************************
          //*****Close wxbll;
          //*** end del *****  08/01/25  ****************************************
          //*** End Add ***** 06/18/25 **************************************

          // Call Scriptura
          Select;
          When ldEndA <> *Blank and ldEndA <> *Zeros;
          When xmlFileName = *Blanks;
          Other;
             // Reorganize to make sure that premium notices show last
             // in combined document
             ReorganizeTemplates(xmlFileName);
             inXml = xmlFileName + '$';
             Monitor;
                GenScriptOut(inXml);

                //*** Begin Add *** 08/28/25 ***************************************
                //Check the S4401MSG data area to see if XML print was successful
                //Replace the old Audit Bill .PDF attachments in the SDCMP Audit iDarts remark
                //diary entries with the Scripture version created during the call to
                //STLETTER in GenScriptOut.
                In dsXmlMsg;
                If %Subst(dsXmlMsg:1:7) = 'Success'
                and QQRMCD <> *Blanks;
                  //Retrieve latest STLETTER diary entry for file names
                  SetGT (QCKEY:QQRMCD:sysdt) pcm01;
                  ReadPE(N) (QCKEY:QQRMCD:sysdt) pcm01;
                  Dow Not %EOF;
                  If  CMIUSR = Q1USER
                  and CMPGM = 'STLETTER';
                    SDCMLDsv = SDCMLDds;
                    //Extract the last folder name (after last /)
                    wkFolder = QQFOLD;
                    SlashPos = 1;
                    Dow SlashPos <> 0;
                      SlashPos = %Scan('/' : wkFolder);
                      If SlashPos <> 0;
                        SlashPos += 1;
                        wkFolder = %Subst(wkFolder:SlashPos);
                      Else;
                        Leave;
                      Endif;
                    Enddo;
                    //Retrieve W7700R Audit diary entries & replace with file names from STLETTER
                    SetGT (CMKEY) pcm01;
                    ReadPE (CMKEY) pcm01;
                    Dow Not %EOF;
                      If  CMIUSR = Q1USER
                      and CMCDAT = sCMCDAT
                      and CMPGM = 'W7700R'
                      and CMDCID = QQDCID
                      and CMFOLD = wkFolder;
                        CMDCID = sCMDCID;
                        CMFOLD = sCMFOLD;
                        CMUNCN = sCMUNCN;
                        update pcm01;
                      Else;
                        Unlock SDCMLD;
                      Endif;
                      ReadPE (CMKEY) pcm01;
                    Enddo;
                    Leave;
                  Endif;
                  ReadPE(N) (QCKEY:QQRMCD:sysdt) pcm01;
                  Enddo;
                Endif;
                //*** End   Add *** 08/28/25 ***************************************

             On-error;
             Endmon;
          Endsl;
       Endif;
       //*** End   Add *** 09/26/23 ***************************************
     C*
     C                   SETON                                        LR
     C*
     C*
     C*==================================================================*
     C*   Set-up and Initialize            ** Subroutine **   S R 1 S T  *
     C*==================================================================*
     C*
     CSR   SR1ST         BEGSR
     C*
     C*  INITIALIZE FIELDS
     C*
     C                   TIME                    DSDTTM
     C*
     C     COKEY         CHAIN     SMCOL                              60
     C*
     C*  REMOVE LEADING BLANKS FROM COMPANY NAME
     C*
     C                   Z-ADD     1             Z                 2 0
     C     *BLANK        LOOKUP    CONM(Z)                            60
     C                   MOVEA     CONM(Z)       XXCONM           64
     C*
     C*  OPEN PRINT FILE
     C*
     C*
     C                   MOVE      '0'           *IN38
     C                   MOVE      '1'           *IN61
     C*
     C                   ENDSR
     C*================================================================
     C*  DARTS - SUBROUTINE TO WRITE A DARTS RECORD
     C*================================================================
     C     DARTS         BEGSR
     C*
     C*  ESTABLISH KEY
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     XXFND         QCFND
     C                   Z-ADD     XXEMP#        QCEMP#
     C                   Z-ADD     XXDIV         QCDIV
     C                   MOVE      *BLANK        QCQTE
     C********
     C***GET*LAST SEQUENCE USED
     C********
     C********
     C***LOAD*WORK FIELDS TO DARTS FILE FIELDS
     C********
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  QCKEY         Q#KEY            40
     C                   MOVE      *BLANKS       Q#COMM
     C     *IN63         IFEQ      *OFF
     C                   MOVEL(P)  RMK(1)        Q#COMM          256
     C                   ELSE                                                   *IN63 = Off
     C                   MOVEL     RMK(2)        Q#COMM
     C                   ENDIF                                                  *IN63 = Off
     C     XXSUMD        IFEQ      'Y'
     C                   MOVEL     RMK(5)        Q#COMM
     C                   ENDIF                                                  SUMD = 'Y'
     C     xxSUMD        IFEQ      'C'
     C     *IN63         IFEQ      *OFF
     C                   Movel(p)  RMK(7)        Q#COMM
     C                   ELSE
     C                   Movel(p)  RMK(8)        Q#COMM
     C                   ENDIF                                                  *IN63 = *OFF
     C                   ENDIF                                                  SUMD = 'C'
      /Free
       If QQREV <> 'Y' and ##mass = 'Y' and ELaudsr = 'Y' and EXaudsr <> *blank;
         XXRM = EXaudsr;
       Endif;
      /End-free
     C                   MOVEL(P)  XXRM          Q#RMCD            5
     C     Q#COMM        CAT       UPUSNM:1      Q#COMM
     C     Q#COMM        CAT       ', on':0      Q#COMM
     C                   Z-ADD     XXDATE        DSDT2
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
     C                   MOVE      XXFYR         XAFYR             2
     C     Q#COMM        CAT       XAFYR:1       Q#COMM
     C                   ENDIF                                                  FYR > 99
     C     Q#COMM        CAT       '.':0         Q#COMM
     C     Q#COMM        CAT       RMK(3):2      Q#COMM
     C                   MOVEL(P)  ELBSCD        XXWK9A
     C     Q#COMM        CAT       XXWK9A:1      Q#COMM
     C     Q#COMM        CAT       '.':0         Q#COMM
     c                   If        isContri = '1'
     c     Q#COMM        CAT       RMK(6):2      Q#COMM
     c                   Else
     c     Q#COMM        CAT       RMK(4):2      Q#COMM
     c                   Endif
     C     Q#COMM        CAT       '$':1         Q#COMM
     C     XXSUMD        IFEQ      'Y'
     C     XXPRO         ADD       XXEXCN        ELAAPR
     C                   ENDIF                                                  PRO = 'Y'
     C     xxSUMD        IFEQ      'C'
     C                   Z-add     xcAAPR        ELAAPR
     C                   Endif                                                  xxSUMD = 'C'
     C                   Z-ADD     ELAAPR        XXWK90            9 0
     C     ELAAPR        IFNE      0
     C                   MOVE      XXWK90        XXWK9A            9
     C     '0'           CHECK     XXWK9A        X
     C                   SUBST(P)  XXWK9A:X      XXWK9A
     C                   ELSE                                                   ANPR <> 0
     C                   MOVEL(P)  '0'           XXWK9A
     C                   ENDIF                                                  ANPR <> 0
     C     Q#COMM        CAT       XXWK9A:0      Q#COMM
     C     Q#COMM        CAT       '.':0         Q#COMM
     C     Q#COMM        CAT       '00':0        Q#COMM
     C     Q#COMM        CAT       '.':0         Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  QQDCID        Q#DCID           12
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  Q1USER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  QQFOLD        Q#FOLD           80
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C     XXNEWF        IFEQ      'Y'
     C                   MOVEL(P)  'O'           Q#IMGF            1
     C                   ELSE
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   MOVEL(P)  *BLANKS       Q#IPTH           10
     C                   MOVE      *ZEROS        Q#PIDX            5
     C                   MOVEL(P)  *BLANK        Q#QPTY            1
     C                   MOVEL(P)  *BLANKS       Q#DCTP            5
     C                   MOVEL     *BLANKS       Q#CSID            2
     C                   MOVEL     *BLANKS       Q#CODE            2
      *
      *  CALL DA700R TWICE; THE FIRST TIME SO THT Q1USER GETS THE REGULAR
      *  REMARK; THE SECOND TIME WITH Q#USER EQUAL TO BLANKS WHICH WILL
      *  TRIGGER DA700R TO WRITE THE REMARK TO MULTIPLE USERS WITH THE
      *  NEW AUDIT ACTIVITY CODE IF THE SYSTEM DATE IS FUTURE OF THE
      *  POLICY EFF DATE + THE # OF MTHS PAST EFF DATE VALUE IN WMEXP.
      *  THE REMARK SHOULD SAY THAT:
      *  "AUDIT OCCURRED AFTER xx MOS OF EFF DATE - REVIEW/REVISE WCSTAT"
      *  WHERE xx = EXAA#M
      *
     C                   DO        2             COUNT
     C     COUNT         IFEQ      2
     C     @@MDR         OREQ      'Y'
      *
      *
     C     COUNT         IFNE      2
     C                   Z-ADD     2             COUNT
     C                   ENDIF
      *
      *  CALCULATE WHAT (ELPFDT + EXAA#M) WOULD BE AND COMPARE
      *  TO SYSTEM DATE (DSDATE). IF THE SYSTEM DATE IS LESS
      *  THAN (ELPFDT + EXAA#M), LEAVE - DO NOT CALL DA700R FOR
      *  MULTIPLE USERS
      *
     C*CONVERT POLICY EFF DATE TO 8 DIGIT FORMAT TO BE USED BY API
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXFROM            8 0
     C*
     C*ADD THE MONTHS TO THE POLICY EFF. DATE
     C*
     C                   CALL      'SRFMTH8'
     C                   PARM                    XXFROM            8 0
     C                   PARM                    EXAA#M            3 0
     C                   PARM                    ##ERR             1
     C*
     C     ##ERR         IFNE      '1'
     C*
     C*CONVERT TODAY'S DATE TO 7 DIGIT FORMAT
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X#DSDT            7 0
     C*
     C*
     C*CONVERT THE EFF DATE ADDED WITH MONTHS TO 7 DIGIT FORMAT
     C*
     C                   Z-ADD     XXFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X#FUDT            7 0
     C*
     C                   ENDIF
     C*
     C*COMPARE THE DATES
     C     X#DSDT        IFLT      X#FUDT
     C                   LEAVE
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       Q#COMM
     C                   MOVE(P)   EXAA#M        XXAA#M            2
     C                   MOVEL     AUDVB1        Q#COMM
     C                   CAT       XXAA#M:1      Q#COMM
     C                   CAT       AUDVB2:1      Q#COMM
     C                   MOVE      *BLANKS       Q#USER
     C                   MOVEL(P)  EXAACD        Q#RMCD
     C                   ENDIF

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
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C*  DARTS1 - SUBROUTINE TO WRITE A DARTS RECORD
     C*================================================================
     C     DARTS1        BEGSR
      *
      *   WRITE FINAL AUDIT READY DIARY REMARK IF RUNNING A CALC & PRINT
      *   AND THE TWO NEW PARMS ARE NOT BLANK
      *
     C                   MOVE      *BLANKS       QCKEY
     C                   Z-ADD     XXCO#         QCCO#
     C                   Z-ADD     XXFND         QCFND
     C                   Z-ADD     XXEMP#        QCEMP#
     C                   Z-ADD     XXDIV         QCDIV
     C                   MOVE      *BLANK        QCQTE
     C                   MOVEL(P)  QCKEY         Q#KEY
     C                   MOVEL(P)  FAR1          Q#COMM
     C                   MOVEL(P)  XXFADC        Q#RMCD
     C                   MOVE      XXFYR         XBFYR
     C     Q#COMM        CAT       XBFYR:1       Q#COMM
     C     Q#COMM        CAT       FAR2:1        Q#COMM
     C     Q#COMM        CAT       '$':1         Q#COMM
     C                   Z-ADD     ELAAPR        XXWK90            9 0
     C     ELAAPR        IFNE      0
     C                   MOVE      XXWK90        XXWK9A            9
     C     '0'           CHECK     XXWK9A        X
     C                   SUBST(P)  XXWK9A:X      XXWK9A
     C                   ELSE                                                   ANPR <> 0
     C                   MOVEL(P)  '0'           XXWK9A
     C                   ENDIF                                                  ANPR <> 0
     C     Q#COMM        CAT       XXWK9A:0      Q#COMM
     C     Q#COMM        CAT       '.':0         Q#COMM
     C     Q#COMM        CAT       '00':0        Q#COMM
     C     Q#COMM        CAT       '.':0         Q#COMM
     C                   MOVE      *BLANKS       Q#COM2           19
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C                   MOVEL(P)  *BLANKS       Q#DCID           12
     C                   MOVEL(P)  'W0050R'      Q#PNAM           10
     C                   MOVE      *ZEROS        Q#SECL            3
     C                   MOVEL(P)  XXUSER        Q#USER           10
     C                   MOVEL(P)  Q1PGM         Q#PGM            10
     C                   MOVEL(P)  *BLANKS       Q#FOLD           80
     C                   MOVEL(P)  *BLANK        Q#DIFL            1
     C                   MOVEL(P)  Q1PGM         Q#IPGM           10
     C                   MOVEL(P)  *BLANK        Q#FL01            1
     C                   MOVEL(P)  *BLANK        Q#FL02            1
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  'N'           Q#SBYL            1
     C                   MOVEL(P)  *BLANKS       Q#APID            2
     C                   MOVEL(P)  *BLANKS       Q#IDOC           12
     C                   MOVEL(P)  *BLANKS       Q#IQUE           10
     C                   MOVEL(P)  *BLANK        Q#IMGF            1
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
      *
     C                   ENDSR
     C*================================================================
     C* SUBROUTINE TO DETERMINE IF A DATE RANGE CONTAINS FEBRUARY 29TH.
     C*================================================================
     C     SRLEAP        BEGSR
     C*
     C*  IF FEB 29 IS A VALID DATE AND IS IN THE RANGE, SUBTRACT ONE DAY
     C*
     C                   Z-ADD     0             XXLPDT
     C*
     C                   Z-ADD     XQFROM        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1FROM
     C*
     C                   Z-ADD     XQTO          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X1TO
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
     C                   MOVE      *BLANK        SRERR             1
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
     C*
     C* PRINT THE INSTALLMENT SUMMARY PAGE FOR THE POLICY
     C*
     C     PRTSUM        BEGSR
     C*
     C                   MOVE      XSDIV         XXDIV
     C*** Begin Add *** 05/07/24 *******************************************
     C                   Eval      isSummary = *On
     C*** End   Add *** 05/07/24 *******************************************
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C* REBUILD ADDRESS FOR SUMMARY
     C*
     C                   MOVEL     SXPOL#        QQPOL#
     C                   MOVEL     SXFROM        XXFROM
     C                   MOVEL     SXTO          XXTO
     C                   MOVEL     SXANNV        XXANNV
     C                   MOVE      SXIN87        *IN87
     C                   MOVEL     SXEMN1        EMN(1)
     C                   MOVEL     SXEMN2        EMN(2)
     C                   MOVEL     SXEMN3        EMN(3)
     C                   MOVEL     SXEMN4        EMN(4)
     C                   MOVEL     SXEMN5        EMN(5)
     C                   MOVEL     SXAGT#        AHAGT#
     C                   MOVEL     SXNAME        ANAME
     C                   MOVEL     SXADD1        AADD1
     C                   MOVEL     SXACSZ        ACSZ
     C                   MOVE      SXIN69        *IN69
     C                   MOVEL     SXARE1        AGARE1
     C                   MOVEL     SXPHN1        AGPHN1
     C                   MOVE      *ON           *IN58
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C     RSKY          CHAIN     PRS01                              60
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      RSJRST        XXST
     C* Lets hold the state reporting division rating state and
     C* save it from being altered once details records are read so that
     C* Header record shows correct value of rating state not the detail
     C* recod value in header
     C                   MOVE      RSJRST        XXST1             2
     C     RRKY          SETGT     PRR01
     C     RRKY2         READPE    PRR01                                  60
     C*
     C*
     C                   MOVEL(P)  CON(27)       XX1STH           20
     C*
     C                   MOVEL(P)  CON(28)       XX2NDH           20
     C                   MOVEL(P)  CON(33)       XX3RDH           20
     C                   MOVEL(P)  CON(40)       XX4THH           20
     C                   MOVEL(P)  CON(41)       XX5THH           20
     C                   MOVEL(P)  CON(75)       XX6THH           20
     C                   MOVEL(P)  CON(76)       XX7THH           20
     C                   MOVEL(P)  CON(36)       XXSFHD           20
     C                   MOVEL(P)  CON(24)       XXTHD            20
     C     CLTID         IFEQ      '046'
     C                   MOVEL(P)  CON(26)       XX1STH
     C                   MOVEL(P)  CON(27)       XX2NDH
     C                   MOVEL(P)  CON(24)       XXTHD
     C                   ENDIF
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   OPEN      W7615O
     C                   MOVE      *ON           XX1ST
     C*
     C* BUILD STRINGS FOR NEW FORMAT BILL.
     C*
     C     ANAME         IFNE      *BLANKS
     C                   MOVEL     ANAME         XQAGNM           40
     C                   ELSE
     C                   MOVE      *BLANKS       XQAGNM
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       XQDIVH
     C                   MOVE      *BLANKS       XQDIV
     C                   MOVEL(P)  'Policy'      XQPROF           20
     C                   CAT       'For:':1      XQPROF
     C*
     C                   MOVEL(P)  'Print'       XQPROD           20
     C                   CAT       'Date:':1     XQPROD
     C*
     C                   MOVEL(P)  'For Peri'    XQPERD
     C                   CAT       'od:':0       XQPERD
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQPRDT           20
     C                   CAT       XXDAY:1       XQPRDT
     C                   CAT       ',':0         XQPRDT
     C                   CAT       XXYR:1        XQPRDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQPFDT           20
     C                   CAT       XXDAY:1       XQPFDT
     C                   CAT       ',':0         XQPFDT
     C                   CAT       XXYR:1        XQPFDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQDT
     C*
     C                   Z-ADD     XXMTH         X
     C                   MOVEL(P)  MNTH(X)       XQPTDT           20
     C                   CAT       XXDAY:1       XQPTDT
     C                   CAT       ',':0         XQPTDT
     C                   CAT       XXYR:1        XQPTDT
     C*
     C                   MOVEL(P)  XQPFDT        XQPOLP           45
     C                   CAT       ' to':1       XQPOLP
     C                   CAT       XQPTDT:1      XQPOLP
     C*
     C                   MOVE      XXCO#         ##CO#             3
     C                   MOVE      XXFND         ##FND             3
     C                   MOVE      XXEMP#        ##EMP#            9
     C                   MOVE      XXDIV         ##DIV             5
     C                   MOVE      'W'           ##ODR             1
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
     C                   MOVE      ##EMN1        XXEMN1           50
     C                   MOVE      ##EMN2        XXEMN2           50
     C                   MOVE      ##EMN3        XXEMN3           50
     C                   MOVE      ##EMN4        XXEMN4           50
     C                   MOVE      ##EMN5        XXEMN5           50
     C                   MOVE      ##EMN6        XXEMN6           50
     C                   MOVE      ##EMN7        XXEMN7           50
     C*
     C                   MOVE      *BLANKS       QDTXT1           80
     C                   MOVE      *BLANKS       QDTXT2           80
     C                   MOVE      *BLANKS       QQTXT1           80
     C                   MOVE      *BLANKS       QQTXT2           80
     C                   MOVE      *BLANKS       QQTXT3           80
     C                   Z-ADD     0             Q#DAYS            3 0
     C*
     C     FNKEY         CHAIN     PEX01                              60
     C                   EXSR      SRAGHD
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL     EXBTXT        QQTXT1
     C                   MOVE      EXBTXT        XQTEMP          160
     C                   MOVEL     XQTEMP        QQTXT2
     C                   MOVE      EXBTXT        QQTXT3
     C                   Z-ADD     EX#DYS        Q#DAYS
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C     QQRMCD        IFNE      *BLANKS
     C     TURN          IFEQ      'E'
     C     XXNEWF        ANDEQ     *BLANK
     C                   OPEN      QSYSPRT
     C                   MOVE      *ON           *INOF
     C                   Z-ADD     *ZEROS        XXPAGE            3 0
     C                   ENDIF                                                  TURN EQ 'E'
     C                   ENDIF                                                  DART EQ 'Y'
     C                   Z-ADD     0             XXPAG4            3 0
     C                   Z-ADD     *ZERO         FILL2             3 0
     C     ELKY2         CHAIN     XEP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       1             FILL2
     C     ELKY2         READE     XEP01                                  60
     C                   ENDDO
     C*
     C* CALCULATE TOTAL # OF PAGES FOR BILL.
     C*
     C                   SUB       9             FILL2
     C     FILL2         IFLE      0
     C                   Z-ADD     1             FILL2
     C                   ELSE
     C*
     C     FILL2         DIV       42            FILLR             6 3
     C                   MOVE      FILLR         FILLRC            3 3
     C                   Z-ADD     FILLR         FILL2
     C                   ADD       1             FILL2
     C     FILLRC        IFNE      0
     C                   ADD       1             FILL2
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     FILL2         XXPAGC            3 0
     C*
     C* PRINT PAYROLLS FOR THE POLICY
     C*
     C                   MOVE      *ON           *INOA
     C                   Z-ADD     0             COUNT             2 0
     C     ELKY2         CHAIN     XEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EPEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XPEFDT            8 0
     C                   MOVEL(P)  EPDESC        XXDESC           15
     C                   MOVE      EPPST         XXST
     C                   Z-ADD     EPEAPR        XXEAPR
     C                   Z-ADD     EPEMPR        XXEMPR
     C                   IF        XXEMPR < 0
     C                   MOVE      '-'           XXEMSN
     C                   ELSE
     C                   MOVE      ' '           XXEMSN
     C                   ENDIF
     C     EPADLD        IFNE      *ZERO
     C                   Z-ADD     EPADLD        XXEMPR
     C                   ENDIF
     C                   Z-ADD     EPDISC        EPDIV
     C     *IN90         IFEQ      *ON
     C     PPKY1         SETGT     PPP01
     C     PPKY          READPE    PPP01                                  60
     C     *IN60         IFEQ      *OFF
     C     PPAMPR        ANDGT     *ZERO
     C     PPANMP        DIV       PPAMPR        XQPROR
     C                   ELSE
     C                   Z-ADD     XZPROR        XQPROR
     C                   ENDIF
     C     EPANTR        IFNE      *ZERO
     C                   Z-ADD     EPANTR        XQARAT            9 4
     C                   ELSE
     C     EPARAT        MULT(H)   XQPROR        XQARAT            9 4
     C                   ENDIF
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C     *INOA         IFEQ      *ON
     C     COUNT         ORGT      41
     C*
     C     COUNT         IFNE      0
     C                   ADD       1             XXPAG4
     C*
     C     TURN          IFEQ      'E'
     C     QQRMCD        ANDNE     *BLANKS
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAGE
     C                   EXCEPT    SUMF
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    SUMFT
     C                   ENDIF                                                  XXNEWF IFEQ
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   MOVE      *OFF          *IN51
     C*
     C                   EXSR      DOPAGE
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QFOOT71
     C                   OTHER
     C                   WRITE     QFOOT
     C                   ENDSL
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   ENDIF
     C*
     C     TURN          IFEQ      'E'
     C     QQRMCD        ANDNE     *BLANKS
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    SUMH
     C                   EXCEPT    SUMH2
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    SUMHT
     C                   EXCEPT    SUMH2T
     C                   ENDIF                                                  XXNEWF IFEQ
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   WRITE     QHEAD
     C     XXST1         IFNE      ' '
     C                   MOVE      XXST          SVST              2
     C                   MOVE      XXST1         XXST
     C                   ENDIF
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QHD171
     C                   WRITE     QLN171
     C                   WRITE     QHD271
     C                   WRITE     QHDS371
     C                   OTHER
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   Movel(p)  xqrsth        svrsth           20
     c                   Movel(p)  xxst          s2st              2
     C                   Clear                   xqrsth
     C                   Clear                   xxst
     C                   WRITE     QHEAD2
     C                   Movel(p)  svrsth        xqrsth
     c                   Movel(p)  s2st          xxst
     C                   WRITE     QHEAD3S
     C                   ENDSL
     C     XXST1         IFNE      ' '
     C                   MOVE      SVST          XXST              2
     C                   ENDIF
     C                   WRITE     QBOX1
     C                   MOVE      *OFF          XX1ST
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   MOVE      *OFF          *INOA
     C                   Z-ADD     0             COUNT
     C                   ENDIF
     C*
     C                   ADD       1             COUNT
     C*
     C     TURN          IFEQ      'E'
     C     QQRMCD        ANDNE     *BLANKS
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    SUMD
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    SUMDT
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   ELSE
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   WRITE     QDETS
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
       If isUseXml;
          Exsr ScriptPayrolls_Sum;
       Endif;
     C*** End   Add *** 09/26/23 ***************************************
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C     ELKY2         READE     XEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     COUNT         IFGT      21
     C     XXNEWF        ANDEQ     *BLANK
     C     COUNT         ORGT      9
     C     XXNEWF        ANDEQ     'Y'
     C                   ADD       1             XXPAG4
     C     TURN          IFEQ      'E'
     C     QQRMCD        ANDNE     *BLANKS
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAGE
     C                   EXCEPT    SUMF
     C                   EXCEPT    SUMH
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    SUMFT
     C                   EXCEPT    SUMHT
     C                   ENDIF                                                  XXNEWF IFEQ
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C                   MOVE      *OFF          *IN57
     C*
     C                   MOVE      *OFF          *IN51
     C*
     C                   EXSR      DOPAGE
     C*
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QFOOT71
     C                   OTHER
     C                   WRITE     QFOOT
     C                   ENDSL
     C                   WRITE     QHEAD
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QHD171
     C                   WRITE     QLN171
     C                   WRITE     QHD271
     C                   WRITE     QHDS371
     C                   OTHER
     C                   WRITE     QHEAD1
     C                   WRITE     QLINE1
     C                   Movel(p)  xqrsth        svrsth           20
     c                   Movel(p)  xxst          s2st              2
     C                   Clear                   xqrsth
     C                   Clear                   xxst
     C                   WRITE     QHEAD2
     C                   Movel(p)  svrsth        xqrsth
     c                   Movel(p)  s2st          xxst
     C                   WRITE     QHEAD3S
     C                   ENDSL
     C                   WRITE     QBOX1
     C*
     C                   MOVE      *ON           *IN57
     C*
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   Z-ADD     0             COUNT
     C                   ENDIF
     C*
     C                   Z-ADD     XTPRO         XXPRO
     C                   Z-ADD     XTMFEE        XXMFEE               5252
     C                   Z-ADD     XTINST        XXINST               6767
     C                   Z-ADD     XTSFEE        XXSFEE               5353
     C                   Z-ADD     XTFEEB        XXFEEB
     C                   Z-ADD     XT1STB        XX1STB               5555
     C                   Z-ADD     XT2NDB        XX2NDB               5656
     C                   Z-ADD     XT3RDB        XX3RDB               6464
     C                   Z-ADD     XT4THB        XX4THB               9191
     C                   Z-ADD     XT5THB        XX5THB               9898
     C     *LIKE         DEFINE    *IN60         SVIN60
     C                   MOVE      *IN60         SVIN60
     C                   Z-ADD     XT6THB        XX6THB               6060
     C                   MOVE      *IN60         PRT6TH            1
     C                   Z-ADD     XT7THB        XX7THB               6060
     C                   MOVE      *IN60         PRT7TH            1
     C                   MOVE      SVIN60        *IN60
     C                   Z-ADD     XTEXCN        XXEXCN                   35
     C     ELFND         IFEQ      2
     C     CLTID         ANDEQ     '114'
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  ELFND IFNE
     C     ELFND         IFEQ      101
     C     CLTID         ANDEQ     '114'
     C                   MOVE      *ON           *IN35
     C                   ENDIF                                                  ELFND IFNE
     C                   Z-ADD     XTTXDU        XXTXDU                   45
     C                   Z-ADD     XTANCA        XXANCA           11 2
     C                   Z-ADD     XTSTOP        XXSTOP
     C*
     C*
     C     CLTID         IFNE      '109'
     C     CLTID         ANDNE     '102'
     C                   MOVE      *BLANKS       XXGHDG
     C                   Z-ADD     0             XXGAMT
     C*
     C     XXFEEB        IFNE      *ZERO
     C     CLTID         OREQ      '046'
     C                   MOVEL     GHDG(1)       XXGHDG
     C                   Z-ADD     XXFEEB        XXGAMT
     C                   ENDIF                                                  XXFEEB IFNE
     C*
     C                   ENDIF                                                  CLTID = 045
     C*
     C     CLTID         IFEQ      '109'
     C                   MOVE      *BLANKS       XXGHDG
     C                   Z-ADD     0             XXGAMT
     C*
     C     XXSTOP        IFNE      *ZERO
     C                   MOVEL     GHDG(2)       XXGHDG
     C                   Z-ADD     XXSTOP        XXGAMT
     C                   ENDIF                                                  XXSTOP IFNE
     C*
     C                   ENDIF                                                  CLTID = 109
     C                   Z-ADD     XTDPRL        XXDPRL
     C                   Z-ADD     XTGPD         XXGPD
     C                   Z-ADD     XTDIVA        XXDIVA
     C                   Z-ADD     XTDPDU        XXDPDU               4142
     C     CLTID         IFEQ      '071'
     C                   Z-ADD     XTRCTL        XXRCTL
     C                   SUB       XXRCTL        XXDPDU
     C                   ENDIF
     C     CLTID         IFEQ      '046'                                        CLTID=46
      *
     C                   Z-ADD     XTPRPD        XXPRPD
     C                   Z-ADD     XTTAXP        XXTAXP
     C                   Z-ADD     XT1SPD        XX1SPD
     C                   Z-ADD     XT2NPD        XX2NPD
     C                   Z-ADD     XTMFPD        XXMFPD
     C                   Z-ADD     XTSFPD        XXSFPD
     C                   Z-ADD     XTGFEE        XXGFEE                   28
     C                   Z-ADD     XTGFPD        XXGFPD
      /free
       If CltId = '111';
          xxDvnd = xtDvnd;
       Endif;
      /end-free
      *
     C     XXPRO         SUB       XXPRPD        XXPBAL           11 2
     C     XXTXDU        SUB       XXTAXP        XXTBAL           11 2
     C     XXMFEE        SUB       XXMFPD        XXMBAL           11 2
     C     XX1STB        SUB       XX1SPD        XX1BAL           11 2
     C     XX2NDB        SUB       XX2NPD        XX2BAL           11 2
     C     XXEXCN        SUB       XXEXPD        XXEBAL           11 2
     C     XXSFEE        SUB       XXSFPD        XXSBAL           11 2
     C     XXGFEE        SUB       XXGFPD        XXGBAL           11 2
     C     XXDPDU        ADD       XXDPRL        XXTBIL           11 2
     C     XXGAMT        SUB       XXGPD         XXFBAL           11 2
      *
     C                   ENDIF                                                  CLTID=46
     C*** Begin Chg *** 09/26/23 ***************************************
     C*****TURN*         IFEQ      'E'
     C*****EXABFL        ANDEQ     'Y'
     c                   If        (turn = 'E') And
     c                             ((EXabFl = 'Y' And isUseSpoolOut) Or
     c                              (BDPRMNA = 'Y' And isUseXML))
     C*** End   Chg *** 09/26/23 ***************************************
      *
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C     XXFND         ANDEQ     5
     C     XXDPDU        ANDGT     0
     C                   EXSR      W7321C
     C                   OTHER
     C     XXUPAR        IFEQ      'Y'
     C     XXDPDU        ANDGT     0
     C                   EXSR      W7321C
     C                   ENDIF
     C                   ENDSL
     C*
     C                   ENDIF                                                  TURN IFEQ
     C*
     C                   MOVE      '0'           *IN43
     C     *IN42         IFEQ      '1'
     C     XXSELF        ANDEQ     'Y'
     C     SVCNCL        ANDNE     'Y'
     C                   MOVE      '1'           *IN43
     C                   END
     C*
     C     COUNT         IFNE      0
     C*
     C     TURN          IFEQ      'E'
     C     QQRMCD        ANDNE     *BLANKS
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
      *
     C     CLTID         IFEQ      '046'
     C                   EXCEPT    SUMA#
     C                   ELSE
     C                   EXCEPT    SUMA1
     C                   ENDIF
      *
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C     CLTID         IFEQ      '046'
     C                   EXCEPT    SUMAT#
     C                   ELSE
     C                   EXCEPT    SUMAT1
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     TURN          IFEQ      'E'
     C     QQRMCD        ANDNE     *BLANKS
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    SUMA
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    SUMAT
     C                   ENDIF
     C                   ADD       1             XXPAG4
     C*
     C     TURN          IFEQ      'E'
     C     QQRMCD        ANDNE     *BLANKS
     C     XXNEWF        ANDEQ     *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   ADD       1             XXPAGE
     C                   EXCEPT    SUMF
     C                   ENDIF
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXCEPT    SUMFT
     C                   ENDIF                                                  XXNEWF IFEQ
     C     XXNEWF        IFEQ      'Y'
     C*
     C                   MOVE      *OFF          *IN57
     C*
     C                   MOVE      *ON           *IN51
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   WRITE     QBOX2
     C                   WRITE     QLINE2
     C                   WRITE     QBOX3
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C                   MOVE      *BLANKS       QHDG
     C                   MOVE      *BLANKS       QPRC
     C                   Z-ADD     0             QTAM
     C                   MOVE      *BLANKS       QSGN
     C*
      *** Beg Chg ***  12/05/23  *******************************************
     C*****              MOVEL     CON(1)        QHDG(1)
     C*****              Z-ADD     XTMAPR        QTAM(1)
     C*
     C*****              MOVEL     CON(15)       QHDG(2)
     C*****              Z-ADD     XTESBJ        QTAM(2)
     C*
     C*****              MOVEL     CON(5)        QHDG(3)
     C*****              Z-ADD     XTSTPR        QTAM(3)
     C*
     C*****              MOVEL     CON(8)        QHDG(4)
     C*****              Z-ADD     XTENMP        QTAM(4)
     C*
     C*****              MOVEL     CON(10)       QHDG(5)
     C*****              Z-ADD     XTESTP        QTAM(5)
      /Free
       //***** If cltid = '025' or isContri = '1';
      /End-free
     C*****              MOVEL     CON(59)       QHDG(1)
     C*****              MOVEL     CON(64)       QHDG(2)
     C*****              MOVEL     CON(60)       QHDG(3)
     C*****              MOVEL     CON(61)       QHDG(4)
     C*****              MOVEL     CON(62)       QHDG(5)
     c*****              ENDIF
     C*
     C*****XTANPR        IFNE      XTESTP
     C*****              MOVEL     CON(12)       QHDG(6)
     C*****              Z-ADD     XTANPR        QTAM(6)
     C*****              ENDIF
     C*
     C*****XTTXDU        IFNE      *ZERO
     C*****              MOVEL     CON(24)       QHDG(7)
     C*****              Z-ADD     XTTXDU        QTAM(7)
     C*
     C*****              MOVEL     CON(25)       QHDG(8)
      /Free
       //***** If cltid = '025' or isContri = '1';
      /End-free
     C*****              MOVEL     CON(62)       QHDG(8)
     c*****              ENDIF
     C*****XTANPR        ADD       XTTXDU        QTAM(8)
     C*****              ENDIF                                                  XTTXDU IFNE
     C*
     C* Print summary premium line items
     C*
     C                   Exsr      PrintSum
      *** End Chg ***  12/05/23  *******************************************
     C*
     C                   Z-ADD     XXPRO         X1PRO            11 2
     C                   Z-ADD     XXINST        X1INST           11 2
     C                   Z-ADD     XXMFEE        X1MFEE            7 2
     C                   Z-ADD     XXEXCN        X1EXCN           11 2
     C                   Z-ADD     XXSFEE        X1SFEE           11 2
     C                   Z-ADD     ELTXDU        X2TXDU           11 2
     C                   Z-ADD     XXDPRL        X1DPRL            9 2
     C                   Z-ADD     XXDPDU        X1DPDU           11 2
     C                   Z-ADD     XXDPDU        Q1NETP           11 2
     C                   MOVE      XXSFHD        X2SFHD           20
     C                   MOVE      XXTHD         X2THD            20
     C                   MOVE      XX1STH        X11STH           20
     C                   MOVE      X196          X1196             9 6
     C                   Z-ADD     XX1STB        X11STB           11 2
     C                   MOVE      XX2NDH        X12NDH           20
     C                   MOVE      X296          X1296             9 6
     C                   Z-ADD     XX2NDB        X12NDB           11 2
     C                   MOVE      XX3RDH        X13RDH           20
     C                   MOVE      X396          X1396             9 6
     C                   Z-ADD     XX3RDB        X13RDB           11 2
     C                   MOVE      XX4THH        X14THH           20
     C                   MOVE      X496          X1496             9 6
     C                   Z-ADD     XX4THB        X14THB           11 2
     C                   MOVE      XX5THH        X15THH           20
     C                   MOVE      X596          X1596             9 6
     C                   Z-ADD     XX5THB        X15THB           11 2
     C                   MOVE      XX6THH        X16THH           20
     C                   MOVE      X696          X1696             9 6
     C                   Z-ADD     XX6THB        X16THB           11 2
     C                   MOVE      XX7THH        X17THH           20
     C                   MOVE      X796          X1796             9 6
     C                   Z-ADD     XX7THB        X17THB           11 2
     C*
     C* COPY ALL BALANCE FIELDS TO GENERIC VARIABLES.
     C*
     C                   EXSR      DOBAL
     C*
     C                   EXSR      DOPAGE
     C*
       //*** Begin Chg *** 09/12/24 ************************************
       If Q1netP > 0 And
          AuditCreditExists(ELco#:ELfnd:ELfyr:ELemp#:ELdiv:sysDt);
          x1DueD = 'Past Due';
       EndIf;
       //*** End   Chg *** 09/12/24 ************************************
     C*** Begin Add *** 09/26/23 ***************************************
       //Print Premiums
       If isUseXml And xmlSumName <> *Blank;
          ScriptHeader(xmlSumName);
          ScriptPremium(xmlSumName);
          ScriptBalance(xmlSumName);
       Endif;
       //*** End   Add *** 09/26/23 ****************************************
     C     isUseSpoolOut IfEq      *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   SELECT
     C     CLTID         WHENEQ    '071'
     C                   WRITE     QPRM171
     C                   WRITE     QPREM2
     C                   WRITE     QFOOT71
     C                   OTHER
     C                   WRITE     QPREM1
     C                   WRITE     QPREM2
     C                   WRITE     QFOOT
     C                   ENDSL
       //*** Begin Add *** 09/26/23 ****************************************
       EndIf;
       //*** End   Add *** 09/26/23 ****************************************
     C*
     C                   MOVE      *ON           *IN57
     C*
     C                   ENDIF                                                  XXNEWF IFEQ
     C*
     C* CLOSE QSYSPRT FOR RESOLVING BILL TO DARTS
     C*
     C     XXNEWF        IFEQ      'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   CLOSE     W7615O
     C*
     C* Combine division bills together if report #6 has been set up.
     C*
     C                   If        divCombine = *on and turn = 'E'
     C*
     C                   Move      *on           divPrints
     C*
     C                   Movel(p)  'B'           x3Splf
     C                   If        CLTID = '133'
     C                   Movel(p)  'P'           x3Splf
     C                   Endif
     C                   Move      xxEmp#        x3Splf
     C                   Move      *blanks       x3Hard
     C                   Move      *blanks       x3Save
     C                   Move      *blanks       x3Hold
     C                   Move      *blanks       x3Cpys
     C                   Move      *blanks       x3Dart
     C                   Move      *blanks       x3Fold
     C                   Move      *blanks       x3Dcid
     C
     C*** Begin Add *** 09/26/23 ***************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 ***************************************
     C                   Call      'W7615C3'
     C                   Parm                    x3Splf
     C                   Parm                    x3Hard
     C                   Parm                    x3Save
     C                   Parm                    x3Hold
     C                   Parm                    x3Cpys
     C                   Parm                    x3Dart
     C                   Parm                    x3Fold
     C                   Parm                    x3Dcid
     C*
       //* Delete W7615O spool file
       If cltid = '133' and QQRMCD = *blanks;
          Monitor;
             cmdString = 'DLTSPLF FILE(W7615O) SPLNBR(*LAST)';
             length = 255;
             QCMDEXC(cmdString:length);
          On-Error;
          EndMon;
       Endif;
      /End-Free
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   Endif                                                  If divCombine = *on
     C                   ENDIF                                                  XXNEWF IFEQ
     C     TURN          IFEQ      'E'
     C     QQRMCD        ANDNE     *BLANKS
     C*
     C     XXNEWF        IFEQ      *BLANK
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut AndEq     *On
     C*** End   Add *** 09/26/23 ***************************************
     C                   CLOSE     QSYSPRT
     C                   ENDIF
     C*
     C                   MOVE      XXFND         QPFND
     C                   MOVE      XXEMP#        QPEMP#
     C                   MOVE      XXFYR         QPFYR
     C                   MOVE      XXDIV         QPDIV             5
     C                   MOVEL     F2FOLD        QQSFLD
     C                   MOVE      *BLANKS       QQFOLD           80
     C                   MOVE      *BLANKS       QQDCID
     C                   MOVE      'BL'          QQPRE             2
     C*
     C* IF RESOLVING NEW FORMAT BILL THEN CHANGE DOC NAME TO .PDF
     C*
     C     XXNEWF        IFEQ      'Y'
     C                   MOVE      'P'           QQRTCD
     C                   ELSE
     C                   MOVE      *BLANK        QQRTCD
     C                   ENDIF                                                  XXNEWF IFEQ
     C                   CALL      'SNXTDCID'
     C                   PARM                    QQFOLD
     C                   PARM                    QQDCID
     C                   PARM                    QQPRE
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQRTCD            1
     C                   PARM      '00000'       QQSUB#            5
     C                   PARM      *BLANKS       QQPFLR           80
     C*
     C     QQPFLR        IFNE      *BLANKS
     C*
     C     QQPFLR        CAT       '/':0         QQPFLR
     C*
     C     F2FOLD        IFEQ      *BLANKS
     C     QQPFLR        CAT(P)    'WCPOLS':0    QQSFLD
     C                   ELSE
     C     QQPFLR        CAT(P)    F2FOLD:0      QQSFLD
     C                   ENDIF                                                  F2FOLD IFEQ
     C*
     C                   ENDIF                                                  QQPFLR IFNE
     C*
     C* IF RESOLVING NEW FORMAT BILL THEN CHANGE DOC NAME TO .PDF
     C*
     C*
     C*
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 ***************************************
     C                   CALL      'W7615C'
     C                   PARM                    QQSPL#            4
     C                   PARM                    QQDCID           12
     C                   PARM                    QQPOL#
     C                   PARM                    QPDIV
     C                   PARM                    QQSFLD           40
     C                   PARM                    QQFOLD
     C                   PARM                    X1HOLD
     C                   PARM      'Y'           QQHARD            1
     C*
      /Free
       If cltid = '133';
       //* Delete W7615O spool file
          Monitor;
             cmdString = 'DLTSPLF FILE(W7615O) SPLNBR(*LAST)';
             length = 255;
             QCMDEXC(cmdString:length);
          On-Error;
          EndMon;
       Endif;
      /End-Free
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   MOVE      'Y'           XXSUMD            1
     C*** Begin Add *** 09/26/23 ***************************************
     C*** Begin Del *** 08/28/25 ***************************************
     C***  isUseSpoolOut IfEq      *On
     C*** End   Del *** 08/28/25 ***************************************
     C*** End   Add *** 09/26/23 ***************************************
     C                   EXSR      DARTS
     C*** Begin Add *** 09/26/23 ***************************************
     C*** Begin Del *** 08/28/25 ***************************************
     C***                EndIf
     C*** End   Del *** 08/28/25 ***************************************
     C*** End   Add *** 09/26/23 ***************************************
     C                   MOVE      *BLANK        XXSUMD            1
     C*
     C                   ENDIF                                                  TURN
     C                   If        CLTID = '133'
     C                   Select
     C                   When      Turn = 'E'  and divCombine = *Off
      ***
     C                   Movel(p)  'P'           x3Splf
     C                   Move      xxEmp#        x3Splf
     C                   Move      *blanks       x3Hard            1
     C                   Move      *blanks       x3Save            4
     C                   Move      *blanks       x3Hold            4
     C                   Move      *blanks       x3Cpys            2
     C                   Move      'X'           x3Dart            1
     C                   Move      *blanks       x3Fold           80
     C                   Move      *blanks       x3Dcid           12
     C
     C*** Begin Add *** 09/26/23 ***************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 ***************************************
     C                   Call      'W7615C3'
     C                   Parm                    x3Splf
     C                   Parm                    x3Hard
     C                   Parm                    x3Save
     C                   Parm                    x3Hold
     C                   Parm                    x3Cpys
     C                   Parm                    x3Dart
     C                   Parm                    x3Fold
     C                   Parm                    x3Dcid
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
      ***
     C                   When      Turn = 'A'
     C                   Move      *Zeros        x3Splf
     C                   Movel     'A'           x3Splf
     C                   Move      XXagt#        x3Splf
     C                   Move      *blanks       x3Hard            1
     C                   Move      *blanks       x3Save            4
     C                   Move      *blanks       x3Hold            4
     C                   Move      *blanks       x3Cpys            2
      *
     C                   If        divCombine = *Off
     C                   Move      'X'           x3Dart
     C                   Else
     C                   Move      *blanks       x3Dart            1
     C                   Endif
      *
     C                   Move      *blanks       x3Fold           80
     C                   Move      *blanks       x3Dcid           12
     C
     C*** Begin Add *** 09/26/23 ***************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 ***************************************
     C                   Call      'W7615C3'
     C                   Parm                    x3Splf
     C                   Parm                    x3Hard
     C                   Parm                    x3Save
     C                   Parm                    x3Hold
     C                   Parm                    x3Cpys
     C                   Parm                    x3Dart
     C                   Parm                    x3Fold
     C                   Parm                    x3Dcid
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C                   Endsl
     C                   Endif
     C***
      /Free
        //* Delete W7615O spool file
        If CLTID = '133';
            Monitor;
            cmdString = 'DLTSPLF FILE(W7615O) SPLNBR(*LAST)';
            length = 255;
            QCMDEXC(cmdString:length);
            On-Error;
            EndMon;
        Endif;
      /End-Free
      /Free

       //* If new running new format bill, release the spool file if needed.

                         If X1Hold <> '*YES' and XXNewF = 'Y';

                            Monitor;
                               cmdString = 'RLSSPLF FILE(W7615O) SPLNBR(*LAST)';
                               length = 255;
                               QCMDEXC(cmdString:length);
                            On-Error;
                            EndMon;

                         Endif;
                         //*** Begin Add *** 05/07/24 **********************
                         isSummary = *Off;
                         //*** End   Add *** 05/07/24 **********************
      /End-free
     C*
     C                   ENDSR                                                  SRLEAP
       //*** Beg Add ***  12/05/23  **************************************
       //=============================================================
       // PrintSum - Load the arrays for summary premium section
       //=============================================================
       Begsr PrintSum;

       i = 1;

       //* Manual Premium
       qtam(i) = xtmapr;
       If cltid = '025' or isContri = '1';
          qhdg(i) = con(59);
       Else;
          qhdg(i) = con(1);
       Endif;
       i += 1;

       //* Subject Premium
       qtam(i) = xtesbj;
       If cltid = '025' or isContri = '1';
          qhdg(i) = con(64);
       Else;
          qhdg(i) = con(15);
       Endif;
       i += 1;

       //* Modified Premium
       qtam(i) = xtempm;
       If cltid = '025' or isContri = '1';
          qhdg(i) = 'Modified Contribution';
       Else;
          qhdg(i) = 'Modified Premium';
       Endif;
       i += 1;

       //* Balance to Minimum
       If xtmbal <> 0;
          qtam(i) = xtmbal;
          qhdg(i) = 'Balance to Minimum';
          i += 1;
       Endif;

       //* Standard Premium
       qtam(i) = xtstpr;
       If cltid = '025' or isContri = '1';
          qhdg(i) = con(60);
       Else;
          qhdg(i) = con(5);
       Endif;
       i += 1;

       //* Normal Premium
       qtam(i) = xtenmp;
       If cltid = '025' or isContri = '1';
          qhdg(i) = con(61);
       Else;
          qhdg(i) = con(8);
       Endif;
       i += 1;

       //* Expense Constant
       qtam(i) = xtexco;
       qhdg(i) = con(9);
       i += 1;

       //*** Beg Add *****  05/12/25  *******************************************
       // Move Terroism Charge to print just after Expense Constant line
       // Earlier it was printing just after Prorated Premium line

       //* Terrorism Charges
       If xteter <> 0 or xtedtr <> 0;
          qtam(i) = xteter + xtedtr;
          qhdg(i) = 'Terrorism Charges';
          i += 1;
       Endif;
       //*** End Add *****  05/12/25  *******************************************

       //* Estimated Premium / Annual Premium
       qtam(i) = xtestp;
       //*** Beg Add *****  05/12/25  *******************************************
       // Add Terroism Charges
       qtam(i) = qtam(i) + xteter + xtedtr;
       //*** End Add *****  05/12/25  *******************************************
       If cltid = '025' or isContri = '1';
          //*** Beg Chg *****  05/12/25  *******************************************
          // qhdg(i) = 'Annual Premium Contribution';
          qhdg(i) = 'Audited Contribution';
          //*** End Chg *****  05/12/25  *******************************************
       Else;
          //*** Beg Chg *****  05/12/25  *******************************************
          // qhdg(i) = 'Annual Premium';
          qhdg(i) = 'Audited Premium';
          //*** End Chg *****  05/12/25  *******************************************
       Endif;
       i += 1;

       //*** Beg Del *****  05/12/25  *******************************************
       // Move Terroism Charge to print just after Expense Constant line

       //* Terrorism Charges
       // If xteter <> 0 or xtedtr <> 0;
       //    qtam(i) = xteter + xtedtr;
       //    qhdg(i) = 'Terrorism Charges';
       //    i += 1;
       // Endif;
       //*** End Del *****  05/12/25  *******************************************

       //* State Tax
       If xttxdu <> 0;
          qtam(i) = xttxdu;
          qhdg(i) = con(24);
          i += 1;
       Endif;

       //* Total Premium
       //*** Beg Chg *****  05/12/25  *******************************************
       // If xteter <> 0 or xtedtr <> 0 or xttxdu <> 0;
       If xttxdu <> 0;
       //*** End Chg *****  05/12/25  *******************************************
          qtam(i) = xtanpr + xttxdu;
          If cltid = '025' or isContri = '1';
             //*** Beg Chg *****  05/12/25  *******************************************
             // qhdg(i) = con(62);
             qhdg(i) = con(65);
             //*** End Chg *****  05/12/25  *******************************************
          Else;
             qhdg(i) = con(25);
          Endif;
          i += 1;
       Endif;

       Endsr;
       //*** End Add *****  12/05/23  **************************************
      /free
       //=============================================================
       // Print Volunteered List
       //=============================================================
       Begsr PrtVolList;

       If CltId = '111';
         intCount = 1;
          If vol(1) <>  ' ' and vol#(1) > 0;
             fill = fill + 2;
             Exsr PrtHeader;

             //*** Begin Add *** 09/26/23 *******************************
             //*** If xxNewF = 'Y';
             If xxNewF = 'Y' And isUseSpoolOut = *On;
             //*** End   Add *** 09/26/23 *******************************
                Write QVOL1;
             Endif;
          Endif;

          Dow intCount <= 5;
             If vol(intCount) <> ' ' and vol#(intCount) > 0;
                xxVol    = vol(intCount);
                xxVolCnt = vol#(intCount);
                fill = fill + 1;
                Exsr PrtHeader;
                //*** End   Add *** 09/26/23 *******************************
                //*** If xxNewF = 'Y';
                If xxNewF = 'Y' And isUseSpoolOut = *On;
                //*** End   Add *** 09/26/23 *******************************
                   write qvol;
                Endif;
             Endif;
             intCount = intCount + 1;
          Enddo;
       Endif;

       Endsr;

       //=============================================================
       // Print Header
       //=============================================================
       Begsr PrtHeader;

       //*** Begin Add *** 09/26/23 *******************************
       //*** If xxNewF = 'Y' and fill > 42;
       If xxNewF = 'Y' And fill > 42 And isUseSpoolOut = *On;
       //*** End   Add *** 09/26/23 *******************************
          xxPag4  = xxPag4 + 1;
          *in51 = *off;
          Exsr DoPage;
          *in31 = *off;
          write QFOOT;
          write QHEAD;
          write QHEAD1;
          write QLINE1;
          write QHEAD2;
          write QHEAD3;
          write QBOX1;
          xx1st = *off;
          fill = 0;
       Endif;

       Endsr;
       //=============================================================
       // GetVolCnt - Get Volunteer List and count
       //=============================================================
       Begsr GetVolCnt;

       // Build Volunteer Array

       Select;
       When EPclas = 7370 and EPspec = 'Q';
          x = 1;
       When EPclas = 7370 and EPspec = 'V';
          x = 2;
       When EPclas = 7520 and EPspec = 'V';
          x = 3;
       When EPclas = 7704 and EPspec = 'V';
          x = 4;
       When EPclas = 7720 and EPspec = 'Q';
          x = 5;
       When EPclas = 7720 and EPspec = 'V';
          x = 6;
       When EPclas = 8742 and EPspec = 'V';
          x = 7;
       When EPclas = 8810 and EPspec = 'V';
          x = 8;
       When EPclas = 9040 and EPspec = 'V';
          x = 9;
       When EPclas = 9101 and EPspec = 'V';
          x = 10;
       When EPclas = 7720 and EPspec = 'N';
          x = 11;
       Other;
          x = 0;
       Endsl;

       // Add Volunteer count to array if there is room.
       If x <> 0 and EPacnt > 0;
          y = 1;
          intCount = %lookUp(x:volA:y);
          If intCount = 0;
             If v < 12;
                v = v + 1;
                volA(v) = x;
                vol(v) = volH(x);
                vol#(v) = EPacnt;
             Endif;
          Else;
             vol#(intCount) = vol#(intCount) + EPacnt;
          Endif;
       Endif;

       Endsr;
      /end-free
     C*****************************************************************
     C     DOPAGE        BEGSR
     C                   MOVE      *BLANKS       XXPAGH           15
     C                   MOVEL     'Page'        XXPAGH
     C*
     C                   SELECT
     C     XXPAG4        WHENGE    100
     C                   MOVE      XXPAG4        XXNUM3            3
     C                   CAT       XXNUM3:1      XXPAGH
     C     XXPAG4        WHENGE    10
     C                   MOVE      XXPAG4        XXNUM2            2
     C                   CAT       XXNUM2:1      XXPAGH
     C                   OTHER
     C                   MOVE      XXPAG4        XXNUM1            1
     C                   CAT       XXNUM1:1      XXPAGH
     C                   ENDSL
     C*
     C                   CAT       'of':1        XXPAGH
     C*
     C                   SELECT
     C     XXPAGC        WHENGE    100
     C                   MOVE      XXPAGC        XXNUM3            3
     C                   CAT       XXNUM3:1      XXPAGH
     C     XXPAGC        WHENGE    10
     C                   MOVE      XXPAGC        XXNUM2            2
     C                   CAT       XXNUM2:1      XXPAGH
     C                   OTHER
     C                   MOVE      XXPAGC        XXNUM1            1
     C                   CAT       XXNUM1:1      XXPAGH
     C                   ENDSL
     C*
     C                   ENDSR
     C*
     C     SRAGHD        BEGSR
     C*
     C     *IN60         IFEQ      *OFF
     C     EXAHDR        IFEQ      *BLANKS
     C                   MOVEL(P)  'Agent #:'    EXAHDR
     C                   ENDIF
     C*
     C                   ELSE
     C                   MOVEL(P)  'Agent #:'    EXAHDR
     C                   ENDIF
     C*
     C                   MOVEL(P)  EXAHDR        XXAHDR
     C                   ENDSR
     C*****************************************************************
     C*
     C* COPY ALL BALANCE FIELDS TO GENERIC VARIABLES.
     C*
     C*****************************************************************
     C     DOBAL         BEGSR
     C*
     C                   CLEAR                   AHD
     C                   MOVEA     *ZERO         APC
     C                   CLEAR                   APS
     C                   CLEAR                   AUL
     C                   MOVEA     *ZERO         AAM
     C                   Z-ADD     1             D
     C*
     C* AUDITED PREMIUM.
     C*
     C                   SELECT
      /Free
       When cltid = '071' or cltid = '025' or isContri = '1';
      /End-free
     C                   MOVEL     CON(69)       AHD(D)
     C                   OTHER
     C                   MOVEL     CON(49)       AHD(D)
     C                   ENDSL
     C*
     C     CLTID         IFEQ      '102'
     C                   MOVEL     CON(57)       AHD(D)
     C                   ENDIF                                                  CLTID IFEQ
     C   51              Z-ADD     X1PRO         AAM(D)
      /free
       // Add a * note beside the word Premium and add a note at the bottom of the
       // balance box to indicate:
       // * Minimum has been invoked and all premium except terrorism charges are
       // billed to state reporting division.
       IF XXMINF = 'Y';
          ahd(d) = 'Premium *';
       Endif;
      /end-free
     C                   ADD       1             D
     C*
     C*
     C* Dividend Amount.
     C*
     C     xxDivA        IFNE      *zero
     C                   MOVEL     'Dividend'    AHD(D)
     C                   Z-ADD     xxDivA        AAM(D)
     C                   ADD       1             D
     C                   ENDIF                                                  xxDivA IFNE
     C*
     C* MEMBERSHIP FEES.
     C*
     C     CLTID         IFNE      '102'
     C   52              MOVEL     CON(50)       AHD(D)
     C   51
     CAN 52              Z-ADD     X1MFEE        AAM(D)
     C   52              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* LATE FEES FOR CLIENT 102.
     C*
     C     CLTID         IFEQ      '102'
     C     *IN68         ANDEQ     *ON
     C                   MOVEL     CON(58)       AHD(D)
     C   51              Z-ADD     XXLFEE        AAM(D)
     C                   ADD       1             D
     C                   ENDIF                                                  CLTID IFEQ
     C*
     C* FEES PAID FOR CLIENT 102.
     C*
     C     CLTID         IFEQ      '102'
     C     XXLFPD        ANDNE     *ZERO
     C                   MOVEL     CON(77)       AHD(D)
     C   51              Z-ADD     XXLFPD        AAM(D)
     C                   ADD       1             D
     C                   ENDIF                                                  CLTID IFEQ
     C*
     C* SAFETY FEES.
     C*
     C     CLTID         IFNE      '102'
     C   53              MOVEL     X2SFHD        AHD(D)
     C   51
     CAN 53              Z-ADD     X1SFEE        AAM(D)
     C   53              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* INSTALLMENT FEES.
     C*
     C     CLTID         IFNE      '102'
     C   67              MOVEL     CON(51)       AHD(D)
     C   51
     CAN 67              Z-ADD     X1INST        AAM(D)
     C   67              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* 1ST ASSESSMENT.
     C*
     C     CLTID         IFNE      '102'
     C   55              MOVEL     X11STH        AHD(D)
     C   55              Z-ADD     X1196         APC(D)
     C   55              MOVE      '%'           APS(D)
     C   51
     CAN 55              Z-ADD     X11STB        AAM(D)
     C   55              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* 2ND ASSESSMENT.
     C*
     C     CLTID         IFNE      '102'
     C   56              MOVEL     X12NDH        AHD(D)
     C   56              Z-ADD     X1296         APC(D)
     C   56              MOVE      '%'           APS(D)
     C   51
     CAN 56              Z-ADD     X12NDB        AAM(D)
     C   56              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* 3RD ASSESSMENT.
     C*
     C     CLTID         IFNE      '102'
     C   64              MOVEL     X13RDH        AHD(D)
     C   64              Z-ADD     X1396         APC(D)
     C   64              MOVE      '%'           APS(D)
     C   51
     CAN 64              Z-ADD     X13RDB        AAM(D)
     C   64              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* 4TH ASSESSMENT.
     C*
     C     CLTID         IFNE      '102'
     C   91              MOVEL     X14THH        AHD(D)
     C   91              Z-ADD     X1496         APC(D)
     C   91              MOVE      '%'           APS(D)
     C   51
     CAN 91              Z-ADD     X14THB        AAM(D)
     C   91              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* 5TH ASSESSMENT.
     C*
     C     CLTID         IFNE      '102'
     C   98              MOVEL     X15THH        AHD(D)
     C   98              Z-ADD     X1596         APC(D)
     C   98              MOVE      '%'           APS(D)
     C   51
     CAN 98              Z-ADD     X15THB        AAM(D)
     C   98              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* 6TH ASSESSMENT.
     C*
     C     CLTID         IFNE      '102'
     C     PRT6TH        ANDEQ     *ON
     C                   MOVEL     X16THH        AHD(D)
     C                   Z-ADD     X1696         APC(D)
     C                   MOVE      '%'           APS(D)
     C   51              Z-ADD     X16THB        AAM(D)
     C                   ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* 7TH ASSESSMENT.
     C*
     C     CLTID         IFNE      '102'
     C     PRT7TH        ANDEQ     *ON
     C                   MOVEL     X17THH        AHD(D)
     C                   Z-ADD     X1796         APC(D)
     C                   MOVE      '%'           APS(D)
     C   51              Z-ADD     X17THB        AAM(D)
     C                   ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* EXPENSE CONSTANT.
     C*
     C     CLTID         IFNE      '102'
     C     CLTID         ANDNE     '071'
     C  N35              MOVEL     CON(9)        AHD(D)
     C   51
     CANN35              Z-ADD     X1EXCN        AAM(D)
     C  N35              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* STATE TAX.
     C*
     C     CLTID         IFNE      '102'
     C  N45              MOVEL     X2THD         AHD(D)
     C   51
     CANN45              Z-ADD     X2TXDU        AAM(D)
     C  N45              ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* STOP GAP FEES BILLED FOR CLIENT 109.
     C*
     C*
     C* FEES BILLED FOR CLIENT 133.
     C*
     C*
     C* FEES BILLED FOR CLIENT 45.
     C*
     C     XXGAMT        IFNE      0
     C                   MOVEL     XXGHDG        AHD(D)
     C   51              Z-ADD     XXGAMT        AAM(D)
     C                   ADD       1             D
     C                   ENDIF                                                  XXGAMT IFNE
     C*
     C* RETRO CREDIT
     C*
     C     CLTID         IFEQ      '071'
     C     XXRCTL        ANDNE     *ZERO
     C                   MOVEL     'Retro'       AHD(D)
     C     AHD(D)        CAT       'Credit':1    AHD(D)
     C                   Z-ADD     XXRCTL        AAM(D)
     C                   ADD       1             D
     C                   ENDIF                                                  CLTID IFNE
     C*
     C* PREVIOUSLY PAID/BILLED.
     C*
     C  N57
     CAN 92              MOVEL     CON(52)       AHD(D)
     C  N57
     CANN92              MOVEL     CON(53)       AHD(D)
     C   51
     CANN57              Z-ADD     X1DPRL        AAM(D)
      /free
       If cltid <> '111' or xxDvnd = 0;
      /end-free
     C  N57              MOVEL     CON(56)       AUL(D)
      /free
       Endif;
      /end-free
     C  N57CLTID         IFEQ      '102'
     C                   MOVEL     CON(78)       AHD(D)
     C                   ENDIF                                                  CLTID IFEQ
     C  N57CLTID         IFEQ      '102'
     C     isContri      andeq     '1'
     C                   MOVEL     CON(87)       AHD(D)
     C                   ENDIF                                                  CLTID IFEQ
     C  N57              ADD       1             D
      /free
       If cltid = '111' and xxDvnd > 0;
          AHD(D) = CON(88);
          AAM(D) = xxDvnd;
          If *in57 = *off;
             AUL(D) = CON(56);
          Endif;
          D = D + 1;
       Endif;
      /end-free
     C*
     C* BALANCE DUE/CREDIT
     C*
     C  N57
     CAN 41              MOVEL     CON(54)       AHD(D)
     C  N57
     CAN 42              MOVEL     CON(55)       AHD(D)
     C   51
     CANN57              Z-ADD     X1DPDU        AAM(D)
     C  N57              ADD       1             D
     C*
     C                   ENDSR                                                  DOBAL SUBR
     C*****************************************************************
      *================================================================
      *   SRP71# - CONCAT GRP-EMP-DIV WITHOUT LEADING ZEROS
      *================================================================
     C     SRP71#        BEGSR
      *
     C                   MOVE(P)   QPFND         ZXFND             3
     C                   MOVE(P)   QPEMP#        ZXEMP             9
     C                   MOVE(P)   QPFYR         ZXFYR             3
     C     '0'           CHECK     ZXFND         X
     C                   SUBST     ZXFND:X       XXTMP1            3
     C     '0'           CHECK     ZXEMP         X
     C                   SUBST     ZXEMP:X       XXTMP2            9
     C     '0'           CHECK     ZXFYR         X
     C                   SUBST     ZXFYR:X       XXTMP3            3
     C     XXTMP1        CAT       '-':0         XQP71#           15
     C     XQP71#        CAT       XXTMP2:0      XQP71#
     C     XQP71#        CAT       '-':0         XQP71#
     C     XQP71#        CAT       XXTMP3:0      XQP71#
     C                   ENDSR
     C*====================================================================
     C* W7321C - CALL W7321C TO WRITE PREMIUM NOTICE
     C*====================================================================
     C     W7321C        BEGSR
      *
       //*** Begin Add *** 03/12/24 ****************************************
       // Don't call premium notices if no payrolls
       If Not HasDivPayrolls(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr);
          LeaveSR;
       EndIf;
       //*** End   Add *** 03/12/24 ****************************************
     C                   MOVE      XXCO#         ##CO#
     C                   MOVE      XXFND         ##FND
     C                   MOVE      XXFYR         ##PYR
     C                   MOVE      XXEMP#        ##EMP#
     C                   MOVE      XXDIV         ##DIV
     C                   Z-ADD     XXYMD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         ##DATE
     C                   MOVEL     SRDTO         ##DAT2
     C     QQRMCD        IFNE      *BLANKS
     C                   MOVE      'Y'           ##DRES
     C                   ENDIF
     C                   SELECT
     C                   OTHER
     C                   MOVE      'P'           ##PFB             1
     C                   ENDSL
     C                   SELECT
      *
     C                   WHEN      CLTID = '071'
     C                   If        QQREV = 'Y'
     C                   MOVE      'R'           ##MODE
     C                   Endif
     C                   If        QQREV = *BLANKS
     C                   MOVE      'A'           ##MODE
     C                   Endif

     C                   WHEN      CLTID = '093'
     C                   If        QQREV = 'Y'
     C                   MOVE      'R'           ##MODE
     C                   Endif
     C                   If        QQREV = *BLANKS
     C                   MOVE      'A'           ##MODE
     C                   Endif
      *
     C                   OTHER
     C                   MOVE      'D'           ##MODE
     C                   ENDSL
     C**   CLTID         IFEQ      '071'
     C**                 SELECT
     C**   QQREV         WHENEQ    'Y'
     C**                 MOVE      'R'           ##MODE
     C**   QQREV         WHENEQ    *BLANK
     C**                 MOVE      'A'           ##MODE
     C**                 ENDSL
     C**                 ELSE
     C**                 MOVE      'D'           ##MODE
     C**                 ENDIF
      *
     C                   CALL      'W7321C'
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##DATE            8
     C                   PARM                    ##DAT2            8
     C                   PARM      *ZEROS        ##ADDU            3
     C                   PARM      'Y'           ##EXDF            1
     C                   PARM      *BLANKS       ##COM1            2
     C                   PARM      *BLANKS       ##COM2            2
     C                   PARM      *BLANKS       ##COM3            2
     C                   PARM                    ##PYR             3
     C                   PARM                    ##EMP#            9
     C                   PARM                    ##DIV             5
     C                   PARM      *ZEROS        ##FRDT            8
     C                   PARM      *ZEROS        ##TODT            8
     C                   PARM      *BLANKS       ##SRT1            2
     C                   PARM      *BLANKS       ##SRT2            2
     C                   PARM      *BLANKS       ##FREA            2
     C                   PARM      *BLANKS       ##TOEA            2
     C                   PARM      *BLANKS       ##FRAA            2
     C                   PARM      *BLANKS       ##TOAA            2
     C                   PARM      *ZEROS        ##FREN            9
     C                   PARM      *ZEROS        ##TOEN            9
     C                   PARM      *ZEROS        ##FRAN            5
     C                   PARM      *ZEROS        ##TOAN            5
     C                   PARM      UPOUTQ        ##QUE            10
     C                   PARM                    ##MODE            1
     C                   PARM      QQRMCD        ##DRMK            5
     C                   PARM      *ZEROS        ##IDYS            3
     C                   PARM      'Y'           ##GRMA            1
     C                   PARM      '0000000'     ##MINB            7
     C                   PARM                    ##DRES            1
     C                   PARM      'Y'           ##DEP             1
     C                   PARM      'Y'           ##EXP             1
     C                   PARM                    ##PFB             1
     C                   PARM                    ##mass
     C                   Parm      EXSPYB        ##supPrYr         1
     C                   Parm      ' '           ##all             1
     C                   Parm      '0000000000'  ##Inv            10

      //*** Beg Add ***** 04/24/24 ***************************************
       If xxEmail = 'Y' and keyEmail <> *blank;
          strEmailD = keyEmail;
          Setll strEmailD xp105;
          Reade strEmailD xp105;
          Dow not %Eof(xmlprel5);
             //*** Begin Chg *** 05/07/24 **********************************
             //**If strXPFileComb <> *Blank and strXPDirComb <> *Blank;
             //***   XPEmailD = strXPDirComb;
             //***   XPEmailF = strXPFileComb;
             //*** Else;
             //***   XPEmailD = strXPDir;
             //***   XPEmailF = strXPFile;
             //***Endif;
             Select;
             When strXPFileComb <> *Blank and strXPDirComb <> *Blank;
                XPEmailD = strXPDirComb;
                XPEmailF = strXPFileComb;
             When strXPFileSRD <> *Blank and strXPDirSRD <> *Blank And
                  isSummary;
                XPEmailD = strXPDirSRD;
                XPEmailF = strXPFileSRD;
             Other;
                XPEmailD = strXPDir;
                XPEmailF = strXPFile;
             EndSl;
             //*** End   Chg *** 05/07/24 **********************************
             Update xp105;
             Reade strEmailD xp105;
          Enddo;
       Endif;
      //*** End Add ***** 04/24/24 ***************************************
      *
     C                   ENDSR                                                  W7321C
     C     CALDBR        BEGSR
     C*  SCHEDULE DB/CR
     C*
     C     ELDBCR        IFNE      *ZERO
     C*
     C     ELDBCR        IFGT      1
     C                   MOVE      '+'           SGN(A)
     C                   ELSE
     C     ELDBCR        IFLT      1
     C                   MOVE      '-'           SGN(A)
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     ELADC         TAM(A)
     C                   IF        XXMINF = 'Y'
     C                   Z-ADD     MPADC         TAM(A)
     C                   ENDIF
     C*
     C     ELSDEF        IFNE      *ZERO
     C                   Z-ADD     ELSDEF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C*
     C     SRDTI         IFNE      ELPFDT
     C     ELSDEX        ORNE      0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EFD(A)
     C     ELSDEX        IFNE      *ZERO
     C                   Z-ADD     ELSDEX        SRDTI
     C                   ELSE
     C                   Z-ADD     0             SRDTI
     C                   ENDIF
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXD(A)
     C     EFD(A)        IFNE      *ZERO
     C     EXD(A)        ANDNE     *ZERO
     C                   MOVE      '-'           DSH(A)
     C                   ENDIF
     C*
     C                   ENDIF                                                  SRDTI IFNE
     C*
     C                   MOVE      '.'           PCT(3)
     C* MOVE DEFAULT HEADING AT FIRST
     C                   MOVEL     CON(20)       HDG(A)
     C* CHECK DEBIT HEADING
     C     ELDBCR        IFGT      1
     C     RRDBHD        ANDNE     *BLANKS
     C                   MOVEL(P)  RRDBHD        HDG(A)
     C                   ENDIF
     C* CHECK CREDIT HEADING
     C     ELDBCR        IFLT      1
     C     RRCRHD        ANDNE     *BLANKS
     C                   MOVEL(P)  RRCRHD        HDG(A)
     C                   ENDIF
     C*
     C     CLTID         IFEQ      '136'
     C                   MOVEL     CON(70)       HDG(A)
     C                   ENDIF
     C                   Eval      dbcrHdg = hdg(a)
     C*
     C                   MOVE      '00000'       XXASP3
     C                   SELECT
     C     CLTID         WHENEQ    '136'
     C     ELDBCR        MULT(H)   ELCMUL        XXWK54            5 4
     C                   MOVE      XXWK54        XXASP3            5
     C                   OTHER
     C                   MOVE      ELDBCR        XXASP3            5
     C                   ENDSL
     C                   MOVEA     XXASP3        NUM
     C                   MOVE      NUM(1)        PCT(2)
     C                   MOVE      NUM(2)        PCT(4)
     C                   MOVE      NUM(3)        PCT(5)
     C                   MOVE      NUM(4)        PCT(6)
     C                   MOVE      NUM(5)        PCT(7)
     C                   MOVEA     PCT           PRC(A)
     C                   CLEAR                   PCT
     C                   ADD       1             A
     C                   ENDIF                                                  ELDBCR IFNE
     C*
     C                   ENDSR                                                  CALDBR
      *================================================================
      * PRINT LARGE DEDUCTIBLE ERROR REPORT
      *================================================================
     C     PRTLDE        BEGSR
     C*** Begin Add *** 09/26/23 ***************************************
     C     isUseSpoolOut IfEq      *On
     C                   LeaveSr
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C     FRSTE         IFEQ      *BLANK
     C                   OPEN      QSYSPRT3
     C                   MOVE      *ON           FRSTE             1
     C                   Move      Q1Pgm         xxPgm
     C                   MOVE      '3'           XXRPT#
     C     ORKY          CHAIN     POR01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     ORTITL        XXTITL           60
     C                   ELSE
     C                   MOVEL     ERTITL        XXTITL
     C                   ENDIF                                                  *IN60 IFEQ
     C                   SELECT
     C     UPOUTQ        WHENNE    *BLANK
     C                   MOVEL(P)  UPOUTQ        XXOUTQ           10
     C     OROUTQ        WHENNE    *BLANK
     C                   MOVEL(P)  OROUTQ        XXOUTQ           10
     C                   OTHER
     C                   MOVEL(P)  'QPRINT'      XXOUTQ           10
     C                   ENDSL
     C                   MOVEL(P)  XXTITL        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        XXTITL
     C                   MOVEL(P)  OFACNM        X1CONM           60
     C                   MOVEL(P)  X1CONM        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      60            ##LENG            3 0
     C                   MOVEL(P)  ##DATA        X1CONM
     C                   MOVE      *ON           *INOB
     C                   ENDIF
     C     *INOB         IFEQ      *ON
     C                   MOVE      *OFF          *INOB
     C                   EXCEPT    EHEAD
     C                   ENDIF
     C                   EXCEPT    EDET
     C                   ENDSR                                                  PRTLDE
      *================================================================
     C*********************************************************************
     c*  srFlatRateAdj - IL Flat rate Adjustment.
     C*********************************************************************
     c     srFlatRateAdj begsr
     c*
     c                   eval      xxFlrtAdj = elampr - xtpred
     c                   eval      xtpred = 0

     c                   if        xxFlrtAdj <> 0
     c                   clear                   pep01
     c                   eval      epco#  = xxco#
     c                   eval      epfnd  = xxfnd
     c                   eval      epfyr  = xxfyr
     c                   eval      epemp# = xxemp#
     c                   eval      epclas = xxstcd
     c                   eval      epcode = xxstcd
     c                   eval      epefdt = xxSpltDt
     c                   eval      epampr = xxFlrtAdj
     c                   eval      epeapr = 0
     c                   eval      eparat = 0
     c                   eval      epdesc = xcILRate
     c                   eval      eppst  = xxst
     c*
     c                   endif                                                  xxFlrtAdj

     c                   endsr
     C*********************************************************************
     c*  FlatRateAdjNY - NY Flat rate Adjustment.
     C*********************************************************************
     c     FlatRateAdjNY begsr
     c*
     c                   eval      xxFlrtAdjNY = xtpredNY - elampr
     c                   eval      xtpredNY = 0

     c                   if        xxFlrtAdjNY <> 0
     c                   clear                   pep01
     c                   eval      epco#  = xxco#
     c                   eval      epfnd  = xxfnd
     c                   eval      epfyr  = xxfyr
     c                   eval      epemp# = xxemp#
     c                   eval      epclas = xxstcd
     c                   eval      epcode = xxstcd
     c                   if        elpfdt > 1170410
     c                   eval      epefdt = elpfdt
     c                   else
     c                   eval      epefdt = 1170410
     c                   endif
     c                   eval      epampr = xxFlrtAdjNY
     c                   eval      epeapr = 0
     c                   eval      eparat = 0
     c                   eval      epdesc = xcNYRate
     c                   eval      eppst  = xxst
     c*
     c                   endif                                                  xxFlrtAdjNY

     c                   endsr
      *================================================================
      * Resolve the combined bill to DARTS if setup.
      *================================================================
     C     RslCombo      BEGSR
     C*
     C                   Movel(p)  'B'           x3Splf
     C                   If        CLTID = '133'
     C                   Movel(p)  'P'           x3Splf
     C                   Endif
     C                   Move      xxEmp#        x3Splf
     C                   Move      x6Save        x3Save
     C                   Move      x6Hold        x3Hold
     C                   Move      x6Cpys        x3Cpys
     C                   Move      QQHard        x3Hard
     C                   If        QQRMCD <> *blanks
     C                   Move      'Y'           x3Dart
     C                   Else
     C                   Move      'N'           x3Dart
     C                   Endif
     C                   Move      *blanks       x3Fold
     C                   Move      *blanks       x3Dcid
     C*
     C* Request next available document name/folder for resolving.
     C*
     C                   If        x3Dart = 'Y'
     C*
     C                   Move      *BLANKS       QQFOLD
     C                   Move      *BLANKS       QQDCID
     C                   Move      'BL'          QQPRE
     C                   Move      'P'           QQRTCD
     C*
     C                   CALL      'SNXTDCID'
     C                   PARM                    QQFOLD
     C                   PARM                    QQDCID
     C                   PARM                    QQPRE
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQRTCD
     C                   PARM      '00000'       QQSUB#
     C                   PARM      *BLANKS       QQPFLR
     C*
     C                   Movel(p)  QQFold        x3Fold
     C                   Movel(p)  QQDcid        x3Dcid
     C*
     C                   Endif                                                  If x3Dart = 'Y'
     C*
     C* Create PDF document and release/save/delete spool file.
     C*
     C*** Begin Add *** 09/26/23 ***************************************
     C                   If        isUseSpoolOut
     C*** End   Add *** 09/26/23 ***************************************
     C                   Call      'W7615C3'
     C                   Parm                    x3Splf
     C                   Parm                    x3Hard
     C                   Parm                    x3Save
     C                   Parm                    x3Hold
     C                   Parm                    x3Cpys
     C                   Parm                    x3Dart
     C                   Parm                    x3Fold
     C                   Parm                    x3Dcid
     C*
     C* Create DARTS remark for combined bill.
     C*
     C                   If        x3Dart = 'Y'
     C                   Z-add     xsDIV         xxDIV
     C                   Move      'C'           xxSUMD
     C                   Exsr      DARTS
     C                   Move      *BLANK        xxSUMD
     C                   Endif                                                  If x3Dart = 'Y'
     C*** Begin Add *** 09/26/23 ***************************************
     C                   EndIf
     C*** End   Add *** 09/26/23 ***************************************
     C*
     C                   ENDSR                                                  RslCombo
      *================================================================
       //*** Begin Add *** 09/26/23 ****************************************
       //===============================================================
       // Write Scriptura Workfile - RetroSettlement Notice for Quote Bills
       //===============================================================
       // Look at QNOTE* record formats.  May need more fields in WXBLP?
       Begsr ScriptNotes;
       Endsr;

       //===============================================================
       // Write Scriptura Workfile - Bills Payroll (WXBLP1)
       //===============================================================
       Begsr ScriptPayrolls;

          // *IN90 = *On  - Net Rate flag (F1NETR) = 'Y'
          //Fields from QHEAD3
          Clear pbl0p1;
          B1CO# = EMCo#;
          B1FND = EMFnd;
          B1EMP# = EMEmp#;
          B1DIV  = EMDiv;
          B1FYR  = ELFyr;
          B1Type = xxBillType;
          intWXBLP1seq += 1;
          B1key  = intWXBLP1seq;
          B1CLEFDT = XPEFDT;

          B1Clas = EPCLAS;
          B1Spec = EPSpec;
          B1Ext = EPEXT;
          B1Desc = PRDESC;

          If *In90 = *Off;
             B1Rate = EPARAT;
          Else;
             B1NetRt = XQARAT;
          Endif;

          B1Pay  = XXEAPR;
          B1Prem = XXEMPR;
          B1MPRMSN = XXEMSN;

          //Additional Load
          If *In62 = *On;
             B1ADDLOAD = XXADLD;
             Select;
             When *In49 = *Off and *In32 = *Off;
                B1ADDTXT = 'Additional Load';
             When *In49 = *Off and *In32 = *On;
                B1ADDTXT = 'Territory Differential';
             When *In49 = *On;
                B1ADDTXT = 'Stop Gap Premium';
             Endsl;
          Endif;

          //Class code From/To dates
          Select;
          When *In97 = *On;
             B1CLFRDT = XXFRDT;
             B1CLTODT = XXTODT;
          When *In97 = *Off and *In88 = *Off;
             B1CLFRDT = XXFRAV;
             B1CLTODT = XXTOAV;
          When *In97 = *Off and *In88 = *On;
             B1CLFRDT = X1FRAV;
             B1CLTODT = X1TOAV;
          Endsl;

          If EPCADT <> 0;
             B1CLTODT = %Dec(%Char(%Date(EPcadt:*cymd):*usa0):8:0);
          Else;
             If B1CLTODT = %Dec(%Char(%Date(ELPTDT:*cymd):*usa0):8:0);
                Clear B1CLTODT;
             Endif;
          Endif;

          B1Pgm  = Q1PGM;
          B1User = Q1USER;
          B1Time = %Dec(%Char(%Time():*iso0):6:0);
          B1Dt   = %Dec(%Char(%Date():*cymd0):7:0);

          //Current Bill
          If xmlBillName <> *Blank;
             B1XML = xmlBillName;
             Write pbl0p1;
          Endif;

          //Combined Bill
          If xmlCombName <> *Blank;
             B1XML = xmlCombName;
             Write pbl0p1;
          Endif;

       Endsr;

       //===============================================================
       // Write Scriptura Workfile - Bills Payroll for Summarized Bill
       // (WXBLP1)
       //===============================================================
       Begsr ScriptPayrolls_Sum;

          // *IN90 = *On  - Net Rate flag (F1NETR) = 'Y'
          //Fields from QHEAD3
          Clear pbl0p1;
          B1CO# = EMCo#;
          B1FND = EMFnd;
          B1EMP# = EMEmp#;
          B1DIV  = EMDiv;
          B1FYR  = ELFyr;
          intWXBLP1seq += 1;
          B1key  = intWXBLP1seq;
          B1Type = xxBillType;

          B1PAYDIV = EPDIV;         //Payroll Division
          B1RATEST = XXst;          //Rating State
          B1Clas = EPCLAS;
          B1Spec = EPSpec;
          B1Ext = EPEXT;
          B1CLEFDT = XPEFDT;        //Effective date

          //*** Beg Add ***** 04/04/24 ****************************************
          //Expiration date
          If EPCADT <> 0;
             B1CLTODT = %Dec(%Char(%Date(EPcadt:*cymd):*usa0):8:0);
          Else;
             Clear B1CLTODT;
          Endif;
          //*** End Add ***** 04/04/24 ****************************************

          B1Desc = EPDESC;

          If *In90 = *Off;
             B1Rate = EPARAT;
          Else;
             B1Rate = XQARAT;
          Endif;

          //*** Beg Add ***** 04/04/24 ****************************************
          //Additional Load
          If EPADLD <> 0;
             B1ADDLOAD = EPADLD;
             ##CO# = %editc(ELCO#: 'X');
             ##FND = %editc(ELFND: 'X');
             ##FYR = %editc(ELFYR: 'X');
             ##EMP# = %editc(ELEMP#: 'X');
             ##DIV = %editc(ELDIV: 'X');
             ##DAT1 = %editc(EPEFDT: 'X');
             ##TYPE = 'P';
             ##Clas = %editc(EPCLAS: 'X');
             SrEffdt(##CO#:##FND:##FYR:##EMP#:##DIV:##DAT1:##TYPE:##CLAS);
             xxEFDT1 = %Int(##DAT1);
             Setgt (EPco#:EPClas:EPExt:RSJRST:XXEfDt1) ppx01;
             Readpe (EPco#:EPClas:EPExt:RSJRST) ppx01;
             B1ADDTXT = 'Additional Load';
             If not %Eof(WMPXL1) and PXcode <> *Zero and EPext <> *Blanks;
                B1ADDTXT = 'Territory Differential';
             Endif;
          Endif;
          //*** End Add ***** 04/04/24 ****************************************

          B1Pay  = EPEAPR;
          B1Prem = EPEMPR;
          B1MPRMSN = XXEMSN;
          B1Pgm  = Q1PGM;
          B1User = Q1USER;
          B1Time = %Dec(%Char(%Time():*iso0):6:0);
          B1Dt   = %Dec(%Char(%Date():*cymd0):7:0);

          //Summarized Bill
          If xmlSumName <> *Blank;
             B1XML = xmlSumName;
             Write pbl0p1;
          Endif;

          //Summarized Combined Bill
          If xmlSumCombName <> *Blank;
             B1XML = xmlSumCombName;
             Write pbl0p1;
          Endif;

       Endsr;

       //=========================================================================
       //  ScriptXML - Reserve/Update XML record for Scriptura output.
       //=========================================================================
       Begsr ScriptXML;

          //*** Beg Add ***** 03/19/24 *****************************************
          Chain Q1User pup01;
          //*** End Add ***** 03/19/24 *****************************************

          xmlFile = ' ';
          message = %trim(processType);
          string = ' ';

          If isMassReq;

             In *Lock *dtaara;
             Out *dtaara;

             If ldMassFile <> ' ';
                xmlFile = %trim(ldMassFile);
             Else;

                // Create mass entry
                StLetter('XMLREQUEST':xmlFile:message:' ':'000000000':'  ':
                processType:'000':'000':massFlag:xmlLink);

                // Update LDA with mass flow name
                In *Lock *dtaara;
                ldMassFile = %trim(xmlFile);
                Out *dtaara;

                // Update XMLPREP with Category
                Chain (xmlFile) xp100;
                XPCat = 'PS';
                Update xp100;
             Endif;

             isMassReq   = *off;
             massReqGen  = 'Y';
             xmlFileName = xmlFile;
             xmlLink     = xmlFile;

          Endif;

          // Create individual entry
          StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):xxPre:
           string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:xmlLink);

          If xmlFileName = *Blank;
             xmlFileName = xmlFile;
          Endif;

          //Update XMLPrep record that was created above
          If xmlLink = *Blank;
             Chain (xmlFile) xp100;
             xmlFile = XPxmlT;
          Else;
             Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
          Endif;

          If %Found(xmlprel1);

             // If we are doing the agent turn then don't create darts
             // and set item to be skipped for agent and insured copies
             If (turn = 'A') Or
                (turn = 'E' And xxCombine = 'Y' And xmlLink <> *Blank);
                XPskip = 'Y';
                If (turn = 'A');
                   XPrmCd = ' ';
                   XPdart = ' ';
                EndIf;
             EndIf;

             XPdata = 'Y';
             If BLpathc <> *Blank;
                XPdcid = BLdocc;
                XPPath = BLpathc;
             Else;
                XPdcid = BLdoc;
                XPPath = BLpath;
             Endif;

             If xxCombine = 'Y' Or turn = 'A';
                Clear XPMail;
                Clear XPMailAdd;
             Endif;
             //*** Beg Add ***** 03/19/24 **************************************
             XPIDesc = RtviDartsDesc(processType:'I');
             //*** End Add ***** 03/19/24 **************************************

             Update xp100;
          Endif;

          //*** Beg Add ***** 04/24/24 ********************************************
          //Set keyEmial field in LDA. This value will get picked up and written to
          // XMLPREP record of the individual premium notice to later set them as
          // an additional attachment to the individual bill email.
          If xxCombine <> 'Y';
             If BDPrmNA = 'Y';
                In *Lock *dtaara;
                keyEmail= %Trim(Q1User) + %subst(%char(%timestamp:*iso0):1:17);
                Out *dtaara;
             Endif;
          Endif;

          strXPFile = XPFile;
          strXPDir = XPDir;

          //*** Begin Add *** 05/07/24 *****************************   *****
          If EMstRp = 'Y';
             strXPFileSRD = XPFile;
             strXPDirSRD  = XPDir;
          EndIf;
          //*** End   Add *** 05/07/24 *****************************   *****
          //*** End Add ***** 04/24/24 ********************************************

          xmlBillName = xmlFile;

          //Summarized Bill
          //*** Begin Chg *** 05/07/24 *************************************
          //*If xxARSUM = 'Y' and xmlLink <> *Blank and xmlSumName = *Blank;
          //*** Beg Chg ***** 06/18/25 **************************************
          //***If xxARSUM = 'Y' and xmlLink <> *Blank and xmlSumName = *Blank And
          //***   xsRRN <> 0;
          //*** begin chg ***  07/29/25  **************************************
          //*****If XXSUM = 'Y' and xmlLink <> *Blank and xmlSumName = *Blank And
          //***** xsRRN <> 0;
          //* Only generate a/r summary bill if running for entire policy.
          //*** begin chg ***  08/01/25  **************************************
          If XXSUM = 'Y' and xmlLink <> *Blank and xmlSumName = *Blank And
           xsRRN <> 0 and (svemp# = 0 or includeSum = *on or allDiv = *on);
          //*** end chg *****  08/01/25  **************************************
          //*** end chg *****  07/29/25  **************************************
          //*** End Chg ***** 06/18/25 **************************************
          //*** End   Chg *** 05/07/24 *************************************

             //*** Begin Chg *** 05/07/24 **********************************
             //*StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):
             //** xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:
             //*** xmlLink);
             StLetter('XMLREQUEST':xmlFile:message:'E':%editc(xsRRN:'X'):
              xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:
              xmlLink);
             //*** End   Chg *** 05/07/24 **********************************

             Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
             If %Found(xmlprel1);

                // If we are doing the agent turn then don't create
                // DARTS and set item to be skipped for agent and
                // insured copies
                If (turn = 'A') Or
                   (turn = 'E' And xxCombine = 'Y' And xmlLink <> *Blank);
                   XPskip = 'Y';
                   If (turn = 'A');
                      XPrmCd = ' ';
                      XPdart = ' ';
                   EndIf;
                EndIf;

                XPdata = 'Y';
                If BLSumPC <> *BLank;
                   XPDcId = BLsumDC;
                   XPPath = BLSumPC;
                Else;
                   XPDcId = BLsumD;
                   XPPath = BLSumP;
                Endif;

                //*** Begin Chg *** 05/07/24 *******************************
                //***If xxCombine = 'Y' Or turn = 'A';
                //***   Clear XPMail;
                //***   Clear XPMailAdd;
                //***Endif;
                Clear XPMail;
                Clear XPMailAdd;
                //*** End   Chg *** 05/07/24 *******************************
                //*** Beg Add ***** 03/19/24 **************************************
                XPIDesc = RtviDartsDesc(processType:'S');
                //*** End Add ***** 03/19/24 **************************************
                Update xp100;
             Endif;

             xmlSumName = xmlFile;

          Endif;
          //*** Begin Add *** 05/07/24 ********************************
          // Setting email preferences on summary bills
          If xmlSumName <> ' ' And strXPfileSRD <> ' ' And
             xxCombine <> 'Y';
             Chain (xmlFileBlank:xmlLink:xmlSumName) xp100;
             If %Found(xmlprel1);
                XPEmailF = strXPFileSRD;
                XPEmailD = strXPDirSRD;
                Update xp100;
             EndIf;
          EndIf;
          //*** End   Add *** 05/07/24 ********************************

          // Combined Bill
          //*** Begin Chg *** 05/07/24 *************************************
          //*If xxCombine = 'Y' And xmlLink <> *Blank And turn = 'E'; // 04/23/24
          If xxCombine = 'Y' And xmlLink <> *Blank And turn = 'E' And
          //*** Beg Chg ***** 04/04/25 *************************************
          //***   xsRRN <> 0;
             xsRRN <> 0 and (allDiv = *On or svExDt = xsptdt);
          //*** End Chg ***** 04/04/25 *************************************
          //*** End   Chg *** 05/07/24 *************************************

             If xmlCombName = *Blank;
                //*** Begin Chg *** 05/07/24 *******************************
                //****StLetter('XMLREQUEST':xmlFile:message:'E':%editc(x#RRN:'X'):
                //** xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):mass  Flag:
                //* xmlLink);
                StLetter('XMLREQUEST':xmlFile:message:'E':%editc(xsRRN:'X'):
                 xxPre:string:%editc(xxCo#:'X'):%editc(xxFnd:'X'):massFlag:
                 xmlLink);
                //*** End   Chg *** 05/07/24 *******************************

                Chain (xmlFileBlank:xmlLink:xmlFile) xp100;
                If %Found(xmlprel1);
                   //*** Beg Add ***** 02/04/25 **************************************
                   comboBillXMLKey = xmlFile;
                   //*** End Add ***** 02/04/25 **************************************
                   XPdata = 'Y';
                   //Set Template file name and Path
                   If BLPathC <> *BLank;
                      XPDcId = BLDocC;
                      XPPath = BLPathC;
                   Else;
                      XPDcId = BLDoc;
                      XPPath = BLPath;
                   Endif;

                   //This will keep the combination bill from being included
                   //in the overall combined document for the entired
                   //process
                   XPNoComb = 'Y';
                   XPEmaild = *Blank;

                   If xxEmail = 'Y' and ldEndA <> *Blank and
                      ldEndA <> *Zeros;
                      Clear XPMail;
                      Clear XPMailAdd;
                      XPEmaild = keyEmail;
                   Endif;
                   //*** Beg Add ***** 03/19/24 **************************************
                   XPIDesc = RtviDartsDesc(processType:'C');
                   //*** End Add ***** 03/19/24 **************************************

                   Update xp100;
                Endif;
                //*** Beg Add ***** 04/24/24 *********************************************
                //Set keyEmial field in LDA. This value will get picked up and written to
                // XMLPREP record of the individual premium notice to later set them as
                // an additional attachment to the combination bill email.
                If xxEmail = 'Y' and BDPrmNA = 'Y' and BDMrgprn = *Blank;
                   In *Lock *dtaara;
                   keyEmail= %Trim(Q1User) +
                    %subst(%char(%timestamp:*iso0):1:17);
                   Out *dtaara;
                Endif;
                strXPFileComb = XPFile;
                strXPDirComb = XPDir;
                //*** End Add ***** 04/24/24 *********************************************

                // Update LDA with output file name
                In *Lock *dtaara;
                ldOutputFile = %trim(XPfile);
                Out *dtaara;

                xmlCombName    = xmlFile;
                outputFileComb = XPFile;

                //*** Beg Add ***** 02/04/25 **********************************************
                //If division being processed in not State Reporting Division,
                // save the xml key for SRD and request XMLTEMP for the div being processed
                If EMdiv = xsDiv;
                   xmlCombName = comboBillXMLKey;
                Else;
                   //Request XMLTEMP for division being processed
                   templateFile = outputFileComb;
                   StLetter('TEMPLRQST':templateFile:message:'E':
                    %editc(x#RRN:'X'): xxPre:string:%editc(xxCo#:'X'):
                    %editc(xxFnd:'X'));
                   Chain (templateFile) xt101;
                   If %Found(xmlteml1);
                      //Set Template file name and Path
                      If BLPathC <> *BLank;
                         XTDcId = BLDocC;
                         XTPath = BLPathC;
                      Else;
                         XTDcId = BLDoc;
                         XTPath = BLPath;
                      Endif;
                      XTData = 'Y';
                      XTCo#  = EMCo#;
                      XTFnd  = EMFnd;
                      XTEmp# = EMEmp#;
                      XTDiv  = EMDiv;
                      XTFyr  = ELFyr;
                      Update xt101;
                   Endif;
                   xmlCombName = templateFile;
                Endif;
                //*** End Add ***** 02/04/25 **********************************************

                If xxARSUM = 'Y';
                   templateFile  = outputFileComb;
                   StLetter('TEMPLRQST':templateFile:message:'E':
                    %editc(x#RRN:'X'):xxPre:string:%editc(xxCo#:'X'):
                    %editc(xxFnd:'X'));

                   Chain (templateFile) xt101;
                   If %Found(xmlteml1);

                      //Set Template file name and Path
                      If BLPathC <> *BLank;
                         XTDcId = BLSumDC;
                         XTPath = BLSumPC;
                      Else;
                         XTDcId = BLSumD;
                         XTPath = BLSumP;
                      Endif;
                      //*** Beg Add ***** 02/04/25 ***********************************
                      XTData = 'Y';
                      XTCo#  = EMCo#;
                      XTFnd  = EMFnd;
                      XTEmp# = EMEmp#;
                      XTDiv  = XSDiv;
                      XTFyr  = ELFyr;
                      //*** End Add ***** 02/04/25 ***********************************

                      Update xt101;
                   Endif;
                   xmlSumCombName = templateFile;
                Endif;
             Else;
             //*** Beg Add ***** 02/04/25 **************************************
             If EMdiv = xsDiv;
                xmlCombName = comboBillXMLKey;
                //Change Template order in the case where first processing Div was not SRD
                Setgt (outputFileComb) xt100;
                Readpe (outputFileComb) xt100;
                If %Found(xmlteml);
                   Chain (xmlFileBlank:xmlLink:comboBillXMLKey) xp100;
                   If %Found(xmlprel1);
                      XPOrder = XIorder + 1;
                      Update xp100;
                   Endif;
                Endif;
             Else;
             //*** End Add ***** 02/04/25 **************************************
                templateFile = outputFileComb;
                StLetter('TEMPLRQST':templateFile:message:'E':
                 %editc(x#RRN:'X'):xxPre:string:%editc(xxCo#:'X'):
                 %editc(xxFnd:'X'));

                Chain (templateFile) xt101;
                If %Found(xmlteml1);
                   //Set Template file name and Path
                   If BLPathC <> *BLank;
                      XTDcId = BLDocC;
                      XTPath = BLPathC;
                   Else;
                      XTDcId = BLDoc;
                      XTPath = BLPath;
                   Endif;
                    //*** Beg Add ***** 02/04/25 ***********************************
                   XTData = 'Y';
                   XTCo#  = EMCo#;
                   XTFnd  = EMFnd;
                   XTEmp# = EMEmp#;
                   XTDiv  = EMDiv;
                   XTFyr  = ELFyr;
                   //*** End Add ***** 02/04/25 ***********************************
                   Update xt101;
                Endif;
                xmlCombName = templateFile;
             //*** Beg Add ***** 02/04/25 **************************************
             Endif;
             //*** End Add ***** 02/04/25 **************************************
             Endif;                                //xmlCombName = *Blank
          Endif;                                   //xxCombine = 'Y'
       //*** begin add ***  10/25/25  **********************************
          If noPayrollBill = 'Y';
             Clear pbl0p1;
             B1CO# = EMCo#;
             B1FND = EMFnd;
             B1EMP# = EMEmp#;
             B1DIV  = EMDiv;
             B1FYR  = ELFyr;
             B1Type = xxBillType;

             B1Desc = '<No Payrolls Exist>';

             B1Pgm  = Q1PGM;
             B1User = Q1USER;
             B1Time = %Dec(%Char(%Time():*iso0):6:0);
             B1Dt   = %Dec(%Char(%Date():*cymd0):7:0);

             intWXBLP1seq += 1;
             B1Key = intWXBLP1seq;
             //Current Bill
             If xmlBillName <> *Blank;
                B1XML = xmlBillName;
                Write pbl0p1;
             Endif;

             //Combined Bill
             If xmlCombName <> *Blank;
                B1XML = xmlCombName;
                Write pbl0p1;
             Endif;

          Endif;
       //*** end add *****  10/25/25  **********************************

       Endsr;
      *** End   add *** 09/26/23 **************************************
      //*** Beg Add ***** 06/18/25 ****************************************
       //=========================================================================
       //  RmvCombBill - Remove Combined Bill records
       //=========================================================================
       Begsr RmvCombBill;
         If strXPFileSRD = *Blank and comboBillXMLKey <> *Blank;
            //Remove Combined Bill xmlprep
            Chain (xmlFileBlank:xmlLink:comboBillXMLKey) xp100;
            If %Found(xmlprel1);
            //*** begin chg ***  07/17/25  ***********************************
            //* Note - instead of removing the combination bill it will instead
            //* generate a blank page for the state reporting division in order
            //* to allow the combination bill to be successfully created.
               //Remove all xmltemp records for this Combined bill
            //*If XPAddT = 'Y';
            //****Setll (XPFile) xt100;
            //****Reade (XPFile) xt100;
            //****Dow not %Eof (xmlteml);
            //****   Chain (XIxmlt) xt101;
            //****   If %Found(xmlteml1);
            //****      Delete xt101;
            //****   Endif;
            //****   //Remove Scriptura header record wxblp
            //****   Chain (XIxmlt) pbl00;
            //****   If %Found(wxbll);
            //****      Delete pbl00;
            //****   Endif;
            //****   Reade (XPFile) xt100;
            //****Enddo;
            //****Delete xp100;
            //****//Remove Scriptura header record wxblp
            //****Chain (XPxmlt) pbl00;
            //****If %Found(wxbll);
            //****   Delete pbl00;
            //****Endif;
            //*Else;
            //****Unlock xmlprel1;
            //*Endif;
               XPType = ' ';
               XPDcid = 'Blank.scd';
               XPPath = '/Reports/Base/Policy/';
               Update xp100;
            //*** end chg *****  07/17/25  ***********************************
            Endif;
         Endif;
       Endsr;
      //*** End Add ***** 06/18/25 ****************************************
     C*
     O/COPY W7615OSPEC
     OQSYSPRT3  E            EROUTE         1 03
     O          E            EROUTE         1
     O                                           15 'ROUTE TO:'
     O                       UPUSNM              67
     O          E            EROUTE         1
     O                                           19 'REPORT TITLE:'
     O                       XXTITL              80
     O          E            EROUTE         1 59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            EROUTE         1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
     O          E            EHEAD          2  6
     O                       DSTIME              08 '  :  :  '
     O                       X1CONM              85
     O                       DSDATE        Y    132
     O          E            EHEAD          2
     O                       Q1PGM               10
     O                       XXTITL              90
     O                                          125 'PAGE:'
     O                       PAGE          Z    132
     O          E            EHEAD          2
     O                                           06 'Group:'
     O                       FNFND         Z     11
     O                       FNNAME              73
     O          E            EHEAD          2
     O                                           11 'Employer'
     O                                           18 'Div'
     O                                           25 'Year'
     O                                           43 'Employer Name'
     O          E            EDET           1
     O                       ELEMP#        Z     11
     O                       ELDIV         Z     18
     O                       ELFYR         Z     25
     O                       EMNAME              70
      *=============================================================
      * ApplyRateSplit - Check option flag to apply rate split
      *=============================================================
     p ApplyRateSplit  b
     d ApplyRateSplit  pi              n
     d   inState                      2a
     d   inDate                       7  0
     d   outstcd                      5  0 options(*nopass)
      *
     d xxState         s              2a
     d xxSpltDt        s              7  0
     d xxPrcRate       s               n
      *
     C     rtky          klist
     C                   kfld                    xxco#
     C                   kfld                    xxfnd
     C                   kfld                    xxstate
     C                   kfld                    xxSpltDt          7 0
     C*
     C                   eval      xxPrcRate = *on
     C                   eval      xxState = inState
     C                   eval      xxSpltDt = inDate
     C     rtky          chain     prt0x
     C                   if        %found and rxopt = 'Y'
     C                   eval      xxPrcRate = *off
     C                   endif
     c                   if        %parms >= 3 and %found
     c                   eval      outstcd = rxstat
     c                   endif
     C*
     C                   return    xxPrcRate
     C*
     p ApplyRateSplit  e
      *============================================
      * Returns *on if a split modifier condition
      * is detected
      *--------------------------------------------
     p isSplitMod      b
      *--------------------------------------------
     d isSplitMod      pi              n
     d  inFact1                            like(ELexm1)
     d  inFact2                            like(ELexm1)
     d  inPrem1                            like(ELexp1)
     d  inPrem2                            like(ELexp1)
     d  inDate1                            like(ELemd1)
     d  inDate2                            like(ELemd1)
      /free
       If (inPrem1 <> 0 or inFact1 = 1) and IsActive(inDate1) and
        (inPrem2 <> 0 or inFact2 = 1) and IsActive(inDate2)   and
        inFact1 <> inFact2;
          Return *on;
       Else;
          Return *off;
       Endif;
      /end-free
     p isSplitMod      e
      *============================================
      * Returns *on if inDate is within policy date
      * range
      *--------------------------------------------
     p isActive        b
      *--------------------------------------------
     d isActive        pi              n
     d  effDt                              like(ELemd1)
      /free
       If effDt < ELptdt and effDt <> 0;
          Return *On;
       Else;
          Return *Off;
       Endif;
      /end-free
     p isActive        e
      *============================================
      *** Begin Add *** 09/26/23 *******************************************
     d*=================================================================
     d* Get Billing Template
     d*=================================================================
     p GetBillingTemplate...
     p                 b
     d GetBillingTemplate...
     d                 pi            25
     d  inCo#                         3  0
     d  inType                       50    Const
     d  inFnd                         3  0
     d  inSt                          2

     d  hasTempFound   s               n   Inz(*Off)

       //Retrieve Bill Document setup file
       Chain inCo# sbl00;

       //Retrieve Bill Documents Template Setup File
       Setgt (inCo#:inType:inFnd:inSt:sysDt) pbl100;
       Readpe (inCo#:inType:inFnd:inSt) pbl100;
       If Not %Eof(WSBL1L);
          hasTempFound = *On;
       EndIf;

       Return hasTempFound;

     p GetBillingTemplate...
     p                 e
     d*=================================================================
     d* Reorganize Templates so the combined document prints in order
     d*=================================================================
     p ReorganizeTemplates...
     p                 b
     d ReorganizeTemplates...
     d                 pi
     d  inXPLink                     60

     d intLastOrder#   s              5  0
     d arrXMLFile      s             60    Dim(99999)

       //Re-arrange template order for Combined document: all the Bills will be printed
       // first, then Summary bill followed by Premium notices.
       Setll (xmlFileBlank:inXPLink) xp100;
       Reade (xmlFileBlank:inXPLink) xp100;
       Dow not %Eof(xmlprel1);
          If XPAddT = 'Y';
             //Find last template number (XTORDER) used for this combined bill
             Setgt (XPFile) xt100;
             Readpe (XPFile) xt100;
             //*** Beg Chg ***** 02/27/25 ****************************************
             //***If %Found(xmlteml);
             If not %Eof(xmlteml);
             //*** End Chg ***** 02/27/25 ****************************************
                intLastOrder# = XIorder;
                //Re-assign XTORDER for Summary Bill Document
                Setll (XPFile) xt100;
                Reade (XPFile) xt100;
                Dow not %Eof (xmlteml);
                   If XIDcid = BLSumd or XIDcid = BLSumDC;
                      intLastOrder# += 1;
                      Chain (XIxmlt) xt101;
                      If %Found(xmlteml1);
                         XTOrder = intLastOrder#;
                         Update xt101;
                         Leave;
                      Endif;
                   Endif;
                   Reade (XPFile) xt100;
                Enddo;
                //Re-assign XTORDER for Premium notice documents
                index = 0;
                Clear arrXMLFile;
                Setll (XPFile) xt100;
                Reade (XPFile) xt100;
                Dow not %Eof(xmlteml);
                   If XIType = 'Premium Notice';
                      index += 1;
                      arrXMLFile(index) = XIxmlt;
                   Endif;
                   Reade (XPFile) xt100;
                Enddo;
                index = 1;
                Dow arrXMLFile(Index) <> *Blank;
                   Chain (arrXMLFile(Index)) xt101;
                   If %Found(xmlteml1);
                      intLastOrder# += 1;
                      XTOrder = intLastOrder#;
                      Update xt101;
                   Endif;
                   index += 1;
                EndDo;
             Endif;
          Endif;
          Reade (xmlFileBlank:inXPLink) xp100;
       Enddo;

     p ReorganizeTemplates...
     p                 e
     d*=================================================================
     d* Write Scriptura Workfile - Bills Header (WXBLP)
     d*=================================================================
     p ScriptHeader    b
     d ScriptHeader    pi
     d  inXMLFileName                60

        //Fields from QHEAD2
        Clear pbl0p;
        B0XML = inXMLFileName;
        B0CO# = EMCo#;
        B0FND = EMFnd;
        B0EMP# = EMEmp#;
        B0DIV  = EMDiv;
        B0FYR  = ELFyr;
        B0TYPE = xxBillType;

        //Title
        If QQRev = 'Y';
           B0Title = tit2(1);                    //Revised Final Audit
        Else;
           B0Title = tit2(2);                    //Final Audit Billing
        EndIf;

        //Valid Untill
        If *In38 = *On;
           B0ValDtHd = 'Valid Untill:';
           If DSDate <> 0;
              B0ValDate = %Dec(%Char(%Date(DSDate:*mdy):*usa0):8:0);
           Endif;
        Endif;

        //*** Beg Add ***** 04/04/24 *********************************
        //Premium/Contribution
        If isContri = '1';
           B0PrmWord = 'Contribution';
        Else;
           B0PrmWord = 'Premium';
        Endif;
        //*** End Add ***** 04/04/24 *********************************

        //Reference#
        If *in38 = *On;
           B0RefHdg = 'Quote RefeXence:';
        Else;
           B0RefHdg = 'Policy Reference:';
        Endif;

        B0Policy# = QQpol#;

        B0PerdHd = XQperd;          //Policy Period heading:Participation Period OR For Period
        B0PfDate = %Dec(%Char(%Date(ELpfdt:*cymd):*usa0):8:0);
        B0PtDate = %Dec(%Char(%Date(ELptdt:*cymd):*usa0):8:0);
        B0DivHd  = XQDivH;

        If XQDiv <> *Blank;
           B0Div = %Int(XQDiv);
        Endif;

        If *In38  = *On;
           B0PrtDtHd = X1PROD;
        Else;
           B0PrtDtHd = XQPROD;
        Endif;

        If DSDate <> 0;
           B0PrtDate = %Dec(%Char(%Date(DSDate:*mdy):*usa0):8:0);
        Endif;

        If *In87  = *On;
           B0AnvDtHd = XQAnnH;
           If ELandt <> 0;
              B0AnvDate = %Dec(%Char(%Date(ELandt:*cymd):*usa0):8:0);
           Endif;
        Endif;

        B0RtStHd = XQRstH;
        B0RateSt = XXst;
        B0CarHdg = XQCapH;
        B0CarPol = XXCap#;
        B0WName1 = XXEMN1;
        B0WName2 = XXEMN2;
        B0WName3 = XXEMN3;
        B0WName4 = XXEMN4;
        B0WAddr1  = XXEMN5;
        B0WAddr2  = XXEMN6;
        B0WAddr3  = XXEMN7;

        //Experiene Modifiers
        If *In58 = *Off;
           If IsActive(ELemd1);
              If ELnoexm1 = 'Y';
                 B0MOD1 = *Zeros;
              Else;
                 B0MOD1 = ELExm1;
              Endif;
              B0MODDT1 = ELEmd1;
              B0ARP1   = ELArp1;
           Endif;
           If IsActive(ELemd2);
              If ELnoexm2 = 'Y';
                 B0MOD2 = *Zeros;
              Else;
                 B0MOD2 = ELExm2;
              Endif;
              B0MODDT2 = ELEmd2;
              B0ARP2   = ELArp2;
           Endif;
           If IsActive(ELemd3);
              If ELnoexm3 = 'Y';
                 B0MOD3 = *Zeros;
              Else;
                 B0MOD3 = ELExm3;
              Endif;
              B0MODDT3 = ELEmd3;
              B0ARP3   = ELArp3;
           Endif;
        Endif;

        //*** Beg Add ***** 06/06/25 *****************************
        //Increased limits
        If ELilcd <> *blanks;
           B0EMLB = LMDESC;
           B0LMACC = LMACC;
           B0LMDIS = LMDIS;
           B0LMAGG = LMAGG;
        Endif;
        //*** End Add ***** 06/06/25 *****************************
        //Standard Liability Limits
        //*** begin chg *** 11/05/25 *****************************
        //*****If *In58 = *Off and *In48 = *Off;
        If ELilcd = *blanks;
        //*** end chg ***** 11/05/25 *****************************
           //*** Beg Add ***** 06/06/25 *****************************
           B0EMLB = RREMLB;
           //*** End Add ***** 06/06/25 *****************************
           B0LMACC = RRACC;
           B0LMDIS = RRDIS;
           B0LMAGG = RRAGG;
        Endif;

        //Deductible Info: Ded Plan,Description,Amount per occurance/Claim, Aggregate amount
        If *In46 = *Off;
           B0dedt = XXdedt;          //Deductible Plan
           B0ddd1 = XXddd1;          //Deductible Description
           B0Ampo = XXampo;
           B0amag = XXamag;
           B0ddvb = XXddvb;          //'Per Claim' or 'Per Occurrence'
        Endif;

        //Amount Due date and Amount Due
        If *in57 = *Off;
           //*** Begin Chg *** 09/12/24 ************************************
           //*** B0NetP = Q1NETP;
           //*** B0duDt = x1DueD;
           If Q1netP > 0;
              B0NetP = Q1NETP;
              B0duDt = x1DueD;
           Else;
              B0NetP = 0;
              B0duDt = ' ';
           EndIf;
           //*** End   Chg *** 09/12/24 ************************************
        EndIf;

        //Footer
        //Comment
        B0COM1 = BCCOM1;
        B0COM2 = BCCOM2;
        B0COM3 = BCCOM3;

        //Make the Check Payable verbiage
        If xx3rdPV <> *Blank;
           B0PayVbg = xx3rdPV;
        Else;
           B0PayVbg = XXChCk;
        EndIf;

        B0ConNam = OFACNM;
        B0ConAdr1 = OFAAD1;
        B0ConAdr2 = OFAAD2;
        B0ConAdr3 =  %Trim(OFACTY) + ', ' + %Trim(OFst) + ' ' + OFMZ1;
        If OFMZ2 <> '0000';
           B0ConAdr3 = %Trim(B0ConAdr3) + '-' + OFMZ2;
        Endif;
        B0ConArea = OFAAra;
        B0ConPhn# = OFAPhn;
        B0AgtNam = X1PrNm;
        B0AgtAdr1 = X2AGNM;
        B0AgtAdr2 = AADD1;
        B0AgtAdr3 = ACSZ;
        B0Agt# = XQAGT#;
        B0Foot1 = %Subst(EXBTXT:1:80);
        B0Foot2 = %Subst(EXBTXT:81:80);
        B0Foot3 = %Subst(EXBTXT:161:80);
        B0AgtArea = AGARE1;
        B0AgtPhn# = AGPHN1;

        //Net Rate Flag
        B0NetRtF = F1NETR;

        //Minimum Premium Invoked Flag
        B0MinF = XXMINF;

        B0Pgm  = Q1PGM;
        B0User = Q1USER;
        B0Time = %Dec(%Char(%Time():*iso0):6:0);
        B0Dt   = %Dec(%Char(%Date():*cymd0):7:0);

        If inXMLFileName = xmlBillName;
           If ELARSM = 'Y';                    //If AR summarized policy
              B0SUMM = 'Y';
           Endif;
           Write pbl0p;
           If xmlCombName <> *Blank And turn = 'E'; // 04/23/24
              B0XML = xmlCombName;
              Write pbl0p;
           Endif;
        Endif;

        If inXMLFileName = xmlSumName;
           B0SUMM  = ' ';
           B0title = %Trim(B0title) + ' SUMMARY';
           Write pbl0p;
           If xmlSumCombName <> *Blank And turn = 'E';  // 04/23/24
              B0XML = xmlSumCombName;
              Write pbl0p;
           Endif;
        Endif;

     pScriptHeader     e

     d*=================================================================
     d* Write Scriptura Workfile - Bill Premium/Balance/Installments
     d* (WXBLP2)
     d*=================================================================
     p ScriptPremium   b
     d ScriptPremium   pi
     d  inXMLFileName                60

        //QPREM1
        Clear pbl0p2;
        B2CO# = EMCo#;
        B2FND = EMFnd;
        B2EMP# = EMEmp#;
        B2DIV  = EMDiv;
        B2FYR  = ELFyr;
        B2Type = xxBillType;
        B2Item = 'P';        //Premium
        B2Pgm  = Q1PGM;
        B2User = Q1USER;
        B2Time = %Dec(%Char(%Time():*iso0):6:0);
        B2Dt   = %Dec(%Char(%Date():*cymd0):7:0);

        For Index = 1 to 30;

           If QHdg(Index)  = *Blank;
              Leave;
           Endif;

           B2Hdg = QHdg(Index);
           //*** Beg Add *****  05/12/25  *******************************************
           // Print Full heading/message
           If B2Hdg = '* Indicated items not Prorated';
              B2Hdg = '* Indicated line items do not get prorated';
           Endif;
           //*** End Add *****  05/12/25  *******************************************
           //Percentage or Factor in Premium section
           Clear B2PrmPrc;
           Clear B2prmFact;
           intPos = %Scan('%':QPrc(index));

           If intPos <> 0;
              xxPercent = %Scanrpl('%':' ':QPrc(index));
              If xxPercent <> *Blank;
                 B2PrmPrc = %Dec(xxPercent:9:6);
              Endif;
           Else;
              If QPrc(index) <> *Blank;
                 B2prmFact = %Dec(QPrc(index):9:6);
              Endif;
           Endif;

           B2TAm = QTAm(Index);
           B2Sgn = QSgn(Index);

           intWXBLP2seq += 1;
           B2key  = intWXBLP2seq;

           //Write Premium record for current Bill for division
           B2XML = inXMLFileName;
           Write pbl0p2;

           //Write Premium record for combined Bill: For each Div
           If inXMLFileName = xmlBillName;
              If xmlCombName <> *Blank And turn = 'E';  // 04/23/24
                 B2XML = xmlCombName;
                 Write pbl0p2;
              Endif;
           Endif;

           //Write Premium record for Summary Bill - Once for each employer
           If inXMLFileName = xmlSumName;
              If xmlSumCombName <> *Blank And turn = 'E'; // 04/23/24
                 B2XML = xmlSumCombName;
                 Write pbl0p2;
              Endif;
           Endif;
        EndFor;

     pScriptPremium    e

     d*=================================================================
     d* Write Scriptura Workfile - Balance (WXBLP2)
     d*=================================================================
     p ScriptBalance   b
     d ScriptBalance   pi
     d  inXMLFileName                60

        Clear pbl0p2;
        B2CO# = EMCo#;
        B2FND = EMFnd;
        B2EMP# = EMEmp#;
        B2DIV  = EMDiv;
        B2FYR  = ELFyr;
        B2Type = xxBillType;
        B2Item = 'B';           //Balance
        B2Pgm  = Q1PGM;
        B2User = Q1USER;
        B2Time = %Dec(%Char(%Time():*iso0):6:0);
        B2Dt   = %Dec(%Char(%Date():*cymd0):7:0);

        For Index = 1 to 20;

           If AHD(Index) = *Blank And APC(Index) = 0 And AAM(Index) = 0;
              Leave;
           Endif;

           intWXBLP2seq += 1;
           B2XML = inXMLFileName;
           B2key  = intWXBLP2seq;

           B2AHD = AHD(Index);
           B2APC = APC(Index);
           B2AAM = AAM(Index);
           B2APS = APS(Index);

           //Write Balance record for current Bill for division
           Write pbl0p2;

           //Write Balance record for combined Bill: For each Div
           If inXMLFileName = xmlBillName;
              If xmlCombName <> *Blank And turn = 'E';  // 04/23/24
                 B2XML = xmlCombName;
                 Write pbl0p2;
              Endif;
           Endif;

           //Write Balance record for Summary Bill - Once for each employer
           If inXMLFileName = xmlSumName;
              If xmlSumCombName <> *Blank And turn = 'E'; // 04/23/24
                 B2XML = xmlSumCombName;
                 Write pbl0p2;
              Endif;
           Endif;

        EndFor;

     pScriptBalance    e

      *** End   Add *** 09/26/23 *******************************************
     d*** Begin Add *** 03/20/24 *******************************************
     d*=================================================================
     d* Check to see if audit Credit Exists
     d*=================================================================
     p AuditCreditExists...
     p                 b
     d AuditCreditExists...
     d                 pi              n
     d  inCo#                         3  0
     d  inFnd                         3  0
     d  inFyr                         3  0
     d  inEmp#                        9  0
     d  inDiv                         5  0
     d  inDate                        7  0

     d auditCredit     s               n

       auditCredit = *Off;

       SetGT  (inCo#:inFnd:inEmp#:inDiv:inFyr:inDate) pea00;
       //*** Begin Chg *** 09/12/24 ****************************************
       //*** ReadPE (inCo#:inFnd:inEmp#:inDiv:inFyr) pea00;
       ReadPE (inCo#:inFnd:inEmp#:inDiv:inFyr:inDate) pea00;
       //*** End   Chg *** 09/12/24 ****************************************
       DoW Not %EOF(wmeal);
          If E0arTy = 'D' And E0amt < 0;
             auditCredit = *On;
             Leave;
          EndIf;
          ReadPE (inCo#:inFnd:inEmp#:inDiv:inFyr) pea00;
       EndDo;

       Return auditCredit;

     pAuditCreditExists...
     p                 e
     d*** End   Add *** 03/20/24 *******************************************
      *** Begin Add *** 03/12/24 *******************************************
     d*=================================================================
     d* Check to see if division has payrolls
     d*=================================================================
     p HasDivPayrolls  b
     d HasDivPayrolls  pi              n
     d  inCo#                         3  0
     d  inFnd                         3  0
     d  inEmp#                        9  0
     d  inDiv                         5  0
     d  inFyr                         3  0

     d hasPayrolls     s               n

       //*** begin chg ***  10/25/25  ***************************
       //*****hasPayrolls = *Off;
       hasPayrolls = *On;

       //*****Chain (inCo#:inFnd:inFyr:inEmp#:inDiv) pep07;
       //*****If %Found(wmepl7);
       //*****   hasPayrolls = *On;
       //*****EndIf;
       //*** end chg *****  10/25/25  ***************************

       Return hasPayrolls;

     pHasDivPayrolls   e
      *** End   Add *** 03/12/24 *******************************************
       //*** Beg Add ***** 03/19/24 *************************************
       //===============================================================
       // Write Scriptura Workfile - Installments
       //===============================================================
       Dcl-Proc RtviDartsDesc;
       Dcl-Pi *N Char(256);
        inProessType Char(60);
        inBillType Char(1) const;
       End-Pi;

       Dcl-S strDesc Char(256);
       Dcl-S pos Zoned(3:0);

          If  QQRev = 'Y';
             strDesc = 'A Revised';
          Else;
             strDesc = 'An';
          Endif;

          Select;
          //Individual Bill
          When inBillType = 'I';
             strDesc = %Trim(strDesc) + ' ' + %trim(inProessType);
          //Summary Bill
          When inBillType = 'S';
             pos = %Scan(' ':inProessType);
             strDesc = %Trim(strDesc) + ' ' + %Subst(inProessType:1:pos-1) +
              ' Summary Bill';
          //Combined Bill
          When inBillType = 'C';
             strDesc = 'A Combined';
             If  QQRev = 'Y';
                strDesc = %Trim(strDesc) + ' Revised';
             Endif;
             strDesc = %Trim(strDesc) + ' ' + %trim(inProessType);
          Endsl;

          strDesc = %Trim(strDesc) + ' was requested by ' + %Trim(UPUSNM) +
           ' on ' + %Char(%Date():*USA) + ' for policy year ' +
           %Char(xxFyr) + '.';

          Return strDesc;

       End-Proc;
       //*** End Add ***** 03/19/24 *************************************
**
Manual Premium                                                                1
Contracting Class Credit
Inc Limits:
Experience Modifier                                    *** 02/28/95 ***
Standard Premium
Deviation
Stock Discount
Normal Premium
Expense Constant
Audited Premium                                        *** 05/12/25 ***       10
Total Minimum Premium
Prorata Factor
Experience Modifier-Split
Deductible Discount                                    *** 02/28/95 ***
Subject Premium                                        *** 02/28/95 ***
Managed Care Credit                                    *** 02/28/95 ***
Safety Credit                                          *** 02/28/95 ***
Additional Load                                        *** 02/28/95 ***
Second Deviation
Schedule Debit/Credit                                                         20
Surcharge
Second Surcharge
2nd I.L.:
State Tax
Total Premium
Short Rate Premium                                     *** 03/14/95 ***
1st Assessment
2nd Assessment
Adjusted Manual Premium
TOTAL MANUAL PREMIUM                                                          30
INDIANA RATE ADJUSTMENT
Waiver Surcharge
3rd Assessment
Merit Rating
Loss Constant
Safety Fee
Foreign Terrorism Chg                                  *** 10/01/10
ARAP Charge
Occupational Disease Coverage
4th Assessment                                                                40
5th Assessment
Voluntary Labor Charge                                 *** 09/01/04 ***
Domestic Terrorism Chg                                 *** 01/07/05 ***
Not Used                                               *** 09/01/04 ***
Not Used                                               *** 09/01/04 ***
ARAP Charge - Split
2nd Deductible Discount                                    01/08/04
CON,48  Future Use
Audited Premium
Membership Dues                                                               50
Installment Fees Due
Previously Billed
Previously Paid
Balance Due
Credit *
___________
Audited Total
Fees Due                                                   ***Late Fees Due
Manual Contribution                                        *** 12/13/05 ***
Standard Contribution                                      *** 12/13/05 ***   60
Normal Contribution                                        *** 12/13/05 ***
Audited Contribution                                       *** 05/12/25 ***
Total Minimum Contribution                                 *** 12/13/05 ***
Subject Contribution                                       *** 12/13/05 ***
Total Contribution                                         *** 12/13/05 ***
Short Rate Contribution                                    *** 12/13/05 ***
Adjusted Manual Contribution                               *** 12/13/05 ***
TOTAL MANUAL CONTRIBUTION                                  *** 12/13/05 ***
Final Audit Contribution                                   *** 12/13/05 ***
Company Factor                                             *** 12/18/05 ***   70
Harbor Exposure Fee                                        *** 06/19/06
Facultative Reinsurance Charge                             *** 07/06/06
CA FLAT RATE INCREASE                                      *** 06/29/07 ***
NY RATE DECREASE                                           *** 10/03/07 ***
6th Assessment                                                                40
7th Assessment
Fees Paid
Premium Paid
Large Deductible                                           01/12/10
2nd Large Deductible                                       01/12/10
STABILIZATION FACTOR                                       *** 01/14/10
CONTRIBUTION                                               *** 01/14/10
REVISED CONTRIBUTION                                       *** 01/14/10
CONTRIBUTION PAID                                          *** 01/14/10
HCN Credit                                                 *** 11/10/10
Unearn prem surcharge                                      07/13/2011
Contribution Paid
Earned Prem Credit                                         07/11/2014
Policy Fee                                                 02/01/18
  CON 90 - NOT USED
Non-Ratable Catastrophe Loading
Balance to I.L. Minimum                                    *** 08/14/24        92
Balance to 2nd I.L. Minimum                                *** 08/14/24        93
Audit Noncompliance Charge                                 07/25/25            94
** # MONTHS USING 93 RATES/HARBOR EXPOSURE %     *** ADD 02/28/95 ***
0100001
0200004
0300007
0400010
0500012
0600015
0700018
0800021
0900023
1000026
1100029
1300032
** RMK - DARTS REMARK
An audited bill print was requested by
A revised audited bill print was requested by
The billing schedule code was
The annual premium was
An installment summary was requested with a bill by
The annual contribution was
A combined audited bill was requested by
A combined revised audited bill was requested by
** TIT - TITLE TABLE
R E V I S E D  F I N A L  A U D I T
F I N A L  A U D I T  B I L L I N G
F I N A L  A U D I T  B I L L I N G
F I N A L  A U D I T  B I L L I N G
**  QCAEXEC TABLE
DLTSPLF FILE(QSYSPRT) SPLNBR(*LAST)
** TIT2 - TITLE TABLE #2
REVISED FINAL AUDIT
FINAL AUDIT BILLING
**
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
**
Fees Billed                                                           08/01/05
Stop Gap Fees Due
NOT USED
NOT USED
NOT USED
NOT USED
NOT USED
NOT USED
NOT USED
NOT USED
**  T71
  NOT AN INVOICE - FOR INFORMATION ONLY
** Messages                                                            ***
* This policy has invoked minimum premium.
* This policy has invoked minimum contribution.
* Indicated items not Prorated                                                   *** 05/12/25 ***
**  VOLUNTEER TABLE
E.M.T. Workers
Search & Rescue
Waterworks
Firemen
Police Officers
Deputies
Salesperson/Outside
Clerical
Hospital
School
Reserve Deputies
Total Payroll
