     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H BNDDIR('WTBNDDIR')
      /TITLE W7720R - FINAL AUDITED PREMIUMS
      *****************************************************************
      * REVISIONS:                                                    *
     H*                                                               *
     H* 09/18/25 C:342 Project: WC3014                                *
     H*                Prorate Control in CX Reason Code Table        *
     H*                                                               *
     H* 09/05/25 C:65  Support: 119669 (Sunz)                         *
     H*                Speed improvements to calculations:  Remove    *
     H*                call to SREXCO in WBILLCSPEC - always update   *
     H*                WDMPP with standard premium.                   *
     H*                                                               *
     H* 08/14/25 C:65  PROJECT: WC2952/PHW#119563 (Amwins)            *
     H*                Change the coding for 119268 change to do the  *
     H*                rounding once instead of twice for rates when  *
     H*                two digit rates are needed.                    *
     H*                                                               *
     H* 08/01/25 C:65  Support: 119398   (Sunz)                       *
     H*                Change the logic which determines which divs   *
     H*                will run. If a summarized div is running, or   *
     H*                if the expiration date includes the state rpt  *
     H*                div, then all summarized divisions will run.   *
     H*                If a non-summarized division with a different  *
     H*                expiration date than state rpt div runs then   *
     H*                do not process any summarized divisions.       *
     H*                                                               *
     H* 06/25/25 C:65  PROJECT: WC2952/PHW#119268 (Prescient)         *
     H*                Add date for adoption of 3 digit rates.  If    *
     H*                the policy is not effective on/after the date  *
     H*                entered, or there is no date, then the payroll *
     H*                rates will be calculated to 2 digits.          *
     H*                                                               *
     H* 05/22/25 C:300 Support: 118382 (Prescient)                    *
     H*                Issue: Total premium on CEU report not matching*
     H*                with Retrospective Settlement Bill amount      *
     H*                Fix: The claims replenishment offset records   *
     H*                needs to be reduced by the premium a/r being   *
     H*                removed from summarized divisions.             *
     H*                                                               *
     H* 04/18/25 C:342 Ticket: 118852 AmWINS                          *
     H*                scrapped divide by zero error while running    *
     H*                audit bill                                     *
     H*                                                               *
     H* 03/05/25 C:062 Ticket: 118644(133)                            *
     H*                Remove WMXAP records if running audits.        *
     H*                                                               *
     H* 01/21/25 C:062 Ticket: 118278(133)                            *
     H*                Don't create wmxap records in audits.          *
     H*                                                               *
     H* 11/19/24 C:062 Ticket: 117721(133)                            *
     H*                Don't recreate contested hold records if the   *
     H*                invoice is no longer being used.               *
     H*                                                               *
     H* 11/13/24 C:291 Project: WC2141                                *
     H*                Update the stock discount calculation based on *
     H*                each periods total premium instead of number  *
     H*                of days in each period.                        *
     H*                                                               *
     H* 11/12/24 C:65  Support: 103922 (Amwins)                       *
     H*                Change premium after increased limits to be    *
     H*                unique for each assessment since premium may   *
     H*                not be the same as regular standard premium.   *
     H*                                                               *
     H* 11/11/24 C:300 Project: WC2985                                *
     H*                Apply Deductible Discount to Discounted Premium*
     H*                in WBILLCSPEC copybook                         *
     H*                                                               *
     H* 11/08/24 C:65  Support: 117363 (Amwins)                       *
     H*                Correct the balance to minimum figure to be    *
     H*                based on sum of MPAAPR instead of XXWKCM since *
     H*                that figure can include annual exp constant.   *
     H*                                                               *
     H* 09/24/24 C:65  Support: 117596 (Prescient)                    *
     H*                KY Assessment not calculating correctly on     *
     H*                Large Deductible - populate XXDDCT so large    *
     H*                deductibles can be excluded properly in        *
     H*                WBILLCSPEC.                                    *
     H*                                                               *
     H* 08/14/24 C:291 Project: WC2974                                *
     H*                Populate actual Increased Limit amount and     *
     H*                Balance to Increased Limit into WDMPP.         *
     H*                                                               *
     H* 07/24/24 C:333 Support: 115804 (SUNZ)                         *
     H*                Safety Fee is not always posting to A/R at audit
     H*                - Safety fee not adding for Summarized A/R.    *
     H*                - Similar issue for member fee and Tax.        *
     H*                                                               *
     H* 07/21/24 C:300 Support: 114005 (AmWins)                       *
     H*                Round Expense Constant to Whole Dollar, which  *
     H*                is causing 'Balance to Minimum Premium' in cents
     H*                                                               *
     H* 06/17/24 C:344 PROJECT: WC2952                                *
     H*                Expand Rate field to zoned 8.3                 *
     H*                                                               *
     H* 06/11/24 C:300 Support: 115743 (SUNZ) (Ref-103547)            *
     H*                Calculate 3rd assessment Deductible Discount on*
     H*                the Large Deductible Policy same as it does on *
     H*                the Small Deductible Policies, when following  *
     H*                flag is 'Y'                                    *
     H*                'Apply to Discount on Deductible Policies Only'*
     H*                Also correcting typo error for 7th Assessment  *
     H*                                                               *
     H* 04/18/24 C:291 Project: WC2961                                *
     H*                Recompile for changes done in WBILLCSPEC for   *
     H*                Schedule DB/CR applied against Manual Premium. *
     H*                                                               *
     H* 02/27/24 C:333 PROJECT: WC2953                                *
     H*                Premium History Balance to Minimum             *
     H*                                                               *
     H* 02/19/24 C:65  Support: 115080 (FMIC)                         *
     H*                Change the minimum manual premium mod for FMIC *
     H*                to not skip minimum premium check when the     *
     H*                exclusion from $2500 checkbox is checked.      *
     H*                                                               *
     H* 02/06/24 C:300 Support: 114575 (AmWINS)                       *
     H*                Annual Policy pulling a large pro-rata factor  *
     H*                Fix - For Commercial Flagged Groups, Calculate *
     H*                Prorate factor based on 365 days instead of the*
     H*                group year lenght days                         *
     H*                                                               *
     H* 01/05/24 C:62  Ticket: 114720(129)                            *
     H*                Add processing for mass process for IVC        *
     H*                                                               *
     H* 12/01/23 C:65  Project: Phw#114374 (Vital) (WBILLCSPEC CHG)   *
     H*                For AZ only process the schedule rating to     *
     H*                the premium before deviation if applied after  *
     H*                deviation.                                     *
     H*                                                               *
     H* 11/07/23 C:65  Project: WC2810                                *
     H*                Add balance to min to WDMPP and add pre-min    *
     H*                manual/std/normal/annual and balance to min    *
     H*                to WDEHP.                                      *
     H*                                                               *
     H* 11/02/23 C:300 Project: WC2810/Phw#114498 (Sunz)              *
     H*                Load full expense constant figure into ELPRTF  *
     H*                                                               *
     H* 10/25/23 C:340 Project: WC2922                                *
     H*                When Audit is run a policy/division which is   *
     H*                flagged as contested audit then do not send the*
     H*                invoice information to IVC. Instead write the  *
     H*                information to the new holding file.           *
     H*                                                               *
     H* 10/19/23 C:65  Project: Phw#114376 (Internal)                 *
     H*                Change the governing class code check to save  *
     H*                the values off per state rather than per emplr.*
     H*                                                               *
     H* 09/13/23 C:65  Project: Phw#113897 (Sunz)                     *
     H*                Restore coding applying territory diff to      *
     H*                regular payroll rates.                         *
     H*                                                               *
     H* 09/12/23 C:62  Ticket: 113870(140)                            *
     H*                -Expand division to 5.0                        *
     H*                -Remove invoice from array if inv# found in AR *
     H*                                                               *
     H* 07/30/23 C:62  Project: WC2918                                *
     H*                Populate EHnoexm2 and EHnoexm3.                *
     H*                                                               *
     H* 07/25/23 C:65  Project: Phw#113053 (SUNZ)                     *
     H*                Cap share of total premium % at 100 if a value *
     H*                would blow up the field.  This could happen    *
     H*                at early stage of calculation when first       *
     H*                writing the WDDSP records and saved total      *
     H*                premium in WXEVP was too low.                  *
     H*                                                               *
     H* 07/18/23 C:65  PROJECT: 150WFY (revision to 150WAR)           *
     H*                Remove the capping of discount under 100%.     *
     H*                                                               *
     H* 07/07/23 C:300 Project: WC2810                                *
     H*                Show Balance to Minimum on Output              *
     H*                                                               *
     H* 06/26/23 C:65  Project: Phw#110527 (SUNZ)                     *
     H*                Change the coding which allows the short term  *
     H*                for concurrency flag to prorate minimum prem   *
     H*                on non-cancelled policies to allow it on non-  *
     H*                renewal cancellations that are full term.      *
     H*                                                               *
     H* 06/15/23 C:65  PROJECT: 150WFY                                *
     H*                Load MPEARC and MPRPE1/2/3.                    *
     H*                                                               *
     H* 06/14/23 C:65  PROJECT: 150WFY (revision to 150WAR)           *
     H*                Percentage of manual premium calculations for  *
     H*                MI divisions will be based on modified premium *
     H*                instead of discounted premium.  Calculate % of *
     H*                manual premium regardless of minimum premium   *
     H*                - use the pre-minimum premium figures.  Cap    *
     H*                the calculated discount at 99.9%.  Very small  *
     H*                policies had expense constant and terrorism    *
     H*                charges that would cause discount to go over   *
     H*                100% to reach the targetted premium.           *
     H*                                                               *
     H* 05/24/23 C:65  Project: Phw#110545 (Prescient)                *
     H*                Restore coding applying LCM to regular payroll *
     H*                rates.                                         *
     H*                                                               *
     H* 04/27/23 C:342 Project: Phw#101984 (SUNZ)                     *
     H*                Reduce looping over WMDSL4, WDDSL, WDELL1      *
     H*                                                               *
     H* 09/26/22 C:65  Project: Phw#103757 (Amwins)                   *
     H*                Change div# in invoice cloud array processing  *
     H*                to be 5 digits instead of 3 digits.            *
     H*                                                               *
     H* 06/29/22 C:340 Support: WC2854                                *
     H*                Invoice Cloud:  Retain Payment Ref#            *
     H*                                                               *
     H* 06/16/22 C:65  Project: Phw#102594 (CoveRisk)                 *
     H*                Ensure when running a mass audit to skip any   *
     H*                division that is not yet expired, either by    *
     H*                policy expiration date or by cancellation date *
     H*                if it exists.  Additionally, correct the check *
     H*                on non-audited payroll to move the status      *
     H*                check off the loop to prevent bailing early    *
     H*                on the loop.                                   *
     H*                                                               *
     H* 05/16/21 C:062 Project WC2663                                 *
     H*                Apply invoice #.                               *
     H*                                                               *
     H* 02/05/21 C:65  Project: Phw#94364 (THE)                       *
     H*                Subtract xxmin1 and xxmin2 from minimum prem   *
     H*                total on state reporting division if they are  *
     H*                begin applied to a different division.         *
     H*                                                               *
     H* 11/18/20 C:180 Project WC2757                                 *
     H*                Recompiled only.                               *
     H*                                                               *
     H* 10/07/20 C:65  Project: Phw#90671 (THE)                       *
     H*                Set increased limits to minimum calculated     *
     H*                field on minimum premium divisions since it    *
     H*                may be applied to an additional division.      *
     H*                                                               *
     H* 09/10/20 C:65  Project: Phw#91280 (Sunz)                      *
     H*                Remove PA exception for pulling rates - always *
     H*                use state reporting division policy eff date.  *
     H*                                                               *
     H* 08/19/20 C:65  Project: Phw#90449 (Sunz)                      *
     H*                Fix population of XSPFDT.                      *
     H*                                                               *
     H* 08/07/20 C:65  Project: WC2754  (WBILLCSPEC UPDATE)           *
     H*                If a KY policy/division has a deductible disc, *
     H*                then do not include schedule db/cr factor in   *
     H*                calculation of any assessment premium basis.   *
     H*                Note - CALDBR subroutine unused, so removed.   *
     H*                                                               *
     H* 08/05/20 C:65  Project: WC2756                                *
     H*                Do not include payroll classes in terrorism    *
     H*                that are flagged to be excluded.               *
     H*                                                               *
     H* 07/30/20 C:65  Project: Phw#90082 (Sunz)                      *
     H*                Populate pre-minimum premium values into wdmpp *
     H*                for exp mods and more premium levels.          *
     H*                                                               *
     H* 04/28/20 C:65  Project: Phw85530 (FMIC)                       *
     H*                Clear out the LM2 array.                       *
     H*                                                               *
     H* 04/03/20 C:136 Project: 148WAH (Rev)                          *
     H*                Zero out policy fee if flat cx'd               *
     H*                                                               *
     H* 01/31/20 C:65  Project: Phw#86691 (CareWest)                  *
     H*                Use unaltered manual premium for split period  *
     H*                file's manual premium.  This will correct net  *
     H*                rate calculation on split policies.            *
     H*                                                               *
     H* 01/22/20 C:65  Project: Phw#79324 (Amwins)                    *
     H*                Add fixes for calculating assessments on incr  *
     H*                limits basis - the coding was missing entirely *
     H*                from the section where assessments were        *
     H*                calculated on a split basis.                   *
     H*                                                               *
     H* 01/10/20 C:65  Project: Phw#86343                             *
     H*                Calculate ELDISP using the total discount and  *
     H*                premium on the policy (or independent state)   *
     H*                instead of just the division, to arrive at a   *
     H*                consistent percentage.                         *
     H*                                                               *
     H* 12/16/19 C:180 Project: C150WEH - Recompile only.             *
     H*                                                               *
     H* 10/17/19 C:65  Project: 150WCS/Phw#83848(Luba)                *
     H*                Set the saved expiration date for checking if  *
     H*                division should process properly for flat cx.  *
     H*                                                               *
     H* 09/06/19 C:136 Support: 71752                                 *
     H*                Don't include terrorism in assessment basis    *
     H*                for 'X' level (annual premium) when including/ *
     H*                excluding increased limits                     *
     H*                                                               *
     H* 08/09/19 C:65  Project: 150WCS/Phw#81786(Luba)/#77600(FMIC)   *
     H*                Change check on expiration date to use ELPTDT  *
     H*                as the saved value is not reflective of cx'd   *
     H*                division.                                      *
     H*                                                               *
     H* 08/02/19 C:291 Project: C146WJF                               *
     H*                Reduce Terrorism charges by Deviation #1       *
     H*                percentage if Group master is flagged as       *
     H*                'Excess Insurance Group'.                      *
     H*                                                               *
     H* 05/08/19 C:65  Project: Phw#79324 (Amwins)                    *
     H*                Use XSPFDT for pulling assessments and terror- *
     H*                ism for NJ instead of ELPFDT.                  *
     H*                                                               *
     H* 04/22/19  C:65  PROJECT: WC2610                               *
     H*                 Recompiled for WBILLCSPEC changes.            *
     H*                                                               *
     H* 02/12/19 C:291 Project: WC2206                                *
     H*                Populate the new flag EHNOEXM1 in WDEHP file.  *
     H*                                                               *
     H* 01/03/19 C:65  PROJECT: Phw# 75954 (Amwins)                   *
     H*                Increase size of field to avoid size error.    *
     H*                                                               *
     H* 12/20/18 C:65  PROJECT: Phw# 74063 (Amwins)                   *
     H*                Alter coding to load base terrorism rates to   *
     H*                employer detail to be more recognizant of      *
     H*                non-payroll based terrorism being the same     *
     H*                rate, or not having a charge.                  *
     H*                                                               *
     H* 12/19/18 C:65  Project: Phw#75474 Amwins                      *
     H*                Remove premium period records that no longer   *
     H*                apply to policy.                               *
     H*                                                               *
     H* 12/18/18 C:291 Support PHW75074                               *
     H*                Fix NY flat rate Adj for cancelled polocies.   *
     H*                                                               *
     H* 08/08/18 C:062 Project: 150WCS                                *
     H*                Add All Division Parameter                     *
     H*                                                               *
     H* 08/06/18 C:136 Support: 71945A (TASB only)                    *
     H*                Modify large deductible aggregate calculation  *
     H*                per client request.                            *
     H*                                                               *
     H* 07/30/18 C:33  Project: 150WAR  #72438                        *
     H*                Check for SRMDIV vs SRMIL                      *
     H*                                                               *
     H* 06/22/18 C:65  Project: 150WAL                                *
     H*                Change rounding on territory factor to round   *
     H*                after applying both LCM and territory.         *
     H*                                                               *
     H* 06/19/18 C:65  Project: 150WCS                                *
     H*                To reduce loops over all divisions three       *
     H*                sections of coding will be stored off in a     *
     H*                qtemp file - WXEVP.  Once the information is   *
     H*                determined it will be written to the file and  *
     H*                retrieved from there for every other instance. *
     H*                The sections are:                              *
     H*                Governing Class Code Check                     *
     H*                Regular Minimum Premium Check which includes   *
     H*                Increased limits, Loss constant, and F-class.  *
     H*                Class 8810 Minimum Check                       *
     H*                                                               *
     H* 06/11/18 C:65  Project: 150WAQ                                *
     H*                Update the writing/adjusting of A/R prevention *
     H*                to check the actual setup file now.            *
     H*                                                               *
     H* 03/19/18 C:65  PROJECT: Phw# 150WAD (Sunz)                    *
     H*                Prorate expense constant before adding to      *
     H*                minimum premium work field when policy is      *
     H*                short term for concurrency.                    *
     H*                                                               *
     H* 02/01/18 C:136 Project: 148WAH                                *
     H*                Add Policy Fee                                 *
     H*                                                               *
     H* 01/30/18 C:291 PROJECT WC2560                                 *
     H*                Apply a flat rate adjustment that reduce       *
     H*                the manual premium total for 'NY' state.       *
     H*
     H* 11/17/17 C:65  PROJECT: Phw# 51139 (Sunz)                     *
     H*                Round dollar basis for terrorism charges to    *
     H*                whole dollar instead of dropping the cents.    *
     H*                Additionally, pull terrorism rates straight    *
     H*                from stat rpt/rating for wdelp when LCM is not *
     H*                being applied.                                 *
     H*                                                               *
     H* 10/20/17 C:65  Project: 150WAQ                                *
     H*                Ensure no writing or adjusting of A/R occurs   *
     H*                for policies with a collateral setup.          *
     H*                (temp change - disable based on 150 - setup    *
     H*                 checking to be done soon)                     *
     H*                                                               *
     H* 10/13/17 C:65  Project: 150WBU                                *
     H*                Run audit bills on all divisions regardless of *
     H*                expiration date for client 150.                *
     H*                                                               *
     H* 10/02/17 C:65  Project: 150WAR                                *
     H*                Clear large deductible % on SRMIL call and do  *
     H*                not update % on minimum premium policies.      *
     H*                Also change calculation to SRMDIV call.        *
     H*                                                               *
     H* 08/03/17 C:65  Project: 150WAR                                *
     H*                Add processing to calculate a large deductible *
     H*                percentage in order to arrive at a % of manual *
     H*                premium figure specified in employer detail.   *
     H*                Assessments and state tax which are flagged to *
     H*                be excluded will not figure into target        *
     H*                estimated premium figure.  The large deduc %   *
     H*                will be calculated during the 'S' call from    *
     H*                SRDIS2 stage of the calculations.              *
     H*                                                               *
     H* 07/26/17 C:62  Project: 150WAL                                *
     H*                Add Territorial Differential Factor Process.   *
     H*                                                               *
     H* 07/17/17 C:65  PROJECT: 150WAP                                *
     H*                ADD CODING TO RECOGNIZE THE 'C' VALUE FOR PEO. *
     H*                                                               *
     H* 06/22/17 C:65  PROJECT: Phw# 46465 (CoveRisk)                 *
     H*                Populate ARAP factors into employer history.   *
      *                                                               *
      * 06/19/17 C:136 Support: 46320                                 *
      *     !!!!!!!!   Correct GetStRptPDays. WPIEMPLR must  !!!!!!!! *
      *                accompany this pgm if this pgm is retrofitted. *
      *                                                               *
      * 04/07/17 C:062 Project 136WHF                                 *
      *                Add Territory Rating for CareWest.             *
      *                                                               *
      * 09/14/16 C:136 Project: WC2070                                *
      *                Add premium charge for uncooperative audit     *
      *                                                               *
      * 08/17/16 C:65  PROJECT: Phw# 42547 (THE)   (Wbillcspec chg)   *
      *                Clear out increased limits amounts from all    *
      *                arrays when minimum is to be invoked.          *
      *                                                               *
      * 04/27/16 C:306 Project: WC2424                                *
      *                Populating the new fields in WDEHP file.       *
      *                Exeperience Modifier - not final status.       *
      *                                                               *
      * 04/19/16 C:242 Project: WC2069                                *
      *                Add Billing Fees to Premium History            *
      *                                                               *
      * 04/06/16 C:136 Phaseware: 40646                               *
      *                Correct stock discount calculation to always   *
      *                use the prorate factor from the state          *
      *                reporting division                             *
      *                                                               *
      * 02/12/16 C:136 Support: 39836                                 *
      *                Correct terrorism rates reported to WCPOLS.    *
      *                Average rates only when different LCMs are     *
      *                present. Do not include non-payroll classes    *
      *                in the terrorism rate calculation.             *
      *                                                               *
      * 10/20/15 C:302 PROJECT: WC2364                                *
      *                Add non-ratable class code flag                *
      *                                                               *
     H* 10/07/15 C:65  PROJECT: Phw# 37696                            *
     H*                Calculate DE deductible in the same manner as  *
     H*                done for PA under supp# 49203 - calculate on   *
     H*                manual premium + increased limits (update is   *
     H*                in WBILLCSPEC only).                           *
     H*                                                               *
     H* 09/09/15 C:65  Project: WC2375                                *
     H*            1)  Restore prior a/r deletions based on posting   *
     H*                date instead of transaction date.  Leaving A/R *
     H*                posted to the future causes problems with the  *
     H*                premium notice and the fact that A/R should    *
     H*                not be owed well into the future once a policy *
     H*                has been cancelled and audited.                *
     H*                                                               *
     H*            2)  If defer prior audits on debit/credit adj is   *
     H*                checked at billing options then offset any     *
     H*                pending audit transactions and add amount to   *
     H*                the debit/credit adjustment.  Change posting   *
     H*                date if final adjustment switches debit/credit *
     H*                status.                                        *
     H*                                                               *
     H*            3)  Restore original functionality of the ADJAR    *
     H*                subroutine as it is meant to delete/reverse    *
     H*                off all A/R on a summarized division.          *
     H*                                                               *
     H* 07/02/15 C:65  PHW# 35261 (UBIC)                              *
     H*                Remove any installment fees with posting date  *
     H*                greater than or equal to system date since an  *
     H*                audited policy is not paying anymore           *
     H*                installments.                                  *
     H*                                                               *
     H* 06/05/15 C:65  PHW# 33614                                     *
     H*                Correct clearing of 4th assessment category.   *
     H*                                                               *
     H* 05/11/15 C:279 Project: WC2361                                *
     H*                Modified to split F-class premium from regular *
     H*                premium if flagged exclude F-class.            *
     H*                                                               *
     H* 02/13/15 C:136 PHW33176                                       *
     H*                Correct terror rates that were added to the    *
     H*                payroll loop by WC2211                         *
     H*                                                               *
     H* 02/04/15 C:136 Support: PHW32962                              *
     H*                Fix amalgated terrorism rates (ELTERR, ELDTRR) *
     H*                implemented by WC2211                          *
     H*                                                               *
     H* 11/13/14 C:136 Support: WA140 87322                           *
     H*                Fix assessment calculation on ARD policies     *
     H*                                                               *
     H* 10/28/14 C:136 Support: WA146 87089                           *
     H*                Correct terrorism calculation. It was only     *
     H*                charging for the last payroll split.           *
     H*                                                               *
     H* 09/23/14 C:136 Support: WA114 86797                           *
     H*                Correct WC2193 so that previous premium data   *
     H*                doesn't overwrite current premium data in      *
     H*                WDEHP                                          *
     H*                                                               *
     H* 07/16/14 C:136 Project: WC2295                                *
     H*                Add OK Rate Split for 02/01/2014               *
     H*                                                               *
     H* 07/14/14 C:283 Support: WA025 85403                           *
     H*                Increasing the size of Array's FY, EF and EX.  *
     H*                                                               *
     H* 07/11/14 C:279 Project: 111WBL                                *
     H*                Modified to subtract the dividend from the     *
     H*                premium prior to determining billing amount.   *
     H*                                                               *
     H* 06/30/14 C:65  Project: WC2211 (Supp# WA114/85972)            *
     H*                Call SRLCM prior to calculating terrorism      *
     H*                charges on payroll classes as it is not called *
     H*                previously for 'A' rated policies.             *
     H*                                                               *
     H* 06/16/14 C:136 Project: WC2283 (TN Rate Split)                *
     H*                Add logic for rate split                       *
     H*                                                               *
     H* 03/19/14 C:136 Support: WA045 84912                           *
     H*                Do not allow flat cx divisions to process      *
     H*                harbor exposure minimum.                       *
     H*                                                               *
     H* 02/21/14 C:180 Support: WA093 82176                           *
     H*                Fix tax adj being incorrectly omitted.         *
     H*                                                               *
     H* 10/30/13 C:283 Support: Supp# WA102/83118                     *
     H*                Minimum Premium not applying for FMIC because  *
     H*                xxpror is not getting the value before that    *
     H*                calculation. Moving the code to calculate the  *
     H*                xxpror before the Minimum premium calculation. *
     H*                                                               *
     H* 10/08/13 C:65  PROJECT: WC1957                                *
     H*                Change the harbor exposure minimum coding to   *
     H*                find the highest harbor minimum across the     *
     H*                policy and to apply it to the division it      *
     H*                originates from.  All divisions will not       *
     H*                charge for the F-class premium if minimum      *
     H*                has been invoked.                              *
     H*                                                               *
     H* 08/21/13 C:65  Project: Supp# WA114/83011                     *
     H*                Correct rewrite of retro offset a/r writing    *
     H*                as the amount should have been negated.        *
     H*                                                               *
     H* 07/15/13 C:282 Project: WC2211                                *
     H*                Apply LCM from each payroll class when         *
     H*                calculating terrorism rate.                    *
     H*                                                               *
     H* 07/24/13 C:65  Project: WC2037                                *
     H*                Change array for annualized premium (change    *
     H*                is in WBILLCSPEC).                             *
     H*                                                               *
     H* 07/19/13 C:136 Project: WC2193                                *
     H*                Add unbilled premium                           *
     H*                 Write/Update WXELP if Calc & Print            *
     H*                 Delete WXELP if A/R is updated                *
     H*                 UserOpen on File for backwards compatibility  *
     H*                                                               *
     H* 06/25/13 C:65  Project: 147WAI                                *
     H*                Adjust the audit premium adjustment by the     *
     H*                amount returned from the dividend API so the   *
     H*                regular billed premium on the policy equals    *
     H*                audited annual premium - dividend.  This is    *
     H*                only called when A/R is written (once for      *
     H*                each division when not summarized or once for  *
     H*                each policy if summarized).  Additionally,     *
     H*                ignore dividend billings when accumulating     *
     H*                prior billed premium as the API will always    *
     H*                the total dividend on the division/policy.     *
     H*                Add binding directory WTBNDDIR as well.        *
     H*                                                               *
     H* 03/28/13 C:65  Project: WC2037                                *
     H*                Revamp the unearned premium surcharge calc.    *
     H*                Build the unearned premium bucket prior to     *
     H*                stock discount calculation and apply the       *
     H*                remaining premium calculations.                *
     H*
     H* 11/26/12 C:136 Support: WA093 81130                           *
     H*            283 Correct logic that writes credits to offset    *
     H*                future installments                            *
     H*                                                               *
     H* 02/08/13 C:190 Project: 140WAU                                *
     H*            Process deposit off of estimating type plans       *
     H*            as well as S/R plans for cancellation audit.       *
     H*                                                               *
     H* 11/06/12 C:136 Support: WA132 80913                           *
     H*                Correct prem adj logic on non-summarized       *
     H*                divisions                                      *
     H*
     H* 10/01/12 C:136 Support: WA102 80864                           *
     H*                Correct deposit transfer logic so that         *
     H*                offsets are posted to the correct year         *
     H*                                                               *
     H* 08/24/12 C:136 Support: WA136 80597                           *
     H*                Correct deposit transfer logic implemented     *
     H*                for 77239                                      *
     H*                                                               *
     H* 08/15/12 C:136 Support: WA114 80401                           *
     H*                Add expense constant category to logic         *
     H*                implemented for 77239                          *
     H*                                                               *
     H* 06/20/12 C:136 Support: WA090 80190                           *
     H*                Correct A/R Summary logic impacted by          *
     H*                77239                                          *
     H*                                                               *
     H* 05/24/12 C:136 Support: WA132 79982                           *
     H*                Correct deposit transfer logic to not          *
     H*                divide the offset amounts by 100               *
     H*                                                               *
     H* 05/21/12 C:255 : Retrofitted changes of support 79945.        *
     H* 05/18/12 C:136 Support: WA132 79945                           *
     H*                Fix post dates with *hival by removing WC1942. *
     H*                77239 provided logic that WC1942 needed to     *
     H*                implement.                                     *
     H*                                                               *
     H* 04/02/12 C:255 Project WC2135                                 *
     H*                Add processing to split the assessments for    *
     H*                any NY division.                               *
     H*                                                               *
     H* 11/16/11 C:255 SUPPORT: WA045 77239                           *
     H*                Added following changes.                       *
     H*                1) Do not change future A/R with transaction   *
     H*                   date less than system date.                 *
     H*                2) Write the Credit with the system date if a  *
     H*                   debit balance doesn't exist for future of   *
     H*                   system date, If it does post the credit to  *
     H*                   the first future debit billing's post date. *
     H*                                                               *
     H* 10/13/11 C:190 PROJECT WC2079                                 *
     H*          C:191                                                *
     H*                To calculate a flat rate adjustment that reduce*
     H*                the manual premium total for 'IL' state.       *
     H*
     H* 09/21/11 C:191 Project: WC1942                                *
     H*                Process credit by posting it to latest audit   *
     H*                transaction date                               *
     H*                                                               *
     H* 08/22/11 C:191 Project: WC2037 Priss #26                      *
     H*                Unearned surcharge can't be -ve                *
     H*                                                               *
     H* 08/16/11 C:061 PROJECT 114WZW
     H*                Use expanded Minimum Prem field (if available).
     H*                                                               *
     H* 07/20/11 C:062 Project: 77467                                 *
     H*                Prevent division by zero.                      *
     H*                                                               *
     H* 07/13/11 C:191 Project: WC2037                                *
     H*                Premium Determination on cancellations in GA   *
     H*                                                               *
     H* 05/26/11 C:191 Project: WC2038                                *
     H*                Account for new assessment calculations flags  *
     H*                                                               *
     H* 04/29/11 C:136 SUPPORT: WA146 76705                           *
     H*                CORRECT TERRORISM CALCULATIONS                 *
     H*                                                               *
     H* 01/06/11 C:136 Support: WA025 74837                           *
     H*                Adjust cancellation check logic to account     *
     H*                for break in coverage                          *
     H*                                                               *
     H* 12/31/10 C:255 PROJECT: WC1918                                *
     H*                RECOMPLIED TO MAKE IT COMPATIBLE WITH THE NEW  *
     H*                FIELD IN WDELP FOR POLICY PAGE, ITEM 3C.       *
     H*                                                               *
     H* 12/23/10 C:255 PROJECT: 71WLD                                 *
     H*                Recompile only as new fields added in file     *
     H*                WDEHP.                                         *
     H*                                                               *
     H* 12/16/10 C:065 SUPPORT: WA025/75112                           *
     H*                Change the cancellation check logic to loop    *
     H*                backwards through the cancellation date array. *
     H*                                                               *
     H* 12/09/10 C:062 SUPPORT: WA025 75034                           *
     H*                Correct SRCALI logic.                          *
     H*                                                               *
     H* 11/10/10 C:65  PROJECT: 139WPL                                *
     H*                Add new calculations for HCN credit for GIC.   *
     H*                This credit will be applied before subject     *
     H*                premium and will have multiple periods. Each   *
     H*                period for the credit will calculate and the   *
     H*                discount will be totaled for the total discount*
     H*                amount.                                        *
     H*                                                               *
     H* 10/19/10 C:136 SUPPORT: WA136 74508                           *
     H*                PAYROLL NET PREMIUM SHOULD EQUAL MINIMUM PREM  *
     H*                SO THAT THE BILL'S PAYROLL PREMIUM DOES NOT    *
     H*                DIFFER FROM THE MAN PREMIUM LINE ITEM ON       *
     H*                NET RATE POLICIES THAT INVOKE MINIMUM PREMIUM  *
     H*                                                               *
     H* 10/18/10 C:191 Project: WC1939                                *
     H*                09/01/2010 OK Rate split                       *
     H*                If a policy with OK coverage is unexpired as   *
     H*                of 09/01/10 then the policy will need to be    *
     H*                split for the rate decrease                    *
     H*                                                               *
     H* 09/14/10 C:257 Project: 93WJC                                 *
     H*                Prevent the normal premium from being updated  *
     H*                if running for KRMS.                           *
     H*                                                               *
     H* 09/07/10 C:255 PROJECT: WC1929                                *
     H*                Generate Audited premium by checking flag      *
     H*                'Disable DE Rate flag' based on this, changes  *
     H*                for project WC1768 are disabled.               *
     H*                                                               *
     H* 07/22/10 C:257 SUPPORT: SEQ70743                              *
     H*                CHANGE KEYFIELD OF KEY POKY FROM EPEAPR  TO    *
     H*                EPAAPR                                         *
     H*                                                               *
     H* 07/06/10 C:190 PROJECT: WC1899                                *
     H*                Add the DE rate split date of 10/01/2008 to    *
     H*                the array used for calculating terrorism       *
     H*                charges in order to split the terrorism        *
     H*                charge for that rate split. Additionally,      *
     H*                populate the new fields in the premium         *
     H*                periods file for the assessment and terr rates.*
     H*                                                               *
     A* 06/24/10 C:227 Project: WC1873                                *
     A*              - Add processing for "Incl terr charges in assmnt*
     A*                calc" for all seven assessment calculations.   *
     A*                                                               *
     H* 06/21/10 C:255 PROJECT: 114WVY                                *
     H*                Add changes in program for 2nd period deviation*
     H*                when deviations are split. Populate 2nd period *
     H*                deviations information in Employer History file*
     H*                as it is for 1st period deviations             *
     H*                                                               *
     H* 05/13/10 C:65  PROJECT: Supp#72690                            *
     H*                Change the net rate calculation for TASB to    *
     H*                apply the net payroll rate against the payroll *
     H*                to determine the net premium.  This is to      *
     H*                avoid rounding differences between net rate    *
     H*                and net premium on very large payrolls.  The   *
     H*                coding to do so is added during the regular    *
     H*                payroll calculation to avoid duplicating all   *
     H*                of the payroll coding, controlled by XXNETF.   *
     H*                                                               *
     H* 05/07/10 C:136 PROJECT: WC1861 PRISS 5                        *
     H*                REQUIREMENTS CHANGED FOR PROJECT               *
     H*                                                               *
     H* 03/12/10 C:235 PROJECT: 102WFC                                *
     H*                ADDITION CLASSES FOR LOGGING CALC              *
     H*                                                               *
     H* 02/26/10 C:136 SUPPORT: SEQ70340                              *
     H*                CORRECT NET RATE CALC FOR SPLIT MOD POLICIES   *
     H*                                                               *
     H* 02/25/10 C:65  PROJECT: 25WRC                                 *
     H*                Remove all changes related to EARS and net     *
     H*                net premium as none of this was intended for   *
     H*                base design.                                   *
     H*                                                               *
     H* 02/25/10 C:33  Project: 25WRC  Priss #13                      *
     H*                Process Retro independently of Large Deductible*
     H*                                                               *
     H* 01/13/10 C:136 PROJECT: WC1838                                *
     H*                SKIP POLICIES WITH LARGE DEDUCTIBLE WHEN       *
     H*                SUBMITTING USER DOES NOT HAVE AUTHORITY TO     *
     H*                RUN
     H*                                                               *
     H* 01/12/10 C:65  PROJECT: WC1838                                *
     H*                LARGE DEDUCTIBLES - USE NEW FILE FOR LARGE     *
     H*                DEDUCTIBLE POLICIES, BASED ON BILLING SCHED $D.*
     H*                SET XXPCT TO SIZE OF LDPCT1, WHICH IS 12.9.    *
     H*                                                               *
     H* 01/14/10 C:191 PROJECT: 25WRC                                 *
     H*                BILLING ALOGRITHM RETROFIT                     *
     H*                - For CLT025,  use stabilization factor instead*
     H*                  of % field when calculating deviation #2     *
     H*                                                               *
     H* 12/29/09 C:136 PROJECT: WC1861                                *
     H*                ADD AZ RATE SPLIT - 01/01/2010                 *
     H*                                                               *
     H* 11/23/09 C:136 PROJECT: 139WNR                                *
     H*                FL SPLIT RATE 07/01/09 (CLT SPECIFIC)          *
     H*                                                               *
     H* 11/19/09 C:33  Project: 25WQI                                 *
     H*               -Prior to deleting A/R, write the information   *
     H*                 to the A/R Holding file                       *
     H*               -For 025, load the invoice # when writing       *
     H*                 billing records                               *
     H*                                                               *
     H* 11/16/09 C:136 PROJECT: 139WNK                                *
     H*               -ADD 'X' AND 'Y' PREMIUM LEVELS TO THE SAFETY   *
     H*                FEE CALCULATON.                                *
     H*               -DO NOT PRORATE SAFETY FEE PER GW               *
     H*                                                               *
     H* 08/14/09 C:189 SUPPORT: 69357                                 *
     H*                ARRAY INDEX GOING OUT OF RANGE                 *
     H*                                                               *
     H* 06/17/09 C:136 PROJECT: WC1834                                *
     H*                07/01/09 FL RATE CHANGE                        *
     H*                                                               *
     H* 06/11/09  C:65  PROJECT: SUPPORT   (93WHW PRISS 32)           *
     H*                 DO NOT CLEAR OUT THE A/R CODES FOR ASSESSMENTS*
     H*                 TO ENSURE THEY BILL PROPERLY FOR MULTI-DIV    *
     H*                 POLICIES.                                     *
     H*                                                               *
     H* 04/23/09 C:136 SUPPORT: WA114 68176                           *
     H*                CORRECT SECOND DEDUCTIBLE CREDIT CALCULATION   *
     H*                CHANGE IN WBILLCSPEC                           *
     H*                                                               *
     H* 01/08/09 C:65  Project: WC1769                                *
     H*                ADD FURTHER CHANGES FOR 6TH/7TH ASSESSMENTS.   *
     H*                                                               *
     H* 12/23/08 C:190 PROJECT: WC1754                                *
     H*                MA EXPENSE CONSTANT REVISION.                  *
     H*                                                               *
     H* 12/16/08 C:136 PROJECT: WC1768                                *
     H*                DE RATE SPLIT FOR 10/01/08                     *
     H*                                                               *
     H* 12/11/08 C:33  Project; WC1769                                *
     H*                Add 6th and 7th assessments.                   *
     H*                                                               *
     H* 10/30/08 C:65  PROJECT:  WC1753                               *
     H*                Added processing to decrease the manual        *
     H*                premium figure if the policy jurisdiction      *
     H*                state is NY and the policy is unexpired as     *
     H*                of 10/01/07.                                   *
     H*                                                               *
     H* 10/15/08 C:65  SUPPORT: SUPP# 65598 (WA045 67220)             *
     H*                POPULATE DEVIATION #1 AMOUNT FOR STD PREMIUM   *
     H*                AND PREVENT IT FROM BEING APPLIED TWICE AT     *
     H*                DISCOUNTED PREMIUM.   (WBILLCSPEC CHANGE)      *
     H*                                                               *
     H* 10/01/08 C:136 SUPPORT: WA146 65394                           *
     H*                FIX TRANSFER DEPOSIT LOGIC FOR PER YEAR        *
     H*                DEPOSIT MODE POLICIES                          *
     H*                                                               *
     H* 09/15/08 C:189 PROJECT: WC1644                                *
     H*                Add an Option to exclude terrorism from LCM    *
     H*                                                               *
     H* 09/04/08 C:062 PROJECT: WC1611                                *
     H*                LOAD ELNWRW INTO EHNWRW                        *
     H*                                                               *
     H* 09/03/08 C:136 SUPPORT: WA146 65004                           *
     H*                EXPAND DEPOSIT TYPE ARRAYS TO PREVENT          *
     H*                OOB ERROR                                      *
     H*                                                               *
     H* 08/21/08 C:191 Project: 102WEP                                *
     H*                Back to base - Client flagged for              *
     H*                 - Minimum manual premium                      *
     H*                 - Exclusion flag for min premium              *
     H*                 - Apply $2,500 min prem regardless of st      *
     H*                 - Add Expense constant on top of minimum      *
     H*                 - Calc Terrorism for Logging Classes          *
     H*                                                               *
     H* 08/21/08 C:191 Project: 102WEP                                *
     H*                Back to base - Client flagged for              *
     H*                 - Minimum manual premium                      *
     H*                 - Exclusion flag for min premium              *
     H*                 - Apply $2,500 min prem regardless of st      *
     H*                 - Add Expense constant on top of minimum      *
     H*                 - Calc Terrorism for Logging Classes          *
     H*                                                               *
     H* 06/20/08 C:183 PROJECT: 93WGX PRISS# 14                       *
     H*                Update statement required for WDELP            *
     H*                update for Business Category Assignment.       *
     H*                                                               *
     H* 06/20/08 C:136 SUPPORT: WA045 63964                           *
     H*                REVERSE OUT MEMBERSHIP AND SAFETY FEES IF      *
     H*                POLICY IS FLAT CANCELLED                       *
     H*                                                               *
     H* 04/29/08 C:136 SUPPORT: WA109 63151                           *
     H*                LOSING STATE TAX IN SUMMARZIED A/R. RESET      *
     H*                WDRRP AFTER ASGNBC
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 04/01/08 C:183 PROJECT: WC1619                                *
     H*                Call SREFFD to set the effective date to be    *
     H*                used on SETGT.                                 *
     H*                                                               *
     H* 02/12/08 C:136 SUPPORT: WA045 67220                           *
     H*                REMOVE 45WBT FROM WBILLCSPEC                   *
     H*                                                               *
     H* 02/06/08 C:65 PROJECT: SUPP#62205/WC995                       *
     H*               ENSURE THE EARLIEST RATING DATE FOR PA POLICIES *
     H*               IS BEING USED.                                  *
     H*                                                               *
     H* 01/22/08  C:65  PROJECT: WC1627                               *
     H*                 ADD PROCESSING TO CALCULATE THE NET RATE      *
     F*                 AFTER NORMAL PREMIUM HAS BEEN DETERMINED AND  *
     H*                 THEN APPLY THE NET RATE FACTOR TO THE PAY-    *
     H*                 ROLLS.  NET RATE AND NET PREMIUM WILL THEN BE *
     H*                 UPDATED IN WMEPP AND NORMAL PREMIUM REPLACED  *
     H*                 WITH A TOTAL OF THE NET PAYROLL PREMIUMS.     *
     H*                                                               *
     H* 01/11/08 C:61  PROJECT WC1610                                 *
     H*                Additional changes. If an assessment is to be  *
     H*                calculated and the "apply to discount on       *
     H*                deductible policy" flag is set 'Y' and XXDEDF  *
     H*                is set 'Y', then set assessment premium amount *
     H*                to XXDIST + XXD2ST.  Do not add in the         *
     H*                terrorism amounts.  Apply XXFPCT factor to     *
     H*                split EL1APR into EL1FPR.                      *
     H*                                                               *
     H*                After minimum premium has been invoked and the *
     H*                various fields cleared for an additional       *
     H*                division, do not reset the assessment premium  *
     H*                fields equal to XSHIGH if the new "apply to    *
     H*                deductible flag" is set to 'Y' and XXDEDF      *
     H*                is equal 'Y'.                                  *
     H*                                                               *
     H* 01/08/08 C:65  PROJECT: SUPP#61542       (WBILLCSPEC CHG)     *
     H*                MOVE THE SETTING OF A1E, A2E, ETC ARRAYS TO    *
     H*                PROPER LOCATION AFTER MODIFIED PREMIUM.        *
     H*                                                               *
     H* 12/17/07 C:189 PROJECT 109WEN                                 *
     H*                Rating Deviation calculation Renewal credit    *
     H*                                                               *
     H* 11/22/07 C:65  PROJECT: WC1104/SUPP#61220                     *
     H*                CORRECT DEVIATIONS APPLIED TO MANUAL TO STORE  *
     H*                AND UPDATE PREMIUM ARRAYS CORRECTLY.           *
     H*                                                               *
     H* 11/04/07 C:61  PROJECT WC1610                                 *
     H*                Do not permit an assessment amount or premium  *
     H*                to be calculated if there isn't a deductible   *
     H*                discount code on the policy (ELDCDE = *blanks) *
     H*                and the 'Apply to deductible policy only' flag *
     H*                equal 'Y' for the particular assessment.       *
     H*                                                               *
     H* 10/11/07 C:190 SUPPORT WA099 55504                            *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE      *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.         *
     H*                                                               *
     H* 08/30/07 C:136 SUPPORT: WA045 59903                           *
     H*                WRITE MEMBERSHIP FEES EVEN IF NOT SET UP IN    *
     H*                GROUP.                                         *
     H*                                                               *
     H* 08/30/07 C:65  SUPPORT: #59895                                *
     H*                APPLY SUPP# 49203 TO SPLIT ASSESSMENT CODING   *
     H*                TO ADD DEDUCTIBLE DISCOUNT BACK INTO ASSESSMNT *
     H*                PREMIUM FOR PA.                                *
     H*                                                               *
     H* 08/22/07 C:65  PROJECT: 114WMF                                *
     H*                ADD CODING TO RECOGNIZE THE 'Y' VALUE FOR PEO  *
     H*                FLAG FOR CSI FOR SPREAD DISCOUNT.              *
     H*                                                               *
     H* 08/21/07 C:65  PROJECT:SUPP# 59495                            *
     H*                DO NOT CLEAR THE TOTAL FIELDS BETWEEN READS ON *
     H*                MASS AUDIT UNTIL THERE IS A POLICY CHANGE.     *
     H*                                                               *
     H* 08/10/07 C:65  PROJECT: WC1120/WC1250 (SUPP# 59504)           *
     H*                ADD THE TERRORISM CHARGES TO XXDIFF TO ENSURE  *
     H*                THEY ARE BILLED INTO A/R ON MINIMUM PREMIUM    *
     H*                DIVISIONS.                                     *
     H*                                                               *
     H* 07/24/07  C:65  PROJECT: SUPP#58868                           *
     H*                 IF ALL CLASS CODES DEVELOP 0 EXPOSURE ON A    *
     H*                 POLICY WITH FL BUSINESS THEN INVOKE USE THE   *
     H*                 HIGHEST MINIMUM FROM THE POLICY'S CLASS CODES *
     H*                 INSTEAD OF CLASS CODE 8810.                   *
     H*                                                               *
     H* 07/18/07 C:136 PROJECT: WC1491                                *
     H*                IMPLEMENT LOGIC TO ACCOUNT FOR 'R' AND 'T'     *
     H*                TYPES IN THE DEPOSIT CATEGORY                  *
     H*                                                               *
     H* 07/16/07 C:183 Project: 45WFO                                 *
     H*               -Remove all processing related to project 45WFN *
     H*                                                              *
     H* 06/29/07 C:136 SUPPORT: WA109 58921                           *
     H*                CORRECT CALUCATION OF CA FLAT RATE INCREASE    *
     H*                                                               *
     H* 05/17/07 C:189 SUPPORT: WA139 57605                           *
     H*                PREMIUM DISCOUNT NEEDS BE APPLIED              *
     H*                TO THE ARD                                     *
     H*                                                               *
     H*05/15/07 C:184 PROJECT WC1568                                  *
     H*               CHARGE ASSESSMENTS ON THE SAME BASIS AS PREMIUM *
     H*               INSTALLMENTS                                    *
     H*                                                               *
     H*                                                               *
     H* 05/14/07 C:136 SUPPORT: WA090 57999                           *
     H*                BILLING EXP CONSTANT TO ALL DIVISIONS NOT      *
     H*                WORKING BECAUSE THE PREM HISTORY LOOP          *
     H*                IS READING PREM HISTORY RECORDS THAT HAVE      *
     H*                NOT BEEN COMPLETED BY SRINPR.  THESE           *
     H*                RECORDS MUST BE EXCLUDED FROM THE EXP          *
     H*                CONSTANT PROCESSING.                           *
     H*                                                               *
     H* 05/07/07  C:189 PROJECT: SUPP# 58032                          *
     H*                 DIFFERENCE COMING IN CALCULATED DISCOUNT      *
     H*                 AND  STOCK DISCOUNT                           *
     H*                                                               *
     H* 05/02/07  C:189 PROJECT: SUPP# 53518                          *
     H*                 PRORATE THE EXPENSE CONSTANT WHEN A POLICY    *
     H*                 IS SHORT TERM                                 *
     H*                                                               *
     H* 04/13/07  C:180 SUPP WA139 52196
     H*                 FIX 1ST ASSMT AMT DEALING WITH DISCREPANCY
     H*                 WHEN RUNNING WITH/WITHOUT ANNIVERSARY DATE.
     H*                                                               *
     H* 04/06/07  C:65  PROJECT: WC1514                               *
     H*                 APPLY SHORT RATE PENALTY TO DIFFERENT PREMIUM *
     H*                 LEVELS AND STORE AMOUNT IN A NEW FIELD.       *
     H*                                                               *
     H* 03/31/07 C:132 PROJECT: 45WFN                                 *
     H*              - Back to Base on Audits-Compile for changes in  *
     H*                WCBILLSPEC for this project.                   *
     H*              - ADD detail for running revised vs non-revised  *
     H*                mass Audits (ADDED TO PROJECT)                 *
     H*                                                               *
     H* 03/22/07 C:132 PROJECT: 45WFN                                 *
     H*                Back to Base on Audits                         *
     H*                - Remove 45WDI verbiage                        *
     H*                - Implement 45WCE, 45WCB, 45WDB, 45WHB Mods    *
     H*                                                               *
     H* 12/22/06  C:65  PROJECT: SUPP# 56358                          *
     H*                 REMOVE WABLP RECORD IF ALL PAYROLLS ARE NOT   *
     H*                 AUDITED.                                      *
     H*
     H* 12/13/06  C:65  PROJECT: SUPP# 52035                          *
     H*                 ENSURE MINIMUM IS NOT DOUBLE PRORATED IN THE  *
     H*                 CASE OF A LA POLICY.                          *
     H*
     H* 11/27/06 C:15  Project: 139WFS                                *
     H*                Added code to process the new GIC PEO type 'G' *
     H*                the same as PEO type 'M'.                      *
     H*
     H* 11/13/06  C:65  PROJECT: 132WEW                               *
     H*                 ADDED CODE TO ENSURE REINSURANCE CHARGE       *
     H*                 EFF AND EXP DATES ARE WITHIN POLICY PERIOD.   *
     H*
     H* 11/01/06 C:180  PROJECT 114WMF - BACK TO BASE
     H*
     H*                 REF.   114WGH/114WBX                          *
     H*                 APPLY THE REAL PRORATA FACTOR TO MINIMUM PREM
     H*                 FIGURE IN THE CASE THAT THE OVERRIDE
     H*                 FLAG IS ON. Search for date 05/16/05.
     H*
     H*                 REF. 114WBX
     H*                 DO NOT OVERRIDE THE PRORATA FACTOR FOR THE
     H*                 EXPENSE CONSTANT. Search for date 06/15/01.
     H*
     H*                 Override the pro-rata factor if selected in
     H*                 the employer detail. Search for date 04/05/01.
     H*
     H*                 WHEN CHECKING FOR HIGHEST MINIMUM PREMIUM,
     H*                 CHECK IF THERE IS EXPOSURE IN THE CLASS CODE
     H*                 OR IF THE STATE REPORTING DIVISION IS NJ.
     H*                 Search for date 04/05/02.
     H*
     H*                 CHECK FOR THE STATE OF PA AS WELL FOR MIN
     H*                 PREM
     H*                 Search for date 05/24/02.
     H*
     H*                 NJ MINIMUM PREMIUM SHOULD ONLY BE PULLED FROM
     H*                 EST FIELD IF THE FIELD HAS BEEN POPULATED.
     H*                 Search for date 11/28/05.
     H*
     H*                 DO NOT PRORATE INCR LIMITS FOR NJ, EVEN ON
     H*                 POLICIES SHORT TERM FOR CONCURRENCY OR CXS.
     H*                 Search for date 08/18/05
     H*
     H*                 PULL ASSESSMENTS AS OF POLICY EFF DATE FOR NJ.
     H*                 Search for date 05/18/05.
     F*
     H*                 MINIMUM PREMIUM CANNOT CHANGE AT AUDIT TIME
     H*                 FOR NJ, THUS IF THE STATE REPORTING DIVISION
     H*                 IS NJ WHEN WE WILL PULL THE MINIMUM PREMIUM
     H*                 FIGURE FROM ELEMNA FIELD.
     H*                 Search for date 10/18/04.
     H*
     H*                 SUPPORT: WA114 53259
     H*                 IMPLEMENT DTEC INTO THE SPLIT ASSESSMENT
     H*                 ALGORITHM PER SABRINA CANTY
     H*                 Search for date 10/25/06.
     H*
     H*                 STORE ASSESSMENT AMOUNTS INTO BUCKETS FOR
     H*                 APPLYING MULTIPLE RATES ON SPLIT POLICIES.
     H*                 Search for date 04/16/02.                    *
     H*
     H*                 CHECK FLAG FOR ADDING TERRORISM CHARGE TO    *
     H*                 ASSESSMENT CALCULATION.                      *
     H*                 DELETE UNUSED CODE FOR CALCULATING BILLED    *
     H*                 ASSESSMENT AMOUNTS                           *
     H*                 ADD SIMILAR PROCESSING TO 114WDN             *
     H*                 Search for dates 05/02/05 and 11/05/03.
     H*
     H*                 POPULATE PREMIUM PERIODS FILE WDPPP FROM THE
     H*                 PREMIUM BUCKET ARRAYS.
     H*                 Search for dates 05/17/05 and 02/27/06.
     H*
     H*                 USE PRE-MINIMUM PREMIUM FIGURES FOR ASSESSMENTS
     H*                 IF THE RATING STATE IS NJ.
     H*                 Search for dates 10/09/02
     H*
     H* 10/02/06  C:33  Project: WC1196                               *
     H*                 Assign Business Categroy                      *
     H*
     H* 09/28/06  C:65  PROJECT: SUPP# 52035                          *
     H*                 ADD IN THE WAIVER CHARGE, REGARDLESS OF AMT   *
     H*                 OR MINIMUM, TO MINIMUM PREMIUM.               *
     H*                                                               *
     H* 09/21/06 C:65  PROJECT: 129WLL (IMPLEMENT 129WKB MA DIA)      *
     H*                ALLOW MA RULES TO APPLY TO ASSESSMENT PREM     *
     H*                CALCULATION.                                   *
     H*                ADD CODE TO INCLUDE/EXCLUDE THE TERRORISM      *
     H*                CHARGE IN THE ASSESSMENT CALCULATIONS.         *
     H*                                                               *
     H* 08/09/06 C:136 PROJECT: 136WBG                                *
     H*                ADD DEPOSIT BY POLICY YEAR MODE                *
     H*                                                               *
     H* 07/06/06  C:168 PROJECT: 132WEW                               *
     H*                 ADDED CODE TO FIND THE AUDITED PREMIUN FOR    *
     H*                 AIRCRAFT REINSURANCE                          *
     H*                                                               *
     H* 06/30/06 C:136 PROJECT: 133WCV                                *
     H*                REWORK HARBOR EXPOSURE MINIMUM WRITTEN BY      *
     H*                133WBS                                         *
     H*                                                               *
     H* 06/28/06  C:65  PROJECT 132WFS                                *
     H*                 ADD CODING TO POPULATE GOVERNING CLASS CODE   *
     H*                 IN ALL DIVISIONS.  ADDITIONALLY, POPULATE     *
     H*                 THE CODE ON A PER STATE BASIS.                *
     H*                                                               *
     H* 05/03/06  C:132 PROJECT 46WFL                                 *
     H*                 Compile to include the WBILLCSPEC chgs from   *
     H*                 Clt 046 MOD 46WEE to base. Moved the F1SDCD   *
     H*                 From WMF1PX to WMF1P file. Added chain to     *
     H*                 PBS01 to get BSSEFL before copy WBILLCSPEC.   *
     H*                                                               *
     H* 04/28/06  C:65  PROJECT 71WGH (REMOVE SUPP#43064)             *
     H*                 REMOVE SUPPORT CHANGE FOR CLIENT 71 TO APPLY  *
     H*                 DEVIATION #1 AND #2 TO SAME PREMIUM FIGURE    *
     H*                 WHEN APPLIED TO MODIFIED PREMIUM.             *
     H*                                                               *
     H*  04/10/06 C:65 PROJECT SUPP#51293                             *
     H*                CHANGE DEDUCTIBLE DISCOUNT TO PULL BASED ON    *
     H*                ANNIV RATING DATE INSTEAD OF POLICY EFF DATE.  *
     H*                                                               *
     H*  03/30/06 C:136 SUPPORT: WA139 52111                          *
     H*                 CORRECT LOGIC IN SRDSCT FOR CALCULATING       *
     H*                 DISCOUNT IN PEO'S                             *
     H*                                                               *
     H*  02/28/06 C:168  PROJECT:140WAO                               *
     H*                ADD CODE FOR AUDIT DISCOUNT REDUCTION, FLAGGED *
     H*                FOR CLIENT 140 ONLY.                           *
     H*                                                               *
     H*  02/27/06 C:136 SUPPORT: WA136 51626                          *
     H*                 ADD CODE TO POPULATE EMPLOYER PREMIUM PERIODS *
     H*                 FILE. NEEDED TO CORRECTLY COMPUTE NET RATE.   *
     H*                 THIS WILL IMPLEMENT PROJECT WC1298.           *
     H*                                                               *
     H*  01/24/06 C:65 PROJECT: 114WHO                                *
     H*                IF RUNNING AN AUDIT ON A RETRO RATED POLICY    *
     H*                WHICH HAS ALREADY RUN A SETTLEMENT, THEN DO    *
     H*                NOT ADJUST THE TOTAL PREMIUM BILLED.  INSTEAD  *
     H*                MATCH THE AUDIT ADJUSTMENT WITH A OFFSETTING   *
     H*                RETRO ADJUSTMENT AND THE SAME FOR EMPLR HISTORY*
     H*                                                               *
     H*  01/13/06 C:65 PROJECT: WC1327/71WGH                          *
     H*                ALLOW PROGRAM TO BE CALLED IN SINGLE DIVISION  *
     H*                MODE.  THIS MODE WILL ALLOW A BILL TO BE RUN   *
     H*                ON A SINGLE DIVISION ON A POLICY.              *
     H*                ENABLED FOR CLIENT 71 ONLY.                    *
     H*                                                               *
     H* 01/05/06 C:136 SUPPORT: WA136 50713                           *
     H*                CORRECT PARMS IN CALL TO SRLCM TO GET THE      *
     H*                CORRECT LCM                                    *
     H*                                                               *
     H* 12/23/05 C:136 PROJECT: 129WKB (MA DIA ASSESSMENT BREAKOUT)   *
     H*                ONLY DEFINITIONS REQUIRED FOR COMPILATION      *
     H*                WERE COPIED IN.  FCC IS STILL RUNNING MODIFIED *
     H*                AUDIT BILL.                                    *
     H*                                                               *
     H* 12/18/05 C:136 PROJECT: 136WBH                                *
     H*                CLIENT SPECIFIC CHANGES TO CREDIT/DEBIT FACTOR *
     H*                IN WBILLCSPEC                                  *
     H*                                                               *
     H* 12/16/05 C:33  Project: WC1411                                *
     H*                If the PEO group flag is set to 'M':           *
     H*                 -Only bill the expense constant to employer   *
     H*                  #1, if the FNPEOE flag is set to 'Y'.        *
     H*                 -only combine premium across all employers    *
     H*                  within the group, if the FNPEOA flag is set  *
     H*                  to 'Y'.                                      *
     H*                                                               *
     H* 11/14/05 C:65  PROJECT: SUPP# 49203/WC1222                    *
     H*                FOR NY, DO NOT SUBTRACT EXPENSE CONSTANT BACK  *
     H*                OUT OF THE ASSESSMENT PREMIUM BASIS.  THE      *
     H*                ASSESSMENT WHICH CALCULATES WITHOUT IT IS      *
     H*                BASED ON STANDARD LEVEL, WHICH HAS NO EXP CNST.*
     H*                                                               *
     H* 10/29/05 C:65  PROJECT: SUPP# 49203                           *
     H*                FOR NY, CALCULATE THE ASSESSMENT WITH STAT     *
     H*                CODE OF 9749 TO INCLUDE THE AMOUNT OF THE 0932 *
     H*                ASSESSMENT IN ITS PREMIUM BASIS. IN OTHER      *
     H*                WORDS THE NY FUND SURCHARGE IS BASED ON TOTAL  *
     H*                POLICY PREMIUM PLUS THE NY STATE ASSESSMENT.   *
     H*                                                               *
     H* 10/28/05 C:65  PROJECT: SUPP# 49203                           *
     H*                FOR PA ONLY, DEDUCTIBLE DISCOUNT WILL BE       *
     H*                CALCULATED OFF MANUAL PREMIUM + INCR LIMITS.   *
     H*                ADDITIONALLY, WHEN CALCULATING PA ASSESSMENTS  *
     H*                ADD THE DEDUCTIBLE DISCOUNT BACK INTO THE      *
     H*                ASSESSMENT PREMIUM INSTEAD OF CALCULATING      *
     H*                WITHOUT IT.                                    *
     H*                                                               *
     H* 10/27/05 C:136 PROJECT: WC1191                                *
     H*               -TRANSFER AUDIT CREDITS TO PREMIUM              *
     H*               -CORRECTED CODE TO INCLUDE 'V' AND 'T' TYPES    *
     H*                IN SRREC                                       *
     H*                                                               *
     H* 09/27/05 C:136 SUPPORT: WA133 49141                           *
     H*                CHANGES TO 133WBS. ADD F-CLASS MINIMUM ON      *
     H*                TOP OF OF SAFETY FEE MINIMUM.                  *
     H*                                                               *
     H* 09/16/05 C:65  PROJECT: SUPP# 49059                           *
     H*                PULL AUDITED STANDARD PREMIUM FOR STOCK DISC   *
     H*                IF DSSTAT = 'A' INCASE AUDITED STD PREM IS 0.  *
     H*                                                               *
     H* 08/10/05 C:136 PROJECT: WC1136                                *
     H*                ADD LOSS COST MULTIPLIER TO CALCULATION.       *
     H*                                                               *
     H* 07/20/05 C:65  PROJECT: SUPP# 47194                           *
     H*                DO NOT PRORATE THE WAIVER PORTION OF MINIMUM   *
     H*                PREMIUM TO MATCH THE ESTIMATED BILLING PROGRAM.*
     H*                                                               *
     H* 07/15/05 C:136 PROJECT: 133WBS                                *
     H*                CLIENT SPECIFIC - CHARGE $1000 MINIMUM FOR     *
     H*                F-CLASS EXPOSURE.                              *
     H*                BASE - ADD CODE FOR WAIVER MAXIMUM             *
     H*                                                               *
     H* 06/29/05 C:136 PROJECT: 133WBT (LA MIN PREM RULES)            *
     H*                DON'T PRORATE NORMAL PREM FIGURE BEFORE        *
     H*                ADDING INTO THE XXWKCM FIGURE.                 *
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
     H* 04/18/04 C:53 PROJECT: WC1279                                 *
     H*               IF THE GROUP IS A PEO MASTER POLICY GROUP THEN  *
     H*               WHEN DETERMINING EXPENSE CONSTANT AND STOCK     *
     H*               DISCOUNT, SERACH ACROSS THE ENTIRE GROUP.       *
     H*               ADDITIONALLY ONLY BILL EXP CNST TO EMPLR 1.     *
     H*                                                               *
     H* 04/08/05 C:136 PROJECT: WC1288 (SUMMARIZE SELF-REPORTING)     *
     H*                REVERSE DEPOSIT ACTIVITY FOR SUMMARIZED        *
     H*                DIVISIONS                                      *
     H*                                                               *
     H* 04/07/05       PROJECT: 126WDK                                *
     H*          C:33  PROJECT: 126WCD                                *
     H*                MULTIPLE DEDUCTIBLE DISCOUNTS PER YEAR.        *
     H*                                                               *
     H* 03/22/05 C:136 PROJECT: WC1222                                *
     H*                ADDITIONAL CHANGES FOR NY ASSESSMENT.          *
     H*               -DO NOT CLEAR ASSESSMENT PREMIUM FIELDS IN      *
     H*                OTHER DIVISIONS IF XXST = 'NY'                 *
     H*               -WHEN ADDING CHARGE AT PREMIUM LEVELS,          *
     H*                ADD CONDITION THAT IT BE THE STATE REPORTING   *
     H*                DIVISION                                       *
     H*               -WHEN ADDING CHARGE TO 'X' LEVEL, SUBTRACT OUT  *
     H*                EXP CONSTANT SINCE X1NET HAS THE EXP CONSTANT  *
     H*                ADDED IN.                                      *
     H*                                                               *
     H* 03/21/05 C:65 PROJECT: SUPP#45364                             *
     H*               PROCESS SRDSCT SUBROUTINE FOR ALL DIVISIONS TO  *
     H*               ALLOW PREMIUM ON ALL DIVISIONS TO BE INCLUDED   *
     H*               IN DISCOUNT CALCULATIONS.                       *
     H*                                                               *
     H* 03/03/05 C:65 PROJECT: WC1182/SUPP#45632                      *
     H*               CHANGE GOVERNING CLASS CODE CHECK TO GREATER    *
     H*               THAN OR EQUAL AS IT WAS MISSING VALID CLASSES   *
     H*               WITH A 0 PAYROLL.                               *
     H*                                                               *
     H* 01/07/05 C:136 PROJECT: WC1250                                *
     H*                ADD PROCESSING FOR DOMESTIC TERRORISM CHARGE   *
     H*                AS A PREMIUM LINE ITEM.                        *
     H*                                                               *
     H*  11/30/04 C:65 PROJECT: 132WBW                                *
     H*                USE ACCOUNT CURRENT INTERIM DAYS FOR ACCOUNT   *
     H*                CURRENT POLICIES.                              *
     H*                                                               *
     H* 12/08/04 C:65 PROJECT: WC1180/WC870/SUPP#44911                *
     H*               DO NOT EXCLUDE SCHEDULE DB/CR FROM KY TAX CALC  *
     H*               IF THEY ARE CHARGING TAX ON DEDUCTIBLES.        *
     H*                                                               *
     H* 11/23/04 C:65 PROJECT: SUPPORT#43064                          *
     H*               APPLY DEVIATION #2 AFTER DEV #1 ON MODIFIED     *
     H*               PREMIUM INSTEAD OF TO THE SAME FIGURE.          *
     H*                                                               *
     H* 11/11/04 C:65 PROJECT: SUPP#44123/WC995                       *
     H*               WHEN PULLING RATES FOR PA ON A MULTI-STATE      *
     H*               POLICY, USE THE EARLIEST EFFECTIVE DATE OF      *
     H*               THE PA DIVISIONS ONLY.                          *
     H*                                                               *
     H* 11/02/04 C:65  PROJECT: WC1037/SUPP#42699                     *
     H*                DO NOT PRORATE MINIMUM PREMIUM, INCR LIMITS    *
     H*                MINIMUM, OR EXPENSE CONSTANT IF THE NCCI CX    *
     H*                CODE FOR POLICY IS A '09' OR '10' AS WELL.     *
     H*                                                               *
     H* 10/11/04  C:65 PROJECT: WC1064                                *
     H*                RECOGNIZE TRANSFER CODES FOR INSTALLMENT FEES. *
     H*                                                               *
     H* 10/08/04 C:65 PROJECT: SUPP#43528                             *
     H*               REMOVE THE CX ON SAME DAY CHECK FOR INCREASED   *
     H*               LIMITS.  INCREASED LIMITS WILL CALC OVER ALL    *
     H*               DIVISIONS REGARDLESS TO ENSURE PROPER CALC.     *
     H*                                                               *
     H* 09/01/04 C:136 PROJECT: WC1237                                *
     H*                BILL VOLUNTARY LABOR CHARGE.                   *
     H*                                                               *
     H* 07/02/04 C:136 PROJECT: WC1227 (REF 109DL)                    *
     H*                Bill 4th and 5th assessments.                  *
     H*                                                               *
     H* 06/23/04 C:65 PROJECT: SUPP#42985                             *
     H*               UPDATE AUDIT STATUS FOR WDILP RECORD IF THE     *
     H*               POLICY HAS BEEN UPDATED.  THIS WAS NOT          *
     H*               HAPPENING WHEN THE INCR LIMITS WAS REMOVED.     *
     H*                                                               *
     H* 06/04/04 C:65 PROJECT: WC1087                                 *
     H*               CHECK FLAG FOR BILLING ASSESSMENTS TO DIVISION  *
     H*               WITH HIGHEST MINIMUM PREMIUM ON MINIMUM PREMIUM *
     H*               POLICIES.                                       *
     H*                                                               *
     H* 06/03/04 C:65 PROJECT: WC1180                                 *
     H*               CHECK THE BILL TAX ON DEDUCTIBLES FLAG FOR      *
     H*               INCLUDING DEDUCTIBLE DISCOUNT WITHIN TAX CALC.  *
     H*                                                               *
     H* 05/12/04 C:65 PROJECT: WC1064                                 *
     H*               TRANSFER INSTALLMENT FEES BACK TO PRIOR YEAR    *
     H*               ON A FLAT CANCELLATION.  ALSO ADD IN MISSING    *
     H*               CODE FROM 122'S VERSION FOR ADJUSTING A/R.      *
     H*                                                               *
     H* 04/30/04 C:65 PROJECT: WC1182                                 *
     H*               POPULATE GOVERNING CLASS CODE FIELD.            *
     H*                                                               *
     H* 04/26/04 C:136 PROJECT: WC1169                                *
     H*                Process occupational disease payroll classes.  *
     H*                                                               *
     H* 03/30/04 C:136 PROJECT: WC1222                                *
     H*                DO NOT INCLUDE EXPENSE CONSTANT IN THE MINI-   *
     H*                MUM PREMIUM FIGURE WHEN CALCULATING THE NY     *
     H*                STATE ASSESSMENT CHARGE.                       *
     H*                                                               *
     H* 03/25/04 C:65 PROJECT: SUPP#41803                             *
     H*               SET THE POLICY TO DATE FOR FLAT CX'S.           *
     H*                                                               *
     H* 02/26/04 C:65 PROJECT: SUPPORT                                *
     H*               POPULATE EXP MODIFIER FIELDS IN WDMPP, MOSTLY   *
     H*               FOR WCPOLS DATA REPORTING.                      *
     H*                                                               *
     H* 02/06/04 C:65 PROJECT: SUPPORT#39886/WC1120                   *
     H*               DO NOT USE THE ARD WHEN PULLING TERRORISM RATE  *
     H*               FOR NJ.  USE THE POLICY EFFECTIVE DATE ONLY.    *
     H*                                                               *
     H* 11/12/03 C:65  PROJECT: SUPP#37684                            *
     H*                WHEN CALCULATING SPREAD DISCOUNT, REMOVE THE   *
     H*                CODE WHICH CHECKS FOR IF A POLICY IS CX'D ON   *
     H*                THE SAME DAY FOR SPREADING THE DISCOUNT.       *
     H*                INSTEAD USE ESTIMATED PREMIUM WHEN THERE IS    *
     H*                NO AUDITED PREMIUM AVAILABLE FOR TOTAL PREMIUM.*
     H*                                                               *
     H* 11/05/03 C:65  PROJECT: WC1120                                *
     H*                CHECK FLAG FOR ADDING TERRORISM CHARGE TO      *
     H*                ASSESSMENT CALCULATION.                        *
     H*                                                               *
     H* 11/04/03 C:136 PROJECT: WC1161                                *
     H*                Add code for new A/R category field in WMEAP.  *
     H*                                                               *
     H* 10/10/03 C:65  PROJECT: SUPP/WC1123/WC1149                    *
     H*                ENSURE RATING DATE ARRAYS ARE NOT FILLED WITH  *
     H*                RATE SPLIT DATES WHEN THE SPLIT IS NOT VALID.  *
     H*                                                               *
     H* 10/07/03 C:65  PROJECT: WC1149                                *
     H*                SPLIT A POLICY PERIOD FOR FL RATE CHANGE EFF   *
     H*                10/1/3.  THIS WILL AFFECT ANY FL POLICY THAT   *
     H*                IS RATED BEFORE 10/1/3 AND UNEXPIRED AS OF     *
     H*                10/1/3.                                        *
     H*                                                               *
     H* 10/03/03 C:136 PROJECT: WC1151                                *
     H*                Use new premium transfer code from Group Detail*
     H*                to transfer premium to previous year if the    *
     H*                policy has a flat cancellation.                *
     H*                                                               *
     H* 09/26/03 C:136 Support: WA129 39086 (WC1106)                  *
     H*                Pull payroll rates from the payroll file if    *
     H*                the policy and payroll class are 'A' rated,    *
     H*                otherwise, pull the rate from the payroll table*
     H*                                                               *
     H* 09/24/03 C:65  PROJECT: SUPP# 39088                           *
     H*                WHEN CHECK FOR HIGHEST MINIMUM PREMIUM, CHECK  *
     H*                IF THERE IS EXPOSURE IN THE CLASS CODE OR IF   *
     H*                THE STATE REPORTING DIVISION IS NJ OR NY.      *
     H*                                                               *
     H* 09/22/03 C:65  PROJECT: WC1123/SUPP#39129                     *
     H*                SET UP ANOTHER RATING ARRAY FOR POLICY CHARGES.*
     H*                THESE CHARGES SHOULD NOT BE RATED AS OF THE    *
     H*                APRIL RATING DATE SPLIT.                       *
     H*                                                               *
     H* 08/19/03  C:65 PROJECT: SY048                                 *
     H*                POPULATE NEW MINIMUM PREMIUM AMOUNT FIELD WITH *
     H*                THE MINIMUM PREMIUM AMOUNT, REGARDLESS OF A    *
     H*                POLICY INVOKING MINIMUM PREMIUM.               *
     H*                                                               *
     H* 06/20/03 C:65 PROJECT SUPP#37653/CM045                        *
     H*               DO NOT PROCESS WAIVER CALC ON A FLAT CX.        *
     H*               COPIED IN BY C:136                              *
     H*                                                               *
     H* 06/10/03 C:65 PROJECT SUPP#37507/CM045                        *
     H*               CLEAR WAIVER AMT ARRAYS FOR EACH EMPLOYER.      *
     H*                                                               *
     H* 06/05/03 C:65 PROJECT SUPP#32863                              *
     H*               RESET POSTING DATE AT BEGINNING OF WRTAR SUBR   *
     H*               TO ENSURE PROPER POSTING DATE ON A/R SUMMARY.   *
     H*                                                               *
     H* 05/27/03 C:136 PROJECT: WC1089                                *
     H*                CALCULATE NJ 2ND INJURY FUND SURCHARGE ON THE  *
     H*                PRE-MINIMUM PREMIUM FIGURE ON POLICIES THAT    *
     H*                INVOKE MINIMUM PREMIUM.                        *
     H*                                                               *
     H* 05/22/03 C:65  PROJECT: WC1120                                *
     H*                APPLY ADDITIONAL RATE FOR TERRORISM CHARGE TO  *
     H*                NON PAYROLL BASED CLASSES, IF ENTERED.         *
     H*                                                               *
     H* 04/30/03 C:65  PROJECT: WC1123                                *
     H*                SPLIT A POLICY PERIOD FOR FL RATE CHANGE EFF   *
     H*                4/1/3 IF RATED BETWEEN 1/1/3 AND 3/31/3 AND    *
     H*                UNEXPIRED AS OF 4/1/3.                         *
     H*                                                               *
     H* 04/03/03 C:136 PROJECT: WC1113                                *
     H*                Add code to calculate NY ambulance payroll     *
     H*                class charge.                                  *
     H*                                                               *
     H* 03/06/03 C:65 PROJECT: WC1027                                 *
     H*               IF A POLICY IS SHORT TERM, PRORATE POPULATION   *
     H*               RATED PAYROLL'S CHARGE.                         *
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
     H* 01/06/03 C:65 PROJECT SUPP#35230                              *
     H*               MOVE THE PA EXPENSE CONSTANT CHECK UP BECAUSE   *
     H*               *IN60 IS BEING TURNED OFF BY OTHER PROCESSING.  *
     H*                                                               *
     H* 01/02/03 C:65 PROJECT WC1027/35226                            *
     H*               DO NOT ADJUST PREMIUM AMOUNTS AT AUDIT FOR      *
     H*               POPULATION RATED CLASS CODES.                   *
     H*                                                               *
     H* 11/25/02 C:65 PROJECT SUPP#34887                              *
     H*               REMOVE PROJECT WC847 WITH REGARDS TO 1ST        *
     H*               ASSESSMENT - IE, CALC WITH INCREASED LIMITS.    *
     H*               COPIED IN BY C:136                              *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT WC1099                                  *
     H*               IF A PAYROLL CLASS CODE HAS AN EXTENSION WITH   *
     H*               A STAT CODE AND AN ADDITIONAL LOAD AMOUNT THEN  *
     H*               ADD THE AMOUNT INTO MANUAL PREMIUM.             *
     H*                                                               *
     H* 11/14/02 C:65 PROJECT SUPP#33855                              *
     H*               LOAD ANNUAL EXPENSE CONSTANT INTO EMPLR HISTORY *
     H*                                                               *
     H* 11/07/02 C:65 PROJECT WC1055                                  *
     H*               BILL EXPENSE CONSTANT TO THE HIGHEST STATE IF   *
     H*               INDICATED.  IF MORE THAN ONE STATE HAS THE      *
     H*               HIGHEST THEN BILL TO THE STATE WITH MOST PREM.  *
     H*                                                               *
     H* 09/20/02 C:65 PROJECT: CM045/#33798                           *
     H*               BILL WAIVER SURCHARGE ON TOP OF MINIMUM PREM.   *
     H*               COPIED IN BY C:136                              *
     H*                                                               *
     H*  09/11/02  C:136 PROJECT: WC1064 (FROM WA121 34120)           *
     H*                  DELETE INSTALLMENT FEES IN THE FUTURE ON A   *
     H*                  CX AUDIT. COPIED FROM C122.BASE/W7720R       *
     H*                                                               *
     H* 09/04/02 C:65 PROJECT: SUPP#34079                             *
     H*               ADD DIVIDE BY ZERO CHECK TO NEW INCR LIMITS     *
     H*               CODE WHEN DETERMINING MIN INCR LIMITS.          *
     H*                                                               *
     H* 08/12/02 C:65 PROJECT: SUPP#31223                             *
     H*               ONLY SET THE NEW/RENEWAL FLAG TO NEW IF THE     *
     H*               POLICY YEAR IS THE EARLIEST ON THE POLICY.      *
     H*                                                               *
     H* 07/29/02 C:65 PROJECT: SUPP#33400                             *
     H*               CLEAR INCR LIMITS MINIMUM DATA FIELDS.          *
     H*               COPIED IN BY C:136                              *
     H*                                                               *
     H* 06/25/02 C:132 SUPPORT#: WA071 33078                          *
     H*          ADD TO UPDATE FOR DATE, TIME, USER FOR WDELP FILE    *
     H*                                                               *
     H* 06/19/02 C:65  PROJECT: WC1059                                *
     H*                CHANGE THE FILE USED FOR STORING THE STANDARD  *
     H*                PREMIUM TO WDMPP IN ORDER TO AVOID CONFLICTS   *
     H*                WITH THE STOCK DISCOUNT PROCESSING.            *
     H*                                                               *
     H* 06/07/02 C:65 PROJECT SUPPORT                                 *
     H*               WHEN DETERMINING EXPENSE CONSTANT AND STATES,   *
     H*               IF NO 'E' WDEHP RECORDS EXIST, CHECK FOR ANY    *
     H*               'R' RECORDS THAT WERE WRITTEN WITHIN THE SAME   *
     H*               MONTH AS THE FIRST REVISED BILL OR UP TO THE    *
     H*               END OF THE FIRST MONTH OF THE POLICY.  IF NO    *
     H*               'R' RECORDS EXIST THEN USE ALL DIVISIONS.       *
     H*               CLEAR THE WDEHP FILE BEFORE WRITING THE HISTORY *
     H*               RECORD AS WELL TO PRESERVE DATA INTEGRITY.      *
     H*                                                               *
     H* 05/08/02 C:65  PROJECT: WC1037                                *
     H*                DO NOT PRORATE MINIMUM PREMIUM, INCR LIMITS    *
     H*                MINIMUM, OR EXPENSE CONSTANT IF THE NCCI CX    *
     H*                CODE FOR POLICY IS A '03' OR '08'. ALSO,       *
     H*                PRORATE INCR LIMITS MINIMUM IF THE ACCOUNT     *
     H*                IS SHORT TERM FOR CONCURRENCY.  FINALLY, DO    *
     H*                NOT PRORATE INCR LIMITS MINIMUM FOR THE AMOUNT *
     H*                OF TIME IT IS EFFECTIVE DURING THE POLICY YEAR.*
     H*                                                               *
     H* 05/02/02 C:65 PROJECT: SUPP#32306                             *
     H*               DO NOT SKIP DISCOUNT CALC IF NO EST INFO.       *
     H*                                                               *
     H* 04/25/02 C:65 PROJECT SUPPORT                                 *
     H*               WHEN CHECKING FOR THE HIGHEST EXPENSE CONSTANT, *
     H*               ONLY CHECK THE STATES WHICH WERE ON THE POLICY  *
     H*               WHEN IT WAS ORIGINALLY WRITTEN.                 *
     H*                                                               *
     H* 04/05/02 C:65  PROJECT: WC1079                                *
     H*                WHEN CHECK FOR HIGHEST MINIMUM PREMIUM, CHECK  *
     H*                IF THERE IS EXPOSURE IN THE CLASS CODE OR IF   *
     H*                THE STATE REPORTING DIVISION IS NJ.            *
     H*                                                               *
     H* 03/19/02 C:65 PROJECT SUPPORT                                 *
     H*               CLEAR OUT PREMIUM FIELDS FOR FEES IF AN ACCOUNT *
     H*               INVOKES MINIMUM PREMIUM ON THE STATE RPT DIV.   *
     H*               MOVE THE CALCULATIONS FOR ASSESSMENTS AND       *
     H*               SAFETY FEES SO THEY CAN BE ACCURATE FOR DIV'S   *
     H*               WHICH ZERO OUT PREMIUM ON INVOKING MINIMUM.     *
     H*                                                               *
     H* 03/14/02 C:65 PROJECT WC1031                                  *
     H*               ADD AUDITED DISCOUNT FIELDS FOR CAPTURED INFO.  *
     H*                                                               *
     H* 02/07/02 C:65 PROJECT WC1067                                  *
     H*               BREAK OUT EXPENSE CONSTANT ON A MIN PREM POLICY *
     H*               IN A/R IF INDICATED.                            *
     H*                                                               *
     H* 01/28/02 C:65 PROJECT WC1031                                  *
     H*               CAPTURE PREMIUM INFO BEFORE MIN PREM IN WDMPP.  *
     H*                                                               *
     H* 01/17/02 C:65 PROJECT: SUPP#29875                             *
     H*               DO NOT PULL INCR LIMITS FROM FLAT CX DIVS.      *
     H*                                                               *
     H* 12/18/01  C:65  PROJECT WC1056                                *
     H*                 TRANSFER FEE PAYMENTS BACK TO THE PRIOR YEAR  *
     H*                 ON A FLAT CX AUDIT IF CODES ARE ENTERED.      *
     H*                                                               *
     H* 12/13/01 C:65 PROJECT: SUPP# 30217                            *
     H*               IF A DIVISION IS CX'D AND THE DISCOUNT IS TO BE *
     H*               SPREAD, HOWEVER THE CX DATE DOES NOT MATCH ON   *
     H*               EVERY DIVISION, THEN USE THE AUDIT CALCULATION  *
     H*               WHEN THERE IS NO ESTIMATED INFORMATION.         *
     H*                                                               *
     H* 11/27/01  C:65  PROJECT WC1059                                *
     H*                 WHEN BILLING EXPENSE CONSTANT, CHECK TO SEE   *
     H*                 IF THE STANDARD PREMIUM IS LESS THAN AMOUNT   *
     H*                 ENTERED ON THE EXP CONSTANT RECORD AND IF SO  *
     H*                 THEN BILL THE REDUCED EXPENSE CONSTANT AMT.   *
     H*                                                               *
     H* 11/20/01 C:65 PROJECT: SUPP# 29938                            *
     H*               DO NOT CONDITION THE EMPLR DETAIL FEE FIELDS    *
     H*               BEING UPDATED ON A CALC, UPD, & PRT RUN.        *
     H*                                                               *
     H* 11/26/01 C:136  PROJECT: SUPPORT 29681                        *
     H*                 ENSURE THAT RECORDS WRITTEN TO WMEAP HAVE     *
     H*                 A UNIQUE TIMESTAMP.                           *
     H*                                                               *
     H* 10/05/01  C:65  PROJECT WC1030                                *
     H*                 CHECK FOR PREVIOUS EMPLR# FOR NEW/RENEWAL.    *
     H*                                                               *
     H* 10/04/01  C:65  PROJECT WC1043                                *
     H*                 APPLY CONTRACTORS CREDIT IMMEDIATELY AFTER    *
     H*                 EXPERIENCE MODIFIER, INSTEAD OF JUST BEFORE   *
     H*                 STANDARD PREMIUM.                             *
     H*                                                               *
     H* 10/04/01  C:65  PROJECT WC705                                 *
     H*                 APPLY INCR LIMITS AND DEDUC DISC TO MANUAL    *
     H*                 PREMIUM WITHOUT WAIVER SURCHARGE.             *
     H*                                                               *
     H* 10/01/01  C:65  PROJECT WC1045                                *
     H*                 CHECK LOSS CONSTANT AMOUNT TO SEE IF IT       *
     H*                 SHOULD BE APPLIED.  A LOSS CONSTANT IS ONLY   *
     H*                 APPLIED IF A STATE'S PREMIUM IS LESS THAN     *
     H*                 THE LOST CONSTANT MINIMUM.  IF APPLIED, DO    *
     H*                 NOT EXCEED THE MINIMUM AMOUNT AS WELL.        *
     H*                                                               *
     H* 09/24/01  C:65  PROJECT WC1035                                *
     H*                 EXPAND PAYROLL RATE FIELD BY 2 DIGITS.        *
     H*                                                               *
     H* 08/21/01 C:65 PROJECT: SUPP#28643                             *
     H*               PULL ASSESSMENTS PER THE ANNIV DATE IF APPLIC.  *
     H*                                                               *
     H* 08/06/01 C:65 PROJECT: SUPP# 27676                            *
     H*               CORRECT A/R ADJUSTMENT CODE FOR A/R SUMMARY.    *
     H*                                                               *
     H* 07/30/01 C:65 PROJECT: SUPP# 28374                            *
     H*               DO NOT TRANSFER PREMIUM TO A PRIOR YEAR ON A    *
     H*               FLAT CANCEL IF THE PRIOR YEAR HAS NEVER BEEN    *
     H*               BILLED ITSELF.                                  *
     H*                                                               *
     H* 06/18/01 C:65 PROJECT: SUPP# 27878                            *
     H*               RESET THE POSTING DATE BEFORE RUNNING EACH DIV. *
     H*                                                               *
     H* 04/27/01 C:65 PROJECT: SUPPORT                                *
     H*               IF AN ACCOUNT IS MULTI-STATE AND ASSESSMENTS    *
     H*               ARE BEING APPLIED TO PA AT AN ANNUAL LEVEL, DO  *
     H*               NOT INCLUDE EXPENSE CONSTANT IF THE EXPENSE     *
     H*               CONSTANT IS PULLED FROM ANOTHER STATE.          *
     H*                                                               *
     H*  04/26/01  C:65  PROJECT: SUPP#27253                          *
     H*                  CORRECT CODE FOR 2ND ASSESSMENT BEING CLEARED*
     H*                                                               *
     H*  04/04/01  C:65  PROJECT: WC1030                              *
     H*                  MARK EMPLR HISTORY RECORDS AS NEW/RENEWAL.   *
     H*                                                               *
     H*  03/26/01  C:65  PROJECT: WC938                               *
     H*                  DO NOT EXCLUDE INDEPENDENT STATES FROM THE   *
     H*                  DISCOUNT CALCULATIONS OF THE NORMAL STATES.  *
     H*                                                               *
     H* 03/14/01 C:65 PROJECT: WC1027                                 *
     H*               ACCOUNT FOR VOLUNTEER FIREMEN CLASS CODES.      *
     H*               THESE WILL BE AN 'O' TYPE THAT WILL REQUIRE     *
     H*               A LOOKUP OF WTPOP TO CALCULATE PREMIUM BASED    *
     H*               ON THE PAYROLL'S POPULATION.                    *
     H*                                                               *
     H* 02/05/01  C:65  PROJECT 109CQ                                 *
     H*                 ADD 'X' OPTION FOR APPLYING ASSESSMENTS.      *
     H*                 ALSO ADD FLAG TO INCLUDE EXP CNST ON DEP TAX. *
     H*                                                               *
     H*  02/08/01  C:65  PROJECT: SUPP#26066                          *
     H*                  CHECK FOR BALANCES EVEN IF THERE IS NO       *
     H*                  FEE FOR THAT DIVISION.                       *
     H*                                                               *
     H*  01/11/01  C:65  PROJECT: SUPP#25772                          *
     H*                  ONLY PULL 8810 MINIMUM IF THERE IS NO        *
     H*                  PREMIUM ON THE ENTIRE POLICY.                *
     H*                                                               *
     H* 01/03/01 C:65  PROJECT: WC1024                                *
     H*                BILL 3RD ASSESSMENTS WHEN APPLICABLE.          *
     H*                                                               *
     H*  08/24/00 C:65 PROJECT: SUPPORT#24109                         *
     H*                CORRECT CODE WHICH CHECKS FOR CANCELLATION     *
     H*                DATE ADDED FOR SUPP# 15050 ON 12/23/98.        *
     H*                                                               *
     H*  06/30/00 C:65 PROJECT: SUPPORT                               *
     H*                DO NOT PULL MIN PREMIUM OFF ANY CLASS CODES    *
     H*                THAT HAVE 0 PAYROLL.                           *
     H*                                                               *
     H*  05/19/00 C:65 PROJECT: WC995                                 *
     H*                PULL PAYROLL RATES BASED ON STATE RPT EFF DATE *
     H*                                                               *
     H*  05/01/00 C:65 PROJECT: SUPPORT                               *
     H*                CLEAR DATES FOR THE DISCOUNT TYPES.            *
     H*                                                               *
     H*  04/19/00 C:65 PROJECT: SUPPORT                               *
     H*                DO NOT SUBRACT 2/29 AS A LEAP DAY IF IT IS THE *
     H*                POLICY START DATE.                             *
     H*                                                               *
     H*  04/17/00 C:65 PROJECT: WC963                                 *
     H*                PRORATE MIN PREM IF SHORT TERM AND CONCURRENCY *
     H*                FLAG IS SET ON FOR THE EMPLOYER.               *
     H*                                                               *
     H*  04/10/00 C:65 PROJECT: WC986                                 *
     H*                PRORATE MIN INCR LIMITS ON CX AUDITS 1/1/2000  *
     H*                AND LATER, AND DO NOT PRORATE SHORT RT AUDITS. *
     H*                DO NOT INVOKE THESE CHANGES IF THE STATE IS MI.*
     H*                                                               *
     H* 03/13/00  C:65  PROJECT SUPPORT#22196                         *
     H*                 SAVE BILLED AMOUNTS FOR MEMBERSHIP AND SAFETY *
     H*                 FEES TO PROPERLY STORE ELMFAM AND ELSFAM.     *
     H*                                                               *
     H* 02/18/00  C:65  PROJECT SUPPORT#21929                         *
     H*                 DO NOT PRORATE PREMIUM FOR ASSESSMENTS.       *
     H*                                                               *
     H* 02/16/00  C:65  PROJECT SUPPORT#21843                         *
     H*                 DO NOT USE EXP MODS THAT ARE DATED PAST THE   *
     H*                 POLICY EXPIRATION DATE.                       *
     H*                                                               *
     H*  11/15/99 C:65 PROJECT: WC936                                 *
     H*                PROCESS ALL DIVISIONS ON A SINGLE POLICY RUN.  *
     H*                IF A POLICY IS SUMMARY A/R, THEN STORE ALL     *
     H*                A/R TO BE WRITTEN TO THE STATE RPT DIVISION    *
     H*                AFTER ALL DIVISIONS HAVE PROCESSED.            *
     H*                IF A MIDTERM CANCELLATION OF A DIVISION, RUN   *
     H*                THAT DIVISION ONLY.                            *
     H*                SRDIS2 WILL CALL THIS PROGRAM WITH A VALUE     *
     H*                OF 'S' IN ORDER TO REDUCE REDUNDENT CALCS.     *
     H*                                                               *
     H* 10/13/99  C:65  PROJECT WC942                                 *
     H*                 EXCLUDE FLAT CANCELLED DIVISIONS WHEN         *
     H*                 CHECKING FOR HIGHEST EXPENSE CONSTANT         *
     H*                                                               *
     H* 10/07/99  C:65  PROJECT WC937                                 *
     H*                 CALCULATE ASSESSMENT FOR BOTH NON-F CLASS     *
     H*                 AND F-CLASS EXPOSURE USING PROPER PERCENTAGES *
     H*                                                               *
     H* 09/29/99  C:65  PROJECT CM045                                 *
     H*                 CHANGE WAIVER PROCESS TO CHECK PREM FLAG AT   *
     H*                 THE EMPLOYER DETAIL LEVEL.                    *
     H*                                                               *
     H* 09/28/99  C:65  PROJECT WC938                                 *
     H*                 CHECK FOR DOMICILE STATES DURING STOCK DISC.  *
     H*                 IF A STATE IS SET TO 'Y' THEN INCLUDE ONLY    *
     H*                 THAT STATE'S PREMIUM FOR THE DISCOUNT. IF     *
     H*                 A STATE IS SET TO 'N', EXCLUDE ALL STATES     *
     H*                 SET TO 'Y' FOR THE DISCOUNT.                  *
     H*                                                               *
     H* 09/16/99 C:65  PROJECT: WC851                                 *
     H*                PULL THE SHORT-RATE/PRORATE FLAG FROM WDELP.   *
     H*                                                               *
     H* 08/12/99  C:65  PROJECT WC935                                 *
     H*                 ALLOW FOR 1ST EXP MOD TO BE ZERO, IN ORDER    *
     H*                 TO SKIP IT.                                   *
     H*                                                               *
     H* 08/12/99  C:65  PROJECT WC891                                 *
     H*                 BEFORE PRORATING EXPENSE CONSTANT ON SHORT    *
     H*                 TERM ACCOUNTS, CHECK NEW FLAG IF NOT CX'D.    *
     H*                                                               *
     H* 06/01/99  C:65  SUPPORT: CM045                                *
     H*                ADD WAIVER SURCHARGE TO PREMIUM CALCULATION    *
     H*                AND ALLOW ADDL LOAD TO APPLY AT J,E,S,D.       *
     H*                                                               *
     H* 04/12/99  C:65  PROJECT WC875                                 *
     H*                 TRANSFER STATE TAX ON DEPOSIT FOR CANCELS     *
     H*                                                               *
     H*  04/15/99  C:65  PROJECT: SUPPORT #17688                      *
     H*                  PAD XXKEY FOR THE MAIN AUDIT TRANSACTION.    *
     H*                                                               *
     H*  04/09/99  C:65  PROJECT: SUPPORT #17555                      *
     H*                  REVERSE ALL TAX BILLED IF MIN PREM IS INVK.  *
     H*                                                               *
     H*  03/30/99  C:65  PROJECT: SUPPORT #17399                      *
     H*                  ZERO OUT THE ADDITIONAL LOAD ARRAY.          *
     H*                                                               *
     H*  03/11/99  C:65  PROJECT: SUPPORT #17040                      *
     H*                  ROUND FEES BEFORE THEY ARE BEING BILLED.     *
     H*                                                               *
     H*  03/02/99  C:65  PROJECT: SUPPORT                             *
     H*                  REMOVE PROJECT MOD WC844.                    *
     H*                                                               *
     H*  02/12/99  C:65  PROJECT: SUPPORT #16607                      *
     H*                  BLANK THE LATE FLAG WHEN WRITING A/R.        *
     H*                                                               *
     H*  01/18/99  C:65  PROJECT: SUPPORT                             *
     H*                  ADD IN DELETED TAX AMOUNTS TO THE TAX ADJ.   *
     H*                                                               *
     H*  01/06/99  C:65  PROJECT: WC705                               *
     H*                  ALLOW SAFTEY FEE MINIMUM WITH NO PERCENTAGE. *
     H*                  CORRECT IF STATEMENTS.                       *
     H*                                                               *
     H* 12/23/98  C:09  PROJECT: SUPPORT/SEQ# 15050                   *
     H*                 TRANSFER SECURITY TO PREMIUM ON CANCELLATION  *
     H*                 AUDIT IF THE CANCELLATION DATE IS LESS THAN   *
     H*                 OR EQUAL TO THE POLICY TO DATE.               *
     H*                                                               *
     H* 12/21/98  C:65  PROJECT: SUPPORT                              *
     H*                 CHANGE SHORT RATE CALC TO USE FDAYS.          *
     H*                                                               *
     H* 12/10/98  C:65  PROJECT: SUPPORT                              *
     H*                 IF POST DATE IS LESS THAN BILLING START DATE  *
     H*                 THEN DEFAULT IT TO THE BILLING START DATE.    *
     H*                                                               *
     H* 12/03/98  C:65  PROJECT: SUPPORT                              *
     H*                 IF A DIVISION IS CANCELLED, ZERO OUT ITS      *
     H*                 ESTIMATED PREMIUM FIELDS FOR SPREAD DISC.     *
     H*                                                               *
     H* 11/24/98  C:65  PROJECT: SUPPORT                              *
     H*                 DO NOT ROUND STOCK DISCOUNT BEFORE USING.     *
     H*                                                               *
     H* 11/18/98  C:65  PROJECT: WC705                                *
     H*                 DEFAULT POLICY EXP DATE TO ONE YEAR IN FUTURE *
     H*                 IF THERE IS NO EMPLR HISTORY RECORD.          *
     H*                                                               *
     H* 11/04/98  C:65  PROJECT: WC705                                *
     H*                 POST ALL ADJUSTMENTS WITH THE DATE USED FOR   *
     H*                 THE MAIN AUDIT ADJUSTMENT.                    *
     H*                                                               *
     H* 10/27/98  C:65  PROJECT: WC705                                *
     H*                 CHANGE THE LOGICAL FOR 'A' TYPE SPREAD DISC.  *
     H*                                                               *
     H* 10/26/98  C:65  PROJECT: WC705                                *
     H*                 APPLY THE PRORATE FACTOR TO ALL ASSESSMENTS.  *
     H*                                                               *
     H* 10/20/98  C:65  PROJECT: WC847                                *
     H*                 CALCULATE 1ST ASSESSMENT APPLIED TO EXPERIENCE*
     H*                 FOR CT WITHOUT INCR LIMITS.                   *
     H*                                                               *
     H* 10/16/98  C:65  PROJECT: WC847                                *
     H*                 CALCULATE DEVIATION AND APPLIED TO EXPERIENCE *
     H*                 FOR CT WITHOUT INCR LIMITS.                   *
     H*                                                               *
     H* 10/13/98  C:65  PROJECT: SUPPORT/WC705                        *
     H*              1) WHEN DELETING A/R, WORK FROM THE POSTING      *
     H*                 DATE AND FORWARD.                             *
     H*              2) CHECK IF A/R CODES EXIST BEFORE USING THEM.   *
     H*                                                               *
     H* 10/05/98  C:65  PROJECT: WC870                                *
     H*                 DO NOT INCLUDE SCHD DB/CR FOR KY TAX.         *
     H*                                                               *
     H* 09/29/98  C:65  PROJECT: SUPPORT WA078/13992                  *
     H*                 EXPAND XXPCT TO 9.6 INSTEAD OF 7.4            *
     H*                                                               *
     H* 09/24/98  C:65  PROJECT: WC705                                *
     H*                 POST ANY DEBIT AUDIT ADJUSTMENTS USING THE    *
     H*                 INTERIM # OF DAYS, HOWEVER, ANY CREDIT AUDIT  *
     H*                 ADJ SHOULD BE POSTED TO THE CURRENT DATE.     *
     H*                 (REPLICATED THIS FOR ALL POSTINGS)            *
     H*                                                               *
     H* 09/23/98  C:65  PROJECT: SUPPORT WA071/13888                  *
     H*                 ROUND NORMAL PREMIUM BEFORE USING.            *
     H*                                                               *
     H* 09/11/98  C:65  PROJECT: WC705                                *
     H*                 SET ELPTDT INSTEAD OF DEFAULTING IT. (CANCEL) *
     H*                                                               *
     H* 09/09/98  C:09  PROJECT: WC705                                *
     H*                 POST ANY DEBIT AUDIT ADJUSTMENTS USING THE    *
     H*                 INTERIM # OF DAYS, HOWEVER, ANY CREDIT AUDIT  *
     H*                 ADJ SHOULD BE POSTED TO THE CURRENT DATE.     *
     H*                                                               *
     H* 08/25/98  C:65  PROJECT: WC705                                *
     H*                 WHEN CHECKING INCR LIMIT MIN, FIND HIGHEST    *
     H*                 ACROSS ALL DIVISIONS WITH AN INCR LIMIT.      *
     H*                                                               *
     H* 08/11/98  C:35  PROJECT: CM036                                *
     H*                 CLEAR PEH01 RECORD AFTER READPE SO DATA WILL  *
     H*                 NOT BE CARRIED FORWARD INTO AUDIT PEH01       *
     H*                 RECORD.                                       *
     H*                 LOAD ADDITIONAL FIELDS IN WDEHP TO BRING CODE *
     H*                 INTO SYNCH WITH HUMANA'S CURRENT VERSION.     *
     H*                                                               *
     H* 08/03/98  C:65  PROJECT: WC705                                *
     H*                 DEFAULT ELPTDT TO CANCEL DATE.                *
     H*                                                               *
     H* 07/31/98  C:65  PROJECT: 88WAJ                                *
     H*                 ADD BILL EXP CNST TO ALL DIVS FLAG.           *
     H*                                                               *
     H* 07/24/98  C:65  PROJECT: SUPPORT WA033/12819                  *
     H*                 CHECK FOR ZERO BEFORE DIVIDING.               *
     H*                                                               *
     H* 07/23/98  C:65  PROJECT: WC705                                *
     H*                 ROUND PREMIUM FIGURES BEFORE WRITING TO FILE. *
     H*                                                               *
     H* 07/20/98  C:65  PROJECT: WC844                                *
     H*                 CHECK FOR INDIANA ACCOUNTS EFFECTIVE DURING   *
     H*                 7/1/97 OR LATER FOR RATE ADJUSTMENT.          *
     H*                                                               *
     H* 07/17/98  C:65  PROJECT: WC847                                *
     H*                 CALCULATE DEVIATION AND 1ST ASSESSMENT        *
     H*                 APPLIED TO STANDARD FOR CT WITHOUT INCR LMT.  *
     H*                                                               *
     H* 07/15/98  C:65  PROJECT: WC705                                *
     H*                 CHECK MINIMUM INCR LIMIT ACROSS ALL DIVISIONS.*
     H*                                                               *
     H* 07/14/98  C:65  PROJECT: WC845                                *
     H*                 LOAD CALCULATED STOCK DISCOUNT FIELD FOR IREP *
     H*                                                               *
     H* 07/13/98  C:65  PROJECT: WC846                                *
     H*                 USE MODIFIED MANUAL FOR STOCK DISC CALC WHEN  *
     H*                 NECESSARY.                                    *
     H*                                                               *
     H* 06/11/98  C:65  PROJECT: WC705                                *
     H*                 CHECK FOR INACTIVE INCR LIMIT CODES.          *
     H*                                                               *
     H*  05/21/98  C:65  PROJECT: WA095/11714                         *
     H*                  USE TEMPORARY FIELD FOR PRORATION FACTOR.    *
     H*                                                               *
     H*  05/18/98  C:65  PROJECT: WC705                               *
     H*                  CHECK NEW FLAGS FOR ASSESSMENT CALCULATIONS. *
     H*                  TWO NEW FIELDS WILL SPECIFY THE PREMIUM      *
     H*                  AMOUNT TO BE USED IN W8011R AND W7615R TO    *
     H*                  WRITE/DISPLAY ADJUSTMENT RECORDS.            *
     H*                                                               *
     H*  05/18/98  C:65  PROJECT: WC705                               *
     H*                  CLEAR DISCOUNT ARRAY FOR EACH EMPLOYER.      *
     H*                                                               *
     H*  05/05/98  C:65  PROJECT: WC705                               *
     H*                  DONT CHANGE THE VALUE OF QQREV ITSELF.       *
     H*                                                               *
     H*  04/30/98  C:65  PROJECT: WC705                               *
     H*                  USE RRIDTP - INCLUDE DEV IN TAXABLE PREM.    *
     H*                                                               *
     H*  04/27/98  C:65  PROJECT: WC705                               *
     H*                  CHANGE REVISED PARM FOR CALC,PRT.            *
     H*                                                               *
     H*  04/17/98  C:65  PROJECT: WC705                               *
     H*                  FIX SPREAD DISCOUNT ROUTINE CALC.            *
     H*                  FIX DISCOUNT FILE KEY.                       *
     H*                                                               *
     H*  03/10/98  C:65  PROJECT: WC705                               *
     H*                  CHANGE ROUNDING TO USE HALF-ADJUST.          *
     H*                                                               *
     H*  02/20/98  C:65  PROJECT: WC705                               *
     H*                  ADD TEMP VAR XXDDCT FOR DEDUC DISC.          *
     H*                                                               *
     H*  02/20/98  C:65  PROJECT: WC705                               *
     H*                  ROUND PAYROLL PREMIUM BEFORE USE.            *
     H*                                                               *
     H*  02/19/98  C:65  PROJECT: WC705                               *
     H*                  ACCUMULATE PREMIUM ACROSS ALL DIVISIONS      *
     H*                  WHEN CHECKING FOR MINIMUM PREMIUM.           *
     H*                                                               *
     H*  01/23/98  C:87  PROJECT: CM033                               *
     H*                  DELETE FUTURE BILLING RECORDS INSTEAD OF     *
     H*                  REVERSING THEM                               *
     H*                                                               *
     H*  01/19/98  C:65  PROJECT: WC633                               *
     H*                  ADD 2ND INCR LIMITS TO EMP HISTORY FILE.     *
     H*                                                               *
     H*  01/01/98  C:65  PROJECT: WC705                               *
     H*                  ADD PROCESSING FOR ANNUALIZE PREM BEFORE     *
     H*                  STOCK DISCOUNT FLAG.                         *
     H*                                                               *
     H*  12/23/97  C:65  PROJECT: WC776                               *
     H*                  1ST AND 2ND ASSESSMENT.                      *
     H*                                                               *
     H*  12/02/97  C:65  PROJECT: WC702                               *
     H*                  FIND HIGHEST MIN PREMIUM ACROSS ALL DIV'S.   *
     H*                                                               *
     H*  12/02/97  C:65  PROJECT: V3                                  *
     H*                  PRORATE MIN DOWN TO 0 FOR FLAT CANCELLATION  *
     H*                                                               *
     H*  11/25/97  C:65  PROJECT: V3                                  *
     H*                  ADDED REV PARM FOR REVISED AUDIT FLAG.       *
     H*                                                               *
     H*  11/20/97  C:65  PROJECT: V3                                  *
     H*                  RESET XXEFDT BEFORE DISCOUNT SECTION.        *
     H*                                                               *
     H*  10/29/97  C:33  PROJECT: WC577                               *
     H*                  Recompiled due to fields being added to WMEPP*
     H*****************************************************************
     H*                                                               *
     H*                                                               *
     FWABLL     IF   E           K DISK
     FWABLL1    UF   E           K DISK
     F                                     RENAME(PBL01:PBL02)
     FWDELL1    UF   E           K DISK
     F                                     INFDS(ELRRDS)
     FWXELL     UF A E           K DISK    RENAME(PEL01:PXEL00) USROPN
     FWMUPL     IF   E           K DISK
     F* Files (WDRRL & WMRSL) found in copybook (WADTTRKFSR)
     FWMEML     IF   E           K DISK
     FWMFNL     IF   E           K DISK
     FWMEPL1    UF   E           K DISK
     FWMPRL1    IF   E           K DISK
     FWDF1L     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PRS01)
     FWTLML1    IF   E           K DISK
     FWMDSL1    IF   E           K DISK
     FWMDSL4    IF   E           K DISK
     F                                     RENAME(PDS01:PDS03)
     FWMEAL4    IF A E           K DISK
     FWMEAL8    UF   E           K DISK
     F                                     RENAME(PEA01:PEA02)
     FWMA#P     O  A E           K DISK
     F                                     RENAME(PEA01:PEA0#)
     FWDDSL     UF A E           K DISK
     F                                     RENAME(PDS01:PDS02)
     FWTDDL2    IF   E           K DISK
     FWMLDL1    UF A E           K DISK
     FWDF2L     IF   E           K DISK
     FWDILL     UF A E           K DISK
     fwmdvl     if   e           k disk    rename(pdv01:pdv00)
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
     FWDEHL     IF A E           K DISK
     FWDECL     IF   E           K DISK
     FWDHEL     IF   E           K DISK
     FWMSTL1    IF   E           K DISK
     FWDSTL     IF   E           K DISK
     F                                     RENAME(PST01:PST02)
     FWDMPL     UF A E           K DISK
     FWMWVL     IF   E           K DISK
     FWTPOL     IF   E           K DISK
     FWDSCL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
     F                                     RENAME(PEX01:PEX02)
     FWDEHL5    IF   E           K DISK
     F                                     RENAME(PEH01:PEH02)
     FWDCXL2    IF   E           K DISK
     FWTCRL     IF   E           K DISK
     FWMIFL     IF   E           K DISK
     FWMPXL1    IF   E           K DISK
     FWDPPL     UF A E           K DISK
     FWDEHL4    IF   E           K DISK
     FWASRL2    IF   E           K DISK
     F*
     FWMEPL     IF   E           K DISK
     F                                     RENAME(PEP01:PEP02)
     FWTGTL     IF   E           K DISK
     FWMRTL     IF   E           K DISK    rename(prt01:prt0x)
     F                                     prefix(rx:2)
     FWMHCL     IF   E           K DISK
     FWDHCL3    IF   E           K DISK    rename(PHC01:PHC02)
     FWMXAP     O    E             DISK    rename(pxa01:pxa0p)
     FWMCOL     IF   E           K DISK    RENAME(PCO01:PCO00) USROPN
     Fwxevl     IF A E           K DISK    RENAME(pev01:pev00)
     F/COPY WADTTRKFSR
     D*** Begin Add *** 01/05/24 ****************************************
     D/Copy siWebSvcPr
     D/copy sprRunCmd
     D*** End   Add *** 01/05/24 ****************************************
     D/COPY WADTTRKDSR
     D/COPY WBILLDSPEC
     d/copy wprDivPlan
     d/copy wprPmtSvc
     d/copy wprAR
     d*** Begin Add *** 03/05/25 *******************************************
     d/copy sprSQL
     d*** End   Add *** 03/05/25 *******************************************
     D*
     D*  THE FOLLOWING 10 ARRAYS ARE FOR SPLITTING ANNIV RATING AMTS
     D*  PDAT - DATES IN WHICH PAYROLLS TAKE ON DIFFERENT RATING
     D*         THERE CAN BE UP TO 3 DIFF RATING PERIODS
     D*  RDAT - CORRESPONDING RATES IN EFFECT FOR PAYROLLS AS OF PDAT
     D*  PRO  - PRORATA FOR EACH RATING PERIOD - NEED THIS IN ORDER TO
     D*         SPLIT EMP PAYROLLS (WMEPP) CORRECTLY.
     D*
     D*
     D PDAT            S              7  0 DIM(8) ASCEND                        PAYROLL DATES
     D RDAT            S              7  0 DIM(8) ASCEND                        RATING DATES
     D FDAT            S              7  0 DIM(8) ASCEND                        RATING DATES
     D PRO             S              5  4 DIM(8)                               PRORATA
      *** Beg Add ***** 11/13/24 *********************************************
     D PRO2            S              5  4 DIM(6)                               PRORATA
      *** End Add ***** 11/13/24 *********************************************
     D PAY             S             15  4 DIM(8)
     D PAY2            S             15  4 DIM(8)
     D MAN             S             15  4 DIM(8)                               MANUAL PREM
     D MANT            S             15  4 DIM(8)                               MANUAL PREM
     D MAN1            S             15  4 DIM(8)                               MANUAL PREM
     D MAN2            S             15  4 DIM(8)                               MANUAL PREM
     D MAN3            S             15  4 DIM(8)                               MANUAL PREM
     D MAN4            S             15  4 DIM(8)                               MANUAL PREM
     D MAN5            S             15  4 DIM(8)                               MANUAL PREM
     D MAN6            S             15  4 DIM(8)                               MANUAL PREM
     D MAN7            S             15  4 DIM(8)                               MANUAL PREM
     D MANSV           S             15  4 DIM(8)                               MANUAL PREM
     D LMT             S             11  0 DIM(8)                               INCR LIMIT
     D LM2T            S             11  0 DIM(8)                               2ND INCR LIMIT
     D LM1             S             11  0 DIM(8)                               INCR LIMIT
     D LM21            S             11  0 DIM(8)                               2ND INCR LIMIT
     D LM2A            S             11  0 DIM(8)                               INCR LIMIT
     D LM22            S             11  0 DIM(8)                               2ND INCR LIMIT
     D LM3             S             11  0 DIM(8)                               INCR LIMIT
     D LM23            S             11  0 DIM(8)                               2ND INCR LIMIT
     D LM4             S             11  0 DIM(8)                               INCR LIMIT
     D LM24            S             11  0 DIM(8)                               2ND INCR LIMIT
     D LM5             S             11  0 DIM(8)                               INCR LIMIT
     D LM25            S             11  0 DIM(8)                               2ND INCR LIMIT
     D LM6             S             11  0 DIM(8)                               INCR LIMIT
     D LM26            S             11  0 DIM(8)                               2ND INCR LIMIT
     D LM7             S             11  0 DIM(8)                               INCR LIMIT
     D LM27            S             11  0 DIM(8)                               2ND INCR LIMIT
      *** Beg Add ***** 08/14/24 ********************************************
     D LMB             S             11  0 DIM(6)                               INCR LIMIT BALANCE
     D LMB2            S             11  0 DIM(6)                               INCR LIMIT BALANCE 2
      *** End Add ***** 08/14/24 ********************************************
     D ADL             S             15  4 DIM(8)                               ADDITIONAL LOAD
     D ADL2            S             15  4 DIM(8)                               ADDITIONAL LOAD
     D ADL3            S             15  4 DIM(6)                               ADDITIONAL LOAD
     D LM              S             11  0 DIM(8)                               INCR LIMIT
     D LM2             S             11  0 DIM(8)                               2ND INCR LIMIT
     D LPR             S              5  4 DIM(8)                               LIMIT DAYS PRORATA
     D STN             S             15  4 DIM(8)                               STANDARD PREM
     D SVN             S             15  4 DIM(8)                               STANDARD PREM - SAVE
     D STNL            S             15  4 DIM(8)                               PREM W/O LC
     D SVNL            S             15  4 DIM(8)                               ARRAYS
     D WAVL            S             11  0 DIM(8)                               WAVIER CHARGE 1ST
     D PRM             S             15  4 DIM(8)                               PREMIUM ARRAY
     D DIS             S             15  4 DIM(8)                               DISCOUNT ARRAY
     D DYS             S              3  0 DIM(8)                               DAYS IN EACH PERIOD
     D DY3             S              3  0 DIM(8)                               15.7 CREDIT DAYS
     D SRP             S             15  4 DIM(8)                               SHORT RATE PENALTY
     D MTH             S              3  1 DIM(12) CTDATA PERRCD(1) ASCEND      MONTHS/HARBOR EXP %
     D HP              S              4  3 DIM(12) ALT(MTH)
     D EXP             S             15  4 DIM(8)                               MODIFIED PREMIUM
     D FYR             S              3  0 DIM(999)
     D TY              S              1    DIM(999)
     D AMT             S             15  4 DIM(999)
     D DESC            S             30    DIM(1) CTDATA PERRCD(1)
     D CLS             S              5  0 DIM(999)
     D AM2             S             11  2 DIM(999)
     D STNT            S             15  4 DIM(8)                               TAXABLE
     D SVNT            S             15  4 DIM(8)                               ARRAYS
     D EXPT            S             15  4 DIM(8)
     D STN1            S             15  4 DIM(8)                               1ST ASSESSMENT
     D SVN1            S             15  4 DIM(8)                               ARRAYS
     D EXP1            S             15  4 DIM(8)
     D STN2            S             15  4 DIM(8)                               2ND ASSESSMENT
     D SVN2            S             15  4 DIM(8)                               ARRAYS
     D EXP2            S             15  4 DIM(8)
     D STN3            S             15  4 DIM(8)                               3RD ASSESSMENT
     D SVN3            S             15  4 DIM(8)                               ARRAYS
     D EXP3            S             15  4 DIM(8)
     D WAV3            S             11  0 DIM(8)                               WAVIER CHARGE 1ST
     D STN4            S             15  4 DIM(8)                               4TH ASSESSMENT
     D SVN4            S             15  4 DIM(8)                               ARRAYS
     D EXP4            S             15  4 DIM(8)
     D WAV4            S             11  0 DIM(8)                               WAVIER CHARGE 4TH
     D STN5            S             15  4 DIM(8)                               5TH ASSESSMENT
     D SVN5            S             15  4 DIM(8)                               ARRAYS
     D EXP5            S             15  4 DIM(8)
     D WAV5            S             11  0 DIM(8)                               WAVIER CHARGE 5TH
     D S2N             S             15  4 DIM(6)
     D S2NL            S             15  4 DIM(6)
     D S2NT            S             15  4 DIM(6)
     D S2N1            S             15  4 DIM(6)
     D S2N2            S             15  4 DIM(6)
     D S2N3            S             15  4 DIM(6)
     D S2N4            S             15  4 DIM(6)
     D S2N5            S             15  4 DIM(6)
     D S2N6            S             15  4 DIM(6)
     D S2N7            S             15  4 DIM(6)
     D STOC            S             15  4 DIM(8)                               STOCK DISCOUNT
     D EXPS            S             15  4 DIM(8)                               STOCK DISCOUNT
     D WAV             S             11  0 DIM(8)                               WAVIER CHARGE
     D WAV1            S             11  0 DIM(8)                               WAVIER CHARGE TAX
     D WAV2            S             11  0 DIM(8)                               WAVIER CHARGE 1ST
     D WAVT            S             11  0 DIM(8)                               WAVIER CHARGE 2ND
     D RDEV            S              5  4 DIM(6)                               MA DIA DEV RATES
     D MAND            S             15  4 DIM(6)                               MANUAL PREM
     D ADLD            S             15  4 DIM(6)                               ADDITIONAL LOAD
     D FCL             S             15  4 DIM(6)                               F-CLASS PREM
     D SUB             S             15  4 DIM(8)
     D MOD             S             15  4 DIM(8)
     D STD             S             15  4 DIM(8)
     D NOM             S             15  4 DIM(8)
     D ANL             S             15  4 DIM(8)
     D EDC1            S             15  4 DIM(8)
     D EDC2            S             15  4 DIM(8)
     D D1E2            S             15  4 DIM(8)
     D D2E2            S             15  4 DIM(8)
     D EDC3            S             15  4 DIM(8)
     D EDV2            S             15  4 DIM(8)
     D EDCT            S             15  4 DIM(8)
     D EDD2            S             15  4 DIM(8)
     D EDCM            S             15  4 DIM(8)
     D EDCS            S             15  4 DIM(8)
     D EDC             S             15  4 DIM(8)
     D EPRM            S             15  4 DIM(8)
     D EXA1            S             15  4 DIM(8)
     D EXA2            S             15  4 DIM(8)
     D EXA3            S             15  4 DIM(8)
     D EHCN            S             15  4 DIM(8)
     D UPSN            S             15  4 DIM(8)
     D UPFP            S             15  4 DIM(8)
     D PRMU            S             15  4 DIM(8)
     D WAVU            S             11  0 DIM(8)
     D ETCA            S             15  4 DIM(8)
     D ASM1            S             11  0 DIM(8)
     D ASM2            S             11  0 DIM(8)
     D ASM3            S             11  0 DIM(8)
     D ASM4            S             11  0 DIM(8)
     D ASM5            S             11  0 DIM(8)
     D ARP1            S             15  4 DIM(8)
     D ARP2            S             15  4 DIM(8)
     D ARP3            S             15  4 DIM(8)
     D DTR             S             15  4 DIM(8)
     D ETR             S             15  4 DIM(8)
     D NTR             S             15  4 DIM(8)
     D DRT             S              5  3 DIM(6)
     D ERT             S              5  3 DIM(6)
     D ART1            S              5  3 DIM(6)
     D ART2            S              5  3 DIM(6)
     D ART3            S              5  3 DIM(6)
     D ART4            S              5  3 DIM(6)
     D ART5            S              5  3 DIM(6)
     D ART6            S              5  3 DIM(6)
     D ART7            S              5  3 DIM(6)
     D A1J             S             15  4 DIM(6)
     D A1E             S             15  4 DIM(6)
     D A1S             S             15  4 DIM(6)
     D A1D             S             15  4 DIM(6)
     D A2J             S             15  4 DIM(6)
     D A2E             S             15  4 DIM(6)
     D A2S             S             15  4 DIM(6)
     D A2D             S             15  4 DIM(6)
     D A3J             S             15  4 DIM(6)
     D A3E             S             15  4 DIM(6)
     D A3S             S             15  4 DIM(6)
     D A3D             S             15  4 DIM(6)
     D ASA             S             15  4 DIM(6)
     D A932            S             15  4 DIM(6)
     D F932            S             15  4 DIM(6)
     D intWk11         S             15  0
     D intDecimal      S             15  0
     D A4J             S             15  4 DIM(6)
     D A4E             S             15  4 DIM(6)
     D A4S             S             15  4 DIM(6)
     D A4D             S             15  4 DIM(6)
     D A5J             S             15  4 DIM(6)
     D A5E             S             15  4 DIM(6)
     D A5S             S             15  4 DIM(6)
     D A5D             S             15  4 DIM(6)
     d pyTRt           s                   like(RRterr) dim(6)                  Foreign Terr Rate
     d dmTrt           s                   like(RRdtrr) dim(6)                  Domestic Terr Rate
     d npTRt           s                   like(RRter2) dim(6)                  Non-Payoll Terr Rate
     d xxTert          s              7  4
     D asmFact         s              9  6
     D xxamt154        s             15  4
     d hasDiffLCMs     s               n
     d xxAmt1          s             11  2
     d xxTotl          s             11  2
     d xy              s              1  0
     d LCM             s                   like(xxLCM)  dim(2)
     D FY              S              3  0 DIM(99)                              FD FUND YEARS
     D EF              S              7  0 DIM(99) ASCEND                       FYR EFF DATES
     D EX              S              7  0 DIM(99) ASCEND                       FYR EXP DATES
     D FYY             S              3  0 DIM(99)                              CB FUND YEARS
     D PRL             S              9  0 DIM(99)                              EST/AUD PYRL
     D PRR             S              5  3 DIM(99)                              PRORATA
     D FAF             S              1    DIM(99)                              EST OR AUDITED
     D XSHGH           S             11  2 DIM(7)                               Terr flag (Assmnts)
     D XXDTRA          S             15  4
     D XODT            S              5  0 DIM(99) DESCEND
     D XXTEIN          S             15  4 INZ
     D xxFlgCalcUPSC   S               n
     D xxPrmFact       S              7  3
     D xxTmp           S             15  4
     D isFactor        S               n   inz
     d xxadjp          s              4  1                                      flat rate adjustment
     d xxrtadj         s              5  3                                      flat rate adjustment
     d invoice#        s             10  0
     d inKey           s             20a
     d hldFnd          s                   Like(EAfnd)
     d hldEmp#         s                   Like(EAemp#)
     d hldDiv          s                   Like(EAdiv)
     d hldInv#         s                   Like(EAnInv)
     d hldPoDt         s                   Like(EApoDt)
     d hldArSm         s                   Like(ELarSm) Inz
     D inJson          s          32000a   Inz varying
      //*** Beg Add ***** 08/14/24 ***************************************
     D XLBal1          s             11s 2
     D XLBal2          s             11s 2
     D XXFact          s              5s 4
     D XXMin1          s             11s 0
      //*** End Add ***** 08/14/24 ***************************************
      //*** Beg Add ***** 11/13/24 *****************************************
     D xxDis5          s              5s 4
      //*** End Add ***** 11/13/24 *****************************************
     d*** Begin Add *** 01/05/24 *******************************************
     d clbBifData      s                   Sqltype(clob:64000)
     D cmdStr          s           3000    Inz varying
     D cmdLength       s             15  5 Inz

     D LodWC2663       ds                  DTAARA(LODWC2663)
     D   sweepID                     40a
     d*** End   Add *** 01/05/24 *******************************************
     d*** Begin Add *** 03/05/25 *******************************************
     ddsWMXAP        e ds                  extname(WMXAP) qualified inz
     d WMXAPnull       s              5i 0 dim(33)
     drrnWMXAP         s              9  0
     d*** End   Add *** 03/05/25 *******************************************

     d*** begin chg ***  09/26/22  ************************************************
     d***** arrInvoice      s             13s 0 Dim(9999) Ascend Inz(9999999999999)
     d***** decDivision     s              3s 0
     d***** decInvoiceDiv   s             13s 0
     d arrInvoice      s             15s 0 Dim(9999) Ascend Inz(999999999999999)
     d decDivision     s              5s 0
     d decInvoiceDiv   s             15s 0
     d*** end chg *****  09/26/22  ************************************************
     d i               s              5s 0
     d invoiceFound    s               n
     d index           s              5s 0
     d srdate1         ds             5
     d  sryy1                  1      3  0
     d  srmm1                  4      5  0
     d*
     d srdate2         ds             5
     d  sryy2                  1      3  0
     d  srmm2                  4      5  0
      *

     d usrdat          ds            20
     d  usrID                        10
     d  runDate                      10

     D                 DS
     D  XXYMD                  1      7  0
     D  XXYY                   1      3  0
     D  XXMM                   4      5  0
     D  XXYM                   1      5  0
     D                 DS
     D  XXPTDT                 1      7  0
     D  XXPY                   1      3  0
     D  XXPM                   4      5  0
     D  XXPYM                  1      5  0
     D*
     D* Start date for serch of payroll file
     D                 DS
     D  XXPFDT                 1      7  0
     D  X#STDT                 1      5  0
     D  XXSTYR                 1      3  0
     D  XXSTMN                 4      5  0
     D* Comparison date for stopping search of payroll
     D                 DS
     D  XXCPDT                 1      7  0
     D  XXCPYM                 1      5  0
     D  XXCPYR                 1      3  0
     D  XXCPMN                 4      5  0
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
     D                 DS
     D  XXANDT                 1      7  0
     D  XXANYR                 1      3  0
     D                 DS
     D  XXENPO                 1      7  0
     D  XXENYR                 1      3  0
     D                 DS
     D  ED                     1    035  0
     D                                     DIM(5) ASCEND                        EFFECTIVE DATES
     D  EMEFD1               001    007  0
     D  EMEFD2               008    014  0
     D  EMEFD3               015    021  0
     D  EMEFD4               022    028  0
     D  EMEFD5               029    035  0
     D                 DS
     D  CD                     1    035  0
     D                                     DIM(5) ASCEND                        CANCEL DATES
     D  EMCAD1               001    007  0
     D  EMCAD2               008    014  0
     D  EMCAD3               015    021  0
     D  EMCAD4               022    028  0
     D  EMCAD5               029    035  0
     D                 DS
     D  EAPODT                 1      7  0
     D  EAPOYM                 1      5  0
     D XPARA           DS                  OCCURS(255)
     D  XPCLAS                 1      5  0
     D  XPPAY                  6     20  0
     D  XPFLAG                21     21
     D ELRRDS          DS
     D  DBRRN                397    400B 0
     DQQFOUND          S              1N
     DQQMPAYR          S              7  5
     DQQMRATE          S              7  5
     DQQRPTCLS         S              5  0
     d xxAPF           s                   like(ELapf)
     d GetPolFee       pr                  like(ELapf)
     d ApplyRateSplit  pr              n
     d inState                        2a
     d inDate                         7  0
     d prIncLmtBasis   pr            15  4
     d   inTotLimits                 15  4
     d   inMinimumPrm                15  4
     d   inPrmIncLmts                15  4
     d   inILF                        1
     d   inAssessLvl                  1
     d   inJPrm                      15  4
     d   inEPrm                      11  0
     d   inSPrm                      15  4
     d   inDPrm                      15  4
     d   inXPrm                      15  4
     d   inAMNP                       1
     d   inATR                        1
     D   outFactor                    9  6 options(*nopass)
     d prGetUnEarn     pr                  like(ELUPSA)
     d   inProPrem                   15  4
     d   inPrmFact                         like(xxPrmFact)
     d   inSurcharge                       like(CRUPSP)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D futAR           DS                  QUALIFIED   dim(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  prem           ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  taxes          ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  memfees        ds                  qualified      DIM(99)                 membership fees
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  saffees        ds                  qualified      DIM(99)                 Safety fees
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  asst1          ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  asst2          ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  asst3          ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  asst4          ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  asst5          ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  asst6          ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  asst7          ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  instfees       ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D  expConst       ds                  qualified      DIM(99)
     D  amt#                          9S 0
     D  podt                          7S 0
      *
     D*Variables definition
     d Instpodt        s                   like(xxdate)
     d InstAmt         s             15  4
     d TotalInstBal    s             15  4
     d CreditAmt       s             15  4
     d @@typ@          s                   like(xx1typ)
     d @@cat@          s                   like(xx1cat)
      ** Prototype of procedures
      * Get the invoice for billing records
     D  GetInv         pr
      * Write the records in file WMEAP e Array
     D  Wrtwmeap       pr
     d @@cde                               like(xx1cde)
     d @@@div                              like(xxdiv)
     d @@typ                               like(xx1typ)
     d @@cat                               like(xx1cat)
     d @@des                               like(xx1des)
     d @@amt                               like(xxdif1) value
     d @@podt                              like(xxdate)
     d @@fyr                               like(xxfyr) options(*nopass)
      * Search the index in Data Structure Array
     D  SrchIdx        pr             3S 0
     d xxpodt                              like(EAPODT)          value
     d @@DS                                like  (futar) dim(99) value
     d* Sum all the elements of Data Structure Array
     D  SumAmt         pr            15s 4
     d @@DS                                like  (futar) dim(99) value
     d finalUpdate     s               n
     d unBilPrmInst    s               n
     d  inObj          s             10
     d  inType         s             10
     d  rtnResult      s              1                                         *off = obj exists
     d ChkObjInLibl    pr                  extpgm('SRCHKOBJ')
     d  inObj                        10
     d  inType                       10
     d  rtnResult                     1                                         *off = obj exists
     d isTN14          s               n
     d isDiscounted    pr              n
     d  state                         2    const
     d  class                         5  0 const
     d  spec                          1    const
     d isOK14          s               n
     d GetLogFact      pr                  extpgm('SRLOGCLAS')
     d  class                              like(PRclas)
     d  found                              like(qqFound)
     d  payroll                            like(qqMPayr)
     d  payrollRate                        like(qqMRate)
     d  reportClass                        like(qqRptCls)
     d* Get the Territorial Differential Factor
     d GetTDF          pr                  extpgm('WA101R')
     d  inCompany#                    3  0
     d  inGroup#                      3  0
     d  inEmployer#                   9  0
     d  inDivision                    5  0
     d  inYear                        3  0
     d  outTDF                        5  4
     d  outTDF         s              5  4
     d/copy wprEmplr
     d s2Emp#          s                   like(EMemp#)
     d stRptPDays      s                   like(pDays)
     d stRptProRt      s                   like(ELpror)
     d xxwh94          s             13  4
     d xxTrDays        s              3  0
     d xxDayRem        s              3  0
     d xxSpltDtB       s              7  0
     d xxSpltDtE       s              7  0
     d xxDateFrom      s               d   datfmt(*iso)
     d xxDateTo        s               d   datfmt(*iso)
     d xxTermEffT      s               d   datfmt(*iso)
     d xxTermEffF      s               d   datfmt(*iso)
     d FromDate8       s              8  0
     d Todate8         s              8  0
     D  allDiv         s               n   Inz(*Off)
     C*** Beg Add ***  10/25/23  *** WC2922 *******************************
     D hldCTST         S              1    Inz
     C*** End Add ***  10/25/23  *** WC2922 *******************************
     d*** begin add ***  06/25/25  *****************************************
     d x2arat          s              7s 2 Inz(*Zero)
     d*** end add *****  06/25/25  *****************************************
     C*
     C*  EMPLOYER MASTER FILE
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C* Key List for the Employer S/R File
     C*
     C     SRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    X#STDT
     C     EMKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFND             3 0
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
     C*  BILLING WORK FILE
     C*
     C     BLKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C     BLKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     BLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C     BLKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  SHORT RATE TABLE FILE
     C*
     C     STKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    X3EFDT
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
     C                   KFLD                    X3EFDT
     C                   KFLD                    EDAYS
     C*
     C*  EMPLOYER DETAIL FILE
     C*
     C     ELKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     ELKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     ELKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXPFYR            3 0
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     ELKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    DSEMP#
     C                   KFLD                    DSDIV
     C     ELKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     ELKY5         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     ELKY7         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C*
     C*  HCN GROUP SETUP KEY
     C*
     C     HCKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XSPFDT
     C*
     C     HCKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  HCN POLICY SETUP KEY
     C*
     C     HCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  PAYROLL CLASS FILE
     C*
     C     PRKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C                   KFLD                    XXCLAS            5 0
     C                   KFLD                    X1EFDT            7 0
     C*
     C     PRKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD            3
     C                   KFLD                    XXCLAS            5 0
     C*
     C*  EMPLOYER PAYROLL FILE
     C*
     C     EPKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EPKY3         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EPKY4         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  EXPERIENCE ADJUSTMENT RATING FILE
     C*
     C*
     C*  CONTRIBUTION BILLING DETAIL FILE
     C*
     C     LDKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     EPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    ELDIV
     C     EPKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C     EPKYL         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXCLAS
     C*
     C*  WAIVER KEY
     C*
     C     WVKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  RATE SET CODE FILE
     C*
     C     RSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXRTCD
     C*
     C*  STATISTICAL REPORTING/RATING FILE
     C*
     C     RRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     RRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*   KEY LIST FOR THE HARBOR EXPOSURE FILE
     C*
     C     HEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     HEKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C*  FUND MASTER
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*  FUND DETAIL
     C*
     C     F1KEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C     F1KEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    EAFYR
     C*
     C*  EMPLOYER ACCOUNTS RECEIVABLE FILE
     C*
     C     EAKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C     EAKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C*
     C*
     C     EAKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C     EAKEY4        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXARTY            1
     C                   KFLD                    XXDATE
     C*
     C     EAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    sysDt
     C*
     C     EAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*  TABLE FILE KEY
     C*
     C     TBKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C*  LIMITS FILE KEY
     C*
     C     LMKEY1        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    XXEFDT
     C     LMKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE DISCOUNT MASTER FILE
     C*
     C     DSKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXCODE            1
     C                   KFLD                    XXTP              1
     C                   KFLD                    XXEFDT
     C*
     C*   2ND LIST FOR THE DISCOUNT MASTER FILE
     C*
     C     DSKEY2        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXTP              1
     C*
     C*   3RD LIST FOR THE DISCOUNT MASTER FILE
     C*
     C     DSKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXCODE
     C                   KFLD                    XXTP              1
     C                   KFLD                    XXEFDT
     C                   KFLD                    XXAMTP           11 2
     C*
     C* EMPLR DISCOUNT SPREAD FILE
     C*
     C     DSKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C*
     C     DSKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     DSKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C*
     C* PREMIUM PERIOD FILE KEY
     C*
     C     PPKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFRDT            7 0
     C*
     C*   KEY LIST FOR THE DEDUCTIBLE DISCOUNT CODE FILE
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
     C                   KFLD                    XXDDT             7 0
     C*
     C     DDKY2         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXDDCD
     C*
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*   KEY LIST FOR THE FUND AUXILIARY FILE
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
      *
      * KEY LISTS FOR THE EXPENSE CONSTANT DEFINITION FILE
      *
     C     ECKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
      *
     C     ECKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXST
     C*
     C* POPULATION FILE KEY
     C*
     C     POKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PRRTCD
     C                   KFLD                    PRCLAS
     C                   KFLD                    PREFDT
     C                   KFLD                    EPAAPR
     C*
     C     POKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    PRRTCD
     C                   KFLD                    PRCLAS
     C                   KFLD                    PREFDT
     C     ZEXKY         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMFND
     C                   KFLD                    EMEMP#
     C                   KFLD                    EMDIV
     C     CRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
     C                   KFLD                    CXCDAT
     C*
     C     CRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXST
     C                   KFLD                    XXKEY
      *
      *  DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XPST              2
     C*
      *
      *  MEMBER FEE EXCEPTION FILE
      *
     C     MEKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
      *
     C     *LIKE         DEFINE    GTKEY         KKKEY
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C*
     C     *LIKE         DEFINE    ELFYR         XXFYR
     C     *LIKE         DEFINE    EPEFDT        X2EFDT
     C     *LIKE         DEFINE    EPEFDT        X3EFDT
     C     *LIKE         DEFINE    ECEXCO        XXEXCO
     C     *LIKE         DEFINE    ECST          XXST
     C     *LIKE         DEFINE    ECST          SV1ST
     C     *LIKE         DEFINE    ELHCN         XXHCNF
     C     *LIKE         DEFINE    ELAHCN        XXHCNA
     C     *LIKE         DEFINE    ELUPSA        XXUPSA
     C     *LIKE         DEFINE    ELPTDT        XXENP2
     C     *LIKE         DEFINE    ELPTDT        XXEFDT
     C     *LIKE         DEFINE    ELPTDT        XXDATE
     C     *LIKE         DEFINE    ELPTDT        XXDAT#
     C     *LIKE         DEFINE    ELPTDT        X1FROM
     C     *LIKE         DEFINE    ELPTDT        X1TO
     C     *LIKE         DEFINE    ELPTDT        SVEFDT
     C     *LIKE         DEFINE    ELPTDT        SVCNDT
     C     *LIKE         DEFINE    ELPTDT        SVDT
     C     *LIKE         DEFINE    ELEMP#        SVEMP#
     C     *LIKE         DEFINE    ELEMP#        XXEMP#
     C     *LIKE         DEFINE    ELDIV         XXDIV
     C     *LIKE         DEFINE    ELDCEX        XXDCEX
     C     *LIKE         DEFINE    ELMCEX        XXMCEX
     C     *LIKE         DEFINE    ELSCEX        XXSCEX
     C     *LIKE         DEFINE    ELDVEX        XXDVEX
     C     *LIKE         DEFINE    ELEMD1        XXEXEF
     C     *LIKE         DEFINE    ELEMD1        XXEXEX
     C     *LIKE         DEFINE    ELCREX        XXCREX
     C     *LIKE         DEFINE    EAPODT        SVPODT
     C     *LIKE         DEFINE    EAPODT        XHEFDT
     C     *LIKE         DEFINE    EAPODT        XXLPDT
     C     *LIKE         DEFINE    EAPODT        HLDPDT
     C     *LIKE         DEFINE    EAARTY        HDARTY
     C     *LIKE         DEFINE    EAARCD        HDARCD
     C     *LIKE         DEFINE    EAAMT         HDAMT
     C     *LIKE         DEFINE    ELDIV         SVDIV
     C     *LIKE         DEFINE    EAAMT         SVAMT
     C     *LIKE         DEFINE    EAARTY        SVARTY
     C     *LIKE         DEFINE    EAFYR         SVFYR
     C     *LIKE         DEFINE    ELDIV         X2DIV
     C     *LIKE         DEFINE    DDPCT         XXDDCT
     C     *LIKE         DEFINE    DDPCT         XXDD2P
     C     *LIKE         DEFINE    EMCAD1        XXCNDT
     C     *LIKE         DEFINE    EPARAT        XXARAT
     C     *LIKE         DEFINE    ELSFMN        XXSFMN
     C     *LIKE         DEFINE    HEHXMN        XXHXMN
     C     *LIKE         DEFINE    DSPCT         XNDISP
     C     *LIKE         DEFINE    ELMFEE        SVMFEE
     C     *LIKE         DEFINE    EPAAPR        XXAAPR
     C     *LIKE         DEFINE    CRUPSP        XXUPSP
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
     C                   PARM                    QQAR              2
     C                   PARM                    QQREV             1
     C                   PARM                    QQALLDIV          1
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

      * Use expanded Minimum Premium if available

     c     *dtaara       define    minprm        usrdat
     c                   in        usrdat

     c                   if        usrid <> *blank
     c                   move      *on           xxMinPrm          1
     c                   else
     c                   move      *off          xxMinPrm
     c                   endif

     C*
     C* Set All division indicator
     C                   If        %Parms() > 8
     C                   If        qqAllDiv = 'Y'
     C                   Eval      allDiv = *On
     C                   EndIf
     C                   EndIf

     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C                   MOVE      XXCLT#        CLTID             3
     C                   MOVE      'A'           BILL              1
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
     C                   MOVE      *ON           *IN71
     C*
     C                   ELSE
     C                   MOVE      *OFF          *IN71
     C                   ENDIF                                                  QQREV IFGE
     C*
     C                   MOVE      ' '           XXUPAR            1
     C                   MOVE      QQREV         XXREV             1
     C*
     C     QQREV         IFEQ      '3'
     C     QQREV         OREQ      '6'
     C                   MOVE      'Y'           XXUPAR
     C                   ENDIF
     C*
     C     QQREV         IFEQ      '1'
     C     QQREV         OREQ      '2'
     C     QQREV         OREQ      '3'
     C                   MOVE      ' '           Q1REV             1
     C                   ENDIF
     C*
     C     QQREV         IFEQ      '4'
     C     QQREV         OREQ      '5'
     C     QQREV         OREQ      '6'
     C                   MOVE      'Y'           Q1REV
     C                   ENDIF
     C*
     C     QQREV         IFEQ      'I'
     C                   MOVE      *ON           *IN96
     C                   MOVE      *BLANK        QQREV
     C                   ELSE
     C                   MOVE      *OFF          *IN96
     C                   ENDIF
     C     QQREV         IFEQ      'Z'
     C                   MOVE      *ON           *IN97
     C                   MOVE      *BLANK        QQREV
     C                   ELSE
     C                   MOVE      *OFF          *IN97
     C                   ENDIF
     C     QQREV         IFEQ      'L'
     C                   MOVE      *ON           *IN97
     C                   MOVE      *ON           *IN94
     C                   MOVE      *BLANK        QQREV
     C                   ELSE
     C                   MOVE      *OFF          *IN94
     C                   ENDIF
     C     QQREV         IFEQ      'S'
     C                   MOVE      *ON           *IN95
     C                   MOVE      *BLANK        QQREV
     C                   ELSE
     C                   MOVE      *OFF          *IN95
     C                   ENDIF

      /free
       finalUpdate = *off;
       If *in94 = *off and *in95 = *off and *in96 = *off and
        *in97 = *off;
          finalUpdate = *on;
          inObj = 'WXELP';
          inType = '*FILE';
          Callp ChkObjInLibl (inObj:inType:rtnResult);
          If rtnResult = *off;
             unBilPrmInst = *on;
          Endif;
       Endif;
      /end-free
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQEMP#        XXEMP#
     C                   MOVE      QQDIV         XXDIV
     C                   MOVE      QQDATE        X@DATE            8 0
     C                   MOVE      QQEMP#        SVEMP#
     C                   Z-ADD     0             S1EMP#            9 0
     C                   Z-ADD     0             S1FYR             3 0
     C*** Begin Add *** 01/05/24 *******************************************
       // Is this a mass run?
       If svEmp# = 0 And (qqRev = '3' Or qqRev = 'C' Or qqRev = 'F');
          // Create Sweep Data Area
         //cmdStr = 'CRTDTAARA DTAARA(QTEMP/LODWC2663) TYPE(*CHAR) LEN(40) '+
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
       EndIf;
     C*** End   Add *** 01/05/24 *******************************************
     C                   TIME                    DSDTTM
     C     *LIKE         DEFINE    EADT          SYSDT
     C     *LIKE         DEFINE    EATIME        SYSTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYSDT
     C                   Z-ADD     DSTIME        SYSTM
     C                   Z-ADD     0             XXACDT            8 0
     C*** Beg Add ***  10/25/23  *** WC2922 *******************************
        Chain(n) (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) Pel01;
        If %found;
           hldCTST = ELCTST;
        EndIf;
     C*** End Add ***  10/25/23  *** WC2922 *******************************
     C*
     C     FNKEY         CHAIN     PEX02                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     EXAAID        XXDAYS            5 0
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
     C                   Z-ADD     0             XSDIV             5 0
     C                   MOVE      ' '           XXWAR
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
     C     EMKY          CHAIN(N)  PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        ANDNE     'Y'
     C     EMKY          READE(N)  PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C                   Z-add     EMDiv         xxDiv
     C*
     C     ELKEY         CHAIN(N)  PEL01                              60
     C                   Move      ELARSM        svARSM
     C*
     C                   Z-ADD     5             XXX
     C                   Z-ADD     0             svCNDT
     C                   DO        5
     C     CD(XXX)       IFLE      ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      CD(XXX)       svCNDT
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             XXX
     C                   ENDDO
     C*
     C                   Select
     C                   When      svCNDT < ELPfdt
     C                   Z-add     ELPfdt        srdEXDT
     C                   When      svCNDT <= ELPTDT
     C                   Z-add     svCNDT        srdEXDT
     C                   Other
     C                   Z-add     ELPtdt        srdEXDT
     C                   Endsl
     C*
     C                   Move      QQDiv         xxDiv
     C*** end add *****  08/01/25  ********************************************
     C     ELKEY         CHAIN(N)  PEL01                              60
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   Z-ADD     5             XXX               1 0
     C                   Z-ADD     0             SVCNDT
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      CD(XXX)       SVCNDT
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             XXX
     C                   ENDDO
     C*
     C     SVCNDT        IFEQ      *ZERO
     C                   Z-ADD     ELPTDT        SVEXDT            7 0
     C                   ELSE
     C                   Z-ADD     SVCNDT        SVEXDT
     C                   ENDIF
     C*
     C* DEFAULT POLICY TO DATE FOR FLAT CANCELLATIONS.
     C*
     C     SVCNDT        IFEQ      *ZERO
     C*
      *
      * Don't let the policy flat CX if an effective date exists after
      * a CX date has been entered for a previous effective date.
      * NOTE: This change was implemented to handle legacy
      * policies with lapse in coverage.
      *
     c                   z-add     5             xxx               1 0
     c                   z-add     6             yyy               1 0
     c                   do        5
     c                   if        cd(xxx) <  ELPFDT and
     c                             cd(xxx) <> 0      and
     c                             ed(xxx) <> 0
     c                   if        yyy     = 6  or
     c                             ed(yyy) = 0
     c                   z-add     ELPFDT        SVEXDT
     c                   leave
     c                   endif
     c                   endif
     c                   sub       1             xxx
     c                   sub       1             yyy
     c                   enddo
     C*
     C                   ENDIF                                                  SVCNDT IFEQ
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
     C*
     C                   ENDIF
     C*
     C                   TIME                    DSDTTM
     C*
     C*  GET THE USER PARMS
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C*
     C                   Z-ADD     X@DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     FNKEY         CHAIN     PF201                              60
     C*
     C*  GET THE FUND MASTER
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         CABEQ     '1'           END
     C*
     C*
     C*  LOAD FLAG THAT DETERMINES WHETHER OR NOT THIS FUND WAS SET-UP
     C*  FOR NON-COMMON EXPIRATION DATE POLICIES
     C*
     C     F2KY          CHAIN     PF201                              60
     C*
     C*  READ THE BILLING WORK FILE
     C*
     C     XXEMP#        CABNE     *ZERO         READEM
     C*
     C     BLKEY         SETLL     PBL01
     C     READE         TAG
     C                   MOVE      'N'           XXVAL             1
     C     BLKEY         READE     PBL01                                  60
     C     *IN60         CABEQ     '1'           END
     C     XXWAR         IFEQ      'Y'
     C     BLEMP#        ANDNE     XXEMP#
     C                   MOVE      *BLANK        XXWAR
     C                   MOVE      'Y'           XXSUM
     C                   MOVE      'Y'           XXAR
     C                   MOVE      XSDIV         XXDIV
     C                   EXSR      WRTAR
     C                   ENDIF
     C*
     C     BLFLG2        CABNE     'Y'           READE
     C*
     C                   Z-ADD     BLEMP#        XXEMP#
     C                   Z-ADD     BLDIV         XXDIV
     C*
     C     BLFYR         IFNE      XXFYR
     C                   Z-ADD     BLFYR         XXFYR
     C                   MOVE      *ON           *IN99
     C                   ENDIF
     C*
     C     ELKEY         CHAIN(N)  PEL01                              60
      *
     C     *IN60         IFEQ      *OFF
      *
     C     ELFAF         IFNE      'A'
     C     Q1REV         ANDEQ     'Y'
     C                   GOTO      READE
     C                   ENDIF                                                  ELFAF <> "A"
     C     ELFAF         IFEQ      'A'
     C     Q1REV         ANDNE     'Y'
     C                   GOTO      READE
     C                   ENDIF                                                  ELFAF = "A"
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C*  READ THE EMPLOYER RECORDS
     C*
     C     READEM        TAG
     C*
     C     QQFYR         IFNE      '000'
     C     *IN99         OREQ      *ON
     C*
     C*  GET THE FUND DETAIL
     C*
     C     F1KEY         CHAIN     PF101                              60
     C                   MOVE      *OFF          *IN99
     C*
     C     *IN60         IFEQ      *ON
     C*
     C     QQFYR         IFNE      '000'
     C                   GOTO      END
     C                   ELSE
     C                   GOTO      READE
     C                   ENDIF                                                  QQFYR IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*  GET THE A/R TRANSACTION CODE FOR MEMBERSHIP FEE
     C*
     C                   MOVE      *BLANK        XXMFTP
     C                   MOVE      *BLANKS       XXMFDS
     C                   MOVE      *BLANKS       XXMFBL            2
     C                   MOVE      *BLANK        XXMFCT
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'M'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXMFBL
     C                   MOVE      TTTYPE        XXMFTP            1
     C                   MOVE      TTDESC        XXMFDS           30
     C                   MOVE      TTCTYP        XXMFCT            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION CODE FOR SAFETY FEE
     C*
     C                   MOVE      *BLANK        XXSFTP
     C                   MOVE      *BLANK        XXSFCT
     C                   MOVE      *BLANKS       XXSFDS
     C                   MOVE      *BLANKS       XXSFCD            2
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'S'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXSFCD
     C                   MOVE      TTTYPE        XXSFTP            1
     C                   MOVE      TTDESC        XXSFDS           30
     C                   MOVE      TTCTYP        XXSFCT            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION CODE FOR 1ST ASSESSMENT
     C*
     C                   MOVE      *BLANK        XX1TYP            1
     C                   MOVE      *BLANKS       XX1DES           30
     C                   MOVE      *BLANKS       XX1CDE            2
     C                   MOVE      *BLANK        XX1CAT
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '1'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX1CDE
     C                   MOVE      TTTYPE        XX1TYP
     C                   MOVE      TTDESC        XX1DES
     C                   MOVE      TTCTYP        XX1CAT            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION CODE FOR 2ND ASSESSMENT
     C*
     C                   MOVE      *BLANK        XX2TYP            1
     C                   MOVE      *BLANKS       XX2DES           30
     C                   MOVE      *BLANKS       XX2CDE            2
     C                   MOVE      *BLANK        XX2CAT
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '2'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX2CDE
     C                   MOVE      TTTYPE        XX2TYP
     C                   MOVE      TTDESC        XX2DES
     C                   MOVE      TTCTYP        XX2CAT            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION CODE FOR 3RD ASSESSMENT
     C*
     C                   MOVE      *BLANK        XX3TYP            1
     C                   MOVE      *BLANKS       XX3DES           30
     C                   MOVE      *BLANKS       XX3CDE            2
     C                   MOVE      *BLANK        XX3CAT
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '3'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX3CDE
     C                   MOVE      TTTYPE        XX3TYP
     C                   MOVE      TTDESC        XX3DES
     C                   MOVE      TTCTYP        XX3CAT            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION CODE FOR 4TH ASSESSMENT
     C*
     C                   MOVE      *BLANK        XX4TYP            1
     C                   MOVE      *BLANKS       XX4DES           30
     C                   MOVE      *BLANKS       XX4CDE            2
     C                   MOVE      *BLANK        XX4CAT
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '4'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX4CDE
     C                   MOVE      TTTYPE        XX4TYP
     C                   MOVE      TTDESC        XX4DES
     C                   MOVE      TTCTYP        XX4CAT            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION CODE FOR 5TH ASSESSMENT
     C*
     C                   MOVE      *BLANK        XX5TYP            1
     C                   MOVE      *BLANKS       XX5DES           30
     C                   MOVE      *BLANKS       XX5CDE            2
     C                   MOVE      *BLANK        XX5CAT
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '5'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX5CDE
     C                   MOVE      TTTYPE        XX5TYP
     C                   MOVE      TTDESC        XX5DES
     C                   MOVE      TTCTYP        XX5CAT            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION CODE FOR 6TH ASSESSMENT
     C*
     C                   MOVE      *BLANK        XX6TYP            1
     C                   MOVE      *BLANKS       XX6DES           30
     C                   MOVE      *BLANKS       XX6CDE            2
     C                   MOVE      *BLANK        XX6CAT
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '6'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX6CDE
     C                   MOVE      TTTYPE        XX6TYP
     C                   MOVE      TTDESC        XX6DES
     C                   MOVE      TTCTYP        XX6CAT            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION CODE FOR 7TH ASSESSMENT
     C*
     C                   MOVE      *BLANK        XX7TYP            1
     C                   MOVE      *BLANKS       XX7DES           30
     C                   MOVE      *BLANKS       XX7CDE            2
     C                   MOVE      *BLANK        XX7CAT
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      '7'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XX7CDE
     C                   MOVE      TTTYPE        XX7TYP
     C                   MOVE      TTDESC        XX7DES
     C                   MOVE      TTCTYP        XX7CAT            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION FILE FOR EXPENSE CONSTANT
     C*
     C                   MOVE      *BLANK        XTTYPE
     C                   MOVE      *BLANKS       XTDESC
     C                   MOVE      *BLANK        XTCTYP            1
     C                   MOVE      *BLANKS       XXCDEB            2
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C     TTCTYP        IFEQ      'E'
     C     TTTYPE        ANDEQ     'B'
     C                   MOVEL     TTKEY         XXCDEB
     C                   MOVE      TTTYPE        XTTYPE            1
     C                   MOVE      TTCTYP        XTCTYP            1
     C                   MOVE      TTDESC        XTDESC           30
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION FILE FOR STATE TAX
     C*
     C                   MOVE      *BLANK        XSTYPE
     C                   MOVE      *BLANKS       XSDESC
     C                   MOVE      *BLANKS       XXTXCD            2
     C                   MOVE      *BLANK        XSCTYP            1
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      'T'
     C     TTTYPE        ANDEQ     'F'
     C                   MOVEL     TTKEY         XXTXCD
     C                   MOVE      TTTYPE        XSTYPE            1
     C                   MOVE      TTDESC        XSDESC           30
     C                   MOVE      TTCTYP        XSCTYP            1
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION FILE FOR RETRO ADJUSTMENT.
     C*
     C                   MOVE      *BLANK        XRTYPE            1
     C                   MOVE      *BLANKS       XRDESC           30
     C                   MOVE      *BLANKS       XRARCD            2
     C                   MOVE      *BLANK        XRCTYP            1
     C*
     C     *LOVAL        SETLL     PTT01
     C                   READ      PTT01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C     TTCTYP        IFEQ      'C'
     C     TTTYPE        ANDEQ     'B'
     C                   MOVEL     TTKEY         XRARCD
     C                   MOVE      TTTYPE        XRTYPE
     C                   MOVE      TTDESC        XRDESC
     C                   MOVE      TTCTYP        XRCTYP
     C                   MOVE      *ON           *IN60
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C  N60              READ      PTT01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*  GET THE A/R TRANSACTION FILE
     C*
     C                   MOVEL(P)  QQAR          XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      TTTYPE        XXTYPE            1
     C                   MOVEL(P)  TTDESC        XXDESC           30
     C                   MOVEL     TTCTYP        XXCTYP            1
     C                   ELSE
     C                   MOVE      *BLANK        XXTYPE
     C                   MOVE      *BLANKS       XXDESC
     C                   MOVE      *BLANK        XXCTYP
     C                   END
     C*
     C*  GET NUMBER OF FUND POLICY DAYS
     C*
     C                   Z-ADD     0             XXLPDT
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
      *** Begin Add *** 02/06/24 *******************************************
      /Free
       If FNcgfl = 'Y';
          FDAYS = 365;
       Endif;
      /End-Free
      *** End   Add *** 02/06/24 *******************************************
     C                   ENDIF                                                  QQFYR IFNE
     C*
     C     SVEMP#        IFEQ      *ZERO
     C*
     C     ELKY1         CHAIN(N)  PEL01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELARSM        IFEQ      'Y'
     C*
     C     EPKY          CHAIN(N)  PEP01                              60
     C     *IN60         DOWEQ     '0'
     C*** begin del ***  06/16/22  *****************************************
     C*****EPSTAT        ANDEQ     'A'
     C*** end del *****  06/16/22  *****************************************
     C*
     C     EPSAFL        IFNE      'A'
     C*** begin add ***  06/16/22  *****************************************
     C     EPSTAT        ANDEQ     'A'
     C*** end add *****  06/16/22  *****************************************
     C     BLKY1         CHAIN     PBL02                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PBL02
     C     BLKY1         READE     PBL02                                  60
     C                   ENDDO
     C     BLKY          SETGT     PBL01
     C                   GOTO      READE
     C                   END
     C*
     C     EPKY          READE(N)  PEP01                                  60
     C                   ENDDO
     C*
     C                   ENDIF                                                  ELARSM IFEQ
     C*
     C     ELKY1         READE(N)  PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  SVEMP# IFEQ
     C*
     C     SVEMP#        IFEQ      *ZERO
     C     *IN95         OREQ      *ON
     C     *IN96         OREQ      *ON
     C     *IN97         OREQ      *ON
     C     *IN71         OREQ      *ON
     C     EMKEY         CHAIN     PEM01                              60
     C                   ELSE
     C     EMKY          CHAIN     PEM01                              60
     C                   ENDIF
     C*
     C     SVEMP#        IFNE      *ZERO
     C     SVEMP#        OREQ      *ZERO
     C     XXEMP#        ANDNE     S1EMP#
     C     SVEMP#        OREQ      *ZERO
     C     XXFYR         ANDNE     S1FYR
     C                   Z-ADD     XXEMP#        S1EMP#
     C                   Z-ADD     XXFYR         S1FYR
     C                   Z-ADD     0             XTDIFX           11 2
     C                   Z-ADD     0             XTDIFF           11 2
     C                   Z-ADD     0             XTSVDF           11 2
     C                   Z-ADD     0             XTDIFE           11 2
     C                   Z-ADD     0             XTDIFM           11 2
     C                   Z-ADD     0             XTDIFS           11 2
     C                   Z-ADD     0             XTDIF1           11 2
     C                   Z-ADD     0             XTDIF2           11 2
     C                   Z-ADD     0             XTDIF3           11 2
     C                   Z-ADD     0             XTDIF4           11 2
     C                   Z-ADD     0             XTDIF5           11 2
     C                   Z-ADD     0             XTDIF6           11 2
     C                   Z-ADD     0             XTDIF7           11 2
     C*** Begin Add *** 05/22/25 *******************************************
     C                   Z-ADD     0             totAdjPrem       11 2
     C                   Z-ADD     0             totAdjExpn       11 2
     C*** End   Add *** 05/22/25 *******************************************
     C                   ENDIF                                                  SVEMP# IFNE
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     EMDIV         XXDIV
     C*
     C     EMSTRP        IFEQ      'Y'
     C                   Z-ADD     EMDIV         XSDIV
     C                   ENDIF

      /free
       If Ememp# <> S2emp#;
          s2Emp#  = EMemp#;
          stRptPDays = GetStRptPDays(xxCo#:xxFnd:xxEmp#:xxFyr:xxdiv);
          If fDays <> 0;
             stRptProRt = stRptPDays/Fdays;
          Else;
             stRptProRt = 0;
          Endif;
       Endif;
      /end-free

     C     ELKEY         CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C*
     C*  FIND IF THE EMPLOYER IS CANCELLED AND WHEN.
     C*
     C                   Z-ADD     5             XXX               1 0
     C                   MOVE      *BLANK        SVCNCL            1
     C                   Z-ADD     0             SVCNDT
     C                   Z-ADD     ELPTDT        SVPTDT            7 0
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      'Y'           SVCNCL
     C                   MOVE      CD(XXX)       SVCNDT
     C                   Z-ADD     SVCNDT        ELPTDT
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             XXX
     C                   ENDDO
     C*
     C* DEFAULT POLICY TO DATE FOR FLAT CANCELLATIONS.
     C*
     C     SVCNCL        IFEQ      *BLANK
     C*
      *
      * Don't let the policy flat CX if an effective date exists after
      * a CX date has been entered for a previous effective date.
      * NOTE: This change was implemented to handle legacy
      * policies with lapse in coverage.
      *
     c                   z-add     5             xxx               1 0
     c                   z-add     6             yyy               1 0
     c                   do        5
     c                   if        cd(xxx) <  ELPFDT and
     c                             cd(xxx) <> 0      and
     c                             ed(xxx) <> 0
     c                   if        yyy     = 6  or
     c                             ed(yyy) = 0
     c                   z-add     ELPFDT        ELPTDT
     c                   leave
     c                   endif
     c                   endif
     c                   sub       1             xxx
     c                   sub       1             yyy
     c                   enddo
     C*
     C                   ENDIF                                                  SVCNCL IFEQ
     C*** begin add ***  06/16/22  ************************************
     C*
     C* If the employer/division is not expired yet and running mass
     C* audit then skip to next employer/division.
     C*
     C                   If        ELPtdt > sysDt and svEmp# = 0
     C     BLKEY2        Setgt     PBL01
     C                   Goto      READE
     C                   Endif                                                  If ELPTDT > sysdt
     C*** end add *****  06/16/22  ************************************
     C*
     C*** begin chg ***  08/01/25  ********************************************
     C*****SVPTDT        IFNE      ELPTDT
     C*****ELPtdt        AndEQ     svExDt
     c*****allDiv        AndEQ     *Off
     C*****svPtDt        OrNE      ELptDt
     c*****allDiv        AndEQ     *On
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
     C* Update policy exp date if div is going to process.
     C                   If        svPtdt <> ELPtdt and processDiv = *on
     C*** end chg *****  08/01/25  ********************************************
      *
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        ELTIME
      *
     C                   UPDATE    PEL01
     C     ELKEY         CHAIN     PEL01                              60
     C                   Z-ADD     ELPTDT        SVPTDT
     C                   ENDIF                                                  SVPTDT IFNE
     C*
     C*** begin chg ***  08/01/25  ********************************************
     C*****ELPTDT        IFEQ      SVEXDT
     C*****SVEMP#        OREQ      *ZERO
     C*****allDiv        orEQ      *On
     C                   If        svemp# = 0 or processDiv = *on
     C*** end chg *****  08/01/25  ********************************************
     C*
     C     *IN71         IFEQ      *ON
     C     ELARSM        ANDEQ     'Y'
     C*
     C     EMKY          CHAIN     PEM01                              60
     C     *IN60         DOWEQ     *OFF
     C     EMSTRP        ANDNE     'Y'
     C     EMKY          READE     PEM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-ADD     EMDIV         XSDIV
     C*
     C* RESET EMPLOYER INFORMATION
     C*
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C                   ENDIF                                                  *IN71 IFEQ
     C*
     C* CHAIN OUT TO THE STATISTICAL REPORTING/RATING FILE.
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C     RRKY          SETGT     PIF01
     C     RRKY1         READPE    PIF01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       IFIFBL
     C                   ENDIF
     C*
     C*  ALL AUDITED PAYROLL FIGURES MUST BE ENTERED FOR THE EMPLOYER
     C*  BEFORE THE AUDIT REQUEST CAN BE PROCESSED
     C*
     C     EPKEY         CHAIN(N)  PEP01                              60
     C     *IN60         DOWEQ     '0'
     C*** begin del ***  06/16/22  *****************************************
     C*****EPSTAT        ANDEQ     'A'
     C*** end del *****  06/16/22  *****************************************
     C     EPSAFL        IFNE      'A'
     C*** begin add ***  06/16/22  *****************************************
     C     EPSTAT        ANDEQ     'A'
     C*** end add *****  06/16/22  *****************************************
     C     QQEMP#        IFEQ      *ZEROS
     C     BLKY1         CHAIN     PBL02                              60
     C     *IN60         DOWEQ     *OFF
     C                   DELETE    PBL02
     C     BLKY1         READE     PBL02                                  60
     C                   ENDDO
     C                   GOTO      READE
     C                   ELSE
     C                   GOTO      END
     C                   END
     C                   END
     C     EPKEY         READE(N)  PEP01                                  60
     C                   END
     C*
      *
     C     ELDCDE        IFNE      *BLANKS
     C                   MOVE      'Y'           XXDEDF            1
     C                   ELSE
     C                   MOVE      'N'           XXDEDF
     C                   ENDIF
     C*
     C* SAVE NCCI CANCEL CODE IF THE POLICY IS CANCELLED.
     C*
     C                   MOVE      *BLANK        XXCXCD            2
     C*** Beg Add *** 09/18/25 **************************************
     C                   MOVE      *BLANK        XXCXNOPRO         1
     C*** End Add *** 09/18/25 **************************************
     C*** begin add ***  06/26/23  ******************************************
     C                   Move      *blank        xxidov            1
     C*** end add *****  06/26/23  ******************************************
     C                   EVAL      XXUPSP = 0
     C     SVCNCL        IFEQ      'Y'
     C*
     C     ELKY4         SETGT     PCX01
     C     ELKY4         READPE    PCX01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C                   MOVEL(P)  CXRSN         XXKEY
     C     CRKY          SETGT     PCR01
     C     CRKY1         READPE    PCR01                                  60
     C*
     C     *IN60         DOWEQ     *OFF
     C     CRSTAT        ANDEQ     'I'
     C     CRKY1         READPE    PCR01                                  60
     C                   ENDDO
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CRNCCI        XXCXCD
     C*** Beg Add *** 09/18/25 **************************************
     C                   MOVE      CRNOPRO       XXCXNOPRO
     C*** End Add *** 09/18/25 **************************************
     C*** begin add ***  06/26/23  ******************************************
     C                   Move      CRIDOV        xxidov
     C*** end add *****  06/26/23  ******************************************
     C                   EVAL      XXUPSP = CRUPSP
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  SVCNCL IFEQ
     C*
     C                   Z-ADD     X@DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
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
     C     XXACDT        ANDNE     *ZERO
     C*
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ENDIF                                                  @@PLAN IFEQ
     C*
     C     ELBSDT        IFGT      XXDATE
     C                   MOVE      ELBSDT        XXDATE
     C                   END
     C                   MOVE      'Y'           XXVAL             1
     C*
     C                   Z-ADD     *ZERO         XXHIGH
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
     C                   CLEAR                   PEH01
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  FNCGFL IFNE
     C*
     C                   Z-ADD     0             WDAYS             5 0
     C*
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
     C                   ENDIF                                                  XXENDT IFNE
     C*
     C*  CALCULATE PREMIUM TWICE, FIRST RUN IS FOR TAXABLE PREMIUM.
     C*
     C*
     C* UPDATE AUDITED STATUS IF THE RECORD HAS NOT ALREADY BEEN UPDATED.
     C*
     C     DSKY1         CHAIN     PIL01                              60
     C*
     C     *IN60         IFEQ      *OFF
     C     ILSTAT        ANDNE     'A'
     C*
     C                   MOVE      'A'           ILSTAT
     C                   MOVEL     Q1USER        ILUSER
     C                   MOVEL     Q1PGM         ILPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ILDT
     C                   Z-ADD     DSTIME        ILTIME
     C*
     C                   UPDATE    PIL01
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   Z-ADD     *ZERO         XXHIGH
     C*
     C*  CLEAR EXPERIENCE MODIFIER BUCKETS
     C*
     C                   Z-ADD     *ZERO         XXEXM1            5 2
     C                   Z-ADD     *ZERO         XXEXM2            5 2
     C                   Z-ADD     *ZERO         XXEXM3            5 2
     C                   Z-ADD     *ZERO         XXEXP1           15 4
     C                   Z-ADD     *ZERO         XXEXP2           15 4
     C                   Z-ADD     *ZERO         XXEXP3           15 4
     C                   Z-ADD     ELMFEE        F1MFEE
     C*
     C* IF ANY DIVISION HAS A MEMBERSHIP FEE, PROCESS IT EVEN IF GROUP DETAIL
     C* ISN'T SET UP FOR IT
     C*
     C                   Z-ADD     ELMFEE        SVMFEE
     C     XXMFFL        IFEQ      ' '
     C     ELMFEE        ANDNE     *ZERO
     C                   MOVE      'Y'           XXMFFL            1
     C                   ENDIF
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         EXTO              8 0
     C*
     C                   MOVE      *BLANKS       XHEFDT
     C                   MOVE      *BLANKS       SVEFDT
     C*
     C*  CLEAR PAYROLL ARRAYS
     C*
     C                   Z-ADD     9999999       PDAT
     C                   Z-ADD     9999999       RDAT
     C                   Z-ADD     9999999       FDAT
     C                   Z-ADD     *ZERO         DY3
     C                   Z-ADD     0             PRO
     C                   Z-ADD     0             SRP
     C                   Z-ADD     0             MAN
     C                   Z-ADD     0             MANT
     C                   Z-ADD     0             LMT
     C                   Z-ADD     0             LM2T
     C                   Z-ADD     0             MAN1
     C                   Z-ADD     0             LM1
     C                   Z-ADD     0             LM21
     C                   Z-ADD     0             MAN2
     C                   Z-ADD     0             LM2A
     C                   Z-ADD     0             LM22
     C                   Z-ADD     0             MAN3
     C                   Z-ADD     0             LM3
     C                   Z-ADD     0             LM23
     C                   Z-ADD     0             MAN4
     C                   Z-ADD     0             LM4
     C                   Z-ADD     0             LM24
     C                   Z-ADD     0             MAN5
     C                   Z-ADD     0             LM5
     C                   Z-ADD     0             LM25
     C                   Z-ADD     0             MAN6
     C                   Z-ADD     0             LM6
     C                   Z-ADD     0             LM26
     C                   Z-ADD     0             MAN7
     C                   Z-ADD     0             LM7
     C                   Z-ADD     0             LM27
     C                   Z-ADD     0             LM
     C                   Z-ADD     0             LM2
     C                   Z-ADD     0             LPR
     C                   Z-ADD     0             MAND                           NON-DEVIATED
     C                   Z-ADD     0             ADLD                           NON-DEVIATED
     C                   Z-ADD     0             STN
     C                   Z-ADD     0             PRM
     C                   Z-ADD     0             DIS
     C                   Z-ADD     0             STNT
     C                   Z-ADD     0             SVNT
     C                   Z-ADD     0             EXPT
     C                   Z-ADD     0             STN1
     C                   Z-ADD     0             SVN1
     C                   Z-ADD     0             EXP1
     C                   Z-ADD     0             STN2
     C                   Z-ADD     0             SVN2
     C                   Z-ADD     0             EXP2
     C                   Z-ADD     0             WAV
     C                   Z-ADD     0             WAV1
     C                   Z-ADD     0             WAV2
     C                   Z-ADD     0             WAV3
     C                   Z-ADD     0             WAV4
     C                   Z-ADD     0             WAV5
     C                   Z-ADD     0             WAV6
     C                   Z-ADD     0             WAV7
     C                   Z-ADD     0             WAVT
     C                   Z-ADD     0             WAVL
     C                   Z-ADD     0             A1J
     C                   Z-ADD     0             A1E
     C                   Z-ADD     0             A1S
     C                   Z-ADD     0             A1D
     C                   Z-ADD     0             A2J
     C                   Z-ADD     0             A2E
     C                   Z-ADD     0             A2S
     C                   Z-ADD     0             A2D
     C                   Z-ADD     0             A3J
     C                   Z-ADD     0             A3E
     C                   Z-ADD     0             A3S
     C                   Z-ADD     0             A3D
     C                   Z-ADD     0             A4J
     C                   Z-ADD     0             A4E
     C                   Z-ADD     0             A4S
     C                   Z-ADD     0             A4D
     C                   Z-ADD     0             A5J
     C                   Z-ADD     0             A5E
     C                   Z-ADD     0             A5S
     C                   Z-ADD     0             A5D
     C                   Z-ADD     0             XXLPRM           15 4
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-ADD     0             XXLPRM1          15 4
     C                   Z-ADD     0             XXLPRM2          15 4
     C                   Z-ADD     0             XXLPRM3          15 4
     C                   Z-ADD     0             XXLPRM4          15 4
     C                   Z-ADD     0             XXLPRM5          15 4
     C                   Z-ADD     0             XXLPRM6          15 4
     C                   Z-ADD     0             XXLPRM7          15 4
     C*** end add *****  11/12/24  ******************************************
     C                   Z-ADD     0             STOC
     C                   Z-ADD     0             EXPS
     C                   Z-ADD     0             SUB
     C                   Z-ADD     0             MOD
     C                   Z-ADD     0             STD
     C                   Z-ADD     0             NOM
     C                   Z-ADD     0             ANL
     C                   Z-ADD     0             EHCN
     C                   Z-ADD     0             UPSN
     C                   Z-ADD     0             UPFP
     C                   Z-ADD     0             EDC1
     C                   Z-ADD     0             EDC2
     C                   Z-ADD     0             D1E2
     C                   Z-ADD     0             D2E2
     C                   Z-ADD     0             EDC3
     C                   Z-ADD     0             EDV2
     C                   Z-ADD     0             EDCT
     C                   Z-ADD     0             EDD2
     C                   Z-ADD     0             EDCM
     C                   Z-ADD     0             EDCS
     C                   Z-ADD     0             EDC
     C                   Z-ADD     0             EPRM
     C                   Z-ADD     0             EXA1
     C                   Z-ADD     0             EXA2
     C                   Z-ADD     0             EXA3
     C                   Z-ADD     0             ETCA
     C                   Z-ADD     0             ASM1
     C                   Z-ADD     0             ASM2
     C                   Z-ADD     0             ASM3
     C                   Z-ADD     0             ASM4
     C                   Z-ADD     0             ASM5
     C                   Z-ADD     0             ARP1
     C                   Z-ADD     0             ARP2
     C                   Z-ADD     0             ARP3
     C                   Z-ADD     0             DTR
     C                   Z-ADD     0             ETR
     c                   Z-ADD     0             NTR
     c                   Z-ADD     0             LCM
     C                   Z-ADD     0             ERT
     C                   Z-ADD     0             DRT
     C                   Z-ADD     0             ART1
     C                   Z-ADD     0             ART2
     C                   Z-ADD     0             ART3
     C                   Z-ADD     0             ART4
     C                   Z-ADD     0             ART5
     C                   Z-ADD     0             ART6
     C                   Z-ADD     0             ART7
     C                   Z-ADD     0             XXDSPR
     C                   Z-ADD     0             FCL
     C                   Z-ADD     0             XNDISP
      //*** Beg Add ***** 08/14/24 ********************************************
       Clear LMB;
       Clear LMB2;
      //*** End Add ***** 08/14/24 ********************************************
     C                   If        ELMANP <> 0 and *in96 = *on
     C     ELKY4         CHAIN     PLD01                              60
     C                   If        *in60 = *off
     C                   Z-add     0             LDPct1
     C                   Update    pld01
     C                   Endif
     C                   Endif
     C*
     C*
     C* FIND THE GOVERNING CLASS ON THE POLICY.
     C*
     C                   Z-ADD     0             XXGOVC            5 0
     C                   MOVE      *BLANK        XXGOVF            1
     C                   Z-ADD     0             XXGOVP           15 0
     C*
     C* ONLY PROCESS CODE IF EMPLOYER DETAIL IS GOING TO BE UPDATED.
     C*
     C     *IN97         IFEQ      *OFF
      /Free

        //*** begin chg *** 10/19/23  **********************************
        //*****Chain(n) (xxCo#:xxFnd:xxFyr:xxEmp#:'G') pev00;
        Chain(n) (xxCo#:xxFnd:xxFyr:xxEmp#:'G':xxst) pev00;
        //*** end chg ***** 10/19/23  **********************************
        If %found;
           xxGovC = EVxxGovC;
        Else;

      /End-Free
     C*
     C* SET THE GOVERNING CLASS FIELD ON STATE RPT DIVISION ONLY.
     C*
     C*
     C     1             DO        255           Z
     C     Z             OCCUR     XPARA
     C                   CLEAR                   XPARA
     C                   ENDDO
     C*
     C* CHECK ALL PAYROLL RECORDS FOR POLICY YEAR.
     C*
     C                   Z-ADD     XXDIV         SVDIV
     C*
     C     EPKEY3        CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     C                   Z-ADD     EPDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C*
     C* BUILD GOVERNING CLASS OFF ALL CLASSES WITHIN THIS DIVISION'S STATE.
     C*
     C     *IN60         IFEQ      *OFF
     C     RSJRST        ANDEQ     XXST
     C*
     C*  SET MULTI OCCR DS TO MATCHING CLASS ELEMENT OR NEXT AVAILABLE.
     C*
     C     1             DO        255           Z
     C     Z             OCCUR     XPARA
     C* NEXT AVAILABLE
     C     XPCLAS        IFEQ      *ZERO
     C                   MOVE      EPCLAS        XPCLAS
     C                   LEAVE
     C                   ENDIF                                                  XPCLAS IFEQ
     C* MATCHING CLASS
     C     XPCLAS        IFEQ      EPCLAS
     C                   LEAVE
     C                   ENDIF                                                  XPCLAS IFEQ
     C*
     C                   ENDDO                                                  DO Z:1-255
     C*
     C* TOTAL PAYROLL FIELD.
     C*
     C                   ADD       EPAAPR        XPPAY
     C*
     C* CHECK GOVERNING CLASS EXCLUSION FLAG.
     C*
     C                   Z-ADD     XPCLAS        XXCLAS
     C                   Z-ADD     0             X1EFDT
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
     C                   MOVE      ##DATE        X1EFDT
     C*
     C     PRKEY         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      PRGVEX        XPFLAG
     C                   ENDIF
     C*
     C     XPFLAG        IFEQ      *BLANK
     C                   MOVE      'Y'           XXGOVF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  EPSTAT IFEQ
     C*
     C     EPKEY3        READE     PEP01                                  60
     C                   ENDDO
     C*
     C* CHECK FOR CLASS WITH HIGHEST PAYROLL TOTAL.  IGNORE EXCLUDED
     C* CLASSES IF ANY CLASS ON THE POLICY IS NOT EXCLUDED.
     C*
     C     1             DO        255           Z
     C     Z             OCCUR     XPARA
     C*
     C     XXGOVF        IFEQ      *BLANK
     C     XPFLAG        OREQ      *BLANK
     C*
     C     XPPAY         IFGE      XXGOVP
     C     XPCLAS        ANDNE     *ZERO
     C                   Z-ADD     XPCLAS        XXGOVC
     C                   Z-ADD     XPPAY         XXGOVP
     C                   ENDIF                                                  XPPAY IFGT
     C*
     C                   ENDIF                                                  XXGOVF IFEQ
     C*
     C                   ENDDO                                                  Z: 1-255
     C*
     C* RESET EMPLOYER INFORMATION.
     C*
     C                   Z-ADD     SVDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C*
     C     RSKY          CHAIN     PRS01                              60
     C*
      /Free

           Clear pev00;
           EVCo# = xxCo#;
           EVFnd = xxFnd;
           EVFyr = xxFyr;
           EVEmp# = xxEmp#;
           EVType = 'G';
           EVxxGovC = xxGovC;
        //*** begin add *** 10/19/23  **********************************
           EVSt  = xxSt;
        //*** end add ***** 10/19/23  **********************************
           Write pev00;
        Endif;

      /End-Free
     C                   ENDIF                                                  *IN97 IFEQ
     C*
     C                   Z-ADD     XXDIV         X1DIV             5 0
     C                   Z-ADD     0             X1PFDT            7 0
     C                   Z-ADD     ELPFDT        XSPFDT            7 0
     C                   Z-ADD     0             X1ANDT            7 0
     C                   MOVE      *OFF          *IN61
     C                   If        xxdiv <> xsdiv
     C                   Z-ADD     XSDIV         XXDIV
     C                   Z-ADD     XSDIV         SV1DIV            5 0
     C     ELKEY         CHAIN     PEL01                              60
     C                   If        *in60 = *off
     C                   Z-add     ELPFDT        XSPFDT
     C                   MOVE      *ON           *IN61
     C                   Z-ADD     ELPFDT        X1PFDT
     C                   Z-ADD     ELANDT        X1ANDT
     C                   Endif
     C                   Z-ADD     X1DIV         XXDIV
     C                   Endif
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C                   Z-ADD     SV1DIV        XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE(P)   ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        SV1ST
     C*
     C* RESET EMPLOYER INFORMATION
     C*
     C                   Z-ADD     X1DIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              60
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C* DETERMINE IF STATE REPORTING DIV INFO MUST BE USED FOR RATES
     C*
     C                   Z-ADD     0             XXSTDT            7 0
     C*
     C     *IN61         IFEQ      *ON
     C     ELANDT        ANDEQ     *ZERO
     C*
     C     X1ANDT        IFNE      *ZERO
     C                   ADD       10000         X1ANDT
     C     X1ANDT        IFLE      ELPFDT
     C                   Z-ADD     X1ANDT        XXSTDT
     C                   ELSE
     C     X1ANDT        SUB       10000         XXSTDT
     C                   ENDIF
     C                   ENDIF
     C*
     C     XXSTDT        IFEQ      *ZERO
     C     X1PFDT        IFLT      ELPFDT
     C                   Z-ADD     X1PFDT        XXSTDT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  *IN61 IFEQ
     C*
     C*   INITIALIZE ADDITIONAL LOAD FIELD
     C*
     C                   Z-ADD     0             XXADLD           15 4
     C                   Z-ADD     0             ADL
     C                   Z-ADD     0             ADL2
     C                   Z-ADD     0             ADL3
     C*
     C*   LOAD DEDUCTIBLE DISCOUNT RATE FROM TABLE FILE
     C*
     C                   MOVE      *ZERO         ELDDCT
     C                   MOVE      *ZERO         XXDDCT
     C                   MOVE      *ZERO         ELDD2P
     C                   MOVE      *ZERO         XXDD2P
     C*
     C                   MOVE      *BLANK        XXLRGD            1
     C*
     C*   VERIFY IF POLICY IS LARGE DEDUCTIBLE BASED ON BILLING SCHEDULE CODE
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C*
     C     *IN60         IFEQ      *ZERO
     C     BSLPGM        ANDEQ     'L'
     C*
     C     ELKY4         CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           XXLRGD
      **** Beg add ***** 06/11/24 **************************
     C                   MOVE      'Y'           XXDEDF
      **** End add ***** 06/11/24 **************************
     C*** begin add ***  09/24/24  *****************************************
     C                   Z-add     LDPCT1        xxddct
     C*** end add *****  09/24/24  *****************************************
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
     C                   GOTO      READE
     C                   ENDIF
      *
     C                   ENDIF
      *
     C                   ELSE
     C     *LIKE         DEFINE    DDCODE        XXDDCD
     C*
     C                   MOVEL(P)  ELDCDE        XXDDCD
     C                   MOVE      ELDCEF        Q1DATE7
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
     C                   PARM                    Q1DATE7           7
     C*
     C                   MOVE      Q1DATE7       XXDDT
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY2         READPE    PDD01                                  60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         ELDDCT
     C                   MOVE      *ZERO         XXDDCT
     C                   ELSE
     C     DDSTAT        IFEQ      'A'
     C                   MOVE      DDPCT         ELDDCT
     C                   MOVE      DDPCT         XXDDCT
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL(P)  ELDCD2        XXDDCD
     C                   MOVE      EL2DEF        Q1DATE7
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
     C                   PARM                    Q1DATE7           7
     C*
     C                   MOVE      Q1DATE7       XXDDT
     C*
     C     DDKY          SETGT     PDD01
     C     DDKY2         READPE    PDD01                                  60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ZERO         ELDD2P
     C                   MOVE      *ZERO         XXDD2P
     C                   ELSE
     C     DDSTAT        IFEQ      'A'
     C                   MOVE      DDPCT         ELDD2P
     C                   MOVE      DDPCT         XXDD2P
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C*   LOAD POLICY FROM DATE INTO ARRAY AS 1ST PAYROLL DATE
     C*
     C                   Z-ADD     ELPFDT        PDAT(1)
     C                   Z-ADD     ELPFDT        RDAT(1)
     C     XXSTDT        IFNE      *ZERO
     C                   Z-ADD     XXSTDT        RDAT(1)
     C                   ENDIF
     C                   Z-ADD     RDAT(1)       FDAT(1)
     C*
     C*   USE ANNIVERSARY DATE TO RETRIEVE RATE IF NOT = 0
     C*
     C                   MOVE      '0'           *IN77
     C                   MOVEA     '11'          *IN(78)
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
     C     XXANDT        IFGE      ELPFDT
     C                   MOVE      ELANDT        RDAT(1)
     C                   Z-ADD     RDAT(1)       FDAT(1)
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
     C*
     C     *IN78         IFEQ      '0'
     C                   Z-ADD     XXANDT        PDAT(X)
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C                   ADD       1             X
     C                   ENDIF                                                    *IN78='0'
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     ELPTDT        PDAT(X)
     C                   MOVE      *OFF          *IN75
     C                   ADD       1             X
     C*
     C* CHECK IF ANY POLICY PERIODS NEED TO BE SPLIT FOR 10/1/3 RATES.
     C*
     C     XXST          IFEQ      'FL'
     C*
     C                   DO        8             W                 2 0
     C     W             ADD       1             Z
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       IFLT      1031001
     C     PDAT(W)       ANDLT     1031001
     C     PDAT(Z)       ANDGT     1031001
     C*
     C                   Z-ADD     1031001       PDAT(X)
     C                   Z-ADD     1031001       RDAT(X)
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   MOVE      *ON           *IN75
     C                   ADD       1             X
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 4 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* SORT DATE ARRAYS INTO SEQUENCE.
     C*
     C                   SORTA     PDAT
     C                   SORTA     RDAT
     C                   SORTA     FDAT
     C                   MOVE      *OFF          *IN76
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
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           *IN76
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 4 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* FL RATE SPLIT FOR 07/01/09
     C*
     C                   MOVE      *OFF          XX0709            1
     C     XXST          IFEQ      'FL'
     C     XXCLT#        ANDEQ     '139'
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
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XX0709            1
     C                   ENDIF                                                  RDAT,W IFGE
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 8 X
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
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XXDEFL
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
     C                   Z-ADD     RDAT(W)       FDAT(X)
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
     C*
     C                   eval      pdat(x) = xxSpltDt
     C                   eval      rdat(x) = xxSpltDt
     C                   eval      fdat(x) = rdat(w)
     C                   eval      x = x + 1
     C                   eval      xxOK10 = *on
     C*
     C                   endif                                                  RDAT,W IFGE
     C*
     C                   enddo                                                  DO 6 X
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
     C                   eval      rdat(x) = pdat(w)
     C                   eval      fdat(x) = pdat(w)
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
     C                   eval      rdat(x) = pdat(w)
     C                   eval      fdat(x) = pdat(w)
     C                   eval      x = x + 1
     C                   leave
     C                   endif                                                  pdat(W)
     C                   enddo                                                  DO 8 X
     C                   endif                                                  ApplyRateSplit
     C                   endif                                                  xxst

     C                   Z-ADD     0             XXRDT1
     C                   Z-ADD     0             XXRDT2
     C     *IN75         IFEQ      *ON
     C                   Z-ADD     1031001       XXRDT2            7 0
     C                   ENDIF
     C     *IN76         IFEQ      *ON
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
     C                   Z-ADD     xxSpltDt      xxrdt1                         SPLIT DATE
     C                   endif                                                  XXOK10 = *ON
      /free
       If isOK14 or isTN14;
           xxrdt1 = xxSpltDt;
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
     C     *IN77         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        RDAT(X)
     C                   ELSE
     C     ELEMD2        IFLT      XXANDT
     C                   Z-ADD     ELANDT        RDAT(X)
     C                   ELSE
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C     *IN76         IFEQ      *ON
     C     ELEMD2        ANDGE     1030401
     C     RDAT(X)       ANDLT     1030401
     C                   Z-ADD     1030401       RDAT(X)
     C                   ENDIF
     C     *IN75         IFEQ      *ON
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
     C* OK rate split 09/01/10
     C                   if        xxOK10 = *on and
     C                             elemd2 >= xxSpltDt and
     C                             rdat(x) < xxSpltDt
     C                   Z-ADD     xxSpltDt      RDAT(X)
     C                   endif                                                  XXOK10 = *ON
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
     C     *IN77         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        RDAT(X)
     C                   ELSE
     C     ELEMD3        IFLT      XXANDT
     C                   Z-ADD     ELANDT        RDAT(X)
     C                   ELSE
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C     *IN76         IFEQ      *ON
     C     ELEMD3        ANDGE     1030401
     C     RDAT(X)       ANDLT     1030401
     C                   Z-ADD     1030401       RDAT(X)
     C                   ENDIF
     C     *IN75         IFEQ      *ON
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
     C* OK rate split 09/01/10
     C                   if        xxOK10 = *on and
     C                             elemd3 >= xxSpltDt and
     C                             rdat(x) < xxSpltDt
     C                   Z-ADD     xxSpltDt      RDAT(X)
     C                   endif                                                  XXOK10 = *ON
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
     C                   SORTA     FDAT
     C*
     C                   DO        8             X
     C*
     C     PDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         PDAT(X)
     C                   ENDIF
     C     RDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         RDAT(X)
     C                   ENDIF
     C     FDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         FDAT(X)
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  GET ACTUAL NUMBER OF DAYS IN EACH ANNIV RATING PERIOD
     C*
     C                   Z-ADD     *ZERO         DYS
     C*
     C                   Z-ADD     1             X
     C*
     C     RDAT(X)       DOWNE     0
     C     X             ADD       1             Z                 3 0
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
     C     PDAYS         IFNE      0
     C     DYS(X)        DIV(H)    PDAYS         PRO(X)
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   END
     C*
     C                   XFOOT     PRO           XXWK54            5 4
     C     1             SUB       XXWK54        XXWK54
     C                   ADD       XXWK54        PRO(1)
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
     C                   Z-ADD     010194        XQTO
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C*
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
     C* LOAD DEVIATION ARRAY FOR MA DIA BREAKOUT
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
     C                   Z-ADD     RDAT(X)       XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C     RRRDEV        MULT(H)   .01           XXRDEV            5 4
     C     1             SUB       XXRDEV        XXRDEV
     C                   Z-ADD     XXRDEV        RDEV(X)
     C                   ADD       1             X
     C                   ENDDO

      * Load up terrorism rates into arrays by FDAT
      /free
       x = 1;
       Dow fDat(x) <> 0;
          xxEfDt = fDat(x);
          If xxSt = 'NJ';
            xxEfDt = xspfdt;
          Endif;
          Setgt rrKy prr01;
          Readpe rrky1 prr01;
          If not(%eof);
            pyTRt(x) = RRterr;      //Payroll based foreign terrorism rate
            dmTrt(x) = RRdtrr;      //Domestic terrorism rate
            npTRt(x) = RRter2;      //Non-payroll based foreign terrorism rate
          Endif;
          x += 1;
       Enddo;
      /end-free

     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C*  IF POLICY IS ANNIVERSARY RATED AND HAS A DEDUCTIBLE DISCOUNT,
     C*  COPY THAT DISCOUNT TO 2ND DEDUCTIBLE FOR RATING PURPOSES IF
     C*  THERE IS NO 2ND DEDUCTIBLE ALREADY.
     C*
     C     *IN78         IFEQ      '0'
     C     ELDCDE        ANDNE     *BLANKS
     C     ELDCD2        ANDEQ     *BLANKS
     C*
     C* FIRST DEDUCTIBLE IS EFFECTIVE BEFORE THE SPLIT.
     C*
     C     ELDCEF        IFLT      XXANDT
     C     ELDCEF        OREQ      *ZERO
     C*
     C* FIRST DEDUCTIBLE DOES NOT EXPIRE BEFORE SPLIT.
     C*
     C     ELDCEX        IFGT      XXANDT
     C     ELDCEX        OREQ      *ZERO
     C*
     C                   MOVE      ELDCDE        ELDCD2
     C                   MOVE      XXANDT        EL2DEF
     C*
     C* UPDATE 2ND DEDUCTIBLE WITH EXPIRATION DATE OF 1ST DEDUCTIBLE.
     C*
     C     ELDCEX        IFGT      XXANDT
     C                   MOVE      ELDCEX        EL2DEX
     C                   ENDIF
     C                   MOVE      XXANDT        ELDCEX
     C*
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C*
     C* UPDATE EMPLOYER DETAIL AND RESET FILE.
     C*
     C                   UPDATE    PEL01
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C                   ENDIF                                                  ELDCEX IFGT
     C*
     C                   ENDIF                                                  ELDCEF IFLT
     C*
     C                   ENDIF                                                  *IN78 IFEQ
     C*
     C*
     C*  READ EMPLOYER PAYROLL RECORDS
     C*
     C                   Z-ADD     0             XXFCLS           15 4
     C                   Z-ADD     0             XXPREM
     C                   Z-ADD     0             PAY
     C                   Z-ADD     0             PAY2
     C                   MOVE      *OFF          XXFCFL            1            F-CLASS FLAG
      *
      *  CALCULATE FL RT CHG AMOUNT FOR 07/01/09
      *
     C                   CLEAR                   XXFLRC                         FL RT CHG FLG
     C     XXST          IFEQ      'FL'
     C     XXCLT#        ANDNE     '139'
     C                   SELECT
     C     ELPFDT        WHENLT    1090701                                      LOOK AT SPLIT DATE
     C     XXANDT        ANDGE     1090401
     C     XXANDT        ANDLT     1090701
     C     ELPTDT        ANDGT     1090701
     C                   MOVE      'Y'           XXFLRC            1
     C     ELANDT        WHENGE    1090401                                      LOOK AT ANDT
     C     ELANDT        ANDLT     1090701
     C     ELPFDT        ANDLT     1090701
     C     ELPTDT        ANDGT     1090701
     C                   MOVE      'Y'           XXFLRC
     C     ELPFDT        WHENGE    1090401                                      LOOK AT PFDT
     C     ELPFDT        ANDLT     1090701
     C     ELPTDT        ANDGT     1090701
     C     ELANDT        ANDEQ     *ZERO
     C                   MOVE      'Y'           XXFLRC
     C                   ENDSL
     C     XXFLRC        IFEQ      'Y'
     C                   Z-ADD     7012009       QQFROM
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQTO
     C                   Z-ADD     *ZERO         QQDAYS
     C                   MOVE      *BLANK        QQERR
     C                   MOVE      '1'           QQTERM
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C                   EXSR      SRLEAP
     C                   Z-ADD     QQDAYS        UDAYS             5 0          #OF DAYS TO PRORATE
     C     UDAYS         DIV(H)    365           X2FACT            7 6          PRORATE FACTOR
     C     X2FACT        MULT(H)   .06           X3FACT            5 4
     C     1             SUB       X3FACT        X3FACT                         NON F-CLASS ADJ
     C     X2FACT        MULT(H)   .02           X4FACT            5 4
     C     1             SUB       X4FACT        X4FACT                         F-CLASS ADJ
     C                   ENDIF                                                  XXFLRC
     C                   ENDIF                                                  XXST = 'FL
     c                   move      *blank        xxnetf            1
     C*
     c                   if        xxclt# = '025'
     C*
     C*  CALCULATE THE NET RATE AND NET PREMIUM FOR EACH PAYROLL
     C*  CLASS, AND THEN OVERRIDE NORMAL PREMIUM WITH THE TOTAL.
     C*
     C     F1NETR        IFEQ      'Y'
     C     ELPFDT        ANDGE     F1NETD
     C     *IN94         ANDEQ     *OFF
     C     *IN95         ANDEQ     *OFF
     C     *IN96         ANDEQ     *OFF
     C     *IN97         ANDEQ     *OFF
     C*
     c                   move      'Y'           xxnetf
     C                   Z-ADD     0             XXTMAN           11 2
     C                   Z-ADD     0             XXANMP           11 2
     C                   Z-ADD     0             XXTNET           11 2
     C*
     C* TOTAL THE MANUAL AND NORMAL PREMIUM FOR THE STATE.
     C*
     C     DSKY          CHAIN(N)  PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     MPST          IFEQ      XXST
     C                   ADD       MPAMPR        XXTMAN
     C                   ADD       MPANMP        XXANMP
     C                   ENDIF
     C*
     C     DSKY          READE(N)  PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* CALCULATE THE NET RATE FACTOR.
     C*
     C     XXTMAN        IFNE      *ZERO
     C     XXANMP        DIV(H)    XXTMAN        XXNETR            9 6
     C                   ELSE
     C                   Z-ADD     1             XXNETR
     C                   ENDIF                                                  XXTMAN IFNE
     c
     c                   endif                                                  if f1netr = 'Y'
     c
     c                   endif                                                  if xxclt# = '025'
      /free
       // Get the Territorial Differential Factor
       GetTDF(ELco#:ELfnd:ELemp#:ELdiv:ELfYr:outTDF);
      /end-free
     C                   SETOFF                                       50
     C     EPKEY         SETGT     PEP01
     C     *IN50         DOUEQ     '1'
     C                   READP     PEP01                                  50
     C  N50EPCO#         COMP      XXCO#                              5050
     C  N50EPFND         COMP      XXFND                              5050
     C  N50EPFYR         COMP      XXFYR                              5050
     C  N50EPEMP#        COMP      XXEMP#                             5050
     C  N50EPDIV         COMP      XXDIV                              5050
     C*
     C*  END DATE IS EITHER END OF POLICY OR EFF.DATE FROM LAST RCD
     C*
     C     *IN50         IFEQ      '0'
     C     EPSTAT        ANDEQ     'A'                                          RLS 2  JJJ
     C     EPSAFL        ANDEQ     'A'                                          |  7-7-88
     C*
     C                   Z-ADD     EPCLAS        XXCLAS
     C     EPEFDT        IFGT      SVEFDT                                       |
     C                   Z-ADD     EPEFDT        XHEFDT                         |
     C                   END                                                    |
     C*
     C*  GET CORRECT DATE FROM RDAT ARRAY TO PULL RATE FROM RATE FILE
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    PDAT(X)                            60
     C     *IN60         IFEQ      '1'
     C     X             ANDNE     1
     C                   SUB       1             X
     C                   Z-ADD     RDAT(X)       XXEFDT
     C                   ELSE
     C                   Z-ADD     1             X
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   END
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   Z-ADD     0             X1EFDT
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
     C                   MOVE      ##DATE        X1EFDT
     C*
     C*  GET PAYROLL CLASS FILE
     C*
     C     PRKEY         SETGT     PPR01
     C                   READP     PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRCO#         COMP      XXCO#                              6060
     C  N60PRCLAS        COMP      XXCLAS                             6060
     C  N60PRRTCD        COMP      XXRTCD                             6060
     C                   END
     C*
     C                   Z-ADD     0             EPANTR
     C                   Z-ADD     0             EPANTP
     C     *IN60         IFEQ      '1'
     C                   CLEAR                   PPR01
     C                   Z-ADD     *ZERO         EPARAT
     C                   Z-ADD     *ZERO         EPAMPR
     C                   UPDATE    PEP01
     C                   END
     C*
     C     *IN60         CABEQ     '1'           NEXT1
     C*
     C*  AIRCRAFT SURCHARGE - SHOULD BE ADDED TO ADDITIONAL LOAD TO
     C*  THEN BE APPLIED TO STANDARD RATHER THAN MANUAL.
     C*
     C     EPCLAS        IFEQ      RRDCCD
     C     PROCCD        ANDEQ     *BLANK
     C     EPAAPR        MULT      PRRT          CHARGE           15 4
     C                   Z-ADD     CHARGE        CHARGD           15 4
     C     PRRT          MULT(H)   RDEV(X)       EPARAT
     C     CHARGE        MULT(H)   RDEV(X)       CHARGE
     c*** begin add ***  06/25/25  *****************************************
     C                   If        RS3Date = 0 or RDAT(1) < RS3Date
     C     PRRT          MULT(H)   RDEV(X)       x2arat
     C                   Z-add     x2arat        EParat
     C     EPEAPR        MULT      PRRT          CHARGE           15 4
     C                   Z-ADD     CHARGE        CHARGD           15 4
     C     CHARGE        MULT(H)   RDEV(X)       CHARGE
     C                   Endif
     c*** end add *****  06/25/25  *****************************************
     C     CHARGE        IFGT      RRAMAX
     C                   Z-ADD     RRAMAX        EPADLD
     C                   ELSE
     C                   Z-ADD     CHARGE        EPADLD
     C                   ENDIF                                                  CHARGE GT
     C     CHARGD        IFGT      RRAMAX
     C                   Z-ADD     RRAMAX        CHARGD
     C                   ENDIF
     C*
     C* PRORATE THE CHARGE
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    PDAT(X)                            60
     C     *IN60         IFEQ      '1'
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ELSE
     C                   Z-ADD     1             X
     C                   ENDIF
     C*
     C     PDAYS         IFNE      *ZEROS
     C     DYS(X)        DIV(H)    PDAYS         XXDPRO
     C                   ELSE
     C                   Z-ADD     0             XXDPRO
     C                   ENDIF                                                  DYS,X IFNE
     C*
     C     XXDPRO        MULT(H)   EPADLD        XXWK10
     C                   Z-ADD     XXWK10        EPADLD
     C*
     C                   Z-ADD     0             EPEMPR
     C                   Z-ADD     0             EPAMPR
     C                   UPDATE    PEP01
     C                   ADD       EPADLD        XXADLD
     C                   ADD       EPADLD        ADL(X)
     C                   ADD       EPADLD        PAY2(X)
     C     XXDPRO        MULT(H)   CHARGD        XXWK10           11 0
     C                   Z-ADD     XXWK10        CHARGD
     C                   ADD       CHARGD        ADLD(X)
     C                   GOTO      NEXT1
     C                   ENDIF                                                  EPCLAS EQ
     C*
     C* IF THIS IS A POPULATION RATED CLASS, PULL PREMIUM AMOUNT FROM
     C* TABLE, UPDATE, AND THEN SKIP TO NEXT.
     C*
     C     PRPCF         IFEQ      'O'
     C*
     C     POKY          SETGT     PPO01
     C     POKY1         READPE    PPO01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     POAMT         EPAMPR
     C                   ELSE
     C                   Z-ADD     0             EPAMPR
     C                   ENDIF
     C*
     C* PRORATE THE CHARGE
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    PDAT(X)                            60
     C     *IN60         IFEQ      '1'
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   ELSE
     C                   Z-ADD     1             X
     C                   ENDIF
     C*
     C     PDAYS         IFNE      *ZEROS
     C     DYS(X)        DIV(H)    PDAYS         XXDPRO
     C                   ELSE
     C                   Z-ADD     0             XXDPRO
     C                   ENDIF                                                  DYS,X IFNE
     C*
     C     XXDPRO        MULT(H)   EPAMPR        XXWK10           11 0
     C                   Z-ADD     XXWK10        EPAMPR
     C*
     C     PDAYS         IFNE      *ZERO
     C     PDAYS         ANDNE     365
     C*
     C     PDAYS         DIV       365           X1FACT            9 6
     C     EPAMPR        MULT(H)   X1FACT        XXWK10
     C                   Z-ADD     XXWK10        EPAMPR
     C*
     C                   ENDIF
     C*
      *
      *  STORE BASE PREM IN MAND FOR MA DIA BREAKOUT LATER. APPLY DEVIATION
      *  TO MAN PREM FOR POPULATION RATED CLASS.
      *
     C                   ADD       EPAMPR        MAND(X)
     C     EPAMPR        MULT      RDEV(X)       XXWK10
     C                   Z-ADD     XXWK10        EPAMPR
     c                   if        xxnetf = 'Y'
     c
     C     XXNETR        MULT(H)   EPAMPR        XXWK10
     C                   Z-ADD     XXWK10        EPANTP
     c
     c                   endif                                                  if xxnetf = 'Y'
     C*
     C                   UPDATE    PEP01
     C                   ADD       EPAMPR        MAN(X)
     C                   ADD       EPAMPR        PAY2(X)
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   PRRTCD        QQRTCD
     C                   MOVE(P)   PRCLAS        QQCLAS
     C                   MOVE(P)   XXEFDT        QQEFDT
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
      /free
       // Calculate non-payroll terrorism charge
       If RRterl <> 'Y';
          Eval(h) xxTert = npTRt(x) * xxLcm;
       Else;
          xxTert = npTRt(x);
       Endif;
       Eval(h) xxDtra = EPempr * xxTert;
       //Apply Excess premium reduction % (Deviation #1) against Terrirism chrages
       If FNExIns = 'Y' and ELASP2 > 0;
          Eval(h) xxDtra = xxDtra - (xxDtra * ELASP2/100);
       Endif;
       nTr(x) = nTr(x) + xxDtra;
      /end-free
     C*
     C                   GOTO      NEXT1
     C                   ENDIF                                                  PRPCF IFEQ
     C*
     C*  OCCUPATION DISEASE CLASS - SHOULD BE ADDED BEFORE STANDARD
     C*  PREMIUM IN A SECONDARY ADDITIONAL LOAD ARRAY.
     C*
     C     PROCCD        IFEQ      'Y'
     C     EPAAPR        MULT      PRRT          CHARGE           15 4
     C                   DIV       100           CHARGE
     C                   Z-ADD     CHARGE        EPADLD
     C*
     C                   Z-ADD     0             EPEMPR
     C                   Z-ADD     0             EPAMPR
     C                   UPDATE    PEP01
     C                   ADD       EPADLD        XXADLD
     C                   ADD       EPADLD        ADL2(X)
     C                   GOTO      NEXT1
     C                   ENDIF                                                  EPCLAS EQ
     C*
     C*  Non-Ratable class codes - should be added before standard
     C*  premium in a secondary additional load array.
     C*
     C     PRnrat        IFEQ      'Y'
     C     EPAAPR        MULT      PRRT          CHARGE           15 4
     C                   DIV       100           CHARGE
     C                   Z-ADD     CHARGE        EPADLD
     C*
     C                   Z-ADD     0             EPEMPR
     C                   Z-ADD     0             EPAMPR
     C                   UPDATE    PEP01
     C                   ADD       EPADLD        XXADLD
     C                   ADD       EPADLD        ADL3(X)
     C                   GOTO      NEXT1
     C                   ENDIF                                                  EPCLAS EQ
     C*
     C*  IF THIS IS AN "A" RATING EMPLOYER, USE EPARAT OTHERWISE
     C*  RETRIEVE RATE FROM WMPRP AND ADJUST IF NECESSARY
     C*
     C     ELSTAT        IFEQ      'Y'
     C     PRARFL        ANDEQ     'Y'
     C                   Z-ADD     EPARAT        XXARAT
     C                   ELSE
     C*
     C*  ADJUST RATE IF NECESSARY
     C*
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   ELRTCD        QQRTCD
     C                   MOVE(P)   PRCLAS        QQCLAS
     C                   MOVE(P)   XXEFDT        QQEFDT
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
      /free
       // Apply Territorial Differential Factor
      /end-free
     C**************     If        outTDF = 0
     C**************     Eval(h)   prrt = xxlcm * prrt
     C**************     Else
     C**************     Eval(h)   prrt = outTDF * xxlcm * prrt
     C**************     Endif
     C*** begin add ***  05/24/23  **************************************
     C*** begin chg ***  09/13/23  **************************************
     C*****PRRT          MULT(H)   XXLCM         PRRT
     c*** begin add ***  08/14/25  *****************************************
     C* Round/calculate payroll rate to 2 digits if not approved yet for 3.
     C                   If        RS3Date = 0 or RDAT(1) < RS3Date
     C                   If        outTDF = 0
     C                   Eval(h)   x2arat = xxlcm * prrt
     C                   Else
     C                   Eval(h)   x2arat = outTDF * xxlcm * prrt
     C                   Endif
     C                   Z-add     x2arat        PRRT
     C                   Else
     c*** end add *****  08/14/25  *****************************************
     C                   If        outTDF = 0
     C                   Eval(h)   prrt = xxlcm * prrt
     C                   Else
     C                   Eval(h)   prrt = outTDF * xxlcm * prrt
     C                   Endif
     C*** end chg *****  09/13/23  **************************************
     C*** end add *****  05/24/23  **************************************
     c*** begin add ***  06/25/25  *****************************************
     c*** begin del ***  08/14/25  *****************************************
     C******             If        RS3Date = 0 or RDAT(1) < RS3Date
     C******             Z-add(h)  PRRT          x2arat
     C******             Z-add     x2arat        PRRT
     c*** end del *****  08/14/25  *****************************************
     C                   Endif
     c*** end add *****  06/25/25  *****************************************
     C                   Z-ADD     PRRT          XXARAT
      /free
       // Reduce 07/01/14 payrolls by 5.9% for TN rate split
       If isTN14 and EPefdt = xxSpltDt;
          If isDiscounted('TN':EPclas:EPspec);
             Eval(h) xxArat = xxArat * .941;
          Endif;
       Endif;
      /end-free
      /free
       // Reduce 07/01/14 payrolls by 12.9% for OK rate split
       If isOK14 and EPefdt = xxSpltDt;
          If isDiscounted('OK':EPclas:EPspec);
             Eval(h) xxArat = xxArat * .871;
          Endif;
       Endif;
      /end-free
      *
      *  STORE ORIGINAL RATE IN X1ARAT FOR MA DIA ASSESSMENT CALC
      *
     C*** Begin Add ***  06/17/24  **************************************
     C****************** Z-ADD     XXARAT        X1ARAT            5 2
     C                   Z-ADD     XXARAT        X1ARAT            8 3
     C*** End   Add ***  06/17/24  **************************************
     c*** begin add ***  08/14/25  *****************************************
     C* Round/calculate payroll rate to 2 digits if not approved yet for 3.
     C                   If        RS3Date = 0 or RDAT(1) < RS3Date
     C     XXARAT        MULT(H)   RDEV(X)       x2arat
     C                   Z-add     x2arat        xxarat
     C                   Else
     c*** end add *****  08/14/25  *****************************************
     C     XXARAT        MULT(H)   RDEV(X)       XXARAT
     c*** begin add ***  06/25/25  *****************************************
     c*** begin del ***  08/14/25  *****************************************
     C******             If        RS3Date = 0 or RDAT(1) < RS3Date
     C******             Z-add(h)  xxarat        x2arat
     C******             Z-add     x2arat        xxarat
     c*** end del *****  08/14/25  *****************************************
     C                   Endif
     c*** end add *****  06/25/25  *****************************************
     C     EPSPEC        IFEQ      'F'
     C     PRLHCD        IFEQ      'N'
     C                   MOVE      ELRTCD        XXRTCD
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     HEKY          SETGT     PHE01
     C     HEKY2         READPE    PHE01                                  60
     C     *IN60         IFEQ      *OFF
     c*** begin add ***  08/14/25  *****************************************
     C* Round/calculate payroll rate to 2 digits if not approved yet for 3.
     C                   If        RS3Date = 0 or RDAT(1) < RS3Date
     C     XXARAT        MULT      HELHNF        x2arat
     C                   Z-add     x2arat        xxarat
     C     X1ARAT        MULT      HELHNF        x2arat
     C                   Z-add     x2arat        x1arat
     C                   Else
     c*** end add *****  08/14/25  *****************************************
     C     XXARAT        MULT      HELHNF        XXARAT
     C     X1ARAT        MULT      HELHNF        X1ARAT
     c*** begin add ***  06/25/25  *****************************************
     c*** begin del ***  08/14/25  *****************************************
     C******             If        RS3Date = 0 or RDAT(1) < RS3Date
     C******             Z-add(h)  xxarat        x2arat
     C******             Z-add     x2arat        xxarat
     C******             Z-add(h)  x1arat        x2arat
     C******             Z-add     x2arat        x1arat
     c*** end del *****  08/14/25  *****************************************
     C                   Endif
     c*** end add *****  06/25/25  *****************************************
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  PRLHCD IFEQ
     C                   ENDIF                                                  EPSPEC IFEQ
     C                   ENDIF                                                  ELSTAT IFEQ
      *
      * APPLY FL RATE DECREASE
      *
     C     XXFLRC        IFEQ      'Y'
     c*** begin add ***  08/14/25  *****************************************
     C* Round/calculate payroll rate to 2 digits if not approved yet for 3.
     C                   If        RS3Date = 0 or RDAT(1) < RS3Date
     C     EPSPEC        IFEQ      'F'
     C     XXARAT        MULT(H)   X4FACT        x2arat
     C                   ELSE
     C     XXARAT        MULT(H)   X3FACT        x2arat
     C                   ENDIF
     C                   Z-add     x2arat        xxarat
     C                   Else
     c*** end add *****  08/14/25  *****************************************
     C     EPSPEC        IFEQ      'F'
     C     XXARAT        MULT(H)   X4FACT        XXARAT
     C                   ELSE
     C     XXARAT        MULT(H)   X3FACT        XXARAT
     C                   ENDIF
     c*** begin add ***  06/25/25  *****************************************
     c*** begin del ***  08/14/25  *****************************************
     C******             If        RS3Date = 0 or RDAT(1) < RS3Date
     C******             Z-add(h)  xxarat        x2arat
     C******             Z-add     x2arat        xxarat
     c*** end del *****  08/14/25  *****************************************
     C                   Endif
     c*** end add *****  06/25/25  *****************************************
     C                   ENDIF
     c                   if        xxnetf = 'Y'
     C     XXNETR        MULT(H)   XXARAT        EPANTR
     c                   endif


     C*
     C*  CALCULATE MANUAL PREMIUM
     C*
     C     PRPCF         IFEQ      'P'
     C     EPAAPR        DIV       100           XXWK92           15 4
     C                   ELSE
     C                   Z-ADD     EPAAPR        XXWK92           15 4
     C                   END
     C*
     C*  ROUND TO NEAREST DOLLAR, UPDATE EMPLOYER PAYROLL FILE
     C*
      *
      * Ambulance payclass in NY - full charge for first person, 1/2 charge for
      * additional people
      *
     C     XXST          IFEQ      'NY'
     C     EPCLAS        ANDEQ     7370
     C                   Z-ADD     EPAAPR        XXWK92
     C                   SUB       1             XXWK92
     C     XXWK92        MULT      XXARAT        XXAMBC           15 4
     C                   DIV(H)    2             XXAMBC
     C     XXAMBC        ADD       XXARAT        XXMAN
     c                   if        xxnetf = 'Y'
     C                   Z-ADD     EPAAPR        XXWK92
     C                   SUB       1             XXWK92
     C     XXWK92        MULT      EPANTR        XXAMBC           15 4
     C                   DIV(H)    2             XXAMBC
     C     XXAMBC        ADD       EPANTR        XXTEMP           15 4
     c                   z-add(h)  XXTEMP        XXWK10
     C                   Z-ADD     XXWK10        EPANTP
     c                   endif
     C                   ELSE
     C     XXWK92        MULT      XXARAT        XXMAN            15 4
     C     XXWK92        MULT(H)   X1ARAT        XXMAND           15 4
     c                   if        xxnetf = 'Y'
     C     XXWK92        MULT(H)   EPANTR        XXWK10
     C                   Z-ADD     XXWK10        EPANTP
     c                   endif
     C                   ENDIF
     C                   MOVE      *BLANKS       XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      XXST          XPST
     C     PXKY          CHAIN     PPX01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             PXCODE
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     PXCODE        IFEQ      *ZERO
     C                   ADD       EPADLD        XXADLD
     C                   ADD       EPADLD        ADL(X)
     C                   ELSE
     C                   ADD       EPADLD        MAN(X)
     C                   ADD       CHARGD        MAND(X)
     C                   ENDIF                                                  PXCODE IFEQ
     C*
     C*  SAVE HIGHEST FOR MINIMUM PREMIUM CHECK
     C*
     C     PRMNPR        IFGT      XXHIGH
     C     EPAAPR        ANDNE     0
     C     PRMNPR        ORGT      XXHIGH
     C     SV1ST         ANDEQ     'NJ'
     C     PRMNPR        ORGT      XXHIGH
     C     SV1ST         ANDEQ     'PA'
     C     PRMNPR        ORGT      XXHIGH
     C     SV1ST         ANDEQ     'NY'
     C                   Z-ADD     PRMNPR        XXHIGH           15 4
     C                   MOVE      PRCLAS        SVCLAS            5 0
     C                   END
     C*
     C*  APPLY 15.7% CREDIT TO PORTION OF MANUAL USING 1993 RATES;
     C*  GET CORRECT DATE FROM RDAT ARRAY TO DETERMINE RATING DATE USED
     C*  TO PULL RATE FROM FILE
     C*
     C                   Z-ADD     *ZEROS        XXDISC
     C*
     C*  15.7% DISCOUNT IS APPLIED TO PORTION OF 94 POLICY USING
     C*  93 RATES
     C*
     C     XX157         IFEQ      'Y'
     C                   Z-ADD     1             Z                 3 0
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
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD(H)  XXMAN         XXSAVE            9 0
     C                   Z-ADD     XXSAVE        XXMAN
     C*
     C     XXMAN         MULT(H)   XXWO54        XXDISC           15 4
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD(H)  XXDISC        XXSAVE
     C                   Z-ADD     XXSAVE        XXDISC
     C*
     C                   END                                                    DY3,Z <> 0
     C*
     C                   END                                                    XX157 = 'Y'
     C*
     C*  ROUND TO NEAREST DOLLAR, UPDATE EMPLOYER PAYROLL FILE
     C*
     C                   Z-ADD(H)  XXMAN         XXSAVE            9 0
     C                   Z-ADD     XXSAVE        XXMAN
     C*
     C                   SUB       XXDISC        XXMAN
     C                   Z-ADD(H)  XXMAN         XXWK10
     C                   Z-ADD     XXWK10        XXMAN
     C                   Z-ADD(H)  XXMAND        XXSAVE            9 0
     C                   Z-ADD     XXSAVE        XXMAND
     C                   SUB       XXDISC        XXMAND
     C                   Z-ADD(H)  XXMAND        XXWK10
     C                   Z-ADD     XXWK10        XXMAND
     C                   Z-ADD     1             XXLCM
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   PRRTCD        QQRTCD
     C                   MOVE(P)   PRCLAS        QQCLAS
     C                   MOVE(P)   XXEFDT        QQEFDT
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
      /free
       // Calc Foreign Terrorism
       xxAapr = EPaapr;
       // Adjust for FMIC's logging conversion
       If cltID = '102' and PRlogf = 'Y';
          qqFound = *off;
          qqMPayr = *zeros;
          qqMRate = *zeros;
          qqRptCls = *zeros;
          Callp GetLogFact(PRclas:qqFound:qqMPayr:QQMRate:qqRptCls);
          If qqFound = *on;
             xxAaPr = xxAaPr * qqMPayr;
          Endif;
       Endif;
       Select;
       When RRterl = 'Y';
       When %lookup(xxLCM:LCM) = 0;
          xy = %lookup(0:LCM);
          If xy <> 0;
            LCM(xy) = xxLCM;
          Endif;
       Endsl;
       Select;
       When PRExTer = 'Y';
       // Do nothing for payroll class.
       When Prpcf = 'P' or
        (cltID = '102' and PRlogf = 'Y' and PRpcf = 'C');
       // Calc Foreign Terrorism
          pay(x) = pay(x) + xxAaPr;
          If RRterl <> 'Y';
             Eval(h) xxTert = pyTRt(x) * xxLcm;
          Else;
             xxTert = pyTRt(x);
          Endif;
          Eval(h) xxwk10 = xxAapr/100;
          Eval(h) xxDtra = xxwk10 * xxTert;
          //Apply Excess premium reduction % (Deviation #1) against Terrirism chrages
          If FNExIns = 'Y' and ELASP2 > 0;
             Eval(h) xxDtra = xxDtra - (xxDtra * ELASP2/100);
          Endif;
          eTr(x) = eTr(x) + xxDtra;
       // Calc Domestic Terrorism
          If RRterl <> 'Y';
             Eval(h) xxTert = dmTRt(x) * xxLcm;
          Else;
             xxTert = dmTRt(x);
          Endif;
          Eval(h) xxwk10 = xxAapr/100;
          Eval(h) xxDtra = xxwk10 * xxTert;
          //Apply Excess premium reduction % (Deviation #1) against Terrirism chrages
          If FNExIns = 'Y' and ELASP2 > 0;
             Eval(h) xxDtra = xxDtra - (xxDtra * ELASP2/100);
          Endif;
          dTr(x) = dTr(x) + xxDtra;
       // Calc Non Payroll Terrorism
       When PRpcf <> 'P' and PRoccd = *blank;
          pay2(x) = pay2(x) + xxMan;
          If RRterl <> 'Y';
             Eval(h) xxTert = npTRt(x) * xxLcm;
          Else;
             xxTert = npTRt(x);
          Endif;
          Eval(h) xxDtra = xxMan * xxTert;
          //Apply Excess premium reduction % (Deviation #1) against Terrirism chrages
          If FNExIns = 'Y' and ELASP2 > 0;
             Eval(h) xxDtra = xxDtra - (xxDtra * ELASP2/100);
          Endif;
          nTr(x) = nTr(x) + xxDtra;
       Endsl;
      /end-free
     C*
     C*  ACCUMULATE MANUAL PREMIUM IN CORRECT ARRAY ENTRY
     C*
     C                   ADD       XXMAN         MAN(X)
     C                   ADD       XXMAND        MAND(X)
     C*
     C     EPSPEC        IFEQ      'F'
     C     PRLHCD        OREQ      'Y'
     C                   ADD       XXMAN         XXFCLS           15 4
     C                   MOVE      *ON           XXFCFL
     C                   ADD       XXMAN         FCL(X)
     C                   ENDIF                                                  EPSPEC IFEQ
     C                   ADD       XXMAN         XXPREM
     C*
     C*  ONLY UPDATE EPARAT IF THIS IS NOT AN "A" RATING EMPLOYER
     C*
     C     ELSTAT        IFEQ      'N'
     C     PRARFL        OREQ      *BLANK
     C                   Z-ADD     XXARAT        EPARAT
     C                   END
     C                   Z-ADD     XXMAN         EPAMPR
     C*
     C                   UPDATE    PEP01
     C*
     C     NEXT1         TAG
     C                   Z-ADD     EPEFDT        SVEFDT
     C*
     C                   END
     C                   END
      /free
       If %lookup(0:lcm) = 0;
          hasDiffLCMs = *on;
       Else;
          hasDiffLCMs = *off;
       Endif;
      /end-free
      *
      * CLIENT SPECIFIC FOR LUBA - SAFETY FEE MINIMUM BASED ON F-CLASS
      * PREM AND # OF WAIVER OF SUBROGATION RECORDS (MAX = 1500)
      *
      *
      *
      *
      *  IF F-CLASS PREMIUM LESS THAN USL&H MINIMUM, SUBTRACT
      *  THE F-CLASS MAN PREM OUT OF THE MAN PREM, AND ADD THE USL&H MIN
      *  TO PREMIUM AFTER THE CHECK FOR MINIMUM PREMIUM.
      *
      *
      *  IF THE USL&H FEE WAS CHARGED THROUGH THE SAFETY FEE MINIMUM FOR 106
      *  YEAR POLICIES, REVERSE THE 1000 CHARGE OUT OF THE SAFETY FEE.
      *
     C*
     C* CALCULATE THE PERCENTAGE OF PAYROLL THAT IS F-CLASS
     C*
     C     XXPREM        IFNE      *ZERO
     C     XXFCLS        DIV       XXPREM        XXFPCT            9 6
     C                   ELSE
     C                   Z-ADD     0             XXFPCT
     C                   ENDIF                                                  XXPREM IFNE
     C*
     C* FIND THE HIGHEST MINIMUM PREMIUM OVER ALL THE DIVISIONS.
     C*
     C                   Z-ADD     XXDIV         X2DIV
     C                   Z-ADD     0             XXHIGH
     C                   MOVE      *BLANKS       XXSTE             2
     C                   MOVE      *BLANK        XXFLST            1
     C                   Z-ADD     0             XFHIGH           15 4
     C                   Z-ADD     0             XFDIV             5 0
     C                   Z-ADD     0             XADIV             5 0
     C                   Z-ADD     0             XLMIN1           11 2
     C                   Z-ADD     0             XLMIN2           11 2
     C                   Z-ADD     0             XLDIV1            5 0
     C                   Z-ADD     0             XLDIV2            5 0
     C                   Z-ADD     0             XXLOSS           11 2
     C                   Z-ADD     0             XXDIV1            5 0
     C                   MOVE      XXST          XSST              2
     C                   Z-ADD     0             XXMIN1
     C                   Z-ADD     0             XXMIN2
     C                   Z-ADD     0             XXWVMN           11 2
     C                   Z-ADD     0             XTFCLS           11 2
     C                   Z-ADD     0             XFMINA           11 2
     C                   Z-ADD     0             XFDIV             5 0
     C                   Z-ADD     0             XFDIV2            5 0
      /Free
        //* Minimum cannot be determined on manual premium run.
        If *in96 = *off;

           Chain(n) (xxCo#:xxFnd:xxFyr:xxEmp#:'M') pev00;
           If %found;
              xlmin1 = EVxlmin1;
              xldiv1 = EVxldiv1;
              xlmin2 = EVxlmin2;
              xldiv2 = EVxldiv2;
              xxhigh = EVxxhigh;
              xadiv  = EVxadiv;
              xxloss = EVxxloss;
              xxdiv1 = EVxxdiv1;
              xfmina = EVxfmina;
              xfdiv  = EVxfdiv;
              xtfcls = EVxtfcls;
              xxflst = EVxxflst;
              xfhigh = EVxfhigh;
              xfdiv2 = EVxfdiv2;
           Else;

      /End-Free
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C                   Z-ADD     ELDIV         XXDIV
     C     ELPFDT        IFNE      ELPTDT
     C                   ENDIF
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL(P)  RSJRST        XXST
     C     XXST          IFEQ      'FL'
     C                   MOVE      'Y'           XXFLST
     C                   ENDIF
     C*
     C* FIND HIGHEST INCR LIMIT MINIMUM WHILE WE ARE CHECKING.
     C*
     C     ELILCD        IFNE      *BLANKS
     C     ELPFDT        ANDNE     ELPTDT
     C     ELINL2        ORNE      *BLANKS
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL(P)  RSJRST        XXST
     C*
     C     ELILCD        IFNE      *BLANKS
     C                   MOVEL(P)  ELILCD        XXKEY
     C*
     C     LMKEY1        SETGT     PLM01
     C                   READP     PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C                   READP     PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      '0'
     C     XXCO#         ANDEQ     LMCO#
     C     XXFND         ANDEQ     LMFND
     C     XXST          ANDEQ     LMST
     C     XXKEY         ANDEQ     LMKEY
     C     XXEFDT        ANDGE     LMEFDT
     C*
     c                   if        xxMinPrm = *off

     C     LMMIN         IFGT      XLMIN1
     C                   Z-ADD     LMMIN         XLMIN1
     C                   Z-ADD     XXDIV         XLDIV1
     C                   ENDIF                                                  LMMIN IFGT

     c                   else

     C     LMMINe        IFGT      XLMIN1
     C                   Z-ADD     LMMINe        XLMIN1
     C                   Z-ADD     XXDIV         XLDIV1
     C                   ENDIF                                                  LMMINe IFGT

     c                   endif
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELILCD IFNE
     C*
     C     ELINL2        IFNE      *BLANKS
     C                   MOVEL(P)  ELINL2        XXKEY
     C*
     C     LMKEY1        SETGT     PLM01
     C                   READP     PLM01                                  60
     C     *IN60         DOWEQ     '0'
     C     LMSTAT        ANDEQ     'I'
     C                   READP     PLM01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     *IN60         IFEQ      '0'
     C     XXCO#         ANDEQ     LMCO#
     C     XXFND         ANDEQ     LMFND
     C     XXST          ANDEQ     LMST
     C     XXKEY         ANDEQ     LMKEY
     C     XXEFDT        ANDGE     LMEFDT
     C*
     c                   if        xxMinPrm = *off

     C     LMMIN         IFGT      XLMIN2
     C                   Z-ADD     LMMIN         XLMIN2
     C                   Z-ADD     XXDIV         XLDIV2
     C                   ENDIF                                                  LMMIN IFGT

     c                   else

     C     LMMINe        IFGT      XLMIN2
     C                   Z-ADD     LMMINe        XLMIN2
     C                   Z-ADD     XXDIV         XLDIV2
     C                   ENDIF                                                  LMMINe IFGT

     c                   endif

     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  ELINL2 IFNE
     C*
     C                   ENDIF                                                  ELILCD IFNE
     C                   Z-ADD     9999999       PDAT
     C                   Z-ADD     9999999       RDAT
     C                   Z-ADD     9999999       FDAT
     C*
     C*   LOAD POLICY FROM DATE INTO ARRAY AS 1ST PAYROLL DATE
     C*
     C                   Z-ADD     ELPFDT        PDAT(1)
     C                   Z-ADD     ELPFDT        RDAT(1)
     C     XXSTDT        IFNE      *ZERO
     C                   Z-ADD     XXSTDT        RDAT(1)
     C                   ENDIF
     C                   Z-ADD     RDAT(1)       FDAT(1)
     C*
     C*   USE ANNIVERSARY DATE TO RETRIEVE RATE IF NOT = 0
     C*
     C                   MOVE      '0'           *IN77
     C                   MOVEA     '11'          *IN(78)
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
     C     XXANDT        IFGE      ELPFDT
     C                   MOVE      ELANDT        RDAT(1)
     C                   Z-ADD     RDAT(1)       FDAT(1)
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
     C*
     C     *IN78         IFEQ      '0'
     C                   Z-ADD     XXANDT        PDAT(X)
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C                   ADD       1             X
     C                   ENDIF                                                    *IN78='0'
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     ELPTDT        PDAT(X)
     C                   MOVE      *OFF          *IN75
     C                   ADD       1             X
     C*
     C* CHECK IF ANY POLICY PERIODS NEED TO BE SPLIT FOR 10/1/3 RATES.
     C*
     C     XXST          IFEQ      'FL'
     C*
     C                   DO        8             W                 2 0
     C     W             ADD       1             Z
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       IFLT      1031001
     C     PDAT(W)       ANDLT     1031001
     C     PDAT(Z)       ANDGT     1031001
     C*
     C                   Z-ADD     1031001       PDAT(X)
     C                   Z-ADD     1031001       RDAT(X)
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   MOVE      *ON           *IN75
     C                   ADD       1             X
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 4 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* SORT DATE ARRAYS INTO SEQUENCE.
     C*
     C                   SORTA     PDAT
     C                   SORTA     RDAT
     C                   SORTA     FDAT
     C                   MOVE      *OFF          *IN76
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
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           *IN76
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 4 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* FL RATE SPLIT FOR 07/01/09
     C*
     C                   MOVE      *OFF          XX0709            1
     C     XXST          IFEQ      'FL'
     C     XXCLT#        ANDEQ     '139'
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
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XX0709            1
     C                   ENDIF                                                  RDAT,W IFGE
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 8 X
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
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XXDEFL
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
     C                   Z-ADD     RDAT(W)       FDAT(X)
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
     C*
     C                   eval      pdat(x) = xxSpltDt
     C                   eval      rdat(x) = xxSpltDt
     C                   eval      fdat(x) = rdat(w)
     C                   eval      x = x + 1
     C                   eval      xxOK10 = *on
     C*
     C                   endif                                                  RDAT,W IFGE
     C*
     C                   enddo                                                  DO 6 X
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
     C                   eval      rdat(x) = pdat(w)
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
     C                   eval      rdat(x) = pdat(w)
     C                   eval      x = x + 1
     C                   leave
     C                   endif                                                  pdat(W)
     C                   enddo                                                  DO 8 X
     C                   endif                                                  ApplyRateSplit
     C                   endif                                                  xxst
     C                   Z-ADD     0             XXRDT1
     C                   Z-ADD     0             XXRDT2
     C     *IN75         IFEQ      *ON
     C                   Z-ADD     1031001       XXRDT2            7 0
     C                   ENDIF
     C     *IN76         IFEQ      *ON
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
     C* OK rate split 09/01/10
     C                   if        xxOK10 = *on
     C                   Z-ADD     xxSpltDt      xxrdt1
     C                   endif                                                  XXOK10 = *ON
      /free
       If isOK14 or isTN14;
           xxrdt1 = xxSpltDt;
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
     C     *IN77         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        RDAT(X)
     C                   ELSE
     C     ELEMD2        IFLT      XXANDT
     C                   Z-ADD     ELANDT        RDAT(X)
     C                   ELSE
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C     *IN76         IFEQ      *ON
     C     ELEMD2        ANDGE     1030401
     C     RDAT(X)       ANDLT     1030401
     C                   Z-ADD     1030401       RDAT(X)
     C                   ENDIF
     C     *IN75         IFEQ      *ON
     C     ELEMD2        ANDGE     1031001
     C     RDAT(X)       ANDLT     1031001
     C                   Z-ADD     1031001       RDAT(X)
     C                   ENDIF
     C     XX0709        IFEQ      *ON
     C     ELEMD2        ANDGE     1090701
     C     RDAT(X)       ANDLT     1090701
     C                   Z-ADD     1090701       RDAT(X)
     C                   ENDIF
     C     XXDEFL        IFEQ      *ON
     C     ELEMD2        ANDGE     1081001
     C     RDAT(X)       ANDLT     1081001
     C                   Z-ADD     1081001       RDAT(X)
     C                   ENDIF
     C     XXAZ10        IFEQ      *ON
     C     ELEMD2        ANDGE     1100101
     C     RDAT(X)       ANDLT     1100101
     C                   Z-ADD     1100101       RDAT(X)
     C                   ENDIF
     C* OK rate split 09/01/10
     C                   if        xxOK10 = *on and
     C                             elemd2 >= xxSpltDt and
     C                             rdat(x) < xxSpltDt
     C                   Z-ADD     xxSpltDt      RDAT(X)
     C                   endif                                                  XXOK10 = *ON
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
     C     *IN77         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        RDAT(X)
     C                   ELSE
     C     ELEMD3        IFLT      XXANDT
     C                   Z-ADD     ELANDT        RDAT(X)
     C                   ELSE
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C     *IN76         IFEQ      *ON
     C     ELEMD3        ANDGE     1030401
     C     RDAT(X)       ANDLT     1030401
     C                   Z-ADD     1030401       RDAT(X)
     C                   ENDIF
     C     *IN75         IFEQ      *ON
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
     C* OK rate split 09/01/10
     C                   if        xxOK10 = *on and
     C                             elemd3 >= xxSpltDt and
     C                             rdat(x) < xxSpltDt
     C                   Z-ADD     xxSpltDt      RDAT(X)
     C                   endif                                                  XXOK10 = *ON
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
     C                   SORTA     FDAT
     C*
     C                   DO        8             X
     C*
     C     PDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         PDAT(X)
     C                   ENDIF
     C     RDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         RDAT(X)
     C                   ENDIF
     C     FDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         FDAT(X)
     C                   ENDIF
     C*
     C                   ENDDO
     C*
     C*  READ EMPLOYER PAYROLL RECORDS
     C*
     C     EPKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C     EPSAFL        ANDEQ     'A'
     C*
     C                   Z-ADD     EPCLAS        XXCLAS
     C*
     C*  GET CORRECT DATE FROM RDAT ARRAY TO PULL RATE FROM RATE FILE
     C*
     C                   Z-ADD     1             X
     C     EPEFDT        LOOKUP    PDAT(X)                            60
     C     *IN60         IFEQ      '1'
     C     X             ANDGT     1
     C                   SUB       1             X
     C                   Z-ADD     RDAT(X)       XXEFDT
     C                   ELSE
     C                   Z-ADD     1             X
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   END
     C*
     C*  USE THE PAYROLL RATE SET CODE IN EMPLOYER DETAIL TO RETRIEVE
     C*  THE CORRECT PAYROLL CLASS CODE
     C*
     C                   MOVE      ELRTCD        XXRTCD
     C                   Z-ADD     0             X1EFDT
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
     C                   MOVE      ##DATE        X1EFDT
     C*
     C*  GET PAYROLL CLASS FILE
     C*
     C     PRKEY         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     PRMNPR        IFGT      XFHIGH
     C                   Z-ADD     PRMNPR        XFHIGH
     C                   Z-ADD     XXDIV         XFDIV2
     C                   ENDIF
     C     PRMNPR        IFGT      XXHIGH
     C     EPAAPR        ANDNE     0
     C     PRMNPR        ORGT      XXHIGH
     C     SV1ST         ANDEQ     'NJ'
     C     PRMNPR        ORGT      XXHIGH
     C     SV1ST         ANDEQ     'PA'
     C     PRMNPR        ORGT      XXHIGH
     C     SV1ST         ANDEQ     'NY'
     C                   Z-ADD     PRMNPR        XXHIGH
     C                   Z-ADD     XXDIV         XADIV
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL(P)  RSJRST        XXSTE
     C                   ENDIF                                                  PRMNPR IFGT
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C*
     C     PRLOSS        IFGT      XXLOSS
     C     RSJRST        ANDEQ     XSST
     C     EPAAPR        ANDNE     *ZERO
     C                   Z-ADD     PRLOSS        XXLOSS
     C                   Z-ADD     XXDIV         XXDIV1
     C                   ENDIF
     C*
     C* Accumulate highest harbor exposure minimum and total F-class premium.
     C*
     C                   If        ELpfdt <> ELptdt and
     C                             (EPspec = 'F' or PRlhcd = 'Y')
     C
     C     HEKY          SETGT     PHE01
     C     HEKY2         READPE    PHE01                                  60
     C                   If        *in60 = *off and HEHXMN > xfmina
     C                   Z-add     HEHXMN        xfmina
     C                   Z-add     ELDiv         xfDiv
     C                   Endif                                                  *IN60 IFEQ
     C
     C                   Add       EPAmpr        xtFcls
     C
     C                   Endif                                                  If EPSEPC = 'F' or
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  EPSTAT IFNE
     C*
     C     EPKEY         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      /Free

              Clear pev00;
              EVCo# = xxCo#;
              EVFnd = xxFnd;
              EVFyr = xxFyr;
              EVEmp# = xxEmp#;
              EVType = 'M';
              EVxlmin1 = xlmin1;
              EVxldiv1 = xldiv1;
              EVxlmin2 = xlmin2;
              EVxldiv2 = xldiv2;
              EVxxhigh = xxhigh;
              EVxadiv  = xadiv;
              EVxxloss = xxloss;
              EVxxdiv1 = xxdiv1;
              EVxfmina = xfmina;
              EVxfdiv  = xfdiv;
              EVxtfcls = xtfcls;
              EVxxflst = xxflst;
              EVxfhigh = xfhigh;
              EVxfdiv2 = xfdiv2;
              Write pev00;
           Endif;

        Endif;

      /End-Free
     C*
     C                   Z-ADD     X2DIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C                   Z-ADD     9999999       PDAT
     C                   Z-ADD     9999999       RDAT
     C                   Z-ADD     9999999       FDAT
     C     SVCNDT        IFNE      0
     C                   Z-ADD     SVCNDT        ELPTDT
     C                   ENDIF
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL(P)  RSJRST        XXST
     C*
     C*   LOAD POLICY FROM DATE INTO ARRAY AS 1ST PAYROLL DATE
     C*
     C                   Z-ADD     ELPFDT        PDAT(1)
     C                   Z-ADD     ELPFDT        RDAT(1)
     C     XXSTDT        IFNE      *ZERO
     C                   Z-ADD     XXSTDT        RDAT(1)
     C                   ENDIF
     C                   Z-ADD     RDAT(1)       FDAT(1)
     C*
     C*   USE ANNIVERSARY DATE TO RETRIEVE RATE IF NOT = 0
     C*
     C                   MOVE      '0'           *IN77
     C                   MOVEA     '11'          *IN(78)
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
     C     XXANDT        IFGE      ELPFDT
     C                   MOVE      ELANDT        RDAT(1)
     C                   Z-ADD     RDAT(1)       FDAT(1)
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
     C*
     C     *IN78         IFEQ      '0'
     C                   Z-ADD     XXANDT        PDAT(X)
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C                   ADD       1             X
     C                   ENDIF                                                    *IN78='0'
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     ELPTDT        PDAT(X)
     C                   MOVE      *OFF          *IN75
     C                   ADD       1             X
     C*
     C* CHECK IF ANY POLICY PERIODS NEED TO BE SPLIT FOR 10/1/3 RATES.
     C*
     C     XXST          IFEQ      'FL'
     C*
     C                   DO        8             W                 2 0
     C     W             ADD       1             Z
     C*
     C     RDAT(W)       IFNE      *HIVAL
     C*
     C     RDAT(W)       IFLT      1031001
     C     PDAT(W)       ANDLT     1031001
     C     PDAT(Z)       ANDGT     1031001
     C*
     C                   Z-ADD     1031001       PDAT(X)
     C                   Z-ADD     1031001       RDAT(X)
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   MOVE      *ON           *IN75
     C                   ADD       1             X
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 4 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* SORT DATE ARRAYS INTO SEQUENCE.
     C*
     C                   SORTA     PDAT
     C                   SORTA     RDAT
     C                   SORTA     FDAT
     C                   MOVE      *OFF          *IN76
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
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           *IN76
     C*
     C                   ENDIF                                                  RDAT,W IFGE
     C*
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 4 X
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C* FL RATE SPLIT FOR 07/01/09
     C*
     C                   MOVE      *OFF          XX0709            1
     C     XXST          IFEQ      'FL'
     C     XXCLT#        ANDEQ     '139'
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
     C                   Z-ADD     RDAT(W)       FDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XX0709            1
     C                   ENDIF                                                  RDAT,W IFGE
     C                   ENDIF                                                  RDAT,W IFNE
     C*
     C                   ENDDO                                                  DO 8 X
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
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C                   ADD       1             X
     C                   MOVE      *ON           XXDEFL
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
     C                   Z-ADD     RDAT(W)       FDAT(X)
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
     C*
     C                   eval      pdat(x) = xxSpltDt
     C                   eval      rdat(x) = xxSpltDt
     C                   eval      fdat(x) = rdat(w)
     C                   eval      x = x + 1
     C                   eval      xxOK10 = *on
     C*
     C                   endif                                                  RDAT,W IFGE
     C*
     C                   enddo                                                  DO 6 X
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
     C                   eval      rdat(x) = pdat(w)
     C                   eval      fdat(x) = pdat(w)
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
     C                   eval      rdat(x) = pdat(w)
     C                   eval      fdat(x) = pdat(w)
     C                   eval      x = x + 1
     C                   leave
     C                   endif                                                  pdat(W)
     C                   enddo                                                  DO 8 X
     C                   endif                                                  ApplyRateSplit
     C                   endif                                                  xxst
     C                   Z-ADD     0             XXRDT1
     C                   Z-ADD     0             XXRDT2
     C     *IN75         IFEQ      *ON
     C                   Z-ADD     1031001       XXRDT2            7 0
     C                   ENDIF
     C     *IN76         IFEQ      *ON
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
     C* OK rate split 09/01/10
     C                   if        xxOK10 = *on
     C                   Z-ADD     xxSpltDt      xxrdt1
     C                   endif                                                  XXOK10 = *ON
      /free
       If isOK14 or isTN14;
           xxrdt1 = xxSpltDt;
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
     C     *IN77         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        RDAT(X)
     C                   ELSE
     C     ELEMD2        IFLT      XXANDT
     C                   Z-ADD     ELANDT        RDAT(X)
     C                   ELSE
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C     *IN76         IFEQ      *ON
     C     ELEMD2        ANDGE     1030401
     C     RDAT(X)       ANDLT     1030401
     C                   Z-ADD     1030401       RDAT(X)
     C                   ENDIF
     C     *IN75         IFEQ      *ON
     C     ELEMD2        ANDGE     1031001
     C     RDAT(X)       ANDLT     1031001
     C                   Z-ADD     1031001       RDAT(X)
     C                   ENDIF
     C     XX0709        IFEQ      *ON
     C     ELEMD2        ANDGE     1090701
     C     RDAT(X)       ANDLT     1090701
     C                   Z-ADD     1090701       RDAT(X)
     C                   ENDIF
     C     XXDEFL        IFEQ      *ON
     C     ELEMD2        ANDGE     1081001
     C     RDAT(X)       ANDLT     1081001
     C                   Z-ADD     1081001       RDAT(X)
     C                   ENDIF
     C     XXAZ10        IFEQ      *ON
     C     ELEMD2        ANDGE     1100101
     C     RDAT(X)       ANDLT     1100101
     C                   Z-ADD     1100101       RDAT(X)
     C                   ENDIF
     C* OK rate split 09/01/10
     C                   if        xxOK10 = *on and
     C                             elemd2 >= xxSpltDt and
     C                             rdat(x) < xxSpltDt
     C                   Z-ADD     xxSpltDt      RDAT(X)
     C                   endif                                                  XXOK10 = *ON
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
     C     *IN77         IFEQ      *OFF
     C                   Z-ADD     ELPFDT        RDAT(X)
     C                   ELSE
     C     ELEMD3        IFLT      XXANDT
     C                   Z-ADD     ELANDT        RDAT(X)
     C                   ELSE
     C                   Z-ADD     XXANDT        RDAT(X)
     C                   ENDIF
     C                   ENDIF
     C                   Z-ADD     RDAT(X)       FDAT(X)
     C     *IN76         IFEQ      *ON
     C     ELEMD3        ANDGE     1030401
     C     RDAT(X)       ANDLT     1030401
     C                   Z-ADD     1030401       RDAT(X)
     C                   ENDIF
     C     *IN75         IFEQ      *ON
     C     ELEMD3        ANDGE     1031001
     C     RDAT(X)       ANDLT     1031001
     C                   Z-ADD     1031001       RDAT(X)
     C                   ENDIF
     C     XX0709        IFEQ      *ON
     C     ELEMD3        ANDGE     1090701
     C     RDAT(X)       ANDLT     1090701
     C                   Z-ADD     1090701       RDAT(X)
     C                   ENDIF
     C     XXDEFL        IFEQ      *ON
     C     ELEMD3        ANDGE     1081001
     C     RDAT(X)       ANDLT     1081001
     C                   Z-ADD     1081001       RDAT(X)
     C                   ENDIF
     C     XXAZ10        IFEQ      *ON
     C     ELEMD3        ANDGE     1100101
     C     RDAT(X)       ANDLT     1100101
     C                   Z-ADD     1100101       RDAT(X)
     C                   ENDIF
     C* OK rate split 09/01/10
     C                   if        xxOK10 = *on and
     C                             elemd3 >= xxSpltDt and
     C                             rdat(x) < xxSpltDt
     C                   Z-ADD     xxSpltDt      RDAT(X)
     C                   endif                                                  XXOK10 = *ON
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
     C                   SORTA     FDAT
     C*
     C                   DO        8             X
     C*
     C     PDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         PDAT(X)
     C                   ENDIF
     C     RDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         RDAT(X)
     C                   ENDIF
     C     FDAT(X)       IFEQ      9999999
     C                   Z-ADD     *ZERO         FDAT(X)
     C                   ENDIF
     C*
     C                   ENDDO
      *
      * CLIENT SPECIFIC FOR LUBA - SAFETY FEE MINIMUM BASED ON F-CLASS
      * PREM AND # OF WAIVER OF SUBROGATION RECORDS (MAX = 1500)
      *
     C     XXCLT#        IFEQ      '133'
     C     XXEFDT        ANDLT     1060101
      *
     C     XXFCFL        IFEQ      *ON                                          07/05/05
     C     XXFCLS        ANDLT     1000                                         07/05/05
     C                   MOVE      'Y'           XXFFL0            1
     C                   ELSE
     C                   MOVE      *BLANK        XXFFL0
     C                   ENDIF
      *
     C     *IN97         IFEQ      *OFF
     C     ELKEY         CHAIN     PEL01                              60
     C                   SELECT
     C     ELFFL0        WHENEQ    *BLANK
     C     XXFFL0        ANDEQ     'Y'
     C                   ADD       1000          ELSFMN
     C     ELFFL0        WHENEQ    'Y'
     C     XXFFL0        ANDEQ     *BLANK
     C     ELSFMN        IFGE      1000
     C                   SUB       1000          ELSFMN
     C                   ENDIF
     C                   ENDSL
     C                   MOVE      XXFFL0        ELFFL0
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C                   ENDIF                                                  *IN97 = *OFF
      *
     C                   ENDIF                                                  XXCLT# = '133'
      *
      *  IF F-CLASS PREMIUM LESS THAN USL&H MINIMUM, SUBTRACT
      *  THE F-CLASS MAN PREM OUT OF THE MAN PREM, AND ADD THE USL&H MIN
      *  TO PREMIUM AFTER THE CHECK FOR MINIMUM PREMIUM.
      *
     C                   CLEAR                   XXHXMN
     C     XXCLT#        IFEQ      '133'
     C     XXEFDT        ANDGE     1060101
     C     XXCLT#        ORNE      '133'
     C*
     C                   If        xtfcls < xfmina and xfmina > 0
     C
     C                   If        ELDiv = xfDiv
     C                   Z-ADD     xfmina        xxHXMN
     C                   Endif
     C*
     C                   CLEAR                   XXFCLS
     C                   Z-ADD     1             X
     C     MAN(X)        DOWNE     *ZERO
     C                   SUB       FCL(X)        MAN(X)
     C                   ADD       1             X
     C                   ENDDO
     C                   ENDIF
     C                   ENDIF
      *
      *  IF THE USL&H FEE WAS CHARGED THROUGH THE SAFETY FEE MINIMUM FOR 106
      *  YEAR POLICIES, REVERSE THE 1000 CHARGE OUT OF THE SAFETY FEE.
      *
     C     XXCLT#        IFEQ      '133'
     C     XXEFDT        ANDGE     1060101
     C     ELFFL0        ANDEQ     'Y'
     C     ELSFMN        ANDGE     1000
     C     ELKEY         CHAIN     PEL01                              60
     C                   SUB       1000          ELSFMN
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C                   ENDIF
     C*
     C                   XFOOT     MAN           WKAMPR           15 4
     C     XXHIGH        IFEQ      0
     C     XXFLST        IFEQ      'Y'
     C                   Z-ADD     XFHIGH        XXHIGH
     C                   Z-ADD     XFDIV2        XADIV
     C                   ELSE
     C                   Z-ADD     8810          XXCLAS
     C                   Z-ADD     0             XXHIGH
     C                   Z-ADD     0             XADIV             5 0
      /Free
        //* Minimum cannot be determined on manual premium run.
        If *in96 = *off;

           Chain(n) (xxCo#:xxFnd:xxFyr:xxEmp#:'8') pev00;
           If %found;
              xxhigh = EVxxhigh;
              xadiv  = EVxadiv;
           Else;

      /End-Free
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C                   MOVE      ELPFDT        XXEFDT
     C                   MOVE(P)   ELRTCD        XXRTCD
     C     EPKYL         CHAIN     PEP01                              62
     C                   Z-ADD     0             X1EFDT
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
     C                   MOVE      ##DATE        X1EFDT
     C     PRKEY         SETGT     PPR01
     C     PRKY2         READPE    PPR01                                  60
     C     *IN60         IFEQ      '0'
     C     PRMNPR        ANDGT     XXHIGH
     C                   Z-ADD     PRMNPR        XXHIGH
     C                   Z-ADD     XXDIV         XADIV
     C                   ENDIF
     C*
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE(P)   ELRTCD        XXRTCD
     C     SVCNDT        IFNE      0
     C                   Z-ADD     SVCNDT        ELPTDT
     C                   ENDIF
      /Free

              Clear pev00;
              EVCo# = xxCo#;
              EVFnd = xxFnd;
              EVFyr = xxFyr;
              EVEmp# = xxEmp#;
              EVType = '8';
              EVxxhigh = xxhigh;
              EVxadiv  = xadiv;
              Write pev00;
           Endif;

        Endif;

      /End-Free
     C*
     C                   ENDIF                                                  XXFLST IFEQ
     C                   ENDIF                                                  WKAMPR IFEQ
     C*
     C* APPLY RATE INCREASE TO INDIANA ACCOUNT IF APPLICABLE.
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C* CA FLAT RATE INCREASE FOR 2002 POLICIES (ANNIVERSARY OR EFF DT)
     C*
     C     XXST          IFEQ      'CA'
     C*
     C                   MOVEL     ELPFDT        XXYR              3 0
     C                   MOVEL     ELANDT        XXAYR             3 0
     C*
     C* APPLY 0.2% INCREASE FOR ANY 2002 POLICIES.
     C*
     C     XXYR          IFEQ      102
     C     XXAYR         OREQ      102
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     MAN(X)        MULT(H)   1.002         XXWK10
     C                   Z-ADD     XXWK10        MAN(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXYR IFEQ
     C*
     C* APPLY 4.9% INCR FOR UNEXPIRED PORTION OF POLICY AS OF 1/1/2003
     C*
     C     XXYR          IFEQ      102
     C     ELPTDT        ANDGT     1030101
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     X             ADD       1             Y
     C*
     C     PDAT(X)       IFNE      *ZERO
     C*
     C     PDAT(Y)       IFNE      *ZERO
     C                   Z-ADD     PDAT(Y)       XXDT              7 0
     C                   ELSE
     C                   Z-ADD     ELPTDT        XXDT
     C                   ENDIF
     C*
     C* APPLY INCR IF 1/1/2003 FALLS WITHIN RATING PERIOD.
     C*
     C     RDAT(X)       IFLT      1030101
     C     XXDT          ANDGT     1030101
     C*
     C* TOTAL DAYS FOR PERIOD.
     C*
     C     PDAT(X)       IFLT      ELPFDT
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ELSE
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   ENDIF
     C*
     C                   Z-ADD     SRDTI         X1STDT            7 0
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     XXDT          SRDTI
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
     C                   Z-ADD     QQDAYS        TDAYS             5 0
     C*
     C* UNEXPIRED DAYS IN PERIOD.
     C*
     C     X1STDT        IFGT      1030101
     C                   Z-ADD     X1STDT        SRDTI
     C                   ELSE
     C                   Z-ADD     1030101       SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   Z-ADD     XXDT          SRDTI
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
     C                   Z-ADD     QQDAYS        UDAYS             5 0
     C*
     C     TDAYS         IFNE      *ZERO
     C     UDAYS         DIV(H)    TDAYS         X2FACT            7 6
     C                   ELSE
     C                   Z-ADD     0             X2FACT
     C                   ENDIF
     C*
     C                   MULT(H)   .049          X2FACT
     C                   ADD       1             X2FACT
     C     MAN(X)        MULT(H)   X2FACT        XXWK10
     C                   Z-ADD     XXWK10        MAN(X)
     C*
     C                   ENDIF                                                  RDAT,X IFLT
     C*
     C                   ENDIF                                                  RDAT,X IFNE
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  XXYR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     c                   z-add     1110901       xxSpltDt
     c* Generate flat rate adjustment if IL coverage as of 09/01/2011
     c                   if        xxst = 'IL'  and
     c                             elpfdt < xxSpltDt and
     c                             elptdt > xxSpltDt and
     c                             ApplyRateSplit(xxst:xxSpltDt)
      *
      * Calculate Unexpired policy portion in months
     c                   movel     xxSpltDt      srdate1
     c                   movel     elptdt        srdate2
     c                   z-add     0             xx#Mth            2 0
     c                   eval      xx#mth = ((sryy2 - sryy1) * 12
     c                                      + (srmm2 - srmm1))
     c                   eval(h)   xxadjp = (-8.8) * (xx#Mth/12)
     c                   eval(h)   xxrtadj = xxadjp/100
      *
     c                   z-add     1             x
     c                   dow       rdat(x) > 0
     c                   eval(h)   xxwk10 = man(x) * xxrtadj
     c                   eval      man(x) = man(x) + xxwk10
     c                   eval      x = x + 1
     c                   enddo                                                  rdat(x) > 0
     c                   endif                                                  xxst = 'IL'
      *
     C     XXST          IFEQ      'NY'
     C     ELPTDT        ANDGT     1071001
     C     RDAT(1)       ANDLT     1071001
     C     RDAT(1)       ANDNE     1061001
      *
     C                   CALL      'SRDDIF7'
     C                   PARM      1071001       SRFRDC            7 0
     C                   PARM      ELPTDT        SRTODC            7 0
     C                   PARM                    SRDAYS            5 0
     C                   PARM      ' '           SRERR             1
     C                   PARM      ' '           SRTERM            1
      *
     C     SRDAYS        DIV(H)    366           X2FACT            7 6
     C     X2FACT        MULT(H)   .161          XXNYP             5 4
      *
     C     XXANDT        IFNE      0
     C     XXANDT        ANDGT     1071001
      *
     C                   CALL      'SRDDIF7'
     C                   PARM      ELPFDT        SRFRD1            7 0
     C                   PARM      XXANDT        SRTOD1            7 0
     C                   PARM                    SRDAY1            5 0
     C                   PARM      ' '           SRERR1            1
     C                   PARM      ' '           SRTER1            1
      *
     C     ELPTDT        IFGT      1070228
     C     SRDAY1        DIV(H)    366           X2FACT
     C                   ELSE
     C     SRDAY1        DIV(H)    365           X2FACT
     C                   ENDIF
      *
     C                   MULT(H)   X2FACT        XXNYP
      *
     C                   ELSE
      *
     C     ELPROR        IFNE      1
     C                   MULT(H)   ELPROR        XXNYP
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     1             SUB       XXNYP         XXNYP
      *
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
     C     RDAT(X)       IFLT      1071001
     C     XXNYP         MULT(H)   MAN(X)        XXWK10
     C                   Z-ADD     XXWK10        MAN(X)
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO
      *
      *
     C                   ENDIF
      *
      * NY 10/17 Flat RateAdj
     c                   z-add     1170410       xxSpltDt
     c                   If        xxst = 'NY' and
     c                             ApplyRateSplit(xxst:xxSpltDt)
      * Calculate No of days in the term
     c     elpfdt        add       19000000      FromDate8                                ISO format
     c     elptdt        add       19000000      Todate8                                  ISO format
     c                   Move      FromDate8     xxTermEffF
     c                   Move      Todate8       xxTermEffT
     c     xxTermEffT    subdur    xxTermEffF    xxTrDays:*days
     c                   z-add     1161001       xxSpltDtB
     c                   z-add     1170930       xxSpltDtE
     c* Generate flat rate adjustment
     c                   Select
     c                   When      xxTrDays = 365    and
     c                             elpfdt >= xxSpltDtB and
     c                             elpfdt <= xxSpltDtE

     c                   Select
     c                   When      elpfdt >= 1161001 and
     c                             elpfdt <= 1161009
     c                   eval      xxadjp =  -0.8
     c                   When      elpfdt >= 1161010 and
     c                             elpfdt <= 1161030
     c                   eval      xxadjp =  -0.9
     c                   When      elpfdt >= 1161031 and
     c                             elpfdt <= 1161121
     c                   eval      xxadjp =  -1.0
     c                   When      elpfdt >= 1161122 and
     c                             elpfdt <= 1161212
     c                   eval      xxadjp =  -1.1
     c                   When      elpfdt >= 1161213 and
     c                             elpfdt <= 1170103
     c                   eval      xxadjp =  -1.2
     c                   When      elpfdt >= 1170104 and
     c                             elpfdt <= 1170124
     c                   eval      xxadjp =  -1.3
     c                   When      elpfdt >= 1170125 and
     c                             elpfdt <= 1170215
     c                   eval      xxadjp =  -1.4
     c                   When      elpfdt >= 1170216 and
     c                             elpfdt <= 1170308
     c                   eval      xxadjp =  -1.5
     c                   When      elpfdt >= 1170309 and
     c                             elpfdt <= 1170330
     c                   eval      xxadjp =  -1.6
     c                   When      elpfdt >= 1170331 and
     c                             elpfdt <= 1170930
     c                   eval      xxadjp =  -1.7
     c                   Endsl
     c                   eval(h)   xxrtadj = xxadjp/100

     c                   When      xxTrDays < 365 and
     c                             elpfdt >= xxSpltDtB and
     c                             elpfdt <= xxSpltDtE
     c                             and elptdt > xxSpltDt                        elptdt > 1170410
     c                             and xxTrDays > 0
      *                  No. of days remaining on the policy after 04/10/17
     c     1170410       add       19000000      FromDate8                                ISO format
     c                   If        elpfdt > xxSpltDt                               Eff dt > 1170410
     c                   Clear                   FromDate8
     c     elpfdt        add       19000000      FromDate8                         FromDate8=elpfdt
     c                   Endif
     c     elptdt        add       19000000      Todate8                                  ISO format
     c                   Move      FromDate8     xxTermEffF
     c                   Move      Todate8       xxTermEffT
     c     xxTermEffT    subdur    xxTermEffF    xxDayRem:*days
     c                   eval(h)   xxadjp = (xxDayRem/xxTrDays) * (-1.7)
     c                   eval(h)   xxrtadj = xxadjp/100
     c                   Endsl
      *
     c                   If        xxrtadj <> 0
     c                   z-add     1             x
     c                   dow       rdat(x) > 0
     c                   eval(h)   xxwk10 = man(x) * xxrtadj
     c                   eval      man(x) = man(x) + xxwk10
     c                   eval      x = x + 1
     c                   enddo                                                  rdat(x) > 0
     c                   endif                                                  xxrtadj <> 0
     c                   endif                                                  xxst = 'NY'
      *
     C*
     C*  CALCULATE SHORT-RATED NORMAL PREMIUM
     C*
     C                   MOVE      *BLANK        XXSRFL            1
     C     ELSRPR        IFEQ      'S'
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
     C*
     C*  ANNUALIZE THE PREMIUM
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
     C                   Z-ADD     ELPFDT        X3EFDT
     C*
     C     STKY          SETGT     PST01
     C     STKY1         READPE    PST01                                  60
     C     *IN60         IFEQ      *OFF
     C*
     C                   Z-ADD     STEFDT        X3EFDT
     C*
     C     STKY2         SETGT     PST02
     C     STKY          READPE    PST02                                  60
     C*
     C                   Z-ADD     1             Z
     C     Z             DO        8             Z                 3 0
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'Y'           XXSRFL
     C                   Z-ADD     STPERC        XXSVPC            7 4
     C                   MULT(H)   .01           XXSVPC
     C                   ELSE
     C                   Z-ADD     0             XXSVPC
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDDO
     C*
     C                   ENDIF                                                  *IN60 = '0'
     C*
     C                   ENDIF                                                  EMSRPR = 'S'
     C*
     C*  INVOKE MIN. MANUAL PREMIUM ROUTINE IF ELFFL2 IS BLANK
     C*
     C*  GET PRORATE FACTOR IF NECESSARY
     C*
     C     FDAYS         IFNE      0
     C     PDAYS         DIV(H)    FDAYS         XEPROR
     C     ELFFL9        IFEQ      'Y'
     C                   Z-ADD     1             XXPROR
     C                   ELSE
     C     PDAYS         DIV(H)    FDAYS         XXPROR            5 3
     C                   ENDIF                                                  ELFFL9 = Y
     C                   ELSE
     C                   Z-ADD(H)  0             XEPROR
     C     ELFFL9        IFEQ      'Y'
     C                   Z-ADD     1             XXPROR
     C                   ELSE
     C                   Z-ADD     0             XXPROR
     C                   ENDIF                                                  ELFFL9 = Y
     C                   END
     C*
     C*  INVOKE MIN. MANUAL PREMIUM ROUTINE IF ELFFL2 IS BLANK
     C     ELFFL2        IFEQ      *BLANK
     C     XXCLT#        ANDEQ     '102'
     C* APPLY A MINIMUM MANUAL PREMIUM FOR ALL POLICIES IF NECESSARY.
     C                   XFOOT     MAN           XXPREM
     C     2500          MULT(H)   XXPROR        XXMINM           11 0
     C*
     C     XXPREM        IFLT      XXMINM
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXMINM        MULT(H)   PRO(X)        XXWK10
     C                   Z-ADD     XXWK10        MAN(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT,X DOWNE
     C*
     C                   XFOOT     MAN           XXPREM
     C     XXPREM        IFNE      XXMINM
     C     XXMINM        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        MAN(1)
     C                   ENDIF                                                  XXPREM IFNE
     C*
     C                   ENDIF                                                  XXPREM IFLT
     C                   ENDIF                                                  ELFFL2 = ' '
     C                   Z-ADD     MAN           STN
     C                   Z-ADD     MAN           STNT
     C                   Z-ADD     MAN           STN1
     C                   Z-ADD     MAN           STN2
     C                   Z-ADD     MAN           STOC
     C*
     C*  FIND IF THE EMPLOYER IS CANCELLED AND WHEN.
     C*
     C                   Z-ADD     5             XXX               1 0
     C                   MOVE      *BLANK        SVCNCL            1
     C                   Z-ADD     0             SVCNDT
     C                   DO        5
     C     CD(XXX)       IFGE      ELPFDT
     C     CD(XXX)       ANDLE     ELPTDT
     C     ED(XXX)       ANDNE     0
     C                   MOVE      'Y'           SVCNCL
     C                   MOVE      CD(XXX)       SVCNDT
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             XXX
     C                   ENDDO
     C*
     C*  GET NUMBER OF ACTIVE DAYS OF A CANCELLED EMPLOYER
     C*
     C                   Z-ADD     0             XXLPDT
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C                   Z-ADD     SVCNDT        SRDTI
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
     C     XXST          IFEQ      'NJ'
     C     ELEMNA        ANDNE     *ZERO
     C                   Z-ADD     ELEMNA        XXHIGH
     C                   ENDIF                                                  XXST IFEQ
      *
     C                   Z-ADD     QQDAYS        ADAYS             5 0
     C                   MOVE      *BLANK        XXCXP             1
     C*
     C                   eval      xxFlgCalcUPSC = *off
     C* If policy is cancelled and prorate flag is set and unearned premium surcharge
     C* percentage is setup, then calculate full policy payroll factor
     C                   if        SVCNCL = 'Y' and ELSRPR = 'P' and
     C                             XXUPSP > 0
     C                   if        ADAYS > 0
     C                   eval(h)   xxPrmFact = WDAYS/ADAYS
     C                   else
     C                   eval      xxPrmFact = 1
     C                   endif
     C                   eval      xxFlgCalcUPSC = *on
     C                   endif
     C*** begin add  ***  06/26/23  ************************************
     C*
     C* Set flag if a policy is cancelled non-renewal and has gone full
     C* term in order to let short rate for concurrency to kick in.
     C*
     C                   Clear                   xxProrNR          1
     C                   If        ELCOFL = 'Y' and (adays = wdays)
     C                             and (xxcxcd = '00' or xxidov = '3')
     C                   Eval      xxProrNR = 'Y'
     C                   Endif
     C*** end add ******  06/26/23  ************************************
     C* PRORATE MINIMUM PREMIUM ONLY IF CANCELLED
     C     SVCNCL        IFEQ      'Y'
     C     FDAYS         ANDNE     0
     C*** Beg Chg *** 09/18/25 **************************************
     C***  XXCXCD        ANDNE     '03'
     C***  XXCXCD        ANDNE     '08'
     C***  XXCXCD        ANDNE     '09'
     C***  XXCXCD        ANDNE     '10'
     C     XXCXNOPRO     ANDNE     'Y'
     C*** End Chg *** 09/18/25 **************************************
     C     ADAYS         DIV(H)    FDAYS         XEPROR
     C     ELFFL9        IFEQ      'Y'
     C                   Z-ADD     1             XXPROR
     C                   ELSE
     C     ADAYS         DIV(H)    FDAYS         XXPROR
     C                   ENDIF                                                  ELFFL9 = Y
     C     ELSRPR        IFEQ      'P'
     C     ELPFDT        ORLT      1000101
     C     XXST          OREQ      'MI'
     C                   MULT      XEPROR        XXHIGH
     C                   MOVE      'Y'           XXCXP
     C                   ENDIF                                                  ELSRPR IFEQ
     C                   END
     C*
     C* IF A FLAT CANCELLATION, PRORATE MIN PREMIUM DOWN TO ZERO.
     C*
     C     ELPFDT        IFEQ      ELPTDT
     C     *LIKE         DEFINE    ELPROR        XEPROR
     C                   Z-ADD     0             XEPROR
     C     ELFFL9        IFEQ      'Y'
     C                   Z-ADD     1             XXPROR
     C                   ELSE
     C                   Z-ADD     0             XXPROR
     C                   ENDIF                                                  ELFFL9 = Y
     C                   Z-ADD     0             XXHIGH
     C                   ENDIF                                                  ELPFDT IFEQ
     C     SVCNCL        IFNE      'Y'
     C     ELCOFL        ANDEQ     'Y'
     C     XXST          OREQ      'LA'
     C     XXCXP         ANDNE     'Y'
     C*** begin add  ***  06/26/23  ************************************
     C     xxProrNR      Oreq      'Y'
     C     xxCXP         AndNE     'Y'
     C*** end add ******  06/26/23  ************************************
     C                   MULT      XEPROR        XXHIGH
     C                   ENDIF                                                  SVCNCL IFNE
     C*
     C*  ROUND TO NEAREST DOLLAR
     C*
     C                   Z-ADD(H)  XXHIGH        XXSAVE            9 0
     C                   Z-ADD     XXSAVE        XXHIGH
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C     FNCGFL        IFEQ      'Y'
     C*
     C                   Z-ADD     ELPFDT        SVDT
     C     ELKY1         CHAIN     PEL01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ELPFDT        IFLT      SVDT
     C                   Z-ADD     ELPFDT        SVDT
     C                   ENDIF                                                  ELPFDT IFLT
     C*
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  DOWEQ *IN60
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C                   Z-ADD     SVDT          XXENPO
     C                   ADD       1             XXENYR
     C                   ELSE
     C                   Z-ADD     F1ENPO        XXENPO
     C                   END
     C*
     C* PRORATE INCR LIMITS MINIMUMS IF CANCELLATION AND NOT SHORT RATE
     C*
     C     SVCNCL        IFEQ      'Y'
     C*** Beg Chg *** 09/18/25 **************************************
     C***  XXCXCD        ANDNE     '03'
     C***  XXCXCD        ANDNE     '08'
     C***  XXCXCD        ANDNE     '09'
     C***  XXCXCD        ANDNE     '10'
     C     XXCXNOPRO     ANDNE     'Y'
     C*** End Chg *** 09/18/25 **************************************
     C     SVCNCL        ORNE      'Y'
     C     ELCOFL        ANDEQ     'Y'
     C*** begin add ***  06/26/23  *******************************************
     C     xxProrNR      Oreq      'Y'
     C*** end add *****  06/26/23  *******************************************
     C*
     C     ELSRPR        IFEQ      'P'
     C     ELPFDT        ANDGE     1000101
     C     XXST          ANDNE     'MI'
     C     XXST          ANDNE     'NJ'
     C*
     C     XLMIN1        MULT(H)   XXPROR        XXWK10
     C                   Z-ADD     XXWK10        XLMIN1
     C     XLMIN2        MULT(H)   XXPROR        XXWK10
     C                   Z-ADD     XXWK10        XLMIN2
     C*
     C                   ENDIF                                                  ELSRPR IFEQ
     C*
     C                   ENDIF                                                  SVCNCL IFEQ
     C*
     C*****************************************************************
     C*
     C*  LOAD C SPECS FOR BILLING DISCOUNT SECTION
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C                   Z-ADD     MAN           MANSV
     C*
     C                   EXSR      BILLC
     C* Determine Unearned premium surcharge amount
     C* Apply full policy factor
     C*
     C* Determine unearned premium
     C*
     C*****************************************************************
     C*
     C                   Z-ADD(H)  XXNET         XXWK10
     C                   Z-ADD     XXWK10        XXNET
     C*
     C*  COMPUTE EXPENSE CONSTANT
     C*
     C                   MOVE      *BLANK        XXECST            2
     C                   Z-ADD     0             XPEXCO            5 2
     C     XXST          IFEQ      'PA'
     C                   MOVE      'PA'          XXECST
     C                   ENDIF
     C*
     C*  CHECK THE TOTAL STANDARD PREMIUM ON THE POLICY AND COMPARE
     C*  TO THE STANDARD PREMIUM LEVEL FOR REDUCED EXPENSE CONSTANT
     C*  IF SPECIFIED.  IF THE PREMIUM IS LESS, USE THE REDUCED CNST.
     C*
     C                   Z-ADD     0             XTSTPR           11 2
     C*
     C     DSKY          CHAIN     PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C                   ADD       MPASPR        XTSTPR
     C     DSKY          READE     PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   Z-ADD     0             XXEXCN                         TLB 032988
     C                   Z-ADD(H)  0             XXWKEX           11 0
     C                   Z-ADD     0             XXEXCO
     C                   Z-ADD     0             XXMPEC           11 2
     C                   MOVE      *BLANK        XXSFUL            1
     C     FNKEY         CHAIN     PEX02                              60
     C*
     C     EXBEST        IFEQ      'Y'
     C     ELPFDT        ANDNE     ELPTDT
     C     EMSTRP        OREQ      'Y'
     C     ELPFDT        ANDNE     ELPTDT
     C     F1EXDV        OREQ      'Y'
     C     ELPFDT        ANDNE     ELPTDT
     C*
     C                   Z-ADD     0             XEPREM           11 2
     C                   MOVE      *BLANKS       XEST              2
     C                   Z-ADD     0             XEDIV             5 0
     C*
     C                   MOVE      ELPFDT        XXEFDT
      *
      *
     C*
     C* SAVE DIVISION NUMBER.
     C*
     C                   Z-ADD     XXDIV         X1DIV             5 0
     C                   MOVE      *BLANK        XXEHF             1
     C                   MOVE      *BLANK        XXEREV            1
     C                   Z-ADD     9999999       XXEHDT            7 0
     C     FNPEOF        IFEQ      'M'
     C     FNPEOE        ANDEQ     'Y'
     C     FNPEOF        OREQ      'G'
     C     FNPEOE        ANDEQ     'Y'
     C     FNPEOF        OREQ      'C'
     C     FNPEOE        ANDEQ     'Y'
     C*
     C     XXEMP#        IFEQ      1
     C*
     C     ECKY          SETGT     PEC01
     C     ECKY1         READPE    PEC01                                  60
      *
      * REDUCE THE EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     XTSTPR        ANDLT     ECSTP3
     C     ECSTP3        ANDNE     *ZERO
     C                   Z-ADD     ECEXC3        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C     *IN60         IFEQ      *OFF
     C     XTSTPR        ANDLT     ECSTPR
     C     ECSTPR        ANDNE     *ZERO
     C                   Z-ADD     ECEXC2        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
      *
      * FIND HIGHEST EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDGT     XXEXCO
     C                   Z-ADD(H)  ECEXCO        XXWKEX           11 0
     C                   Z-ADD     ECEXCO        XXEXCO
     C                   Z-ADD     ECMPEC        XXMPEC
     C                   MOVE      ECSFUL        XXSFUL
     C                   ENDIF                                                  *IN60 IFEQ
      *
      * STORE PA EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     XXST          ANDEQ     'PA'
     C                   Z-ADD     ECEXCO        XPEXCO
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXEMP# IFEQ
     C*
     C                   ELSE
     C*
     C* READ THROUGH EMPLR HISTORY FOR EXPENSE CONSTANT.
     C*
     C     ELKY1         CHAIN     PEH02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EHTRDT        IFLT      XXEHDT
     C     EHTRDT        ANDNE     *ZERO
     C                   Z-ADD     EHTRDT        XXEHDT
     C                   ENDIF
     C*
     C     EHFAF         IFNE      'A'
     C                   MOVE      'Y'           XXEREV
     C                   ENDIF
     C     EHFAF2        IFEQ      'E'
     C                   MOVE      EHDIV         XXDIV
     C                   MOVE      'Y'           XXEHF
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     ECKY          SETGT     PEC01
     C     ECKY1         READPE    PEC01                                  60
      *
      * REDUCE THE EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ELPFDT        ANDNE     ELPTDT
     C     XTSTPR        ANDLT     ECSTP3
     C     ECSTP3        ANDNE     *ZERO
     C                   Z-ADD     ECEXC3        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     *IN60         IFEQ      *OFF
     C     ELPFDT        ANDNE     ELPTDT
     C     XTSTPR        ANDLT     ECSTPR
     C     ECSTPR        ANDNE     *ZERO
     C                   Z-ADD     ECEXC2        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
      *
      * STORE PA EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ELPFDT        ANDNE     ELPTDT
     C     XXST          ANDEQ     'PA'
     C                   Z-ADD     ECEXCO        XPEXCO
     C                   ENDIF
      *
      * FIND HIGHEST EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDGT     XXEXCO
     C     ELPFDT        ANDNE     ELPTDT
     C                   Z-ADD(H)  ECEXCO        XXWKEX           11 0
     C                   Z-ADD     ECEXCO        XXEXCO
     C                   Z-ADD     ECMPEC        XXMPEC           11 2
     C                   MOVE      ECSFUL        XXSFUL
     C*
     C* STORE STATE WITH HIGHEST EXPENSE CONSTANT.
     C*
     C     XEST          IFNE      XXST
     C     EXBEST        ANDEQ     'Y'
     C*
     C                   MOVE      XXST          XEST
     C                   Z-ADD     XXDIV         XEDIV
     C                   Z-ADD     0             XEPREM
     C*
     C     DSKY          CHAIN(N)  PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C     MPST          IFEQ      XXST
     C                   ADD       MPAAPR        XEPREM
     C                   SUB       MPPRTD        XEPREM
     C                   ENDIF
     C     DSKY          READE(N)  PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XEST IFNE
     C*
     C                   ELSE
     C*
     C* IF THIS STATE HAS SAME EXP CONSTANT AS HIGHEST, COMPARE PREMIUM
     C*
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDEQ     XXEXCO
     C     ELPFDT        ANDNE     ELPTDT
     C     XEST          ANDNE     XXST
     C     EXBEST        ANDEQ     'Y'
     C*
     C                   Z-ADD     0             XXPREM
     C*
     C     DSKY          CHAIN(N)  PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C     MPST          IFEQ      XXST
     C                   ADD       MPAAPR        XXPREM
     C                   SUB       MPPRTD        XXPREM
     C                   ENDIF
     C     DSKY          READE(N)  PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXPREM        IFGT      XEPREM
     C                   MOVE      XXST          XEST
     C                   Z-ADD     XXDIV         XEDIV
     C                   Z-ADD     XXPREM        XEPREM
     C                   ENDIF                                                  XXPREM IFGT
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
      *
      * STORE PA EXPENSE CONSTANT.
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  EHFAF2 IFEQ
      *
      * READ NEXT EMPLR HISTORY RECORD.
      *
     C     ELKY1         READE     PEH02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C     XXEHF         IFEQ      *BLANK
     C     XXEREV        ANDEQ     'Y'
     C*
     C     X1PFDT        IFEQ      *ZERO
     C                   MOVEL     ELPFDT        XXYM              5 0
     C                   ELSE
     C                   MOVEL     X1PFDT        XXYM
     C                   ENDIF
     C                   MOVEL     XXEHDT        XXYM2             5 0
     C*
     C     XXYM          IFLT      XXYM2
     C                   MOVEL     XXYM2         XXYM
     C                   ENDIF
     C                   MOVEL     XXYM          XXEHDT
     C                   MOVE      31            XXEHDT
     C*
     C* READ THROUGH EMPLR HISTORY FOR EXPENSE CONSTANT.
     C*
     C     ELKY1         CHAIN     PEH02                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EHFAF         IFEQ      'R'
     C     EHTRDT        ANDLE     XXEHDT
     C*
     C                   MOVE      EHDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     ECKY          SETGT     PEC01
     C     ECKY1         READPE    PEC01                                  60
      *
      * REDUCE THE EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ELPFDT        ANDNE     ELPTDT
     C     XTSTPR        ANDLT     ECSTP3
     C     ECSTP3        ANDNE     *ZERO
     C                   Z-ADD     ECEXC3        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
     C     *IN60         IFEQ      *OFF
     C     ELPFDT        ANDNE     ELPTDT
     C     XTSTPR        ANDLT     ECSTPR
     C     ECSTPR        ANDNE     *ZERO
     C                   Z-ADD     ECEXC2        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
      *
      * STORE PA EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ELPFDT        ANDNE     ELPTDT
     C     XXST          ANDEQ     'PA'
     C                   Z-ADD     ECEXCO        XPEXCO
     C                   ENDIF
      *
      * FIND HIGHEST EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDGT     XXEXCO
     C     ELPFDT        ANDNE     ELPTDT
     C                   Z-ADD(H)  ECEXCO        XXWKEX           11 0
     C                   Z-ADD     ECEXCO        XXEXCO
     C                   Z-ADD     ECMPEC        XXMPEC           11 2
     C                   MOVE      ECSFUL        XXSFUL
     C*
     C* STORE STATE WITH HIGHEST EXPENSE CONSTANT.
     C*
     C     XEST          IFNE      XXST
     C     EXBEST        ANDEQ     'Y'
     C*
     C                   MOVE      XXST          XEST
     C                   Z-ADD     XXDIV         XEDIV
     C                   Z-ADD     0             XEPREM
     C*
     C     DSKY          CHAIN(N)  PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C     MPST          IFEQ      XXST
     C                   ADD       MPAAPR        XEPREM
     C                   SUB       MPPRTD        XEPREM
     C                   ENDIF
     C     DSKY          READE(N)  PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XEST IFNE
     C*
     C                   ELSE
     C*
     C* IF THIS STATE HAS SAME EXP CONSTANT AS HIGHEST, COMPARE PREMIUM
     C*
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDEQ     XXEXCO
     C     ELPFDT        ANDNE     ELPTDT
     C     XEST          ANDNE     XXST
     C     EXBEST        ANDEQ     'Y'
     C*
     C                   Z-ADD     0             XXPREM
     C*
     C     DSKY          CHAIN(N)  PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C     MPST          IFEQ      XXST
     C                   ADD       MPAAPR        XXPREM
     C                   SUB       MPPRTD        XXPREM
     C                   ENDIF
     C     DSKY          READE(N)  PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXPREM        IFGT      XEPREM
     C                   MOVE      XXST          XEST
     C                   Z-ADD     XXDIV         XEDIV
     C                   Z-ADD     XXPREM        XEPREM
     C                   ENDIF                                                  XXPREM IFGT
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
      *
      * STORE PA EXPENSE CONSTANT.
      *
     C                   ENDIF                                                  *IN60 IFEQ
      *
     C                   ENDIF                                                  EHFAF  IFEQ
      *
      * READ NEXT EMPLR HISTORY RECORD.
      *
     C     ELKY1         READE     PEH02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   ENDIF                                                  XXEHF IFEQ
      *
     C     XXEHF         IFEQ      *BLANK
     C     XXEREV        ANDEQ     *BLANK
     C*
     C* READ THROUGH EMPLR DETAIL FOR EXPENSE CONSTANT.
     C*
     C     ELKY1         CHAIN     PEL01                              60
     C     *IN60         DOWEQ     *OFF
      *
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     ECKY          SETGT     PEC01
     C     ECKY1         READPE    PEC01                                  60
      *
      * REDUCE THE EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ELPFDT        ANDNE     ELPTDT
     C     XTSTPR        ANDLT     ECSTP3
     C     ECSTP3        ANDNE     *ZERO
     C                   Z-ADD     ECEXC3        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
     C     *IN60         IFEQ      *OFF
     C     ELPFDT        ANDNE     ELPTDT
     C     XTSTPR        ANDLT     ECSTPR
     C     ECSTPR        ANDNE     *ZERO
     C                   Z-ADD     ECEXC2        ECEXCO
     C                   ENDIF                                                  *IN60 IFEQ
      *
      * STORE PA EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ELPFDT        ANDNE     ELPTDT
     C     XXST          ANDEQ     'PA'
     C                   Z-ADD     ECEXCO        XPEXCO
     C                   ENDIF
      *
      * FIND HIGHEST EXPENSE CONSTANT.
      *
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDGT     XXEXCO
     C     ELPFDT        ANDNE     ELPTDT
     C                   Z-ADD(H)  ECEXCO        XXWKEX           11 0
     C                   Z-ADD     ECEXCO        XXEXCO
     C                   Z-ADD     ECMPEC        XXMPEC           11 2
     C                   MOVE      ECSFUL        XXSFUL
     C*
     C* STORE STATE WITH HIGHEST EXPENSE CONSTANT.
     C*
     C     XEST          IFNE      XXST
     C     EXBEST        ANDEQ     'Y'
     C*
     C                   MOVE      XXST          XEST
     C                   Z-ADD     ELDIV         XEDIV
     C                   Z-ADD     0             XEPREM
     C*
     C     DSKY          CHAIN(N)  PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C     MPST          IFEQ      XXST
     C                   ADD       MPAAPR        XEPREM
     C                   SUB       MPPRTD        XEPREM
     C                   ENDIF
     C     DSKY          READE(N)  PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XEST IFNE
     C*
     C                   ELSE
     C*
     C* IF THIS STATE HAS SAME EXP CONSTANT AS HIGHEST, COMPARE PREMIUM
     C*
     C     *IN60         IFEQ      *OFF
     C     ECEXCO        ANDEQ     XXEXCO
     C     ELPFDT        ANDNE     ELPTDT
     C     XEST          ANDNE     XXST
     C     EXBEST        ANDEQ     'Y'
     C*
     C                   Z-ADD     0             XXPREM
     C*
     C     DSKY          CHAIN(N)  PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C     MPST          IFEQ      XXST
     C                   ADD       MPAAPR        XXPREM
     C                   SUB       MPPRTD        XXPREM
     C                   ENDIF
     C     DSKY          READE(N)  PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C     XXPREM        IFGT      XEPREM
     C                   MOVE      XXST          XEST
     C                   Z-ADD     ELDIV         XEDIV
     C                   Z-ADD     XXPREM        XEPREM
     C                   ENDIF                                                  XXPREM IFGT
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   ENDIF                                                  *IN60 IFEQ
      *
      * STORE PA EXPENSE CONSTANT.
      *
      *
      * READ NEXT EMPLR DETAIL RECORD.
      *
     C     ELKY1         READE     PEL01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
      *
     C                   ENDIF                                                  XXEHF IFEQ
      *
     C                   ENDIF                                                  FNPEOF IFEQ
     C*
     C* RESET DIVISION NUMBER.
     C*
     C                   Z-ADD     X1DIV         XXDIV
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C     SVCNDT        IFNE      0
     C                   Z-ADD     SVCNDT        ELPTDT
     C                   ENDIF
     C*
     C     ELSRPR        IFNE      'S'
      *
     C     XXSFUL        IFNE      'Y'
     C     SVCNCL        OREQ      'Y'
     C     ELCOFL        OREQ      'Y'
     C     XEPROR        MULT(H)   XXEXCO        XXEXCN           15 4
     C                   ELSE
     C                   Z-ADD(H)  XXEXCO        XXEXCN           15 4
     C                   ENDIF                                                  XXSFUL IFNE
     C*** Begin Add ***  07/21/24  ******************************************
     C*  ROUND TO NEAREST DOLLAR
     C                   Z-ADD(H)  XXEXCN        XXSAVE
     C                   Z-ADD     XXSAVE        XXEXCN
     C*** End   Add ***  07/21/24  ******************************************
     C*** begin add ***  06/26/23  ******************************************
     C                   If        xxProrNR <> 'Y'
     C*** end add *****  06/26/23  ******************************************
     C*** Beg Chg *** 09/18/25 **************************************
     C***  SVCNCL        IFEQ      'Y'
     C***  XXCXCD        ANDEQ     '03'
     C***  SVCNCL        OREQ      'Y'
     C***  XXCXCD        ANDEQ     '08'
     C***  SVCNCL        OREQ      'Y'
     C***  XXCXCD        ANDEQ     '09'
     C***  SVCNCL        OREQ      'Y'
     C***  XXCXCD        ANDEQ     '10'
     C     SVCNCL        IFEQ      'Y'
     C     XXCXNOPRO     ANDEQ     'Y'
     C*** End Chg *** 09/18/25 **************************************
     C                   Z-ADD(H)  XXEXCO        XXEXCN           15 4
     C                   ENDIF                                                  XXSFUL IFNE
     C*** begin add ***  06/26/23  ******************************************
     C                   Endif
     C*** end add *****  06/26/23  ******************************************
      *
     C     XXEXCN        IFLT      XXMPEC
     C                   Z-ADD     XXMPEC        XXEXCN
     C                   ENDIF                                                  XXEXCN IFLT
      *
     C                   ELSE
      *
     C                   Z-ADD     XXEXCO        XXEXCN
     C     XXSVPC        MULT(H)   XXEXCN        XXSREX           11 0
     C                   Z-ADD     XXSREX        XXEXCN
      *
     C     XXEXCN        IFLT      XXMPEC
     C                   Z-ADD     XXMPEC        XXEXCN
     C                   ENDIF                                                  XXEXCN IFLT
      *                                                                ***
     C                   ENDIF                                                  EMSRPR IFNE
     C*
     C* TEMPORARY VAR FOR STORING EXPENSE CONSTANT WITHOUT BEING
     C* PRORATED FOR DETERMINING IF TO INVOKE MINIMUM PREMIUM.
     C*
     C                   Z-ADD     XXEXCO        XXTEXC           15 4
     C*
     C* TEMPORARY VAR FOR STORING EXPENSE CONSTANT THAT IS PRORATED
     C* FOR CANCELLED EMP'S TO SEE IF TO INVOKE MINIMUM PREMIUM.
     C*
     C                   Z-ADD     XXEXCN        XXCEXC           15 4
     C                   END
     C*  ROUND TO NEAREST DOLLAR                           |
     C                   Z-ADD(H)  XXEXCN        XXSAVE            9 0
     C                   Z-ADD     XXSAVE        XXEXCN
     C*
     C     XXECST        IFEQ      'PA'
     C     XXEXCO        ANDNE     *ZERO
     C     XXEXCO        ANDGT     XPEXCO
     C                   MOVE      'XX'          XXECST
     C                   ENDIF
     C                   if        xxFlgCalcUPSC
     C* Calculate surcharge on unearned portion of expense constant
     C                   eval      xxwk92 = (1 - 1 / xxPrmFact) * XXEXCO
     c                   Z-add(h)  xxwk92        xxwk10
     c*
     C                   If        xxwk10 < xxmpec
     c                   eval      xxwk10 = xxmpec
     c                   endif
     c*
     c                   eval      xunet = xunet + xxwk10
     c                   eval      UPSN(1) = UPSN(1) + xxwk10
     c*
     c                   eval      xxwk92 = xunet * xxUPSP/100
     c                   Z-add(h)  xxwk92        xxwk10
     c                   Z-add     xxwk10        xxUPSA
     c*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     c                   eval      UPSN(X) = UPSN(X) * xxUPSP/100
     C                   ADD       1             X
     C                   ENDDO
     c*
     C* Add to net premium for minimum premium determination
     C                   eval      xxnet = xxnet + xxUPSA
     C                   eval      xtnet = xtnet + xxUPSA
     C                   eval      x1net = x1net + xxUPSA
     C                   eval      x2net = x2net + xxUPSA
     C                   eval      x3net = x3net + xxUPSA
     C                   eval      x4net = x4net + xxUPSA
     C                   eval      x5net = x5net + xxUPSA
     C                   eval      x6net = x6net + xxUPSA
     C                   eval      x7net = x7net + xxUPSA
     C                   endif                                                  xxFlgCalcUPSC
     C*
     C*  CHECK MINIMUM PREMIUM
     C*
     C                   Z-ADD     0             XXCOMP
     C                   MOVE      *BLANKS       ELAMNP
     C     SVCNCL        IFEQ      'Y'
     C     ELCOFL        OREQ      'Y'
     C     XXCEXC        ADD       XXNET         XXWKCM           15 4
     C                   ELSE
     C     XXTEXC        ADD       XXNET         XXWKCM           15 4
     C                   END
     C*
     C*
     C* If Client 045, just use XXNET
     C*
     C     DSKY1         CHAIN     PMP01                              60
     C                   Z-ADD     XXCO#         MPCO#
     C                   Z-ADD     XXFND         MPFND
     C                   Z-ADD     XXEMP#        MPEMP#
     C                   Z-ADD     XXDIV         MPDIV
     C                   Z-ADD     XXFYR         MPFYR
     C*
     C* POPULATE PREMIUM FIELDS BEFORE MINIMUM PREMIUM IS CHECKED.
     C*
     C                   Z-ADD     XXDIS1        MPADC1
     C                   Z-ADD     XXDIS2        MPADC2
     C                   Z-ADD     XXD1D2        MPD1A2
     C                   Z-ADD     XXDIS3        MPADC3
     C                   Z-ADD     XXHCNA        MPAHCN
     C                   Z-ADD(H)  XXUPSA        XXWK10
     C                   Z-ADD     XXWK10        MPAUSA
     C     XXEXPP        SUB       XXPRXP        MPAXPA
     C                   Z-add     xxexpp        MPAMPM
     C                   Z-add     xxprxp        MPAPBM
     C                   Z-add     xxexp1        MPAXP1
     C                   Z-add     xxexp2        MPAXP2
     C                   Z-add     xxexp3        MPAXP3
     C                   Z-ADD(H)  XXSUBJ        XXWK10
     C                   Z-ADD     XXWK10        MPASBJ
     C                   Z-ADD     XXDIST        MPADCT
     C                   Z-ADD     XXD2ST        MPADD2
     C                   Z-ADD     XXDISM        MPADCM
     C                   Z-ADD     XXDISS        MPADCS
     C     XXDISD        MULT      -1            MPADC
     C                   Z-ADD     XXDEV2        MPADV2
     C                   Z-ADD     XXD2D2        MPD2A2
     C                   XFOOT(H)  WAV           XXWK10
     C                   Z-ADD     XXWK10        MPAWAV
     C                   Z-ADD     0             MPATCA
     C*
     C                   Z-ADD(H)  XXSTAN        XXWK10
     C                   Z-ADD     XXWK10        MPASPR
     C                   Z-ADD     XSMAN         MPAMPR
     C                   Z-ADD(H)  XXNORM        XXWK10
     C                   Z-ADD     XXWK10        MPANMP
     C     XXDIV         IFEQ      XEDIV
     C     EXBEST        ANDEQ     'Y'
     C     EMSTRP        OREQ      'Y'
     C     EXBEST        ANDNE     'Y'
     C     F1EXDV        OREQ      'Y'
     C                   Z-ADD     XXEXCN        MPPRTD
     C                   Z-ADD     MPPRTD        SVPRTD           11 2
      *** Begin Add *** 11/02/23 *******************************************
     C                   Z-ADD     XXEXCO        SVPRTF           11 2
      *** End   Add *** 11/02/23 *******************************************
     C                   ELSE
     C                   Z-ADD     0             SVPRTD
      *** Begin Add *** 11/02/23 *******************************************
     C                   Z-ADD     0             SVPRTF
      *** End   Add *** 11/02/23 *******************************************
     C     EXBEXP        IFEQ      'Y'
     C                   Z-ADD     XXEXCN        SVPRTD
      *** Begin Add *** 11/02/23 *******************************************
     C                   Z-ADD     XXEXCO        SVPRTF           11 2
      *** End   Add *** 11/02/23 *******************************************
     C                   ENDIF
     C                   Z-ADD     0             XXEXCN
     C                   Z-ADD     0             XXEXCO
     C                   ENDIF                                                  XXDIV IFEQ
     C     XXNET         ADD       MPPRTD        MPAAPR
     C                   Z-ADD     MPPRTD        S1PRTD           11 2
     C                   Z-ADD(H)  XLNET         XXWK10
     C                   Z-ADD     XXWK10        MPAAPL
     C                   MOVE      XXST          MPST
     C                   Z-ADD     XXVLCA        MPAVLC
     C                   MOVEL     Q1USER        MPUSER
     C                   MOVEL     Q1PGM         MPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         MPDT
     C                   Z-ADD     DSTIME        MPTIME
     C*** Beg Add *****  02/27/24  **************************************
     C                   Eval      MPSRAM = ELSRAM
     C*** End Add *****  02/27/24  **************************************
      //*** Beg Add ***** 08/14/24 **************************************
       MPain1 = %XFoot(LM);
       MPain2 = %XFoot(LM2);
       MPabil = %XFoot(LMB);
       MPabil2 = %XFoot(LMB2);
      //*** End Add ***** 08/14/24 **************************************
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PMP01
     C                   ELSE
     C                   WRITE     PMP01
     C                   ENDIF
     C                   Z-ADD     XXHIGH        XSHIGH           11 2
     C     ELAIN1        ADD       ELAIN2        XSILAM           11 2
     C*
     C*  STORE TOTAL WAIVER SURCHARGE AMOUNT.
     C*
     C                   XFOOT(H)  WAV           XXWK10
     C                   Z-ADD     XXWK10        XXWVMN
     C*** begin add ***  11/08/24  ****************************************
     C                   Z-ADD     0             xxActp           11 2
     C*** end add *****  11/08/24  ****************************************
     C     EMSTRP        IFEQ      'Y'
     C*
     C     DSKY          CHAIN     PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     MPDIV         IFNE      ELDIV
     C                   ADD       MPAAPR        XXWKCM
     C                   SUB       MPPRTD        XXWKCM
     C                   ADD       MPAWAV        XXWVMN
     C                   ENDIF                                                  MPDIV IFNE
     C*** begin add ***  11/08/24  ****************************************
     C                   ADD       MPAAPR        xxActp
     C*** end add *****  11/08/24  ****************************************
     C*
     C     DSKY          READE     PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ADD       XXWVMN        XXHIGH
     C                   ADD       XXWVMN        XSHIGH
     C     XXCLT#        IFEQ      '102'
     C                   ADD       SVPRTD        XXHIGH
     C                   ENDIF                                                  XXCLT#='102'
     C                   Z-ADD     XXHIGH        SVHIGH           11 2
     C     XXHIGH        IFGT      XXWKCM
     C*** begin del ***  02/19/24  *****************************************
     C*****XXCLT#        ANDNE     '102'
     C*****XXHIGH        ORGT      XXWKCM
     C*****XXCLT#        ANDEQ     '102'
     C*****ELFFL2        ANDEQ     *BLANK
     C*** end del *****  02/19/24  *****************************************
     C                   MOVE      'Y'           ELAMNP
     C                   Z-ADD     XXHIGH        XXNET
     C                   Z-ADD     XXHIGH        XXNORM
     C                   Z-ADD     XXNET         XXCOMP           15 4          RLS 2  JJJ
     C* Minimum premium invoke, clear unearned premium sucharge (if any)
     C                   Z-ADD     0             xxUPSA
     C*
     C* If Client 045, Don't zero XXEXCN & XXEXCO
     C                   Z-ADD     0             XXEXCN                         RLS 2  SAJ
     C                   Z-ADD     0             XXEXCO
     C*
     C     QQAR          IFNE      *BLANKS
     C                   SETOFF                                       60
      *** Beg Del *****  07/07/23  *******************************************
      * Do not update the class code value to xxhigh
      * and Do not zero out the other classes
     C*****EPKEY         SETLL     PEP01
     C******IN60         DOUEQ     '1'
     C*****EPKEY         READE     PEP01                                  60
     C******IN60         IFEQ      '0'
     C*****EPSTAT        ANDEQ     'A'
     C*****EPCLAS        IFEQ      SVCLAS
     C*****              Z-ADD     XXHIGH        EPAMPR
     C*****              Z-ADD     *ZERO         XXHIGH
     C*****              UPDATE    PEP01
     C*****              ELSE
     C*****              Z-ADD     *ZERO         EPAMPR
     C*****              UPDATE    PEP01
     C*****              END
     C*****              END
     C*****              END
      *** End Del *****  07/07/23  *******************************************
     C                   ENDIF                                                  TAX IFEQ 2
     C                   END
     C                   ENDIF                                                  EMSTRP IFEQ
     C*
     C*  CALCULATE THE NET RATE AND NET PREMIUM FOR EACH PAYROLL
     C*  CLASS, AND THEN OVERRIDE NORMAL PREMIUM WITH THE TOTAL.
     C*
     C     F1NETR        IFEQ      'Y'
     C     ELPFDT        ANDGE     F1NETD
     C     *IN94         ANDEQ     *OFF
     C     *IN95         ANDEQ     *OFF
     C     *IN96         ANDEQ     *OFF
     C     *IN97         ANDEQ     *OFF
     C     xxclt#        oreq      '093'
     C     *IN94         ANDEQ     *OFF
     C     *IN95         ANDEQ     *OFF
     C     *IN96         ANDEQ     *OFF
     C     *IN97         ANDEQ     *OFF
     C*
     C                   Z-ADD     0             XXTMAN           11 2
     C                   Z-ADD     0             XXANMP           11 2
     C                   Z-ADD     0             XXTNET           11 2
     C*
     C* TOTAL THE MANUAL AND NORMAL PREMIUM FOR THE STATE.
     C*
     C     DSKY          CHAIN(N)  PMP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     MPST          IFEQ      XXST
     C                   ADD       MPAMPR        XXTMAN
     C                   ADD       MPANMP        XXANMP
     C                   ENDIF
     C*
     C     DSKY          READE(N)  PMP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* CALCULATE THE NET RATE FACTOR.
     C*
     C     XXTMAN        IFNE      *ZERO
     C     XXANMP        DIV(H)    XXTMAN        XXNETR            9 6
     C                   ELSE
     C                   Z-ADD     1             XXNETR
     C                   ENDIF                                                  XXTMAN IFNE
     C*
     C* APPLY THE NET RATE FACTOR AGAINST ALL ACTIVE PAYROLLS.
     C*
     C     EPKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C*
     c                   if        xxnetf = 'Y'
     C                   ADD       epantp        XXTNET
     c                   else
     C                   SELECT
     C     ELAMNP        WHENEQ    'Y'
     C                   Z-ADD     EPAMPR        EPANTP
     C                   ADD       EPANTP        XXTNET
     C                   OTHER
     C     XXNETR        MULT(H)   EPARAT        EPANTR
     C     XXNETR        MULT(H)   EPAMPR        XXWK10
     C                   Z-ADD     XXWK10        EPANTP
     C                   ADD       XXWK10        XXTNET
     C                   ENDSL
     C*
     C                   UPDATE    PEP01
     c                   endif                                                  xxnetf if = 'Y'
     C*
     C                   ENDIF                                                  EPSTAT IFNE
     C*
     C     EPKEY         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* OVERRIDE THE NORMAL PREMIUM FIGURE WITH TOTAL OF NET
     C* PREMIUMS IF MINIMUM PREMIUM HAS NOT BEEN INVOKED.
     C*
     C     ELAMNP        IFNE      'Y'
     c     XXCLT#        ANDNE     '093'
     C     XXCLT#        IFEQ      '102'
     C     ELFFL2        ANDEQ     *BLANKS
      * MINIMUM PREMIUM INVOKED, DO NOT OVERRIDE
     C                   ELSE
     C                   Z-ADD     XXTNET        XXNORM
     C                   Z-ADD     XXTNET        XXNET
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF                                                  F1NETR IFEQ

      * Get Policy Fee
      /Free
       xxAPF = GetPolFee();
       If xxAPF <> 0;
          xxNet += xxAPF;
          xtNet += xxAPF;
          x1Net += xxAPF;
          x2Net += xxAPF;
          x3Net += xxAPF;
          x4Net += xxAPF;
          x5Net += xxAPF;
          x6Net += xxAPF;
          x7Net += xxAPF;
       Endif;
      /End-Free

     C     XXHXMN        IFNE      *ZERO
     C                   ADD       XXHXMN        XXNET
     C                   ADD       XXHXMN        XXHIGH
     C                   ADD       XXHXMN        XSHIGH
     C                   ADD       XXHXMN        XTNET
     C                   ADD       XXHXMN        X1NET
     C                   ADD       XXHXMN        X2NET
     C                   ADD       XXHXMN        X3NET
     C                   ADD       XXHXMN        X4NET
     C                   ADD       XXHXMN        X5NET
     C                   ADD       XXHXMN        X6NET
     C                   ADD       XXHXMN        X7NET
     C                   ENDIF
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C*
     C     ELAMNP        IFEQ      'Y'
     C     XXNET         MULT(H)   PRO(X)        ANL(X)
     C                   ELSE
     C                   Z-ADD(H)  NOM(X)        ANL(X)
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C     ELAMNP        IFNE      'Y'
     C                   ADD       XXEXCN        ANL(1)
     C                   ENDIF
      *
      * Calculates the aircraft surchage
      *
     C                   Z-ADD     *ZEROS        XXEARC           11 2
      *
     C     ELARCP        IFNE      *ZEROS
     C     ELARCP        MULT      XXNET         XXEARC
     C     XXEARC        DIV(H)    100           XXWK10
     C                   Z-ADD     XXWK10        XXEARC
     C                   ENDIF
      *
     C     ELARCA        IFGT      XXEARC
     C                   Z-ADD     ELARCA        XXEARC
     C                   ENDIF
     C*
     C* FIND # DAYS IN THE AIRCRAFT RE-INSURANCE PERIOD
     C*
     C     ELAREF        IFGT      ELPFDT
     C     ELAREF        ANDNE     *ZERO
     C     ELAREX        ORLT      ELPTDT
     C     ELAREX        ANDNE     *ZERO
     C*
     C     ELAREF        IFNE      *ZERO
     C                   Z-ADD     ELAREF        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPFDT        SRDTI
     C                   ENDIF
     C     ELAREF        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   ENDIF
     C*
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM
     C*
     C     ELAREX        IFNE      *ZERO
     C                   Z-ADD     ELAREX        SRDTI
     C                   ELSE
     C                   Z-ADD     ELPTDT        SRDTI
     C                   ENDIF
     C     ELAREX        IFGT      ELPTDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   ENDIF
     C*
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
     C* PRORATES THE NUMBER OF DAYS
     C                   Z-ADD     QQDAYS        XXARDY            5 0
     C     PDAYS         IFNE      *ZERO
     C     XXARDY        DIV(H)    PDAYS         XXDPRO
     C     XXEARC        MULT(H)   XXDPRO        XXWK10
     C                   Z-ADD     XXWK10        XXEARC
     C                   ENDIF
      *
     C                   ENDIF                                                  ELAREF IFNE
      *
     C                   ADD       XXEARC        XXNET
     C                   ADD       XXEARC        XSHIGH
     C                   ADD       XXEARC        XTNET
     C                   ADD       XXEARC        X1NET
     C                   ADD       XXEARC        X2NET
     C                   ADD       XXEARC        X3NET
     C                   ADD       XXEARC        X4NET
     C                   ADD       XXEARC        X5NET
     C                   ADD       XXEARC        X6NET
     C                   ADD       XXEARC        X7NET
      *
     C*** begin add ***  06/15/23  **************************************
     C     DSKY1         CHAIN     PMP01                              60
     C                   If        *in60 = *off
     C                   Z-add     XXEARC        MPAARC
     C                   Z-add     XXLOSA        MPALOS
     C                   Z-ADD     XXARP1        MPRPA1
     C                   Z-ADD     XXARP2        MPRPA2
     C                   Z-ADD     XXARP3        MPRPA3
     C*** begin add ***  11/07/23  **************************************
     C* Balance to minimum
     C                   If        ELAmnp = 'Y'
     C*** begin chg ***  11/08/24  ****************************************
     C********           Eval      MPABal = xxHigh - xxwkcm
     C                   Eval      MPABal = xxHigh - xxActp
     C*** end chg *****  11/08/24  ****************************************
     C                   Else
     C                   Eval      MPABal = 0
     C                   Endif
     C*** end add *****  11/07/23  **************************************
     C*** Beg Add *****  02/27/24  **************************************
     C                   Eval      MPSRAM = ELSRAM
     C*** End Add *****  02/27/24  **************************************
     C                   Update    pmp01
     C                   Endif
     C*** end add *****  06/15/23  **************************************
     C*
     C*  CALCULATE AND ADD TERRORISM RISK ACT AMOUNT TO ANNUAL PREMIUM.
     C*
     C                   Z-ADD     0             XXPAYR           15 4
     C                   Z-ADD     0             XXADTR           15 4
     C*
     C                   Z-ADD     1             X
     C*
     C     FDAT(X)       DOWNE     *ZERO
     C*
     C                   Z-ADD     FDAT(X)       XXEFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C*
     C* GET LCM
     C*
     C     RRTERR        IFNE      *ZERO
     C     RRDTRR        ORNE      *ZERO
     C                   MOVE(P)   ELCO#         QQCO#
     C                   MOVE(P)   ELFND         QQFND
     C                   MOVE(P)   ELRTCD        QQRTCD
     C                   MOVE(P)   RRTERC        QQCLAS
     C                   MOVE(P)   XXEFDT        QQEFDT
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
     C*
     C                   ENDIF                                                  RRTERR <> *ZERO
     C*****************
     C**DOMESTIC*TERRORISM SURCHARGE
      /free
       // Add foreign terrorism to premium basis for assessment calc
       If %xfoot(pyTrt) > 0;
          xxPayr = xxPayr + etr(x);
          ert(x) = pyTrt(x);
          eval(h) etr(x) = etr(x)  * ELpror;
          stn1(x) = stn1(x) + etr(x);
          stn2(x) = stn2(x) + etr(x);
          stn3(x) = stn3(x) + etr(x);
          stn4(x) = stn4(x) + etr(x);
          stn5(x) = stn5(x) + etr(x);
          stn6(x) = stn6(x) + etr(x);
          stn7(x) = stn7(x) + etr(x);
       Else;
          pay(x) = 0;
       Endif;
       // Add domestic terrorism to premium basis for assessment calc
       If %xfoot(dmTrt) > 0;
          xxAdTr = xxAdTr + dtr(x);
          drt(x) = dmTrt(x);
          eval(h) dtr(x) = dtr(x) * ELpror;
          stn1(x) = stn1(x) + dtr(x);
          stn2(x) = stn2(x) + dtr(x);
          stn3(x) = stn3(x) + dtr(x);
          stn4(x) = stn4(x) + dtr(x);
          stn5(x) = stn5(x) + dtr(x);
          stn6(x) = stn6(x) + dtr(x);
          stn7(x) = stn7(x) + dtr(x);
       Endif;
       // Add non-payroll terrorism to premium basis for assessment calc
       If %xfoot(npTrt) > 0;
          xxPayr = xxPayr + ntr(x);
          eval(h) ntr(x) = ntr(x) * ELpror;
          stn1(x) = stn1(x) + ntr(x);
          stn2(x) = stn2(x) + ntr(x);
          stn3(x) = stn3(x) + ntr(x);
          stn4(x) = stn4(x) + ntr(x);
          stn5(x) = stn5(x) + ntr(x);
          stn6(x) = stn6(x) + ntr(x);
          stn7(x) = stn7(x) + ntr(x);
       Endif;
      /end-free
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   Z-ADD(H)  XXPAYR        XXWK10
     C                   Z-ADD     XXWK10        XXPAYR
     C*
     C* Calculate surcharge on unearned portion of Foreign terrorism charge
     C*
     C                   ADD       XXPAYR        XXNET
     C                   ADD       XXPAYR        XTNET
     C                   ADD       XXPAYR        X1NET
     C                   ADD       XXPAYR        X2NET
     C                   ADD       XXPAYR        X3NET
     C                   ADD       XXPAYR        X4NET
     C                   ADD       XXPAYR        X5NET
     C                   ADD       XXPAYR        X6NET
     C                   ADD       XXPAYR        X7NET
     C                   Z-ADD     1             XT                2 0
     C                   DOW       XT <= 7
     C                   EVAL      XSHGH(XT) = XSHIGH
     C                   EVAL      XT = XT + 1
     C                   ENDDO
     C*** FOR FIRST ASSESSMENT
     C                   IF        (RR1ATR = 'B') OR (RR1ATR = 'F')
     C                   EVAL      XSHGH(1) = XSHGH(1) + XXPAYR
     C                   ENDIF
     C*** FOR SECOND ASSESSMENT
     C                   IF        (RR2ATR = 'B') OR (RR2ATR = 'F')
     C                   EVAL      XSHGH(2) = XSHGH(2) + XXPAYR
     C                   ENDIF
     C*** FOR THIRD ASSESSMENT
     C                   IF        (RR3ATR = 'B') OR (RR3ATR = 'F')
     C                   EVAL      XSHGH(3) = XSHGH(3) + XXPAYR
     C                   ENDIF
     C*** FOR FOURTH ASSESSMENT
     C                   IF        (RR4ATR = 'B') OR (RR4ATR = 'F')
     C                   EVAL      XSHGH(4) = XSHGH(4) + XXPAYR
     C                   ENDIF
     C*** FOR FIFTH ASSESSMENT
     C                   IF        (RR5ATR = 'B') OR (RR5ATR = 'F')
     C                   EVAL      XSHGH(5) = XSHGH(5) + XXPAYR
     C                   ENDIF
     C*** FOR SIXTH ASSESSMENT
     C                   IF        (RR6ATR = 'B') OR (RR6ATR = 'F')
     C                   EVAL      XSHGH(6) = XSHGH(6) + XXPAYR
     C                   ENDIF
     C*** FOR SEVENTH ASSESSMENT
     C                   IF        (RR7ATR = 'B') OR (RR7ATR = 'F')
     C                   EVAL      XSHGH(7) = XSHGH(7) + XXPAYR
     C                   ENDIF
     C*
     C                   Z-ADD(H)  XXADTR        XXWK10
     C                   Z-ADD     XXWK10        XXADTR
     C*
     C* Calculate surcharge on unearned portion of Domestic terrorism charge
     C*
     C     ELPROR        MULT(H)   XXADTR        XXWK10
     C                   ADD       XXADTR        XXNET
     C                   ADD       XXADTR        XTNET
     C                   ADD       XXADTR        X1NET
     C                   ADD       XXADTR        X2NET
     C                   ADD       XXADTR        X3NET
     C                   ADD       XXADTR        X4NET
     C                   ADD       XXADTR        X5NET
     C                   ADD       XXADTR        X6NET
     C                   ADD       XXADTR        X7NET
     C*
     C*** FOR FIRST ASSESSMENT
     C                   IF        (RR1ATR = 'B') OR (RR1ATR = 'D')
     C                   EVAL      XSHGH(1) = XSHGH(1) + XXADTR
     C                   ENDIF
     C*** FOR SECOND ASSESSMENT
     C                   IF        (RR2ATR = 'B') OR (RR2ATR = 'D')
     C                   EVAL      XSHGH(2) = XSHGH(2) + XXADTR
     C                   ENDIF
     C*** FOR THIRD ASSESSMENT
     C                   IF        (RR3ATR = 'B') OR (RR3ATR = 'D')
     C                   EVAL      XSHGH(3) = XSHGH(3) + XXADTR
     C                   ENDIF
     C*** FOR FOURTH ASSESSMENT
     C                   IF        (RR4ATR = 'B') OR (RR4ATR = 'D')
     C                   EVAL      XSHGH(4) = XSHGH(4) + XXADTR
     C                   ENDIF
     C*** FOR FIFTH ASSESSMENT
     C                   IF        (RR5ATR = 'B') OR (RR5ATR = 'D')
     C                   EVAL      XSHGH(5) = XSHGH(5) + XXADTR
     C                   ENDIF
     C*** FOR SIXTH ASSESSMENT
     C                   IF        (RR6ATR = 'B') OR (RR6ATR = 'D')
     C                   EVAL      XSHGH(6) = XSHGH(6) + XXADTR
     C                   ENDIF
     C*** FOR SEVENTH ASSESSMENT
     C                   IF        (RR7ATR = 'B') OR (RR7ATR = 'D')
     C                   EVAL      XSHGH(7) = XSHGH(7) + XXADTR
     C                   ENDIF
     C*
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C*
     C*
     C*
     C     ELAMNP        IFEQ      'Y'
     C                   CLEAR                   XXANCC
     C                   CLEAR                   XXANCM
     C     ELANCF        IFEQ      'Y'
     C     XXNET         MULT(H)   RRANCM        XXANCC            9 0
     C                   Z-ADD     RRANCM        XXANCM            5 4
     C                   ADD       XXANCC        XXNET
     C                   ENDIF
     C     XXNET         MULT      RRTXPC        XXTXDU            9 0
     C                   MULT(H)   .01           XXTXDU
     C                   Z-ADD     XXNET         XXTXPR            9 0

     C                   ELSE
     C                   ADD       XXEXCN        XTNET
     C                   Z-ADD     XXEXCN        X1EXCN           15 4
     C                   ADD       XXEXCN        X1NET
     C                   ADD       XXEXCN        X2NET
     C                   ADD       XXEXCN        X3NET
     C                   ADD       XXEXCN        X4NET
     C                   ADD       XXEXCN        X5NET
     C                   ADD       XXEXCN        X6NET
     C                   ADD       XXEXCN        X7NET
     C     XXECST        IFEQ      'XX'
     C                   SUB       XXEXCN        X1NET
     C                   SUB       XXEXCN        X2NET
     C                   SUB       XXEXCN        X3NET
     C                   SUB       XXEXCN        X4NET
     C                   SUB       XXEXCN        X5NET
     C                   SUB       XXEXCN        X6NET
     C                   SUB       XXEXCN        X7NET
     C                   ENDIF
     C                   CLEAR                   XXANCC
     C                   CLEAR                   XXANCM
     C     ELANCF        IFEQ      'Y'
     C     XTNET         MULT(H)   RRANCM        XXANCC            9 0
     C                   Z-ADD     RRANCM        XXANCM
     C                   ADD       XXANCC        XXNET
     C                   ADD       XXANCC        XTNET
     C                   ADD       XXANCC        X1NET
     C                   ADD       XXANCC        X2NET
     C                   ADD       XXANCC        X3NET
     C                   ADD       XXANCC        X4NET
     C                   ADD       XXANCC        X5NET
     C                   ADD       XXANCC        X6NET
     C                   ADD       XXANCC        X7NET
     C                   ENDIF
     C     XTNET         MULT      RRTXPC        XXTXDU            9 0
     C                   MULT(H)   .01           XXTXDU
     C                   Z-ADD     XTNET         XXTXPR            9 0
     C                   ENDIF
     C*
     C     ELAMNP        IFEQ      'Y'
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXNET         MULT(H)   PRO(X)        STN1(X)
     C     XXNET         MULT(H)   PRO(X)        STN2(X)
     C     XXNET         MULT(H)   PRO(X)        STN3(X)
     C     XXNET         MULT(H)   PRO(X)        STN4(X)
     C     XXNET         MULT(H)   PRO(X)        STN5(X)
     C     XXNET         MULT(H)   PRO(X)        STN6(X)
     C     XXNET         MULT(H)   PRO(X)        STN7(X)
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     STN1          XXPREM           15 4
     C*
     C     XXPREM        IFNE      XXNET
     C     XXNET         SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        STN1(1)
     C                   ADD       XXPREM        STN2(1)
     C                   ADD       XXPREM        STN3(1)
     C                   ADD       XXPREM        STN4(1)
     C                   ADD       XXPREM        STN5(1)
     C                   ADD       XXPREM        STN6(1)
     C                   ADD       XXPREM        STN7(1)
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XXECST        IFNE      'XX'
     C     PRO(X)        MULT      XXEXCN        XXPREM
     C                   ADD       XXPREM        STN1(X)
     C                   ADD       XXPREM        STN2(X)
     C                   ADD       XXPREM        STN3(X)
     C                   ADD       XXPREM        STN4(X)
     C                   ADD       XXPREM        STN5(X)
     C                   ADD       XXPREM        STN6(X)
     C                   ADD       XXPREM        STN7(X)
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     STN1          XXPREM           15 4
     C*
     C     XXPREM        IFNE      X1NET
     C     X1NET         SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        STN1(1)
     C                   ENDIF
     C*
     C                   XFOOT     STN2          XXPREM           15 4
     C*
     C     XXPREM        IFNE      X2NET
     C     X2NET         SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        STN2(1)
     C                   ENDIF
     C*
     C                   XFOOT     STN3          XXPREM           15 4
     C*
     C     XXPREM        IFNE      X3NET
     C     X3NET         SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        STN3(1)
     C                   ENDIF
     C*
     C                   XFOOT     STN4          XXPREM           15 4
     C*
     C     XXPREM        IFNE      X4NET
     C     X4NET         SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        STN4(1)
     C                   ENDIF
     C*
     C                   XFOOT     STN5          XXPREM           15 4
     C*
     C     XXPREM        IFNE      X5NET
     C     X5NET         SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        STN5(1)
     C                   ENDIF
     C                   XFOOT     STN6          XXPREM           15 4
     C*
     C     XXPREM        IFNE      X6NET
     C     X6NET         SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        STN6(1)
     C                   ENDIF
     C*
     C                   XFOOT     STN7          XXPREM           15 4
     C*
     C     XXPREM        IFNE      X7NET
     C     X7NET         SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        STN7(1)
     C                   ENDIF
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*  SAVE CHANGE AMOUNT TO BE WRITTEN AS ADJUSTMENT
     C*
     C     QQAR          IFNE      *BLANKS
     C     XXREV         OREQ      '2'
     C     XXREV         OREQ      '5'
     C     *in97         OREQ      *on
     C     ELARSM        IFEQ      'Y'
     C     EMSTRP        ANDNE     'Y'
     C     F1UPAR        ANDEQ     'Y'
     C     QQAR          ANDNE     *BLANKS
     C                   EXSR      ADJAR
     C                   ENDIF
     C*
     C*  GET ALL BILLING INFORMATION FOR THE FUND YEAR
     C*
     C                   Z-ADD     *ZERO         XXFB92                         | 7-8-88
     C                   Z-ADD     0             XXWK92
     C                   Z-ADD     0             XXTXBL           15 4
     C     EAKEY         CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* IGNORE BILLINGS FOR RETROSPECTIVE RATING.
     C*
     C     TTCTYP        IFNE      'C'
     C     EAARCD        ANDNE     F1DVBL
     C     EAARTY        IFEQ      'B'
     C     EAARTY        OREQ      'D'
     C     EAARTY        OREQ      'A'
     C     TTCTYP        IFNE      'E'
     C                   ADD       EAAMT         XXWK92           15 4
     C                   ELSE
     C                   ADD       EAAMT         XXFB92           15 4
     C                   END
     C                   END
     C                   ENDIF                                                  TTCTYP IFNE
     C*  ACCUMULATE THE STATE TAX BILLED.
     C*
     C     TTCTYP        IFEQ      'T'
     C     EAARTY        ANDEQ     'F'
     C                   ADD       EAAMT         XXTXBL
     C                   END
     C                   ENDIF                                                  *IN60 IFEQ
     C     EAKEY         READE     PEA01                                  60
     C                   END
     C*
     C*   ACCUMULATE MEMBERSHIP FEE BILLINGS ALREADY ON FILE FOR FYR
     C*
     C*
     C                   Z-ADD     0             XXMFBT           15 4
     C                   MOVE      'F'           XXARTY
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'M'                                          MEMBERSHIP FEES
     C                   ADD       EAAMT         XXMFBT
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   END
     C*
     C                   Z-ADD     *ZERO         XXMFEE           15 4
     C     SVCNCL        IFEQ      'Y'
     C     F1CXAD        ANDEQ     'Y'
     C     SVCNCL        OREQ      ' '
     C     F1YRAD        ANDEQ     'Y'
     C     F1PMFL        IFEQ      'Y'                                          PRORATE MEMBER FEE
     C     PDAYS         ANDNE     FDAYS
     C                   Z-ADD     *ZEROS        XXDPRO
     C     FDAYS         IFNE      *ZERO
     C     PDAYS         DIV(H)    FDAYS         XXDPRO
     C                   ENDIF                                                  IF FDAYS <> 0
     C     F1MFEE        MULT(H)   XXDPRO        XXMFEE
     C                   Z-ADD(H)  XXMFEE        XXWK10
     C                   Z-ADD     XXWK10        XXMFEE
     C                   ELSE
     C                   Z-ADD     SVMFEE        XXMFEE                         BILL FULL MEMBER FEE
     C                   ENDIF
     C*
     C* If Membership Fee Exception Amt exists for employer, use that
     C*
     C     XXMFEE        SUB       XXMFBT        XXDIFM           15 4
     C*
     C                   ELSE
     C                   Z-ADD     0             XXDIFM
     C                   ENDIF                                                  SVCNCL IFEQ
     C     ELPFDT        IFEQ      ELPTDT                                       IF FLAT CX
     C                   Z-ADD     0             XXMFEE
     C                   Z-SUB     XXMFBT        XXDIFM
     C                   ENDIF
     C*
     C*   ACCUMULATE SAFETY FEE BILLINGS ALREADY ON FILE FOR FYR
     C*
     C*
     C*
     C*
     C*   CALCULATE SAFETY FEE BILLED.
     C*
     C*
     C*
     C*   ACCUMULATE 1ST ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*   CALCULATE 1ST ASSESSMENT BILLED.
     C*
     C*
     C*
     C*
     C*
     C*   ACCUMULATE 2ND ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
     C*
     C*
     C*
     C*   CALCULATE 2ND ASSESSMENT BILLED.
     C*
     C*
     C*
     C*
     C*
     C*   ACCUMULATE 3RD ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
     C*
     C*
     C*
     C*   CALCULATE 3RD ASSESSMENT BILLED.
     C*
     C*
     C*
     C     XXNET         SUB       XXWK92        XXDIFF           15 4          RLS 2  SAJ
     C     XXEXCN        SUB       XXFB92        XXDIFE           15 4          RLS 2  JJJ
     C                   ADD       XXEXCN        XXNET                          RLS 2  SAJ
     C*
     C     EXBEXP        IFEQ      'Y'
     C     ELAMNP        IFEQ      'Y'
     C                   SUB       SVPRTD        XXDIFF
     C     ELDIV         IFEQ      XEDIV
     C                   ADD       SVPRTD        XXDIFE
     C                   ENDIF                                                  ELDIV IFEQ
     C                   ENDIF                                                  ELAMNP IFEQ
     C                   ELSE
     C                   Z-ADD     0             SVPRTD           11 2
     C                   ENDIF                                                  EXBEXP IFEQ
     C*
     C*  CALCULATE STATE TAX DIFFERENCE
     C*
     C     XXTXDU        SUB       XXTXBL        XXDIFX           15 4
     C*
     C*  UPDATE EMPLOYER DETAIL FILE
     C*
      *
      * SAVE OFF DIFFERENCE BETWEEN MINIMUM PREM AND MANUAL PREMIUM TO
      * APPLY TO NY STATE ASSESSMENT
      *
     C                   CLEAR                   XXNYCH
     C     ELAMNP        IFEQ      'Y'
     C     SVHIGH        SUB(H)    XXWKCM        XXNYCH           11 0
     C                   ENDIF
     C*
     C     XXUPAR        IFEQ      'Y'
     C                   MOVE      'A'           ELFAF
     C                   ENDIF
     C*
     C     Q1REV         IFEQ      'Y'
     C                   MOVE      'R'           ELRAFL
     C                   ELSE
     C                   MOVE      ' '           ELRAFL
     C                   ENDIF                                                  QQREV IFEQ
     C*** begin add ***  09/24/24  *****************************************
     C                   If        xxLrgD = 'Y'
     C                   Clear                   xxddct
     C                   Endif
     C*** end add *****  09/24/24  *****************************************
     C                   MOVE      XXPROR        ELPROR
     C                   MOVE      XXDDCT        ELDDCT
     C                   MOVE      XXDD2P        ELDD2P
     C*
     C* SPECIAL CODE ADDED FOR FCC TO USE THE PFDT FOR RATING THE ASSESSMENTS
     C* SO THAT 106 YEAR POLICIES WILL GET THE MA DIA, BUT STILL ALLOW THE
     C* ANNIV DATE TO BE USED TO SPLIT THE PAYROLLS.
     C*
     C     XXST          IFEQ      'MA'
     C     PDAT(1)       ANDGE     1060101
     C     RDAT(1)       ANDLT     1060101
     C     XXCLT#        ANDEQ     '129'
     C                   Z-ADD     PDAT(1)       XXEFDT
     C                   ELSE
     C                   Z-ADD     FDAT(1)       XXEFDT
     C                   ENDIF
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C                   Z-ADD     0             XX932A           11 2
     C                   Z-ADD     0             XX932F           11 2
      /Free
       A932 = *zeros;
       F932 = *zeros;
      /End-Free
     C                   Z-ADD     0             EL1FPR
     C                   Z-ADD     0             EL2FPR
     C                   XFOOT(H)  LM            XXLM1            11 0
     C                   XFOOT(H)  LM2           XXLM2            11 0
     C                   EVAL      XXTEIN = XXLM1 + XXLM2
     C                   Z-ADD     0             EL1APR
     C     RR1STP        IFNE      *ZERO
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDNE     'NJ'
     C     XXST          ANDNE     'NY'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR1STL        ANDEQ     'X'
     C                   Z-ADD     XXNET         EL1APR
     C*** FIRST ASSESSMENT
     C                   IF        (RR1ATR <> 'B') AND (RR1ATR <> 'F')
     C                   SUB       XXPAYR        EL1APR
     C                   ENDIF
     C                   IF        (RR1ATR <> 'B') AND (RR1ATR <> 'D')
     C                   SUB       XXADTR        EL1APR
     C                   ENDIF
     C                   ELSE
     C                   SELECT
     C     RR1STL        WHENEQ    'J'
     C                   Z-ADD     X1SUBJ        EL1APR
     C     RR1STL        WHENEQ    'E'
     C                   Z-ADD     X1EXPP        EL1APR
     C     RR1STL        WHENEQ    'S'
     C                   Z-ADD     X1STAN        EL1APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL1APR
     C                   ENDIF
     C     RR1STL        WHENEQ    'D'
     C                   Z-ADD     X1DISP        EL1APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL1APR
     C                   ENDIF
     C     RR1STL        WHENEQ    'X'
     C                   Z-ADD     X1NET         EL1APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL1APR
     C                   ENDIF
     C*** FIRST ASSESSMENT
     C                   IF        (RR1ATR <> 'B') AND (RR1ATR <> 'F')
     C                   SUB       XXPAYR        EL1APR
     C                   ENDIF
     C                   IF        (RR1ATR <> 'B') AND (RR1ATR <> 'D')
     C                   SUB       XXADTR        EL1APR
     C                   ENDIF
     C                   ENDSL                                                  RR1STL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR1IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR1STL        ANDNE     'J'
     C     RR1STL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR1STL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR1STL        ANDNE     'J'
     C     RR1STL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR1STL        ANDNE     'J'
     C     RR1STL        ANDNE     'E'
     C     RR1STL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR1STL        ANDNE     'J'
     C     RR1STL        ANDNE     'E'
     C     RR1STL        ANDNE     'S'
     C     RR1STL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   ADD       XXDIST        EL1APR
     C                   ADD       XXD2ST        EL1APR
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C                   IF        RR1ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm1       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   EL1APR = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR1ILF:RR1STL:X1SUBJ:X1EXPP:X1STAN:
     C                                      X1DISP:X1NET:ELAMNP:RR1ATR)
     C*
     C                   ENDIF                                                  RR1ILF <> ' '
     C                   IF        RR1STL <> 'X'
     C                   IF        (RR1ATR = 'B') OR (RR1ATR = 'F')
     C                   ADD       XXPAYR        EL1APR
     C                   ENDIF
     C                   IF        (RR1ATR = 'B') OR (RR1ATR = 'D')
     C                   ADD       XXADTR        EL1APR
     C                   ENDIF
     C                   ENDIF                                                  RR1STL <> X
     C                   ENDIF                                                  ELAMNP IFEQ
     C     RR1STF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR1xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C     XXFPCT        MULT(H)   EL1APR        XXWK10
     C                   Z-ADD     XXWK10        EL1FPR
     C                   SUB       EL1FPR        EL1APR
     C                   ENDIF                                                  RR1STF IFNE
      *
     C     RR1ATD        IFEQ      'Y'
     C     XXDEDF        ANDEQ     'Y'
     C     XXDIST        ADD       XXD2ST        EL1APR
      *
     C     RR1STF        IFNE      0
     c     RR1xfc        OrEq      'Y'
     C     XXFPCT        MULT(H)   EL1APR        XXWK10
     C                   Z-ADD     XXWK10        EL1FPR
     C                   SUB       EL1FPR        EL1APR
     C                   ENDIF                                                  RR1STF
      *
     C                   ENDIF                                                  RR1ATD
      *
     C                   ENDIF                                                  RR1STP IFNE
     C*
     C                   Z-ADD     0             EL2APR
     C     RR2NDP        IFNE      *ZERO
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDNE     'NJ'
     C     XXST          ANDNE     'NY'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR2NDL        ANDEQ     'X'
     C                   Z-ADD     XXNET         EL2APR
     C*** SECOND ASSESSMENT
     C                   IF        (RR2ATR <> 'B') AND (RR2ATR <> 'F')
     C                   SUB       XXPAYR        EL2APR
     C                   ENDIF
     C                   IF        (RR2ATR <> 'B') AND (RR2ATR <> 'D')
     C                   SUB       XXADTR        EL2APR
     C                   ENDIF
     C                   ELSE
     C                   SELECT
     C     RR2NDL        WHENEQ    'J'
     C                   Z-ADD     X2SUBJ        EL2APR
     C     RR2NDL        WHENEQ    'E'
     C                   Z-ADD     X2EXPP        EL2APR
     C     RR2NDL        WHENEQ    'S'
     C                   Z-ADD     X2STAN        EL2APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL2APR
     C                   ENDIF
     C     RR2NDL        WHENEQ    'D'
     C                   Z-ADD     X2DISP        EL2APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL2APR
     C                   ENDIF
     C     RR2NDL        WHENEQ    'X'
     C                   Z-ADD     X2NET         EL2APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL2APR
     C                   ENDIF
     C*** SECOND ASSESSMENT
     C                   IF        (RR2ATR <> 'B') AND (RR2ATR <> 'F')
     C                   SUB       XXPAYR        EL2APR
     C                   ENDIF
     C                   IF        (RR2ATR <> 'B') AND (RR2ATR <> 'D')
     C                   SUB       XXADTR        EL2APR
     C                   ENDIF
     C                   ENDSL                                                  RR2NDL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR2IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR2NDL        ANDNE     'J'
     C     RR2NDL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR2NDL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR2NDL        ANDNE     'J'
     C     RR2NDL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR2NDL        ANDNE     'J'
     C     RR2NDL        ANDNE     'E'
     C     RR2NDL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR2NDL        ANDNE     'J'
     C     RR2NDL        ANDNE     'E'
     C     RR2NDL        ANDNE     'S'
     C     RR2NDL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   ADD       XXDIST        EL2APR
     C                   ADD       XXD2ST        EL2APR
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C                   IF        RR2ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm2       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   EL2APR = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR2ILF:RR2NDL:X2SUBJ:X2EXPP:X2STAN:
     C                                      X2DISP:X2NET:ELAMNP:RR2ATR)
     C*
     C                   ENDIF                                                  RR2ILF <> ' '
     C                   IF        RR2NDL <> 'X'
     C                   IF        (RR2ATR = 'B') OR (RR2ATR = 'F')
     C                   ADD       XXPAYR        EL2APR
     C                   ENDIF
     C                   IF        (RR2ATR = 'B') OR (RR2ATR = 'D')
     C                   ADD       XXADTR        EL2APR
     C                   ENDIF
     C                   ENDIF                                                  RR2NDL <> X
     C                   ENDIF                                                  ELAMNP IFEQ
     C     RR2NDF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR2xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C     XXFPCT        MULT(H)   EL2APR        XXWK10
     C                   Z-ADD     XXWK10        EL2FPR
     C                   SUB       EL2FPR        EL2APR
     C                   ENDIF                                                  RR2NDF IFNE
      *
     C     RR2ATD        IFEQ      'Y'
     C     XXDEDF        ANDEQ     'Y'
     C     XXDIST        ADD       XXD2ST        EL2APR
      *
     C     RR2NDF        IFNE      0
     c     RR2xfc        OrEq      'Y'
     C     XXFPCT        MULT(H)   EL2APR        XXWK10
     C                   Z-ADD     XXWK10        EL2FPR
     C                   SUB       EL2FPR        EL2APR
     C                   ENDIF                                                  RR2NDF
      *
     C                   ENDIF                                                  RR2ATD
      *
     C                   ENDIF                                                  RR2NDP IFNE
     C*
     C                   Z-ADD     0             EL3APR
     C                   Z-ADD     0             EL3FPR
     C     RR3RDP        IFNE      *ZERO
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDNE     'NJ'
     C     XXST          ANDNE     'NY'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR3RDL        ANDEQ     'X'
     C                   Z-ADD     XXNET         EL3APR
     C*** THIRD ASSESSMENT
     C                   IF        (RR3ATR <> 'B') AND (RR3ATR <> 'F')
     C                   SUB       XXPAYR        EL3APR
     C                   ENDIF
     C                   IF        (RR3ATR <> 'B') AND (RR3ATR <> 'D')
     C                   SUB       XXADTR        EL3APR
     C                   ENDIF
     C                   ELSE
     C                   SELECT
     C     RR3RDL        WHENEQ    'J'
     C                   Z-ADD     X3SUBJ        EL3APR
     C     RR3RDL        WHENEQ    'E'
     C                   Z-ADD     X3EXPP        EL3APR
     C     RR3RDL        WHENEQ    'S'
     C                   Z-ADD     X3STAN        EL3APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL3APR
     C                   ENDIF
     C     RR3RDL        WHENEQ    'D'
     C                   Z-ADD     X3DISP        EL3APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL3APR
     C                   ENDIF
     C     RR3RDL        WHENEQ    'X'
     C                   Z-ADD     X3NET         EL3APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL3APR
     C                   ENDIF
     C*** THIRD ASSESSMENT
     C                   IF        (RR3ATR <> 'B') AND (RR3ATR <> 'F')
     C                   SUB       XXPAYR        EL3APR
     C                   ENDIF
     C                   IF        (RR3ATR <> 'B') AND (RR3ATR <> 'D')
     C                   SUB       XXADTR        EL3APR
     C                   ENDIF
     C                   ENDSL                                                  RR3RDL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR3IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR3RDL        ANDNE     'J'
     C     RR3RDL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR3RDL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR3RDL        ANDNE     'J'
     C     RR3RDL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR3RDL        ANDNE     'J'
     C     RR3RDL        ANDNE     'E'
     C     RR3RDL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR3RDL        ANDNE     'J'
     C     RR3RDL        ANDNE     'E'
     C     RR3RDL        ANDNE     'S'
     C     RR3RDL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   ADD       XXDIST        EL3APR
     C                   ADD       XXD2ST        EL3APR
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C                   IF        RR3ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm3       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   EL3APR = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR3ILF:RR3RDL:X3SUBJ:X3EXPP:X3STAN:
     C                                      X3DISP:X3NET:ELAMNP:RR3ATR)
     C*
     C                   ENDIF                                                  RR3ILF <> ' '
     C                   IF        RR3RDL <> 'X'
     C                   IF        (RR3ATR = 'B') OR (RR3ATR = 'F')
     C                   ADD       XXPAYR        EL3APR
     C                   ENDIF
     C                   IF        (RR3ATR = 'B') OR (RR3ATR = 'D')
     C                   ADD       XXADTR        EL3APR
     C                   ENDIF
     C                   ENDIF                                                  RR3RDL <> X
     C                   ENDIF                                                  ELAMNP IFEQ
     C     RR3RDF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR3xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C     XXFPCT        MULT(H)   EL3APR        XXWK10
     C                   Z-ADD     XXWK10        EL3FPR
     C                   SUB       EL3FPR        EL3APR
     C                   ENDIF                                                  RR3RDF IFNE
      *
     C     RR3ATD        IFEQ      'Y'
     C     XXDEDF        ANDEQ     'Y'
     C     XXDIST        ADD       XXD2ST        EL3APR
      *
     C     RR3RDF        IFNE      0
     c     RR3xfc        OrEq      'Y'
     C     XXFPCT        MULT(H)   EL3APR        XXWK10
     C                   Z-ADD     XXWK10        EL3FPR
     C                   SUB       EL3FPR        EL3APR
     C                   ENDIF                                                  RR3RDF
      *
     C                   ENDIF                                                  RR3ATD
      *
     C                   ENDIF                                                  RR3RDP IFNE
     C*
     C                   Z-ADD     0             EL4APR
     C                   Z-ADD     0             EL4FPR
     C     RR4THP        IFNE      *ZERO
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDNE     'NJ'
     C     XXST          ANDNE     'NY'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR4THL        ANDEQ     'X'
     C                   Z-ADD     XXNET         EL4APR
     C                   IF        (RR4ATR <> 'B') AND (RR4ATR <> 'F')
     C                   SUB       XXPAYR        EL4APR
     C                   ENDIF
     C                   IF        (RR4ATR <> 'B') AND (RR4ATR <> 'D')
     C                   SUB       XXADTR        EL4APR
     C                   ENDIF
     C                   ELSE
     C                   SELECT
     C     RR4THL        WHENEQ    'J'
     C                   Z-ADD     X4SUBJ        EL4APR
     C     RR4THL        WHENEQ    'E'
     C                   Z-ADD     X4EXPP        EL4APR
     C     RR4THL        WHENEQ    'S'
     C                   Z-ADD     X4STAN        EL4APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL4APR
     C                   ENDIF
     C     RR4THL        WHENEQ    'D'
     C                   Z-ADD     X4DISP        EL4APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL4APR
     C                   ENDIF
     C     RR4THL        WHENEQ    'X'
     C                   Z-ADD     X4NET         EL4APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL4APR
     C                   ENDIF
     C*** FOURTH ASSESSMENT
     C                   IF        (RR4ATR <> 'B') AND (RR4ATR <> 'F')
     C                   SUB       XXPAYR        EL4APR
     C                   ENDIF
     C                   IF        (RR4ATR <> 'B') AND (RR4ATR <> 'D')
     C                   SUB       XXADTR        EL4APR
     C                   ENDIF
     C                   ENDSL                                                  RR4THL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR4IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR4THL        ANDNE     'J'
     C     RR4THL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR4THL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR4THL        ANDNE     'J'
     C     RR4THL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR4THL        ANDNE     'J'
     C     RR4THL        ANDNE     'E'
     C     RR4THL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR4THL        ANDNE     'J'
     C     RR4THL        ANDNE     'E'
     C     RR4THL        ANDNE     'S'
     C     RR4THL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   ADD       XXDIST        EL4APR
     C                   ADD       XXD2ST        EL4APR
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C                   IF        RR4ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm4       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   EL4APR = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR4ILF:RR4THL:X4SUBJ:X4EXPP:X4STAN:
     C                                      X4DISP:X4NET:ELAMNP:RR4ATR)
     C*
     C                   ENDIF                                                  RR4ILF <> ' '
     C                   IF        RR4THL <> 'X'
     C                   IF        (RR4ATR = 'B') OR (RR4ATR = 'F')
     C                   ADD       XXPAYR        EL4APR
     C                   ENDIF
     C                   IF        (RR4ATR = 'B') OR (RR4ATR = 'D')
     C                   ADD       XXADTR        EL4APR
     C                   ENDIF
     C                   ENDIF                                                  RR4THL <> X
     C                   ENDIF                                                  ELAMNP IFEQ
     C     RR4THF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR4xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C     XXFPCT        MULT(H)   EL4APR        XXWK10
     C                   Z-ADD     XXWK10        EL4FPR
     C                   SUB       EL4FPR        EL4APR
     C                   ENDIF                                                  RR4THF IFNE
      *
     C     RR4ATD        IFEQ      'Y'
     C     XXDEDF        ANDEQ     'Y'
     C     XXDIST        ADD       XXD2ST        EL4APR
      *
     C     RR4THF        IFNE      0
     c     RR4xfc        OrEq      'Y'
     C     XXFPCT        MULT(H)   EL4APR        XXWK10
     C                   Z-ADD     XXWK10        EL4FPR
     C                   SUB       EL4FPR        EL4APR
     C                   ENDIF                                                  RR4THF
      *
     C                   ENDIF                                                  RR4ATD
      *
     C                   ENDIF                                                  RR4THP IFNE
     C*
     C                   Z-ADD     0             EL5APR
     C                   Z-ADD     0             EL5FPR
     C     RR5THP        IFNE      *ZERO
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDNE     'NJ'
     C     XXST          ANDNE     'NY'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR5THL        ANDEQ     'X'
     C                   Z-ADD     XXNET         EL5APR
     C*** FIFTH ASSESSMENT
     C                   IF        (RR5ATR <> 'B') AND (RR5ATR <> 'F')
     C                   SUB       XXPAYR        EL5APR
     C                   ENDIF
     C                   IF        (RR5ATR <> 'B') AND (RR5ATR <> 'D')
     C                   SUB       XXADTR        EL5APR
     C                   ENDIF
     C                   ELSE
     C                   SELECT
     C     RR5THL        WHENEQ    'J'
     C                   Z-ADD     X5SUBJ        EL5APR
     C     RR5THL        WHENEQ    'E'
     C                   Z-ADD     X5EXPP        EL5APR
     C     RR5THL        WHENEQ    'S'
     C                   Z-ADD     X5STAN        EL5APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL5APR
     C                   ENDIF
     C     RR5THL        WHENEQ    'D'
     C                   Z-ADD     X5DISP        EL5APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL5APR
     C                   ENDIF
     C     RR5THL        WHENEQ    'X'
     C                   Z-ADD     X5NET         EL5APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL5APR
     C                   ENDIF
     C*** FIFTH ASSESSMENT
     C                   IF        (RR5ATR <> 'B') AND (RR5ATR <> 'F')
     C                   SUB       XXPAYR        EL5APR
     C                   ENDIF
     C                   IF        (RR5ATR <> 'B') AND (RR5ATR <> 'D')
     C                   SUB       XXADTR        EL5APR
     C                   ENDIF
     C                   ENDSL                                                  RR5THL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR5IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR5THL        ANDNE     'J'
     C     RR5THL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR5THL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR5THL        ANDNE     'J'
     C     RR5THL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR5THL        ANDNE     'J'
     C     RR5THL        ANDNE     'E'
     C     RR5THL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR5THL        ANDNE     'J'
     C     RR5THL        ANDNE     'E'
     C     RR5THL        ANDNE     'S'
     C     RR5THL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   ADD       XXDIST        EL5APR
     C                   ADD       XXD2ST        EL5APR
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C                   IF        RR5ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm5       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   EL5APR = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR5ILF:RR5THL:X5SUBJ:X5EXPP:X5STAN:
     C                                      X5DISP:X5NET:ELAMNP:RR5ATR)
     C*
     C                   ENDIF                                                  RR5ILF <> ' '
     C                   IF        RR5THL <> 'X'
     C                   IF        (RR5ATR = 'B') OR (RR5ATR = 'F')
     C                   ADD       XXPAYR        EL5APR
     C                   ENDIF
     C                   IF        (RR5ATR = 'B') OR (RR5ATR = 'D')
     C                   ADD       XXADTR        EL5APR
     C                   ENDIF
     C                   ENDIF                                                  RR5THL <> X
     C                   ENDIF                                                  ELAMNP IFEQ
     C     RR5THF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR5xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C     XXFPCT        MULT(H)   EL5APR        XXWK10
     C                   Z-ADD     XXWK10        EL5FPR
     C                   SUB       EL5FPR        EL5APR
     C                   ENDIF                                                  RR5THF IFNE
      *
     C     RR5ATD        IFEQ      'Y'
     C     XXDEDF        ANDEQ     'Y'
     C     XXDIST        ADD       XXD2ST        EL5APR
      *
     C     RR5THF        IFNE      0
     c     RR5xfc        OrEq      'Y'
     C     XXFPCT        MULT(H)   EL5APR        XXWK10
     C                   Z-ADD     XXWK10        EL5FPR
     C                   SUB       EL5FPR        EL5APR
     C                   ENDIF                                                  RR5THF
      *
     C                   ENDIF                                                  RR5ATD
      *
     C                   ENDIF                                                  RR5THP IFNE
     C*
     C                   Z-ADD     0             EL6APR
     C                   Z-ADD     0             EL6FPR
     C     RR6THP        IFNE      *ZERO
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDNE     'NJ'
     C     XXST          ANDNE     'NY'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR6THL        ANDEQ     'X'
     C                   Z-ADD     XXNET         EL6APR
     C*** SIXTH ASSESSMENT
     C                   IF        (RR6ATR <> 'B') AND (RR6ATR <> 'F')
     C                   SUB       XXPAYR        EL6APR
     C                   ENDIF
     C                   IF        (RR6ATR <> 'B') AND (RR6ATR <> 'D')
     C                   SUB       XXADTR        EL6APR
     C                   ENDIF
     C                   ELSE
     C                   SELECT
     C     RR6THL        WHENEQ    'J'
     C                   Z-ADD     X6SUBJ        EL6APR
     C     RR6THL        WHENEQ    'E'
     C                   Z-ADD     X6EXPP        EL6APR
     C     RR6THL        WHENEQ    'S'
     C                   Z-ADD     X6STAN        EL6APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL6APR
     C                   ENDIF
     C     RR6THL        WHENEQ    'D'
     C                   Z-ADD     X6DISP        EL6APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL6APR
     C                   ENDIF
     C     RR6THL        WHENEQ    'X'
     C                   Z-ADD     X6NET         EL6APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL6APR
     C                   ENDIF
     C*** SIXTH ASSESSMENT
     C                   IF        (RR6ATR <> 'B') AND (RR6ATR <> 'F')
     C                   SUB       XXPAYR        EL6APR
     C                   ENDIF
     C                   IF        (RR6ATR <> 'B') AND (RR6ATR <> 'D')
     C                   SUB       XXADTR        EL6APR
     C                   ENDIF
     C                   ENDSL                                                  RR6THL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR6IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR6THL        ANDNE     'J'
     C     RR6THL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR6THL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR6THL        ANDNE     'J'
     C     RR6THL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR6THL        ANDNE     'J'
     C     RR6THL        ANDNE     'E'
     C     RR6THL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR6THL        ANDNE     'J'
     C     RR6THL        ANDNE     'E'
     C     RR6THL        ANDNE     'S'
     C     RR6THL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   ADD       XXDIST        EL6APR
     C                   ADD       XXD2ST        EL6APR
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C                   IF        RR6ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm6       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   EL6APR = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR6ILF:RR6THL:X6SUBJ:X6EXPP:X6STAN:
     C                                      X6DISP:X6NET:ELAMNP:RR6ATR)
     C*
     C                   ENDIF                                                  RR6ILF <> ' '
     C                   IF        RR6THL <> 'X'
     C                   IF        (RR6ATR = 'B') OR (RR6ATR = 'F')
     C                   ADD       XXPAYR        EL6APR
     C                   ENDIF
     C                   IF        (RR6ATR = 'B') OR (RR6ATR = 'D')
     C                   ADD       XXADTR        EL6APR
     C                   ENDIF
     C                   ENDIF                                                  RR6THL <> X
     C                   ENDIF                                                  ELAMNP IFEQ
     C     RR6THF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR6xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C     XXFPCT        MULT(H)   EL6APR        XXWK10
     C                   Z-ADD     XXWK10        EL6FPR
     C                   SUB       EL6FPR        EL6APR
     C                   ENDIF                                                  RR6THF IFNE
      *
     C     RR6ATD        IFEQ      'Y'
     C     XXDEDF        ANDEQ     'Y'
     C     XXDIST        ADD       XXD2ST        EL6APR
      *
     C     RR6THF        IFNE      0
     c     RR6xfc        OrEq      'Y'
     C     XXFPCT        MULT(H)   EL6APR        XXWK10
     C                   Z-ADD     XXWK10        EL6FPR
     C                   SUB       EL6FPR        EL6APR
     C                   ENDIF                                                  RR6THF
      *
     C                   ENDIF                                                  RR6ATD
      *
     C                   ENDIF                                                  RR6THP IFNE
     C*
     C                   Z-ADD     0             EL7APR
     C                   Z-ADD     0             EL7FPR
     C     RR7THP        IFNE      *ZERO
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDNE     'NJ'
     C     XXST          ANDNE     'NY'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR7THL        ANDEQ     'X'
     C                   Z-ADD     XXNET         EL7APR
     C*** SEVENTH ASSESSMENT
     C                   IF        (RR7ATR <> 'B') AND (RR7ATR <> 'F')
     C                   SUB       XXPAYR        EL7APR
     C                   ENDIF
     C                   IF        (RR7ATR <> 'B') AND (RR7ATR <> 'D')
     C                   SUB       XXADTR        EL7APR
     C                   ENDIF
     C                   ELSE
     C                   SELECT
     C     RR7THL        WHENEQ    'J'
     C                   Z-ADD     X7SUBJ        EL7APR
     C     RR7THL        WHENEQ    'E'
     C                   Z-ADD     X7EXPP        EL7APR
     C     RR7THL        WHENEQ    'S'
     C                   Z-ADD     X7STAN        EL7APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL7APR
     C                   ENDIF
     C     RR7THL        WHENEQ    'D'
     C                   Z-ADD     X7DISP        EL7APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL7APR
     C                   ENDIF
     C     RR7THL        WHENEQ    'X'
     C                   Z-ADD     X7NET         EL7APR
     C     XXST          IFEQ      'NY'
     C     ELDIV         ANDEQ     XSDIV
     C                   ADD       XXNYCH        EL7APR
     C                   ENDIF
     C*** SEVENTH ASSESSMENT
     C                   IF        (RR7ATR <> 'B') AND (RR7ATR <> 'F')
     C                   SUB       XXPAYR        EL7APR
     C                   ENDIF
     C                   IF        (RR7ATR <> 'B') AND (RR7ATR <> 'D')
     C                   SUB       XXADTR        EL7APR
     C                   ENDIF
     C                   ENDSL                                                  RR7THL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR7IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR7THL        ANDNE     'J'
     C     RR7THL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR7THL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR7THL        ANDNE     'J'
     C     RR7THL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR7THL        ANDNE     'J'
     C     RR7THL        ANDNE     'E'
     C     RR7THL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR7THL        ANDNE     'J'
     C     RR7THL        ANDNE     'E'
     C     RR7THL        ANDNE     'S'
     C     RR7THL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   ADD       XXDIST        EL7APR
     C                   ADD       XXD2ST        EL7APR
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C                   IF        RR7ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm7       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   EL7APR = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR7ILF:RR7THL:X7SUBJ:X7EXPP:X7STAN:
     C                                      X7DISP:X7NET:ELAMNP:RR7ATR)
     C*
     C                   ENDIF                                                  RR7ILF <> ' '
     C                   IF        RR7THL <> 'X'
     C                   IF        (RR7ATR = 'B') OR (RR7ATR = 'F')
     C                   ADD       XXPAYR        EL7APR
     C                   ENDIF
     C                   IF        (RR7ATR = 'B') OR (RR7ATR = 'D')
     C                   ADD       XXADTR        EL7APR
     C                   ENDIF
     C                   ENDIF                                                  RR7THL <> X
     C                   ENDIF                                                  ELAMNP IFEQ
     C     RR7THF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR7xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C     XXFPCT        MULT(H)   EL7APR        XXWK10
     C                   Z-ADD     XXWK10        EL7FPR
      **** Beg Chg ***** 06/11/24 **************************
     C*****              SUB       EL5FPR        EL5APR
     C                   SUB       EL7FPR        EL7APR
      **** End Chg ***** 06/11/24 **************************
     C                   ENDIF                                                  RR7THF IFNE
      *
     C     RR7ATD        IFEQ      'Y'
     C     XXDEDF        ANDEQ     'Y'
     C     XXDIST        ADD       XXD2ST        EL7APR
      *
     C     RR7THF        IFNE      0
     c     RR7xfc        OrEq      'Y'
      **** Beg Chg ***** 06/11/24 **************************
     C*****XXFPCT        MULT(H)   EL5APR        XXWK10
     C     XXFPCT        MULT(H)   EL7APR        XXWK10
      **** End Chg ***** 06/11/24 **************************
     C                   Z-ADD     XXWK10        EL7FPR
     C                   SUB       EL7FPR        EL7APR
     C                   ENDIF                                                  RR7THF
      *
     C                   ENDIF                                                  RR7ATD
      *
     C                   ENDIF                                                  RR7THP IFNE
      *
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C                   Z-ADD     XXHCNF        ELHCN
     C                   Z-ADD     XXHCNA        ELAHCN
     C                   Z-ADD(H)  XXUPSA        XXWK10
     C                   Z-ADD     XXWK10        ELUPSA
     C                   Z-ADD     XXDIS1        ELADC1
     C                   Z-ADD     XXDIS2        ELADC2
     C                   Z-ADD     XXD1D2        ELD1A2
     C                   Z-ADD     XXTXDU        ELTXDU
     C                   Z-ADD     XXANCC        ELANCA
     C                   Z-ADD     XXANCM        ELANCM
     C*
     C* Populate the Deviation #2 Before and After Premium Fields
     C*
     C                   Eval      ELdev2BP = xxdev2BP
     C                   Eval      ELdev2AP = xxdev2AP
     C*
     C* Populate the Territorial Differential Factor
     C*
     C                   Eval      ELtdf = outTDF
     C                   Eval      ELapf = xxAPF
     C*** end add ***** 02/01/18 ********************************************
     C                   Z-ADD     XXTXPR        ELTXPR
     C                   Z-ADD     XXDIS3        ELADC3
     C                   Z-ADD     XXLOSA        ELLOSS
     C                   Z-ADD     XXDIST        ELADCT
     C                   Z-ADD     XXD2ST        ELADD2
     C                   Z-ADD     XXDISM        ELADCM
     C                   Z-ADD     XXDISS        ELADCS
     C                   Z-ADD     XXMODP        ELADCR
     C     XXDISD        MULT      -1            ELADC
     C                   Z-ADD     XXDEV2        ELADV2
     C                   Z-ADD     XXD2D2        ELD2A2
     C                   Z-ADD     XXEXP1        ELAXP1
     C                   Z-ADD     XXEXP2        ELAXP2
     C                   Z-ADD     XXEXP3        ELAXP3
     C*
     C                   XFOOT(H)  LM            XXWK10           11 0
     C                   Z-ADD     XXWK10        ELAIN1
     C                   XFOOT(H)  LM2           XXWK10
     C                   Z-ADD     XXWK10        ELAIN2
      //*** Beg Add ***** 08/14/24 *******************************************
       Eval(h) xxWk10 = %Xfoot(LMB);
       ELAIN1 = ELAIN1 + xxWk10;
       Eval(h) xxWk10 = %Xfoot(LMB2);
       ELAIN2 = ELAIN2 + xxWk10;
      //*** End Add ***** 08/14/24 *******************************************
     C                   Z-ADD(H)  XXNORM        XXWK10
     C                   Z-ADD     XXWK10        ELANMP
     C                   Z-ADD     XSMAN         ELAMPR
     C                   XFOOT(H)  WAV           XXWK10
     C                   Z-ADD     XXWK10        ELAWAV
     C*
     C* TOTAL THE SHORT RATE PENALTY FOR EMPLR DETAIL FIELD.
     C*
     C                   XFOOT     SRP           ELSRAM
     C*
     C                   Z-ADD     XXMODP        ELAMPM
     C     XXMODP        SUB       XXDISD        ELAFRS
     C                   Z-ADD(H)  XXSUBJ        XXWK10
     C                   Z-ADD     XXWK10        ELASBJ
     C                   Z-ADD(H)  XXNET         XXWK10
     C                   Z-ADD     XXWK10        ELAAPR
     C                   Z-ADD     XXPAYR        ELATER
     C                   Z-ADD     XXADTR        ELADTR
     C                   XFOOT     PAY           XXAMT1
     C                   XFOOT     PAY2          XXTOTL
     C                   ADD       XXAMT1        XXTOTL
     C                   DIV(H)    100           XXTOTL
     C                   IF        XXTOTL > 0
     C     ELATER        DIV(H)    XXTOTL        ELTERR
     C                   ENDIF
     C                   XFOOT     PAY           XXTOTL
     C                   DIV(H)    100           XXTOTL
     C                   IF        XXTOTL > 0
     C     ELADTR        DIV(H)    XXTOTL        ELDTRR
     C                   ENDIF
     C*
     C* If LCM is excluded from terrorism rates then load directly
     C* from Stat Rpt/Rating when only one rate is being applied.
     C*
     C                   If        RRTerL = 'Y'
     C* Foreign Terrorism
     C                   If        (pyTrt(1) <> 0 and pyTrt(2) = 0
     C                             and (npTrt(1) = 0 or npTrt(1) = pyTrt(1)
     C                              or %xfoot(nTr) = 0))
     C                             or (pyTrt(1) <> 0 and pyTrt(2) = pyTrt(1)
     C                             and (npTrt(1) = 0 or npTrt(1) = pyTrt(1)
     C                              or %xfoot(nTr) = 0))
     C                   Z-add     pyTrt(1)      ELTERR
     C                   Endif
     C* Domestic Terrorism
     C                   If        (dmTrt(1) <> 0 and dmTrt(2) = 0) or
     C                             (dmTrt(1) <> 0 and dmTrt(2) = dmTrt(1))
     C                   Z-add     dmTrt(1)      ELDTRR
     C                   Endif
     C*
     C                   Endif
     C                   Z-ADD     XXVLCA        ELVLCA
     C                   Z-ADD(H)  XXSTAN        XXWK10
     C                   Z-ADD     XXWK10        ELASPR
     C                   Z-ADD     SVHIGH        ELAMNA
     C                   Z-ADD     XXEXCN        ELPRTD                         TLB 032988
     C                   Z-ADD     XXEXCO        ELPRTF
     C                   Z-ADD     XXARP1        ELRPA1
     C                   Z-ADD     XXARP2        ELRPA2
     C                   Z-ADD     XXARP3        ELRPA3
     C*
     C                   MOVE      *BLANKS       Q2CALL
     C*
     C     EMSTRP        IFNE      'Y'
     C*
     C     QQAR          IFNE      *BLANKS
     C     *IN97         OREQ      *OFF
     C*
     C                   MOVE      XXCO#         Q2CO#
     C                   MOVE      XXFND         Q2FND
     C                   MOVE      XXEMP#        Q2EMP#
     C                   MOVE      XXFYR         Q2FYR
     C                   MOVE      XXDIV         Q2DIV
     C*
     C*  ACCUMULATE PREMIUM ACROSS ALL DIVISIONS
     C*
     C                   CALL      'SRMDIV'
     C                   PARM                    Q2CO#             3
     C                   PARM                    Q2FND             3
     C                   PARM                    Q2FYR             3
     C                   PARM                    Q2EMP#            9
     C                   PARM                    Q2DIV             5
     C                   PARM                    Q2PREM           11 2
     C                   PARM      'A'           Q2TYPE            1
     C                   PARM      '1'           Q2CALL            1
     C                   PARM      'B'           Q2RNB             1
     C*
     C     Q2CALL        IFEQ      'Y'
     C*
     C                   Z-ADD     0             XXMAN
     C                   Z-ADD     0             XXSTAN
     C                   Z-ADD     0             XXNORM
     C                   Z-ADD     0             XXNET
     C                   Z-ADD     0             ELTXDU
     C                   Z-ADD     0             ELANCA
     C                   Z-ADD     0             ELANCM
     C     XXST          IFNE      'NJ'
     C                   Z-SUB     XX1BAL        XXDIF1
     C                   Z-SUB     XX2BAL        XXDIF2
     C                   Z-SUB     XX3BAL        XXDIF3
     C                   Z-SUB     XX4BAL        XXDIF4
     C                   Z-SUB     XX5BAL        XXDIF5
     C                   Z-SUB     XX6BAL        XXDIF6
     C                   Z-SUB     XX7BAL        XXDIF7
     C                   ENDIF                                                  XXST IFNE
     C                   Z-SUB     XXWK92        XXDIFF
     C                   Z-SUB     XXTXBL        XXDIFX
     C                   Z-ADD     0             XXTXDU
     C                   Z-ADD     0             XXANCC
     C                   Z-ADD     0             XXANCM
     C     XXST          IFNE      'NJ'
     C     XXST          ANDNE     'NY'
     C                   Z-ADD     0             EL1APR
     C                   Z-ADD     0             EL2APR
     C                   ENDIF                                                  XXST NE 'NJ'
     C                   Z-ADD     0             XXSUBJ
     C                   Z-ADD     0             XXEXPP
     C                   Z-ADD     0             XXDISP
     C     XXST          IFNE      'NJ'
     C     XXST          ANDNE     'NY'
     C                   Z-ADD     0             EL1FPR
     C                   Z-ADD     0             EL2FPR
     C                   Z-ADD     0             EL3APR
     C                   Z-ADD     0             EL3FPR
     C                   Z-ADD     0             EL4APR
     C                   Z-ADD     0             EL4FPR
     C                   Z-ADD     0             EL5APR
     C                   Z-ADD     0             EL5FPR
     C                   Z-ADD     0             EL6APR
     C                   Z-ADD     0             EL6FPR
     C                   Z-ADD     0             EL7APR
     C                   Z-ADD     0             EL7FPR
     C                   If        ELDiv = xldiv1
     c                   Eval      EL1apr += xlmin1
     c                   Eval      EL2apr += xlmin1
     c                   Eval      EL3apr += xlmin1
     c                   Eval      EL4apr += xlmin1
     c                   Eval      EL5apr += xlmin1
     c                   Eval      EL6apr += xlmin1
     c                   Eval      EL7apr += xlmin1
     C                   Endif
     C                   If        ELDiv = xldiv2
     c                   Eval      EL1apr += xlmin2
     c                   Eval      EL2apr += xlmin2
     c                   Eval      EL3apr += xlmin2
     c                   Eval      EL4apr += xlmin2
     c                   Eval      EL5apr += xlmin2
     c                   Eval      EL6apr += xlmin2
     c                   Eval      EL7apr += xlmin2
     C                   Endif
     C                   ENDIF                                                  XXST NE 'NJ'
     C                   Z-ADD     0             ELTXPR
     C                   Z-ADD     0             ELAMPR
     C                   Z-ADD     0             ELSRAM
     C                   Z-ADD     0             ELAAPR
     C                   Z-ADD     0             ELASPR
     C                   Z-ADD     0             ELANMP
     C                   Z-ADD     0             ELASBJ
     C                   Z-ADD     0             ELAMPM
     C                   Z-ADD     0             ELAIN1
     C                   Z-ADD     0             ELAIN2
     C                   Z-ADD     0             ELAFRS
     C                   Z-ADD     0             ELADCR
     C                   Z-ADD     0             ELADC
     C                   Z-ADD     0             ELADC1
     C                   Z-ADD     0             ELADC2
     C                   Z-ADD     0             ELADV2
     C                   Z-ADD     0             ELADC3
     C                   Z-ADD     0             ELAHCN
     C                   Z-ADD     0             ELUPSA
     C                   Z-ADD     0             ELLOSS
     C                   Z-Add     0             ELdev2BP
     C                   Z-Add     0             ELdev2AP
     C                   Z-Add     0             ELtdf
     C     EXBEXP        IFNE      'Y'
     C                   Z-ADD     0             ELPRTD
     C                   Z-ADD     0             ELPRTF
     C                   ELSE
     C     EXBEST        IFEQ      'Y'
     C     ELDIV         ANDEQ     XEDIV
     C                   Z-ADD     SVPRTD        ELPRTD
      *** Begin Chg *** 11/02/23 *******************************************
     C*****              Z-ADD     SVPRTD        ELPRTF
     C                   Z-ADD     SVPRTF        ELPRTF
      *** End   Chg *** 11/02/23 *******************************************
     C                   Z-ADD     SVPRTD        ELAAPR
     C                   ENDIF                                                  EXBEST IFEQ
     C                   ENDIF                                                  EXBEXP IFNE
      * Policy fee shows on top of minimum
     c                   Eval      xxNet  += xxAPF
     c                   Eval      ELaapr += xxAPF
     C                   If        ELDiv = xldiv1
     C                   Z-ADD     xlmin1        ELAIN1
     c                   Eval      xxDiff += xlmin1
     c                   Eval      xxNet  += xlmin1
     c                   Eval      xxnorm += xlmin1
     c                   Eval      xxstan += xlmin1
     c                   Eval      ELaapr += xlmin1
     c                   Eval      ELanmp += xlmin1
     c                   Eval      ELaspr += xlmin1
     c                   Eval      ELampm += xlmin1
     c                   Eval      ELasbj += xlmin1
     C                   Endif
     C                   If        ELDiv = xldiv2
     C                   Z-ADD     xlmin2        ELAIN2
     c                   Eval      xxDiff += xlmin2
     c                   Eval      xxNet  += xlmin2
     c                   Eval      xxnorm += xlmin2
     c                   Eval      xxstan += xlmin2
     c                   Eval      ELaapr += xlmin2
     c                   Eval      ELanmp += xlmin2
     c                   Eval      ELaspr += xlmin2
     c                   Eval      ELampm += xlmin2
     c                   Eval      ELasbj += xlmin2
     C                   Endif
     C                   Z-ADD     0             ELAWAV
     C                   Z-ADD     *ZERO         ELADCT
     C                   Z-ADD     *ZERO         ELADD2
     C                   Z-ADD     *ZERO         ELADCM
     C                   Z-ADD     *ZERO         ELADCS
     C                   Z-ADD     *ZERO         ELAXP1
     C                   Z-ADD     *ZERO         ELAXP2
     C                   Z-ADD     *ZERO         ELAXP3
     C                   ADD       XXPAYR        ELAAPR
     C                   ADD       XXADTR        ELAAPR
     C                   ADD       XXPAYR        XXDIFF
     C                   ADD       XXADTR        XXDIFF
     C                   Z-ADD     0             ELRPA1
     C                   Z-ADD     0             ELRPA2
     C                   Z-ADD     0             ELRPA3
     C*
     C                   ENDIF                                                  QQCALL IFEQ
     C*
     C                   ENDIF                                                  QQAR   IFNE
     C*
     C                   ENDIF                                                  EMSTRP IFNE
     C*                                                    MLS 3/7/88
     C*  IF MINIMUM PREMIUM WE MUST GO BACK AND OVERRIDE        "
     C*                                                         "
     C     ELAMNP        IFEQ      'Y'                                               "
     C                   If        ELDiv <> xldiv1
     C                   Sub       xlmin1        xxnorm
     C                   Sub       xlmin1        xxnet
     c                   Sub       xlmin1        xxDiff
     C                   Endif
     C                   If        ELDiv <> xldiv2
     C                   Sub       xlmin2        xxnorm
     C                   Sub       xlmin2        xxnet
     c                   Sub       xlmin2        xxDiff
     C                   Endif
     C                   Z-ADD     XXNORM        ELANMP                         RLS 2  JJJ
     C                   Z-ADD     XXNORM        ELAMPR
     C                   Z-ADD     XXNET         ELAAPR
     C                   Z-ADD     XXNORM        ELASPR
     C                   Z-ADD     XXNORM        XXSTAN
     C                   Z-ADD     XXNORM        ELASBJ
     C                   Z-ADD     XXNORM        ELAMPM
     C     *IN97         IFEQ      *OFF
     C     DSKY1         CHAIN     PDS02                              60
     C     *IN60         IFEQ      '0'
     C                   SUB       DSACSP        DSTACP
     C                   ADD       XXNORM        DSTACP
     C                   Z-ADD     XXNORM        DSACSP
     C                   Z-ADD     XXNORM        DSANSP
     C                   Z-ADD     0             DSADIS
     C                   Z-ADD     0             DSAPCT
     C                   UPDATE    PDS02
     C                   END
     C                   ENDIF
     C                   If        ELDiv = xldiv1
     C                   Z-ADD     xlmin1        ELAIN1
     C                   Endif
     C                   If        ELDiv = xldiv2
     C                   Z-ADD     xlmin2        ELAIN2
     C                   Endif
     C                   Z-ADD     0             ELADCR
     C                   Z-ADD     0             ELAFRS
     C                   Z-ADD     0             ELSRAM
     C                   Z-ADD     0             ELADC
     C                   Z-ADD     0             ELADC1
     C                   Z-ADD     0             ELADC2
     C                   Z-ADD     0             ELADV2
     C                   Z-ADD     0             ELADC3
     C                   Z-ADD     0             ELAHCN
     C                   Z-ADD     0             ELUPSA
     C                   Z-ADD     0             ELLOSS
     C                   Z-ADD     0             ELPRTD
     C                   Z-ADD     0             ELPRTF
     C                   Z-ADD     *ZERO         ELADCT
     C                   Z-ADD     *ZERO         ELADD2
     C                   Z-ADD     *ZERO         ELADCM
     C                   Z-ADD     *ZERO         ELADCS
     C                   Z-ADD     0             ELRPA1
     C                   Z-ADD     0             ELRPA2
     C                   Z-ADD     0             ELRPA3
     C                   Z-ADD     *ZERO         ELAXP1
     C                   Z-ADD     *ZERO         ELAXP2
     C                   Z-ADD     *ZERO         ELAXP3
     C                   Z-ADD     XXWVMN        ELAWAV
     C     SVPRTD        IFNE      *ZERO
     C                   Z-ADD     SVPRTD        ELPRTD
      *** Begin Chg *** 11/02/23 *******************************************
     C*****              Z-ADD     SVPRTD        ELPRTF
     C                   Z-ADD     SVPRTF        ELPRTF
      *** End   Chg *** 11/02/23 *******************************************
     C                   SUB       SVPRTD        ELAMPR                         MANUAL
     C                   SUB       SVPRTD        ELASBJ                         SUBJECT
     C                   SUB       SVPRTD        ELAMPM                         MODIFIED
     C                   SUB       SVPRTD        ELASPR                         STANDARD
     C                   SUB       SVPRTD        ELANMP                         NORMAL
     C     EXBEST        IFEQ      'Y'
     C     ELDIV         ANDNE     XEDIV
     C                   Z-ADD     0             ELPRTD
     C                   Z-ADD     0             ELPRTF
     C                   SUB       SVPRTD        ELAAPR                         ANNUAL
     C                   ENDIF
     C                   ENDIF                                                  *IN60 IFEQ
     C                   END
     C*
     C* CALCULATE STOCK DISCOUNT PERCENTAGE FOR IREP.
     C*
     C                   Z-ADD     0             ELDISP
     C     ELASP3        IFNE      *ZERO
     C                   Z-ADD     ELASP3        ELDISP
     C                   ELSE
     C     XNDISP        IFNE      *ZERO
     C                   Z-ADD     XNDISP        ELDISP
     C                   ELSE
     C     XXDSPR        IFNE      *ZERO
     C     ELADC3        DIV(H)    XXDSPR        XZDISP            7 4
     C     XZDISP        MULT      100           ELDISP
     C                   If        ELDISP <> 0 and DSAPCT <> 0
     C                   Z-add     DSAPCT        ELDISP
     C                   Endif
     C                   ENDIF                                                  XXDSPR IFNE
     C                   ENDIF
     C                   ENDIF
     C*
     C*   ACCUMULATE SAFETY FEE BILLINGS ALREADY ON FILE FOR FYR
     C*
     C                   Z-ADD     0             XXSFBT           15 4
     C                   MOVE      'F'           XXARTY
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      'S'                                          SAFETY FEES
     C                   ADD       EAAMT         XXSFBT
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   END
     C*
     C                   Z-ADD     0             XXSFEE
     C*
     C     SVCNCL        IFEQ      'Y'
     C     RRCXAD        ANDEQ     'Y'
     C     SVCNCL        OREQ      ' '
     C     RRYRAD        ANDEQ     'Y'
     C     XXSFCD        IFNE      *BLANK
     C     ELSFPC        ANDNE     *ZEROS
     C     XXSFCD        ORNE      *BLANK
     C     ELSFMN        ANDNE     *ZEROS
     C*
     C*   CALCULATE SAFETY FEE BILLED.
     C*
     C                   Z-ADD     0             XXPREM           15 4
     C                   SELECT
     C     RRSFAP        WHENEQ    'J'
     C                   Z-ADD     XXSUBJ        XXPREM
     C     RRSFAP        WHENEQ    'E'
     C                   Z-ADD     XXEXPP        XXPREM
     C     RRSFAP        WHENEQ    'S'
     C                   Z-ADD     XXSTAN        XXPREM
     C     RRSFAP        WHENEQ    'D'
     C                   Z-ADD     XXDISP        XXPREM
     C     RRSFAP        WHENEQ    'X'
     C                   Z-ADD     XXNORM        XXPREM
     C                   ADD       XXPAYR        XXPREM
     C                   ADD       XXADTR        XXPREM
     C     ELAMNP        IFNE      'Y'
     C                   ADD       XXEXCO        XXPREM
     C                   ENDIF
     C     RRSFAP        WHENEQ    'Y'
     C                   Z-ADD     XXNORM        XXPREM
     C                   ADD       XXPAYR        XXPREM
     C                   ADD       XXADTR        XXPREM
     C     ELAMNP        IFEQ      'Y'
     C                   SUB       S1PRTD        XXPREM
     C                   ENDIF
     C                   ENDSL                                                  RRSFAP SELC
     C*
     C     ELSFPC        MULT      XXPREM        XXSFAM           15 4
     C     XXSFAM        DIV(H)    100           XXSFEE           15 4
     C*
     C*
     C     XXSFEE        IFLT      ELSFMN
     C                   Z-ADD     ELSFMN        XXSFEE
     C                   ENDIF                                                  XXSFEE IFLT
     C                   ENDIF                                                  XXSFCD IFNE
     C*
     C                   Z-ADD(H)  XXSFEE        XXWK10
     C                   Z-ADD     XXWK10        XXSFEE
     C     XXSFEE        SUB       XXSFBT        XXDIFS           15 4
     C*
     C                   ELSE
     C                   Z-ADD     0             XXDIFS
     C                   ENDIF                                                  SVCNCL IFEQ
     C     ELPFDT        IFEQ      ELPTDT                                       IF FLAT CX
     C                   Z-ADD     0             XXSFEE
     C                   Z-SUB     XXSFBT        XXDIFS
     C                   ENDIF
     C*
     C* BILL ASSESSMENTS TO DIVISION WITH HIGHEST MINIMUM PREMIUM IF
     C* FLAG IS SET ON, MINIMUM HAS BEEN INVOKED, AND NOT NJ.  NJ USES
     C* PRE-MINIMUM PREMIUM FIGURES ON MINIMUM PREMIUM ACCOUNTS.
     C*
     C     EXBASM        IFEQ      'Y'
     C     XXST          ANDNE     'NJ'
     C     XXST          ANDNE     'NY'
     C*
     C* MINIMUM PREMIUM HAS BEEN INVOKED.
     C*
     C     ELAMNP        IFEQ      'Y'
     C     Q2CALL        OREQ      'Y'
     C*
     C                   Z-ADD     0             EL1APR
     C                   Z-ADD     0             EL2APR
     C                   Z-ADD     0             EL3APR
     C                   Z-ADD     0             EL1FPR
     C                   Z-ADD     0             EL2FPR
     C                   Z-ADD     0             EL3FPR
     C                   Z-ADD     0             EL4FPR
     C                   Z-ADD     0             EL4APR
     C                   Z-ADD     0             EL5FPR
     C                   Z-ADD     0             EL5APR
     C                   Z-ADD     0             EL6FPR
     C                   Z-ADD     0             EL6APR
     C                   Z-ADD     0             EL7FPR
     C                   Z-ADD     0             EL7APR
     C*
     C* DIVISION WITH HIGHEST MINIMUM.
     C*
     C     XXDIV         IFEQ      XADIV
     C*
     C     RR1STP        IFNE      *ZERO
      *
     C     RR1ATD        IFNE      'Y'
     C     XXDEDF        ORNE      'Y'
     C                   Z-ADD     XSHGH(1)      EL1APR
     C                   ENDIF                                                  RR1ATD
     C
      *
     C                   SELECT
     C                   WHEN      RR1ILF = 'I'
     C                   EVAL      EL1APR = XSILAM
     C                   WHEN      RR1ILF = 'W'
     C                   EVAL      EL1APR = XSHGH(1) - XSILAM
     C                   ENDSL
     C                   ENDIF
     C*
     C     RR2NDP        IFNE      *ZERO
      *
     C     RR2ATD        IFNE      'Y'
     C     XXDEDF        ORNE      'Y'
     C                   Z-ADD     XSHGH(2)      EL2APR
     C                   ENDIF                                                  RR2ATD
      *
     C                   SELECT
     C                   WHEN      RR2ILF = 'I'
     C                   EVAL      EL2APR = XSILAM
     C                   WHEN      RR2ILF = 'W'
     C                   EVAL      EL2APR = XSHGH(2) - XSILAM
     C                   ENDSL
     C                   ENDIF
     C*
     C     RR3RDP        IFNE      *ZERO
      *
     C     RR3ATD        IFNE      'Y'
     C     XXDEDF        ORNE      'Y'
     C                   Z-ADD     XSHGH(3)      EL3APR
     C                   ENDIF                                                  RR3ATD
      *
     C                   SELECT
     C                   WHEN      RR3ILF = 'I'
     C                   EVAL      EL3APR = XSILAM
     C                   WHEN      RR3ILF = 'W'
     C                   EVAL      EL3APR = XSHGH(3) - XSILAM
     C                   ENDSL
     C                   ENDIF
     C*
     C     RR4THP        IFNE      *ZERO
      *
     C     RR4ATD        IFNE      'Y'
     C     XXDEDF        ORNE      'Y'
     C                   Z-ADD     XSHGH(4)      EL4APR
     C                   ENDIF                                                  RR4ATD
      *
     C                   SELECT
     C                   WHEN      RR4ILF = 'I'
     C                   EVAL      EL4APR = XSILAM
     C                   WHEN      RR4ILF = 'W'
     C                   EVAL      EL4APR = XSHGH(4) - XSILAM
     C                   ENDSL
     C                   ENDIF
     C*
     C     RR5THP        IFNE      *ZERO
      *
     C     RR5ATD        IFNE      'Y'
     C     XXDEDF        ORNE      'Y'
     C                   Z-ADD     XSHGH(5)      EL5APR
     C                   ENDIF                                                  RR5ATD
      *
     C                   SELECT
     C                   WHEN      RR5ILF = 'I'
     C                   EVAL      EL5APR = XSILAM
     C                   WHEN      RR5ILF = 'W'
     C                   EVAL      EL5APR = XSHGH(5) - XSILAM
     C                   ENDSL
     C                   ENDIF
      *
     C     RR6THP        IFNE      *ZERO
      *
     C     RR6ATD        IFNE      'Y'
     C     XXDEDF        ORNE      'Y'
     C                   Z-ADD     XSHGH(6)      EL6APR
     C                   ENDIF                                                  RR6ATD
     C                   SELECT
     C                   WHEN      RR6ILF = 'I'
     C                   EVAL      EL6APR = XSILAM
     C                   WHEN      RR6ILF = 'W'
     C                   EVAL      EL6APR = XSHGH(6) - XSILAM
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C     RR7THP        IFNE      *ZERO
      *
     C     RR7ATD        IFNE      'Y'
     C     XXDEDF        ORNE      'Y'
     C                   Z-ADD     XSHGH(7)      EL7APR
     C                   ENDIF                                                  RR7ATD
     C                   SELECT
     C                   WHEN      RR7ILF = 'I'
     C                   EVAL      EL7APR = XSILAM
     C                   WHEN      RR7ILF = 'W'
     C                   EVAL      EL7APR = XSHGH(7) - XSILAM
     C                   ENDSL
      *
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXDIV IFEQ
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*
     C                   ENDIF                                                  EXBASM IFEQ
     C*
     C*   ACCUMULATE 1ST ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
      /Free
       If xxst <> 'PA' and xxst <> 'NJ'
        and xxst <> 'NY';
      /End-Free
     C                   Z-ADD     0             XX1BAL           15 4
     C                   Z-ADD     0             XX1STB
     C                   Z-ADD     0             XXDIF1
     C                   Z-ADD     0             XX1AMT
     C*
     C                   MOVE      'F'           XXARTY
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '1'                                          1ST ASSESSMT
     C                   ADD       EAAMT         XX1BAL
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   ENDDO
     C*
     C* SPECIAL CODE ADDED FOR FCC TO USE THE PFDT FOR RATING THE ASSESSMENTS
     C* SO THAT 106 YEAR POLICIES WILL GET THE MA DIA, BUT STILL ALLOW THE
     C* ANNIV DATE TO BE USED TO SPLIT THE PAYROLLS.
     C*
     C     XXST          IFEQ      'MA'
     C     PDAT(1)       ANDGE     1060101
     C     RDAT(1)       ANDLT     1060101
     C     XXCLT#        ANDEQ     '129'
     C                   Z-ADD     PDAT(1)       XXEFDT
     C                   ELSE
     C                   Z-ADD     FDAT(1)       XXEFDT
     C                   ENDIF
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR1ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX1CDE        IFNE      *BLANK
     C     RR1STP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 1ST ASSESSMENT BILLED.
     C*
     C     RR1STF        MULT      EL1FPR        XX1AMT           15 4
     C     RR1STP        MULT      EL1APR        XXPREM
     C                   ADD       XXPREM        XX1AMT
     C     XX1AMT        DIV(H)    100           XX1STB           11 0
     C                   Z-ADD     RR1STP        XXART1            5 3
     C*
     C* STORE THE AMOUNT OF THE NY STATE ASSESSMENT CHARGE.
     C*
     C     XXST          IFEQ      'NY'
     C     RR1STC        ANDEQ     932
     C     XXPREM        DIV(H)    100           XXWK10
     C                   Z-ADD     XXWK10        XX932A           11 2
     C     XX1STB        SUB       XXWK10        XX932F           11 2
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  XX1CDE IFNE
     C     XX1STB        SUB       XX1BAL        XXDIF1           15 4
     C*
     C*   ACCUMULATE 2ND ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
     C                   Z-ADD     0             XX2BAL           15 4
     C                   Z-ADD     0             XX2NDB
     C                   Z-ADD     0             XXDIF2
     C                   Z-ADD     0             XX2AMT
     C*
     C                   MOVE      'F'           XXARTY
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '2'                                          2ND ASSESSMT
     C                   ADD       EAAMT         XX2BAL
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   ENDDO
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR2ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX2CDE        IFNE      *BLANK
     C     RR2NDP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 2ND ASSESSMENT BILLED.
     C*
     C*
     C* ADD THE AMOUNT OF THE NY STATE ASSESSMENT INTO PREMIUM BASIS.
     C*
     C     XXST          IFEQ      'NY'
     C     RR2NDC        ANDEQ     9749
     C                   ADD       XX932A        EL2APR
     C                   ADD       XX932F        EL2FPR
     C                   ENDIF                                                  XXST IFEQ
     C     RR2NDF        MULT      EL2FPR        XX2AMT           15 4
     C     RR2NDP        MULT      EL2APR        XXPREM
     C                   ADD       XXPREM        XX2AMT
     C     XX2AMT        DIV(H)    100           XX2NDB           11 0
     C                   Z-ADD     RR2NDP        XXART2            5 3
     C*
     C* STORE THE AMOUNT OF THE NY STATE ASSESSMENT CHARGE.
     C*
     C     XXST          IFEQ      'NY'
     C     RR2NDC        ANDEQ     932
     C     XXPREM        DIV(H)    100           XXWK10
     C                   Z-ADD     XXWK10        XX932A           11 2
     C     XX2NDB        SUB       XXWK10        XX932F           11 2
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  XX2CDE IFNE
     C*
     C     XX2NDB        SUB       XX2BAL        XXDIF2           15 4
     C*
     C*   ACCUMULATE 3RD ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
     C                   Z-ADD     0             XX3BAL           15 4
     C                   Z-ADD     0             XX3RDB
     C                   Z-ADD     0             XXDIF3
     C                   Z-ADD     0             XX3AMT
     C*
     C                   MOVE      'F'           XXARTY
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '3'                                          3RD ASSESSMT
     C                   ADD       EAAMT         XX3BAL
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   ENDDO
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR3ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX3CDE        IFNE      *BLANK
     C     RR3RDP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 3RD ASSESSMENT BILLED.
     C*
     C*
     C* ADD THE AMOUNT OF THE NY STATE ASSESSMENT INTO PREMIUM BASIS.
     C*
     C     XXST          IFEQ      'NY'
     C     RR3RDC        ANDEQ     9749
     C                   ADD       XX932A        EL3APR
     C                   ADD       XX932F        EL3FPR
     C                   ENDIF                                                  XXST IFEQ
     C     RR3RDF        MULT      EL3FPR        XX3AMT           15 4
     C     RR3RDP        MULT      EL3APR        XXPREM
     C                   ADD       XXPREM        XX3AMT
     C     XX3AMT        DIV(H)    100           XX3RDB           11 0
     C                   Z-ADD     RR3RDP        XXART3            5 3
     C*
     C* STORE THE AMOUNT OF THE NY STATE ASSESSMENT CHARGE.
     C*
     C     XXST          IFEQ      'NY'
     C     RR3RDC        ANDEQ     932
     C     XXPREM        DIV(H)    100           XXWK10
     C                   Z-ADD     XXWK10        XX932A           11 2
     C     XX3RDB        SUB       XXWK10        XX932F           11 2
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  XX3CDE IFNE
     C*
     C     XX3RDB        SUB       XX3BAL        XXDIF3           15 4
     C*
     C*
     C*   ACCUMULATE 4TH ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
     C                   Z-ADD     0             XX4BAL           15 4
     C                   Z-ADD     0             XX4THB
     C                   Z-ADD     0             XXDIF4
     C                   Z-ADD     0             XX4AMT
     C*
     C                   MOVE      'F'           XXARTY
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '4'                                          4TH ASSESSMT
     C                   ADD       EAAMT         XX4BAL
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   ENDDO
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR4ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX4CDE        IFNE      *BLANK
     C     RR4THP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 4TH ASSESSMENT BILLED.
     C*
     C*
     C* ADD THE AMOUNT OF THE NY STATE ASSESSMENT INTO PREMIUM BASIS.
     C*
     C     XXST          IFEQ      'NY'
     C     RR4THC        ANDEQ     9749
     C                   ADD       XX932A        EL4APR
     C                   ADD       XX932F        EL4FPR
     C                   ENDIF                                                  XXST IFEQ
     C     RR4THF        MULT      EL4FPR        XX4AMT           15 4
     C     RR4THP        MULT      EL4APR        XXPREM
     C                   ADD       XXPREM        XX4AMT
     C     XX4AMT        DIV(H)    100           XX4THB           11 0
     C                   Z-ADD     RR4THP        XXART4            5 3
     C*
     C* STORE THE AMOUNT OF THE NY STATE ASSESSMENT CHARGE.
     C*
     C     XXST          IFEQ      'NY'
     C     RR4THC        ANDEQ     932
     C     XXPREM        DIV(H)    100           XXWK10
     C                   Z-ADD     XXWK10        XX932A           11 2
     C     XX4THB        SUB       XXWK10        XX932F           11 2
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  XX4CDE IFNE
     C*
     C     XX4THB        SUB       XX4BAL        XXDIF4           15 4
     C*
     C*   ACCUMULATE 5TH ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
     C                   Z-ADD     0             XX5BAL           15 4
     C                   Z-ADD     0             XX5THB
     C                   Z-ADD     0             XXDIF5
     C                   Z-ADD     0             XX5AMT
     C*
     C                   MOVE      'F'           XXARTY
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '5'                                          5TH ASSESSMT
     C                   ADD       EAAMT         XX5BAL
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   ENDDO
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR5ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX5CDE        IFNE      *BLANK
     C     RR5THP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 5TH ASSESSMENT BILLED.
     C*
     C*
     C* ADD THE AMOUNT OF THE NY STATE ASSESSMENT INTO PREMIUM BASIS.
     C*
     C     XXST          IFEQ      'NY'
     C     RR5THC        ANDEQ     9749
     C                   ADD       XX932A        EL5APR
     C                   ADD       XX932F        EL5FPR
     C                   ENDIF                                                  XXST IFEQ
     C     RR5THF        MULT      EL5FPR        XX5AMT           15 4
     C     RR5THP        MULT      EL5APR        XXPREM
     C                   ADD       XXPREM        XX5AMT
     C     XX5AMT        DIV(H)    100           XX5THB           11 0
     C                   Z-ADD     RR5THP        XXART5            5 3
     C*
     C                   ENDIF                                                  XX5CDE IFNE
     C*
     C     XX5THB        SUB       XX5BAL        XXDIF5           15 4
     C*
     C*   ACCUMULATE 6TH ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
     C                   Z-ADD     0             XX6BAL           15 4
     C                   Z-ADD     0             XX6THB
     C                   Z-ADD     0             XXDIF6
     C                   Z-ADD     0             XX6AMT
     C*
     C                   MOVE      'F'           XXARTY
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '6'                                          6TH ASSESSMT
     C                   ADD       EAAMT         XX6BAL
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   ENDDO
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR6ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX6CDE        IFNE      *BLANK
     C     RR6THP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 6TH ASSESSMENT BILLED.
     C*
     C* ADD THE AMOUNT OF THE NY STATE ASSESSMENT INTO PREMIUM BASIS.
     C*
     C     XXST          IFEQ      'NY'
     C     RR6THC        ANDEQ     9749
     C                   ADD       XX932A        EL6APR
     C                   ADD       XX932F        EL6FPR
     C                   ENDIF                                                  XXST IFEQ
     C     RR6THF        MULT      EL6FPR        XX6AMT           15 4
     C     RR6THP        MULT      EL6APR        XXPREM
     C                   ADD       XXPREM        XX6AMT
     C     XX6AMT        DIV(H)    100           XX6THB           11 0
     C                   Z-ADD     RR6THP        XXART6            5 3
     C*
     C* STORE THE AMOUNT OF THE NY STATE ASSESSMENT CHARGE.
     C*
     C     XXST          IFEQ      'NY'
     C     RR6THC        ANDEQ     932
     C     XXPREM        DIV(H)    100           XXWK10
     C                   Z-ADD     XXWK10        XX932A           11 2
     C     XX6THB        SUB       XXWK10        XX932F           11 2
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  XX6CDE IFNE
     C*
     C     XX6THB        SUB       XX6BAL        XXDIF6           15 4
     C*
     C*   ACCUMULATE 7TH ASSESSMENT BILLINGS ALREADY ON FILE FOR FYR
     C*
     C                   Z-ADD     0             XX7BAL           15 4
     C                   Z-ADD     0             XX7THB
     C                   Z-ADD     0             XXDIF7
     C                   Z-ADD     0             XX7AMT
     C*
     C                   MOVE      'F'           XXARTY
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        IFEQ      '7'                                          7TH ASSESSMT
     C                   ADD       EAAMT         XX7BAL
     C                   ENDIF
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   ENDDO
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR7ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX7CDE        IFNE      *BLANK
     C     RR7THP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 7TH ASSESSMENT BILLED.
     C*
     C* ADD THE AMOUNT OF THE NY STATE ASSESSMENT INTO PREMIUM BASIS.
     C*
     C     XXST          IFEQ      'NY'
     C     RR7THC        ANDEQ     9749
     C                   ADD       XX932A        EL7APR
     C                   ADD       XX932F        EL7FPR
     C                   ENDIF                                                  XXST IFEQ
     C     RR7THF        MULT      EL7FPR        XX7AMT           15 4
     C     RR7THP        MULT      EL7APR        XXPREM
     C                   ADD       XXPREM        XX7AMT
     C     XX7AMT        DIV(H)    100           XX7THB           11 0
     C                   Z-ADD     RR7THP        XXART7            5 3
     C*
     C                   ENDIF                                                  XX7CDE IFNE
     C*
     C     XX7THB        SUB       XX7BAL        XXDIF7           15 4
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C                   ELSE
     C*
     C                   Z-ADD     0             XX1STB
     C                   Z-ADD     0             XX2NDB
     C                   Z-ADD     0             XX3RDB
     C                   Z-ADD     0             XX4THB
     C                   Z-ADD     0             XX5THB
     C                   Z-ADD     0             XX6THB
     C                   Z-ADD     0             XX7THB
     C                   Z-ADD     0             XXART1
     C                   Z-ADD     0             XXART2
     C                   Z-ADD     0             XXART3
     C                   Z-ADD     0             XXART4
     C                   Z-ADD     0             XXART5
     C                   Z-ADD     0             XXART6
     C                   Z-ADD     0             XXART7
     C*
     C                   Z-ADD     1             X
     C*
     C                   Z-ADD     0             XX1BAL           15 4
     C                   Z-ADD     0             XX2BAL           15 4
     C                   Z-ADD     0             XX3BAL           15 4
     C                   Z-ADD     0             XX4BAL           15 4
     C                   Z-ADD     0             XX5BAL           15 4
     C                   Z-ADD     0             XX6BAL           15 4
     C                   Z-ADD     0             XX7BAL           15 4
     C*
     C                   Z-ADD     0             XX1AMT
     C                   Z-ADD     0             XX2AMT
     C                   Z-ADD     0             XX3AMT
     C                   Z-ADD     0             XX4AMT
     C                   Z-ADD     0             XX5AMT
     C                   Z-ADD     0             XX6AMT
     C                   Z-ADD     0             XX7AMT
     C*
     C                   Z-ADD     0             XXDIF1
     C                   Z-ADD     0             XXDIF2
     C                   Z-ADD     0             XXDIF3
     C                   Z-ADD     0             XXDIF4
     C                   Z-ADD     0             XXDIF5
     C                   Z-ADD     0             XXDIF6
     C                   Z-ADD     0             XXDIF7
     C*
     C* ACCUMULATE BILLED AMOUNTS.
     C*
     C                   MOVE      'F'           XXARTY
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C     TTCTYP        IFEQ      '1'                                          1ST ASSESSMT
     C                   ADD       EAAMT         XX1BAL
     C                   ENDIF
     C*
     C     TTCTYP        IFEQ      '2'                                          2ND ASSESSMT
     C                   ADD       EAAMT         XX2BAL
     C                   ENDIF
     C*
     C     TTCTYP        IFEQ      '3'                                          3RD ASSESSMT
     C                   ADD       EAAMT         XX3BAL
     C                   ENDIF
     C*
     C     TTCTYP        IFEQ      '4'                                          4TH ASSESSMT
     C                   ADD       EAAMT         XX4BAL
     C                   ENDIF
     C*
     C     TTCTYP        IFEQ      '5'                                          5TH ASSESSMT
     C                   ADD       EAAMT         XX5BAL
     C                   ENDIF
     C*
     C     TTCTYP        IFEQ      '6'                                          6TH ASSESSMT
     C                   ADD       EAAMT         XX6BAL
     C                   ENDIF
     C*
     C     TTCTYP        IFEQ      '7'                                          7TH ASSESSMT
     C                   ADD       EAAMT         XX7BAL
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   ENDDO
     C*
     C* PROCESS ASSESSMENT FOR EACH RATING PERIOD.
     C*
     C     RDAT(X)       DOWNE     *ZERO
     C*
     C                   Z-ADD     RDAT(X)       XXEFDT
     C     XXST          IFEQ      'NJ'
     C                   Z-ADD     xsPFDT        XXEFDT
     C                   ENDIF
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR1ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX1CDE        IFNE      *BLANK
     C     RR1STP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 1ST ASSESSMENT BILLED.
     C*
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR1STL        ANDEQ     'X'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDNE     'NJ'
     C                   Z-ADD     STN1          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR1ATR <> 'B') AND (RR1ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR1ATR <> 'B') AND (RR1ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ELSE
     C*
     C                   SELECT
     C     RR1STL        WHENEQ    'J'
     C                   Z-ADD     A1J           ASA
     C     RR1STL        WHENEQ    'E'
     C                   Z-ADD     A1E           ASA
     C     RR1STL        WHENEQ    'S'
     C                   Z-ADD     A1S           ASA
     C     RR1STL        WHENEQ    'D'
     C                   Z-ADD     A1D           ASA
     C     RR1STL        WHENEQ    'X'
     C                   Z-ADD     STN1          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR1ATR <> 'B') AND (RR1ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR1ATR <> 'B') AND (RR1ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDSL                                                  RR1STL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR1IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR1STL        ANDNE     'J'
     C     RR1STL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR1STL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR1STL        ANDNE     'J'
     C     RR1STL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR1STL        ANDNE     'J'
     C     RR1STL        ANDNE     'E'
     C     RR1STL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR1STL        ANDNE     'J'
     C     RR1STL        ANDNE     'E'
     C     RR1STL        ANDNE     'S'
     C     RR1STL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   ADD       EDCT(XX)      ASA(XX)
     C                   ADD       EDD2(XX)      ASA(XX)
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*
     C                   Z-ADD     1             XX                2 0
     C                   IF        RR1STL <> 'X'
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR1ATR = 'B') OR (RR1ATR = 'F')
      * Add terrorism charge, not payroll to the assesment premium base
     C                   ADD       ETR(XX)       ASA(XX)
     C                   ADD       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR1ATR = 'B') OR (RR1ATR = 'D')
     C                   ADD       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDIF                                                  RR1STL <> 'X'
     C                   IF        RR1ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm1       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   xxamt154 = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR1ILF:RR1STL:X1SUBJ:X1EXPP:X1STAN:
     C                                      X1DISP:X1NET:ELAMNP:RR1ATR:asmFact)
     C*
     C                   Eval(h)   xxwk10 = asa(x) * asmFact
     C                   Eval      asa(x) = xxwk10
     C                   ENDIF                                                  RR1ILF <> ' '
     C*
     C     RR1STF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR1xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C*
     C     XXFPCT        MULT(H)   ASA(X)        XXWK10
     C     RR1STF        MULT      XXWK10        XX1AMT           15 4
      /Free
       If xxst = 'NY' AND RR1stc = 932;
         F932(x) = %IntH(xx1amt/100);
       EndIf;
      /End-Free
     C     ASA(X)        SUB       XXWK10        XXPREM
     C     RR1STP        MULT      XXPREM        XXPREM
      /Free
       If xxst = 'NY' AND RR1stc = 932;
         A932(x) = %IntH(XXprem/100);
       EndIf;
      /End-Free
     C                   ADD       XXPREM        XX1AMT
     C     XX1AMT        DIV(H)    100           XXWK10
     C                   ADD       XXWK10        XX1STB
     C                   ADD       XXWK10        ASM1(X)
     C*
     C                   ELSE
     C*
     C     RR1STP        MULT      ASA(X)        XX1AMT
     C     XX1AMT        DIV(H)    100           XXWK10
      /Free
       If xxst = 'NY' AND RR1stc = 932;
         A932(x) = xxwk10;
       EndIf;
      /End-Free
     C                   ADD       XXWK10        XX1STB
     C                   ADD       XXWK10        ASM1(X)
     C*
     C                   ENDIF                                                  RR1STF IFNE
     C                   Z-ADD     RR1STP        ART1(X)
     C*
     C                   ENDIF                                                  XX1CDE IFNE
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR2ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX2CDE        IFNE      *BLANK
     C     RR2NDP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 2ND ASSESSMENT BILLED.
     C*
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR2NDL        ANDEQ     'X'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDNE     'NJ'
      * WARNING ++++ NY NOT OMITTED ++++++
     C                   Z-ADD     STN2          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR2ATR <> 'B') AND (RR2ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR2ATR <> 'B') AND (RR2ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ELSE
     C*
     C                   SELECT
     C     RR2NDL        WHENEQ    'J'
     C                   Z-ADD     A2J           ASA
     C     RR2NDL        WHENEQ    'E'
     C                   Z-ADD     A2E           ASA
     C     RR2NDL        WHENEQ    'S'
     C                   Z-ADD     A2S           ASA
     C     RR2NDL        WHENEQ    'D'
     C                   Z-ADD     A2D           ASA
     C     RR2NDL        WHENEQ    'X'
     C                   Z-ADD     STN2          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR2ATR <> 'B') AND (RR2ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR2ATR <> 'B') AND (RR2ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDSL                                                  RR2NDL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR2IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR2NDL        ANDNE     'J'
     C     RR2NDL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR2NDL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR2NDL        ANDNE     'J'
     C     RR2NDL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR2NDL        ANDNE     'J'
     C     RR2NDL        ANDNE     'E'
     C     RR2NDL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR2NDL        ANDNE     'J'
     C     RR2NDL        ANDNE     'E'
     C     RR2NDL        ANDNE     'S'
     C     RR2NDL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   ADD       EDCT(XX)      ASA(XX)
     C                   ADD       EDD2(XX)      ASA(XX)
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*
     C                   Z-ADD     1             XX                2 0
     C                   IF        RR2NDL <> 'X'
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR2ATR = 'B') OR (RR2ATR = 'F')
      * Add terrorism charge, not payroll to the assesment premium base
     C                   ADD       ETR(XX)       ASA(XX)
     C                   ADD       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR2ATR = 'B') OR (RR2ATR = 'D')
     C                   ADD       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDIF                                                  RR2NDL <> 'X'
     C                   IF        RR2ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm2       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   xxamt154 = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR2ILF:RR2NDL:X2SUBJ:X2EXPP:X2STAN:
     C                                      X2DISP:X2NET:ELAMNP:RR2ATR:asmFact)
     C*
     C                   Eval(h)   xxwk10 = asa(x) * asmFact
     C                   Eval      asa(x) = xxwk10
     C                   ENDIF                                                  RR2ILF <> ' '
     C*
     C     RR2NDF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR2xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C*
     C     XXFPCT        MULT(H)   ASA(X)        XXWK10

      /Free
       // For 9749 assessment code & NY state, 932 assmt. code F class amount should be added.
       If xxst = 'NY' And RR2ndc = 9749;
          intWk11 = xxwk10 + F932(x);
          xx2Amt = intWk11 * RR2ndf ;
       Else;
          xx2Amt = xxwk10 * RR2ndf ;
       EndIf;

       //Calculate F932 for next assessment.
       If xxst = 'NY' AND RR2ndc = 932;
         F932(x) = %IntH(xx2amt/100);
       EndIf;

       // Regular assmt =  premium - F class assmt amount.
       xxprem = asa(x) - xxwk10 ;

       // For 9749 assessment code & NY state, 932 assmt. code regular amount should be added.
       If xxst = 'NY' And RR2ndc = 9749;
          xxprem = xxprem + A932(x);
       EndIf;

      /End-Free
     C     RR2NDP        MULT      XXPREM        XXPREM
       //Calculate A932 for next assessment.
      /Free
       If xxst = 'NY' AND RR2ndc = 932;
         A932(x) = %IntH(XXprem/100);
       EndIf;
      /End-Free
     C                   ADD       XXPREM        XX2AMT
     C     XX2AMT        DIV(H)    100           XXWK10
     C                   ADD       XXWK10        XX2NDB
     C                   ADD       XXWK10        ASM2(X)
     C*
     C                   ELSE
     C*
      /Free
       If xxst = 'NY' And RR2ndc = 9749;
          intWk11 = Asa(x) + A932(x);
          xx2amt = RR2ndp * intWk11;
       Else;
          xx2amt = RR2ndp * Asa(x);
       EndIf;
      /End-Free
     C     XX2AMT        DIV(H)    100           XXWK10
      /Free
       If xxst = 'NY' And RR2ndc = 932;
          A932(x) = xxwk10;
       EndIf;
      /End-Free
     C                   ADD       XXWK10        XX2NDB
     C                   ADD       XXWK10        ASM2(X)
     C*
     C                   ENDIF                                                  RR2NDF IFNE
     C                   Z-ADD     RR2NDP        ART2(X)
     C*
     C                   ENDIF                                                  XX2CDE IFNE
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR3ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX3CDE        IFNE      *BLANK
     C     RR3RDP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 3RD ASSESSMENT BILLED.
     C*
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR3RDL        ANDEQ     'X'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDNE     'NJ'
      *++++ WARNING NY NOT OMITTED   ++++++++++++++
     C                   Z-ADD     STN3          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR3ATR <> 'B') AND (RR3ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR3ATR <> 'B') AND (RR3ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ELSE
     C*
     C                   SELECT
     C     RR3RDL        WHENEQ    'J'
     C                   Z-ADD     A3J           ASA
     C     RR3RDL        WHENEQ    'E'
     C                   Z-ADD     A3E           ASA
     C     RR3RDL        WHENEQ    'S'
     C                   Z-ADD     A3S           ASA
     C     RR3RDL        WHENEQ    'D'
     C                   Z-ADD     A3D           ASA
     C     RR3RDL        WHENEQ    'X'
     C                   Z-ADD     STN3          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR3ATR <> 'B') AND (RR3ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR3ATR <> 'B') AND (RR3ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDSL                                                  RR3RDL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR3IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR3RDL        ANDNE     'J'
     C     RR3RDL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR3RDL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR3RDL        ANDNE     'J'
     C     RR3RDL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR3RDL        ANDNE     'J'
     C     RR3RDL        ANDNE     'E'
     C     RR3RDL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR3RDL        ANDNE     'J'
     C     RR3RDL        ANDNE     'E'
     C     RR3RDL        ANDNE     'S'
     C     RR3RDL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   ADD       EDCT(XX)      ASA(XX)
     C                   ADD       EDD2(XX)      ASA(XX)
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*
     C                   Z-ADD     1             XX                2 0
     C                   IF        RR3RDL <> 'X'
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR3ATR = 'B') OR (RR3ATR = 'F')
      * Add terrorism charge, not payroll to the assesment premium base
     C                   ADD       ETR(XX)       ASA(XX)
     C                   ADD       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR3ATR = 'B') OR (RR3ATR = 'D')
     C                   ADD       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDIF                                                  RR3RDL <> 'X'
     C                   IF        RR3ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm3       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   xxamt154 = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR3ILF:RR3RDL:X3SUBJ:X3EXPP:X3STAN:
     C                                      X3DISP:X3NET:ELAMNP:RR3ATR:asmFact)
     C*
     C                   Eval(h)   xxwk10 = asa(x) * asmFact
     C                   Eval      asa(x) = xxwk10
     C                   ENDIF                                                  RR3ILF <> ' '
     C*
     C     RR3RDF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR3xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C*
     C     XXFPCT        MULT(H)   ASA(X)        XXWK10

      /Free
       // For 9749 assessment code & NY state, 932 assmt. code F class amount should be added.
       If xxst = 'NY' And RR3rdc = 9749;
          intWk11 = xxwk10 + F932(x);
          xx3Amt = intWk11 * RR3rdf ;
       Else;
          xx3Amt = xxwk10 * RR3rdf ;
       EndIf;

       //Calculate F932 for next assessment.
       If xxst = 'NY' AND RR3rdc = 932;
         F932(x) = %IntH(xx3amt/100);
       EndIf;

       // Regular assmt =  premium - F class assmt amount.
       xxprem = asa(x) - xxwk10 ;

       // For 9749 assessment code & NY state, 932 assmt. code regular amount should be added.
       If xxst = 'NY' And RR3rdc = 9749;
          xxprem = xxprem + A932(x);
       EndIf;

      /End-Free
     C     RR3RDP        MULT      XXPREM        XXPREM
       //Calculate A932 for next assessment.
      /Free
       If xxst = 'NY' AND RR3rdc = 932;
         A932(x) = %IntH(XXprem/100);
       EndIf;
      /End-Free
     C                   ADD       XXPREM        XX3AMT
     C     XX3AMT        DIV(H)    100           XXWK10
     C                   ADD       XXWK10        XX3RDB
     C                   ADD       XXWK10        ASM3(X)
     C*
     C                   ELSE
     C*
      /Free
       If xxst = 'NY' And RR3rdc = 9749;
          intWk11 = Asa(x) + A932(x);
          xx3amt = RR3rdp * intWk11;
       Else;
          xx3amt = RR3rdp * Asa(x);
       EndIf;
      /End-Free
     C     XX3AMT        DIV(H)    100           XXWK10
      /Free
       If xxst = 'NY' And RR3rdc = 932;
          A932(x) = xxwk10;
       EndIf;
      /End-Free
     C                   ADD       XXWK10        XX3RDB
     C                   ADD       XXWK10        ASM3(X)
     C*
     C                   ENDIF                                                  RR3RDF IFNE
     C                   Z-ADD     RR3RDP        ART3(X)
     C*
     C                   ENDIF                                                  XX3CDE IFNE
     C*
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR4ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
     C*
     C     XX4CDE        IFNE      *BLANK
     C     RR4THP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 4TH ASSESSMENT BILLED.
     C*
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR4THL        ANDEQ     'X'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDNE     'NJ'
      *++++ WARNING NY NOT OMITTED   ++++++++++++++
     C                   Z-ADD     STN4          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR4ATR <> 'B') AND (RR4ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR4ATR <> 'B') AND (RR4ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ELSE
     C*
     C                   SELECT
     C     RR4THL        WHENEQ    'J'
     C                   Z-ADD     A4J           ASA
     C     RR4THL        WHENEQ    'E'
     C                   Z-ADD     A4E           ASA
     C     RR4THL        WHENEQ    'S'
     C                   Z-ADD     A4S           ASA
     C     RR4THL        WHENEQ    'D'
     C                   Z-ADD     A4D           ASA
     C     RR4THL        WHENEQ    'X'
     C                   Z-ADD     STN4          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR4ATR <> 'B') AND (RR4ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR4ATR <> 'B') AND (RR4ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDSL                                                  RR4THL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR4IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR4THL        ANDNE     'J'
     C     RR4THL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR4THL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR4THL        ANDNE     'J'
     C     RR4THL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR4THL        ANDNE     'J'
     C     RR4THL        ANDNE     'E'
     C     RR4THL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR4THL        ANDNE     'J'
     C     RR4THL        ANDNE     'E'
     C     RR4THL        ANDNE     'S'
     C     RR4THL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   ADD       EDCT(XX)      ASA(XX)
     C                   ADD       EDD2(XX)      ASA(XX)
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*
     C                   Z-ADD     1             XX                2 0
     C                   IF        RR4THL <> 'X'
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR4ATR = 'B') OR (RR4ATR = 'F')
      * Add terrorism charge, not payroll to the assesment premium base
     C                   ADD       ETR(XX)       ASA(XX)
     C                   ADD       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR4ATR = 'B') OR (RR4ATR = 'D')
     C                   ADD       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDIF                                                  RR4THL <> 'X'
     C                   IF        RR4ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm4       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   xxamt154 = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR4ILF:RR4THL:X4SUBJ:X4EXPP:X4STAN:
     C                                      X4DISP:X4NET:ELAMNP:RR4ATR:asmFact)
     C*
     C                   Eval(h)   xxwk10 = asa(x) * asmFact
     C                   Eval      asa(x) = xxwk10
     C                   ENDIF                                                  RR4ILF <> ' '
     C*
     C     RR4THF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR4xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C*
     C     XXFPCT        MULT(H)   ASA(X)        XXWK10

      /Free
       // For 9749 assessment code & NY state, 932 assmt. code F class amount should be added.
       If xxst = 'NY' And RR4thc = 9749;
          intWk11 = xxwk10 + F932(x);
          xx4Amt = intWk11 * RR4thf ;
       Else;
          xx4Amt = xxwk10 * RR4thf ;
       EndIf;

       //Calculate F932 for next assessment.
       If xxst = 'NY' AND RR4thc = 932;
         F932(x) = %IntH(xx4amt/100);
       EndIf;

       // Regular assmt =  premium - F class assmt amount.
       xxprem = asa(x) - xxwk10 ;

       // For 9749 assessment code & NY state, 932 assmt. code regular amount should be added.
       If xxst = 'NY' And RR4thc = 9749;
          xxprem = xxprem + A932(x);
       EndIf;

      /End-Free
     C     RR4THP        MULT      XXPREM        XXPREM
       //Calculate A932 for next assessment.
      /Free
       If xxst = 'NY' AND RR4thc = 932;
         A932(x) = %IntH(XXprem/100);
       EndIf;
      /End-Free
     C                   ADD       XXPREM        XX4AMT
     C     XX4AMT        DIV(H)    100           XXWK10
     C                   ADD       XXWK10        XX4THB
     C                   ADD       XXWK10        ASM4(X)
     C*
     C                   ELSE
     C*
      /Free
       If xxst = 'NY' And RR4thc = 9749;
          intWk11 = Asa(x) + A932(x);
          xx4amt = RR4thp * intWk11;
       Else;
          xx4amt = RR4thp * Asa(x);
       EndIf;
      /End-Free
     C     XX4AMT        DIV(H)    100           XXWK10
      /Free
       If xxst = 'NY' And RR4thc = 932;
          A932(x) = xxwk10;
       EndIf;
      /End-Free
     C                   ADD       XXWK10        XX4THB
     C                   ADD       XXWK10        ASM4(X)
     C*
     C                   ENDIF                                                  RR4THF IFNE
     C                   Z-ADD     RR4THP        ART4(X)
     C*
     C                   ENDIF                                                  XX4CDE IFNE
     C*
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR5ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
      *
     C     XX5CDE        IFNE      *BLANK
     C     RR5THP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 5TH ASSESSMENT BILLED.
     C*
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR5THL        ANDEQ     'X'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDNE     'NJ'
      *++++ WARNING NY NOT OMITTED   ++++++++++++++
     C                   Z-ADD     STN5          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR5ATR <> 'B') AND (RR5ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR5ATR <> 'B') AND (RR5ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ELSE
     C*
     C                   SELECT
     C     RR5THL        WHENEQ    'J'
     C                   Z-ADD     A5J           ASA
     C     RR5THL        WHENEQ    'E'
     C                   Z-ADD     A5E           ASA
     C     RR5THL        WHENEQ    'S'
     C                   Z-ADD     A5S           ASA
     C     RR5THL        WHENEQ    'D'
     C                   Z-ADD     A5D           ASA
     C     RR5THL        WHENEQ    'X'
     C                   Z-ADD     STN5          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR5ATR <> 'B') AND (RR5ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR5ATR <> 'B') AND (RR5ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDSL                                                  RR5THL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR5IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR5THL        ANDNE     'J'
     C     RR5THL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR5THL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR5THL        ANDNE     'J'
     C     RR5THL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR5THL        ANDNE     'J'
     C     RR5THL        ANDNE     'E'
     C     RR5THL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR5THL        ANDNE     'J'
     C     RR5THL        ANDNE     'E'
     C     RR5THL        ANDNE     'S'
     C     RR5THL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   ADD       EDCT(XX)      ASA(XX)
     C                   ADD       EDD2(XX)      ASA(XX)
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*
     C                   Z-ADD     1             XX                2 0
     C                   IF        RR5THL <> 'X'
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR5ATR = 'B') OR (RR5ATR = 'F')
      * Add terrorism charge, not payroll to the assesment premium base
     C                   ADD       ETR(XX)       ASA(XX)
     C                   ADD       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR5ATR = 'B') OR (RR5ATR = 'D')
     C                   ADD       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDIF                                                  RR5THL <> 'X'
     C                   IF        RR5ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm5       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   xxamt154 = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR5ILF:RR5THL:X5SUBJ:X5EXPP:X5STAN:
     C                                      X5DISP:X5NET:ELAMNP:RR5ATR:asmFact)
     C*
     C                   Eval(h)   xxwk10 = asa(x) * asmFact
     C                   Eval      asa(x) = xxwk10
     C                   ENDIF                                                  RR5ILF <> ' '
     C*
     C     RR5THF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR5xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C*
     C     XXFPCT        MULT(H)   ASA(X)        XXWK10

      /Free
       // For 9749 assessment code & NY state, 932 assmt. code F class amount should be added.
       If xxst = 'NY' And RR5thc = 9749;
          intWk11 = xxwk10 + F932(x);
          xx5Amt = intWk11 * RR5thf ;
       Else;
          xx5Amt = xxwk10 * RR5thf ;
       EndIf;

       //Calculate F932 for next assessment.
       If xxst = 'NY' AND RR5thc = 932;
         F932(x) = %IntH(xx5amt/100);
       EndIf;

       // Regular assmt =  premium - F class assmt amount.
       xxprem = asa(x) - xxwk10 ;

       // For 9749 assessment code & NY state, 932 assmt. code regular amount should be added.
       If xxst = 'NY' And RR5thc = 9749;
          xxprem = xxprem + A932(x);
       EndIf;

      /End-Free
     C     RR5THP        MULT      XXPREM        XXPREM
       //Calculate A932 for next assessment.
      /Free
       If xxst = 'NY' AND RR5thc = 932;
         A932(x) = %IntH(XXprem/100);
       EndIf;
      /End-Free
     C                   ADD       XXPREM        XX5AMT
     C     XX5AMT        DIV(H)    100           XXWK10
     C                   ADD       XXWK10        XX5THB
     C                   ADD       XXWK10        ASM5(X)
     C*
     C                   ELSE
     C*
      /Free
       If xxst = 'NY' And RR5thc = 9749;
          intWk11 = Asa(x) + A932(x);
          xx5amt = RR5thp * intWk11;
       Else;
          xx5amt = RR5thp * Asa(x);
       EndIf;
      /End-Free
     C     XX5AMT        DIV(H)    100           XXWK10
      /Free
       If xxst = 'NY' And RR5thc = 932;
          A932(x) = xxwk10;
       EndIf;
      /End-Free
     C                   ADD       XXWK10        XX5THB
     C                   ADD       XXWK10        ASM5(X)
     C*
     C                   ENDIF                                                  RR5THF IFNE
     C                   Z-ADD     RR5THP        ART5(X)
     C*
     C                   ENDIF                                                  XX5CDE IFNE
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR6ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
      *
     C     XX6CDE        IFNE      *BLANK
     C     RR6THP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 6TH ASSESSMENT BILLED.
     C*
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR6THL        ANDEQ     'X'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDNE     'NJ'
     C                   Z-ADD     STN6          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR6ATR <> 'B') AND (RR6ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR6ATR <> 'B') AND (RR6ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ELSE
     C*
     C                   SELECT
     C     RR6THL        WHENEQ    'J'
     C                   Z-ADD     A6J           ASA
     C     RR6THL        WHENEQ    'E'
     C                   Z-ADD     A6E           ASA
     C     RR6THL        WHENEQ    'S'
     C                   Z-ADD     A6S           ASA
     C     RR6THL        WHENEQ    'D'
     C                   Z-ADD     A6D           ASA
     C     RR6THL        WHENEQ    'X'
     C                   Z-ADD     STN6          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR6ATR <> 'B') AND (RR6ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR6ATR <> 'B') AND (RR6ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C                   ENDSL                                                  RR6THL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR6IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR6THL        ANDNE     'J'
     C     RR6THL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR6THL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR6THL        ANDNE     'J'
     C     RR6THL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR6THL        ANDNE     'J'
     C     RR6THL        ANDNE     'E'
     C     RR6THL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR6THL        ANDNE     'J'
     C     RR6THL        ANDNE     'E'
     C     RR6THL        ANDNE     'S'
     C     RR6THL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   ADD       EDCT(XX)      ASA(XX)
     C                   ADD       EDD2(XX)      ASA(XX)
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*
     C                   Z-ADD     1             XX                2 0
     C                   IF        RR6THL <> 'X'
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR6ATR = 'B') OR (RR6ATR = 'F')
      * Add terrorism charge, not payroll to the assesment premium base
     C                   ADD       ETR(XX)       ASA(XX)
     C                   ADD       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR6ATR = 'B') OR (RR6ATR = 'D')
     C                   ADD       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDIF                                                  RR6THL <> 'X'
     C                   IF        RR6ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm6       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   xxamt154 = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR6ILF:RR6THL:X6SUBJ:X6EXPP:X6STAN:
     C                                      X6DISP:X6NET:ELAMNP:RR6ATR:asmFact)
     C*
     C                   Eval(h)   xxwk10 = asa(x) * asmFact
     C                   Eval      asa(x) = xxwk10
     C                   ENDIF                                                  RR6ILF <> ' '
     C*
     C     RR6THF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR6xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C*
     C     XXFPCT        MULT(H)   ASA(X)        XXWK10

      /Free
       // For 9749 assessment code & NY state, 932 assmt. code F class amount should be added.
       If xxst = 'NY' And RR6thc = 9749;
          intWk11 = xxwk10 + F932(x);
          xx6Amt = intWk11 * RR6thf ;
       Else;
          xx6Amt = xxwk10 * RR6thf ;
       EndIf;

       //Calculate F932 for next assessment.
       If xxst = 'NY' AND RR6thc = 932;
         F932(x) = %IntH(xx6amt/100);
       EndIf;

       // Regular assmt =  premium - F class assmt amount.
       xxprem = asa(x) - xxwk10 ;

       // For 9749 assessment code & NY state, 932 assmt. code regular amount should be added.
       If xxst = 'NY' And RR6thc = 9749;
          xxprem = xxprem + A932(x);
       EndIf;

      /End-Free
     C     RR6THP        MULT      XXPREM        XXPREM
       //Calculate A932 for next assessment.
      /Free
       If xxst = 'NY' AND RR6thc = 932;
         A932(x) = %IntH(XXprem/100);
       EndIf;
      /End-Free
     C                   ADD       XXPREM        XX6AMT
     C     XX6AMT        DIV(H)    100           XXWK10
     C                   ADD       XXWK10        XX6THB
     C                   ADD       XXWK10        ASM6(X)
     C*
     C                   ELSE
     C*
      /Free
       If xxst = 'NY' And RR6thc = 9749;
          intWk11 = Asa(x) + A932(x);
          xx6amt = RR6thp * intWk11;
       Else;
          xx6amt = RR6thp * Asa(x);
       EndIf;
      /End-Free
     C     XX6AMT        DIV(H)    100           XXWK10
      /Free
       If xxst = 'NY' And RR6thc = 932;
          A932(x) = xxwk10;
       EndIf;
      /End-Free
     C                   ADD       XXWK10        XX6THB
     C                   ADD       XXWK10        ASM6(X)
     C*
     C                   ENDIF                                                  RR6THF IFNE
     C                   Z-ADD     RR6THP        ART6(X)
     C*
     C                   ENDIF                                                  XX6CDE IFNE
      *
     C                   MOVE      'Y'           XXCHRG            1
     C     XXDEDF        IFNE      'Y'
     C     RR7ATD        ANDEQ     'Y'
     C                   MOVE      *BLANK        XXCHRG
     C                   ENDIF
      *
     C     XX7CDE        IFNE      *BLANK
     C     RR7THP        ANDNE     *ZEROS
     C     XXCHRG        ANDEQ     'Y'
     C*
     C*   CALCULATE 7TH ASSESSMENT BILLED.
     C*
     C     ELAMNP        IFEQ      'Y'
     C     XXST          ANDEQ     'NJ'
     C     RR7THL        ANDEQ     'X'
     C     ELAMNP        OREQ      'Y'
     C     XXST          ANDNE     'NJ'
     C                   Z-ADD     STN7          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR7ATR <> 'B') AND (RR7ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR7ATR <> 'B') AND (RR7ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ELSE
     C*
     C                   SELECT
     C     RR7THL        WHENEQ    'J'
     C                   Z-ADD     A7J           ASA
     C     RR7THL        WHENEQ    'E'
     C                   Z-ADD     A7E           ASA
     C     RR7THL        WHENEQ    'S'
     C                   Z-ADD     A7S           ASA
     C     RR7THL        WHENEQ    'D'
     C                   Z-ADD     A7D           ASA
     C     RR7THL        WHENEQ    'X'
     C                   Z-ADD     STN7          ASA
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR7ATR <> 'B') AND (RR7ATR <> 'F')
     C                   SUB       ETR(XX)       ASA(XX)
     C                   SUB       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR7ATR <> 'B') AND (RR7ATR <> 'D')
     C                   SUB       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C                   ENDSL                                                  RR7THL SELC
     C*
     C* FOR PA, ADD DEDUCTIBE DISCOUNT BACK INTO PREMIUM IF NECESSARY.
     C*
     C     XXST          IFEQ      'PA'
     C     RR7IDD        ANDNE     'Y'
     C*
     C     RRRDPR        IFEQ      'M'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     RRRDPR        OREQ      'S'
     C     RR7THL        ANDNE     'J'
     C     RR7THL        ANDNE     'E'
     C     XXLRGD        ANDNE     'Y'
     C*
     C     LDPLV1        OREQ      'M'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'J'
     C     RR7THL        ANDNE     'J'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'E'
     C     RR7THL        ANDNE     'J'
     C     RR7THL        ANDNE     'E'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'S'
     C     RR7THL        ANDNE     'J'
     C     RR7THL        ANDNE     'E'
     C     RR7THL        ANDNE     'S'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C     LDPLV1        OREQ      'D'
     C     RR7THL        ANDNE     'J'
     C     RR7THL        ANDNE     'E'
     C     RR7THL        ANDNE     'S'
     C     RR7THL        ANDNE     'D'
     C     XXLRGD        ANDEQ     'Y'
     C*
     C                   Z-ADD     1             XX                2 0
     C     RDAT(XX)      DOWNE     *ZERO
     C                   ADD       EDCT(XX)      ASA(XX)
     C                   ADD       EDD2(XX)      ASA(XX)
     C                   ADD       1             XX
     C                   ENDDO
     C*
     C                   ENDIF                                                  RRRDPR IFEQ
     C*
     C                   ENDIF                                                  XXST IFEQ
     C*
     C                   ENDIF                                                  ELEMNP IFEQ
     C*
     C                   Z-ADD     1             XX                2 0
     C                   IF        RR7THL <> 'X'
     C     RDAT(XX)      DOWNE     *ZERO
     C                   IF        (RR7ATR = 'B') OR (RR7ATR = 'F')
      * Add terrorism charge, not payroll to the assesment premium base
     C                   ADD       ETR(XX)       ASA(XX)
     C                   ADD       NTR(XX)       ASA(XX)
     C                   ENDIF
     C                   IF        (RR7ATR = 'B') OR (RR7ATR = 'D')
     C                   ADD       DTR(XX)       ASA(XX)
     C                   ENDIF
     C                   ADD       1             XX
     C                   ENDDO
     C                   ENDIF                                                  RR7THL <> 'X'
     C                   IF        RR7ILF <> ' '
     C*
     C*** begin add ***  11/12/24  ******************************************
     C                   Z-add     xxlprm7       xxlprm
     C*** end add *****  11/12/24  ******************************************
     C                   EVAL(H)   xxamt154 = prIncLmtBasis(XXTEIN:XXNET:XXLPRM:
     C                                      RR7ILF:RR7THL:X7SUBJ:X7EXPP:X7STAN:
     C                                      X7DISP:X7NET:ELAMNP:RR7ATR:asmFact)
     C*
     C                   Eval(h)   xxwk10 = asa(x) * asmFact
     C                   Eval      asa(x) = xxwk10
     C                   ENDIF                                                  RR7ILF <> ' '
     C*
     C     RR7THF        IFNE      *ZERO
     C     ELAMNP        ANDNE     'Y'
     c     RR7xfc        OrEq      'Y'
     c     ELaMnP        AndNE     'Y'
     C*
     C     XXFPCT        MULT(H)   ASA(X)        XXWK10

      /Free
       // For 9749 assessment code & NY state, 932 assmt. code F class amount should be added.
       If xxst = 'NY' And RR7thc = 9749;
          intWk11 = xxwk10 + F932(x);
          xx7Amt = intWk11 * RR7thf ;
       Else;
          xx7Amt = xxwk10 * RR7thf ;
       EndIf;

       //Calculate F932 for next assessment.
       If xxst = 'NY' AND RR7thc = 932;
         F932(x) = %IntH(xx7amt/100);
       EndIf;

       // Regular assmt =  premium - F class assmt amount.
       xxprem = asa(x) - xxwk10 ;

       // For 9749 assessment code & NY state, 932 assmt. code regular amount should be added.
       If xxst = 'NY' And RR7thc = 9749;
          xxprem = xxprem + A932(x);
       EndIf;

      /End-Free
     C     RR7THP        MULT      XXPREM        XXPREM
       //Calculate A932 for next assessment.
      /Free
       If xxst = 'NY' AND RR7thc = 932;
         A932(x) = %IntH(XXprem/100);
       EndIf;
      /End-Free
     C                   ADD       XXPREM        XX7AMT
     C     XX7AMT        DIV(H)    100           XXWK10
     C                   ADD       XXWK10        XX7THB
     C                   ADD       XXWK10        ASM7(X)
     C*
     C                   ELSE
     C*
      /Free
       If xxst = 'NY' And RR7thc = 9749;
          intWk11 = Asa(x) + A932(x);
          xx7amt = RR7thp * intWk11;
       Else;
          xx7amt = RR7thp * Asa(x);
       EndIf;
      /End-Free
     C     XX7AMT        DIV(H)    100           XXWK10
      /Free
       If xxst = 'NY' And RR6thc = 932;
          A932(x) = xxwk10;
       EndIf;
      /End-Free
     C                   ADD       XXWK10        XX7THB
     C                   ADD       XXWK10        ASM7(X)
     C*
     C                   ENDIF                                                  RR7THF IFNE
     C                   Z-ADD     RR7THP        ART7(X)
     C*
     C                   ENDIF                                                  XX7CDE IFNE
      *
     C                   ADD       1             X
     C                   ENDDO
     C*
     C     XX1STB        SUB       XX1BAL        XXDIF1           15 4
     C     XX2NDB        SUB       XX2BAL        XXDIF2           15 4
     C     XX3RDB        SUB       XX3BAL        XXDIF3           15 4
     C     XX4THB        SUB       XX4BAL        XXDIF4           15 4
     C     XX5THB        SUB       XX5BAL        XXDIF5           15 4
     C     XX6THB        SUB       XX6BAL        XXDIF6           15 4
     C     XX7THB        SUB       XX7BAL        XXDIF7           15 4
     C*
     C                   ENDIF                                                  XXST IFNE
     C*
     C                   Z-ADD     XX1STB        EL1AAM
     C                   Z-ADD     XX2NDB        EL2AAM
     C                   Z-ADD     XX3RDB        EL3AAM
     C                   Z-ADD     XX4THB        EL4AAM
     C                   Z-ADD     XX5THB        EL5AAM
     C                   Z-ADD     XX6THB        EL6AAM
     C                   Z-ADD     XX7THB        EL7AAM
     C     SVCNCL        IFEQ      'Y'
     C     F1CXAD        ANDEQ     'Y'
     C     SVCNCL        OREQ      ' '
     C     F1YRAD        ANDEQ     'Y'
     C                   Z-ADD     XXMFEE        ELMFAM
     C                   ELSE
     C                   Z-ADD     XXMFBT        ELMFAM
     C                   ENDIF
     C*
     C     SVCNCL        IFEQ      'Y'
     C     RRCXAD        ANDEQ     'Y'
     C     SVCNCL        OREQ      ' '
     C     RRYRAD        ANDEQ     'Y'
     C                   Z-ADD     XXSFEE        ELSFAM
     C                   ELSE
     C                   Z-ADD     XXSFBT        ELSFAM
     C                   ENDIF
      *
     C                   Z-ADD     XXGOVC        ELGOVC
     C                   Z-ADD     XXHXMN        ELHXMN

      /free
       If *in94 = *off and *in95 = *off and *in96 = *off and *in97 = *off;
          ELDvpr = CalcDividend(ELCo#: ELFnd : ELEmp# : ELDiv : ELFyr);
       Endif;
      /end-free
     C*
     C* Calculate the large deductible percentage in order to reach
     C* the indicated percent of manual premium.
     C*
     C*** begin chg ***  06/14/23  *************************************
     C******             If        (ELManP <> 0 and ELAMNP <> 'Y'
     C******                       and Q2Call <> 'Y') and
     C******                       (*in97 = *on)
     C                   If        ELManP <> 0 and *in97 = *on
     C*** end chg *****  06/14/23  *************************************
     C*
     C                   Z-ADD     ELPFDT        XXEFDT
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C* Establish target premium (manual premium x % / 100)
     C*
     C*** begin chg ***  06/14/23  *************************************
     C*****ELManP        Mult(h)   ELAmpr        xxTargPrem       11 0
     C     ELManP        Mult(h)   MPAmpr        xxTargPrem       11 0
     C*** end chg *****  06/14/23  *************************************
     C                   Div(h)    100           xxTargPrem
     C*
     C* Subtract out any line items which apply after large deduc discount
     C*
     C                   Sub       ELATer        xxTargPrem
     C                   Sub       ELADTr        xxTargPrem
     C                   Sub       ELPRTD        xxTargPrem
     C                   Sub       ELHXMN        xxTargPrem
     C*** begin chg ***  06/14/23  *************************************
     C******             Sub       ELAARC        xxTargPrem
     C******             Sub       ELRPA1        xxTargPrem
     C******             Sub       ELRPA2        xxTargPrem
     C******             Sub       ELRPA3        xxTargPrem
     C                   Sub       MPAARC        xxTargPrem
     C                   Sub       MPRPA1        xxTargPrem
     C                   Sub       MPRPA2        xxTargPrem
     C                   Sub       MPRPA3        xxTargPrem
     C*** end chg *****  06/14/23  *************************************
     C*** begin add ***  06/14/23  ****************************************
     C*
     C* Remove more potential amounts for MI as its target base is modified
     C* premium instead of discounted premium.
     C*
     C                   If        xxst = 'MI'
     C*
     C                   Add       MPADC3        xxTargPrem
     C                   Sub       MPALOS        xxTargPrem
     C*
     C                   If        RRASFL = 'D' or RRASFL = 'S'
     C                   Xfoot     ADL           xxwk10
     C                   Sub       xxwk10        xxTargPrem
     C                   Endif
     C*
     C                   If        ELWVFL = 'D' or ELWVFL = 'S'
     C                   Sub       MPAWAV        xxTargPrem
     C                   Endif
     C*
     C                   If        RRDV2L = 'D' or RRDV2L = 'S'
     C                   Add       MPADV2        xxTargPrem
     C                   Add       MPD2A2        xxTargPrem
     C                   Endif
     C*
     C                   If        RRDV1L = 'D' or RRDV1L = 'S'
     C                   Add       MPADC2        xxTargPrem
     C                   Add       MPD1A2        xxTargPrem
     C                   Endif
     C*
     C                   Endif                                                  If xxst = 'MI'
     C*** end add *****  06/14/23  ****************************************
     C*
     C* Subtract out any taxes/assessments which are indicated as included.
     C*
     C                   If        RRTaxM = 'Y'
     C                   Sub       ELTXDU        xxTargPrem
     C                   Endif
     C                   If        RR1stM = 'Y'
     C                   Sub       EL1aam        xxTargPrem
     C                   Endif
     C                   If        RR2ndM = 'Y'
     C                   Sub       EL2aam        xxTargPrem
     C                   Endif
     C                   If        RR3rdM = 'Y'
     C                   Sub       EL3aam        xxTargPrem
     C                   Endif
     C                   If        RR4thM = 'Y'
     C                   Sub       EL4aam        xxTargPrem
     C                   Endif
     C                   If        RR5thM = 'Y'
     C                   Sub       EL5aam        xxTargPrem
     C                   Endif
     C                   If        RR6thM = 'Y'
     C                   Sub       EL6aam        xxTargPrem
     C                   Endif
     C                   If        RR7thM = 'Y'
     C                   Sub       EL7aam        xxTargPrem
     C                   Endif
     C*
     C* Subtract target premium from premium before large deductible discount
     C* to arrive at estimated discount amount.
     C*
     C     xxTargBase    Sub       xxTargPrem    xxLrgDisc        11 0
     C*
     C* Calcualte large deductible percentage.
     C*
     C                   If        xxTargBase <> 0
     C     xxLrgDisc     Div(h)    xxTargBase    xxLrgDiscP       1411
     C                   Else
     C     LDPct1        Div       100           xxLrgDiscP
     C                   Endif
     C*** begin add ***  06/14/23  ****************************************
     C*
     C* Do not allow calculated percentage to be 100% or more, reduce to 99.9%
     C* for cases where the added amounts removed caused discount to go too high.
     C*
     C*** begin del ***  07/18/23  ***************************************
     C*****              If        xxLrgDiscP >= 1
     C*****              Z-add     .999          xxLrgDiscP
     C*****              Endif
     C*** end del *****  07/18/23  ***************************************
     C*** end add *****  06/14/23  ****************************************
     C*
     C* Update (or write if necessary) the large deductible file.
     C*
     C     elky4         Chain     PLD01                              60
     C                   If        *in60 = *off
     C     100           Mult(h)   xxLrgDiscP    LDPct1
     C                   Move      'D'           LDPlv1
     C*** begin add ***  06/14/23  ****************************************
     C                   If        xxst = 'MI'
     C                   Move      'E'           LDPlv1
     C                   Endif
     C*** end add *****  06/14/23  ****************************************
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1PGM         LDPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        LDTIME
     C                   Update    pld01
     C                   Else
     C                   Clear                   pld01
     C                   Z-add     XXCo#         LDCo#
     C                   Z-add     XXFnd         LDFnd
     C                   Z-add     XXFyr         LDFyr
     C                   Z-add     XXEmp#        LDEmp#
     C                   Z-add     XXDiv         LDDiv
     C     100           Mult(h)   xxLrgDiscP    LDPct1
     C                   Move      'D'           LDPlv1
     C*** begin add ***  06/14/23  ****************************************
     C                   If        xxst = 'MI'
     C                   Move      'E'           LDPlv1
     C                   Endif
     C*** end add *****  06/14/23  ****************************************
     C                   Move      'G'           LDTyp1
     C                   Move      'C'           LDVrb1
     C                   Move      'C'           LDOcv1
     C                   Move      'ME'          LDR101
     C                   Z-add     ELPfdt        LDEfd1
     C                   Eval      LDDes1 = 'Large Deductible #1'
     C                   MOVEL     Q1USER        LDUSER
     C                   MOVEL     Q1USER        LDIUSR
     C                   MOVEL     Q1PGM         LDPGM
     C                   MOVEL     Q1PGM         LDIPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         LDDT
     C                   MOVEL     SRDTO         LDIDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        LDTIME
     C                   Z-ADD     DSTIME        LDITIM
     C                   Write     pld01
     C                   Endif
     C*
     C                   Endif                                                  % of Manual Premium
      *
     C     *IN97         IFEQ      *OFF
      *
     C                   MOVEL     Q1USER        ELUSER
     C                   MOVEL     Q1PGM         ELPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   ADD       1             DSTIME
     C                   Z-ADD     DSTIME        ELTIME
     C                   Z-ADD     XXEARC        ELAARC
      *
     C                   UPDATE    PEL01
     C                   ENDIF

       // Process for Unbilled Premium File
      /free
       If finalUpdate = *on and unBilPrmInst = *on;
          Open WXELL;
          Select;
          When qqRev = '2';
             Delete (ELCo#: ELFnd : ELEmp# : ELDiv : ELFyr) pxel00;
             Write  pxel00;
          When qqRev = '3';
             Delete (ELCo#: ELFnd : ELEmp# : ELDiv : ELFyr) pxel00;
          Endsl;
          Close WXELL;
       Endif;
      /end-free
     C*
     C* CHAIN EMPLOYER DETAIL
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C* Subroutine found in copybook (WADTTRKCSR)
     C                   EXSR      ASGNBC
     C     ELBCTG        IFNE      *BLANKS
     C                   UPDATE    PEL01
     C                   ENDIF                                                  ELBCTG IFNE *BLANKS
     C*
     C* RECHAIN TO RATING/STATISTICAL FILE AFTER ASGNBC CALL.
     C*
     C     RSKY          CHAIN     PRS01                              60
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
      *
      *  WRITE/UPDATE PREMIUM PERIODS FILE
      *
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C     XX1STB        MULT(H)   PRO(X)        ASM1(X)
     C     XX2NDB        MULT(H)   PRO(X)        ASM2(X)
     C     XX3RDB        MULT(H)   PRO(X)        ASM3(X)
     C     XX4THB        MULT(H)   PRO(X)        ASM4(X)
     C     XX5THB        MULT(H)   PRO(X)        ASM5(X)
     C     XX6THB        MULT(H)   PRO(X)        ASM6(X)
     C     XX7THB        MULT(H)   PRO(X)        ASM7(X)
     C                   IF        XXST <> 'PA'
     C                   Z-ADD     XXART1        ART1(X)
     C                   Z-ADD     XXART2        ART2(X)
     C                   Z-ADD     XXART3        ART3(X)
     C                   Z-ADD     XXART4        ART4(X)
     C                   Z-ADD     XXART5        ART5(X)
     C                   Z-ADD     XXART6        ART6(X)
     C                   Z-ADD     XXART7        ART7(X)
     C                   ENDIF
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     ASM1          XXPREM           15 4
     C*
     C     XXPREM        IFNE      XX1STB
     C     XX1STB        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        ASM1(1)
     C                   ENDIF
     C*
     C                   XFOOT     ASM2          XXPREM           15 4
     C*
     C     XXPREM        IFNE      XX2NDB
     C     XX2NDB        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        ASM2(1)
     C                   ENDIF
     C*
     C                   XFOOT     ASM3          XXPREM           15 4
     C*
     C     XXPREM        IFNE      XX3RDB
     C     XX3RDB        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        ASM3(1)
     C                   ENDIF
     C*
     C                   XFOOT     ASM4          XXPREM           15 4
     C*
     C     XXPREM        IFNE      XX4THB
     C     XX4THB        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        ASM4(1)
     C                   ENDIF
     C*
     C                   XFOOT     ASM5          XXPREM           15 4
     C*
     C     XXPREM        IFNE      XX5THB
     C     XX5THB        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        ASM5(1)
     C                   ENDIF
     C*
     C                   XFOOT     ASM6          XXPREM           15 4
     C*
     C     XXPREM        IFNE      XX6THB
     C     XX6THB        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        ASM6(1)
     C                   ENDIF
     C*
     C                   XFOOT     ASM7          XXPREM           15 4
     C*
     C     XXPREM        IFNE      XX7THB
     C     XX7THB        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        ASM7(1)
     C                   ENDIF
     C*
     C     *IN94         IFEQ      *OFF
     C     *IN95         ANDEQ     *OFF
     C     *IN96         ANDEQ     *OFF
     C*
     C                   XFOOT(H)  NOM           XXWK10
     C                   Z-ADD     XXWK10        SVNORM           11 2
     C*
     C* COMPLETE ROUNDING ON PREMIUM PERIOD AMOUNTS.
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C*
     C                   Z-ADD(H)  MAN(X)        XXWK10
     C                   Z-ADD     XXWK10        MAN(X)
     C                   Z-ADD(H)  SUB(X)        XXWK10
     C                   Z-ADD     XXWK10        SUB(X)
     C                   Z-ADD(H)  MOD(X)        XXWK10
     C                   Z-ADD     XXWK10        MOD(X)
     C                   Z-ADD(H)  STD(X)        XXWK10
     C                   Z-ADD     XXWK10        STD(X)
     C                   Z-ADD(H)  NOM(X)        XXWK10
     C                   Z-ADD     XXWK10        NOM(X)
     C                   Z-ADD(H)  ETR(X)        XXWK10
     C                   Z-ADD     XXWK10        ETR(X)
     C     ANL(X)        ADD(H)    ETR(X)        XXWK10
     C                   Z-ADD(H)  NTR(X)        XXWK10
     C                   Z-ADD     XXWK10        NTR(X)
     C     ANL(X)        ADD(H)    NTR(X)        XXWK10
     C                   Z-ADD     XXWK10        ANL(X)
     C                   Z-ADD(H)  DTR(X)        XXWK10
     C                   Z-ADD     XXWK10        DTR(X)
     C     ANL(X)        ADD(H)    DTR(X)        XXWK10
     C                   Z-ADD     XXWK10        ANL(X)
     C                   Z-ADD(H)  EHCN(X)       XXWK10
     C                   Z-ADD     XXWK10        EHCN(X)
     C                   Z-ADD(H)  UPSN(X)       XXWK10
     C                   Z-ADD     XXWK10        UPSN(X)
     C                   Z-ADD(H)  EDC1(X)       XXWK10
     C                   Z-ADD     XXWK10        EDC1(X)
     C                   Z-ADD(H)  EDC2(X)       XXWK10
     C                   Z-ADD     XXWK10        EDC2(X)
     C                   Z-ADD(H)  EDV2(X)       XXWK10
     C                   Z-ADD     XXWK10        EDV2(X)
     C                   Z-ADD     D1E2(X)       XXWK10
     C                   Z-ADD     XXWK10        D1E2(X)
     C                   Z-ADD     D2E2(X)       XXWK10
     C                   Z-ADD     XXWK10        D2E2(X)
     C                   Z-ADD(H)  EDC3(X)       XXWK10
     C                   Z-ADD     XXWK10        EDC3(X)
     C                   Z-ADD(H)  LM(X)         XXWK10
     C                   Z-ADD     XXWK10        LM(X)
     C                   Z-ADD(H)  LM2(X)        XXWK10
     C                   Z-ADD     XXWK10        LM2(X)
     C                   Z-ADD(H)  EDCT(X)       XXWK10
     C                   Z-ADD     XXWK10        EDCT(X)
     C                   Z-ADD(H)  EDD2(X)       XXWK10
     C                   Z-ADD     XXWK10        EDD2(X)
     C                   Z-ADD(H)  EDCM(X)       XXWK10
     C                   Z-ADD     XXWK10        EDCM(X)
     C                   Z-ADD(H)  EDCS(X)       XXWK10
     C                   Z-ADD     XXWK10        EDCS(X)
     C     EDC(X)        MULT(H)   -1            XXWK10
     C                   Z-ADD     XXWK10        EDC(X)
     C                   Z-ADD(H)  WAV(X)        XXWK10
     C                   Z-ADD     XXWK10        WAV(X)
     C     MOD(X)        SUB(H)    EPRM(X)       XXWK10
     C                   Z-ADD     XXWK10        EPRM(X)
     C                   Z-ADD(H)  EXA1(X)       XXWK10
     C                   Z-ADD     XXWK10        EXA1(X)
     C                   Z-ADD(H)  EXA2(X)       XXWK10
     C                   Z-ADD     XXWK10        EXA2(X)
     C                   Z-ADD(H)  EXA3(X)       XXWK10
     C                   Z-ADD     XXWK10        EXA3(X)
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   XFOOT     MAN           XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELAMPR
     C     ELAMPR        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        MAN(1)
     C                   ENDIF
     C*
     C                   XFOOT     SUB           XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELASBJ
     C     ELASBJ        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        SUB(1)
     C                   ENDIF
     C*
     C                   XFOOT     MOD           XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELAMPM
     C     ELAMPM        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        MOD(1)
     C                   ENDIF
     C*
     C                   XFOOT     STD           XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELASPR
     C     ELASPR        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        STD(1)
     C                   ENDIF
     C*
     C                   XFOOT     NOM           XXPREM           15 4
     C*
     C     XXPREM        IFNE      SVNORM
     C     SVNORM        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        NOM(1)
     C                   ENDIF
     C*
     C                   XFOOT     ANL           XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELAAPR
     C     ELAAPR        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        ANL(1)
     C                   ENDIF
     C*
     C                   XFOOT     EHCN          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELAHCN
     C     ELAHCN        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EHCN(1)
     C                   ENDIF
     C                   XFOOT     UPSN          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELUPSA
     C     ELUPSA        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        UPSN(1)
     C                   ENDIF
     C*
     C                   XFOOT     EDC1          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELADC1
     C     ELADC1        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EDC1(1)
     C                   ENDIF
     C*
     C                   XFOOT     EDC2          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELADC2
     C     ELADC2        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EDC2(1)
     C                   ENDIF
     C*
     C                   XFOOT     D1E2          XXPREM
     C     XXPREM        IFNE      ELD1A2
     C     ELD1A2        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        D1E2(1)
     C                   ENDIF
     C                   XFOOT     EDV2          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELADV2
     C     ELADV2        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EDV2(1)
     C                   ENDIF
     C                   XFOOT     D2E2          XXPREM
     C     XXPREM        IFNE      ELD2A2
     C     ELD2A2        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        D2E2(1)
     C                   ENDIF
     C                   XFOOT     EDC3          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELADC3
     C     ELADC3        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EDC3(1)
     C                   ENDIF
     C*
     C                   XFOOT     LM            XXPREM           15 4
      //*** Beg Add ***** 08/14/24 *******************************************
       xxPrem = xxPrem + %Xfoot(LMB);
      //*** Beg Add ***** 08/14/24 *******************************************
     C*
     C     XXPREM        IFNE      ELAIN1
     C     ELAIN1        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        LM(1)
     C                   ENDIF
     C*
     C                   XFOOT     LM2           XXPREM           15 4
      //*** Beg Add ***** 08/14/24 *******************************************
       xxPrem = xxPrem + %Xfoot(LMB2);
      //*** Beg Add ***** 08/14/24 *******************************************
     C*
     C     XXPREM        IFNE      ELAIN2
     C     ELAIN2        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        LM2(1)
     C                   ENDIF
     C*
     C                   XFOOT     EDCT          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELADCT
     C     ELADCT        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EDCT(1)
     C                   ENDIF
     C*
     C                   XFOOT     EDD2          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELADD2
     C     ELADD2        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EDD2(1)
     C                   ENDIF
     C*
     C                   XFOOT     EDC           XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELADC
     C     ELADC         SUB       XXPREM        XXPREM
     C                   SUB       XXPREM        EDC(1)
     C                   ENDIF
     C*
     C                   XFOOT     WAV           XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELAWAV
     C     ELAWAV        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        WAV(1)
     C                   ENDIF
     C*
     C                   XFOOT     EPRM          XXPREM           15 4
     C*
     C     XXPREM        IFNE      MPAXPA
     C     MPAXPA        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EPRM(1)
     C                   ENDIF
     C*
     C                   XFOOT     EXA1          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELAXP1
     C     ELAXP1        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EXA1(1)
     C                   ENDIF
     C*
     C                   XFOOT     EXA2          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELAXP2
     C     ELAXP2        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EXA2(1)
     C                   ENDIF
     C*
     C                   XFOOT     EXA3          XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELAXP3
     C     ELAXP3        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        EXA3(1)
     C                   ENDIF
     C*
      *++++ WARNING ++++ BEGIN SECTION NOT MATCHING 114 CLIENT CODE.
      *
     C                   XFOOT     ETR           XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELATER
     C     ELATER        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        ETR(1)
     C                   ENDIF
     C*
     C                   XFOOT     DTR           XXPREM           15 4
     C*
     C     XXPREM        IFNE      ELADTR
     C     ELADTR        SUB       XXPREM        XXPREM
     C                   ADD       XXPREM        DTR(1)
     C                   ENDIF
     C*
      *++++ WARNING ++++ END   SECTION NOT MATCHING 114 CLIENT CODE.
     C     EPKEY         CHAIN     PPP01                              60
     C                   Dow       *in60 = *off
     C                   Clear                   xxSkip            1
     C                   Z-add     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C                   If        PDAT(X) = PPFRDT
     C                   Eval      xxSkip = 'Y'
     C                   Leave
     C                   Endif
     C                   Add       1             x
     C                   Enddo
     C                   If        xxSkip = ' '
     C                   Delete    ppp01
     C                   Endif
     C     EPKEY         READE     PPP01                                  60
     C                   Enddo
     C*
     C* POPULATE THE PREMIUM PERIODS FILE
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C*
     C     X             ADD       1             Z                 3 0
     C*
     C                   Z-ADD     PDAT(X)       XXFRDT
     C*
     C                   CLEAR                   PPP01
     C*
     C     PPKY          CHAIN     PPP01                              60
     C*
     C                   Z-ADD     XXCO#         PPCO#
     C                   Z-ADD     XXFND         PPFND
     C                   Z-ADD     XXEMP#        PPEMP#
     C                   Z-ADD     XXDIV         PPDIV
     C                   Z-ADD     XXFYR         PPFYR
     C                   Z-ADD     PDAT(X)       PPFRDT
     C                   Z-ADD     PDAT(Z)       PPTODT
     C                   MOVE      XXST          PPST
     C                   Z-ADD     PRO(X)        PPPROR
     C                   Z-ADD     MANSV(X)      PPAMPR
     C                   Z-ADD     SUB(X)        PPASBJ
     C                   Z-ADD     MOD(X)        PPAMPM
     C                   Z-ADD     STD(X)        PPASPR
     C                   Z-ADD     NOM(X)        PPANMP
     C                   Z-ADD     ANL(X)        PPAAPR
     C                   Z-ADD     EHCN(X)       PPAHCN
     C                   Z-ADD     UPSN(X)       PPAUSA
     C                   Z-ADD     EDC1(X)       PPADC1
     C                   Z-ADD     EDC2(X)       PPADC2
     C                   Z-ADD     EDV2(X)       PPADV2
     C                   Z-ADD     D1E2(X)       PPD1A2
     C                   Z-ADD     D2E2(X)       PPD2A2
     C                   Z-ADD     EDC3(X)       PPADC3
     C                   Z-ADD     LM(X)         PPAIN1
     C                   Z-ADD     LM2(X)        PPAIN2
     C                   Z-ADD     EDCT(X)       PPADCT
     C                   Z-ADD     EDD2(X)       PPADD2
     C                   Z-ADD     EDCM(X)       PPADCM
     C                   Z-ADD     EDCS(X)       PPADCS
     C                   Z-ADD     EDC(X)        PPADC
     C                   Z-ADD     WAV(X)        PPAWAV
     C                   Z-ADD     EPRM(X)       PPAXPA
     C                   Z-ADD     EXA1(X)       PPAXP1
     C                   Z-ADD     EXA2(X)       PPAXP2
     C                   Z-ADD     EXA3(X)       PPAXP3
     C                   Z-ADD     ETR(X)        PPATER
     C                   Z-ADD     DTR(X)        PPADTR
     C                   Z-ADD     ASM1(X)       PP1AAM
     C                   Z-ADD     ASM2(X)       PP2AAM
     C                   Z-ADD     ASM3(X)       PP3AAM
     C                   Z-ADD     ASM4(X)       PP4AAM
     C                   Z-ADD     ASM5(X)       PP5AAM
     C                   Z-ADD     ASM6(X)       PP6AAM
     C                   Z-ADD     ASM7(X)       PP7AAM

      /free
       // Get Terrorism Rates
       Select;
       When hasDiffLCMs;
          If pay(x) <> 0;
             eval(h) PPterr = etr(x)/(pay(x) * .01);
             eval(h) PPdtrr = dtr(x)/(pay(x) * .01);
          Endif;
       Other;
          PPterr = pyTrt(x);
          PPdtrr = dmTrt(x);
       Endsl;
      /end-free
     C                   Z-ADD     ART1(X)       PPA1RT
     C                   Z-ADD     ART2(X)       PPA2RT
     C                   Z-ADD     ART3(X)       PPA3RT
     C                   Z-ADD     ART4(X)       PPA4RT
     C                   Z-ADD     ART5(X)       PPA5RT
     C                   Z-ADD     ART6(X)       PPA6RT
     C                   Z-ADD     ART7(X)       PPA7RT
     C                   Z-ADD     ARP1(X)       PPRPA1
     C                   Z-ADD     ARP2(X)       PPRPA2
     C                   Z-ADD     ARP3(X)       PPRPA3
     C*
     C                   MOVEL     Q1USER        PPUSER
     C                   MOVEL     Q1PGM         PPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPDT
     C                   Z-ADD     DSTIME        PPTIME
     C*
     C     *IN60         IFEQ      '0'
     C                   UPDATE    PPP01
     C                   ELSE
     C                   MOVEL     Q1USER        PPIUSR
     C                   MOVEL     Q1PGM         PPIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PPIDT
     C                   Z-ADD     DSTIME        PPITIM
     C                   WRITE     PPP01
     C                   ENDIF
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C*  CALCULATE THE NET RATE AND NET PREMIUM FOR EACH PAYROLL
     C*  CLASS BY STATE AND EFFECTIVE DATE FOR SPLIT MOD POLICIES
     C*
     C     F1NETR        IFEQ      'Y'
     C     *IN97         ANDEQ     *OFF
     C     F1SPLA        ANDEQ     'Y'
     C     ELPFDT        ANDGE     F1NETD
     C     ELAMNP        ANDNE     'Y'
     C*
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     *ZERO
     C*
     C                   Z-ADD     0             XXTMAN           11 2
     C                   Z-ADD     0             XXANMP           11 2
     C                   Z-ADD     PDAT(X)       XXFRDT
     C*
     C* TOTAL THE MANUAL AND NORMAL PREMIUM FOR THE STATE.
     C*
     C     DSKY          CHAIN(N)  PPP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     PPST          IFEQ      XXST
     C     PPFRDT        ANDEQ     XXFRDT
     C                   ADD       PPAMPR        XXTMAN
     C                   ADD       PPANMP        XXANMP
     C                   ENDIF
     C*
     C     DSKY          READE(N)  PPP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* CALCULATE THE NET RATE FACTOR.
     C*
     C     XXTMAN        IFNE      *ZERO
     C     XXANMP        DIV(H)    XXTMAN        XXNETR            9 6
     C                   ELSE
     C                   Z-ADD     1             XXNETR
     C                   ENDIF                                                  XXTMAN IFNE
     C*
     C* APPLY THE NET RATE FACTOR AGAINST ALL ACTIVE PAYROLLS.
     C*
     C     EPKEY         CHAIN     PEP01                              60
     C     *IN60         DOWEQ     *OFF
     C*
     C     EPSTAT        IFNE      'I'
     C     EPEFDT        ANDEQ     XXFRDT
     C*
     C     XXNETR        MULT(H)   EPARAT        EPANTR
     C                   SELECT
     C     ELAMNP        WHENEQ    'Y'
     C                   Z-ADD     EPAMPR        EPANTP
     C                   ADD       EPANTP        XXTNET
     C                   OTHER
     C     XXNETR        MULT(H)   EPAMPR        XXWK10
     C                   Z-ADD     XXWK10        EPANTP
     C                   ADD       XXWK10        XXTNET
     C                   ENDSL
     C*
     C                   UPDATE    PEP01
     C*
     C                   ENDIF                                                  EPSTAT IFNE
     C*
     C     EPKEY         READE     PEP01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ADD       1             X
     C                   ENDDO
     C*
     C                   ENDIF                                                  F1NETR IFEQ
     C*
     C                   ENDIF                                                  *IN94 IFEQ
     C*
     C*  WRITE AN EMPLOYER DETAIL HISTORY RECORD FOR THE AUDIT
     C*  BILL.
     C*
     C                   CLEAR                   PEH01
     C                   MOVE      ELCO#         EHCO#
     C                   MOVE      ELFND         EHFND
     C                   MOVE      ELFYR         EHFYR
     C                   MOVE      ELEMP#        EHEMP#
     C                   MOVE      ELDIV         EHDIV
     C                   MOVE      XXDATE        EHEFDT
     C                   MOVE      ELFAF         EHFAF
     C                   MOVE      ELBSCD        EHBSCD
     C                   MOVE      ELPFDT        EHPFDT
     C                   MOVE      ELPTDT        EHPTDT
     C                   MOVE      ELANDT        EHANDT
     C                   MOVE      ELPROR        EHPROR
     C                   MOVE      ELAMPR        EHWMPR
     C                   MOVE      ELSRAM        EHWSRA
     C                   MOVE      ELASPR        EHWSPR
     C                   MOVE      ELANMP        EHWNMP
     C                   MOVE      ELAAPR        EHWAPR
     C*** begin add ***  11/07/23  **************************************
     C* Actual figures from WDMPP
     C                   MOVE      MPAMPR        EHWAMAN
     C                   MOVE      MPASPR        EHWASTD
     C                   MOVE      MPANMP        EHWANORM
     C                   MOVE      MPAAPR        EHWAANN
     C* Balance to minimum
     C                   If        ELAmnp = 'Y'
     C                   Eval      EHWBal = xxHigh - xxwkcm
     C                   Else
     C                   Eval      EHWBal = 0
     C                   Endif
     C*** end add *****  11/07/23  **************************************
     C*** Beg Add *****  02/27/24  **************************************
       EHWASBJ = MPASBJ;
       EHWAMOD = MPAMPM;
       //*** Beg Chg ***** 08/14/24 *************************************
       //***EHWAIL1 = ELAIN1;
       //***EHWAIL2 = ELAIN2;
       EHWAIL1 = MPAIN1;
       EHWAIL2 = MPAIN2;
       EHWBIL  = MPABIL;
       EHWBIL2 = MPABIL2;
       //*** End Chg ***** 08/14/24 *************************************
       EHWADC1 = MPADC1;
       EHWADC2 = MPADC2;
       EHWADC3 = MPADC3;
       EHWADV2 = MPADV2;
       EHWADCT = MPADCT;
       EHWADD2 = MPADD2;
       EHWADCM = MPADCM;
       EHWADCS = MPADCS;
       EHWADC = MPADC;
       EHWAWAV = MPAWAV;
       EHWATCA = MPATCA;
       EHWAPRD = MPPRTD;
       EHWAPRF = 0;
       EHWAXPA = MPAXPA;
       EHWAVLC = MPAVLC;
       EHWAARC = MPAARC;
       EHWAD12 = MPD1A2;
       EHWAD22 = MPD2A2;
       EHWAHCN = MPAHCN;
       EHWAUSA = MPAUSA;
       EHWAXP1 = MPAXP1;
       EHWAXP2 = MPAXP2;
       EHWAXP3 = MPAXP3;
       EHWARP1 = MPRPA1;
       EHWARP2 = MPRPA2;
       EHWARP3 = MPRPA3;
       EHWALOS = MPALOS;
     C*** End Add *****  02/27/24  **************************************
     C                   MOVE      ELAMNP        EHWMNP
     C                   MOVE      ELPRTD        EHPRTD
     C                   MOVE      ELPRTF        EHPRTF
     C                   MOVE      ELWVFL        EHWVFL
     C                   MOVE      ELASP1        EHASP1
     C                   MOVE      ELCREF        EHCREF
     C                   MOVE      ELCREX        EHCREX
     C                   MOVE      ELADC1        EHWDC1
     C                   MOVE      ELASP2        EHASP2
     C                   MOVE      ELHCN         EHHCN
     C                   MOVE      ELAHCN        EHWHCN
     C                   MOVE      ELUPSA        EHWUSA
     C                   MOVE      ELD1S2        EHD1S2
     C                   MOVE      ELD1D2        EHD1D2
     C                   MOVE      ELD1X2        EHD1X2
     C                   MOVE      ELD1A2        EHD1W2
     C                   MOVE      ELD2S2        EHD2S2
     C                   MOVE      ELD2D2        EHD2D2
     C                   MOVE      ELD2X2        EHD2X2
     C                   MOVE      ELD2A2        EHD2W2
     C                   MOVE      ELDVEF        EHDVEF
     C                   MOVE      ELDVEX        EHDVEX
     C                   MOVE      ELADC2        EHWDC2
     C                   MOVE      ELASP3        EHASP3
     C                   MOVE      ELDISC        EHDISC
     C                   MOVE      ELADC3        EHWDC3
     C                   MOVE      ELEXM1        EHEXM1
     C                   MOVE      ELEMD1        EHEMD1
     C                   MOVE      ELAXP1        EHWXP1
     C                   MOVE      ELEXM2        EHEXM2
     C                   MOVE      ELEMD2        EHEMD2
     C                   MOVE      ELAXP2        EHWXP2
     C                   MOVE      ELEXM3        EHEXM3
     C                   MOVE      ELEMD3        EHEMD3
     C                   MOVE      ELAXP3        EHWXP3
     C                   Move      ELARP1        EHARP1
     C                   Move      ELARP2        EHARP2
     C                   Move      ELARP3        EHARP3
     C                   MOVE      ELILCD        EHILCD
     C                   MOVE      ELILEF        EHILEF
     C                   MOVE      ELILEX        EHILEX
     C                   MOVE      ELAIN1        EHWIN1
     C                   MOVE      ELINL2        EHI2CD
     C                   MOVE      ELI2EF        EHI2EF
     C                   MOVE      ELI2EX        EHI2EX
     C                   MOVE      ELAIN2        EHWIN2
     C                   MOVE      ELDCDE        EHDCDE
     C                   MOVE      ELDDCT        EHDDCT
     C     XXLRGD        IFEQ      'Y'
     C                   Z-ADD     LDPCT1        EHPCT1
     C                   Z-ADD     LDPCT2        EHPCT2
     C                   ENDIF
     C                   MOVE      ELCXBL        EHCXBL
     C                   MOVE      ELDCEF        EHDCEF
     C                   MOVE      ELDCEX        EHDCEX
     C                   MOVE      ELADCT        EHWDCT
     C                   MOVE      ELDCD2        EHDCD2
     C                   MOVE      ELDD2P        EHDD2P
     C                   MOVE      EL2DEF        EH2DEF
     C                   MOVE      EL2DEX        EH2DEX
     C                   MOVE      ELADD2        EHWDD2
     C                   MOVE      ELASPM        EHASPM
     C                   MOVE      ELMCEF        EHMCEF
     C                   MOVE      ELMCEX        EHMCEX
     C                   MOVE      ELADCM        EHWDCM
     C                   MOVE      ELASPS        EHASPS
     C                   MOVE      ELSCEF        EHSCEF
     C                   MOVE      ELSCEX        EHSCEX
     C                   MOVE      ELADCS        EHWDCS
     C                   MOVE      ELRTCD        EHRTCD
     C                   MOVE      ELSTAT        EHSTAT
     C                   MOVE      ELUSER        EHUSER
     C                   MOVE      ELPGM         EHPGM
     C                   MOVE      ELTIME        EHTIME
     C                   MOVE      ELDT          EHDT
     C                   MOVE      EHUSER        EHIUSR
     C                   MOVE      EHDT          EHIDT
     c                   Select
     c                   When      cltid = '025'
     C                   MOVE      ELSTFC        EHSTFC
     c                   When      cltid = '136'
     C                   Eval      EHdev2F   = ELdev2F
     c                   Other
     C                   MOVE      ELDEV2        EHDEV2
     c                   EndSl
     C                   Eval      EHdev2WBP = ELdev2BP
     C                   Eval      EHdev2WAP = ELdev2AP
     C                   Eval      EHtdf     = ELtdf
     C                   Eval      EHwpf = ELapf
     C                   MOVE      ELADV2        EHWDV2
     C                   MOVE      ELD2EF        EHD2EF
     C                   MOVE      ELD2EX        EHD2EX
     C                   MOVE      ELDBCR        EHDBCR
     C                   MOVE      ELCMUL        EHCMUL
     C                   MOVE      ELADCR        EHWDCR
     C                   MOVE      ELADC         EHWDC
     C                   MOVE      ELASBJ        EHWSBJ
     C                   MOVE      ELAMPM        EHWMPM
     C                   MOVE      ELAFRS        EHWFRS
     C                   MOVE      ELTXPR        EHTXPR
     C                   MOVE      ELAVCC        EHAVCC
     C                   MOVE      ELSDEX        EHSDEX
     C                   MOVE      ELSDEF        EHSDEF
     C                   MOVE      ELLFEF        EHLFEF
     C                   Z-ADD     ELAWAV        EHWWAV
     C                   Z-ADD     XXADLD        EHWADL
     C                   MOVE      ELNWRW        EHNWRW
     C                   MOVE      ELLOSS        EHWLOS
     C                   Z-ADD     ELATER        EHWTER
     C                   Z-ADD     ELADTR        EHWDTR
     C                   Z-ADD     ELTERR        EHWTRR
     C                   Z-ADD     ELDTRR        EHWDRR
     C                   Z-ADD     XXVLCA        EHWVLC
     C                   Z-ADD     XXHXMN        EHWHXM
     C                   Z-ADD     ELRPA1        EHWRP1
     C                   Z-ADD     ELRPA2        EHWRP2
     C                   Z-ADD     ELRPA3        EHWRP3
     C                   Z-ADD     ELARCA        EHARCA
     C                   Z-ADD     ELARCP        EHARCP
     C                   Z-ADD     ELAREF        EHAREF
     C                   Z-ADD     ELAREX        EHAREX
     C                   Z-ADD     ELAARC        EHWARC
     C                   Z-ADD     EL1AAM        EHW1AS
     C                   Z-ADD     EL2AAM        EHW2AS
     C                   Z-ADD     EL3AAM        EHW3AS
     C                   Z-ADD     EL4AAM        EHW4AS
     C                   Z-ADD     EL5AAM        EHW5AS
     C                   Z-ADD     EL6AAM        EHW6AS
     C                   Z-ADD     EL7AAM        EHW7AS
     C                   Z-ADD     ELSFAM        EHWSF
     C                   Z-ADD     ELMFAM        EHWMF
      /Free
        EHexMd1Nf =  ELexMd1Nf;
        EHexMd2Nf =  ELexMd2Nf;
        EHexMd3Nf =  ELexMd3Nf;
      /End-Free
      /Free
       EHwanc = ELanca;
      /End-free
      /Free
        EHnoexm1 =  ELnoexm1;
        //*** Begin Add *** 07/30/23 ***************************************
        EHnoexm2 =  ELnoexm2;
        EHnoexm3 =  ELnoexm3;
        //*** End   Add *** 07/30/23 ***************************************
      /End-free
     C*
     C     QQAR          IFNE      *BLANKS
     C                   WRITE     PEH01
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C*
     C                   if        ELSTAM <> *zero
     C                              and (BSLPGM = 'P' or BSLPGM = 'I')
     C*
     C                   MOVE      'X'           EHFAF
     C                   Z-ADD     ELSTAM        EHWRPR
     C*
     C     ELKY4         SETGT     PEQ01
     C     ELKY4         READPE    PEQ01                                  60
     C     *IN60         IFEQ      *OFF
     C     EQWAPR        SUB       EHWAPR        EHXRPR
     C                   ENDIF
     C*
     C                   WRITE     PEH01
     C*
     C                   ENDIF                                                  BSLPGM IFNE
     C                   ENDIF                                                  QQAR = BLANK
     C*
     C                   END                                                    QQAR = BLANK
     C*
     C*** WRITE AUDITED AMOUNT ADJUSTMENT TO A/R
     C*
     C     F1UPAR        IFEQ      'Y'                                          RLS 2  JJJ
     C     QQAR          ANDNE     *BLANKS
     C*                                                    JJJ 9-7-89
     C*  GET FUTURE BILLING RECORDS & OFFSET THEM WITH A NEGATIVE AMT
     C*  EQUAL TO BILLING AMOUNT.  ACCUMULATE ALL OFFSETTING AMOUNTS
     C*  THEN ADD THE TOTAL TO ORIGINAL DIFFERENCE (XXDIFF).
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     0             XXTOFF           15 4
     C                   Z-ADD     0             XXOFFS           15 4
     C                   Z-ADD     0             X1TOFF           15 4
     C                   Z-ADD     0             X1OFFS           15 4
     C                   clear                   prem
     C                   clear                   taxes
     C                   clear                   memfees
     C                   clear                   saffees
     C                   clear                   asst1
     C                   clear                   asst2
     C                   clear                   asst3
     C                   clear                   asst4
     C                   clear                   asst5
     C                   clear                   asst6
     C                   clear                   asst7
     C                   clear                   expConst
     C                   clear                   instFees
       //*** Begin Add *** 03/05/25 ****************************************
       //*
       //* 1) Remove WMXAP records
       //* 2) Remove the invoice from Invoice Cloud
       RemoveWMXAP(xxCo#:xxFnd:xxEmp#:xxDiv);
       //*** End   Add *** 03/05/25 ****************************************
     C     ELARSM        IFNE      'Y'
     C     EMSTRP        OREQ      'Y'
     C     EAKY          SETLL     PEA02
     C     EAKY1         READE     PEA02                                  60
     C  N60              Z-ADD     EAPODT        SVPODT
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C*                                                    |
     C*                                                    |
     C****Billed
     C                   if        eaarty = 'B' or eaarty = 'A' or eaarty = 'D'
     C                   if        ttctyp <> 'E'  and ttctyp <> 'C'
     C                             and EAArcd <> F1DVBL
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxtoff
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt:prem)
      *
     C                   select
     c*
     C                   when      x > 0
     C                   eval      prem(x).amt# = prem(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0 :prem)
     C                   eval      prem(x).amt# = eaamt
     C                   eval      prem(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  ttctyp <> 'C'

     C                   if        ttctyp = 'E'

     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdife
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt:expConst)

     C                   select
     C                   when      x > 0
     C                   eval      expConst(x).amt# = expConst(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0 :expConst)
     C                   eval      expConst(x).amt# = eaamt
     C                   eval      expConst(x).podt = eapodt
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt

     C                   endif                                                  ttctyp = 'E'

     C                   endif                                                  eaarty = 'B'
     C****TAX
     C                   if        eaarty = 'F' and ttctyp = 'T'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         x1toff
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt :taxes)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      taxes(x).amt# = taxes(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0 :taxes)
     C                   eval      taxes(x).amt# = eaamt
     C                   eval      taxes(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                eatrdt >=sysdt
     C                   endif                                                  eaarty IFEQ
     C***SAFETY FEES
     C*
     C                   if        eaarty = 'F' and ttctyp = 'S'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdifs
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt :saffees)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      saffees(x).amt# = saffees(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0 :saffees)
     C                   eval      saffees(x).amt# = eaamt
     C                   eval      saffees(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarty > 'F'
     C**MEMBERSHIP FEES
     C*
     C                   if        eaarty = 'F' and ttctyp = 'M'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdifm
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt :memfees)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      memfees(x).amt# = memfees(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0 :memfees)
     C                   eval      memfees(x).amt# = eaamt
     C                   eval      memfees(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarty = 'F'
     C*
     C* 1ST ASSESSMENT FEES
     C*
     C                   if        eaarty = 'F' and ttctyp = '1'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdif1
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt :asst1)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      asst1(x).amt# = asst1(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0 :asst1)
     C                   eval      asst1(x).amt# = eaamt
     C                   eval      asst1(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarty ='F'
     C* 2ND ASSESSMENT FEES
     C*
     C                   if        eaarty = 'F' and ttctyp = '2'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdif2
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt :asst2)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      asst2(x).amt# = asst2(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0:asst2)
     C                   eval      asst2(x).amt# = eaamt
     C                   eval      asst2(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarty ='F'
     C* 3RD ASSESSMENT FEES
     C*
     C                   if        eaarty = 'F' and ttctyp = '3'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdif3
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt:asst3)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      asst3(x).amt# = asst3(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0:asst3)
     C                   eval      asst3(x).amt# = eaamt
     C                   eval      asst3(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarty ='F'
     C* 4TH ASSESSMENT FEES
     C*
     C                   if        eaarty = 'F' and ttctyp = '4'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdif4
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt:asst4)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      asst4(x).amt# = asst4(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0:asst4)
     C                   eval      asst4(x).amt# = eaamt
     C                   eval      asst4(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarty ='F'
     C* 5TH ASSESSMENT FEES
     C*
     C                   if        eaarty = 'F' and ttctyp = '5'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdif5
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt:asst5)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      asst5(x).amt# = asst5(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0:asst5)
     C                   eval      asst5(x).amt# = eaamt
     C                   eval      asst5(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarty ='F'
     C* 6TH ASSESSMENT FEES
     C*
     C                   if        eaarty = 'F' and ttctyp = '6'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdif6
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt:asst6)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      asst6(x).amt# = asst6(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0:asst6)
     C                   eval      asst6(x).amt# = eaamt
     C                   eval      asst6(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarty ='F'
     C* 7TH ASSESSMENT FEES
     C*
     C                   if        eaarty = 'F' and ttctyp = '7'
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   add       eaamt         xxdif7
     C                   delete    pea02
     C                   else
     C                   If        EAPgm = 'W7720R'
     C                   eval      x = SrchIdx(eapodt:asst7)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      asst7(x).amt# = asst7(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0:asst7)
     C                   eval      asst7(x).amt# = eaamt
     C                   eval      asst7(x).podt = eapodt
     c*
     C                   endsl
     C                   Endif                                                  eapgm = 'W7720R'
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarty ='F'
     C* INSTALLMENT FEES
     C                   if        eaarcd = ififbl
     C*
     C                   If        EAPodt >= xxDate
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   delete    pea02
     C                   else
     C                   eval      x = SrchIdx(eapodt:instfees)
     C                   select
     c*
     C                   when      x > 0
     C                   eval      instfees(x).amt# = instfees(x).amt# + eaamt
     C                   when      x = 0
     C                   eval      x = SrchIdx(0:instfees)
     C                   eval      instfees(x).amt# = eaamt
     C                   eval      instfees(x).podt = eapodt
     c*
     C                   endsl
     C                   endif                                                  eatrdt >= sysdt
     C                   endif                                                  eaarcd = ififbl
     C                   ENDIF                                                  *IN60 IFEQ
     C*                                                    |
     C     EAKY1         READE     PEA02                                  60    |
     C                   END                                                    *IN60 DOW='0'
     C                   ENDIF                                                  ELARSM IFNE
     C*                                                    |
     C*                                                    |
     C                   Z-ADD     XXDIFF        SVDIFF           11 2
     C                   ADD       XXTOFF        XXDIFF                         |
      /free
       Chain (xxCo#:xxFnd:xxFyr:xxEmp#:xxDiv) pdv00;
       If %found and cltId = '111';
          xxDiff = xxDiff - DVamt;
       Endif;
      /end-free
     C                   ADD       X1TOFF        XXDIFX                         |
     C*
      *
      *  IF TOTAL AUDIT ADJUSTMENT IS A CREDIT THAN POST TO THE
      *  CURRENT DATE, IF DEBIT USE INTERIM # OF DAYS TO DETERMINE
      *  POST DATE
      *
     C*
     C* IF AUDIT POST DATE IS PRIOR TO START OF POLICY, DEFAULT IT.
     C*
     C*
     C* POST ALL ADJUSTMENTS WITH THE POSTING DATE OF THE MAIN
     C* AUDIT ADJUSTMENT.
     C*
     C*
     C*
     C*** WRITE EXPENSE CONSTANT ADJUSTMENT TO A/R         |  7-8-88
     C*                                                    |
     C*
     C*  IF A SELF-REPORTING EMPLOYER, AND A CANCELLATION AUDIT WITHIN
     C*  THE CURRENT FUND YEAR, WE WILL REVERSE ALL DEPOSIT ACTIVITY
     C*  FOR ALL ******** FUND YEARS (YEAR BY YEAR), AND THEN TRANSFER
     C*  ANY REMAINING DEPOSITS TO PREMIUM
     C*
     C*
     C*  GET CANCEL DATE FROM EMPLOYER MASTER
     C*
     C*
     C*
     C*  ACCUMULATE I & H TRANSACTIONS TO BUCKETS BY TYPE & FYR
     C*
     C*
     C*                                                    'H' OR 'I'
     C*
     C*  FORCE LAST TO ARRAYS
     C*
     C*
     C*  INITIALIZE OUTPUT FIELDS
     C*
     C*
     C*  WRITE REVERSING ENTRIES FOR EACH FYR/TYPE
     C*
     C*  DEPOSIT PAYMENT NEGATION
     C*  DEPOSIT BILLING NEGATION
     C*  NEGATE AMOUNT
     C*  ACCUMULATE PAYMENT AMOUNTS FOR LATER TRANSFER TO PREMIUM
     C*
     C*  IF THERE IS AN ACCRUED DEPOSIT PAID AMOUNT, TRANSFER IT TO
     C*
     C*
     C*  IF THERE IS AN ACCRUED TAX ON DEP PAID AMOUNT, TRANSFER IT.
     C*
     C*
     C*
     C*  CHK FOR PRIOR FYR POLICY
     C*
     C*
     C*
     C*                    *** INITIALIZE
     C*
     C*                    *** READ PREMIUM PAYED TRANSFER CODE FOR
     C*                    *** CURRENT FYR FROM FUND DETAIL
     C*
     C*  RETRIEVE EMPLOYER DETAIL RECORD
     C*
     C*
     C*                    *** IF A FLAT CANCELLATION IS BEING RUN
     C*                    *** AND CANCEL DATE IS IN THE PRIOR YEAR
     C*
     C*                    *** PROCESS RECORDS (P,R,N) (WMEAP)
     C*
     C*                    *** CREATE PREMIUM PAYMENT X-FER TO PRIOR FY
     C*                    *** FOR SUM P RECORDS IN CURRENT FYR
     C*
     C*
     C*
     C*
     C*   WRITE SAFETY FEE ADJUSTMENT TO A/R
     C*
     C*
     C*
     C*   WRITE 1ST ASSESSMENT ADJUSTMENT TO A/R
     C*
     C*
     C*
     C*   WRITE 2ND ASSESSMENT ADJUSTMENT TO A/R
     C*
     C*
     C*
     C*
     C*   WRITE MEMBER FEE ADJUSTMENT TO A/R
     C*
     C*
     C*
     C*** WRITE STATE INCOME TAX ADJUSTMENT TO A/R
     C*
     C*
     C*
     C                   MOVE      ' '           XXSUM             1
     C     ELARSM        IFEQ      'Y'
     C                   MOVE      'Y'           XXWAR             1
     C                   MOVE      ' '           XXAR              1
     C                   ELSE
     C                   MOVE      'Y'           XXAR
     C                   ENDIF
     C*
     C                   EXSR      WRTAR
     C*
     C                   ENDIF                                                  F1UPAR IFEQ
     C                   ENDIF                                                  ELPTDT IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     SVEMP#        IFEQ      *ZERO
     C     *IN95         OREQ      *ON
     C     *IN96         OREQ      *ON
     C     *IN97         OREQ      *ON
     C     *IN71         OREQ      *ON
     C                   MOVE      *ON           *IN60
     C                   ELSE
     C     EMKY          READE     PEM01                                  60
     C                   ENDIF
     C*
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*
     C*  IF THIS IS NOT EMPLOYER SELECTION, GO DO NEXT EMPLOYER
     C*
     C     SVEMP#        CABEQ     *ZERO         READE
     C*
     C     END           TAG
     C     XXWAR         IFEQ      'Y'
     C                   MOVE      'Y'           XXSUM
     C                   MOVE      'Y'           XXAR
     C                   MOVE      XSDIV         XXDIV
     C                   EXSR      WRTAR
     C                   ENDIF
       // Process Final Invoice and then finalize
       If hldInv# <> 0 And ICGroupOpt(EACo#:hldFnd);

          // Is this a state reported division or non-summarized
          If GetStRptDv(EAco#:hldFnd:hldEmp#) = hldDiv Or hldArSm <> 'Y';
             inJson = '{"type":"billing",' +
                      '"invoice":"'  + %EditC(hldInv#:'X') + '",' +
                      '"company":"'  + %EditC(EAco#:'X')  + '",' +
                      '"group":"'    + %EditC(hldFnd:'X')  + '",' +
                      '"employer":"' + %EditC(hldEmp#:'X') + '",' +
                      '"division":"' + %EditC(hldDiv:'X')  + '",' +
                      '"active":"true"}';
     C*** Beg Chg ***  10/25/23  *** WC2922 *******************************
          //   ProcessAdjustedInvoicing(inJson);
               If hldCTST = 'Y';
                  ReplaceHeldInvoices(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:
                                      hldInv#:'A');
               Else;
                  ProcessAdjustedInvoicing(inJson);
               EndIf;
     C*** End Chg ***  10/25/23  *** WC2922 *******************************
             FinalizeThirdPartyInvoicing(EAco#:hldFnd);
          EndIf;

          // Send deleted unused invoice#
          ReportUnusedInvoices(EAco#:hldFnd:hldEmp#:*on);

       EndIf;
     C     *IN71         IFEQ      *ON
     C*
     C                   SELECT
     C     QQREV         WHENEQ    '1'
     C                   MOVE      'A'           QQREV
     C     QQREV         WHENEQ    '2'
     C                   MOVE      'B'           QQREV
     C     QQREV         WHENEQ    '3'
     C                   MOVE      'C'           QQREV
     C     QQREV         WHENEQ    '4'
     C                   MOVE      'D'           QQREV
     C     QQREV         WHENEQ    '5'
     C                   MOVE      'E'           QQREV
     C     QQREV         WHENEQ    '6'
     C                   MOVE      'F'           QQREV
     C                   ENDSL
     C*
     C                   ENDIF                                                  *IN71 IFEQ
     C*** Begin Add *** 01/05/24 *******************************************
       // Is this a mass run?
       If svEmp# = 0 And (qqRev = '3' Or qqRev = 'C' Or qqRev = 'F');
         // Send End Sweep
         clbBifData_Data = '{"sweepID":"' + %Trim(sweepID) +
                           '","transaction":"sweep completed"}';
         clbBifData_Len  = %Len(%Trim(clbBifData_Data));
         LoadSIWEBSVCDataQ(clbBifData);
       EndIf;
     C*** End   Add *** 01/05/24 *******************************************
     C                   SETON                                        LR
      *================================================
      *                   SUBROUTINE SRCALI
      *================================================

      /Free
       Begsr SrCali;
        Chain elKy4 pld01;
        If %Found;
          // Get total payrolls.
           Setll epKy3 pep02;
           Dou %Eof(WMEPL);
              Reade epKy3 pep02;
              If %Eof;
                 Leave;
              Endif;
              If EPstat = 'I';
                 Iter;
              Endif;
              xxPayr += EPaapr;
           Enddo;
          // Calculate aggregate.
           Eval(h) LDamg1 = xxPayr * LDlirt;
           Update pld01;
        Else;
          // Prevent carryover.
           LDamg1 = 0;
        Endif;
       Endsr;
      /End-Free
     C*====================================================================
     C* Write A/R information to holding file for billing records deleted
     C*====================================================================
     C     HOLDAR        BEGSR

       // Save off invoice #'s for reuse
       If EAninv <> 0;
          decInvoiceDiv = (EAdiv * 10000000000) + EAninv;
          index = %LookUp(decInvoiceDiv:arrInvoice);
          If index = 0;
       //*** begin chg ***  09/26/22  *********************************************
       //****index = %LookUp(9999999999999:arrInvoice);
             index = %LookUp(999999999999999:arrInvoice);
       //*** end chg *****  09/26/22  *********************************************
             arrInvoice(index) = decInvoiceDiv;
             SortA arrInvoice;
          EndIf;
       EndIf;
     C*
     C     EATRDT        IFLT      SYSDT
     C*
     C                   ADD       1             SYSTM
     C*
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     Q1USER        EAUSER
     C                   Z-ADD     SYSDT         EADT
     C                   Z-ADD     SYSTM         EATIME
     C*
     C                   WRITE     PEA0#
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*                   SUBROUTINE SRDSCT
     C*   SUBROUTINE TO SPREAD DISCOUNTS IF NECESSARY AND TO CALC
     C*   ACCORDING TO THE DISCOUNT TABLE.
     C     SRDSCT        BEGSR
     C*
     C                   Z-ADD     0             XXTOTS
     C                   Z-ADD     0             XXTDIS
     C                   XFOOT     DYS           XXDDAY            5 0
     C     RRDSIN        IFEQ      'Y'
     C                   MOVE      RRST          XXDMST
     C                   ELSE
     C                   MOVE      *BLANKS       XXDMST            2
     C                   ENDIF                                                  RRDSIN IFEQ
     C*
     C*  IF DISCOUNT PERIOD COVERS THE ENTIRE POLICY PERIOD, ANNUALIZE
     C*
     C     PDAYS         IFEQ      XXDDAY
     C     F1APFD        ANDEQ     'Y'
     C*
      /free
       //*** Beg Add*** 04/18/25 *******************************************
       //*** If pDays <> 0;
       If pDays <> 0 and stRptPDays <> 0;
       //*** End Add*** 04/18/25 *******************************************
          xxwh94 = xxprem/stRptPDays;
       Else;
          xxwh94 = 0;
       Endif;
      /end-free
     C                   If        xxUnearned = ' Y'
     c*
     C     wdays         sub       adays         xxdays
     c*
     C                   If        xxdays <> 0
     C                   eval      xxWH94 = xxprem / xxdays
     C                   else
     C                   z-add     0             xxwh94
     C                   Endif
     c*
     C                   Endif
     C*
     C     XXWH94        MULT      365           XXWH92           11 2
     C     XXWH92        MULT(H)   1.00          XWWK              9 0
     C                   Z-ADD     XWWK          XXWH92
     C*
     C* IF ENTIRE YEAR IS NOT UNDER DISCOUNT (IN THE CASE OF A STATE
     C* CHANGE WHERE ARD IS IN EFFECT), THEN USE THE ACTUAL
     C                   ELSE
     C                   Z-ADD     XXPREM        XXWH92
     C                   END
     C*
     C*  DETERMINE IF SPREAD IS IN EFFECT
     C*
     C                   MOVE      XXCO#         Q1CO#             3
     C                   MOVE      XXFND         Q1FND             3
     C                   MOVE      XXFYR         Q1FYR             3
     C                   MOVE      XXEMP#        Q1EMP#            9
     C                   MOVE      ' '           Q1SPRD            1
     C                   CALL      'SRDISC'
     C                   PARM                    Q1CO#
     C                   PARM                    Q1FND
     C                   PARM                    Q1FYR
     C                   PARM                    Q1EMP#
     C                   PARM                    Q1SPRD
     C                   MOVE      '0'           *IN62
     C     FNPEOF        IFEQ      'M'
     C     FNPEOA        ANDEQ     'Y'
     C     FNPEOF        OREQ      'G'
     C     FNPEOA        ANDEQ     'Y'
     C     FNPEOF        OREQ      'Y'
     C     FNPEOF        OREQ      'C'
     C     FNPEOA        ANDEQ     'Y'
     C                   MOVE      'Y'           Q1SPRD
     C                   MOVE      '1'           *IN62
     C                   ENDIF
     C*
     C*
     C*  IF DISCOUNT IS TO BE SPREAD OVER ALL DIVISIONS AND THIS
     C*  DIVISION IS CANCELLED, USE THE ESTIMATED DISCOUNT % AND
     C*  DO NOT INCLUDE THIS DIVISIONS PREMIUM IN CALC FOR OTHER DIV
     C*
     C*
     C*  IF THIS IS A NON-COMMON EXPIRATION DATE POLICY,
     C*  CONSIDER A DIVISION CANCELLED IF ELPTDT IS LESS THAN
     C*  ELPFDT + 1 YEAR
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*  IF ALL DIVISIONS WERE NOT CANCELLED ON THE SAME DAY THEN USE
     C*  THE ESTIMATED DISCOUNT %, OTHERWISE RECALCULATE THE DISCOUNT.
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*  IF DISCOUNT IS TO BE SPREAD OVER ALL DIVISIONS, READ
     C*  DISCOUNT DETAIL FILE & ACCUM STD PREM FOR ALL DIV
     C*
     C     Q1SPRD        IFEQ      'Y'
     C*
     C*  USE NEW CALCULATED STD FOR THIS DIVISION
     C*
     C                   Z-ADD     0             XXPCT
     C                   Z-ADD     XXWH92        XXLMT            11 2
     C                   Z-ADD     XXWH92        SVANSP           11 2
     C                   Z-ADD     XXPREM        XXTACP           11 2
     C*** begin add ***  04/27/23  *******************************
      /Free

        Chain(n) (xxCo#:xxFnd:xxFyr:xxEmp#:'P':xxDmst) pev00;
        If %found;
           xxLmt = EVxxTots;
           xxTacp = EVxxTacp;
        Else;

      /End-Free
     C*** end add *****  04/27/23  *******************************
     C  N62DSKY          CHAIN     PDS02                              60
     C   62DSKY2         CHAIN     PDS02                              60
     C     *IN60         DOWEQ     '0'
     C                   EXCEPT    PDS02E
     C     DSDIV         IFNE      XXDIV
     C     FNPEOF        ANDNE     'M'
     C     FNPEOF        ANDNE     'G'
     C     FNPEOF        ANDNE     'Y'
     C     FNPEOF        ANDNE     'C'
     C     DSEMP#        ORNE      XXEMP#
     C     FNPEOF        ANDEQ     'M'
     C     DSDIV         ORNE      XXDIV
     C     DSEMP#        ANDEQ     XXEMP#
     C     FNPEOF        ANDEQ     'M'
     C     DSEMP#        ORNE      XXEMP#
     C     FNPEOF        ANDEQ     'G'
     C     DSDIV         ORNE      XXDIV
     C     DSEMP#        ANDEQ     XXEMP#
     C     FNPEOF        ANDEQ     'G'
     C     DSEMP#        ORNE      XXEMP#
     C     FNPEOF        ANDEQ     'Y'
     C     DSDIV         ORNE      XXDIV
     C     DSEMP#        ANDEQ     XXEMP#
     C     FNPEOF        ANDEQ     'Y'
     C     DSEMP#        ORNE      XXEMP#
     C     FNPEOF        ANDEQ     'C'
     C     DSDIV         ORNE      XXDIV
     C     DSEMP#        ANDEQ     XXEMP#
     C     FNPEOF        ANDEQ     'C'
     C     ELKY3         CHAIN     PEL01                              60
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL(P)  RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C     XXDMST        ANDEQ     *BLANKS
     C     *IN60         OREQ      *OFF
     C     XXDMST        ANDNE     *BLANKS
     C     XXST          ANDEQ     XXDMST
     C*
     C     DSANSP        IFNE      *ZERO
     C     DSSTAT        OREQ      'A'
     C                   ADD       DSANSP        XXLMT
     C                   ADD       DSACSP        XXTACP
     C                   ELSE
     C                   ADD       DSSTPR        XXLMT
     C                   ADD       DSSTPR        XXTACP
     C                   ENDIF                                                  DSANSP IFNE
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C                   END
     C  N62DSKY          READE     PDS02                                  60
     C   62DSKY2         READE     PDS02                                  60
     C                   END
     C*** begin add ***  04/27/23  *******************************
      /Free
           Clear pev00;
           EVCo# = xxCo#;
           EVFnd = xxFnd;
           EVFyr = xxFyr;
           EVEmp# = xxEmp#;
           EVType = 'P';
           EVSt = xxDmst;
           EVxxTots = xxLmt;
           EVxxTacp = xxTacp;
           Write pev00;
        Endif;
      /End-Free
     C*** end add *****  04/27/23  *******************************
     C     ELKEY         CHAIN     PEL01                              60
     C                   Z-ADD     ELPFDT        XXEFDT
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVEL(P)  RSJRST        XXST
     C     RRKY          SETGT     PRR01
     C     RRKY1         READPE    PRR01                                  60
     C                   Z-ADD     XXLMT         XXTOTS           15 4
     C     XXLMT         IFNE      *ZERO
     C     XXPREM        DIV(H)    XXLMT         XXPCT
     C                   ELSE
     C                   Z-ADD     0             XXPCT
     C                   ENDIF                                                  XXLMT IFNE
     C*** begin add ***  07/25/23  ***********************************************
     C                   If        xxpct > 1
     C                   Z-add     1             xxpct
     C                   Endif
     C*** end add *****  07/25/23  ***********************************************
     C*
     C                   ELSE
     C*
     C*  IF DISCOUNT NOT SPREAD, JUST LOAD STD PREM OF THIS EMPLR/DIV
     C*  AND MAKE SURE THERE ARE NO RECORDS IN WDDSP FOR THIS EMPLR
     C*
     C                   Z-ADD     1             XXPCT
     C                   Z-ADD     XXWH92        XXLMT            11 2
     C     TAX           IFEQ      2
     C                   ENDIF                                                  TAX IFEQ
     C*
     C                   END                                                    Q1SPRD = Y
     C*
     C*  GET THE DISCOUNT BASED ON THE TABLE VALUES
     C*
     C                   MOVE      ELDISC        XXCODE
     C                   Z-ADD     RDAT(1)       XXEFDT
     C                   Z-ADD     0             AEFDT
     C                   Z-ADD     0             BEFDT
     C                   Z-ADD     XXDIV         SVDIV
     C                   MOVE      'A'           XXTP
     C     DSKEY         SETGT     PDS01
     C     DSKEY2        READPE    PDS01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DSEFDT        AEFDT             7 0
     C                   ENDIF
     C*
     C                   MOVE      'B'           XXTP
     C     DSKEY         SETGT     PDS01
     C     DSKEY2        READPE    PDS01                                  60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     DSEFDT        BEFDT             7 0
     C                   ENDIF
     C*
     C     AEFDT         IFGE      BEFDT
     C                   MOVE      'A'           XXTP
     C                   ELSE
     C                   MOVE      'B'           XXTP
     C                   ENDIF                                                  AEFDT IFGE
     C*
     C     Q1SPRD        IFEQ      'Y'
     C*
     C*** begin add ***  04/27/23  *******************************
      /Free

        Chain(n) (xxCo#:xxFnd:xxFyr:xxEmp#:'D':xxDmst) pev00;
        If %found;
           xxTdis = EVxxTdis;
           If xxTots = *zeros;
              xxPct = *zeros;
           Else;
        //*** begin chg ***  07/25/23  ***********************************************
        //****xxPct = xxWh92/xxTots;
              Monitor;
                 xxPct = xxWh92/xxTots;
              On-error;
                 xxPct = 1;
              Endmon;
        //*** end chg *****  07/25/23  ***********************************************
           Endif;
           xxLmt = xxTots;

           Exsr Sprdd;
           If xxTp ='B';
              xnDisp = DSpct;
           Endif;
           svDisa = xxDisa;
        Else;

      /End-Free
     C*** end add *****  04/27/23  *******************************
     C  N62DSKY          CHAIN     PDS02                              60
     C   62DSKY2         CHAIN     PDS02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   Z-ADD     DSDIV         XXDIV
     C     ELKY3         CHAIN(N)  PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C                   MOVE      ELPFDT        XXEFDT
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C*
     C     XXTOTS        IFNE      *ZERO
     C     DSANSP        DIV(H)    XXTOTS        XXPCT
     C                   ELSE
     C                   Z-ADD     0             XXPCT
     C                   ENDIF                                                  XXTOTS IFNE
     C*** begin add ***  07/25/23  ***********************************************
     C                   If        xxpct > 1
     C                   Z-add     1             xxpct
     C                   Endif
     C*** end add *****  07/25/23  ***********************************************
     C                   Z-ADD     XXTOTS        XXLMT
     C                   Z-ADD     0             XXDISA
     C     SVDIV         IFEQ      DSDIV
     C     FNPEOF        ANDNE     'M'
     C     FNPEOF        ANDNE     'G'
     C     FNPEOF        ANDNE     'Y'
     C     FNPEOF        ANDNE     'C'
     C     SVDIV         OREQ      DSDIV
     C     DSEMP#        ANDEQ     XXEMP#
     C     FNPEOF        ANDEQ     'M'
     C     SVDIV         OREQ      DSDIV
     C     DSEMP#        ANDEQ     XXEMP#
     C     FNPEOF        ANDEQ     'G'
     C     SVDIV         OREQ      DSDIV
     C     DSEMP#        ANDEQ     XXEMP#
     C     FNPEOF        ANDEQ     'Y'
     C     SVDIV         OREQ      DSDIV
     C     DSEMP#        ANDEQ     XXEMP#
     C     FNPEOF        ANDEQ     'C'
     C                   MOVE      'Y'           XXFLG             1
     C                   ENDIF                                                  XXDIV IFEQ
     C*
     C                   EXSR      SPRDD
     C*
     C                   ADD       XXDISA        XXTDIS           15 4
     C     XXFLG         IFEQ      'Y'
     C     XXTP          IFEQ      'B'
     C                   Z-ADD     DSPCT         XNDISP
     C                   ENDIF
     C                   Z-ADD     XXDISA        SVDISA           15 4
     C                   MOVE      *BLANK        XXFLG
     C                   ENDIF                                                  XXFLG IFEQ
     C*
     C  N62DSKY          READE     PDS02                                  60
     C   62DSKY2         READE     PDS02                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C*** begin add ***  04/27/23  *******************************
      /Free

           Clear pev00;
           EVCo# = xxCo#;
           EVFnd = xxFnd;
           EVFyr = xxFyr;
           EVEmp# = xxEmp#;
           EVType = 'D';
           EVSt = xxDmst;
           EVxxTdis = xxTdis;
           Write pev00;
        Endif;
      /End-Free
     C*** end add *****  04/27/23  *******************************
     C                   Z-ADD     SVDIV         XXDIV
     C     ELKEY         CHAIN     PEL01                              60
     C                   MOVE      ELRTCD        XXRTCD
     C     RSKY          CHAIN     PRS01                              60
     C                   MOVE      RSJRST        XXST
     C                   Z-ADD     ELPFDT        XXEFDT
     C*
     C                   ELSE
     C                   EXSR      SPRDD
     C     XXTP          IFEQ      'B'
     C                   Z-ADD     DSPCT         XNDISP
     C                   ENDIF
     C                   Z-ADD     XXDISA        SVDISA           15 4
     C                   ENDIF                                                  Q1SPRD IFEQ
     C*
     C*  SINCE DISCOUNT CALCULATED (XXDIS3) WAS BASED ON ANNUALIZING
     C*  XXPREM, WE NEED TO UN-ANNUALIZE THE DISCOUNT BACK
     C*
     C     XXWH92        IFNE      0
     C     SVDISA        DIV(H)    XXWH92        XXPCT1            9 6
     C                   END
     C     XXPCT1        MULT(H)   XXPREM        XXDIS3
     C     XXPCT1        MULT(H)   100           X2PCT1            7 2
     C*
     C*  LOAD WDDSP IF SPREADING DISCOUNT
     C*
     C     Q1SPRD        IFEQ      'Y'
     C*
     C     *IN97         IFEQ      *OFF
     C     DSKY1         CHAIN     PDS02                              60
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     XXCO#         DSCO#
     C                   Z-ADD     XXFND         DSFND
     C                   Z-ADD     XXFYR         DSFYR
     C                   Z-ADD     XXEMP#        DSEMP#
     C                   Z-ADD     XXDIV         DSDIV
     C                   Z-ADD     0             DSSTPR
     C                   Z-ADD     0             DSEDIS
     C                   Z-ADD     0             DSTEPR
     C                   Z-ADD     0             DSTEDS
     C                   Z-ADD     0             DSEPCT
     C                   END
     C                   Z-ADD     SVANSP        DSANSP
     C                   Z-ADD     XXPREM        DSACSP
     C                   Z-ADD     XXDIS3        DSADIS
     C                   Z-ADD     XXTOTS        DSTANP
     C                   Z-ADD     XXTACP        DSTACP
     C                   Z-ADD     X2PCT1        DSAPCT
     C                   MOVE      'A'           DSSTAT
     C                   MOVE      Q1USER        DSUSER
     C                   MOVE      Q1PGM         DSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DSDT
     C*
     C                   Z-ADD     DSTIME        DSTIME
     C     TAX           IFEQ      2
     C  N60              UPDATE    PDS02
     C   60              WRITE     PDS02
     C                   ENDIF                                                  TAX IFEQ
     C                   ENDIF                                                  *IN97 IFEQ
     C*
     C                   END                                                    Q1SPRD = Y
     C*
     C     ENDDIS        ENDSR
     C*
     C*****************************************************************
     C*
     C*  SUBROUTINE FOR APPLYING A DISCOUNT ON AN ARRAY-SPLIT PREMIUM.
     C*
     C     DISCA         BEGSR
     C*
     C                   Z-ADD     0             XXDISA           15 4
     C                   Z-ADD     1             X
     C     RDAT(X)       DOWNE     0
     C*
     C     PRM(X)        MULT(H)   XXPCT         XXWK90           15 4
     C                   DIV(H)    100           XXWK90
     C* if factor was passed in then reverse divide by 100
     C                   if        isFactor = *on
     C                   MULT(H)   100           XXWK90
     C                   endif
     C     X             ADD       1             Z                 3 0
     C     XXEFD         IFGT      PDAT(X)
     C     XXEFD         ANDLT     PDAT(Z)
     C     XXEXD         ORGT      PDAT(X)
     C     XXEXD         ANDLT     PDAT(Z)
     C*
     C*  GET NUMBER OF DAYS AFFECTED BY THE DISCOUNT IN THE
     C*  RATING PERIOD
     C*
     C     XXEFD         IFGT      PDAT(X)
     C                   Z-ADD     XXEFD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   ELSE
     C                   Z-ADD     PDAT(X)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQFROM            8 0
     C                   ENDIF                                                  XXEFD IFGT
     C*
     C     XXEXD         IFLT      PDAT(Z)
     C                   Z-ADD     XXEXD         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   ELSE
     C                   Z-ADD     PDAT(Z)       SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XQTO              8 0
     C                   ENDIF                                                  XXEXD IFLT
     C*
     C                   MOVE      XQFROM        QQFROM
     C                   MOVE      XQTO          QQTO
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
     C                   Z-ADD     QQDAYS        VDAYS             5 0
     C*
     C                   Z-ADD     *ZEROS        XXDPRO            5 4
     C*
     C     DYS(X)        IFNE      *ZEROS
     C     VDAYS         DIV(H)    DYS(X)        XXDPRO
     C                   ENDIF                                                  DYS,X IFNE
     C*
     C                   ELSE
     C*
     C     XXEFD         IFLE      PDAT(X)
     C     XXEXD         ANDGE     PDAT(Z)
     C                   Z-ADD     1             XXDPRO
     C*
     C                   ELSE
     C*
     C     XXEFD         IFGE      PDAT(Z)
     C     XXEXD         ORLE      PDAT(X)
     C                   Z-ADD     *ZEROS        XXDPRO
     C                   ENDIF                                                  XXEFD  >= PDAT,Z
     C*
     C                   ENDIF                                                  XXEFD  <= PDAT,X
     C*
     C                   ENDIF                                                  XXEFD  > PDAT,X
     C*                                                    & XXEFD  < PDAT,Z
     C*
     C*  PRORATE THE DISCOUNT AMOUNT.
     C*
     C                   MULT(H)   XXDPRO        XXWK90
     C                   Z-ADD(H)  XXWK90        XXWK10
     C                   ADD       XXWK10        XXDISA
     C*
     C                   Z-ADD     XXWK10        DIS(X)
     C*
     C                   SUB       XXWK10        PRM(X)
     C                   ADD       1             X
     C                   ENDDO                                                  RDAT DOWNE
     C*
     C                   ENDSR
     C*
     C*****************************************************************
     C     SPRDD         BEGSR
     C*
     C* CALCULATE 'A' TYPE DISCOUNT.
     C*
     C     XXTP          IFEQ      'A'
     C*
     C     DSKEY         SETGT     PDS03
     C     DSKEY2        READPE    PDS03                                  60
     C                   Z-ADD     DSEFDT        XXEFDT
     C     DSKEY         CHAIN     PDS03                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C     XXLMT         ANDGT     *ZERO
     C*
     C     XXLMT         SUB       DSLMT         XXDIF            15 4
     C     XXDIF         IFLT      *ZERO
     C                   Z-ADD     XXLMT         XXAMT            15 4
     C                   ELSE
     C                   Z-ADD     DSLMT         XXAMT
     C                   ENDIF                                                  XXDIF IFLT
     C     XXLMT         SUB       XXAMT         XXLMT
     C*
     C     XXPCT         MULT      XXAMT         XXAMT
     C     DSPCT         MULT      XXAMT         XXAMT
     C     XXAMT         DIV       100           XXAMT
     C                   ADD       XXAMT         XXDISA
     C*
     C     DSKEY         READE     PDS03                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C                   ENDIF                                                  XXTP IFEQ
     C*
     C* CALCULATE 'B' TYPE DISCOUNT.
     C*
     C     XXTP          IFEQ      'B'
     C*
     C                   Z-ADD     BEFDT         XXEFDT
     C                   Z-ADD(H)  XXLMT         XXAMTP
     C     DSKEY3        SETLL     PDS01
     C     DSKEY         READE     PDS01                                  60
     C     *IN60         IFEQ      *OFF
     C     XXPCT         MULT      XXLMT         XXLMT
     C     DSPCT         MULT      XXLMT         XXLMT
     C     XXLMT         DIV(H)    100           XXDISA
     C                   ENDIF                                                  *IN60 IFEQ
     C                   Z-ADD     ELPFDT        XXEFDT
     C*
     C                   ENDIF                                                  XXTP IFEQ
     C*
     C                   ENDSR
     C*
     C************************************************************
     C*  SUBROUTINE TO SEARCH FOR AND SUM RECORDS IN WMEAL
     C************************************************************
     C*
     C     SRREC         BEGSR
     C*
     C* SUM RECORDS
     C*
     C*                    *** INITIALIZE
     C                   MOVE      'N'           XFOUND            1
     C                   Z-ADD     *ZERO         XSUM             11 2
     C*
     C*                    *** READ WMEAL - EMPLOYER ACCOUNTS RECEIVABL
     C     EAKEY         CHAIN     PEA01                              61
     C*
     C*                    *** WHILE ABLE TO READ EMPLOYER AR FILE
     C     *IN61         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDNE     'D'
     C*
     C*                    *** IF RECORD FOUND (EAARTY)
     C     EAARTY        IFEQ      'P'
     C     EAARTY        OREQ      'R'
     C     EAARTY        OREQ      'N'
     C     EAARTY        OREQ      'V'
     C     EAARTY        OREQ      'T'
     C*
     C*                    *** SUM AMOUNTS
     C                   MOVE      'Y'           XFOUND            1
     C                   SELECT
     C     EAARTY        WHENEQ    'P'
     C                   ADD       EAAMT         XSUM
     C     EAARTY        WHENEQ    'R'
     C                   SUB       EAAMT         XSUM
     C     EAARTY        WHENEQ    'N'
     C                   SUB       EAAMT         XSUM
     C     EAARTY        WHENEQ    'V'
     C                   SUB       EAAMT         XSUM
     C     EAARTY        WHENEQ    'T'
     C                   ADD       EAAMT         XSUM
     C                   ENDSL
     C*
     C                   ENDIF                                                  ARTY
     C*
     C                   ENDIF                                                  TTCTYP <> 'D'
     C*
     C*                    *** READ NEXT EMPLOYER AR RECORD
     C     EAKEY         READE     PEA01                                  61
     C*
     C                   ENDDO                                                  WMEAL
     C*
     C* WRITE OFFSET RECORD
     C*
     C*                    *** IF RECORD FOUND
     C* WRITE OFFSET RECORD
     C*
     C*                    *** IF RECORD FOUND
     C     xfound        ifeq      'Y'
     C     xsum          andne     *zero
     C*
     C     xsum          mult      -1            EAAMT
      /Free
       @@typ@ = 'P';
       @@cat@ = 'P';
       Callp     WrtWmeap(xxarcd : eldiv : @@typ@ : @@cat@ :
                 desc(1):  eaamt   : sysDt );
      /End-Free
     C                   ENDIF                                                  FOUND
     C                   ENDSR                                                  SRREC
     C*
     C************************************************************
     C*  SUBROUTINE WRITE PREMIUM PAYMENT TRANSFER RECORD
     C************************************************************
     C*
     C     SRXFER        BEGSR
     C*
     C*                    *** IF RECORD FOUND
     C     XFOUND        IFEQ      'Y'
     C     XSUM          ANDNE     *ZERO
     C*
     C*                    *** WRITE A PREMIUM PAYMENT TRANSFER RECORD
     C*                    *** TO PREVIOUS FYR
     C*
     C                   move      'P'           eaarty
     C                   move      'P'           eactyp
     C*
      /Free
       callp     WrtWmeap(xxarcd : eldiv : eaarty : eactyp :
                 desc(1):  xsum    : sysDt );
      /End-Free
     C                   ENDIF                                                  REC FOUND
     C*
     C                   ENDSR                                                  SRREC
     C*
     C*****************************************************************
     C*
     C* SUBROUTINE TO DETERMINE IF A DATE RANGE CONTAINS FEBRUARY 29TH.
     C*
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
     C*
     C* SRADR - SUBROUTINE TO CALCULATE THE AUDIT DISC REDUCTION FACTOR
     C*         DUP FROM PROGRAM ON CLT045 (45WBT)
     C*
     C*****************************************************************
     C*
     C     SRADR         BEGSR
     C*
     C                   Z-ADD     ELPTDT        XXPTDT
     C                   Z-ADD     ELPFDT        XXYMD
     C                   Z-ADD     0             XXINST            2 0
     C                   Z-ADD     0             XXLATE            2 0
     C                   Z-ADD     0             XXCNT             2 0
     C                   Z-ADD     0             XXADR             5 4
     C                   MOVEA     *ZEROS        XODT
     C*
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C*
     C* DETERMINE # OF INSTALLMENTS FOR ESTIMATING OR S/R.
     C*
     C     BSSEFL        IFEQ      'S'                                          SELF REPORT
     C*
     C* FOR S/R, COUNT THE NUMBER OF MONTHS IN THE POLICY.
     C*
     C     XXYM          DOWLT     XXPYM
     C                   ADD       1             XXINST
     C*
     C     XXMM          IFEQ      12
     C                   Z-ADD     1             XXMM
     C                   ADD       1             XXYY
     C                   ELSE
     C                   ADD       1             XXMM
     C                   ENDIF
     C*
     C                   ENDDO                                                  XXYM DOWLE
     C*
     C                   MOVE      '01'          XXYMD
     C     XXYMD         IFLT      XXPTDT
     C                   ADD       1             XXINST
     C                   ENDIF
     C                   ELSE                                                   BSSEFL IFEQ
     C*
     C* FOR ESTIMATING, COUNT THE NUMBER OF NON-ZERO ENTRIES IN THE
     C* BILLING FILE.
     C*
     C     1             DO        12            X
     C*
     C     AB(X)         IFNE      *ZERO
     C                   ADD       1             XXINST
     C                   ENDIF                                                  AB,X IFNE
     C*
     C                   ENDDO                                                  DO: X = 1-12
     C*
     C                   ENDIF                                                  BSSEFL IFEQ
     C*
     C* NOW DETERMINE THE NUMBER OF LATE PAYMENTS, SELECT 'P' TYPE
     C* RECORDS (EXCLUDING EXPENSE CONSTANT) AND TOTAL ALL THE 'Y'
     C* ENTRIES FOR EALATE.
     C*
     C*
     C* If Client "045" then use "B" in XXARTY to pull Billings
     C* Instead of Payments.
     C*
     C*
     C                   MOVE      'P'           XXARTY
     C*
     C     EAKEY2        CHAIN     PEA01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EALATE        IFEQ      'Y'
     C*
     C*  ONLY CONSIDER PREMIUM PAYMENTS
     C*
     C                   MOVE      *BLANKS       XXKEY
     C                   MOVEL     EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C     TTCTYP        ANDEQ     'P'                                          PREMIUM PMTS ONLY
     C*
     C*  ONLY COUNT A LATE PAYMENT FOR A GIVEN POSTING MONTH/YEAR ONCE
     C*
     C                   Z-ADD     1             X
     C     EAPOYM        LOOKUP    XODT(X)                                55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *ZEROS        LOOKUP    XODT(X)                                65
     C     *IN65         IFEQ      '1'
     C                   Z-ADD     EAPOYM        XODT(X)
     C                   END                                                    *IN65=1
     C                   ADD       1             XXLATE
     C                   ENDIF                                                  *IN55=0
     C                   ENDIF                                                  TTCTYP='P'
     C                   ENDIF                                                  EALATE IFEQ
     C*
     C     EAKEY2        READE     PEA01                                  60
     C                   ENDDO                                                  *IN60 DOWEQ
     C*
     C* FOR S/R EMPLOYERS, ADD IN MISSING MONTHS TO XXLATE.
     C*
     C     BSSEFL        IFEQ      'S'
     C                   EXSR      SRMN
     C                   ADD       XXCNT         XXLATE
     C                   ENDIF                                                  BSSEFL IFEQ
     C*
     C* ENSURE WE DON'T HAVE MORE LATE PAYMENTS THAN INSTALLMENTS.
     C*
     C     XXLATE        IFGT      XXINST
     C                   Z-ADD     XXINST        XXLATE
     C                   ENDIF                                                  XXLATE IFGT
     C*
     C* FINALLY, CALCULATE THE AUDIT DISCOUNT REDUCTION FACTOR.
     C*
     C     XXINST        IFNE      *ZERO
     C     XXLATE        DIV(H)    XXINST        XXADR
     C                   ENDIF                                                  XXINST IFNE
     C*
     C                   ENDSR                                                  SRADR
     C*
     C*****************************************************************
     C*             MISSING MONTHS' REPORTS SUBROUTINE                *
     C*****************************************************************
     C*
     C     SRMN          BEGSR
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
     C*
     C* The range of dates begins with employer's policy from date to
     C* the month prior to the previous requested month. Take requested
     C* month and subtract 1 to get the prior month.
     C*
     C                   Z-ADD     ELPTDT        XXCPDT
     C*
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         QQDT              8 0
     C*
     C                   CALL      'SRPAST8'
     C                   PARM                    QQDT              8 0
     C                   PARM      1             QQDAYS            5 0
     C                   PARM                    QQERR             1
     C                   PARM                    QQTERM            1
     C*
     C     QQERR         IFNE      '1'
     C                   Z-ADD     QQDT          SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXCPDT
     C                   ENDIF
     C*
     C                   Z-ADD     ELPFDT        XXPFDT
     C*
     C* Loop through all months from start date to comparison date.
     C*
     C     X#STDT        IFLE      XXCPYM
     C     X#STDT        DOWLE     XXCPYM
     C*
     C                   MOVE      *ZEROS        CLS
     C                   MOVE      *ZEROS        AM2
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
     C                   ADD       SRAMTP        AM2(Y1)
     C                   Z-ADD     1000          Y1
     C                   ELSE
     C     CLS(Y1)       IFEQ      0
     C                   Z-ADD     SRCLAS        CLS(Y1)
     C                   Z-ADD     SRAMTP        AM2(Y1)
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
     C                   ADD       AM2(Y1)       XAMT
     C                   ADD       1             Y1
     C                   END
     C*
     C     XAMT          IFEQ      0
     C*
     C*  ONLY COUNT A LATE PAYMENT FOR A GIVEN POSTING MONTH/YEAR ONCE
     C*
     C                   Z-ADD     1             X
     C     X#STDT        LOOKUP    XODT(X)                                55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *ZEROS        LOOKUP    XODT(X)                                65
     C     *IN65         IFEQ      '1'
     C                   Z-ADD     X#STDT        XODT(X)
     C                   END                                                    *IN65=1
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  *IN55=0
     C                   Z-ADD     XXSTMN        X1                3 0
     C                   ADD       1             XXSTMN                         get nxt mnth
     C                   GOTO      NXT1
     C                   END
     C                   END
     C*
     C* Check for case 3.
     C*
     C     CNT           IFEQ      0
     C*
     C*  ONLY COUNT A LATE PAYMENT FOR A GIVEN POSTING MONTH/YEAR ONCE
     C*
     C                   Z-ADD     1             X
     C     X#STDT        LOOKUP    XODT(X)                                55
     C     *IN55         IFEQ      '0'
     C                   Z-ADD     1             X
     C     *ZEROS        LOOKUP    XODT(X)                                65
     C     *IN65         IFEQ      '1'
     C                   Z-ADD     X#STDT        XODT(X)
     C                   END                                                    *IN65=1
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  *IN55=0
     C                   Z-ADD     XXSTMN        X1                3 0
     C                   ADD       1             XXSTMN                         get nxt mnth
     C*
     C* ADD 1 TO XXCNT FOR THE MISSING MONTH.
     C*
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
     C                   ENDIF                                                  XXSTMN IFGT
     C*
     C                   ENDDO                                                  X#STDT DOWLE
     C*
     C                   ENDIF                                                  X#STDT IFLE
     C*
     C                   ENDSR
     C*
      *
      *  SUBROUTINE TO WRITE A/R RECORDS FOR THE POLICY
      *
     C*****************************************************************
      *
     C     WRTAR         BEGSR
      *
      *  IF TOTAL AUDIT ADJUSTMENT IS A CREDIT THAN POST TO THE
      *  CURRENT DATE, IF DEBIT USE INTERIM # OF DAYS TO DETERMINE
      *  POST DATE
      *
     C                   Z-ADD     X@DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
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
     C     XXACDT        ANDNE     *ZERO
     C*
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ENDIF                                                  @@PLAN IFEQ
     C*
     C*
     C     ELKEY         CHAIN(N)  PEL01                              60
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
     C*
     C     XXAR          IFEQ      ' '
     C                   ADD       XXDIFF        XTDIFF
     C                   ENDIF
     C*
     C     XXSUM         IFEQ      'Y'
     C                   Z-ADD     XTDIFF        XXDIFF
     C                   ENDIF
     C     XXAR          IFEQ      ' '
     C                   ADD       SVDIFF        XTSVDF
     C                   ENDIF
     C*
     C     XXSUM         IFEQ      'Y'
     C                   Z-ADD     XTSVDF        SVDIFF
     C                   ENDIF
     C*
     C* Process audit credit
     C**IF*AUDIT POST DATE IS PRIOR TO START OF POLICY, DEFAULT IT.
     C*
     C* POST ALL ADJUSTMENTS WITH THE POSTING DATE OF THE MAIN
     C* AUDIT ADJUSTMENT.
     C*
     C     xxar          ifeq      'Y'
      /free

       xxdiff = xxdiff - UpdateDividend(ELCo#: ELFnd : ELEmp# : ELDiv : ELFyr);

      /end-free
     C**Call subroutine to write record in WMEAP
     C
      /Free
      /End-Free
     C*
     C*
     C
      /Free

       //* Defer prior audit records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDiff <= 0) or (EXDPAD = 'Y' and xxDiff > 0);

          For x = 1 to 99;

             instamt = prem(x).amt#;
             instpodt = prem(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(qqar:xxdiv:xxtype:xxctyp:xxdesc:-instamt:instpodt);
                xxDiff = xxDiff + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write audit adjustment to A/R.

       If xxDiff > 0;
          WrtWmeap(qqar:xxdiv:xxtype:xxctyp:xxdesc:xxdiff:xxdate);
       Else;
          WrtWmeap(qqar:xxdiv:xxtype:xxctyp:xxdesc:xxdiff:sysDt);
       Endif;

      /End-Free
     C                   if        ELSTAM <> *zero
     C                              and XRARCD <> *blanks
     C*** Begin Chg *** 05/22/25 *******************************************
     C*****                         and SVDIFF <> *zero
     C                              and (SVDIFF - totAdjPrem) <> *zero
     C*** End   Chg *** 05/22/25 *******************************************
     C                              and (BSLPGM = 'P' or BSLPGM = 'I')
     C*
     C*
     C*** Begin Chg *** 05/22/25 *******************************************
     C*****              callp     WrtWmeap(xrarcd : xxdiv : xrtype : xrctyp :
     C*****                                 xrdesc :(svdiff * -1): xxdate)
     C                   callp     WrtWmeap(xrarcd : xxdiv : xrtype : xrctyp :
     C                             xrdesc :((svdiff - totAdjPrem) * -1): xxdate)
     C*** End   Chg *** 05/22/25 *******************************************
     C                   ENDIF                                                  BSLPGM IFNE
     C                   ENDIF
     C*
     C*** WRITE EXPENSE CONSTANT ADJUSTMENT TO A/R         |  7-8-88
     C*                                                    |
     C     XXAR          IFEQ      ' '
     C                   ADD       XXDIFE        XTDIFE
     C                   ENDIF
     C*
     C     XXSUM         IFEQ      'Y'
     C                   Z-ADD     XTDIFE        XXDIFE
     C                   ENDIF
     C*
     C*
     C     xxar          ifeq      'Y'
     C**Call subroutine to write record in WMEAP

      /Free

       //* Defer prior expense constant records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDife <= 0) or (EXDPAD = 'Y' and xxDife > 0);

          For x = 1 to 99;

             instamt = expConst(x).amt#;
             instpodt = expConst(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xxcdeb:xxdiv:xttype:xtctyp:xtdesc:-instamt:instpodt);
                xxDife = xxDife + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write expense constant adjustment to A/R.

       If xxDife > 0;
          WrtWmeap(xxcdeb:xxdiv:xttype:xtctyp:xtdesc:xxdife:xxdate);
       Endif;

       If xxDife < 0;
          WrtWmeap(xxcdeb:xxdiv:xttype:xtctyp:xtdesc:xxdife:sysDt);
       Endif;

      /End-Free
     C                   if        ELSTAM <> *zero
     C                              and XRARCD <> *blanks
     C*** Begin Chg *** 05/22/25 *******************************************
     C*****                         and XXDIFE <> *zero
     C                              and (XXDIFE - totAdjExpn) <> *zero
     C*** End   Chg *** 05/22/25 *******************************************
     C                              and (BSLPGM = 'P' or BSLPGM = 'I')
     C*
     C*** Begin Chg *** 05/22/25 *******************************************
     C*****              callp     WrtWmeap(xrarcd : xxdiv : xrtype : xrctyp :
     C*****                                 xrdesc : xxdife : xxdate)
     C                   callp     WrtWmeap(xrarcd : xxdiv : xrtype : xrctyp :
     C                               xrdesc : (xxdife - totAdjExpn): xxdate)
     C*** End   Chg *** 05/22/25 *******************************************
     C*
     C                   ENDIF                                                  BSLPGM IFNE
     C                   ENDIF
     C*
     C*
     C*  IF A SELF-REPORTING EMPLOYER, AND A CANCELLATION AUDIT WITHIN
     C*  THE CURRENT FUND YEAR, WE WILL REVERSE ALL DEPOSIT ACTIVITY
     C*  FOR ALL ******** FUND YEARS (YEAR BY YEAR), AND THEN TRANSFER
     C*  ANY REMAINING DEPOSITS TO PREMIUM
     C*
     C*  GET CANCEL DATE FROM EMPLOYER MASTER
     C*
     C     XXSUM         IFNE      'Y'
     C*
     C                   Z-ADD     *ZEROS        XXCNDT
     C                   Z-ADD     5             XXX
     C                   DO        5
     C     ED(XXX)       IFNE      *ZEROS
     C                   Z-ADD     CD(XXX)       XXCNDT
     C                   LEAVE
     C                   END
     C                   SUB       1             XXX
     C                   ENDDO
     C*
     C     F1PPTR        IFNE      *BLANKS
     C     F1DBTR        ANDNE     *BLANKS
     C     F1DPTR        ANDNE     *BLANKS
     C                   MOVEL(P)  ELBSCD        XXKEY
     C     TBKEY         CHAIN     PBS01                              60
      *
      *  TURN ON THE DEPOSIT MODE BY YEAR FLAG (XXCYDM) IF THE CURRENT
      *  POLICY YEAR IS DEFINED AS SUCH IN WDF1P
      *
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      'Y'           XXCYDM            1
     C                   ELSE
     C                   MOVE      'N'           XXCYDM            1
     C                   ENDIF
     C*
      /free
       If (*in60='0' and (BSsefl='S' or BSsefl='E')
        and xxCNDT <= ELptdt and xxCNDT <> *zeros) Or
        (*in60='0' and (BSsefl='S' or BSsefl='E') and
        xxCYDM = 'Y');
      /end-free
     C                   Clear                   xxColt            1
      /free
       Open wmcol;
       Chain(n) (XXCO#:XXFND:XXEMP#) PCO00;
       If %found;
          xxColt = 'Y';
       Endif;
       Close wmcol;
      /end-free
     C*
     C                   exsr      xfrdeposit
     C*
     C*  ACCUMULATE I & H TRANSACTIONS TO BUCKETS BY TYPE & FYR
      * DO NOT INCLUDE OTHER YEAR DEPOSIT TRANSACTIONS IF THAT YEAR IS FLAGGED
      * AS A PER YEAR DEPOSIT MODE
      *
     C* RESET GROUP DETAIL
     C*  INITIALIZE OUTPUT FIELDS
     C*  WRITE REVERSING ENTRIES FOR EACH FYR/TYPE
     C*  DEPOSIT BILLING NEGATION
     C*  ACCUMULATE PAYMENT AMOUNTS FOR LATER TRANSFER TO PREMIUM
     C*  IF THERE IS AN ACCRUED TAX ON DEP PAID AMOUNT, TRANSFER IT.
     C*
     C                   ENDIF                                                  BSSEFL = S
     C*
     C                   ENDIF                                                  F1PPTR IFNE
     C*
     C*  CHK FOR PRIOR FYR POLICY
     C*
     C                   Z-ADD     XXFYR         X1FYR             3 0
     C     XXFYR         SUB       1             XXPFYR
     C     ELKY2         CHAIN     PEL01                              62
     C*
     C     *IN62         IFEQ      *OFF
     C     ELFAF         ANDNE     *BLANK
     C*
     C*                    *** READ PREMIUM PAYED TRANSFER CODE FOR
     C*                    *** CURRENT FYR FROM FUND DETAIL
     C     F1KEY         CHAIN     PF101                              60
     C                   MOVE      F1FCTR        XXPPTR            2
     C*
     C*  RETRIEVE EMPLOYER DETAIL RECORD
     C*
     C     ELKEY         CHAIN     PEL01                              60
     C*
     C*                    *** IF A FLAT CANCELLATION IS BEING RUN
     C*                    *** AND CANCEL DATE IS IN THE PRIOR YEAR
     C     *IN60         IFEQ      '0'
     C     ELPFDT        ANDEQ     ELPTDT
     C     EMCAD1        ANDLE     ELPFDT
     C     F1FCTR        ANDNE     *BLANKS
     C*
     C*                    *** PROCESS RECORDS (P,R,N) (WMEAP)
     C                   MOVE      XXPPTR        XXARCD            2            AR CODE
     C                   EXSR      SRREC
     C*
     C*                    *** CREATE PREMIUM PAYMENT X-FER TO PRIOR FY
     C*                    *** FOR SUM P RECORDS IN CURRENT FYR
     C                   SUB       1             XXFYR
     C                   EXSR      SRXFER
     C*
     C* TRANSFER ANY FEE BALANCES TO A PRIOR YEAR AS WELL.
     C*
     C                   Z-ADD     X1FYR         XXFYR
     C                   EXSR      SRFEE
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     X1FYR         XXFYR
     C*
     C                   ENDIF                                                  *IN62 = *OFF
     C*
     C*
     C                   ENDIF                                                  XXSUM IFNE
     C*
     C*   WRITE ASSESSMENTS ADJUSTMENT TO A/R
     C                   if        xxar = '  '
     C                   add       xxdif1        xtdif1
     C                   add       xxdif2        xtdif2
     C                   add       xxdif3        xtdif3
     C                   add       xxdif4        xtdif4
     C                   add       xxdif5        xtdif5
     C                   add       xxdif6        xtdif6
     C                   add       xxdif7        xtdif7
      *** Beg Chg ***** 07/24/24 *******************************
     C*Safety Fees
     C***                z-add     xxdifs        xtdifs
     C*MemberShip Fees
     C***                z-add     xxdifm        xtdifm
     C* Taxes
     C***                z-add     xxDifx        xtDifx
     C*Safety Fees
     C                   add       xxdifs        xtdifs
     C*MemberShip Fees
     C                   add       xxdifm        xtdifm
     C* Taxes
     C                   add       xxDifx        xtDifx
      *** End Chg ***** 07/24/24 *******************************
     C                   endif
     C*
     C                   if        xxsum = 'Y'
     C                   z-add     xtdif1        xxdif1
     C                   z-add     xtdif2        xxdif2
     C                   z-add     xtdif3        xxdif3
     C                   z-add     xtdif4        xxdif4
     C                   z-add     xtdif5        xxdif5
     C                   z-add     xtdif6        xxdif6
     C                   z-add     xtdif7        xxdif7
     C*Safety Fees
     C                   z-add     xtdifs        xxdifs
     C*MemberShip Fees
     C                   z-add     xtdifm        xxdifm
      /Free
         XXDIFX = xtdifx;
      /End-Free
     C                   endif

     c                   if        xxar = 'Y'
      /Free
       //**************************************************************************

       //* Defer Safety fee records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDifs <= 0) or (EXDPAD = 'Y' and xxDifs > 0);

          For x = 1 to 99;

             instamt = safFees(x).amt#;
             instpodt = safFees(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xxsfcd:xxdiv:xxsftp:xxsfct:xxsfds:-instamt:instpodt);
                xxDifs = xxDifs + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write Safety fee adjustment to A/R.

       If xxDifs > 0;
          WrtWmeap(xxsfcd:xxdiv:xxsftp:xxsfct:xxsfds:xxdifs:xxdate);
       Endif;

       If xxDifs < 0;
          WrtWmeap(xxsfcd:xxdiv:xxsftp:xxsfct:xxsfds:xxdifs:sysDt);
       Endif;

       //**************************************************************************

       //* Defer 1st assessment records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDif1 <= 0) or (EXDPAD = 'Y' and xxDif1 > 0);

          For x = 1 to 99;

             instamt = asst1(x).amt#;
             instpodt = asst1(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xx1cde:xxdiv:xx1typ:xx1cat:xx1des:-instamt:instpodt);
                xxDif1 = xxDif1 + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write 1st assessment adjustment to A/R.

       If xxDif1 > 0;
          WrtWmeap(xx1cde:xxdiv:xx1typ:xx1cat:xx1des:xxdif1:xxdate);
       Endif;

       If xxDif1 < 0;
          WrtWmeap(xx1cde:xxdiv:xx1typ:xx1cat:xx1des:xxdif1:sysDt);
       Endif;

       //**************************************************************************

       //* Defer 2nd assessment records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDif2 <= 0) or (EXDPAD = 'Y' and xxDif2 > 0);

          For x = 1 to 99;

             instamt = asst2(x).amt#;
             instpodt = asst2(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xx2cde:xxdiv:xx2typ:xx2cat:xx2des:-instamt:instpodt);
                xxDif2 = xxDif2 + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write 2nd assessment adjustment to A/R.

       If xxDif2 > 0;
          WrtWmeap(xx2cde:xxdiv:xx2typ:xx2cat:xx2des:xxdif2:xxdate);
       Endif;

       If xxDif2 < 0;
          WrtWmeap(xx2cde:xxdiv:xx2typ:xx2cat:xx2des:xxdif2:sysDt);
       Endif;

       //**************************************************************************

       //* Defer 3rd assessment records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDif3 <= 0) or (EXDPAD = 'Y' and xxDif3 > 0);

          For x = 1 to 99;

             instamt = asst3(x).amt#;
             instpodt = asst3(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xx3cde:xxdiv:xx3typ:xx3cat:xx3des:-instamt:instpodt);
                xxDif3 = xxDif3 + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write 3rd assessment adjustment to A/R.

       If xxDif3 > 0;
          WrtWmeap(xx3cde:xxdiv:xx3typ:xx3cat:xx3des:xxdif3:xxdate);
       Endif;

       If xxDif3 < 0;
          WrtWmeap(xx3cde:xxdiv:xx3typ:xx3cat:xx3des:xxdif3:sysDt);
       Endif;

       //**************************************************************************

       //* Defer 4th assessment records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDif4 <= 0) or (EXDPAD = 'Y' and xxDif4 > 0);

          For x = 1 to 99;

             instamt = asst4(x).amt#;
             instpodt = asst4(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xx4cde:xxdiv:xx4typ:xx4cat:xx4des:-instamt:instpodt);
                xxDif4 = xxDif4 + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write 4th assessment adjustment to A/R.

       If xxDif4 > 0;
          WrtWmeap(xx4cde:xxdiv:xx4typ:xx4cat:xx4des:xxdif4:xxdate);
       Endif;

       If xxDif4 < 0;
          WrtWmeap(xx4cde:xxdiv:xx4typ:xx4cat:xx4des:xxdif4:sysDt);
       Endif;

       //**************************************************************************

       //* Defer 5th assessment records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDif5 <= 0) or (EXDPAD = 'Y' and xxDif5 > 0);

          For x = 1 to 99;

             instamt = asst5(x).amt#;
             instpodt = asst5(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xx5cde:xxdiv:xx5typ:xx5cat:xx5des:-instamt:instpodt);
                xxDif5 = xxDif5 + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write 5th assessment adjustment to A/R.

       If xxDif5 > 0;
          WrtWmeap(xx5cde:xxdiv:xx5typ:xx5cat:xx5des:xxdif5:xxdate);
       Endif;

       If xxDif5 < 0;
          WrtWmeap(xx5cde:xxdiv:xx5typ:xx5cat:xx5des:xxdif5:sysDt);
       Endif;

       //**************************************************************************

       //* Defer 6th assessment records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDif6 <= 0) or (EXDPAD = 'Y' and xxDif6 > 0);

          For x = 1 to 99;

             instamt = asst6(x).amt#;
             instpodt = asst6(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xx6cde:xxdiv:xx6typ:xx6cat:xx6des:-instamt:instpodt);
                xxDif6 = xxDif6 + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write 6th assessment adjustment to A/R.

       If xxDif6 > 0;
          WrtWmeap(xx6cde:xxdiv:xx6typ:xx6cat:xx6des:xxdif6:xxdate);
       Endif;

       If xxDif6 < 0;
          WrtWmeap(xx6cde:xxdiv:xx6typ:xx6cat:xx6des:xxdif6:sysDt);
       Endif;

       //**************************************************************************

       //* Defer 7th assessment records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDif7 <= 0) or (EXDPAD = 'Y' and xxDif7 > 0);

          For x = 1 to 99;

             instamt = asst7(x).amt#;
             instpodt = asst7(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xx7cde:xxdiv:xx7typ:xx7cat:xx7des:-instamt:instpodt);
                xxDif7 = xxDif7 + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write 7th assessment adjustment to A/R.

       If xxDif7 > 0;
          WrtWmeap(xx7cde:xxdiv:xx7typ:xx7cat:xx7des:xxdif7:xxdate);
       Endif;

       If xxDif7 < 0;
          WrtWmeap(xx7cde:xxdiv:xx7typ:xx7cat:xx7des:xxdif7:sysDt);
       Endif;

       //**************************************************************************

       //* Defer Membership fee records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDifm <= 0) or (EXDPAD = 'Y' and xxDifm > 0);

          For x = 1 to 99;

             instamt = memFees(x).amt#;
             instpodt = memFees(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xxmfbl:xxdiv:xxmftp:xxmfct:xxmfds:-instamt:instpodt);
                xxDifm = xxDifm + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write Membership fees adjustment to A/R.

       If xxDifm > 0;
          WrtWmeap(xxmfbl:xxdiv:xxmftp:xxmfct:xxmfds:xxdifm:xxdate);
       Endif;

       If xxDifm < 0;
          WrtWmeap(xxmfbl:xxdiv:xxmftp:xxmfct:xxmfds:xxdifm:sysDt);
       Endif;

       //**************************************************************************

       //* Defer State tax records if flagged at billing options.

       If (EXDPAC = 'Y' and xxDifx <= 0) or (EXDPAD = 'Y' and xxDifx > 0);

          For x = 1 to 99;

             instamt = taxes(x).amt#;
             instpodt = taxes(x).podt;

             If instpodt = 0;
                Leave;
             Endif;

             If instamt <> 0;
                WrtWmeap(xxtxcd:xxdiv:xstype:xsctyp:xsdesc:-instamt:instpodt);
                xxDifx = xxDifx + instamt;
             Endif;

          Endfor;

       Endif;

       //* Write State tax adjustment to A/R.

       If xxDifx > 0;
          WrtWmeap(xxtxcd:xxdiv:xstype:xsctyp:xsdesc:xxdifx:xxdate);
       Endif;

       If xxDifx < 0;
          WrtWmeap(xxtxcd:xxdiv:xstype:xsctyp:xsdesc:xxdifx:sysDt);
       Endif;

       //**************************************************************************

      /End-Free

     C                   endif                                                  xxar = 'Y'

     C     XXVAL         IFEQ      'Y'
     C     XXCLT#        ANDEQ     '025'
     C                   MOVEL(P)  'PTYP'        KKTBID
     C                   MOVEL(P)  ELQTYP        KKKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTFG01        ANDEQ     'A'
     C     *IN60         OREQ      *OFF
     C     GTFG01        ANDEQ     'M'
     C                   EXSR      SRCALI
     C                   ENDIF                                                  GTFG01= 'A'OR 'M'
     C                   ENDIF                                                  XXVAL IFEQ
      *
      * DISSOLVE AUDIT CREDITS, IF ANY AND POLICY NOT FLAT CX
      *
     C     ELPFDT        IFNE      ELPTDT
     C                   Z-ADD     DBRRN         ELRRN             9 0
     C                   CALL      'WA059R'
     C                   PARM                    ELRRN
     C                   ENDIF
     C*
     C                   ENDSR                                                  WRTAR
     C*****************************************************************
     C*
     C* SUBROUTINE TO ADJUST A/R OFF A POLICY THAT IS SUMMARY.
     C*
     C     ADJAR         BEGSR
     C*
     C                   Z-ADD     0             XADIFF           11 2
     C                   Z-ADD     0             XADIFE           11 2
     C                   Z-ADD     0             XADIFX           11 2
     C                   Z-ADD     0             XADIF1           11 2
     C                   Z-ADD     0             XADIF2           11 2
     C                   Z-ADD     0             XADIF3           11 2
     C                   Z-ADD     0             XADIF4           11 2
     C                   Z-ADD     0             XADIF5           11 2
     C                   Z-ADD     0             XADIF6           11 2
     C                   Z-ADD     0             XADIF7           11 2
     C                   Z-ADD     0             XADIFS           11 2
     C                   Z-ADD     0             XADIFM           11 2
     C                   Z-ADD     0             XADIFI           11 2
     C*
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* BILLED
     C*
     C**TAX
     C*
     C* SAFETY FEES
     C*
     C* MEMBERSHIP FEES
     C*
     C* 1ST ASSESSMENT FEES
     C*
     C* 2ND ASSESSMENT FEES
     C**3RD ASSESSMENT FEES
     C**4TH ASSESSMENT FEES
     C**5TH ASSESSMENT FEES
     C**6TH ASSESSMENT FEES
     C**7TH ASSESSMENT FEES
     C**INSTALLMENT FEES
     C****Billed
     C* BILLED
     C*
     C     EAArty        Ifeq      'B'
     C     EAArty        Oreq      'A'
     C     EAArty        Oreq      'D'
     C*
     C*** Begin Add *** 05/22/25 *******************************************
     C     TTCtyp        Ifne      'E'
     C                   Add       EAAmt         totAdjPrem
     C                   Else
     C                   Add       EAAmt         totAdjExpn
     C                   Endif                                                  TTCTYP IFNE
     C*** End   Add *** 05/22/25 *******************************************
     C*
     C     EAPodt        Iflt      xxDate
     C*
     C     TTCtyp        Ifne      'E'
     C                   Add       EAAmt         xaDiff
     C                   Else
     C                   Add       EAAmt         xaDife
     C                   Endif                                                  TTCTYP IFNE
     C*
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* TAX
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     'T'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDifx
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* SAFETY FEES
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     'S'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDifs
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* MEMBERSHIP FEES
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     'M'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDifm
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* 1ST ASSESSMENT FEES
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     '1'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDif1
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* 2ND ASSESSMENT FEES
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     '2'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDif2
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* 3RD ASSESSMENT FEES
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     '3'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDif3
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* 4TH ASSESSMENT FEES
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     '4'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDif4
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* 5TH ASSESSMENT FEES
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     '5'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDif5
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* 6TH ASSESSMENT FEES
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     '6'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDif6
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* 7TH ASSESSMENT FEES
     C*
     C     EAArty        Ifeq      'F'
     C     TTCtyp        Andeq     '7'
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDif7
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C* INSTALLMENT FEES
     C*
     C     EAArcd        Ifeq      IFIfbl
     C*
     C     EAPodt        Iflt      xxDate
     C                   Add       EAAmt         xaDifi
     C                   Else
     C*** Begin Del *** 01/21/25 **************************************
     C**********         Exsr      SrCXWrit
     C*** End   Del *** 01/21/25 **************************************
     C                   Exsr      HoldAR
     C                   Delete    pea02
     C                   Endif                                                  EAPODT IFLT
     C*
     C                   Endif                                                  EAARTY IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO                                                  *IN60 DOW='0'
     C*
     C* WRITE ADJUSTING RECORDS
     C*
     C     XADIFF        IFNE      *ZERO
     C     QQAR          ANDNE     *BLANKS
     C                   MOVE      QQAR          XXAACD            2
     C                   Z-ADD     XADIFF        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFF IFNE
     C*
     C     XADIFE        IFNE      *ZERO
     C     XXCDEB        ANDNE     *BLANKS
     C                   MOVE      XXCDEB        XXAACD            2
     C                   Z-ADD     XADIFE        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFE IFNE
     C*
     C     XADIFX        IFNE      *ZERO
     C     XXTXCD        ANDNE     *BLANKS
     C                   MOVE      XXTXCD        XXAACD            2
     C                   Z-ADD     XADIFX        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFX IFNE
     C*
     C     XADIFS        IFNE      *ZERO
     C     XXSFCD        ANDNE     *BLANKS
     C                   MOVE      XXSFCD        XXAACD            2
     C                   Z-ADD     XADIFS        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFS IFNE
     C*
     C     XADIFM        IFNE      *ZERO
     C     XXMFBL        ANDNE     *BLANKS
     C                   MOVE      XXMFBL        XXAACD            2
     C                   Z-ADD     XADIFM        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFM IFNE
     C*
     C     XADIF1        IFNE      *ZERO
     C     XX1CDE        ANDNE     *BLANKS
     C                   MOVE      XX1CDE        XXAACD            2
     C                   Z-ADD     XADIF1        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF1 IFNE
     C*
     C     XADIF2        IFNE      *ZERO
     C     XX2CDE        ANDNE     *BLANKS
     C                   MOVE      XX2CDE        XXAACD            2
     C                   Z-ADD     XADIF2        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF2 IFNE
     C     XADIF3        IFNE      *ZERO
     C     XX3CDE        ANDNE     *BLANKS
     C                   MOVE      XX3CDE        XXAACD            2
     C                   Z-ADD     XADIF3        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF3 IFNE
     C*
     C     XADIF4        IFNE      *ZERO
     C     XX4CDE        ANDNE     *BLANKS
     C                   MOVE      XX4CDE        XXAACD            2
     C                   Z-ADD     XADIF4        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF4 IFNE
     C*
     C     XADIF5        IFNE      *ZERO
     C     XX5CDE        ANDNE     *BLANKS
     C                   MOVE      XX5CDE        XXAACD            2
     C                   Z-ADD     XADIF5        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF5 IFNE
     C*
     C     XADIF6        IFNE      *ZERO
     C     XX6CDE        ANDNE     *BLANKS
     C                   MOVE      XX6CDE        XXAACD            2
     C                   Z-ADD     XADIF6        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF6 IFNE
     C*
     C     XADIF7        IFNE      *ZERO
     C     XX7CDE        ANDNE     *BLANKS
     C                   MOVE      XX7CDE        XXAACD            2
     C                   Z-ADD     XADIF7        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF7 IFNE
     C*
     C     XADIFI        IFNE      *ZERO
     C     IFIFBL        ANDNE     *BLANKS
     C                   MOVE      IFIFBL        XXAACD            2
     C                   Z-ADD     XADIFI        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFI IFNE
     C*
     C                   ENDSR                                                  ADJAR
     C*****************************************************************
     C*
     C* WRITE A/R RECORDS
     C*
     C     ARW           BEGSR
     C*
     C                   MOVEL(P)  XXAACD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C*
     C                   z-sub     xxaamt        eaamt
     C*
     C                   callp     WrtWmeap(xxaacd : eldiv : tttype : ttctyp :
     C                                      ttdesc :  eaamt   : xxdate)
     C                   ENDSR                                                  ADJAR
     C*
     C* TRANSFER ANY FEE BALANCES TO A PRIOR YEAR AS WELL.
     C*
     C*****************************************************************
     C     SRFEE         BEGSR
     C*
     C                   Z-ADD     0             XADIFM           11 2
     C                   Z-ADD     0             XADIFS           11 2
     C                   Z-ADD     0             XADIFX           11 2
     C                   Z-ADD     0             XADIF1           11 2
     C                   Z-ADD     0             XADIF2           11 2
     C                   Z-ADD     0             XADIF3           11 2
     C                   Z-ADD     0             XADIF4           11 2
     C                   Z-ADD     0             XADIF5           11 2
     C                   Z-ADD     0             XADIF6           11 2
     C                   Z-ADD     0             XADIF7           11 2
     C                   Z-ADD     0             XADIFI           11 2
     C*
     C     EAKY1         CHAIN     PEA02                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     EAARTY        IFEQ      'Q'
     C*
     C                   MOVEL(P)  EAARCD        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C     *IN60         IFEQ      *OFF
     C*
     C* MEMBERSHIP FEES
     C*
     C     TTCTYP        IFEQ      'M'
     C                   ADD       EAAMT         XADIFM
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C* SAFETY FEES
     C*
     C     TTCTYP        IFEQ      'S'
     C                   ADD       EAAMT         XADIFS
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C* STATE TAX
     C*
     C     TTCTYP        IFEQ      'T'
     C                   ADD       EAAMT         XADIFX
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C* 1ST ASSESSMENT FEES
     C*
     C     TTCTYP        IFEQ      '1'
     C                   ADD       EAAMT         XADIF1
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C* 2ND ASSESSMENT FEES
     C*
     C     TTCTYP        IFEQ      '2'
     C                   ADD       EAAMT         XADIF2
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C* 3RD ASSESSMENT FEES
     C*
     C     TTCTYP        IFEQ      '3'
     C                   ADD       EAAMT         XADIF3
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C* 4TH ASSESSMENT FEES
     C*
     C     TTCTYP        IFEQ      '4'
     C                   ADD       EAAMT         XADIF4
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C*
     C* 5TH ASSESSMENT FEES
     C*
     C     TTCTYP        IFEQ      '5'
     C                   ADD       EAAMT         XADIF5
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C* 6TH ASSESSMENT FEES
     C*
     C     TTCTYP        IFEQ      '6'
     C                   ADD       EAAMT         XADIF6
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C*
     C* 7TH ASSESSMENT FEES
     C*
     C     TTCTYP        IFEQ      '7'
     C                   ADD       EAAMT         XADIF7
     C                   ENDIF                                                  TTCTYP IFEQ
     C*
     C* INSTALLMENT FEES
     C*
     C     EAARCD        IFEQ      IFIFPD
     C     EAARCD        OREQ      IFIFTR
     C     EAARCD        OREQ      IFIFTC                                                   EES
     C                   ADD       EAAMT         XADIFI
     C                   ENDIF                                                  EAARCD IFEQ
     C*
     C                   ENDIF                                                  *IN60 IFEQ
     C*
     C                   ENDIF                                                  EAARTY IFEQ
     C*
     C     EAKY1         READE     PEA02                                  60
     C                   ENDDO                                                  *IN60 DOW='0'
     C*
     C* WRITE ADJUSTING RECORDS
     C*
     C     XADIFM        IFNE      *ZERO
     C     F1MFTR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F1MFTR        XXAACD            2
     C                   Z-ADD     XADIFM        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIFM        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFM IFNE
     C*
     C     XADIFS        IFNE      *ZERO
     C     F1SATR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F1SATR        XXAACD            2
     C                   Z-ADD     XADIFS        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIFS        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFS IFNE
     C*
     C     XADIFX        IFNE      *ZERO
     C     F1TXTR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F1TXTR        XXAACD            2
     C                   Z-ADD     XADIFX        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIFX        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFX IFNE
     C*
     C     XADIF1        IFNE      *ZERO
     C     F11ATR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F11ATR        XXAACD            2
     C                   Z-ADD     XADIF1        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIF1        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF1 IFNE
     C*
     C     XADIF2        IFNE      *ZERO
     C     F12ATR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F12ATR        XXAACD            2
     C                   Z-ADD     XADIF2        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIF2        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF2 IFNE
     C*
     C     XADIF3        IFNE      *ZERO
     C     F13ATR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F13ATR        XXAACD            2
     C                   Z-ADD     XADIF3        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIF3        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF3 IFNE
     C*
     C     XADIF4        IFNE      *ZERO
     C     F14ATR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F14ATR        XXAACD            2
     C                   Z-ADD     XADIF4        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIF4        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF4 IFNE
     C*
     C     XADIF5        IFNE      *ZERO
     C     F15ATR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F15ATR        XXAACD            2
     C                   Z-ADD     XADIF5        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIF5        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF5 IFNE
     C*
     C     XADIF6        IFNE      *ZERO
     C     F16ATR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F16ATR        XXAACD            2
     C                   Z-ADD     XADIF6        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIF6        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF6 IFNE
     C*
     C     XADIF7        IFNE      *ZERO
     C     F17ATR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      F17ATR        XXAACD            2
     C                   Z-ADD     XADIF7        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIF7        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIF7 IFNE
     C*
     C     XADIFI        IFNE      *ZERO
     C     IFIFTR        ANDNE     *BLANKS
     C                   Z-ADD     X1FYR         XXFYR
     C                   MOVE      IFIFTR        XXAACD            2
     C                   Z-ADD     XADIFI        XXAAMT           11 2
     C                   EXSR      ARW
     C                   SUB       1             XXFYR
     C                   Z-SUB     XADIFI        XXAAMT           11 2
     C                   EXSR      ARW
     C                   ENDIF                                                  XADIFI IFNE
     C*
     C                   ENDSR
     C     xfrdeposit    begsr
     C*
     C*
     C* Ensure no writing/adjusting of deposit occurs for client 150.
     C*
     C                   If        xxColt = 'Y'
     C                   Leavesr
     C                   Endif
     C                   add       1             dstime
     C                   z-add     0             x                 3 0
     C                   move      'Y'           xxfrst            1
     C                   move      *zeros        svamt
     C                   move      *zeros        svarty
     C                   move      *zeros        svfyr
     C*
     C*  ACCUMULATE I & H TRANSACTIONS TO BUCKETS BY TYPE & FYR
     C*
     C     eakey3        chain     pea01                              60
     C     *in60         doweq     '0'
     C*
     C                   movel(p)  eaarcd        XXKEY
     C     tbkey         chain     ptt01                              60
     C*
     C     TTCTYP        IFEQ      'D'
     C*
     C     eaarty        IFEQ      'F'
     C     f1tftr        ANDNE     *BLANKS
     C     f1tqtr        ANDNE     *BLANKS
     C     f1tptr        ANDNE     *BLANKS
     C     eaarty        OREQ      'Q'
     C     f1tftr        ANDNE     *BLANKS
     C     f1tqtr        ANDNE     *BLANKS
     C     f1tptr        ANDNE     *BLANKS
     C     eaarty        OREQ      'I'
     C     eaarty        OREQ      'H'
     C     eaarty        OREQ      'R'
     C     eaarty        OREQ      'T'
      *
     C                   SELECT
     C     EAARTY        WHENEQ    'R'
     C                   MULT      -1            EAAMT
     C                   MOVE      'I'           EAARTY
     C     EAARTY        WHENEQ    'T'
     C                   MOVE      'I'           EAARTY
     C                   ENDSL
     C*
     C                   MOVE      'N'           XXOYDM            1
     C     EAFYR         IFNE      XXFYR
      *
      * DO NOT INCLUDE OTHER YEAR DEPOSIT TRANSACTIONS IF THAT YEAR IS FLAGGED
      * AS A PER YEAR DEPOSIT MODE
      *
     C     F1KEY2        SETLL     PF101
     C     F1KEY2        READE     PF101                                  60
     C     F1DPYM        IFEQ      'Y'
     C                   MOVE      'Y'           XXOYDM            1
     C                   ELSE
     C                   ENDIF
      *
     C                   ENDIF
      *
     C     XXCYDM        IFEQ      'N'
      *
     C     EAFYR         IFNE      XXFYR
     C     XXOYDM        ANDEQ     'N'
     C     EAFYR         OREQ      XXFYR
      *
     C     EAARTY        IFNE      SVARTY
     C     EAFYR         ORNE      SVFYR
     C     XXFRST        ANDEQ     'N'
     C*
     C     SVAMT         IFNE      *ZEROS
     C                   ADD       1             X
     C                   Z-ADD     SVFYR         FYR(X)
     C                   MOVEL     SVARTY        TY(X)
     C                   Z-ADD     SVAMT         AMT(X)
     C                   ENDIF                                                  SVAMT <> 0
     C*
     C                   MOVE      EAARTY        SVARTY
     C                   MOVE      EAFYR         SVFYR
     C                   MOVE      *ZEROS        SVAMT
     C                   ENDIF                                                  LEVEL BRK
     C*
     C                   ADD       EAAMT         SVAMT
     C*
     C     XXFRST        IFEQ      'Y'
     C                   Z-ADD     EAFYR         SVFYR
     C                   MOVE      EAARTY        SVARTY
     C                   MOVE      'N'           XXFRST
     C                   ENDIF                                                  XXFRST = 'Y'
     C*
     C                   ENDIF                                                  EAFYR <> XXFYR
     C*
      *
     C                   ELSE
      *
     C     EAFYR         IFEQ      XXFYR
      *
     C     EAARTY        IFNE      SVARTY
      *
     C     SVAMT         IFNE      *ZEROS
     C                   ADD       1             X
     C                   Z-ADD     SVFYR         FYR(X)
     C                   MOVEL     SVARTY        TY(X)
     C                   Z-ADD     SVAMT         AMT(X)
     C                   ENDIF                                                  SVAMT <> 0
      *
     C                   MOVE      EAARTY        SVARTY
     C                   MOVE      EAFYR         SVFYR
     C                   MOVE      *ZEROS        SVAMT
     C                   ENDIF                                                  LEVEL BRK
      *
     C                   ADD       EAAMT         SVAMT
      *
     C     XXFRST        IFEQ      'Y'
     C                   Z-ADD     EAFYR         SVFYR
     C                   MOVE      EAARTY        SVARTY
     C                   MOVE      'N'           XXFRST
     C                   ENDIF                                                  XXFRST = 'Y'
      *
     C                   ENDIF                                                  EAFYR = XXFYR
      *
     C                   ENDIF                                                  XXCYDM = 'N'
      *
     C                   ENDIF                                                  EAARTY = 'F'
     C*
     C                   ENDIF                                                  TTCTYP = 'D'
     C*
     C     EAKEY3        READE     PEA01                                  60
     C                   ENDDO                                                  *IN60
     C*
     C* RESET GROUP DETAIL
     C*
     C     F1KEY         SETLL     PF101
     C     F1KEY         READE     PF101                                  60
     C*
     C*  FORCE LAST TO ARRAYS
     C*
     C     SVAMT         IFNE      *ZEROS
     C                   ADD       1             X
     C                   Z-ADD     SVFYR         FYR(X)
     C                   MOVEL     SVARTY        TY(X)
     C                   Z-ADD     SVAMT         AMT(X)
     C                   ENDIF                                                  XXFRST = 'N'
     C*
     C*  WRITE REVERSING ENTRIES FOR EACH FYR/TYPE
     C*
     c                   clear                   xxamt
     c                   clear                   xtamt
     C                   do        x             z                 3 0
     C                   z-add     fyr(z)        eafyr
     C*  DEPOSIT PAYMENT NEGATION
     C                   select
     C     ty(z)         wheneq    'I'
     C                   move      f1dptr        eaarcd
     C                   movel(P)  f1dptr        xxkey
     C     tbkey         chain     ptt01                              60
     C                   move      tttype        eaarty
     C                   movel(P)  ttdesc        eadesc
     C                   move      ttctyp        eactyp
     C     ty(z)         wheneq    'H'
     C*  DEPOSIT BILLING NEGATION
     C                   move      f1dbtr        EAARCD
     C                   movel(p)  f1dbtr        XXKEY
     C     tbkey         chain     ptt01                              60
     C                   move      tttype        eaarty
     C                   move      ttctyp        eactyp
     C                   movel(p)  ttdesc        eadesc
     C     TY(Z)         WHENEQ    'F'
     C                   move      F1TFTR        eaarcd
     C                   movel(P)  F1TFTR        xxkey
     C     tbkey         chain     ptt01                              60
     C                   move      tttype        eaarty
     C                   move      ttctyp        eactyp
     C                   movel(P)  ttdesc        eadesc
     C     ty(z)         wheneq    'Q'
     C                   move      f1tqtr        eaarcd
     C                   movel(P)  f1tqtr        xxkey
     C     tbkey         CHAIN     ptt01                              60
     C                   move      tttype        eaarty
     C                   move      ttctyp        eactyp
     C                   movel(p)  ttdeSc        eadesc
     C                   endsl
     C*

     c     amt(z)        mult      -1            eaamt
     c                   z-add     EAAMT         xxamt#           15 4
     c                   callp     WrtWmeap( EAARCD :
     c                                       xxdiv  :
     c                                       EAARTY :
     c                                       EACTYP :
     c                                       EADESC :
     c                                       xxamt# :
     c                                       sysDt  :
     c                                       fyr(z)  )
     C
     C*  ACCUMULATE PAYMENT AMOUNTS FOR LATER TRANSFER TO PREMIUM
     C     ty(z)         ifeq      'I'
     C                   add       amt(z)        xxamt
     C                   endif
     C     ty(z)         ifeq      'Q'
     C                   add       amt(z)        xtamt            11 2
     C                   endif
     C                   enddo                                                  DO  X  Z
     C*
     C*  IF THERE IS AN ACCRUED DEPOSIT PAID AMOUNT, TRANSFER IT TO
     C*
     C     XXAMT         IFNE      *ZEROS
     C*
     C     XXAR          IFEQ      *BLANK
     C                   Z-ADD     XSDIV         EADIV
     C                   ELSE
     C                   Z-ADD     XXDIV         EADIV
     C                   ENDIF
     C*
     C                   move      F1PPTR        EAARCD
     C                   movel(P)  F1PPTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   move      TTTYPE        EAARTY
     C                   move      TTCTYP        EACTYP
     C                   moveL(P)  TTDESC        EADESC
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EAPODT
     C                   Z-ADD     XXAMT         xxamt#
     C                   move      eapodt        xxdat#
     C*
     C                   callp     WrtWmeap(eaarcd : eadiv : eaarty : eactyp :
     C                                      eadesc : xxamt# : xxdat#)
     C                   ENDIF                                                  XXAMT <> 0
     C*
     C*  IF THERE IS AN ACCRUED TAX ON DEP PAID AMOUNT, TRANSFER IT.
     C*
     C     XTAMT         IFNE      *ZEROS
     C*
     C     XXAR          IFEQ      *BLANK
     C                   Z-ADD     XSDIV         EADIV
     C                   ELSE
     C                   Z-ADD     XXDIV         EADIV
     C                   ENDIF
     C                   MOVE      F1TPTR        EAARCD
     C                   MOVEL(P)  F1TPTR        XXKEY
     C     TBKEY         CHAIN     PTT01                              60
     C                   MOVE      TTTYPE        EAARTY
     C                   MOVE      TTCTYP        EACTYP
     C                   MOVEL(P)  TTDESC        EADESC
     C                   Z-ADD     DSDATE        SRDTI
     C                   call      'SRCVTDAT'    DTCYMD
     C                   movel     SRDTO         EAPODT
     C                   z-add     XTAMT         xxamt#
     C                   move      eapodt        xxdat#
     C*
     C                   callp     WrtWmeap(eaarcd : eadiv : eaarty : eactyp :
     C                                      eadesc : xxamt# : xxdat#)
     C                   endif                                                  XXAMT <> 0
     C*
     C                   endsr                                                  xfrdeposit
     C* srPrcAudCrd - Process audit credit
     C******************************************************************
     C     srPrcAudCrd   begsr
     C*
     C                   eval      xxdate = *hival
     C                   eval      xxarty = 'D'
     C     eakey4        setgt     pea01
     C     eakey2        readpe    pea01
     C                   dow       not %eof(wmeal4)
     C*
     C                   if        xxdiff = 0
     C                   leave
     C                   endif
     C*
     C                   eval      xxtmp = eaamt * -1
     C                   eval      xxdate = eapodt
     C                   exsr      srWrtCrd
     C*
     C     eakey2        readpe    pea01
     C                   enddo
     C*
     C                   endsr
     C******************************************************************
     C* srWrtCrd   - Write credit transaction
     C******************************************************************
     C     srWrtCrd      BEGSR
     C*
     C                   CLEAR                   PEA01
     C*
     C                   IF        XXDIFF < XXTMP
     C                   EVAL      EAAMT = XXTMP
     C                   ELSE
     C                   EVAL      EAAMT = XXDIFF
     C                   ENDIF
     C*
     C*
      /Free
       If xxar = 'Y';
         Callp     WrtWmeap(qqar : xxdiv : xxtype : xxctyp :
                   xxdesc : eaamt    : sysDt );
      /End-Free
     C                   endif
     C*
     C                   EVAL      XXDIFF = XXDIFF - EAAMT
     C*
     C                   ENDSR
     C*** Begin Del *** 01/21/25 **************************************
     C*================================================================
     C*  SRCXWRIT - Write WMEAP record to WMAXP for future A/R on cancelled policies
     C*================================================================
     C*****SrCxWrit      Begsr
     C*
     C**********         Move      EACO#         XACO#
     C**********         Move      EAFND         XAFND
     C**********         Move      EAFYR         XAFYR
     C**********         Move      EAEMP#        XAEMP#
     C**********         Move      EADIV         XADIV
     C**********         Move      EAARCD        XAARCD
     C**********         Move      EAARTY        XAARTY
     C**********         Move      EADESC        XADESC
     C**********         Move      EAAMT         XAAMT
     C**********         Move      EATRDT        XATRDT
     C**********         Move      EAPODT        XAPODT
     C**********         Move      EAEDIT        XAEDIT
     C**********         Move      EASTAT        XASTAT
     C**********         Move      EAUSER        XAUSER
     C**********         Move      EAPGM         XAPGM
     C**********         Move      EATIME        XATIME
     C**********         Move      EADT          XADT
     C**********         Move      EABUL1        XABUL1
     C**********         Move      EACHK#        XACHK#
     C**********         Move      EANINV        XANINV
     C**********         Move      EALATE        XALATE
     C**********         Move      EAFL01        XAFL01
     C**********         Move      EAFL02        XAFL02
     C**********         Move      EADT01        XADT01
     C**********         Move      EADT02        XADT02
     C**********         Move      EACTYP        XACTYP
     C**********         Move      EARDIV        XARDIV
     C**********         Move      EACCTP        XACCTP
     C**********         Move      EACNUM        XACNUM
     C**********         Move      EACEXP        XACEXP
     C**********         Move      EACNAM        XACNAM
          //*** Beg Add ***** 06/29/22 ***************************
     C**********         Move      EAref#        XAref#
          //*** End Add ***** 06/29/22 ***************************
     C*
     C*              Maintenance fields
     C*
     C**********         Move      q1USER        XAIUSR
     C**********         Move      q1PGM         XAIPGM
     C**********         Move      dsTIME        XAITM
     C**********         Z-add     dsDATE        srDTI
     C**********         Call      'SRCVTDAT'    dtCYMD
     C**********         MoveL     srDTO         XAIDT
     C*
     C**********         Write     pxa0p
     C*
     C**********         Endsr                                                  SrCxWrit
     C**********
     C*** End   Del *** 01/21/25 **************************************
     C*
     C/COPY WBILLCSPEC
     C/COPY WADTTRKCSR
     C*
     OPDS02     E            PDS02E
     OPEL01     E            PEL01E
     OPEP01     E            PEP01E
     C*====================================================================
     C* Get the index
     C*====================================================================
     PSrchIdx          b
     dSrchIdx          pi             3S 0
     d xxpodt                              like(EAPODT)          value
     d @@DS                                like  (@@ds1) dim(99) value
     d*
     d @@DS1           ds                  qualified dim(99)
     d @@amt@                         9s 0
     d @@podt@                        7s 0
     d*
     d found           s              3s 0 inz(0)
     d x               s              3s 0 inz(1)
     c*
     c                   dow        x <= 99
     c                   move      @@ds(x)       @@ds1(x)
     c*
     c                   if         @@ds1(x).@@podt@= xxpodt
     c                   eval       found= x
     c                   leave
     c                   endif
     c                   eval       x = x+1
     c*
     c                   enddo
     c                   return                  found
     P SrchIdx         e
     c*
     C*====================================================================
     C* Sum the elements of Data structure Array.
     C*====================================================================
     PSumAmt           b
     dSumAmt           pi            15s 4
     d @@DS                                like  (@@ds1) dim(99) value
     d*
     d @@DS1           ds                  qualified dim(99)
     d @@amt@                         9s 0
     d @@podt@                        7s 0
     d*
     d TotalBal        s             15s 4
     d x               s              3s 0 inz(1)
     c*
     c*
     c                   do        99            x
     c                   move      @@ds(x)       @@ds1(x)
     c                   eval      TotalBal = TotalBal + @@ds1(x).@@amt@
     c                   enddo
     c                   return                  TotalBal
     P SumAmt          e
     c*
     C*====================================================================
     C* GETINV   - Get the invoice # for billing records being created
     C*====================================================================
     P GetInv          b
     d GetInv          pi
     C*
     C     *like         define    eaco#         #ico#
     C     *like         define    eafnd         #ifnd
     C     *like         define    eaemp#        #iemp#
     C     *like         define    eadiv         #idiv
     C     *like         define    eafyr         #ifyr
     C     *like         define    eapodt        #idt
     C     *like         define    eaninv        #iinv#
     C*
     C                   if        xxclt# = '025'
     C*
     C                   if        eaarty = 'A' or eaarty = 'B' or eaarty ='D'
     C                             or eaarty = 'F' or eaarty = 'H'
     C*
     C                   call      'SRINV#'
     C                   parm      eaco#         #ico#
     C                   parm      eafnd         #ifnd
     C                   parm      eaemp#        #iemp#
     C                   parm      eadiv         #idiv
     C                   parm      eafyr         #ifyr
     C                   parm      eapodt        #idt
     C     eaninv        parm                    #iinv#
     C*
     C                   endif
       Else;
          // Assign Invoice #
          If (hldFnd <> EAfnd Or hldEmp# <> EAemp# Or hldDiv <> EAdiv Or
             hldPoDt <> EApoDt) And finalUpdate = *On;
             // Send Invoice to Invoice Cloud
             If hldInv# <> 0 And ICGroupOpt(EACo#:hldFnd);
                // Is this a state reported division or non-summarized
                If GetStRptDv(EAco#:hldFnd:hldEmp#) = hldDiv Or
                   hldArSm <> 'Y';
                   inJson = '{"type":"billing",' +
                            '"invoice":"'  + %EditC(hldInv#:'X') + '",' +
                            '"company":"'  + %EditC(EAco#:'X')   + '",' +
                            '"group":"'    + %EditC(hldFnd:'X')  + '",' +
                            '"employer":"' + %EditC(hldEmp#:'X') + '",' +
                            '"division":"' + %EditC(hldDiv:'X')  + '",' +
                            '"active":"true"}';
     C*** Beg Chg ***  10/25/23  *** WC2922 *******************************
             //    ProcessAdjustedInvoicing(inJson);
                   If hldCTST = 'Y';
                      ReplaceHeldInvoices(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:
                                          hldInv#:'A');
                   Else;
                      ProcessAdjustedInvoicing(inJson);
                   EndIf;
     C*** End Chg ***  10/25/23  *** WC2922 *******************************
                EndIf;
             EndIf;

             // Get invoice#
             EAnInv = 0;
             Exec Sql Select EAninv Into :EAninv From WMEAP
                      Where EAco#=:EAco# And EAfnd=:EAfnd And
                      EAemp#=:EAemp# And EAdiv=:EAdiv And EAfyr=:EAfyr
                      And EACTyp<> 'C' And EACTyp <> 'R' And
                      EApoDt=:EApoDt And EAarTy in ('A','B','D','F','H');

             If EAninv = 0;
                invoiceFound = *Off;
                For i=1 to 9999;
             //*** begin chg ***  09/26/22  *********************************************
             //****If arrInvoice(i) = 9999999999999;
                   If arrInvoice(i) = 999999999999999;
             //*** end chg *****  09/26/22  *********************************************
                      Leave;
                   Else;
             //*** begin chg ***  09/26/22  *********************************************
             //*****  decDivision =
             //*****     %Dec(%Subst(%EditC(arrInvoice(i):'X'):1:3):3:0);
             //*****  If decDivision = EAdiv;
             //*****     invoice#      =
             //*****        %Dec(%Subst(%EditC(arrInvoice(i):'X'):4:10):10: 0);
             //*****     arrInvoice(i) = 9999999999999;
                      decDivision =
                         %Dec(%Subst(%EditC(arrInvoice(i):'X'):1:5):5:0);
                      If decDivision = EAdiv;
                         invoice#      =
                            %Dec(%Subst(%EditC(arrInvoice(i):'X'):6:10):10: 0);
                         arrInvoice(i) = 999999999999999;
             //*** end chg *****  09/26/22  *********************************************
                         //*** Begin Add *** 09/12/23 *************
                         // Look to see if the invoice # is being used
                         EAnInv = 0;
                         Exec Sql Select EAninv Into :EAninv From WMEAP
                           Where EAco#=:EAco# And EAfnd=:EAfnd And
                           EAemp#=:EAemp# And EAdiv=:EAdiv And EAfyr=:EAfyr
                           And EACTyp<> 'C' And EACTyp <> 'R' And
                           EAninv = :invoice# And
                           EAarTy in ('A','B','D','F','H');
                         If EAninv <> 0;
                            Iter;
                         EndIf;
                         //*** End   Add *** 09/12/23 *************
                         SortA arrInvoice;
                         invoiceFound = *On;
                         Leave;
                      EndIf;
                   EndIf;
                EndFor;

                If Not invoiceFound;
                   inKey = %EditC(EAco#:'X') + %EditC(EAfnd:'X') +
                           %EditC(EAemp#:'X') + %EditC(EAdiv:'X');
                   invoice# = GetNext3rdPartySeq#(inKey);
                EndIf;
             Else;
                invoice# = EAninv;
                //*** Begin Add *** 09/12/23 *******************************
                decInvoiceDiv = (EAdiv * 10000000000) + EAninv;
                index = %LookUp(decInvoiceDiv:arrInvoice);
                If index <> 0;
                    arrInvoice(index) = 999999999999999;
                    SortA arrInvoice;
                EndIf;
                //*** End   Add *** 09/12/23 *******************************
             EndIf;

             // Populate holding fields
             hldFnd  = EAfnd;
             hldEmp# = EAemp#;
             hldDiv  = EAdiv;
             hldPoDt = EApoDt;
             hldInv# = invoice#;
             hldArSm = ELarSm;
          EndIf;

          EAnInv = invoice#;
     C*
     C                   endif
     PGetInv           e
     C*====================================================================
     C* WRTWMEAP - Write Records in WMEAP
     C*====================================================================
     P Wrtwmeap        b
     d Wrtwmeap        pi
     d @@cde                               like(xx1cde)
     d @@@div                              like(xxdiv)
     d @@typ                               like(xx1typ)
     d @@cat                               like(xx1cat)
     d @@des                               like(xx1des)
     d @@amt                               like(xxdif1) value
     d @@podt                              like(xxdate)
     d @@fyr                               like(xxfyr) options(*nopass)
     d*
     C                   move      @@podt        eapodt                         |
     C                   Z-ADD     dsdate        srdti
     C                   CALL      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eatrdt
     C                   movel     @@cde         eaarcd
     C                   movel     @@typ         eaarty
     C                   movel     @@des         eadesc
     C                   movel     @@cat         eactyp
     C                   z-add     @@amt         eaamt
     C*
     C                   move      xxco#         eaco#
     C                   move      xxfnd         eafnd
     c                   if        %parms >= 8
     c                   eval      EAFYR = @@fyr
     c                   else
     c                   eval      EAFYR = xxfyr
     c                   endif
     C                   move      elemp#        eaemp#
      /Free
       EAdiv = @@@div;
       If *In71 = *On;
         EArdiv = %Int(qqdiv);
       Else;
         eardiv = 0 ;
         eardiv = eldiv;
       EndIf;
      /End-Free
     C                   move      'A'           eastat
     C                   movel     q1user        eauser
     C                   movel     Q1PGM         eapgm
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eadt
     C                   add       1             dstime
     C                   z-add     DSTIME        eatime
     C                   move      ' '           eaedit
     C                   z-add     0             eabul1
     C                   z-add     0             eachk#
     C                   z-add     0             eaninv
     C                   move      *blank        ealate
     C*
     C* Get the invoice # for billing records being created
     C                   callp     GetInv
     C*Write records in file WMEAP.
     C                   write     pea01
     PWrtwmeap         e
      *=============================================================
      * ApplyRateSplit - Check option flag to apply rate split
      *=============================================================
     p ApplyRateSplit  b
     d ApplyRateSplit  pi              n
     d   inState                      2a
     d   inDate                       7  0
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
     C                   eval      xxPrcRate = *on
     C     rtky          chain     prt0x
     C                   if        %found and rxopt = 'Y'
     C                   eval      xxPrcRate = *off
     C                   endif
     C*
     C                   return    xxPrcRate
     C*
     p ApplyRateSplit  e
      *=============================================================
      * GetCaPolFee - Returns policy fee to division with highest premium
      *  for state
      *=============================================================
     p GetPolFee       b
     d GetPolFee       pi                  like(ELepf)
     d index           s              5  0
     d xxHighPrem      s                   like(MPanpr)
     d xxHighDiv       s                   like(MPdiv)
      /Free
       If RRpfam = 0
        Or *in94 = *On Or *in95 = *On Or *in96 = *On Or *in97 = *On
        Or ELpfdt = ELptdt;
          Return 0;
       Endif;
       Setll (ELco#:ELfnd:ELfyr:ELemp#) pmp01;
       Dou %Eof;
          Reade (ELco#:ELfnd:ELfyr:ELemp#) pmp01;
          If %Eof;
             Leave;
          EndIf;
          If MPst = xxSt and MPaapr > xxHighPrem;
             xxHighPrem = MPaapr;
             xxHighDiv = MPdiv;
          Endif;
       Enddo;
       If xxHighDiv = ELdiv;
          Return RRpfam;
       Else;
          Return 0;
       Endif;
      /End-Free
     p GetPolFee       e
      *=============================================================
      * prIncLmtBasis - Increased limit basis for assessment calculations
      *=============================================================
     p prIncLmtBasis   b
     d prIncLmtBasis   pi            15  4
     d   inTotLimits                 15  4
     d   inMinimumPrm                15  4
     d   inPrmIncLmts                15  4
     d   inILF                        1
     d   inAssessLvl                  1
     d   inJPrm                      15  4
     d   inEPrm                      11  0
     d   inSPrm                      15  4
     d   inDPrm                      15  4
     d   inXPrm                      15  4
     d   inAMNP                       1
     d   inATR                        1
     d   outFactor                    9  6 options(*nopass)
     d*
     d xxlmtFact       s              9  6 inz
     d outAssessPrm    s             15  4 inz
     c*
     C* Calculate factor field, If minimum premium invoked then use XXNET
     C*
     C                   eval      xxlmtFact=0
     C                   if        inAMNP = 'Y'
     C                   if        inMinimumPrm<>0
     C                   eval(h)   xxlmtFact = inTotLimits/inMinimumPrm
     C                   endif
     C                   else
     C                   if        inPrmIncLmts<>0
     C                   eval(h)   xxlmtFact = inTotLimits/inPrmIncLmts
     C                   endif
     C                   endif
     C*
     C                   if        inILF = 'W'
     C                   eval      xxlmtFact = 1 - xxlmtFact
     C                   endif                                                  inILF = 'W'
     C*
     C                   select
     C                   when      inAssessLvl = 'J'
     C                   eval(h)   outAssessPrm = inJPrm * xxlmtFact
     C                   when      inAssessLvl = 'E'
     C                   eval(h)   outAssessPrm = inEPrm * xxlmtFact
     C                   when      inAssessLvl = 'S'
     C                   eval(h)   outAssessPrm = inSPrm * xxlmtFact
     C                   when      inAssessLvl = 'D'
     C                   eval(h)   outAssessPrm = inDPrm * xxlmtFact
     C                   when      inAssessLvl = 'X'
      * Subtract terrorism and expense constant and apply the IL Factor
     c                   Eval(h)   outAssessPrm =
     c                              (inXPrm - xxPayR - xxADTr - xxExCn)
     c                              * xxLmtFact
      * Add expense constant back into the assessment basis
     c                   Eval      outAssessPrm += xxExCn
      * Add terrorism into premium basis if flagged to do so
     c                   Select
     c                   When      inAtr = 'F'
     c                   Eval      outAssessPrm += xxPayR
     c                   When      inAtr = 'D'
     c                   Eval      outAssessPrm += xxADTr
     c                   When      inAtr = 'B'
     c                   Eval      outAssessPrm += (xxADTr + xxPayR)
     c                   Endsl
     C                   endsl
     C                   If        %parms >= 13
     C                   Eval      outFactor = xxLmtFact
     C                   Endif
     C*
     C                   return    outAssessPrm
     C*
     p prIncLmtBasis   e
      *=============================================================
      * prGetUnEarn   - Get Unearned protion of premium component
      *=============================================================
     p prGetUnEarn     b
     d prGetUnEarn     pi                  like(ELUPSA)
     d   inProPrem                   15  4
     d   inPrmFact                         like(xxPrmFact)
     d   inSurcharge                       like(CRUPSP)
     d*
     d  xxTmp1         s             15  4
     d*
     C                   eval(h)   xxTmp1 = inProPrem * inPrmFact
     C                   eval(h)   xxTmp1 = (xxTmp1 - inProPrem)
     C                                      * (inSurcharge/100)
     C                   if        xxtmp1 < 0
     C                   eval      xxtmp1 = 0
     C                   endif
     C                   return    xxTmp1
     p prGetUnEarn     e
      *===============================================================
      * Don't discount if L&H exposure, maritime class codes, or A-rated
      *===============================================================
     p isDiscounted    b
     d isDiscounted    pi              n
     d  state                         2    const
     d  class                         5  0 const
     d  spec                          1    const
      /free
       Select;
       When ELstat = 'Y' and PRarfl = 'Y';
          Return *off;
       When state = 'TN' or state = 'OK';
          If spec = 'F' or
           class = 6703 or
           class = 7047 or
           class = 7050 or
           class = 7099 or
           class = 7152 or
           class = 7337 or
           class = 7398 or
           class = 8738 or
           class = 8815;
             Return *off;
          Else;
             Return *on;
          Endif;
       Other;
          Return *off;
       Endsl;
      /end-free
     p isDiscounted    e
      *=============================================================
      * ReportUnusedInvoices - Report unused invoices
      *=============================================================
     p ReportUnusedInvoices...
     p                 b
     d ReportUnusedInvoices...
     d                 pi
     d inCo#                          3  0
     d inGroup                        3  0
     d inEmp#                         9  0
     d inFinal                         n   Const

     d index           s              5  0
     d inJson          s          32000a   Varying Inz
     d invoice#        s             10  0
     D*** Begin Chg *** 09/12/23 *******************************************
     d*division*       s              3  0
     d division        s              5  0
     D*** End   Chg *** 09/12/23 *******************************************

      /Free

       For index=1 To 9999;
       //*** begin chg ***  09/26/22  *********************************************
       //*****   If arrInvoice(index) = 9999999999999;
          If arrInvoice(index) = 999999999999999;
       //*** end chg *****  09/26/22  *********************************************
             If index > 1 And inFinal;
                FinalizeThirdPartyInvoicing(inCo#:inGroup);
             EndIf;
             Leave;
          Else;
       //*** begin chg ***  09/26/22  *********************************************
       //****division = %Dec(%Subst(%EditC(arrInvoice(index):'X'):1:3):3:0);
       //****invoice# = %Dec(%Subst(%EditC(arrInvoice(index):'X'):4:10):10:0);
             division = %Dec(%Subst(%EditC(arrInvoice(index):'X'):1:5):5:0);
             invoice# = %Dec(%Subst(%EditC(arrInvoice(index):'X'):6:10):10:0);
       //*** end chg *****  09/26/22  *********************************************
             inJson   = '{"type":"billing",' +
                          '"invoice":"'  + %EditC(invoice#:'X')  + '",' +
                          '"company":"'  + %EditC(inCo#:'X')     + '",' +
                          '"group":"'    + %EditC(inGroup:'X')   + '",' +
                          '"employer":"' + %EditC(inEmp#:'X')    + '",' +
                          '"division":"' + %EditC(division:'X')  + '",' +
                          '"active":"false","delete":"true"}';
     C*** Beg Chg ***  10/25/23  *** WC2922 *******************************
          //   ThirdPartyInvoicing(inJson);
               If hldCTST = 'Y';
                  //*** Begin Chg *** 11/19/24 *************************
                  //*** ReplaceHeldInvoices(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:
                  //***                     invoice#:'A');
                  ReplaceHeldInvoices(xxCo#:xxFnd:xxEmp#:xxDiv:xxFyr:
                                      hldInv#:'A':'D');
                  //*** End   Chg *** 11/19/24 *************************
               Else;
                  ThirdPartyInvoicing(inJson);
               EndIf;
     C*** Beg Chg ***  10/25/23  *** WC2922 *******************************
          EndIf;
       EndFor;

       Reset arrInvoice;

      /End-Free

     p ReportUnusedInvoices...
     p                 e
     p*** Begin Add *** 03/05/25 ****************************************
      *=============================================================
      * RemoveWMXAP - Removes WMXAP and communicate to Invoice Cloud
      *               to remove the invoice.
      *=============================================================
     p RemoveWMXAP     b
     d RemoveWMXAP     pi
     d inCo#                          3  0
     d inFnd                          3  0
     d inEmp#                         9  0
     d inDiv                          5  0

     d strSQL          s           1000
     d moreRows        s               n

      /Free

       strSQL = 'Select rrn(WMXAP), a.* from WMXAP a ' +
                'Where XAco#='  + %EditC(inCo#:'X')    + ' And ' +
                      'XAfnd='  + %EditC(inFnd:'X')    + ' And ' +
                      'XAemp#=' + %EditC(inEmp#:'X')   + ' And ' +
                      'XAdiv='  + %EditC(inDiv:'X');

       Exec Sql Prepare sqlWMXAP From :strSql;
       Exec Sql Declare cursorWMXAP Cursor For sqlWMXAP;
       Exec Sql Open cursorWMXAP;

       // If row was fetched then process
       moreRows = *On;
       DoU moreRows = *Off;

          // Fetch next Record
          Exec SQL fetch next from cursorWMXAP
                   into :rrnWMXAP, :dsWMXAP :WMXAPnull;

          Select;
          When not IsValidFetch(sqlstt);
             moreRows = *off;
             Iter;
          Endsl;

          // Delete WMXAP records
          Exec Sql Delete from wmxap where rrn(wmxap)=:rrnWMXAP;

          // Remove the invoice from Invoice Cloud
          inJson   = '{"type":"billing",' +
             '"invoice":"'  + %EditC(dsWMXAP.XAninv:'X') + '",' +
             '"company":"'  + %EditC(dsWMXAP.XAco#:'X')  + '",' +
             '"group":"'    + %EditC(dsWMXAP.XAfnd:'X')  + '",' +
             '"employer":"' + %EditC(dsWMXAP.XAemp#:'X') + '",' +
             '"division":"' + %EditC(dsWMXAP.XAdiv:'X')  + '",' +
             '"active":"false","delete":"true"}';

           If hldCTST = 'Y';
              ReplaceHeldInvoices(dsWMXAP.XAco#:dsWMXAP.XAfnd:
                                  dsWMXAP.XAemp#:dsWMXAP.XAdiv:
                                  dsWMXAP.XAfyr:dsWMXAP.XAninv:'A':'D');
           Else;
              ThirdPartyInvoicing(inJson);
           EndIf;

       EndDo;

       FinalizeThirdPartyInvoicing(dsWMXAP.XAco#:dsWMXAP.XAfnd);
       Exec SQL close cursorWMXAP;

      /End-Free

     p RemoveWMXAP     e
     p*** End   Add *** 03/05/25 ****************************************
** # MONTHS USING 93 RATES/HARBOR EXPOSURE %
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
** EADESC A/R FILE DESCRIPTION
PREM TRNSF DUE TO REV CX AUDIT
