     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     H**************************************************************************************
     H/TITLE W7351R - Extract Data                                                         *
     H**************************************************************************************
     H*  Description:  Extracts data for submission to state reporting agency. Places data *
     H*                in files to be transferred to tape at a later time.                 *
     H*                                                                                    *
     H*  Called from: W7351C                                                               *
     H*  Calls to:                                                                         *
     H*                                                                                    *
     H*  Rewrite: 07/19/00   created: 06/1/98    Programmer C:87    Project:WC591          *
     H**************************************************************************************
     H* Revisions:                                                                         *
     H*                                                                                    *
     H* 11/21/25 C:333 Support: 119896 (Amwins)                                            *
     H*                WCSTAT - AMX3160199001 state code 9812 and 9848                     *
     H*                appears to be duplicated.                                           *
     H*                 - Cosidering Increased limit premiums from WDMPP instead of WDELP. *
     H*                                                                                    *
     H* 09/24/25 C:333 Support: Phw#119743 (Amwins)                                        *
     H*                WCSTAT - bal to min in USR does not match bill, causing totals error*
     H*                - svmodp is not clearing when min_prem subroutine is skipped.       *
     H*                                                                                    *
     H* 08/28/25 C:65  Support: Phw#119636 (Amwins)                                        *
     H*                Build the merit rating exposure record using WDMPP data on          *
     H*                minimum premium policies. Additionally change the othExpArr         *
     H*                to build the data based on policy minimum flag variable so          *
     H*                divisions pull from WDMPP when appropriate.                         *
     H*                                                                                    *
     H* 08/05/25 C:333 Support: Phw#119313 (SunZ)                     *
     H*                WCSTAT - MA should be including stat code 0277 *
     H*                 for ARAP charges.                             *
     H*                                                               *
     H* 07/31/25 C:333 Support: Phw#113678 (Amwins)                   *
     H*                WCSTAT - balance to min not being reported on  *
     H*                 USRs causing totals errors.                   *
     H*                                                               *
     H* 12/03/24 C:333 Support: Phw#117627 (Amwins)                   *
     H*                WCSTAT - Split indicator is 0 for both splits  *
     H*                - Exp. Mod <> 0 if EffMod <> 0.                *
     H*                                                               *
     H* 08/14/24 C:65  Project: Phw#117111 (Amwins)                   *
     H*                Pull the policy effective date for the policy  *
     H*                from the earliest found on policy year instead *
     H*                of from the state reporting division.          *
     H*
     H* 07/16/24 C:300 Support: 114004 (AmWins)
     H*                WCSTAT - Increased limit reporting does not match final
     H*                bill causing totals error
     H*
     H* 06/17/24 C:344 Project: WC2952
     H*                Increase Rate field to zone 8.3.
     H*
     H* 04/09/24 C:079 Support: 148/112928
     H*                For split mod 0 use Policy Effective date for Mod and Rate
     H*
     H* 04/18/24 C:291 Project: WC2961
     H*                Report Exposure record for Schedule DB/CR when applied
     H*                against Manual Premium.
     H*
     H* 03/25/24 C:079 Ticket: 115269(150)
     H*                Position CSZ correctly in the address field SRaddr
     H*
     H* 03/13/24 C:079 Ticket: 114941(150)
     H*                Do not report stat code 0932 for NY
     H*
     H* 02/23/24 C:079 Support: 148/115056
     H*                For no exposure, 1111, set mod/rate eff date to
     H*                Policy From Date.
     H*
     H* 09/20/23 C:062 Ticket: 114003(148)
     H*                Add class code premium to standard premium if class
     H*                code is 174 and MN
     H*
     H* 06/10/22 C:062 Ticket: 102512(148)
     H*                Correct chain to wmsqp
     H*
     H* 05/11/22 C:062 Ticket: 98304(136)
     H*                Make sure to use the current emp# rather the previous
     H*                reported emp# of a given claim
     H*
     H* 02/07/22 C:062 Ticket: 96282(148)
     H*                Non-exposre records need exp mod, exp mod date and
     H*                rating date.
     H*
     H* 05/27/21 c:062 Project 150WFG
     H*                Populate state claim #
     H*
     H* 12/14/20 C:062 Ticket: 93392(148)
     H*                Don't report other exposures if CA unless it is
     H*                terrorism
     H*
     H* 12/10/20 C:062 Project: 89WCR
     H*                Add Exclude from ALAE logic.
     H*
     H* 12/08/20 C:062 Ticket: 93208(133)
     H*                Expand the definition of uncooperative audit flag.
     H*
     H* 11/24/20 C:062 Ticket: 70220(150)
     H*                Don't issue minimum premium exposure if no minimum
     H*                premium.
     H*
     H* 11/18/20 C:180 Project WC2757
     H*                Recompiled only.
     H*
     H* 09/29/20 C:62  Ticket: 91739(136)
     H*                Correct employer assignment.
     H*
     H* 09/10/20 C:62  Ticket: 90912(150)
     H*                Set interstate flags to N and off if NJ.
     H*
     H* 08/11/20 C:62  Ticket: 90641(129)
     H*                Assign experience dt to rating date for MA and NH.
     H*
     H* 08/05/20 C:65  Project: WC2756
     H*                Flag payroll classes with 00 exposure act if
     H*                it has been specified on the payroll table.
     H*                                                                                    *
     H* 04/23/20 C:062 Ticket: 88770(148)                                                  *
     H*                Expand the Flat Cancel Logic to look at Cx Date.                    *
     H*                                                                                    *
     H* 04/22/20 C:062 Project WC2743                                                      *
     H*                Add Catastrophe processing                                          *
     H*                                                                                    *
     H* 03/17/20 C:295 Ticket: 82033 To revert back WC2211 , this is for reporting to      *
     H*                        bureau removing out new coding and pull from table as       *
     H*                        it was earlier                                              *
     H*                                                                                    *
     H* 09/19/19 C:180 Ticket: 82043 Retrieve reserve category before making decisions     *
     H*                              based on the reserve category.                        *
     H*                                                                                    *
     H* 09/11/19 C:062 Ticket: 82964(150)                                                  *
     H*                Rate eff date has to equal pol eff date for NJ                      *
     H*                                                                                    *
     H* 09/11/19 C:062 Ticket: 82873(150)                                                  *
     H*                Fix ded plan.                                                       *
     H*                                                                                    *
     H* 08/20/19 C:062 Ticket: 81640(148)                                                  *
     H*                Use the correct value from correct division          o small to hold*
     H*                                                                                    *
     H* 08/20/19 C:062 Ticket: 81781(148)                                                  *
     H*                Don't clear jurisdiction state.                      o small to hold*
     H*                                                                                    *
     H* 05/24/19 C:298 Ticket: 79332                                                       *
     H*                GA short rate penalties to be replaced by                           *
     H*                stat code 9776.                                                     *
     H*                                                                                    *
     H* 06/05/19 C:062 Ticket: 71764(150)                                                  *
     H*                Don't clear jurisdiction state.                      o small to hold*
     H*                                                                                    *
     H* 05/08/19 C:62  Ticket: 77630(150)                                                  *
     H*                -Apply modifier to NY policies                                      *
     H*                -Rates datres and DE policies                                       *
     H*
     H* 04/30/19 C:062 Project: 152WAB
     H*                Apply Optacomp Mods.
     H*                                                                                    *
     H* 04/30/19 C:300 Ticket: 78358(150)                                                  *
     H*                Fix error - 'The target for a numeric operation is too small to hold*
     H*                the result'. Enlarge data field man_prem.                           *
     H*                                                                                    *
     H* 04/22/19 C:65  Ticket: 77630(150)                                                  *
     H*                Populate modifier/eff date on exposure if large deductible has      *
     H*                stat code of 9664 - but do not change subject premium.              *
     H*                                                                                    *
     H* 03/19/19 C:62  Ticket: 77624(150)                                                  *
     H*                Populate employee leasing if TX and lease id <> 1                   *
     H*                                                                                    *
     H* 03/12/19 C:62  Project: 150WAU(71764)                                              *
     H*                Use CMPDS# for SUNZ.                                                *
     H*                                                                                    *
     H* 02/01/19 C:291 Project: WC2206                                                     *
     H*                Report a zero for exposures effective as of the policy effective    *
     H*                date if the first modifier is not rated for PA, DE and CA.          *
     H*                                                                                    *
     H* 11/26/18 C:79  Project: 136WHX                                                     *
     H*                For client 136 send entire TPA Claim #                              *
     H*                                                                                    *
     H* 08/13/18 C:53  Project: WC1917                                                     *
     H*                Part code field change                                              *
     H*                                                                                    *
     H* 07/10/18 C:062 Ticket: 70589(136)                                                  *
     H*                Enahance CA Subro processing                                        *
     H*                                                                                    *
     H* 05/30/18 C:062 Ticket: 71277(136)                                                  *
     H*                Initialize recovery fields.                                         *
     H*                                                                                    *
     H* 01/30/18 C:291 Project: WC2560                                                     *
     H*                NY Premium Adjustment eff 10/17                                     *
     H*                                                                                    *
     H* 01/24/18 C:65  Project: Phw# 51497 (CareWest)                                      *
     H*                Do not report a 1.000 for no modifier on CA units.                  *
     H*                                                                                    *
     H* 01/23/18 C:65  Project: Phw# 63577 (CareWest)                                      *
     H*                Populate the exp mod effective date on short rate exposure records  *
     H*                for CA - process this by using the 1st section of code in short     *
     H*                rate subroutine and clearing out modifier.                          *
     H*                                                                                    *
     H* 01/02/18 C:267 Project: C150WCI                                                    *
     H*                Add new field Business Segment ID#                                  *
     H*                                                                                    *
     H* 09/22/17 C:65  Project: Phw# 48264 (LCTA)                                          *
     H*                Include waiver surcharge to exposure records to ignore when         *
     H*                determining if '1111' no exposure should be written.                *
     H*                                                                                    *
     H* 04/07/17 C:65  Project: Phw# 46240 (LCTA)                                          *
     H*                Include balance to increased limits minimum to exposure records to  *
     H*                ignore when determining if '1111' no exposure should be written.    *
     H*                                                                                    *
     H* 11/01/16 C:65  Project: Phw# 43707 (CareWest)                                      *
     H*                Zero out the modifier on short rate exposure records on standard    *
     H*                premium for CA.                                                     *
     H*                                                                                    *
     H* 08/17/16 C:298 Project: WC2070                                                     *
     H*                Add processing for Uncooperative Audit.                             *
     H*                                                                                    *
     H* 04/12/16 C:65  Project: Phw# 40276 (CoveRisk)                                      *
     H*                Report assessment exposure records for MA with exp mod populated.   *
     H*
     H* 12/07/15 C:180 Support: 31542
     H*                Fix Medical and indemnity amounts for subrougation in state of CA.  *
     H*                                                                                    *
     H* 12/03/15 C:65  Project: 45WKP                                                      *
     H*                If processing a policy effective prior to 1/1/2016 for LCTA then    *
     H*                use old Carrier #.                                                  *
     H*                                                                                    *
     H* 11/06/15 C:65  SUPPORT: Phw# 37907 (CSI)                                           *
     H*                Remove PA and DE from list of states restricted from receiving a    *
     H*                '1111' no reported exposure record.                                 *
     H*                                                                                    *
      * 10/20/15 C:302 PROJECT: WC2364
      *                Add non-ratable class code processing
      *
     H* 01/14/15 C:282 Project: WC2334
     H*                Process to read the state code table to
     H*                determine if a state code would be reported
     H*                if there is no exposure amount. Removed
     H*                the hardcoded assessment & terrorism handling
     H*                for NY state.
     H*
     H* 07/16/14 C:136 Project: WC2295
     H*                Add OK Rate Split logic
     H*
     H* 06/16/14 C:136 Project: WC2283
     H*                Add TN Rate Split logic
     H*
     H* 01/27/14 C:242 Support Seq# 84119                             *
     H*                Do not include flat cancelled policies.        *
     H*                                                               *
     H* 12/23/13 C:136 Support: WA146 84135                                                *
     H*                Include adjustments to Reserve History written by Post Net          *
     H*                to balance net deductible recoveries to deductible billed           *
     H*                per GA requirements                                                 *
     H*                                                                                    *
     H* 11/25/13 C:282 PROJECT 129WSC                                                      *
     H*                Replace legal literal with control field                            *
     H*                for cltid 129 only. (Changes already exist in base)                 *
     H*                                                                                    *
     H* 11/13/13 C:65  SUPPORT: WA147/84011                                                *
     H*                Rate effective date to be populated for all states as manual says   *
     H*                it is a required field. Same with experience modification date.     *
     H*                                                                                    *
     H* 10/15/13 C:136 Project: WC2184                                                     *
     H*                Report the policy effective date as the rate                        *
     H*                date on NC exposure records instead of using                        *
     H*                the payroll effective date                                          *
     H*                                                                                    *
     H* 10/08/13 C:65  Project: WC1957                                                     *
     H*                Add coding to report the harbor exposure                            *
     H*                balance to minimum record if a stat code has                        *
     H*                been entered on the harbor exposure file.                           *
     H*                                                                                    *
     H* 09/02/13 C:190 Project 147WCB                                                      *
     H*                For OptaComp:                                                       *
     H*                - Remove the "-" (dash) from the alternate claim number.            *
     H*                - Truncate the left-most digit from the alternate claim number.     *
     H*                                                                                    *
     H* 07/15/13 C:282 Project: WC2211                                                     *
     H*                Apply LCM from each payroll class when                              *
     H*                calculating terrorism rate.                                         *
     H*                                                                                    *
     H* 04/08/13 C:190 Project 147WBF                                                      *
     H*                Added a condition that will populate SLCLM# with CMPDS#             *
     H*                if FNGRPASC = 'OPTACOMP'and CMPDS# <> *blanks.                      *
     H*
     H* 02/06/13 C:189 SUPPORT WA133 81632/80310
     H*                Rate effective date and Exp mod eff date should always
     H*                be populated for LA, MS and AR
     H*                                                                                    *
     H* 02/01/13 C:180 SUPPORT WA099 80861
     H*                Modify reporting of no exposures to include
     H*                policies with no exposures, but that have
     H*                reported an expense constant.
     H*
     H*                Remove no-exposure reporting exclusion for
     H*                state of MI.
     H*
     H*                Clean up old code of ChkExpo.
     H*
     H*
     H* 11/09/12 C:283 SUPPORT WA102 80965                                                 *
     H*                Rate effective date and Exp mod eff date should be same             *
     H*                for exposure records reported to 'TN', 'SC' and 'VA'.               *
     H*                                                                                    *
     H* 11/09/12 C:283 SUPPORT WC102 80064                                                 *
     H*                Populate Rate effective date and Exp mod eff date                   *
     H*                for exposure records reported to 'TN', 'SC' and 'VA'.               *
     H*                                                                                    *
     H* 11/02/12 C:180 SUPPORT WA133 80310
     H*                Set default for exp act code in SplitPyrl PROC
     H*
     H* 10/03/12 C:180 SUPPORT WA133 80310                                                 *
     H*            283 Fix invalid rate effective date and exp mod eff date                *
     H*                for sched rating pgm and waiver of subrogation                      *
     H*                reported to LA.                                                     *
     H*                                                                                    *
     H* 10/19/12 C:283 SUPPORT WA089 80873                                                 *
     H*                Correct the Condition to Apply Flat Rate Adjustment                 *
     H*                                                                                    *
     H* 09/13/12 C:180 SUPPORT 78938                                                       *
     H*                Change NCCI table - Use id WCST                                     *
     H*                                                                                    *
     H* 07/11/12 C:180 WC2123 - Restore changes under support 71882, but
     H*                qualify based on NCCI table entry found.
     H*
     H* 01/11/12 C:180 SUPPORT 71882 - Remove initial changes for now until
     H*                P & K provides details for all states.
     H*
     H* 11/24/11 C:190 PROJECT: WC1953 - PRTSK 11                                          *
     H*                The standard premium total for the unit total record should be      *
     H*                normal premium + expense constant + deductible credit #1 +          *
     H*                deductible credit #2, if the state is CA.
     H*                                                                                    *
     H* 10/13/11 C:191 Project: WC2079                                                     *
     H*                IL Premium Adjustment eff 9-1-11                                    *
     H*                                                                                    *
     H* 09/06/11 C:180 Support 77118
     H*                Added code to prevent the assessments 6 & 7 and      rted to CA.    *
     H*                Min Prem from being reported for state of CA.
     H*
     H* 08/30/11 C:180 Support 77753
     H*                Report 2nd Injury fund surcharge for NJ
     H*
     H* 07/13/11 C:061 PROJECT WC2037                                                      *
     H*               - Modify WCSTAT extract process to report surcharge                  *
     H*                 as exposure record using stat code in WTCRP                        *
     H*                 (CRUPSC).                                                          *
     H*               - If cancellation code has                                           *
     H*                 unearned premium surcharge stat code setup and                     *
     H*                 unearned premium surcharge is not equal to zero.                   *
     H*                 Then report surcharge as exposure.                                 *
     H*                                                                                    *
     H* 05/26/11 C:180 SUPPPORT 71882
     H*                Fix class effective date
     h*
     H* 04/07/11 C:190 SUPPORT 76471
     H*                Initialized STRTCG numeric values to avoid Decimal data error       *
     H*                                                                                    *
     H* 04/06/11 C:255 SUPPORT 71759                                                       *
     H*                Compared EFFMOD against ELBPFDT, stop being overriden earliest      *
     H*                pol. eff date with policy effective date of division.               *
     H*                                                                                    *
     H* 03/31/11 C:180 SUPPORT 74290
     H*                Fix error dealing with missing P record.
     H*
     H* 03/09/11 C:189 Support: WA133 76059                                                *
     H*                Incorrect Waiver is being reported to NCCI. Experiance Modifier     *
     H*                information should be populated from ELWVFL instead of RRWVFL       *
     H*                                                                                    *
     H* 02/25/11 C:180 SUPPORT 74871                                                       *
     H*                Do not clear exp-mod eff date when exp mod factor
     H*                is cleared (Applies to NY short rate canc).
     H*
     H* 02/24/11 C:180 SUPPORT 74871                                                       *
     H*                Clear exp mod for NY short-rate - Addt'l clearing.
     H*
     H* 02/03/11 C:180 SUPPORT 75353                                                       *
     H*                Fix discrepancy between indmenity paid and incurred
     H*                for closed claims.
     H*
     H* 01/13/11 C:180 SUPPORT 74871                                                       *
     H*                Clear exp mod for NY short-rate.
     H*                                                                                    *
     H* 01/10/11 C:53  Project: 45WIE
     H*                Remove project 45WEV
     H*                                                                                    *
     H* 01/02/11 C:189 SUPPORT 75221                                                       *
     H*                Earlist Policy effective date across all divisions within the same
     H*                state should be used for discount exposures on additional divisions.*
     H*                                                                                    *
     H*  11/23/10  C:189 Project 139WPQ                                                    *
     H*                  Ignore state restrictions for populating data (such as CA exposure
     H*                  restrictions) and disregard groups flagged as test groups for 'F' *
     H*                  mode                                                              *
     H*                                                                                    *
     H* 11/10/10 C:65  PROJECT: 139WPL                                                     *
     H*                Report HCN credit as an exposure record.                            *
     H*                                                                                    *
     H* 10/22/10 C:65  PROJECT: Supp#74585/102WFC                                          *
     H*                Correct the logging class changes to move the '102' client check.   *
     H*                The client check was preventing class codes with a 0 rate from      *
     H*                being properly processed for all other clients.                     *
     H*                                                                                    *
     H* 10/18/10 C:251 Project: WC1939                                                     *
     H*                09/01/2010 OK Rate split                                            *
     H*                If a policy with OK coverage is unexpired as of 09/01/10 then the   *
     H*                policy will need to be split for the rate decrease                  *
     H*                                                                                    *
     H* 10/05/10  C:189 PROJECT WC1776                                                     *
     H*                 The old changes of WC1776 dated 09/07/09 has been removed          *
     H*                 from this version                                                  *
     H*                                                                                    *
     H* 10/14/10 C:190 PROJECT: WC1953                                                     *
     H*                Clear the Social Security # in the Loss records if the              *
     H*                submission state = CA                                               *
     H*                                                                                    *
     H* 09/07/10 C:255 PROJECT: WC1929                                                     *
     H*                Add changes in program to check flag 'Disable DE Rate flag' based   *
     H*                on this, changes for project WC1768 are disabled.                   *
     H*                                                                                    *
     H* 08/17/10 C:190 PROJECT: WC1899 (Priss#26)                                          *
     H*                Report correct Stat codes for separate records.                     *
     H*                                                                                    *
     H* 07/08/10 C:65  PROJECT: Supp#70560                                                 *
     H*                Report terrorism records to NJ even if there is no amount.          *
     H*                                                                                    *
     H* 07/06/10 C:191 PROJECT: WC1899                                                     *
     H*                Report separate records for terrorism and                           *
     H*                assessment amounts if rate changes between                          *
     H*                periods                                                             *
     H*                                                                                    *
     H* 06/21/10 C:255 PROJECT: 114WVY                                                     *
     H*                Add changes when deviations are split                               *
     H*                In OTHEXPARR Subroutine: Add audited and estimated amount           *
     H*                                      fields of 2nd period deviations from WDMPP &  *
     H*                                      WDELP files in OthExpArr.
     H*                In SRMERIT Subroutine: Add audited and estimated amount             *
     H*                                      fields of 2nd period deviations from WDMPP &  *
     H*                                      WDELP files in XXMRRT.                        *
     H*                                                                                    *
     H* 05/11/10 C:65  PROJECT: Supp#72307                                                 *
     H*                Sort the rate date array to ensure a policy with two splits uses    *
     H*                the proper rating date for each period.                             *
     H*                                                                                    *
     H* 04/26/10 C:180 support: 71695 Change class code to 064 for state                   *
     H*                of MA.
     H*
     H* 03/12/10 C:235 PROJECT: 102WFC                                                     *
     H*                ADDITION CLASSES FOR LOGGING CALC                                   *
     H*                                                                                    *
     H* 03/03/10 C:232 Project:  139WNH                                                    *
     H*                Update all of the initial maintenance fields.                       *
     H*                Populating RHIDT and RHDT with the current system date.             *
     H*                                                                                    *
     H* 02/12/10 C:65  PROJECT: Supp#71557/68856                                           *
     H*                Check claim history for determining the claim open/closed status    *
     H*                since the status on claim master can change after the reporting     *
     H*                month.  Also for NJ, if a claim is closed as medical only and no    *
     H*                medical reported, submit the 'P' record only to alert them to close *
     H*                the claim.                                                          *
     H*                                                                                    *
     H* 01/12/10 C:65  PROJECT: WC1838                                                     *
     H*                LARGE DEDUCTIBLES - USE NEW FILE FOR LARGE                          *
     H*                DEDUCTIBLE POLICIES, BASED ON BILLING SCHED CD.                     *
     H*                                                                                    *
     H* 12/29/09 C:136 Project: WC1861                                                     *
     H*                AZ rate split for 01/01/2010                                        *
     H*                                                                                    *
     H* 11/23/09 C:136 Project: 139WNR                                                     *
     H*                FL rate split for 07/01/09 - client specific                        *
     H*                                                                                    *
     H* 09/30/09 C:65  Project: Supp  WA135 69911                                          *
     H*                Chain to deductible table file before writing other exposure to     *
     H*                ensure proper information is used.                                  *
     H*                                                                                    *
     H* 08/26/09 C:65  Project: Supp  WA140 67321                                          *
     H*                Add the deductible recovery amounts back in to the incurred totals  *
     H*                in the case of net deductible reporting, to report gross amounts    *
     H*                to match the paid figures.                                          *
     H*                                                                                    *
     H*  08/05/09  C:65  Project: 114WNI                                                   *
     H*                 -Swap fields ICR for testing.                                      *
     H*                                                                                    *
     H* 07/27/09 C:65  Project: Supp  WA114 69176                                          *
     H*                Do not multiple the payroll for a population based class by 10 or   *
     H*                the premium by 100.                                                 *
     H*                                                                                    *
     H* 06/22/09 C:65  Project: Supp  WA139 67445                                          *
     H*                If short rate is not being applied to standard premium then         *
     H*                populate the rating and mod information on the exposure record.     *
     H*                                                                                    *
     H* 06/08/09 C:65  Project: Supp  WA114 68512                                          *
     H*                Change the check for previous loss records to read backwards        *
     H*                instead of just checking the prior year's records.                  *
     H*                                                                                    *
     H* 04/22/09 C:65  Project: Supp  WA114 66041                                          *
     H*                Apply the LCM to the terrorism rates before                         *
     H*                reporting the exposure.                                             *
     H*                                                                                    *
     H* 04/20/09 C:65  Support: Support#66779                                              *
     H*                If a claim is currently open then report it, regardless of changes  *
     H*                to the loss data.                                                   *
     H*                                                                                    *
     H* 04/20/09 C:190 Project: WC1473                                                     *
     H*                Use WMPXP to get extension description                              *
     H*                                                                                    *
     H* 03/20/09 C:65  Support: Support#66370                                              *
     H*                When processing CHKEXPO subroutine, verify the state from the       *
     H*                WMSRP file since juris_st is not valid at that time.                *
     H*                                                                                    *
     H* 02/04/09 C:65  Project: WC1635                                                     *
     H*                Ensure NY Rate Adjustment is not reported as a negative value.      *
     H*                                                                                    *
     H* 01/19/09 C:65  Support: Support#66093                                              *
     H*                Report payrolls with zero exposure to CA to allow policies with no  *
     H*                exposure to be reported.                                            *
     H*                                                                                    *
     H* 12/16/08 C:136 Project: WC1768                                                     *
     H*                DE rate split for 10/01/08                                          *
     H*                                                                                    *
     H* 12/11/08 C:33  Project: WC1769                                                     *
     H*                Add 6th and 7th assessments.                                        *
     H*                                                                                    *
     H* 11/10/08 C:65  Support: Support                                                    *
     H*                Do not report claim status with '2' to NC and add CA to the prior   *
     H*                record as it was missing there.                                     *
     H*                                                                                    *
     H* 11/06/08 C:220 Project: WC1635                                                     *
     H*                WCSTAT NY Rate Adjustment                                           *
     H*                                                                                    *
     H* 07/18/08 C:65  Support: 126WCD/Supp#64178                                          *
     H*                Add code to accumulate the second deductible amount with the first  *
     H*                deductible amount, as there is no place to report 2nd deductible    *
     H*                rate.                                                               *
     H*                                                                                    *
     H* 05/13/08 C:15  Support: WC126/63420                                                *
     H*                Added code to load mod and rate dates for all exposure records for  *
     H*                NY policies.                                                        *
     H*                                                                                    *
     H* 05/02/08 C:15  Project: 114WNI                                                     *
     H*               -Changed ICR calculation to prevent rounding issues.                 *
     H*               -Changes for NJ ICR reporting.                                       *
     H*                                                                                    *
     H* 04/01/08 C:183 PROJECT: WC1619                                                     *
     H*                Call SREFFD to set the effective date to be                         *
     H*                used on SETGT.                                                      *
     H*                                                                                    *
     H* 03/13/08 C:15  Support: WC135                                                      *
     H*                Per NJ, exposure act code must be 01 for non-payroll exposure.      *
     H*                                                                                    *
     H* 02/22/08 C:15  Project: 114WJG                                                     *
     H*                Removed code that automatically changes the NCCI to '02' for PA.    *
     H*                                                                                    *
     H* 01/14/08 C:15  Support: WC139/61805                                                *
     H*                Added code to remove exposure records if the stat code is '0000'.   *
     H*                                                                                    *
     H* 12/31/07 C:15  Project: 114WJG                                                     *
     H*                Added code to load the exp mod date and rate date on exposure       *
     H*                records applied after the modified premium, for PA and DE.          *
     H*                                                                                    *
     H* 12/13/07 C:183 Project: 139WCI                                                     *
     H*                If FNPEOF = ' ' then add a call in SRADD1 and repopulate SRNAME     *
     H*                with QQNAME.                                                        *
     H*                                                                                    *
     H* 11/29/07 C:15  Support: WC139/60647                                                *
     H*                Added code to prevent duplicate exposure records if a split period  *
     H*                has no premium.                                                     *
     H*                                                                                    *
     H* 11/26/07 C:190 Project: WC1590                                                     *
     H*                Provide more accurate data in the "P" type loss records             *
     H*                                                                                    *
     H* 10/26/07 C:190 WC1588                                                              *
     H*                Changed to load the indemnity(slpdin) and medical(slpdmd)           *
     H*                paid amounts from the new SITF amounts, if the SITF amounts are     *
     H*                not 0 and effevtive prior to the extract month/year.
     H*                                                                                    *
     H* 10/18/07 C:15  Support: WC129/59945                                                *
     H*                Added code to load the exp mod date and rate date on exposure       *
     H*                records that these fields don't apply to for MA.                    *
     H*                                                                                    *
     H* 10/11/07 C:190 SUPPORT WA099 54504                                                 *
     H*                MAKE SURE TO DO NOT ACCESS THE DEDUCTIBLE                           *
     H*                DISCOUNT CODE HAVING INACTIVE RECORDS.                              *
     H*                                                                                    *
     H* 10/03/07 C:15  Support: WC139/59808                                                *
     H*                Added code to write a '1111' record for policies that have multiple *
     H*                states and some of the states did not develop exposure.  The '1111' *
     H*                exposure record will be assigned to the states with no exposure.    *
     H*                                                                                    *
     H* 10/02/07 C:15  Support: WC132/60368                                                *
     H*                Claim status '2' is not valid for 'CA'.                             *
     H*                                                                                    *
     H* 09/26/07 C:15  Support: WC139/59671                                                *
     H*                Added code to correct the IL balance to minimum calculation.        *
     H*                                                                                    *
     H* 09/24/07 C:15  Project: 102WEB                                                     *
     H*                Added code to process the 32702 class code the same as 12702 and    *
     H*                22702.                                                              *
     H*                                                                                    *
     H* 09/14/07 C:15  Project: 114WHO                                                     *
     H*                Added code to write the retro premium exposure record.              *
     H*                                                                                    *
     H* 09/07/07 C:15  Project: 114WJG                                                     *
     H*                Corrected code so ALAE is not included in the previous incurred amt *
     H*                on claims for NJ policies.  On claims with Ind or Med incurred amts *
     H*                are reported.                                                       *
     H*                                                                                    *
     H* 08/10/07 C:15  Project: 114WJG                                                     *
     H*                Added code to calculate the correct monthend date for the prior     *
     H*                WCSTAT submission, so the correct "prior" claim amts are calculated.*
     H*                                                                                    *
     H* 07/31/07 C:15  Project: 114WJG                                                     *
     H*                Added code to load the rate for the Domestic terrorism exposure     *
     H*                when reporting to PA/DE.                                            *
     H*                                                                                    *
     H* 07/10/07 C:15  Support: WC139/57424                                                *
     H*                Changed pgm to apply the minimum premium to the state reporting div *
     H*                on multi-state policies with min prem.                              *
     H*                                                                                    *
     H* 06/22/07 C:15  Support: WC102/56991                                                *
     H*                Added code to load the correct rate date for non-payroll exposure   *
     H*                on split mod policies.                                              *
     H*                                                                                    *
     H* 06/20/07 C:15  Support: WC114/57247                                                *
     H*                Managed care credit was not being included in the min prem calc due *
     H*                to RRDV1L not being loaded correctly in the pgm.                    *
     H*                                                                                    *
     H* 06/19/07 C:15  Support: WC093/58445                                                *
     H*                Chgd to prevent premium figures from being loaded in the Unit Total *
     H*                record on subsequent reports.                                       *
     H*                                                                                    *
     H* 04/24/07 C:65  Project: support                                                    *
     H*                Do not subtract stock discount from subject premium when its        *
     H*                applied to modified manual, as the discount is applied after std.   *
     H*                                                                                    *
     H* 04/06/07 C:65  Project: wc1514                                                     *
     H*                Pull the short rate penalty premium from new emplr detail field.    *
     H*                                                                                    *
     H* 02/26/07 C:15  Project: 114WJG                                                     *
     H*                Chgd short rate exposure record to exclude the exp mod date.        *
     H*                                                                                    *
     H* 02/07/07 C:15  Support: WC102/57027                                                *
     H*                Chgd premium calc for class codes 12702 and 22702.                  *
     H*                                                                                    *
     H* 01/17/07 C:185 Project: 114WNI                                                     *
     H*                Rpt ICR Data (Individual Case Report) NJ.                           *
     H*                                                                                    *
     H* 12/21/06 C:15  Support: WC114/51114                                                *
     H*                Chgd pgm to create "Increased limits, balance to minimum" exposure  *
     H*                records.                                                            *
     H*                                                                                    *
     H* 11/15/06 C:15  Project: 139WFS                                                     *
     H*                Chgd pgm to process all employers together if the PEO type is a     *
     H*                GIC Master policy.                                                  *
     H*                                                                                    *
     H* 10/28/06 C:15  Support: WC102/55491                                                *
     H*               -Chgd pgm to load the claim catastrophe # (CMCATA) into the loss     *
     H*                record.                                                             *
     H*                                                                                    *
     H* 10/27/06 C:15  Project: 114WNP                                                     *
     H*                Added code to prevent loss code '05' from being sent on PA/DE claims*
     H*                that have been open more than 1 year.  Code '02' will be used.      *
     H*                                                                                    *
     H* 10/26/06 C:15  Support: WC114/54094                                                *
     H*               -Chgd pgm to not include non-payroll exposure in the exposure total  *
     H*                for the policy.                                                     *
     H*               -Chgd pgm to not load the exp mod on PA/DE policies if the premium   *
     H*                factor comes after the modified premium.                            *
     H*               -For PA/DE policies, don't default the exp mod to 1.00 if it is 0.   *
     H*                                                                                    *
     H* 08/03/06 C:15  Support: WC132/53904                                                *
     H*               -Added code to exclude non-payroll exposure for CA policies.         *
     H*               -Added code to strip leading zeros from the claim #.                 *
     H*                                                                                    *
     H* 07/26/06 C:15  Support: WC093/53657                                                *
     H*                Since more than one WDELP logical is used, some of the fields are   *
     H*                being used from the wrong record.  I corrected these instances.     *
     H*                                                                                    *
     H* 06/28/06 C:15  Support: WC132/53428                                                *
     H*                Added code to prevent the assessments from being reported to CA.    *
     H*                                                                                    *
     H* 06/27/06 C:15  Support: WC136/53002                                                *
     H*                Added code for the expanded format fields for CA.                   *
     H*                                                                                    *
     H* 06/26/06 C:15  Support: WC135/53310                                                *
     H*                Added code to correct the claim status calculation.                 *
     H*                                                                                    *
     H* 06/21/06 C:132 Support: 136WCG                                                     *
     H*                If Client 136 & CMPDS# not = Blanks, load SLCLM# with positions     *
     H*                20-27 of CMPDS# right-justified.                                    *
     H*                                                                                    *
     H* 06/15/06 C:15  Support: WC081/53183                                                *
     H*                Added code to clear SVMINP to prevent erroneous minimum premium     *
     H*                exposure records.                                                   *
     H*                                                                                    *
     H* 05/16/06 C:15  Project:                                                            *
     H*                Added code to prevent record locks on WDSEL.                        *
     H*                                                                                    *
     H* 04/28/06 C:15  Project:                                                            *
     H*                Added code to load the 4th and 5th assessment amounts.              *
     H*                                                                                    *
     H* 04/13/06 C:53  Project: 45WEV                                                      *
     H*                Do not process fields for client 45 LLFRAU, LLLUMP, LLMTST          *
     H*                                                                                    *
     H* 02/14/06 C:15  Support: WC081/51179                                                *
     H*                Chgd code to send 0 premium for no exposure class code '1111' for   *
     H*                NC bureau.                                                          *
     H*                                                                                    *
     H* 02/03/06 C:15  Support: WC129/51301                                                *
     H*                Per client, claim status '2' is not valid for 'MA'.                 *
     H*                                                                                    *
     H* 01/18/06 C:15  Project: 135WFR                                                     *
     H*               1.For clt135, report all payrolls with a rate date equal to the PEOs *
     H*                 policy effective date.                                             *
     H*               2.For clt135, if the rate on a payroll is incorrect, load the correct*
     H*                 rate and adjust the exposure amount so the premium is unchanged.   *
     H*                                                                                    *
     H* 01/17/06 C:15  Support: WC102/49989                                                *
     H*                Chgd exposure/rate calc for class codes 12702 and 22702.            *
     H*                                                                                    *
     H* 12/27/05 C:15  Project: 139WAI                                                     *
     H*                For client 139, use CMPDS# as the claim #.                          *
     H*                                                                                    *
     H* 12/23/05 C:15  Project: 135WFR                                                     *
     H*                If the modifier = 0, report it as 1.000.                            *
     H*                                                                                    *
     H* 12/12/05 C:15  Project:                                                            *
     H*                Change code to calculate the claim paid and incurred amounts using  *
     H*                the payment detail file.                                            *
     H*                                                                                    *
     H* 10/18/05 C:15  Project: 114WJG                                                     *
     H*                Chgd code to include payrolls with a zero rate, if there is premium *
     H*                associated with that class code.  (ex. class code 994)              *
     H*                                                                                    *
     H* 09/16/05 C:15  Project: 114WJG                                                     *
     H*                Added code to split ALL exposure records on split rate policies,    *
     H*                for PA/DE.                                                          *
     H*                                                                                    *
     H* 08/30/05 C:15  Project: 102WDC                                                     *
     H*               -Corrected code that calculates the previous claim status.           *
     H*                                                                                    *
     H* 08/26/05 C:15  Project: 114WJG                                                     *
     H*               -47379: Added code to load the state eff dt, if it is different than *
     H*                the policy eff dt.                                                  *
     H*               -Added code to guarantee that no stat codes are blank.               *
     H*               -Chgd code so the mod and rate dates are equal on split policies.    *
     H*               -Chgd code so the short rate prem is not added to the subj prem.     *
     H*               -For NJ, only send claims with indemnity or medical incurred.        *
     H*                                                                                    *
     H* 08/09/05 C:15  Project: WC093/48431                                                *
     H*                Chgd code to include "pre capita" payrolls when calculating the     *
     H*                manual premium amount.                                              *
     H*                                                                                    *
     H* 08/04/05 C:15  Project: 114WJG                                                     *
     H*                Chgd code to use audited figures for merit rating exposure when the *
     H*                policy is audited.                                                  *
     H*                                                                                    *
     H* 06/01/05 C:15  Support: WC114/47384                                                *
     H*                Chgd code to pickup all divisions, if there is more than one div    *
     H*                per state.                                                          *
     H*                                                                                    *
     H* 05/31/05 C:15  Support: WC114/47384                                                *
     H*                Added code to default PREACT to '01' if it is blank.                *
     H*                                                                                    *
     H* 03/28/05 C:15  Project: WC591                                                      *
     H*                Changed 'prem_ttl' and 'mod_prem' to 9.0 fields so the reported     *
     H*                mod premium and subject premium in the unit total record will be    *
     H*                correct.  This issue was reported by NJCRIB.                        *
     H*                                                                                    *
     H* 03/07/05 C:15  Project: 102CN                                                      *
     H*                Add code for logging rate adjustment for electronic reporting.      *
     H*                                                                                    *
     H* 03/06/05 C:15  Project: WC591                                                      *
     H*                Add code to call SA001R to retrieve the client number to use for    *
     H*                client modifications in the base code.                              *
     H*                                                                                    *
     H* 02/18/05 C:15  Support: WC081/45593                                                *
     H*                Changed the Mod_prem calculation, to get the correct minimum prem   *
     H*                amount.                                                             *
     H*                                                                                    *
     H* 02/14/05 C:15  Project: WC1026                                                     *
     H*                Added code to report "Merit rating" changes.                        *
     H*                                                                                    *
     H* 02/10/05 C:15   Project: 129BSP                                                    *
     H*                 Chgd code to use CMPDS#, if it is not blank, instead of CMCASE.    *
     H*                                                                                    *
     H* 02/08/05 C:136 Project: WC1250                                                     *
     H*                Add domestic terrorism charge.                                      *
     H*                                                                                    *
     H* 02/01/05 C:15   Support: WC093/45653                                               *
     H*                 Removed code added on 09/09/04.                                    *
     H*                                                                                    *
     H* 02/01/05 C:15   Support: WC089                                                     *
     H*                 Added code to use the state reporting div eff dt to determine if   *
     H*                 the employer should be reported.                                   *
     H*                                                                                    *
     H* 01/18/05 C:15   Support: WC093/45457                                               *
     H*                 Fixed code to change the NCCI injury code to '06' if no comp pmts  *
     H*                 have been made on the claim.                                       *
     H*                                                                                    *
     H* 10/04/04 C:15   Support: WC591                                                     *
     H*                 Applied various NJ specific rules.                                 *
     H*                 -Stat code '1111' is not valid in NJ.  Report payroll class codes  *
     H*                  even if amount is 0.                                              *
     H*                 -Claim status code 2 is not valid.  Report either 0 or 1.          *
     H*                 -Added code to load: expense constant, premium discount, and 1st,  *
     H*                  2nd and 3rd assessments.                                          *
     H*                                                                                    *
     H* 09/30/04 C:15   Support: WC081/44003                                               *
     H*                 Chgd code to correct the '1111' record.                            *
     H*                                                                                    *
     H* 09/24/04 C:15   Support: WC081/43969                                               *
     H*                 Chgd code to process the terrorism surcharge correctly.            *
     H*                                                                                    *
     H* 09/09/04 C:15   Project: WC591                                                     *
     H*                 Added code as a prerequisite for upcoming changes to the way deduct*
     H*                 recoveries will be handles for "gross" policies.                   *
     H*                                                                                    *
     H* 09/01/04 C:136  Project: WC1237                                                    *
     H*                 Add voluntary labor charge. Note: this code was reimplemented      *
     H*                 after WC591. Also, removed the code that populated the SEMDDT      *
     H*                 field from the terrorism charge per LK.                            *
     H*                                                                                    *
     H* 08/30/04 C:15   Project: WC1123, WC1149  Seq# 43667                                *
     H*                 Added code to handle FL policies that were affected by the FL      *
     H*                 April and October rate changes.                                    *
     H*                                                                                    *
     H* 08/12/04 C:15   Support: WC081/43122                                               *
     H*                 Chgd code to correctly load the increased limits premium and base  *
     H*                 the '1111' stat transaction off payroll amount = 0.                *
     H*                                                                                    *
     H* 05/10/04 C:15   Support: WC091/42453                                               *
     H*                 Chgd code to prevent the paid amount from exceeding the incurred   *
     H*                 amount.                                                            *
     H*                                                                                    *
     H* 04/26/04 C:136  Project: WC1169                                                    *
     H*                 Process Occupational Disease payroll classes the same as           *
     H*                 Aircraft surcharge (EPCLAS = RRDCCD)                               *
     H*                                                                                    *
     H* 01/26/04 C:15   Support: WC091/40980                                               *
     H*                 Fixed the 'GetDedTyp' procedure.                                   *
     H*                                                                                    *
     H* 01/07/04 C:15   Support: WC045/40273                                               *
     H*                 Had to increase the size of the payroll rate work fields to match  *
     H*                 the size of WMEPP/EPARAT, to prevent an error.                     *
     H*                                                                                    *
     H* 12/29/03 C:136  Project: WC1120                                                    *
     H*                 Write type exposure records for terrorism surcharge.               *
     H*                                                                                    *
     H* 12/08/03  C:15  Support:  WC091/40257                                              *
     H*                 Chgd the Waiver surcharge calc.  The mod eff dt will be loaded for *
     H*                 the waiver surcharge, regardless of premium level.                 *
     H*                                                                                    *
     H* 11/03/03  C:15  Support:  WC091/39552                                              *
     H*                 Chgd the Waiver surcharge calc to load the mod eff dt if ELWVFL =  *
     H*                 'M', 'J', 'E' or blank.                                            *
     H*                                                                                    *
     H* 10/30/03  C:15  Support:  WC081/39348                                              *
     H*                 Chgd code to not send a '0990' record on policies that have no     *
     H*                 payroll (policies that report a '1111' record).                    *
     H*                                                                                    *
     H* 03/20/03  C:15  Support:  WC048/36529                                              *
     H*                 Chgd code that determines if a 'previous' claim record gets        *
     H*                 written.                                                           *
     H*
     H* 02/25/03  C:53  Project: 129WAZ
     H*                 Load employers liability PTD by RTCTGY
     H*                                                                                    *
     H* 02/13/03  C:15  Support:  WC093/35996                                              *
     H*                 While reviewing the WCSTAT manual, I noticed that the claim JRST   *
     H*                 code is not required unless it is different than the rating state. *
     H*                 I changed the pgm accordingly.                                     *
     H*                                                                                    *
     H* 12/20/02  C:15  Support:  WC091/34235                                              *
     H*                 Added code to load the exp mode date and rate date on exposure     *
     H*                 records that these fields don't apply to for NC and WI.  This      *
     H*                 change was outlined in NCCI Circular UNIT-02-05 dated 9/5/02.      *
     H*                                                                                    *
     H* 09/12/02  C:15  Support:  WC096/33954                                              *
     H*                 Chgd '1111' calc to check all division for exposure before writing *
     H*                 the '1111' WDSEP record.                                           *
     H*                                                                                    *
     H* 08/22/02  C:15  Support:  WC096/33954                                              *
     H*                 Added code to account for a 0 exp mod, to prevent the mod_prem     *
     H*                 from being incorrect.                                              *
     H*                                                                                    *
     H* 04/16/02  C:15  Support:  WC113/31613                                              *
     H*                 Added code to keep separate any class codes with an 'F' type       *
     H*                 designation, since these represent USL&H payrolls.                 *
     H*                                                                                    *
     H* 03/20/02  C:15  Project:  WC591                                                    *
     H*                 Reversed chgs made on 7/23/01, 11/21/01 and 2/15/02.  Added new    *
     H*                 code to guarantee that all appropriate employers/divisions will be *
     H*                 processed.                                                         *
     H*                                                                                    *
     H* 03/15/02  C:15  Project:  WC1031   seq# 26262                                      *
     H*                 On min prem accts, get the other exposure figures from WDMPP, and  *
     H*                 load the standard prem from WDMPP if the total exposure is greater *
     H*                 than the min prem.                                                 *
     H*                                                                                    *
     H* 03/14/02  C:15  Project:  WC591                                                    *
     H*                 Added code to write an exposure record with code '1111' when no    *
     H*                 payroll exposure has been produced.                                *
     H*                                                                                    *
     H* 03/01/02  C:15  Support:  WC096/31332                                              *
     H*                 Fixed attorney fees calculation.                                   *
     H*                                                                                    *
     H* 02/15/02  C:15  Support:  WC096/30698                                              *
     H*                 Fixed code to report all divisions on the same submission.         *
     H*
     H* 12/12/01  C:53  PROJECT: WC1060
     H*                 ADD EXCLUDE MEDICAL PROCESSING.
     H*                                                                                    *
     H* 11/21/01  C:15  Support:  WC096/26262                                              *
     H*                 -Added code to use the state reporting div eff dt to determine     *
     H*                  if the employer should be reported.                               *
     H*                                                                                    *
     H* 11/07/01  C:15  Support:  WC093/29481                                              *
     H*                 -Added code to load all fields for the "previous" claim record.    *
     H*                                                                                    *
     H* 10/16/01  C:15  Support:  WC096/29265                                              *
     H*                 -Changed code to correct issue with subsequent reporting of claims.*
     H*                                                                                    *
     H* 10/03/01  C:15  Support:  WC096/28767                                              *
     H*                 -Added code to load the attorneys' fees fields.                    *
     H*                                                                                    *
     H* 09/12/01  C:15  Support:  WC045/                                                   *
     H*                 -Added code to not load "other" exposure discounts if they come    *
     H*                  after the standard premium.                                       *
     H*                                                                                    *
     H* 08/22/01  C:15  Support:  WC093/28587                                              *
     H*                 -Added code to load the split mod/rate indicator.                  *
     H*                 -Corrected the claim status calc on "previous" report records.     *
     H*                 -Corrected code to not send "previous" report loss records unless  *
     H*                  the total incurred is > 0.                                        *
     H*                                                                                    *
     H* 08/03/01  C:15  Support:  WC096/28239                                              *
     H*                 -Added code to default the first exp mod eff dt if = 0.            *
     H*                 -Added code to only load an exp mod on discounts/surcharges that   *
     H*                  occur before the exp mod.                                         *
     H*                                                                                    *
     H* 08/02/01  C:15  Support:  WC048/27983                                              *
     H*                 Added code to report the waiver surcharge.                         *
     H*                                                                                    *
     H* 07/23/01  C:15  Support:  WC096/28333                                              *
     H*                 Changed pgm so all selected policy years will be processed for     *
     H*                 all divisions, regardless of what years are setup on the state     *
     H*                 reporting division.                                                *
     H*                                                                                    *
     H* 04/10/01  C:15  Support:  WC088/26900                                              *
     H*                -Report policy conditions regardless of ASWG flag.                  *
     H*                -Fix claim status calc for type '4' status.                         *
     H*                                                                                    *
     H* 03/22/01  C:15  Support:  WC048/26095                                              *
     H*                -Chgd code to split all discounts and credits to indicate the       *
     H*                 correct exp mod for which the amount was calculated against.       *
     H*                -Correct code was loads the additional load factor data.            *
     H*                                                                                    *
     H* 01/31/01  C:15  Support:  WC093/25764                                              *
     H*                -Added code to round all dollar figures instead of truncating.      *
     H*                -Added code to include the following credits to the subject prem:   *
     H*                 deviation #1 (if prem level = 'J'), deviation #2 (if prem level =  *
     H*                 'J', subj prem credit 1A, and subj prem credit 1B.                 *
     H*                                                                                    *
     H* 01/10/01  C:15  Support:  WC093/25633                                              *
     H*                 Changed the pgm to load the interstate rated flag based on the     *
     H*                 field in the employer master file.                                 *
     H*                                                                                    *
     H* 12/14/00  C:15  Support:  WC093/25376                                              *
     H*                 Added code to check for blank values in the pmt val 1 code of the  *
     H*                 claim payments and group record when calculating deduct recoveries.*
     H*                                                                                    *
     H* 12/07/00  C:15  Support:  WC096/25305                                              *
     H*                 New NCCI rule:  report claims with expense incurred as well as     *
     H*                 medical and indemnity.                                             *
     H*                                                                                    *
     H* 11/28/00  C:15  Support:  WC093/25061                                              *
     H*                -Changed the deduct recovery calc to use the claim pmt history file *
     H*                 instead of the deduct detail file.                                 *
     H*                -Changed the total incurred calc to add the deduct recoveries back  *
     H*                 into to the amount to report the gross amount.                     *
     H*                                                                                    *
     H* 10/25/00  C:87  Project: supp 24716                                                *
     H*                 Check that payroll effective date is not beyond cancel date.       *
     H*                                                                                    *
     H* 09/28/00  C:87  Project: WC591 sup#24469                                           *
     H*                 Non-payroll exposure should have exposure act = 0.                 *
     H*                                                                                    *
     H* 09/27/00  C:87  Project: WC591 sup#24377                                           *
     H*                 Use ELSRPR instead of EMSRPR to determine if short-rated.  Chg     *
     H*                 process to calc total standard exposure (was not figuring split    *
     H*                 payrolls correctly).                                               *
     H*                                                                                    *
     H* 09/05/00  C:87  Project: WC591                                                     *
     H*                 Added code to only submit loss records on subsequent reports if    *
     H*                 the claim chgd from the previous report.  Write an accompanying    *
     H*                 'P' update type record on subsequent reports also.                 *
     H*                 Also, check subsequent reports: if no loss records reported, then  *
     H*                 don't report header or unit records (delete them).                 *
     H*                                                                                    *
     H**************************************************************************************
     Fwdell3    if   e           k disk                                         emp detail
     Fwdell1    if   e           k disk    Rename(pel01 : pel02) Prefix(el2:2)   "     "
     Fwdell8    if   e           k disk    Rename(pel01 : pel03) Prefix(el3:2)   "     "
     Fwdell     if   e           k disk    Rename(pel01 : pel00) Prefix(el0:2)   "     "
     Fwmfnp     if   e             disk                                         fund master
     Fwmeml     if   e           k disk                                         emp master
     Fwdf2l     if   e           k disk                                         fund auxilliary
     Fwmrsl     if   e           k disk                                         rate set code
     Fststl3    if   e           k disk                                         st code table
     Fwtddl2    if   e           k disk                                         ded dscnt code
     Fwmldl1    if   e           k disk
     Fwmepl1    if   e           k disk                                         emp payrolls
     Fwmepl7    if   e           k disk    Rename(pep01 : pep02) Prefix(ep2:2)
     Fwmcml5    if   e           k disk                                         claim master
     Fwmchl     if   e           k disk                                         claim history
     Fwtabl     if   e           k disk    Include(ppa01 : pna01 : pca01 :      table logical
     F                                             prt01 : pbs01 : pb201)
     Fwtocl     if   e           k disk                                         occupation code
     Fwdehl     if   e           k disk                                         emp dtl history
     Fwdrrl     if   e           k disk                                         stat rptg rating
     Fwtgtl     if   e           k disk                                         generic table file
     Fwmprl1    if   e           k disk                                         payroll class
     Fwmlll     if   e           k disk                                         claim legal mstr
     Fwtlml1    if   e           k disk                                         limits table file
     Fwmrhl     if   e           k disk                                         reserve history
     Fwdpal5    if   e           k disk    Rename(ppa01 : pa201)                payment detail
     Fwdmpl     if   e           k disk                                         premium detail
     Fwdill     if   e           k disk                                         premium detail
     FWMCML1    uf   e           k disk    rename(pcm01 : pcm02)
     FWMCDL     IF   E           K DISK
     FWDACL     IF   E           K DISK
     FWMPDL     IF   E           K DISK
     F*
      * WCSTAT Extract data files
     Fwmsql     uf a e           k disk
     Fwdsll     uf a e           k disk
     Fwdsel     uf a e           k disk
     Fwmsrl     uf a e           k disk
     Fwmssl     uf   e           k disk
     FWDSIL     UF A E           K DISK
     Fwdsll2    if   e           k disk    rename(psl01 : psl02)
     FWMPXL1    IF   E           K DISK
     FWMRTL     IF   E           K DISK    rename(prt01:prt0x)
     F                                     prefix(rx:2)
     Fwdppl     if   e           k disk
     Fwmexl     if   e           k disk
     Fwmhcl     if   e           k disk
     Fwdellb    if   e           k disk    Rename(pel01 : pel0b) Prefix(elb:2)

      * CX reason code table
     Fwtcrl     if   e           k disk
     FWDHEL     IF   E           K DISK
     Fwtstl     IF   E           K DISK    rename(pst01:pst00)
     F                                     prefix(st2:2)


     d FClassTotal     pr            11  2



     D*  Procedure prototypes
     D/copy sprdate
     D/copy wpremplr
     D
     Ddt6to8L          pr             8  0
     D numDate6                       6  0 const
     D
     D wrtLnkHdr       pr                  like(*in99)
     D   ##co#                        3  0 Const
     D   ##fnd                        3  0 Const
     D   ##rpmy                       5  0 Const
     D   ##emp#                       9  0 Const
     D   ##fyr                        3  0 Const
     D   ##stcd                       2  0 Const

     D writeExp        pr
     D   ##co#                        3  0 Const
     D   ##fnd                        3  0 Const
     D   ###rpmy                      5  0 Const
     D   ##emp#                       9  0 Const
     D   ##fyr                        3  0 Const
     D   ##stcd                       2  0 Const
     D   ##clas                       5  0 Const
     D   ##exp#                       2  0 Const
     D   ##expMod                     5  4 Const
     D   ##effMod                     7  0 Const
     D   ##rateDt                     7  0 Const
     D   ##expamt                    11  2 Const
     D   ##prmamt                    11  2 Const
      **** Beg Chg **** 06/17/24 **************************************
     D****##rate                       7  3 Const
     D   ##rate                       8  3 Const
      **** End Chg **** 06/17/24 **************************************
     D   ##tier                       2  0 Const
     D   ##expo                       2  0 Const
     D   ##splt                       1  0 Const


     D splitPyrl       pr
     D   ##co#                        3  0 Const
     D   ##fnd                        3  0 Const
     D   ##fyr                        3  0 Const
     D   ##emp#                       9  0 Const
     D   ##div                        5  0 Const
     D   ##clas                       5  0 Const
     D   ##spec                       1    Const
     D   ##idx                        3  0 Const
     D   ##eamt                      11  2
     D   ##prem                      11  2
     D**** Beg Chg **** 06/17/24 *******************************************
     D****##arat                       7  2 Const
     D   ##arat                       8S 3 Const
     D**** End Chg **** 06/17/24 *******************************************
     D   ##efdt                       7  0 Const
     D   ##beg_dt                     7  0 Const
     D   ##end_dt                     7  0 Const
     D   ##percap                     1    Const
     D   ##pcf                        1    Const


     D getDedTyp       pr             2
     D   company                      3  0
     D   dedRsvArr                   20


     D pro_rate        pr            11  2
     D   fdt                          7  0 Const
     D   tdt                          7  0 Const
     D   stdt                         7  0 Const
     D   endt                         7  0 Const
     D   amount                      11  2 Const

     d getLossCost     pr             5  4
     d  class                              like(EPCLAS)
     d  effDt                              like(EPEFDT) const
     d getRateDate     pr             7  0
     d  effDt                              like(EPEFDT)
     d prChkRateChg    pr              n
     d  rate1                         5s 3
     d  rate2                         5s 3
     d  rate3                         5s 3

     D ApplyRateSplit  pr              n
     D inState                        2a
     D inDate                         7  0
     D outstcd                        5  0 options(*nopass)
     d xxFlRtAdj       s                   like(epempr)
     d xtpred          s                   like(epempr)
     d hldExMd         s                   like(SEexMd)
     d hldMdDt         s                   like(SEmdDt)
     d hldRtDt         s                   like(SErtDt)
     d xxstcd          s              5  0
     D xcILRate        c                   const('IL Flat rate adjustment')
     D INDX            C                   CONST(30)                            array index const
     d xxFlRtAdjNY     s                   like(epempr)
     d xtpredNY        s                   like(epempr)
     d xcNYRate        c                   const('NY Flat rate Decrease')

     D Lower           C                   CONST('abcdefghijklmnopqrstuvwxyz')
     D Upper           C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     d sv2State        s                   like(ST2state)
     d sv2StCd         s                   like(ST2code)
     d sv2PrtZrFlg     s                   like(ST2flag)
     d sv2Status       s                   like(ST2state)
     d xxExpMod        s              5  4
     d ncciState       s               n   Inz(*Off)

     D*  Arrays
     D                 DS
     D expMod                  1     15  4 dim(3)
     D  el2exm1                1      5  4
     D  el2exm2                6     10  4
     D  el2exm3               11     15  4
     D                 DS
     D effMod                  1     21  0 dim(3)
     D  el2emd1                1      7  0
     D  el2emd2                8     14  0
     D  el2emd3               15     21  0
     D*
     D* Create new datastructure to hold terror/assessment rate change data
     D strtcg          ds
     D tcst                           2
     D tcef1                          7  0
     D tcef2                          7  0
     D tcef3                          7  0
     D tca1r1                         5  3
     D tca1r2                         5  3
     D tca1r3                         5  3
     D tca2r1                         5  3
     D tca2r2                         5  3
     D tca2r3                         5  3
     D tca3r1                         5  3
     D tca3r2                         5  3
     D tca3r3                         5  3
     D tca4r1                         5  3
     D tca4r2                         5  3
     D tca4r3                         5  3
     D tca5r1                         5  3
     D tca5r2                         5  3
     D tca5r3                         5  3
     D tca6r1                         5  3
     D tca6r2                         5  3
     D tca6r3                         5  3
     D tca7r1                         5  3
     D tca7r2                         5  3
     D tca7r3                         5  3
     D tctrr1                         5  3
     D tctrr2                         5  3
     D tctrr3                         5  3
     D tcdtr1                         5  3
     D tcdtr2                         5  3
     D tcdtr3                         5  3
     D tca1a1                        11  2
     D tca1a2                        11  2
     D tca1a3                        11  2
     D tca2a1                        11  2
     D tca2a2                        11  2
     D tca2a3                        11  2
     D tca3a1                        11  2
     D tca3a2                        11  2
     D tca3a3                        11  2
     D tca4a1                        11  2
     D tca4a2                        11  2
     D tca4a3                        11  2
     D tca5a1                        11  2
     D tca5a2                        11  2
     D tca5a3                        11  2
     D tca6a1                        11  2
     D tca6a2                        11  2
     D tca6a3                        11  2
     D tca7a1                        11  2
     D tca7a2                        11  2
     D tca7a3                        11  2
     D tctra1                        11  2
     D tctra2                        11  2
     D tctra3                        11  2
     D tcdta1                        11  2
     D tcdta2                        11  2
     D tcdta3                        11  2
     D tca1s1                         5  0
     D tca1s2                         5  0
     D tca1s3                         5  0
     D tca2s1                         5  0
     D tca2s2                         5  0
     D tca2s3                         5  0
     D tca3s1                         5  0
     D tca3s2                         5  0
     D tca3s3                         5  0
     D tca4s1                         5  0
     D tca4s2                         5  0
     D tca4s3                         5  0
     D tca5s1                         5  0
     D tca5s2                         5  0
     D tca5s3                         5  0
     D tca6s1                         5  0
     D tca6s2                         5  0
     D tca6s3                         5  0
     D tca7s1                         5  0
     D tca7s2                         5  0
     D tca7s3                         5  0
     D tctrs1                        11  2
     D tctrs2                        11  2
     D tctrs3                        11  2
     D tcdds1                        11  2
     D tcdds2                        11  2
     D tcdds3                        11  2
     D rate_dt         S              7  0 dim(3) ascend
     D othExp          S             12  2 dim(INDX)                            Exposure-not payroll
     D lvl             S              1    dim(35) CTDATA PerRcd(35)            Reporting levels
     D splt_dt         s              7  0 dim(5) ascend
     D expamt          s             11  2 dim(5) inz
     D pramt           s             11  2 dim(5) inz
     D*** Beg Chg *** 07/16/24 *********************************************
     D***** prem_tl         s             11  2 dim(5) inz
     D prem_tl         s              9  0 dim(5) inz
     D*** End Chg *** 07/16/24 *********************************************
     D xxclm#          s              1    dim(12)

     D*  arrays for spliting employer payrolls by periods
     D pefdt           s              7  0 dim(5) inz
     D pexmd           s              5  4 dim(5) inz
     D ratedt          s              7  0 dim(5) inz
     D***manrt           s              5  2 dim(5) inz
     D**** Beg Chg **** 06/17/24 *******************************************
     D***manrt           s              7  2 dim(5) inz
     D manrt           s              8S 3 dim(5) inz
     D**** End Chg **** 06/17/24 *******************************************
     D splitp          s              1  0 dim(5) inz
     D rttier          s              2  0 dim(5) inz
     D expact          s              2  0 dim(5) inz
     D expos#          s              2  0 dim(5) inz
     D xxRtChg         s               n   inz
     D xxCalMod        s               n   inz
     D**** Beg Chg **** 06/17/24 *******************************************
     D***xxRate1         s              5  3 inz
     D***xxRate2         s              5  3 inz
     D***xxRate3         s              5  3 inz
     D xxRate1         s              8S 3 inz
     D xxRate2         s              8S 3 inz
     D xxRate3         s              8S 3 inz
     D**** End Chg **** 06/17/24 *******************************************
     D xxAmt1          s             11  2 inz
     D xxAmt2          s             11  2 inz
     D xxAmt3          s             11  2 inz
     D xxAmt           s             11  2 inz
     D xxefdt          s              7  0 inz
     D xxMcls1         s              5  0 inz
     D xxMcls2         s              5  0 inz
     D xxMcls3         s              5  0 inz
     D polEffDt        s              7  0


     D*  Date/time data structure
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D*  Date field definitions
     D fromDt          s               d   datfmt(*iso)
     D toDate          s               d   datfmt(*iso)
     D rptDate         s               d   datfmt(*iso)
     D Anniv_dt        s               d   datfmt(*iso)
     D Pol_eff         s               d   datfmt(*iso)
     D lastSubDt       s               d   datfmt(*iso)
     D mthendDt        s               d   datfmt(*iso)


     D*  key fields
     D kkcase          s                   like(cmcase)
     D kkclas          s                   like(cmclas)
     D kkco#           s                   like(fnco#)
     D kkdcde          s                   like(eldcde)
     D kkdcef          s                   like(eldcef)
     D kkdiv           s                   like(eldiv)
     D kkefdt          s                   like(prefdt)
     D kkemp#          s                   like(elemp#)
     D kkfnd           s                   like(elfnd)
     D kkfyr           s                   like(elfyr)
     D kkkey           s                   like(stkey)
     D kktbid          s                   like(gttbid)
     D kkpfdt          s                   like(elpfdt)
     D kkrpmy          s                   like(serpmy)
     D kkrtcd          s                   like(rsrtcd)
     D kkspec          s                   like(epspec)
     D kkst            s                   like(rsjrst)
     D kkstcd          s                   like(sqstcd)
     D kksubdt         s                   like(chefdt)
     D kkMthDt         s                   like(patrdt)
     D svpfdt          s                   like(elpfdt)
     D svptdt          s                   like(elpfdt)
     D xxPrvYr         s                   like(chefdt)
     D svfyr           s                   like(elfyr)
     D prvRptMY        s                   like(##RPMY)
     D kkclm#          s                   like(slclm#)
     D XXPRVRCD        s                   like(*in10)
     D xxinct          s             11  2
     D xxinci          s             11  2
     D xxvrhb          s             11  2
     D xxincm          s             11  2
     D xxhosp          s             11  2
     D xxphpd          s             11  2
     D xxpdmd          s             11  2
     D xxpdin          s             11  2
     D xxlegc          s             11  2
     D xxlegd          s             11  2
     D xxdisp          s             11  2
     D xxptpd          s             11  2
     D xxrhbp          s             11  2
     D xxprmp          s             11  2
     D xxdthp          s             11  2
     D xxstla          s             11  2
     D xxsumd          s             11  2
     D xxvind          s             11  2
     D xxvtrn          s             11  2
     D xxvevl          s             11  2
     D xxssam          s             11  2

     D*  Policy conditions data structures
     D PolCond1        ds
     D  excessCvg                     1  0
     D  interSt1                      1  0
     D  asgnRisk                      1  0
     D  canxPol                       1  0
     D  estAudit                      1  0
     D  diseaseB                      1  0
     D  exclDis                       1  0
     D  clerErr                       1  0
     D  retroRat1                     1  0
     D  noExcPay                      1  0
     D  lrgRisk                       1  0
     D  mco1                          1  0
     D*
     D PolCond2        DS
     D  threeYr                       1
     D  multiSt                       1
     D  interSt2                      1
     D  estExp                        1
     D  retroRat2                     1
     D  canxMid                       1
     D  mco2                          1
     D*
     D*  Deductible type data structures
     D Ded_type        DS
     D  Ded_ty12               1      2
     D  Ded_ty34               3      4
     D
     D*resvCod                 1     20    Dim(10)
     D*payRsv                  1     90  2 Dim(10)
     D*remRsv                  1     90  2 Dim(10)
     D*
     D                 DS
     D rhcod                   1     20    Dim(10)
     D  rhrt01                 1      2
     D  rhrt02                 3      4
     D  rhrt03                 5      6
     D  rhrt04                 7      8
     D  rhrt05                 9     10
     D  rhrt06                11     12
     D  rhrt07                13     14
     D  rhrt08                15     16
     D  rhrt09                17     18
     D  rhrt10                19     20
     D*
     D                 DS
     D rhamt                   1     90  2 Dim(10)
     D  rhsu01                 1      9  2
     D  rhsu02                10     18  2
     D  rhsu03                19     27  2
     D  rhsu04                28     36  2
     D  rhsu05                37     45  2
     D  rhsu06                46     54  2
     D  rhsu07                55     63  2
     D  rhsu08                64     72  2
     D  rhsu09                73     81  2
     D  rhsu10                82     90  2
     D*
     D*  Deductible reserve codes
     D                 DS
     D dedRsvCod               1     20
     D  dddc01                 1      2
     D  dddc02                 3      4
     D  dddc03                 5      6
     D  dddc04                 7      8
     D  dddc05                 9     10
     D  dddc06                11     12
     D  dddc07                13     14
     D  dddc08                15     16
     D  dddc09                17     18
     D  dddc10                19     20
     D*
     D*  Large Deductible reserve codes
     D                 DS
     D ldedRsvCod              1     20
     D  ldr101                 1      2
     D  ldr102                 3      4
     D  ldr103                 5      6
     D  ldr104                 7      8
     D  ldr105                 9     10
     D  ldr106                11     12
     D  ldr107                13     14
     D  ldr108                15     16
     D  ldr109                17     18
     D  ldr110                19     20

     D*  Loss condition codes (from generic table file)
     D                 DS
     D gtcf01                  1     10
     D  act                    1      2
     D  typ_los                3      4

     D                 DS
     D  claim#                       12
     D   xxcase                       7  0 Overlay(claim#:6)


     D                 DS
     D SIBCR                   1      5    Dim(5)
     D  sibcr1                 1      1
     D  sibcr2                 2      2
     D  sibcr3                 3      3
     D  sibcr4                 4      4
     D  sibcr5                 5      5

     D                 DS
     D SIBCD                   1      5    Dim(5)
     D  sibcd1                 1      1
     D  sibcd2                 2      2
     D  sibcd3                 3      3
     D  sibcd4                 4      4
     D  sibcd5                 5      5

     D                 DS
     D SIBBD                   1     30    Dim(5)
     D  sibbd1                 1      6  0
     D  sibbd2                 7     12  0
     D  sibbd3                13     18  0
     D  sibbd4                19     24  0
     D  sibbd5                25     30  0

     D*  field definitions
     D link_car#       s                   like(f2car#)
     D link_bsi#       s                   like(f2bsi#)
     D link_emp#       s                   like(elemp#)
     D link_fyr        s                   like(elfyr)
     D link_stcd       s                   like(stnst)
     D link_crt#       s              1
     D link_efdt       s                   like(elpfdt)
     D link_rpt#       s              1
     D link_corr       s              1
     D ASWGflag        s              1
     D ASWGdate        s                   like(elpfdt) inz(0960101)

     D*  Field definitions for accumulation of unit totals
     D Uexp_pay        s             11  0
     D Uexp_oth        s             10  1
     D Usubj_prem      s             11  2
     D Ustd_prem       s             11  2
     D U#claims        s              5  0
     D U#exposure      s              5  0
     D U#lnkhdr        s              5  0
     D Uindem_inc      s             10  0
     D Umed_inc        s             10  0
     D Uindem_pd       s             10  0
     D Umed_pd         s             10  0
     D Uclmnt_att      s             10  0
     D Uemp_att        s             10  0
     D Ualae_pd        s             10  0
     D Ualae_inc       s             10  0
     D
     D calc_prem       s             11  2                                      calc premium
     D*prem_ttl        s             11  2                                      prem total - emplyr
     D*mod_prem        s             11  2                                      modified prem - emp
     D prem_ttl        s              9  0                                      prem total - emplyr
     D mod_prem        s              9  0                                      modified prem - emp
     D nDays           s              3  0                                      nmbr of days
     D xtra_prem       s              9  0                                      excess minimum prem
     D in_adjust       s             11  2                                      IN flat rt adjstmnt
     D ind_amt         s              9  0                                      hold variable
     D man_prem        s             12  2                                      manual prem - emp

     D xxctystzip      s             36                                         city,state,zip
     D*** Beg Add *** 03/25/24 *********************************************
     D xxAddress       s             42                                         city,state,zip
     D*** End Add *** 03/25/24 *********************************************
     D adtPyrl         s              1                                         audited payroll
     D juris_St        s                   like(RSJRST)                         jurisdiction state
     D stcode          s              2  0
     D beg_dt          s              7  0
     D end_dt          s              7  0
     D idx             s              3  0
     D expa            s                   like(epeapr)
     D expo            s              2  0
     D prem            s                   like(epeapr)
     D curLvl          s              2  0                                      current level
     D indemPay        s             10  2                                      indemnity pay resrv
     D medicPay        s             10  2                                      medical pay resrv
     D allocPay        s             10  2                                      allocated pay resrv
     D indemInc        s             10  2                                      indemnity incurred
     D medicInc        s             10  2                                      medical incurred
     D allocInc        s             10  2                                      allocated incurred
     D sum_arr         s             10  2                                      sum PayRsv array
     D begDt           s                   like(epefdt)
     D endDt           s                   like(epefdt)
     D cxDate          s                   like(emcad1)
     D hold_jrst       s                   like(rsjrst)
     D cat#            s              2  0                                      catastrophe number
     D exp#            s              2  0
     D i               S              2  0                                      counter
     D j               S              1  0                                      index
     D k               S              1  0                                      index
     D m               S              3  0
     D n               S              3  0
     D r               S              3  0
     D s               S              3  0
     D t               S              3  0
     D z               S              2  0                                      counter
     D NJaddr          S             50                                         NewJersey address
     D percap          s              1
     D xxzip           s              9
     D tier            s              2  0
     D prevOpen        s              1                                         claim previously opn
     D prevClsd        s              1                                         claim prev closed
     D prevNone        s              1                                         claim prev not rptd
     D rptLoss         s              1                                         report loss flag
     D prvIndInc       s             10  2                                      prev indemnity incur
     D prvIndPd        s             10  2                                      prev indemnity paid
     D prvMedInc       s             10  2                                      prev medical incur
     D prvMedPd        s             10  2                                      prev medical paid
     D totClms         s              5  0
     D dedIndRec       s              9  2                                      ded comp recovery
     D dedMedRec       s              9  2                                      ded med recovery
     D dedExpRec       s              9  2                                      ded expense recovery
     D dedLegRec       s              9  2                                      ded legal recovery
     D prvExpInc       s             10  2                                      prev medical incur
     D prvExpPd        s             10  2                                      prev medical paid
     D xxprevinc       s             11  2
     D xxprevpd        s             11  2
     D xxprevcnt       s              7  0
     D clmattfee       s             11  2
     D empattfee       s             11  2
     D prvcattfe       s             11  2
     D prveattfe       s             11  2
     D prvdedInd       s              9  2                                      ded comp recovery
     D prvdedMed       s              9  2                                      ded med recovery
     D prvdedExp       s              9  2                                      ded expense recovery
     D prvdedLeg       s              9  2                                      ded legal recovery
     D vocRehab        s             11  2
     D prvVocReh       s             11  2
     D xxexpa          s             11  2
     D xxadld          s             11  2
     D xxmrrt          s             11  2

     D svcccd          s                   like(rrcccd)
     D svdv1c          s                   like(rrdv1c)
     D svdv2c          s                   like(rrdv2c)
     D svdccd          s                   like(rrdccd)
     D sv1acd          s                   like(rr1acd)
     D sv1bcd          s                   like(rr1bcd)
     D svcrcd          s                   like(rrcrcd)
     D svdbcd          s                   like(rrdbcd)
     D sveccd          s                   like(rreccd)
     D svmpcd          s                   like(rrmpcd)
     D svwvac          s                   like(rrwvac)
     D svtdcd          s                   like(rrtdcd)
     D svtccd          s                   like(rrtccd)
     D sv1stc          s                   like(rr1stc)
     D sv2ndc          s                   like(rr2ndc)
     D svcoal          s                   like(rrcoal)
     D sv3rdc          s                   like(rr3rdc)
     D svmrrd          s                   like(rrmrrd)
     D svmrrn          s                   like(rrmrrn)
     D svmrrc          s                   like(rrmrrc)
     D svtxcd          s                   like(rrtxcd)
     D svterc          s                   like(rrterc)
     D sv4thc          s                   like(rr4thc)
     D sv5thc          s                   like(rr5thc)
     D svvlcc          s                   like(rrvlcc)
     D svdtrc          s                   like(rrdtrc)
     D svminp          s                   like(elaspr)
     D svmodp          s                   like(mod_prem)
     D bal_min         s                   like(mod_prem)
     D svrdpr          s                   like(rrrdpr)
     D svdv1l          s                   like(rrdv1l)
     D svdv2l          s                   like(rrdv2l)
     D svwvfl          s                   like(rrwvfl)
     D svterr          s                   like(rrterr)
     D svdspr          s                   like(rrdspr)
     D sv1stl          s                   like(rr1stl)
     D sv2ndl          s                   like(rr2ndl)
     D sv3rdl          s                   like(rr3rdl)
     D svasfl          s                   like(rrasfl)
     D noexpo          s              1
     D svilmc          s                   like(rrilmc)
     D svstcd          s                   like(stcode)
     D svdtrr          s                   like(rrdtrr)
     D svrtrc          s                   like(rrrtrc)
     DQQFOUND          S              1N
     DQQMPAYR          S              7  5
     DQQMRATE          S              7  5
     DQQRPTCLS         S              5  0
     Dxxcrsn           S             65
     Dxxccod           S              2
     D*
     dcharClm#         s                   like(CMpds#)
     dintPos           s              2  0
     dfrstHypen        s              1    inz(' ')
     d@@tempClm#       s                   like(CMpds#)
     D MedRecv         s                   like(paamt)
     D IndRecv         s                   like(paamt)
     D PMedRecv        s                   like(paamt)
     D PIndRecv        s                   like(paamt)
      *** Beg Add ***** 08/05/25 ********************
     D ma_ARAP         s             11  2                                      MA ARAP Surcharge
      *** End Add ***** 08/05/25 ********************

     DPSDS           ESDS                  Extname(SZQ1P)


      *****************************************************************
      *  Key lists
      *****************************************************************

      *  CX Reason Code Table

     C     crky          klist
     C                   kfld                    el2co#
     C                   kfld                    Juris_st
     C                   kfld                    k1key            10

     C
     C*   employer detail
     C     elky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkpfdt
     C*
     C     elky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C*
     C     elky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C*
     C     elky3         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkfyr
     C*
     C     elky4         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    emdiv
     C     elky5         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C*
     C     elky6         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkpfdt
     C                   kfld                    kkemp#

     C     elkyb         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C     elky0         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    epdiv
     C                   kfld                    kkfyr
     C*   employer master
     C     emky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#

     C*   large deductible detail file
     C     ldky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr


     C*   group auxiliary file
     C     f2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd

     C*   rate set code master file
     C     rsky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd

     C*   state code table file
     C     stky          klist
     C                   kfld                    kkkey

     C*   employer history file
     C     ehky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr

     C*   table file
     C     tbky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey

     C*   deductible discount code file
     C     ddky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkdcde
     C                   KFLD                    kkdcef
     C     ddky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    rsjrst
     C                   kfld                    el2dcde
     C                   KFLD                    el2pfdt
     C     ddky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    rsjrst
     C                   kfld                    el2dcde

     C*   deductible detail file
     C     d2ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

     C*   employer payroll file
     C     epky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C*
     C     epky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkclas
     C                   kfld                    kkspec
     C*
     C     epky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#

     C*
     C     epkyl         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv
     C                   kfld                    kkclas

     C*   payroll master file
     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd
     C                   kfld                    kkclas
     C                   kfld                    kkefd1            7 0
     C*
     C     prky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkrtcd
     C                   kfld                    kkclas
     C     prky3         klist
     C                   kfld                    epco#
     C                   kfld                    el0rtcd
     C                   kfld                    epclas
     C                   kfld                    x1efdt            7 0
     C*
     C     prky4         klist
     C                   kfld                    epco#
     C                   kfld                    el0rtcd
     C                   kfld                    epclas

     C*  PAYROLL DESCRIPTION EXT FILE
     C*
     C     pxky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkclas
     C                   kfld                    cmext
     C                   kfld                    rsjrst
     C                   kfld                    kkefd1
     C*
     C     pxky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkclas
     C                   kfld                    cmext
     C                   kfld                    rsjrst
     C*
     C*   statistical rate reporting file
     C     rrky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkefdt

     C     rrky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C     rrky5         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    xpprdt
     C*
     C*  HCN GROUP SETUP KEY
     C*
     C     HCKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKST
     C                   KFLD                    SVPFDT
     C*
     C     HCKEY1        KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKST

     C*   worker's comp limits table file
     C     lmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkkey
     C                   kfld                    kkefdt
     C*
     C     lmky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkst
     C                   kfld                    kkkey

     C*   claim master file
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkemp#
     C                   kfld                    kkdiv

     C*   claim history file
     C     chky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

     C     chky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkMthDt

     C*   multiple claim file

     C*   generic table id file
     C     gtky          klist
     C                   kfld                    kkco#
     C                   kfld                    kktbid
     C                   kfld                    kkkey

     C*   claim legal master file
     C     llky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

     C*   occupation code table file
     C     ocky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkkey

     C*   wcstat extraction file
     C     ssky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkrpmy


     C*   WDPDL Key List
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod

     C*   WMCDL Key List
     C     cdky          klist
     C                   kfld                    cmss#

     C*   WDACL Key List
     C     acky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkseq#
     C*   WCSTAT Loss File Maintenance
     C     slky2         klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C                   kfld                    kkclm#
     C                   kfld                    ##rpmy
     c*
     C     slky3         klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C                   kfld                    kkclm#
     C*
     C*   KEY LIST FOR THE HARBOR EXPOSURE FILE
     C*
     C     heky          KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    kkFND
     C                   KFLD                    kkST
     C                   KFLD                    el2pfdt
     C*
     C     heky2         KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    kkFND
     C                   KFLD                    kkST
     C*   Key List State Code file for Reporting
     C*
     C     keyWTSTL      KLIST
     C                   KFLD                    kkCO#
     C                   KFLD                    sv2State
     C                   KFLD                    sv2StCd

     C     *entry        plist
     C                   parm                    @@fnrrn           9            fund rel rec#
     C                   parm                    @@rpmy            5            rpt date: cyymm
     C                   parm                    @@rptlvls         2            # of rpt levels
     C                   parm                    @@mode            1

     C     inAdjPl       plist
     C                   parm                    @@co#             3
     C                   parm                    @@fnd             3
     C                   parm                    @@fyr             3
     C                   parm                    @@emp#            9
     C                   parm                    @@div             5
     C                   parm                    @@amt             9
     C*  Call SA001R to retrieve the client number
     C
     C                   call      'SA001R'
     C                   parm                    @@wcif            1
     C                   parm                    @@mcif            1
     C                   parm                    @@wcin            1
     C                   parm                    @@ar              1
     C                   parm                    @@ntwk            1
     C                   parm                    @@ur              1
     C                   parm                    @@med             1
     C                   parm                    @@edi             1
     C                   parm                    @@wcdk            1
     C                   parm                    @@rhab            1
     C                   parm                    @@mss             1
     C                   parm                    @@fsch            1
     C                   parm                    @@prvb            1
     C                   parm                    @@npwc            1
     C                   parm                    @@os              1
     C                   parm                    @@arls            1
     C                   parm                    @@ccod            1
     C                   parm                    @@crvw            1
     C                   parm                    @@prom            1
     C                   parm                    @@mcrv            1
     C                   parm                    @@freq            1
     C                   parm                    @@crac            1
     C                   parm                    @@preo            1
     C                   parm                    @@parn            1
     C                   parm                    @@mgri            1
     C                   parm                    @@kcif            1
     C                   parm                    @@curs            1
     C                   parm                    @@pcib            1
     C                   parm                    @@clt#            3
     C                   parm                    @@ucpf            1
     C                   parm                    @@dcf             1
     C                   parm                    @@autb            1

     C                   call      'RTVCLTID'
     C                   parm                    CLTID             3
     C                   clear                   strtcg
     C
     C                   if        %parms > 3
     C                   Move      @@mode        xxmode            1
     C                   Endif
     C

     C                   exsr      srdriver

     C*====================================================================
     C*  mainloop - process all policies that fall within the date range
     C*             for the current level
     C*====================================================================
     C     mainloop      begsr
     C
     C*  Process policies that fall within date range for reporting level
     C     elky          setll     pel01
     C     elky1         reade     pel01                                  60
     C                   dow       *in60 = *off
     C                   eval      svpfdt = elpfdt
     C
     C*    Do not include flat cancelled policies
     C                   Eval      cxDate = GetCxDate(ELco#:ELfnd:ELemp#:ELdiv)
     C                   If        (ELpfDt < ELptDt) And
     C                             ((ELpfDt < cxDate And cxDate <> 0) Or
     C                              (cxDate = 0))
     C                   eval      kkemp# = elemp#
     C                   eval      kkdiv = eldiv
     C
     C*  Determine if this employer has already been processed.
     C*
     C                   move      elrtcd        kkrtcd
     C     rsky          chain     prs01                              60
     C                   movel(p)  rsjrst        kkkey
     C     stky          chain     pst01                              60
     C                   move      stnst         stcode
     C*
     C                   if        wrtLnkHdr(elco#:elfnd:##rpmy:
     C                                         elemp#:elfyr:stcode) = *on
     C
     C*    Only gather info for the state reporting div
     C     emky          chain     pem01                              61
     C                   dow       *in61 = *off
     C                             and emstrp <> 'Y'
     C     emky          reade     pem01                                  61
     C                   enddo
     C*
     C*  Get state reporting division policy eff dt to determine if this policy
     C*    should be reported.
     C*
     C                   eval       kkfyr = elfyr
     C     elky4         chain     pel02                              99
     C*** begin add ***  08/14/24  ********************************************
     C     elky2         Setll     pel0b
     C     elky2         Reade     pel0b                                  60
     C                   Move      ELBpfdt       el2pfdt
     C                   Move      ELBpfdt       elpfdt
     C                   Move      ELBpfdt       svpfdt
     C*** end add *****  08/14/24  ********************************************
     C                   if        *in99 = *off
     C                             and cvtToDate(el2pfdt) >= fromdt
     C                             and cvtToDate(el2pfdt) <= toDate
     C                             or *in99 = *on
     C                             and cvtToDate(svpfdt) >= fromdt
     C                             and cvtToDate(svpfdt) <= toDate
     C
     C*  Save the pol eff dt and min prem amt of the state reporting division.
     C*
     C                   eval      svminp = 0
     C*** begin add ***  08/28/25  *******************************************
     C                   Clear                   xxminf            1
     C*** end add *****  08/28/25  *******************************************
     C                   if        *in99 = *off
     C*
     C                   eval      svpfdt = el2pfdt
     C                   eval      svptdt = el2ptdt
     C                   if        el2faf = 'A'
     C                             and el2amnp = 'Y'
     C                   eval      svminp = el2aspr
     C*** begin add ***  08/28/25  *******************************************
     C                   Eval      xxminf = 'Y'
     C*** end add *****  08/28/25  *******************************************
     C                   else
     C                   if        el2faf <> 'A'
     C                             and el2emnp = 'Y'
     C                   eval      svminp = el2stpr
     C*** begin add ***  08/28/25  *******************************************
     C                   Eval      xxminf = 'Y'
     C*** end add *****  08/28/25  *******************************************
     C                   endif
     C                   endif
     C*
     C                   else
     C*
     C                   eval      svpfdt = elpfdt
     C                   eval      svptdt = elptdt
     C                   if        elfaf = 'A'
     C                             and elamnp = 'Y'
     C                   eval      svminp = elaspr
     C*** begin add ***  08/28/25  *******************************************
     C                   Eval      xxminf = 'Y'
     C*** end add *****  08/28/25  *******************************************
     C                   else
     C                   if        elfaf <> 'A'
     C                             and elemnp = 'Y'
     C                   eval      svminp = elstpr
     C*** begin add ***  08/28/25  *******************************************
     C                   Eval      xxminf = 'Y'
     C*** end add *****  08/28/25  *******************************************
     C                   endif
     C                   endif
     C*
     C                   endif
     C*
     C                   eval      noexpo = 'Y'
     C
     C*  Process all divisions for this employer.
     C
     C                   eval       kkfyr = elfyr
     C     elky2         chain     pel02                              62
     C                   dow       *in62 = *off
     C                   eval       kkdiv = el2div
     C
      /free
       //  Do not include flat cancelled policies
       cxDate = GetCxDate(EL2co#:EL2fnd:EL2emp#:EL2div);
       If (EL2pfDt < EL2ptDt) And
          ((EL2pfDt < cxDate And cxDate <> 0) Or (cxDate = 0));
      /end-free
     C                   MOVE      *BLANK        XXLRGD            1
     C*
     C                   movel(p)  el2bscd       kkkey
     C     tbky          chain     pbs01                              60
     C*
     C                   if        *in60 = *off and bslpgm = 'L'
     C*
     C     ldky          CHAIN     pld01                              60
     c                   if        *in60 = *off
     C                   MOVE      'Y'           XXLRGD
     C                   ENDIF                                                  *in60 = *off
     C*
     C                   endif
     c*
     C*                 -get link data
     C                   exsr      getLink
     C*                 -check if header record written; if not then write one
     C                   exsr      lnkHdrNmAd
     C*                 -Exposure record
     C                   if        curLvl = 1
     C                   exsr      exposure
     C*                 -non-payroll exposure
     C                   exsr      othExpArr
     C
     c                   if        xxlrgd <> 'Y'
     C                   move      el2rtcd       kkrtcd
     C     rsky          chain     prs01                              60
     C     ddky1         setgt     pdd01
     C     ddky2         readpe    pdd01                                  60
     c                   endif
     C
     C                   do        INDX          i
      /free
    
       sv2PrtZrFlg = 'N';
       sv2Status   = 'I';
       sv2State   = juris_st;
    
       Select;
       When i = 11;
          sv2StCd = rrterc;
       When i = 14;
          sv2StCd = rr1stc;
       When i = 15;
          sv2StCd = rr2ndc;
       When i = 16;
          sv2StCd = rr3rdc;
       When i = 18;
          sv2StCd = rrdtrc;
       When i = 19;
          sv2StCd = rr4thc;
       When i = 20;
          sv2StCd = rr5thc;
       When i = 23;
          sv2StCd = rr6thc;
       When i = 24;
          sv2StCd = rr7thc;
       Other;
          sv2StCd = *Zeros;
      EndSl;

       Chain(n) keyWTSTL pst00;
       If %Found(wtstl);
          sv2PrtZrFlg = ST2flag;
          sv2Status   = ST2stat;
       EndIf;

      /End-free
     C                   if        othExp(i) <> 0
     c                             or sv2PrtZrFlg = 'Y' and sv2Status = 'A'
     c*** Begin Add *** 12/14/20 *******************************************
     c                             And xxMode = 'F'
     c*** End   Add *** 12/14/20 *******************************************
     C                   exsr      wrtOthExp
     C                   endif
     C                   enddo
     C                   clear                   othexp
     C
     C*                 -merit rating
     C                   if        el2prom = 'M'
     C                   exsr      srMerit
     C                   endif
     C
     C*                 -indiana flat rate
     C                   if        juris_st = 'IN'
     C                   exsr      inAdjust
     C                   endif                                                  juris_st=IN
     C
     C*
     C                   if        juris_st = 'NY'
     C                   exsr      NYadjust
     C                   endif
     C*
      *** Beg Add ***** 08/05/25 *****************************
     C*                 -MA ARAP Charges
     C                   if        juris_st = 'MA'
     C                   exsr      maARAP
     C                   endif                                                  juris_st=MA
      *** End Add ***** 08/05/25 *****************************
     C
     C*                 -short rate
     C                   exsr      Short_rt
     C
     C*                 -harbor exposure balance to minimum
     C                   if        el2hxmn <> 0
     C                   exsr      srHarborMin
     C                   endif
     C
     C*                 -no exposure
     C                   exsr      No_Expos
     C
     C                   endif                                                  curLvl=1
     C
     C*                 -loss data
     C                   exsr      lossdata
     C*
     C*      Accumulate standard premium values over all divs for unit total record
     C                   if        AdtPyrl = *on
     c                   if        juris_st = 'CA'
     c                   eval      Ustd_prem = Ustd_prem +
     c                             (el2anmp + el2prtd + el2adct + el2add2)
     c                   else
     C                   add       el2aspr       Ustd_prem                      audited amt
     c                   endif                                                  juris_st = 'CA'
     C                   else
     c                   if        juris_st = 'CA'
     c                   eval      Ustd_prem = Ustd_prem +
     c                             (el2enmp + el2prtd + el2edct + el2edd2)
     c                   else
     C                   add       el2stpr       Ustd_prem                      non-adtd amt
     c                   endif                                                  juris_st = 'CA'
     C                   endif
     C
     C                   eval      Usubj_prem = Usubj_prem + prem_ttl

     C                   eval      kkstcd = stcode
     C                   exsr      unitTotal
     C                   eval      prem_ttl = 0
     C
      /free
       endif;
      /end-free
     C     elky2         reade     pel02                                  62
     C                   enddo                                                  *in62=*off
     C
     C                   if        curLvl = 1
     C*                 -minimum premium
     C                   exsr      Min_Prem
     C
     C                   eval      kkstcd = stcode
     C                   exsr      unitTotal
     C
     C                   endif                                                  curLvl=1
     C
     C                   endif                                                  *in61=*off
     C*
     C                   endif                                                  *in99=*off
     C
     C                   endif                                                  elpfdt<>elptdt

     C                   eval      *in91 = *off
     C
     C     elky1         reade     pel01                                  60
     C                   enddo                                                  *in60=*off
     C
     C                   endsr                                                  mainloop


     C*====================================================================
     C*  mainloopG - process all policies for the GIC Master Group for
     C*      for the current level.
     C*====================================================================
     C     mainloopG     begsr
     C
     C*  Retrieve the Master Policy information.
     C     emky          chain     pem01                              99
     C                   eval      kkfyr = svfyr
     C                   eval      kkpfdt = svpfdt
     C     elky6         chain     pel01                              99
     C
     C*  Save the pol eff dt.
     C*
     C                   eval      svpfdt = elpfdt
     C                   eval      svptdt = elptdt
     C*
     C*  Process all policies within the group, for the current level (year).
     C*
     C     elky5         chain     pel02                              62
     C                   dow       *in62 = *off
     C                   eval       kkdiv = el2div
     C
     C*    Do not include flat cancelled policies
     C                   Eval      cxDate =
     C                                GetCxDate(EL2co#:EL2fnd:EL2emp#:ELdiv)
     C                   If        (EL2pfDt < EL2ptDt) And
     C                             ((EL2pfDt < cxDate And cxDate <> 0) Or
     C                              (cxDate = 0))
     C                   eval      kkemp# = el2emp#
     C                   eval      kkdiv = el2div
     C*
     C                   eval      svminp = 0
     C                   eval      noexpo = 'Y'
     C*
     C*   Save the minimum premium amount from the main division.
     C                   if        el2div = 0
     C                   if        el2faf = 'A'
     C                             and el2amnp = 'Y'
     C                   eval      svminp = el2aspr
     C                   else
     C                   if        el2faf <> 'A'
     C                             and el2emnp = 'Y'
     C                   eval      svminp = el2stpr
     C                   endif
     C                   endif
     C                   endif
     C*
     C                   MOVE      *BLANK        XXLRGD
     C*
     C                   movel(p)  el2bscd       kkkey
     C     tbky          chain     pbs01                              60
     C*
     C                   if        *in60 = *off and bslpgm = 'L'
     C*
     C     ldky          CHAIN     pld01                              60
     c                   if        *in60 = *off
     C                   MOVE      'Y'           XXLRGD
     C                   ENDIF                                                  *in60 = *off
     C*
     C                   endif
     c*
     C
     C*                 -get link data
     C                   exsr      getLink
     C*                 -check if header record written; if not then write one
     C                   exsr      lnkHdrNmAd
     C*                 -Exposure record
     C                   if        curLvl = 1
     C                   exsr      exposure
     C*                 -non-payroll exposure
     C                   exsr      othExpArr
     C
     c                   if        xxlrgd <> 'Y'
     C
     C                   move      el2rtcd       kkrtcd
     C     rsky          chain     prs01                              60
     C     ddky1         setgt     pdd01
     C     ddky2         readpe    pdd01                                  60
     C
     c                   endif
      /free
    
       sv2PrtZrFlg = 'N';
       sv2Status   = 'I';
       sv2State   = juris_st;
    
       Select;
       When i = 11;
          sv2StCd = rrterc;
       When i = 14;
          sv2StCd = rr1stc;
       When i = 15;
          sv2StCd = rr2ndc;
       When i = 16;
          sv2StCd = rr3rdc;
       When i = 18;
          sv2StCd = rrdtrc;
       When i = 19;
          sv2StCd = rr4thc;
       When i = 20;
          sv2StCd = rr5thc;
       When i = 23;
          sv2StCd = rr6thc;
       When i = 24;
          sv2StCd = rr7thc;
       Other;
          sv2StCd = *Zeros;
      EndSl;

       Chain(n) keyWTSTL pst00;
       If %Found(wtstl);
          sv2PrtZrFlg = ST2flag;
          sv2Status   = ST2stat;
       EndIf;

      /End-free
     C
     C                   do        INDX          i
     C                   if        othExp(i) <> 0
     c                             or sv2PrtZrFlg = 'Y' and sv2Status = 'A'
     c                             And xxMode = 'F'
     C                   exsr      wrtOthExp
     C                   endif
     C                   enddo
     C                   clear                   othexp
     C
     C*                 -merit rating
     C                   if        el2prom = 'M'
     C                   exsr      srMerit
     C                   endif
     C
     C*                 -indiana flat rate
     C                   if        juris_st = 'IN'
     C                   exsr      inAdjust
     C                   endif                                                  juris_st=IN
     C
      *** Beg Add ***** 08/05/25 *****************************
     C*                 -MA ARAP Charges
     C                   if        juris_st = 'MA'
     C                   exsr      maARAP
     C                   endif                                                  juris_st=MA
      *** End Add ***** 08/05/25 *****************************
     C
     C*                 -short rate
     C                   exsr      Short_rt
     C
     C*                 -harbor exposure balance to minimum
     C                   if        el2hxmn <> 0
     C                   exsr      srHarborMin
     C                   endif
     C
     C*                 -no exposure
     C                   exsr      No_Expos
     C
     C                   endif                                                  curLvl=1
     C
     C*                 -loss data
     C                   exsr      lossdata
     C*
     C*      Accumulate standard premium values over all divs for unit total record
     C
     C                   if        AdtPyrl = *on
     c                   if        juris_st = 'CA'
     c                   eval      Ustd_prem = Ustd_prem +
     c                             (el2anmp + el2prtd + el2adct + el2add2)
     c                   else
     C                   add       el2aspr       Ustd_prem                      audited amt
     c                   endif                                                  juris_st = 'CA'
     C                   else
     c                   if        juris_st = 'CA'
     c                   eval      Ustd_prem = Ustd_prem +
     c                             (el2enmp + el2prtd + el2edct + el2edd2)
     c                   else
     C                   add       el2stpr       Ustd_prem                      non-adtd amt
     c                   endif                                                  juris_st = 'CA'
     C                   endif
     C
     C                   eval      Usubj_prem = Usubj_prem + prem_ttl

     C                   eval      kkstcd = stcode
     C                   eval      kkemp# = elemp#
     C                   exsr      unitTotal
     C                   eval      prem_ttl = 0
     C                   endif
     C
     C     elky5         reade     pel02                                  62
     C                   enddo                                                  *in62=*off
     C
     C                   eval      *in91 = *off
     C
     C                   endsr                                                  mainloopG
     C

     C*============================================================================
     C*  getLink - Subroutine to get the information for the Link data
     C*====================================================================
     C     getLink       begsr
     C*
     C     f2ky          chain     pf201                              60
     C                   eval      link_car# = f2car#
     C                   eval      link_bsi# = f2bsi#
     C*
     C* Use old NCCI Carrier # for LCTA if processing policy prior to 1/1/2016.
     C*
     C                   If        cltid = '045' and EL2Pfdt < 1160101
     C                   Eval      link_car# = 33057
     C                   Endif
     C                   eval      link_emp# = el2emp#
     C                   eval      link_fyr = el2fyr
     C*
     C                   move      el2rtcd       kkrtcd
     C     rsky          chain     prs01                              60
     C                   movel(p)  rsjrst        kkkey
     C                   eval       juris_St = rsjrst
     C     stky          chain     pst01                              60
     C                   eval      link_stcd = stnst
       // Is this an NCCI State?
       ncciState = *Off;
       Chain (kkCo#:'WCST':kkKey) pgt01;
       If %Found(wtgtl) And GTstat <> 'I';
          ncciState = *On;
       EndIf;
     C*
     c
     C     Elkyb         Setll     pel0b
     C     Elkyb         reade     pel0b                                  60
     C                   dow       *in60 = *off
     c
     c                   move      elbrtcd       kkrtcd
     c     rsky          chain     prs01                              60
     c
     c                   If        *in60 = *off and
     c                             rsjrst    = JUris_st
     c*** Beg Chg *** 04/09/24 ****************************************
     c*****************  eval      EffMod(1) = ELbpfdt
     c                   eval      EffMod(1) = svPfDt
     c*** End Chg *** 04/09/24 ****************************************
     c                   leave
     c                   endif
     c
     C     Elkyb         reade     pel0b                                  60
     C                   enddo
     C*  Save the rate state of the state reporting division.
     C*
     C                   if        el2div = emdiv
     C                   move      stnst         svstcd
     C                   endif
     C*
     C                   eval      link_crt# = ' '
     C*
     C                   eval      link_efdt = svpfdt
     C                   eval      link_rpt# = lvl(curLvl)
     C                   eval      link_corr = '0'
     C*
     C                   eval      kkst = rsjrst
     C                   eval      kkefdt = svpfdt
     C*
     C*    ASWG submission flag
     C                   eval       ASWGflag = ' '
     C
     C                   if        elpfdt >= ASWGdate
     C                   eval       ASWGflag = 'Y'
     C                   endif                                                  llpfdt>=ASWGdate
     C*
     C                   endsr                                                  getLink



     C*====================================================================
     C*  LnkHdrNmAd - Subroutine to get Link, Header, Name and Address info
     C*====================================================================
     C     LnkHdrNmAd    begsr
     C*
     C                   move      link_stcd     stcode
     C                   if        wrtLnkHdr(elco#:elfnd:##rpmy:
     C                                         elemp#:elfyr:stcode) =*on
     C*
     C                   clear                   psr01
     C                   eval      srco# = fnco#
     C                   eval      srfnd = fnfnd
     C                   eval      srrpmy = ##rpmy
     C*
     C*  Assign Link data
     C                   eval      srcarr = link_car#
     C                   eval      srbsi# = link_bsi#
     C                   eval      sremp# = link_emp#
     C                   eval      srfyr = link_fyr
     C*
     C                   move      link_stcd     srstcd
     C*
     C                   eval      srcrt# = link_crt#
     C*
     C                   eval      srefdt = link_efdt
     C                   eval      srrpt# = link_rpt#
     C                   move      link_corr     srcorr
     C*
     C*    ASWG submission flag
     C                   if        ASWGflag = 'Y'
     C                   if        srstcd = 04
     C                   eval       sraswg = 'E'
     C                   else
     C                   eval       sraswg = 'A'
     C                   endif
     C                   endif
     C*
     C*    Policy conditions
     C*          Multi state rated  AND  Estimated Audit/Exposure
     C                   eval       multiSt = 'N'
     C                   eval       interSt2 = 'N'
     C                   eval       interSt1 = 0
     C                   eval       hold_jrst = RSJRST
     C                   eval       estAudit = 0
     C                   eval       estExp = 'N'
     C                   eval       adtPyrl = *on
      /Free
       If Elancf = 'Y' And Not ncciState And hold_jrst <> 'MN';
          estExp = 'U';
       Endif;
      /End-Free
     C*
     C                   eval       kkfyr = elfyr
     C     elky3         chain     pel03                              60

     C                   dow       *in60 = *off
     C                   move      el3rtcd       kkrtcd
     C     rsky          chain     prs01                              60
     C                   if        (*in60=*off)  AND  (rsjrst <> hold_jrst)
     C                   eval       multiSt = 'Y'
     C                   endif
     C*
     C*            Estimated audit and estimated exposure
     C                   if        el3faf <> 'A'
     C                   eval       estAudit = 1
     C                   eval       estExp = 'Y'
     C                   eval       adtPyrl = *off
     C                   endif
     C*
     C     elky3         reade     pel03                                  60
     C                   enddo                                                  *in60=*off

     C*          Interstate rated
     C                   if        (emexrt = '1' or emexrt = '2') And
     C                             juris_St <> 'NJ'
     C                   eval       interSt1 = 1
     C                   eval       interSt2 = 'Y'
     C                   endif
     C
     C*          Cancelled policy
     C                   eval      cxDate = GetCxDate(elco#:elfnd:ememp#:emdiv)
     C                   if        cxDate = 0
     C                   eval       canxPol = 1
     C                   eval       canxMid = 'N'
     C                   eval       srcndt = svptdt
     C                   else
     C                   if        cxDate < svptdt
     C                   eval       canxPol = 1
     C                   eval       canxMid = 'Y'
     C                   move      cxDate        srcndt
     C                   else
     C     ehky          setgt     peh01
     C     ehky          readpe    peh01                                  60
     C                   if        *in60=*off
     C                             and ehptdt > cxDate
     C                   eval       canxPol = 1
     C                   eval       canxMid = 'Y'
     C                   move      cxDate        srcndt
     C                   else
     C                   eval       canxPol = 0
     C                   eval       canxMid = 'N'
     C                   eval       srcndt = svptdt
     C                   endif                                                  *in60=*off
     C                   endif                                                  cxDate<elptdt
     C                   endif                                                  cxDate=0

     C*          Retrospective rated
     C                   movel(p)  el2bscd       kkkey
     C     tbky          chain     pbs01                              60
     C                   if        bslpgm = 'P' or bslpgm = 'I'
     C                   eval       retroRat1 = 1
     C                   eval       retroRat2 = 'Y'
     C                   else
     C                   eval       retroRat1 = 0
     C                   eval       retroRat2 = 'N'
     C                   endif
     C*          Managed care organization
     C                   if        el2mccd = '00'
     C                               or el2mccd = *blanks
     C                   eval       mco1 = 0
     C                   eval       mco2 = 'N'
     C                   else
     C                   eval       mco1 = 1
     C                   eval       mco2 = 'Y'
     C                   endif
     C*          Three year fixed
     C                   eval      threeYr = 'N'
     C
     C*          Default these fields
     C                   eval      excessCvg = 0                                default
     C                   eval      asgnRisk = 0                                 default
     C                   eval      diseaseB = 0                                 default
     C                   eval      exclDis = 0                                  default
     C                   eval      clerErr = 0                                  default
     C                   eval      noExcPay = 0                                 default
     C                   eval      lrgRisk = 0                                  default
     C*          Report field  9'Policy conditions' if not in ASWG format
     C*            or report field 25'Policy conditions' if in ASWG format
     C                   eval      srpolc = PolCond2
     C                   move      PolCond1      srpcon
     C*
     C*    Risk ID#
     C                   eval      srrsk# = emrsk#
     C*    Pending file#
     C                   eval      srpnd# = ' '
     C*    Term
     C                   eval      srterm = 1                                   default
     C*    Unit report resubmission indicator
     C                   eval      srurri = ' '
     C                   If        juris_St = 'TX' And EMlsID <> '1'
     C                   eval      srurri = 'E'
     c                   EndIf
     C*    Policy type identification code
     C                   eval      srptyp = 1
     C*    Replacement report indicator
     C                   eval      srrrpt = ' '

     C*  ONLY REPORT THE FOLLOWING FIELDS IF IN ASWG FORMAT
     C                   if        ASWGflag = 'Y'
     C*    Term
     C                   eval      srterm = 1                                   default
     C*    Correction type
     C                   move      *blank        srctyp
     C*    State effective date
     C                   eval      srsted = 0                                   default
     C                   if        el2pfdt <> svpfdt
     C                   eval      srsted = el2pfdt
     C                   endif

     C*    Federal employer identification #
     C                   eval      srfein = emfein
     C*    Policy type ID code
     C                   eval      srpid# = 010101                              default
     C                   if        el2exmf = 'Y'
     C                   if        juris_St = 'NJ'
     C                              or juris_St = 'NY'
     C                              or juris_St = 'TX'
     C                   eval      srpid# = 010102                              default
     C                   endif                                                  ELDCDE<>*blank
     C                   endif                                                  ELDCDE<>*blank
     C*    Deductible type code
     c                   if        xxlrgd <> 'Y'
     C                   if        el2dcde <> *blank
     C                   move      juris_St      kkst
     C                   move      el2dcde       kkdcde
     C                   z-add     el2dcef       kkdcef
     C*
     C     ddky          setgt     pdd01
     C                   readp     pdd01                                  60
     C                   if         *in60=*off
     c                             and (ddstat = 'A')
     C
     C                   eval       ded_ty12 = GetDedTyp(fnco# : dedRsvCod)
      /free
       Select;
       When DDVerb = 'C' and DDAmag <> 0 and DDAmpo <> 0;
          ded_ty34 = '10';
       When DDVerb <> 'C' and DDAmag <> 0 and DDAmpo <> 0;
          ded_ty34 = '09';
       When DDAmag <> 0 and DDAmpo = 0;
          ded_ty34 = '03';
       When DDVerb <> 'C' and DDAmag = 0 and DDAmpo <> 0;
          ded_ty34 = '02';
       When DDVerb = 'C' and DDAmag = 0 and DDAmpo <> 0;
          ded_ty34 = '01';
       Other;
          ded_ty34 = '00';
       Endsl;
      /end-free
     C                   movel     ded_type      srdtyp
     C*
     C*    Deductible amount per claim/accident AND Deductible amount-aggregate
     C                   eval      srdamp = ddampo
     C                   eval      srdagg = ddamag
     C*
     C                   endif                                                  *in60=*off
     C                   endif                                                  ELDCDE<>*blank
     c                   else

     C                   eval       ded_ty12 = GetDedTyp(fnco# : ldedRsvCod)
      /free
       Select;
       When LDVrb1 = 'C' and LDAmg1 <> 0 and LDAmp1 <> 0;
          ded_ty34 = '10';
       When LDVrb1 <> 'C' and LDAmg1 <> 0 and LDAmp1 <> 0;
          ded_ty34 = '09';
       When LDAmg1 <> 0 and LDAmp1 = 0;
          ded_ty34 = '03';
       When LDVrb1 <> 'C' and LDAmg1 = 0 and LDAmp1 <> 0;
          ded_ty34 = '02';
       When LDVrb1 = 'C' and LDAmg1 = 0 and LDAmp1 <> 0;
          ded_ty34 = '01';
       Other;
          ded_ty34 = '00';
       Endsl;
      /end-free
     C                   movel     ded_type      srdtyp
     C*
     C*    Deductible amount per claim/accident AND Deductible amount-aggregate
     C                   eval      srdamp = ldamp1
     C                   eval      srdagg = ldamg1

     c                   endif                                                  if xxlrgd <> 'Y'
     C*
     C*    Deductible percent
     C                   eval      srdpct = 0
     C                   if        ded_ty34 = '04'
     C                             or ded_ty34 = '05'
     C                             or ded_ty34 = '06'
     C                             or ded_ty34 = '07'
     C                             or ded_ty34 = '08'
     C                             or ded_ty34 = '11'
     c                   if        xxlrgd <> 'Y'
     C                   eval       srdpct = el2ddct
     c                   else
     C                   eval       srdpct = ldpct1
     c                   endif                                                  if xxlrgd <> 'Y'
     C                   endif
     C*
     C                   endif                                                  ASWGflag='Y'
     C*
     C*   All "Previous" fields should be 0/blank on initial extract
     C                   eval      srprp# = 0
     C                   eval      srpcor = ' '
     C                   eval      srpcar = 0
     C                   eval      srppol = ' '
     C                   eval      srpefd = 0
     C                   eval      srpexp = 0
     C                   eval      srpcer = ' '

     C*
     C*  Get Name
     C     Lower:Upper   xlate     emname        srname
     C                   IF        FNPEOF <> ' '
      *
     c                   MOVE      ELCO#         QQCO#
     c                   MOVE      ELFND         QQFND
     c                   MOVE      ELEMP#        QQEMP#
     c                   MOVE      ELDIV         QQDIV
     c                   MOVE      ELFYR         QQFYR
      *
     c                   CALL      'SRADD1'
     c                   PARM                    QQCO#             3 0
     c                   PARM                    QQFND             3 0
     c                   PARM                    QQEMP#            9 0
     c                   PARM                    QQDIV             5 0
     c                   PARM                    QQFYR             3 0
     c                   PARM                    QQNAME          100
     c                   PARM                    QQFNAM          100
     c                   PARM                    QQEAD1           40
     c                   PARM                    QQEAD2           40
     c                   PARM                    QQECSZ           40
     C                   PARM                    QQCTY            25
     C                   PARM                    QQST              2
     C                   PARM                    QQZIP             9 0
     C                   PARM                    QQFEIN            9 0
     C                   PARM                    QQEADR           90
      *
     C                   MOVEL     QQNAME        SRNAME
      *
     C                   ENDIF                                                  FNPEOF <> ' '
     C*
     C*  Get Address data (if jurisdiction state is 'NJ' use shorter field for submit)
     C                   if        emmpfl = 'P'
       //*** Beg Chg *** 03/25/24 ************************************
     C***********        eval       sraddr = %trim(empad1) + %trim(empad2)
     C***********        move      empzip        xxzip
     C***********        eval       xxctystzip = %trim(empcty) + ' ' + empst +
     C***********                                                   ' ' + xxzip
       xxZip = %Editc(EMpZip:'X');
       If EMpAd2 <> *blanks;
          xxAddress = %Trim(EMpAd1) + ' ' + %Trim(EMpAd2);
        SRaddr = %Trim(xxAddress) + ' ' + %trim(empcty) +
        ' ' + empst + ' ' + xxZip;
       Else;
          SRaddr = %Trim(EMpAd1) + ' ' + %trim(empcty) +
           ' ' + empst + ' ' + xxZip;
       Endif;
       //*** End Chg *** 03/25/24 ************************************
     C                   if        juris_St = 'NJ'
     C                   movel(p)  sraddr        NJaddr
       //*** Beg Del *** 03/25/24 ************************************
     C***********        move      xxctystzip    NJaddr
     C***********        movel(p)  NJaddr        sraddr
     C***********        else
     C***********        move      xxctystzip    sraddr
       //*** End Del *** 03/25/24 ************************************
     C                   endif                                              juris_St='NJ'
     C*
     C                   else
     C*
       //*** Beg Chg *** 03/25/24 ************************************
     C***********        eval       sraddr = %trim(emmad1) + %trim(emmad2)
     C***********        move      empzip        xxzip
     C***********        eval       xxctystzip = %trim(emmcty) + ' ' + emmst +
     C***********                                                   ' ' + xxzip
       xxZip = %Editc(EMmZip:'X');
       If EMmAd2 <> *blanks;
          xxAddress = %Trim(EMmAd1) + ' ' + %Trim(EMmAd2);
        SRaddr = %Trim(xxAddress) + ' ' + %trim(emmcty) +
         ' ' + emmst + ' ' + xxZip;
       Else;
        SRaddr = %Trim(EMmAd1) + ' ' + %trim(emmcty) +
         ' ' + emmst + ' ' + xxZip;
       Endif;
       //*** End Chg *** 03/25/24 ************************************
     C                   if        juris_St = 'NJ'
     C                   movel(p)  sraddr        NJaddr
       //*** Beg Del *** 03/25/24 ************************************
     C*****************  move      xxctystzip    NJaddr
     C*****************  movel(p)  NJaddr        sraddr
     C*****************  else
     C*****************  move      xxctystzip    sraddr
       //*** End Del *** 03/25/24 ************************************
     C                   endif                                              juris_St='NJ'
     C*
     C                   endif                                              emmpfl='P'
     C*
     C*    Maintenance fields
     C                   eval      srstat = 'A'
     C                   eval      sruser = q1user
     C                   eval      srpgm = q1pgm
     C                   eval      srtime = dstime
     C                   eval      srdt = dtCYMD(dsdate)
     C                   eval      sridt = srdt
     C                   eval      sriusr = sruser
     C*
     C                   write     psr01
     C                   add       1             U#lnkhdr
     C*
     C                   endif                                                  wrtLnkHdr
     C*
     C                   endsr                                                  lnkHdrNmAd


     C*==========================================================================
     C*  Exposure - Subroutine to get exposure record info.  Multiple exposure
     C*             records can be written for each employer based on class code
     C*             experience mod date, rate date, and manual rate.
     C*==========================================================================
     C     Exposure      begsr
     C
     C*  If the experience modifier is 0, change to 1.000.
      /Free
        If EL2noexm1 = 'Y';
           expmod(1) = 0;
        Endif;
      /End-Free
     C
     C                   if        expmod(1) = 0
     C                             and juris_st <> 'PA'
     C                             and juris_st <> 'DE'
     C                             and juris_st <> 'CA'
     C                   eval      expmod(1) = 1
     C                   endif
     C                   eval      xxadld = 0
     C                   eval      xxmrrt = 0
     C
     C*  If the policy is merit rated in PA or DE, the exp mod should be 0
     C*  for all exposure records, per the bureau.
     C
     C                   if        el2prom = 'M'
     C                   if        juris_st = 'PA'
     C                             or juris_st = 'DE'
     C                   eval      expmod(1) = 0
     C                   eval      expmod(2) = 0
     C                   eval      expmod(3) = 0
     C                   endif
     C                   endif
     C*
     C                   exsr      Rate_array
     C                   exsr      splt_array

     C*    Get the aircraft surcharge code
     C                   exsr      stat_codes


     C*
     c                   Eval      polEffDt = EL2pfDt
     C     epky          chain     pep01                              62
     C                   dow       *in62 = *off
     C
     C*     Only process if class code is active and rate does not equal 0
     C                   if        (epstat = 'A')
     C                             and epefdt < el2ptdt
     C                             or @@clt# = '135' and epstat = 'A'

     C                   if        juris_st = 'IL'
     C*
     C                   if        adtPyrl = *on
     C                   z-add     epampr        xxwk10           11 0
     C                   else
     C                   z-add     epempr        xxwk10
     C                   endif
     C*
     C                   eval(h)   xtpred = xtpred + xxwk10
     C                   endif

     C                   if        juris_st = 'NY'
     C*
     C                   if        adtPyrl = *on
     C                   z-add     epampr        xxwk10
     C                   else
     C                   z-add     epempr        xxwk10
     C                   endif
     C*
     C                   eval(h)   xtpredNY = xtpredNY + xxwk10
     C                   endif
     C
     C                   if        adtPyrl = *on
     C                   eval      expa = epaapr
     C                   else
     C                   eval      expa = epeapr
     C                   endif
     C
     C                   eval      kkclas = epclas
     C                   eval      kkefdt = epefdt
     C                   eval      percap = *off
     C                   eval      kkrtcd = el2rtcd
     C                   if        fnpeof <> ' '
     C                   eval      kkefdt = svpfdt
     C                   endif
     c                   move      *off          QQFOUND
     C                   IF        @@CLT# = '102'
     C                   CALL      'SRLOGCLAS'
     C                   PARM                    EPCLAS
     C                   PARM      *OFF          QQFOUND
     C                   PARM      *ZEROS        QQMPAYR
     C                   PARM      *ZEROS        QQMRATE
     C                   PARM      *ZEROS        QQRPTCLS
     c                   endif                                                  @@CLT# = '102'
     C     QQFOUND       IFEQ      *ON
     C                   EVAL      kkclas = QQRPTCLS
     C                   endif
     C                   Z-ADD     0             KKEFD1
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   KKCLAS        ##CLAS            5
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
     C                   MOVE      ##DATE        KKEFD1

     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  70
     C   70prky          setgt     ppr01
     C   70prky1         reade     ppr01                                  70
     C   70              eval      prlhcd = *blank
     C   70              eval      prtier = *blanks
     C   70              eval      prpcf  = *blank
     C                   if        *in70 = *off
     C                             and prpcf = 'C'
     C                             or *in70 = *off and prpcf = 'O'
     C                   eval       percap = *on
     C                   endif
     C                   if        preact = '  '
     C                   eval      preact = '01'
     C                   endif
     C
     C                   if        prlhcd = 'Y' or epspec = 'F'
     C                   eval       expo = 02
     C                   else
     C                   eval       expo = 01
     C                   endif
     C                   if        preact = '00'
     C                   eval       expo = 00
     C                   endif
     C
     C*  If clt135 and the rate is incorrect, change the rate and exposure amounts.
     C*
     C                   if        @@clt# = '135' and *in70 = *off
     C                             and el2stat <> 'Y' and eparat <> prrt
     C                             and prrt <> 0
     C
     C                   eval      eparat = prrt
     C
     C                   if        percap = *on
     C
     C                   if        adtPyrl = *on
     C                   eval(h)   expa = epampr/prrt
     C                   else
     C                   eval(h)   expa = epempr/prrt
     C                   endif
     C
     C                   else
     C
     C                   if        adtPyrl = *on
     C                   eval(h)   expa = (epampr * 100)/prrt
     C                   else
     C                   eval(h)   expa = (epempr * 100)/prrt
     C                   endif
     C
     C                   endif
     C
     C                   endif
     C
     C*      additional load exposure record
     C                   if        epclas = rrdccd
     C                             or proccd = 'Y'
     C                             or prnrat = 'Y'
     C                   move      prtier        tier
     C                   if        juris_st <> 'NJ'
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : epclas : 00 : ExpMod(1) :
     C                             effMod(1) : rate_dt(1) : (10 * expa) :
     C                             epadld : eparat : tier : expo : 0)
     C
     C                   else
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : epclas : 00 : ExpMod(1) :
     C                             rate_dt(1) : rate_dt(1) : (10 * expa) :
     C                             epadld : eparat : tier : expo : 0)
     C                   endif
     C
     C                   if        epadld <> 0
     C                             and proccd <> 'Y'
     C                             and prnrat <> 'Y'
     C                   eval      xxadld = xxadld + epadld
     C                   endif
     C
     C*      regular payroll exposure record
     C                   else
     C                   eval      idx = 1
     C     epefdt        lookup    splt_dt(idx)                         8080
     C
     C                   eval(h)   prem = (expa * eparat)/100
     c                   move      *off          QQFOUND
     C                   IF        @@CLT# = '102'
     C                   CALL      'SRLOGCLAS'
     C                   PARM                    EPCLAS
     C                   PARM      *OFF          QQFOUND
     C                   PARM      *ZEROS        QQMPAYR
     C                   PARM      *ZEROS        QQMRATE
     C                   PARM      *ZEROS        QQRPTCLS
     c                   endif                                                  @@CLT# = '102'
     C                   IF        QQFOUND = *ON
     C                             OR eparat = 0
     C                   if        adtPyrl = *on
     C                   eval      prem = epampr
     C                   else
     C                   eval      prem = epempr
     C                   endif
     C                   endif
     C                   eval      begDt = epefdt
     C                   eval      endDt = el2ptdt
     C                   if         epcadt <> 0
     C                   eval       endDt = epcadt
     C                   endif
     C
     C
     C                   if        prlhcd = 'Y' or epspec = 'F'
     C                   eval       exp# = 21
     C                   else
     C                   eval       exp# = 11
     C                   endif
     C                   eval       exp# = 00

      * CLT102 mod - Convert data on logging class codes so that is reported as payroll
      *              amounts and rates
     C
     c                   move      *off          QQFOUND
     C                   If        @@clt# = '102'
     C                   CALL      'SRLOGCLAS'
     C                   PARM                    EPCLAS
     C                   PARM      *OFF          QQFOUND
     C                   PARM      *ZEROS        QQMPAYR
     C                   PARM      *ZEROS        QQMRATE
     C                   PARM      *ZEROS        QQRPTCLS
     c                   endif                                                  @@CLT# = '102'
     C                   IF        QQFOUND = *ON
     C                   eval      EPCLAS = QQRPTCLS
     C                   eval      eparat = prrt
     C                   eval(h)   expa = (prem * 100)/prrt
     C                   endif
     C
     C

     C                   callp     splitPyrl(epco# : epfnd : epfyr :
     C                               epemp# : epdiv : epclas : epspec :
     C                               idx : expa : prem : eparat :
     C                               epefdt : begDt : endDt : percap : prpcf)

     C                   if        juris_st = 'NJ'
     c                             or juris_st = 'CA'
     C                   xfoot     expamt        man_prem
     C                   endif

     C                   do        5             i
     C                   if        expamt(i) <> 0
     C                             or juris_st = 'NJ'
     C                             and man_prem = 0
     C                             and i = 1
     C                             and manrt(i) <> 0
     C                             or juris_st = 'CA'
     C                             and man_prem = 0
     C                             and i = 1
     C                             and manrt(i) <> 0
     C
     C                   if        percap = *off
     C                             and prpcf <> 'O'
     C                   eval(h)   uexp_pay = uexp_pay + expamt(i)
     C                   endif
     C                   eval      man_prem = man_prem + expamt(i)
     C
     C                   If        polEffDt = 0
     c                   Eval      polEffDt = pefdt(i)
     c                   EndIf
     C                   if        juris_st = 'NJ'
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : epclas : expos#(i) :
     C                             pexmd(i) : pefdt(i) : polEffDt :
     C                             expamt(i) : pramt(i) : manrt(i) : rttier(i) :
     C                             expact(i) : i-1)
     C                   Else
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : epclas : expos#(i) :
     C                             pexmd(i) : pefdt(i) : pefdt(i) :
     C                             expamt(i) : pramt(i) : manrt(i) : rttier(i) :
     C                             expact(i) : i-1)
     C                   EndIf
     C
     C
     C                   eval(h)   prem_ttl = prem_ttl + pramt(i)
     C
     C                   if        pexmd(i) = 0
     C                   eval(h)   mod_prem = mod_prem + pramt(i)
     C                   else
     C                   eval(h)   mod_prem = mod_prem + (pramt(i)*pexmd(i))
     C                   endif

     D*** Beg Chg *** 07/16/24 *********************************************
     C*****              eval      prem_tl(i) = prem_tl(i) + pramt(i)
     C                   eval(h)   prem_tl(i) = prem_tl(i) + pramt(i)
     D*** End Chg *** 07/16/24 *********************************************
     C
     C                   endif                                                  expamt(i)<>0
     C                   enddo                                                  do 5 i

     C                   endif                                                  epclas=rrdccd

     C                   exsr      clrPyrlArr

     C                   endif                                                  epstat='A'
     C
     C     epky          reade     pep01                                  62
     C                   enddo                                                  *in61=*off
     c                   z-add     1110901       xxSpltDt
     c                   eval      xxstcd = 0
     c                   if        juris_st  = 'IL' and
     c                             elpfdt < xxSpltDt and
     c                             elptdt > xxSpltDt and
     c                             ApplyRateSplit(juris_st:xxSpltDt:xxstcd)
     c                   if        xxstcd <> 0
     c                   exsr      srFlatRateAdj
     C                   endif
     C                   endif

     c                   z-add     1170410       xxSpltDt
     c                   eval      xxstcd = 0
     c                   if        juris_st  = 'NY' and
     c                             elpfdt >= 1161001 and
     c                             elpfdt <= 1170930 and
     c                             ApplyRateSplit(juris_st:xxSpltDt:xxstcd)
     c                   if        xxstcd <> 0
     c                   exsr      FlatRateAdjNY
     C                   endif
     C                   endif
     C
     C*
     C                   endsr                                                  Exposure



     C*===========================================================================
     C*  Rate_array - Subroutine to load array with rate dates
     C*===========================================================================
     C     Rate_array    begsr
     C*
     C                   eval       rate_dt(1) = svpfdt
     C                   eval       rate_dt(2) = el2ptdt
     C                   eval       rate_dt(3) = 9999999
     C*
     C                   if        el2andt <> 0
     C                             and juris_st = 'NJ'
     C                   eval       rate_dt(1) = el2andt
     C                   endif
     C*
     C*  Added code to load the FL April/Oct 2003 rate changes into rate_dt(2) and
     C*  rate_dt(3), if applicable, since there will be no anniversary rating date
     C*  on accounts that these rate changes apply to.
     C*
     C                   eval      z = 2
     C                   if        juris_st = 'FL'
     C*
     C                   if        el2pfdt < 1030401 and el2ptdt > 1030401
     C                             or el2pfdt < 1031001 and el2ptdt > 1031001
     C*
     C     epky          chain     pep01                              90
     C                   dow       *in90 = *off
     C                   if        epefdt = 1030401
     C                   eval       rate_dt(z) = 1030401
     C                   eval      z = z + 1
     C                   leave
     C                   endif
     C     epky          reade     pep01                                  90
     C                   enddo
     C*
     C     epky          chain     pep01                              90
     C                   dow       *in90 = *off
     C                   if        epefdt = 1031001
     C                   eval       rate_dt(z) = 1031001
     C                   eval      z = z + 1
     C                   leave
     C                   endif
     C     epky          reade     pep01                                  90
     C                   enddo
     C*
     C                   endif
      *
     c                   if        cltid = '139'      and
     c                             el2pfdt >= 1090401 and
     c                             el2pfdt <  1090701 and
     c                             el2ptdt >  1090701 or
     c                             cltid = '139'      and
     c                             el2andt >= 1080401 and
     c                             el2andt <  1080701 and
     c                             el2ptdt >  1090701
      *
     c     epky          chain     pep01                              90
     c                   dow       *in90 = *off
     c                   if        epefdt = 1090701
     c                   eval       rate_dt(z) = 1090701
     c                   eval      z = z + 1
     c                   leave
     c                   endif
     c     epky          reade     pep01                                  90
     c                   enddo
      *
     c                   endif
      *
     C                   endif
     C*
     C* DE Rate Split
     C*
     C                   if        juris_st = 'DE'
     C                   if        exde08 <>'Y'
     C*
     C                   if        el2pfdt < 1081001 and el2ptdt > 1081001
     C     epky          chain     pep01                              90
     C                   dow       *in90 = *off
     C                   if        epefdt = 1081001
     C                   eval       rate_dt(z) = 1081001
     C                   eval      z = z + 1
     C                   leave
     C                   endif
     C     epky          reade     pep01                                  90
     C                   enddo
     C                   endif
     C*
     C                   endif
     C                   endif
     C*
     C* AZ Rate Split - 01/01/2010
     C*
     C                   if        juris_st = 'AZ'
     C*
     C                   if        el2pfdt < 1100101 and el2ptdt > 1100101
     C     epky          chain     pep01                              90
     C                   dow       *in90 = *off
     C                   if        epefdt = 1100101
     C                   eval       rate_dt(z) = 1100101
     C                   eval      z = z + 1
     C                   leave
     C                   endif
     C     epky          reade     pep01                                  90
     C                   enddo
     C                   endif
     C*
     C                   endif
     C* OK RATE SPLIT FOR 09/01/2010
     C*
     C                   if        juris_st = 'OK'
     C*
     C* Check opt out flag for OK 1100901 rate change
     C*
     C                   z-add     1100901       xxSpltDt          7 0
     C                   if        ApplyRateSplit(juris_st:xxSpltDt)
     C*
     C                   if        el2pfdt < xxSpltdt and el2ptdt > xxSpltDt
     C*
     C     epky          chain     pep01                              90
     C                   dow       *in90 = *off
     C                   if        epefdt = xxSpltDt
     C                   eval       rate_dt(z) = xxSpltDt
     C                   eval      z = z + 1
     C                   leave
     C                   endif
     C     epky          reade     pep01                                  90
     C                   enddo
     C*
     C                   endif                                                  El2PF,EL2PT
     C*
     C                   endif                                                  ApplyRateSplit
     C*
     C                   endif                                                  state
     C* TN RATE SPLIT FOR 07/01/2014
     C*
     C                   if        juris_st = 'TN'
     C*
     C* Check opt out flag for TN rate change
     C*
     C                   z-add     1140701       xxSpltDt          7 0
     C                   if        ApplyRateSplit(juris_st:xxSpltDt)
     C*
     C                   if        el2pfdt < xxSpltdt and el2ptdt > xxSpltDt
     C*
     C     epky          chain     pep01                              90
     C                   dow       *in90 = *off
     C                   if        epefdt = xxSpltDt
     C                   eval      rate_dt(z) = xxSpltDt
     C                   eval      z = z + 1
     C                   leave
     C                   endif
     C     epky          reade     pep01                                  90
     C                   enddo
     C*
     C                   endif                                                  El2PF,EL2PT
     C*
     C                   endif                                                  ApplyRateSplit
     C*
     C                   endif                                                  state
     C* OK RATE SPLIT FOR 02/01/2014
     C*
     C                   if        juris_st = 'OK'
     C*
     C* Check opt out flag for TN rate change
     C*
     C                   z-add     1140201       xxSpltDt          7 0
     C                   if        ApplyRateSplit(juris_st:xxSpltDt)
     C*
     C                   if        el2pfdt < 1140101 and el2ptdt > xxSpltDt
     C*
     C     epky          chain     pep01                              90
     C                   dow       *in90 = *off
     C                   if        epefdt = xxSpltDt
     C                   eval      rate_dt(z) = xxSpltDt
     C                   eval      z = z + 1
     C                   leave
     C                   endif
     C     epky          reade     pep01                                  90
     C                   enddo
     C*
     C                   endif                                                  El2PF,EL2PT
     C*
     C                   endif                                                  ApplyRateSplit
     C*
     C                   endif                                                  state
     C*
     C                   if        z < 4
     C*
     C                   if        el2andt <> 0
     C                             and el2andt <> el2pfdt
     C*
     C                   eval      Anniv_dt = cvtToDate(el2andt)
     C                   eval      Pol_eff = cvtToDate(el2pfdt)
     C*
     C                   Adddur    1:*years      Anniv_dt
     C*
     C                   if        Anniv_dt > Pol_eff
     C                             and Anniv_dt < cvtToDate(el2ptdt)
     C*
     C     *cymd         move      Anniv_dt      rate_dt(z)
     C                   eval      z = z + 1
     C                   if        z = 3
     C                   eval       rate_dt(z) = el2ptdt
     C                   endif
     C*
     C                   endif                                                  Anniv_dt>Pol_eff
     C*
     C                   endif                                                  elandt<>0 ...
     C*
     C                   endif
     C*
     C                   sorta     rate_dt
     C*
     C                   do        3             i
     C
     C                   if        rate_dt(i) = 9999999
     C                   eval      rate_dt(i) = 0
     C                   endif
     C
     C                   enddo                                                  do 3 i
     C                   endsr                                                  Rate_array



     C*===========================================================================
     C*  splt_array - Subroutine to load array with rate dates
     C*===========================================================================
     C     splt_array    begsr
     C*
     C                   z-add     9999999       splt_dt
     C                   clear                   prem_tl
     C*
     C*  Combine the experience modifiers if the rates are the same.
     C*
     C                   if        ExpMod(3) <> 0
     C                             and ExpMod(3) = ExpMod(2)
     C                   clear                   ExpMod(3)
     C                   clear                   EffMod(3)
     C                   endif
     C                   if        ExpMod(2) <> 0
     C                             and ExpMod(2) = ExpMod(1)
     C                   clear                   ExpMod(2)
     C                   clear                   EffMod(2)
     C                   endif
     C*
     C*** Beg Del *** 04/09/24 *****************************************
     C***************    if        EffMod(1) < elbpfdt
     C***************    eval       EffMod(1) = elbpfdt
     C***************    endif
     C*** End Del *** 04/09/24 *****************************************
     C*
     C                   if        EffMod(1) = 0
     C                             or fnpeof <> ' '
     C                   eval       EffMod(1) = rate_dt(1)
     C                   endif


     C                   eval      splt_dt(1) = el2pfdt
     C                   eval      splt_dt(2) = rate_dt(2)
     C                   if         rate_dt(3) <> 0
     C                   eval        splt_dt(3) = rate_dt(3)
     C                   endif
     C
     C                   if        ExpMod(2)<>0
     C                   eval       splt_dt(4) = EffMod(2)
      //*** Beg Add ***** 12/03/24 ********************************
     C                   elseif    EffMod(2) <> 0
     C                   eval       splt_dt(4) = EffMod(2)
      //*** End Add ***** 12/03/24 ********************************
     C                   endif

     C                   if        ExpMod(3)<>0
     C                   eval       splt_dt(5) = EffMod(3)
      //*** Beg Add ***** 12/03/24 ********************************
     C                   elseif    EffMod(3) <> 0
     C                   eval       splt_dt(5) = EffMod(3)
      //*** End Add ***** 12/03/24 ********************************
     C                   endif
     C

     C*  If more than one date in array the same, remove all but one
     C                   do        5             i
     C                   if        splt_dt(i)<>9999999
     C
     C                   eval       j=5-i
     C                   do        j             k
     C                   if        splt_dt(i)=splt_dt(i+k)
     C                   eval       splt_dt(i+k)=9999999
     C                   endif
     C                   enddo                                                  do j k
     C
     C                   endif                                                  splt_dt(i)<>9999999
     C                   enddo                                                  do 5 i

     C                   sorta     splt_dt

     C*  Fill the experience mod array with appropriate dates per rate_dt
     C*
     C                   endsr                                                  splt_array

     C*===========================================================================
     C*  stat_codes - Subroutine to load array with rate dates
     C*===========================================================================
     C     stat_codes    begsr
     C*
     C                   eval       svcccd = 0
     C                   eval       svdv1c = 0
     C                   eval       svdv2c = 0
     C                   eval       svdccd = 0
     C                   eval       sv1acd = 0
     C                   eval       sv1bcd = 0
     C                   eval       svcrcd = 0
     C                   eval       svdbcd = 0
     C                   eval       sveccd = 0
     C                   eval       svmpcd = 0
     C                   eval       svwvac = 0
     C                   eval       svtdcd = 0
     C                   eval       svtccd = 0
     C                   eval       sv1stc = 0
     C                   eval       sv2ndc = 0
     C                   eval       svcoal = 0
     C                   eval       sv3rdc = 0
     C                   eval       svmrrd = 0
     C                   eval       svmrrn = 0
     C                   eval       svmrrc = 0
     C                   eval       svtxcd = 0
     C                   eval       svterc = 0
     C                   eval       sv4thc = 0
     C                   eval       sv5thc = 0
     C                   eval       svvlcc = 0
     C                   eval       svdtrc = 0
     C                   eval       svrdpr = *blanks
     C                   eval       svdv1l = *blanks
     C                   eval       svdv2l = *blanks
     C                   eval       svwvfl = *blanks
     C                   eval       svterr = 0
     C                   eval       svdspr = *blanks
     C                   eval       sv1stl = *blanks
     C                   eval       sv2ndl = *blanks
     C                   eval       sv3rdl = *blanks
     C                   eval       svasfl = *blanks
     C                   eval       svilmc = 0
     C                   eval       svdtrr = 0
     C                   eval       svrtrc = 0
     C*
     C     rrky          setgt     prr01
     C     rrky1         readpe    prr01                                  60
     C   60rrky          setgt     prr01
     C   60rrky1         reade     prr01                                  60
     C                   dow       *in60 = *off
     C                             and rrefdt <= svptdt
     C*
     C                   if        svcccd = 0
     C                   eval       svcccd = rrcccd
     C                   endif
     C                   if        svdv1c = 0
     C                   eval       svdv1c = rrdv1c
     C                   endif
     C                   if        svdv2c = 0
     C                   eval       svdv2c = rrdv2c
     C                   endif
     C                   if        svdccd = 0
     C                   eval       svdccd = rrdccd
     C                   endif
     C                   if        sv1acd = 0
     C                   eval       sv1acd = rr1acd
     C                   endif
     C                   if        sv1bcd = 0
     C                   eval       sv1bcd = rr1bcd
     C                   endif
     C                   if        svcrcd = 0
     C                   eval       svcrcd = rrcrcd
     C                   endif
     C                   if        svdbcd = 0
     C                   eval       svdbcd = rrdbcd
     C                   endif
     C                   if        sveccd = 0
     C                   eval       sveccd = rreccd
     C                   endif
     C                   if        svmpcd = 0
     C                   eval       svmpcd = rrmpcd
     C                   endif
     C                   if        svwvac = 0
     C                   eval       svwvac = rrwvac
     C                   endif
     C                   if        svtdcd = 0
     C                   eval       svtdcd = rrtdcd
     C                   endif
     C                   if        svtccd = 0
     C                   eval       svtccd = rrtccd
     C                   endif
     C                   if        sv1stc = 0
     C                   eval       sv1stc = rr1stc
     C                   endif
     C                   if        sv2ndc = 0
     C                   eval       sv2ndc = rr2ndc
     C                   endif
     C                   if        svcoal = 0
     C                   eval       svcoal = rrcoal
     C                   endif
     C                   if        sv3rdc = 0
     C                   eval       sv3rdc = rr3rdc
     C                   endif
     C                   if        svmrrd = 0
     C                   eval       svmrrd = rrmrrd
     C                   endif
     C                   if        svmrrn = 0
     C                   eval       svmrrn = rrmrrn
     C                   endif
     C                   if        svmrrc = 0
     C                   eval       svmrrc = rrmrrc
     C                   endif
     C                   if        svtxcd = 0
     C                   eval       svtxcd = rrtxcd
     C                   endif
     C                   if        svterc = 0
     C                   eval       svterc = rrterc
     C                   endif
     C                   if        sv4thc = 0
     C                   eval       sv4thc = rr4thc
     C                   endif
     C                   if        sv5thc = 0
     C                   eval       sv5thc = rr5thc
     C                   endif
     C                   if        svvlcc = 0
     C                   eval       svvlcc = rrvlcc
     C                   endif
     C                   if        svdtrc = 0
     C                   eval       svdtrc = rrdtrc
     C                   endif
     C                   if        svrdpr = *blanks
     C                   eval       svrdpr = rrrdpr
     C                   endif
     C                   if        svdv1l = *blanks
     C                   eval       svdv1l = rrdv1l
     C                   endif
     C                   if        svdv2l = *blanks
     C                   eval       svdv2l = svdv2l
     C                   endif
     C                   if        svwvfl = *blanks
     C                   eval       svwvfl = svwvfl
     C                   endif
     C                   if        svterr = 0
     C                   eval       svterr = rrterr
     C                   endif
     C                   if        svdspr = *blanks
     C                   eval       svdspr = rrdspr
     C                   endif
     C                   if        sv1stl = *blanks
     C                   eval       sv1stl = rr1stl
     C                   endif
     C                   if        sv2ndl = *blanks
     C                   eval       sv2ndl = rr2ndl
     C                   endif
     C                   if        sv3rdl = *blanks
     C                   eval       sv3rdl = rr3rdl
     C                   endif
     C                   if        svasfl = *blanks
     C                   eval       svasfl = rrasfl
     C                   endif
     C                   if        svilmc = 0
     C                   eval       svilmc = rrilmc
     C                   endif
     C                   if        svdtrr = 0
     C                   eval       svdtrr = rrdtrr
     C                   endif
     C                   if        svrtrc = 0
     C                   eval       svrtrc = rrrtrc
     C                   endif
     C*
     C     rrky1         reade     prr01                                  60
     C                   enddo
     C*
     C                   eval       rrcccd = svcccd
     C                   eval       rrdv1c = svdv1c
     C                   eval       rrdv2c = svdv2c
     C                   eval       rrdccd = svdccd
     C                   eval       rr1acd = sv1acd
     C                   eval       rr1bcd = sv1bcd
     C                   eval       rrcrcd = svcrcd
     C                   eval       rrdbcd = svdbcd
     C                   eval       rreccd = sveccd
     C                   eval       rrmpcd = svmpcd
     C                   eval       rrwvac = svwvac
     C                   eval       rrtdcd = svtdcd
     C                   eval       rrtccd = svtccd
     C                   eval       rr1stc = sv1stc
     C                   eval       rr2ndc = sv2ndc
     C                   eval       rrcoal = svcoal
     C                   eval       rr3rdc = sv3rdc
     C                   eval       rrmrrd = svmrrd
     C                   eval       rrmrrn = svmrrn
     C                   eval       rrmrrc = svmrrc
     C                   eval       rrtxcd = svtxcd
     C                   eval       rrterc = svterc
     C                   eval       rr4thc = sv4thc
     C                   eval       rr5thc = sv5thc
     C                   eval       rrvlcc = svvlcc
     C                   eval       rrdtrc = svdtrc
     C                   eval       rrrdpr = svrdpr
     C                   eval       rrdv1l = svdv1l
     C                   eval       rrdv2l = svdv2l
     C                   eval       rrwvfl = svwvfl
     C                   eval       rrterr = svterr
     C                   eval       rrdspr = svdspr
     C                   eval       rr1stl = sv1stl
     C                   eval       rr2ndl = sv2ndl
     C                   eval       rr3rdl = sv3rdl
     C                   eval       rrasfl = svasfl
     C                   eval       rrilmc = svilmc
     C                   eval       rrdtrr = svdtrr
     C                   eval       rrrtrc = svrtrc
     C*
     C                   endsr


     C*============================================================================
     C*  othExpArr - Subroutine to assign values to non-payroll exposure array
     C*============================================================================
     C     othExpArr     begsr
     C*
     C*  Assign values based on audit status
     C                   if        adtpyrl = *on
     C
     C*                AUDITED
     C*** begin add ***  08/28/25  *********************************************
     C     epky          chain     pmp01                              63
     C                   If        xxminf = ' '
     C*** end add *****  08/28/25  *********************************************
     C*      Deductible discount exposure amt
     C                   add       el2adct       othExp(1)
     C                   add       el2add2       othExp(1)
     C*      Managed care credit exposure amt
     C                   add       el2adcm       othExp(2)
     C*      Safety credit exposure amt
     C                   add       el2adcs       othExp(3)
     C*      Contractor credit exposure amt
     C                   add       el2adc1       othExp(4)
     C*      Deviation exposure amt
     C                   add       el2adc2       othExp(5)
     C*      2nd Deviation exposure amt
     C                   add       el2adv2       othExp(6)
     C*      Deviation 1 2nd period  exposure amt
     C                   add       el2d1a2       othExp(25)
     C*      2nd Deviation 2nd period exposure amt
     C                   add       el2d2a2       othExp(26)
     C*      HCN CREDIT exposure amt
     C                   add       el2AHCN       othExp(27)
      *      Unearned Premium Surcharge
     C                   add       el2UPSA       othExp(28)
     C*      Schedule debit/credit exposure amt
     C                   add       el2adc        othExp(7)
     C*      Increased limits premium 1
      *** Beg Chg ***** 11/21/25 ***********************************************
     C***                add       el2ain1       othExp(8)
     C                   add       mpain1        othExp(8)
      *** End Chg ***** 11/21/25 ***********************************************
     C*      Increased limits premium 2
      *** Beg Chg ***** 11/21/25 ***********************************************
     C***                add       el2ain2       othExp(9)
     C                   add       mpain2        othExp(9)
      *** End Chg ***** 11/21/25 ***********************************************
     C*      Waiver surcharge
     C                   add       el2awav       othExp(10)
     C*      Terrorism surcharge
     C                   add       el2ater       othExp(11)
     C*      Stock discount (aka premium discount)
     C                   add       el2adc3       othexp(12)
     C*      Expense constant
     C                   add       el2prtd       othexp(13)
     C*** begin add ***  08/28/25  *********************************************
     C                   Else
     C                   add       el2ater       othExp(11)
     C*
     C                   If        *in63 = *off
     C                   Add       mpadct        othExp(1)
     C                   Add       mpadd2        othExp(1)
     C                   Add       mpadcm        othExp(2)
     C                   Add       mpadcs        othExp(3)
     C                   Add       mpadc1        othExp(4)
     C                   Add       mpadc2        othExp(5)
     C                   Add       mpadv2        othExp(6)
     C                   Add       mpd1a2        othExp(25)
     C                   Add       mpd2a2        othExp(26)
     C                   Add       MPAHCN        othExp(27)
     C                   Add       mpAUSA        othExp(28)
     C                   Add       mpadc         othExp(7)
     C                   Add       mpain1        othExp(8)
     C                   Add       mpain2        othExp(9)
     C                   Add       mpawav        othExp(10)
     C                   Add       mpadc3        othExp(12)
     C                   Add       mpprtd        othExp(13)
     C                   Endif                                                  *in63 = *off
     C*
     C                   Endif                                                  xxminf = ' '
     C*
     C*      Determine if an Increased limits balance to minimum exists.
     C*
     C                   If        *in63 = *off and rrilmc > 0
     C                   Add       mpabil        othExp(21)
     C                   Add       mpabil2       othExp(21)
     C                   Endif
     C*** end add *****  08/28/25  *********************************************
     C*      1st Assessment premium
     C                   add       el21aam       othExp(14)
     C*      2nd Assessment premium
     C                   add       el22aam       othExp(15)
     C*      3rd Assessment premium
     C                   add       el23aam       othExp(16)
     C*      Voluntary Labor Charge
     C                   add       el2vlca       othExp(17)
     C*      Domestic Terrorism Chg
     C                   add       el2adtr       othExp(18)
     C*      4th Assessment premium
     C                   add       el24aam       othExp(19)
     C*      5th Assessment premium
     C                   add       el25aam       othExp(20)
     C*      6th Assessment premium
     C                   add       el26aam       othExp(23)
     C*      7th Assessment premium
     C                   add       el27aam       othExp(24)
      *      Retrospective Rating Premium (settlement amount)
     C                   if        el2stam <> 0
     C     el2stam       sub       el2aapr       othExp(22)
     C                   endif
      /Free
       //    ANC Charge
       othExp(29) = othExp(29) + ELanca;
      /End-free
     C*
     C*      Load values from Premium Detail record, if record exists.
     C*
     C*** begin del ***  08/28/25  *********************************************
     C*****epky          chain     pmp01                              63
     C*****              if        *in63 = *off
     C*****                        and el2amnp = 'Y'
     C*
     C*****              z-add     mpadct        othExp(1)
     C*****              add       mpadd2        othExp(1)
     C*****              z-add     mpadcm        othExp(2)
     C*****              z-add     mpadcs        othExp(3)
     C*****              z-add     mpadc1        othExp(4)
     C*****              z-add     mpadc2        othExp(5)
     C*****              z-add     mpadv2        othExp(6)
     C*****              add       mpd1a2        othExp(25)
     C*****              add       mpd2a2        othExp(26)
     C*****              Z-ADD     MPAHCN        othExp(27)
     C*****              z-add     mpAUSA        othExp(28)
     C*****              z-add     mpadc         othExp(7)
     C*****              z-add     mpawav        othExp(10)
     C*****              z-add     mpadc3        othExp(12)
     C*****              z-add     mpprtd        othExp(13)
     C*
     C*****              endif                                                  adtpyrl=*on
     C*
     C*      Determine if an Increased limits balance to minimum exists.
     C*
     C*****epky          chain     pil01                              63
     C*****              if        *in63 = *off and rrilmc > 0
      *****
     C*****              if        ilain1 < othexp(8)
     C*****              eval      othExp(21) = othexp(8) - ilain1
     C*****              eval      othExp(8)  = ilain1
     C*****              endif
      *****
     C*****              if        ilain2 < othexp(9)
     C*****              eval      othExp(21) = othexp(21) + othexp(9) - ilain2
     C*****              eval      othExp(9)  = ilain2
     C*****              endif
      *****
     C*****              endif
     C*** end del *****  08/28/25  *********************************************
     C*
     C                   else
     C*
     C*                NOT AUDITED
     C*** begin add ***  08/28/25  *********************************************
     C     epky          chain     pmp01                              63
     C                   If        xxminf = ' '
     C*** end add *****  08/28/25  *********************************************
     C*      Deductible discount exposure amt
     C                   add       el2edct       othExp(1)
     C                   add       el2edd2       othExp(1)
     C*      Managed care credit exposure amt
     C                   add       el2edcm       othExp(2)
     C*      Safety credit exposure amt
     C                   add       el2edcs       othExp(3)
     C*      Contractor credit exposure amt
     C                   add       el2edc1       othExp(4)
     C*      Deviation exposure amt
     C                   add       el2edc2       othExp(5)
     C*      2nd Deviation exposure amt
     C                   add       el2edv2       othExp(6)
     C                   add       el2d1e2       othExp(25)
     C                   add       el2d2e2       othExp(26)
     C                   add       EL2EHCN       othExp(27)
     C*      Schedule debit/credit exposure amt
     C                   add       el2edc        othExp(7)
     C*      Increased limits premium 1
      *** Beg Chg ***** 11/21/25 ***********************************************
     C***                add       el2ein1       othExp(8)
     C                   add       mpein1        othExp(8)
      *** End Chg ***** 11/21/25 ***********************************************
     C*      Increased limits premium 2
      *** Beg Chg ***** 11/21/25 ***********************************************
     C***                add       el2ein2       othExp(9)
     C                   add       mpein2        othExp(9)
      *** End Chg ***** 11/21/25 ***********************************************
     C*      Waiver surcharge
     C                   add       el2ewav       othExp(10)
     C*      Terrorism surcharge
     C                   add       el2eter       othExp(11)
     C*      Stock discount (aka premium discount)
     C                   add       el2edc3       othexp(12)
     C*      Expense constant
     C                   add       el2prtd       othexp(13)
     C*** begin add ***  08/28/25  *********************************************
     C                   Else
     C                   add       el2eter       othExp(11)
     C*
     C                   If        *in63 = *off
     C                   Add       mpedct        othExp(1)
     C                   Add       mpedd2        othExp(1)
     C                   Add       mpedcm        othExp(2)
     C                   Add       mpedcs        othExp(3)
     C                   Add       mpedc1        othExp(4)
     C                   Add       mpedc2        othExp(5)
     C                   Add       mpedv2        othExp(6)
     C                   Add       mpd1e2        othExp(25)
     C                   Add       mpd2e2        othExp(26)
     C                   Add       MPEHCN        othExp(27)
     C                   Add       mpedc         othExp(7)
     C                   Add       mpein1        othExp(8)
     C                   Add       mpein2        othExp(9)
     C                   Add       mpewav        othExp(10)
     C                   Add       mpedc3        othExp(12)
     C                   Add       mpprtd        othExp(13)
     C                   Endif                                                  *in63 = *off
     C*
     C                   Endif                                                  xxminf = ' '
     C*
     C*      Determine if an Increased limits balance to minimum exists.
     C*
     C                   If        *in63 = *off and rrilmc > 0
     C                   Add       mpebil        othExp(21)
     C                   Add       mpebil2       othExp(21)
     C                   Endif
     C*** end add *****  08/28/25  *********************************************
     C*      1st Assessment premium
     C                   add       el21aam       othExp(14)
     C*      2nd Assessment premium
     C                   add       el22aam       othExp(15)
     C*      3rd Assessment premium
     C                   add       el23aam       othExp(16)
     C*      Voluntary Labor Charge
     C                   add       el2vlca       othExp(17)
     C*      Domestic Terrorism Charge
     C                   add       el2edtr       othExp(18)
     C*      4th Assessment premium
     C                   add       el24aam       othExp(19)
     C*      5th Assessment premium
     C                   add       el25aam       othExp(20)
     C*      6th Assessment premium
     C                   add       el26aam       othExp(23)
     C*      7th Assessment premium
     C                   add       el27aam       othExp(24)
      *      Retrospective Rating Premium (settlement amount)
     C                   if        el2stam <> 0
     C     el2stam       sub       el2anpr       othExp(22)
     C                   endif
     C*
     C*      Load values from Premium Detail record, if record exists.
     C*
     C*** begin del ***  08/28/25  *********************************************
     C*****epky          chain     pmp01                              63
     C*****              if        *in63 = *off
     C*****                        and el2emnp = 'Y'
     C*
     C*****              z-add     mpedct        othExp(1)
     C*****              add       mpedd2        othExp(1)
     C*****              z-add     mpedcm        othExp(2)
     C*****              z-add     mpedcs        othExp(3)
     C*****              z-add     mpedc1        othExp(4)
     C*****              z-add     mpedc2        othExp(5)
     C*****              z-add     mpedv2        othExp(6)
     C*****              z-add     mpd1e2        othExp(25)
     C*****              z-add     mpd2e2        othExp(26)
     C*****              Z-ADD     MPEHCN        othExp(27)
     C*****              z-add     mpedc         othExp(7)
     C*****              z-add     mpewav        othExp(10)
     C*****              z-add     mpedc3        othExp(12)
     C*****              z-add     mpprtd        othExp(13)
     C*
     C*****              endif                                                  adtpyrl=*on
     C*
     C*      Determine if an Increased limits balance to minimum exists.
     C*
     C*****epky          chain     pil01                              63
     C*****              if        *in63 = *off and rrilmc > 0
      *****
     C*****              if        ilein1 < othexp(8)
     C*****              eval      othExp(21) = othexp(8) - ilein1
     C*****              eval      othExp(8)  = ilein1
     C*****              endif
      *****
     C*****              if        ilein2 < othexp(9)
     C*****              eval      othExp(21) = othexp(21) + othexp(9) - ilein2
     C*****              eval      othExp(9)  = ilein2
     C*****              endif
      *****
     C*****              endif
     C*** end del *****  08/28/25  *********************************************
     C
     C                   endif                                                  adtpyrl=*on
     C
     C                   if        juris_st = 'CA' and xxmode <> 'F'
     C                   z-add     0             othexp(1)
     C                   z-add     0             othexp(2)
     C                   z-add     0             othexp(3)
     C                   z-add     0             othexp(4)
     C                   z-add     0             othexp(5)
     C                   z-add     0             othexp(6)
     C                   z-add     0             othexp(7)
     C                   z-add     0             othexp(8)
     C                   z-add     0             othexp(9)
     C                   z-add     0             othexp(10)
     C                   z-add     0             othexp(12)
     C                   z-add     0             othexp(13)
     C                   z-add     0             othexp(17)
     C                   z-add     0             othexp(18)
     C                   z-add     0             othexp(14)
     C                   z-add     0             othexp(15)
     C                   z-add     0             othexp(16)
     C                   z-add     0             othexp(19)
     C                   z-add     0             othexp(20)
     C                   z-add     0             othexp(21)
     C                   z-add     0             othexp(22)
     C                   z-add     0             othexp(23)
     C                   z-add     0             othexp(24)
     C                   z-add     0             othexp(25)
     C                   z-add     0             othexp(26)
     C                   z-add     0             othexp(27)
     C                   z-add     0             othExp(28)
     C                   endif
     C                   if        juris_st <>'CA'  or xxmode = 'F'
     C                   exsr      srPrmPrdFil
     C                   endif
     C
     C                   endsr                                                  othExpArr

     C*===========================================================================
     C*  wrtOthExp - Writes an exposure record (other than payroll)
     C*===========================================================================
     C     wrtOthExp     begsr
     C
     C                   clear                   pse01
     C                   setoff                                           21
     C                   setoff                                           22
     C
     C*   Class code
     C                   select
     C
     C*      Deductible discount exposure amt
     C                   when      i = 1
     c                   if        xxlrgd <> 'Y'
     C                   eval      seclas = ddclas
     C                   if        rrrdpr = 'M'
     C                             or rrrdpr = 'J'
     C                   eval      semddt = EffMod(1)
     C                   eval       Usubj_prem = Usubj_prem - OthExp(i)
     C                   seton                                            22
     C                   else
     C                   eval       bal_min = bal_min - OthExp(i)
     C                   endif
     c                   else

     C                   eval      seclas = ldcls1
     C                   if        ldplv1 = 'M'
     C                             or ldplv1 = 'J'
     C                             or ldcls1 = 9664
     C                   eval      semddt = EffMod(1)
     C                   if        ldplv1 = 'M' or ldplv1 = 'J'
     C                   eval       Usubj_prem = Usubj_prem - OthExp(i)
     C                   else
     C                   eval       bal_min = bal_min - OthExp(i)
     C                   endif
     C                   seton                                            22
     C                   else
     C                   eval       bal_min = bal_min - OthExp(i)
     C                   endif

     c                   endif                                                  if xxlrgd <> 'Y'
     C
     C*      Subject prem credit 1A
     C                   when      i = 2
     C                   eval      seclas = rr1acd
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem - OthExp(i)
     C                   seton                                            22
     C
     C*      Subject prem credit 1B
     C                   when      i = 3
     C                   eval      seclas = rr1bcd
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem - OthExp(i)
     C                   seton                                            22
     C
     C*      Contractor credit exposure amt
     C                   when      i = 4                                        contractor credit
     C                   eval      seclas = rrcccd
     C                   eval       bal_min = bal_min - OthExp(i)
     C
     C*      Deviation exposure amt
     C                   when      i = 5 or i=25                                dev#1 1st or 2nd prd
     C                   eval      seclas = rrdv1c
     C                   if        othExp(i) < 0
     C                   eval       othExp(i) = -(othExp(i))
     C                   seton                                            21
     C                   endif
     C                   if        rrdv1l = 'J'
     C                             or rrdv1l = 'M'
     C                   eval      semddt = effMod(1)
     C  n21              eval       Usubj_prem = Usubj_prem - OthExp(i)
     C   21              eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   if        rrdv1l = 'E'
     C  n21              eval       bal_min = bal_min - OthExp(i)
     C   21              eval       bal_min = bal_min + OthExp(i)
     C                   endif
     C
     C*      2nd Deviation exposure amt
     C                   when      i = 6 or i=26                                dev#2 1st or 2nd prd
     C                   eval      seclas = rrdv2c
     C                   if        othExp(i) < 0
     C                   eval       othExp(i) = -(othExp(i))
     C                   seton                                            21
     C                   endif
     C                   if        rrdv2l = 'J'
     C                             or rrdv2l = 'M'
     C                   eval      semddt = effMod(1)
     C  n21              eval       Usubj_prem = Usubj_prem - OthExp(i)
     C   21              eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   if        rrdv2l = 'E'
     C  n21              eval       bal_min = bal_min - OthExp(i)
     C   21              eval       bal_min = bal_min + OthExp(i)
     C                   endif
     C
     C*      Schedule debit/credit exposure amt
     C                   when      i = 7                                        schedule dbt/crdt
     C                   eval       bal_min = bal_min + OthExp(i)
     C                   if        othExp(i) > 0
     C                   eval       seclas = rrdbcd
     C                   else
     C                   eval       seclas = rrcrcd
     C                   eval       othExp(i) = -(othExp(i))
      //*** Beg Add ***** 04/18/24 *****************************************
          *In21 = *on;
      //*** End Add ***** 04/18/24 *****************************************
     C                   endif
      //*** Beg Add ***** 04/18/24 *****************************************
       If RRdcap = 'M';
          SEmdDt = effMod(1) ;
          If *In21 = *On;                             //Scheduled Credit
             Usubj_prem = Usubj_prem - OthExp(i);
          Else;
             Usubj_prem = Usubj_prem + OthExp(i);     //Scheduled Debit
          Endif;
          *In22 = *On;
       Endif;
      //*** End Add ***** 04/18/24 *****************************************
     C*
     C*      Increased limits premium 1 & 2
     C                   when      i = 8                                        increased limits 1
     C                             or i = 9
     C                   eval       seclas = 0
     C                   seton                                            21
     C
     C                   eval       kkefdt = svpfdt
     C                   if         i = 8
     C                   movel(p)  el2ilcd       kkkey
     C                   else
     C                   movel(p)  el2inl2       kkkey
     C                   endif                                                  i=8
     C
     C*      Get record for the appropriate limits code
     C     lmky          setgt     plm01
     C     lmky2         readpe    plm01                                  60
     C                   dow       (*in60 = *off) and (lmstat = 'I')
     C     lmky2         readpe    plm01                                  60
     C                   enddo
     C  N60              eval       seclas = lmclas
     C*        add to subj premium
     C                   eval       Usubj_prem = Usubj_prem + othExp(i)
     C                   seton                                            22
     C*    If the Increased Limits minimum has not been reach, load the bal to min.
     C
     C*      Waiver surcharge
     C*
     C                   when      i = 10                                       Waiver surcharge
     C                   eval       seclas = rrwvac
     C                   if        el2wvfl = 'M' or el2wvfl  = 'J'
     C                             or el2wvfl = ' '
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   seton                                            21

      *
      *  Terrorism surcharge
      *
     C                   when      i = 11                                       Terrorism surcharge
     C                   eval       seclas = rrterc
     C                   if        juris_st = 'PA'
     C                             or juris_st = 'DE'
     C                   if        RRTERL <> 'Y'
     c                   eval(h)   rrterr = rrterr * getLossCost(RRTERC:
     c                                               getRateDate(el2pfdt))
     C                   endif                                                      RRTERL IFNE
     C                   eval      semnrt = rrterr
     C                   endif
     C                   seton                                            21
     C                   eval      xxRtChg = prChkRateChg(tctrr1:tctrr2:tctrr3)
     C                   if        xxRtChg
     C                   eval      xxRate1 = tctrr1
     C                   eval      xxRate2 = tctrr2
     C                   eval      xxRate3 = tctrr3
     C                   eval      xxAmt1 =  tctra1
     C                   eval      xxAmt2 =  tctra2
     C                   eval      xxAmt3 =  tctra3
     C                   eval      xxMcls1 = tctrs1
     C                   eval      xxMcls2 = tctrs2
     C                   eval      xxMcls3 = tctrs3
     C                   endif
     C
      *
      *  Stock Discount
      *
     C                   when      i = 12
     C                   move      '0063'        seclas
     c                   if        juris_st = 'MA' and CLTID = '089'
     C                   move      '0064'        seclas
     C                   endif
      *
      *  Expense Constant
      *
     C                   when      i = 13
     C                   move      rreccd        seclas
     C                   seton                                            21
      *
      *  Assesment code 1
      *
     C                   when      i = 14
     C                   move      rr1stc        seclas
       //*** Beg Add *** 03/13/24 ******************************************
       // For NY do not report Stat Code 0932
       If Juris_St = 'NY'and SEclas = 0932;
          SEclas = 0;
       Endif;
       //*** End Add *** 03/13/24 ******************************************
     C                   if        rr1stl = 'J'
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   seton                                            21
     C                   eval      xxRtChg = prChkRateChg(tca1r1:tca1r2:tca1r3)
     C                   if        xxRtChg
     C                   eval      xxRate1 = tca1r1
     C                   eval      xxRate2 = tca1r2
     C                   eval      xxRate3 = tca1r3
     C                   eval      xxAmt1 =  tca1a1
     C                   eval      xxAmt2 =  tca1a2
     C                   eval      xxAmt3 =  tca1a3
     C                   eval      xxMcls1 = tca1s1
     C                   eval      xxMcls2 = tca1s2
     C                   eval      xxMcls3 = tca1s3
     C                   endif
     C                   If        SEclas = 0932 And Juris_St = 'NY'
     C                   SetOn                                        22
     C                   EndIf
     C                   If        SEclas = 0174 And Juris_St = 'MN' And
     C                             rr1stl <> 'J'
     C                   eval       Ustd_prem = Ustd_prem + OthExp(i)
     C                   EndIf
      *
      *  Assesment code 2
      *
     C                   when      i = 15
     C                   move      rr2ndc        seclas
       //*** Beg Add *** 03/13/24 ******************************************
       // For NY do not report Stat Code 0932
       If Juris_St = 'NY'and SEclas = 0932;
          SEclas = 0;
       Endif;
       //*** End Add *** 03/13/24 ******************************************
     C                   if        rr2ndl = 'J'
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   seton                                            21
     C                   eval      xxRtChg = prChkRateChg(tca2r1:tca2r2:tca2r3)
     C                   if        xxRtChg
     C                   eval      xxRate1 = tca2r1
     C                   eval      xxRate2 = tca2r2
     C                   eval      xxRate3 = tca2r3
     C                   eval      xxAmt1 =  tca2a1
     C                   eval      xxAmt2 =  tca2a2
     C                   eval      xxAmt3 =  tca2a3
     C                   eval      xxMcls1 = tca2s1
     C                   eval      xxMcls2 = tca2s2
     C                   eval      xxMcls3 = tca2s3
     C                   endif
      *
      *  Assesment code 3
      *
     C                   when      i = 16
     C                   move      rr3rdc        seclas
       //*** Beg Add *** 03/13/24 ******************************************
       // For NY do not report Stat Code 0932
       If Juris_St = 'NY'and SEclas = 0932;
          SEclas = 0;
       Endif;
       //*** End Add *** 03/13/24 ******************************************
     C                   if        rr3rdl = 'J'
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   seton                                            21
     C                   eval      xxRtChg = prChkRateChg(tca3r1:tca3r2:tca3r3)
     C                   if        xxRtChg
     C                   eval      xxRate1 = tca3r1
     C                   eval      xxRate2 = tca3r2
     C                   eval      xxRate3 = tca3r3
     C                   eval      xxAmt1 =  tca3a1
     C                   eval      xxAmt2 =  tca3a2
     C                   eval      xxAmt3 =  tca3a3
     C                   eval      xxMcls1 = tca3s1
     C                   eval      xxMcls2 = tca3s2
     C                   eval      xxMcls3 = tca3s3
     C                   endif
      *
      *  Voluntary labor charge
      *
     C                   when      i = 17
     C                   eval       seclas = rrvlcc
     C                   seton                                            21
      *
      *      Domestic Terrorism Charge
      *
     C                   when      i = 18
     C                   eval       seclas = rrdtrc
     C                   seton                                            21
     C                   if        juris_st = 'PA'
     C                             or juris_st = 'DE'
     C                   if        RRTERL <> 'Y'
     c                   eval(h)   rrdtrr = rrdtrr * getLossCost(RRDTRC:
     c                                               getRateDate(el2pfdt))
     C                   endif                                                      RRTERL IFNE
     C                   eval      semnrt = rrdtrr
     C                   endif
     C                   eval      xxRtChg = prChkRateChg(tcdtr1:tcdtr2:tcdtr3)
     C                   if        xxRtChg
     C                   eval      xxRate1 = tcdtr1
     C                   eval      xxRate2 = tcdtr2
     C                   eval      xxRate3 = tcdtr3
     C                   eval      xxAmt1 =  tcdta1
     C                   eval      xxAmt2 =  tcdta2
     C                   eval      xxAmt3 =  tcdta3
     C                   eval      xxMcls1 = tcdds1
     C                   eval      xxMcls2 = tcdds2
     C                   eval      xxMcls3 = tcdds3
     C                   endif
      *  Assesment code 4
      *
     C                   when      i = 19
     C                   move      rr4thc        seclas
       //*** Beg Add *** 03/13/24 ******************************************
       // For NY do not report Stat Code 0932
       If Juris_St = 'NY'and SEclas = 0932;
          SEclas = 0;
       Endif;
       //*** End Add *** 03/13/24 ******************************************
     C                   if        rr4thl = 'J'
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   seton                                            21
     C                   eval      xxRtChg = prChkRateChg(tca4r1:tca4r2:tca4r3)
     C                   if        xxRtChg
     C                   eval      xxRate1 = tca4r1
     C                   eval      xxRate2 = tca4r2
     C                   eval      xxRate3 = tca4r3
     C                   eval      xxAmt1 =  tca4a1
     C                   eval      xxAmt2 =  tca4a2
     C                   eval      xxAmt3 =  tca4a3
     C                   eval      xxMcls1 = tca4s1
     C                   eval      xxMcls2 = tca4s2
     C                   eval      xxMcls3 = tca4s3
     C                   endif
      *
      *  Assesment code 5
      *
     C                   when      i = 20
     C                   move      rr5thc        seclas
       //*** Beg Add *** 03/13/24 ******************************************
       // For NY do not report Stat Code 0932
       If Juris_St = 'NY'and SEclas = 0932;
          SEclas = 0;
       Endif;
       //*** End Add *** 03/13/24 ******************************************
     C                   if        rr5thl = 'J'
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   seton                                            21
     C                   eval      xxRtChg = prChkRateChg(tca5r1:tca5r2:tca5r3)
     C                   if        xxRtChg
     C                   eval      xxRate1 = tca5r1
     C                   eval      xxRate2 = tca5r2
     C                   eval      xxRate3 = tca5r3
     C                   eval      xxAmt1 =  tca5a1
     C                   eval      xxAmt2 =  tca5a2
     C                   eval      xxAmt3 =  tca5a3
     C                   eval      xxMcls1 = tca5s1
     C                   eval      xxMcls2 = tca5s2
     C                   eval      xxMcls3 = tca5s3
     C                   endif
      *
      *  Assesment code 6
      *
     C                   when      i = 23
     C                   move      rr6thc        seclas
       //*** Beg Add *** 03/13/24 ******************************************
       // For NY do not report Stat Code 0932
       If Juris_St = 'NY'and SEclas = 0932;
          SEclas = 0;
       Endif;
       //*** End Add *** 03/13/24 ******************************************
     C                   if        rr6thl = 'J'
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   seton                                            21
     C                   eval      xxRtChg = prChkRateChg(tca6r1:tca6r2:tca6r3)
     C                   if        xxRtChg
     C                   eval      xxRate1 = tca6r1
     C                   eval      xxRate2 = tca6r2
     C                   eval      xxRate3 = tca6r3
     C                   eval      xxAmt1 =  tca6a1
     C                   eval      xxAmt2 =  tca6a2
     C                   eval      xxAmt3 =  tca6a3
     C                   eval      xxMcls1 = tca6s1
     C                   eval      xxMcls2 = tca6s2
     C                   eval      xxMcls3 = tca6s3
     C                   endif
      *
      *  Assesment code 7
      *
     C                   when      i = 24
     C                   move      rr7thc        seclas
       //*** Beg Add *** 03/13/24 ******************************************
       // For NY do not report Stat Code 0932
       If Juris_St = 'NY'and SEclas = 0932;
          SEclas = 0;
       Endif;
       //*** End Add *** 03/13/24 ******************************************
     C                   if        rr7thl = 'J'
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem + OthExp(i)
     C                   seton                                            22
     C                   endif
     C                   seton                                            21
     C                   eval      xxRtChg = prChkRateChg(tca7r1:tca7r2:tca7r3)
     C                   if        xxRtChg
     C                   eval      xxRate1 = tca7r1
     C                   eval      xxRate2 = tca7r2
     C                   eval      xxRate3 = tca7r3
     C                   eval      xxAmt1 =  tca7a1
     C                   eval      xxAmt2 =  tca7a2
     C                   eval      xxAmt3 =  tca7a3
     C                   eval      xxMcls1 = tca7s1
     C                   eval      xxMcls2 = tca7s2
     C                   eval      xxMcls3 = tca7s3
     C                   endif
     C*
     C*      Increased limits balance to minimum
     C*
     C                   when      i = 21
     C                   move      rrilmc        seclas
     C                   eval       Usubj_prem = Usubj_prem + othExp(i)
     C                   seton                                            21
     C                   seton                                            22
     C
      *      Retrospective Rating Premium (settlement amount)
      *
     C                   when      i = 22
     C                   eval       seclas = rrrtrc
     C                   seton                                            21
      *      HCN Credit
      *
     C                   when      i = 27
      *
     c     hckey         setgt     phc01
     c     hckey1        readpe    phc01                                  60
      *
     C                   eval      seclas = hcstcd
     C                   eval      semddt = effMod(1)
     C                   eval       Usubj_prem = Usubj_prem - OthExp(i)
     C                   seton                                            22
      *
      *      Unearned Premium Surcharge

     C                   when      i = 28
      *
     c                   if        el2upsa > 0
     c                   eval      xxcrsn = GetCxRsn(el2co#:
     c                                               el2fnd:
     c                                               el2emp#:
     c                                               el2div:
     c                                               GetCxDate(el2co#:
     c                                                         el2fnd:
     c                                                         el2emp#:
     c                                                         el2div):
     c                                               xxccod)
     c                   movel     xxccod        k1key
     c     crky          setgt     pcr01
     c     crky          readpe    pcr01

     c                   if        (not %eof(wtcrl)) and (crupsc <> 0)
     C                   eval      seclas = crupsc
     c                   eval      *in21 = *on
     c                   endif

     c                   endif

      /Free
       // ANC Charge

       When i = 29;
          seclas = RRancs;
          *in21 = *on;
      /End-free
     C
     C                   endsl

     C                   eval      kkclas = seclas
     C
     C                   eval      seexpo = 00
     C                   if        juris_st = 'NJ'
     C                   eval      seexpo = 01
     C                   endif
     C*
     C*   Rating tier id
     C                   move      prtier        tier
     C

     C*   Process increased limits separately so they can be split
     C*   Process contractors credit separately and split all other discounts.
     C*
     C*   Only split discounts/surcharges if applied before the exp mod.
     C*
     C*  Process premium factors that occur after the modified premium
     C*  separate from the factors that occur before the modified premium.
     C*
     C                   if        *in22 = *off

     C                   if        juris_st = 'NC'
     C                             or juris_st = 'WI'
     C                             or juris_st = 'MA'
     C                             or juris_st = 'PA'
     C                             or juris_st = 'NJ'
     C                             or juris_st = 'NY'
     C                             or juris_st = 'LA'
     C                             or juris_st = 'MS'
     C                             or juris_st = 'AR'
     C                             or juris_st = 'TN'
     C                             or juris_st = 'SC'
     C                             or juris_st = 'VA'
     C                   if        semddt = 0
     C                   eval      semddt = EffMod(1)
     C                   endif
     C                   eval      sertdt = rate_dt(1)
     C                   else
     C                   if        semddt = 0
     C                   eval      semddt = EffMod(1)
     C                   endif
     C                   eval      sertdt = rate_dt(1)
     C                   endif
     C                   if        juris_st = 'NJ'
     C                             Or juris_st = 'MA'
     C                             Or juris_st = 'NH'
     C                   eval      sertdt = semddt
     C                   endif

     C                   if        not xxRtChg
     C                   If        juris_st = 'PA'
     C                             and splt_dt(3) <> 9999999
     C                             or juris_st = 'DE'
     C                             and splt_dt(3) <> 9999999
     C*
     C*  Split discounts based on premium percentage for each splt_dt.
     C*
     C                   eval       r = 1
     C     9999999       lookup    splt_dt(r)                             70
     C   70              eval       s = r-2
     C  N70              eval       s = 4
     C*
     C                   do        s             t
     C                   if        prem_ttl <> 0
     C                             and prem_tl(t) <> 0
     C                   eval(h)   prem = othexp(i) * (prem_tl(t)/prem_ttl)
     C                   else
     C                   eval      prem = othexp(i)
     C                   if        t > 1
     C                   leave
     C                   endif
     C                   endif
     C
     C                   eval      n=1
     C                   do        3             m
     C                   if        splt_dt(t) >= effMod(m)
     C                             and effMod(m) <> 0
     C                   eval       n = m
     C                   endif
     C                   enddo

     C                   If         juris_st = 'NJ'
     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : seclas : 00 : 0 :
     C                               splt_dt(t) : polEffDt : 0 : prem :
     C                               semnrt : tier : seexpo : t-1)
     C                   Else
     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : seclas : 00 : 0 :
     C                               splt_dt(t) : splt_dt(t) : 0 : prem :
     C                               semnrt : tier : seexpo : t-1)
     C                   EndIf

     C                   if        prem_ttl = 0
     C                   leave
     C                   endif

     C                   enddo

     C                   else

     C*
     C                   Eval      xxExpMod = 0
     C*
     C* Write MA assessment exposure records with modifier.
     C*
     C                   If        juris_st = 'MA' and (i = 14 or
     C                             i = 15 or i = 16 or i = 19 or
     C                             i = 20 or i = 23 or i = 24)
     C                   Eval      xxExpMod = ExpMod(1)
     C                   Endif
     C*
     C                   If         juris_st = 'NJ'
     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : seclas : 00 : xxExpMod :
     C                               semddt : polEffDt : 0 : othexp(i) : semnrt
     C                               : tier : seexpo : 0)
     C                   Else
     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : seclas : 00 : xxExpMod :
     C                               semddt : semddt : 0 : othexp(i) : semnrt
     C                               : tier : seexpo : 0)
     C                   EndIf

     C                   endif
     C                   else
     C                   eval      xxCalMod = *off
     C                   exsr      srWrtRtChg
     C                   endif                                                  not xxRtChg

     C                   else
     C                   if        not xxRtChg
     C*
     C*  If processing 'increased limits' then split records like payrolls
     C*
     C                   eval       r = 1
     C     9999999       lookup    splt_dt(r)                             70
     C                   if        *in70=*on
     C                   eval       s = r-2
     C                   else
     C                   eval       s = 4
     C                   endif
     C*
     C                   do        s             t
     C*
     C*  Split discounts based on premium percentage for each splt_dt.
     C*
     C                   if        prem_ttl <> 0
     C                             and prem_tl(t) <> 0
     C                   eval(h)   prem = othexp(i) * (prem_tl(t)/prem_ttl)
     C                   else
     C                   eval      prem = othexp(i)
     C                   if        t > 1
     C                   leave
     C                   endif
     C                   endif
     C
     C                   eval      n=1
     C                   do        3             m
     C                   if        splt_dt(t) >= effMod(m)
     C                             and effMod(m) <> 0
     C                   eval       n = m
     C                   endif
     C                   enddo

     C                   if        juris_st <> 'NJ'
     C                             and juris_st <> 'PA'

     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : seclas : 00 : expMod(n) :
     C                               effMod(n) : effMod(n) : 0 : prem :
     C                               0 : tier : seexpo : t-1)

     C                   else
     C                   If         juris_st = 'NJ'
     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : seclas : 00 : expMod(n) :
     C                               splt_dt(t) : polEffDt : 0 : prem :
     C                               semnrt : tier : seexpo : t-1)
     C                   Else
     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : seclas : 00 : expMod(n) :
     C                               splt_dt(t) : splt_dt(t) : 0 : prem :
     C                               semnrt : tier : seexpo : t-1)
     C                   EndIf
     C                   endif

     C*  Calculate the modified premim.
     C*
     C                   if        expmod(n) = 0
     C  n21              eval(h)    mod_prem = mod_prem - prem
     C   21              eval(h)    mod_prem = mod_prem + prem
     C                   else
     C  n21              eval(h)    mod_prem = mod_prem - (expmod(n) * prem)
     C   21              eval(h)    mod_prem = mod_prem + (expmod(n) * prem)
     C                   endif

     C                   if        prem_ttl = 0
     C                   leave
     C                   endif

     C                   enddo                                                  s t
     C                   else
     C                   eval      xxCalMod = *on
     C                   exsr      srWrtRtChg
     C                   endif                                                  not xxRtChg
     C
     C                   endif                                                  *in22 = *off/*on
     C*
     C*  Accumulate exposure amt for unit total
     C                   add       seexpa        Uexp_oth
     C
     C     skip          tag
     C
     C                   endsr                                                  wrtOthExp



     C*===========================================================================
     C*  Short_rt - Detrmine if this is a "short rated" account.  If it is then
     C*     write an exposure record reflecting the difference.
     C*===========================================================================
     C     Short_rt      Begsr
     C*
     C*  Check if account is audited and employer is short rated
     C*  and that minimum premium has not been invoked
     C                   if        adtPyrl = *on
     C                             and el2srpr = 'S'
     C                             and el2amnp <> 'Y'
     C*
     C*    Check if policy is for a partial year (less than 365 days)
     C                   eval      nDays = dtDiff(dtMDYY(el2pfdt) :
     C                                               dtMDYY(el2ptdt))
     C                   if        nDays < 365
     C*
     C*      If audited manual premium from employer detail (WDELP) is not
     C*      equal to accumulated premium then write exposure record.
     C                   if        prem_ttl < el2ampr
     C                             or el2sram <> 0
     c
     c                   if        el2sram <> 0
     c                   z-add     el2sram       xxsram           11 2
     c                   else
     c                   eval      xxsram =  el2ampr - prem_ttl
     c                   endif
     C                   if        juris_st = 'NJ'
     C                   eval      seexpo = 01
     C                   endif
     C
     C                   if        juris_st = 'GA'
     C                   z-add     expmod(1)     seexmd
     C                   eval      sertdt = rate_dt(1)
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : 9776 : 00 : seexmd :
     C                             0 : sertdt : 0 : xxsram :
     C                             0 : 0 : seexpo : 0)
     C                   else
     C                   if        juris_st = 'NC'
     C                             or juris_st = 'WI'
     C                             or juris_st = 'MA'
     C                             or juris_st = 'PA'
     C                             or juris_st = 'DE'
     C                             or juris_st = 'NJ'
     c                             or juris_st = 'NY'
     c                             or juris_st = 'CA'
     c                             or rrsraf <> 'S'
     c                   z-add     expmod(1)     seexmd
     c                   if        juris_st = 'NY'
     c                             or juris_st = 'CA'
     c                   move      *zeros        seexmd
     c                   endif
     C                   eval      sertdt = rate_dt(1)
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : 0931 : 00 : seexmd :
     C                             effMod(1) : sertdt : 0 : xxsram :
     C                             0 : 0 : seexpo : 0)
     C                   else
     c                   z-add     expmod(1)     seexmd
     c                   if        juris_st = 'CA'
     c                   move      *zeros        seexmd
     c                   endif
     C                   eval      sertdt = rate_dt(1)
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : 0931 : 00 : seexmd :
     C                             0 : sertdt : 0 : xxsram :
     C                             0 : 0 : seexpo : 0)
     C                   endif
     C                   endif
     C                   if        juris_st <> 'NJ'
     C                   eval       prem_ttl = el2ampr
     C                   endif
     C*
     C                   endif                                                  prem_ttl<elampr
     C*
     C                   endif                                                  nDays<365
     C*
     C                   endif                                                  adtPyrl=*on
     C*
     C                   endsr                                                  Short_rt


     C*===========================================================================
     C*  No_Expos - Write a "no exposure" ('1111') exposure record if no payrolls
     C*     exist for the policy at the state level.
     C*===========================================================================
     C     No_Expos      begsr
     C*
     C*
     C*  If no payroll exposure has been produced, add record '1111'.
     C*
     C                   if        man_prem = 0
     C                             and juris_st <> 'NJ'
     C                             and juris_st <> 'CA'
     C                             and @@clt# <> '135'
     C                             and @@clt# <> '129'
     C                             and fnpeof = ' '
     C*
     C                   if        juris_st = 'NC'
     C                             or juris_st = 'WI'
     C                   eval      semddt = EffMod(1)
     C                   eval      sertdt = rate_dt(1)
     C                   else
     C                   eval      semddt = 0
     C                   eval      sertdt = 0
     C                   endif
     C*
     C                   if        svminp = 0
     C                   if        el2faf = 'A'
     C                   eval      svminp = el2aspr
     C                   else
     C                   eval      svminp = el2stpr
     C                   endif
     C                   endif
     C*
     C                   eval       xtra_prem = svminp - mod_prem
     C                   if        juris_st = 'NC'
     C                   eval       xtra_prem = 0
     C                   endif
     C*
     C*
     C                   endif
     C*
     C                   if        man_prem > 0
     C                   eval      noexpo = 'N'
     C                   endif
     C                   eval      svmodp = svmodp + mod_prem
     C                   eval      mod_prem = 0
     C                   eval      man_prem = 0
     C*
     C                   endsr

     C*===========================================================================
     C*  min_Prem - Calc the difference between the modified premium and the
     C*     standard prem to get the minimum premium.
     C*===========================================================================
     C     min_Prem      begsr
     C*
       If EL2aMnP <> 'Y' And EL2eMnP <> 'Y';
          //*** Beg Add ***** 09/24/25 *******************************
          svmodp = 0;
          //*** End Add ***** 09/24/25 *******************************
          LeaveSr;
       Endif;
     C                   eval       xtra_prem = 0
     C*
     C                   eval       xtra_prem = svminp - (svmodp + bal_min)
     C*  If no payroll exposure has been produced, add record '1111'.
     C*
     C*  If min premium exists then write a record
     C                   if        xtra_prem > 0
      *** Beg Del ***** 07/31/25 ***********************
     C***                          and noexpo = 'N'
      *** End Del ***** 07/31/25 ***********************
     c                             and juris_st <> 'CA'
     C                   if        juris_st = 'NC'
     C                             or juris_st = 'WI'
     C                             or juris_st = 'MA'
     C                             or juris_st = 'PA'
     C                             or juris_st = 'DE'
     C                             or juris_st = 'NJ'
     C                             or juris_st = 'NY'
     C                             or juris_st = 'AR'
     C                             or juris_st = 'LA'
     C                             or juris_st = 'MS'
     C                   eval      semddt = EffMod(1)
     C                   eval      sertdt = rate_dt(1)
     C                   else
     C                   eval      semddt = EffMod(1)
     C                   eval      sertdt = rate_dt(1)
     C                   endif
     C                   if        juris_st = 'NJ'
     C                   eval      seexpo = 01
     C                   endif
     C*
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : svstcd : 0990 : 00 : 0 :
     C                               semddt : sertdt :
     C                              0 : xtra_prem : 0 : 0 : seexpo : 0)
     C                   eval       xtra_prem = 0
     C                   endif                                                  xtra_prem>0
     C*
     C*  If min prem exists but the exposure amount is > min prem, load the
     C*  standard premium from the premium detail file WDMPP.
     C*
     C                   if        xtra_prem < 0
     C     epky          chain     pmp01                              99
     C                   if        *in99 = *off
     C*
     C                   if        AdtPyrl = *on and mpaspr > 0
     c                   if        juris_st = 'CA'
     ** Subtract out the audited normal prem, expense constant and add in the deductible credits.
     c                   eval      Ustd_prem = Ustd_prem - (el2anmp + el2prtd) -
     c                                         (el2adct + el2add2)
     ** Add in Min prem file Normal prem, expece constant and subtract deductible credits.
     c                   eval      Ustd_prem = Ustd_prem + (mpanmp + mpprtd) +
     c                                         (mpadct + mpadd2)
     c                   else
     C                   sub       el2aspr       Ustd_prem                      audited amt
     C                   add       mpaspr        Ustd_prem                      audited amt
     c                   endif                                                  juris_st = 'CA'
     C                   endif
     C                   if        AdtPyrl = *off and mpstpr > 0
     c                   if        juris_st = 'CA'
     ** Subtract out the estimated normal prem, expense constant and add in the deductible credits.
     c                   eval      Ustd_prem = Ustd_prem - (el2enmp + el2prtd) -
     c                                         (el2edct + el2edd2)
     ** Add in Min prem file Normal prem, expece constant and subtract deductible credits.
     c                   eval      Ustd_prem = Ustd_prem + (mpenmp + mpprtd) +
     c                                         (mpedct + mpedd2)
     c                   else
     C                   sub       el2stpr       Ustd_prem                      non-adtd amt
     C                   add       mpstpr        Ustd_prem                      non-adtd amt
     c                   endif                                                  juris_st = 'CA'
     C                   endif
     C*
     C                   endif
     C                   endif
     C*
     C                   eval      mod_prem = 0
     C                   eval      man_prem = 0
     C                   eval      svmodp = 0
     C                   eval      bal_min = 0
     C*
     C                   endsr                                                  min_Prem

     C*===========================================================================
     C*  inAdjust - write a record for Indiana flat rate adjustment where applicable
     C*===========================================================================
     C     inAdjust      begsr
     C
     C                   move      fnco#         @@co#
     C                   move      fnfnd         @@fnd
     C                   move      elfyr         @@fyr
     C                   move      el2emp#       @@emp#
     C                   move      el2div        @@div
     C
     C                   call      'SRFLAT'      inAdjPl
     C                   move      @@amt         ind_amt
     C                   add       ind_amt       in_adjust
     C*
     C                   if        in_adjust <> 0
     C
     C
     C*      Write an exposure record if adjustment<>0
     C                   if         in_adjust < 0
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : 0994 : 00 : expMod(1) :
     C                             effMod(1) : 0 : 0 :
     C                              -in_adjust : 0 : 0 : 0 : 0)
     C                   else
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : 0998 : 00 : expMod(1) :
     C                             effMod(1) : 0 : 0 :
     C                              in_adjust : 0 : 0 : 0 : 0)
     C                   endif                                                  in_adjust<0
     C
     C                   endif                                                  IN_adjust<>0
     C
     C                   eval      prem_ttl = prem_ttl + in_adjust
     C                   eval      in_adjust = 0
     C
     C                   endsr                                                  inAdjust

      *** Beg Add ***** 08/05/25 ******************************
     C*===========================================================================
     C*  maARAP - write a record for Massachusetts , code 0277
     C*===========================================================================
     C     maARAP        begsr

     C                   if        adtPyrl = *on
     C
     C                   eval      ma_ARAP = el2rpa1 + el2rpa2 + el2rpa3
     C
     C                   else
     C
     C                   eval      ma_ARAP = el2rpe1 + el2rpe2 + el2rpe3
     C
     C                   endif
     C
     C*  Write exposure record.
     C
     C                   if         ma_ARAP > 0
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : 0277 : 00 : 0 :
     C                             effMod(1) : rate_dt(1) : 0 :
     C                              ma_ARAP : 0 : 0 : 0 : 0)
     C                   endif                                                  ma_ARAP > 0
     C                   eval      ma_ARAP = 0

     C                   endsr                                                  maARAP

      *** End Add ***** 08/05/25 ******************************
     C*===========================================================================
     C*  srHarborMin - write a record for Harbor Exposure balance to minimum when applicable
     C*===========================================================================
      /free
       Begsr srHarborMin;

       Setgt heky phe01;
       Readpe heky2 phe01;

       If not %eof and hehemc <> 0
        and juris_st <> 'CA';

          If juris_st = 'NC'
           or juris_st = 'WI'
           or juris_st = 'MA'
           or juris_st = 'PA'
           or juris_st = 'DE'
           or juris_st = 'NJ'
           or juris_st = 'NY'
           or juris_st = 'AR'
           or juris_st = 'LA'
           or juris_st = 'MS';
              semddt = EffMod(1);
              sertdt = rate_dt(1);
           Else;
              semddt = EffMod(1);
              sertdt = rate_dt(1);
           Endif;

           If juris_st = 'NJ';
              seexpo = 01;
           Endif;

           WriteExp(fnco# : fnfnd : ##rpmy : el2emp# :
            el2fyr : stcode : hehemc : 00 : 0 :
            semddt : sertdt : 0 :
            el2hxmn - FClassTotal() : 0 : 0 : seexpo : 0);

       Endif;

       Endsr;                                                     //srHarbormin Subr
      /end-free


     C*============================================================================
     C*  lossdata - Subroutine to get Loss record info.  Multiple loss records can
     C*             be written for each employer and for multiple reporting levels.
     C*============================================================================
     C     lossdata      begsr
     C*
     C     cmky          chain     pcm01                              60
     C                   dow       *in60 = *off
     C*
     C                   eval       kkcase = cmcase
     C*
     C*  Exclude record only claims from extraction
     C                   if        cmltfl <> 'R'
     C*
     C*  Calculate the deduct recovery amt for this claim.
     C*
     C                   exsr      loss_link
     C                   exsr      srPmtHst
     C                   exsr      srRsvHst
     C*
     C*    Deductible reimbursement
     c                   if        xxlrgd <> 'Y'
     C                   eval      kkst = juris_st
     C     ddky          setgt     pdd01
     C                   readp     pdd01                                  60
     c                   endif
     C                   if         *in60=*off
     c                             and ddstat = 'A'
     C                             and ddtype <> 'G'
     c                             and xxlrgd <> 'Y'
     c                             or xxlrgd = 'Y'
     c                             and ldtyp1 <> 'G'
     C*
     c                   add       dedindrec     indeminc
     c                   add       dedmedrec     medicinc
     c                   add       dedexprec     allocinc
     C*
     c                   add       prvdedind     prvindinc
     c                   add       prvdedmed     prvmedinc
     c                   add       prvdedexp     prvexpinc
     C*
     c                   endif                                                  *ni60 = *off
     C                   if        cmefdt <= kkMthDt
     C*
     C     chky2         setgt     pch01
     C     chky          readpe    pch01                                  71
     C*
     C                   if        cmcstf <> 0
     C                   eval      indemPay = cmcstf
     C                   if        chocst = 'C'
     C                   eval      indemInc = cmcstf
     C                   endif
     C                   endif
     C*
     C                   if        cmmstf <> 0
     C                   eval      medicPay = cmmstf
     C                   if        chocst = 'C'
     C                   eval      medicInc = cmmstf
     C                   endif
     C                   endif
     C*
      /Free
       Chain llky pll01;
       If Not %Found(wmlll);
          Clear pll01;
       EndIf;
       if CMjrSt = 'CA' and (CMlsrv = '03' or CMlsrv = '05' or
          CMlsrv = '06' Or LLfrau = '01' Or CMncci = '08');
           prvindinc = prvindinc + PindRecv;
           prvmedinc = prvmedinc + PmedRecv;
           indeminc  = indeminc  + indRecv;
           medicinc  = medicinc  + medRecv;
       endif;
      /End-free
     C                   endif
     C*
     C*
     C*    If report for 2nd reporting level or greater, check if the claim
     C*    changed in order to report it.
     C                   eval       rptLoss = *off
     C                   if        curLvl > 1
     C                   exsr      srPrevRpt
     C
     C*    If the previous incurred and paid amounts are 0, the record was
     C*    NOT previously reported.  Therefore a 'P' type record should not
     C*    be reported.
     C
     C                   eval      xxprevinc = prvindinc + prvmedinc + prvexpinc
     C                   eval      xxprevpd = prvindpd + prvmedpd + prvexppd
     C                   if        juris_st = 'NJ'
     C                   eval      xxprevinc = xxprevinc - prvexpinc
     C                   endif
     C
     C
     C*    If claim record is to be written then write a 'P' type record
     C*    only if the prevNone flag is *off
     C                   if        rptLoss = *on
     C                             and prevNone = *off
     C                             and ((xxprevinc > 0) or
     C                              (xxprevinc = 0 and xxprevcnt > 0))
     C                   exsr      srWrtPrv
     C                   endif                                                  rptLoss=*on
     C
     C                   endif                                                  curLvl>1
     C*
     C* Incurred indemnity and incurred medical
     C* Incurred expense
     C*    Incurred indemnity, medical, and expense
     C*
     C                   eval(h)   slindm = indemInc
     C                   eval(h)   slmedi = medicInc
     C                   eval(h)   slalai = allocInc
     c*
     c* Do not report a subsequent report on a medical only
     c*
     C     chky2         setgt     pch01
     C     chky          readpe    pch01                                  60
     c*
     c                   if        slmedi = 0 and cmltfl = 'M' and
     c                             chocst = 'C' and juris_st = 'NJ'
     c                   eval      rptLoss = *off
     c                   endif
     C*
     C*  Do not write Loss record if no incurred indemnity or medical
     C                   if        (slindm <> 0) OR (slmedi <> 0)
     C                             or (slalai <> 0)
     C                             and juris_st <> 'NJ'
     C                             or rptLoss = *on
     C*
     C*  REPORT THE FOLLOWING FIELDS BASED ON ASWG FORMAT
     C                   if        ASWGflag = 'Y'
     C*    Paid indemnity and paid medical
     C                   eval(h)   slpdin = indemPay
     C                   eval(h)   slpdmd = medicPay
     C*
     C*    Allocated loss adjustment expense - paid and incurred
     C                   eval(h)   slalap = allocPay
     C*
     C                   else
     C*    Previously reported
     C                   eval      slprvr = 0
     C*    Loss coverage code
     C                   move      cmlscv        sllos#
     C                   endif                                                  ASWGflag='Y'
     C*
     C*    Class code
     C                   eval      slclas = cmclas
     C*
     C*
     C*    Number of claims
     C                   eval      sl#clm = 1
     C*    Accident date
     C                   eval      slacdt = cmacdt
     C*    Claim number
     C                   movel     cmcase        xxnum            80
     C                   move      *blanks       edtmsk           10
     C                   call      'SA020R'
     C                   parm                    xxnum
     C                   parm                    edtmsk
     C*
     C                   movel(p)  xxnum         xxcase
     C                   movel     claim#        slclm#
     C*
     C                   if        juris_st = 'CA'
     C                   movea     slclm#        xxclm#
     C                   do        12            i
     C                   if        xxclm#(i) <> '0' and xxclm#(i) <> ' '
     C                   leave
     C                   endif
     C                   if        xxclm#(i) = '0'
     C                   movel     ' '           xxclm#(i)
     C                   endif
     C                   enddo
     C                   movea     xxclm#        slclm#
     C                   endif
     C*

      /free
       If @@clt# = '129' and CMpds# <> *blanks or
          @@clt# = '139' and CMpds# <> *blanks or
          FNgrpasc = 'OPTACOMP' and CMpds# <> *blanks;
      /end-free
     C*
     C                   movel(p)  cmpds#        slclm#
     C                   endif
      /free
         If (@@clt#  = '114' Or @@clt# = '152') and
            FNgrpasc = 'OPTACOMP' and CMpds# <> *blanks;
            Exsr ClrHypClm;
            SLclm# = @@tempClm#;
         Endif;
      /end-free
     C*
     C                   if        (@@clt# = '136' Or @@clt# = '150')
     C                             and cmpds# <> *blanks
     C                   movel(p)  CMpds#        slclm#
     C                   endif
     C* Populate state claim# for NY
     c                   Clear                   SLcas#
     c                   If        juris_st = 'NY'
     c                   If        cltID = '150'
     c                   Movel     CMjrC#        SLcas#
     c                   Else
     c                   Movel     CMacl#        SLcas#
     c                   EndIf
     c                   EndIf
     C*
     C*    Determine claim status
     C*
     C*      Look at history records for closed
     C     chky2         setgt     pch01
     C     chky          readpe    pch01                                  60
     C*
     C                   if        chocst = 'O'
     C                   eval       slclst = 0                                   Open claim
     C     chky          readpe    pch01                                  60
     C*
     C                   if        chocst = 'C'
     C                             and juris_st <> 'DE'
     C                             and juris_st <> 'NJ'
     C                             and juris_st <> 'PA'
     C                             and juris_st <> 'MA'
     C                             and juris_st <> 'CA'
     C                             and juris_st <> 'NC'
     C                             and prevclsd = *on
     C                             and curLvl > 1
     C                   eval       slclst = 2                                   Reopened claim
     C                   endif
     C                   eval      sum_arr = indemPay + medicPay + allocPay
     C                   if        cmjrst = 'MI' and sum_arr=0
     C                   eval       slclst = 4                                   pmt not made
     C                   endif
     C                   else
     C                   eval       slclst = 1                                   closed claim
     C                   endif
     C*
     C*    Injury code
     C                   move      cmncci        slinj#
     C                   if        slindm = 0
     C                   eval      slinj# = 06
     C                   endif
     C*
     C*    Catastrophe number
     C*
     C                   If        CMnatr = '83' And CMcaus = '83' And
     C                             CMacDt > 1191201
     C                   Eval      SLcat# = 12
     C                   Else
     C                   move      cmcata        slcat#
     C                   EndIf
     C*
     C*    Social security number
     c                   if        juris_st <>  'MN'
     c                             and juris_st <> 'CA'
     C                   move      cmss#         slss#
     C                   endif
     C*
     C*  ONLY REPORT THE FOLLOWING FIELDS IN THE ASWG FORMAT
     C                   if        ASWGflag = 'Y'
     C*    Update type
     C                   eval       slupdt = 'R'                                default
     C*
     C*    Lump sum indicator
     C     llky          chain     pll01                              60
     C                   if        *in60=*off
     C*
     C                   if        lllump = 'Y'
     C                   eval       sllump = 'Y'
     C                   else
     C                   eval       sllump = 'N'
     C                   endif                                                  LLLUMP='Y'
     C*
     C                   movel     llmtst        sllc5
     C*
     C*    Fraudulent claim indicator
     C                   move      llfrau        slfrau
     C*
     C                   else
     C*
     C                   eval       sllump = 'N'
     C                   eval       sllc5 = 0
     C                   eval       slfrau = 0
     C*
     C                   endif                                                  *in60=*off
     C*
     C*    Loss conditions
     C                   eval       kktbid = CMJRST + 'LC'
     C                   movel(p)  cmlscv        kkkey
     C     gtky          chain     pgt01                              60
     C                   if        *in60 = *on
     C                   eval       kktbid = '  LC'
     C     gtky          chain     pgt01                              60
     C                   endif
     C*
     C                   if         *in60=*off
     C                   move      Act           sllc1
     C                   move      Typ_los       sllc2
     C                   endif                                                  *in60=*off
     C*
     C*      If code is blank, default this value to '01'
     C                   if        cmlsrv = *blank
     C                   move      '01'          sllc3                           default
     C                   else
     C                   move      cmlsrv        sllc3
     C                   endif                                                  cmlsrv=*blank
     C*
     C*      If code is blank, default this value to '01'
     C                   if        cmcovt = *blank
     C                   move      '01'          sllc4                           default
     C                   else
     C                   move      cmcovt        sllc4
     C                   endif                                                  cmcovt=*blank
     C
     C*
     C*    Jurisdiction state (from claim)
     C                   movel(p)  cmjrst        kkkey
     C     stky          chain     pst01                              60
     C  N60              move      stnst         sljrst
     C*
     C*    Managed care organization type
     C                   move      cmmctp        slmctp
     C*
     C*    Injury description code
     C                   movel(p)  cmpart        kkkey
     C     tbky          chain     ppa01                              60
     C                   movel(p)  paedip        slpart
     C                   movel(p)  cmnatr        kkkey
     C     tbky          chain     pna01                              60
     C                   move      naedin        slnatr
     C                   movel(p)  cmcaus        kkkey
     C     tbky          chain     pca01                              60
     C                   move      caedic        slcaus
     C*
     C*    Occupational description
     C                   eval       sloccu = *blank
     C                   if        cmocds <> *blank
     C                   movel     cmocds        sloccu
     C*
     C                   else
     C                   if        cmoccd <> *blank
     C                   movel(p)  cmoccd        kkkey
     C     ocky          chain     poc01                              60
     C*
     C                   if        *in60 = *off
     C                   movel     ocdesc        sloccu
     C                   endif                                                  *in60=*off
     C*
     C                   else
     C                   eval      kkclas = cmclas
     C                   eval      kkrtcd = el2rtcd
     C     EPKYL         CHAIN     PEP01                              63
     C                   Z-ADD     0             KKEFD1
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   KKCLAS        ##CLAS            5
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
     C                   MOVE      ##DATE        KKEFD1
     C     cmext         ifne      ' '
     C     pxky          setgt     ppx01
     C     pxky1         readpe    ppx01                                  61
     C     *in61         ifeq      *off
     C                   movel(p)  pxdesc        sloccu
     C                   endif                                                  *in61=*off
     C                   else
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  60
     C                   if        *in60 = *off
     C                   movel(p)  prdesc        sloccu
     C                   endif                                                  *in60=*off
     C                   endif                                                  CMEXT<>' '
     C*
     C                   endif                                                  cmoccd<>*blank
     C*
     C                   endif                                                  cmocds<>*blank
     C*
     C*
     C*    Vocational description
     C                   if        cmvorc = 'Y'
     C                   eval       slvocr = 'Y'
     C                   else
     C                   eval       slvocr = 'N'
     C                   endif
     C*
     C*    Claimant's and Employer's attorney fees are optional
     C*
     C                   z-add     clmattfee     slcatf                         clmnt atty fees
     C                   z-add     empattfee     sleatf                         emp atty fees
     C*
     C*    Deductible reimbursement
     c                   if        xxlrgd <> 'Y'
     C                   eval      kkst = juris_st
     C     ddky          setgt     pdd01
     C                   readp     pdd01                                  60
     C                   if         *in60=*off
     c                             and (ddstat = 'A')
     C*
     C                   if        ddtype = 'G'
     C                   eval       sldedr = 0
     C
     C                   else
     C*
     C                   eval(h)   sldedr = dedIndRec + dedMedRec +
     C                                         dedExpRec + dedLegRec
     C*
     C                   endif                                                  ddtype = 'G'
     C*
     C                   endif                                                  *in60=*off
     c                   else

     C                   if        ldtyp1 = 'G'
     C                   eval       sldedr = 0
     C                   else
     C*
     C                   eval(h)   sldedr = dedIndRec + dedMedRec +
     C                                         dedExpRec + dedLegRec
     C                   endif                                                  ldtyp1 = 'G'
     C
     c                   endif                                                  if xxlrgd <> 'Y'
     C*
     C                   endif                                                  ASWGflag='Y'
     C*
     C                   eval       slaswg = ASWGflag
     C*
     C                   eval       slaww  = cmaww
     C                   eval       slvrhb = vocRehab
     C                   eval       slipct = cmipct
     C*                  eval       sltgi  = slindm + slmedi + slalai
     C*
     C*           Update unit total values
     C                   add       SLINDM        Uindem_inc
     C                   add       SLMEDI        Umed_inc
     C                   add       SLPDIN        Uindem_pd
     C                   add       SLPDMD        Umed_pd
     C                   add       SLCATF        Uclmnt_att
     C                   add       SLEATF        Uemp_att
     C                   add       SLALAP        Ualae_pd
     C                   add       SLALAI        Ualae_inc
     C*
     C*  Maintenance fields
     C                   eval      sluser = q1user
     C                   eval      slpgm = q1pgm
     C                   eval      sltime = dstime
     C                   eval      sldt = dtCYMD(dsdate)
     C                   eval      slidt = sldt
     C                   eval      sliusr = sluser
     C*
     C                   if        curLvl = 1
     C                             or rptLoss = *on
     C*
     C                   write     psl01
     C*
     C*  Increment # loss records counter
     C                   add       1             U#claims
     C*
     C                   if        juris_st = 'NJ'
     C                   if        svpfdt < 1070101 and indemPay > 10000
     C                             or svpfdt >= 1070101 and cmncci = '01'
     C                             or svpfdt >= 1070101 and cmncci = '02'
     C                             or svpfdt >= 1070101 and cmncci = '03'
     C                             or cmicr = 'Y'
     C                   exsr      SR_ICR
     C                   endif
     C                   endif
     C*
     C                   if        juris_st = 'PA' or juris_st = 'DE'
     C                   if        slinj# = 01 or slinj# = 02 or slinj# = 03
     C                   exsr      SR_ICR
     C                   endif
     C                   endif
     C*
     C                   endif                                                  if curLvl=1
     C*                                                                         or rptLoss=*on
     C*    Increment total claims counter -  this will represent the total
     C*    number of claims reported for the policy.  This will reflect
     C*    those that are associated w/ policy but not reported on subsequent
     C*    reports (i.e. a revised total)
     C                   add       1             totClms
     C*
     C                   endif                                                  slindm<>0,slmedi<>0
     C*
     C                   endif                                                  cmltfl<>'R'
     C*
     C*  Read next record
     C     cmky          reade     pcm01                                  60
     C*
     C                   enddo                                                  *in60=*off
     C*
     C                   endsr                                                  lossdata



     C*============================================================================
     C*  loss_link - Subroutine to assign link data values for Loss records
     C*====================================================================
     C     loss_link     begsr
     C*
     C                   clear                   psl01
     C                   eval      slco# = fnco#
     C                   eval      slfnd = fnfnd
     C                   eval      slrpmy = ##rpmy
     C*
     C                   eval      slcarr = link_car#
     C                   eval      slemp# = link_emp#
     C                   eval      slfyr = link_fyr
     C*
     C                   move      link_stcd     slstcd
     C*
     C                   eval      slefdt = link_efdt
     C                   eval      slrpt# = link_rpt#
     C                   move      link_corr     slcorr
     C*
     C                   endsr                                                  loss_link



     C*============================================================================
     C*  rsrv_Amt  - Subroutine to accumulate remaining reserves (CMRR_) and pay-
     C*              ments against reserves (CMAR_) based on remaining reserve
     C*              type associated with rem rsrv code.
     C*====================================================================
     C*
     C*
     C*
     C*
     C*
     C*
     C*
     C*  Add any deduct recovery amounts to the paid amounts and subtract
     C*    from the remaining reserve amounts.  This will result in the total
     C*    paid being a gross figure and the total incurred will not change.
     C*
     C*
     C*  This code was deleted to prevent the paid amount from exceeding
     C*  the incurred amount.  The deduct recoveries should not affect
     C*  the outstanding reserves for reporting purposes.
     C*



     C*==========================================================================
     C*  clrPyrlArr - clears the values in the arrays used to accumulate values
     C*               when splitting the payrolls
     C*==========================================================================
     C     clrPyrlArr    begsr

     C                   clear                   expamt
     C                   clear                   pramt
     C                   clear                   pefdt
     C                   clear                   manrt
     C                   clear                   splitp
     C                   clear                   rttier
     C                   clear                   expact
     C                   clear                   expos#
     C
     C                   endsr                                                  clrPyrlArr



     C*==========================================================================
     C*  srCombine - combines exposure recs that were split according to rate
     C*              and eff mod dates but don't have rate or mod change
     C*==========================================================================
     C     srCombine     begsr

     C*   wcstat exposure file
     C     seky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkrpmy
     C
     C     seky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkrpmy
     C                   kfld                    savemp#
     C                   kfld                    savfyr
     C                   kfld                    savstcd
     C                   kfld                    savclas
     C                   kfld                    savmddt
     C                   kfld                    savrtdt
     C                   kfld                    savmnrt
     C                   kfld                    savexpo
     C
     C*    wcstat unit total
     C     sqky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkrpmy
     C                   kfld                    savemp#
     C                   kfld                    savfyr
     C                   kfld                    savstcd
     C
     C                   z-add     0             savemp#           9 0
     C                   z-add     0             savfyr            3 0
     C                   z-add     0             savstcd           2 0
     C                   z-add     0             savclas           5 0
     C                   z-add     0             savexmd           5 4
      **** Beg Chg **** 06/17/24 *******************************************
     C*******************z-add     0             savmnrt           7 3
     C                   z-add     0             savmnrt           8 3
      **** End Chg **** 06/17/24 *******************************************
     C                   z-add     0             savmddt           7 0
     C                   z-add     0             savrtdt           7 0
     C                   z-add     0             savexpo           2 0
     C
     C     seky1         chain     pse01                              70
     C                   dow       *in70 = *off
     C
     C                   if        seemp# = savemp#
     C                             and sefyr = savfyr
     C                             and sestcd = savstcd
     C                             and seclas = savclas
     C                             and seexmd = savexmd
     C                             and semnrt = savmnrt
     C                             and seexpo = savexpo

     C                   If        sestcd <> 07                                 state <> DE
     C                             and sestcd <> 37                             state <> PA
     C                             or seexpo <> 00
     C                             or fnpeof <> ' '

     C                   eval       expa = seexpa
     C                   eval       prem = seprma
     C                   delete    pse01
     C*               reduce # of records in unit total
     C     sqky          chain     psq01                              72
     C                   if        *in72 =  *off
     C                   sub       1             sq#rcd
     C                   update    psq01
     C                   endif
     C
     C     seky2         chain     pse01                              71
     C                   if        *in71=*off
     C                   eval       seexpa = seexpa + expa
     C                   eval       seprma = seprma + prem
     C                   update    pse01
     C                   endif                                                  *in71=*off
     C
     C                   else
     C                   unlock    wdsel
     C                   endif                                                  Juris_st <> PA/DE
     C
     C                   else
     C                   unlock    wdsel
     C                   endif                                                  seemp#=savemp#
     C
     C                   z-add     seemp#        savemp#
     C                   z-add     sefyr         savfyr
     C                   z-add     sestcd        savstcd
     C                   z-add     seclas        savclas
     C                   z-add     seexmd        savexmd
     C                   z-add     semnrt        savmnrt
     C                   z-add     semddt        savmddt
     C                   z-add     sertdt        savrtdt
     C                   z-add     seexpo        savexpo
     C     seky1         reade     pse01                                  70
     C
     C                   enddo                                                  *in70=*off
     C
     C                   endsr                                                  srCombine


     C*==========================================================================
     C*  srGMerge - This subroutine assigns a value of 1 to the employer #
     C*             for all extract records for the GIC Master Policy group.
     C*==========================================================================
     C     srGMerge      begsr
     C
     C                   z-add     1             savemp#
     C
     C*   Update the Header records.
     C*
     C     seky1         chain     psr01                              70
     C                   dow       *in70 = *off
     C
     C                   if        sremp# <> 1
     C
     C                   if        wrtLnkHdr(srco#:srfnd:srrpmy:
     C                                         savemp#:srfyr:srstcd) = *on
     C                   eval       sremp# = 1
     C                   update    psr01
     C                   else
     C                   delete    psr01
     C                   endif

     C                   endif

     C     seky1         reade     psr01                                  70
     C                   enddo
     C
     C                   feod      wmsrl
     C
     C*   Update the Exposure records.
     C*
     C     seky1         chain     pse01                              70
     C                   dow       *in70 = *off
     C
     C                   if        seemp# <> 1
     C                   eval       seemp# = 1
     C                   update    pse01
     C                   endif

     C     seky1         reade     pse01                                  70
     C                   enddo
     C
     C                   feod      wdsel
     C
     C*   Update the loss records.
     C*
     C     seky1         chain     psl01                              70
     C                   dow       *in70 = *off
     C
     C                   if        slemp# <> 1
     C                   eval       slemp# = 1
     C                   update    psl01
     C                   endif

     C     seky1         reade     psl01                                  70
     C                   enddo
     C
     C                   feod      wdsll
     C

     C                   endsr                                                  srGMerge

     C*==========================================================================
     C*  chkExpo - look at each header record for an extraction and ensure that
     C*            a corresponding exposure record exists, if there is none then
     C*            write an exposre record indicating no payrolls.
     C*==========================================================================
     C     chkExpo       begsr

     C     srky1         klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C                   kfld                    ##rpmy

     C     seky3         klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C                   kfld                    ##rpmy
     C                   kfld                    sremp#
     C                   kfld                    srfyr
     C                   kfld                    srstcd

     C     sqky3         klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C                   kfld                    ##rpmy
     C                   kfld                    sremp#
     C                   kfld                    srfyr

     C     srky1         chain     psr01                              70
     C                   dow       *in70 = *off
     C
     C*   If report #1
                         Clear  hldMdDt;
                         Clear  hldRtDt;
                         Clear  hldExmd;
     C                   if        srrpt# = '1'
     C
      *   If exposure is not found then write a 'no payrolls' record
      *
     C     seky3         chain     pse01                              71

     C                   If        *in71 = *off
     C                   dou       *in60 = *on
                         If SEmdDt <> 0 And hldMdDt = 0;
                            hldMdDt = SEmdDt;
                            hldRtDt = SErtDt;
                         EndIf;
                         If SEexMd <> 0 And hldExMd = 0;
                            hldExmd = SEexMd;
                         EndIf;
     c                   select
     c                   when      seclas = sveccd or seclas = svilmc
     c                             or seclas = svwvac
     C                   move      *on           *in71
      *
      * Other non-exp code future use
      *                  when      seclas = svxxxxx

     C                   other
     C                   move      *Off          *in71
     C                   leave
     C                   endsl
     c
     C     seky3         reade     pse01                                  60
     C                   enddo
     C                   endif

     C                   if        *in71 = *on
     C                             and srstcd <> 29
     C                             and srstcd <> 04

     C                   eval      xxexpa = 0
     C     sqky3         chain     psq01                              71
     C                   dow       *in71 = *off
     C                   eval      xxexpa = xxexpa + sqstdp
     C     sqky3         reade     psq01                                  71
     C                   enddo

     C                   if        xxexpa > 0

     C                   clear                   pse01
     C                   eval      seclas = 1111
     C                   eval      seexp# = 00
     C                   eval      seexmd = hldExmd
     C                   eval      semddt = hldMdDt
     C                   eval      sertdt = hldRtDt
     C                   eval      seexpa = 0
     C                   eval      seprma = 0
     C                   eval      semnrt = 0
     C                   eval      setier = 0
     C                   eval      seexpo = 0
     C
     C                   if        srstcd = 32
     C                             or srstcd = 48
     C                   eval      semddt = srefdt
     C                   eval      sertdt = srefdt
     C                   endif
     C
     C                   eval      seco# = srco#
     C                   eval      sefnd = srfnd
     C                   eval      serpmy = srrpmy
     C                   eval      seemp# = sremp#
     C                   eval      sefyr = srfyr
     C                   eval      sestcd = srstcd
     C                   eval      seprvr = 0
     C                   eval      setier = 0
     C                   eval      seupdt = 'R'
     C                   eval      seuser = q1user
     C                   eval      sepgm = q1pgm
     C                   eval      setime = dstime
     C                   eval      sedt = dtCYMD(dsdate)
     C                   eval      seiusr = seuser
     C                   eval      seidt = sedt
       //*** Beg Add *** 02/23/24 *************************************
                   // Report the policy effective date if Mod/Rate date = 0
                   Chain(n) (SRco#:SRfnd:SRfyr:SRemp#) pel0b;
                   If SEmdDt = 0;
                      SEmdDt = ELBpfDt;
                   Endif;
                   If SErtDt = 0;
                      SERtDt = ELBpfDt;
                   Endif;
       //*** End Add *** 02/23/24 *************************************

     C                   write     pse01
     C                   add       1             u#exposure


     C                   eval      kkemp# = sremp#
     C                   eval      kkfyr = srfyr
     C                   eval      kkstcd = srstcd
     C                   exsr      unitTotal


     C                   endif

     C                   endif

     C                   endif                                                  srrpt#=1

     C     srky1         reade     psr01                                  70
     C                   enddo
     C                   endsr                                                  chkExpo



     C*============================================================================
     C*  chkSubsqnt - check header records on subsequent reports for corresponding
     C*               loss records: delete headers and unit reports if no loss
     C*               record exists.
     C*============================================================================
     C     chkSubsqnt    begsr
     C
     C     srky2         klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C                   kfld                    ##rpmy

     C     slky          klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C                   kfld                    ##rpmy
     C                   kfld                    sremp#
     C                   kfld                    srfyr
     C                   kfld                    srstcd

     C     sqky1         klist
     C                   kfld                    fnco#
     C                   kfld                    fnfnd
     C                   kfld                    ##rpmy
     C                   kfld                    sremp#
     C                   kfld                    srfyr
     C                   kfld                    srstcd

     C     srky2         chain     psr01                              70
     C                   dow       *in70 = *off
     C
     C*   If report #1
     C                   if        srrpt# <> '1'
     C
     C*   If a record is not found then delete header and unit records
     C     slky          chain     psl01                              71
     C                   if        *in71 = *on

     C*              -delete unit
     C     sqky1         chain     psq01                              72
     C                   if        *in72 = *off
     C                   delete    psq01
     C                   endif
     C
     C*              -delete header
     C                   delete    psr01
     C
     C                   endif                                                  *in71=*on

     C                   else
     C
     C*   If a record is not found then delete header and unit records
     C     slky          chain     pse01                              71
     C  n71              unlock    wdsel
     C     slky          chain     psl01                              72
     C                   if        *in71 = *on and *in72 = *on

     C*              -delete unit
     C     sqky1         chain     psq01                              72
     C                   if        *in72 = *off
     C                   delete    psq01
     C                   endif
     C
     C*              -delete header
     C                   delete    psr01
     C
     C                   endif                                                  *in71=*on

     C                   endif                                                  srrpt#=1

     C     srky2         reade     psr01                                  70
     C                   enddo
     C
     C                   endsr                                                  chkSubsqnt



     C*============================================================================
     C*  unitTotal - Subroutine to assign values for Unit total record
     C*============================================================================
     C     unitTotal     begsr
     C
     C*    wcstat unit total
     C     sqky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkrpmy
     C                   kfld                    kkemp#
     C                   kfld                    kkfyr
     C                   kfld                    kkstcd
     C     sqky2         chain     psq01                              70
     C                   if        *in70 = *on
     C*
     C                   clear                   psq01
     C*  Assign link data
     C                   eval      sqco# = fnco#
     C                   eval      sqfnd = fnfnd
     C                   eval      sqrpmy = ##rpmy
     C                   eval      sqcarr = link_car#
     C                   eval      sqemp# = link_emp#
     C                   eval      sqfyr = link_fyr
     C                   move      link_stcd     sqstcd
     C                   eval      sqefdt = link_efdt
     C                   move      link_rpt#     sqrpt#
     C                   move      link_corr     sqcorr
     C*
     C                   if        curLvl = 1
     C*
     C*    Exposure - payroll
     C                   eval(h)   sqepay = Uexp_pay
     C*    Exposure - other than payroll
     C                   eval(h)   sqeoth = Uexp_oth
     C*    Subject premium total
     C                   eval(h)   sqsubp = Usubj_prem
     C*    Standard premium total
     C                   eval(h)   sqstdp = Ustd_prem
     C*
     C                   endif
     C*
     C*    Number of claims total
     C                   eval(h)   sq#clt = totClms
     C*    Incurred indemnity total
     C                   eval(h)   sqtind = Uindem_inc
     C*    Incurred medical total
     C                   eval(h)   sqtmed = Umed_inc
     C*    # records in unit report(1header, 1name, 1address, #exposure, #losses, 1unit)
     C                   eval      sq#rcd = U#exposure + U#claims +
     C                                           (U#lnkhdr*3) + 1
     C*    Previously reported indicator
     C                   eval      sqprpt = 0
     C*    Total paid indemnity
     C                   eval(h)   sqpdin = Uindem_pd
     C*    Total paid medical
     C                   eval(h)   sqpdmd = Umed_pd
     C*    Total claimants attorney fees
     C                   eval(h)   sqclaf = Uclmnt_att
     C*    Total employee attorney fees
     C                   eval(h)   sqemaf = Uemp_att
     C*    Total ALAE paid
     C                   eval(h)   sqtalp = Ualae_pd
     C*    Total ALAE incurred
     C                   eval(h)   sqtali = Ualae_inc
     C*
     C                   eval      sqaswg = ASWGflag
     C*
     C*  Maintenance fields
     C*
     C                   eval      squser = q1user
     C                   eval      sqpgm = q1pgm
     C                   eval      sqtime = dstime
     C                   eval      sqdt = dtCYMD(dsdate)
     C                   eval      sqidt = sqdt
     C                   eval      sqiusr = squser
     C*
     C                   write     psq01
     C
     C
     C                   else
     C
     C                   add(h)    Uexp_pay      sqepay
     C                   add(h)    Uexp_oth      sqeoth
     C                   add(h)    Usubj_prem    sqsubp
     C                   add(h)    Ustd_prem     sqstdp
     C                   add(h)    totClms       sq#clt
     C                   add(h)    Uindem_inc    sqtind
     C                   add(h)    Umed_inc      sqtmed
     C                   eval      sq#rcd = sq#rcd + U#exposure + U#claims +
     C                                       (U#lnkhdr*3)
     C                   add(h)    Uindem_pd     sqpdin
     C                   add(h)    Umed_pd       sqpdmd
     C                   add(h)    Uclmnt_att    sqclaf
     C                   add(h)    Uemp_att      sqemaf
     C                   add(h)    Ualae_pd      sqtalp
     C                   add(h)    Ualae_inc     sqtali
     C
     C                   update    psq01
     C
     C                   endif                                                  *in70=*off
     C*
     C*  Reset fields used for accumulating unit totals
     C                   eval      Uexp_pay = 0
     C                   eval      Uexp_oth = 0
     C                   eval      Usubj_prem = 0
     C                   eval      Ustd_prem = 0
     C                   eval      U#claims = 0
     C                   eval      U#exposure = 0
     C                   eval      U#lnkhdr = 0
     C                   eval      Uindem_inc = 0
     C                   eval      Umed_inc = 0
     C                   eval      Uindem_pd = 0
     C                   eval      Umed_pd = 0
     C                   eval      Uclmnt_att = 0
     C                   eval      Uemp_att = 0
     C                   eval      Ualae_pd = 0
     C                   eval      Ualae_inc = 0
     C                   eval      totClms = 0
     C
     C                   endsr                                                  unitTotal



     C*==========================================================================
     C*  *inzsr - performs all preprocessing necessary to begin extractions
     C*==========================================================================
     C     *inzsr        begsr
     C
     C                   eval      curLvl = 1
     C
     C                   time                    dsdttm
     C*
     C*    Move entry parms into numeric fields
     C                   move      @@fnrrn       ##fnrrn           9 0
     C                   move      @@rpmy        ##rpmy            5 0
     C                   move      @@rptlvls     ##rptlvls         2 0
     C*
     C                   eval       kkrpmy = ##rpmy
     C*
     C*    Get fund information and set keys
     C     ##fnrrn       chain     pfn01                              60
     C                   z-add     fnco#         kkco#
     C                   z-add     fnfnd         kkfnd
     C*
     c*Chain Admin/Darts Control Definition file
     C     elky1         chain     pex01
     C*    Change reporting mth/yr parm to *ISO date field (e.g. ccyy/mm/01)
     C                   eval      RptDate = cvttoDate(dt6to8(dt6MDYY(##rpmy)))
     C*
     C*    Set the first and last day of month for 1st reporting level mth/yr
     C     Rptdate       Subdur    18:*months    fromDt
     C                   eval      toDate = getLastDay(fromDt)
     C*
     C*    Set the date corresponding to last submission of data
     C     toDate        Adddur    6:*months     lastSubDt
     C*
     C*    Convert fromDt to numeric for key field used in chain
     C     *cymd         move      fromDt        kkpfdt
     C*
     C*  Set the monthend date to use with the clm pmt history file
     C*
     C                   eval      mthenddt = getLastDay(rptdate)
     C     *cymd         move      mthenddt      kkMthDt
     C                   eval      lastsubdt = getLastDay(lastsubdt)
     C     *cymd         move      lastSubDt     kksubdt
     C*
     C                   endsr                                                  *inzsr




     C*==========================================================================
     C*  srdriver - this subroutine drives the process to extract data for each
     C*             reporting level; calls "mainloop" subroutine to get policies
     C*==========================================================================
     C     srdriver      begsr
     C
     C                   If        (xxmode = ' ') or (xxmode='F'and FNTEST<>'Y')
     C*  Process for reporting levels requested
     C                   dow       curLvl <= ##rptlvls
     C
     C*  If the PEO flag = 'Y', determine if the pol eff dt for emp# 1 falls within
     C*  the reporting period.  If so, process all employers for this group.
     C
     C                   if        fnpeor = 'Y'
     C
     C                   eval      kkemp# = 1
     C     emky          chain     pel03                              60
     C                   dow       *in60 = *off
     C                   if        cvtToDate(el3pfdt) >= fromdt
     C                             and cvtToDate(el3pfdt) <= toDate
     C                   eval      svfyr = el3fyr
     C                   eval      svpfdt = el3pfdt
     C                   leave
     C                   endif
     C     emky          reade     pel03                                  60
     C                   enddo
     C
     C  n60              exsr      mainloopG
     C
     C                   else
     C
     C                   exsr      mainloop
     C
     C                   endif
     C
     C*      Get ready for the next reporting level
     C                   add       1             curLvl
     C*
     C*      Set fromDt and toDate for next reporting level
     C                   Subdur    1:*years      fromDt
     C                   Subdur    1:*years      toDate
     C*
     C*      Convert Fromdt to numeric for key field used in chain
     C     *cymd         move      fromDt        kkpfdt
     C*
     C                   enddo                                                  curLvl<=##rptlvls

     C                   if        fnpeor = 'Y'
     C                   exsr      srGMerge
     C                   endif

     C*    Combine exposure records that do not have change in mod or rate
     C                   exsr      srCombine

     C*    Check each header rec for corresponding exposure

     C*    Check each header rec for corresponding exposure
     C                   exsr      chkExpo

     C*    Check each header rec on subsequent reports for loss record
     C                   exsr      chkSubsqnt
     C
     C*    Call the state pgm to process state specific requirements.
     C*
     C                   move      fnco#         @@co#
     C                   move      fnfnd         @@fnd
     C                   call      'W7351RST'
     C                   parm                    @@co#
     C                   parm                    @@fnd
     C                   parm                    @@rpmy
     C
     C*    Reset extraction flag in extraction file
     C     ssky          chain     pss01                              60
     C                   eval      ssxtrc = 'N'
     C  N60              update    pss01
     C
     C                   else
     C     ssky          chain     pss01                              60
     C                   if        *in60 = *off
     C                   delete    pss01
     C                   Endif
     C                   Endif
     C
     C                   eval      *inlr = *on
     C                   return
     C
     C                   endsr                                                  srdriver



     C*==========================================================================
     C*  srPrevRpt - this subroutine determines whether it is appropriate to
     C*              report a claim based on previous reporting.  Set the
     C*              'rptLoss' flag *on if claim is to be reported.
     C*==========================================================================
     C     srPrevRpt     begsr
     C
     C*   claim history file
     C     chky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kksubdt
     C
     C                   eval      prevOpen = *off
     C                   eval      prevClsd = *off
     C                   eval      prevNone = *off
     c                   move      *blank        xxocst            1
     C     chky2         setgt     pch01
     C     chky          readpe    pch01                                  60
     c                   move      chocst        xxocst
     C
     C*  Chain to claim history to get previous status
     C     chky1         setgt     pch01
     C     chky          readpe    pch01                                  71
     C                   if        *in71 = *off
     C
     C*      report claim if previously open
     C                   if         chocst = 'O'
     C                   eval       prevOpen = *on
     C                   eval       rptLoss = *on
     C                   else
     C
     C*      report claim if previously closed but open now
     C                   if         chocst = 'C'
     C                   eval        prevClsd = *on
     c                   if         xxocst = 'O'
     C                   eval        rptLoss = *on
     C                   endif                                                  cmstat='O'
     C                   endif                                                  chocst='C'
     C                   endif                                                  chocst='O'
     C
     C*      report claim if previously not reported
     C                   else
     C                   eval       prevNone = *on
     C                   eval       rptLoss = *on
     C                   endif                                                  *in71=*on
     C
     C                   if         juris_st = 'NJ'
     C                   eval       rptLoss = *off
     C                   endif
     C
     C                   if        cmefdt <= kkSubDt
     C*
     C                   if        cmcstf <> 0
     C                   eval      prvIndPd = cmcstf
     C                   if        chocst = 'C'
     C                   eval      prvIndInc = cmcstf
     C                   endif
     C                   endif
     C*
     C                   if        cmcstf <> 0
     C                   eval      prvMedPd  = cmmstf
     C                   if        chocst = 'C'
     C                   eval      prvMedInc = cmmstf
     C                   endif
     C                   endif
     C*
     C                   endif
     C                   if        prvIndPd <> indempay
     C                             or prvMedPd <> medicPay
     C                             or prvExpPd <> allocPay
     C                             and juris_st <> 'NJ'
     C                             or prvIndInc <> indemInc
     C                             or prvMedInc <> medicInc
     C                             or prvExpInc <> allocInc
     C                             and juris_st <> 'NJ'
     C                   eval       rptLoss = *on
     C                   endif
     C
     c*
     c* If claim is currently open then report it.
     c*
     c                   if         xxocst = 'O'
     C                   eval        rptLoss = *on
     C                   endif                                                  cmstat='O'
     C                   endsr                                                  srPrevRpt



     C*==========================================================================
     C*  srPrevInc - claculates what should have been reported in the previous
     C*              submission for incurred indemnity/medical.  Set the 'rptLoss'
     C*              flag *on if the claim is to be reported.
     C*==========================================================================
     C*   claim history file
     C*  Chain to reserve history to calc previous balances
     C*    Loop through all reserve history entries in record
     C*
     C*      Accumulate indemnity incurred
     C*
     C*      Accumulate medical incurred
     C*
     C*      Accumulate expense incurred



     C*==========================================================================
     C*  srPrevPd - calculates what should have been reported in the previous
     C*             submission for paid indemnity/medical.  Set the 'rptLoss'
     C*             flag *on if the claim is to be reported.
     C*==========================================================================
     C*   payment detail file
     C*  Do not include deduct recoveries in the paid total.
     C*
     C*
     C*
     C*  Calculate the previous claimant and employer attorney fees.
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
     C*



     C*==========================================================================
     C*  srWrtPrv - writes a 'P' update type loss record
     C*==========================================================================
     C     srWrtPrv      begsr
     C
     C                   eval(h)   xxprvrcd = *off
     C                   exsr      srCHkPrv
     C                   if        xxprvrcd = *off
     C                   eval      slemp# = link_emp#
     C                   eval      slfyr  = link_fyr
     C                   eval(h)   slindm = prvIndInc
     C                   eval(h)   slmedi = prvMedInc
     C                   eval(h)   slpdin = prvIndPd
     C                   eval(h)   slpdmd = prvMedPd
     C                   eval(h)   slalai = prvExpInc
     C                   eval(h)   slalap = prvExpPd
     C
     C                   eval      slclas = cmclas
     C                   eval      sl#clm = 1
     C                   eval      slacdt = cmacdt
     C
     C*                 claim number
     C                   movel     cmcase        xxnum            80
     C                   move      *blanks       edtmsk           10
     C                   call      'SA020R'
     C                   parm                    xxnum
     C                   parm                    edtmsk
     C*
     C                   movel(p)  xxnum         xxcase
     C                   movel     claim#        slclm#
     C*

      /free
       If @@clt# = '129' and CMpds# <> *blanks or
          @@clt# = '139' and CMpds# <> *blanks or
          FNgrpasc = 'OPTACOMP' and CMpds# <> *blanks;
      /end-free
     C                   movel(p)  cmpds#        slclm#
     C*
     C                   endif
      /free
         If (@@clt#  = '114' Or @@clt# = '152') and
            FNgrpasc = 'OPTACOMP' and CMpds# <> *blanks;
            Exsr ClrHypClm;
            SLclm# = @@tempClm#;
         Endif;
      /End-free
     C*
     C                   if        juris_st = 'CA'
     C                   movea     slclm#        xxclm#
     C                   do        12            i
     C                   if        xxclm#(i) <> '0' and xxclm#(i) <> ' '
     C                   leave
     C                   endif
     C                   if        xxclm#(i) = '0'
     C                   movel     ' '           xxclm#(i)
     C                   endif
     C                   enddo
     C                   movea     xxclm#        slclm#
     C                   endif
     C*
     C*
     C                   if        (@@clt# = '136' Or @@clt# = '150')
     C                             and cmpds# <> *blanks
     C                   movel(p)  CMpds#        slclm#
     C                   endif
     C
     C
     C*    Determine claim status
     C*
     C*      Look at history records for closed
     C     chky1         setgt     pch01
     C     chky          readpe    pch01                                  60
     C*
     C                   if        chocst = 'O'
     C                   eval       slclst = 0                                   Open claim
     C     chky          readpe    pch01                                  60
     C                   if        chocst = 'C'
     C                             and juris_st <> 'DE'
     C                             and juris_st <> 'NJ'
     C                             and juris_st <> 'PA'
     C                             and juris_st <> 'MA'
     C                             and juris_st <> 'CA'
     C                             and juris_st <> 'NC'
     C                   eval       slclst = 2                                   Reopened claim
     C                   endif
     C*
     C                   if        cmjrst = 'MI' and xxprevpd = 0
     C                   eval       slclst = 4                                   pmt not made
     C                   endif
     C*
     C                   else
     C                   eval       slclst = 1                                   closed claim
     C                   endif
     C
     C                   move      cmncci        slinj#
     C                   if        slindm = 0
     C                   eval      slinj# = 06
     C                   endif
     C
     C                   if        juris_st <>  'MN'
     C                             and juris_st <> 'CA'
     C                   move      cmss#         slss#
     C                   endif
     C                   eval      slupdt = 'P'
     C
     C                   if        ASWGflag <> 'Y'
     C                   eval      slprvr = 1
     C                   move      cmlscv        sllos#
     C                   endif
     C
     C                   z-add     prvcattfe     slcatf                         clmnt atty fees
     C                   z-add     prveattfe     sleatf                         emp atty fees
     C
     C*    Catastrophe number
     C*
     C                   If        CMnatr = '83' And CMcaus = '83' And
     C                             CMacDt > 1191201
     C                   Eval      SLcat# = 12
     C                   Else
     C                   move      cmcata        slcat#
     C                   EndIf
     C*
     C     llky          chain     pll01                              60
     C*
     C*
     C                   if        *in60=*off
     C*
     C                   if        lllump = 'Y'
     C                   eval       sllump = 'Y'                                Lump sum indicator
     C                   else
     C                   eval       sllump = 'N'
     C                   endif
     C*
     C                   movel     llmtst        sllc5
     C                   move      llfrau        slfrau                         Fraud indicator
     C*
     C                   else
     C*
     C                   eval       sllump = 'N'
     C                   eval       sllc5 = 0
     C                   eval       slfrau = 0
     C*
     C                   endif                                                  *in60=*off
     C*
     C*    Loss conditions
     C                   eval       kktbid = CMJRST + 'LC'
     C                   movel(p)  cmlscv        kkkey
     C     gtky          chain     pgt01                              60
     C                   if        *in60 = *on
     C                   eval       kktbid = '  LC'
     C     gtky          chain     pgt01                              60
     C                   endif
     C*
     C                   if         *in60=*off
     C                   move      Act           sllc1
     C                   move      Typ_los       sllc2
     C                   endif                                                  *in60=*off
     C*
     C*      If code is blank, default this value to '01'
     C                   if        cmlsrv = *blank
     C                   move      '01'          sllc3                           default
     C                   else
     C                   move      cmlsrv        sllc3
     C                   endif                                                  cmlsrv=*blank
     C*
     C*      If code is blank, default this value to '01'
     C                   if        cmcovt = *blank
     C                   move      '01'          sllc4                           default
     C                   else
     C                   move      cmcovt        sllc4
     C                   endif                                                  cmcovt=*blank
     C
     C*
     C*    Jurisdiction state (from claim)
     C                   movel(p)  cmjrst        kkkey
     C     stky          chain     pst01                              60
     C  N60              move      stnst         sljrst
     C*
     C*    Managed care organization type
     C                   move      cmmctp        slmctp
     C*
     C*    Injury description code
     C                   movel(p)  cmpart        kkkey
     C     tbky          chain     ppa01                              60
     C                   movel(p)  paedip        slpart
     C                   movel(p)  cmnatr        kkkey
     C     tbky          chain     pna01                              60
     C                   move      naedin        slnatr
     C                   movel(p)  cmcaus        kkkey
     C     tbky          chain     pca01                              60
     C                   move      caedic        slcaus
     C*
     C*    Occupational description
     C                   eval       sloccu = *blank
     C                   if        cmocds <> *blank
     C                   movel     cmocds        sloccu
     C*
     C                   else
     C                   if        cmoccd <> *blank
     C                   movel(p)  cmoccd        kkkey
     C     ocky          chain     poc01                              60
     C*
     C                   if        *in60 = *off
     C                   movel     ocdesc        sloccu
     C                   endif                                                  *in60=*off
     C*
     C                   else
     C                   eval      kkclas = cmclas
     C                   eval      kkrtcd = el2rtcd
     C     EPKYL         CHAIN     PEP01                              63
     C                   Z-ADD     0             KKEFD1
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   KKCLAS        ##CLAS            5
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
     C                   MOVE      ##DATE        KKEFD1
     C     cmext         ifne      ' '
     C     pxky          setgt     ppx01
     C     pxky1         readpe    ppx01                                  61
     C     *in61         ifeq      *off
     C                   movel(p)  pxdesc        sloccu
     C                   endif                                                  *in61=*off
     C                   else
     C     prky          setgt     ppr01
     C     prky1         readpe    ppr01                                  60
     C                   if        *in60 = *off
     C                   movel(p)  prdesc        sloccu
     C                   endif                                                  *in60=*off
     C                   endif                                                  cmext<>' '
     C*
     C                   endif                                                  cmoccd<>*blank
     C*
     C                   endif                                                  cmocds<>*blank
     C*
     C*    Vocational description
     C                   if        cmvorc = 'Y'
     C                   eval       slvocr = 'Y'
     C                   else
     C                   eval       slvocr = 'N'
     C                   endif
     C*
     C                   if        ddtype = 'G'
     c                             and xxlrgd <> 'Y'
     c                             or xxlrgd = 'Y'
     c                             and ldtyp1 = 'G'
     C                   eval       sldedr = 0
     C
     C                   else
     C                   eval(h)   sldedr = prvdedInd + prvdedMed +
     C                                         prvdedExp + prvdedLeg
     C                   endif
     C
     C                   eval       slaww  = cmaww
     C                   eval       slvrhb = prvVocReh
     C                   eval       slipct = cmipct
     C
     C                   eval      sluser = q1user
     C                   eval      sliusr = q1user
     C                   eval      slpgm = q1pgm
     C                   eval      sltime = dstime
     C                   eval      sldt = dtCYMD(dsdate)
     C                   eval      slidt = sldt
     C*
     C                   eval       slaswg = ASWGflag
     C*
     C                   write     psl01
     C                   endif                                                  xxprvrcd = *off
     C*
     C*  Increment # loss records counter
     C                   add       1             U#claims
     C
     C                   endsr                                                  srWrtPrv

     C*==========================================================================
     C*  ded_rec - calculates deductible recovery amount for a claim
     C*==========================================================================
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
     C*
     C*  Calculate the claimant and employer attorney fees.
     C*
     C*
     C*
     C*

     C*==========================================================================
     C*  srRsvHst - Calculates the total incurred amounts, both "Current" and
     C*             "Previous".
     C*==========================================================================
     C     srRsvHst      begsr
     C*

     C*   claim history file
     C     rhky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase

     C                   eval      indemInc = 0
     C                   eval      medicInc = 0
     C                   eval      allocInc = 0
     C                   eval      prvIndInc = 0
     C                   eval      prvMedInc = 0
     C                   eval      prvExpInc = 0
     C                   eval      vocRehab = 0
     C                   eval      prvVocReh = 0
     C                   eval      xxprevcnt = 0
     C
     C*  Calculate total incurred using the Reserve History File
     C
     C     rhky          chain     prh01                              71
     C                   dow       *in71 = *off
     C                   if        (rhefdt <= kkMthDt and cltid <> '139')
     C                             or (rhidt <= kkMthDt and cltid = '139')
     C
     C*  Don't include entries for deductible recoveries.
     C
     C
     C*    Loop through all reserve history entries in record
     C
     C                   do        10            i
     C
     C                   if        rhamt(i) <> 0
     C                   movel(p)  rhcod(i)      kkkey
     C     tbky          chain     prt01                              60
     C
     C*      "Current" amounts.
     C
     C                   select
     C                   when      rttype = 'C'
     C                   add       rhamt(i)      indemInc
     C                   when      rttype = 'M'
     C                   add       rhamt(i)      medicInc
     C                   when      rttype = 'E' or rttype = 'L'
     C                   If        Rtctgy <> '1B'
     C                   add       rhamt(i)      allocInc
     C                   else
     C                   add       rhamt(i)      indemInc
     C                   endif
     C                   endsl
     C
     C                   if        rtctgy = 'RE'
     C                   eval      vocRehab = vocRehab + rhamt(i)
     C                   endif
     C
     C*      "Previous" amounts.
     C
     C                   if        rhefdt <= kkSubDt
     C
     C                   select
     C                   when      rttype = 'C'
     C                   add       rhamt(i)      prvIndInc
     C                   when      rttype = 'M'
     C                   add       rhamt(i)      prvMedInc
     C                   when      rttype = 'E' or rttype = 'L'
     C                   If        Rtctgy <> '1B'
     C                   add       rhamt(i)      prvExpInc
     C                   else
     C                   add       rhamt(i)      prvIndInc
     C                   endif
     C                   endsl
     C
     C                   if        rtctgy = 'RE'
     C                   eval      prvVocReh = prvVocReh + rhamt(i)
     C                   endif
     C
     C                   if        (rttype = 'C' or rttype ='M' or
     C                              rttype = 'E' or rttype ='L')  and
     C                             (rhamt(i) <> *zero)
     C                   eval      xxprevcnt = xxprevcnt + 1
     c                   endif
     C                   endif
     C*
     C                   endif
     C                   enddo
     C
     C*
     C                   endif
     C*
     C     rhky          reade     prh01                                  71
     C                   enddo

     C                   endsr

     C*==========================================================================
     C*  srPmtHst - 1. Calculates "previous" and "current" paid amounts, excluding
     C*                deductible recoveries, which gives a gross paid amount.
     C*             2. Calculates "previous" and "current" deductible recovery
     C*                amounts.
     C*             3. Calculates the claimant and employer attorney fees.
     C*==========================================================================
     C     srPmtHst      begsr
     C
     C*   payment detail file
     C     paky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C
     C                   eval      indemPay = 0
     C                   eval      medicPay = 0
     C                   eval      allocPay = 0
     C                   eval      prvIndPd = 0
     C                   eval      prvMedPd = 0
     C                   eval      prvExpPd = 0
     C                   eval      dedIndRec = 0
     C                   eval      dedMedRec = 0
     C                   eval      dedExpRec = 0
     C                   eval      prvdedind = 0
     C                   eval      prvdedmed = 0
     C                   eval      prvdedexp = 0
     C                   eval      clmattfee = 0
     C                   eval      empattfee = 0
     C                   eval      prvcattfe = 0
     C                   eval      prveattfe = 0
     C                   eval      MedRecv   = 0
     C                   eval      IndRecv   = 0
     C                   eval      PMedRecv  = 0
     C                   eval      PIndRecv  = 0
     C
     C*  Calculate the previous report monthend date.
     C
     C     paky          chain     pa201                              99
     C                   dow       *in99 = *off
     C                             and patrdt <= kkMthDt
     C
     C                   if        patype = 'V'
     C                             or patype = 'S'
     C                             or patype = 'R'
     C                   eval      paamt = -paamt
     C                   endif
     C
      /Free
       If patype = 'R';
          select;
            When patrcd = 'MR';
               eval MedRecv = MedRecv - paamt;
            When patrcd = 'CR';
               eval IndRecv = IndRecv - paamt;
          endsl;
       Endif;
       kkKey = PAbr2;
       Chain (kkCo#:kkKey) pb201;
       If Not %Found(wtabl);
          B2plCd   = *blank;
          B2exALAE = *blank;
       EndIf;
      /End-free
     C*  Calculate the total paid amounts. Don't include deductible recoveries.
     C
     C                   if        pabr2 <> fnmep1
     C                             and pabr2 <> fncop1
     C                             and pabr2 <> fnexp1
     C                             and pabr2 <> fnlep1
     C                             or  pabr2 = *blanks
     C
      *
      *
     C*      "Current" amounts.
     C
     C                   select
     C                   when      paretp = 'C'
     C                   add       paamt         indemPay
     C                   when      paretp = 'M'
     C                   add       paamt         medicPay
     C                   when      paretp = 'E' or paretp = 'L'
     C                   movel(p)  parecd        kkkey
     C     tbky          chain     prt01                              65
     C                   If        Rtctgy <> '1B' And B2exALAE <> 'Y'
     C                   add       paamt         allocpay
     C                   else
     C                   add       paamt         indemPay
     C                   endif
     C                   endsl
     C
     C*      "Previous" amounts.
     C
     C                   if        patrdt <= kkSubDt
     C
     C                   select
     C                   when      paretp = 'C'
     C                   add       paamt         prvIndPd
     C                   when      paretp = 'M'
     C                   add       paamt         prvMedPd
     C                   when      paretp = 'E' or paretp = 'L'
     C                   movel(p)  parecd        kkkey
     C     tbky          chain     prt01                              65
     C                   If        Rtctgy <> '1B' And B2exALAE <> 'Y'
     C                   add       paamt         prvExpPd
     C                   else
     C                   add       paamt         prvIndPd
     C                   endif
     C                   endsl
     C
      /Free
       If patype = 'R';
          select;
            When patrcd = 'MR';
               eval PMedRecv = PMedRecv - paamt;
            When patrcd = 'CR';
               eval PIndRecv = PIndRecv - paamt;
          endsl;
       Endif;
      /End-free
     C                   endif
     C
     C*  Calculate the previous claimant and employer attorney fees.
     C*
     C
     C*      "Current" amounts.
     C
     C                   select
     C                   when      b2plcd = 'M'
     C                   add       paamt         clmattfee
     C                   when      b2plcd = 'N'
     C                   add       paamt         empattfee
     C                   endsl
     C
     C*      "Previous" amounts.
     C
     C                   if        patrdt <= kkSubDt
     C
     C                   select
     C                   when      b2plcd = 'M'
     C                   add       paamt         prvcattfe
     C                   when      b2plcd = 'N'
     C                   add       paamt         prveattfe
     C                   endsl
     C
     C                   endif
     C
     C                   else
     C*
     C*  Calculate the deductible recovery amounts.
     C*
     C
     C*      "Current" amounts.
     C
     C                   select
     C                   when      paretp = 'C'
     C                   sub       paamt         dedIndRec
     C                   when      paretp = 'M'
     C                   sub       paamt         dedMedRec
     C                   when      paretp = 'E' or paretp = 'L'
     C                   movel(p)  parecd        kkkey
     C     tbky          chain     prt01                              65
     C                   If        Rtctgy <> '1B' And B2exALAE <> 'Y'
     C                   sub       paamt         dedExpRec
     C                   else
     C                   sub       paamt         dedIndRec
     C                   endif
     C                   endsl
     C
     C*      "Previous amounts"
     C
     C                   if        patrdt <= kkSubDt
     C
     C                   select
     C                   when      paretp = 'C'
     C                   sub       paamt         prvdedInd
     C                   when      paretp = 'M'
     C                   sub       paamt         prvdedMed
     C                   when      paretp = 'E' or paretp = 'L'
     C                   movel(p)  parecd        kkkey
     C     tbky          chain     prt01                              65
     C                   If        Rtctgy <> '1B' And B2exALAE <> 'Y'
     C                   sub       paamt         prvdedExp
     C                   else
     C                   sub       paamt         prvdedInd
     C                   endif
     C                   endsl
     C
     C                   endif
     C*
     C                   endif
     C
     C     paky          reade     pa201                                  99
     C                   enddo
     C
     C                   endsr                                                  srPmtHst

     C*==========================================================================
     C*  srMerit - This subroutine processes the merit rating exposure record.
     C*==========================================================================
     C     srMerit       begsr
     C
     C                   if        adtPyrl = *on
     C*** begin add ***  08/28/25  *********************************************
     C     epky          chain     pmp01                              63
     C                   If        xxminf = ' '
     C*** end add *****  08/28/25  *********************************************
     C
     C                   eval      xxmrrt = xxmrrt + el2axp1 + el2axp2 + el2axp3
     C                                      - el2asbj + el2adcs + el2adcm
     C                   if        rrdv1l = 'J'
     C                   eval      xxmrrt = xxmrrt + el2adc2
     C                   eval      xxmrrt = xxmrrt + el2d1a2
     C                   endif
     C                   if        rrdv2l = 'J'
     C                   eval      xxmrrt = xxmrrt + el2adv2
     C                   eval      xxmrrt = xxmrrt + el2d2a2
     C                   endif
     C                   if        el2wvfl = 'J'
     C                   eval      xxmrrt = xxmrrt - el2awav
     C                   endif
     C*** begin add ***  08/28/25  *********************************************
     C                   Else
     C
     C                   If        *in63 = *off
     C                   eval      xxmrrt = xxmrrt + mpaxp1 + mpaxp2 + mpaxp3
     C                                      - mpasbj + mpadcs + mpadcm
     C                   if        rrdv1l = 'J'
     C                   eval      xxmrrt = xxmrrt + mpadc2
     C                   eval      xxmrrt = xxmrrt + mpd1a2
     C                   endif
     C                   if        rrdv2l = 'J'
     C                   eval      xxmrrt = xxmrrt + mpadv2
     C                   eval      xxmrrt = xxmrrt + mpd2a2
     C                   endif
     C                   if        el2wvfl = 'J'
     C                   eval      xxmrrt = xxmrrt - mpawav
     C                   endif
     C
     C                   Endif                                                  *in63 = *off
     C
     C                   Endif                                                  xxminf = ' '
     C*** end add *****  08/28/25  *********************************************
     C
     C                   else
     C
     C*** begin add ***  08/28/25  *********************************************
     C     epky          chain     pmp01                              63
     C                   If        xxminf = ' '
     C*** end add *****  08/28/25  *********************************************
     C                   eval      xxmrrt = xxmrrt + el2exp1 + el2exp2 + el2exp3
     C                                      - el2esbj + el2edcs + el2edcm
     C                   if        rrdv1l = 'J'
     C                   eval      xxmrrt = xxmrrt + el2edc2
     C                   eval      xxmrrt = xxmrrt + el2d1e2
     C                   endif
     C                   if        rrdv2l = 'J'
     C                   eval      xxmrrt = xxmrrt + el2edv2
     C                   eval      xxmrrt = xxmrrt + el2d2e2
     C                   endif
     C                   if        el2wvfl = 'J'
     C                   eval      xxmrrt = xxmrrt - el2ewav
     C                   endif
     C*** begin add ***  08/28/25  *********************************************
     C                   Else
     C
     C                   If        *in63 = *off
     C                   eval      xxmrrt = xxmrrt + mpexp1 + mpexp2 + mpexp3
     C                                      - mpesbj + mpedcs + mpedcm
     C                   if        rrdv1l = 'J'
     C                   eval      xxmrrt = xxmrrt + mpedc2
     C                   eval      xxmrrt = xxmrrt + mpd1e2
     C                   endif
     C                   if        rrdv2l = 'J'
     C                   eval      xxmrrt = xxmrrt + mpedv2
     C                   eval      xxmrrt = xxmrrt + mpd2e2
     C                   endif
     C                   if        el2wvfl = 'J'
     C                   eval      xxmrrt = xxmrrt - mpewav
     C                   endif
     C
     C                   Endif                                                  *in63 = *off
     C
     C                   Endif                                                  xxminf = ' '
     C*** end add *****  08/28/25  *********************************************
     C
     C                   endif
     C
     C                   if        rrasfl = 'J'
     C                   eval      xxmrrt = xxmrrt - xxadld
     C                   endif
     C
     C*  Write exposure record.
     C
     C                   if        xxmrrt > 0
     C                   eval      seclas = rrmrrd
     C                   else
     C                   if        xxmrrt < 0
     C                   eval      seclas = rrmrrc
     C                   eval      xxmrrt = xxmrrt * -1
     C                   else
     C                   eval      seclas = rrmrrn
     C                   endif
     C                   endif
     C
     C                   if        seclas <> 0
     C                   if        juris_st = 'NJ'
     C                   eval      seexpo = 01
     C                   endif

     C                   If        juris_st = 'PA'
     C                             and splt_dt(3) <> 9999999
     C                             or juris_st = 'DE'
     C                             and splt_dt(3) <> 9999999
     C*
     C*  Split merit rating based on premium percentage for each splt_dt.
     C*
     C                   eval       r = 1
     C     9999999       lookup    splt_dt(r)                             70
     C   70              eval       s = r-2
     C  N70              eval       s = 4
     C*
     C                   do        s             t
     C                   if        prem_ttl <> 0
     C                             and prem_tl(t) <> 0
     C                   eval(h)   prem = xxmrrt * (prem_tl(t)/prem_ttl)
     C                   else
     C                   eval      prem = xxmrrt
     C                   endif
     C
     C                   eval      n=1
     C                   do        3             m
     C                   if        splt_dt(t) >= effMod(m)
     C                             and effMod(m) <> 0
     C                   eval       n = m
     C                   endif
     C                   enddo

     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : seclas : 00 : expMod(n) :
     C                               splt_dt(t) : splt_dt(t) : 0 : prem :
     C                               semnrt : tier : seexpo : t-1)

     C                   enddo

     C                   else

     C                   if        juris_st = 'NC'
     C                             or juris_st = 'WI'
     C                             or juris_st = 'MA'
     C                             or juris_st = 'PA'
     C                             or juris_st = 'DE'
     C                             or juris_st = 'NJ'
     C                             or juris_st = 'NY'
     C                   if        semddt = 0
     C                   eval      semddt = EffMod(1)
     C                   endif
     C                   eval      sertdt = rate_dt(1)
     C                   else
     C                   if        semddt = 0
     C                   eval      semddt = EffMod(1)
     C                   endif
     C                   eval      sertdt = rate_dt(1)
     C                   endif

     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : seclas : 00 : 0 :
     C                               semddt : sertdt : 0 : xxmrrt : 0 : 0 :
     C                               seexpo : 0)

     C                   endif

     C                   endif
     C
     C                   endsr
     C*==========================================================================


     C*==========================================================================
     C*  SR_ICR    - This subroutine processes ICR data.                    d.
     C*==========================================================================
     C     SR_ICR        BegSR


     C                   Exsr      InzIcrDta

     C                   if        curLvl = 1
     C                             or cmicr <> 'Y'
     C                   move      '1'           sittyp                                   ICR7I-16
     C                   else
     C                   move      '2'           sittyp                                   ICR7I-16
     C                   EndIf

     C                   move      CMCO#         SICO#
     C                   move      CMFND         SIFND
     C                   move      KKRPMY        SIRPMY
     C                   move      CMEMP#        SIEMP#
     C                   move      CMFYR         SIFYR
     C                   move      STCODE        SISTCD
     C                   move      CMCASE        SICLM#
      /free
       If FNgrpasc = 'OPTACOMP' and CMpds# <> *blanks;
          siCLM# = CMpds#;
       Endif;
         If (@@clt#  = '114' Or @@clt# = '152') and
            FNgrpasc = 'OPTACOMP' and CMpds# <> *blanks;
            Exsr ClrHypClm;
            siCLM# = @@tempClm#;
         Endif;
      /end-free
     C                   eval      sirpt# = link_rpt#

     c*  Move data from WMCML to WMSIL, unconditionally.
     c*
     C                   move      cmrsvt        sirtyp                         ICR7A-11
     C                   move      cmyrlx        siyrex                         ICR7A-12
     C                   move      cmdod         sidod                          ICR7A-21
     C                   move      cmdtrp        sidtrp                         ICR7A-22
     C                   move      cmipct        sisds1                         ICR7B-16
     C                   movel     cmlsn         silsn                          ICR7F-12
     C                   movel     cmocds        sioccp                         ICR7F-13
     C                   move      cmss#         siss#                          ICR7F-14
     C                   move      cmhidt        sidoh                          ICR7H-11
     C                   movel     srname        sinmin                         ICR7E-27
     C*
     C                   eval       siemst = '01'
     C                   eval       kktbid = 'EMST'
     C                   movel(p)  cmests        kkkey
     C     gtky          chain     pgt01                              60
     C  n60              movel     gtcf01        siemst                         ICR7H-12
      *
     C                   If        slpart = 13 or slpart = 14
     C                             or slpart = 16  or slpart = 31
     C                             or slpart = 33  or slpart = 35
     C                             or slpart = 36  or slpart = 37
     C                             or slpart = 54  or slpart = 56
     C                             or slpart = 57  or slpart = 70
     C                   move      cmipct        sisds1                         ICR7B-16
     C                   EndIf

      *
     c*  Move data from WMCML to WMSIL, Conditionally.
     C                   If        cmsgdt <> 0
     C                   move      '1'           sisurg                                   ICR7I-16
     C                   else
     C                   move      '2'           sisurg                                   ICR7I-16
     C                   EndIf
      *

     c*  Total Gross Incurred.
     C                   Eval      SIGRSS = CMRR01+CMRR02+CMRR03+CMRR04+CMRR05 +          ICR7G-14
     C                                      CMRR06+CMRR07+CMRR08+CMRR09+CMRR10 +
     C                                      CMAR01+CMAR02+CMAR03+CMAR04+CMAR05 +
     C                                      CMAR06+CMAR07+CMAR08+CMAR09+CMAR10

     c*  Claim Closed Date, using last record.
     C                   IF        cmstat = 'C'
     C     chky1         setgt     pch01
     C     chky          readpe    pch01                                  60

     C                   If        *in60 = *off and chocst = 'C'
     C                   move      CHEFDT        xxdate6           6 0
     C                   movel     xxdate6       SIDTCL                         ICR7A-25
     C                   EndIf

     C                   EndIF


     C*  Move data from WMLLL to WMSIL.
     C     llky          setll     pll01
     C     llky          reade     pll01                                  60
     C                   If        *in60 = *off
     C                   movel     llmtst        simtst                         ICR7A-28

     C                   if        LLPLAA <> 0
     C                   eval      SIATTY = '2'                                 ICR7I-17
     C                   else
     C                   eval      SIATTY = '3'                                 ICR7I-17
     C                   endif

     C                   EndIf

     C*  Incurred Cost of Temp Indemnity Total.
     C
     C     rhky          setll     prh01
     C     rhky          reade     prh01                                  71
     C                   dow       *in71 = *off
     C                   if        (rhefdt <= kkMthDt and cltid <> '139')
     C                             or (rhidt <= kkMthDt and cltid = '139')

     C*  Loop through all reserve history entries in record

     C                   do        10            i

     C                   if        rhamt(i) <> 0
     C                   movel(p)  rhcod(i)      kkkey
     C     tbky          chain     prt01                              60

     C                   select
     C                   when      rttype = 'C'
     C                   add       rhamt(i)      xxINCT                         ICR7B-15
     C                   add       rhamt(i)      xxINCI                         ICR7D-11
     C                   when      rtctgy = 'RE'
     C                   add       rhamt(i)      xxVRHB                         ICR7C-14
     C                   when      rttype = 'M'
     C                   add       rhamt(i)      xxINCM                         ICR7D-12
     C                   endsl
     C                   endif

     C                   enddo
     C                   endif

     C     rhky          reade     prh01                                  71
     C                   EndDo


     C* Move PAAMT From WDPAP in case Res Type is 'M' and Payee Type is '4'.

     C     paky          setll     pa201
     C     paky          reade     pa201                                  99
     C                   DoW       *in99 = *off
     C                             and patrdt <= kkMthDt

     C                   if        patype = 'V'
     C                             or patype = 'S'
     C                             or patype = 'R'
     C                   eval      paamt = -paamt
     C                   endif

     C                   move      PAFEIN        kkfein            9 0
     C                   move      PAMOD         kkmod             3 0

     C     pdky          setll     ppd01
     C     pdky          reade     ppd01                                  77
     C                   If        *IN77  = *OFF
     C                   select
     C                   When      PARETP = 'M' and PDTYPE = '4'
     C                   Add       PAAMT         xxHOSP                         ICR7G-11
     C                   When      PARETP = 'M' and PDTYPE = '1'
     C                   Add       PAAMT         xxPHPD                         ICR7I-12
     C                   endsl
     C                   endif
      *
     C                   select
     C                   When      PARETP = 'M'
     C                   Add       PAAMT         xxPDMD                         ICR7D-14
     C                   When      PARETP = 'C'
     C                   Add       PAAMT         xxPDIN                         ICR7D-13
     C                   endsl

      *
     C                   movel(p)  pabr2         kkkey
     C     tbky          chain     pb201                              99
     C   99              eval      b2plcd = *blank
     C
     C*      "Current" amounts.
     C
     C                   select
     C                   when      b2plcd = 'M'
     C                   add       paamt         xxLEGC                         ICR7G-13
     C                   when      b2plcd = 'N'
     C                   add       paamt         xxLEGD                         ICR7G-12

     C                   when      b2plcd ='1' OR b2plcd ='2' OR b2plcd ='7'
     C                               OR b2plcd ='3'
     C                   add       paamt         xxDISP                         ICR7H-13
     C                   when      b2plcd = 'T'
     C                   add       paamt         xxPTPD                         ICR7H-14
     C                   when      b2plcd = 'I'
     C                   add       paamt         xxRHBP                         ICR7H-15
     C                   when      b2plcd ='4' OR b2plcd ='5'
     C                               OR b2plcd ='9'
     C                   add       paamt         xxPRMP                         ICR7H-16
     C                   when      b2plcd ='6'
     C                   add       paamt         xxDTHP                         ICR7H-17
     C                   when      b2plcd = 'L'
     C                   add       paamt         xxSTLA                         ICR7I-11
     C                   move      patrdt        SISUMD                         ICR7J-13

     C                   endsl

     C                   select
     C                   when      b2minr = '201'
     C                   add       paamt         xxVIND                         ICR7G-15
     C                   when      b2minr = '202'
     C                   add       paamt         xxVTRN                         ICR7G-16
     C                   when      b2minr = '062'
     C                   add       paamt         xxVEVL                         ICR7G-17
     C                   when      b2minr = '087'
     C                   add       paamt         xxSSAM                         ICR7D-15

     C                   endsl

     C     paky          reade     pa201                                  99

     C                   EndDo


     C*    Birth Date from WMCDL.
     C     Cdky          Chain     PCD01                              55
     C                   If        *in55 = *off
     C                   move      CDDTOB        SIDOB                          ICR7A-23
     C                   movel     cdsex         sisex                          ICR7A-19
     C                   EndIf

     C*    Move Beneficiary Relationship Code WDACL to WDSIP.
     C                   z-add     1             i
     C                   z-add     1             kkseq#            5 0

     C     acky          chain     pac01                              88
     C                   Dow       *in88=*off and (i <= 5)
     C                   IF        ACREL <> *blanks
     C                   move      acrel         sibcr(i)                       ICR7E (1,2,3,4,5)
     C                   move      acdpcy        sibcd(i)
     C                   move      acddob        sibbd(i)
     C                   eval      i  = i + 1
     C                   EndIF
     C                   eval      kkseq# = kkseq# + 1

     C     acky          reade     pac01                                  88

     C                   EndDo

     C                   eval(h)   siinct = xxINCT                              ICR7D-11
     C                   eval(h)   siinci = xxINCI                              ICR7D-11
     C                   eval(h)   sivrhb = xxVRHB                              ICR7C-14
     C                   eval(h)   siincm = xxINCM                              ICR7D-12
     C                   eval(h)   sihosp = xxHOSP                              ICR7G-11
     C                   eval(h)   siphpd = xxPHPD                              ICR7I-12
     C                   eval(h)   sipdmd = xxPDMD                              ICR7D-14
     C                   eval(h)   sipdin = xxPDIN                              ICR7D-13
     C                   eval(h)   silegc = xxLEGC                              ICR7G-13
     C                   eval(h)   silegd = xxLEGD                              ICR7G-12
     C                   eval(h)   sidisp = xxDISP                              ICR7H-13
     C                   eval(h)   siptpd = xxPTPD                              ICR7H-14
     C                   eval(h)   sirhbp = xxRHBP                              ICR7H-15
     C                   eval(h)   siprmp = xxPRMP                              ICR7H-16
     C                   eval(h)   sidthp = xxDTHP                              ICR7H-17
     C                   eval(h)   sistla = xxSTLA                              ICR7I-11
     C                   eval(h)   sivind = xxVIND                              ICR7G-15
     C                   eval(h)   sivtrn = xxVTRN                              ICR7G-16
     C                   eval(h)   sivevl = xxVEVL                              ICR7G-17
     C                   eval(h)   sissam = xxSSAM                              ICR7D-15
     C*
     C                   move      cmwktb        siwktb
     C                   move      cmsds1        sisds1
     C                   move      cmsbm1        sisbm1
     C                   move      cmswk1        siswk1
     C                   move      cmsls1        sisls1
     C                   move      cmsds2        sisds2
     C                   move      cmsbm2        sisbm2
     C                   move      cmswk2        siswk2
     C                   move      cmsls2        sisls2
     C                   move      cmnsds        sinsds
     C                   move      cmnsls        sinsls
     C                   move      cmliab        siliab
     C                   move      cmpib         sipib
     C                   move      cmpvfp        sipvfp
     C                   move      cmfnrl        sifnrl
     C                   move      cmrmrg        sirmrg
     C                   move      cmppr         sippr
     C                   move      cmatdt        siatdt
     C                   move      cmannu        siannu
     C*
     C                   if        juris_st = 'NJ'
     C*
     C                   eval      siinci = siinct + sisls1 + sisls2 + sinsls
     C                                      + siliab + sivrhb + silegc + sipib
     C                                      + sippr + sipvfp + sifnrl + sirmrg
     C*
     C                   clear                   simetd
     C                   clear                   sidmdp
     C                   clear                   siimdp
     C                   clear                   siannu
     C                   clear                   sigrss
     C                   clear                   siptpd
     C                   clear                   siprmp
     C                   clear                   sidthp
     C                   clear                   sistla
     C                   clear                   sivind
     C                   clear                   sivtrn
     C                   clear                   sivevl
     C*
     C                   Endif

     C                   Write     PSI01                                        WDSIL

     C     paky          chain     pcm02                              99
     C                   If        *IN99  = *OFF
     C                   move      'Y'           CMICR
     C                   update    pcm02
     C                   Endif

     C                   EndSr                                                  BldIcrDta
     C*==========================================================================
     C*==========================================================================
     C*  srChkPrv  -
     C*==========================================================================
     C     srChkPrv      BegSR
     C*
     C*    Load claim number
     C                   movel     cmcase        xxnum            80
     C                   move      *blanks       edtmsk           10
     C                   call      'SA020R'
     C                   parm                    xxnum
     C                   parm                    edtmsk
     C*
     C                   movel(p)  xxnum         xxcase
     C                   movel     claim#        slclm#
     C*

      /free
       If @@clt# = '129' and CMpds# <> *blanks or
          @@clt# = '139' and CMpds# <> *blanks or
          FNgrpasc = 'OPTACOMP' and CMpds# <> *blanks;
      /end-free
     C                   movel(p)  cmpds#        slclm#
     C*
     C                   endif
      /free
         If (@@clt#  = '114' Or @@clt# = '152') and
            FNgrpasc = 'OPTACOMP' and CMpds# <> *blanks;
            Exsr ClrHypClm;
            SLclm# = @@tempClm#;
         Endif;
      /End-free
     C*
     C                   if        juris_st = 'CA'
     C                   movea     slclm#        xxclm#
     C                   do        12            i
     C                   if        xxclm#(i) <> '0' and xxclm#(i) <> ' '
     C                   leave
     C                   endif
     C                   if        xxclm#(i) = '0'
     C                   movel     ' '           xxclm#(i)
     C                   endif
     C                   enddo
     C                   movea     xxclm#        slclm#
     C                   endif
     C*
     C                   if        (@@clt# = '136' Or @@clt# = '150')
     C                             and cmpds# <> *blanks
     C                   movel(p)  CMpds#        slclm#
     C                   endif
     C*
     C                   move      slclm#        kkclm#
     C     slky2         setll     psl02
     c     slky3         readpe    psl02                                  99
     C                   dow       *in99 = *off
     C                             and slupdt = 'P'
     C     slky3         readpe    psl02                                  99
     C                   enddo
     C*
     C                   if        *in99 = *off
     C                   eval      slemp# = link_emp#
     C                   eval      slfyr  = link_fyr
     C                   eval      slrpmy = ##rpmy
     C                   eval      slupdt = 'P'
     C                   eval      sluser = q1user
     C                   eval      slpgm = q1pgm
     C                   eval      sltime = dstime
     C                   eval      sldt = dtCYMD(dsdate)
     C                   eval      slidt = sldt
     C                   eval      sliusr = sluser
     C                   write     psl01
     C                   eval      xxprvrcd = *on
     C                   endif
     C*
     C                   EndSr                                                  srChkprv
     C*==========================================================================


     C*==========================================================================
     C* InzIcrDta - Subroutine to initialize outfile fields.
     C*==========================================================================
     C     InzIcrDta     BegSr
     C                   Move      *zeros        SICO#
     C                   Move      *zeros        SIFND
     C                   Move      *zeros        SIRPMY
     C                   Move      *zeros        SIEMP#
     C                   Move      *zeros        SIFYR
     C                   Move      *blanks       SICLM#
     C                   Move      *zeros        SISTCD
     C                   Move      *blanks       SIRPT#
     C                   Move      *blanks       SIRTYP
     C                   Move      *zeros        SIYREX
     C                   Move      *blanks       SILSCV
     C                   Move      *blanks       SITTYP
     C                   Move      *zeros        SIDOD
     C                   Move      *zeros        SIDTRP
     C                   Move      *zeros        SIDOB
     C                   Move      *zeros        SIDTCL
     C                   Move      *blanks       SIMTST
     C                   Move      *zeros        SIINCT
     C                   Move      *zeros        SISDS1
     C                   Move      *blanks       SISBM1
     C                   Move      *zeros        SISWK1
     C                   Move      *zeros        SISLS1
     C                   Move      *zeros        SIWKTB
     C                   Move      *zeros        SISDS2
     C                   Move      *blanks       SISBM2
     C                   Move      *zeros        SISWK2
     C                   Move      *zeros        SISLS2
     C                   Move      *zeros        SINSDS
     C                   Move      *zeros        SINSLS
     C                   Move      *zeros        SILIAB
     C                   Move      *zeros        SIVRHB
     C                   Move      *zeros        SIPIB
     C                   Move      *zeros        SIPVFP
     C                   Move      *zeros        SIFNRL
     C                   Move      *zeros        SIRMRG
     C                   Move      *zeros        SIINCI
     C                   Move      *zeros        SIINCM
     C                   Move      *zeros        SIPDIN
     C                   Move      *zeros        SIPDMD
     C                   Move      *zeros        SISSAM
     C                   Move      *zeros        SIPPR
     C                   Move      *blanks       SIBCR1
     C                   Move      *blanks       SIBCD1
     C                   Move      *zeros        SIBBD1
     C                   Move      *blanks       SIBCR2
     C                   Move      *blanks       SIBCD2
     C                   Move      *zeros        SIBBD2
     C                   Move      *blanks       SIBCR3
     C                   Move      *blanks       SIBCD3
     C                   Move      *zeros        SIBBD3
     C                   Move      *blanks       SIBCR4
     C                   Move      *blanks       SIBCD4
     C                   Move      *zeros        SIBBD4
     C                   Move      *blanks       SIBCR5
     C                   Move      *blanks       SIBCD5
     C                   Move      *zeros        SIBBD5
     C                   Move      *blanks       SINMIN
     C                   Move      *blanks       SINMCR
     C                   Move      *blanks       SILSN
     C                   Move      *blanks       SIOCCP
     C                   Move      *blanks       SISS#
     C                   Move      *zeros        SIHOSP
     C                   Move      *zeros        SILEGD
     C                   Move      *zeros        SILEGC
     C                   Move      *zeros        SIGRSS
     C                   Move      *zeros        SIVIND
     C                   Move      *zeros        SIVTRN
     C                   Move      *zeros        SIVEVL
     C                   Move      *zeros        SIDOH
     C                   Move      *blanks       SIEMST
     C                   Move      *zeros        SIDISP
     C                   Move      *zeros        SIPTPD
     C                   Move      *zeros        SIRHBP
     C                   Move      *zeros        SIPRMP
     C                   Move      *zeros        SIDTHP
     C                   Move      *zeros        SISTLA
     C                   Move      *zeros        SIPHPD
     C                   Move      *zeros        SIMETD
     C                   Move      *zeros        SIDMDP
     C                   Move      *zeros        SIIMDP
     C                   Move      *blanks       SISURG
     C                   Move      *blanks       SIATTY
     C                   Move      *zeros        SIATDT
     C                   Move      *zeros        SIANNU
     C                   Move      *zeros        SISUMD
     C                   EndSr
     C*==========================================================================
     C
     C*==========================================================================
     C*  NYadjust - This subroutine processes the 10/1/07 NY rate decrease.
     C*==========================================================================
     C     NYadjust      begsr
     C
     C                   move      fnco#         @@co#
     C                   move      fnfnd         @@fnd
     C                   move      el2fyr        @@fyr
     C                   move      el2emp#       @@emp#
     C                   move      el2div        @@div
     C
     C                   call      'SRFLAT'      inAdjPl
     C                   move      @@amt         ind_amt
     C                   z-add     ind_amt       NY_amt           11 2
     C
     C***** Update tech info to only write record if amount is not equal to zero*
     C
     C                   if        NY_amt <> 0
     C
     c                   if        NY_amt < 0
     c                   mult      -1            NY_amt
     c                   endif
     C
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : 0994 : 00 : expMod(1) :
     C                             effMod(1) : 0 : 0 : NY_amt : 0 : 0 : 0 : 0)
     C
     C                   eval      prem_ttl = prem_ttl - NY_amt
     C
     C                   endif                                                  NY_amt <> 0
     C
     C                   endsr
     C*==========================================================================
     C*==========================================================================
     C*  srPrmPrdFil - Process premium periods file
     C*==========================================================================
     C     srPrmPrdFil   begsr
     C                   clear                   strtcg
     C* Read records from premium periods file, to process split records for terror and assessments
     C     epky          setll     ppp01
     C     epky          reade     ppp01
     C                   dow       not %eof
     C                   if        ppfrdt >= elpfdt and ppfrdt < elptdt
     C                   move(p)   ppco#         #@co#             3
     C                   move(p)   ppfnd         #@fnd             3
     C                   move(p)   ppfyr         #@fyr             3
     C                   move(p)   ppemp#        #@emp#            9
     C                   move(p)   ppdiv         #@div             5
     C                   move(p)   ppfrdt        xpfrdt            7
     C                   call      'SREFFD'
     C                   parm                    #@co#
     C                   parm                    #@fnd
     C                   parm                    #@fyr
     C                   parm                    #@emp#
     C                   parm                    #@div
     C                   parm                    xpfrdt
     C                   move      xpfrdt        xpprdt            7 0

     C     rrky5         setgt     prr01
     C     rrky1         readpe    prr01                                  60
     C                   select
     C                   when      tcef1 = 0 or tcef1 = ppfrdt
     C                   eval      tcef1 = ppfrdt
     C                   eval      tca1r1 = ppa1rt
     C                   eval      tca1a1 = tca1a1 + pp1aam
     C                   eval      tca2r1 = ppa2rt
     C                   eval      tca2a1 = tca2a1 + pp2aam
     C                   eval      tca3r1 = ppa3rt
     C                   eval      tca3a1 = tca3a1 + pp3aam
     C                   eval      tca4r1 = ppa4rt
     C                   eval      tca4a1 = tca4a1 + pp4aam
     C                   eval      tca5r1 = ppa5rt
     C                   eval      tca5a1 = tca5a1 + pp5aam
     C                   eval      tca6r1 = ppa6rt
     C                   eval      tca6a1 = tca6a1 + pp6aam
     C                   eval      tca7r1 = ppa7rt
     C                   eval      tca7a1 = tca7a1 + pp7aam
     C                   eval      tca1s1 = rr1stc
     C                   eval      tca2s1 = rr2ndc
     C                   eval      tca3s1 = rr3Rdc
     C                   eval      tca4s1 = rr4thc
     C                   eval      tca5s1 = rr5thc
     C                   eval      tca6s1 = rr6thc
     C                   eval      tca7s1 = rr7thc
     C                   eval      tctrs1 = rrterc
     C                   eval      tcdds1 = rrdtrc
     C                   eval      tctrr1 = ppterr
     C                   eval      tcdtr1 = ppdtrr
     C                   if        adtpyrl = *on
     C                   eval      tctra1 = tctra1 + ppater
     C                   eval      tcdta1 = tcdta1 + ppadtr
     C                   else
     C                   eval      tctra1 = tctra1 + ppeter
     C                   eval      tcdta1 = tcdta1 + ppedtr
     C                   endif
     C*
     C                   when      tcef2 = 0 or tcef2 = ppfrdt
     C* Second set
     C                   eval      tcef2 = ppfrdt
     C                   eval      tca1r2 = ppa1rt
     C                   eval      tca1a2 = tca1a2 + pp1aam
     C                   eval      tca2r2 = ppa2rt
     C                   eval      tca2a2 = tca2a2 + pp2aam
     C                   eval      tca3r2 = ppa3rt
     C                   eval      tca3a2 = tca3a2 + pp3aam
     C                   eval      tca4r2 = ppa4rt
     C                   eval      tca4a2 = tca4a2 + pp4aam
     C                   eval      tca5r2 = ppa5rt
     C                   eval      tca5a2 = tca5a2 + pp5aam
     C                   eval      tca6r2 = ppa6rt
     C                   eval      tca6a2 = tca6a2 + pp6aam
     C                   eval      tca7r2 = ppa7rt
     C                   eval      tca7a2 = tca7a2 + pp7aam
     C                   eval      tca1s2 = rr1stc
     C                   eval      tca2s2 = rr2ndc
     C                   eval      tca3s2 = rr3Rdc
     C                   eval      tca4s2 = rr4thc
     C                   eval      tca5s2 = rr5thc
     C                   eval      tca6s2 = rr6thc
     C                   eval      tca7s2 = rr7thc
     C                   eval      tctrs2 = rrterc
     C                   eval      tcdds2 = rrdtrc
     C                   eval      tctrr2 = ppterr
     C                   eval      tcdtr2 = ppdtrr
     C                   if        adtpyrl = *on
     C                   eval      tctra2 = tctra2 + ppater
     C                   eval      tcdta2 = tcdta2 + ppadtr
     C                   else
     C                   eval      tctra2 = tctra2 + ppeter
     C                   eval      tcdta2 = tcdta2 + ppedtr
     C                   endif
     C*
     C                   when      tcef3 = 0 or tcef3 = ppfrdt
     C* Third set
     C                   eval      tcef3 = ppfrdt
     C                   eval      tca1r3 = ppa1rt
     C                   eval      tca1a3 = tca1a3 + pp1aam
     C                   eval      tca2r3 = ppa2rt
     C                   eval      tca2a3 = tca2a3 + pp2aam
     C                   eval      tca3r3 = ppa3rt
     C                   eval      tca3a3 = tca3a3 + pp3aam
     C                   eval      tca4r3 = ppa4rt
     C                   eval      tca4a3 = tca4a3 + pp4aam
     C                   eval      tca5r3 = ppa5rt
     C                   eval      tca5a3 = tca5a3 + pp5aam
     C                   eval      tca6r3 = ppa6rt
     C                   eval      tca6a3 = tca6a3 + pp6aam
     C                   eval      tca7r3 = ppa7rt
     C                   eval      tca7a3 = tca7a3 + pp7aam
     C                   eval      tca1s3 = rr1stc
     C                   eval      tca2s3 = rr2ndc
     C                   eval      tca3s3 = rr3Rdc
     C                   eval      tca4s3 = rr4thc
     C                   eval      tca5s3 = rr5thc
     C                   eval      tca6s3 = rr6thc
     C                   eval      tca7s3 = rr7thc
     C                   eval      tctrs3 = rrterc
     C                   eval      tcdds3 = rrdtrc
     C                   eval      tctrr3 = ppterr
     C                   eval      tcdtr3 = ppdtrr
     C                   if        adtpyrl = *on
     C                   eval      tctra3 = tctra3 + ppater
     C                   eval      tcdta3 = tcdta3 + ppadtr
     C                   else
     C                   eval      tctra3 = tctra3 + ppeter
     C                   eval      tcdta3 = tcdta3 + ppedtr
     C                   endif
     C                   endsl
     C                   endif
     C     epky          reade     ppp01
     C                   enddo
     C     rrky          setgt     prr01
     C     rrky1         readpe    prr01                                  60
     C   60rrky          setgt     prr01
     C   60rrky1         reade     prr01                                  60
     C                   endsr
     C*==========================================================================
     C*  srWrtRtChg   - Report Separate exposure records for Terror and Assessments
     C*==========================================================================
     C     srWrtRtChg    begsr
     C                   if         xxrate1 <> 0
     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : xxMcls1 : 00 : 0 :
     C                               tcef1 : tcef1 : 0 : xxamt1:
     C                               xxrate1: tier : seexpo : 0)
     C                   if        xxCalMod
     C                   eval      xxefdt=tcef1
     C                   eval      xxamt = xxamt1
     C                   exsr      srCalModPrem
     C                   endif
     C                   endif
     C                   if         xxrate2 <> 0
     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : xxMcls2 : 00 : 0 :
     C                               tcef2 : tcef2 : 0 : xxamt2:
     C                               xxrate2: tier : seexpo : 1)
     C                   if        xxCalMod
     C                   eval      xxefdt = tcef2
     C                   eval      xxamt = xxamt2
     C                   exsr      srCalModPrem
     C                   endif
     C                   endif
     C                   if         xxrate3 <> 0
     C                   callp      writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                              el2fyr : stcode : xxMcls3 : 00 : 0 :
     C                               tcef3 : tcef3 : 0 : xxamt3:
     C                               xxrate3: tier : seexpo : 2)
     C                   if        xxCalMod
     C                   eval      xxefdt = tcef3
     C                   eval      xxamt = xxamt3
     C                   exsr      srCalModPrem
     C                   endif
     C                   endif
     C                   endsr
     C*==========================================================================
     C*  srCalModPrem - Calulate modified premium
     C*==========================================================================
     C     srCalModPrem  begsr
     C*
     C                   eval      n=1
     C                   do        3             m
     C                   if        xxefdt >= effMod(m)
     C                             and effMod(m) <> 0
     C                   eval       n = m
     C                   endif
     C                   enddo
     C*
     C                   if        expmod(n) = 0
     C  n21              eval(h)    mod_prem = mod_prem - xxamt
     C   21              eval(h)    mod_prem = mod_prem + xxamt
     C                   else
     C  n21              eval(h)    mod_prem = mod_prem - (expmod(n) * xxamt)
     C   21              eval(h)    mod_prem = mod_prem + (expmod(n) * xxamt)
     C                   endif                                                  expmod(n) = 0
     C                   endsr
     C*==========================================================================
     C*********************************************************************
     c*  srFlatRateAdj - IL Flat rate Adjustment.
     C*********************************************************************
     c     srFlatRateAdj begsr
     c*
     C                   if        adtPyrl = *on
     c                   eval      xxFlrtAdj = el2ampr - xtpred
     C                   else
     c                   eval      xxFlrtAdj = el2mapr - xtpred
     C                   endif
     C*
     c                   eval      xtpred = 0

     c                   if        xxFlrtAdj <> 0
     c                   z-add     1110901       xxSpltDt
     C                   callp     writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
     C                             el2fyr : stcode : xxstcd : 00 : 0 :
     C                             0 : xxSpltDt : 0 :
     C                             xxFlrtAdj : 0 : 0 : 0 : 0)
     c                   endif                                                  xxFlrtAdj

     c                   endsr

      /Free
       //********************************************************************
       //  FlatRateAdjNY - NY Flat rate Adjustment.
       //********************************************************************
       Begsr FlatRateAdjNY;

          If adtPyrl = *on;
             xxFlrtAdjNY = xtpredNY - el2ampr;
          Else;
             xxFlrtAdjNY = xtpredNY - el2mapr;
          Endif;
          xtpredNY = 0;

          If xxFlrtAdjNY <> 0;
             xxSpltDt = 1170410;
             writeExp(fnco# : fnfnd : ##rpmy : el2emp# :
                      el2fyr : stcode : xxstcd : 00 : 0 : 0 :
                      xxSpltDt : 0 : xxFlrtAdjNY : 0 : 0 : 0 : 0);

          Endif;

       Endsr;
      /End-free

      /Free
       Begsr ClrHypClm;
          @@tempClm# = ' ';
          charClm# =  CMpds#;
          dow (%scan('-':charClm#) > 0);
              intPos = %scan('-':charClm#);
              If intPos = 1;
                 frstHypen = 'Y';
              Endif;
              charClm# = %subst(charClm#:1:intPos-1)
               + %subst(charClm#:intPos+1);
          enddo;
          If frstHypen <> 'Y';
             @@tempclm# = %subst(charClm#:2:39);
          Else;
             @@tempClm# = charClm#;
          Endif;
       Endsr;
      /End-free

     **Procedures
     P*===========================================================================
     P* wrtLnkHdr - checks if Header record has already been written
     P*      Parameters - company #        3,0
     P*                   fund             3,0
     P*                   rptg  cyymm      5,0
     P*                   employer #       9,0
     P*                   fund year        3,0
     P*                   state code       2,0
     P*      Returns - *on if no record written, else *off
     P*===========================================================================
     P wrtLnkHdr       b                   export
     D wrtLnkHdr       pi                  like(*in99)
     D   ##co#                        3  0 Const
     D   ##fnd                        3  0 Const
     D   ##rpmy                       5  0 Const
     D   ##emp#                       9  0 Const
     D   ##fyr                        3  0 Const
     D   ##stcd                       2  0 Const
     D
      ***
     C     srky          klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd
     C                   kfld                    ##rpmy
     C                   kfld                    ##emp#
     C                   kfld                    ##fyr
     C                   kfld                    ##stcd
     C*
     C     srky          chain     psr01                              70
     C                   if        *in70 = *off
     C                   return     *off
     C                   else
     C                   return     *on
     C                   endif
     C*

     P wrtLnkHdr       e
     P*===========================================================================



     P*===========================================================================
     P* writeExp - writes exposure record (updates if already written)
     P*      Parameters - company#                 3,0
     P*                   group                    3,0
     P*                   reporting month/year     5,0
     P*                   employer#                9,0
     P*                   year                     3,0
     P*                   state code               2,0
     P*                   class                    5,0
     P*                   exposure coverage code   2,0
     P*                   experience mod           5,4
     P*                   effective dt of mod      7,0
     P*                   effective dt of rate     7,0
     P*                   exposure amt            11,2
     P*                   premium amt             11,2
     P*                   manual rate              5,2
     P*                   rating tier              2,0
     P*                   exposure act             2,0
     P*                   split period             1,0
     P*===========================================================================
     P writeExp        b                   export
     D writeExp        pi
     D   ##co#                        3  0 Const
     D   ##fnd                        3  0 Const
     D   ###rpmy                      5  0 Const
     D   ##emp#                       9  0 Const
     D   ##fyr                        3  0 Const
     D   ##stcd                       2  0 Const
     D   ##clas                       5  0 Const
     D   ##exp#                       2  0 Const
     D   ##expMod                     5  4 Const
     D   ##effMod                     7  0 Const
     D   ##rateDt                     7  0 Const
     D   ##expamt                    11  2 Const
     D   ##prmamt                    11  2 Const
      **** Beg Chg **** 06/17/24 **************************************
     D****##rate                       7  3 Const
     D   ##rate                       8  3 Const
      **** End Chg **** 06/17/24 **************************************
     D   ##tier                       2  0 Const
     D   ##expo                       2  0 Const
     D   ##splt                       1  0 Const

     C     seky          klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd
     C                   kfld                    ###rpmy
     C                   kfld                    ##emp#
     C                   kfld                    ##fyr
     C                   kfld                    ##stcd
     C                   kfld                    ##clas
     C                   kfld                    ##effMod
     C                   kfld                    ##rateDt
      **** Beg Chg **** 06/17/24 **************************************
     C*******************kfld                    ##kkrate          7 3
     C                   kfld                    ##kkrate          8 3
      **** End Chg **** 06/17/24 **************************************
     C                   kfld                    ##expo

     C                   if        ##clas <> 0

     C                   eval      ##kkrate = ##rate
     C     seky          chain     pse01                              70
     C                   if        *in70 = *on

     C                   eval      seclas = ##clas
     C                   eval      seexp# = ##exp#

     C                   If        SEclas = 12
     C                   Eval      SEexMd = 0
     C                   Else
     C                   eval      seexmd = ##expMod
     C                   EndIf
     C                   eval      semddt = ##effMod
     C                   eval      sertdt = ##rateDt
     C                   eval(h)   seexpa = ##expamt
     C                   eval(h)   seprma = ##prmamt
     C                   eval      semnrt = ##rate
     C                   eval      setier = ##tier
     C                   eval      seexpo = ##expo
     C
     C                   eval      seco# = ##co#
     C                   eval      sefnd = ##fnd
     C                   eval      seemp# = ##emp#
     C                   eval      sefyr = ##fyr
     C                   eval      sestcd = ##stcd
     C                   eval      serpmy = ##rpmy
     C                   eval      seprvr = 0
     C                   eval      seupdt = 'R'
     C                   eval      seuser = q1user
     C                   eval      sepgm = q1pgm
     C                   eval      setime = dstime
     C                   eval      sedt = dtCYMD(dsdate)
     C                   eval      seiusr = seuser
     C                   eval      seidt = sedt
     C                   eval      sesplt = ##splt

     C                   write     pse01
     C                   add       1             u#exposure

     C                   else

     C                   eval(h)   seexpa = seexpa + ##expamt
     C                   eval(h)   seprma = seprma + ##prmamt
     C                   update    pse01

     C                   endif                                                  *in70=*on

     C                   endif                                                  *in70=*on

     P writeExp        e
     P*===========================================================================



     P*===========================================================================
     P* splitPyrl - splits payroll records
     P*      Parameters - company#                 3,0
     P*                   fund                     3,0
     P*                   fund year                3,0
     P*                   employer #               9,0
     P*                   division                 5,0
     P*                   payroll class            5,0
     P*                   payroll spec             1
     P*                   index                    3,0
     P*                   exposure amount         11,2
     P*                   premium amount          11,2
     P*                   manual rate              5,2
     P*                   pyrl eff date            7,0
     P*                   beginning date           7,0
     P*                   ending date              7,0
     P*                   per capita flag          1
     P*                   per capita flag          1
     P*===========================================================================
     P splitPyrl       b                   export
     D splitPyrl       pi
     D   ##co#                        3  0 Const
     D   ##fnd                        3  0 Const
     D   ##fyr                        3  0 Const
     D   ##emp#                       9  0 Const
     D   ##div                        5  0 Const
     D   ##clas                       5  0 Const
     D   ##spec                       1    Const
     D   ##idx                        3  0 Const
     D   ##eamt                      11  2
     D   ##prem                      11  2
     D**** Beg Chg **** 06/17/24 *******************************************
     D****##arat                       7  2 Const
     D   ##arat                       8S 3 Const
     D**** End Chg **** 06/17/24 *******************************************
     D   ##efdt                       7  0 Const
     D   ##beg_dt                     7  0 Const
     D   ##end_dt                     7  0 Const
     D   ##percap                     1    Const
     D   ##pcf                        1    Const
     D
     D  fromdt         s              7  0
     D  todt           s              7  0

     C     epky3         klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd
     C                   kfld                    ##fyr
     C                   kfld                    ##emp#
     C                   kfld                    ##div
     C                   kfld                    ##clas
     C                   kfld                    ##spec
     C                   kfld                    ##efdt
     C*
     C     epky4         klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd
     C                   kfld                    ##fyr
     C                   kfld                    ##emp#
     C                   kfld                    ##div
     C                   kfld                    ##clas
     C                   kfld                    ##spec

     C                   eval      fromdt = ##efdt
     C                   eval      kkefdt = splt_dt(##idx)
     C     epky3         setgt     pep02
     C     epky4         reade     pep02                                  61
     C                   if        *in61 = *off
     C                   eval       todt = ep2efdt
     C                   else
     C                   eval       todt = ##end_dt
     C                   endif

     C                   if        todt > ##end_dt
     C                   eval       todt = ##end_dt
     C                   endif
     C
     C                   if        ##idx < 5
     C                             and todt > splt_dt(##idx+1)
     C                   eval(h)    expamt(##idx) = pro_rate(##beg_dt :
     C                                                    splt_dt(##idx+1) :
     C                                                       fromdt : todt :
     C                                                          ##eamt)
     C                   eval(h)    ##eamt = ##eamt - expamt(##idx)
     C
     C                   eval(h)    pramt(##idx) = pro_rate(##beg_dt :
     C                                                   splt_dt(##idx+1) :
     C                                                      fromdt : todt :
     C                                                         ##prem)
     C                   eval(h)    ##prem = ##prem - pramt(##idx)
     C
     C                   callp     splitPyrl(##co# : ##fnd : ##fyr : ##emp#
     C                               : ##div : ##clas : ##spec : ##idx+1 :
     C                               ##eamt : ##prem : ##arat : ##efdt :
     C                               splt_dt(##idx+1) : ##end_dt : ##percap :
     c                               ##pcf)
     C
     C                   else
     C
     C                   eval(h)   expamt(##idx) = expamt(##idx) + ##eamt
     C                   eval(h)   pramt(##idx) = pramt(##idx) + ##prem
     C                   endif

     C                   if        ##percap=*on
     C                             and ##pcf <> 'O'
     C                   eval       expamt(##idx) = 10 * expamt(##idx)
     C                   eval       pramt(##idx) = 100 * pramt(##idx)
     C                   endif


     C*          *fill buckets*
     C                   eval       n = 1
     C                   do        3             m
     C                   if        ##beg_dt >= EffMod(m)
     C                             and EffMod(m) <> 0
     C                   eval       n = m
     C                   endif
     C                   enddo
     C
     C                   eval      pefdt(##idx) = effMod(n)
     C                   eval      pexmd(##idx) = expMod(n)
     C*
     C*                find rate date based on epefdt
     C                   eval       n = 1
     C                   do        3             m
     C                   if        epefdt >= Rate_dt(m)
     C                             and Rate_dt(m) <> 0
     C                   eval       n = m
     C                   endif
     C                   enddo
     C*
     C                   eval      ratedt(##idx) = rate_dt(n)

     C                   eval       kktbid = 'WCST'
     C                   movel(p)  Juris_st      kkkey
     C     gtky          chain     pgt01
      /free
       If %found and gtstat = 'A'
        and juris_st <> 'NC';
      /end-free
     C                   move      el2rtcd       kkrtcd
     C                   move      epclas        kkclas
     C                   move      rate_dt(n)    kkefd1
     C     prky          SETGT     PPR01
     c     prky1         READPE    PPR01                                  64
     C  N64              eval      ratedt(##idx) = PREFDT
     C                   endif
     C                   eval      manrt(##idx) = ##arat
     C                   eval      splitp(##idx) = ##idx-1
     C                   move      prtier        rttier(##idx)
     C                   if        preact = '  '
     C                   eval      preact = '01'
     C                   endif
     C                   move      preact        expact(##idx)
     C
     C                   if        prlhcd = 'Y' or epspec = 'F'
     C                   move      '02'          expact(##idx)
     C                   eval      expos#(##idx) = 21
     C                   else
     C                   eval      expos#(##idx) = 11
     C                   endif
     C                   eval      expos#(##idx) = 00

     P splitPyrl       e



     P*===========================================================================
     P* pro_rate - this procedure pro rates the amount passed in using the dates passed
     P*            in to determine the factor:       (tdt - fdt)
     P*                                            ---------------- * amount
     P*                                             (end_dt - stdt)
     P*===========================================================================
     P pro_rate        b                   export
     D pro_rate        pi            11  2
     D   fdt                          7  0 Const
     D   tdt                          7  0 Const
     D   stdt                         7  0 Const
     D   endt                         7  0 Const
     D   amount                      11  2 Const

     D  dtfrom         s               d   datfmt(*usa)
     D  dtto           s               d   datfmt(*usa)
     D  dtstrt         s               d   datfmt(*usa)
     D  dtend          s               d   datfmt(*usa)
     D
     D  xdays          s              5  0
     D  ydays          s              5  0

     C     *cymd         move      fdt           dtfrom
     C     *cymd         move      tdt           dtto
     C     *cymd         move      stdt          dtstrt
     C     *cymd         move      endt          dtend

     C     dtto          subdur    dtfrom        xdays:*days
     C     dtend         subdur    dtstrt        ydays:*days
     C
     C                   return    (xdays/ydays) * amount

     P pro_rate        e



     P*============================================================================
     P*  GetDedTyp - procedure to determine the value for the first two positions
     P*              of Deductible Type in the header record
     P*====================================================================
     P getDedTyp       b                   export
     D getDedTyp       pi             2
     D   company                      3  0
     D   array                       20
     D
     D  dedRsvArr      s              2    dim(10)
     D
     D  ctype          s              3  0 inz(0)
     D  mtype          s              3  0 inz(0)
     D  @@key          s             10
     D  dtype          s              3  0 inz(0)
     D  dedChar        s              2

     C*   table file
     C     tbky2         klist
     C                   kfld                    company
     C                   kfld                    @@key

     C                   movea     array         dedRsvArr

     C                   do        10            i
     C                   if        dedRsvArr(i) <> ' '
     C                   movel(p)  dedRsvArr(i)  @@key
     C*
     C     tbky2         chain     prt01                              60
     C                   if         *in60=*off
     C                   select
     C     rttype        wheneq    'M'
     C                   eval       mtype = 1
     C*
     C     rttype        wheneq    'C'
     C                   eval       ctype = 2
     C*
     C                   endsl
     C                   endif                                                  *in60=*off
     C*
     C                   endif                                                  ded_arr(i)<>' '
     C                   enddo
     C*
     C                   eval       dtype = ctype + mtype
     C                   move(p)   dtype         dedChar
     C
     C                   return     dedChar
     C*
     P getDedTyp       e



     P*=====================================================================
     P* dt6to8L- converts a numeric date in MMYYYY format to MM01YYYY
     P*  Parameter 1 - 6 numeric date
     P*  Returns - 8 numeric date (zero if invalid)
     P*=====================================================================
     Pdt6to8L          B
     Ddt6to8L          PI             8  0
     D numDate6                       6  0 Const
     D
     D num8            s              8  0
     D alpha6          s              6
     D alpha8          s              8
     C
     C                   move      numDate6      alpha6
     C                   if        numDate6 > 10000
     C                   eval      alpha8   = %subst(alpha6:1:2) + '01' +
     C                                        %subst(alpha6:3:4)
     C                   else
     C                   eval      alpha8   = '00' + %subst(alpha6:3:2) +
     C                                        '01' + %subst(alpha6:5:2)
     C                   endif
     C                   move      alpha8        num8
     C*
     C                   eval      num8 = dtCheck(num8)
     C*
     C                   return    num8
     C
     Pdt6to8L          E

     *=============================================================================================
      * Gets loss cost multiplier for payroll class being reported  (returns 1 if no LCM)
     *=============================================================================================
     p getLossCost     b
     d getLossCost     pi             5  4
     d  class                               like(EPCLAS)
     d  effDt                               like(EPEFDT) const

     d qqco#           s              3
     d qqfnd           s              3
     d qqrtcd          s              3
     d qqclas          s              5
     d qqefdt          s              7
     d qqlcm           s              5
     d lossCostMult    s              5  4       inz(1)

     c                   move(p)   EL2CO#        qqco#
     c                   move(p)   EL2FND        qqfnd
     c                   move(p)   EL2RTCD       qqrtcd
     c                   move(p)   class         qqclas
     c                   move(p)   effDt         qqefdt

     c                   call      'SRLCM'
     c                   parm                    qqco#
     c                   parm                    qqfnd
     c                   parm                    qqrtcd
     c                   parm                    qqclas
     c                   parm                    qqefdt
     c                   parm      *blanks       qqlcm

     c                   if        (qqlcm <> *blanks)
     c                   move      qqlcm         lossCostMult
     c                   endif

     c                   return    lossCostMult

     p getLossCost     e

     *=============================================================================================
      * Gets rating date for discount/factor/charge.
     *=============================================================================================
     p getRateDate     b
     d getRateDate     pi             7  0
     d  effDt                               like(EPEFDT)

     d qqco#           s              3
     d qqfnd           s              3
     d qqrtcd          s              3
     d qqefdt          s              7
     d qqlcm           s              5
     d RateDate        s              7  0       inz(1)

     c                   move(p)   EL2CO#        qqco#
     c                   move(p)   EL2FND        qqfnd
     C                   MOVE      EL2FYR        Q1FYR
     C                   MOVE      EL2EMP#       Q1EMP#
     C                   MOVE      EL2DIV        Q1DIV
     C                   MOVE      effDt         Q1DATE
     C*
     C                   CALL      'SREFFD'
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    Q1FYR             3
     C                   PARM                    Q1EMP#            9
     C                   PARM                    Q1DIV             5
     C                   PARM                    Q1DATE            7
     C*
     C                   MOVE      Q1DATE        RateDate

     c                   return    RateDate

     p getRateDate     e
     *=============================================================================================
      * Check if rate change exist for Terror charge or assessments
     *=============================================================================================
     p prChkRateChg    b
     d prChkRateChg    pi              n
     d  rate1                         5s 3
     d  rate2                         5s 3
     d  rate3                         5s 3

     d split           s               n

     c                   eval      split = *on
     C                   if        (rate1 = rate2 and (rate2 = rate3  or
     C                              rate3 = 0)) or (rate2 = 0 and rate3 = 0)
     C                   eval      split = *off
     C                   endif

     c                   return    split

     p prChkRateChg    e
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
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
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
      *====================================================================
      *  Total all F-class or harbor exposure payroll on policy
      *====================================================================
     p FClassTotal     b
     d FClassTotal     pi            11  2

     d xxTotal         s             11  2 inz(0)

      /free
       Setll epky2 pep01;
       Reade epky2 pep01;

       Dow not %eof;

          If epstat <> 'I';

      /end-free
     C                   Z-ADD     0             X1EFDT
     C                   MOVE(P)   ELCO#         ##CO#             3
     C                   MOVE(P)   ELFND         ##FND             3
     C                   MOVE(P)   ELFYR         ##FYR             3
     C                   MOVE(P)   ELEMP#        ##EMP#            9
     C                   MOVE(P)   ELDIV         ##DIV             5
     C                   MOVE(P)   EPEFDT        ##DATE            7
     C                   MOVE(P)   'P'           ##TYPE            1
     C                   MOVE(P)   EPCLAS        ##CLAS            5
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
      /free

             Chain elky0 pel00;

             Setgt prky3 ppr01;
             Readpe prky4 ppr01;

             If not %eof
              and prlhcd = 'Y'
              or EPSpec = 'F';

                If adtPyrl = *on;
                   xxTotal = xxTotal + EPAmpr;
                Else;
                   xxTotal = xxTotal + EPEmpr;
                Endif;

             Endif;

          Endif;

          Reade epky2 pep01;
       Enddo;

       Return xxTotal;

      /end-free

     p FClassTotal     e

**Ctdata lvl
123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ
